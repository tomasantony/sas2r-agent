out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"

find_file_for <- function(dir, memname) {
  if (!dir.exists(dir)) return(NA_character_)
  allf <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(allf) == 0) return(NA_character_)
  bn <- tools::file_path_sans_ext(basename(allf))
  idx <- which(tolower(bn) == tolower(memname))
  if (length(idx) == 0) return(NA_character_)
  # prefer .sas7bdat then .xpt then others
  cands <- allf[idx]
  ext <- tolower(tools::file_ext(cands))
  ord <- order(match(ext, c("sas7bdat", "xpt", "sas7bdatx", "csv", "parquet")), na.last = TRUE)
  cands[ord[1]]
}

read_sas_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    # try sas then xpt
    try1 <- try(haven::read_sas(path), silent = TRUE)
    if (!inherits(try1, "try-error")) {
      df <- try1
    } else {
      df <- haven::read_xpt(path)
    }
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

lb_path <- find_file_for(sdtm_dir, "lb")
adsl_path <- find_file_for(adam_dir, "adsl")

if (is.na(lb_path)) {
  # fallback: try raw cdisc locations
  alt1 <- find_file_for("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm", "lb")
  lb_path <- alt1
}
if (is.na(adsl_path)) {
  adsl_path <- find_file_for("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam", "adsl")
}
message("lb_path=", lb_path)
message("adsl_path=", adsl_path)
stopifnot(!is.na(lb_path), !is.na(adsl_path))

lb <- read_sas_dataset(lb_path)
adsl <- read_sas_dataset(adsl_path)

# normalize names to SAS uppercase for processing, but keep original for output
# Work with uppercase names internally, then restore
orig_lb_names <- names(lb)
orig_adsl_names <- names(adsl)
names(lb) <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))

stopifnot("USUBJID" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl))
# PARAM, LBDTC, AVAL expected in lb; RFSTDTC, TRT01P in adsl
stopifnot("PARAM" %in% names(lb))
stopifnot("LBDTC" %in% names(lb))
stopifnot("AVAL" %in% names(lb))
stopifnot("RFSTDTC" %in% names(adsl))

# PROC SORT data=sdtm.lb; by USUBJID PARAM LBDTC;
# PROC SORT data=adam.adsl; by USUBJID;
# SAS sort: ascending, missing first, binary (uppercase before lowercase)
sas_order <- function(df, cols) {
  # replace NA with "" for character ordering handled by na.last=FALSE
  # use order with radix for binary order
  args <- lapply(cols, function(c) df[[c]])
  # order with method radix, na.last FALSE (missing first)
  do.call(order, c(args, list(method = "radix", na.last = FALSE)))
}

lb_ord <- sas_order(lb, c("USUBJID", "PARAM", "LBDTC"))
lb <- lb[lb_ord, , drop = FALSE]
adsl_ord <- sas_order(adsl, c("USUBJID"))
adsl <- adsl[adsl_ord, , drop = FALSE]

# keep USUBJID TRT01P RFSTDTC from adsl
keep_adsl_cols <- intersect(c("USUBJID", "TRT01P", "RFSTDTC"), names(adsl))
adsl_keep <- adsl[, keep_adsl_cols, drop = FALSE]

# Before merging, drop overlapping non-key columns from lb so no .x/.y
# SAS MERGE: later dataset overwrites earlier for overlapping vars
common_nonkey <- intersect(names(lb), names(adsl_keep))
common_nonkey <- setdiff(common_nonkey, "USUBJID")
if (length(common_nonkey) > 0) {
  lb <- lb[, setdiff(names(lb), common_nonkey), drop = FALSE]
}
stopifnot("USUBJID" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl_keep))

# merge sdtm.lb (in=a) adam.adsl (in=b); by USUBJID; if a and b; => inner join
merged <- inner_join(lb, adsl_keep, by = "USUBJID")

# SAS merge output is in BY order; re-sort by USUBJID PARAM LBDTC
if (nrow(merged) > 0) {
  m_ord <- sas_order(merged, c("USUBJID", "PARAM", "LBDTC"))
  merged <- merged[m_ord, , drop = FALSE]
}
stopifnot("LBDTC" %in% names(merged))
stopifnot("RFSTDTC" %in% names(merged))
stopifnot("AVAL" %in% names(merged))
stopifnot("PARAM" %in% names(merged))

# Helper: SAS INPUT(..., yymmdd10.) : parse first 10 chars as YYYY-MM-DD
parse_yymmdd10 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }
  xc <- as.character(x)
  # haven labelled -> as.character may give labels? ensure character
  # empty string -> NA
  s10 <- substr(xc, 1, 10)
  # strict YYYY-MM-DD
  suppressWarnings(as.Date(s10, format = "%Y-%m-%d"))
}

lb_dt <- parse_yymmdd10(merged[["LBDTC"]])
rf_dt <- parse_yymmdd10(merged[["RFSTDTC"]])

# SAS missing (<) semantics: missing is smallest
# is_predose = lb_dt < rf_dt with missing handling
is_predose <- rep(FALSE, length(lb_dt))
for (i in seq_along(lb_dt)) {
  a <- lb_dt[i]; b <- rf_dt[i]
  na_a <- is.na(a); na_b <- is.na(b)
  if (na_a && na_b) {
    is_predose[i] <- FALSE
  } else if (na_a && !na_b) {
    is_predose[i] <- TRUE
  } else if (!na_a && na_b) {
    is_predose[i] <- FALSE
  } else {
    is_predose[i] <- (a < b)
  }
}

aval <- suppressWarnings(as.numeric(merged[["AVAL"]]))

n <- nrow(merged)
BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep("", n)
CHG_vec <- rep(NA_real_, n)

# RETAIN BASE; FIRST.PARAM reset
# Emulate FIRST.PARAM as first row of each USUBJID+PARAM group in sorted order
# (SAS code references FIRST.PARAM with BY USUBJID; intended per PARAM)
current_base <- NA_real_
prev_usub <- NA_character_
prev_param <- NA_character_
first_row <- TRUE

usub_vec <- as.character(merged[["USUBJID"]])
param_vec <- as.character(merged[["PARAM"]])
# keep NA as NA_character_ (as.character gives NA for NA)

sas_equal <- function(a, b) {
  # SAS: . = . is true; . = non-missing false
  na_a <- is.na(a); na_b <- is.na(b)
  if (na_a && na_b) return(TRUE)
  if (na_a || na_b) return(FALSE)
  return(a == b)
}

for (i in seq_len(n)) {
  u <- usub_vec[i]
  p <- param_vec[i]
  # determine first.PARAM: change in USUBJID or PARAM (NA equals NA)
  is_first <- FALSE
  if (first_row) {
    is_first <- TRUE
  } else {
    eq_u <- (is.na(u) && is.na(prev_usub)) || (!is.na(u) && !is.na(prev_usub) && u == prev_usub)
    eq_p <- (is.na(p) && is.na(prev_param)) || (!is.na(p) && !is.na(prev_param) && p == prev_param)
    if (!(eq_u && eq_p)) is_first <- TRUE
  }
  if (is_first) {
    current_base <- NA_real_
  }
  # if predose and not missing AVAL then BASE = AVAL
  if (is_predose[i] && !is.na(aval[i])) {
    current_base <- aval[i]
  }
  BASE_vec[i] <- current_base
  # ABLFL
  if (is_predose[i] && sas_equal(aval[i], current_base)) {
    ABLFL_vec[i] <- "Y"
  } else {
    ABLFL_vec[i] <- ""
  }
  # CHG = AVAL - BASE
  if (is.na(aval[i]) || is.na(current_base)) {
    CHG_vec[i] <- NA_real_
  } else {
    CHG_vec[i] <- aval[i] - current_base
  }
  prev_usub <- u
  prev_param <- p
  first_row <- FALSE
}

merged[["BASE"]] <- BASE_vec
merged[["ABLFL"]] <- ABLFL_vec
merged[["CHG"]] <- CHG_vec

# drop LBDTC RFSTDTC
drop_cols <- intersect(names(merged), c("LBDTC", "RFSTDTC"))
merged <- merged[, setdiff(names(merged), drop_cols), drop = FALSE]

# Format dates for CSV: Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# Convert any Date/POSIXct columns to character ISO
for (cn in names(merged)) {
  col <- merged[[cn]]
  if (inherits(col, "Date")) {
    merged[[cn]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    # format as ISO datetime
    merged[[cn]] <- format(as.POSIXct(col, tz = "UTC"), "%Y-%m-%dT%H:%M:%S")
  } else if (is.factor(col)) {
    merged[[cn]] <- as.character(col)
  } else if (inherits(col, "haven_labelled")) {
    # zap labels but keep values
    merged[[cn]] <- haven::zap_labels(col)
    # re-check date? haven dates are numeric with format; read_sas already converted?
    if (inherits(merged[[cn]], "Date")) {
      merged[[cn]] <- format(merged[[cn]], "%Y-%m-%d")
    } else if (inherits(merged[[cn]], "POSIXct")) {
      merged[[cn]] <- format(merged[[cn]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  # haven_labelled after zap may still be class? ensure plain
  if (inherits(merged[[cn]], "haven_labelled")) {
    merged[[cn]] <- as.vector(merged[[cn]])
  }
}

# Ensure column order: original lb order (minus dropped) + TRT01P (if not already) + BASE ABLFL CHG
# merged already has that order from join; keep as is

write.csv(merged, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
message("Wrote ", nrow(merged), " rows to ", file.path(out, "adlb.csv"))
