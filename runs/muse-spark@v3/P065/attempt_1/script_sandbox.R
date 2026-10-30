out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

sdtm_dir <- "/bench/data/derived/wxzhai/sdtm"
adam_dir <- "/bench/data/derived/wxzhai/adam"

find_mem <- function(libdir, mem) {
  files <- list.files(libdir, full.names = TRUE, ignore.case = FALSE)
  # case-insensitive match on basename without extension
  pat <- paste0("^", mem, "\\.")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) == 0) {
    stop(paste0("Dataset ", mem, " not found in ", libdir))
  }
  cand <- files[idx]
  # prefer sas7bdat, then xpt, then sas7bdat upper, then csv
  pref <- c("\\.sas7bdat$", "\\.xpt$", "\\.sas7bdat$", "\\.csv$", "\\.parquet$")
  # simple priority: sas7bdat first
  ord <- rep(99, length(cand))
  for (i in seq_along(cand)) {
    lc <- tolower(cand[i])
    if (grepl("\\.sas7bdat$", lc)) ord[i] <- 1
    else if (grepl("\\.xpt$", lc)) ord[i] <- 2
    else if (grepl("\\.csv$", lc)) ord[i] <- 3
    else ord[i] <- 4
  }
  cand[order(ord)[1]]
}

read_any <- function(f) {
  lc <- tolower(f)
  if (grepl("\\.sas7bdat$", lc)) {
    df <- haven::read_sas(f)
  } else if (grepl("\\.xpt$", lc)) {
    df <- haven::read_xpt(f)
  } else if (grepl("\\.csv$", lc)) {
    df <- readr::read_csv(f, show_col_types = FALSE, guess_max = 100000)
  } else {
    stop(paste0("Unsupported extension: ", f))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  # zap haven labels/formats to plain vectors
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "labelled")) x <- haven::zap_labels(x)
    if (inherits(x, "haven_labelled")) x <- as.vector(x)
    # convert Date/POSIX to character ISO to preserve raw strings
    if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
    else if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) x <- format(x, "%Y-%m-%dT%H:%M:%S")
    df[[nm]] <- x
  }
  # SAS names case-insensitive: normalise to uppercase for processing
  names(df) <- toupper(names(df))
  df
}

lb_file <- find_mem(sdtm_dir, "lb")
adsl_file <- find_mem(adam_dir, "adsl")
cat("Reading:", lb_file, "\n")
cat("Reading:", adsl_file, "\n")
lb <- read_any(lb_file)
adsl <- read_any(adsl_file)

# Ensure expected columns exist
stopifnot("USUBJID" %in% names(lb))
stopifnot("PARAM" %in% names(lb))
stopifnot("LBDTC" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("RFSTDTC" %in% names(adsl))
stopifnot("TRT01P" %in% names(adsl))

# PROC SORT data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
# SAS sorts: missing first, character ASCII (uppercase before lowercase)
# Use base order with na.last=FALSE. For character ASCII ordering, use xtfrm via
# ordering on raw bytes: convert with charToRaw collation by using order on
# translated values. Base order() on Windows may be locale-dependent, so force
# byte ordering by using data.table-style frank? Implement byte-rank ordering:
sas_order_key <- function(v) {
  if (is.numeric(v)) return(v)
  # character: map each string to a sortable form preserving ASCII order
  # order() with method="radix" uses C locale byte order
  v
}

# sort lb_qc
o_lb <- order(lb$USUBJID, lb$PARAM, lb$LBDTC, na.last = FALSE, method = "radix")
lb_qc <- lb[o_lb, , drop = FALSE]
rownames(lb_qc) <- NULL

# proc sort data=adam.adsl out=adsl_qc; by USUBJID;
o_adsl <- order(adsl$USUBJID, na.last = FALSE, method = "radix")
adsl_qc <- adsl[o_adsl, , drop = FALSE]
rownames(adsl_qc) <- NULL

# keep only USUBJID RFSTDTC TRT01P from adsl_qc (SAS keep=)
adsl_keep <- adsl_qc[, c("USUBJID", "RFSTDTC", "TRT01P"), drop = FALSE]

# SAS MERGE lb_qc (in=a) adsl_qc (in=b) by USUBJID; if a and b;
# Before merging drop/rename overlapping non-key columns to avoid .x/.y
common <- intersect(names(lb_qc), names(adsl_keep))
common <- setdiff(common, "USUBJID")
if (length(common) > 0) {
  # SAS merge would take value from second dataset; drop from first to emulate
  lb_qc <- lb_qc[, setdiff(names(lb_qc), common), drop = FALSE]
}
# inner join
merged <- merge(lb_qc, adsl_keep, by = "USUBJID", sort = FALSE, all = FALSE)
stopifnot("RFSTDTC" %in% names(merged))
stopifnot("LBDTC" %in% names(merged))
stopifnot("PARAM" %in% names(merged))

# Re-establish SAS merge order: by USUBJID, and within USUBJID retain lb order
# (lb_qc was sorted by USUBJID PARAM LBDTC). So sort merged by USUBJID PARAM LBDTC
o_m <- order(merged$USUBJID, merged$PARAM, merged$LBDTC, na.last = FALSE, method = "radix")
merged <- merged[o_m, , drop = FALSE]
rownames(merged) <- NULL

# Helper: SAS INPUT(LBDTC, yymmdd10.) -> first 10 chars as YYYY-MM-DD
parse_yymmdd10 <- function(x) {
  n <- length(x)
  res <- rep(as.Date(NA), n)
  for (i in seq_len(n)) {
    v <- x[i]
    if (is.na(v)) next
    v <- as.character(v)
    if (v == "") next
    # SAS yymmdd10. reads first 10 characters
    s <- substr(v, 1, 10)
    d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
    if (!is.na(d)) res[i] <- d
    else res[i] <- as.Date(NA)
  }
  res
}

LBDTC_chr <- as.character(merged$LBDTC)
RFSTDTC_chr <- as.character(merged$RFSTDTC)
lbdt <- parse_yymmdd10(LBDTC_chr)
rfstdt <- parse_yymmdd10(RFSTDTC_chr)

# AVAL numeric
AVAL_num <- suppressWarnings(as.numeric(merged$AVAL))

# SAS missing < non-missing is TRUE; missing vs missing is FALSE (equal)
sas_lt_date <- function(a, b) {
  # a, b are Date vectors (may contain NA)
  n <- length(a)
  out <- rep(FALSE, n)
  for (i in seq_len(n)) {
    ai <- a[i]; bi <- b[i]
    na_a <- is.na(ai); na_b <- is.na(bi)
    if (na_a && na_b) out[i] <- FALSE
    else if (na_a && !na_b) out[i] <- TRUE
    else if (!na_a && na_b) out[i] <- FALSE
    else out[i] <- (ai < bi)
  }
  out
}
pre <- sas_lt_date(lbdt, rfstdt)

# SAS equality for numeric with missing=missing TRUE
sas_eq <- function(a, b) {
  na_a <- is.na(a); na_b <- is.na(b)
  if (na_a && na_b) return(TRUE)
  if (na_a || na_b) return(FALSE)
  return(a == b)
}

# RETAIN BASE; FIRST.PARAM logic: reset at first row of each USUBJID+PARAM
# SAS BY USUBJID only would be invalid; emulate intended BY USUBJID PARAM
sas_key_equal <- function(a, b) {
  # treat NA == NA as equal (SAS groups missing BY together)
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  return(as.character(a) == as.character(b))
}

n <- nrow(merged)
BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep(NA_character_, n)
CHG_vec <- rep(NA_real_, n)

cur_BASE <- NA_real_
prev_usub <- NA
prev_param <- NA
first_row <- TRUE

for (i in seq_len(n)) {
  usub_i <- merged$USUBJID[i]
  param_i <- merged$PARAM[i]
  is_first_param <- FALSE
  if (first_row) {
    is_first_param <- TRUE
  } else {
    if (!sas_key_equal(usub_i, prev_usub) || !sas_key_equal(param_i, prev_param)) {
      is_first_param <- TRUE
    }
  }
  if (is_first_param) cur_BASE <- NA_real_

  aval_i <- AVAL_num[i]
  pre_i <- pre[i]

  if (!is.na(pre_i) && pre_i && !is.na(aval_i)) {
    cur_BASE <- aval_i
  }

  eq_i <- sas_eq(aval_i, cur_BASE)
  if (!is.na(pre_i) && pre_i && eq_i) {
    ABLFL_vec[i] <- "Y"
  } else {
    ABLFL_vec[i] <- NA_character_
  }

  BASE_vec[i] <- cur_BASE
  if (!is.na(cur_BASE)) {
    if (is.na(aval_i)) CHG_vec[i] <- NA_real_
    else CHG_vec[i] <- aval_i - cur_BASE
  } else {
    CHG_vec[i] <- NA_real_
  }

  prev_usub <- usub_i
  prev_param <- param_i
  first_row <- FALSE
}

merged$BASE <- BASE_vec
merged$ABLFL <- ABLFL_vec
merged$CHG <- CHG_vec

# drop temporary lbdt rfstdt (not stored) - nothing to drop

# Order columns: original lb_qc order + RFSTDTC TRT01P + BASE ABLFL CHG
# lb_qc names preserved; merged currently has USUBJID + rest of lb + RFSTDTC TRT01P + new
# Ensure final order sensible
orig_lb_names <- names(lb_qc)
extra_adsl <- c("RFSTDTC", "TRT01P")
# merged names: USUBJID, setdiff(orig_lb_names,"USUBJID"), extra, BASE,ABLFL,CHG
# Reorder explicitly if all present
desired <- c(orig_lb_names, setdiff(extra_adsl, orig_lb_names), c("BASE", "ABLFL", "CHG"))
desired <- desired[desired %in% names(merged)]
other_cols <- setdiff(names(merged), desired)
final_names <- c(desired, other_cols)
adlb_qc <- merged[, final_names, drop = FALSE]

# Write dataset as CSV: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# Character date columns already ISO; ensure Date columns formatted if any
for (nm in names(adlb_qc)) {
  x <- adlb_qc[[nm]]
  if (inherits(x, "Date")) adlb_qc[[nm]] <- format(x, "%Y-%m-%d")
  else if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) adlb_qc[[nm]] <- format(x, "%Y-%m-%dT%H:%M:%S")
}
write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")
cat("Wrote", nrow(adlb_qc), "rows to", file.path(out, "adlb_qc.csv"), "\n")
