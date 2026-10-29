out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(lubridate)
})

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"
fallback_sdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
fallback_adam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
fallback_split <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"

read_mem <- function(libdir, mem, fallbacks = c()) {
  dirs <- c(libdir, fallbacks)
  for (d in dirs) {
    if (is.na(d) || d == "") next
    if (!dir.exists(d)) next
    files <- list.files(d, full.names = TRUE)
    if (length(files) == 0) next
    base_noext <- tools::file_path_sans_ext(basename(files))
    idx <- which(tolower(base_noext) == tolower(mem))
    if (length(idx) == 0) next
    cand <- files[idx]
    # prefer .sas7bdat then .xpt then others
    ext <- tolower(tools::file_ext(cand))
    ord <- order(match(ext, c("sas7bdat", "xpt", "sas7bcat", "csv")), na.last = TRUE)
    cand <- cand[ord]
    for (f in cand) {
      e <- tolower(tools::file_ext(f))
      df <- NULL
      if (e == "sas7bdat") {
        df <- haven::read_sas(f)
      } else if (e == "xpt") {
        df <- haven::read_xpt(f)
      } else if (e == "csv") {
        df <- readr::read_csv(f, show_col_types = FALSE)
      } else {
        next
      }
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      return(df)
    }
  }
  stop(paste0("Dataset ", mem, " not found in ", libdir))
}

zap_all <- function(df) {
  # remove haven labels/formats but keep values
  df <- tryCatch(haven::zap_labels(df), error = function(e) df)
  df <- tryCatch(haven::zap_formats(df), error = function(e) df)
  df <- tryCatch(haven::zap_label(df), error = function(e) df)
  as.data.frame(df, stringsAsFactors = FALSE)
}

message("Reading sdtm.lb and adam.adsl")
lb_raw <- read_mem(sdtm_dir, "lb", fallbacks = c(fallback_sdtm))
adsl_raw <- read_mem(adam_dir, "adsl", fallbacks = c(fallback_adam, fallback_split))

lb_raw <- zap_all(lb_raw)
adsl_raw <- zap_all(adsl_raw)

# normalize names to uppercase for processing (case-insensitive match is OK for output)
names(lb_raw) <- toupper(names(lb_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

# keep only needed vars from adsl: USUBJID TRT01P RFSTDTC
adsl_keep_vars <- intersect(c("USUBJID", "TRT01P", "RFSTDTC"), names(adsl_raw))
adsl <- adsl_raw[, adsl_keep_vars, drop = FALSE]

lb <- lb_raw

# ensure key vars exist
for (v in c("USUBJID", "PARAM", "LBDTC", "AVAL")) {
  if (!v %in% names(lb)) {
    if (v == "AVAL") lb$AVAL <- NA_real_
    else if (v == "LBDTC") lb$LBDTC <- NA_character_
    else if (v == "PARAM") lb$PARAM <- NA_character_
    else if (v == "USUBJID") lb$USUBJID <- NA_character_
  }
}
for (v in c("USUBJID", "TRT01P", "RFSTDTC")) {
  if (!v %in% names(adsl)) {
    adsl[[v]] <- NA_character_
  }
}

# coerce types: USUBJID/PARAM/LBDTC/RFSTDTC/TRT01P to character, AVAL numeric
to_char_vec <- function(x) {
  if (inherits(x, "POSIXt")) format(x, "%Y-%m-%dT%H:%M:%S")
  else if (inherits(x, "Date")) format(x, "%Y-%m-%d")
  else as.character(x)
}
lb$USUBJID <- to_char_vec(lb$USUBJID)
lb$PARAM <- to_char_vec(lb$PARAM)
lb$LBDTC <- to_char_vec(lb$LBDTC)
lb$AVAL <- suppressWarnings(as.numeric(lb$AVAL))
adsl$USUBJID <- to_char_vec(adsl$USUBJID)
if ("TRT01P" %in% names(adsl)) adsl$TRT01P <- to_char_vec(adsl$TRT01P)
if ("RFSTDTC" %in% names(adsl)) adsl$RFSTDTC <- to_char_vec(adsl$RFSTDTC)

# proc sort data=sdtm.lb; by USUBJID PARAM LBDTC;
ord_lb <- order(lb$USUBJID, lb$PARAM, lb$LBDTC, na.last = FALSE, method = "radix")
lb <- lb[ord_lb, , drop = FALSE]
# proc sort data=adam.adsl; by USUBJID;
ord_adsl <- order(adsl$USUBJID, na.last = FALSE, method = "radix")
adsl <- adsl[ord_adsl, , drop = FALSE]

# merge sdtm.lb (in=a) adam.adsl (in=b keep=USUBJID TRT01P RFSTDTC); by USUBJID; if a and b;
# inner join on USUBJID (ADSL assumed one row per subject; SAS merge semantics equal inner join here)
merged <- dplyr::inner_join(lb, adsl, by = "USUBJID")

# ensure order by USUBJID PARAM LBDTC after merge (SAS merge output is in BY order)
if (nrow(merged) > 0) {
  ord_m <- order(merged$USUBJID, merged$PARAM, merged$LBDTC, na.last = FALSE, method = "radix")
  merged <- merged[ord_m, , drop = FALSE]
}

# helper: SAS yymmdd10. input -> Date (first 10 chars)
sas_yymmdd10_vec <- function(x) {
  n <- length(x)
  res <- as.Date(rep(NA_character_, n))
  for (i in seq_len(n)) {
    xi <- x[i]
    if (is.na(xi)) next
    s <- as.character(xi)
    if (is.na(s) || trimws(s) == "") next
    s10 <- substr(s, 1, 10)
    d <- suppressWarnings(lubridate::ymd(s10, quiet = TRUE))
    if (is.na(d)) {
      d2 <- suppressWarnings(as.Date(s10, format = "%Y-%m-%d"))
      if (!is.na(d2)) d <- d2
    }
    res[i] <- d
  }
  res
}

is_missing_char <- function(x) is.na(x) | x == ""
sas_char_eq <- function(a, b) {
  ma <- is_missing_char(a)
  mb <- is_missing_char(b)
  if (ma && mb) return(TRUE)
  if (ma || mb) return(FALSE)
  return(a == b)
}

n <- nrow(merged)
# preserve original LBDTC/RFSTDTC char for calc
lbdtc_char <- if ("LBDTC" %in% names(merged)) to_char_vec(merged$LBDTC) else rep(NA_character_, n)
rfstdtc_char <- if ("RFSTDTC" %in% names(merged)) to_char_vec(merged$RFSTDTC) else rep(NA_character_, n)
aval_vec <- if ("AVAL" %in% names(merged)) suppressWarnings(as.numeric(merged$AVAL)) else rep(NA_real_, n)

lb_date <- sas_yymmdd10_vec(lbdtc_char)
rf_date <- sas_yymmdd10_vec(rfstdtc_char)

BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep("", n)
CHG_vec <- rep(NA_real_, n)

BASE <- NA_real_
if (n > 0) {
  for (i in seq_len(n)) {
    usub <- merged$USUBJID[i]
    par <- merged$PARAM[i]
    # FIRST.PARAM : reset BASE at first obs of each USUBJID+PARAM group (sorted order)
    is_first <- FALSE
    if (i == 1) {
      is_first <- TRUE
    } else {
      prev_usub <- merged$USUBJID[i - 1]
      prev_par <- merged$PARAM[i - 1]
      if (!sas_char_eq(usub, prev_usub) || !sas_char_eq(par, prev_par)) is_first <- TRUE
    }
    if (is_first) BASE <- NA_real_
    # Baseline = last non-missing pre-dose value
    ld <- lb_date[i]
    rd <- rf_date[i]
    av <- aval_vec[i]
    date_lt <- !is.na(ld) && !is.na(rd) && (ld < rd)
    if (date_lt && !is.na(av)) {
      BASE <- av
    }
    BASE_vec[i] <- BASE
    # Baseline flag: if date< and AVAL=BASE (SAS: missing=missing is true)
    aval_eq_base <- FALSE
    if (date_lt) {
      if (is.na(av) && is.na(BASE)) aval_eq_base <- TRUE
      else if (!is.na(av) && !is.na(BASE) && av == BASE) aval_eq_base <- TRUE
      else aval_eq_base <- FALSE
    }
    if (date_lt && aval_eq_base) ABLFL_vec[i] <- "Y" else ABLFL_vec[i] <- ""
    # Change from baseline
    if (is.na(av) || is.na(BASE)) CHG_vec[i] <- NA_real_ else CHG_vec[i] <- av - BASE
  }
}

merged$BASE <- BASE_vec
merged$ABLFL <- ABLFL_vec
merged$CHG <- CHG_vec

# drop LBDTC RFSTDTC
drop_vars <- intersect(c("LBDTC", "RFSTDTC"), names(merged))
if (length(drop_vars) > 0) merged <- merged[, setdiff(names(merged), drop_vars), drop = FALSE]

# Ensure column order: lb vars (minus LBDTC) + TRT01P + BASE ABLFL CHG
# (merged already in that order because join appends adsl vars then new vars at end)

adlb <- merged

# Format Dates / datetimes for CSV per spec
for (cn in names(adlb)) {
  col <- adlb[[cn]]
  if (inherits(col, "POSIXt")) {
    adlb[[cn]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  } else if (inherits(col, "Date")) {
    adlb[[cn]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "haven_labelled")) {
    adlb[[cn]] <- haven::zap_labels(col)
  }
}

message(paste0("Writing adlb.csv with ", nrow(adlb), " rows"))
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
