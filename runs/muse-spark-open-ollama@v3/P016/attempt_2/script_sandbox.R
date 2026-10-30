# P016_sdtm_mh.sas -> R translation
# Output: dataset mh -> OUT_DIR/mh.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: round half away from zero (SAS behavior)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

project_path <- "/bench/data/derived/c2c"
root <- "/bench"

candidates <- c(
  file.path(project_path, "data/raw/raw_mh.csv"),
  "/bench/data/derived/c2c/data/raw/raw_mh.csv",
  file.path(root, "data/derived/c2c/data/raw/raw_mh.csv"),
  file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_mh.csv"),
  "/bench/data/raw/raw_mh.csv"
)
candidates <- candidates[!is.na(candidates) & candidates != ""]
raw_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  poss <- list.files("/bench", pattern = "raw_mh.csv", recursive = TRUE, full.names = TRUE)
  if (length(poss) > 0) raw_path <- poss[1]
}
if (is.null(raw_path) || !file.exists(raw_path)) {
  stop(paste0("raw_mh.csv not found. Tried: ", paste(candidates, collapse="; ")))
}
cat("Reading:", raw_path, "\n")

# PROC IMPORT emulation: read as character to inspect raw values
# getnames=yes, dbms=csv replace
raw_mh_raw <- readr::read_csv(
  raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  trim_ws = FALSE,
  show_col_types = FALSE
)
# options validvarname=upcase -> SAS forces variable names to uppercase
names(raw_mh_raw) <- toupper(names(raw_mh_raw))

# Ensure expected columns exist (case-insensitive)
stopifnot("SUBJECT" %in% names(raw_mh_raw))
stopifnot("CONDITION" %in% names(raw_mh_raw))
# DIAGNOSIS_DATE may be present; if not, try to locate date column
if (!"DIAGNOSIS_DATE" %in% names(raw_mh_raw)) {
  # fallback: find column containing DIAGNOSIS
  cand <- grep("DIAGNOSIS", names(raw_mh_raw), value = TRUE)
  if (length(cand) > 0) {
    names(raw_mh_raw)[names(raw_mh_raw) == cand[1]] <- "DIAGNOSIS_DATE"
  } else {
    # create empty if missing
    raw_mh_raw$DIAGNOSIS_DATE <- NA_character_
  }
}

# Emulate PROC IMPORT GUESSINGROWS informat decision for DIAGNOSIS_DATE
# SAS: column of n/n/yyyy values becomes numeric SAS date read as DDMMYY10 when any value has first field >12 (otherwise MMDDYY10)
# Every value in that column is then read in that same order.
diag_vals_raw <- raw_mh_raw$DIAGNOSIS_DATE
diag_vals_trim <- trimws(diag_vals_raw)
# pattern for n/n/yyyy with / or -
slash_pat <- "^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$"
is_slash <- !is.na(diag_vals_trim) & diag_vals_trim != "" & grepl(slash_pat, diag_vals_trim)

informat <- "NONE"
use_dmy <- FALSE
if (any(is_slash)) {
  first_fields <- suppressWarnings(as.integer(sub("^\\s*(\\d{1,2})[/-].*", "\\1", diag_vals_trim[is_slash])))
  if (any(first_fields > 12, na.rm = TRUE)) {
    informat <- "DDMMYY"
    use_dmy <- TRUE
  } else {
    informat <- "MMDDYY"
    use_dmy <- FALSE
  }
} else {
  # check if any ISO-like YYYY-MM-DD present
  iso_pat <- "^\\d{4}-\\d{1,2}-\\d{1,2}$"
  if (any(grepl(iso_pat, diag_vals_trim, perl = TRUE), na.rm = TRUE)) {
    informat <- "IS8601DA"
  }
}
cat("Inferred informat for DIAGNOSIS_DATE:", informat, "\n")

# Parse DIAGNOSIS_DATE with ONE explicit format decided above
# This emulates: if vtype(DIAGNOSIS_DATE)='C' then input(anydtdte.) else _mhdt=DIAGNOSIS_DATE
# In R we have raw strings; we parse with the single decided order
parse_with_informat <- function(x, informat, use_dmy) {
  if (is.na(x) || trimws(x) == "") return(as.Date(NA))
  x_trim <- trimws(x)
  # slash dates
  if (grepl(slash_pat, x_trim)) {
    if (use_dmy) {
      # DDMMYY10 -> dmy
      d <- suppressWarnings(lubridate::dmy(x_trim, quiet = TRUE))
      return(d)
    } else {
      # MMDDYY10 -> mdy
      d <- suppressWarnings(lubridate::mdy(x_trim, quiet = TRUE))
      return(d)
    }
  }
  # ISO dates YYYY-MM-DD
  if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", x_trim)) {
    d <- suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d"))
    if (!is.na(d)) return(d)
    # fallback to ymd
    return(suppressWarnings(lubridate::ymd(x_trim, quiet = TRUE)))
  }
  if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", x_trim)) {
    d <- suppressWarnings(as.Date(x_trim, format = "%Y/%m/%d"))
    if (!is.na(d)) return(d)
    return(suppressWarnings(lubridate::ymd(x_trim, quiet = TRUE)))
  }
  # For other character values, SAS anydtdte. would try to parse;
  # but per instruction we must NOT choose per value - use the single decided format
  # So attempt with decided format only, then fallback to ymd if informat is NONE
  if (informat == "DDMMYY") {
    return(suppressWarnings(lubridate::dmy(x_trim, quiet = TRUE)))
  } else if (informat == "MMDDYY") {
    return(suppressWarnings(lubridate::mdy(x_trim, quiet = TRUE)))
  } else {
    # NONE or IS8601DA: try ymd then mdy/dmy as last resort (to avoid losing data)
    d <- suppressWarnings(lubridate::ymd(x_trim, quiet = TRUE))
    if (!is.na(d)) return(d)
    if (use_dmy) return(suppressWarnings(lubridate::dmy(x_trim, quiet = TRUE)))
    return(suppressWarnings(lubridate::mdy(x_trim, quiet = TRUE)))
  }
}

# Vectorized parsing
mh_dates <- vapply(diag_vals_raw, function(v) {
  d <- parse_with_informat(v, informat, use_dmy)
  if (is.na(d)) return(NA_real_)
  as.numeric(d)
}, numeric(1))
mh_dates <- as.Date(mh_dates, origin = "1970-01-01")

# Build sdtm.mh step 1: length STUDYID $8 DOMAIN $2 USUBJID $40 MHTERM $60 MHSTDTC $10
# STUDYID="CDISC-01" DOMAIN="MH" USUBJID=catx('-',STUDYID,strip(SUBJECT)) MHTERM=strip(CONDITION)
# MHSTDTC=put(_mhdt,is8601da.)
sdtm_mh <- raw_mh_raw %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "MH",
    USUBJID = paste0("CDISC-01-", trimws(as.character(SUBJECT))),
    MHTERM = trimws(as.character(CONDITION)),
    `_mhdt` = mh_dates,
    MHSTDTC = ifelse(is.na(`_mhdt`), NA_character_, format(`_mhdt`, "%Y-%m-%d"))
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, MHTERM, MHSTDTC)

# proc sort data=sdtm.mh; by USUBJID MHSTDTC; run;
# SAS sorts missing first, character ordering uppercase before lowercase
# Use base order with na.last=FALSE to emulate missing first
ord <- order(sdtm_mh$USUBJID, sdtm_mh$MHSTDTC, na.last = FALSE)
sdtm_mh <- sdtm_mh[ord, , drop = FALSE]

# data sdtm.mh; retain STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC; set sdtm.mh; by USUBJID;
# if first.USUBJID then MHSEQ=1; else MHSEQ+1;
sdtm_mh <- sdtm_mh %>%
  group_by(USUBJID) %>%
  mutate(MHSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHSTDTC) %>%
  arrange(USUBJID, MHSTDTC)

# Ensure types
sdtm_mh$MHSEQ <- as.integer(sdtm_mh$MHSEQ)
sdtm_mh$STUDYID <- as.character(sdtm_mh$STUDYID)
sdtm_mh$DOMAIN <- as.character(sdtm_mh$DOMAIN)
sdtm_mh$USUBJID <- as.character(sdtm_mh$USUBJID)
sdtm_mh$MHTERM <- as.character(sdtm_mh$MHTERM)
sdtm_mh$MHSTDTC <- as.character(sdtm_mh$MHSTDTC)

# Write dataset as CSV with SAS variable names, dates as YYYY-MM-DD, na=""
out_file <- file.path(out, "mh.csv")
write.csv(sdtm_mh, out_file, row.names = FALSE, na = "")
cat("Wrote", nrow(sdtm_mh), "rows to", out_file, "\n")
