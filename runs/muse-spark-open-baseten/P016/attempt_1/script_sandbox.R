# P016_sdtm_mh.sas -> R translation
# Outputs: mh.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- Sys.getenv("PROJECT_PATH")
if (project_path == "" || is.na(project_path)) {
  project_path <- "/bench/data/derived/c2c"
}
# Fallback to root if needed
if (!file.exists(file.path(project_path, "data/raw/raw_mh.csv"))) {
  alt_root <- Sys.getenv("root")
  if (alt_root != "" && !is.na(alt_root)) {
    alt_pp <- file.path(alt_root, "data/derived/c2c")
    if (file.exists(file.path(alt_pp, "data/raw/raw_mh.csv"))) project_path <- alt_pp
  }
}
raw_path <- file.path(project_path, "data/raw/raw_mh.csv")

# Also check legacy raw location
if (!file.exists(raw_path)) {
  cand2 <- "/bench/data/derived/c2c/data/raw/raw_mh.csv"
  if (file.exists(cand2)) raw_path <- cand2
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

# Helper: SAS strip = trim both sides
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, str_trim(as.character(x), side = "both"))
}

# Helper: SAS catx
sas_catx <- function(delim, ...) {
  args <- list(...)
  # strip each and remove NA/empty
  stripped <- lapply(args, function(a) {
    s <- sas_strip(a)
    s[s == ""] <- NA_character_
    s
  })
  # vectorized catx for vectors of same length
  n <- max(sapply(stripped, length))
  stripped <- lapply(stripped, function(v) rep_len(v, n))
  result <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(stripped, function(v) v[i])
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts) == 0) result[i] <- ""
    else result[i] <- paste(parts, collapse = delim)
  }
  result
}

# Helper: parse anydtdte. -> Date
parse_anydtdte <- function(x) {
  # x is character vector
  # return Date vector (NA where missing/invalid)
  if (length(x) == 0) return(as.Date(character(0)))
  # trim
  x_trim <- str_trim(as.character(x))
  x_trim[x_trim == ""] <- NA_character_
  res <- rep(as.Date(NA), length(x_trim))
  # For each non-NA, try parsing
  # Use lubridate parse_date_time with multiple orders
  # Also try as.Date with ISO first for speed
  idx <- which(!is.na(x_trim))
  if (length(idx) == 0) return(res)
  vals <- x_trim[idx]
  # Try lubridate
  # Suppress warnings
  parsed <- suppressWarnings(
    parse_date_time(vals,
                    orders = c("ymd", "Ymd", "mdy", "mdY", "dmy", "dmY",
                               "ymd HMS", "Ymd HMS", "mdy HMS", "mdY HMS", "dmy HMS", "dmY HMS",
                               "ymd HM", "mdy HM", "dmy HM",
                               "dmy", "mdy", "ymd",
                               "BdY", "bdy", "dBY", "dbY", "Ybd", "ymd", "dmy"),
                    exact = FALSE, truncated = 0, quiet = TRUE)
  )
  # parse_date_time returns POSIXct
  # Convert to Date
  parsed_date <- as.Date(parsed)
  # For those still NA, try additional formats manually (e.g., "15JUN2005", "15-JUN-2005")
  na_idx <- which(is.na(parsed_date))
  if (length(na_idx) > 0) {
    for (k in na_idx) {
      v <- vals[k]
      # Try formats
      fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y", "%d/%m/%Y", "%d-%m-%Y",
                "%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%d%b%y", "%d-%b-%y",
                "%b %d %Y", "%b %d, %Y", "%B %d %Y", "%B %d, %Y",
                "%d %B %Y", "%Y%m%d", "%m/%d/%y", "%d/%m/%y")
      found <- NA
      for (f in fmts) {
        d <- suppressWarnings(as.Date(v, format = f))
        if (!is.na(d)) { found <- d; break }
        # try upper case month
        d2 <- suppressWarnings(as.Date(toupper(v), format = toupper(f)))
        if (!is.na(d2)) { found <- d2; break }
      }
      if (!is.na(found)) parsed_date[k] <- found
    }
  }
  res[idx] <- parsed_date
  res
}

# Read raw_mh.csv - mimic PROC IMPORT getnames=yes
if (!file.exists(raw_path)) {
  stop(paste0("Raw file not found: ", raw_path))
}

# Read with readr guessing to detect numeric vs character for DIAGNOSIS_DATE
# First try read_csv with guess
raw_mh_raw <- read_csv(raw_path, show_col_types = FALSE, guess_max = 10000)

# Force names to uppercase to mimic options validvarname=upcase
names(raw_mh_raw) <- toupper(names(raw_mh_raw))

# Ensure expected columns exist (case-insensitive)
# SUBJECT, CONDITION, DIAGNOSIS_DATE
# If DIAGNOSIS_DATE column is numeric (SAS numeric date), handle separately
# SAS logic:
# if vtype(DIAGNOSIS_DATE)='C' then _mhdt = input(strip(DIAGNOSIS_DATE), anydtdte.)
# else _mhdt = DIAGNOSIS_DATE

# Prepare vectors
n <- nrow(raw_mh_raw)
STUDYID_vec <- rep("CDISC-01", n)
DOMAIN_vec <- rep("MH", n)

# USUBJID = catx('-', STUDYID, strip(SUBJECT))
if ("SUBJECT" %in% names(raw_mh_raw)) {
  subj_raw <- raw_mh_raw$SUBJECT
} else {
  subj_raw <- rep(NA_character_, n)
}
# SUBJECT may be numeric; convert to character
subj_char <- as.character(subj_raw)
subj_strip <- sas_strip(subj_char)
USUBJID_vec <- sas_catx("-", STUDYID_vec, subj_strip)
# Truncate to $40 (SAS length)
USUBJID_vec <- substr(USUBJID_vec, 1, 40)
USUBJID_vec[USUBJID_vec == ""] <- NA_character_

# MHTERM = strip(CONDITION)
if ("CONDITION" %in% names(raw_mh_raw)) {
  cond_raw <- raw_mh_raw$CONDITION
} else {
  cond_raw <- rep(NA_character_, n)
}
MHTERM_vec <- sas_strip(as.character(cond_raw))
MHTERM_vec <- substr(MHTERM_vec, 1, 60)

# DIAGNOSIS_DATE handling
if ("DIAGNOSIS_DATE" %in% names(raw_mh_raw)) {
  diag_col <- raw_mh_raw$DIAGNOSIS_DATE
} else {
  diag_col <- rep(NA_character_, n)
}

# Determine vtype: if column is numeric (double/integer) then treat as SAS numeric date
# haven SAS dates are numeric days since 1960-01-01
is_numeric_type <- is.numeric(diag_col) || is.integer(diag_col)

mhdt <- rep(as.Date(NA), n)

if (is_numeric_type) {
  # Numeric SAS date: _mhdt = DIAGNOSIS_DATE (numeric)
  # Convert SAS date (days since 1960-01-01) to R Date
  # SAS missing is NA
  mhdt <- as.Date(diag_col, origin = "1960-01-01")
} else {
  # Character: input(strip(DIAGNOSIS_DATE), anydtdte.)
  diag_char <- as.character(diag_col)
  # SAS strip before input
  diag_strip <- sas_strip(diag_char)
  # Empty -> NA
  diag_strip[diag_strip == ""] <- NA_character_
  # Parse
  mhdt <- parse_anydtdte(diag_strip)
  # Also handle case where character vector contains pure numeric strings that are SAS dates
  # e.g., "16500" - SAS anydtdte would interpret as date? But SAS vtype would be C, so it would try anydtdte.
  # anydtdte on "16500" would be ambiguous; we keep parsed result (likely NA) which matches SAS.
}

# MHSTDTC = put(_mhdt, is8601da.) => YYYY-MM-DD, blank if missing
MHSTDTC_vec <- ifelse(is.na(mhdt), NA_character_, format(mhdt, "%Y-%m-%d"))
MHSTDTC_vec <- substr(MHSTDTC_vec, 1, 10)

# Build initial sdtm.mh (before sort)
sdtm_mh <- tibble(
  STUDYID = substr(STUDYID_vec, 1, 8),
  DOMAIN = substr(DOMAIN_vec, 1, 2),
  USUBJID = USUBJID_vec,
  MHTERM = MHTERM_vec,
  MHSTDTC = MHSTDTC_vec
)

# PROC SORT by USUBJID MHSTDTC (SAS: missing first, uppercase before lowercase)
# Use base order with radix for byte ordering, na.last=FALSE
# Ensure character vectors for ordering (NA stays NA)
ord <- order(sdtm_mh$USUBJID, sdtm_mh$MHSTDTC, na.last = FALSE, method = "radix")
sdtm_mh <- sdtm_mh[ord, , drop = FALSE]

# Second data step: by USUBJID, MHSEQ
# retain STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC
# if first.USUBJID then MHSEQ=1 else MHSEQ+1
sdtm_mh <- sdtm_mh %>%
  group_by(USUBJID) %>%
  mutate(MHSEQ = row_number()) %>%
  ungroup()

# Reorder columns to retain order
sdtm_mh <- sdtm_mh %>%
  select(STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHSTDTC)

# Ensure types: MHSEQ integer
sdtm_mh$MHSEQ <- as.integer(sdtm_mh$MHSEQ)

# Write dataset as CSV
write.csv(sdtm_mh, file.path(out, "mh.csv"), row.names = FALSE, na = "")

cat("P016 mh done: ", nrow(sdtm_mh), " rows written to ", file.path(out, "mh.csv"), "\n", sep = "")
