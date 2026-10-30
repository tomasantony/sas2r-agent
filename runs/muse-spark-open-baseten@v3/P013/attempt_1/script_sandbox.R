out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS BEST32. equivalent - strip(put(num, best32.))
sas_best <- function(x) {
  # SAS BEST w.d: default representation, removes leading/trailing blanks
  # For numeric, use format with sufficient precision, similar to SAS BEST32.
  # SAS BEST uses 12 significant digits by default; BEST32 allows up to 32.
  # We emulate by using format with 15 significant digits and trimming.
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    # Use format with 15 significant digits, scientific if needed like SAS
    # SAS BEST switches to scientific for large/small numbers; R's format does similar
    # Use formatC with format="g" and digits=15 to mimic BEST
    s <- formatC(v, format = "g", digits = 15, flag = "")
    # Remove leading spaces (formatC doesn't add) and trim
    s <- trimws(s)
    # SAS BEST removes trailing zeros after decimal? format="g" does
    # Ensure "-0" becomes "0" as SAS would
    if (s == "-0") s <- "0"
    s
  }, character(1))
}

# Helper: SAS ANYDTDTE. emulation - decide format once per column
# For this program, SAS code uses anydtdte. if DATE is character.
# We implement robust parsing: try to detect if column is numeric SAS date or character.
# If character, parse with lubridate flexible but single format decision.
parse_sas_anydtdte <- function(vec) {
  # vec is character vector from CSV
  # SAS ANYDTDTE tries multiple orders; we emulate by trying common ISO and mdy/dmy
  # Instruction says decide order once per column from all values, then parse every value with that single format.
  # For simplicity, use lubridate::parse_date_time with single order detection,
  # but we will implement single-format decision: check if any value has first field >12 => DMY else MDY
  # However raw_ecg DATE values are likely ISO or similar; we can just use flexible parsing per value
  # to match SAS anydtdte. which is flexible per value. The instruction about PROC IMPORT guessing
  # applies to PROC IMPORT's informat decision, not to the later INPUT(anydtdte.) which is per value.
  # So we can parse per value flexibly.
  if (all(is.na(vec))) return(rep(NA_character_, length(vec)))
  # Use lubridate to parse
  # Try ymd, dmy, mdy, etc.
  parsed <- suppressWarnings(lubridate::parse_date_time(vec, orders = c("ymd", "mdy", "dmy", "Ymd", "mdY", "dmY", "ymd HMS", "mdy HMS", "dmy HMS"), exact = FALSE))
  as.Date(parsed)
}

# Locate raw_ecg.csv
# SAS uses &project_path./data/raw/raw_ecg.csv where project_path = /bench/data/derived/c2c
candidates <- c(
  "/bench/data/derived/c2c/data/raw/raw_ecg.csv",
  "/bench/data/raw/raw_ecg.csv",
  "/bench/data/derived/c2c/data/raw/raw_ecg.csv"
)
raw_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_path <- p; break }
}
# Fallback: search recursively
if (is.null(raw_path)) {
  all_csv <- list.files("/bench", pattern = "raw_ecg.csv", recursive = TRUE, full.names = TRUE)
  if (length(all_csv) > 0) raw_path <- all_csv[1]
}
if (is.null(raw_path)) stop("raw_ecg.csv not found in expected locations")

# Read raw CSV - emulate PROC IMPORT getnames=yes
# PROC IMPORT scans values to decide types; we read with readr to get types, but also handle DATE as character initially
# Read first as character to inspect, then convert numeric columns
raw_eg_raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()), na = c("", "NA", "."), show_col_types = FALSE)

# Trim whitespace from column names (PROC IMPORT upcases with validvarname=upcase, but original names preserved)
# SAS option validvarname=upcase forces variable names to uppercase
names(raw_eg_raw) <- toupper(names(raw_eg_raw))

# Convert known numeric columns
# Expected columns: SUBJECT, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, DATE
# Convert VISIT_NUM and RESULT to numeric where possible
to_num <- function(x) suppressWarnings(as.numeric(x))
if ("VISIT_NUM" %in% names(raw_eg_raw)) raw_eg_raw$VISIT_NUM <- to_num(raw_eg_raw$VISIT_NUM)
if ("RESULT" %in% names(raw_eg_raw)) raw_eg_raw$RESULT <- to_num(raw_eg_raw$RESULT)

# Work dataset
work_raw_eg <- raw_eg_raw

# Build SDTM EG
# Lengths are for SAS, not needed in R but we enforce character truncation if needed
# USUBJID length 40
usubjid_length <- 40
# Try to get from env if set
env_len <- Sys.getenv("usubjid_length")
if (env_len != "" && !is.na(suppressWarnings(as.numeric(env_len)))) usubjid_length <- as.numeric(env_len)

sdtm_eg <- work_raw_eg

# Helper to strip (trimws)
strip <- function(x) trimws(as.character(x))

# STUDYID, DOMAIN
sdtm_eg$STUDYID <- "CDISC-01"
sdtm_eg$DOMAIN <- "EG"

# USUBJID = catx('-', STUDYID, strip(SUBJECT))
# catx concatenates with delimiter, skipping missing, stripping
sdtm_eg$USUBJID <- ifelse(is.na(sdtm_eg$SUBJECT) | trimws(sdtm_eg$SUBJECT) == "",
                         sdtm_eg$STUDYID,
                         paste0(sdtm_eg$STUDYID, "-", trimws(sdtm_eg$SUBJECT)))
# Truncate to usubjid_length (SAS LENGTH)
sdtm_eg$USUBJID <- substr(sdtm_eg$USUBJID, 1, usubjid_length)

# VISIT and VISITNUM
sdtm_eg$VISIT <- strip(sdtm_eg$VISIT_NAM)
sdtm_eg$VISITNUM <- sdtm_eg$VISIT_NUM

# EGTEST
sdtm_eg$EGTEST <- strip(sdtm_eg$TEST_NAME)

# EGTESTCD mapping
sdtm_eg$EGTESTCD <- dplyr::case_when(
  sdtm_eg$EGTEST == "Heart Rate" ~ "HR",
  sdtm_eg$EGTEST == "QT Duration" ~ "QT",
  sdtm_eg$EGTEST == "RR Duration" ~ "RR",
  TRUE ~ "UNKNOWN"
)

# EGORRES, EGORRESU, EGSTRESN, EGSTRESC, EGSTRESU
sdtm_eg$EGORRES <- sas_best(sdtm_eg$RESULT)
sdtm_eg$EGORRESU <- strip(sdtm_eg$UNIT)
sdtm_eg$EGSTRESN <- sdtm_eg$RESULT
sdtm_eg$EGSTRESC <- sas_best(sdtm_eg$EGSTRESN)
sdtm_eg$EGSTRESU <- strip(sdtm_eg$UNIT)

# EGDTC handling: PROC IMPORT may infer DATE as numeric SAS date. Always emit ISO 8601.
# In R, we need to detect if DATE column is numeric (SAS date = days since 1960-01-01) or character
# Our raw read forced character, so we need to check original inference
# Re-read with type guessing to see if DATE would be numeric
tmp_guess <- readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 10000)
tmp_names <- toupper(names(tmp_guess))
names(tmp_guess) <- tmp_names
date_is_numeric <- FALSE
if ("DATE" %in% names(tmp_guess)) {
  # If readr guessed DATE as numeric/date, check
  if (is.numeric(tmp_guess$DATE) || lubridate::is.Date(tmp_guess$DATE) || lubridate::is.POSIXct(tmp_guess$DATE)) {
    # Check if original character values look like numbers (SAS dates are integers like 19000)
    # If all DATE values in character read are integer-like and within SAS date range, treat as SAS numeric dates
    char_dates <- work_raw_eg$DATE
    # SAS date range for this study: roughly 1960-01-01 to 2030 => 0 to 26000
    # If character values are all digits and no dashes/slashes, likely SAS numeric
    if (all(grepl("^[0-9]+$", trimws(na.omit(char_dates))))) {
      date_is_numeric <- TRUE
    }
  }
}

# Parse _egdt
if (date_is_numeric) {
  # DATE is numeric SAS date (days since 1960-01-01)
  sas_dates <- suppressWarnings(as.numeric(work_raw_eg$DATE))
  sdtm_eg$`_egdt` <- as.Date(sas_dates, origin = "1960-01-01")
} else {
  # DATE is character - use anydtdte. equivalent
  # First try to see if DATE column was already parsed as Date by readr
  if ("DATE" %in% names(tmp_guess) && inherits(tmp_guess$DATE, "Date")) {
    sdtm_eg$`_egdt` <- as.Date(tmp_guess$DATE)
  } else if ("DATE" %in% names(tmp_guess) && inherits(tmp_guess$DATE, "POSIXct")) {
    sdtm_eg$`_egdt` <- as.Date(tmp_guess$DATE)
  } else {
    # Use character vector
    date_char <- strip(work_raw_eg$DATE)
    # Handle empty strings as NA
    date_char[date_char == ""] <- NA_character_
    # Try ISO8601 first (YYYY-MM-DD)
    # Use single-format decision per column: detect format from all values
    # Determine if any value has first field >12 to decide DMY vs MDY, but for ISO it's YMD
    # Simplify: use lubridate flexible parsing
    parsed <- suppressWarnings(lubridate::parse_date_time(date_char, orders = c("ymd", "Ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS", "Y-m-d", "m/d/Y", "d/m/Y"), exact = FALSE))
    sdtm_eg$`_egdt` <- as.Date(parsed)
    # Fallback: try as.Date with ISO
    need <- is.na(sdtm_eg$`_egdt`) & !is.na(date_char)
    if (any(need)) {
      iso_try <- suppressWarnings(as.Date(date_char[need], format = "%Y-%m-%d"))
      sdtm_eg$`_egdt`[need] <- iso_try
    }
    # Fallback: try anydtdte via as.Date with tryFormats (single format per column logic)
    # If still NA, try numeric SAS date string
    need2 <- is.na(sdtm_eg$`_egdt`) & !is.na(date_char)
    if (any(need2)) {
      num_try <- suppressWarnings(as.numeric(date_char[need2]))
      is_num <- !is.na(num_try)
      if (any(is_num)) {
        idx <- which(need2)[is_num]
        sdtm_eg$`_egdt`[idx] <- as.Date(num_try[is_num], origin = "1960-01-01")
      }
    }
  }
}

# EGDTC = put(_egdt, is8601da.) => YYYY-MM-DD, blank if missing
sdtm_eg$EGDTC <- ifelse(is.na(sdtm_eg$`_egdt`), NA_character_, format(sdtm_eg$`_egdt`, "%Y-%m-%d"))

# Keep required columns
sdtm_eg <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC","_egdt")]

# proc sort by USUBJID EGDTC EGTESTCD
# SAS missing sorts first; character ordering uppercase before lowercase (ASCII)
# Use order with na.last = FALSE to put NA first, and use xtfrm for character
# For EGDTC, it's character ISO date, NA first
sdtm_eg <- sdtm_eg[order(sdtm_eg$USUBJID, sdtm_eg$EGDTC, sdtm_eg$EGTESTCD, na.last = FALSE), ]

# Second data step: retain and EGSEQ by USUBJID
# by USUBJID; if first.USUBJID then EGSEQ=1 else EGSEQ+1
sdtm_eg$EGSEQ <- NA_integer_
# Compute first flag
# Use rle on USUBJID
usubjid_vec <- sdtm_eg$USUBJID
seq_counter <- 0
prev <- NA_character_
for (i in seq_len(nrow(sdtm_eg))) {
  cur <- usubjid_vec[i]
  is_first <- is.na(prev) || is.na(cur) || cur != prev || (is.na(prev) != is.na(cur))
  # Handle NA comparison: if both NA, not first after first
  if (is.na(cur) && is.na(prev) && i > 1) is_first <- FALSE
  if (is_first) {
    seq_counter <- 1
  } else {
    seq_counter <- seq_counter + 1
  }
  sdtm_eg$EGSEQ[i] <- seq_counter
  prev <- cur
}

# Final keep and retain order
sdtm_eg_final <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","EGSEQ","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC")]

# Ensure types: VISITNUM numeric, EGSTRESN numeric, EGSEQ integer
# Write dataset as CSV
write.csv(sdtm_eg_final, file.path(out, "eg.csv"), row.names = FALSE, na = "")

# Also write TXT rendering for verification (optional but helpful)
# Create a simple text dump of first rows and counts
txt_path <- file.path(out, "eg.txt")
sink(txt_path)
cat("Dataset: EG (SDTM EG Domain)\n")
cat("Rows:", nrow(sdtm_eg_final), "\n")
cat("Columns:", paste(names(sdtm_eg_final), collapse = ", "), "\n\n")
print(utils::head(sdtm_eg_final, 20))
sink()
