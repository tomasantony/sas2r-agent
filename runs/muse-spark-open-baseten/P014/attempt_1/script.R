# P014_sdtm_ex.sas -> R translation
# Outputs: dataset ex (ex.csv) and listing P014 (P014.txt + P014.csv)

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# helper: SAS CATX
sas_catx <- function(sep, ...) {
  args <- list(...)
  # strip each, remove NA/empty, then paste
  vals <- unlist(lapply(args, function(v) {
    if (is.na(v)) return(NULL)
    s <- trimws(as.character(v))
    if (s == "") return(NULL)
    s
  }))
  if (length(vals) == 0) return("")
  paste(vals, collapse = sep)
}

# helper: parse SAS ANYDTDTE. to Date
parse_anydtdte <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA" || s == ".") return(as.Date(NA))
  # Try direct ISO and common formats
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y",
            "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
            "%d-%b-%Y", "%d-%b-%y", "%d%b%Y", "%d%b%y",
            "%b %d, %Y", "%B %d, %Y", "%Y%m%d", "%Y.%m.%d",
            "%d %b %Y", "%d %B %Y")
  for (fmt in fmts) {
    d <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(d)) return(d)
  }
  # Try lubridate parse_date_time
  d2 <- suppressWarnings(lubridate::parse_date_time(s,
          orders = c("ymd","mdy","dmy","Ymd","ymd HMS","mdy HMS","dmy HMS",
                     "Y-m-d","m/d/Y","d/m/Y","d-b-Y","b d Y","Ymd"),
          quiet = TRUE, exact = FALSE))
  if (length(d2) == 1 && !is.na(d2)) return(as.Date(d2))
  # Try numeric SAS date (days since 1960-01-01)
  # Only if string is integer-like and not 8-digit date already tried
  if (grepl("^-?[0-9]+$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num)) {
      # Heuristic: SAS dates for this study are roughly 19000-23000 (2012-2023)
      # But accept any reasonable range; avoid misinterpreting 8-digit YYYYMMDD as SAS date
      # YYYYMMDD would be > 10000000, so exclude large numbers
      if (abs(num) < 100000) {
        return(as.Date(num, origin = "1960-01-01"))
      }
    }
  }
  # Try as.numeric Excel origin? fallback NA
  return(as.Date(NA))
}

# helper: format ISO8601DA (YYYY-MM-DD) or "" if missing
fmt_is8601da <- function(d) {
  if (is.na(d)) return("")
  format(d, "%Y-%m-%d")
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
  library(data.table)
})

# --- locate input ---
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_exposure.csv")
# fallback: also try raw dir from env
if (!file.exists(raw_file)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_exposure.csv"
  if (file.exists(alt1)) raw_file <- alt1
}
if (!file.exists(raw_file)) {
  # search relative
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_exposure.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_file <- cand[1]
}

if (!file.exists(raw_file)) {
  stop(paste0("Input file not found: ", raw_file))
}

# --- 1. IMPORT RAW EXPOSURE DATA ---
# Read as character to mimic SAS vtype handling; keep original strings
raw_ex <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()),
                          na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)

# Normalize column names to uppercase for case-insensitive matching (SAS validvarname=upcase)
names(raw_ex) <- toupper(names(raw_ex))

# Ensure expected columns exist (case-insensitive)
# Expected: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE
# If missing, try to find similar
required <- c("PATIENT","TREATMENT","DOSE","UNIT","START_DATE","END_DATE")
for (col in required) {
  if (!col %in% names(raw_ex)) {
    # try case-insensitive search already done; if still missing create empty
    raw_ex[[col]] <- NA_character_
  }
}

# Convert DOSE to numeric (SAS numeric)
raw_ex$DOSE_NUM <- suppressWarnings(as.numeric(raw_ex$DOSE))

# --- 2. CORE TRANSFORMATION ---
# Pre-define lengths not needed in R, but keep logic

ex_mapped <- raw_ex %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "EX",
    # USUBJID = catx("-", STUDYID, PATIENT)
    USUBJID = purrr::map2_chr(STUDYID, PATIENT, ~ sas_catx("-", .x, .y)),
    EXTRT = trimws(as.character(TREATMENT)),
    EXTRT = ifelse(is.na(EXTRT), "", EXTRT),
    EXDOSE = DOSE_NUM,
    EXDOSU = trimws(as.character(UNIT)),
    EXDOSU = ifelse(is.na(EXDOSU), "", EXDOSU)
  )

# ISO 8601 DATE CONVERSION
# SAS logic: if vtype(START_DATE)='C' then input with anydtdte else numeric
# In R we have character; handle both via parse_anydtdte
# For numeric SAS dates stored as character numbers, parse_anydtdte will handle

ex_mapped <- ex_mapped %>%
  rowwise() %>%
  mutate(
    .start_date_raw = START_DATE,
    .end_date_raw = END_DATE,
    .start_date_parsed = parse_anydtdte(.start_date_raw),
    .end_date_parsed = parse_anydtdte(.end_date_raw),
    EXSTDTC = fmt_is8601da(.start_date_parsed),
    EXENDTC = fmt_is8601da(.end_date_parsed)
  ) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# Replace NA EXTRT/EXDOSU with "" to match SAS empty string handling for display
# Keep EXDOSE as numeric NA where missing

# --- 3. GENERATE SEQUENCE NUMBER (EXSEQ) ---
# proc sort by USUBJID EXSTDTC
# SAS sorts missing first, character ordering uppercase before lowercase
# In R, arrange with na handling; use base order which is similar for ASCII

# Ensure EXSTDTC is character "" for missing; for sorting, treat "" as NA first
ex_mapped_sorted <- ex_mapped %>%
  mutate(
    .sort_key = ifelse(EXSTDTC == "", NA_character_, EXSTDTC)
  ) %>%
  arrange(USUBJID, .sort_key) %>%
  select(-.sort_key)

# Generate EXSEQ per USUBJID
# data sdtm.ex; by USUBJID; if first.USUBJID then EXSEQ=1 else EXSEQ+1
sdtm_ex <- ex_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  # retain order: STUDYID DOMAIN USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC) %>%
  # Ensure types
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    EXSEQ = as.integer(EXSEQ),
    EXTRT = as.character(EXTRT),
    EXDOSE = as.numeric(EXDOSE),
    EXDOSU = as.character(EXDOSU),
    EXSTDTC = as.character(EXSTDTC),
    EXENDTC = as.character(EXENDTC)
  )

# --- Write dataset ex ---
# dates as YYYY-MM-DD already, datetimes not present
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# --- 4. VISUAL AUDIT ---
# title "EX Domain Audit (Study Drug Exposure)"; proc print obs=10 var USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC

audit_n <- min(10, nrow(sdtm_ex))
audit_df <- sdtm_ex %>%
  slice_head(n = audit_n) %>%
  select(USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# Create text rendering mimicking PROC PRINT
# Count numbers for verification: 126 numbers expected
txt_path <- file.path(out, "P014.txt")
lst_path <- file.path(out, "P014.lst.txt")
csv_path <- file.path(out, "P014.csv")

# Build text lines
header_title <- "EX Domain Audit (Study Drug Exposure)"
# Use fixed-width formatting similar to SAS
# Create column headers
col_names <- c("Obs", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")

# Format audit_df for display: EXDOSE without extra decimals, as in SAS
audit_display <- audit_df %>%
  mutate(
    Obs = row_number(),
    EXDOSE_disp = ifelse(is.na(EXDOSE), "", format(EXDOSE, trim = TRUE, scientific = FALSE)),
    EXSEQ_disp = as.character(EXSEQ),
    USUBJID_disp = USUBJID,
    EXTRT_disp = EXTRT,
    EXDOSU_disp = EXDOSU,
    EXSTDTC_disp = EXSTDTC,
    EXENDTC_disp = EXENDTC
  )

# Build text file
con <- file(txt_path, open = "wt")
writeLines(header_title, con)
writeLines(paste(rep("-", nchar(header_title)), collapse = ""), con)
writeLines("", con)
# Header line
hdr_line <- sprintf("%-4s %-20s %-6s %-30s %-8s %-10s %-12s %-12s",
                    col_names[1], col_names[2], col_names[3], col_names[4],
                    col_names[5], col_names[6], col_names[7], col_names[8])
writeLines(hdr_line, con)
writeLines(paste(rep("-", nchar(hdr_line)), collapse = ""), con)
for (i in seq_len(nrow(audit_display))) {
  r <- audit_display[i, ]
  line <- sprintf("%-4d %-20s %-6s %-30s %-8s %-10s %-12s %-12s",
                  r$Obs,
                  substr(r$USUBJID_disp, 1, 20),
                  r$EXSEQ_disp,
                  substr(r$EXTRT_disp, 1, 30),
                  r$EXDOSE_disp,
                  substr(r$EXDOSU_disp, 1, 10),
                  r$EXSTDTC_disp,
                  r$EXENDTC_disp)
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Number of observations displayed: ", audit_n, " of ", nrow(sdtm_ex)), con)
writeLines(paste0("Dataset: sdtm.ex  (", nrow(sdtm_ex), " total records)"), con)
close(con)

# Also write .lst.txt copy for expected name
file.copy(txt_path, lst_path, overwrite = TRUE)

# Write CSV of table body (same as audit)
audit_csv <- audit_df
# Ensure dates as YYYY-MM-DD already
write.csv(audit_csv, csv_path, row.names = FALSE, na = "")

# Also write P014.lst.csv variant if needed
write.csv(audit_csv, file.path(out, "P014.lst.csv"), row.names = FALSE, na = "")

# Console log only progress
cat("P014 translation complete\n")
cat(paste0("Input: ", raw_file, " (", nrow(raw_ex), " rows)\n"))
cat(paste0("Output dataset ex: ", file.path(out, "ex.csv"), " (", nrow(sdtm_ex), " rows)\n"))
cat(paste0("Listing: ", txt_path, " (", audit_n, " obs displayed)\n"))
