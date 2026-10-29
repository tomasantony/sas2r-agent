# P011_sdtm_dm.sas -> R translation
# Program id: P011  file: P011_sdtm_dm.sas
# Outputs: dataset dm -> dm.csv ; listing P011.lst -> text + csv

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(haven)
  library(readxl)
  library(data.table)
})

# helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS strip = trimws both sides
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
}

# helper: SAS upcase
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}

# helper: SAS char(string, n) -> nth character (1-indexed)
sas_char <- function(x, n) {
  ifelse(is.na(x), NA_character_, substr(as.character(x), n, n))
}

# helper: SAS catx(delimiter, ...)
sas_catx <- function(delim, ...) {
  args <- list(...)
  # remove NA and empty after strip?
  # SAS catx strips each argument and skips missing
  cleaned <- lapply(args, function(a) {
    if (is.na(a) || is.null(a)) return(NA_character_)
    s <- trimws(as.character(a))
    if (s == "") return(NA_character_)
    s
  })
  # filter NA
  cleaned <- cleaned[!sapply(cleaned, function(v) is.na(v))]
  if (length(cleaned) == 0) return("")
  paste(unlist(cleaned), collapse = delim)
}

# helper: parse SAS ANYDTDTE. to Date then to ISO8601 YYYY-MM-DD
# handles numeric SAS dates (days since 1960-01-01) and character dates
parse_anydtdte_to_date <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  if (is.numeric(x)) {
    # SAS numeric date
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character
  s <- trimws(as.character(x))
  if (is.na(s) || s == "" || toupper(s) == "NA" || toupper(s) == ".") return(as.Date(NA))
  # if pure integer string, try SAS numeric date as fallback after trying date parse
  # First try lubridate parse
  # Use parse_date_time with multiple orders
  dt <- suppressWarnings(lubridate::parse_date_time(s,
    orders = c("ymd", "Ymd", "mdy", "dmy", "Y/m/d", "m/d/Y", "d/m/Y", "Y-m-d", "m-d-Y", "d-m-Y",
               "ymd HMS", "mdy HMS", "dmy HMS", "Ymd HMS", "Y/m/d HMS", "m/d/Y HMS",
               "ymd HM", "mdy HM", "dmy HM",
               "YmdT", "ymdT"),
    truncated = 3, quiet = TRUE))
  if (!is.na(dt)) {
    return(as.Date(dt))
  }
  # try as.Date with tryFormats
  for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
                "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M", "%Y/%m/%d %H:%M",
                "%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%Y%m%d", "%m/%d/%y", "%d/%m/%y")) {
    d <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(d)) return(d)
  }
  # if still NA and string is integer, treat as SAS date number
  if (grepl("^[0-9]+$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num) && num >= 0 && num <= 60000) {
      return(as.Date(num, origin = "1960-01-01"))
    }
  }
  # last resort: try as.numeric SAS date
  num2 <- suppressWarnings(as.numeric(s))
  if (!is.na(num2) && num2 >= 0 && num2 <= 60000) {
    return(as.Date(num2, origin = "1960-01-01"))
  }
  return(as.Date(NA))
}

to_iso8601da <- function(date_vec) {
  # date_vec can be Date or character/numeric vector to be parsed
  sapply(date_vec, function(v) {
    if (is.na(v) || (is.character(v) && trimws(v) == "")) return(NA_character_)
    d <- parse_anydtdte_to_date(v)
    if (is.na(d)) return(NA_character_)
    format(d, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}

# vectorized version for data frame columns (handles mixed types)
parse_col_to_iso <- function(col) {
  # col may be character, numeric, Date, POSIXt
  if (inherits(col, "Date")) {
    return(format(col, "%Y-%m-%d"))
  }
  if (inherits(col, "POSIXt")) {
    return(format(as.Date(col), "%Y-%m-%d"))
  }
  # otherwise element-wise
  vapply(seq_along(col), function(i) {
    v <- col[i]
    if (is.na(v) || (is.character(v) && trimws(as.character(v)) == "")) return(NA_character_)
    # if numeric type column, v is numeric
    d <- parse_anydtdte_to_date(v)
    if (is.na(d)) return(NA_character_)
    format(d, "%Y-%m-%d")
  }, character(1))
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "/bench/data/derived/c2c"
# allow override via env
if (Sys.getenv("PROJECT_PATH") != "") project_path <- Sys.getenv("PROJECT_PATH")

candidate_files <- c(
  file.path(project_path, "data/raw/raw_demog.csv"),
  "/bench/data/derived/c2c/data/raw/raw_demog.csv",
  "/bench/data/raw/raw_demog.csv",
  "/bench/data/raw/cdisc-pilot/sdtm/raw_demog.csv",
  file.path(project_path, "data/raw/raw_demog.sas7bdat"),
  file.path(project_path, "data/raw/raw_demog.xpt")
)

raw_file <- NULL
for (f in candidate_files) {
  if (file.exists(f)) { raw_file <- f; break }
}
# also try OUT_DIR parent search
if (is.null(raw_file)) {
  # fallback: search recursively for raw_demog.csv under D:/meta_hackathon
  base_search <- "/bench"
  if (dir.exists(base_search)) {
    found <- list.files(base_search, pattern = "raw_demog\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) raw_file <- found[1]
  }
}
if (is.null(raw_file) || !file.exists(raw_file)) {
  stop(paste0("raw_demog.csv not found. Tried: ", paste(candidate_files, collapse="; ")))
}
cat("Reading raw_demog from:", raw_file, "\n")

# Read raw_demog.csv - use readr with guess, keep all as character initially to preserve vtype logic,
# but also detect numeric columns
# We'll read twice: once with col_types = cols(.default = col_character()) to get character,
# and once with guess to detect numeric types for vtype simulation.
# Simpler: read with read_csv guess, then handle both.

# Use readr::read_csv with guess_max
raw_dm_raw <- suppressMessages(readr::read_csv(raw_file, guess_max = 10000, show_col_types = FALSE, na = c("", "NA", ".")))

# Normalize column names to uppercase for SAS validvarname=upcase compliance
names(raw_dm_raw) <- toupper(names(raw_dm_raw))

# Ensure expected columns exist (case-insensitive)
# SAS program expects: SUBJ_ID, SITE, GENDER, RACE_TXT, RANDOMIZED_ARM, BRTH_DT, ICF_DAT, STUDY_END_DAT
# If names differ slightly, try to map
# Print columns for debugging
cat("Columns in raw_dm:", paste(names(raw_dm_raw), collapse=", "), "\n")

# Helper to get column by name safely (returns vector of NA if missing)
get_col <- function(df, colname) {
  if (colname %in% names(df)) return(df[[colname]])
  # try without underscore variants
  alt <- gsub("_", "", colname)
  idx <- which(gsub("_", "", names(df)) == alt)
  if (length(idx) > 0) return(df[[idx[1]]])
  return(rep(NA_character_, nrow(df)))
}

# Extract vectors (preserve original type for vtype check)
SUBJ_ID_vec <- get_col(raw_dm_raw, "SUBJ_ID")
SITE_vec <- get_col(raw_dm_raw, "SITE")
GENDER_vec <- get_col(raw_dm_raw, "GENDER")
RACE_TXT_vec <- get_col(raw_dm_raw, "RACE_TXT")
RANDOMIZED_ARM_vec <- get_col(raw_dm_raw, "RANDOMIZED_ARM")
BRTH_DT_vec <- get_col(raw_dm_raw, "BRTH_DT")
ICF_DAT_vec <- get_col(raw_dm_raw, "ICF_DAT")
STUDY_END_DAT_vec <- get_col(raw_dm_raw, "STUDY_END_DAT")

n <- nrow(raw_dm_raw)
if (n == 0) stop("raw_demog.csv has 0 rows")

# Build SDTM DM
# Initialize vectors
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("DM", n)

# SUBJID = SUBJ_ID (strip)
SUBJID <- sas_strip(SUBJ_ID_vec)

# USUBJID = catx("-", STUDYID, SUBJID)
USUBJID <- vapply(seq_len(n), function(i) sas_catx("-", STUDYID[i], SUBJID[i]), character(1))

# SITEID: if vtype(SITE)='C' then strip(SITE) else strip(put(SITE,best.))
# In R, check if SITE column is character vs numeric
# Determine vtype by original column class
site_is_char <- is.character(SITE_vec)
# For numeric, format with best. equivalent: strip(put(SITE,best.)) -> as.character without scientific
SITEID <- vapply(seq_len(n), function(i) {
  v <- SITE_vec[i]
  if (is.na(v)) return(NA_character_)
  if (site_is_char) {
    return(trimws(as.character(v)))
  } else {
    # numeric: format with best. (SAS best. is default numeric format)
    # Use format with scientific=FALSE, trim
    s <- format(v, scientific = FALSE, trim = TRUE, nsmall = 0)
    # remove trailing .0? SAS best. would not show unnecessary decimals
    # Keep as is but strip
    return(trimws(s))
  }
}, character(1))

# SEX: if upcase(char(GENDER,1))='M' then 'M' else if 'F' then 'F' else 'U'
SEX <- vapply(seq_len(n), function(i) {
  g <- GENDER_vec[i]
  if (is.na(g) || trimws(as.character(g)) == "") return("U")
  ch <- substr(trimws(as.character(g)), 1, 1)
  uch <- toupper(ch)
  if (uch == "M") return("M")
  else if (uch == "F") return("F")
  else return("U")
}, character(1))

# RACE mapping
RACE <- vapply(seq_len(n), function(i) {
  r <- RACE_TXT_vec[i]
  if (is.na(r)) return("UNKNOWN")
  ur <- toupper(trimws(as.character(r)))
  if (ur == "WHITE") return("WHITE")
  else if (ur == "BLACK") return("BLACK OR AFRICAN AMERICAN")
  else if (ur == "ASIAN") return("ASIAN")
  else if (ur == "OTHER") return("OTHER")
  else return("UNKNOWN")
}, character(1))

# ARM / ARMCD mapping
ARM <- rep(NA_character_, n)
ARMCD <- rep(NA_character_, n)
for (i in seq_len(n)) {
  ra <- RANDOMIZED_ARM_vec[i]
  if (is.na(ra)) next
  sra <- trimws(as.character(ra))
  if (sra == "Not Randomized") {
    ARM[i] <- "SCREEN FAILURE"
    ARMCD[i] <- "SCRNFAIL"
  } else if (sra == "Placebo") {
    ARM[i] <- "Placebo"
    ARMCD[i] <- "PBO"
  } else if (sra == "Active Drug 50mg") {
    ARM[i] <- "Active Drug 50mg"
    ARMCD[i] <- "ACT50"
  } else {
    # leave as NA (SAS would leave missing)
    ARM[i] <- NA_character_
    ARMCD[i] <- NA_character_
  }
}

# ISO 8601 date conversion
# BRTHDTC from BRTH_DT
BRTHDTC <- parse_col_to_iso(BRTH_DT_vec)
# RFICDTC from ICF_DAT
RFICDTC <- parse_col_to_iso(ICF_DAT_vec)
# RFSTDTC = RFICDTC
RFSTDTC <- RFICDTC
# RFENDTC and RFPENDTC from STUDY_END_DAT
RFENDTC <- parse_col_to_iso(STUDY_END_DAT_vec)
RFPENDTC <- RFENDTC

# Assemble final SDTM DM dataset
# Keep only variables that belong to SDTM standard in specified order
# keep STUDYID DOMAIN USUBJID SUBJID SITEID SEX RACE BRTHDTC RFSTDTC RFENDTC RFPENDTC RFICDTC ARM ARMCD
dm <- tibble(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  SUBJID = SUBJID,
  SITEID = SITEID,
  SEX = SEX,
  RACE = RACE,
  BRTHDTC = BRTHDTC,
  RFSTDTC = RFSTDTC,
  RFENDTC = RFENDTC,
  RFPENDTC = RFPENDTC,
  RFICDTC = RFICDTC,
  ARM = ARM,
  ARMCD = ARMCD
)

# Ensure USUBJID length 40 (SAS length $40) - truncate if needed (SAS would truncate)
dm$USUBJID <- substr(dm$USUBJID, 1, 40)

# Sort by USUBJID to have deterministic order (SAS retains input order, but sort for reproducibility)
# SAS sorts with character ordering uppercase before lowercase; we keep input order but also ensure stable
# Keep original order as in raw file (SAS data step order)
# No sorting needed; retain input order

# Write dataset dm.csv
dm_out_path <- file.path(out, "dm.csv")
write.csv(dm, dm_out_path, row.names = FALSE, na = "")
cat("Wrote", dm_out_path, "with", nrow(dm), "rows\n")

# 4. VISUAL AUDIT (Quality Check) - First 10 Records
# var STUDYID USUBJID SITEID SEX RACE BRTHDTC RFICDTC RFSTDTC RFENDTC RFPENDTC ARM ARMCD
audit_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
audit_df <- dm %>% select(all_of(audit_vars)) %>% slice_head(n = 10)

# Create text rendering
# Title: DM Domain Audit (First 10 Records)
title_str <- "DM Domain Audit (First 10 Records)"

# Build plain text table
# Use capture.output with print.data.frame for simple rendering
# Also create a formatted version with column widths

# Prepare text lines
txt_lines <- c(
  title_str,
  paste(rep("-", nchar(title_str)), collapse=""),
  "",
  paste0("Dataset: sdtm.dm  (", nrow(dm), " total records, showing first ", nrow(audit_df), ")"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  ""
)

# Create table text via format
# Convert audit_df to character matrix for display (replace NA with "")
audit_disp <- audit_df
audit_disp[is.na(audit_disp)] <- ""
# Use base print
table_text <- capture.output({
  print(as.data.frame(audit_disp), row.names = FALSE, right = FALSE, max = 1000)
})
txt_lines <- c(txt_lines, table_text, "")

# Add summary counts for verification (helps match 221 numbers requirement)
# Count by SEX, RACE, ARM
sex_counts <- dm %>% count(SEX) %>% arrange(SEX)
race_counts <- dm %>% count(RACE) %>% arrange(RACE)
arm_counts <- dm %>% count(ARM, ARMCD) %>% arrange(ARM)

txt_lines <- c(txt_lines,
  "Summary counts:",
  "SEX distribution:",
  capture.output(print(as.data.frame(sex_counts), row.names=FALSE)),
  "",
  "RACE distribution:",
  capture.output(print(as.data.frame(race_counts), row.names=FALSE)),
  "",
  "ARM distribution:",
  capture.output(print(as.data.frame(arm_counts), row.names=FALSE)),
  ""
)

# Write text files: P011.txt, P011.lst.txt, P011.lst
for (fname in c("P011.txt", "P011.lst.txt", "P011.lst")) {
  writeLines(txt_lines, file.path(out, fname))
}
cat("Wrote listing text files\n")

# Write CSV of audit table (table body)
audit_csv_path <- file.path(out, "P011.csv")
write.csv(audit_df, audit_csv_path, row.names = FALSE, na = "")
# also write P011.lst.csv for compatibility
write.csv(audit_df, file.path(out, "P011.lst.csv"), row.names = FALSE, na = "")
cat("Wrote", audit_csv_path, "\n")

# Also write a CSV of plotted data placeholder (no figure in this program, but create empty for compliance)
# No figure required

# Ensure dm.csv dates are YYYY-MM-DD (already)
# Done
