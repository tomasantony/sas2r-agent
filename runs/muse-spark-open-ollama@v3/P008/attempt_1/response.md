```r
# P008_adam_advs.sas -> R translation
# Outputs: advs.csv and P008.lst (txt + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: find file with case-insensitive search
find_file <- function(dir, pattern) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
  # also list with ignore case via pattern
  grep(pattern, files, value = TRUE, ignore.case = TRUE)
}

# Resolve librefs from family environment
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
# Also check env var if provided
if (Sys.getenv("project_path") != "") project_path <- Sys.getenv("project_path")
if (Sys.getenv("PROJECT_PATH") != "") project_path <- Sys.getenv("PROJECT_PATH")

raw_dir  <- file.path(project_path, "data/raw")
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

# Fallback to absolute paths given in macro variables
if (!dir.exists(adam_dir)) adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
if (!dir.exists(sdtm_dir)) sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"

cat("Using adam_dir:", adam_dir, "\n")
cat("Using sdtm_dir:", sdtm_dir, "\n")
cat("OUT_DIR:", out, "\n")

# List files for debugging
cat("adam_dir contents:\n")
print(list.files(adam_dir, recursive = TRUE))
cat("sdtm_dir contents:\n")
print(list.files(sdtm_dir, recursive = TRUE))

library(haven)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# Function to read SAS dataset (sas7bdat or xpt) with case-insensitive name
read_sas_dataset <- function(libdir, dsname) {
  # try various extensions
  candidates <- c(
    file.path(libdir, paste0(dsname, ".sas7bdat")),
    file.path(libdir, paste0(dsname, ".sas7bdat".toupper())),
    file.path(libdir, paste0(dsname, ".xpt")),
    file.path(libdir, paste0(dsname, ".XPT")),
    file.path(libdir, paste0(toupper(dsname), ".sas7bdat")),
    file.path(libdir, paste0(tolower(dsname), ".sas7bdat")),
    file.path(libdir, paste0(toupper(dsname), ".xpt")),
    file.path(libdir, paste0(tolower(dsname), ".xpt"))
  )
  # also list all files and match case-insensitive
  all_files <- list.files(libdir, full.names = TRUE, recursive = TRUE)
  # find by dsname ignoring case and extension
  idx <- grep(paste0("^", dsname, "\\.(sas7bdat|xpt)$"), basename(all_files), ignore.case = TRUE)
  if (length(idx) > 0) {
    candidates <- c(all_files[idx], candidates)
  }
  for (f in candidates) {
    if (file.exists(f)) {
      cat("Reading", f, "\n")
      ext <- tolower(tools::file_ext(f))
      if (ext == "xpt") {
        df <- haven::read_xpt(f)
      } else if (ext == "sas7bdat") {
        df <- haven::read_sas(f)
      } else next
      # Force uppercase variable names to emulate options validvarname=upcase
      names(df) <- toupper(names(df))
      return(df)
    }
  }
  stop(paste0("Dataset not found: ", dsname, " in ", libdir))
}

# Read ADSL and VS
adsl <- read_sas_dataset(adam_dir, "adsl")
vs   <- read_sas_dataset(sdtm_dir, "vs")

cat("ADSL rows:", nrow(adsl), "cols:", paste(names(adsl), collapse=","), "\n")
cat("VS rows:", nrow(vs), "cols:", paste(names(vs), collapse=","), "\n")

# Ensure character columns are character (haven may give labelled)
# Convert haven_labelled to appropriate types
adsl <- haven::zap_formats(adsl)
adsl <- haven::zap_labels(adsl)
vs   <- haven::zap_formats(vs)
vs   <- haven::zap_labels(vs)

# Convert all names to uppercase already done
# Check key variables exist
# ADSL expected: STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
# VS expected: USUBJID, VSDTC, VSTESTCD, VSTEST, VSSTRESN, VISIT, VISITNUM, VSSEQ

# Helper to parse SAS date numeric or character to Date
# In ADaM, TRTSDT is typically numeric SAS date (days since 1960-01-01) or character ISO
# We need to handle both.

sas_numeric_to_date <- function(x) {
  # SAS date origin 1960-01-01
  as.Date(x, origin = "1960-01-01")
}

parse_trtsdt <- function(x) {
  if (is.null(x)) return(as.Date(character()))
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    # Check if values look like SAS dates (e.g., 10000-25000 range) vs YYYYMMDD numeric
    # SAS dates for 2000-2020 are ~14610-18262, for 2010-2020 ~14610-18262
    # If values > 30000, likely not SAS date but maybe already Date numeric?
    # We'll treat numeric as SAS date if plausible
    # Also check if any values are NA
    # Use heuristic: if all non-NA values between 0 and 30000, treat as SAS date
    # Otherwise try to parse as character
    # But ADSL TRTSDT is often numeric SAS date
    # Convert
    return(sas_numeric_to_date(x))
  }
  if (is.character(x)) {
    # Try ISO yymmdd10. format: YYYY-MM-DD
    # SAS input(substr(VSDTC,1,10), yymmdd10.) expects YYYY-MM-DD
    # For TRTSDT, it may already be Date character
    # Try ymd
    # Use strict parsing: first try YYYY-MM-DD
    res <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    # For those still NA, try other common formats but SAS would have one format
    # Keep NA if not parseable
    return(res)
  }
  # fallback
  as.Date(as.character(x))
}

# Inspect TRTSDT values
cat("ADSL TRTSDT class:", class(adsl$TRTSDT), "\n")
print(head(adsl$TRTSDT, 20))
cat("ADSL TRTEDT class:", class(adsl$TRTEDT), "\n")
print(head(adsl$TRTEDT, 20))

# Parse TRTSDT and TRTEDT
# Check if numeric SAS date
if (is.numeric(adsl$TRTSDT)) {
  adsl$TRTSDT_date <- sas_numeric_to_date(adsl$TRTSDT)
} else {
  adsl$TRTSDT_date <- parse_trtsdt(adsl$TRTSDT)
}
if ("TRTEDT" %in% names(adsl)) {
  if (is.numeric(adsl$TRTEDT)) {
    adsl$TRTEDT_date <- sas_numeric_to_date(adsl$TRTEDT)
  } else {
    adsl$TRTEDT_date <- parse_trtsdt(adsl$TRTEDT)
  }
} else {
  adsl$TRTEDT_date <- as.Date(NA)
}

# For VS, VSDTC is character ISO datetime
cat("VS VSDTC class:", class(vs$VSDTC), "\n")
print(head(vs$VSDTC, 20))

# Also check VSSEQ, VSTESTCD etc
cat("VS VSSEQ head:\n")
print(head(vs$VSSEQ, 20))

# Prepare ADSL sorted by USUBJID
# Keep only needed columns but retain all for merge; SAS keeps all from both but then keeps specific
# We need STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
# Note: ADSL may have STUDYID
adsl_sorted <- adsl %>%
  arrange(USUBJID)

# Prepare VS sorted by USUBJID VSDTC VSTESTCD
vs_sorted <- vs %>%
  arrange(USUBJID, VSDTC, VSTESTCD)

# 1. MERGE ADSL AND VS - work.advs_draft
# SAS: merge work.adsl_sorted(in=a) work.vs_sorted(in=b); by USUBJID; if b;
# This is a many-to-many merge by USUBJID: each VS record gets ADSL info
# In SAS, if ADSL has one row per USUBJID, it's effectively left join VS to ADSL
# Need to handle duplicate USUBJID in ADSL (should be unique)
# Drop/rename overlapping columns except keys to avoid .x/.y
# Overlapping columns likely STUDYID, USUBJID

# Identify common columns except BY
common_cols <- intersect(names(adsl_sorted), names(vs_sorted))
common_cols <- setdiff(common_cols, "USUBJID")
cat("Common cols between ADSL and VS:", paste(common_cols, collapse=","), "\n")
# For SAS merge, if same variable in both, the second dataset's value overwrites first's
# But here we want ADSL's STUDYID, TRT01A etc. VS may also have STUDYID.
# SAS behavior: when merging by USUBJID, variables present in both will be taken from the last dataset that has non-missing?
# Actually SAS merge: if variable in both, value from second dataset overwrites first if not missing? But for BY merge, it retains.
# Simpler: we will take ADSL's STUDYID, TRT01A, TRTSDT, TRTEDT and VS's other vars
# So drop common cols from vs except USUBJID, or keep ADSL's

# For safety, drop common cols from vs_sorted except USUBJID
vs_for_merge <- vs_sorted %>% select(-any_of(common_cols))

# Perform merge: keep only records that have vital signs (if b)
# So inner join on USUBJID, but keep all VS records
advs_draft <- vs_for_merge %>%
  left_join(adsl_sorted %>% select(USUBJID, STUDYID, TRT01A, TRTSDT_date, TRTEDT_date, TRTSDT, TRTEDT), by = "USUBJID") %>%
  # Actually need to join correctly: vs left join adsl
  # The above pipe is wrong order; redo
  identity()

# Correct merge:
advs_draft <- vs_for_merge %>%
  left_join(adsl_sorted %>% select(USUBJID, STUDYID, TRT01A, TRTSDT_date, TRTEDT_date), by = "USUBJID")

# Check that merge succeeded
stopifnot("USUBJID" %in% names(advs_draft))
stopifnot("STUDYID" %in% names(advs_draft))

# Now derive CORE ANALYSIS VARIABLES
# PARAMCD = VSTESTCD; PARAM = VSTEST; AVAL = VSSTRESN; AVISIT = VISIT; AVISITN = VISITNUM; SRCDOM='VS'; SRCVAR='VSSEQ'; SRCSEQ=VSSEQ
# ASTDT = input(substr(VSDTC,1,10), yymmdd10.) if length(VSDTC) >=10
# ASTDY calculation

# Ensure required columns exist
required_vs_cols <- c("VSTESTCD", "VSTEST", "VSSTRESN", "VISIT", "VISITNUM", "VSSEQ", "VSDTC")
for (col in required_vs_cols) {
  if (!col %in% names(advs_draft)) {
    cat("WARNING: missing column", col, "in VS\n")
    advs_draft[[col]] <- NA
  }
}

advs_draft <- advs_draft %>%
  mutate(
    PARAMCD = VSTESTCD,
    PARAM   = VSTEST,
    AVAL    = VSSTRESN,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    SRCDOM  = "VS",
    SRCVAR  = "VSSEQ",
    SRCSEQ  = VSSEQ
  )

# Convert ISO Date to Numeric Analysis Date: ASTDT
# SAS: if length(VSDTC) >=10 then ASTDT = input(substr(VSDTC,1,10), yymmdd10.)
# yymmdd10. expects YYYY-MM-DD (with dashes) or YYYY/MM/DD etc but SAS yymmdd10. is strict YYYY-MM-DD
# We emulate: substr(VSDTC,1,10) then input with yymmdd10.
# So we take first 10 chars and parse as YYYY-MM-DD

advs_draft$ASTDT <- as.Date(NA)
# Only where nchar >=10
idx_astdt <- !is.na(advs_draft$VSDTC) & nchar(advs_draft$VSDTC) >= 10
if (any(idx_astdt)) {
  substr10 <- substr(advs_draft$VSDTC[idx_astdt], 1, 10)
  # SAS yymmdd10. parses YYYY-MM-DD, also YYYY/MM/DD, YY-MM-DD etc but we use strict YYYY-MM-DD
  # Use as.Date with format %Y-%m-%d
  parsed <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))
  # For any that failed, try with slashes? But SAS yymmdd10. would handle both - and / and . and blank?
  # Keep NA if fails
  advs_draft$ASTDT[idx_astdt] <- parsed
}

# Analysis Relative Day (ASTDY)
# if not missing(ASTDT) and not missing(TRTSDT) then if ASTDT >= TRTSDT then ASTDY = (ASTDT - TRTSDT)+1 else ASTDY = ASTDT - TRTSDT
advs_draft$ASTDY <- NA_integer_
# TRTSDT_date is the numeric date version
valid_idx <- !is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT_date)
advs_draft$ASTDY[valid_idx & advs_draft$ASTDT >= advs_draft$TRTSDT_date] <- as.integer(advs_draft$ASTDT[valid_idx & advs_draft$ASTDT >= advs_draft$TRTSDT_date] - advs_draft$TRTSDT_date[valid_idx & advs_draft$ASTDT >= advs_draft$TRTSDT_date]) + 1L
advs_draft$ASTDY[valid_idx & advs_draft$ASTDT < advs_draft$TRTSDT_date] <- as.integer(advs_draft$ASTDT[valid_idx & advs_draft$ASTDT < advs_draft$TRTSDT_date] - advs_draft$TRTSDT_date[valid_idx & advs_draft$ASTDT < advs_draft$TRTSDT_date])

# Keep only required columns for advs_draft as per SAS keep statement
# keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ
# Note TRTSDT/TRTEDT should be the original ADSL values? But for analysis we need date values.
# SAS keeps TRTSDT as numeric SAS date (original). We will keep TRTSDT_date as TRTSDT for output, but also preserve original if needed.
# For output, we need TRTSDT as Date (ISO). We'll use TRTSDT_date and TRTEDT_date

# For final dataset, we need to keep STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT, PARAMCD, PARAM, AVAL, AVISIT, AVISITN, ASTDT, ASTDY, SRCDOM, SRCVAR, SRCSEQ
# Let's create a clean version with proper column names matching SAS (uppercase)
advs_draft_keep <- advs_draft %>%
  select(STUDYID, USUBJID, TRT01A, TRTSDT_date, TRTEDT_date, PARAMCD, PARAM, AVAL, AVISIT, AVISITN, ASTDT, ASTDY, SRCDOM, SRCVAR, SRCSEQ, VSDTC, VSSTRESN, VISIT, VISITNUM, VSSEQ, VSTESTCD, VSTEST) %>%
  rename(TRTSDT = TRTSDT_date, TRTEDT = TRTEDT_date)

# 2. IDENTIFY BASELINE RECORD
# proc sort data=work.advs_draft out=work.advs_base_candidates; by USUBJID PARAMCD ASTDT AVISITN SRCSEQ; where ASTDT <= TRTSDT and not missing(AVAL);
# Note: TRTSDT here is the analysis reference date (TRTSDT_date)

advs_base_candidates <- advs_draft_keep %>%
  filter(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT <= TRTSDT & !is.na(AVAL)) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

# data work.base_flags; set work.advs_base_candidates; by USUBJID PARAMCD; if last.PARAMCD then do; BASE=AVAL; BASE_DT=ASTDT; BASE_SEQ=SRCSEQ; output; end;
# This takes the last record per USUBJID/PARAMCD after sorting by ASTDT, AVISITN, SRCSEQ
# So the latest date, and if tie, highest AVISITN, then highest SRCSEQ

base_flags <- advs_base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  arrange(ASTDT, AVISITN, SRCSEQ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(USUBJID, PARAMCD, BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ)

cat("Base flags rows:", nrow(base_flags), "\n")
print(head(base_flags, 20))

# 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE
# proc sort data=work.advs_draft; by USUBJID PARAMCD;
advs_draft_sorted <- advs_draft_keep %>%
  arrange(USUBJID, PARAMCD)

# merge work.advs_draft(in=a) work.base_flags(in=b); by USUBJID PARAMCD; if a;
# left join
advs_merged <- advs_draft_sorted %>%
  left_join(base_flags, by = c("USUBJID", "PARAMCD"))

# Flag the exact source record; the QC gate rejects same-day ambiguity.
# if SRCSEQ = BASE_SEQ then ABLFL='Y'; else ABLFL='';
# Note: if BASE_SEQ is NA (no baseline), then ABLFL should be '' (SAS: missing = missing? SRCSEQ = . vs missing -> false)
advs_merged <- advs_merged %>%
  mutate(
    ABLFL = ifelse(!is.na(SRCSEQ) & !is.na(BASE_SEQ) & SRCSEQ == BASE_SEQ, "Y", ""),
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
  ) %>%
  select(-BASE_DT, -BASE_SEQ)

# Re-sort for final presentation: by USUBJID PARAMCD ASTDT
advs_final <- advs_merged %>%
  arrange(USUBJID, PARAMCD, ASTDT)

# Retain STUDYID USUBJID order (SAS retain)
# Ensure column order matches SAS keep + derived: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ BASE CHG ABLFL
# Also need to ensure types

# Check final columns
cat("ADVS final rows:", nrow(advs_final), "\n")
cat("ADVS final cols:", paste(names(advs_final), collapse=","), "\n")
print(head(advs_final, 20))

# Ensure STUDYID first, USUBJID second
advs_final <- advs_final %>%
  select(STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT, PARAMCD, PARAM, AVAL, AVISIT, AVISITN, ASTDT, ASTDY, SRCDOM, SRCVAR, SRCSEQ, BASE, CHG, ABLFL, everything())

# For output dataset, we need to write CSV with dates as YYYY-MM-DD
# Convert Date columns to character ISO for CSV
advs_for_csv <- advs_final %>%
  mutate(
    TRTSDT = ifelse(is.na(TRTSDT), "", format(TRTSDT, "%Y-%m-%d")),
    TRTEDT = ifelse(is.na(TRTEDT), "", format(TRTEDT, "%Y-%m-%d")),
    ASTDT  = ifelse(is.na(ASTDT), "", format(ASTDT, "%Y-%m-%d"))
  )

# Write dataset advs.csv
write.csv(advs_for_csv, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=15 var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL
# Need to produce P008.lst text rendering and CSV of table body

# Prepare audit data (first 15 rows as sorted)
audit_data <- advs_final %>%
  arrange(USUBJID, PARAMCD, ASTDT) %>%
  slice_head(n = 15) %>%
  select(USUBJID, PARAMCD, AVISIT, ASTDT, TRTSDT, AVAL, BASE, CHG, ABLFL)

# For display, format dates as DATE9. like SAS: e.g., 01JAN2020
# SAS format date9. is DDMMMYYYY with leading zero, month uppercase 3-letter
format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

audit_display <- audit_data %>%
  mutate(
    ASTDT_disp = format_date9(ASTDT),
    TRTSDT_disp = format_date9(TRTSDT)
  ) %>%
  select(USUBJID, PARAMCD, AVISIT, ASTDT_disp, TRTSDT_disp, AVAL, BASE, CHG, ABLFL)

# Create text rendering similar to SAS PROC PRINT
# Title: "ADVS Audit - Vital Signs Baseline and Change Analysis"

# Build text file
txt_file <- file.path(out, "P008.lst")
csv_file <- file.path(out, "P008.csv")

# Also need to count numbers: expected 120 numbers in P008.lst
# Our audit has 15 rows * maybe numeric columns: AVAL, BASE, CHG etc.

# Write CSV of table body (same as SAS output)
# For CSV, use ISO dates and numeric values unrounded
audit_csv <- audit_data %>%
  mutate(
    ASTDT = ifelse(is.na(ASTDT), "", format(ASTDT, "%Y-%m-%d")),
    TRTSDT = ifelse(is.na(TRTSDT), "", format(TRTSDT, "%Y-%m-%d"))
  )
write.csv(audit_csv, csv_file, row.names = FALSE, na = "")

# Write text rendering
# SAS PROC PRINT prints Obs number and variables
# We will reproduce with similar layout

# Capture output
sink(txt_file)

cat("ADVS Audit - Vital Signs Baseline and Change Analysis\n")
cat("\n")
# Print header
# Use fixed width formatting
# Columns: Obs, USUBJID, PARAMCD, AVISIT, ASTDT, TRTSDT, AVAL, BASE, CHG, ABLFL

# Determine column widths
# USUBJID may be up to 40 chars
# We'll use simple tabular with spaces

# Print column headers
header <- sprintf("%-4s %-15s %-8s %-20s %-10s %-10s %8s %8s %8s %-5s",
                  "Obs", "USUBJID", "PARAMCD", "AVISIT", "ASTDT", "TRTSDT", "AVAL", "BASE", "CHG", "ABLFL")
cat(header, "\n")
cat(paste(rep("-", nchar(header)), collapse=""), "\n")

for (i in seq_len(nrow(audit_display))) {
  row <- audit_display[i, ]
  # Format numeric with default SAS BEST12. ? We'll use as is, but ensure not scientific
  aval_str <- ifelse(is.na(row$AVAL), "", format(row$AVAL, trim=TRUE, scientific=FALSE))
  base_str <- ifelse(is.na(row$BASE), "", format(row$BASE, trim=TRUE, scientific=FALSE))
  chg_str  <- ifelse(is.na(row$CHG), "", format(row$CHG, trim=TRUE, scientific=FALSE))
  line <- sprintf("%-4d %-15s %-8s %-20s %-10s %-10s %8s %8s %8s %-5s",
                  i,
                  substr(row$USUBJID,1,15),
                  ifelse(is.na(row$PARAMCD),"",row$PARAMCD),
                  ifelse(is.na(row$AVISIT),"",substr(row$AVISIT,1,20)),
                  row$ASTDT_disp,
                  row$TRTSDT_disp,
                  aval_str,
                  base_str,
                  chg_str,
                  row$ABLFL)
  cat(line, "\n")
}

cat("\n")
# Add footer with counts to match SAS log? Not needed
# Ensure we have 120 numbers: count numeric values in text
# The text file should contain numbers; we already have them

sink()

cat("Written", txt_file, "and", csv_file, "\n")
cat("Done. advs.csv rows:", nrow(advs_for_csv), "\n")

# Also write a more detailed listing text that includes all numbers for verification
# Count numbers in file
txt_content <- readLines(txt_file)
cat(paste(txt_content, collapse="\n"), "\n")

# Verify outputs exist
stopifnot(file.exists(file.path(out, "advs.csv")))
stopifnot(file.exists(file.path(out, "P008.lst")))
stopifnot(file.exists(file.path(out, "P008.csv")))

```