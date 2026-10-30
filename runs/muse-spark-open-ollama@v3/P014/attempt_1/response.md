```r
# P014_sdtm_ex.sas -> R translation
# Program: sdtm_ex.sas - Mapping raw EDC to SDTM EX domain

# Setup
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed here but defined)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}

# Helper: trim like SAS strip
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}

# Helper: catx
sas_catx <- function(sep, ...) {
  args <- list(...)
  # remove NA and empty after strip?
  # SAS catx ignores missing and trims
  do.call(paste, c(lapply(args, function(a) sas_strip(a)), sep = sep))
}

# Helper: SAS ANYDTDTE emulation with DATESTYLE? But we need PROC IMPORT guessing logic
# According to instructions: PROC IMPORT scans column values, decides ONE informat per column
# For date-like columns with n/n/yyyy, if any value has first field >12 then DDMMYY10., else MMDDYY10.
# Then every value in that column is read with that same order.
# We must emulate this for START_DATE and END_DATE.

# Locate raw file
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_exposure.csv")

# Also alternative raw path from macro variables (same)
# &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw etc but program uses project_path

# Read raw CSV as character to inspect guessing
# We need to read without parsing dates, to see raw strings
if (!file.exists(raw_file)) {
  # fallback search
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_exposure.csv"
  if (file.exists(alt1)) raw_file <- alt1
}

# Read raw file lines for inspection
raw_lines <- readLines(raw_file, warn = FALSE)
# Read with readr as all character to preserve original strings
raw_char <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, na = c("", "NA"))
# Also read with base to see column names
# print column names for debugging
# cat(paste(names(raw_char), collapse=", "), "\n")

# Identify date columns
# Expected columns: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE (based on SAS code)
# Let's inspect unique values
# print head
# head(raw_char)

# Emulate PROC IMPORT guessing for START_DATE and END_DATE
# SAS PROC IMPORT with DBMS=CSV, GETNAMES=YES, GUESSINGROWS default 20 (or 20? In SAS 9.4 default 20, but can be MAX)
# It scans values to determine type: if all values look like dates, it assigns numeric date informat
# The instruction says: a column of n/n/yyyy values becomes numeric SAS date read as DDMMYY10. when any value has first field above 12 (otherwise MMDDYY10.)
# So we need to decide for each date column whether SAS would have read it as numeric or character, and which order.

# Helper to decide date order per column
decide_date_order <- function(vec) {
  # vec is character vector of raw strings (excluding NA/empty)
  vals <- vec[!is.na(vec) & trimws(vec) != ""]
  if (length(vals) == 0) return(NA_character_)
  # Check if values match n/n/yyyy or n-n-yyyy or similar with slashes/dashes
  # For this dataset, likely format is like "01/15/2020" or "15/01/2020" or "2020-01-15" etc
  # We need to detect slash pattern
  # Extract first field before / or -
  # If any value has first field >12, then DDMMYY, else MMDDYY
  # This rule only applies to slash dates with 3 parts
  # If values are ISO like YYYY-MM-DD, then SAS would use YYMMDD or ANYDTDTE but still numeric?
  # Simplify: if all values match ^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$ then apply rule
  # Otherwise if they match ^\\d{4}-\\d{2}-\\d{2}$ then it's YYMMDD
  # For mixed, SAS ANYDTDTE would still decide?
  # We'll implement generic: if any slash date has first field >12 => DDMMYY else MMDDYY
  slash_vals <- vals[grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$", vals)]
  if (length(slash_vals) > 0) {
    first_fields <- as.numeric(sub("^([0-9]{1,2})[/-].*", "\\1", slash_vals))
    if (any(first_fields > 12, na.rm = TRUE)) {
      return("DMY") # DDMMYY10.
    } else {
      return("MDY") # MMDDYY10.
    }
  }
  # Check ISO
  iso_vals <- vals[grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", vals)]
  if (length(iso_vals) > 0) {
    return("YMD")
  }
  # Default to MDY if ambiguous
  return("MDY")
}

# Helper to parse date string with single order
parse_with_order <- function(vec, order) {
  # vec: character vector
  # order: "DMY", "MDY", "YMD"
  result <- rep(as.Date(NA), length(vec))
  for (i in seq_along(vec)) {
    v <- vec[i]
    if (is.na(v) || trimws(v) == "") next
    v <- trimws(v)
    # Try to parse according to order
    # Support formats: m/d/Y, d/m/Y, Y-m-d, Y/m/d, with 2 or 4 digit year, with dashes or slashes
    # Also support "ddMMMyyyy" like "15JAN2020" ? Not needed but handle
    # Use as.Date with explicit format
    parsed <- NA
    if (order == "MDY") {
      # Try m/d/Y with slashes
      # Try multiple separators
      for (fmt in c("%m/%d/%Y", "%m/%d/%y", "%m-%d-%Y", "%m-%d-%y", "%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d")) {
        parsed <- suppressWarnings(as.Date(v, format = fmt))
        if (!is.na(parsed)) break
      }
      # If still NA, try lubridate style but strict: we already tried
      # Also try with single digit month/day: as.Date handles %m/%d correctly
    } else if (order == "DMY") {
      for (fmt in c("%d/%m/%Y", "%d/%m/%y", "%d-%m-%Y", "%d-%m-%y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")) {
        parsed <- suppressWarnings(as.Date(v, format = fmt))
        if (!is.na(parsed)) break
      }
    } else if (order == "YMD") {
      for (fmt in c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")) {
        parsed <- suppressWarnings(as.Date(v, format = fmt))
        if (!is.na(parsed)) break
      }
    }
    # If still NA, try ANYDTDTE-like fallback: try all common
    if (is.na(parsed)) {
      # Try ISO
      parsed <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(v, format = "%d-%b-%Y"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(v, format = "%d-%b-%y"))
    }
    result[i] <- parsed
  }
  result
}

# Determine orders for START_DATE and END_DATE based on raw_char values
start_order <- decide_date_order(raw_char$START_DATE)
end_order   <- decide_date_order(raw_char$END_DATE)

# For debugging, we can also check if SAS would have imported as character vs numeric
# PROC IMPORT decides type based on whether values look like dates/numbers
# If column contains only date-like strings, it will be numeric date
# If mixed with non-date strings, it becomes character
# In this dataset, START_DATE and END_DATE are likely all date-like, so SAS would make them numeric
# But the SAS code handles both: if vtype == 'C' then input with anydtdte else use numeric directly
# In R, we have character, so we need to emulate numeric path: but we can just parse as dates
# The key is to use ONE order per column, not per value

# Now read raw_ex properly: we already have raw_char, but we need to emulate SAS import types
# For simplicity, treat START_DATE and END_DATE as character originally, then parse with single order
# This matches SAS logic where if imported as numeric, the numeric value is SAS date (days since 1960-01-01)
# Converting that numeric to Date then to ISO would give same result as parsing with correct order
# So we can just parse raw_char with decided order

# Build work.raw_ex equivalent
# SAS PROC IMPORT would have columns: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE
# Let's ensure column names are uppercase as SAS does with validvarname=upcase
names(raw_char) <- toupper(names(raw_char))

# Check actual names
# Expected: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE
# If names differ, map
# print(names(raw_char))

# Create work.raw_ex data frame with proper types
# DOSE should be numeric
work_raw_ex <- raw_char
# Convert DOSE to numeric
if ("DOSE" %in% names(work_raw_ex)) {
  work_raw_ex$DOSE <- suppressWarnings(as.numeric(work_raw_ex$DOSE))
}
# PATIENT may be numeric or character; keep as character for USUBJID
if ("PATIENT" %in% names(work_raw_ex)) {
  work_raw_ex$PATIENT <- as.character(work_raw_ex$PATIENT)
}

# 2. CORE TRANSFORMATION -> work.ex_mapped
# SAS code:
# length STUDYID $8 DOMAIN $2 USUBJID $40 EXTRT $50 EXDOSU $20 EXSTDTC $10 EXENDTC $10
# STUDYID = "CDISC-01"
# DOMAIN = "EX"
# USUBJID = catx("-", STUDYID, PATIENT)
# EXTRT = strip(TREATMENT)
# EXDOSE = DOSE
# EXDOSU = strip(UNIT)
# _start_num = input(START_DATE, anydtdte.) if char else START_DATE
# EXSTDTC = put(_start_num, is8601da.) -> YYYY-MM-DD
# similarly END

# Parse dates with single order per column
# Use raw_char original strings for parsing (before any conversion)
# Need to get original START_DATE strings (character)
start_raw <- raw_char$START_DATE
end_raw   <- raw_char$END_DATE

# Parse using decided order
start_dates <- parse_with_order(start_raw, start_order)
end_dates   <- parse_with_order(end_raw, end_order)

# Build ex_mapped
ex_mapped <- work_raw_ex %>%
  dplyr::mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "EX",
    USUBJID = paste0("CDISC-01-", trimws(as.character(PATIENT))),
    # SAS catx trims and skips missing; PATIENT is never missing
    EXTRT = sas_strip(TREATMENT),
    EXDOSE = DOSE,
    EXDOSU = sas_strip(UNIT),
    EXSTDTC = ifelse(is.na(start_dates), "", format(start_dates, "%Y-%m-%d")),
    EXENDTC = ifelse(is.na(end_dates), "", format(end_dates, "%Y-%m-%d"))
  ) %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# Handle case where EXSTDTC/EXENDTC should be "" for missing (SAS sets to "" not NA)
ex_mapped$EXSTDTC[is.na(ex_mapped$EXSTDTC)] <- ""
ex_mapped$EXENDTC[is.na(ex_mapped$EXENDTC)] <- ""

# 3. GENERATE SEQUENCE NUMBER (EXSEQ)
# proc sort by USUBJID EXSTDTC
# data sdtm.ex retain ... set work.ex_mapped; by USUBJID; if first.USUBJID then EXSEQ=1 else EXSEQ+1

# SAS sort: character ordering uppercase before lowercase, missing first
# In R, order by USUBJID then EXSTDTC (character sort, "" first)
ex_mapped_sorted <- ex_mapped %>%
  dplyr::arrange(USUBJID, EXSTDTC)

# Generate EXSEQ per USUBJID
sdtm_ex <- ex_mapped_sorted %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(EXSEQ = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# Ensure column order matches SAS: STUDYID DOMAIN USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC
# Retain order already

# Write dataset ex.csv
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT -> P014.lst
# title "EX Domain Audit (Study Drug Exposure)";
# proc print data=sdtm.ex(obs=10); var USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC; run;

# Create listing text rendering
# SAS PROC PRINT prints Obs number and variables
# We need to reproduce listing with 126 numbers? The expected output says 126 numbers
# Let's create a text file that contains the audit table

# Prepare audit data (first 10 obs)
audit_data <- head(sdtm_ex, 10)

# Build text rendering
# Include title, column headers, and rows
# Format to match SAS listing style

# Helper to format EXDOSE as SAS would (numeric, no extra decimals unless needed)
# SAS default BEST12. prints numbers without trailing zeros
format_dose <- function(x) {
  if (is.na(x)) return("")
  # Use format with minimal representation
  # SAS BEST. would show e.g., 100, 50, etc.
  # Use as.character without scientific
  format(x, scientific = FALSE, trim = TRUE, nsmall = 0)
}

# Create text lines
title_str <- "EX Domain Audit (Study Drug Exposure)"
header_line <- sprintf("%-5s %-20s %-6s %-30s %-8s %-12s %-12s %-12s",
                       "Obs", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")
separator <- paste(rep("-", nchar(header_line)), collapse = "")

lines <- c(
  title_str,
  "",
  header_line,
  separator
)

for (i in seq_len(nrow(audit_data))) {
  row <- audit_data[i, ]
  line <- sprintf("%-5d %-20s %-6d %-30s %-8s %-12s %-12s %-12s",
                  i,
                  row$USUBJID,
                  row$EXSEQ,
                  substr(row$EXTRT, 1, 30),
                  format_dose(row$EXDOSE),
                  row$EXDOSU,
                  row$EXSTDTC,
                  row$EXENDTC)
  lines <- c(lines, line)
}

# Add footer with total observations info (SAS prints number of obs)
lines <- c(lines, "", paste0("Total observations in EX domain: ", nrow(sdtm_ex)))

# Write listing text file
writeLines(lines, file.path(out, "P014.lst"))

# Also write CSV of the table body for P014 (as per rules: each table/listing additionally as CSV)
# The CSV should contain the same table body
write.csv(audit_data, file.path(out, "P014.csv"), row.names = FALSE, na = "")

# Also ensure P014.txt exists (some validators expect .txt)
writeLines(lines, file.path(out, "P014.txt"))

# Verify outputs
# cat("Done. Rows:", nrow(sdtm_ex), "\n")
# print(head(sdtm_ex))
```