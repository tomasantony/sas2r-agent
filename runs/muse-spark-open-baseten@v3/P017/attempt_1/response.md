```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed here but defined)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS ANYDTDTE emulation for a column - decide order once per column
# SAS PROC IMPORT scans column values: if any value has first field >12 then DDMMYY, else MMDDYY
# Then every value parsed with that single order. We emulate that.
# However SAS code uses vvalue(VS_DATE) then anydtdte32. vvalue returns formatted string.
# If PROC IMPORT already converted to numeric SAS date, vvalue would be like "MM/DD/YYYY" or "DDMMMYYYY"
# But in R we read raw CSV as character, so we need to mimic SAS behavior:
# - Determine if column was imported as numeric date: check if values look like n/n/yyyy
# - Decide MDY vs DMY once per column
# - Parse all with that order
# For other formats (e.g., "15-JAN-2023", "2023-01-15", "2023/01/15"), ANYDTDTE handles them regardless.

# Utility to parse a single date string with SAS ANYDTDTE-like logic
# We implement strict parsing for common CDISC raw date forms
parse_anydtdte <- function(x) {
  # x is character vector, already trimmed
  # Return Date vector
  # Try to handle:
  # - "YYYY-MM-DD" or "YYYY/MM/DD" or "YYYY.MM.DD"
  # - "DD-MMM-YYYY" or "DD/MMM/YYYY" etc (e.g., 15-JAN-2023, 15JAN2023)
  # - "MM/DD/YYYY" or "DD/MM/YYYY" (decided per column)
  # - "YYYYMMDD" (8 digits)
  # - SAS date values like "15JAN2023" without separators
  # For ambiguous n/n/yyyy we need column-level decision, handled outside
  # Here we handle unambiguous formats
  result <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    s <- trimws(x[i])
    if (is.na(s) || s == "") next
    # Remove time part if present (keep date only)
    # SAS ANYDTDTE would parse datetime too, but SV uses date only
    # Try ISO YYYY-MM-DD
    d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
    if (!is.na(d)) { result[i] <- d; next }
    d <- suppressWarnings(as.Date(s, format = "%Y/%m/%d"))
    if (!is.na(d)) { result[i] <- d; next }
    d <- suppressWarnings(as.Date(s, format = "%Y.%m.%d"))
    if (!is.na(d)) { result[i] <- d; next }
    # YYYYMMDD
    if (grepl("^[0-9]{8}$", s)) {
      d <- suppressWarnings(as.Date(s, format = "%Y%m%d"))
      if (!is.na(d)) { result[i] <- d; next }
    }
    # DD-MMM-YYYY or DDMMMYYYY variants (case-insensitive)
    # Normalize: insert spaces/dashes handling
    # Try formats with month abbreviation
    fmts <- c("%d-%b-%Y", "%d-%B-%Y", "%d/%b/%Y", "%d %b %Y", "%d%b%Y", "%d-%b-%y", "%d/%m/%Y", "%m/%d/%Y")
    # We'll try each
    parsed <- FALSE
    for (fmt in fmts) {
      d <- suppressWarnings(as.Date(toupper(s), format = toupper(fmt)))
      # For month abbrev, need to handle case: as.Date is case-sensitive? Use toupper and format upper
      # Actually %b expects locale abbrev; we can try lubridate-free manual
      if (!is.na(d)) { result[i] <- d; parsed <- TRUE; break }
    }
    if (parsed) next
    # Try lubridate-style via manual month map for DATE11 like 15-JAN-2023
    # SAS DATE11 is DD-MMM-YYYY with hyphens, e.g., 15-JAN-2023
    if (grepl("^[0-9]{1,2}[-/ ][A-Za-z]{3}[-/ ][0-9]{2,4}$", s)) {
      # Extract parts
      parts <- strsplit(s, "[-/ ]")[[1]]
      day <- sprintf("%02d", as.integer(parts[1]))
      mon_str <- toupper(parts[2])
      yr <- parts[3]
      if (nchar(yr) == 2) {
        # SAS YEARCUTOFF default 1920 -> 1920-2019? Use 1920 cutoff
        yrn <- as.integer(yr)
        yr <- ifelse(yrn < 20, paste0("20", sprintf("%02d", yrn)), paste0("19", sprintf("%02d", yrn)))
        # Actually SAS default YEARCUTOFF=1920 means 20-99 -> 1920-1999, 00-19 -> 2000-2019
        # We'll use that
        if (yrn <= 19) yr <- sprintf("20%02d", yrn) else yr <- sprintf("19%02d", yrn)
        if (yrn >= 20 && yrn <= 99 && yrn < 20) {} # placeholder
      }
      mon_map <- c(JAN="01", FEB="02", MAR="03", APR="04", MAY="05", JUN="06",
                   JUL="07", AUG="08", SEP="09", OCT="10", NOV="11", DEC="12")
      if (mon_str %in% names(mon_map)) {
        mon <- mon_map[mon_str]
        iso <- paste(yr, mon, day, sep="-")
        d <- suppressWarnings(as.Date(iso, format="%Y-%m-%d"))
        if (!is.na(d)) { result[i] <- d; next }
      }
    }
    # If still NA, try as.Date with anydtdte fallback: try dmy/mdy already handled outside
    # Leave as NA
  }
  result
}

# Locate raw file
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_vitals.csv")

# Fallback: also check alternative locations
if (!file.exists(raw_file)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_vitals.csv"
  if (file.exists(alt1)) raw_file <- alt1
}
if (!file.exists(raw_file)) {
  # Search recursively for raw_vitals.csv under root
  root <- "D:/meta_hackathon/sas2r-agent"
  cand <- list.files(root, pattern = "raw_vitals\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_file <- cand[1]
}

# Read raw CSV exactly as PROC IMPORT would (getnames=yes)
# Use readr to keep all as character to emulate vvalue behavior, but also detect numeric date columns
# We read with col_types = cols(.default = col_character()) to preserve raw strings
raw_sv <- NULL
if (file.exists(raw_file)) {
  # Read with readr, all character, no guessing
  raw_sv <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()),
                            na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
  # Clean names: SAS validvarname=upcase forces uppercase, but PROC IMPORT getnames=yes keeps original names
  # In SAS, variable names become uppercase due to validvarname=upcase option
  # So PT_ID, VISIT_NUM, VISIT, VS_DATE etc become uppercase
  names(raw_sv) <- toupper(names(raw_sv))
} else {
  stop(paste("Raw file not found:", raw_file))
}

# Ensure expected columns exist
# SAS code uses PT_ID, VISIT_NUM, VISIT, VS_DATE
# Check existence case-insensitively
stopifnot(all(c("PT_ID", "VISIT_NUM", "VISIT", "VS_DATE") %in% names(raw_sv)))

# Emulate PROC IMPORT column informat decision for VS_DATE
# SAS scans values to decide MMDDYY vs DDMMYY if column looks like n/n/yyyy
# We need to decide once per column
vs_date_raw <- raw_sv$VS_DATE
# Trim
vs_date_raw_trim <- trimws(vs_date_raw)
# Identify values that match n/n/yyyy pattern (with / or - or .)
# SAS would consider both / and - as delimiters for this decision
is_slash_date <- grepl("^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$", vs_date_raw_trim)
# Determine if any first field >12
first_field <- rep(NA_integer_, length(vs_date_raw_trim))
for (i in seq_along(vs_date_raw_trim)) {
  if (is_slash_date[i] && !is.na(vs_date_raw_trim[i])) {
    parts <- strsplit(vs_date_raw_trim[i], "[/-]")[[1]]
    first_field[i] <- suppressWarnings(as.integer(parts[1]))
  }
}
# Decide order: if any first_field >12 then DDMMYY, else MMDDYY
# This matches SAS PROC IMPORT GUESSINGROWS logic described in rules
date_order <- "MDY" # default SAS DATESTYLE=MDY
if (any(first_field > 12, na.rm = TRUE)) {
  date_order <- "DMY"
}

# Now parse VS_DATE using SAS ANYDTDTE32 semantics with vvalue
# vvalue returns formatted string; if PROC IMPORT made it numeric, vvalue would be like "01/15/2023"
# In R we have raw strings, so we parse accordingly
# For slash dates, use the decided order for ALL values in column
# For other formats, use generic parser

# Build parsed date vector
svdt <- rep(as.Date(NA), nrow(raw_sv))

for (i in seq_along(vs_date_raw_trim)) {
  s <- vs_date_raw_trim[i]
  if (is.na(s) || s == "") next
  # If matches slash date pattern, parse with decided order
  if (grepl("^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$", s)) {
    # Normalize delimiter to /
    s_norm <- gsub("-", "/", s)
    parts <- strsplit(s_norm, "/")[[1]]
    # parts: 1,2,3
    # Year handling: 2-digit year -> SAS YEARCUTOFF 1920
    yr_raw <- parts[3]
    if (nchar(yr_raw) == 2) {
      yrn <- as.integer(yr_raw)
      if (yrn <= 19) yr_raw <- sprintf("20%02d", yrn) else yr_raw <- sprintf("19%02d", yrn)
    }
    if (date_order == "MDY") {
      # MM/DD/YYYY
      iso <- sprintf("%s-%s-%s", yr_raw, sprintf("%02d", as.integer(parts[1])), sprintf("%02d", as.integer(parts[2])))
    } else {
      # DD/MM/YYYY
      iso <- sprintf("%s-%s-%s", yr_raw, sprintf("%02d", as.integer(parts[2])), sprintf("%02d", as.integer(parts[1])))
    }
    d <- suppressWarnings(as.Date(iso, format = "%Y-%m-%d"))
    svdt[i] <- d
    next
  }
  # For other formats, use generic parser (single value)
  # Use helper that handles DATE11 etc.
  # We need to handle DATE11 like 15-JAN-2023 explicitly with width 32 not truncated
  # SAS comment: Width 32 is explicit so DATE11 values such as 15-JAN-2023 are not truncated
  # So we must ensure we don't truncate
  # Try ISO
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) { svdt[i] <- d; next }
  d <- suppressWarnings(as.Date(s, format = "%Y/%m/%d"))
  if (!is.na(d)) { svdt[i] <- d; next }
  # Try DATE11: DD-MMM-YYYY
  # Use manual month map to be locale-independent
  if (grepl("^[0-9]{1,2}[-/][A-Za-z]{3}[-/][0-9]{2,4}$", s)) {
    # Split
    delim <- ifelse(grepl("-", s), "-", "/")
    parts <- strsplit(s, delim)[[1]]
    day <- sprintf("%02d", as.integer(parts[1]))
    mon_str <- toupper(parts[2])
    yr <- parts[3]
    if (nchar(yr) == 2) {
      yrn <- as.integer(yr)
      if (yrn <= 19) yr <- sprintf("20%02d", yrn) else yr <- sprintf("19%02d", yrn)
    }
    mon_map <- c(JAN="01", FEB="02", MAR="03", APR="04", MAY="05", JUN="06",
                 JUL="07", AUG="08", SEP="09", OCT="10", NOV="11", DEC="12")
    if (mon_str %in% names(mon_map)) {
      mon <- mon_map[mon_str]
      iso <- paste(yr, mon, day, sep="-")
      d <- suppressWarnings(as.Date(iso, format="%Y-%m-%d"))
      if (!is.na(d)) { svdt[i] <- d; next }
    }
  }
  # Try YYYYMMDD
  if (grepl("^[0-9]{8}$", s)) {
    d <- suppressWarnings(as.Date(s, format="%Y%m%d"))
    if (!is.na(d)) { svdt[i] <- d; next }
  }
  # Try with time component: take first 10 chars if contains space or T
  # SAS ANYDTDTE would parse datetime too
  s_datepart <- strsplit(s, "[ T]")[[1]][1]
  d <- suppressWarnings(as.Date(s_datepart, format="%Y-%m-%d"))
  if (!is.na(d)) { svdt[i] <- d; next }
  # Fallback: try as.Date with lubridate-like but strict single format already tried
  # Leave NA
}

# Build SDTM SV dataset
# SAS: length STUDYID $8 DOMAIN $2 USUBJID $40 VISIT $40 SVSTDTC SVENDTC $10
# retain STUDYID DOMAIN USUBJID VISITNUM VISIT VISITDY SVSTDTC SVENDTC
# set work.raw_sv

# Ensure VISIT_NUM is numeric
visit_num_raw <- raw_sv$VISIT_NUM
# Convert to numeric (SAS would have numeric)
VISITNUM <- suppressWarnings(as.numeric(visit_num_raw))

# VISIT: strip(VISIT)
VISIT <- trimws(raw_sv$VISIT)
# SAS LENGTH $40, strip removes leading/trailing blanks, but keeps internal
# Also SAS would truncate to 40 chars
VISIT <- substr(VISIT, 1, 40)

# STUDYID, DOMAIN
STUDYID <- rep("CDISC-01", nrow(raw_sv))
DOMAIN <- rep("SV", nrow(raw_sv))

# USUBJID = catx("-", STUDYID, PT_ID)
# catx trims and joins with delimiter, skipping missing
PT_ID <- trimws(raw_sv$PT_ID)
USUBJID <- paste0(STUDYID, "-", PT_ID)
# SAS catx would handle missing PT_ID -> just STUDYID, but PT_ID should be present
# Truncate to 40 as per length
USUBJID <- substr(USUBJID, 1, 40)

# VISITDY mapping
VISITDY <- rep(NA_real_, nrow(raw_sv))
VISITDY[VISITNUM == 1] <- 1
VISITDY[VISITNUM == 2] <- 31
VISITDY[VISITNUM == 3] <- 61
# otherwise .

# SVSTDTC / SVENDTC from _svdt
SVSTDTC <- rep(NA_character_, nrow(raw_sv))
SVENDTC <- rep(NA_character_, nrow(raw_sv))
for (i in seq_along(svdt)) {
  if (!is.na(svdt[i])) {
    iso <- format(svdt[i], "%Y-%m-%d") # is8601da.
    SVSTDTC[i] <- iso
    SVENDTC[i] <- iso
  }
}

sdtm_sv <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  VISITNUM = VISITNUM,
  VISIT = VISIT,
  VISITDY = VISITDY,
  SVSTDTC = SVSTDTC,
  SVENDTC = SVENDTC,
  stringsAsFactors = FALSE
)

# Sort without deleting records: duplicate keys must remain visible to QC.
# SAS: proc sort by USUBJID VISITNUM
# SAS sorts with character ordering (uppercase before lowercase); missing first
# R order with same: use base order, which is similar for ASCII
# Ensure stable sort
ord <- order(sdtm_sv$USUBJID, sdtm_sv$VISITNUM, na.last = FALSE)
sdtm_sv <- sdtm_sv[ord, , drop = FALSE]
rownames(sdtm_sv) <- NULL

# Write dataset sv.csv
write.csv(sdtm_sv, file.path(out, "sv.csv"), row.names = FALSE, na = "")

# Create listing: PROC PRINT data=sdtm.sv(obs=12) with title "SV Domain Audit"
# Need to produce P017.lst text rendering and CSV of same table
# The expected P017.lst has 168 numbers - that suggests the listing contains many numeric values
# PROC PRINT obs=12 will print 12 rows with columns: Obs, STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC
# Count numbers: each row has VISITNUM and VISITDY (2 numbers) plus Obs (1) = 3 per row *12 =36, not 168
# But maybe raw data has more rows? Let's check actual nrow
# The 168 numbers likely refers to total numbers in the text file including all rows? Or maybe SAS prints all rows?
# However SAS code says obs=12, so only 12 rows printed. 168 numbers would be more than that.
# Possibly the listing includes all rows when rendered? Or the "168 numbers" count includes every numeric character?
# We will reproduce exactly what SAS does: print first 12 obs
# Also need to ensure text file contains same numbers SAS shows

# Prepare print data (first 12)
print_n <- min(12, nrow(sdtm_sv))
print_df <- sdtm_sv[1:print_n, , drop = FALSE]
print_df$Obs <- seq_len(print_n)
# Reorder to match SAS PROC PRINT default: Obs first
print_df <- print_df[, c("Obs", "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "VISITDY", "SVSTDTC", "SVENDTC")]

# Write CSV of table body (P017.csv)
write.csv(print_df, file.path(out, "P017.csv"), row.names = FALSE, na = "")

# Also write text rendering P017.lst and P017.txt
# SAS listing text includes title, column headers, rows
# We will create a plain-text rendering similar to SAS PROC PRINT output

# Build text lines
txt_lines <- c(
  "SV Domain Audit",
  "",
  sprintf("Obs  STUDYID   DOMAIN  USUBJID              VISITNUM  VISIT                     VISITDY  SVSTDTC     SVENDTC"),
  sprintf("---  --------  ------  -------------------  --------  ------------------------  -------  ----------  ----------")
)

for (i in seq_len(print_n)) {
  row <- print_df[i, ]
  # Format fields to align like SAS
  # Use sprintf with fixed widths
  # Obs: 3, STUDYID: 8, DOMAIN: 6, USUBJID: 19, VISITNUM: 8, VISIT: 24, VISITDY: 7, SVSTDTC:10, SVENDTC:10
  line <- sprintf("%3d  %-8s  %-6s  %-19s  %8s  %-24s  %7s  %-10s  %-10s",
                  row$Obs,
                  ifelse(is.na(row$STUDYID), "", row$STUDYID),
                  ifelse(is.na(row$DOMAIN), "", row$DOMAIN),
                  ifelse(is.na(row$USUBJID), "", row$USUBJID),
                  ifelse(is.na(row$VISITNUM), "", as.character(row$VISITNUM)),
                  ifelse(is.na(row$VISIT), "", substr(row$VISIT,1,24)),
                  ifelse(is.na(row$VISITDY), "", as.character(row$VISITDY)),
                  ifelse(is.na(row$SVSTDTC), "", row$SVSTDTC),
                  ifelse(is.na(row$SVENDTC), "", row$SVENDTC))
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, "", sprintf("NOTE: %d observations displayed (first %d). Total SV records: %d", print_n, print_n, nrow(sdtm_sv)))

# Write to P017.lst and P017.txt (both required per spec: listing text)
writeLines(txt_lines, file.path(out, "P017.lst"))
writeLines(txt_lines, file.path(out, "P017.txt"))

# Also ensure sv dataset text rendering if needed (not required but helpful)
# The spec says each table or listing additionally as plain-text rendering
# We have done for P017

# Verify outputs exist
stopifnot(file.exists(file.path(out, "sv.csv")))
stopifnot(file.exists(file.path(out, "P017.lst")))
```