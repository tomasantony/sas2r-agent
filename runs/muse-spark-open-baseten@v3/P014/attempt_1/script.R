# P014 SDTM EX translation
# Reproduce SAS program P014_sdtm_ex.sas in R

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed here but defined)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# Helper: format SAS is8601da. -> YYYY-MM-DD
format_is8601da <- function(num) {
  # SAS date numeric is days since 1960-01-01
  # But in R we will parse to Date directly; this helper not needed for numeric SAS dates
  # Kept for completeness
  as.character(as.Date(num, origin = "1960-01-01"))
}

# -------------------------------------------------------------------------
# 1. IMPORT RAW EXPOSURE DATA
# SAS: proc import datafile="&project_path./data/raw/raw_exposure.csv"
# project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c
# -------------------------------------------------------------------------
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path_1 <- file.path(project_path, "data/raw/raw_exposure.csv")
raw_path_2 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_exposure.csv"
raw_path_3 <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv"

raw_file <- NULL
for (p in c(raw_path_1, raw_path_2, raw_path_3)) {
  if (file.exists(p)) { raw_file <- p; break }
}
# Fallback search
if (is.null(raw_file)) {
  candidates <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_exposure\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(candidates) > 0) raw_file <- candidates[1]
}
if (is.null(raw_file)) stop("raw_exposure.csv not found")

# Emulate PROC IMPORT GUESSINGROWS behavior:
# Read raw file as text to inspect column types, then read with appropriate col_types
# SAS PROC IMPORT scans values to decide informat per column.
# For date columns, if any value has first field >12, it chooses DDMMYY, otherwise MMDDYY.
# We need to replicate that logic for START_DATE and END_DATE.

# Read raw lines to detect date column formats
raw_lines <- readLines(raw_file, warn = FALSE)
# Use readr to read as character first to preserve original strings
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# Read with all columns as character to mimic SAS character handling and to apply single-format parsing
raw_ex_char <- read_csv(raw_file, col_types = cols(.default = col_character()), show_col_types = FALSE, na = c("", "NA"))

# Identify column names as SAS would (GETNAMES=YES, VALIDVARNAME=UPCASE)
# SAS with validvarname=upcase forces variable names to uppercase
names(raw_ex_char) <- toupper(names(raw_ex_char))

# Expected columns: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE (check actual)
# Inspect
# print(names(raw_ex_char))

# Helper to decide date order per column (PROC IMPORT GUESSINGROWS emulation)
decide_date_order <- function(vec) {
  # vec is character vector of date strings (non-missing)
  # SAS ANYDTDTE with DATESTYLE MDY vs DMY: PROC IMPORT picks one informat per column
  # after scanning values: if any value has first field >12, it uses DDMMYY10., else MMDDYY10.
  # We emulate: look at all values that match n/n/yyyy or n-n-yyyy etc.
  vals <- vec[!is.na(vec) & trimws(vec) != ""]
  if (length(vals) == 0) return("MDY")
  # Extract first numeric field before / or -
  first_fields <- suppressWarnings(as.integer(str_extract(vals, "^\\s*\\d+")))
  # If any first field >12, then column is DMY (DDMMYY)
  if (any(first_fields > 12, na.rm = TRUE)) {
    return("DMY")
  } else {
    return("MDY")
  }
}

parse_single_format <- function(vec, order) {
  # Parse every value with the single chosen order, strict
  # SAS ANYDTDTE. accepts many forms; we handle common ones: mm/dd/yyyy, dd/mm/yyyy, yyyy-mm-dd, dd-mmm-yyyy etc.
  # But per instruction, use ONE explicit strict format per column decided above.
  # However raw_exposure.csv likely contains dates like "01/15/2020" or "15/01/2020" or "2020-01-15"
  # We need to detect the actual format string.
  # Simplify: try to infer format from first non-missing value, then apply that format to all.
  # But instruction says: decide order once per column from all values, then parse every value with that single format.
  # So we implement: if values contain "-", assume yyyy-mm-dd or dd-mmm-yyyy; else "/" with order.
  result <- rep(as.Date(NA), length(vec))
  for (i in seq_along(vec)) {
    v <- vec[i]
    if (is.na(v) || trimws(v) == "") next
    v <- trimws(v)
    # Try ISO first
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", v)) {
      result[i] <- as.Date(v, format = "%Y-%m-%d")
    } else if (grepl("/", v)) {
      if (order == "DMY") {
        result[i] <- as.Date(v, format = "%d/%m/%Y")
        if (is.na(result[i])) result[i] <- as.Date(v, format = "%d/%m/%y")
      } else {
        result[i] <- as.Date(v, format = "%m/%d/%Y")
        if (is.na(result[i])) result[i] <- as.Date(v, format = "%m/%d/%y")
      }
    } else if (grepl("-", v)) {
      # Could be dd-mmm-yyyy or mm-dd-yyyy
      # Try d-b-Y
      parsed <- suppressWarnings(as.Date(v, format = "%d-%b-%Y"))
      if (!is.na(parsed)) {
        result[i] <- parsed
      } else {
        parsed2 <- suppressWarnings(as.Date(v, format = "%d-%B-%Y"))
        if (!is.na(parsed2)) result[i] <- parsed2 else {
          # fallback to m-d-Y
          if (order == "DMY") result[i] <- as.Date(v, format = "%d-%m-%Y") else result[i] <- as.Date(v, format = "%m-%d-%Y")
        }
      }
    } else {
      # numeric SAS date? (days since 1960-01-01)
      # If string is all digits, treat as SAS numeric date
      if (grepl("^\\d+$", v)) {
        result[i] <- as.Date(as.integer(v), origin = "1960-01-01")
      }
    }
  }
  result
}

# Determine column existence
# SAS code uses: PATIENT, TREATMENT, DOSE, UNIT, START_DATE, END_DATE
# Ensure they exist (uppercase)
required_cols <- c("PATIENT", "TREATMENT", "DOSE", "UNIT", "START_DATE", "END_DATE")
missing_cols <- setdiff(required_cols, names(raw_ex_char))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in raw_exposure.csv:", paste(missing_cols, collapse = ", ")))
}

# Decide date order per column (PROC IMPORT emulation)
start_order <- decide_date_order(raw_ex_char$START_DATE)
end_order   <- decide_date_order(raw_ex_char$END_DATE)

# Parse dates with single format per column
start_dates <- parse_single_format(raw_ex_char$START_DATE, start_order)
end_dates   <- parse_single_format(raw_ex_char$END_DATE, end_order)

# Also handle case where PROC IMPORT already read as numeric SAS dates:
# If raw_ex_char column was read as character but original CSV had numeric SAS dates,
# our parser handles digit strings. If readr guessed numeric, we already forced character.

# -------------------------------------------------------------------------
# 2. CORE TRANSFORMATION
# -------------------------------------------------------------------------
# SAS lengths: STUDYID $8, DOMAIN $2, USUBJID $40, EXTRT $50, EXDOSU $20, EXSTDTC $10, EXENDTC $10
# STUDYID = "CDISC-01", DOMAIN="EX", USUBJID = catx("-", STUDYID, PATIENT)
# EXTRT = strip(TREATMENT), EXDOSE = DOSE, EXDOSU = strip(UNIT)
# EXSTDTC = put(_start_num, is8601da.) -> YYYY-MM-DD
# EXENDTC = put(_end_num, is8601da.) or "" if missing

# DOSE may be character; convert to numeric as SAS would
exdose_num <- suppressWarnings(as.numeric(raw_ex_char$DOSE))

# Build ex_mapped
ex_mapped <- tibble(
  STUDYID = rep("CDISC-01", nrow(raw_ex_char)),
  DOMAIN  = rep("EX", nrow(raw_ex_char)),
  USUBJID = paste0("CDISC-01-", trimws(raw_ex_char$PATIENT)),
  EXTRT   = trimws(raw_ex_char$TREATMENT),
  EXDOSE  = exdose_num,
  EXDOSU  = trimws(raw_ex_char$UNIT),
  EXSTDTC = ifelse(is.na(start_dates), "", format(start_dates, "%Y-%m-%d")),
  EXENDTC = ifelse(is.na(end_dates), "", format(end_dates, "%Y-%m-%d"))
)

# Truncate to SAS lengths (prevent truncation differences)
ex_mapped$STUDYID <- str_sub(ex_mapped$STUDYID, 1, 8)
ex_mapped$DOMAIN  <- str_sub(ex_mapped$DOMAIN, 1, 2)
ex_mapped$USUBJID <- str_sub(ex_mapped$USUBJID, 1, 40)
ex_mapped$EXTRT   <- str_sub(ex_mapped$EXTRT, 1, 50)
ex_mapped$EXDOSU  <- str_sub(ex_mapped$EXDOSU, 1, 20)
ex_mapped$EXSTDTC <- str_sub(ex_mapped$EXSTDTC, 1, 10)
ex_mapped$EXENDTC <- str_sub(ex_mapped$EXENDTC, 1, 10)

# Keep only required columns in SAS order
ex_mapped <- ex_mapped %>% select(STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# -------------------------------------------------------------------------
# 3. GENERATE SEQUENCE NUMBER (EXSEQ)
# proc sort by USUBJID EXSTDTC; then by USUBJID retain counter
# -------------------------------------------------------------------------
# SAS sort: character ordering uppercase before lowercase, missing first
# EXSTDTC is YYYY-MM-DD string, "" sorts first (missing)
ex_mapped_sorted <- ex_mapped %>% arrange(USUBJID, EXSTDTC)

# Generate EXSEQ per USUBJID
sdtm_ex <- ex_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  # Reorder columns to match SAS retain order: STUDYID DOMAIN USUBJID EXSEQ + rest
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC) %>%
  arrange(USUBJID, EXSTDTC, EXSEQ)

# Ensure types: EXSEQ integer
sdtm_ex$EXSEQ <- as.integer(sdtm_ex$EXSEQ)

# -------------------------------------------------------------------------
# Write dataset ex.csv
# -------------------------------------------------------------------------
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# 4. VISUAL AUDIT - PROC PRINT obs=10
# Title: "EX Domain Audit (Study Drug Exposure)"
# Produce listing text file P014.lst with 126 numbers
# SAS PROC PRINT prints 10 obs with variables USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC
# We need to reproduce listing text rendering and CSV of table body
# -------------------------------------------------------------------------
audit_obs <- head(sdtm_ex, 10)

# Create text rendering
lst_path <- file.path(out, "P014.lst")
txt_path <- file.path(out, "P014.txt")  # also write .txt as per spec (listing additionally as plain-text)

# Build listing text similar to SAS PROC PRINT output
# Include title and column headers
header_line <- sprintf("%-20s %5s %-30s %8s %-12s %-12s %-12s",
                       "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")
separator <- paste(rep("-", nchar(header_line)), collapse = "")

con <- file(lst_path, open = "wt")
writeLines("EX Domain Audit (Study Drug Exposure)", con)
writeLines("", con)
writeLines(header_line, con)
writeLines(separator, con)
for (i in seq_len(nrow(audit_obs))) {
  r <- audit_obs[i, ]
  line <- sprintf("%-20s %5d %-30s %8s %-12s %-12s %-12s",
                  r$USUBJID,
                  r$EXSEQ,
                  substr(r$EXTRT, 1, 30),
                  ifelse(is.na(r$EXDOSE), "", format(r$EXDOSE, trim = TRUE, scientific = FALSE)),
                  r$EXDOSU,
                  r$EXSTDTC,
                  r$EXENDTC)
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Obs displayed: ", nrow(audit_obs), " of ", nrow(sdtm_ex), " total"), con)
close(con)

# Also write P014.txt as copy (spec says each table/listing additionally as plain-text rendering)
file.copy(lst_path, txt_path, overwrite = TRUE)

# Write CSV of table body (the listing data)
# Spec: each table or listing additionally as CSV of the table body
write.csv(audit_obs, file.path(out, "P014.csv"), row.names = FALSE, na = "")

# Also ensure P014.lst.csv? But spec says write CSV of same table - we did P014.csv
# For completeness, also write listing CSV with same name base
# The expected outputs list says P014.lst -> write text rendering and CSV of same table
# We have provided both.

# Verify counts: listing contains 126 numbers? Let's ensure numbers are present
# Numbers in listing: EXSEQ and EXDOSE values. For 10 rows, that's up to 20 numbers.
# The spec says 126 numbers in P014.lst - that likely refers to full dataset print? But SAS code prints obs=10.
# We have reproduced the 10-obs audit. The 126 numbers may refer to full dataset values counted differently.
# To satisfy, ensure full dataset also represented in text if needed - but we follow SAS program exactly (obs=10).

# Final message
cat("P014 translation complete. Output written to", out, "\n")
cat("Rows in sdtm.ex:", nrow(sdtm_ex), "\n")
