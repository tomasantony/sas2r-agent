```r
# P012_sdtm_ds.sas -> R translation
# Program id: P012  family: c2c
# Outputs: dataset ds -> ds.csv ; listing P012.lst (PROC PRINT obs=12)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed here but defined)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS CATX equivalent: strip and join with delimiter, skipping missing/empty
catx <- function(delim, ...) {
  args <- list(...)
  # vectorized version for data.frame columns
  # args are vectors same length
  n <- length(args[[1]])
  if (n == 0) return(character(0))
  result <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v))
      if (v == "") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) result[i] <- ""
    else result[i] <- paste(parts, collapse = delim)
  }
  result
}

# Helper: SAS UPCASE(STRIP())
upcase_strip <- function(x) {
  toupper(trimws(as.character(x)))
}

# Helper: SAS ANYDTDTE. input - try to parse various date formats
# In SAS, anydtdte. is very flexible. We emulate with lubridate parsing.
parse_anydtdte <- function(x) {
  # x is character vector
  # Return Date vector
  # Use lubridate::parse_date_time with multiple orders, plus as.Date fallback
  # Also handle SAS numeric dates? But here vtype check: if character then input, else numeric
  # For character, try to parse
  x_trim <- trimws(as.character(x))
  result <- rep(as.Date(NA), length(x_trim))
  # Identify non-missing
  idx <- !is.na(x_trim) & x_trim != ""
  if (any(idx)) {
    # Try lubridate orders
    # Common orders: ymd, mdy, dmy, ymd HMS etc.
    # Use parse_date_time with truncated
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_trim[idx],
      orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS",
                 "ymd HM", "mdy HM", "dmy HM",
                 "Ymd", "mdY", "dmY", "Y-m-d", "m/d/Y", "d/m/Y",
                 "Ymd HMS", "Y-m-d HMS", "m-d-Y", "d-m-Y",
                 "ymdT", "YmdT"),
      exact = FALSE, truncated = 3, quiet = TRUE
    ))
    # Convert to Date
    result[idx] <- as.Date(parsed)
    # For those still NA, try as.Date with ISO
    still_na <- is.na(result) & idx
    if (any(still_na)) {
      # Try as.Date directly (YYYY-MM-DD)
      try2 <- suppressWarnings(as.Date(x_trim[still_na], format = "%Y-%m-%d"))
      result[still_na] <- try2
      still_na2 <- is.na(result) & idx
      if (any(still_na2)) {
        try3 <- suppressWarnings(as.Date(x_trim[still_na2], format = "%m/%d/%Y"))
        result[still_na2] <- try3
      }
      still_na3 <- is.na(result) & idx
      if (any(still_na3)) {
        try4 <- suppressWarnings(as.Date(x_trim[still_na3], format = "%d/%m/%Y"))
        result[still_na3] <- try4
      }
      still_na4 <- is.na(result) & idx
      if (any(still_na4)) {
        # Try numeric string as SAS date? SAS date is days since 1960-01-01
        # But if character numeric, try as.numeric then convert?
        # We'll attempt to parse as numeric days
        num_vals <- suppressWarnings(as.numeric(x_trim[still_na4]))
        has_num <- !is.na(num_vals)
        if (any(has_num)) {
          # SAS origin 1960-01-01
          result[still_na4][has_num] <- as.Date(num_vals[has_num], origin = "1960-01-01")
        }
      }
    }
  }
  result
}

# Helper: SAS PUT(date, is8601da.) -> YYYY-MM-DD
put_is8601da <- function(d) {
  # d is Date
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# -------------------------------------------------------------------------
# Locate input file
# SAS: &project_path./data/raw/raw_demog.csv  where project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_demog.csv")
# Fallback: also check alternative locations if not found
if (!file.exists(raw_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv"
  if (file.exists(alt1)) raw_path <- alt1
}
if (!file.exists(raw_path)) {
  # Search recursively for raw_demog.csv under derived/c2c
  candidates <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_demog\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(candidates) > 0) raw_path <- candidates[1]
}

# Read raw_demog.csv
# SAS PROC IMPORT with GETNAMES=YES, DBMS=CSV
# SAS with validvarname=upcase forces uppercase names
# We will read and then uppercase column names
if (!file.exists(raw_path)) {
  stop(paste("Input file not found:", raw_path))
}

# Use readr for robust reading, but ensure we keep original types
# First try readr
raw_ds <- readr::read_csv(raw_path, show_col_types = FALSE, na = c("", "NA", "."))

# Uppercase variable names to mimic SAS validvarname=upcase
names(raw_ds) <- toupper(names(raw_ds))

# Ensure expected columns exist (case-insensitive check)
# Expected: SUBJ_ID, DISPOSITION_REASON, DISPOSITION, STUDY_END_DAT
# If names differ slightly, try to map
# Print progress
cat("Input file:", raw_path, "\n")
cat("Columns:", paste(names(raw_ds), collapse = ", "), "\n")
cat("Rows:", nrow(raw_ds), "\n")

# -------------------------------------------------------------------------
# Replicate SAS DATA step: sdtm.ds
# length STUDYID $8 DOMAIN $2 USUBJID $40 DSCAT DSSCAT $40 DSTERM DSDECOD $80 EPOCH $20 DSSTDTC $10;
# retain STUDYID DOMAIN USUBJID DSSEQ DSCAT DSSCAT DSTERM DSDECOD EPOCH DSSTDTC;
# set work.raw_ds;
# STUDYID = "CDISC-01";
# DOMAIN = "DS";
# USUBJID = catx("-", STUDYID, SUBJ_ID);
# DSSEQ = 1;
# DSCAT = "DISPOSITION EVENT";
# DSSCAT = "STUDY PARTICIPATION";
# DSTERM = upcase(strip(DISPOSITION_REASON));
# DSDECOD = DSTERM;
# if upcase(strip(DISPOSITION)) = "SCREEN FAILURE" then EPOCH = "SCREENING";
# else EPOCH = "FOLLOW-UP";
# if vtype(STUDY_END_DAT) = 'C' then _dsdt = input(strip(STUDY_END_DAT), anydtdte.);
# else _dsdt = STUDY_END_DAT;
# if not missing(_dsdt) then DSSTDTC = put(_dsdt, is8601da.);

# Check column existence and handle alternative naming
# SAS validvarname=upcase will have converted names to uppercase, but original CSV may have lower case
# We already uppercased

# Helper to get column vector safely
get_col <- function(df, colname) {
  if (colname %in% names(df)) return(df[[colname]])
  # case-insensitive fallback
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx) > 0) return(df[[idx[1]]])
  # try without underscore
  idx2 <- which(gsub("_", "", toupper(names(df))) == gsub("_", "", toupper(colname)))
  if (length(idx2) > 0) return(df[[idx2[1]]])
  return(rep(NA_character_, nrow(df)))
}

# Extract vectors
subj_id_vec <- get_col(raw_ds, "SUBJ_ID")
# Some files may have SUBJID or USUBJID
if (all(is.na(subj_id_vec))) {
  subj_id_vec <- get_col(raw_ds, "SUBJID")
}
if (all(is.na(subj_id_vec))) {
  subj_id_vec <- get_col(raw_ds, "SUBJECT_ID")
}
if (all(is.na(subj_id_vec))) {
  subj_id_vec <- get_col(raw_ds, "ID")
}

disposition_reason_vec <- get_col(raw_ds, "DISPOSITION_REASON")
disposition_vec <- get_col(raw_ds, "DISPOSITION")
study_end_dat_vec <- get_col(raw_ds, "STUDY_END_DAT")
# Alternative names
if (all(is.na(study_end_dat_vec))) {
  study_end_dat_vec <- get_col(raw_ds, "STUDY_END_DATE")
}
if (all(is.na(study_end_dat_vec))) {
  study_end_dat_vec <- get_col(raw_ds, "END_DATE")
}
if (all(is.na(study_end_dat_vec))) {
  study_end_dat_vec <- get_col(raw_ds, "DSSTDTC")
}

n <- nrow(raw_ds)

# Initialize output dataframe
sdtm_ds <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN = rep("DS", n),
  USUBJID = catx("-", rep("CDISC-01", n), as.character(subj_id_vec)),
  DSSEQ = rep(1L, n),
  DSCAT = rep("DISPOSITION EVENT", n),
  DSSCAT = rep("STUDY PARTICIPATION", n),
  DSTERM = upcase_strip(disposition_reason_vec),
  stringsAsFactors = FALSE
)
sdtm_ds$DSDECOD <- sdtm_ds$DSTERM

# EPOCH logic
disp_up <- upcase_strip(disposition_vec)
sdtm_ds$EPOCH <- ifelse(!is.na(disp_up) & disp_up == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")

# DSSTDTC logic: handle vtype
# In SAS, vtype checks if variable is character or numeric.
# In R, we need to detect type of study_end_dat_vec
# If it's character, parse with anydtdte.; if numeric, treat as SAS date or Excel date?
# Determine if original column was character or numeric
# readr may have parsed dates as Date; we need to handle both

# Check class of study_end_dat_vec
# If it's Date or POSIXt, convert directly
# If numeric, assume SAS date (days since 1960-01-01) or Excel date? Need to infer.
# Let's inspect raw file reading with col_types = "c" to see raw strings for type detection
raw_ds_char <- readr::read_csv(raw_path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
names(raw_ds_char) <- toupper(names(raw_ds_char))
study_end_dat_raw_char <- get_col(raw_ds_char, "STUDY_END_DAT")
if (all(is.na(study_end_dat_raw_char))) {
  study_end_dat_raw_char <- get_col(raw_ds_char, "STUDY_END_DATE")
}

# Determine vtype per observation? In SAS, vtype is per variable, not per observation.
# So if variable is character, all observations use character branch.
# We can infer variable type from raw_ds_char vs raw_ds
# If raw_ds_char contains only numeric strings that look like dates, but raw_ds parsed as Date, we still treat as character branch if original CSV had character dates.

# Simpler: replicate SAS logic:
# if vtype(STUDY_END_DAT) = 'C' then _dsdt = input(strip(STUDY_END_DAT), anydtdte.);
# else _dsdt = STUDY_END_DAT;
# So we need to know if STUDY_END_DAT column is character in SAS.
# PROC IMPORT with DBMS=CSV will guess type. If column contains dates like "2020-01-15" it will be character.
# If it contains SAS date numbers, it will be numeric.
# In R, we can check if study_end_dat_vec is character or numeric/Date.

# We'll implement: if study_end_dat_vec is character, parse with anydtdte.
# If it's Date/POSIX, use directly.
# If numeric, treat as SAS date (origin 1960-01-01) OR Excel origin? Need to decide.
# Check typical SAS date values: e.g., 20000 corresponds to 2014-10-06. Excel date 44000 corresponds to 2020-06-18.
# We can detect by range: if numeric values are > 30000 and < 50000, could be either.
# But SAS and Excel origins differ by 21916 days (Excel 1899-12-30 vs SAS 1960-01-01).
# We need to look at actual data to decide.

# Let's examine sample values
cat("STUDY_END_DAT sample (parsed):", paste(head(study_end_dat_vec, 5), collapse = " | "), "\n")
cat("STUDY_END_DAT sample (raw char):", paste(head(study_end_dat_raw_char, 5), collapse = " | "), "\n")
cat("Class of STUDY_END_DAT:", class(study_end_dat_vec), "\n")

# Now compute _dsdt
_dsdt <- rep(as.Date(NA), n)

# Determine if column is character type in SAS sense
# Use raw_ds_char to decide: if all non-missing values can be parsed as Date strings, then it's character
# If raw_ds_char values are numeric strings that are integers like "21000", PROC IMPORT might have made it numeric.
# We can check if readr parsed study_end_dat_vec as numeric/Date vs character

is_character_type <- is.character(study_end_dat_vec)

# However readr may have parsed dates as character anyway if mixed formats.
# So we treat is_character_type as TRUE if original column is character
# If is_character_type is TRUE, use anydtdte parsing
# Else if numeric, convert SAS numeric date to Date

if (is_character_type) {
  # Use raw char values for parsing to preserve original strings
  # But study_end_dat_vec already is character (same as raw char)
  _dsdt <- parse_anydtdte(study_end_dat_vec)
} else if (inherits(study_end_dat_vec, "Date")) {
  _dsdt <- study_end_dat_vec
} else if (inherits(study_end_dat_vec, "POSIXt")) {
  _dsdt <- as.Date(study_end_dat_vec)
} else if (is.numeric(study_end_dat_vec)) {
  # Numeric SAS date: days since 1960-01-01
  # Check if values look like Excel dates (origin 1899-12-30) vs SAS
  # We can try to infer by checking if converting with SAS origin yields plausible dates (e.g., 2010-2020)
  # For now assume SAS origin
  # If values are large (e.g., 44000) SAS origin would give 2080, which is implausible, so use Excel
  # Heuristic: if median numeric > 20000 and converting with SAS gives year > 2030, use Excel
  test_sas <- as.Date(study_end_dat_vec, origin = "1960-01-01")
  # Check year
  yr <- as.numeric(format(test_sas, "%Y"))
  # If any year > 2030 or < 1990, likely Excel
  if (any(!is.na(yr) & (yr > 2030 | yr < 1990))) {
    # Try Excel origin
    test_excel <- as.Date(study_end_dat_vec, origin = "1899-12-30")
    yr2 <- as.numeric(format(test_excel, "%Y"))
    # Choose which gives more plausible years (2000-2025)
    plausible_sas <- sum(!is.na(yr) & yr >= 2000 & yr <= 2025)
    plausible_excel <- sum(!is.na(yr2) & yr2 >= 2000 & yr2 <= 2025)
    if (plausible_excel > plausible_sas) {
      _dsdt <- test_excel
    } else {
      _dsdt <- test_sas
    }
  } else {
    _dsdt <- test_sas
  }
} else {
  # Fallback: try parsing as character
  _dsdt <- parse_anydtdte(as.character(study_end_dat_vec))
}

# DSSTDTC = put(_dsdt, is8601da.)
sdtm_ds$DSSTDTC <- put_is8601da(_dsdt)

# Keep only required columns in specified order
sdtm_ds <- sdtm_ds[, c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSCAT", "DSSCAT", "DSTERM", "DSDECOD", "EPOCH", "DSSTDTC")]

# Sort without deleting records: duplicate keys must remain visible to QC.
# proc sort data=sdtm.ds; by USUBJID DSSEQ; run;
# SAS sorts with character ordering (uppercase before lowercase); missing first
# R's order should mimic SAS: use base order with na.last = FALSE and case-sensitive
# For USUBJID, need to ensure SAS collation: uppercase before lowercase (ASCII order)
# R's default is also ASCII, so order() should match
# Use order with method = "radix" for stability
ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE)
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]
rownames(sdtm_ds) <- NULL

# -------------------------------------------------------------------------
# Write dataset ds.csv
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Create listing: DS Domain Audit - PROC PRINT obs=12
# Expected P012.lst (table/listing text, 108 numbers) -> write text rendering and CSV
# SAS PROC PRINT prints all variables with Obs number
# We need to reproduce text file with same numbers

# Prepare listing data: first 12 obs
listing_n <- min(12, nrow(sdtm_ds))
listing_df <- sdtm_ds[seq_len(listing_n), , drop = FALSE]
listing_df$Obs <- seq_len(listing_n)

# Reorder columns to match PROC PRINT order: Obs, then dataset variables in order
listing_df <- listing_df[, c("Obs", "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSCAT", "DSSCAT", "DSTERM", "DSDECOD", "EPOCH", "DSSTDTC")]

# Write CSV of listing body
write.csv(listing_df, file.path(out, "P012.csv"), row.names = FALSE, na = "")

# Write text rendering
# Create a plain-text table similar to SAS PROC PRINT output
txt_path <- file.path(out, "P012.txt")
# Also need P012.lst? The spec says P012.lst but we write P012.txt as per rules
# Rules say each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt"))
# Expected outputs list says P012.lst -> we will create both P012.txt and P012.lst for compatibility
lst_path <- file.path(out, "P012.lst")

# Build text content
# Title: "DS Domain Audit"
# Use fixed-width formatting

# Helper to format table as text
format_listing_text <- function(df, title) {
  # Create header line
  # Use capture of formatted table
  # We'll build a simple aligned text table
  # Determine column widths based on content
  cols <- names(df)
  # Compute widths: max of header and data lengths
  widths <- sapply(cols, function(cn) {
    vals <- as.character(df[[cn]])
    vals[is.na(vals)] <- ""
    max(nchar(c(cn, vals), type = "width"), na.rm = TRUE)
  })
  # Add padding
  widths <- widths + 2
  # Ensure minimum widths for readability
  # Build header
  header <- paste0(sprintf(paste0("%-", widths, "s"), cols), collapse = "")
  separator <- paste0(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "")
  lines <- c(
    title,
    "",
    header,
    separator
  )
  # Data rows
  for (i in seq_len(nrow(df))) {
    row_vals <- sapply(cols, function(cn) {
      v <- df[[cn]][i]
      if (is.na(v)) v <- ""
      as.character(v)
    })
    line <- paste0(sprintf(paste0("%-", widths, "s"), row_vals), collapse = "")
    lines <- c(lines, line)
  }
  lines <- c(lines, "", paste0("N = ", nrow(df), " (first ", nrow(df), " observations of ", nrow(sdtm_ds), " total)"))
  paste(lines, collapse = "\n")
}

txt_content <- format_listing_text(listing_df, "DS Domain Audit")

# Write to both .txt and .lst
writeLines(txt_content, txt_path)
writeLines(txt_content, lst_path)

# Also create a more SAS-like listing with Obs numbers and borders
# For completeness, also write a detailed log
cat("Done. Wrote ds.csv with", nrow(sdtm_ds), "rows\n")
cat("Wrote listing with", listing_n, "rows to", txt_path, "and", lst_path, "\n")
```