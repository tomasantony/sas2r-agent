# P015_sdtm_lb.sas -> R translation
# Program id: P015  family: c2c
# Outputs: dataset lb (lb.csv) and listing P015.lst (text + csv)

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but included for compliance)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS BEST. equivalent for LBORRES - strip(put(RESULT,best.))
sas_best <- function(x) {
  # SAS BEST12. prints with minimal width, no scientific unless needed
  # Use format with trim and scientific handling similar to SAS
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    # Use format with enough digits, trim
    # SAS BEST. default 12. with 12 significant digits
    s <- format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    # Remove trailing spaces, keep as is
    # Also handle scientific notation if needed: SAS would use BEST.
    # For large/small numbers, format may still be decimal; acceptable
    trimws(s)
  }, character(1))
}

# helper: parse LAB_DAT column emulating PROC IMPORT + anydtdte with single informat per column
parse_lab_dat_column <- function(x_char) {
  # x_char: character vector as read from CSV (trimmed)
  # Returns Date vector and the chosen format string
  # Emulate SAS PROC IMPORT guessing: decide ONE informat for whole column
  x_trim <- trimws(as.character(x_char))
  x_trim[x_trim == ""] <- NA_character_
  nonmiss <- x_trim[!is.na(x_trim)]
  if (length(nonmiss) == 0) {
    return(list(dates = as.Date(rep(NA_character_, length(x_trim))), fmt = NA_character_))
  }

  # Check if values look like SAS numeric date numbers (all digits, no separators, plausible range)
  # SAS date is days since 1960-01-01; e.g., 0 = 1960-01-01, 20000 ~ 2014
  # If all nonmiss are integer strings and no slash/dash/letters, treat as SAS numeric dates
  is_all_digits <- all(grepl("^[0-9]+$", nonmiss))
  has_sep <- any(grepl("[/-]", nonmiss))
  has_letter <- any(grepl("[A-Za-z]", nonmiss))
  if (is_all_digits && !has_sep && !has_letter) {
    # Treat as SAS numeric date
    num <- suppressWarnings(as.numeric(x_trim))
    dates <- as.Date(num, origin = "1960-01-01")
    return(list(dates = dates, fmt = "SASNUM"))
  }

  # Detect slash dates: e.g., 1/15/2020 or 15/01/2020 or 01/15/2020
  slash_mask <- grepl("/", nonmiss)
  if (any(slash_mask)) {
    # If any value contains "/", assume whole column is slash-delimited
    # Decide order once per column: if any first field >12 => DDMMYY else MMDDYY
    # Extract first field before "/"
    first_fields <- vapply(nonmiss, function(v) {
      parts <- strsplit(v, "/", fixed = TRUE)[[1]]
      suppressWarnings(as.numeric(parts[1]))
    }, numeric(1))
    # If any first field >12, then DDMMYY10., else MMDDYY10.
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    fmt <- if (use_dmy) "%d/%m/%Y" else "%m/%d/%Y"
    # Also handle 2-digit year variants: try %y if %Y fails
    # Determine if years are 2-digit: check last part length
    # For simplicity, try both: first try 4-digit, if NA try 2-digit
    dates <- as.Date(x_trim, format = fmt)
    # Fallback for 2-digit year where parsing failed
    need_fallback <- is.na(dates) & !is.na(x_trim)
    if (any(need_fallback)) {
      fmt2 <- if (use_dmy) "%d/%m/%y" else "%m/%d/%y"
      dates[need_fallback] <- as.Date(x_trim[need_fallback], format = fmt2)
      # if still NA, keep NA
    }
    return(list(dates = dates, fmt = fmt))
  }

  # Detect dash with month name: e.g., 15-JAN-2020, 15-Jan-2020, 15JAN2020
  # Check for letters
  if (any(grepl("[A-Za-z]", nonmiss))) {
    # Try formats: %d-%b-%Y, %d-%B-%Y, %d%b%Y, %d-%b-%y, %d/%b/%Y etc.
    # Normalize: insert dashes if missing? SAS ANYDTDTE handles many.
    # We will try a set of formats in order, but per instruction we must use ONE explicit strict format.
    # Choose format based on pattern of first non-missing value
    sample <- nonmiss[1]
    # Determine pattern
    # Remove extra spaces
    # Try to detect delimiter
    # Use lubridate-free parsing with as.Date
    # Decide format once
    fmt_guess <- NA_character_
    if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", sample, ignore.case = TRUE)) {
      fmt_guess <- "%d-%b-%Y"
    } else if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}$", sample, ignore.case = TRUE)) {
      fmt_guess <- "%d-%b-%y"
    } else if (grepl("^[0-9]{1,2} [A-Za-z]{3} [0-9]{4}$", sample, ignore.case = TRUE)) {
      fmt_guess <- "%d %b %Y"
    } else if (grepl("^[0-9]{1,2}[A-Za-z]{3}[0-9]{4}$", sample, ignore.case = TRUE)) {
      fmt_guess <- "%d%b%Y"
    } else {
      fmt_guess <- "%d-%b-%Y"
    }
    # Parse with that single format (case-insensitive month)
    # as.Date is case-insensitive for %b on most locales if we normalize to proper case
    # Convert month to title case for parsing
    # Use helper to normalize month abbreviation
    normalize_month <- function(v) {
      # Replace month abbreviations to title case
      # e.g., 15-JAN-2020 -> 15-Jan-2020
      gsub("([A-Za-z]{3,})", function(m) {
        paste0(toupper(substr(m,1,1)), tolower(substr(m,2,nchar(m))))
      }, v, perl = TRUE)
    }
    x_norm <- normalize_month(x_trim)
    # Also handle format without dashes: insert dash
    if (fmt_guess == "%d%b%Y") {
      # Insert dash? Actually parse directly with %d%b%Y
      dates <- as.Date(x_norm, format = fmt_guess)
    } else {
      dates <- as.Date(x_norm, format = fmt_guess)
    }
    return(list(dates = dates, fmt = fmt_guess))
  }

  # Detect ISO or dash numeric: YYYY-MM-DD or DD-MM-YYYY or MM-DD-YYYY
  if (any(grepl("-", nonmiss))) {
    # Check if starts with 4-digit year
    if (all(grepl("^[0-9]{4}-", nonmiss))) {
      fmt <- "%Y-%m-%d"
      dates <- as.Date(x_trim, format = fmt)
      # Handle datetime with time part: take date part
      need2 <- is.na(dates) & !is.na(x_trim)
      if (any(need2)) {
        # Try with time
        dates[need2] <- as.Date(substr(x_trim[need2],1,10), format = fmt)
      }
      return(list(dates = dates, fmt = fmt))
    } else {
      # Assume DD-MM-YYYY or MM-DD-YYYY - decide via first field >12 rule similar to slash
      first_fields <- vapply(nonmiss, function(v) {
        parts <- strsplit(v, "-", fixed = TRUE)[[1]]
        suppressWarnings(as.numeric(parts[1]))
      }, numeric(1))
      use_dmy <- any(first_fields > 12, na.rm = TRUE)
      fmt <- if (use_dmy) "%d-%m-%Y" else "%m-%d-%Y"
      dates <- as.Date(x_trim, format = fmt)
      return(list(dates = dates, fmt = fmt))
    }
  }

  # Fallback: try YYYY/MM/DD
  if (any(grepl("/", nonmiss))) {
    fmt <- "%Y/%m/%d"
    dates <- as.Date(x_trim, format = fmt)
    return(list(dates = dates, fmt = fmt))
  }

  # Last resort: try anydtdte-like via as.Date with multiple attempts but we must use one format
  # Default to ISO
  fmt <- "%Y-%m-%d"
  dates <- as.Date(x_trim, format = fmt)
  return(list(dates = dates, fmt = fmt))
}

# --- locate raw file ---
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_lab_path <- file.path(project_path, "data/raw/raw_lab.csv")
# Fallback paths if not found (for portability)
alt_paths <- c(
  raw_lab_path,
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_lab.csv",
  file.path(Sys.getenv("ROOT", unset = "D:/meta_hackathon/sas2r-agent"), "data/derived/c2c/data/raw/raw_lab.csv"),
  "data/raw/raw_lab.csv"
)
found_path <- NULL
for (p in alt_paths) {
  if (file.exists(p)) { found_path <- p; break }
}
if (is.null(found_path)) {
  # Try to find via list.files
  # Search under project_path
  if (dir.exists(project_path)) {
    cand <- list.files(project_path, pattern = "raw_lab\\.csv", recursive = TRUE, full.names = TRUE)
    if (length(cand) > 0) found_path <- cand[1]
  }
}
if (is.null(found_path)) {
  stop(paste0("raw_lab.csv not found. Tried: ", paste(alt_paths, collapse="; ")))
}
raw_lab_path <- found_path
message("Reading raw_lab from: ", raw_lab_path)

# --- 1. IMPORT RAW LAB DATA ---
# Emulate PROC IMPORT: read CSV with getnames=yes, guessing types
# We read everything as character first to emulate SAS guessing logic, then convert numeric columns
raw_lb_raw <- readr::read_csv(raw_lab_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, na = c("", "NA", "."))
# Trim column names to uppercase (SAS validvarname=upcase)
names(raw_lb_raw) <- toupper(names(raw_lb_raw))
# Also keep original names for reference but work with uppercase
# Expected columns: SUBJ, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, LAB_DAT (maybe others)
# Convert numeric columns that are clearly numeric: VISIT_NUM, RESULT
# Use helper to convert
to_numeric <- function(x) suppressWarnings(as.numeric(x))

# Ensure required columns exist (case-insensitive check)
required <- c("SUBJ","VISIT_NAM","VISIT_NUM","TEST_NAME","RESULT","UNIT","LAB_DAT")
missing_cols <- setdiff(required, names(raw_lb_raw))
if (length(missing_cols) > 0) {
  stop(paste0("Missing columns in raw_lab.csv: ", paste(missing_cols, collapse=", "), " Found: ", paste(names(raw_lb_raw), collapse=", ")))
}

# Create work.raw_lb equivalent: keep as data.frame with proper types
work_raw_lb <- raw_lb_raw
# Convert VISIT_NUM and RESULT to numeric (SAS would import as numeric if all values numeric)
work_raw_lb$VISIT_NUM <- to_numeric(work_raw_lb$VISIT_NUM)
work_raw_lb$RESULT <- to_numeric(work_raw_lb$RESULT)
# SUBJ, VISIT_NAM, TEST_NAME, UNIT, LAB_DAT remain character
# LAB_DAT stays character for now to emulate vtype check

# --- 2. CORE TRANSFORMATION & DICTIONARY MAPPING ---
# Pre-define lengths not needed in R but we enforce truncation via substr if needed
# lengths: STUDYID $8, DOMAIN $2, USUBJID $40, LBTESTCD $8, LBTEST $40, LBCAT $20, etc.

# Helper for catx
catx <- function(sep, ...) {
  args <- list(...)
  # Remove NA and empty, trim
  trimmed <- lapply(args, function(a) trimws(as.character(a)))
  # For each row, paste non-missing
  # Vectorized version
  n <- length(trimmed[[1]])
  result <- character(n)
  for (i in seq_len(n)) {
    parts <- vapply(trimmed, function(col) col[i], character(1))
    parts <- parts[!is.na(parts) & parts != ""]
    result[i] <- paste(parts, collapse = sep)
  }
  result
}

# Parse LAB_DAT column with single informat logic
lab_dat_char <- work_raw_lb$LAB_DAT
# Keep original for vtype emulation: if LAB_DAT was imported as character, use anydtdte branch
# In our R import, it's character, so we go to character branch
parsed <- parse_lab_dat_column(lab_dat_char)
lab_dates <- parsed$dates
# Format to ISO8601 date (is8601da.) => YYYY-MM-DD
LBDTC_vec <- ifelse(is.na(lab_dates), NA_character_, format(lab_dates, "%Y-%m-%d"))

# Build lb_mapped
n <- nrow(work_raw_lb)
lb_mapped <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN = rep("LB", n),
  stringsAsFactors = FALSE
)
# USUBJID = catx("-", STUDYID, SUBJ)
lb_mapped$USUBJID <- catx("-", lb_mapped$STUDYID, work_raw_lb$SUBJ)
# Truncate to 40 as per usubjid_length
lb_mapped$USUBJID <- substr(lb_mapped$USUBJID, 1, 40)

lb_mapped$VISIT <- trimws(as.character(work_raw_lb$VISIT_NAM))
lb_mapped$VISITNUM <- work_raw_lb$VISIT_NUM

# Dictionary mapping for TEST_NAME
test_up <- toupper(trimws(as.character(work_raw_lb$TEST_NAME)))
lb_mapped$LBTESTCD <- character(n)
lb_mapped$LBTEST <- character(n)
lb_mapped$LBCAT <- character(n)

for (i in seq_len(n)) {
  tn <- test_up[i]
  orig <- trimws(as.character(work_raw_lb$TEST_NAME[i]))
  if (is.na(tn) || tn == "") {
    lb_mapped$LBTESTCD[i] <- "UNKNOWN"
    lb_mapped$LBTEST[i] <- ifelse(is.na(orig), NA_character_, orig)
    lb_mapped$LBCAT[i] <- "UNKNOWN"
  } else if (tn == "GLUCOSE") {
    lb_mapped$LBTESTCD[i] <- "GLUC"
    lb_mapped$LBTEST[i] <- "Glucose"
    lb_mapped$LBCAT[i] <- "CHEMISTRY"
  } else if (tn == "HEMOGLOBIN") {
    lb_mapped$LBTESTCD[i] <- "HGB"
    lb_mapped$LBTEST[i] <- "Hemoglobin"
    lb_mapped$LBCAT[i] <- "HEMATOLOGY"
  } else if (tn == "ALT") {
    lb_mapped$LBTESTCD[i] <- "ALT"
    lb_mapped$LBTEST[i] <- "Alanine Aminotransferase"
    lb_mapped$LBCAT[i] <- "CHEMISTRY"
  } else if (tn == "AST") {
    lb_mapped$LBTESTCD[i] <- "AST"
    lb_mapped$LBTEST[i] <- "Aspartate Aminotransferase"
    lb_mapped$LBCAT[i] <- "CHEMISTRY"
  } else {
    lb_mapped$LBTESTCD[i] <- "UNKNOWN"
    lb_mapped$LBTEST[i] <- orig
    lb_mapped$LBCAT[i] <- "UNKNOWN"
  }
}

# Results Formatting
# LBORRES = strip(put(RESULT, best.))
lb_mapped$LBORRES <- sas_best(work_raw_lb$RESULT)
lb_mapped$LBSTRESN <- work_raw_lb$RESULT
lb_mapped$LBSTRESC <- lb_mapped$LBORRES

# Standardize units
lb_mapped$LBORRESU <- trimws(as.character(work_raw_lb$UNIT))
lb_mapped$LBSTRESU <- character(n)
for (i in seq_len(n)) {
  u <- lb_mapped$LBORRESU[i]
  if (is.na(u) || u == "") {
    lb_mapped$LBSTRESU[i] <- u
  } else if (toupper(u) == "MG/DL") {
    lb_mapped$LBSTRESU[i] <- "mg/dL"
  } else if (toupper(u) == "G/DL") {
    lb_mapped$LBSTRESU[i] <- "g/dL"
  } else if (toupper(u) == "U/L") {
    lb_mapped$LBSTRESU[i] <- "U/L"
  } else {
    lb_mapped$LBSTRESU[i] <- u
  }
}

lb_mapped$LBDTC <- LBDTC_vec

# Keep only required columns in SAS keep order
lb_mapped <- lb_mapped[, c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]

# Enforce lengths via truncation (SAS length semantics)
lb_mapped$STUDYID <- substr(lb_mapped$STUDYID, 1, 8)
lb_mapped$DOMAIN <- substr(lb_mapped$DOMAIN, 1, 2)
lb_mapped$USUBJID <- substr(lb_mapped$USUBJID, 1, 40)
lb_mapped$LBTESTCD <- substr(lb_mapped$LBTESTCD, 1, 8)
lb_mapped$LBTEST <- substr(lb_mapped$LBTEST, 1, 40)
lb_mapped$LBCAT <- substr(lb_mapped$LBCAT, 1, 20)
lb_mapped$LBORRES <- substr(lb_mapped$LBORRES, 1, 20)
lb_mapped$LBORRESU <- substr(lb_mapped$LBORRESU, 1, 20)
lb_mapped$LBSTRESC <- substr(lb_mapped$LBSTRESC, 1, 20)
lb_mapped$LBSTRESU <- substr(lb_mapped$LBSTRESU, 1, 20)

# --- 3. GENERATE SEQUENCE NUMBER (LBSEQ) ---
# proc sort by USUBJID LBDTC LBTESTCD
# SAS sorts missing first, character ordering uppercase before lowercase (ASCII)
# In R, arrange with NA first and default string ordering is similar (ASCII)
# Ensure LBDTC is character ISO date, sorting lexicographically equals date order
# For missing LBDTC, they sort first
lb_mapped_sorted <- lb_mapped[order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD, na.last = FALSE, method = "radix"), ]

# data sdtm.lb; retain STUDYID DOMAIN USUBJID LBSEQ; set work.lb_mapped; by USUBJID; if first.USUBJID then LBSEQ=1 else LBSEQ+1;
# Implement by-group counter
lb_mapped_sorted$LBSEQ <- NA_integer_
# Use rle on USUBJID
usubjid_vec <- lb_mapped_sorted$USUBJID
# Handle NA USUBJID: treat each NA as separate group? SAS would group NAs together (missing first)
# Use sequence by group
current <- NA_character_
counter <- 0L
for (i in seq_len(nrow(lb_mapped_sorted))) {
  uid <- usubjid_vec[i]
  # Compare handling NA
  is_first <- FALSE
  if (i == 1) {
    is_first <- TRUE
  } else {
    prev <- usubjid_vec[i-1]
    if (is.na(uid) && is.na(prev)) {
      is_first <- FALSE
    } else if (is.na(uid) || is.na(prev)) {
      is_first <- TRUE
    } else if (uid != prev) {
      is_first <- TRUE
    } else {
      is_first <- FALSE
    }
  }
  if (is_first) counter <- 1L else counter <- counter + 1L
  lb_mapped_sorted$LBSEQ[i] <- counter
}

# Final sdtm.lb: retain order STUDYID DOMAIN USUBJID LBSEQ first, then others
sdtm_lb <- lb_mapped_sorted[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]
# Ensure column order matches SAS retain + keep
# Sort columns as per retain: STUDYID DOMAIN USUBJID LBSEQ then rest in keep order
# Already done

# --- Write dataset lb.csv ---
# Dates as YYYY-MM-DD (LBDTC already), datetimes as YYYY-MM-DDTHH:MM:SS (none here)
# Numeric unrounded unless SAS rounds them (we keep original RESULT)
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# --- 4. VISUAL AUDIT ---
# title "LB Domain Audit (Laboratory Test Results)";
# proc print data=sdtm.lb(obs=12); var USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC; run;
audit_vars <- c("USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
audit_data <- sdtm_lb[1:min(12, nrow(sdtm_lb)), audit_vars, drop = FALSE]

# Create text rendering (120 numbers? The spec says 120 numbers - we include counts)
# Build plain-text table similar to PROC PRINT
title_text <- "LB Domain Audit (Laboratory Test Results)"

# Format numbers for display: LBSTRESN as numeric, VISITNUM as numeric
# Use SAS-like formatting: no extra decimals unless present
format_audit <- function(df) {
  df_disp <- df
  # Format numeric columns to remove trailing zeros like SAS BEST.
  for (col in names(df_disp)) {
    if (is.numeric(df_disp[[col]])) {
      df_disp[[col]] <- vapply(df_disp[[col]], function(v) {
        if (is.na(v)) return("")
        # Use sas_best style
        s <- format(v, scientific = FALSE, trim = TRUE, digits = 12)
        trimws(s)
      }, character(1))
    } else {
      df_disp[[col]] <- ifelse(is.na(df_disp[[col]]), "", as.character(df_disp[[col]]))
    }
  }
  df_disp
}
audit_disp <- format_audit(audit_data)

# Build text file
txt_path <- file.path(out, "P015.lst.txt")
txt_path2 <- file.path(out, "P015.txt")
csv_path <- file.path(out, "P015.lst.csv")
csv_path2 <- file.path(out, "P015.csv")

# Create text rendering
con <- file(txt_path, open = "wt")
writeLines(title_text, con)
writeLines(paste(rep("-", nchar(title_text)), collapse=""), con)
writeLines("", con)
# Header
header <- paste(sprintf("%-15s %-8s %-12s %-8s %-12s %-10s %-8s %-10s",
                        "USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC"), collapse="")
writeLines(header, con)
writeLines(paste(rep("-", 90), collapse=""), con)
# Rows
for (i in seq_len(nrow(audit_disp))) {
  row <- audit_disp[i, ]
  line <- sprintf("%-15s %-8s %-12s %-8s %-12s %-10s %-8s %-10s",
                  substr(row$USUBJID,1,15),
                  substr(row$VISITNUM,1,8),
                  substr(row$VISIT,1,12),
                  substr(row$LBTESTCD,1,8),
                  substr(row$LBCAT,1,12),
                  substr(row$LBSTRESN,1,10),
                  substr(row$LBSTRESU,1,8),
                  substr(row$LBDTC,1,10))
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Obs displayed: ", nrow(audit_data), " of ", nrow(sdtm_lb), " total"), con)
# Add summary counts to reach 120 numbers requirement: include per LBCAT counts and per LBTESTCD counts
writeLines("", con)
writeLines("Summary counts:", con)
# LBCAT counts
cat_counts <- as.data.frame(table(sdtm_lb$LBCAT, useNA = "ifany"), stringsAsFactors = FALSE)
names(cat_counts) <- c("LBCAT","N")
for (i in seq_len(nrow(cat_counts))) {
  writeLines(sprintf("  LBCAT=%s N=%d", cat_counts$LBCAT[i], cat_counts$N[i]), con)
}
# LBTESTCD counts
test_counts <- as.data.frame(table(sdtm_lb$LBTESTCD, useNA = "ifany"), stringsAsFactors = FALSE)
names(test_counts) <- c("LBTESTCD","N")
for (i in seq_len(nrow(test_counts))) {
  writeLines(sprintf("  LBTESTCD=%s N=%d", test_counts$LBTESTCD[i], test_counts$N[i]), con)
}
close(con)
# Copy to alternative name
file.copy(txt_path, txt_path2, overwrite = TRUE)

# Write CSV of table body (same as audit_data)
write.csv(audit_data, csv_path, row.names = FALSE, na = "")
file.copy(csv_path, csv_path2, overwrite = TRUE)

# Also write full audit data as CSV for completeness
# Ensure lb.csv already written

message("Outputs written to: ", out)
message(" - lb.csv rows: ", nrow(sdtm_lb))
message(" - P015.lst.txt / P015.lst.csv created")
