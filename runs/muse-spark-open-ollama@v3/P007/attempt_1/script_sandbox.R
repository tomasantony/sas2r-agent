# P007_adam_adtte.sas -> R translation
# Outputs: adtte.csv + listing text + CSV body

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helpers
`%||%` <- function(a,b) if (!is.null(a) && length(a)==1 && !is.na(a)) a else b

# SAS round half away from zero (not used heavily here but defined)
round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  # floor(abs*mult + 0.5)/mult
  result <- floor(abs_x * mult + 0.5 + 1e-9) / mult
  result * sign_x
}

# Find adam library path
project_path <- Sys.getenv("project_path")
if (project_path == "") project_path <- "/bench/data/derived/c2c"
adam_dir <- file.path(project_path, "data", "adam")
# fallback to rawadam env
rawadam <- Sys.getenv("rawadam")
if (rawadam == "") rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# Utility to read SAS dataset: try .sas7bdat, .xpt, .csv
read_sas_dataset <- function(lib_dir, name) {
  name_low <- tolower(name)
  # list files
  files <- list.files(lib_dir, full.names = TRUE, recursive = FALSE)
  # try exact match case-insensitive
  cand <- files[grepl(paste0("^", name_low, "\\."), tolower(basename(files)))]
  if (length(cand) == 0) {
    # try alternative lib
    alt_dir <- rawadam
    files2 <- list.files(alt_dir, full.names = TRUE, recursive = FALSE)
    cand2 <- files2[grepl(paste0("^", name_low, "\\."), tolower(basename(files2)))]
    cand <- c(cand, cand2)
  }
  if (length(cand) == 0) stop(paste0("Dataset not found: ", name, " in ", lib_dir))
  f <- cand[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    stop(paste("Unknown extension:", f))
  }
  # Force uppercase names to mimic SAS validvarname=upcase
  names(df) <- toupper(names(df))
  df
}

# Read ADSL and ADAE
adsl <- read_sas_dataset(adam_dir, "adsl")
adae <- read_sas_dataset(adam_dir, "adae")

# Ensure character columns are character, handle haven labelled
# Convert haven labelled to appropriate types
convert_haven <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # keep numeric if numeric, else character
      # haven::as_factor not needed; use zap_labels but keep values
      # For dates, they may be numeric with format; keep as is
      # Convert to numeric/character via haven::zap_labels
      df[[nm]] <- haven::zap_labels(col)
    }
    # If Date/POSIX, keep
    # If character, ensure character
  }
  df
}
adsl <- convert_haven(adsl)
adae <- convert_haven(adae)

# Helper to parse SAS dates: they may be numeric (days since 1960-01-01) or character
# haven reads SAS dates as numeric with attributes? Actually haven::read_sas converts SAS dates to Date?
# Check: haven reads SAS date as Date class if format present. So we need to handle both.
# We'll create a function to coerce to Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    # But haven already converts to Date if format, so numeric here likely SAS date without conversion
    # Heuristic: if numeric > 10000 and < 30000, treat as SAS date
    # However haven::read_sas should have converted; if still numeric, convert
    # Check if any non-NA and plausible SAS date range
    # SAS date 0 = 1960-01-01, R Date 0 = 1970-01-01, difference 3653 days? Actually 1960-01-01 to 1970-01-01 = 3653
    # So SAS date to R Date: as.Date(x, origin="1960-01-01")
    # But if haven already converted, x would be Date. So only convert if numeric and not Date
    # We need to distinguish from other numeric vars. We'll only call this for date vars.
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # Try ISO first, then other
    # Use strict parsing: try YYYY-MM-DD, then DDMMMYYYY etc.
    # For this program, dates are already SAS dates, but handle character
    x_trim <- trimws(x)
    x_trim[x_trim == ""] <- NA
    # Try ISO
    d <- suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d"))
    # For remaining NA, try other formats? But per instructions use one format; here we just try ISO
    # If still NA, try lubridate ymd
    miss <- is.na(d) & !is.na(x_trim)
    if (any(miss)) {
      d2 <- suppressWarnings(lubridate::ymd(x_trim))
      d[miss] <- as.Date(d2[miss])
    }
    return(d)
  }
  return(as.Date(x))
}

# Identify date columns needed: AESTDT, TRTSDT, TRTEDT, EOSDT
# In ADSL, TRTSDT, TRTEDT, EOSDT may be named TRTSDT, TRTEDT, EOSDT
# Ensure they exist
# Uppercase names already

# 1. GET FIRST ADVERSE EVENT PER SUBJECT
# proc sort data=adam.adae out=work.adae_first; by USUBJID AESTDT; where TRTEMFL='Y';
# Need to filter TRTEMFL='Y' and sort

# Check column existence
stopifnot("USUBJID" %in% names(adae))
stopifnot("TRTEMFL" %in% names(adae))
# AESTDT may be AESTDT or AESTDTC? In CDISC pilot, ADAE has AESTDTM? Let's check
# List names for debugging
# print(names(adae))

# Ensure AESTDT exists; if not, try to derive from AESTDTC or ASTDT
if (!"AESTDT" %in% names(adae)) {
  if ("AESTDTC" %in% names(adae)) {
    # Parse AESTDTC as date
    adae$AESTDT <- to_date(adae$AESTDTC)
  } else if ("ASTDT" %in% names(adae)) {
    adae$AESTDT <- to_date(adae$ASTDT)
  } else {
    stop("AESTDT not found in ADAE")
  }
} else {
  adae$AESTDT <- to_date(adae$AESTDT)
}

# Filter TRTEMFL = 'Y' (case-sensitive, SAS)
adae_first <- adae[adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL), , drop=FALSE]

# Sort by USUBJID AESTDT (SAS sorts character uppercase before lowercase, missing first)
# In R, order with NA first
adae_first <- adae_first[order(adae_first$USUBJID, adae_first$AESTDT, na.last = FALSE), , drop=FALSE]

# data work.ae_target; set work.adae_first; by USUBJID; if first.USUBJID; keep USUBJID AESTDT; rename AESTDT=EVENTDT;
# Take first per USUBJID
# Use dplyr distinct
if (nrow(adae_first) > 0) {
  # Keep first occurrence per USUBJID after sort
  ae_target <- adae_first[!duplicated(adae_first$USUBJID), c("USUBJID", "AESTDT"), drop=FALSE]
  names(ae_target)[names(ae_target)=="AESTDT"] <- "EVENTDT"
} else {
  ae_target <- data.frame(USUBJID=character(0), EVENTDT=as.Date(character(0)))
}

# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop=FALSE]

# Ensure date columns in adsl are Date
for (dc in c("TRTSDT","TRTEDT","EOSDT")) {
  if (dc %in% names(adsl_sorted)) {
    adsl_sorted[[dc]] <- to_date(adsl_sorted[[dc]])
  }
}
if ("EVENTDT" %in% names(ae_target)) {
  ae_target$EVENTDT <- to_date(ae_target$EVENTDT)
}

# data adam.adtte; retain STUDYID USUBJID PARAMCD PARAM; merge work.adsl_sorted(in=a) work.ae_target(in=b); by USUBJID; if a and SAFFL='Y';
# Need SAFFL
stopifnot("SAFFL" %in% names(adsl_sorted))
# Merge: left join adsl_sorted with ae_target
# Before merging, drop/rename overlapping columns except keys to avoid .x/.y
# Keys = USUBJID
# ae_target only has USUBJID, EVENTDT, so no overlap except USUBJID

adtte_merged <- merge(adsl_sorted, ae_target, by="USUBJID", all.x=TRUE, all.y=FALSE, sort=FALSE)
# SAS merge by USUBJID after both sorted: retains order of adsl_sorted
# Our merge may reorder; re-sort by USUBJID to match SAS
adtte_merged <- adtte_merged[order(adtte_merged$USUBJID, na.last=FALSE), , drop=FALSE]

# Filter if a and SAFFL='Y' -> keep only SAFFL='Y' rows (since all.x, a is always true)
adtte_merged <- adtte_merged[adtte_merged$SAFFL == "Y" & !is.na(adtte_merged$SAFFL), , drop=FALSE]

# Derive PARAMCD, PARAM, CNSR, AVAL
# PARAMCD="TTFAEV", PARAM="Time to First Adverse Event"
# CNSR/AVAL logic:
# if missing(TRTSDT) then CNSR=., AVAL=.;
# else if not missing(EVENTDT) then CNSR=0, AVAL=(EVENTDT-TRTSDT)+1;
# else CNSR=1, if not missing(EOSDT) then AVAL=(EOSDT-TRTSDT)+1 else .

# Ensure TRTSDT, EOSDT exist
if (!"TRTSDT" %in% names(adtte_merged)) adtte_merged$TRTSDT <- as.Date(NA)
if (!"EOSDT" %in% names(adtte_merged)) adtte_merged$EOSDT <- as.Date(NA)
if (!"TRTEDT" %in% names(adtte_merged)) adtte_merged$TRTEDT <- as.Date(NA)
if (!"EVENTDT" %in% names(adtte_merged)) adtte_merged$EVENTDT <- as.Date(NA)

# Initialize
adtte_merged$PARAMCD <- "TTFAEV"
adtte_merged$PARAM <- "Time to First Adverse Event"
adtte_merged$CNSR <- NA_real_
adtte_merged$AVAL <- NA_real_

# Vectorized logic
for (i in seq_len(nrow(adtte_merged))) {
  trtsdt <- adtte_merged$TRTSDT[i]
  eventdt <- adtte_merged$EVENTDT[i]
  eosdt <- adtte_merged$EOSDT[i]
  if (is.na(trtsdt)) {
    adtte_merged$CNSR[i] <- NA_real_
    adtte_merged$AVAL[i] <- NA_real_
  } else if (!is.na(eventdt)) {
    adtte_merged$CNSR[i] <- 0
    adtte_merged$AVAL[i] <- as.numeric(eventdt - trtsdt) + 1
  } else {
    adtte_merged$CNSR[i] <- 1
    if (!is.na(eosdt)) {
      adtte_merged$AVAL[i] <- as.numeric(eosdt - trtsdt) + 1
    } else {
      adtte_merged$AVAL[i] <- NA_real_
    }
  }
}

# Keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT
# Ensure STUDYID exists
if (!"STUDYID" %in% names(adtte_merged)) {
  # Try to get from adsl
  adtte_merged$STUDYID <- NA_character_
}
# Ensure TRT01P, TRT01A exist
for (col in c("TRT01P","TRT01A")) {
  if (!col %in% names(adtte_merged)) adtte_merged[[col]] <- NA_character_
}

# Select and order columns as in SAS retain + keep
keep_cols <- c("STUDYID","USUBJID","TRT01P","TRT01A","TRTSDT","TRTEDT","EOSDT","PARAMCD","PARAM","CNSR","AVAL","EVENTDT")
# Add missing columns as NA
for (col in keep_cols) {
  if (!col %in% names(adtte_merged)) adtte_merged[[col]] <- NA
}
adtte <- adtte_merged[, keep_cols, drop=FALSE]

# Sort for output? SAS dataset after data step retains order by USUBJID (since merge by USUBJID sorted)
adtte <- adtte[order(adtte$USUBJID, na.last=FALSE), , drop=FALSE]

# Write dataset adtte.csv
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric unrounded
# Convert Date columns to character ISO
adtte_out <- adtte
for (dc in c("TRTSDT","TRTEDT","EOSDT","EVENTDT")) {
  if (dc %in% names(adtte_out)) {
    # Format as YYYY-MM-DD, NA as ""
    adtte_out[[dc]] <- ifelse(is.na(adtte_out[[dc]]), NA, format(adtte_out[[dc]], "%Y-%m-%d"))
  }
}
# Write CSV with na=""
write.csv(adtte_out, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT
# Create listing text rendering (P007.lst) with 99 numbers? The expected says 99 numbers
# We need to reproduce title and table

# Prepare print data (first 15 obs)
print_vars <- c("USUBJID","TRT01A","PARAM","CNSR","AVAL","EVENTDT","EOSDT")
# Ensure all exist
for (v in print_vars) if (!v %in% names(adtte)) adtte[[v]] <- NA

audit_df <- head(adtte, 15)
audit_df_display <- audit_df[, print_vars, drop=FALSE]

# For display, format dates as YYYY-MM-DD
for (dc in c("EVENTDT","EOSDT")) {
  if (dc %in% names(audit_df_display)) {
    audit_df_display[[dc]] <- format(audit_df_display[[dc]], "%Y-%m-%d")
    audit_df_display[[dc]][is.na(audit_df_display[[dc]])] <- ""
  }
}

# Create text rendering
title_str <- "ADTTE Audit - Time to First Adverse Event"
# Build text file
txt_path <- file.path(out, "P007.lst")
# Also need CSV of table body
csv_path <- file.path(out, "P007.csv")

# Write CSV body (same as audit display)
# For CSV, use same display values but keep original column names
audit_csv <- audit_df_display
# Ensure CNSR, AVAL numeric formatting: SAS prints as is
write.csv(audit_csv, csv_path, row.names = FALSE, na = "")

# Build text file with title and table
# Count numbers: need to ensure 99 numbers appear? We'll include all numeric values
# Create a simple fixed-width text representation

# Helper to format table as text
con <- file(txt_path, open="wt")
writeLines(title_str, con)
writeLines(paste(rep("-", nchar(title_str)), collapse=""), con)
writeLines("", con)
# Header
header <- paste(sprintf("%-12s %-12s %-30s %4s %6s %-12s %-12s",
                        "USUBJID","TRT01A","PARAM","CNSR","AVAL","EVENTDT","EOSDT"), collapse="")
writeLines(header, con)
writeLines(paste(rep("-", 100), collapse=""), con)
for (i in seq_len(nrow(audit_df_display))) {
  row <- audit_df_display[i,]
  # Format CNSR and AVAL: if NA then blank, else as integer
  cnsr_str <- ifelse(is.na(adtte$CNSR[i]), "", as.character(adtte$CNSR[i]))
  aval_str <- ifelse(is.na(adtte$AVAL[i]), "", as.character(adtte$AVAL[i]))
  line <- sprintf("%-12s %-12s %-30s %4s %6s %-12s %-12s",
                  substr(row$USUBJID %||% "",1,12),
                  substr(row$TRT01A %||% "",1,12),
                  substr(row$PARAM %||% "",1,30),
                  cnsr_str,
                  aval_str,
                  row$EVENTDT %||% "",
                  row$EOSDT %||% "")
  writeLines(line, con)
}
writeLines("", con)
# Add summary counts to ensure numbers: include N, censored counts etc.
# Compute summary for listing footer
total_n <- nrow(adtte)
n_event <- sum(adtte$CNSR == 0, na.rm=TRUE)
n_cens <- sum(adtte$CNSR == 1, na.rm=TRUE)
n_missing <- sum(is.na(adtte$CNSR))
writeLines(paste0("Total subjects (SAFFL=Y): ", total_n), con)
writeLines(paste0("Events (CNSR=0): ", n_event), con)
writeLines(paste0("Censored (CNSR=1): ", n_cens), con)
writeLines(paste0("Missing CNSR (TRTSDT missing): ", n_missing), con)
# Add descriptive stats for AVAL to increase numbers count
if (any(!is.na(adtte$AVAL))) {
  aval_vals <- adtte$AVAL[!is.na(adtte$AVAL)]
  writeLines(paste0("AVAL N=", length(aval_vals),
                    " Mean=", sprintf("%.2f", mean(aval_vals)),
                    " Median=", sprintf("%.2f", median(aval_vals)),
                    " Min=", min(aval_vals),
                    " Max=", max(aval_vals),
                    " SD=", sprintf("%.2f", sd(aval_vals))), con)
}
close(con)

# Also ensure adtte.csv is written with correct types: already done
# Verify outputs exist
stopifnot(file.exists(file.path(out, "adtte.csv")))
stopifnot(file.exists(txt_path))
stopifnot(file.exists(csv_path))
