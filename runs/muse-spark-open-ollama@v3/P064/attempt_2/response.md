```r
# P064_adlb.sas translation to R
# Purpose: Create ADaM ADLB dataset based on CDISC
# SAS logic reproduced faithfully

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
  library(data.table)
  library(readxl)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: round half away from zero (SAS behavior) - not needed here but included
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: parse yymmdd10. strictly (SAS yymmdd10.)
parse_yymmdd10 <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  # SAS yymmdd10. reads first 10 chars as YYYY-MM-DD (with - / . or blank separators)
  # For this program we use strict YYYY-MM-DD as in CDISC pilot
  # Use substr 1,10 and as.Date with %Y-%m-%d
  res <- as.Date(substr(x_chr, 1, 10), format = "%Y-%m-%d")
  # Also handle NA strings
  res[x_chr == "" | is.na(x_chr)] <- NA
  res
}

# Helper: SAS missing comparison semantics
sas_lt <- function(a, b) {
  # a < b in SAS: missing < non-missing is TRUE, missing < missing is FALSE
  if (is.na(a) && !is.na(b)) return(TRUE)
  if (!is.na(a) && is.na(b)) return(FALSE)
  if (is.na(a) && is.na(b)) return(FALSE)
  return(a < b)
}
sas_eq <- function(a, b) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  return(a == b)
}

# Helper: find dataset file
find_dataset_file <- function(lib_paths, dsname) {
  exts <- c("sas7bdat", "xpt", "sas7bdat", "csv", "parquet", "rds")
  for (lp in lib_paths) {
    if (is.na(lp) || lp == "") next
    # normalize path: replace backslashes
    lp_norm <- gsub("\\\\", "/", lp)
    if (!dir.exists(lp_norm)) next
    files <- list.files(lp_norm, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
    if (length(files) == 0) next
    # match basename without ext
    for (f in files) {
      base <- basename(f)
      name_no_ext <- tools::file_path_sans_ext(base)
      ext <- tolower(tools::file_ext(base))
      if (tolower(name_no_ext) == tolower(dsname) && ext %in% exts) {
        return(f)
      }
    }
  }
  return(NA_character_)
}

# Helper: read dataset
read_dataset <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path)
  } else {
    # try sas
    df <- haven::read_sas(path)
  }
  # zap formats/labels but keep values
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  for (col in names(df)) {
    if (haven::is.labelled(df[[col]])) {
      df[[col]] <- haven::zap_labels(df[[col]])
    }
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  return(df)
}

# Helper: format dates for output
format_dates_for_output <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], "Date")) {
      df[[col]] <- format(df[[col]], "%Y-%m-%d")
    } else if (inherits(df[[col]], "POSIXct") || inherits(df[[col]], "POSIXt")) {
      df[[col]] <- format(df[[col]], "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(df[[col]], "POSIXlt")) {
      df[[col]] <- format(as.POSIXct(df[[col]]), "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

# Define possible library paths (absolute as in SAS context)
sdtm_lib_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm",
  "./data/derived/wxzhai/sdtm",
  "data/derived/wxzhai/sdtm",
  "../data/derived/wxzhai/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw",
  "./data/derived/wxzhai/raw"
)

adam_lib_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
  "./data/derived/wxzhai/adam",
  "data/derived/wxzhai/adam",
  "../data/derived/wxzhai/adam"
)

# Also add generic search root
generic_roots <- c(
  "D:/meta_hackathon/sas2r-agent/data",
  "./data",
  "data",
  "."
)

# Try to find sdtm.lb
lb_path <- find_dataset_file(sdtm_lib_paths, "lb")
if (is.na(lb_path)) {
  # fallback: search generic roots recursively for lb.*
  for (gr in generic_roots) {
    if (!dir.exists(gr)) next
    all_files <- list.files(gr, recursive = TRUE, full.names = TRUE)
    cand <- all_files[grepl("(^|/)lb\\.(sas7bdat|xpt|csv)$", all_files, ignore.case = TRUE)]
    # prefer sdtm folder
    if (length(cand) > 0) {
      # prioritize path containing sdtm
      sdtm_cand <- cand[grepl("sdtm", cand, ignore.case = TRUE)]
      if (length(sdtm_cand) > 0) lb_path <- sdtm_cand[1] else lb_path <- cand[1]
      break
    }
  }
}

# Try to find adam.adsl
adsl_path <- find_dataset_file(adam_lib_paths, "adsl")
if (is.na(adsl_path)) {
  for (gr in generic_roots) {
    if (!dir.exists(gr)) next
    all_files <- list.files(gr, recursive = TRUE, full.names = TRUE)
    cand <- all_files[grepl("(^|/)adsl\\.(sas7bdat|xpt|csv)$", all_files, ignore.case = TRUE)]
    if (length(cand) > 0) {
      adam_cand <- cand[grepl("adam", cand, ignore.case = TRUE)]
      if (length(adam_cand) > 0) adsl_path <- adam_cand[1] else adsl_path <- cand[1]
      break
    }
  }
}

cat("LB path:", lb_path, "\n")
cat("ADSL path:", adsl_path, "\n")

lb <- read_dataset(lb_path)
adsl <- read_dataset(adsl_path)

if (is.null(lb)) stop("Failed to read sdtm.lb from any known path")
if (is.null(adsl)) stop("Failed to read adam.adsl from any known path")

# Ensure required columns exist
stopifnot("USUBJID" %in% names(lb))
stopifnot("PARAM" %in% names(lb))
stopifnot("LBDTC" %in% names(lb))
stopifnot("AVAL" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("RFSTDTC" %in% names(adsl))

# Keep only needed columns from adsl (as in SAS keep=USUBJID TRT01P RFSTDTC)
# If TRT01P not present, keep what exists
adsl_keep_cols <- intersect(c("USUBJID", "TRT01P", "RFSTDTC"), names(adsl))
adsl_sub <- adsl[, adsl_keep_cols, drop = FALSE]

# Sort as SAS does: proc sort data=sdtm.lb; by USUBJID PARAM LBDTC
# Use order with na.last=FALSE to mimic SAS missing first, and character ordering
# Ensure character columns are character
lb$USUBJID <- as.character(lb$USUBJID)
lb$PARAM <- as.character(lb$PARAM)
lb$LBDTC <- as.character(lb$LBDTC)
adsl_sub$USUBJID <- as.character(adsl_sub$USUBJID)
if ("TRT01P" %in% names(adsl_sub)) adsl_sub$TRT01P <- as.character(adsl_sub$TRT01P)
adsl_sub$RFSTDTC <- as.character(adsl_sub$RFSTDTC)

# Sort lb
lb_sorted <- lb[order(lb$USUBJID, lb$PARAM, lb$LBDTC, na.last = FALSE), , drop = FALSE]
# Sort adsl
adsl_sorted <- adsl_sub[order(adsl_sub$USUBJID, na.last = FALSE), , drop = FALSE]

# Merge: data adam.adlb; merge sdtm.lb (in=a) adam.adsl (in=b keep=...) by USUBJID; if a and b;
# This is inner join by USUBJID
# Before merging, drop or rename every column present in both tables except keys, so no .x/.y suffixes
common_cols <- intersect(names(lb_sorted), names(adsl_sorted))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  # adsl is second in merge, so its values would overwrite lb's in SAS
  # But we keep lb's version and drop from adsl (except keep list already limited)
  # So remove common from adsl_sub if any remain
  adsl_sorted <- adsl_sorted[, setdiff(names(adsl_sorted), common_cols), drop = FALSE]
}

# Inner join
merged <- merge(lb_sorted, adsl_sorted, by = "USUBJID", all = FALSE, sort = FALSE)

# After merge, SAS retains order by USUBJID and within USUBJID by lb order (which was PARAM LBDTC)
# So re-sort merged by USUBJID PARAM LBDTC to replicate
merged <- merged[order(merged$USUBJID, merged$PARAM, merged$LBDTC, na.last = FALSE), , drop = FALSE]

# Prepare date vectors for comparison (SAS yymmdd10.)
merged$LBDTC_date <- parse_yymmdd10(merged$LBDTC)
merged$RFSTDTC_date <- parse_yymmdd10(merged$RFSTDTC)

# Initialize new variables
merged$BASE <- NA_real_
merged$ABLFL <- ""
merged$CHG <- NA_real_

# Retain logic: retain BASE; if first.PARAM then BASE=.; 
# SAS first.PARAM would be first row per USUBJID-PARAM group (since sort is by USUBJID PARAM LBDTC)
# Implement group-wise retain
n <- nrow(merged)
if (n > 0) {
  current_BASE <- NA_real_
  prev_USUBJID <- NA_character_
  prev_PARAM <- NA_character_
  
  for (i in seq_len(n)) {
    usubjid <- merged$USUBJID[i]
    param <- merged$PARAM[i]
    
    # Check first.PARAM: reset BASE at start of each USUBJID-PARAM group
    is_first_param <- FALSE
    if (i == 1) {
      is_first_param <- TRUE
    } else {
      # handle NA in keys: SAS missing sorts first, and first. logic treats missing as distinct
      usubjid_changed <- !identical(usubjid, prev_USUBJID) && !(is.na(usubjid) && is.na(prev_USUBJID))
      # More robust: compare with is.na handling
      if (is.na(usubjid) && is.na(prev_USUBJID)) usubjid_changed <- FALSE
      else if (is.na(usubjid) || is.na(prev_USUBJID)) usubjid_changed <- TRUE
      else usubjid_changed <- usubjid != prev_USUBJID
      
      if (is.na(param) && is.na(prev_PARAM)) param_changed <- FALSE
      else if (is.na(param) || is.na(prev_PARAM)) param_changed <- TRUE
      else param_changed <- param != prev_PARAM
      
      if (usubjid_changed || param_changed) is_first_param <- TRUE
    }
    
    if (is_first_param) {
      current_BASE <- NA_real_
    }
    
    lbdtc_date <- merged$LBDTC_date[i]
    rfstdtc_date <- merged$RFSTDTC_date[i]
    aval <- merged$AVAL[i]
    
    # Determine if LBDTC < RFSTDTC with SAS missing semantics
    is_pre_dose <- FALSE
    if (is.na(lbdtc_date) && !is.na(rfstdtc_date)) {
      is_pre_dose <- TRUE
    } else if (!is.na(lbdtc_date) && is.na(rfstdtc_date)) {
      is_pre_dose <- FALSE
    } else if (is.na(lbdtc_date) && is.na(rfstdtc_date)) {
      is_pre_dose <- FALSE
    } else {
      # both non-missing
      is_pre_dose <- lbdtc_date < rfstdtc_date
    }
    
    # Baseline = last non-missing pre-dose value
    if (is_pre_dose && !is.na(aval)) {
      current_BASE <- aval
    }
    
    # Baseline flag: if LBDTC < RFSTDTC and AVAL = BASE then ABLFL="Y" else ""
    # SAS equality includes missing==missing as true
    aval_eq_base <- FALSE
    if (is.na(aval) && is.na(current_BASE)) {
      aval_eq_base <- TRUE
    } else if (!is.na(aval) && !is.na(current_BASE) && aval == current_BASE) {
      aval_eq_base <- TRUE
    } else {
      aval_eq_base <- FALSE
    }
    
    if (is_pre_dose && aval_eq_base) {
      merged$ABLFL[i] <- "Y"
    } else {
      merged$ABLFL[i] <- ""
    }
    
    merged$BASE[i] <- current_BASE
    
    # Change from baseline
    if (!is.na(aval) && !is.na(current_BASE)) {
      merged$CHG[i] <- aval - current_BASE
    } else {
      merged$CHG[i] <- NA_real_
    }
    
    prev_USUBJID <- usubjid
    prev_PARAM <- param
  }
}

# Drop LBDTC RFSTDTC and helper date columns as in SAS drop LBDTC RFSTDTC
drop_cols <- c("LBDTC", "RFSTDTC", "LBDTC_date", "RFSTDTC_date")
merged <- merged[, setdiff(names(merged), drop_cols), drop = FALSE]

# Ensure column order: original lb columns (minus LBDTC) + TRT01P + BASE ABLFL CHG
# Already in that order due to merge; ensure BASE, ABLFL, CHG at end
# Reorder to put BASE, ABLFL, CHG last if not already
other_cols <- setdiff(names(merged), c("BASE", "ABLFL", "CHG"))
merged <- merged[, c(other_cols, "BASE", "ABLFL", "CHG"), drop = FALSE]

# Format dates for output (any remaining Date/POSIXct columns)
merged_out <- format_dates_for_output(merged)

# Write dataset as CSV
out_file <- file.path(out, "adlb.csv")
write.csv(merged_out, out_file, row.names = FALSE, na = "")

cat("ADLB dataset written to:", out_file, "with", nrow(merged_out), "rows and", ncol(merged_out), "cols\n")
print(head(merged_out))

# Also ensure output directory contains file
stopifnot(file.exists(out_file))
```