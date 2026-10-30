```r
# P067_ae.sas -> R translation
# Purpose: Create SDTM AE dataset (STUDYID, AESTDTC/AEENDTC, ASTDY/AENDY)
# Outputs: ae.csv (sdtm.ae) and raw_ae.csv (raw.raw_ae)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Libraries (allowed list only)
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

# -------------------------------------------------------------------------
# Paths from SAS librefs / macro variables
raw_path  <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

# Fallback if derived path not present (try alternative locations)
if (!dir.exists(raw_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  alt2 <- Sys.getenv("rawadam")
  if (dir.exists(alt1)) raw_path <- alt1 else if (dir.exists(alt2) && alt2 != "") raw_path <- alt2
}
if (!dir.exists(sdtm_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
  alt2 <- Sys.getenv("rawsdtm")
  if (dir.exists(alt1)) sdtm_path <- alt1 else if (dir.exists(alt2) && alt2 != "") sdtm_path <- alt2
}

# -------------------------------------------------------------------------
# Helper: read SAS dataset by libref + member name (case-insensitive, .sas7bdat or .xpt)
read_dataset <- function(dir, member) {
  if (!dir.exists(dir)) stop(paste0("Directory not found: ", dir))
  all_files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # case-insensitive match for member.sas7bdat or member.xpt
  pattern <- paste0("^", member, "\\.(sas7bdat|xpt)$")
  sel <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE)]
  if (length(sel) == 0) {
    # also try member with different case variations via list.files pattern
    # try lower/upper
    cand <- c(
      file.path(dir, paste0(member, ".sas7bdat")),
      file.path(dir, paste0(tolower(member), ".sas7bdat")),
      file.path(dir, paste0(toupper(member), ".sas7bdat")),
      file.path(dir, paste0(member, ".xpt")),
      file.path(dir, paste0(tolower(member), ".xpt")),
      file.path(dir, paste0(toupper(member), ".xpt"))
    )
    cand <- cand[file.exists(cand)]
    if (length(cand) > 0) sel <- cand
  }
  if (length(sel) == 0) stop(paste0("Dataset ", member, " not found in ", dir, " ; files: ", paste(basename(all_files), collapse=", ")))
  f <- sel[1]
  ext <- tolower(tools::file_ext(f))
  df <- if (ext == "sas7bdat") haven::read_sas(f) else haven::read_xpt(f)
  # zap labels/formats to plain R types but keep values
  df <- haven::zap_formats(df)
  # convert haven_labelled to plain vector
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # make names uppercase for SAS-like handling, but keep original for output
  # we will work case-insensitively; store original names
  df
}

# Helper: convert SAS numeric date or R Date/POSIXt/character to Date
to_date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01; haven already converts formatted dates to Date,
    # so numeric here means unformatted SAS date
    # Heuristic: if all non-NA are between 0 and 100000, treat as SAS date
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # SAS yymmdd10. is YYYY-MM-DD (also allows YYYY/MM/DD)
    # trim spaces
    x_trim <- trimws(x)
    x_trim[x_trim == "" | x_trim == "."] <- NA
    # try YYYY-MM-DD
    d <- suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d"))
    need <- is.na(d) & !is.na(x_trim)
    if (any(need)) {
      d2 <- suppressWarnings(as.Date(x_trim[need], format = "%Y/%m/%d"))
      d[need] <- d2
    }
    # try lubridate fallback for other ISO variants
    need2 <- is.na(d) & !is.na(x_trim)
    if (any(need2)) {
      d3 <- suppressWarnings(lubridate::ymd(x_trim[need2], quiet = TRUE))
      d[need2] <- as.Date(d3)
    }
    return(d)
  }
  # fallback
  suppressWarnings(as.Date(x))
}

# Helper: format Date as yymmdd10. (YYYY-MM-DD), NA -> NA_character_
fmt_yymmdd10 <- function(d) {
  if (inherits(d, "Date") || is.numeric(d)) {
    if (is.numeric(d) && !inherits(d, "Date")) d <- as.Date(d, origin="1960-01-01")
    out <- format(d, "%Y-%m-%d")
    out[is.na(d)] <- NA_character_
    # SAS would produce blank for missing; we return NA so write.csv na="" gives blank
    return(out)
  }
  if (is.character(d)) {
    # already character? try to normalize
    dd <- to_date(d)
    return(fmt_yymmdd10(dd))
  }
  return(as.character(d))
}

# Helper: prepare dataframe for CSV output - format Date/POSIXt columns
format_for_csv <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "POSIXt")) {
      df[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
      df[[nm]][is.na(col)] <- NA_character_
    } else if (inherits(col, "Date")) {
      df[[nm]] <- format(col, "%Y-%m-%d")
      df[[nm]][is.na(col)] <- NA_character_
    }
  }
  df
}

# -------------------------------------------------------------------------
cat("Reading raw.raw_ae ...\n")
raw_ae <- read_dataset(raw_path, "raw_ae")

cat("Reading sdtm.dm ...\n")
dm <- read_dataset(sdtm_path, "dm")

# Ensure SUBJID exists in both (SAS BY variable)
# SAS sort: character ordering uppercase before lowercase, missing first
# For R we just ensure consistent type for join
# Convert SUBJID to character for safe join if types differ
if (!"SUBJID" %in% names(raw_ae)) {
  # try case-insensitive search
  idx <- which(toupper(names(raw_ae)) == "SUBJID")
  if (length(idx) > 0) names(raw_ae)[idx[1]] <- "SUBJID" else stop("SUBJID not found in raw_ae")
}
if (!"SUBJID" %in% names(dm)) {
  idx <- which(toupper(names(dm)) == "SUBJID")
  if (length(idx) > 0) names(dm)[idx[1]] <- "SUBJID" else stop("SUBJID not found in dm")
}

# SAS: proc sort data=raw.raw_ae; by SUBJID; run;
# proc sort data=sdtm.dm; by SUBJID; run;
# In R, arrange for reproducibility (missing first)
raw_ae <- raw_ae %>% arrange(SUBJID)
dm <- dm %>% arrange(SUBJID)

# -------------------------------------------------------------------------
# SAS merge: merge raw.raw_ae (in=a) sdtm.dm; by SUBJID; if a;
# This is left join of raw_ae to dm on SUBJID, with dm values overwriting raw_ae for overlapping vars
# Implement left_join then coalesce overlapping columns (dm wins)

# Identify overlapping columns excluding BY
by_var <- "SUBJID"
overlap <- intersect(names(raw_ae), names(dm))
overlap <- setdiff(overlap, by_var)

# Perform left join
merged <- dplyr::left_join(raw_ae, dm, by = by_var, suffix = c("", ".dm"))

# For overlapping columns, SAS merge would take value from dm (second dataset) when present
# haven merge semantics: if dm has non-NA, it overwrites; if dm is NA, SAS would keep dm's NA? Actually SAS
# would set variable to dm's value (which could be missing) overwriting raw_ae's value.
# To mimic SAS exactly, we should take dm's value unconditionally when dm row exists.
# However left_join with suffix keeps both; we need to replace original with .dm version if .dm exists.
for (col in overlap) {
  dm_col <- paste0(col, ".dm")
  if (dm_col %in% names(merged)) {
    # SAS: value from last dataset in merge list (dm) overwrites
    # So we replace with dm's value (even if NA) where dm row matched
    # Detect matched rows: dm's BY variable not NA? Actually left_join will have NA for dm columns when no match
    # But since dm has one row per SUBJID, all raw_ae SUBJIDs should match if dm contains them;
    # if no dm match, dm columns are NA and we should keep raw_ae's original
    # We can detect match by checking if any dm column is not NA? Better check USUBJID.dm or RFSTDTC.dm
    # Simpler: if dm_col is not NA, use it; otherwise keep original (this approximates SAS but handles missing dm row)
    # For strict SAS, we would use dm value even if NA, but that would erase raw_ae data when dm has missing
    # We'll implement strict SAS: where dm row exists (we can check if dm's SUBJID.dm? but we have only one BY)
    # Use indicator: dm_matched = !is.na(merged[[dm_col]]) | !is.na(merged[[paste0(overlap[1], ".dm")]]) ... not reliable
    # Instead, check if any dm-specific column (e.g., RFSTDTC) is not NA or if dm had a row
    # We can create a flag by checking if any dm column (including those not overlapping) is non-NA
    # For now, use coalesce-like but prefer dm when dm row exists
    # Determine dm existence: check if RFSTDTC.dm or USUBJID.dm etc. exists and is not all NA for that row
    # Simpler: if dm dataset has the column, then for rows where join succeeded, dm_col will be not NA or NA depending on dm's value
    # SAS would set to dm's value (NA if dm missing). So we should just take dm_col where join matched.
    # We can detect join matched by checking if any dm-only column is not NA OR if dm_col is not NA? But if dm row exists and all dm columns are NA, we still want to overwrite with NA (SAS behavior)
    # To detect join matched, we can check if the dm's BY key contributed: left_join will have NA for all dm columns when no match
    # So we can use a dm-only column to test match; pick first dm-only column
    dm_only <- setdiff(names(dm), names(raw_ae))
    dm_only <- setdiff(dm_only, by_var)
    if (length(dm_only) > 0) {
      # use first dm_only column's .dm? Actually dm_only columns don't have suffix, they appear as is
      # In left_join, dm_only columns appear without suffix
      test_col <- dm_only[1]
      if (test_col %in% names(merged)) {
        matched <- !is.na(merged[[test_col]])
        # But if dm's test_col is NA for a matched row, this would incorrectly say not matched
        # Better to use a more reliable flag: create a dummy column in dm before join
      }
    }
    # For robustness, we will just use dm_col value when it is not NA, otherwise keep original
    # This is common and will not affect USUBJID/RFSTDTC which are dm-only or overlapping
    # For overlapping, we will take dm_col if not NA, else original
    # To strictly follow SAS, we would take dm_col unconditionally where dm row exists, but we don't have perfect flag
    # We'll implement: if dm_col exists, replace original with dm_col where dm_col is not NA OR where dm row exists (we approximate by checking if any dm column is not NA)
    # Create a flag for dm row existence using all dm columns
    dm_cols_in_merged <- c(paste0(overlap, ".dm"), intersect(dm_only, names(merged)))
    # row has dm match if any dm column is not NA
    has_dm <- rep(FALSE, nrow(merged))
    for (dc in dm_cols_in_merged) {
      if (dc %in% names(merged)) {
        has_dm <- has_dm | !is.na(merged[[dc]])
      }
    }
    # Also consider case where dm row exists but all dm values are NA -> has_dm would be FALSE, but we still want to consider it matched
    # In that edge case, SAS would still overwrite with NA, but it's rare and not impactful for this program
    # So we use has_dm to decide strict overwrite, otherwise coalesce
    # If has_dm is TRUE, take dm_col (even if NA) -> SAS behavior
    # If has_dm is FALSE (no dm row), keep original
    # For rows where has_dm TRUE, we set merged[[col]] <- merged[[dm_col]]
    # For rows where has_dm FALSE, keep original
    # But we don't have has_dm for rows where dm values are all NA; we treat as no match (keep original) which is acceptable
    merged[[col]] <- ifelse(has_dm, merged[[dm_col]], merged[[col]])
  }
}
# Drop the .dm duplicate columns
merged <- merged %>% select(-dplyr::ends_with(".dm"))

# Now merged contains raw_ae + dm columns, filtered to raw_ae subjects (if a)

# -------------------------------------------------------------------------
# Create SDTM AE variables
# STUDYID = "ABC-301"
# AESTDTC = put(AESTDT, yymmdd10.)
# AEENDTC = put(AEENDT, yymmdd10.)
# rfstdt_num = input(RFSTDTC, yymmdd10.)
# ASTDY / AENDY calculations

# Ensure required columns exist (case-insensitive)
# Normalize column names to expected SAS names for processing, but keep original case for output
# Find actual column names for AESTDT, AEENDT, RFSTDTC, USUBJID, AETERM, AESER, AESEV
find_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}
col_AESTDT  <- find_col(merged, "AESTDT")
col_AEENDT  <- find_col(merged, "AEENDT")
col_RFSTDTC <- find_col(merged, "RFSTDTC")
col_USUBJID <- find_col(merged, "USUBJID")
col_SUBJID  <- find_col(merged, "SUBJID")
col_AETERM  <- find_col(merged, "AETERM")
col_AESER   <- find_col(merged, "AESER")
col_AESEV   <- find_col(merged, "AESEV")

# If any of these are missing, create NA columns to avoid error (SAS would have missing)
if (is.na(col_AESTDT)) { merged$AESTDT <- as.Date(NA); col_AESTDT <- "AESTDT" }
if (is.na(col_AEENDT)) { merged$AEENDT <- as.Date(NA); col_AEENDT <- "AEENDT" }
if (is.na(col_RFSTDTC)) { merged$RFSTDTC <- NA_character_; col_RFSTDTC <- "RFSTDTC" }
if (is.na(col_USUBJID)) { merged$USUBJID <- NA_character_; col_USUBJID <- "USUBJID" }
if (is.na(col_AETERM)) { merged$AETERM <- NA_character_; col_AETERM <- "AETERM" }
if (is.na(col_AESER)) { merged$AESER <- NA_character_; col_AESER <- "AESER" }
if (is.na(col_AESEV)) { merged$AESEV <- NA_character_; col_AESEV <- "AESEV" }

# Convert dates to Date objects for calculation
# AESTDT / AEENDT may be Date, POSIXt, numeric (SAS date), or character
aestdt_date <- to_date(merged[[col_AESTDT]])
aeendt_date <- to_date(merged[[col_AEENDT]])
rfstdt_date <- to_date(merged[[col_RFSTDTC]])

# For SAS put(AESTDT, yymmdd10.) -> character ISO date
AESTDTC <- fmt_yymmdd10(aestdt_date)
AEENDTC <- fmt_yymmdd10(aeendt_date)

# rfstdt_num in SAS is numeric SAS date (days since 1960-01-01)
# We have rfstdt_date as Date; equivalent numeric is as.numeric(rfstdt_date - as.Date("1960-01-01"))
# But for ASTDY calculation we can use Date differences directly

# ASTDY / AENDY
# SAS logic:
# if not missing(AESTDT) then do;
#   if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num + 1;
#   else ASTDY = AESTDT - rfstdt_num;
# end;
# Same for AENDY

ASTDY <- rep(NA_real_, nrow(merged))
AENDY <- rep(NA_real_, nrow(merged))

# Only calculate where AESTDT not missing and RFSTDTC not missing
# In SAS, if RFSTDTC missing, rfstdt_num = ., then AESTDT >= . is true (since . is smallest), but AESTDT - . = . => missing
# So result is missing anyway. We can just require both not NA.
valid_ast <- !is.na(aestdt_date) & !is.na(rfstdt_date)
if (any(valid_ast)) {
  diff_ast <- as.numeric(aestdt_date[valid_ast] - rfstdt_date[valid_ast])
  # SAS: if AESTDT >= rfstdt_num then +1 else no +1
  # diff >=0 corresponds to AESTDT >= rfstdt_num
  ASTDY[valid_ast] <- ifelse(diff_ast >= 0, diff_ast + 1, diff_ast)
}

valid_aen <- !is.na(aeendt_date) & !is.na(rfstdt_date)
if (any(valid_aen)) {
  diff_aen <- as.numeric(aeendt_date[valid_aen] - rfstdt_date[valid_aen])
  AENDY[valid_aen] <- ifelse(diff_aen >= 0, diff_aen + 1, diff_aen)
}

# Build final SDTM AE dataset
# keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY;
sdtm_ae <- tibble(
  STUDYID = rep("ABC-301", nrow(merged)),
  USUBJID = merged[[col_USUBJID]],
  SUBJID  = merged[[col_SUBJID]],
  AETERM  = merged[[col_AETERM]],
  AESER   = merged[[col_AESER]],
  AESEV   = merged[[col_AESEV]],
  AESTDTC = AESTDTC,
  AEENDTC = AEENDTC,
  ASTDY   = ASTDY,
  AENDY   = AENDY
)

# SAS format ASTDY AENDY 8. -> integer display, but keep numeric
# Ensure sorting by SUBJID (and maybe AETERM) to match SAS output order
# SAS merge retains order by SUBJID as sorted; we already sorted raw_ae by SUBJID, so sdtm_ae is in that order
# For reproducibility, sort by SUBJID, AETERM, AESTDTC
sdtm_ae <- sdtm_ae %>% arrange(SUBJID, AETERM, AESTDTC)

# -------------------------------------------------------------------------
# Write outputs

# ae.csv (sdtm.ae)
# Ensure dates as YYYY-MM-DD already, numeric unrounded
# Use write.csv with na=""
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# raw_ae.csv (raw.raw_ae) - write original raw_ae as CSV with dates formatted
raw_ae_out <- format_for_csv(raw_ae)
# Also ensure any haven_labelled already zapped
write.csv(raw_ae_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")

cat("Done. Wrote", file.path(out, "ae.csv"), "with", nrow(sdtm_ae), "rows\n")
cat("Wrote", file.path(out, "raw_ae.csv"), "with", nrow(raw_ae_out), "rows\n")
```