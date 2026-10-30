# P067_ae.sas translation to R
# Purpose: Create SDTM AE dataset

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Load allowed packages (only if available)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
})

raw_dir  <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

# Helper: find file for a dataset name in a lib directory
find_dataset_file <- function(lib, name) {
  # list all files in lib
  if (!dir.exists(lib)) return(NA_character_)
  files <- list.files(lib, full.names = TRUE, recursive = FALSE)
  # case-insensitive match for name with extensions
  pat <- paste0("^", name, "\\.(sas7bdat|xpt|sas7bat|xport|csv)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) > 0) return(files[idx[1]])
  # also try without extension exact
  idx2 <- grep(paste0("^", name, "$"), basename(files), ignore.case = TRUE)
  if (length(idx2) > 0) return(files[idx2[1]])
  return(NA_character_)
}

read_dataset <- function(lib, name) {
  f <- find_dataset_file(lib, name)
  if (is.na(f)) {
    # fallback: try lower case file name directly
    cand1 <- file.path(lib, paste0(name, ".sas7bdat"))
    cand2 <- file.path(lib, paste0(tolower(name), ".sas7bdat"))
    cand3 <- file.path(lib, paste0(name, ".xpt"))
    cand4 <- file.path(lib, paste0(toupper(name), ".xpt"))
    cand5 <- file.path(lib, paste0(name, ".csv"))
    for (cand in c(cand1, cand2, cand3, cand4, cand5)) {
      if (file.exists(cand)) { f <- cand; break }
    }
  }
  if (is.na(f) || !file.exists(f)) {
    stop(paste0("Dataset file not found for lib=", lib, " name=", name, " tried: ", f))
  }
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat" || ext == "sas7bat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt" || ext == "xport") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try sas then xpt
    tryCatch({
      df <- haven::read_sas(f)
    }, error = function(e) {
      df <- haven::read_xpt(f)
    })
  }
  # haven labelled -> convert to plain, keep Date types
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  return(df)
}

# Helper: convert SAS numeric date or character to R Date
to_R_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    # haven already converts formatted dates to Date, so numeric here is unformatted SAS date
    # Convert all non-NA numerics
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # trim
    x_trim <- trimws(x)
    x_trim[x_trim == ""] <- NA
    # yymmdd10. is YYYY-MM-DD, SAS input with yymmdd10. expects that
    # Use strict format %Y-%m-%d on first 10 chars
    # If contains time, take first 10
    x10 <- substr(x_trim, 1, 10)
    d <- as.Date(x10, format = "%Y-%m-%d")
    # fallback for other ISO like YYYY/MM/DD (should not happen, but keep)
    # Do not use multi-format; only yymmdd10.
    return(d)
  }
  return(x)
}

# Helper: format Date columns to ISO character for CSV writing
format_dates_for_csv <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "Date")) {
      df[[nm]] <- format(col, "%Y-%m-%d")
    } else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
      df[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(col, "POSIXlt")) {
      df[[nm]] <- format(as.POSIXct(col), "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

# Read inputs
raw_ae <- read_dataset(raw_dir, "raw_ae")
dm     <- read_dataset(sdtm_dir, "dm")

# Preserve original raw_ae for output raw_ae.csv (before any manipulation)
raw_ae_original <- raw_ae

# Sort by SUBJID (SAS sorts character with uppercase before lowercase; R default is similar for ASCII)
# Ensure SUBJID exists
stopifnot("SUBJID" %in% names(raw_ae))
stopifnot("SUBJID" %in% names(dm))

# SAS proc sort: missing first, then character ordering
# Use order with na.last=FALSE to put NA first
raw_ae <- raw_ae[order(raw_ae$SUBJID, na.last = FALSE), , drop = FALSE]
dm     <- dm[order(dm$SUBJID, na.last = FALSE), , drop = FALSE]

# Before merge, drop or rename every column present in both tables except keys, so no .x/.y suffixes
common_cols <- intersect(names(raw_ae), names(dm))
common_cols <- setdiff(common_cols, c("SUBJID"))
if (length(common_cols) > 0) {
  # SAS merge: second dataset overwrites first for common vars
  # To emulate, drop common from first? Actually SAS keeps second's value.
  # We drop from raw_ae? No, we want dm's value to win, so drop from raw_ae and keep dm's.
  # But for this program, keep list does not include overlapping except SUBJID/USUBJID
  # Safer to drop from dm to avoid losing raw_ae AE vars if overlap incorrectly
  # Check which common are in keep list: STUDYID etc not in raw_ae
  # We will drop common from dm except USUBJID/RFSTDTC which should not be in raw_ae anyway
  # If USUBJID is in both, we want dm's version, so drop from raw_ae
  # Implement: if common includes USUBJID, drop from raw_ae; otherwise drop from dm
  for (cc in common_cols) {
    if (cc %in% c("USUBJID", "RFSTDTC", "RFENDTC", "SITEID", "BRTHDTC", "SEX", "RACE", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "COUNTRY", "DMDTC", "DMDY")) {
      # keep dm's version
      if (cc %in% names(raw_ae)) raw_ae[[cc]] <- NULL
    } else {
      # keep raw_ae's version
      if (cc %in% names(dm)) dm[[cc]] <- NULL
    }
  }
}

# Merge raw.raw_ae (in=a) with sdtm.dm by SUBJID, keep only a (left join)
merged <- merge(raw_ae, dm, by = "SUBJID", all.x = TRUE, sort = FALSE)

# Verify merge kept all raw_ae rows
stopifnot(nrow(merged) == nrow(raw_ae))
stopifnot("RFSTDTC" %in% names(merged) || "RFSTDTC" %in% names(dm))

# Create SDTM AE dataset
# Length STUDYID $10 AESTDTC AEENDTC $10
# STUDYID = "ABC-301"
# AESTDTC = put(AESTDT, yymmdd10.)
# AEENDTC = put(AEENDT, yymmdd10.)
# rfstdt_num = input(RFSTDTC, yymmdd10.)
# ASTDY / AENDY calculations

# Ensure required columns exist
# AESTDT and AEENDT may be named AESTDT/AEENDT or AESTDTN etc; check
# In raw_ae, they are AESTDT, AEENDT (numeric SAS dates)
if (!"AESTDT" %in% names(merged)) {
  # try case-insensitive search
  idx <- which(toupper(names(merged)) == "AESTDT")
  if (length(idx) > 0) names(merged)[idx[1]] <- "AESTDT"
}
if (!"AEENDT" %in% names(merged)) {
  idx <- which(toupper(names(merged)) == "AEENDT")
  if (length(idx) > 0) names(merged)[idx[1]] <- "AEENDT"
}
stopifnot("AESTDT" %in% names(merged) || "AESTDT" %in% toupper(names(merged)))
stopifnot("AEENDT" %in% names(merged) || "AEENDT" %in% toupper(names(merged)))

# Convert dates to R Date for calculations
AESTDT_date <- to_R_Date(merged$AESTDT)
AEENDT_date <- to_R_Date(merged$AEENDT)

# RFSTDTC is character ISO date; convert via yymmdd10.
RFSTDTC_char <- merged$RFSTDTC
# RFSTDTC may already be Date if haven converted; handle both
if (inherits(RFSTDTC_char, "Date") || inherits(RFSTDTC_char, "POSIXt")) {
  rfstdt_num <- as.Date(RFSTDTC_char)
} else {
  # input(RFSTDTC, yymmdd10.) - strict YYYY-MM-DD
  rfstdt_num <- as.Date(substr(trimws(as.character(RFSTDTC_char)), 1, 10), format = "%Y-%m-%d")
}

# AESTDTC and AEENDTC as ISO 8601 date strings (yymmdd10.)
AESTDTC <- ifelse(!is.na(AESTDT_date), format(AESTDT_date, "%Y-%m-%d"), NA_character_)
AEENDTC <- ifelse(!is.na(AEENDT_date), format(AEENDT_date, "%Y-%m-%d"), NA_character_)

# AE Start Day
ASTDY <- rep(NA_real_, nrow(merged))
idx_ast <- !is.na(AESTDT_date) & !is.na(rfstdt_num)
if (any(idx_ast)) {
  diff_ast <- as.numeric(AESTDT_date[idx_ast] - rfstdt_num[idx_ast])
  ASTDY[idx_ast] <- ifelse(diff_ast >= 0, diff_ast + 1, diff_ast)
}

# AE End Day
AENDY <- rep(NA_real_, nrow(merged))
idx_aen <- !is.na(AEENDT_date) & !is.na(rfstdt_num)
if (any(idx_aen)) {
  diff_aen <- as.numeric(AEENDT_date[idx_aen] - rfstdt_num[idx_aen])
  AENDY[idx_aen] <- ifelse(diff_aen >= 0, diff_aen + 1, diff_aen)
}

# Build final sdtm.ae
# keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY
STUDYID <- rep("ABC-301", nrow(merged))

# Ensure USUBJID exists; if not, try to construct or leave NA
if (!"USUBJID" %in% names(merged)) {
  # try case-insensitive
  idx_u <- which(toupper(names(merged)) == "USUBJID")
  if (length(idx_u) > 0) {
    USUBJID <- merged[[ idx_u[1] ]]
  } else {
    USUBJID <- rep(NA_character_, nrow(merged))
  }
} else {
  USUBJID <- merged$USUBJID
}

# Ensure other keep vars exist
for (v in c("AETERM", "AESER", "AESEV")) {
  if (!v %in% names(merged)) {
    idx_v <- which(toupper(names(merged)) == v)
    if (length(idx_v) > 0) {
      merged[[v]] <- merged[[ idx_v[1] ]]
    } else {
      merged[[v]] <- rep(NA_character_, nrow(merged))
    }
  }
}

SUBJID <- merged$SUBJID
AETERM <- merged$AETERM
AESER  <- merged$AESER
AESEV  <- merged$AESEV

ae <- data.frame(
  STUDYID = STUDYID,
  USUBJID = as.character(USUBJID),
  SUBJID  = SUBJID,
  AETERM  = as.character(AETERM),
  AESER   = as.character(AESER),
  AESEV   = as.character(AESEV),
  AESTDTC = as.character(AESTDTC),
  AEENDTC = as.character(AEENDTC),
  ASTDY   = as.numeric(ASTDY),
  AENDY   = as.numeric(AENDY),
  stringsAsFactors = FALSE
)

# Sort final by SUBJID to match SAS output order (SAS retains sorted order)
ae <- ae[order(ae$SUBJID, ae$AETERM, ae$AESTDTC, na.last = FALSE), , drop = FALSE]

# Write outputs
# Format dates for CSV (already character for AESTDTC/AEENDTC, but ensure no Date columns remain)
ae_out <- format_dates_for_csv(ae)
write.csv(ae_out, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# raw_ae dataset output
raw_ae_out <- format_dates_for_csv(raw_ae_original)
# Ensure raw_ae sorted by SUBJID as SAS did
if ("SUBJID" %in% names(raw_ae_out)) {
  raw_ae_out <- raw_ae_out[order(raw_ae_out$SUBJID, na.last = FALSE), , drop = FALSE]
}
write.csv(raw_ae_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")

cat("P067 ae.sas translation complete. Wrote ae.csv (", nrow(ae), " rows) and raw_ae.csv (", nrow(raw_ae_out), " rows) to ", out, "\n", sep = "")
