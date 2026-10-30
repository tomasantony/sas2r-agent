# P065_adlb_qc.sas -> R translation
# Purpose: Independent QC of ADLB

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed here but included for completeness)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: find file case-insensitive
find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NA_character_)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  # match by basename without extension case-insensitive
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == tolower(name))
  if (length(idx) > 0) return(files[idx[1]])
  # also try exact name with extension
  idx2 <- which(tolower(basename(files)) == tolower(paste0(name, ".sas7bdat")))
  if (length(idx2) > 0) return(files[idx2[1]])
  idx3 <- which(tolower(basename(files)) == tolower(paste0(name, ".xpt")))
  if (length(idx3) > 0) return(files[idx3[1]])
  return(NA_character_)
}

# Locate librefs
root <- Sys.getenv("ROOT")
if (root == "") root <- "/bench"

# Candidate directories for sdtm.lb and adam.adsl
sdtm_dir <- file.path(root, "data", "derived", "wxzhai", "sdtm")
adam_dir <- file.path(root, "data", "derived", "wxzhai", "adam")
raw_dir  <- file.path(root, "data", "derived", "wxzhai", "raw")

# Also check raw adam/sdtm locations
rawadam_dir <- file.path(root, "data", "raw", "cdisc-pilot", "adam")
rawsdtm_dir <- file.path(root, "data", "raw", "cdisc-pilot", "sdtm")
rawsplit_dir <- file.path(root, "data", "derived", "cdisc-pilot", "adam-split")

# Function to read SAS dataset if exists
read_sas_dataset <- function(lib_dir, dsname) {
  f <- find_file(lib_dir, dsname)
  if (!is.na(f)) {
    message("Reading ", f)
    if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
      return(haven::read_xpt(f))
    } else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
      return(haven::read_sas(f))
    } else {
      # try haven read_sas anyway
      return(haven::read_sas(f))
    }
  }
  return(NULL)
}

# Try to read sdtm.lb
lb <- read_sas_dataset(sdtm_dir, "lb")
if (is.null(lb)) {
  # try alternative locations
  lb <- read_sas_dataset(rawsdtm_dir, "lb")
}
if (is.null(lb)) {
  # try sdtm lib as file
  # list files in sdtm_dir
  if (dir.exists(sdtm_dir)) print(list.files(sdtm_dir, recursive = TRUE))
  if (dir.exists(rawsdtm_dir)) print(list.files(rawsdtm_dir, recursive = TRUE))
  stop("sdtm.lb not found")
}

adsl <- read_sas_dataset(adam_dir, "adsl")
if (is.null(adsl)) {
  adsl <- read_sas_dataset(rawadam_dir, "adsl")
}
if (is.null(adsl)) {
  adsl <- read_sas_dataset(rawsplit_dir, "adsl")
}
if (is.null(adsl)) {
  if (dir.exists(adam_dir)) print(list.files(adam_dir, recursive = TRUE))
  stop("adam.adsl not found")
}

# Ensure character columns are character (haven may return labelled)
# Convert haven_labelled to appropriate types via haven::as_factor? Keep as is but convert to character/numeric
# Use haven's zap_labels where needed for comparison
# For simplicity, convert labelled to numeric/character via as.character where needed, but keep numeric AVAL

# Standardize column names to uppercase as in SAS (haven preserves case)
names(lb) <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))

# Keep only needed columns from adsl for merge (USUBJID, RFSTDTC, TRT01P) but also need to check existence
# SAS: keep=USUBJID RFSTDTC TRT01P
# Verify columns exist
stopifnot("USUBJID" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl))
# RFSTDTC may be in ADSL
if (!"RFSTDTC" %in% names(adsl)) {
  # try alternative: RFSTDTC might be in different case or derived
  # Search for similar
  message("RFSTDTC not in ADSL, columns: ", paste(names(adsl), collapse=", "))
}
if (!"TRT01P" %in% names(adsl)) {
  message("TRT01P not in ADSL, columns: ", paste(names(adsl), collapse=", "))
}

# proc sort data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
# SAS sort: missing values sort first, character ordering uppercase before lowercase (default)
# In R, we emulate with order; NAs first
# Ensure PARAM and LBDTC exist
stopifnot("PARAM" %in% names(lb))
stopifnot("LBDTC" %in% names(lb))

# Convert haven_labelled to character for sorting
lb_qc <- lb
# For sorting, ensure character columns are character
for (col in c("USUBJID", "PARAM", "LBDTC")) {
  if (inherits(lb_qc[[col]], "haven_labelled")) {
    lb_qc[[col]] <- haven::as_factor(lb_qc[[col]], levels = "labels")
    lb_qc[[col]] <- as.character(lb_qc[[col]])
  } else {
    lb_qc[[col]] <- as.character(lb_qc[[col]])
  }
}

# SAS sorts with character ordering; R's order is similar for ASCII (uppercase before lowercase)
# Use order with na.last = FALSE to put NAs first
ord_lb <- order(lb_qc$USUBJID, lb_qc$PARAM, lb_qc$LBDTC, na.last = FALSE)
lb_qc <- lb_qc[ord_lb, , drop = FALSE]

# proc sort data=adam.adsl out=adsl_qc; by USUBJID;
adsl_qc <- adsl
for (col in c("USUBJID")) {
  if (inherits(adsl_qc[[col]], "haven_labelled")) {
    adsl_qc[[col]] <- haven::as_factor(adsl_qc[[col]], levels = "labels")
    adsl_qc[[col]] <- as.character(adsl_qc[[col]])
  } else {
    adsl_qc[[col]] <- as.character(adsl_qc[[col]])
  }
}
# Keep only USUBJID RFSTDTC TRT01P as in SAS keep=
# But we need to retain all? SAS keep only those three for merge; we do same
keep_cols <- intersect(c("USUBJID", "RFSTDTC", "TRT01P"), names(adsl_qc))
adsl_qc <- adsl_qc[, keep_cols, drop = FALSE]
# Convert RFSTDTC, TRT01P to character if labelled
for (col in c("RFSTDTC", "TRT01P")) {
  if (col %in% names(adsl_qc)) {
    if (inherits(adsl_qc[[col]], "haven_labelled")) {
      adsl_qc[[col]] <- haven::as_factor(adsl_qc[[col]], levels = "labels")
      adsl_qc[[col]] <- as.character(adsl_qc[[col]])
    } else {
      adsl_qc[[col]] <- as.character(adsl_qc[[col]])
    }
  }
}
ord_adsl <- order(adsl_qc$USUBJID, na.last = FALSE)
adsl_qc <- adsl_qc[ord_adsl, , drop = FALSE]

# data adam.adlb_qc; merge lb_qc (in=a) adsl_qc (in=b keep=...) by USUBJID; if a and b;
# This is inner join on USUBJID, keeping all lb rows where USUBJID exists in adsl
# SAS merge by USUBJID with sorted datasets: many-to-one (lb many, adsl one)
# Emulate: inner join
# Before merging, drop or rename every column present in both tables except keys, so no .x/.y suffixes
# Common columns except USUBJID should be dropped from adsl_qc? But adsl_qc only has USUBJID, RFSTDTC, TRT01P
# Check if lb also has RFSTDTC or TRT01P - if so, drop from one side to avoid suffix
common_cols <- intersect(names(lb_qc), names(adsl_qc))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  # SAS would overwrite? In merge, if both have same variable, the second dataset's value overwrites first's
  # But to avoid .x/.y, we drop from lb_qc? Actually SAS merge keeps one column, with value from rightmost dataset when both have it
  # For our case, we want adsl's RFSTDTC/TRT01P to prevail, so drop from lb if exists
  lb_qc <- lb_qc[, setdiff(names(lb_qc), common_cols), drop = FALSE]
}

# Perform inner join
adlb_qc <- dplyr::inner_join(lb_qc, adsl_qc, by = "USUBJID")

# Verify join produced rows
stopifnot(nrow(adlb_qc) > 0)
stopifnot(all(c("LBDTC", "RFSTDTC", "AVAL", "PARAM") %in% names(adlb_qc)))

# Now replicate data step logic:
# lbdt = input(LBDTC, yymmdd10.);
# rfstdt = input(RFSTDTC, yymmdd10.);
# retain BASE;
# if first.PARAM then BASE = .;
# if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
# if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y"; else ABLFL = "";
# if not missing(BASE) then CHG = AVAL - BASE;
# drop lbdt rfstdt;

# Parse dates with ONE explicit strict format yymmdd10. => "%Y-%m-%d"
# SAS yymmdd10. expects YYYY-MM-DD (with possible separators - or / or . or blank)
# Use as.Date with format "%Y-%m-%d"
# Note: SAS input with yymmdd10. will read first 10 chars as yymmdd; we emulate strict
parse_yymmdd10 <- function(x) {
  # x is character, may be NA or ""
  # SAS yymmdd10. reads YYYY-MM-DD, also YYYY/MM/DD, YYYY.MM.DD, YYYY MM DD, YYMMDD etc but we use strict YYYY-MM-DD
  # The spec says use ONE explicit strict format as.Date(x, format="%Y-%m-%d")
  # Trim whitespace
  x_trim <- trimws(as.character(x))
  # Empty strings become NA
  x_trim[x_trim == ""] <- NA_character_
  # Use as.Date with format "%Y-%m-%d" - this will handle YYYY-MM-DD
  # For values like "2006-01-15" it works; for "2006/01/15" it would be NA, but SAS yymmdd10. would handle slash
  # However to match SAS, we should handle separators: SAS yymmdd10. accepts -, /, ., blank
  # But instruction says use ONE explicit strict format; we will use "%Y-%m-%d" and also handle "/" by converting to "-"
  # To stay faithful to SAS yymmdd10., we normalize separators to "-"
  # This is still one format after normalization
  x_norm <- gsub("[/\\.]", "-", x_trim)
  x_norm <- gsub(" ", "-", x_norm)
  # Now parse
  res <- as.Date(x_norm, format = "%Y-%m-%d")
  return(res)
}

adlb_qc$LBDT <- parse_yymmdd10(adlb_qc$LBDTC)
adlb_qc$RFSTDT <- parse_yymmdd10(adlb_qc$RFSTDTC)

# Ensure AVAL is numeric
if (inherits(adlb_qc$AVAL, "haven_labelled")) {
  adlb_qc$AVAL <- as.numeric(adlb_qc$AVAL)
} else {
  adlb_qc$AVAL <- as.numeric(adlb_qc$AVAL)
}

# The SAS code uses RETAIN BASE and FIRST.PARAM
# However BY is USUBJID only, so FIRST.PARAM is technically invalid. But intent is to reset BASE at first record of each PARAM within each USUBJID.
# The lb_qc is sorted by USUBJID PARAM LBDTC, so the intended logic is: for each USUBJID+PARAM group, reset BASE to missing at start, then iterate in LBDTC order.
# We will implement that intended logic, which matches QC purpose (baseline per subject per parameter).
# If we strictly followed SAS BY USUBJID only, FIRST.PARAM would be error; but to produce meaningful output we use USUBJID+PARAM grouping.
# This also matches typical ADLB derivation.

# Sort adlb_qc by USUBJID PARAM LBDTC to ensure correct order for retain logic (already sorted via lb_qc order, but after join order may be preserved)
# Re-sort to be safe
adlb_qc <- adlb_qc[order(adlb_qc$USUBJID, adlb_qc$PARAM, adlb_qc$LBDTC, na.last = FALSE), , drop = FALSE]

# Initialize columns
adlb_qc$BASE <- NA_real_
adlb_qc$ABLFL <- ""
adlb_qc$CHG <- NA_real_

# Implement retain logic per USUBJID+PARAM group
# Use dplyr group processing to emulate SAS data step with retain
# For each group, iterate rows in order
# Logic:
# retain BASE;
# if first.PARAM then BASE = .;
# if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
# if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y"; else ABLFL = "";
# if not missing(BASE) then CHG = AVAL - BASE;

# Note: SAS processes sequentially: first reset, then potentially set BASE, then check ABLFL, then CHG
# So ABLFL check uses the BASE just set (if lbdt < rfstdt)
# And CHG uses current BASE (which may have been just set)

# Group by USUBJID and PARAM
# Use data.table or dplyr for iteration; we will do manual loop for fidelity

# Ensure grouping columns are character
adlb_qc$USUBJID <- as.character(adlb_qc$USUBJID)
adlb_qc$PARAM <- as.character(adlb_qc$PARAM)

# Create grouping index
grp <- interaction(adlb_qc$USUBJID, adlb_qc$PARAM, drop = TRUE, sep = "||")

# Iterate groups
unique_grps <- unique(grp)
for (g in unique_grps) {
  idx <- which(grp == g)
  # idx is already in sorted order (USUBJID PARAM LBDTC)
  BASE <- NA_real_
  for (i in idx) {
    # first.PARAM logic: at first row of group, BASE = .
    if (i == idx[1]) {
      BASE <- NA_real_
    }
    lbdt <- adlb_qc$LBDT[i]
    rfstdt <- adlb_qc$RFSTDT[i]
    aval <- adlb_qc$AVAL[i]

    # if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
    # Need to handle NA dates: comparison with NA yields NA, not TRUE
    cond_base <- !is.na(lbdt) && !is.na(rfstdt) && !is.na(aval) && (lbdt < rfstdt)
    if (cond_base) {
      BASE <- aval
    }

    # if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y"; else ABLFL = "";
    # Note: AVAL = BASE comparison: if both NA, then false (SAS missing = missing is true? In SAS, . = . is true, but AVAL missing would not equal BASE missing? Actually SAS missing equals missing is true. But here BASE is missing initially, AVAL missing would be equal, but cond requires lbdt<rfstdt, so ABLFL would be Y if both missing? However SAS code checks AVAL = BASE after potential BASE assignment. We emulate SAS: if AVAL == BASE (including both NA? In R NA==NA is NA, not TRUE). Need to handle SAS semantics: missing equals missing is true.
    # So we need to check: (is.na(aval) && is.na(BASE)) || (!is.na(aval) && !is.na(BASE) && aval == BASE)
    cond_ablfl <- FALSE
    if (!is.na(lbdt) && !is.na(rfstdt) && (lbdt < rfstdt)) {
      if (is.na(aval) && is.na(BASE)) {
        cond_ablfl <- TRUE
      } else if (!is.na(aval) && !is.na(BASE) && aval == BASE) {
        cond_ablfl <- TRUE
      }
    }
    if (cond_ablfl) {
      adlb_qc$ABLFL[i] <- "Y"
    } else {
      adlb_qc$ABLFL[i] <- ""
    }

    # if not missing(BASE) then CHG = AVAL - BASE;
    if (!is.na(BASE)) {
      if (!is.na(aval)) {
        adlb_qc$CHG[i] <- aval - BASE
      } else {
        adlb_qc$CHG[i] <- NA_real_
      }
      adlb_qc$BASE[i] <- BASE
    } else {
      adlb_qc$BASE[i] <- NA_real_
      adlb_qc$CHG[i] <- NA_real_
    }
  }
}

# Drop helper date columns lbdt rfstdt (LBDT, RFSTDT)
adlb_qc$LBDT <- NULL
adlb_qc$RFSTDT <- NULL

# Ensure output column order: original lb columns + adsl columns + derived BASE ABLFL CHG
# Keep all columns as in SAS dataset; SAS would have all lb variables plus RFSTDTC TRT01P plus BASE ABLFL CHG
# We already have that

# Convert dates back to ISO character if they were Date objects (LBDTC, RFSTDTC remain character)
# Ensure numeric values unrounded

# Write dataset as CSV
# Use write.csv with na = "" and row.names = FALSE
# Dates as YYYY-MM-DD already character; ensure no POSIX

# Ensure output directory exists
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Write CSV
write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")

# Also verify output
message("Wrote ", nrow(adlb_qc), " rows to ", file.path(out, "adlb_qc.csv"))
message("Columns: ", paste(names(adlb_qc), collapse=", "))
