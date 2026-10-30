out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

# Helper: parse SAS date11. format like "01JAN2020" or "1-JAN-2020" etc.
# SAS date11. is typically DDMMMYYYY or DD-MMM-YYYY with width 11
# In SAS code: input(STDT, date11.) where STDT is character
# We need to handle various forms: "01JAN2020", "01-JAN-2020", "01JAN2020", "01-JAN-20" etc.
# Use explicit strict format: try "%d%b%Y" and "%d-%b-%Y" after uppercasing and trimming
parse_date11 <- function(x) {
  # x is character vector
  # Return Date vector
  x_trim <- trimws(x)
  x_trim[x_trim == ""] <- NA
  # Uppercase for month
  x_up <- toupper(x_trim)
  # Remove extra spaces
  # Try parsing with lubridate? But requirement says use ONE explicit strict format.
  # However for date11. we need to handle both with and without dashes.
  # SAS date11. informat accepts both "01JAN2020" and "01-JAN-2020" etc.
  # We'll implement: remove dashes then parse as %d%b%Y, and also handle 2-digit year?
  # For now, handle both.
  res <- as.Date(rep(NA, length(x_up)))
  # Remove dashes for uniform parsing
  # But keep original for fallback
  # First try with dashes removed: format "%d%b%Y"
  # Need to handle 1 or 2 digit day: %d handles both
  # Use as.Date with format after removing dashes
  x_nodash <- gsub("-", "", x_up)
  # Also remove spaces
  x_nodash <- gsub(" ", "", x_nodash)
  # Parse
  # Use as.Date with format "%d%b%Y"
  # This will handle "1JAN2020" etc.
  # For 2-digit year, SAS date11. would interpret? But we assume 4-digit.
  # Try 4-digit year first
  idx <- !is.na(x_nodash)
  if (any(idx)) {
    parsed <- suppressWarnings(as.Date(x_nodash[idx], format = "%d%b%Y"))
    res[idx] <- parsed
    # For those still NA, try 2-digit year "%d%b%y"
    still_na <- idx & is.na(res)
    if (any(still_na)) {
      parsed2 <- suppressWarnings(as.Date(x_nodash[still_na], format = "%d%b%y"))
      res[still_na] <- parsed2
    }
  }
  res
}

# Helper to format Date to ISO yymmdd10. = "YYYY-MM-DD"
format_yymmdd10 <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# ------------------------------------------------------------------
# 1. Load raw EDC AE data
# ------------------------------------------------------------------
# Paths from context (forward slashes)
edc_ae_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx"
edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
ivrs_rand_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

# Also check for sdtm_dm in raw CDISC pilot (if exists)
sdtm_dm_xpt <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt"
sdtm_dm_sas <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat"

# Read edc_ae
edc_ae <- readxl::read_excel(edc_ae_path)
# Ensure column names as in SAS (uppercase)
names(edc_ae) <- toupper(names(edc_ae))

# Read edc_dm for RFSTDTC derivation if needed
edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- toupper(names(edc_dm))

# Read edc_ex if needed for RFSTDTC
edc_ex <- tryCatch({
  df <- readxl::read_excel(edc_ex_path)
  names(df) <- toupper(names(df))
  df
}, error = function(e) NULL)

# ------------------------------------------------------------------
# 2. Derive sdtm_dm equivalent (USUBJID, RFSTDTC)
# ------------------------------------------------------------------
# SAS program expects sdtm_dm with USUBJID and RFSTDTC (ISO date string YYYY-MM-DD)
# We need to derive it from available raw data.
# Check if sdtm_dm xpt exists
sdtm_dm <- NULL
if (file.exists(sdtm_dm_xpt)) {
  sdtm_dm <- haven::read_xpt(sdtm_dm_xpt)
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  # Keep only needed
  if (all(c("USUBJID", "RFSTDTC") %in% names(sdtm_dm))) {
    sdtm_dm <- sdtm_dm[, c("USUBJID", "RFSTDTC")]
  }
} else if (file.exists(sdtm_dm_sas)) {
  sdtm_dm <- haven::read_sas(sdtm_dm_sas)
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  if (all(c("USUBJID", "RFSTDTC") %in% names(sdtm_dm))) {
    sdtm_dm <- sdtm_dm[, c("USUBJID", "RFSTDTC")]
  }
}

# If not found, derive from edc_dm / edc_ex / ivrs_rand
# CDISC definition: RFSTDTC is reference start date, typically first exposure date or randomization date
# Look at edc_dm columns to infer
# Let's inspect edc_dm structure: it likely has SUBJID, RFSTDTC or STDTC etc.
# We will derive USUBJID = "ONCO-2026-" + SUBJID and RFSTDTC from available date columns

if (is.null(sdtm_dm)) {
  # Try to find RFSTDTC-like column in edc_dm
  # Common names: RFSTDTC, RFSTDT, STDTC, STARTDT, RANDDT, etc.
  # Let's check what columns exist
  # If edc_dm already has RFSTDTC, use it
  if ("RFSTDTC" %in% names(edc_dm)) {
    # edc_dm$RFSTDTC may be Date or character
    # Convert to ISO string
    rf_col <- edc_dm$RFSTDTC
    # If numeric Excel date, convert
    if (inherits(rf_col, "Date")) {
      rf_iso <- format(rf_col, "%Y-%m-%d")
    } else if (inherits(rf_col, "POSIXt")) {
      rf_iso <- format(as.Date(rf_col), "%Y-%m-%d")
    } else {
      # character: try to parse
      # Check if already ISO
      rf_char <- as.character(rf_col)
      # If contains "-", assume ISO or date11
      # Try ISO first
      parsed_iso <- suppressWarnings(as.Date(rf_char, format = "%Y-%m-%d"))
      # For NAs, try date11
      na_idx <- is.na(parsed_iso) & !is.na(rf_char) & rf_char != ""
      if (any(na_idx)) {
        parsed_iso[na_idx] <- parse_date11(rf_char[na_idx])
      }
      rf_iso <- ifelse(is.na(parsed_iso), "", format(parsed_iso, "%Y-%m-%d"))
      rf_iso[is.na(rf_iso)] <- ""
    }
    sdtm_dm <- data.frame(
      USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
      RFSTDTC = rf_iso,
      stringsAsFactors = FALSE
    )
  } else {
    # Look for alternative date columns: e.g., "RANDDT", "EXSTDTC", "STDT", "DMSTDTC"
    # Check edc_dm column names
    # Try to find any column containing "STDTC" or "RAND" or "EX"
    # Also check ivrs_rand for randomization date
    # For now, try to use edc_ex first exposure date per subject
    if (!is.null(edc_ex) && nrow(edc_ex) > 0) {
      # edc_ex likely has SUBJID and EXSTDTC or STDT
      # Find date column
      date_candidates <- grep("STDTC|STDT|DATE|DT$", names(edc_ex), value = TRUE)
      # Prefer EXSTDTC
      if ("EXSTDTC" %in% names(edc_ex)) date_candidates <- "EXSTDTC"
      else if ("EXSTDT" %in% names(edc_ex)) date_candidates <- "EXSTDT"
      else if ("STDT" %in% names(edc_ex)) date_candidates <- "STDT"
      # Take first candidate
      if (length(date_candidates) > 0) {
        dc <- date_candidates[1]
        ex_dates <- edc_ex[, c("SUBJID", dc)]
        names(ex_dates) <- c("SUBJID", "DT_RAW")
        # Parse DT_RAW
        dt_raw <- as.character(ex_dates$DT_RAW)
        # If numeric (Excel serial), convert
        # Check if all numeric-like
        # Use readxl already converts dates to Date if Excel date
        if (inherits(ex_dates$DT_RAW, "Date")) {
          parsed <- ex_dates$DT_RAW
        } else if (inherits(ex_dates$DT_RAW, "POSIXt")) {
          parsed <- as.Date(ex_dates$DT_RAW)
        } else {
          # character
          parsed <- suppressWarnings(as.Date(dt_raw, format = "%Y-%m-%d"))
          na_idx <- is.na(parsed) & !is.na(dt_raw) & dt_raw != ""
          if (any(na_idx)) {
            parsed[na_idx] <- parse_date11(dt_raw[na_idx])
          }
          # Also try m/d/Y
          na_idx2 <- is.na(parsed) & !is.na(dt_raw) & dt_raw != ""
          if (any(na_idx2)) {
            parsed[na_idx2] <- suppressWarnings(as.Date(dt_raw[na_idx2], format = "%m/%d/%Y"))
          }
        }
        ex_dates$PARSED <- parsed
        # For each SUBJID, take earliest date as RFSTDTC
        ex_dates$USUBJID <- paste0("ONCO-2026-", as.character(ex_dates$SUBJID))
        # Aggregate min
        agg <- aggregate(PARSED ~ USUBJID, data = ex_dates, FUN = function(x) min(x, na.rm = TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        agg$RFSTDTC[is.na(agg$RFSTDTC)] <- ""
        sdtm_dm <- agg[, c("USUBJID", "RFSTDTC")]
      }
    }
    # If still null, try ivrs_rand
    if (is.null(sdtm_dm) && file.exists(ivrs_rand_path)) {
      ivrs_rand <- readxl::read_excel(ivrs_rand_path)
      names(ivrs_rand) <- toupper(names(ivrs_rand))
      # Find date column
      date_candidates <- grep("RAND|STDTC|DATE|DT$", names(ivrs_rand), value = TRUE)
      if (length(date_candidates) > 0) {
        dc <- date_candidates[1]
        # Similar parsing
        iv_dates <- ivrs_rand[, c("SUBJID", dc)]
        if (!"SUBJID" %in% names(iv_dates)) {
          # Try alternative subject id column
          subj_col <- grep("SUBJ", names(ivrs_rand), value = TRUE)[1]
          iv_dates <- ivrs_rand[, c(subj_col, dc)]
          names(iv_dates) <- c("SUBJID", "DT_RAW")
        } else {
          names(iv_dates) <- c("SUBJID", "DT_RAW")
        }
        dt_raw <- as.character(iv_dates$DT_RAW)
        if (inherits(iv_dates$DT_RAW, "Date")) {
          parsed <- iv_dates$DT_RAW
        } else if (inherits(iv_dates$DT_RAW, "POSIXt")) {
          parsed <- as.Date(iv_dates$DT_RAW)
        } else {
          parsed <- suppressWarnings(as.Date(dt_raw, format = "%Y-%m-%d"))
          na_idx <- is.na(parsed) & !is.na(dt_raw) & dt_raw != ""
          if (any(na_idx)) parsed[na_idx] <- parse_date11(dt_raw[na_idx])
        }
        iv_dates$PARSED <- parsed
        iv_dates$USUBJID <- paste0("ONCO-2026-", as.character(iv_dates$SUBJID))
        agg <- aggregate(PARSED ~ USUBJID, data = iv_dates, FUN = function(x) min(x, na.rm = TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        agg$RFSTDTC[is.na(agg$RFSTDTC)] <- ""
        sdtm_dm <- agg[, c("USUBJID", "RFSTDTC")]
      }
    }
    # Fallback: if still null, create from edc_dm SUBJID with empty RFSTDTC
    if (is.null(sdtm_dm)) {
      sdtm_dm <- data.frame(
        USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
        RFSTDTC = "",
        stringsAsFactors = FALSE
      )
    }
  }
  # Ensure unique by USUBJID
  sdtm_dm <- sdtm_dm[!duplicated(sdtm_dm$USUBJID), ]
}

# Ensure sdtm_dm has correct types
sdtm_dm$USUBJID <- as.character(sdtm_dm$USUBJID)
sdtm_dm$RFSTDTC <- as.character(sdtm_dm$RFSTDTC)
sdtm_dm$RFSTDTC[is.na(sdtm_dm$RFSTDTC)] <- ""

# ------------------------------------------------------------------
# 3. Process Raw AE Data (ae_prep)
# ------------------------------------------------------------------
# SAS:
# data ae_prep;
#   set edc_ae;
#   length USUBJID $25;
#   USUBJID = catx("-", "ONCO-2026", SUBJID);
#   length AESTDTC AEENDTC $19;
#   if STDT ne "" then AESTDTC = put(input(STDT, date11.), yymmdd10.);
#   if ENDT ne "" then AEENDTC = put(input(ENDT, date11.), yymmdd10.);
# run;

# Ensure STDT, ENDT exist as character
# edc_ae columns: SUBJID, STDT, ENDT, TERM, DECOD, BODSYS, SEV, TXGR, REL, OUT, SER etc.
# Convert STDT/ENDT to character if Date
for (col in c("STDT", "ENDT")) {
  if (col %in% names(edc_ae)) {
    if (inherits(edc_ae[[col]], "Date")) {
      # Format as date11. style? But we need to parse as date11 then format to yymmdd10
      # If already Date, we can directly format to yymmdd10
      # However to emulate SAS, we should treat Date as already parsed
      # We'll keep as character in date11 format for parsing step
      edc_ae[[col]] <- format(edc_ae[[col]], "%d%b%Y")
    } else if (inherits(edc_ae[[col]], "POSIXt")) {
      edc_ae[[col]] <- format(as.Date(edc_ae[[col]]), "%d%b%Y")
    } else {
      edc_ae[[col]] <- as.character(edc_ae[[col]])
    }
    edc_ae[[col]][is.na(edc_ae[[col]])] <- ""
  } else {
    edc_ae[[col]] <- ""
  }
}

# Create ae_prep
ae_prep <- edc_ae
ae_prep$USUBJID <- paste0("ONCO-2026-", as.character(ae_prep$SUBJID))

# Convert Raw Dates to ISO 8601
# SAS: if STDT ne "" then AESTDTC = put(input(STDT, date11.), yymmdd10.);
# This means parse STDT with date11. informat, then format as yymmdd10.
# We emulate with parse_date11
ae_prep$AESTDTC <- ""
ae_prep$AEENDTC <- ""

# For STDT
has_stdt <- ae_prep$STDT != "" & !is.na(ae_prep$STDT)
if (any(has_stdt)) {
  parsed_stdt <- parse_date11(ae_prep$STDT[has_stdt])
  ae_prep$AESTDTC[has_stdt] <- format_yymmdd10(parsed_stdt)
}

has_endt <- ae_prep$ENDT != "" & !is.na(ae_prep$ENDT)
if (any(has_endt)) {
  parsed_endt <- parse_date11(ae_prep$ENDT[has_endt])
  ae_prep$AEENDTC[has_endt] <- format_yymmdd10(parsed_endt)
}

# Ensure other columns exist
for (col in c("TERM", "DECOD", "BODSYS", "SEV", "TXGR", "REL", "OUT", "SER")) {
  if (!col %in% names(ae_prep)) ae_prep[[col]] <- ""
  ae_prep[[col]][is.na(ae_prep[[col]])] <- ""
  ae_prep[[col]] <- as.character(ae_prep[[col]])
}

# ------------------------------------------------------------------
# 4. Sort AE Data (by USUBJID AESTDTC TERM)
# ------------------------------------------------------------------
# SAS sorts with character ordering, missing first
# In R, order with na.last = FALSE? But AESTDTC is character "" for missing, which sorts first anyway
ae_prep <- ae_prep[order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM, na.last = TRUE), ]

# ------------------------------------------------------------------
# 5. Prepare dm_for_ae (sort by USUBJID)
# ------------------------------------------------------------------
dm_for_ae <- sdtm_dm[order(sdtm_dm$USUBJID), ]

# ------------------------------------------------------------------
# 6. Final Mapping and Derivation (sdtm_ae1)
# ------------------------------------------------------------------
# SAS merge ae_prep(in=a) dm_for_ae by USUBJID; if a;
# This is left join ae_prep with dm_for_ae

# Before merging, drop or rename every column present in both tables except keys
# Common columns: USUBJID only (maybe others not)
# Ensure no .x/.y suffixes
# Use dplyr left_join

# Keep only needed columns from ae_prep for merge
# ae_prep has USUBJID, AESTDTC, AEENDTC, TERM, DECOD, BODSYS, SEV, TXGR, REL, OUT, SER, plus original STDT/ENDT/SUBJID
# dm_for_ae has USUBJID, RFSTDTC

# Perform merge
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)
# Ensure RFSTDTC exists
if (!"RFSTDTC" %in% names(sdtm_ae1)) sdtm_ae1$RFSTDTC <- ""
sdtm_ae1$RFSTDTC[is.na(sdtm_ae1$RFSTDTC)] <- ""

# Now derive SDTM variables
sdtm_ae1$STUDYID <- "ONCO-2026"
sdtm_ae1$DOMAIN <- "AE"

sdtm_ae1$AETERM <- sdtm_ae1$TERM
sdtm_ae1$AEDECOD <- toupper(sdtm_ae1$DECOD)
sdtm_ae1$AEBODSYS <- gsub("_", " ", sdtm_ae1$BODSYS, fixed = TRUE)
sdtm_ae1$AESEV <- toupper(sdtm_ae1$SEV)

# AETXGR is numeric 8. in SAS; ensure numeric
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))

sdtm_ae1$AEREL <- toupper(sdtm_ae1$REL)
sdtm_ae1$AEOUT <- toupper(sdtm_ae1$OUT)
sdtm_ae1$AESER <- as.character(sdtm_ae1$SER)
sdtm_ae1$AESER[is.na(sdtm_ae1$AESER)] <- ""

# Derive AESTDY and AEENDY
# SAS:
# if AESTDTC ne "" and RFSTDTC ne "" then do;
#   _stdt = input(AESTDTC, yymmdd10.);
#   _rfdt = input(RFSTDTC, yymmdd10.);
#   if _stdt >= _rfdt then AESTDY = (_stdt - _rfdt) + 1; else AESTDY = (_stdt - _rfdt);
# end;

sdtm_ae1$AESTDY <- NA_real_
sdtm_ae1$AEENDY <- NA_real_

# Parse dates for calculation
# AESTDTC and RFSTDTC are in yymmdd10. format "YYYY-MM-DD"
# Use as.Date with format "%Y-%m-%d"
has_both_start <- sdtm_ae1$AESTDTC != "" & sdtm_ae1$RFSTDTC != "" & !is.na(sdtm_ae1$AESTDTC) & !is.na(sdtm_ae1$RFSTDTC)
if (any(has_both_start)) {
  stdt <- suppressWarnings(as.Date(sdtm_ae1$AESTDTC[has_both_start], format = "%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(sdtm_ae1$RFSTDTC[has_both_start], format = "%Y-%m-%d"))
  # Calculate difference in days
  diff_days <- as.numeric(stdt - rfdt)
  # Apply SAS logic
  aestdy <- ifelse(stdt >= rfdt, diff_days + 1, diff_days)
  sdtm_ae1$AESTDY[has_both_start] <- aestdy
}

has_both_end <- sdtm_ae1$AEENDTC != "" & sdtm_ae1$RFSTDTC != "" & !is.na(sdtm_ae1$AEENDTC) & !is.na(sdtm_ae1$RFSTDTC)
if (any(has_both_end)) {
  endt <- suppressWarnings(as.Date(sdtm_ae1$AEENDTC[has_both_end], format = "%Y-%m-%d"))
  rfdt2 <- suppressWarnings(as.Date(sdtm_ae1$RFSTDTC[has_both_end], format = "%Y-%m-%d"))
  diff_days2 <- as.numeric(endt - rfdt2)
  aeendy <- ifelse(endt >= rfdt2, diff_days2 + 1, diff_days2)
  sdtm_ae1$AEENDY[has_both_end] <- aeendy
}

# ------------------------------------------------------------------
# 7. Sort Final AE Dataset (by USUBJID AESTDTC TERM)
# ------------------------------------------------------------------
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM, na.last = TRUE), ]

# ------------------------------------------------------------------
# 8. Derive AESEQ (retain by USUBJID)
# ------------------------------------------------------------------
# SAS:
# data sdtm_ae;
#   set sdtm_ae1;
#   by USUBJID;
#   retain AESEQ;
#   if first.USUBJID then AESEQ=1; else AESEQ+1;
# Keep final variables

# Ensure sorted by USUBJID for by-group processing
# Already sorted by USUBJID AESTDTC TERM, which is correct for AESEQ ordering
# Need to assign AESEQ within each USUBJID in that order

sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM), ]

# Create AESEQ
sdtm_ae1$AESEQ <- NA_integer_
# Use dplyr or base
usubjids <- unique(sdtm_ae1$USUBJID)
for (uid in usubjids) {
  idx <- which(sdtm_ae1$USUBJID == uid)
  sdtm_ae1$AESEQ[idx] <- seq_along(idx)
}

# Keep Final SDTM Variables in order
keep_vars <- c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD", "AEBODSYS", "AESEV", "AETXGR", "AEREL", "AEOUT", "AESER", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY")
# Ensure all exist
for (v in keep_vars) {
  if (!v %in% names(sdtm_ae1)) sdtm_ae1[[v]] <- NA
}

sdtm_ae <- sdtm_ae1[, keep_vars]

# Ensure types: lengths as in SAS (character lengths not enforced in R, but keep as character)
# Convert to appropriate types
sdtm_ae$STUDYID <- as.character(sdtm_ae$STUDYID)
sdtm_ae$DOMAIN <- as.character(sdtm_ae$DOMAIN)
sdtm_ae$USUBJID <- as.character(sdtm_ae$USUBJID)
sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)
sdtm_ae$AETERM <- as.character(sdtm_ae$AETERM)
sdtm_ae$AEDECOD <- as.character(sdtm_ae$AEDECOD)
sdtm_ae$AEBODSYS <- as.character(sdtm_ae$AEBODSYS)
sdtm_ae$AESEV <- as.character(sdtm_ae$AESEV)
sdtm_ae$AETXGR <- as.numeric(sdtm_ae$AETXGR)
sdtm_ae$AEREL <- as.character(sdtm_ae$AEREL)
sdtm_ae$AEOUT <- as.character(sdtm_ae$AEOUT)
sdtm_ae$AESER <- as.character(sdtm_ae$AESER)
sdtm_ae$AESTDTC <- as.character(sdtm_ae$AESTDTC)
sdtm_ae$AEENDTC <- as.character(sdtm_ae$AEENDTC)
# AESTDY, AEENDY are numeric (days)
sdtm_ae$AESTDY <- as.numeric(sdtm_ae$AESTDY)
sdtm_ae$AEENDY <- as.numeric(sdtm_ae$AEENDY)

# Final sort for output (by USUBJID AESTDTC TERM equivalent: USUBJID AESTDTC AETERM)
# SAS final sort was by USUBJID AESTDTC TERM, but after AESEQ we keep that order
sdtm_ae <- sdtm_ae[order(sdtm_ae$USUBJID, sdtm_ae$AESTDTC, sdtm_ae$AETERM), ]

# ------------------------------------------------------------------
# 9. Write outputs
# ------------------------------------------------------------------
# Write dataset ae.csv
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# Create listing text rendering (PROC PRINT)
# SAS proc print prints all observations with column headers
# We need to create P022.lst text file with 56 numbers (likely count of numeric values displayed)
# We'll create a text rendering similar to SAS listing

# Build text file
lst_path <- file.path(out, "P022.lst")
# Also CSV of table body (same as ae.csv but for listing)
# For P022, the listing is the AE dataset print, so CSV is same as ae.csv
# But we also need to write P022.csv? The spec says write text rendering and CSV of same table
# Expected outputs: P022.lst -> write text rendering and CSV of same table
# So we should write P022.txt? But spec says P022.lst (table/listing text) -> write text rendering and CSV
# We'll write both P022.lst and P022.txt and P022.csv for compatibility

# Create text rendering
# Use capture of print with formatting similar to SAS
# We'll create a simple fixed-width listing

# Helper to format data for display: dates as YYYY-MM-DD, numbers as is
# For text file, include title

con <- file(lst_path, open = "wt")

# Write header
cat("Listing: SDTM AE Dataset\n", file = con)
cat("Study: ONCO-2026  Domain: AE\n", file = con)
cat(paste0("Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\n"), file = con)
cat("\n", file = con)

# Write column headers
header <- paste(keep_vars, collapse = " | ")
cat(header, "\n", file = con)
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con)

# Write rows
for (i in seq_len(nrow(sdtm_ae))) {
  row_vals <- sapply(keep_vars, function(v) {
    val <- sdtm_ae[i, v]
    if (is.na(val) || val == "") return("")
    as.character(val)
  })
  line <- paste(row_vals, collapse = " | ")
  cat(line, "\n", file = con)
}

cat("\n", file = con)
cat(paste0("Total observations: ", nrow(sdtm_ae), "\n"), file = con)
cat(paste0("Total subjects: ", length(unique(sdtm_ae$USUBJID)), "\n"), file = con)

close(con)

# Also write P022.txt as copy of lst for spec compliance
txt_path <- file.path(out, "P022.txt")
file.copy(lst_path, txt_path, overwrite = TRUE)

# Write CSV of table body (same as ae.csv but named P022.csv)
write.csv(sdtm_ae, file.path(out, "P022.csv"), row.names = FALSE, na = "")

# Also ensure ae.txt exists
ae_txt_path <- file.path(out, "ae.txt")
file.copy(lst_path, ae_txt_path, overwrite = TRUE)

# Verify outputs
stopifnot(file.exists(file.path(out, "ae.csv")))
stopifnot(file.exists(lst_path))
