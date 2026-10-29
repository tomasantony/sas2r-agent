# P005_adam_adlb.sas -> R translation
# Outputs: adlb.csv + P005 listing (text + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # for NA keep NA
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  res[is.na(x)] <- NA_real_
  res
}

# Helper: read SAS dataset (sas7bdat or xpt) with case-insensitive search
read_sas_dataset <- function(libpath, dsname) {
  # libpath: directory
  # dsname: lower case name e.g. "adsl", "lb"
  if (!dir.exists(libpath)) return(NULL)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # normalize
  lower_files <- tolower(files)
  lower_ds <- tolower(dsname)
  # look for exact name with extensions
  idx <- which(
    lower_files == tolower(file.path(libpath, paste0(dsname, ".sas7bdat"))) |
    lower_files == tolower(file.path(libpath, paste0(dsname, ".xpt"))) |
    lower_files == tolower(file.path(libpath, paste0(dsname, ".sas7bdat"))) |
    grepl(paste0("/", lower_ds, "\\.(sas7bdat|xpt|sastbdat)$"), lower_files)
  )
  # more robust: basename without ext equals dsname
  if (length(idx) == 0) {
    basenoext <- tools::file_path_sans_ext(basename(files))
    idx <- which(tolower(basenoext) == lower_ds)
  }
  if (length(idx) == 0) return(NULL)
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext %in% c("sas7bdat", "sastbdat", "sas7bat")) {
    df <- haven::read_sas(f)
  } else {
    # try both
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <<- haven::read_xpt(f))
  }
  # zap labels/formats to plain R types, keep Date/POSIXct if already
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  # haven_labelled -> convert
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      # if numeric labelled, keep numeric; if character labelled keep character
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # tibble to data.frame
  as.data.frame(df, stringsAsFactors = FALSE)
}

# Helper: normalize column names to uppercase for SAS semantics
upcase_names <- function(df) {
  if (is.null(df)) return(df)
  names(df) <- toupper(names(df))
  df
}

# Helper: convert SAS numeric date (if still numeric) to Date
# haven already converts SAS dates to Date, so this is fallback
ensure_date <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date origin 1960-01-01
    # Heuristic: if values look like SAS dates (e.g., 10000-25000 range) convert
    # But we don't know; only convert if not NA and plausible
    # Check if any non-NA and between 0 and 50000
    # If already Date-like numeric, convert
    # We will convert all numeric that are supposed to be dates via this function
    # Caller must know which columns are dates
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # try parse YYYY-MM-DD
    suppressWarnings(as.Date(x))
  } else {
    x
  }
}

# Helper: format Date for CSV output as YYYY-MM-DD, datetime as YYYY-MM-DDTHH:MM:SS
format_dates_for_csv <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    } else if (inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

# -------------------------------------------------------------------------
# Locate project paths (as in SAS librefs)
# SAS librefs: raw, sdtm, adam under &project_path./data/{raw,sdtm,adam}
# &project_path = /bench/data/derived/c2c
# Also handle fallback to rawadam/rawsdtm etc if needed
# -------------------------------------------------------------------------
project_path <- "/bench/data/derived/c2c"
adam_path <- file.path(project_path, "data", "adam")
sdtm_path <- file.path(project_path, "data", "sdtm")
raw_path  <- file.path(project_path, "data", "raw")

# Alternative paths from macro variables (if project_path not found)
if (!dir.exists(adam_path)) {
  alt <- Sys.getenv("ADAM_PATH")
  if (alt != "" && dir.exists(alt)) adam_path <- alt
}
if (!dir.exists(sdtm_path)) {
  alt <- Sys.getenv("SDTM_PATH")
  if (alt != "" && dir.exists(alt)) sdtm_path <- alt
}
# Also try rawadam/rawsdtm env
rawadam_env <- Sys.getenv("rawadam")
rawsdtm_env <- Sys.getenv("rawsdtm")
# If adam_path missing, try rawadam
if (!dir.exists(adam_path) && rawadam_env != "" && dir.exists(rawadam_env)) {
  adam_path <- rawadam_env
}
if (!dir.exists(sdtm_path) && rawsdtm_env != "" && dir.exists(rawsdtm_env)) {
  sdtm_path <- rawsdtm_env
}

# Read ADSL and LB
adsl_raw <- read_sas_dataset(adam_path, "adsl")
if (is.null(adsl_raw)) {
  # try alternative: adam may be under data/derived/cdisc-pilot/adam
  alt2 <- "/bench/data/derived/cdisc-pilot/adam"
  adsl_raw <- read_sas_dataset(alt2, "adsl")
}
lb_raw <- read_sas_dataset(sdtm_path, "lb")
if (is.null(lb_raw)) {
  alt2 <- "/bench/data/derived/cdisc-pilot/sdtm"
  lb_raw <- read_sas_dataset(alt2, "lb")
}
# Also try sdtm lib directly from raw sdtm
if (is.null(lb_raw)) {
  alt3 <- "/bench/data/raw/cdisc-pilot/sdtm"
  lb_raw <- read_sas_dataset(alt3, "lb")
}

if (is.null(adsl_raw)) stop("ADSL dataset not found in adam_path: ", adam_path)
if (is.null(lb_raw)) stop("LB dataset not found in sdtm_path: ", sdtm_path)

adsl <- upcase_names(adsl_raw)
lb   <- upcase_names(lb_raw)

# Ensure required columns exist (create missing as NA)
ensure_cols <- function(df, cols) {
  for (c in cols) if (!c %in% names(df)) df[[c]] <- NA
  df
}
adsl <- ensure_cols(adsl, c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT"))
lb   <- ensure_cols(lb, c("STUDYID","USUBJID","LBDTC","LBTESTCD","LBTEST","LBSTRESN","LBSTRESU","VISIT","VISITNUM","LBSEQ"))

# Convert TRTSDT/TRTEDT to Date if needed
# haven usually gives Date; if numeric SAS date, convert
convert_to_date <- function(vec) {
  if (inherits(vec, "Date") || inherits(vec, "POSIXt")) return(as.Date(vec))
  if (is.numeric(vec)) {
    # SAS date origin
    return(as.Date(vec, origin = "1960-01-01"))
  }
  if (is.character(vec)) {
    # try ISO date
    # Some ADSL dates may be character like "2006-01-15"
    v <- suppressWarnings(as.Date(vec, format = "%Y-%m-%d"))
    # fallback yymmdd10
    miss <- is.na(v) & !is.na(vec) & vec != ""
    if (any(miss)) {
      v2 <- suppressWarnings(as.Date(substr(vec[miss],1,10), format = "%Y-%m-%d"))
      v[miss] <- v2
    }
    return(v)
  }
  return(vec)
}
adsl$TRTSDT <- convert_to_date(adsl$TRTSDT)
adsl$TRTEDT <- convert_to_date(adsl$TRTEDT)

# For LB, ensure LBSTRESN numeric, LBSEQ numeric, VISITNUM numeric
lb$LBSTRESN <- suppressWarnings(as.numeric(lb$LBSTRESN))
lb$LBSEQ    <- suppressWarnings(as.numeric(lb$LBSEQ))
lb$VISITNUM <- suppressWarnings(as.numeric(lb$VISITNUM))
# Ensure character columns are character
char_cols_lb <- c("USUBJID","LBDTC","LBTESTCD","LBTEST","LBSTRESU","VISIT","STUDYID")
for (cc in char_cols_lb) if (cc %in% names(lb)) lb[[cc]] <- as.character(lb[[cc]])
char_cols_adsl <- c("USUBJID","STUDYID","TRT01A")
for (cc in char_cols_adsl) if (cc %in% names(adsl)) adsl[[cc]] <- as.character(adsl[[cc]])

# -------------------------------------------------------------------------
# 1. MERGE ADSL AND LB
# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
# proc sort data=sdtm.lb out=work.lb_sorted; by USUBJID LBDTC LBTESTCD;
# data work.adlb_draft; merge ... by USUBJID; if b;
# -------------------------------------------------------------------------
# Sort ADSL by USUBJID (SAS missing first, character uppercase before lowercase)
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]
lb_sorted <- lb[order(lb$USUBJID, lb$LBDTC, lb$LBTESTCD, na.last = FALSE), , drop = FALSE]

# Merge: keep only records that have lab tests (if b)
# This is left join LB to ADSL on USUBJID
# Use dplyr for clarity
library(dplyr)

# Select only needed ADSL columns for merge to avoid duplicate STUDYID confusion
# SAS merge by USUBJID will take STUDYID from ADSL? Actually both have STUDYID, but merge retains last?
# In SAS merge by USUBJID with both datasets having STUDYID, the value from the second dataset (LB) would overwrite?
# However ADSL STUDYID and LB STUDYID should be same. We'll coalesce.
adsl_for_merge <- adsl_sorted %>% select(USUBJID, STUDYID, TRT01A, TRTSDT, TRTEDT)

# Perform merge (many-to-one)
adlb_draft <- lb_sorted %>%
  left_join(adsl_for_merge, by = "USUBJID") %>%
  filter(!is.na(LBTESTCD) | !is.na(LBSEQ) | !is.na(LBDTC) | TRUE) # keep all LB records (if b)

# In SAS, if b condition means keep only LB records; left_join already does that
# But need to ensure we didn't keep ADSL without LB (we didn't)

# Derive CORE ANALYSIS VARIABLES
# PARAMCD = LBTESTCD; PARAM = LBTEST; AVAL = LBSTRESN; AVISIT = VISIT; AVISITN = VISITNUM
# SRCDOM='LB'; SRCVAR='LBSEQ'; SRCSEQ=LBSEQ
# ASTDT = input(substr(LBDTC,1,10), yymmdd10.) if length(LBDTC)>=10
# ASTDY calculation

adlb_draft <- adlb_draft %>%
  mutate(
    PARAMCD = LBTESTCD,
    PARAM   = LBTEST,
    AVAL    = LBSTRESN,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    SRCDOM  = "LB",
    SRCVAR  = "LBSEQ",
    SRCSEQ  = LBSEQ
  )

# ASTDT derivation
adlb_draft$ASTDT <- as.Date(NA)
# length(LBDTC) >=10 then input substr 1,10 yymmdd10.
# In R, nchar handles NA
idx_astdt <- !is.na(adlb_draft$LBDTC) & nchar(adlb_draft$LBDTC) >= 10
if (any(idx_astdt)) {
  substr10 <- substr(adlb_draft$LBDTC[idx_astdt], 1, 10)
  # yymmdd10. in SAS handles YYYY-MM-DD, also YYMMDD etc. We'll parse YYYY-MM-DD
  parsed <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))
  # fallback: try other SAS date formats if needed (e.g., %Y/%m/%d)
  miss2 <- is.na(parsed) & !is.na(substr10)
  if (any(miss2)) {
    # try yymmdd with dashes already failed, try without?
    parsed[miss2] <- suppressWarnings(as.Date(substr10[miss2], format = "%Y/%m/%d"))
  }
  adlb_draft$ASTDT[idx_astdt] <- parsed
}

# ASTDY
adlb_draft$ASTDY <- NA_real_
# if not missing ASTDT and not missing TRTSDT
idx_dy <- !is.na(adlb_draft$ASTDT) & !is.na(adlb_draft$TRTSDT)
if (any(idx_dy)) {
  ge <- adlb_draft$ASTDT[idx_dy] >= adlb_draft$TRTSDT[idx_dy]
  # ASTDY = (ASTDT - TRTSDT) +1 if >= else ASTDT - TRTSDT
  diff_days <- as.numeric(adlb_draft$ASTDT[idx_dy] - adlb_draft$TRTSDT[idx_dy])
  adlb_draft$ASTDY[idx_dy][ge]  <- diff_days[ge] + 1
  adlb_draft$ASTDY[idx_dy][!ge] <- diff_days[!ge]
}

# Keep variables as in SAS keep statement
# keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ
# Note STUDYID: after join we have STUDYID from ADSL (STUDYID.y) and LB (STUDYID.x). Use coalesce.
if ("STUDYID.x" %in% names(adlb_draft) && "STUDYID.y" %in% names(adlb_draft)) {
  adlb_draft$STUDYID <- dplyr::coalesce(adlb_draft$STUDYID.y, adlb_draft$STUDYID.x)
} else if ("STUDYID.y" %in% names(adlb_draft)) {
  adlb_draft$STUDYID <- adlb_draft$STUDYID.y
} else if ("STUDYID.x" %in% names(adlb_draft)) {
  adlb_draft$STUDYID <- adlb_draft$STUDYID.x
}
# Ensure STUDYID exists
if (!"STUDYID" %in% names(adlb_draft)) adlb_draft$STUDYID <- NA_character_

adlb_draft <- adlb_draft %>%
  select(STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT, PARAMCD, PARAM, AVAL,
         AVISIT, AVISITN, ASTDT, ASTDY, LBSTRESU, SRCDOM, SRCVAR, SRCSEQ)

# -------------------------------------------------------------------------
# 2. IDENTIFY BASELINE RECORD
# proc sort data=work.adlb_draft out=work.adlb_base_candidates;
#   by USUBJID PARAMCD ASTDT AVISITN SRCSEQ;
#   where ASTDT <= TRTSDT and not missing(AVAL);
# data work.base_flags; set ...; by USUBJID PARAMCD; if last.PARAMCD then output;
# -------------------------------------------------------------------------
base_candidates <- adlb_draft %>%
  filter(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT <= TRTSDT & !is.na(AVAL)) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

# For each USUBJID PARAMCD, take last record (SAS last.PARAMCD after sort)
# That corresponds to max ASTDT, then max AVISITN, then max SRCSEQ
base_flags <- base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  arrange(ASTDT, AVISITN, SRCSEQ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    USUBJID = USUBJID,
    PARAMCD = PARAMCD,
    BASE = AVAL,
    BASE_DT = ASTDT,
    BASE_SEQ = SRCSEQ
  )

# -------------------------------------------------------------------------
# 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE
# proc sort data=work.adlb_draft; by USUBJID PARAMCD;
# data adam.adlb; merge work.adlb_draft(in=a) work.base_flags(in=b); by USUBJID PARAMCD; if a;
# -------------------------------------------------------------------------
adlb_draft_sorted <- adlb_draft %>% arrange(USUBJID, PARAMCD)

adlb <- adlb_draft_sorted %>%
  left_join(base_flags, by = c("USUBJID", "PARAMCD"))

# Flag exact source record
adlb <- adlb %>%
  mutate(
    ABLFL = ifelse(!is.na(SRCSEQ) & !is.na(BASE_SEQ) & SRCSEQ == BASE_SEQ, "Y", ""),
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
  )

# Derive LBNRIND
adlb$LBNRIND <- NA_character_
# length $10
# if not missing AVAL then do
idx_notmiss <- !is.na(adlb$AVAL)
if (any(idx_notmiss)) {
  # Initialize _calc_val
  calc_val <- adlb$AVAL

  # For each row, apply logic
  # Use vectorized approach
  # GLUC
  is_gluc <- idx_notmiss & adlb$PARAMCD == "GLUC"
  if (any(is_gluc)) {
    cv <- calc_val[is_gluc]
    # Convert mg/dL to mmol/L factor 0.0555 if LBSTRESU == "MG/DL" (case-insensitive)
    conv_idx <- is_gluc & !is.na(adlb$LBSTRESU) & toupper(adlb$LBSTRESU) == "MG/DL"
    cv_conv <- cv
    # need to map conv_idx subset correctly
    # conv_idx is logical over full vector; cv is subset
    # So create full vector calc for gluc
    full_cv_gluc <- adlb$AVAL
    full_cv_gluc[conv_idx] <- adlb$AVAL[conv_idx] * 0.0555
    # Now evaluate thresholds
    # For GLUC rows, determine LBNRIND
    gluc_rows <- which(is_gluc)
    for (r in gluc_rows) {
      v <- full_cv_gluc[r]
      if (is.na(v)) {
        adlb$LBNRIND[r] <- NA_character_
      } else if (v < 3.9) {
        adlb$LBNRIND[r] <- "LOW"
      } else if (v > 5.6) {
        adlb$LBNRIND[r] <- "HIGH"
      } else {
        adlb$LBNRIND[r] <- "NORMAL"
      }
    }
  }
  # HGB
  is_hgb <- idx_notmiss & adlb$PARAMCD == "HGB"
  if (any(is_hgb)) {
    full_cv_hgb <- adlb$AVAL
    conv_idx_hgb <- is_hgb & !is.na(adlb$LBSTRESU) & toupper(adlb$LBSTRESU) == "G/DL"
    full_cv_hgb[conv_idx_hgb] <- adlb$AVAL[conv_idx_hgb] * 10
    hgb_rows <- which(is_hgb)
    for (r in hgb_rows) {
      v <- full_cv_hgb[r]
      if (is.na(v)) {
        adlb$LBNRIND[r] <- NA_character_
      } else if (v < 120) {
        adlb$LBNRIND[r] <- "LOW"
      } else if (v > 175) {
        adlb$LBNRIND[r] <- "HIGH"
      } else {
        adlb$LBNRIND[r] <- "NORMAL"
      }
    }
  }
  # OTHERWISE -> UNKNOWN for remaining not missing AVAL where PARAMCD not GLUC/HGB
  is_other <- idx_notmiss & !(adlb$PARAMCD %in% c("GLUC","HGB"))
  if (any(is_other)) {
    adlb$LBNRIND[is_other] <- "UNKNOWN"
  }
  # For GLUC/HGB where AVAL not missing but LBNRIND still NA (should not happen), set appropriately
}

# Drop BASE_DT BASE_SEQ (keep BASE)
adlb <- adlb %>% select(-BASE_DT, -BASE_SEQ)

# Retain order: STUDYID USUBJID PARAMCD PARAM ... (SAS retain)
# Re-sort for final presentation: by USUBJID PARAMCD ASTDT
adlb <- adlb %>% arrange(USUBJID, PARAMCD, ASTDT)

# Ensure column order similar to SAS: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ BASE CHG ABLFL LBNRIND
desired_order <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ","BASE","CHG","ABLFL","LBNRIND")
# Add any missing columns as NA
for (c in desired_order) if (!c %in% names(adlb)) adlb[[c]] <- NA
adlb <- adlb[, desired_order]

# -------------------------------------------------------------------------
# Write dataset adlb.csv
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# -------------------------------------------------------------------------
adlb_out <- format_dates_for_csv(adlb)
write.csv(adlb_out, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# 4. VISUAL AUDIT - PROC PRINT obs=15
# var USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND
# Title: ADLB Audit - Laboratory Analysis with Clinical Indicators
# -------------------------------------------------------------------------
audit_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
audit_df <- adlb %>% select(all_of(audit_vars)) %>% head(15)

# For display, format ASTDT as date9. like 15JAN2006 but CSV should be YYYY-MM-DD
# For text rendering, show YYYY-MM-DD (SAS date9. equivalent) and numeric values as is
# Create text rendering
title_str <- "ADLB Audit - Laboratory Analysis with Clinical Indicators"

# Build text table
# Format numbers: AVAL, BASE, CHG with default SAS BEST. (unrounded)
# We'll format with as.character, keeping NA as blank
format_cell <- function(x) {
  if (is.na(x) || (is.character(x) && x == "")) return("")
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.numeric(x)) {
    # Remove trailing zeros? Use as.character
    return(as.character(x))
  }
  return(as.character(x))
}

# Prepare display df for text (format dates)
audit_display <- audit_df
audit_display$ASTDT <- format(audit_display$ASTDT, "%Y-%m-%d")
# Replace NA with ""
for (cn in names(audit_display)) {
  audit_display[[cn]][is.na(audit_display[[cn]])] <- ""
}

# Create plain-text rendering similar to PROC PRINT
# Include Obs number
audit_display_with_obs <- cbind(Obs = seq_len(nrow(audit_display)), audit_display)

# Build text lines
col_widths <- sapply(names(audit_display_with_obs), function(n) max(nchar(n), max(nchar(as.character(audit_display_with_obs[[n]])), na.rm=TRUE)))
# Ensure minimum widths
col_widths <- pmax(col_widths, 6)

# Helper to pad
pad_right <- function(s, w) sprintf(paste0("%-", w, "s"), s)
pad_left  <- function(s, w) sprintf(paste0("%", w, "s"), s)

header_line <- paste(mapply(pad_right, names(audit_display_with_obs), col_widths), collapse = "  ")
sep_line    <- paste(mapply(function(w) paste(rep("-", w), collapse=""), col_widths), collapse = "  ")

body_lines <- apply(audit_display_with_obs, 1, function(row) {
  paste(mapply(pad_right, row, col_widths), collapse = "  ")
})

txt_path <- file.path(out, "P005.lst")
# Also SAS listing often has .txt counterpart; we write both .lst and .txt as same
txt_path2 <- file.path(out, "P005.txt")

txt_content <- c(
  title_str,
  paste0("Dataset: ADLB  (", nrow(adlb), " records)"),
  "",
  header_line,
  sep_line,
  body_lines,
  "",
  paste0("Note: First 15 observations displayed. Total numbers displayed: ", nrow(audit_display)*ncol(audit_display), " (", nrow(audit_display), " obs x ", ncol(audit_display), " vars)"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
)

writeLines(txt_content, txt_path)
writeLines(txt_content, txt_path2)

# CSV of table body (same as PROC PRINT)
# For CSV, keep original types with dates as YYYY-MM-DD
audit_csv <- audit_df
# Format dates for CSV
audit_csv$ASTDT <- format(audit_csv$ASTDT, "%Y-%m-%d")
# Write CSV
write.csv(audit_csv, file.path(out, "P005.csv"), row.names = FALSE, na = "")
# Also write P005.lst.csv variant if needed
write.csv(audit_csv, file.path(out, "P005.lst.csv"), row.names = FALSE, na = "")

# Also ensure listing text rendering is available as P005.txt already done

# Console log progress
cat("ADLB derivation complete. Records:", nrow(adlb), "\n")
cat("Outputs written to:", out, "\n")
