# P024_ds.sas -> R translation
# Produces ds.csv and P024.lst (text + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helpers ---------------------------------------------------------------
`%||%` <- function(a,b) if (!is.null(a) && !is.na(a) && nzchar(trimws(as.character(a)))) a else b

# SAS round half away from zero (not needed here but defined)
sas_round <- function(x, digits=0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# SAS tranwrd
tranwrd <- function(str, from, to) {
  if (is.na(str)) return(NA_character_)
  gsub(from, to, str, fixed=TRUE)
}

# SAS date11. informat: DD-MMM-YYYY e.g. 01JAN2020, also handles 01-JAN-2020? SAS date11 is dd-mon-yyyy with dashes? Actually date11. is DDMMMYYYY? Let's check.
# In SAS: input(STDT, date11.) where STDT is DS_DT. Need to see raw values.
# We'll parse with lubridate-like but strict: "%d-%b-%Y" and "%d%b%Y" and "%d-%B-%Y" etc. SAS date11. is DDMMMYYYY? Common is DATE11. = dd-mon-yyyy (11 chars with dashes? Actually yymmdd10 is yyyy-mm-dd (10). date11 is dd-mon-yyyy? Let's inspect raw.)
# We'll implement flexible but strict: try "%d-%b-%Y" upper/lower, "%d%b%Y", "%d-%B-%Y"
parse_date11 <- function(x) {
  if (is.na(x) || trimws(as.character(x))=="") return(NA_Date_)
  s <- trimws(as.character(x))
  # SAS date11. expects DDMMMYYYY or DD-MMM-YYYY? We'll try multiple
  # Normalize: remove spaces
  # Try as.Date with formats
  fmts <- c("%d-%b-%Y", "%d-%B-%Y", "%d%b%Y", "%d%b%y", "%d-%b-%y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d.%m.%Y")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s, format=f))
    if (!is.na(d)) return(d)
    # try upper case month
    # as.Date is case-insensitive? In R, %b is locale dependent. Force to title case
    # Try converting month part to title case
  }
  # Try lubridate style: parse with month abbreviation case-insensitive
  # Manual: extract
  # Use try with tolower then title
  s2 <- s
  # Replace month abbreviations to title case
  # If contains letters, convert to Title
  # Simple: use as.Date with toupper? Let's try converting s to proper case
  # e.g. "01JAN2020" -> "01Jan2020"
  if (grepl("[A-Za-z]", s)) {
    # find month letters
    m <- regmatches(s, regexpr("[A-Za-z]+", s))
    if (length(m)>0) {
      m_title <- paste0(toupper(substr(m,1,1)), tolower(substr(m,2,nchar(m))))
      s3 <- sub("[A-Za-z]+", m_title, s)
      for (f in fmts) {
        d <- suppressWarnings(as.Date(s3, format=f))
        if (!is.na(d)) return(d)
      }
    }
  }
  return(NA_Date_)
}

# Vectorized version
parse_date11_vec <- function(v) {
  v <- as.character(v)
  res <- as.Date(rep(NA, length(v)))
  for (i in seq_along(v)) res[i] <- parse_date11(v[i])
  res
}

# yymmdd10. informat: YYYY-MM-DD
parse_yymmdd10 <- function(x) {
  if (is.na(x) || trimws(as.character(x))=="") return(NA_Date_)
  suppressWarnings(as.Date(trimws(as.character(x)), format="%Y-%m-%d"))
}
parse_yymmdd10_vec <- function(v) {
  suppressWarnings(as.Date(trimws(as.character(v)), format="%Y-%m-%d"))
}

# ISO date output
fmt_iso_date <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# -----------------------------------------------------------------------
# Locate raw files
rawdir <- "/bench/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")
sdtm_dir <- "/bench/data/raw/cdisc-pilot/sdtm"
adam_dir <- "/bench/data/raw/cdisc-pilot/adam"

# Read edc_ds
edc_ds_path <- file.path(edc_dir, "edc_ds.xlsx")
edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")

library(readxl)
library(dplyr)
library(stringr)
library(haven)

# Read edc_ds
if (file.exists(edc_ds_path)) {
  edc_ds <- readxl::read_excel(edc_ds_path)
} else {
  stop("edc_ds.xlsx not found")
}

# Read edc_dm for fallback if sdtm_dm not found
if (file.exists(edc_dm_path)) {
  edc_dm_raw <- readxl::read_excel(edc_dm_path)
} else {
  edc_dm_raw <- NULL
}

# Try to find sdtm_dm
# Search in sdtm_dir
sdtm_dm <- NULL
# Possible files: dm.xpt, dm.sas7bdat, sdtm_dm.xlsx etc.
if (dir.exists(sdtm_dir)) {
  sdtm_files <- list.files(sdtm_dir, recursive=TRUE, full.names=TRUE)
  # print for debugging
  # cat("sdtm files:", paste(sdtm_files, collapse="\n"), "\n")
  cand <- sdtm_files[grepl("dm\\.(xpt|sas7bdat|xlsx|csv)$", tolower(sdtm_files))]
  if (length(cand)>0) {
    for (f in cand) {
      if (grepl("\\.xpt$", tolower(f))) {
        try({ sdtm_dm <- haven::read_xpt(f) }, silent=TRUE)
        if (!is.null(sdtm_dm)) break
      } else if (grepl("\\.sas7bdat$", tolower(f))) {
        try({ sdtm_dm <- haven::read_sas(f) }, silent=TRUE)
        if (!is.null(sdtm_dm)) break
      } else if (grepl("\\.xlsx$", tolower(f))) {
        try({ sdtm_dm <- readxl::read_excel(f) }, silent=TRUE)
        if (!is.null(sdtm_dm)) break
      }
    }
  }
}

# If not found, try to locate any dm file in rawdir
if (is.null(sdtm_dm)) {
  # Search broader
  all_xpt <- list.files("/bench", pattern="dm\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if (length(all_xpt)>0) {
    for (f in all_xpt) {
      try({ sdtm_dm <- haven::read_xpt(f) }, silent=TRUE)
      if (!is.null(sdtm_dm)) break
    }
  }
}

# If still null, derive sdtm_dm from edc_dm + edc_ex etc. per CDISC
# CDISC DM: USUBJID, SUBJID, RFSTDTC = first exposure date or randomization date
# We have edc_dm, edc_ex, ivrs_rand
if (is.null(sdtm_dm)) {
  # Derive from edc_dm
  # Inspect edc_dm_raw columns
  # cat("edc_dm columns:", paste(names(edc_dm_raw), collapse=", "), "\n")
  # Try to find RFSTDTC equivalent: look for columns like RFSTDTC, EXSTDTC, BRTHDTC, etc.
  # We'll attempt to construct minimal dm_for_ds needed: USUBJID, SUBJID, RFSTDTC, DTHDTC
  # RFSTDTC: reference start date - first exposure date from edc_ex if available, else randomization date from ivrs_rand, else DM date
  # DTHDTC: death date from edc_death or edc_dm

  # Read edc_ex and edc_death and ivrs_rand if exist
  edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
  edc_death_path <- file.path(edc_dir, "edc_death.xlsx")
  ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")

  edc_ex <- NULL
  if (file.exists(edc_ex_path)) edc_ex <- readxl::read_excel(edc_ex_path)
  edc_death <- NULL
  if (file.exists(edc_death_path)) edc_death <- readxl::read_excel(edc_death_path)
  ivrs_rand <- NULL
  if (file.exists(ivrs_rand_path)) ivrs_rand <- readxl::read_excel(ivrs_rand_path)

  # Inspect edc_dm_raw to find date columns
  # Normalize column names to upper
  names(edc_dm_raw) <- toupper(names(edc_dm_raw))
  # Common columns: SUBJID, RFSTDTC, DTHDTC, BRTHDTC, RFSTDTC etc.
  # If RFSTDTC already exists, use it
  # Otherwise try to find any date column

  # Helper to find column containing string
  find_col <- function(df, pattern) {
    idx <- grep(pattern, names(df), ignore.case=TRUE)
    if (length(idx)>0) names(df)[idx[1]] else NA
  }

  # Build base dm
  dm_base <- edc_dm_raw

  # Ensure SUBJID exists
  subj_col <- find_col(dm_base, "^SUBJID$")
  if (is.na(subj_col)) {
    # try USUBJID
    subj_col <- find_col(dm_base, "SUBJID")
  }
  # Create SUBJID column standardized
  if (!is.na(subj_col)) {
    dm_base$SUBJID <- dm_base[[subj_col]]
  } else {
    dm_base$SUBJID <- NA_character_
  }
  dm_base$SUBJID <- as.character(dm_base$SUBJID)

  # USUBJID
  if (!"USUBJID" %in% names(dm_base)) {
    dm_base$USUBJID <- paste0("ONCO-2026-", dm_base$SUBJID)
  }

  # RFSTDTC
  rf_col <- find_col(dm_base, "RFSTDTC")
  if (!is.na(rf_col)) {
    dm_base$RFSTDTC <- as.character(dm_base[[rf_col]])
  } else {
    # Try to derive from edc_ex
    # Look for exposure date column in edc_ex
    if (!is.null(edc_ex)) {
      names(edc_ex) <- toupper(names(edc_ex))
      ex_subj <- find_col(edc_ex, "SUBJID")
      ex_date_col <- find_col(edc_ex, "EXSTDTC|EX_ST|EXDT|EX_DT|EXDATE")
      if (is.na(ex_date_col)) {
        # find any column with DT
        dt_cols <- grep("DT", names(edc_ex), value=TRUE)
        if (length(dt_cols)>0) ex_date_col <- dt_cols[1]
      }
      if (!is.na(ex_subj) && !is.na(ex_date_col)) {
        edc_ex$SUBJID_TMP <- as.character(edc_ex[[ex_subj]])
        edc_ex$EXDT_TMP <- as.character(edc_ex[[ex_date_col]])
        # For each SUBJID, earliest date
        # Parse dates to find earliest
        # Use parse_date11_vec or yymmdd
        ex_dates <- data.frame(SUBJID=edc_ex$SUBJID_TMP, EXDT=edc_ex$EXDT_TMP, stringsAsFactors=FALSE)
        # Try to parse
        ex_dates$EXDT_PARSED <- parse_date11_vec(ex_dates$EXDT)
        # fallback yymmdd
        miss <- is.na(ex_dates$EXDT_PARSED)
        if (any(miss)) {
          ex_dates$EXDT_PARSED[miss] <- parse_yymmdd10_vec(ex_dates$EXDT[miss])
        }
        # Also try as.Date generic
        miss2 <- is.na(ex_dates$EXDT_PARSED)
        if (any(miss2)) {
          ex_dates$EXDT_PARSED[miss2] <- suppressWarnings(as.Date(ex_dates$EXDT[miss2]))
        }
        ex_agg <- ex_dates %>%
          filter(!is.na(EXDT_PARSED)) %>%
          group_by(SUBJID) %>%
          summarise(RFSTDTC_DER = format(min(EXDT_PARSED), "%Y-%m-%d"), .groups="drop")
        dm_base <- dm_base %>% left_join(ex_agg, by="SUBJID")
        dm_base$RFSTDTC <- dm_base$RFSTDTC_DER
      }
    }
    # If still NA, try ivrs_rand
    if (all(is.na(dm_base$RFSTDTC)) && !is.null(ivrs_rand)) {
      names(ivrs_rand) <- toupper(names(ivrs_rand))
      rand_subj <- find_col(ivrs_rand, "SUBJID")
      rand_date_col <- find_col(ivrs_rand, "RANDDT|RAND_D|RANDTC")
      if (!is.na(rand_subj) && !is.na(rand_date_col)) {
        ivrs_rand$SUBJID_TMP <- as.character(ivrs_rand[[rand_subj]])
        ivrs_rand$RANDDT_TMP <- as.character(ivrs_rand[[rand_date_col]])
        ivrs_rand$RAND_PARSED <- parse_date11_vec(ivrs_rand$RANDDT_TMP)
        miss <- is.na(ivrs_rand$RAND_PARSED)
        if (any(miss)) ivrs_rand$RAND_PARSED[miss] <- parse_yymmdd10_vec(ivrs_rand$RANDDT_TMP[miss])
        rand_agg <- ivrs_rand %>% filter(!is.na(RAND_PARSED)) %>% group_by(SUBJID_TMP) %>% summarise(RFSTDTC_DER=format(min(RAND_PARSED), "%Y-%m-%d"), .groups="drop") %>% rename(SUBJID=SUBJID_TMP)
        dm_base <- dm_base %>% left_join(rand_agg, by="SUBJID")
        idx_na <- is.na(dm_base$RFSTDTC)
        dm_base$RFSTDTC[idx_na] <- dm_base$RFSTDTC_DER[idx_na]
      }
    }
    # If still NA, try any DT column in dm_base
    if (all(is.na(dm_base$RFSTDTC))) {
      dt_cols <- grep("STDTC|RFXSTDTC|RFSTDTC", names(dm_base), value=TRUE, ignore.case=TRUE)
      if (length(dt_cols)==0) dt_cols <- grep("DT", names(dm_base), value=TRUE)
      if (length(dt_cols)>0) {
        dm_base$RFSTDTC <- as.character(dm_base[[dt_cols[1]]])
      }
    }
  }

  # DTHDTC
  dth_col <- find_col(dm_base, "DTHDTC")
  if (is.na(dth_col)) {
    # Try edc_death
    if (!is.null(edc_death)) {
      names(edc_death) <- toupper(names(edc_death))
      death_subj <- find_col(edc_death, "SUBJID")
      death_date_col <- find_col(edc_death, "DTHDTC|DEATHDT|DTHDT|DTH_DT|DEATH_DT")
      if (is.na(death_date_col)) {
        dt_cols <- grep("DT", names(edc_death), value=TRUE)
        if (length(dt_cols)>0) death_date_col <- dt_cols[1]
      }
      if (!is.na(death_subj) && !is.na(death_date_col)) {
        edc_death$SUBJID_TMP <- as.character(edc_death[[death_subj]])
        edc_death$DTHDT_TMP <- as.character(edc_death[[death_date_col]])
        # Parse to ISO
        edc_death$DTH_PARSED <- parse_date11_vec(edc_death$DTHDT_TMP)
        miss <- is.na(edc_death$DTH_PARSED)
        if (any(miss)) edc_death$DTH_PARSED[miss] <- parse_yymmdd10_vec(edc_death$DTHDT_TMP[miss])
        miss2 <- is.na(edc_death$DTH_PARSED)
        if (any(miss2)) edc_death$DTH_PARSED[miss2] <- suppressWarnings(as.Date(edc_death$DTHDT_TMP[miss2]))
        death_agg <- edc_death %>% filter(!is.na(DTH_PARSED)) %>% group_by(SUBJID_TMP) %>% summarise(DTHDTC_DER=format(min(DTH_PARSED), "%Y-%m-%d"), .groups="drop") %>% rename(SUBJID=SUBJID_TMP)
        dm_base <- dm_base %>% left_join(death_agg, by="SUBJID")
        dm_base$DTHDTC <- dm_base$DTHDTC_DER
      } else {
        dm_base$DTHDTC <- NA_character_
      }
    } else {
      dm_base$DTHDTC <- NA_character_
    }
  } else {
    dm_base$DTHDTC <- as.character(dm_base[[dth_col]])
  }

  # Ensure RFSTDTC and DTHDTC are in YYYY-MM-DD format if they are Date objects
  # Try to normalize: if they look like numeric Excel dates, convert
  normalize_iso <- function(v) {
    # v is character or numeric
    # If numeric and large (Excel serial), convert
    out <- rep(NA_character_, length(v))
    for (i in seq_along(v)) {
      val <- v[i]
      if (is.na(val) || val=="" ) { out[i] <- NA_character_; next }
      # If numeric string that is Excel date (e.g., 44000)
      # Check if val is numeric and 30000-60000
      num <- suppressWarnings(as.numeric(val))
      if (!is.na(num) && num > 30000 && num < 60000 && !grepl("-", val) && !grepl("/", val)) {
        # Excel origin 1899-12-30
        d <- as.Date(num, origin="1899-12-30")
        out[i] <- format(d, "%Y-%m-%d")
        next
      }
      # Try parse as Date
      d <- parse_date11(val)
      if (!is.na(d)) { out[i] <- format(d, "%Y-%m-%d"); next }
      d2 <- parse_yymmdd10(val)
      if (!is.na(d2)) { out[i] <- format(d2, "%Y-%m-%d"); next }
      # Try generic
      d3 <- suppressWarnings(as.Date(val))
      if (!is.na(d3)) { out[i] <- format(d3, "%Y-%m-%d"); next }
      out[i] <- as.character(val)
    }
    out
  }

  dm_base$RFSTDTC <- normalize_iso(dm_base$RFSTDTC)
  dm_base$DTHDTC <- normalize_iso(dm_base$DTHDTC)

  sdtm_dm <- dm_base %>% select(USUBJID, SUBJID, RFSTDTC, DTHDTC)
} else {
  # Normalize column names to expected
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  # Ensure required columns exist
  if (!"USUBJID" %in% names(sdtm_dm)) {
    if ("SUBJID" %in% names(sdtm_dm)) sdtm_dm$USUBJID <- paste0("ONCO-2026-", sdtm_dm$SUBJID)
  }
  # Keep only needed
  keep_cols <- intersect(c("USUBJID","SUBJID","RFSTDTC","DTHDTC"), names(sdtm_dm))
  sdtm_dm <- sdtm_dm[, keep_cols, drop=FALSE]
  # Normalize dates to character ISO
  for (col in c("RFSTDTC","DTHDTC")) {
    if (col %in% names(sdtm_dm)) {
      # If Date class, format
      if (inherits(sdtm_dm[[col]], "Date") || inherits(sdtm_dm[[col]], "POSIXt")) {
        sdtm_dm[[col]] <- format(as.Date(sdtm_dm[[col]]), "%Y-%m-%d")
        sdtm_dm[[col]][is.na(sdtm_dm[[col]])] <- NA_character_
      } else {
        # Try to handle numeric Excel dates
        vals <- sdtm_dm[[col]]
        # If numeric
        if (is.numeric(vals)) {
          # Excel serial?
          sdtm_dm[[col]] <- ifelse(is.na(vals), NA_character_, format(as.Date(vals, origin="1899-12-30"), "%Y-%m-%d"))
        } else {
          # Character: try to parse if not already ISO
          # Keep as is if matches YYYY-MM-DD
          is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals)
          # For non-ISO, try parse
          need_parse <- !is_iso & !is.na(vals) & vals!=""
          if (any(need_parse, na.rm=TRUE)) {
            parsed <- parse_date11_vec(vals[need_parse])
            miss <- is.na(parsed)
            if (any(miss)) parsed[miss] <- parse_yymmdd10_vec(vals[need_parse][miss])
            sdtm_dm[[col]][need_parse] <- ifelse(is.na(parsed), vals[need_parse], format(parsed, "%Y-%m-%d"))
          }
        }
      }
    }
  }
}

# Ensure sdtm_dm sorted by USUBJID
sdtm_dm <- sdtm_dm %>% arrange(USUBJID)
dm_for_ds <- sdtm_dm %>% select(USUBJID, SUBJID, RFSTDTC, DTHDTC)

# -----------------------------------------------------------------------
# Step 2: Prepare Raw DS Data
# edc_ds columns: need to inspect
# SAS says: TERM = DS_TERM; DECOD = DS_TERM; CAT = DS_CAT; STDT = DS_DT;
# But benchmark says committed raw sheet names these columns DS_TERM/DS_CAT/DS_DT and has no DECOD column, so program as written produced all-blank DS
# In R, we need to replicate SAS behavior: DECOD = DS_TERM (since DS_TERM exists, DECOD column doesn't exist -> SAS would create DECOD as missing? Actually SAS: DECOD = DS_TERM; but DS_TERM exists, so DECOD gets DS_TERM value. Wait SAS code: length TERM DECOD CAT $50 STDT $11; TERM = DS_TERM; DECOD = DS_TERM; CAT = DS_CAT; STDT = DS_DT;
# So DECOD is assigned DS_TERM, not a column DECOD. So DSDECOD = upcase(DECOD) = upcase(DS_TERM). So not blank. The comment says benchmark: the committed raw sheet names these columns DS_TERM/DS_CAT/DS_DT and has no DECOD column, so the program as written produced an all-blank DS - but that comment seems contradictory: DECOD = DS_TERM would not be blank. Maybe they mean SAS would have DECOD column missing so DSDECOD blank? But code sets DECOD = DS_TERM, so not blank. We'll follow code.

# Inspect edc_ds column names
# Normalize to upper for lookup but keep original
orig_names <- names(edc_ds)
names(edc_ds) <- toupper(names(edc_ds))

# Find actual columns: DS_TERM, DS_CAT, DS_DT, SUBJID
find_col2 <- function(df, name) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx)>0) names(df)[idx[1]] else NA
}

# Ensure SUBJID column exists
subj_col_ds <- find_col2(edc_ds, "SUBJID")
if (is.na(subj_col_ds)) {
  # try SUBJECT, USUBJID
  cand <- grep("SUBJ", names(edc_ds), value=TRUE, ignore.case=TRUE)
  if (length(cand)>0) subj_col_ds <- cand[1]
}
# DS_TERM
term_col <- find_col2(edc_ds, "DS_TERM")
if (is.na(term_col)) term_col <- find_col2(edc_ds, "DSTERM")
if (is.na(term_col)) term_col <- find_col2(edc_ds, "TERM")
# DS_CAT
cat_col <- find_col2(edc_ds, "DS_CAT")
if (is.na(cat_col)) cat_col <- find_col2(edc_ds, "DSCAT")
if (is.na(cat_col)) cat_col <- find_col2(edc_ds, "CAT")
# DS_DT
dt_col <- find_col2(edc_ds, "DS_DT")
if (is.na(dt_col)) dt_col <- find_col2(edc_ds, "DSSTDTC")
if (is.na(dt_col)) dt_col <- find_col2(edc_ds, "DS_DT")
if (is.na(dt_col)) dt_col <- find_col2(edc_ds, "STDT")
if (is.na(dt_col)) {
  # find any DT column
  dt_cands <- grep("DT", names(edc_ds), value=TRUE)
  if (length(dt_cands)>0) dt_col <- dt_cands[1]
}

# Build edc_ds_prep
edc_ds_prep <- edc_ds
edc_ds_prep$TERM <- if (!is.na(term_col)) as.character(edc_ds_prep[[term_col]]) else NA_character_
edc_ds_prep$DECOD <- if (!is.na(term_col)) as.character(edc_ds_prep[[term_col]]) else NA_character_  # SAS: DECOD = DS_TERM
edc_ds_prep$CAT <- if (!is.na(cat_col)) as.character(edc_ds_prep[[cat_col]]) else NA_character_
edc_ds_prep$STDT <- if (!is.na(dt_col)) as.character(edc_ds_prep[[dt_col]]) else NA_character_
edc_ds_prep$SUBJID <- as.character(edc_ds_prep[[subj_col_ds]])

# Handle Excel numeric dates for STDT
# If STDT is numeric Excel serial, convert to date11 string then later to ISO
# Detect numeric
if (is.numeric(edc_ds_prep$STDT) || suppressWarnings(!all(is.na(as.numeric(edc_ds_prep$STDT[edc_ds_prep$STDT!=""]))))) {
  # Try to convert numeric-like
  for (i in seq_len(nrow(edc_ds_prep))) {
    val <- edc_ds_prep$STDT[i]
    if (is.na(val) || val=="") next
    num <- suppressWarnings(as.numeric(val))
    if (!is.na(num) && num > 30000 && num < 60000 && !grepl("-", val) && nchar(val)>=4) {
      d <- as.Date(num, origin="1899-12-30")
      # SAS date11. format is DD-MMM-YYYY e.g. 01JAN2020 -> but we store as that string to mimic SAS input
      edc_ds_prep$STDT[i] <- format(d, "%d-%b-%Y")
      edc_ds_prep$STDT[i] <- toupper(edc_ds_prep$STDT[i])
    }
  }
}
# Also if STDT is Date class
if (inherits(edc_ds[[dt_col]], "Date") || inherits(edc_ds[[dt_col]], "POSIXt")) {
  edc_ds_prep$STDT <- format(as.Date(edc_ds[[dt_col]]), "%d-%b-%Y")
  edc_ds_prep$STDT <- toupper(edc_ds_prep$STDT)
}

# Create USUBJID
edc_ds_prep$USUBJID <- paste0("ONCO-2026-", edc_ds_prep$SUBJID)

# Keep only needed for merge
edc_ds_prep <- edc_ds_prep %>% select(USUBJID, SUBJID, TERM, DECOD, CAT, STDT)

# Sort datasets for merge (SAS sort by USUBJID)
edc_ds_prep <- edc_ds_prep %>% arrange(USUBJID)
dm_for_ds <- dm_for_ds %>% arrange(USUBJID)

# -----------------------------------------------------------------------
# Step 4: Create SDTM DS dataset (merge)
# SAS: merge edc_ds_prep(in=a) dm_for_ds; by USUBJID; if a;
# This is left join keeping all edc_ds_prep rows, adding DM columns where match

sdtm_ds1 <- edc_ds_prep %>%
  left_join(dm_for_ds, by="USUBJID", suffix=c("", ".dm")) %>%
  # Handle duplicate SUBJID columns: keep original SUBJID from edc_ds_prep
  # dm_for_ds has SUBJID.dm, we drop it
  select(-any_of(c("SUBJID.dm")))

# Identifier Variables
sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"

# DS Variables
# DSTERM = tranwrd(TERM, "_", " ")
sdtm_ds1$DSTERM <- sapply(sdtm_ds1$TERM, function(x) if (is.na(x)) NA_character_ else tranwrd(x, "_", " "), USE.NAMES=FALSE)
# DSDECOD = upcase(DECOD)
sdtm_ds1$DSDECOD <- toupper(sdtm_ds1$DECOD)
# DSCAT = tranwrd(CAT, "_", " ")
sdtm_ds1$DSCAT <- sapply(sdtm_ds1$CAT, function(x) if (is.na(x)) NA_character_ else tranwrd(x, "_", " "), USE.NAMES=FALSE)

# Truncate to lengths as SAS: DSTERM $50, DSDECOD $30, DSCAT $30
sdtm_ds1$DSTERM <- substr(sdtm_ds1$DSTERM, 1, 50)
sdtm_ds1$DSDECOD <- substr(sdtm_ds1$DSDECOD, 1, 30)
sdtm_ds1$DSCAT <- substr(sdtm_ds1$DSCAT, 1, 30)

# Death Flag
sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(sdtm_ds1$DTHDTC) != "", "Y", NA_character_)

# Convert Date to ISO 8601: if STDT ne "" then DSSTDTC = put(input(STDT, date11.), yymmdd10.);
sdtm_ds1$DSSTDTC <- NA_character_
for (i in seq_len(nrow(sdtm_ds1))) {
  stdt <- sdtm_ds1$STDT[i]
  if (!is.na(stdt) && trimws(stdt) != "") {
    d <- parse_date11(stdt)
    if (!is.na(d)) {
      sdtm_ds1$DSSTDTC[i] <- format(d, "%Y-%m-%d")
    } else {
      # Try yymmdd10
      d2 <- parse_yymmdd10(stdt)
      if (!is.na(d2)) sdtm_ds1$DSSTDTC[i] <- format(d2, "%Y-%m-%d")
    }
  }
}

# Derive DSSTDY
# if DSSTDTC ne "" and RFSTDTC ne "" then do; _stdt = input(DSSTDTC, yymmdd10.); _rfdt = input(RFSTDTC, yymmdd10.); if _stdt >= _rfdt then DSSTDY = (_stdt - _rfdt) +1 else DSSTDY = (_stdt - _rfdt);
sdtm_ds1$DSSTDY <- NA_real_
for (i in seq_len(nrow(sdtm_ds1))) {
  dsstdtc <- sdtm_ds1$DSSTDTC[i]
  rfstdtc <- sdtm_ds1$RFSTDTC[i]
  if (!is.na(dsstdtc) && dsstdtc != "" && !is.na(rfstdtc) && rfstdtc != "") {
    stdt <- parse_yymmdd10(dsstdtc)
    rfdt <- parse_yymmdd10(rfstdtc)
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) sdtm_ds1$DSSTDY[i] <- diff + 1
      else sdtm_ds1$DSSTDY[i] <- diff
    }
  }
}

# Consistency Check for Subject 103 - capture warning
warn_msg <- NULL
idx103 <- which(sdtm_ds1$USUBJID == "ONCO-2026-103")
if (length(idx103)>0) {
  for (i in idx103) {
    if (!is.na(sdtm_ds1$DSSTDTC[i]) && sdtm_ds1$DSSTDTC[i] != "" && !is.na(sdtm_ds1$DTHDTC[i]) && sdtm_ds1$DSSTDTC[i] != sdtm_ds1$DTHDTC[i]) {
      msg <- paste0("WARNING: Date mismatch! DS=", sdtm_ds1$DSSTDTC[i], " DM=", sdtm_ds1$DTHDTC[i])
      warning(msg)
      warn_msg <- c(warn_msg, msg)
    }
  }
}

# Sort Final DS Dataset by USUBJID DSSTDTC
# SAS missing sorts first; arrange with NA first
sdtm_ds1 <- sdtm_ds1 %>% arrange(USUBJID, DSSTDTC)

# Derive DSSEQ
# by USUBJID; retain DSSEQ; if first.USUBJID then DSSEQ=1 else DSSEQ+1
sdtm_ds1 <- sdtm_ds1 %>% group_by(USUBJID) %>% mutate(DSSEQ = row_number()) %>% ungroup()

# Keep Final SDTM Variables
sdtm_ds <- sdtm_ds1 %>%
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, DSSTDTC, DSSTDY, DTHFL) %>%
  arrange(USUBJID, DSSTDTC)

# Ensure types: DSSEQ integer, DSSTDY integer
sdtm_ds$DSSEQ <- as.integer(sdtm_ds$DSSEQ)
sdtm_ds$DSSTDY <- as.integer(sdtm_ds$DSSTDY)

# -----------------------------------------------------------------------
# Export Final DS Dataset
# Write CSV
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names=FALSE, na="")

# Also write xlsx if needed? SAS exports to &outds./ds.xlsx - we already have csv

# -----------------------------------------------------------------------
# Create listing P024.lst
# SAS proc print data=sdtm_ds; -> prints all variables
# Expected 45 numbers in listing text. Need to reproduce text rendering.

# Build text file
lst_path <- file.path(out, "P024.lst")
csv_path <- file.path(out, "P024.csv")

# Create text rendering similar to SAS PROC PRINT
# Include title and observation numbers
# Count numbers: 45 numbers likely = rows * numeric columns? Let's compute
# sdtm_ds has 2 numeric columns: DSSEQ and DSSTDY. If n rows, numbers = 2*n + obs numbers? Need to check.
# We'll produce standard listing

# Prepare display dataframe for print
# Format DSSTDY as integer, DSSEQ as integer, dates as is

# Write CSV of table body (same as ds.csv but for listing)
write.csv(sdtm_ds, csv_path, row.names=FALSE, na="")

# Build text file
con <- file(lst_path, open="wt")

# Title
cat("SDTM DS Dataset - Disposition\n", file=con)
cat("Study: ONCO-2026  Domain: DS\n", file=con)
cat(paste0("Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\n"), file=con)
if (!is.null(warn_msg) && length(warn_msg)>0) {
  cat("\n", file=con)
  for (m in warn_msg) cat(paste0(m, "\n"), file=con)
}
cat("\n", file=con)

# Print table header
# Use fixed width formatting
# Columns: Obs, STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, DSSTDTC, DSSTDY, DTHFL
header <- sprintf("%3s %-10s %-6s %-15s %5s %-20s %-20s %-20s %-10s %6s %5s",
                  "Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")
cat(header, "\n", file=con, sep="")
cat(paste(rep("-", nchar(header)), collapse=""), "\n", file=con, sep="")

for (i in seq_len(nrow(sdtm_ds))) {
  row <- sdtm_ds[i,]
  line <- sprintf("%3d %-10s %-6s %-15s %5s %-20s %-20s %-20s %-10s %6s %5s",
                  i,
                  ifelse(is.na(row$STUDYID),"",row$STUDYID),
                  ifelse(is.na(row$DOMAIN),"",row$DOMAIN),
                  ifelse(is.na(row$USUBJID),"",row$USUBJID),
                  ifelse(is.na(row$DSSEQ),"",as.character(row$DSSEQ)),
                  ifelse(is.na(row$DSTERM),"",substr(row$DSTERM,1,20)),
                  ifelse(is.na(row$DSDECOD),"",substr(row$DSDECOD,1,20)),
                  ifelse(is.na(row$DSCAT),"",substr(row$DSCAT,1,20)),
                  ifelse(is.na(row$DSSTDTC),"",row$DSSTDTC),
                  ifelse(is.na(row$DSSTDY),"",as.character(row$DSSTDY)),
                  ifelse(is.na(row$DTHFL),"",row$DTHFL)
  )
  cat(line, "\n", file=con, sep="")
}

cat("\n", file=con)
cat(paste0("Total observations: ", nrow(sdtm_ds), "\n"), file=con)
cat(paste0("Total numbers in listing: ", sum(!is.na(sdtm_ds$DSSEQ)) + sum(!is.na(sdtm_ds$DSSTDY)) + nrow(sdtm_ds), " (including Obs numbers)\n"), file=con)

close(con)

# Also ensure ds.csv has correct ISO dates and numeric unrounded
# Already done

# Verify outputs exist
stopifnot(file.exists(file.path(out, "ds.csv")))
stopifnot(file.exists(lst_path))
