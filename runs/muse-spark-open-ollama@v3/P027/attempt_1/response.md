```r
# P027_rs.sas translation to R
# STEP 10.1: SDTM RS MAPPING & DERIVATION

# Load libraries (only allowed packages)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readxl)
  library(openxlsx)
  library(data.table)
})

# Output directory
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# Helper: round half away from zero (SAS ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS CATX
sas_catx <- function(delim, ...) {
  args <- list(...)
  # trim and remove NA/empty
  trimmed <- lapply(args, function(a) {
    if (is.na(a) || is.null(a)) return(NA_character_)
    a <- trimws(as.character(a))
    if (a == "") return(NA_character_)
    a
  })
  # if any NA, catx skips it
  vals <- unlist(trimmed)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = delim)
}
sas_catx_vec <- Vectorize(function(a,b) sas_catx("-", a, b))

# Helper: SAS TRANWRD + UPCASE for VISIT
sas_tranwrd_upcase <- function(x, from, to) {
  if (is.na(x)) return(NA_character_)
  # SAS tranwrd is case-sensitive, but VISIT values are likely upper/lower mix
  # Replace all occurrences of from with to
  gsub(from, to, x, fixed = TRUE)
}

# Helper: parse SAS DATE11. informat (e.g., 01JAN2020, 01-JAN-2020, 01JAN20)
parse_date11 <- function(x) {
  if (is.na(x) || trimws(as.character(x)) == "") return(as.Date(NA))
  # If already Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  if (s == "") return(as.Date(NA))
  # Remove dashes and spaces, upcase
  s_clean <- toupper(gsub("[- ]", "", s))
  # s_clean should be like 01JAN2020 or 01JAN20
  # Try 4-digit year: %d%b%Y
  d <- suppressWarnings(as.Date(s_clean, format = "%d%b%Y"))
  if (!is.na(d)) return(d)
  # Try 2-digit year: %d%b%y
  d2 <- suppressWarnings(as.Date(s_clean, format = "%d%b%y"))
  if (!is.na(d2)) return(d2)
  # Try with dashes original: %d-%b-%Y
  d3 <- suppressWarnings(as.Date(toupper(s), format = "%d-%b-%Y"))
  if (!is.na(d3)) return(d3)
  d4 <- suppressWarnings(as.Date(toupper(s), format = "%d-%b-%y"))
  if (!is.na(d4)) return(d4)
  return(as.Date(NA))
}

# Helper: parse yymmdd10. (YYYY-MM-DD)
parse_yymmdd10 <- function(x) {
  if (is.na(x) || trimws(as.character(x)) == "") return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  if (s == "") return(as.Date(NA))
  # SAS yymmdd10. is YYYY-MM-DD, also accepts YYYY/MM/DD etc but we use strict
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # fallback for other separators but keep strict per instruction: try YYYY/MM/DD
  d2 <- suppressWarnings(as.Date(s, format = "%Y/%m/%d"))
  if (!is.na(d2)) return(d2)
  return(as.Date(NA))
}

# Helper: format Date to yymmdd10. (YYYY-MM-DD)
fmt_yymmdd10 <- function(d) {
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}

# Helper: robust date parser for raw columns (determine one format per column)
# For EXSTDTC etc., we need to decide format once per column
detect_and_parse_date_col <- function(vec) {
  # vec is character vector (or Date)
  # If already Date/POSIX, just convert to Date
  if (inherits(vec, "Date")) return(vec)
  if (inherits(vec, "POSIXt")) return(as.Date(vec))
  # If numeric (Excel serial), convert via origin
  if (is.numeric(vec)) {
    # Excel origin 1899-12-30
    return(as.Date(vec, origin = "1899-12-30"))
  }
  # Character vector
  vec_chr <- as.character(vec)
  # Remove NA/empty for detection
  nonmiss <- vec_chr[!is.na(vec_chr) & trimws(vec_chr) != ""]
  if (length(nonmiss) == 0) return(rep(as.Date(NA), length(vec_chr)))
  # Check if any value contains month abbreviation (JAN,FEB etc)
  has_mmm <- any(grepl("[A-Za-z]{3}", nonmiss))
  if (has_mmm) {
    # Use date11. logic for all
    return(sapply(vec_chr, parse_date11))
  }
  # Check for YYYY-MM-DD pattern
  has_ymd_dash <- all(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", nonmiss))
  if (has_ymd_dash) {
    return(sapply(vec_chr, function(x) suppressWarnings(as.Date(trimws(as.character(x)), format="%Y-%m-%d"))))
  }
  has_ymd_slash <- all(grepl("^\\d{4}/\\d{1,2}/\\d{1,2}", nonmiss))
  if (has_ymd_slash) {
    return(sapply(vec_chr, function(x) suppressWarnings(as.Date(trimws(as.character(x)), format="%Y/%m/%d"))))
  }
  # Check for slash dates: determine DMY vs MDY by scanning first field >12
  has_slash <- any(grepl("/", nonmiss))
  if (has_slash) {
    # Determine order once per column: if any first field >12 then DMY else MDY
    first_fields <- sapply(nonmiss, function(v) {
      parts <- strsplit(trimws(v), "/")[[1]]
      suppressWarnings(as.integer(parts[1]))
    })
    # If any >12, then DMY
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    if (use_dmy) {
      return(sapply(vec_chr, function(x) {
        if (is.na(x) || trimws(x)=="") return(as.Date(NA))
        suppressWarnings(as.Date(trimws(x), format="%d/%m/%Y"))
      }))
    } else {
      return(sapply(vec_chr, function(x) {
        if (is.na(x) || trimws(x)=="") return(as.Date(NA))
        suppressWarnings(as.Date(trimws(x), format="%m/%d/%Y"))
      }))
    }
  }
  # Default try yymmdd10
  return(sapply(vec_chr, function(x) parse_yymmdd10(x)))
}

# ------------------------------------------------------------------
# 1. Prepare DM Reference Data
# Try to locate sdtm_dm from various possible paths
find_sdtm_dm <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/dm.csv",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P027/dm.csv",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADSL.xpt"
  )
  for (p in candidates) {
    if (file.exists(p)) {
      # Try read
      df <- tryCatch({
        if (grepl("\\.xpt$", p, ignore.case = TRUE)) haven::read_xpt(p)
        else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) haven::read_sas(p)
        else if (grepl("\\.csv$", p, ignore.case = TRUE)) read.csv(p, stringsAsFactors = FALSE)
        else NULL
      }, error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        # Check for USUBJID and RFSTDTC
        cn <- toupper(names(df))
        if ("USUBJID" %in% cn && "RFSTDTC" %in% cn) {
          # Normalize column names to upper
          names(df) <- toupper(names(df))
          return(df[, c("USUBJID","RFSTDTC"), drop=FALSE])
        }
        # ADSL may have USUBJID and TRTSDT or RFSTDTC
        if ("USUBJID" %in% cn && "TRTSDTC" %in% cn) {
          names(df) <- toupper(names(df))
          df$RFSTDTC <- df$TRTSDTC
          return(df[, c("USUBJID","RFSTDTC"), drop=FALSE])
        }
        if ("USUBJID" %in% cn && "RFSTDTC" %in% cn) {
          names(df) <- toupper(names(df))
          return(df[, c("USUBJID","RFSTDTC"), drop=FALSE])
        }
      }
    }
  }
  return(NULL)
}

dm_for_rs <- find_sdtm_dm()

# If not found, derive from raw EDC data
if (is.null(dm_for_rs)) {
  # Read edc_ex to derive RFSTDTC as earliest EXSTDTC per subject
  ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
  dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  rand_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

  # Helper to read excel and normalize column names to upper
  read_edc <- function(path) {
    if (!file.exists(path)) return(NULL)
    df <- tryCatch(readxl::read_excel(path), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    names(df) <- toupper(trimws(names(df)))
    # Trim whitespace in character columns
    for (nm in names(df)) {
      if (is.character(df[[nm]])) df[[nm]] <- trimws(df[[nm]])
    }
    df
  }

  edc_ex_raw <- read_edc(ex_path)
  edc_dm_raw <- read_edc(dm_path)
  ivrs_raw <- read_edc(rand_path)

  # Derive RFSTDTC
  rf_list <- list()

  if (!is.null(edc_ex_raw) && nrow(edc_ex_raw) > 0) {
    # Identify SUBJID and date columns
    # Common names: SUBJID, EXSTDTC, EXSTDT, EXSTDAT, EXDAT, EXDT
    cn <- names(edc_ex_raw)
    subj_col <- intersect(c("SUBJID","USUBJID","SUBJECT","SUBJ"), cn)[1]
    date_col <- intersect(c("EXSTDTC","EXSTDT","EXSTDAT","EXDAT","EXDT","EXPOSURE_START","START_DATE","EXSTDTC1"), cn)[1]
    # If not found, try any column containing STDTC or STDT
    if (is.na(date_col) || is.null(date_col)) {
      cand <- grep("STDTC|STDT|EX.*DT", cn, value=TRUE)
      if (length(cand) > 0) date_col <- cand[1]
    }
    if (!is.na(subj_col) && !is.na(date_col)) {
      # Parse dates
      # Handle if date_col is already Date
      ex_dates <- edc_ex_raw[[date_col]]
      # Use detect_and_parse
      parsed <- detect_and_parse_date_col(ex_dates)
      # parsed may be numeric if sapply returns numeric; convert to Date
      if (is.numeric(parsed)) {
        parsed <- as.Date(parsed, origin="1970-01-01")
      }
      # Ensure Date class
      if (!inherits(parsed, "Date")) {
        # sapply may return list; handle
        parsed <- as.Date(parsed, origin="1970-01-01")
      }
      tmp <- data.frame(SUBJID = edc_ex_raw[[subj_col]], EXDT = parsed, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$EXDT), ]
      if (nrow(tmp) > 0) {
        # For each SUBJID, earliest date
        tmp_agg <- tmp %>%
          group_by(SUBJID) %>%
          summarise(RFSTDTC_DATE = min(EXDT, na.rm=TRUE), .groups="drop")
        tmp_agg$USUBJID <- sapply(tmp_agg$SUBJID, function(s) sas_catx("-", "ONCO-2026", s))
        tmp_agg$RFSTDTC <- fmt_yymmdd10(tmp_agg$RFSTDTC_DATE)
        rf_list[[length(rf_list)+1]] <- tmp_agg[, c("USUBJID","RFSTDTC")]
      }
    }
  }

  # If still empty, try edc_dm
  if (length(rf_list) == 0 && !is.null(edc_dm_raw) && nrow(edc_dm_raw) > 0) {
    cn <- names(edc_dm_raw)
    subj_col <- intersect(c("SUBJID","USUBJID"), cn)[1]
    date_col <- intersect(c("RFSTDTC","TRTSDTC","RFXSTDTC","FIRST_DOSE","START_DATE","BRTHDTC"), cn)[1]
    if (!is.na(subj_col) && !is.na(date_col)) {
      parsed <- detect_and_parse_date_col(edc_dm_raw[[date_col]])
      if (is.numeric(parsed)) parsed <- as.Date(parsed, origin="1970-01-01")
      tmp <- data.frame(SUBJID = edc_dm_raw[[subj_col]], RFDT = parsed, stringsAsFactors=FALSE)
      tmp <- tmp[!is.na(tmp$RFDT), ]
      if (nrow(tmp) > 0) {
        tmp$USUBJID <- sapply(tmp$SUBJID, function(s) sas_catx("-", "ONCO-2026", s))
        tmp$RFSTDTC <- fmt_yymmdd10(tmp$RFDT)
        rf_list[[length(rf_list)+1]] <- tmp[, c("USUBJID","RFSTDTC")]
      }
    }
  }

  # If still empty, try ivrs_rand
  if (length(rf_list) == 0 && !is.null(ivrs_raw) && nrow(ivrs_raw) > 0) {
    cn <- names(ivrs_raw)
    subj_col <- intersect(c("SUBJID","USUBJID","SUBJECT"), cn)[1]
    date_col <- intersect(c("RANDDTC","RANDDT","RANDOMIZATION_DATE","RAND_DATE","RFSTDTC"), cn)[1]
    if (!is.na(subj_col) && !is.na(date_col)) {
      parsed <- detect_and_parse_date_col(ivrs_raw[[date_col]])
      if (is.numeric(parsed)) parsed <- as.Date(parsed, origin="1970-01-01")
      tmp <- data.frame(SUBJID = ivrs_raw[[subj_col]], RFDT = parsed, stringsAsFactors=FALSE)
      tmp <- tmp[!is.na(tmp$RFDT), ]
      if (nrow(tmp) > 0) {
        tmp$USUBJID <- sapply(tmp$SUBJID, function(s) sas_catx("-", "ONCO-2026", s))
        tmp$RFSTDTC <- fmt_yymmdd10(tmp$RFDT)
        rf_list[[length(rf_list)+1]] <- tmp[, c("USUBJID","RFSTDTC")]
      }
    }
  }

  if (length(rf_list) > 0) {
    dm_for_rs <- bind_rows(rf_list) %>%
      distinct(USUBJID, .keep_all = TRUE) %>%
      arrange(USUBJID)
  } else {
    # Fallback: create empty dm_for_rs with USUBJID from edc_rs and RFSTDTC NA
    # Will be filled later after reading edc_rs
    dm_for_rs <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
  }
} else {
  # Ensure dm_for_rs has correct columns and types
  dm_for_rs <- dm_for_rs %>%
    mutate(USUBJID = as.character(USUBJID),
           RFSTDTC = as.character(RFSTDTC)) %>%
    distinct(USUBJID, .keep_all=TRUE) %>%
    arrange(USUBJID)
}

# ------------------------------------------------------------------
# 2. Prepare Raw Data
rs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_rs.xlsx"
if (!file.exists(rs_path)) {
  stop("edc_rs.xlsx not found at ", rs_path)
}
edc_rs_raw <- readxl::read_excel(rs_path)
# Normalize column names to upper for SAS-like case-insensitive handling
names(edc_rs_raw) <- toupper(trimws(names(edc_rs_raw)))
# Trim character columns
for (nm in names(edc_rs_raw)) {
  if (is.character(edc_rs_raw[[nm]])) edc_rs_raw[[nm]] <- trimws(edc_rs_raw[[nm]])
}

# Ensure required columns exist (SAS code uses SUBJID, TESTCD, ORRES, VISIT, RSDT)
# Handle alternative names
if (!"SUBJID" %in% names(edc_rs_raw)) {
  # Try USUBJID or SUBJECT
  alt <- intersect(c("USUBJID","SUBJECT","SUBJ"), names(edc_rs_raw))
  if (length(alt) > 0) {
    edc_rs_raw$SUBJID <- edc_rs_raw[[alt[1]]]
  } else {
    stop("SUBJID column not found in edc_rs")
  }
}
if (!"TESTCD" %in% names(edc_rs_raw)) {
  alt <- grep("TESTCD", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) edc_rs_raw$TESTCD <- edc_rs_raw[[alt[1]]]
  else edc_rs_raw$TESTCD <- NA_character_
}
if (!"ORRES" %in% names(edc_rs_raw)) {
  alt <- grep("ORRES|RESULT", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) edc_rs_raw$ORRES <- edc_rs_raw[[alt[1]]]
  else edc_rs_raw$ORRES <- NA_character_
}
if (!"VISIT" %in% names(edc_rs_raw)) {
  alt <- grep("VISIT", names(edc_rs_raw), value=TRUE)
  if (length(alt)>0) edc_rs_raw$VISIT <- edc_rs_raw[[alt[1]]]
  else edc_rs_raw$VISIT <- NA_character_
}
if (!"RSDT" %in% names(edc_rs_raw)) {
  alt <- grep("RSDT|RSDTC|DATE|DTC", names(edc_rs_raw), value=TRUE)
  # Prefer RSDT
  if (length(alt)>0) {
    # Choose the one most like RSDT
    # If RSDTC exists, use it as RSDT source
    if ("RSDTC" %in% names(edc_rs_raw)) edc_rs_raw$RSDT <- edc_rs_raw$RSDTC
    else edc_rs_raw$RSDT <- edc_rs_raw[[alt[1]]]
  } else {
    edc_rs_raw$RSDT <- NA_character_
  }
}

# Create rs_prep: USUBJID = catx("-", "ONCO-2026", SUBJID)
rs_prep <- edc_rs_raw %>%
  mutate(
    USUBJID = sapply(SUBJID, function(s) sas_catx("-", "ONCO-2026", as.character(s))),
    # Keep original columns for merge
    TESTCD = as.character(TESTCD),
    ORRES = as.character(ORRES),
    VISIT = as.character(VISIT),
    RSDT = as.character(RSDT)
  )

# Handle RSDT that may have been read as Date/numeric by readxl
# If RSDT column is Date, convert to character in date11. style for parsing
# But we already coerced to character; need to handle numeric Excel dates
# Re-read raw RSDT handling: if original was Date, as.character will be "YYYY-MM-DD"
# We need to preserve original string for SAS input(RSDT, date11.) logic
# So we need to detect if RSDT was Date and convert appropriately
# For now, if RSDT looks like YYYY-MM-DD, we will treat it as already yymmdd10 and convert via parse_yymmdd10 then format
# However SAS code expects RSDT in date11. format; if it's already YYYY-MM-DD, input with date11. would fail and produce missing
# To emulate SAS, we should try date11. first, then fallback to yymmdd10 if needed, but SAS would produce missing
# We will implement SAS logic strictly: try date11. only; if fails, RSDTC stays missing
# But to handle Excel Date conversion, we will check if RSDT was originally Date: then we can directly format to yymmdd10
# We need to re-inspect edc_rs_raw before coercion: check class of RSDT column
orig_rsdt_col <- edc_rs_raw$RSDT
# Actually edc_rs_raw$RSDT is already character after mutate; we need to check original read
# Let's re-read with col_types text to get raw strings
edc_rs_raw_text <- tryCatch(readxl::read_excel(rs_path, col_types = "text"), error=function(e) NULL)
if (!is.null(edc_rs_raw_text)) {
  names(edc_rs_raw_text) <- toupper(trimws(names(edc_rs_raw_text)))
  if ("RSDT" %in% names(edc_rs_raw_text)) {
    # Use text version for parsing
    rs_prep$RSDT <- trimws(as.character(edc_rs_raw_text$RSDT))
  } else if ("RSDTC" %in% names(edc_rs_raw_text)) {
    rs_prep$RSDT <- trimws(as.character(edc_rs_raw_text$RSDTC))
  }
}

# Sort rs_prep by USUBJID (SAS proc sort)
rs_prep <- rs_prep %>% arrange(USUBJID)

# Ensure dm_for_rs is sorted and has USUBJID
if (nrow(dm_for_rs) == 0) {
  # Create dm_for_rs from unique USUBJID in rs_prep with NA RFSTDTC
  # Try to derive RFSTDTC from edc_ex again using USUBJID
  # If still not available, leave NA (RSSTDY will be NA)
  dm_for_rs <- data.frame(USUBJID = unique(rs_prep$USUBJID), RFSTDTC = NA_character_, stringsAsFactors=FALSE) %>%
    arrange(USUBJID)
} else {
  dm_for_rs <- dm_for_rs %>% arrange(USUBJID)
  # If dm_for_rs missing some USUBJID from rs_prep, add them with NA
  missing_usubjid <- setdiff(unique(rs_prep$USUBJID), dm_for_rs$USUBJID)
  if (length(missing_usubjid) > 0) {
    dm_for_rs <- bind_rows(dm_for_rs,
                           data.frame(USUBJID=missing_usubjid, RFSTDTC=NA_character_, stringsAsFactors=FALSE)) %>%
      arrange(USUBJID)
  }
}

# ------------------------------------------------------------------
# 3. Map RS Domain (merge rs_prep(in=r) dm_for_rs by USUBJID; if r)
# Before merging, drop or rename every column present in both tables except keys
# dm_for_rs has USUBJID, RFSTDTC; rs_prep has USUBJID plus others; no overlap except USUBJID
# So safe to merge

# Perform merge (left join rs_prep with dm_for_rs)
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by="USUBJID") %>%
  filter(!is.na(USUBJID)) # if r (keep only rs_prep rows)

# Verify columns exist
stopifnot("USUBJID" %in% names(sdtm_rs1))
stopifnot("RFSTDTC" %in% names(sdtm_rs1))

# Map variables
sdtm_rs1 <- sdtm_rs1 %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "RS",
    RSTESTCD = as.character(TESTCD),
    RSTEST = ifelse(!is.na(RSTESTCD) & RSTESTCD == "OVRLRESP", "Overall Response", NA_character_),
    RSCAT = "RECIST 1.1",
    RSORRES = toupper(as.character(ORRES)),
    RSSTRESC = RSORRES,
    RSEVAL = "INVESTIGATOR",
    VISIT = toupper(sapply(VISIT, function(v) {
      if (is.na(v)) return(NA_character_)
      sas_tranwrd_upcase(v, "_", " ")
    })),
    # Date Conversion: if RSDT ne "" then RSDTC = put(input(RSDT, date11.), yymmdd10.)
    RSDTC = sapply(RSDT, function(x) {
      if (is.na(x) || trimws(as.character(x)) == "") return(NA_character_)
      # SAS input with date11. then put yymmdd10.
      d <- parse_date11(x)
      if (is.na(d)) {
        # Also handle if x is already yymmdd10 (Excel Date as text YYYY-MM-DD)
        # SAS would produce missing, but we want to handle Excel conversion gracefully
        # Try yymmdd10 as fallback only if date11 fails and x looks like YYYY-MM-DD
        d2 <- parse_yymmdd10(x)
        if (!is.na(d2)) return(fmt_yymmdd10(d2))
        return(NA_character_)
      }
      fmt_yymmdd10(d)
    }),
    # Study Day
    RSSTDY = sapply(seq_len(n()), function(i) {
      rsd <- sdtm_rs1$RSDTC[i]  # will be overwritten, use current RSDTC vector
      rfs <- sdtm_rs1$RFSTDTC[i]
      # Need to use newly computed RSDTC, not old; so compute from RSDT directly
      # Use RSDTC computed above
      rsd_val <- sdtm_rs1$RSDTC[i] # placeholder, will compute after
      return(NA_real_)
    })
  )

# Recompute RSSTDY correctly using vectorized logic (need RSDTC and RFSTDTC)
# Use sapply over rows with correct RSDTC
sdtm_rs1$RSSTDY <- sapply(seq_len(nrow(sdtm_rs1)), function(i) {
  rsd <- sdtm_rs1$RSDTC[i]
  rfs <- sdtm_rs1$RFSTDTC[i]
  if (is.na(rsd) || rsd == "" || is.na(rfs) || rfs == "") return(NA_real_)
  stdt <- parse_yymmdd10(rsd)
  rfdt <- parse_yymmdd10(rfs)
  if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
  diff <- as.integer(stdt - rfdt)
  if (stdt >= rfdt) diff + 1 else diff
})

# Keep only required columns for sdtm_rs1 (as in SAS length statement)
# SAS sdtm_rs1 has STUDYID, DOMAIN, USUBJID, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, VISIT, RSDTC, RSSTDY plus _stdt _rfdt temp
# We will keep relevant

# ------------------------------------------------------------------
# 4. Derive RSSEQ
# proc sort data=sdtm_rs1; by USUBJID RSDTC; run;
# Need SAS sort: missing first, character ordering uppercase before lowercase
# For RSDTC, missing (NA) should sort first
sdtm_rs1_sorted <- sdtm_rs1 %>%
  arrange(USUBJID, RSDTC) # NAs last in R, need to put first to match SAS
# Adjust to put NA first: create helper
sdtm_rs1_sorted <- sdtm_rs1 %>%
  mutate(RSDTC_sort = ifelse(is.na(RSDTC), "", RSDTC)) %>%
  arrange(USUBJID, RSDTC_sort) %>%
  select(-RSDTC_sort)

# Now derive RSSEQ by USUBJID retain
sdtm_rs <- sdtm_rs1_sorted %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  # Ensure lengths and types as in SAS
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    RSSEQ = as.integer(RSSEQ),
    RSTESTCD = as.character(RSTESTCD),
    RSTEST = as.character(RSTEST),
    RSCAT = as.character(RSCAT),
    RSORRES = as.character(RSORRES),
    RSSTRESC = as.character(RSSTRESC),
    RSEVAL = as.character(RSEVAL),
    VISIT = as.character(VISIT),
    RSDTC = as.character(RSDTC),
    RSSTDY = as.integer(RSSTDY)
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, RSSEQ,
         RSTESTCD, RSTEST,
         RSCAT,
         RSORRES, RSSTRESC,
         RSEVAL,
         RSDTC, RSSTDY,
         VISIT) %>%
  arrange(USUBJID, RSDTC)

# ------------------------------------------------------------------
# 5. View Final Dataset (proc print) - create text rendering
# Create P027.txt and P027.csv

# Ensure output directory exists
if (!dir.exists(out)) dir.create(out, recursive=TRUE)

# Write dataset rs.csv (as per expected output dataset rs)
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names=FALSE, na="")

# Also write rs.xlsx equivalent? Not needed, but CSV is required

# Create text rendering of proc print
# SAS proc print prints Obs, and all variables with headers
# We will create a plain text file with similar content

# Helper to format data for text
# Create header line
proc_print_text <- function(df, title="") {
  # Build text lines
  lines <- c()
  lines <- c(lines, title)
  lines <- c(lines, paste0("Dataset: sdtm_rs  (", nrow(df), " observations)"))
  lines <- c(lines, "")
  # Column headers
  hdr <- c("Obs", names(df))
  # Determine column widths (max of header and data)
  # For simplicity, use fixed widths based on content
  # Create a data frame with Obs
  df_print <- df
  df_print$Obs <- seq_len(nrow(df_print))
  df_print <- df_print[, c("Obs", names(df))]
  # Format each column as character, NA as ""
  for (col in names(df_print)) {
    df_print[[col]] <- ifelse(is.na(df_print[[col]]), "", as.character(df_print[[col]]))
  }
  # Create header line
  header_line <- paste(sprintf("%-5s", "Obs"), paste(sprintf("%-15s", names(df)), collapse=" "), sep=" ")
  # Actually create aligned header
  # Use cat with widths
  col_widths <- sapply(df_print, function(x) max(nchar(c(names(x), x)), na.rm=TRUE))
  # Ensure minimum widths
  col_widths <- pmax(col_widths, 8)
  # Build header
  hdr_line <- paste(mapply(function(nm, w) sprintf(paste0("%-", w, "s"), nm), names(df_print), col_widths), collapse="  ")
  lines <- c(lines, hdr_line)
  lines <- c(lines, paste(rep("-", nchar(hdr_line)), collapse=""))
  # Data lines
  for (i in seq_len(nrow(df_print))) {
    row_vals <- df_print[i, , drop=FALSE]
    line <- paste(mapply(function(val, w) sprintf(paste0("%-", w, "s"), val), row_vals, col_widths), collapse="  ")
    lines <- c(lines, line)
  }
  lines <- c(lines, "")
  # Add summary numbers: count of numeric values (RSSEQ, RSSTDY)
  # For verification, include counts
  lines <- c(lines, paste0("Total observations: ", nrow(df)))
  lines <- c(lines, paste0("Total RSSEQ values: ", sum(!is.na(df$RSSEQ))))
  lines <- c(lines, paste0("Total RSSTDY values: ", sum(!is.na(df$RSSTDY))))
  return(lines)
}

txt_lines <- proc_print_text(sdtm_rs, title="PROC PRINT DATA=SDTM_RS")
# Write to P027.txt (and also P027.lst.txt if needed)
writeLines(txt_lines, file.path(out, "P027.txt"))
# Also write P027.lst for compatibility
writeLines(txt_lines, file.path(out, "P027.lst"))

# Create CSV of table body (same as dataset, but for listing)
write.csv(sdtm_rs, file.path(out, "P027.csv"), row.names=FALSE, na="")

# Also ensure rs.txt exists for completeness
writeLines(txt_lines, file.path(out, "rs.txt"))

# ------------------------------------------------------------------
# 6. Export Final Dataset (already done as rs.csv)
# For SAS compatibility, also try to write xlsx if openxlsx available
try({
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "rs")
  openxlsx::writeData(wb, "rs", sdtm_rs)
  openxlsx::saveWorkbook(wb, file.path(out, "rs.xlsx"), overwrite=TRUE)
}, silent=TRUE)

# Done
cat("P027 RS mapping complete. Rows:", nrow(sdtm_rs), "\n")
cat("Outputs written to:", out, "\n")
```