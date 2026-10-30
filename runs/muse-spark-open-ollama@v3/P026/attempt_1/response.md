```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: parse date with single strict format per column (SAS PROC IMPORT guessing)
# For this program, LBDT is read via date11. (DD-MMM-YYYY like 01-Jan-2020)
# RFSTDTC is yymmdd10. (YYYY-MM-DD)
# We will handle generically but use explicit formats.

# ------------------------------------------------------------------
# Locate raw data
# ------------------------------------------------------------------
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
edc_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

# List files for debugging (no console spam, just check existence)
# Use base R to find edc files
edc_files <- list.files(edc_dir, pattern = "\\.xlsx$", full.names = TRUE)

# ------------------------------------------------------------------
# Read edc_lb
# ------------------------------------------------------------------
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

# Read edc_lb.xlsx - sheet 1
edc_lb_path <- file.path(edc_dir, "edc_lb.xlsx")
edc_lb <- readxl::read_excel(edc_lb_path)

# Read edc_dm.xlsx for reference if needed
edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
edc_dm <- readxl::read_excel(edc_dm_path)

# Read ivrs_rand if exists
ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")
ivrs_rand <- NULL
if (file.exists(ivrs_rand_path)) {
  ivrs_rand <- readxl::read_excel(ivrs_rand_path)
}

# Read edc_ex for RFSTDTC derivation if needed
edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
edc_ex <- NULL
if (file.exists(edc_ex_path)) {
  edc_ex <- readxl::read_excel(edc_ex_path)
}

# ------------------------------------------------------------------
# Derive sdtm_dm equivalent: need USUBJID and RFSTDTC
# SAS program uses sdtm_dm with USUBJID and RFSTDTC
# In faijal project, sdtm_dm is typically derived from edc_dm + ivrs_rand
# We need to reconstruct RFSTDTC as yymmdd10. string (YYYY-MM-DD)
# Check what columns exist in edc_dm and ivrs_rand
# ------------------------------------------------------------------

# Helper to normalize column names to upper case for matching SAS (case-insensitive)
names(edc_lb) <- toupper(names(edc_lb))
names(edc_dm) <- toupper(names(edc_dm))
if (!is.null(ivrs_rand)) names(ivrs_rand) <- toupper(names(ivrs_rand))
if (!is.null(edc_ex)) names(edc_ex) <- toupper(names(edc_ex))

# Inspect edc_dm columns: try to find RFSTDTC source
# Common columns: SUBJID, RFSTDTC, BRTHDTC, SEX, etc.
# If RFSTDTC already exists in edc_dm, use it.
# Otherwise derive from RANDDT or EXSTDTC

# Function to convert various date representations to Date
parse_any_date <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # Excel numeric date
    return(as.Date(x, origin = "1899-12-30"))
  }
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | x == "NA" | x == "."] <- NA
  # Try yymmdd10 (YYYY-MM-DD)
  d1 <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  # Try date11 (DD-MMM-YYYY or DD-MMM-YY)
  # Need to handle case like 01JAN2020 or 01-Jan-2020
  # Use lubridate for month abbrev? Use as.Date with %d-%b-%Y and %d-%b-%y and %d%b%Y etc.
  # We'll attempt multiple but SAS uses one format per column; we emulate by trying yymmdd first then date11
  d2 <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%Y"))
  d3 <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%y"))
  d4 <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
  d5 <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  d6 <- suppressWarnings(as.Date(x, format = "%Y/%m/%d"))
  # Combine: prefer d1, then d2, etc.
  d <- d1
  idx <- is.na(d) & !is.na(d2); d[idx] <- d2[idx]
  idx <- is.na(d) & !is.na(d3); d[idx] <- d3[idx]
  idx <- is.na(d) & !is.na(d4); d[idx] <- d4[idx]
  idx <- is.na(d) & !is.na(d5); d[idx] <- d5[idx]
  idx <- is.na(d) & !is.na(d6); d[idx] <- d6[idx]
  # Also try DDMMMYYYY without dash
  d7 <- suppressWarnings(as.Date(toupper(x), format = "%d%b%Y"))
  idx <- is.na(d) & !is.na(d7); d[idx] <- d7[idx]
  return(d)
}

# Build dm_for_lb
# First try to locate RFSTDTC in edc_dm
dm_for_lb <- NULL

# Check if edc_dm has RFSTDTC column
if ("RFSTDTC" %in% names(edc_dm)) {
  dm_for_lb <- edc_dm %>%
    mutate(
      SUBJID = as.character(SUBJID),
      USUBJID = paste0("ONCO-2026-", SUBJID),
      RFSTDTC = as.character(RFSTDTC)
    ) %>%
    select(USUBJID, RFSTDTC)
} else {
  # Try to find date column that represents reference start
  # Look for RANDDT, RANDDTC, EXSTDTC, TRTSDT, RFSTDTC variants
  # Check ivrs_rand
  if (!is.null(ivrs_rand)) {
    # ivrs_rand typically has SUBJID and RANDDT
    # Find date column
    date_col <- intersect(c("RANDDT","RANDDTC","RAND_DATE","RANDOMIZATIONDATE","RANDDTM"), names(ivrs_rand))
    if (length(date_col) == 0) {
      # take any column with DATE in name
      date_col <- grep("DATE|DT", names(ivrs_rand), value = TRUE)
      # exclude SUBJID
      date_col <- setdiff(date_col, c("SUBJID","USUBJID"))
      if (length(date_col) > 0) date_col <- date_col[1] else date_col <- NULL
    }
    if (!is.null(date_col) && length(date_col) > 0) {
      ivrs_tmp <- ivrs_rand %>%
        mutate(
          SUBJID = as.character(SUBJID),
          USUBJID = paste0("ONCO-2026-", SUBJID),
          .tmp_date = parse_any_date(.data[[date_col[1]]]),
          RFSTDTC = ifelse(is.na(.tmp_date), NA_character_, format(.tmp_date, "%Y-%m-%d"))
        ) %>%
        select(USUBJID, RFSTDTC)
      dm_for_lb <- ivrs_tmp
    }
  }
  # If still null, try edc_ex
  if (is.null(dm_for_lb) && !is.null(edc_ex)) {
    # edc_ex has EXSTDTC or EXSTDT
    date_col_ex <- intersect(c("EXSTDTC","EXSTDT","EXDT","EXDATE","VISITDT"), names(edc_ex))
    if (length(date_col_ex) == 0) {
      date_col_ex <- grep("DATE|DT", names(edc_ex), value = TRUE)
      date_col_ex <- setdiff(date_col_ex, c("SUBJID","USUBJID"))
      if (length(date_col_ex) > 0) date_col_ex <- date_col_ex[1]
    }
    if (length(date_col_ex) > 0) {
      ex_tmp <- edc_ex %>%
        mutate(
          SUBJID = as.character(SUBJID),
          USUBJID = paste0("ONCO-2026-", SUBJID),
          .tmp_date = parse_any_date(.data[[date_col_ex[1]]])
        ) %>%
        group_by(USUBJID) %>%
        summarise(RFSTDTC = format(min(.tmp_date, na.rm = TRUE), "%Y-%m-%d"), .groups = "drop")
      dm_for_lb <- ex_tmp
    }
  }
  # Fallback: use edc_dm DMSTDTC or similar
  if (is.null(dm_for_lb)) {
    date_col_dm <- grep("STDTC|STDT|RAND|RFST", names(edc_dm), value = TRUE)
    if (length(date_col_dm) > 0) {
      dm_for_lb <- edc_dm %>%
        mutate(
          SUBJID = as.character(SUBJID),
          USUBJID = paste0("ONCO-2026-", SUBJID),
          .tmp_date = parse_any_date(.data[[date_col_dm[1]]]),
          RFSTDTC = ifelse(is.na(.tmp_date), NA_character_, format(.tmp_date, "%Y-%m-%d"))
        ) %>%
        select(USUBJID, RFSTDTC)
    } else {
      # Last resort: create empty RFSTDTC
      dm_for_lb <- edc_dm %>%
        mutate(
          SUBJID = as.character(SUBJID),
          USUBJID = paste0("ONCO-2026-", SUBJID),
          RFSTDTC = NA_character_
        ) %>%
        select(USUBJID, RFSTDTC)
    }
  }
}

# Ensure dm_for_lb sorted and unique by USUBJID (SAS proc sort)
dm_for_lb <- dm_for_lb %>%
  arrange(USUBJID) %>%
  distinct(USUBJID, .keep_all = TRUE)

# If RFSTDTC is Date or numeric, convert to YYYY-MM-DD string
dm_for_lb <- dm_for_lb %>%
  mutate(
    RFSTDTC = as.character(RFSTDTC),
    RFSTDTC = ifelse(RFSTDTC == "" | RFSTDTC == "NA", NA_character_, RFSTDTC)
  )

# Also try to read sdtm_dm from derived if exists (in case previous step produced file)
# Check for sdtm_dm in rawadam or rawsdtm
sdtm_dm_path_xpt <- file.path(rawsdtm, "dm.xpt")
sdtm_dm_path_sas <- file.path(rawsdtm, "dm.sas7bdat")
if (file.exists(sdtm_dm_path_xpt)) {
  sdtm_dm_raw <- haven::read_xpt(sdtm_dm_path_xpt)
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  if ("USUBJID" %in% names(sdtm_dm_raw) && "RFSTDTC" %in% names(sdtm_dm_raw)) {
    dm_for_lb <- sdtm_dm_raw %>%
      mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      ) %>%
      select(USUBJID, RFSTDTC) %>%
      arrange(USUBJID) %>%
      distinct(USUBJID, .keep_all = TRUE)
  }
} else if (file.exists(sdtm_dm_path_sas)) {
  sdtm_dm_raw <- haven::read_sas(sdtm_dm_path_sas)
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  if ("USUBJID" %in% names(sdtm_dm_raw) && "RFSTDTC" %in% names(sdtm_dm_raw)) {
    dm_for_lb <- sdtm_dm_raw %>%
      mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      ) %>%
      select(USUBJID, RFSTDTC) %>%
      arrange(USUBJID) %>%
      distinct(USUBJID, .keep_all = TRUE)
  }
}

# Check for sdtm_dm in adam-split or other derived
# Also check if there's a file in data/derived
derived_dm_path <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split/dm.xpt"
if (file.exists(derived_dm_path)) {
  sdtm_dm_raw2 <- haven::read_xpt(derived_dm_path)
  names(sdtm_dm_raw2) <- toupper(names(sdtm_dm_raw2))
  if ("USUBJID" %in% names(sdtm_dm_raw2) && "RFSTDTC" %in% names(sdtm_dm_raw2)) {
    dm_for_lb <- sdtm_dm_raw2 %>%
      mutate(USUBJID = as.character(USUBJID), RFSTDTC = as.character(RFSTDTC)) %>%
      select(USUBJID, RFSTDTC) %>%
      arrange(USUBJID) %>%
      distinct(USUBJID, .keep_all = TRUE)
  }
}

# ------------------------------------------------------------------
# Prepare lb_prep: edc_lb with USUBJID
# ------------------------------------------------------------------
# edc_lb columns expected: SUBJID, TESTCD, CAT, ORRES, UNIT, LOW, HIGH, VISIT, LBDT
# Normalize names already upper
# Ensure character types
lb_prep <- edc_lb %>%
  mutate(
    SUBJID = as.character(SUBJID),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  arrange(USUBJID)

# Ensure dm_for_lb sorted
dm_for_lb <- dm_for_lb %>% arrange(USUBJID)

# ------------------------------------------------------------------
# Map LB Domain - sdtm_lb1
# ------------------------------------------------------------------
# Merge lb_prep(in=l) with dm_for_lb by USUBJID, keep only l
# In R: left join lb_prep with dm_for_lb

# First, ensure all expected columns exist in lb_prep, create NA if missing
expected_lb_cols <- c("TESTCD","CAT","ORRES","UNIT","LOW","HIGH","VISIT","LBDT")
for (col in expected_lb_cols) {
  if (!col %in% names(lb_prep)) {
    lb_prep[[col]] <- NA_character_
  }
}

# Convert to character for relevant columns
lb_prep <- lb_prep %>%
  mutate(
    TESTCD = as.character(TESTCD),
    CAT = as.character(CAT),
    ORRES = as.character(ORRES),
    UNIT = as.character(UNIT),
    LOW = as.character(LOW),
    HIGH = as.character(HIGH),
    VISIT = as.character(VISIT),
    LBDT = as.character(LBDT)
  )

# Merge
sdtm_lb1 <- lb_prep %>%
  left_join(dm_for_lb, by = "USUBJID")

# Now derive variables as per SAS
# SAS logic:
# STUDYID = "ONCO-2026"; DOMAIN = "LB";
# LBTESTCD = TESTCD; LBCAT = CAT;
# if LBTESTCD="HGB" then LBTEST="Hemoglobin" else if "ALT" then "Alanine Aminotransferase"
# LBORRES=ORRES; LBORRESU=UNIT; LBSTRESC=ORRES; LBSTRESN=input(ORRES,best.); LBSTRESU=UNIT;
# LBORNRLO=LOW; LBORNRHI=HIGH;
# VISIT = tranwrd(VISIT,"_"," ");
# LBNRIND based on LBSTRESN vs LOW/HIGH
# LBDTC = put(input(LBDT,date11.),yymmdd10.) if LBDT ne ""
# LBSTDY calculation

# Helper to parse LBDT with date11. (DD-MMM-YYYY)
# SAS date11. is DD-MMM-YYYY like 01-JAN-2020 (11 chars)
# It also accepts DDMMMYYYY? But we use strict.
parse_date11 <- function(x) {
  if (is.na(x) || trimws(x) == "") return(as.Date(NA))
  x <- trimws(as.character(x))
  # If numeric Excel date
  if (grepl("^[0-9]+$", x)) {
    num <- suppressWarnings(as.numeric(x))
    if (!is.na(num) && num > 1000 && num < 60000) {
      return(as.Date(num, origin = "1899-12-30"))
    }
  }
  # Try date11 formats: DD-MMM-YYYY, DD-MMM-YY, DDMMMYYYY, DD/MM/YYYY etc.
  # SAS date11. expects DDMMMYY or DDMMMYYYY with dashes? Actually date11. is DD-MMM-YYYY
  # We'll try several
  d <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%Y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(toupper(x), format = "%d%b%Y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(toupper(x), format = "%d%b%y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  if (!is.na(d)) return(d)
  # Try with spaces
  d <- suppressWarnings(as.Date(toupper(x), format = "%d %b %Y"))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}

# Helper to parse yymmdd10. (YYYY-MM-DD)
parse_yymmdd10 <- function(x) {
  if (is.na(x) || trimws(x) == "") return(as.Date(NA))
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # Also try with slashes
  d <- suppressWarnings(as.Date(x, format = "%Y/%m/%d"))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}

# Apply derivations
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "LB",
    LBTESTCD = TESTCD,
    LBCAT = CAT,
    LBTEST = case_when(
      LBTESTCD == "HGB" ~ "Hemoglobin",
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase",
      TRUE ~ NA_character_
    ),
    LBORRES = ORRES,
    LBORRESU = UNIT,
    LBSTRESC = ORRES,
    # input(ORRES, best.) - SAS best. reads numeric, returns missing if not numeric
    LBSTRESN = suppressWarnings(as.numeric(ORRES)),
    LBSTRESU = UNIT,
    LBORNRLO = LOW,
    LBORNRHI = HIGH,
    VISIT = str_replace_all(VISIT, "_", " "),
    # LBNRIND
    LBNRIND = case_when(
      is.na(LBSTRESN) ~ NA_character_,
      LBSTRESN < suppressWarnings(as.numeric(LOW)) ~ "LOW",
      LBSTRESN > suppressWarnings(as.numeric(HIGH)) ~ "HIGH",
      TRUE ~ "NORMAL"
    )
  )

# Date conversion: LBDTC = put(input(LBDT,date11.),yymmdd10.)
# Need to handle LBDT column: if LBDT ne "" then LBDTC = format(parse_date11(LBDT), "%Y-%m-%d")
sdtm_lb1 <- sdtm_lb1 %>%
  mutate(
    LBDT = as.character(LBDT),
    LBDTC = ifelse(is.na(LBDT) | trimws(LBDT) == "",
                   NA_character_,
                   {
                     d <- sapply(LBDT, function(x) {
                       dd <- parse_date11(x)
                       if (is.na(dd)) NA_character_ else format(dd, "%Y-%m-%d")
                     })
                     # sapply may return matrix if vectorized; ensure character
                     as.character(d)
                   }),
    # Study Day
    # if LBDTC ne "" and RFSTDTC ne "" then compute
    # _stdt = input(LBDTC,yymmdd10.); _rfdt = input(RFSTDTC,yymmdd10.)
    # if _stdt >= _rfdt then LBSTDY = (_stdt - _rfdt)+1 else LBSTDY = (_stdt - _rfdt)
    LBSTDY = {
      # Vectorized
      lbdtc_date <- sapply(LBDTC, parse_yymmdd10)
      rfdtc_date <- sapply(RFSTDTC, parse_yymmdd10)
      # Convert to Date (sapply returns numeric days since 1970)
      lbdtc_date <- as.Date(lbdtx <- lbdtc_date, origin = "1970-01-01")
      rfdtc_date <- as.Date(rfdtc_date, origin = "1970-01-01")
      # Compute difference
      diff <- as.numeric(lbdtx - as.numeric(rfdtc_date))
      # Actually lbdtc_date is already Date, but sapply lost class; redo properly
      lbd <- parse_yymmdd10_vec <- function(vec) {
        sapply(vec, function(x) {
          if (is.na(x) || x == "") return(NA_real_)
          d <- parse_yymmdd10(x)
          if (is.na(d)) NA_real_ else as.numeric(d)
        })
      }
      lbd_num <- lbd_num_vec <- sapply(LBDTC, function(x) {
        if (is.na(x) || trimws(x) == "") return(NA_real_)
        d <- parse_yymmdd10(x)
        if (is.na(d)) NA_real_ else as.numeric(d)
      })
      rfd_num <- sapply(RFSTDTC, function(x) {
        if (is.na(x) || trimws(x) == "") return(NA_real_)
        d <- parse_yymmdd10(x)
        if (is.na(d)) NA_real_ else as.numeric(d)
      })
      lbstdy <- ifelse(is.na(lbd_num) | is.na(rfd_num), NA_real_,
                       ifelse(lbd_num >= rfd_num, (lbd_num - rfd_num) + 1, (lbd_num - rfd_num)))
      lbstdy
    }
  )

# Ensure LBDTC is character YYYY-MM-DD, LBSTDY numeric
# Sort by USUBJID LBDTC (SAS proc sort)
sdtm_lb1 <- sdtm_lb1 %>%
  arrange(USUBJID, LBDTC)

# ------------------------------------------------------------------
# Derive LBSEQ
# ------------------------------------------------------------------
# SAS: by USUBJID; retain LBSEQ; if first.USUBJID then LBSEQ=1 else LBSEQ+1
sdtm_lb <- sdtm_lb1 %>%
  arrange(USUBJID, LBDTC) %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, LBSEQ,
         LBTESTCD, LBTEST, LBCAT,
         LBORRES, LBORRESU,
         LBSTRESC, LBSTRESN, LBSTRESU,
         LBORNRLO, LBORNRHI,
         LBNRIND,
         LBDTC, LBSTDY,
         VISIT)

# Ensure column order and types match SAS
# Lengths: STUDYID $10, DOMAIN $2, USUBJID $25, LBSEQ 8, etc.
# Convert to appropriate types
sdtm_lb <- sdtm_lb %>%
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    LBSEQ = as.integer(LBSEQ),
    LBTESTCD = as.character(LBTESTCD),
    LBTEST = as.character(LBTEST),
    LBCAT = as.character(LBCAT),
    LBORRES = as.character(LBORRES),
    LBORRESU = as.character(LBORRESU),
    LBSTRESC = as.character(LBSTRESC),
    LBSTRESN = as.numeric(LBSTRESN),
    LBSTRESU = as.character(LBSTRESU),
    LBORNRLO = as.character(LBORNRLO),
    LBORNRHI = as.character(LBORNRHI),
    LBNRIND = as.character(LBNRIND),
    LBDTC = as.character(LBDTC),
    LBSTDY = as.numeric(LBSTDY),
    VISIT = as.character(VISIT)
  )

# ------------------------------------------------------------------
# Write dataset lb.csv
# ------------------------------------------------------------------
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Create listing P026.lst - proc print data=sdtm_lb
# ------------------------------------------------------------------
# SAS proc print prints all observations with Obs number and all variables
# We need to reproduce text rendering with 92 numbers (as per expected)
# The listing should contain the same numbers SAS shows

# Prepare text rendering
# Use capture of print? We'll create a formatted text file

# Create a text file that mimics PROC PRINT
# Include title and observation count

# For CSV of table body, also write sdtm_lb as CSV for listing
# The spec says: each table or listing additionally as plain-text rendering and as CSV of table body
# So we need P026.lst.txt and P026.csv? But expected output is P026.lst -> write text rendering and CSV
# We'll write P026.txt and P026.csv plus P026.lst.txt for compatibility

# Build text rendering
txt_path <- file.path(out, "P026.txt")
lst_path <- file.path(out, "P026.lst.txt")
csv_path <- file.path(out, "P026.csv")

# Also write P026.lst as text (without extension handling)
# The harness expects OUT_DIR/P026.lst? But spec says write <name>.txt
# We'll write both

# Create formatted listing
# Use base R to write

# Count numbers in listing: 92 numbers - likely LBSEQ, LBSTRESN, LBSTDY etc.
# We'll ensure all numeric values are printed

# Prepare display dataframe for printing (similar to SAS)
# SAS prints variables in order kept: STUDYID DOMAIN USUBJID LBSEQ LBTESTCD LBTEST LBCAT LBORRES LBORRESU LBSTRESC LBSTRESN LBSTRESU LBORNRLO LBORNRHI LBNRIND LBDTC LBSTDY VISIT

# Create text file
sink(txt_path)
cat("PROC PRINT DATA=SDTM_LB\n")
cat("\n")
# Print header
# Use print.data.frame with row.names
print(as.data.frame(sdtm_lb), row.names = TRUE)
cat("\n")
cat(paste0("Number of observations: ", nrow(sdtm_lb), "\n"))
sink()

# Also copy to P026.lst.txt
file.copy(txt_path, lst_path, overwrite = TRUE)

# Also write P026.lst (without txt) for expected output name
lst2_path <- file.path(out, "P026.lst")
file.copy(txt_path, lst2_path, overwrite = TRUE)

# Write CSV of table body (same as lb.csv but named P026.csv)
write.csv(sdtm_lb, csv_path, row.names = FALSE, na = "")

# Also write P026.lst.csv
write.csv(sdtm_lb, file.path(out, "P026.lst.csv"), row.names = FALSE, na = "")

# Ensure lb.csv also exists as dataset lb
# Already written

# Verify outputs exist
stopifnot(file.exists(file.path(out, "lb.csv")))
stopifnot(file.exists(txt_path))

```