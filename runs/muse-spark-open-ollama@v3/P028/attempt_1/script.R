# P028_tr.sas -> R translation
# SDTM TR mapping

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: trim like SAS
sas_catx <- function(sep, ...) {
  args <- list(...)
  # catx trims leading/trailing blanks and joins non-missing
  # In SAS catx("-", "ONCO-2026", SUBJID) -> if SUBJID missing then just "ONCO-2026"
  # Implement simple
  vals <- lapply(args, function(v) {
    if (is.null(v)) return(NA_character_)
    as.character(v)
  })
  # vectorized
  n <- max(lengths(vals))
  vals <- lapply(vals, function(v) rep_len(v, n))
  res <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(vals, function(v) v[i])
    # SAS catx: remove leading/trailing blanks, omit missing (empty after trim)
    parts <- trimws(parts)
    parts <- parts[!is.na(parts) & parts != ""]
    res[i] <- paste(parts, collapse = sep)
  }
  res
}

# Helper: SAS tranwrd + upcase
sas_tranwrd_upcase <- function(x, from, to) {
  # SAS tranwrd is case-sensitive, replaces all occurrences
  # then upcase
  ifelse(is.na(x), NA_character_, toupper(gsub(from, to, x, fixed = TRUE)))
}

# Helper: parse SAS date11. (DD-MMM-YYYY or DDMMMYYYY with optional dashes)
# SAS DATEw. informat: reads dates like 01JAN2020, 01-JAN-2020, 01JAN20 etc.
# We'll handle both with and without dashes, case-insensitive month
parse_date11 <- function(x) {
  # x is character like "01-JAN-2020" or "01JAN2020" or "01-Jan-2020"
  # Return Date or NA
  # Use strict parsing: try with dashes first, then without
  # SAS date11. width 11 expects DD-MMM-YYYY (11 chars inc dashes)
  # But be flexible
  x_trim <- trimws(as.character(x))
  res <- rep(as.Date(NA), length(x_trim))
  # Identify non-missing non-empty
  idx <- !is.na(x_trim) & x_trim != ""
  if (any(idx)) {
    # Normalize: remove dashes for parsing? Actually we can try both formats
    # Try format "%d-%b-%Y" (with dashes) and "%d%b%Y" (without)
    # Need to handle case-insensitive month: convert to proper case for %b
    # R's %b is locale dependent (English). Ensure English locale.
    # We'll uppercase then title case? Simpler: use toupper and match manually
    # Instead, parse manually: extract day, mon, year
    for (i in which(idx)) {
      v <- x_trim[i]
      # Remove leading/trailing spaces
      # Try to parse with regex
      # Pattern: 1-2 digits, optional dash, 3 letters, optional dash, 2 or 4 digits year
      m <- regmatches(v, regexec("^\\s*([0-9]{1,2})[- ]*([A-Za-z]{3})[- ]*([0-9]{2,4})\\s*$", v))[[1]]
      if (length(m) == 4) {
        d <- as.integer(m[2])
        mon_str <- toupper(m[3])
        y <- as.integer(m[4])
        # 2-digit year handling: SAS DATE handles 2-digit year with YEARCUTOFF
        # For simplicity, if 2-digit, assume 1900s? But our data likely 4-digit
        if (y < 100) {
          # SAS default YEARCUTOFF=1920 -> 00-19 => 2000-2019, 20-99 => 1920-1999
          # Implement that
          if (y <= 19) y <- 2000 + y else y <- 1900 + y
        }
        mon_num <- match(mon_str, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
        if (!is.na(mon_num) && !is.na(d) && !is.na(y)) {
          # Validate date
          dt_str <- sprintf("%04d-%02d-%02d", y, mon_num, d)
          dt <- as.Date(dt_str, format="%Y-%m-%d")
          # Check that day is valid (as.Date will handle invalid like 31 Feb -> NA)
          res[i] <- dt
        }
      } else {
        # Try yymmdd10. format? Not needed for TRDT input, but for safety
        # If already YYYY-MM-DD, parse
        dt2 <- suppressWarnings(as.Date(v, format="%Y-%m-%d"))
        if (!is.na(dt2)) res[i] <- dt2
      }
    }
  }
  res
}

format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# ------------------------------------------------------------------
# Locate raw data directories (from macro variables in SAS program)
# &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
# &rawadam. etc. but we need edc_* files
raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(raw_base, "faijal-oncology/raw_data")

# Also check for sdtm_dm if exists as xpt or dataset
# The SAS program expects sdtm_dm to exist; we will derive it if not found
# Try to locate any sdtm_dm file in raw directories
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(openxlsx)

# ------------------------------------------------------------------
# Import edc_* datasets (as SAS did via proc import)
# List xlsx files in edc_dir
xlsx_files <- list.files(edc_dir, pattern="\\.xlsx$", full.names=TRUE, ignore.case=TRUE)

# Helper to read xlsx with proc import semantics: first row is header, all columns as character initially?
# SAS proc import dbms=xlsx will guess types. We'll read with readxl and keep as character where needed
# For our purposes, read everything as text to preserve original strings, then convert as needed
read_edc <- function(path) {
  # readxl::read_excel guesses types; we want to keep original strings for dates etc.
  # Use col_types="text" to force text, then handle
  df <- readxl::read_excel(path, col_types="text", .name_repair="minimal")
  # Trim whitespace from column names (SAS would uppercase? but keep as is)
  names(df) <- trimws(names(df))
  # Also trim character values
  df <- df %>% mutate(across(everything(), ~trimws(as.character(.x))))
  # Convert empty strings to NA for consistency, but SAS treats "" as missing
  # Keep "" as "" for SAS logic (TRDT ne ""), but also handle NA
  df <- df %>% mutate(across(everything(), ~ifelse(is.na(.x), "", .x)))
  df
}

# Load all edc datasets into list
edc_list <- list()
for (f in xlsx_files) {
  ds_name <- tools::file_path_sans_ext(basename(f))
  # ds_name like edc_tr, edc_dm etc.
  df <- read_edc(f)
  edc_list[[ds_name]] <- df
}

# Extract needed datasets
edc_tr <- edc_list[["edc_tr"]]
edc_dm <- edc_list[["edc_dm"]]
edc_ex <- edc_list[["edc_ex"]]
edc_ds <- edc_list[["edc_ds"]]
ivrs_rand <- edc_list[["ivrs_rand"]]

# Debug: check columns
# print(names(edc_tr)); print(head(edc_tr))
# print(names(edc_dm)); print(head(edc_dm))
# print(names(edc_ex)); print(head(edc_ex))

# ------------------------------------------------------------------
# Derive sdtm_dm.RFSTDTC if not available
# In CDISC, RFSTDTC is reference start date, typically first exposure date (EXSTDTC) or randomization date
# For this pilot, we need to determine RFSTDTC per USUBJID
# Look at edc_dm and edc_ex structures to infer

# Inspect edc_dm columns
# Expected: SUBJID, RFSTDTC maybe already present? Let's check
# If edc_dm already has RFSTDTC, we can use it directly
# Otherwise derive from EX or RAND

# Helper to find column case-insensitively
find_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx)==0) return(NULL)
  names(df)[idx[1]]
}

# Try to locate RFSTDTC in edc_dm
rf_col_dm <- find_col(edc_dm, "RFSTDTC")
rf_col_ex <- if (!is.null(edc_ex)) find_col(edc_ex, "EXSTDTC") else NULL
rf_col_ex2 <- if (!is.null(edc_ex)) find_col(edc_ex, "EXSTDT") else NULL
rf_col_rand <- if (!is.null(ivrs_rand)) find_col(ivrs_rand, "RANDDT") else NULL

# Also check for sdtm_dm file in derived or raw
# Search for sdtm_dm.xpt or similar
sdtm_dm_path_candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm/dm.xpt",
  file.path(raw_base, "cdisc-pilot/sdtm/dm.xpt"),
  file.path(raw_base, "cdisc-pilot/sdtm/DM.xpt")
)
sdtm_dm_found <- NULL
for (p in sdtm_dm_path_candidates) {
  if (file.exists(p)) { sdtm_dm_found <- p; break }
}
# Also check for any dm file in sdtm folder
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
if (is.null(sdtm_dm_found) && dir.exists(sdtm_dir)) {
  cand <- list.files(sdtm_dir, pattern="(?i)dm\\.(xpt|sas7bdat|xlsx|csv)$", full.names=TRUE)
  if (length(cand)>0) sdtm_dm_found <- cand[1]
}

if (!is.null(sdtm_dm_found)) {
  # Read sdtm_dm
  if (grepl("\\.xpt$", sdtm_dm_found, ignore.case=TRUE)) {
    sdtm_dm_raw <- haven::read_xpt(sdtm_dm_found)
  } else if (grepl("\\.sas7bdat$", sdtm_dm_found, ignore.case=TRUE)) {
    sdtm_dm_raw <- haven::read_sas(sdtm_dm_found)
  } else if (grepl("\\.xlsx$", sdtm_dm_found, ignore.case=TRUE)) {
    sdtm_dm_raw <- readxl::read_excel(sdtm_dm_found, col_types="text")
  } else {
    sdtm_dm_raw <- read.csv(sdtm_dm_found, stringsAsFactors=FALSE)
  }
  # Normalize column names to uppercase for matching
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  # Keep USUBJID and RFSTDTC
  if ("USUBJID" %in% names(sdtm_dm_raw) && "RFSTDTC" %in% names(sdtm_dm_raw)) {
    dm_for_tr <- sdtm_dm_raw %>%
      select(USUBJID, RFSTDTC) %>%
      mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      ) %>%
      arrange(USUBJID)
  } else {
    sdtm_dm_found <- NULL # fallback to derivation
  }
}

if (is.null(sdtm_dm_found)) {
  # Derive RFSTDTC from available data
  # Strategy: Use edc_ex EXSTDTC if available, else edc_dm RFSTDTC, else ivrs_rand
  # First, inspect edc_dm to see what date columns exist
  # Print for debugging (will not show in final log much)
  # We'll attempt to derive RFSTDTC as earliest EX date per subject, or DM registration date

  # Check edc_dm columns
  # cat("edc_dm columns:", paste(names(edc_dm), collapse=", "), "\n")
  # cat("edc_ex columns:", if(!is.null(edc_ex)) paste(names(edc_ex), collapse=", ") else "NULL", "\n")
  # cat("ivrs_rand columns:", if(!is.null(ivrs_rand)) paste(names(ivrs_rand), collapse=", ") else "NULL", "\n")

  # Try to find a date column in edc_dm that could be RFSTDTC
  # Common: RFSTDTC, RFSTDT, ENROLLDT, etc.
  # Let's look at actual data sample to decide
  # We'll create dm_for_tr by constructing USUBJID and RFSTDTC

  # Helper to parse any date column to yymmdd10 character
  parse_any_to_yymmdd <- function(vec) {
    # vec is character, may be in date11 format (DD-MMM-YYYY) or YYYY-MM-DD or MM/DD/YYYY etc.
    # Try date11 first, then yymmdd10, then mmddyy
    res_date <- parse_date11(vec)
    # For those still NA, try yymmdd10
    idx_na <- is.na(res_date) & vec != "" & !is.na(vec)
    if (any(idx_na)) {
      res_date[idx_na] <- suppressWarnings(as.Date(vec[idx_na], format="%Y-%m-%d"))
    }
    # Try mm/dd/yyyy
    idx_na <- is.na(res_date) & vec != "" & !is.na(vec)
    if (any(idx_na)) {
      res_date[idx_na] <- suppressWarnings(as.Date(vec[idx_na], format="%m/%d/%Y"))
    }
    # Try dd/mm/yyyy
    idx_na <- is.na(res_date) & vec != "" & !is.na(vec)
    if (any(idx_na)) {
      res_date[idx_na] <- suppressWarnings(as.Date(vec[idx_na], format="%d/%m/%Y"))
    }
    format_yymmdd10(res_date)
  }

  # Attempt 1: if edc_dm has RFSTDTC column, use it
  if (!is.null(rf_col_dm)) {
    dm_for_tr <- edc_dm %>%
      mutate(
        USUBJID = sas_catx("-", "ONCO-2026", .data[[find_col(edc_dm, "SUBJID")]]),
        RFSTDTC = parse_any_to_yymmdd(.data[[rf_col_dm]])
      ) %>%
      select(USUBJID, RFSTDTC) %>%
      arrange(USUBJID)
  } else if (!is.null(edc_ex) && !is.null(find_col(edc_ex, "EXSTDTC"))) {
    # Derive from EX
    ex_date_col <- find_col(edc_ex, "EXSTDTC")
    ex_subj_col <- find_col(edc_ex, "SUBJID")
    dm_for_tr <- edc_ex %>%
      mutate(
        USUBJID = sas_catx("-", "ONCO-2026", .data[[ex_subj_col]]),
        tmp_date = parse_any_to_yymmdd(.data[[ex_date_col]]),
        tmp_date_d = as.Date(tmp_date, format="%Y-%m-%d")
      ) %>%
      group_by(USUBJID) %>%
      summarise(RFSTDTC = format_yymmdd10(min(tmp_date_d, na.rm=TRUE)), .groups="drop") %>%
      mutate(RFSTDTC = ifelse(RFSTDTC=="NA" | is.na(RFSTDTC), "", RFSTDTC)) %>%
      arrange(USUBJID)
  } else if (!is.null(edc_ex) && !is.null(find_col(edc_ex, "EXSTDT"))) {
    ex_date_col <- find_col(edc_ex, "EXSTDT")
    ex_subj_col <- find_col(edc_ex, "SUBJID")
    dm_for_tr <- edc_ex %>%
      mutate(
        USUBJID = sas_catx("-", "ONCO-2026", .data[[ex_subj_col]]),
        tmp_date = parse_any_to_yymmdd(.data[[ex_date_col]]),
        tmp_date_d = as.Date(tmp_date, format="%Y-%m-%d")
      ) %>%
      group_by(USUBJID) %>%
      summarise(RFSTDTC = format_yymmdd10(min(tmp_date_d, na.rm=TRUE)), .groups="drop") %>%
      mutate(RFSTDTC = ifelse(RFSTDTC=="NA" | is.na(RFSTDTC), "", RFSTDTC)) %>%
      arrange(USUBJID)
  } else {
    # Fallback: try to find any date column in edc_dm
    # Look for columns containing DT or DATE
    date_candidates <- names(edc_dm)[grepl("DT|DATE", toupper(names(edc_dm)))]
    if (length(date_candidates) > 0) {
      # Use first candidate
      dc <- date_candidates[1]
      subj_col <- find_col(edc_dm, "SUBJID")
      dm_for_tr <- edc_dm %>%
        mutate(
          USUBJID = sas_catx("-", "ONCO-2026", .data[[subj_col]]),
          RFSTDTC = parse_any_to_yymmdd(.data[[dc]])
        ) %>%
        select(USUBJID, RFSTDTC) %>%
        arrange(USUBJID)
    } else {
      # Last resort: create empty RFSTDTC
      subj_col <- find_col(edc_dm, "SUBJID")
      dm_for_tr <- edc_dm %>%
        mutate(
          USUBJID = sas_catx("-", "ONCO-2026", .data[[subj_col]]),
          RFSTDTC = ""
        ) %>%
        select(USUBJID, RFSTDTC) %>%
        arrange(USUBJID)
    }
  }

  # If dm_for_tr still missing some subjects that are in edc_tr, ensure we have all USUBJIDs
  # Also check ivrs_rand for additional subjects
  # Ensure dm_for_tr has entry for every USUBJID in edc_tr (even if RFSTDTC missing)
  tr_subjs <- unique(sas_catx("-", "ONCO-2026", edc_tr[[find_col(edc_tr, "SUBJID")]]))
  missing_subjs <- setdiff(tr_subjs, dm_for_tr$USUBJID)
  if (length(missing_subjs) > 0) {
    add_df <- data.frame(USUBJID = missing_subjs, RFSTDTC = "", stringsAsFactors=FALSE)
    dm_for_tr <- bind_rows(dm_for_tr, add_df) %>% arrange(USUBJID)
  }
}

# Ensure dm_for_tr sorted by USUBJID (SAS proc sort)
dm_for_tr <- dm_for_tr %>% arrange(USUBJID)

# ------------------------------------------------------------------
# 2. Prepare Raw Data: tr_prep
# edc_tr columns: need to identify SUBJID, LNKID, TESTCD, ORRES, UNIT, VISIT, TRDT
# Let's find them case-insensitively
tr_subj_col <- find_col(edc_tr, "SUBJID")
tr_lnkid_col <- find_col(edc_tr, "LNKID")
tr_testcd_col <- find_col(edc_tr, "TESTCD")
tr_orres_col <- find_col(edc_tr, "ORRES")
tr_unit_col <- find_col(edc_tr, "UNIT")
tr_visit_col <- find_col(edc_tr, "VISIT")
tr_trdt_col <- find_col(edc_tr, "TRDT")

# If any not found, try alternative names
if (is.null(tr_lnkid_col)) tr_lnkid_col <- find_col(edc_tr, "LNKID")
if (is.null(tr_testcd_col)) tr_testcd_col <- find_col(edc_tr, "TRTESTCD")
if (is.null(tr_orres_col)) tr_orres_col <- find_col(edc_tr, "TRORRES")
if (is.null(tr_unit_col)) tr_unit_col <- find_col(edc_tr, "TRORRESU")
if (is.null(tr_visit_col)) tr_visit_col <- find_col(edc_tr, "VISITNUM")

# Build tr_prep
tr_prep <- edc_tr %>%
  mutate(
    USUBJID = sas_catx("-", "ONCO-2026", .data[[tr_subj_col]]),
    LNKID = if (!is.null(tr_lnkid_col)) as.character(.data[[tr_lnkid_col]]) else "",
    TESTCD = if (!is.null(tr_testcd_col)) as.character(.data[[tr_testcd_col]]) else "",
    ORRES = if (!is.null(tr_orres_col)) as.character(.data[[tr_orres_col]]) else "",
    UNIT = if (!is.null(tr_unit_col)) as.character(.data[[tr_unit_col]]) else "",
    VISIT = if (!is.null(tr_visit_col)) as.character(.data[[tr_visit_col]]) else "",
    TRDT = if (!is.null(tr_trdt_col)) as.character(.data[[tr_trdt_col]]) else ""
  ) %>%
  # Keep original columns plus USUBJID for merge
  select(USUBJID, LNKID, TESTCD, ORRES, UNIT, VISIT, TRDT) %>%
  arrange(USUBJID)

# SAS: proc sort data=tr_prep by USUBJID
# Already sorted

# ------------------------------------------------------------------
# 3. Map TR Domain: sdtm_tr1
# Merge tr_prep(in=t) dm_for_tr by USUBJID, if t
# This is left join keeping all tr_prep rows, adding RFSTDTC

# Ensure dm_for_tr has unique USUBJID (SAS merge would take last? But we have one per USUBJID)
dm_for_tr_unique <- dm_for_tr %>% distinct(USUBJID, .keep_all=TRUE)

sdtm_tr1 <- tr_prep %>%
  left_join(dm_for_tr_unique, by="USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "TR",
    TRLNKID = LNKID,
    TRTESTCD = TESTCD,
    TRTEST = ifelse(TRTESTCD == "LDIAM", "Longest Diameter", ""),
    TRORRES = ORRES,
    TRORRESU = UNIT,
    TRSTRESC = ORRES,
    # TRSTRESN = input(ORRES, best.) -> SAS best. reads numeric with possible decimals, scientific
    TRSTRESN = suppressWarnings(as.numeric(ORRES)),
    TRSTRESU = UNIT,
    TREVAL = "INVESTIGATOR",
    VISIT = sas_tranwrd_upcase(VISIT, "_", " "),
    # Date Conversion: if TRDT ne "" then TRDTC = put(input(TRDT, date11.), yymmdd10.)
    # Need to handle TRDT missing as "" -> TRDTC missing
    TRDTC = ifelse(TRDT != "" & !is.na(TRDT),
                   format_yymmdd10(parse_date11(TRDT)),
                   NA_character_),
    TRDTC = ifelse(is.na(TRDTC), "", TRDTC),
    RFSTDTC = ifelse(is.na(RFSTDTC), "", RFSTDTC)
  ) %>%
  # Study Day calculation
  mutate(
    # _stdt = input(TRDTC, yymmdd10.), _rfdt = input(RFSTDTC, yymmdd10.)
    `_stdt` = suppressWarnings(as.Date(TRDTC, format="%Y-%m-%d")),
    `_rfdt` = suppressWarnings(as.Date(RFSTDTC, format="%Y-%m-%d")),
    TRSTDY = case_when(
      TRDTC != "" & RFSTDTC != "" & !is.na(`_stdt`) & !is.na(`_rfdt`) & `_stdt` >= `_rfdt` ~ as.numeric(`_stdt` - `_rfdt`) + 1,
      TRDTC != "" & RFSTDTC != "" & !is.na(`_stdt`) & !is.na(`_rfdt`) & `_stdt` < `_rfdt` ~ as.numeric(`_stdt` - `_rfdt`),
      TRUE ~ NA_real_
    )
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, TRLNKID, TRTESTCD, TRTEST, TRORRES, TRORRESU, TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, VISIT, TRDTC, TRSTDY, RFSTDTC, `_stdt`, `_rfdt`)

# ------------------------------------------------------------------
# 4. Derive TRSEQ: sort by USUBJID TRDTC, then by USUBJID retain
# SAS: proc sort data=sdtm_tr1 by USUBJID TRDTC
# Note: SAS sorts character TRDTC ascending, missing first
sdtm_tr1_sorted <- sdtm_tr1 %>%
  arrange(USUBJID, TRDTC)

# Now derive TRSEQ per USUBJID
sdtm_tr <- sdtm_tr1_sorted %>%
  group_by(USUBJID) %>%
  mutate(TRSEQ = row_number()) %>%
  ungroup() %>%
  # Keep required columns in SAS keep order
  select(STUDYID, DOMAIN, USUBJID, TRSEQ, TRLNKID, TRTESTCD, TRTEST, TRORRES, TRORRESU, TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, TRDTC, TRSTDY, VISIT) %>%
  # Ensure types: TRSEQ numeric, TRSTRESN numeric, TRSTDY numeric
  arrange(USUBJID, TRDTC, TRSEQ)

# ------------------------------------------------------------------
# 5. View Final Dataset (proc print) -> we will create listing text
# 6. Export Final Dataset

# Write dataset tr.csv
# Dates as YYYY-MM-DD (TRDTC already), numeric unrounded
# SAS variable names same case (we have uppercase)
write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names=FALSE, na="")

# Also need to handle SAS export to xlsx: we already wrote csv, but also ensure tr.csv matches expected
# The expected output dataset tr is the same as sdtm_tr

# ------------------------------------------------------------------
# Create listing text rendering (P028.lst)
# SAS proc print prints all observations with columns in order
# We need to reproduce text rendering with 52 numbers
# The listing should contain same numbers SAS shows (counts, etc.)
# For proc print, it's just the data rows

# We need to create a plain-text rendering that contains the same numbers
# Let's create a text file that mimics SAS listing output

# Count numbers: 52 numbers in P028.lst - likely number of numeric values displayed
# We'll generate a text file with title and table

# Build text output
lst_path <- file.path(out, "P028.lst")
# Also need .txt version per rules: each table/listing additionally as plain-text rendering
txt_path <- file.path(out, "P028.txt")
csv_body_path <- file.path(out, "P028.csv")

# Create formatted listing
# Use capture of print-like output
# We'll create a simple fixed-width listing similar to SAS proc print

# Prepare display dataframe for listing (convert NA to "" for display)
display_df <- sdtm_tr %>%
  mutate(
    TRSTRESN_disp = ifelse(is.na(TRSTRESN), "", as.character(TRSTRESN)),
    TRSTDY_disp = ifelse(is.na(TRSTDY), "", as.character(TRSTDY)),
    TRDTC_disp = ifelse(TRDTC=="", "", TRDTC)
  )

# Create text rendering
con <- file(lst_path, open="wt")
writeLines("SDTM TR Domain - Tumor Results", con)
writeLines(paste0("Study: ONCO-2026  Domain: TR  Listing generated: ", format(Sys.Date(), "%Y-%m-%d")), con)
writeLines("", con)
writeLines(paste0("Number of observations: ", nrow(sdtm_tr)), con)
writeLines(paste0("Number of subjects: ", n_distinct(sdtm_tr$USUBJID)), con)
writeLines("", con)

# Header line
header <- sprintf("%-12s %-4s %-15s %5s %-7s %-16s %-8s %-8s %-8s %-8s %-8s %-12s %-10s %6s %-15s",
                  "STUDYID","DOM","USUBJID","SEQ","LNKID","TESTCD","TEST","ORRES","ORRESU","STRESC","STRESN","STRESU","EVAL","DTC","STDY","VISIT")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse=""), con)

for (i in seq_len(nrow(sdtm_tr))) {
  row <- sdtm_tr[i,]
  line <- sprintf("%-12s %-4s %-15s %5d %-7s %-16s %-8s %-8s %-8s %-8s %-8s %-12s %-10s %6s %-15s",
                  row$STUDYID,
                  row$DOMAIN,
                  substr(row$USUBJID,1,15),
                  row$TRSEQ,
                  ifelse(is.na(row$TRLNKID),"",row$TRLNKID),
                  ifelse(is.na(row$TRTESTCD),"",row$TRTESTCD),
                  substr(ifelse(is.na(row$TRTEST),"",row$TRTEST),1,8),
                  ifelse(is.na(row$TRORRES),"",row$TRORRES),
                  ifelse(is.na(row$TRORRESU),"",row$TRORRESU),
                  ifelse(is.na(row$TRSTRESC),"",row$TRSTRESC),
                  ifelse(is.na(row$TRSTRESN),"",as.character(row$TRSTRESN)),
                  ifelse(is.na(row$TRSTRESU),"",row$TRSTRESU),
                  ifelse(is.na(row$TREVAL),"",row$TREVAL),
                  ifelse(is.na(row$TRDTC),"",row$TRDTC),
                  ifelse(is.na(row$TRSTDY),"",as.character(row$TRSTDY)),
                  ifelse(is.na(row$VISIT),"",substr(row$VISIT,1,15))
  )
  writeLines(line, con)
}
writeLines("", con)
writeLines("Notes:", con)
writeLines("TRTEST = Longest Diameter when TRTESTCD = LDIAM", con)
writeLines("TRDTC derived from TRDT via DATE11. -> YYMMDD10.", con)
writeLines("TRSTDY = (TRDTC - RFSTDTC) +1 if TRDTC >= RFSTDTC else (TRDTC - RFSTDTC)", con)
close(con)

# Copy to .txt as well (required)
file.copy(lst_path, txt_path, overwrite=TRUE)

# Write CSV of table body (same as tr.csv but for listing)
write.csv(sdtm_tr, csv_body_path, row.names=FALSE, na="")

# Also ensure tr.txt exists as plain-text rendering per rules (each table/listing additionally as txt)
# We already have P028.txt, but also need tr.txt? The rule says each table/listing additionally as txt
# Our listing is P028, so P028.txt is correct. Also create tr.txt as copy
file.copy(lst_path, file.path(out, "tr.txt"), overwrite=TRUE)

# Verify outputs
cat("Done. Wrote", nrow(sdtm_tr), "rows to", file.path(out, "tr.csv"), "\n")
cat("Listing written to", lst_path, "\n")
