# P013_sdtm_eg.sas -> R translation
# SDTM EG domain from raw_ecg.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Ensure C collation for SAS-like character ordering (uppercase before lowercase)
try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE)

# Helper: SAS round half away from zero (not needed here but for fidelity)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS BEST32. equivalent (12 significant digits, strip)
sas_best_vec <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || is.nan(v) || is.infinite(v)) return(NA_character_)
    trimws(formatC(v, format = "g", digits = 12))
  }, character(1), USE.NAMES = FALSE)
}

# Helper: parse SAS ANYDTDTE. with single informat per column (slash order decided once)
parse_anydtdte_vec <- function(dates_char) {
  dates_char <- trimws(as.character(dates_char))
  dates_char[dates_char == ""] <- NA_character_
  n <- length(dates_char)
  result <- as.Date(rep(NA_character_, n))

  # Detect if all non-missing values are pure integers -> treat as SAS numeric dates
  nonmiss <- dates_char[!is.na(dates_char)]
  if (length(nonmiss) > 0 && all(grepl("^\\d+$", nonmiss))) {
    # Heuristic: if values look like SAS date numbers (e.g., 0-30000 range) treat as SAS dates
    # But to avoid misclassifying e.g., "20200101" as SAS date, check magnitude
    nums <- suppressWarnings(as.numeric(nonmiss))
    # SAS dates for 2000-2030 are ~14610-23741; yyyymmdd numbers are >20000000
    if (all(nums < 50000, na.rm = TRUE)) {
      result <- as.Date(suppressWarnings(as.numeric(dates_char)), origin = "1960-01-01")
      return(result)
    }
  }

  # Determine slash order once per column (MDY vs DMY)
  slash_vals <- dates_char[grepl("/", dates_char, fixed = TRUE) & !is.na(dates_char)]
  slash_order <- NULL
  if (length(slash_vals) > 0) {
    first_fields <- suppressWarnings(as.integer(sub("/.*", "", slash_vals)))
    # Also handle cases like "13/02/2020" -> first field >12 => DMY
    if (any(first_fields > 12, na.rm = TRUE)) {
      slash_order <- "DMY"
    } else {
      slash_order <- "MDY"
    }
  }

  for (i in seq_along(dates_char)) {
    d <- dates_char[i]
    if (is.na(d)) next
    if (grepl("/", d, fixed = TRUE)) {
      fmt <- if (!is.null(slash_order) && slash_order == "DMY") "%d/%m/%Y" else "%m/%d/%Y"
      parsed <- suppressWarnings(as.Date(d, format = fmt))
      if (is.na(parsed)) {
        fmt2 <- if (!is.null(slash_order) && slash_order == "DMY") "%d/%m/%y" else "%m/%d/%y"
        parsed <- suppressWarnings(as.Date(d, format = fmt2))
      }
      # Try with time component
      if (is.na(parsed) && nchar(d) > 10) {
        parsed <- suppressWarnings(as.Date(substr(d, 1, 10), format = fmt))
      }
      result[i] <- parsed
    } else if (grepl("-", d, fixed = TRUE)) {
      # Try ISO YMD first (most common for raw_ecg)
      parsed <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%d-%b-%Y"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%d-%B-%Y"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%Y/%m/%d"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%d-%m-%Y"))
      if (is.na(parsed)) {
        # datetime like 2020-01-01T10:00:00 or with space
        parsed <- suppressWarnings(as.Date(substr(d, 1, 10), format = "%Y-%m-%d"))
      }
      if (is.na(parsed)) {
        # Try lubridate-free fallback: as.Date with tryFormats is forbidden, so keep NA
        parsed <- NA
      }
      result[i] <- parsed
    } else if (grepl("^[0-9]{8}$", d)) {
      result[i] <- suppressWarnings(as.Date(d, format = "%Y%m%d"))
    } else {
      # Fallback ISO
      result[i] <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
    }
  }
  result
}

# Locate raw_ecg.csv (handle Windows absolute paths and Linux container)
find_raw_file <- function() {
  candidates <- c(
    "/bench/data/derived/c2c/data/raw/raw_ecg.csv",
    "/bench/data/raw/cdisc-pilot/sdtm/raw_ecg.csv",
    "/bench/data/raw/raw_ecg.csv",
    file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_ecg.csv"),
    file.path(Sys.getenv("project_path"), "data/raw/raw_ecg.csv"),
    file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_ecg.csv"),
    file.path(Sys.getenv("ROOT"), "data/derived/c2c/data/raw/raw_ecg.csv"),
    "data/derived/c2c/data/raw/raw_ecg.csv",
    "data/raw/raw_ecg.csv",
    file.path(getwd(), "data/derived/c2c/data/raw/raw_ecg.csv"),
    file.path(getwd(), "data/raw/raw_ecg.csv"),
    "../data/derived/c2c/data/raw/raw_ecg.csv"
  )
  candidates <- unique(candidates[candidates != "" & !is.na(candidates)])
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  # Recursive search from plausible roots
  search_roots <- unique(c(getwd(), dirname(getwd()), "/bench", ".", "..", "/tmp"))
  for (root in search_roots) {
    if (!is.na(root) && dir.exists(root)) {
      found <- list.files(root, pattern = "^raw_ecg\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(found) > 0) {
        # Prefer path containing c2c/data/raw
        pref <- found[grepl("c2c.*raw", found, ignore.case = TRUE)]
        if (length(pref) > 0) return(normalizePath(pref[1], winslash = "/", mustWork = FALSE))
        return(normalizePath(found[1], winslash = "/", mustWork = FALSE))
      }
    }
  }
  return(NA_character_)
}

raw_path <- find_raw_file()
if (is.na(raw_path) || !file.exists(raw_path)) {
  stop(paste0("raw_ecg.csv not found. Searched candidates and recursive search. OUT_DIR=", out))
}

# Read raw_ecg.csv - emulate PROC IMPORT getnames=yes
# Force character to control informat decision per column (SAS GUESSINGROWS logic)
suppressWarnings({
  raw_eg <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                            show_col_types = FALSE, na = c("", "NA", "."), trim_ws = TRUE)
})
# Fallback to base read.csv if readr fails
if (nrow(raw_eg) == 0) {
  raw_eg <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "."))
}

# SAS validvarname=upcase -> column names uppercase
names(raw_eg) <- toupper(names(raw_eg))

# Verify required columns exist (case-insensitive)
# Expected: SUBJECT, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, DATE
# Handle alternative spellings if needed
col_alias <- function(df, primary, alts = character(0)) {
  if (primary %in% names(df)) return(primary)
  for (a in alts) if (a %in% names(df)) return(a)
  return(NA_character_)
}
# Map aliases
c_subject <- col_alias(raw_eg, "SUBJECT", c("SUBJID", "USUBJID"))
c_visit_nam <- col_alias(raw_eg, "VISIT_NAM", c("VISITNAM", "VISIT", "VISIT_NAME"))
c_visit_num <- col_alias(raw_eg, "VISIT_NUM", c("VISITNUM", "VISIT_NO"))
c_test_name <- col_alias(raw_eg, "TEST_NAME", c("TESTNAME", "TEST", "EGTEST"))
c_result <- col_alias(raw_eg, "RESULT", c("RESULT_N", "AVAL", "EGORRES"))
c_unit <- col_alias(raw_eg, "UNIT", c("UNITS", "EGORRESU", "RESULT_UNIT"))
c_date <- col_alias(raw_eg, "DATE", c("EGDTC", "EGDAT", "DATE_COL", "ECG_DATE", "EGDATE"))

# Stop if critical columns missing
stopifnot(!is.na(c_subject), !is.na(c_test_name), !is.na(c_result), !is.na(c_date))

# Extract vectors (ensure character)
SUBJECT_vec <- trimws(as.character(raw_eg[[c_subject]]))
VISIT_NAM_vec <- if (!is.na(c_visit_nam)) trimws(as.character(raw_eg[[c_visit_nam]])) else rep(NA_character_, nrow(raw_eg))
VISIT_NUM_vec <- if (!is.na(c_visit_num)) raw_eg[[c_visit_num]] else rep(NA_character_, nrow(raw_eg))
TEST_NAME_vec <- trimws(as.character(raw_eg[[c_test_name]]))
RESULT_vec_raw <- raw_eg[[c_result]]
UNIT_vec <- if (!is.na(c_unit)) trimws(as.character(raw_eg[[c_unit]])) else rep(NA_character_, nrow(raw_eg))
DATE_vec_raw <- raw_eg[[c_date]]

# Convert numeric columns (SAS PROC IMPORT would infer numeric for RESULT and VISIT_NUM)
RESULT_num <- suppressWarnings(as.numeric(RESULT_vec_raw))
VISIT_NUM_num <- suppressWarnings(as.numeric(VISIT_NUM_vec))

# Handle DATE: check if numeric SAS date (vtype = N) vs character
# Since we forced character, detect numeric SAS date strings vs date strings
# If DATE column was originally numeric in SAS, it would be SAS date number; we handle via parse helper
# Also handle case where DATE was read as numeric (if read.csv fallback)
if (is.numeric(DATE_vec_raw)) {
  # Direct SAS numeric date
  egdt_date <- as.Date(DATE_vec_raw, origin = "1960-01-01")
} else {
  # Character path: emulate if vtype(DATE)='C' then input(anydtdte.) else DATE
  # Decide informat once per column (slash order) inside parse_anydtdte_vec
  egdt_date <- parse_anydtdte_vec(DATE_vec_raw)
  # Also handle if DATE values are numeric strings representing SAS dates (e.g., "21916")
  # parse_anydtdte_vec already handles pure-digit case as SAS dates
}

# Build SDTM EG dataset (first data step)
STUDYID_vec <- rep("CDISC-01", nrow(raw_eg))
DOMAIN_vec <- rep("EG", nrow(raw_eg))

# USUBJID = catx('-', STUDYID, strip(SUBJECT)) - catx skips missing
USUBJID_vec <- ifelse(is.na(SUBJECT_vec) | SUBJECT_vec == "",
                      STUDYID_vec,
                      paste0(STUDYID_vec, "-", SUBJECT_vec))

VISIT_vec <- VISIT_NAM_vec
VISIT_vec[VISIT_vec == ""] <- NA_character_

VISITNUM_vec <- VISIT_NUM_num

EGTEST_vec <- TEST_NAME_vec
EGTEST_vec[EGTEST_vec == ""] <- NA_character_

# EGTESTCD mapping (case-sensitive exact match)
EGTESTCD_vec <- rep("UNKNOWN", length(EGTEST_vec))
EGTESTCD_vec[!is.na(EGTEST_vec) & EGTEST_vec == "Heart Rate"] <- "HR"
EGTESTCD_vec[!is.na(EGTEST_vec) & EGTEST_vec == "QT Duration"] <- "QT"
EGTESTCD_vec[!is.na(EGTEST_vec) & EGTEST_vec == "RR Duration"] <- "RR"

EGORRES_vec <- sas_best_vec(RESULT_num)
EGORRESU_vec <- UNIT_vec
EGORRESU_vec[EGORRESU_vec == ""] <- NA_character_

EGSTRESN_vec <- RESULT_num
EGSTRESC_vec <- sas_best_vec(EGSTRESN_vec)
EGSTRESU_vec <- EGORRESU_vec

# EGDTC = put(_egdt, is8601da.) => YYYY-MM-DD, blank if missing
EGDTC_vec <- ifelse(is.na(egdt_date), NA_character_, format(egdt_date, "%Y-%m-%d"))

# Assemble first-stage dataset with keep list
sdtm_eg <- data.frame(
  STUDYID = STUDYID_vec,
  DOMAIN = DOMAIN_vec,
  USUBJID = USUBJID_vec,
  VISITNUM = VISITNUM_vec,
  VISIT = VISIT_vec,
  EGTESTCD = EGTESTCD_vec,
  EGTEST = EGTEST_vec,
  EGORRES = EGORRES_vec,
  EGORRESU = EGORRESU_vec,
  EGSTRESC = EGSTRESC_vec,
  EGSTRESN = EGSTRESN_vec,
  EGSTRESU = EGSTRESU_vec,
  EGDTC = EGDTC_vec,
  stringsAsFactors = FALSE
)

# PROC SORT by USUBJID EGDTC EGTESTCD (missing first, SAS character order)
# Use base order with na.last=FALSE to put missing first
ord <- order(sdtm_eg$USUBJID, sdtm_eg$EGDTC, sdtm_eg$EGTESTCD, na.last = FALSE)
sdtm_eg <- sdtm_eg[ord, , drop = FALSE]

# Second data step: by USUBJID, generate EGSEQ
# retain STUDYID DOMAIN USUBJID EGSEQ VISITNUM VISIT EGTESTCD EGTEST EGORRES EGORRESU EGSTRESC EGSTRESN EGSTRESU EGDTC
# if first.USUBJID then EGSEQ=1 else EGSEQ+1
sdtm_eg <- sdtm_eg %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(EGSEQ = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, EGSEQ, VISITNUM, VISIT, EGTESTCD, EGTEST, EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC)

# Ensure column order and types
# Convert VISITNUM and EGSTRESN to numeric (already), EGSEQ integer
sdtm_eg$EGSEQ <- as.integer(sdtm_eg$EGSEQ)
sdtm_eg$VISITNUM <- suppressWarnings(as.numeric(sdtm_eg$VISITNUM))
sdtm_eg$EGSTRESN <- suppressWarnings(as.numeric(sdtm_eg$EGSTRESN))

# Sort final check (already sorted, but ensure within USUBJID EGSEQ is sequential)
# Write dataset
write.csv(sdtm_eg, file.path(out, "eg.csv"), row.names = FALSE, na = "")
