out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(haven)
})

try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE)

sas_best_vec <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || is.nan(v) || is.infinite(v)) return(NA_character_)
    trimws(formatC(v, format = "g", digits = 12))
  }, character(1), USE.NAMES = FALSE)
}

parse_anydtdte_vec <- function(dates_char) {
  dates_char <- trimws(as.character(dates_char))
  dates_char[dates_char == ""] <- NA_character_
  n <- length(dates_char)
  result <- as.Date(rep(NA_character_, n))
  nonmiss <- dates_char[!is.na(dates_char)]
  if (length(nonmiss) > 0 && all(grepl("^\\d+$", nonmiss))) {
    nums <- suppressWarnings(as.numeric(nonmiss))
    if (all(nums < 50000, na.rm = TRUE)) {
      result <- as.Date(suppressWarnings(as.numeric(dates_char)), origin = "1960-01-01")
      return(result)
    }
  }
  slash_vals <- dates_char[grepl("/", dates_char, fixed = TRUE) & !is.na(dates_char)]
  slash_order <- NULL
  if (length(slash_vals) > 0) {
    first_fields <- suppressWarnings(as.integer(sub("/.*", "", slash_vals)))
    if (any(first_fields > 12, na.rm = TRUE)) slash_order <- "DMY" else slash_order <- "MDY"
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
      if (is.na(parsed) && nchar(d) > 10) parsed <- suppressWarnings(as.Date(substr(d, 1, 10), format = fmt))
      result[i] <- parsed
    } else if (grepl("-", d, fixed = TRUE)) {
      parsed <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%d-%b-%Y"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%d-%B-%Y"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%Y/%m/%d"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(d, format = "%d-%m-%Y"))
      if (is.na(parsed)) parsed <- suppressWarnings(as.Date(substr(d, 1, 10), format = "%Y-%m-%d"))
      result[i] <- parsed
    } else if (grepl("^[0-9]{8}$", d)) {
      result[i] <- suppressWarnings(as.Date(d, format = "%Y%m%d"))
    } else {
      result[i] <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
    }
  }
  result
}

find_raw_file <- function() {
  candidates <- c(
    "/bench/data/derived/c2c/data/raw/raw_ecg.csv",
    "/bench/data/derived/c2c/data/raw/raw_ecg.csv",
    file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_ecg.csv"),
    file.path(Sys.getenv("project_path"), "data/raw/raw_ecg.csv"),
    file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_ecg.csv"),
    "data/derived/c2c/data/raw/raw_ecg.csv",
    "data/raw/raw_ecg.csv",
    file.path(getwd(), "data/derived/c2c/data/raw/raw_ecg.csv"),
    "../data/derived/c2c/data/raw/raw_ecg.csv"
  )
  candidates <- unique(candidates[candidates != "" & !is.na(candidates)])
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  search_roots <- unique(c(getwd(), dirname(getwd()), "/bench", "/tmp", ".", ".."))
  for (root in search_roots) {
    if (!is.na(root) && dir.exists(root)) {
      found <- list.files(root, pattern = "^raw_ecg\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(found) > 0) {
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
  stop(paste0("raw_ecg.csv not found. OUT_DIR=", out))
}

suppressWarnings({
  raw_eg <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                            show_col_types = FALSE, na = c("", "NA", "."), trim_ws = TRUE)
})
if (nrow(raw_eg) == 0) {
  raw_eg <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "."))
}
names(raw_eg) <- toupper(names(raw_eg))

col_alias <- function(df, primary, alts = character(0)) {
  if (primary %in% names(df)) return(primary)
  for (a in alts) if (a %in% names(df)) return(a)
  return(NA_character_)
}
c_subject <- col_alias(raw_eg, "SUBJECT", c("SUBJID", "USUBJID"))
c_visit_nam <- col_alias(raw_eg, "VISIT_NAM", c("VISITNAM", "VISIT", "VISIT_NAME"))
c_visit_num <- col_alias(raw_eg, "VISIT_NUM", c("VISITNUM", "VISIT_NO"))
c_test_name <- col_alias(raw_eg, "TEST_NAME", c("TESTNAME", "TEST", "EGTEST"))
c_result <- col_alias(raw_eg, "RESULT", c("RESULT_N", "AVAL", "EGORRES"))
c_unit <- col_alias(raw_eg, "UNIT", c("UNITS", "EGORRESU", "RESULT_UNIT"))
c_date <- col_alias(raw_eg, "DATE", c("EGDTC", "EGDAT", "DATE_COL", "ECG_DATE", "EGDATE"))
stopifnot(!is.na(c_subject), !is.na(c_test_name), !is.na(c_result), !is.na(c_date))

SUBJECT_vec <- trimws(as.character(raw_eg[[c_subject]]))
VISIT_NAM_vec <- if (!is.na(c_visit_nam)) trimws(as.character(raw_eg[[c_visit_nam]])) else rep(NA_character_, nrow(raw_eg))
VISIT_NUM_vec <- if (!is.na(c_visit_num)) raw_eg[[c_visit_num]] else rep(NA_character_, nrow(raw_eg))
TEST_NAME_vec <- trimws(as.character(raw_eg[[c_test_name]]))
RESULT_vec_raw <- raw_eg[[c_result]]
UNIT_vec <- if (!is.na(c_unit)) trimws(as.character(raw_eg[[c_unit]])) else rep(NA_character_, nrow(raw_eg))
DATE_vec_raw <- raw_eg[[c_date]]

RESULT_num <- suppressWarnings(as.numeric(RESULT_vec_raw))
VISIT_NUM_num <- suppressWarnings(as.numeric(VISIT_NUM_vec))

if (is.numeric(DATE_vec_raw)) {
  egdt_date <- as.Date(DATE_vec_raw, origin = "1960-01-01")
} else {
  egdt_date <- parse_anydtdte_vec(DATE_vec_raw)
}

STUDYID_vec <- rep("CDISC-01", nrow(raw_eg))
DOMAIN_vec <- rep("EG", nrow(raw_eg))
USUBJID_vec <- ifelse(is.na(SUBJECT_vec) | SUBJECT_vec == "", STUDYID_vec, paste0(STUDYID_vec, "-", SUBJECT_vec))
VISIT_vec <- VISIT_NAM_vec
VISIT_vec[VISIT_vec == ""] <- NA_character_
VISITNUM_vec <- VISIT_NUM_num
EGTEST_vec <- TEST_NAME_vec
EGTEST_vec[EGTEST_vec == ""] <- NA_character_
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
EGDTC_vec <- ifelse(is.na(egdt_date), NA_character_, format(egdt_date, "%Y-%m-%d"))

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

ord <- order(sdtm_eg$USUBJID, sdtm_eg$EGDTC, sdtm_eg$EGTESTCD, na.last = FALSE)
sdtm_eg <- sdtm_eg[ord, , drop = FALSE]

# by USUBJID: EGSEQ = 1,2,... (retain logic) - base R to avoid pipe dependency
sdtm_eg$EGSEQ <- ave(seq_len(nrow(sdtm_eg)), sdtm_eg$USUBJID, FUN = seq_along)
sdtm_eg$EGSEQ <- as.integer(sdtm_eg$EGSEQ)

# retain column order as in second data step
sdtm_eg <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","EGSEQ","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC")]

sdtm_eg$VISITNUM <- suppressWarnings(as.numeric(sdtm_eg$VISITNUM))
sdtm_eg$EGSTRESN <- suppressWarnings(as.numeric(sdtm_eg$EGSTRESN))

write.csv(sdtm_eg, file.path(out, "eg.csv"), row.names = FALSE, na = "")
