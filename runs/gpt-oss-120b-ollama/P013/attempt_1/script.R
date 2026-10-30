# ------------------------------------------------------------
# R translation of SAS program: P013_sdtm_eg.sas
# Produces dataset: eg.csv
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Define paths from macro variables (hard‑coded as per the SAS run)
# ------------------------------------------------------------------
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_ecg_path <- file.path(project_path, "data", "raw", "raw_ecg.csv")

# Output directory (environment variable set by the benchmark driver)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("OUT_DIR environment variable is not set.")
}

# ------------------------------------------------------------------
# 1. Import raw ECG CSV (equivalent to PROC IMPORT)
# ------------------------------------------------------------------
# readr guesses column types; we will coerce later where needed
raw_eg <- read_csv(
  raw_ecg_path,
  col_names = TRUE,
  na = c("", "NA"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# ------------------------------------------------------------------
# 2. Build the SDTM EG dataset (first DATA step)
# ------------------------------------------------------------------
# Helper: strip leading/trailing blanks (SAS STRIP)
strip <- function(x) {
  if (is.character(x)) trimws(x) else x
}

# Convert RESULT to numeric (SAS numeric)
raw_eg <- raw_eg %>%
  mutate(
    RESULT_NUM = as.numeric(RESULT)               # numeric version of RESULT
  )

# Date handling – mimic SAS VTYPE logic
# If DATE is character, parse with any date format; else treat as SAS numeric date (days since 1960‑01‑01)
raw_eg <- raw_eg %>%
  mutate(
    _egdt = case_when(
      is.character(DATE) ~ {
        # parse character dates; anydtdte. equivalent -> lubridate::parse_date_time
        parsed <- parse_date_time(DATE, orders = "any", exact = FALSE)
        as.Date(parsed)
      },
      !is.na(DATE) ~ {
        # numeric SAS date (days since 1960‑01‑01)
        as.Date(as.numeric(DATE), origin = "1960-01-01")
      },
      TRUE ~ as.Date(NA)                           # missing
    )
  ) %>%
  mutate(
    EGDTC = if_else(!is.na(_egdt), format(_egdt, "%Y-%m-%d"), NA_character_)
  )

# Create the EG domain variables
eg_step1 <- raw_eg %>%
  transmute(
    STUDYID = "CDISC-01",
    DOMAIN  = "EG",
    USUBJID = paste0("CDISC-01", "-", strip(SUBJECT)),
    VISIT   = strip(VISIT_NAM),
    VISITNUM = VISIT_NUM,
    EGTEST  = strip(TEST_NAME),
    EGTESTCD = case_when(
      strip(TEST_NAME) == "Heart Rate" ~ "HR",
      strip(TEST_NAME) == "QT Duration" ~ "QT",
      strip(TEST_NAME) == "RR Duration" ~ "RR",
      TRUE                            ~ "UNKNOWN"
    ),
    EGORRES = strip(as.character(RESULT_NUM)),
    EGORRESU = strip(UNIT),
    EGSTRESN = RESULT_NUM,
    EGSTRESC = strip(as.character(RESULT_NUM)),
    EGSTRESU = strip(UNIT),
    EGDTC   = EGDTC
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT,
    EGTESTCD, EGTEST, EGORRES, EGORRESU,
    EGSTRESC, EGSTRESN, EGSTRESU, EGDTC
  )

# ------------------------------------------------------------------
# 3. Sort by USUBJID, EGDTC (missing first), EGTESTCD
# ------------------------------------------------------------------
eg_sorted <- eg_step1 %>%
  arrange(
    USUBJID,
    desc(is.na(EGDTC)),   # missing dates first
    EGDTC,
    EGTESTCD
  )

# ------------------------------------------------------------------
# 4. Add sequence number per subject (second DATA step)
# ------------------------------------------------------------------
eg_final <- eg_sorted %>%
  group_by(USUBJID) %>%
  mutate(EGSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, EGSEQ, VISITNUM, VISIT,
    EGTESTCD, EGTEST, EGORRES, EGORRESU,
    EGSTRESC, EGSTRESN, EGSTRESU, EGDTC
  )

# ------------------------------------------------------------------
# 5. Write the output dataset as CSV (SAS style: no row names, empty strings for NA)
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "eg.csv")
write.csv(
  eg_final,
  file = output_path,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
