# ------------------------------------------------------------
# R translation of SAS program: P013_sdtm_eg.sas
# Produces dataset: eg.csv
# ------------------------------------------------------------

# Load required packages (pre‑installed)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Paths (mirroring the macro variables used in the SAS run)
# ------------------------------------------------------------------
project_path <- "/bench/data/derived/c2c"
raw_ecg_path <- file.path(project_path, "data", "raw", "raw_ecg.csv")

# Output directory supplied by the benchmark driver
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable is not set.")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------
# 1. Import raw ECG CSV (equivalent to PROC IMPORT)
# ------------------------------------------------------------------
raw_eg <- read_csv(
  raw_ecg_path,
  col_names = TRUE,
  na = c("", "NA"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# ------------------------------------------------------------------
# Helper: SAS STRIP (remove leading/trailing blanks)
# ------------------------------------------------------------------
strip <- function(x) {
  if (is.character(x)) trimws(x) else x
}

# ------------------------------------------------------------------
# 2. Prepare numeric RESULT and SAS‑style date handling
# ------------------------------------------------------------------
raw_eg <- raw_eg %>%
  mutate(
    RESULT_NUM = as.numeric(RESULT)               # numeric version of RESULT
  ) %>%
  mutate(
    _egdt = case_when(
      is.character(DATE) ~ as.Date(parse_date_time(DATE, orders = "any", exact = FALSE)),
      !is.na(DATE)        ~ as.Date(as.numeric(DATE), origin = "1960-01-01"),
      TRUE                ~ as.Date(NA)
    ),
    EGDTC = if_else(!is.na(_egdt), format(_egdt, "%Y-%m-%d"), NA_character_)
  )

# ------------------------------------------------------------------
# 3. Build the first EG data step (mirrors the SAS DATA step)
# ------------------------------------------------------------------
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
      TRUE                              ~ "UNKNOWN"
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
# 4. Sort as PROC SORT would (missing dates first, then by date, then EGTESTCD)
# ------------------------------------------------------------------
eg_sorted <- eg_step1 %>%
  arrange(
    USUBJID,
    desc(is.na(EGDTC)),   # missing dates first
    EGDTC,
    EGTESTCD
  )

# ------------------------------------------------------------------
# 5. Add sequence number per subject (second DATA step with BY‑group processing)
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
# 6. Write the final SDTM EG dataset as CSV (SAS style: no row names, empty strings for NA)
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "eg.csv")
write.csv(
  eg_final,
  file = output_path,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
