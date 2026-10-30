# P029_tu.R - translation of SAS program P029_tu.sas
# -------------------------------------------------
# This script reads the required raw datasets, performs the TU domain derivation,
# and writes the final SDTM TU dataset and a listing file to the directory
# specified by the OUT_DIR environment variable.

# Load required packages -------------------------------------------------
library(haven)      # read SAS datasets / transport files
library(readxl)     # read Excel files
library(dplyr)      # data manipulation
library(stringr)    # string handling
library(lubridate)  # date handling
library(tidyr)      # (not strictly needed but available)

# ----------------------------------------------------------------------
# Helper: convert SAS DATE11. (e.g., "01JAN2020") to Date (YYYY-MM-DD)
# ----------------------------------------------------------------------
sas_date11_to_ymd <- function(x) {
  as.Date(x, format = "%d%b%Y")
}

# ----------------------------------------------------------------------
# Define base (container) paths
# ----------------------------------------------------------------------
# Inside the Docker container the host directory /bench
# is mounted at /bench
bench_root <- "/bench"

# Raw data root (contains the Excel files and the SDTM folder)
rawdir  <- file.path(bench_root, "data", "raw")
rawsdtm <- file.path(rawdir, "cdisc-pilot", "sdtm")

# Output directory (provided by the environment)
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------
# 1. Prepare DM Reference Data
# ----------------------------------------------------------------------
# SDTM DM may be a SAS7BDAT or a transport XPT file.
dm_path_sas7b <- file.path(rawsdtm, "sdtm_dm.sas7bdat")
dm_path_xpt   <- file.path(rawsdtm, "sdtm_dm.xpt")

if (file.exists(dm_path_sas7b)) {
  dm_raw <- read_sas(dm_path_sas7b)
} else if (file.exists(dm_path_xpt)) {
  dm_raw <- read_xpt(dm_path_xpt)
} else {
  stop("Neither sdtm_dm.sas7bdat nor sdtm_dm.xpt found in ", rawsdtm)
}

dm_for_tu <- dm_raw %>%
  select(USUBJID, RFSTDTC) %>%
  mutate(
    USUBJID = as.character(USUBJID),
    RFSTDTC = as.character(RFSTDTC)   # keep as character for later conversion
  ) %>%
  arrange(is.na(USUBJID), USUBJID)   # SAS sort: missing first, case‑sensitive

# ----------------------------------------------------------------------
# 2. Prepare Raw Data (edc_tu)
# ----------------------------------------------------------------------
edc_tu_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_tu.xlsx")
tu_raw <- read_excel(edc_tu_path, guess_max = 10000) %>%
  mutate(across(everything(), as.character))

tu_prep <- tu_raw %>%
  mutate(
    USUBJID = paste0("ONCO-2026-", SUBJID),
    TULOC   = str_to_upper(str_replace_all(LOC, "_", " ")),
    TUMETHOD= str_to_upper(str_replace_all(METH, "_", " "))
  ) %>%
  select(USUBJID, LNKID, TYPE, VISIT, TUDT, TULOC, TUMETHOD) %>%
  arrange(is.na(USUBJID), USUBJID)   # SAS sort

# ----------------------------------------------------------------------
# 3. Map TU Domain (merge with DM)
# ----------------------------------------------------------------------
sdtm_tu1 <- tu_prep %>%
  left_join(dm_for_tu, by = "USUBJID", suffix = c("", ".dm")) %>%
  # Keep only rows that came from tu_prep (the SAS "if t;" flag)
  filter(!is.na(USUBJID)) %>%
  mutate(
    STUDYID   = "ONCO-2026",
    DOMAIN    = "TU",
    TULNKID   = LNKID,
    TUTESTCD  = "TUMIDENT",
    TUTEST    = "Tumor Identification",
    TUORRES   = str_to_upper(TYPE),
    TUSTRESC  = TUORRES,
    TUEVAL    = "INVESTIGATOR",
    VISIT     = str_to_upper(str_replace_all(VISIT, "_", " ")),
    TUDTC     = ifelse(!is.na(TUDT) & TUDT != "",
                       format(sas_date11_to_ymd(TUDT), "%Y-%m-%d"),
                       NA_character_)
  ) %>%
  # Compute Study Day (TUSTDY) when both dates are present
  mutate(
    TUSTDY = ifelse(!is.na(TUDTC) & !is.na(RFSTDTC) & TUDTC != "" & RFSTDTC != "",
                    {
                      stdt <- as.Date(TUDTC)
                      rfdt <- as.Date(RFSTDTC)
                      diff <- as.numeric(stdt - rfdt)
                      if (diff >= 0) diff + 1 else diff
                    },
                    NA_integer_)
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, TULNKID,
    TUTESTCD, TUTEST,
    TUORRES, TUSTRESC,
    TULOC, TUMETHOD,
    TUEVAL,
    TUDTC, TUSTDY,
    VISIT
  )

# ----------------------------------------------------------------------
# 4. Derive TUSEQ (sequence within USUBJID ordered by TUDTC)
# ----------------------------------------------------------------------
sdtm_tu1 <- sdtm_tu1 %>%
  arrange(is.na(USUBJID), USUBJID,
          is.na(TUDTC), TUDTC)   # SAS sort by USUBJID then TUDTC

sdtm_tu <- sdtm_tu1 %>%
  group_by(USUBJID) %>%
  mutate(TUSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, TUSEQ,
    TULNKID,
    TUTESTCD, TUTEST,
    TUORRES, TUSTRESC,
    TULOC,
    TUMETHOD,
    TUEVAL,
    TUDTC, TUSTDY,
    VISIT
  )

# ----------------------------------------------------------------------
# 5. Write final dataset (tu.csv)
# ----------------------------------------------------------------------
out_dataset_path <- file.path(out_dir, "tu.csv")
write.csv(sdtm_tu, out_dataset_path, row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# 6. Produce listing (text rendering + CSV) – mimics PROC PRINT
# ----------------------------------------------------------------------
listing_txt_path <- file.path(out_dir, "P029.lst")   # SAS expects .lst
listing_csv_path <- file.path(out_dir, "P029.csv")

# Text rendering: space‑separated, no quotes, missing as blank
write.table(
  sdtm_tu,
  file = listing_txt_path,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the listing (identical to the dataset CSV)
write.csv(sdtm_tu, listing_csv_path, row.names = FALSE, na = "")

# End of script ---------------------------------------------------------
