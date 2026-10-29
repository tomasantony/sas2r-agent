```r
# P028_tr.R – translation of SAS program P028_tr.sas to R
# --------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)      # for reading SAS transport files if needed
library(readxl)     # for reading the supplied Excel raw files
library(dplyr)      # data manipulation
library(stringr)    # string handling
library(lubridate)  # date handling

# --------------------------------------------------------------
# Environment & output folder
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Paths supplied by the benchmark
rawdir  <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

# --------------------------------------------------------------
# 1. Prepare DM reference data (sdtm_dm)
# The DM dataset may be stored as .sas7bdat or .xpt – try both
dm_file_sas7b <- file.path(rawsdtm, "dm.sas7bdat")
dm_file_xpt   <- file.path(rawsdtm, "dm.xpt")

if (file.exists(dm_file_sas7b)) {
  dm_raw <- read_sas(dm_file_sas7b)
} else if (file.exists(dm_file_xpt)) {
  dm_raw <- read_xpt(dm_file_xpt)
} else {
  stop("DM dataset not found in ", rawsdtm, " (looked for dm.sas7bdat or dm.xpt)")
}

dm_for_tr <- dm_raw %>%
  select(USUBJID, RFSTDTC) %>%      # keep only needed columns
  arrange(USUBJID)                  # PROC SORT default order

# --------------------------------------------------------------
# 2. Prepare Raw Data – import edc_tr.xlsx (already in WORK in SAS)
edc_tr_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_tr.xlsx")
if (!file.exists(edc_tr_path)) stop("edc_tr.xlsx not found at ", edc_tr_path)

tr_prep <- read_excel(edc_tr_path, guess_max = 10000) %>%
  mutate(
    # Create USUBJID as in SAS: catx("-", "ONCO-2026", SUBJID)
    USUBJID = paste0("ONCO-2026-", as.character(SUBJID))
  ) %>%
  arrange(USUBJID)

# --------------------------------------------------------------
# 3. Map TR domain (merge with DM reference)
sdtm_tr1 <- tr_prep %>%
  left_join(dm_for_tr, by = "USUBJID") %>%      # MERGE by USUBJID, keep all tr rows (t flag)
  mutate(
    STUDYID   = "ONCO-2026",
    DOMAIN    = "TR",
    TRLNKID   = as.character(LNKID),          # LNKID from raw TR
    TRTESTCD  = as.character(TESTCD),         # TESTCD from raw TR
    TRTEST    = if_else(TRTESTCD == "LDIAM", "Longest Diameter", NA_character_),
    TRORRES   = as.character(ORRES),
    TRORRESU  = as.character(UNIT),
    TRSTRESC  = as.character(ORRES),
    # Convert character ORRES to numeric the same way SAS INPUT(ORRES, best.)
    TRSTRESN  = suppressWarnings(as.numeric(ORRES)),
    TRSTRESU  = as.character(UNIT),
    TREVAL    = "INVESTIGATOR",
    # VISIT: upcase and replace "_" with " "
    VISIT     = toupper(str_replace_all(as.character(VISIT), "_", " ")),
    # Date conversion: TRDT (character like 01JAN2020) -> TRDTC (YYYY-MM-DD)
    TRDTC = if_else(
      !is.na(TRDT) & str_trim(TRDT) != "",
      format(dmy(toupper(TRDT)), "%Y-%m-%d"),
      NA_character_
    ),
    # Study Day calculation (TRSTDY)
    TRSTDY = case_when(
      !is.na(TRDTC) & !is.na(RFSTDTC) &
        !is.na(as.Date(TRDTC)) & !is.na(as.Date(RFSTDTC)) ~ {
          stdt <- as.Date(TRDTC)
          rfdt <- as.Date(RFSTDTC)
          diff <- as.numeric(stdt - rfdt)
          ifelse(stdt >= rfdt, diff + 1, diff)
        },
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, TRLNKID,
    TRTESTCD, TRTEST,
    TRORRES, TRORRESU,
    TRSTRESC, TRSTRESN, TRSTRESU,
    TREVAL,
    TRDTC, TRSTDY,
    VISIT
  )

# --------------------------------------------------------------
# 4. Derive TRSEQ (sequential record number per USUBJID)
sdtm_tr <- sdtm_tr1 %>%
  arrange(USUBJID, TRDTC) %>%          # PROC SORT BY USUBJID TRDTC
  group_by(USUBJID) %>%
  mutate(TRSEQ = row_number()) %>%     # RETAIN + FIRST. logic
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, TRSEQ,
    TRLNKID,
    TRTESTCD, TRTEST,
    TRORRES, TRORRESU,
    TRSTRESC, TRSTRESN, TRSTRESU,
    TREVAL,
    TRDTC, TRSTDY,
    VISIT
  )

# --------------------------------------------------------------
# 5. Export final dataset (CSV as required by the task)
write.csv(
  sdtm_tr,
  file = file.path(out_dir, "tr.csv"),
  row.names = FALSE,
  na = ""
)

# --------------------------------------------------------------
# 6. Create the listing output (text rendering + CSV of the table)
# According to the specification, the listing name is P028 (SAS produced P028.lst)
listing_base <- "P028"

# Text rendering – tab‑separated, no quotes, matching SAS PROC PRINT layout loosely
write.table(
  sdtm_tr,
  file = file.path(out_dir, paste0(listing_base, ".txt")),
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same table body
write.csv(
  sdtm_tr,
  file = file.path(out_dir, paste0(listing_base, ".csv")),
  row.names = FALSE,
  na = ""
)

# --------------------------------------------------------------
# End of script
```