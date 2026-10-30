# P027_rs.R - translation of SAS program to R
# -------------------------------------------------
# Required libraries
library(haven)      # for reading SAS datasets if needed
library(readxl)    # for reading Excel work files
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# -------------------------------------------------
# Helper: safe date conversion from SAS DATE11. (e.g., "01JAN2020")
sas_date11_to_date <- function(x) {
  # Return NA for empty strings or NA
  if (is.na(x) || x == "") return(NA_Date_)
  # Try parsing; SAS DATE11. is DDMMMYYYY (e.g., 01JAN2020)
  as.Date(x, format = "%d%b%Y")
}

# -------------------------------------------------
# Set output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -------------------------------------------------
# Load WORK datasets from the provided Excel files
# (paths are absolute as given in the problem statement)
edc_ae   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_ae.xlsx")
edc_death<- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_death.xlsx")
edc_dm   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx")
edc_ds   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_ds.xlsx")
edc_ex   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx")
edc_lb   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_lb.xlsx")
edc_rs   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_rs.xlsx")
edc_tr   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_tr.xlsx")
edc_tu   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_tu.xlsx")
edc_vs   <- read_excel("/bench/data/raw/faijal-oncology/raw_data/edc_vs.xlsx")
ivrs_rand<- read_excel("/bench/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx")

# -------------------------------------------------
# Load SDTM DM reference dataset (assumed to be a SAS dataset)
# Adjust the filename if the actual file differs.
sdtm_dm_path <- "/bench/data/raw/cdisc-pilot/sdtm/dm.sas7bdat"
if (file.exists(sdtm_dm_path)) {
  sdtm_dm <- read_sas(sdtm_dm_path)
} else {
  stop("SDTM DM dataset not found at: ", sdtm_dm_path)
}

# -------------------------------------------------
# 1. Prepare DM Reference Data (keep USUBJID and RFSTDTC, sort by USUBJID)
dm_for_rs <- sdtm_dm %>%
  select(USUBJID, RFSTDTC) %>%
  arrange(USUBJID)

# -------------------------------------------------
# 2. Prepare Raw Data (edc_rs) and create USUBJID
rs_prep <- edc_rs %>%
  mutate(
    USUBJID = str_c("ONCO-2026", "-", SUBJID)  # catx("-", "ONCO-2026", SUBJID)
  ) %>%
  select(everything()) %>%                     # keep all original variables plus USUBJID
  arrange(USUBJID)

# -------------------------------------------------
# 3. Map RS Domain (inner merge of rs_prep with dm_for_rs)
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by = "USUBJID", suffix = c("", ".dm")) %>%
  filter(!is.na(USUBJID)) %>%                 # equivalent to 'if r;' (keep rows from rs_prep)
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "RS",
    RSTESTCD = TESTCD,
    RSTEST = if_else(RSTESTCD == "OVRLRESP", "Overall Response", NA_character_),
    RSCAT = "RECIST 1.1",
    RSORRES = toupper(ORRES),
    RSSTRESC = RSORRES,
    RSEVAL = "INVESTIGATOR",
    VISIT = toupper(str_replace_all(VISIT, "_", " ")),
    RSDTC = if_else(
      !is.na(RSDT) & RSDT != "",
      format(sas_date11_to_date(RSDT), "%Y-%m-%d"),
      NA_character_
    ),
    # Study Day calculation
    RSSTDY = case_when(
      !is.na(RSDTC) & RSDTC != "" & !is.na(RFSTDTC) & RFSTDTC != "" ~ {
        stdt <- as.Date(RSDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(stdt - rfdt)
        ifelse(stdt >= rfdt, diff + 1, diff)
      },
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, RSTESTCD, RSTEST,
    RSCAT, RSORRES, RSSTRESC, RSEVAL,
    VISIT, RSDTC, RSSTDY
  ) %>%
  arrange(USUBJID, RSDTC)

# -------------------------------------------------
# 4. Derive RSSEQ (sequence within each USUBJID ordered by RSDTC)
sdtm_rs <- sdtm_rs1 %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, RSSEQ,
    RSTESTCD, RSTEST,
    RSCAT,
    RSORRES, RSSTRESC,
    RSEVAL,
    RSDTC, RSSTDY,
    VISIT
  ) %>%
  arrange(USUBJID, RSDTC)

# -------------------------------------------------
# 5. Export Final Dataset -------------------------------------------------
# Write the final SDTM RS dataset as CSV
write.csv(
  sdtm_rs,
  file = file.path(out_dir, "rs.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# 6. Create Listing (PROC PRINT equivalent) -------------------------------
# Produce a plain‑text rendering similar to PROC PRINT
listing_path_txt <- file.path(out_dir, "P027.txt")
listing_path_csv <- file.path(out_dir, "P027.csv")

# Text rendering (fixed‑width, space‑separated)
write.table(
  sdtm_rs,
  file = listing_path_txt,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the listing
write.csv(
  sdtm_rs,
  file = listing_path_csv,
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# End of script
