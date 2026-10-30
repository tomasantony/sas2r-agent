# P027_rs.R - translation of SAS program to R (attempt 3)
# -------------------------------------------------
# Required libraries (pre‑installed in the execution environment)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(haven)

# -------------------------------------------------
# Helper: convert SAS DATE11. (e.g., "01JAN2020") to Date
sas_date11_to_date <- function(x) {
  if (is.na(x) || x == "") return(NA_Date_)
  as.Date(x, format = "%d%b%Y")
}

# -------------------------------------------------
# Output directory (provided by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -------------------------------------------------
# Helper to read Excel work files (mounted under /bench)
read_work_excel <- function(rel_path) {
  read_excel(file.path("/bench", rel_path))
}

# Load all WORK datasets (Excel files)
edc_ae    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_ae.xlsx")
edc_death <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_death.xlsx")
edc_dm    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_dm.xlsx")
edc_ds    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_ds.xlsx")
edc_ex    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_ex.xlsx")
edc_lb    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_lb.xlsx")
edc_rs    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_rs.xlsx")
edc_tr    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_tr.xlsx")
edc_tu    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_tu.xlsx")
edc_vs    <- read_work_excel("data/raw/faijal-oncology/raw_data/edc_vs.xlsx")
ivrs_rand <- read_work_excel("data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx")

# -------------------------------------------------
# Load SDTM DM reference dataset.
# The file may be .sas7bdat or .xpt; try both.
sdtm_dm_path_sas7bdat <- "/bench/data/raw/cdisc-pilot/sdtm/dm.sas7bdat"
sdtm_dm_path_xpt      <- "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt"

if (file.exists(sdtm_dm_path_sas7bdat)) {
  sdtm_dm <- haven::read_sas(sdtm_dm_path_sas7bdat)
} else if (file.exists(sdtm_dm_path_xpt)) {
  sdtm_dm <- haven::read_xpt(sdtm_dm_path_xpt)
} else {
  stop("SDTM DM dataset not found at either: ",
       sdtm_dm_path_sas7bdat, " or ", sdtm_dm_path_xpt)
}

# -------------------------------------------------
# 1. Prepare DM Reference Data (keep USUBJID and RFSTDTC, sort)
dm_for_rs <- sdtm_dm %>%
  select(USUBJID, RFSTDTC) %>%
  arrange(USUBJID)

# -------------------------------------------------
# 2. Prepare Raw Data (edc_rs) and create USUBJID
rs_prep <- edc_rs %>%
  mutate(
    USUBJID = str_c("ONCO-2026", "-", SUBJID)   # catx("-", "ONCO-2026", SUBJID)
  ) %>%
  arrange(USUBJID)

# -------------------------------------------------
# 3. Map RS Domain (inner merge of rs_prep with dm_for_rs)
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by = "USUBJID") %>%      # merge by USUBJID
  filter(!is.na(USUBJID)) %>%                  # keep rows that came from rs_prep (if r)
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
    # Date conversion from RSDT (DATE11.) to ISO YYYY‑MM‑DD
    RSDTC = if ("RSDT" %in% names(.)) {
      if_else(!is.na(RSDT) & RSDT != "",
              format(sas_date11_to_date(RSDT), "%Y-%m-%d"),
              NA_character_)
    } else {
      NA_character_
    },
    # Study Day calculation (RSSTDY)
    RSSTDY = ifelse(!is.na(RSDTC) & RSDTC != "" &
                     !is.na(RFSTDTC) & RFSTDTC != "",
                   {
                     stdt <- as.Date(RSDTC)
                     rfdt <- as.Date(RFSTDTC)
                     diff <- as.numeric(stdt - rfdt)
                     ifelse(stdt >= rfdt, diff + 1, diff)
                   },
                   NA_real_)
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
# 5. Export Final Dataset (CSV) – matches SAS PROC EXPORT to Excel, but CSV is required by spec
write.csv(
  sdtm_rs,
  file = file.path(out_dir, "rs.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# 6. Create Listing (PROC PRINT equivalent)
# SAS would produce a .lst file; we write a plain‑text version and a CSV version.
listing_txt_path <- file.path(out_dir, "P027.lst")   # .lst as requested
listing_csv_path <- file.path(out_dir, "P027.csv")

# Plain‑text rendering (space‑separated, no quotes, missing as blank)
write.table(
  sdtm_rs,
  file = listing_txt_path,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same table (useful for automated checks)
write.csv(
  sdtm_rs,
  file = listing_csv_path,
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# End of script
