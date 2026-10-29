# P028_tr.R - translation of SAS program to R
# -------------------------------------------------
# Load required packages
library(haven)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# -------------------------------------------------
# Set up environment
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

rawdir   <- "/bench/data/raw"
rawsdtm  <- "/bench/data/raw/cdisc-pilot/sdtm"

# -------------------------------------------------
# 1. Prepare DM Reference Data
dm_path <- file.path(rawsdtm, "dm.sas7bdat")   # assumed filename
dm_raw  <- read_sas(dm_path) %>% 
  select(USUBJID, RFSTDTC) %>% 
  arrange(USUBJID)

# -------------------------------------------------
# 2. Prepare Raw Data (EDC_TR)
edc_tr_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_tr.xlsx")
tr_raw <- read_excel(edc_tr_path, guess_max = 10000) %>% 
  mutate(
    USUBJID = paste0("ONCO-2026-", as.character(SUBJID))
  ) %>% 
  select(everything(), USUBJID) %>% 
  arrange(USUBJID)

# -------------------------------------------------
# 3. Map TR Domain
tr_merged <- tr_raw %>% 
  left_join(dm_raw, by = "USUBJID") %>% 
  mutate(
    STUDYID   = "ONCO-2026",
    DOMAIN    = "TR",
    TRLNKID   = as.character(LNKID),
    TRTESTCD  = as.character(TESTCD),
    TRTEST    = if_else(TRTESTCD == "LDIAM", "Longest Diameter", NA_character_),
    TRORRES   = as.character(ORRES),
    TRORRESU  = as.character(UNIT),
    TRSTRESC  = as.character(ORRES),
    TRSTRESN  = suppressWarnings(as.numeric(ORRES)),
    TRSTRESU  = as.character(UNIT),
    TREVAL    = "INVESTIGATOR",
    VISIT     = toupper(str_replace_all(as.character(VISIT), "_", " ")),
    # Date conversion: TRDT (e.g., "01JAN2020") -> TRDTC ("YYYY-MM-DD")
    TRDTC = if_else(
      !is.na(TRDT) & str_trim(TRDT) != "",
      format(dmy(toupper(TRDT)), "%Y-%m-%d"),
      NA_character_
    ),
    # Study Day calculation
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

# -------------------------------------------------
# 4. Derive TRSEQ
tr_final <- tr_merged %>% 
  arrange(USUBJID, TRDTC) %>% 
  group_by(USUBJID) %>% 
  mutate(TRSEQ = row_number()) %>% 
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

# -------------------------------------------------
# 5. Export Final Dataset (CSV)
write.csv(
  tr_final,
  file = file.path(out_dir, "tr.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# 6. Create Listing (text rendering + CSV)
listing_txt_path <- file.path(out_dir, "P028.lst")
listing_csv_path <- file.path(out_dir, "P028.csv")

# Text rendering – tab‑separated, no quotes
write.table(
  tr_final,
  file = listing_txt_path,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the listing
write.csv(
  tr_final,
  file = listing_csv_path,
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------
# End of script
