```r
# P028_tr.R – translation of SAS program P028_tr.sas to R
# --------------------------------------------------------------
# Load required packages (pre‑installed)
library(haven)      # SAS data import
library(readxl)     # Excel import
library(dplyr)      # Data manipulation
library(stringr)   # String handling
library(lubridate)  # Date handling

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
# Try .sas7bdat first, then .xpt
dm_path_sas7b <- file.path(rawsdtm, "dm.sas7bdat")
dm_path_xpt   <- file.path(rawsdtm, "dm.xpt")

if (file.exists(dm_path_sas7b)) {
  dm_raw <- read_sas(dm_path_sas7b)
} else if (file.exists(dm_path_xpt)) {
  dm_raw <- read_xpt(dm_path_xpt)
} else {
  stop("DM dataset not found in ", rawsdtm,
       " (looked for dm.sas7bdat or dm.xpt)")
}

dm_for_tr <- dm_raw %>%
  select(USUBJID, RFSTDTC) %>%   # keep only needed columns
  arrange(USUBJID)               # PROC SORT default order

# --------------------------------------------------------------
# 2. Prepare Raw Data – import edc_tr.xlsx
edc_tr_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_tr.xlsx")
if (!file.exists(edc_tr_path)) stop("edc_tr.xlsx not found at ", edc_tr_path)

tr_raw <- read_excel(edc_tr_path, guess_max = 10000) %>%
  mutate(
    # Create USUBJID exactly as SAS: catx("-", "ONCO-2026", SUBJID)
    USUBJID = paste0("ONCO-2026-", as.character(SUBJID))
  ) %>%
  arrange(USUBJID)

# --------------------------------------------------------------
# Helper: convert TRDT (raw date) to ISO character (YYYY-MM-DD)
convert_trdt <- function(x) {
  # x may be character like "01JAN2020" or numeric Excel serial
  if (is.numeric(x) || is.integer(x)) {
    # Excel serial number (origin 1899-12-30)
    as.Date(x, origin = "1899-12-30") %>% format("%Y-%m-%d")
  } else if (is.character(x)) {
    x_trim <- str_trim(x)
    if (x_trim == "" || is.na(x_trim)) return(NA_character_)
    # Try dmy first (e.g., "01JAN2020")
    d <- dmy(toupper(x_trim), quiet = TRUE)
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    # Fallback: try ymd (e.g., "2020-01-01")
    d2 <- ymd(x_trim, quiet = TRUE)
    if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    NA_character_
  } else {
    NA_character_
  }
}

# --------------------------------------------------------------
# 3. Map TR domain (merge with DM reference)
sdtm_tr1 <- tr_raw %>%
  left_join(dm_for_tr, by = "USUBJID") %>%          # MERGE by USUBJID, keep all TR rows
  mutate(
    STUDYID   = "ONCO-2026",
    DOMAIN    = "TR",
    TRLNKID   = as.character(LNKID),               # from raw TR
    TRTESTCD  = as.character(TESTCD),               # from raw TR
    TRTEST    = if_else(TRTESTCD == "LDIAM", "Longest Diameter", NA_character_),
    TRORRES   = as.character(ORRES),
    TRORRESU  = as.character(UNIT),
    TRSTRESC  = as.character(ORRES),
    # SAS: input(ORRES, best.) – numeric conversion, missing becomes .
    TRSTRESN  = suppressWarnings(as.numeric(ORRES)),
    TRSTRESU  = as.character(UNIT),
    TREVAL    = "INVESTIGATOR",
    # VISIT: upcase and replace "_" with " "
    VISIT     = toupper(str_replace_all(as.character(VISIT), "_", " ")),
    # Date conversion: TRDT -> TRDTC (character YYYY-MM-DD)
    TRDTC = convert_trdt(TRDT),
    # Study Day (TRSTDY) – replicate SAS logic
    TRSTDY = case_when(
      !is.na(TRDTC) & !is.na(RFSTDTC) &
        !is.na(as.Date(TRDTC, "%Y-%m-%d")) &
        !is.na(as.Date(RFSTDTC, "%Y-%m-%d")) ~ {
          stdt <- as.Date(TRDTC, "%Y-%m-%d")
          rfdt <- as.Date(RFSTDTC, "%Y-%m-%d")
          diff <- as.numeric(stdt - rfdt)
          if (stdt >= rfdt) diff + 1 else diff
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
  mutate(TRSEQ = row_number()) %>%   # RETAIN + FIRST. logic
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
# 5. Export final dataset (CSV as required)
write.csv(
  sdtm_tr,
  file = file.path(out_dir, "tr.csv"),
  row.names = FALSE,
  na = ""
)

# --------------------------------------------------------------
# 6. Create the listing output (text rendering + CSV)
listing_base <- "P028"

# Text rendering – tab‑separated, no quotes, matching PROC PRINT layout loosely
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