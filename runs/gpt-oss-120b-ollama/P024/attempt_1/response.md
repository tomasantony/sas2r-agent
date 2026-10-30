```r
# P024_ds translation to R
# ------------------------------------------------------------
# Load required packages
library(readxl)
library(haven)
library(dplyr)
library(lubridate)
library(stringr)
library(openxlsx)

# Helper: round half away from zero (not needed here but defined)
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  floor(x * 10^digits + 0.5) / 10^digits * ifelse(pos, 1, -1)
}

# ------------------------------------------------------------------
# Define paths (macro values from SAS program)
raw_dir   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
raw_sdtm  <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
out_dir   <- Sys.getenv("OUT_DIR")               # where all outputs go
out_ds    <- file.path(out_dir, "ds.xlsx")       # final Excel file

# ------------------------------------------------------------------
# 1. Load source datasets ------------------------------------------------

# EDc DS (Excel)
edc_ds <- read_excel(file.path(raw_dir, "edc_ds.xlsx"))

# SDTM DM (look for .xpt or .sas7bdat in the SDTM folder)
dm_file_xpt   <- list.files(raw_sdtm, pattern = "(?i)dm\\.(xpt)$", full.names = TRUE)
dm_file_sas7b <- list.files(raw_sdtm, pattern = "(?i)dm\\.(sas7bdat)$", full.names = TRUE)

if (length(dm_file_xpt) > 0) {
  sdtm_dm <- read_xpt(dm_file_xpt[1])
} else if (length(dm_file_sas7b) > 0) {
  sdtm_dm <- read_sas(dm_file_sas7b[1])
} else {
  stop("DM dataset not found in ", raw_sdtm)
}

# ------------------------------------------------------------------
# 2. STEP 5.1: SDTM DS MAPPING & DERIVATION ----------------------------

# --------------------------------------------------------------
# 1. Sort DM for Reference Dates (keep only needed columns)
dm_for_ds <- sdtm_dm %>%
  select(USUBJID, SUBJID, RFSTDTC, DTHDTC) %>%
  arrange(USUBJID)

# --------------------------------------------------------------
# 2. Prepare Raw DS Data
edc_ds_prep <- edc_ds %>%
  mutate(
    TERM  = DS_TERM,
    DECOD = DS_TERM,
    CAT   = DS_CAT,
    STDT  = DS_DT,
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  select(USUBJID, TERM, DECOD, CAT, STDT)

# --------------------------------------------------------------
# 3. Sort Datasets for Merge
edc_ds_prep <- edc_ds_prep %>% arrange(USUBJID)
dm_for_ds    <- dm_for_ds %>% arrange(USUBJID)

# --------------------------------------------------------------
# 4. Create SDTM DS Dataset (merge)
sdtm_ds1 <- edc_ds_prep %>%
  left_join(dm_for_ds, by = "USUBJID") %>%
  # keep only rows that came from edc_ds (a flag)
  filter(!is.na(TERM)) %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "DS",
    DSTERM  = str_replace_all(TERM, "_", " "),
    DSDECOD = toupper(DECOD),
    DSCAT   = str_replace_all(CAT, "_", " "),
    DTHFL   = if_else(!is.na(DTHDTC) & DTHDTC != "", "Y", NA_character_)
  ) %>%
  # Convert STDT (date11.) to ISO 8601
  mutate(
    DSSTDTC = if_else(
      !is.na(STDT) & STDT != "",
      {
        # SAS date11. format is DDMMMYYYY (e.g., 01JAN2020)
        dt <- as.Date(STDT, format = "%d%b%Y")
        ifelse(is.na(dt), "", format(dt, "%Y-%m-%d"))
      },
      ""
    )
  ) %>%
  # Derive DSSTDY
  mutate(
    DSSTDY = if_else(
      DSSTDTC != "" & RFSTDTC != "",
      {
        stdt <- as.Date(DSSTDTC, format = "%Y-%m-%d")
        rfdt <- as.Date(RFSTDTC, format = "%Y-%m-%d")
        diff <- as.numeric(stdt - rfdt)
        ifelse(stdt >= rfdt, diff + 1, diff)
      },
      NA_real_
    )
  ) %>%
  # Consistency check for subject 103
  {
    if (any(.$USUBJID == "ONCO-2026-103" & .$DSSTDTC != .$DTHDTC, na.rm = TRUE)) {
      warning("WARNING: Date mismatch! DS= ", .[.$USUBJID == "ONCO-2026-103", "DSSTDTC"],
              " DM= ", .[.$USUBJID == "ONCO-2026-103", "DTHDTC"])
    }
    .
  }

# --------------------------------------------------------------
# 5. Sort Final DS Dataset
sdtm_ds1 <- sdtm_ds1 %>% arrange(USUBJID, DSSTDTC)

# --------------------------------------------------------------
# 6. Derive DSSEQ
sdtm_ds <- sdtm_ds1 %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT, DSSTDTC,
    DSSTDY, DTHFL
  )

# ------------------------------------------------------------------
# 7. Export Final DS Dataset -----------------------------------------

# Write Excel file
openxlsx::write.xlsx(sdtm_ds, out_ds, overwrite = TRUE)

# Write CSV version (as required by the generic output handling)
write.csv(sdtm_ds,
          file = file.path(out_dir, "ds.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 8. Create Listing (PROC PRINT equivalent) ---------------------------

# Text rendering of the table
write.table(
  sdtm_ds,
  file = file.path(out_dir, "P024.lst.txt"),
  row.names = FALSE,
  col.names = TRUE,
  sep = " ",
  quote = FALSE
)

# CSV version of the listing
write.csv(
  sdtm_ds,
  file = file.path(out_dir, "P024.lst.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
```