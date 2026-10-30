```r
# ------------------------------------------------------------
# R translation of P006_adam_adsl.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------
read_sas_dataset <- function(path_no_ext) {
  # Try .sas7bdat first, then .xpt
  f1 <- paste0(path_no_ext, ".sas7bdat")
  f2 <- paste0(path_no_ext, ".xpt")
  if (file.exists(f1)) {
    read_sas(f1)
  } else if (file.exists(f2)) {
    read_xpt(f2)
  } else {
    stop(paste("Neither", f1, "nor", f2, "found"))
  }
}

# Round-half-up (SAS default) – not needed for current calculations
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}

# ------------------------------------------------------------------
# Environment setup
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# Resolve macro variables (hard‑coded from the description)
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"

# Build paths to SDTM datasets
dm_path   <- file.path(project_path, "data", "sdtm", "dm")
ex_path   <- file.path(project_path, "data", "sdtm", "ex")

# ------------------------------------------------------------------
# 1. Read source datasets
# ------------------------------------------------------------------
dm  <- read_sas_dataset(dm_path)
ex  <- read_sas_dataset(ex_path)

# Ensure variable names are uppercase (SAS option VALIDVARNAME=UPCASE)
names(dm) <- toupper(names(dm))
names(ex) <- toupper(names(ex))

# ------------------------------------------------------------------
# 2. Get first exposure record per subject
# ------------------------------------------------------------------
ex_first <- ex %>%
  arrange(USUBJID, EXSTDTC) %>%          # PROC SORT BY USUBJID EXSTDTC
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%                  # KEEP ONLY FIRST PER USUBJID
  ungroup() %>%
  select(USUBJID, EXTRT, EXSTDTC, EXENDTC)

# ------------------------------------------------------------------
# 3. Sort DM (already sorted by the join below)
# ------------------------------------------------------------------
dm_sorted <- dm %>% arrange(USUBJID)

# ------------------------------------------------------------------
# 4. Merge DM and first EX to create ADSL draft
# ------------------------------------------------------------------
adsl_draft <- dm_sorted %>%
  left_join(ex_first, by = "USUBJID", suffix = c(".dm", ".ex")) %>%
  # Keep only subjects present in DM (a flag in SAS)
  filter(!is.na(USUBJID)) %>%
  # Core attributes (retain order)
  mutate(
    STUDYID = STUDYID,
    SUBJID  = SUBJID,
    SITEID  = SITEID
  ) %>%
  # --------------------------------------------------------
  # Treatment variables & population flags
  # --------------------------------------------------------
  mutate(
    TRT01P = if_else(!is.na(ARMCD) & ARMCD != "SCRNFAIL", ARM, "UNPLANNED"),
    TRT01A = if_else(!is.na(EXTRT) & !is.na(EXTRT), EXTRT, "NOT TREATED"),
    ITTFL  = if_else(!is.na(ARMCD) & ARMCD != "SCRNFAIL", "Y", "N"),
    SAFFL  = if_else(!is.na(EXSTDTC) & EXSTDTC != "", "Y", "N")
  ) %>%
  # --------------------------------------------------------
  # Numeric date derivations
  # --------------------------------------------------------
  mutate(
    BRTHDT = if_else(str_length(BRTHDTC) >= 10,
                     ymd(str_sub(BRTHDTC, 1, 10)), as.Date(NA)),
    RFICDT = if_else(str_length(RFICDTC) >= 10,
                     ymd(str_sub(RFICDTC, 1, 10)), as.Date(NA)),
    TRTSDT = if_else(str_length(EXSTDTC) >= 10,
                     ymd(str_sub(EXSTDTC, 1, 10)), as.Date(NA)),
    TRTEDT = if_else(str_length(EXENDTC) >= 10,
                     ymd(str_sub(EXENDTC, 1, 10)), as.Date(NA)),
    EOSDT  = if_else(str_length(RFPENDTC) >= 10,
                     ymd(str_sub(RFPENDTC, 1, 10)), as.Date(NA))
  ) %>%
  # --------------------------------------------------------
  # Clinical math derivations
  # --------------------------------------------------------
  mutate(
    AGE = if_else(!is.na(BRTHDT) & !is.na(RFICDT),
                  floor(time_length(interval(BRTHDT, RFICDT), "years")),
                  as.numeric(NA)),
    TRTDURD = if_else(!is.na(TRTSDT) & !is.na(TRTEDT),
                      as.numeric(TRTEDT - TRTSDT) + 1,
                      as.numeric(NA))
  ) %>%
  # Keep only required variables, in the order specified by SAS KEEP statement
  select(
    STUDYID, USUBJID, SUBJID, SITEID,
    TRT01P, TRT01A, ITTFL, SAFFL,
    BRTHDT, RFICDT, TRTSDT, TRTEDT, EOSDT,
    AGE, TRTDURD, SEX, RACE
  ) %>%
  arrange(USUBJID)

# ------------------------------------------------------------------
# 5. Write final ADaM dataset
# ------------------------------------------------------------------
adsl_path <- file.path(out_dir, "adsl.csv")
write.csv(adsl_draft, adsl_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 6. Create listing (first 10 rows) – emulate PROC PRINT
# ------------------------------------------------------------------
listing_df <- adsl_draft %>%
  slice_head(n = 10) %>%
  select(USUBJID, TRT01P, TRT01A, ITTFL, SAFFL, AGE)

# Text rendering (space‑separated, similar to SAS PROC PRINT)
listing_txt_path <- file.path(out_dir, "P006.txt")
write.table(
  listing_df,
  file = listing_txt_path,
  sep = " ",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

# CSV version of the listing
listing_csv_path <- file.path(out_dir, "P006.csv")
write.csv(listing_df, listing_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```