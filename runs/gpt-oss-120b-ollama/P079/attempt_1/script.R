# l_sae.R - translation of SAS program P079_l_sae.sas
# -------------------------------------------------
# Reads ADaM AE and subject-level data, creates a Serious Adverse Events (SAE) listing,
# and writes the result as CSV and plain‑text files in the directory given by OUT_DIR.

library(haven)      # read_sas
library(dplyr)      # data manipulation
library(lubridate)  # date parsing
library(stringr)    # string handling
library(tidyr)      # for replace_na if needed

# ----------------------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" ) stop("Environment variable OUT_DIR is not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Paths to the ADaM libraries (hard‑coded from the SAS macro variables)
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# ----------------------------------------------------------------------
# 2. Read source datasets
# ----------------------------------------------------------------------
adae_file <- file.path(adam_path, "adae.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

adae <- read_sas(adae_file)
adsl <- read_sas(adsl_file)

# ----------------------------------------------------------------------
# 3. Filter and join (SAFFL = 'Y' and AESER = 'Y')
# ----------------------------------------------------------------------
# Keep only safety analysis set subjects
adsl_saff <- adsl %>% filter(toupper(SAFFL) == "Y")

# Keep only serious adverse events
adae_sae  <- adae %>% filter(toupper(AESER) == "Y")

# Inner join on USUBJID
sae_list <- adae_sae %>%
  inner_join(adsl_saff, by = "USUBJID", suffix = c(".ae", ".sl")) %>%
  # Select and rename needed variables
  transmute(
    USUBJID      = USUBJID,
    TRTA        = TRTA,
    AGE         = AGE,
    SEX         = SEX,
    RACE        = RACE,
    TRT_START   = as.Date(TRTSDT),          # already a Date from haven
    SOC         = AEBODSYS,
    PT          = AEDECOD,
    VERBATIM    = AETERM,
    START_DATE  = AESTDTC,
    END_DATE    = AEENDTC,
    GRADE       = AETOXGR,
    RELATIONSHIP= AEREL,
    ACTION_TAKEN= AEACN,
    OUTCOME     = AEOUT,
    AESDTH      = AESDTH,
    AESLIFE     = AESLIFE,
    AESHOSP     = AESHOSP,
    AESDISAB    = AESDISAB,
    AESCONG     = AESCONG,
    AESMIE      = AESMIE
  )

# ----------------------------------------------------------------------
# 4. Order the data (TRTA, USUBJID, START_DATE)
# ----------------------------------------------------------------------
# Convert START_DATE to Date for ordering; keep original character for output
sae_list <- sae_list %>%
  mutate(
    _start_dt = ymd(START_DATE, quiet = TRUE)   # NA if not a valid date
  )

# Order: missing values first, case‑sensitive character ordering
sae_list <- sae_list %>%
  arrange(
    # character ordering (uppercase before lowercase) is default in R
    # NA values first (na.last = FALSE)
    order(TRTA, na.last = FALSE),
    order(USUBJID, na.last = FALSE),
    order(_start_dt, na.last = FALSE)
  ) %>%
  select(-_start_dt)

# ----------------------------------------------------------------------
# 5. Derive reporting variables
# ----------------------------------------------------------------------
sae_report <- sae_list %>%
  mutate(
    # SAE criteria concatenation
    sae_criteria = case_when(
      TRUE ~ str_c(
        ifelse(toupper(AESDTH)   == "Y", "Death",               NA_character_),
        ifelse(toupper(AESLIFE)  == "Y", "Life-threatening",    NA_character_),
        ifelse(toupper(AESHOSP)  == "Y", "Hospitalization",     NA_character_),
        ifelse(toupper(AESDISAB) == "Y", "Disability",          NA_character_),
        ifelse(toupper(AESCONG)  == "Y", "Congenital anomaly", NA_character_),
        ifelse(toupper(AESMIE)   == "Y", "Medically important",NA_character_),
        sep = ", "
      )
    ),
    # Remove leading/trailing commas/spaces
    sae_criteria = str_replace_all(sae_criteria, "^, |, $", ""),
    sae_criteria = str_trim(sae_criteria, side = "both"),
    # Onset day calculation
    onset_day = {
      # parse START_DATE (character) to Date; keep NA if not parseable
      start_dt <- ymd(START_DATE, quiet = TRUE)
      if (!is.na(TRT_START) && !is.na(start_dt)) {
        onset_num <- as.integer(start_dt - TRT_START + 1)
        as.character(onset_num)
      } else {
        ""
      }
    },
    # Demographics string AGE/SEX/first‑letter‑of‑RACE
    demog = str_c(
      ifelse(is.na(AGE), "", as.character(AGE)),
      "/",
      ifelse(is.na(SEX), "", SEX),
      "/",
      ifelse(is.na(RACE), "", str_sub(RACE, 1, 1))
    )
  ) %>%
  # Keep only columns needed for the final listing, in the order required by PROC REPORT
  select(
    TRTA, USUBJID, demog, SOC, PT,
    START_DATE, END_DATE, onset_day,
    GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME,
    sae_criteria
  )

# ----------------------------------------------------------------------
# 6. Write outputs
# ----------------------------------------------------------------------
# CSV of the listing
csv_path <- file.path(out_dir, "l_sae.csv")
write.csv(sae_report, csv_path, row.names = FALSE, na = "")

# Plain‑text rendering (tab‑separated, similar to SAS LISTING)
txt_path <- file.path(out_dir, "l_sae.txt")
write.table(
  sae_report,
  file = txt_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)

# ----------------------------------------------------------------------
# 7. Clean‑up (mirrors PROC DATASETS DELETE in SAS)
# ----------------------------------------------------------------------
rm(adae, adsl, adae_sae, adsl_saff, sae_list, sae_report)

# End of script
