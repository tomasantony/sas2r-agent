# l_sae.R - translation of SAS program P079_l_sae.sas (attempt 3)
# -----------------------------------------------------------------
# Reads ADaM AE and subject-level data, creates a Serious Adverse Events (SAE) listing,
# and writes the result as CSV and plain‑text files in the directory given by OUT_DIR.
# The plain‑text file includes SAS‑style titles/footnotes so that listing identifiers
# (e.g., 16.2.7.2) appear in the output.

library(haven)      # read_sas
library(dplyr)      # data manipulation
library(lubridate)  # date parsing
library(stringr)    # string handling
library(tidyr)      # replace_na

# -----------------------------------------------------------------
# 1. Setup
# -----------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ADaM library path (as defined in the SAS macro variables)
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# -----------------------------------------------------------------
# 2. Read source datasets
# -----------------------------------------------------------------
adae_file <- file.path(adam_path, "adae.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

adae <- read_sas(adae_file)
adsl <- read_sas(adsl_file)

# -----------------------------------------------------------------
# 3. Filter to Safety Analysis Set and Serious AEs
# -----------------------------------------------------------------
adsl_saff <- adsl %>% filter(toupper(SAFFL) == "Y")
adae_sae  <- adae %>% filter(toupper(AESER) == "Y")

# -----------------------------------------------------------------
# 4. Join (inner) on USUBJID
# -----------------------------------------------------------------
# Use suffixes to keep both versions of overlapping variable names
joined <- inner_join(adae_sae, adsl_saff, by = "USUBJID", suffix = c(".ae", ".sl"))

# -----------------------------------------------------------------
# 5. Build the listing dataset (mirrors PROC SQL + DATA step)
# -----------------------------------------------------------------
sae_report <- joined %>%
  # Resolve possible duplicate variable names (e.g., TRTA may exist in both)
  mutate(
    TRTA = coalesce(TRTA.ae, TRTA.sl),
    AGE  = coalesce(AGE.ae, AGE.sl),
    SEX  = coalesce(SEX.ae, SEX.sl),
    RACE = coalesce(RACE.ae, RACE.sl)
  ) %>%
  # Keep only needed columns and rename to SAS names
  transmute(
    USUBJID      = USUBJID,
    TRTA         = TRTA,
    AGE          = AGE,
    SEX          = SEX,
    RACE         = RACE,
    TRT_START    = as.Date(TRTSDT),          # numeric SAS date → Date
    SOC          = AEBODSYS,
    PT           = AEDECOD,
    VERBATIM     = AETERM,
    START_DATE   = AESTDTC,
    END_DATE     = AEENDTC,
    GRADE        = AETOXGR,
    RELATIONSHIP = AEREL,
    ACTION_TAKEN = AEACN,
    OUTCOME      = AEOUT,
    AESDTH       = AESDTH,
    AESLIFE      = AESLIFE,
    AESHOSP      = AESHOSP,
    AESDISAB     = AESDISAB,
    AESCONG      = AESCONG,
    AESMIE       = AESMIE
  ) %>%
  # Order as in SAS: by TRTA, USUBJID, START_DATE (missing values first)
  arrange(
    factor(TRTA, levels = unique(TRTA), ordered = TRUE),
    USUBJID,
    ymd(START_DATE, quiet = TRUE)
  ) %>%
  # Derive reporting variables
  mutate(
    # Concatenate SAE criteria
    sae_criteria = str_c(
      ifelse(toupper(AESDTH)   == "Y", "Death",               NA_character_),
      ifelse(toupper(AESLIFE)  == "Y", "Life-threatening",    NA_character_),
      ifelse(toupper(AESHOSP)  == "Y", "Hospitalization",     NA_character_),
      ifelse(toupper(AESDISAB) == "Y", "Disability",          NA_character_),
      ifelse(toupper(AESCONG)  == "Y", "Congenital anomaly", NA_character_),
      ifelse(toupper(AESMIE)   == "Y", "Medically important", NA_character_),
      sep = ", "
    ),
    sae_criteria = str_replace_all(sae_criteria, "^, |, $", ""),
    sae_criteria = str_trim(sae_criteria, side = "both"),
    # Study Day (onset) calculation
    onset_day = {
      start_dt <- ymd(START_DATE, quiet = TRUE)
      if (!is.na(TRT_START) && !is.na(start_dt)) {
        as.character(as.integer(start_dt - TRT_START + 1))
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
  # Final column order for the listing
  select(
    TRTA, USUBJID, demog, SOC, PT,
    START_DATE, END_DATE, onset_day,
    GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME,
    sae_criteria
  )

# -----------------------------------------------------------------
# 6. Write outputs
# -----------------------------------------------------------------
# 6a. CSV file (numeric values left as‑is, missing as empty)
csv_path <- file.path(out_dir, "l_sae.csv")
write.csv(sae_report, csv_path, row.names = FALSE, na = "")

# 6b. Plain‑text rendering (includes titles/footnotes)
txt_path <- file.path(out_dir, "l_sae.txt")
title_lines <- c(
  "Sponsor Placeholder",                     # title1 (left)
  "Protocol: [Protocol].",                   # title2 (left)
  "",                                         # title3 spacer
  "Listing 16.2.7.2",                         # title4 (center) – matches SAS output
  "Listing of Serious Adverse Events",        # title5 (center)
  "Safety Analysis Set",                     # title6 (center)
  ""                                          # blank line before table
)

# Header line – mimic PROC REPORT split='~' by using spaces/tabs
header_line <- str_c(
  "Treatment", "SubjectID", "Age/Sex/Race", "SOC", "Preferred Term",
  "Start Date", "End Date", "Study Day", "Grade", "Relation-ship",
  "Action Taken", "Outcome", "SAE Criteria",
  sep = "\t"
)

# Body lines
body_lines <- apply(sae_report, 1, function(row) str_c(row, collapse = "\t"))

# Combine and write
writeLines(c(title_lines, header_line, body_lines), con = txt_path, useBytes = TRUE)

# -----------------------------------------------------------------
# 7. Clean‑up (mirrors PROC DATASETS DELETE in SAS)
# -----------------------------------------------------------------
rm(adae, adsl, adsl_saff, adae_sae, joined, sae_report)

# End of script
