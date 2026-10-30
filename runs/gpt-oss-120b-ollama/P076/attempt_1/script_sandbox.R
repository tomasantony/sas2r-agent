# ------------------------------------------------------------
# R translation of SAS program P076_l_ae.sas
# Produces listing l_ae as CSV and plain‑text files
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(stringr)   # string handling
library(tidyr)      # for possible reshaping
library(readr)      # for writing CSVs

# ------------------------------------------------------------------
# Helper functions to mimic SAS PROPCASE and IFc behavior
# ------------------------------------------------------------------
propcase <- function(x) {
  # Convert to proper case (first letter of each word uppercase, rest lowercase)
  # Handles NA safely
  ifelse(is.na(x), NA_character_, str_to_title(tolower(x)))
}

# ------------------------------------------------------------------
# Set up output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# Read ADaM datasets (adam.adae and adam.adsl)
# ------------------------------------------------------------------
adam_path <- "/bench/data/raw/own-synthetic/adam"

adae_file <- file.path(adam_path, "adae.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

adae <- read_sas(adae_file)
adsl <- read_sas(adsl_file)

# ------------------------------------------------------------------
# Step 1: Prepare listing data (SQL equivalent)
# ------------------------------------------------------------------
ae_list <- adae %>%
  inner_join(adsl, by = "USUBJID") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y") %>%
  select(
    USUBJID,
    TRTA,
    AGE,
    SEX,
    RACE,
    SOC = AEBODSYS,
    PT = AEDECOD,
    VERBATIM = AETERM,
    START_DATE = AESTDTC,
    END_DATE = AEENDTC,
    SEVERITY = AESEV,
    RELATIONSHIP = AEREL,
    SERIOUS = AESER,
    ACTION_TAKEN = AEACN,
    OUTCOME = AEOUT,
    CTCAE_GRADE = AETOXGR,
    TRTEMFL
  ) %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# ------------------------------------------------------------------
# Step 2: Format display values (DATA step equivalent)
# ------------------------------------------------------------------
ae_report <- ae_list %>%
  mutate(
    severity_d = propcase(SEVERITY),
    rel_d      = propcase(RELATIONSHIP),
    serious_d  = ifelse(SERIOUS == "Y", "Yes", "No"),
    action_d   = case_when(
      toupper(ACTION_TAKEN) == "DRUG WITHDRAWN"          ~ "Drug Withdrawn",
      toupper(ACTION_TAKEN) == "DOSE REDUCED"            ~ "Dose Reduced",
      toupper(ACTION_TAKEN) == "DRUG INTERRUPTED"       ~ "Drug Interrupted",
      toupper(ACTION_TAKEN) == "DOSE NOT CHANGED"       ~ "Dose Not Changed",
      toupper(ACTION_TAKEN) == "NOT APPLICABLE"         ~ "Not Applicable",
      TRUE                                              ~ propcase(ACTION_TAKEN)
    ),
    outcome_d = case_when(
      toupper(OUTCOME) == "RECOVERED/RESOLVED"                         ~ "Resolved",
      toupper(OUTCOME) == "RECOVERING/RESOLVING"                       ~ "Resolving",
      toupper(OUTCOME) == "NOT RECOVERED/NOT RESOLVED"                ~ "Not Resolved",
      toupper(OUTCOME) == "RECOVERED/RESOLVED WITH SEQUELAE"          ~ "Resolved w/ Sequelae",
      toupper(OUTCOME) == "FATAL"                                      ~ "Fatal",
      TRUE                                                            ~ propcase(OUTCOME)
    )
  ) %>%
  # Keep only the columns needed for the final listing, in the required order
  select(
    TRTA,
    USUBJID,
    SOC,
    PT,
    VERBATIM,
    START_DATE,
    END_DATE,
    CTCAE_GRADE,
    severity_d,
    serious_d,
    rel_d,
    action_d,
    outcome_d
  )

# ------------------------------------------------------------------
# Step 3: Write outputs
# ------------------------------------------------------------------
# CSV of the listing (dataset body)
csv_path <- file.path(out_dir, "l_ae.csv")
write_csv(ae_report, csv_path, na = "")

# Plain‑text rendering of the listing (tab‑delimited)
txt_path <- file.path(out_dir, "l_ae.txt")
write.table(
  ae_report,
  file = txt_path,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# Clean‑up (not strictly necessary in R, but mirrors SAS PROC DATASETS)
# ------------------------------------------------------------------
rm(adae, adsl, ae_list, ae_report)

# End of script
