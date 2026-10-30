# ------------------------------------------------------------
# R translation of SAS program P076_l_ae.sas
# Produces listing l_ae as CSV and plain‑text files
# ------------------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(haven)   # read SAS datasets
library(dplyr)   # data manipulation
library(stringr) # string handling
library(readr)   # write CSV files

# ------------------------------------------------------------------
# Helper: SAS‑style PROPCASE (first letter uppercase, rest lowercase)
# ------------------------------------------------------------------
propcase <- function(x) {
  ifelse(is.na(x), NA_character_, str_to_title(tolower(x)))
}

# ------------------------------------------------------------------
# Output directory (provided by the harness via OUT_DIR)
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# Read ADaM datasets (adam.adae and adam.adsl)
# ------------------------------------------------------------------
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

adae_file <- file.path(adam_path, "adae.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

adae <- read_sas(adae_file)   # column names become lower‑case
adsl <- read_sas(adsl_file)

# ------------------------------------------------------------------
# Step 1: Prepare listing data (SQL‑like join & filter)
# ------------------------------------------------------------------
ae_list <- adae %>%
  inner_join(adsl, by = "usubjid") %>%                     # join on USUBJID
  filter(saffl == "Y", trtemfl == "Y") %>%               # safety set & treatment‑emergent flag
  transmute(
    USUBJID      = usubjid,
    TRTA         = trta,
    AGE          = age,
    SEX          = sex,
    RACE         = race,
    SOC          = aebodsys,
    PT           = aedecod,
    VERBATIM    = aeterm,
    START_DATE   = aestdtc,
    END_DATE     = aeendtc,
    SEVERITY     = aesev,
    RELATIONSHIP = aerl,
    SERIOUS      = aeser,
    ACTION_TAKEN = aeacn,
    OUTCOME      = aeout,
    CTCAE_GRADE  = aetoxgr,
    TRTEMFL      = trtemfl
  ) %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# ------------------------------------------------------------------
# Step 2: Format display values (DATA‑step equivalents)
# ------------------------------------------------------------------
ae_report <- ae_list %>%
  mutate(
    severity_d = propcase(SEVERITY),
    rel_d      = propcase(RELATIONSHIP),
    serious_d  = ifelse(SERIOUS == "Y", "Yes", "No"),
    action_d   = case_when(
      toupper(ACTION_TAKEN) == "DRUG WITHDRAWN"   ~ "Drug Withdrawn",
      toupper(ACTION_TAKEN) == "DOSE REDUCED"     ~ "Dose Reduced",
      toupper(ACTION_TAKEN) == "DRUG INTERRUPTED"~ "Drug Interrupted",
      toupper(ACTION_TAKEN) == "DOSE NOT CHANGED"~ "Dose Not Changed",
      toupper(ACTION_TAKEN) == "NOT APPLICABLE"  ~ "Not Applicable",
      TRUE                                        ~ propcase(ACTION_TAKEN)
    ),
    outcome_d = case_when(
      toupper(OUTCOME) == "RECOVERED/RESOLVED"                          ~ "Resolved",
      toupper(OUTCOME) == "RECOVERING/RESOLVING"                        ~ "Resolving",
      toupper(OUTCOME) == "NOT RECOVERED/NOT RESOLVED"                 ~ "Not Resolved",
      toupper(OUTCOME) == "RECOVERED/RESOLVED WITH SEQUELAE"           ~ "Resolved w/ Sequelae",
      toupper(OUTCOME) == "FATAL"                                      ~ "Fatal",
      TRUE                                                             ~ propcase(OUTCOME)
    )
  ) %>%
  # Keep only columns required for the final listing, in the exact order SAS uses
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
# Step 3: Write outputs (CSV + plain‑text rendering)
# ------------------------------------------------------------------
# CSV file (dataset body)
csv_path <- file.path(out_dir, "l_ae.csv")
write_csv(ae_report, csv_path, na = "")

# Plain‑text file (tab‑delimited, mimicking a listing)
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
# Clean‑up (mirrors PROC DATASETS DELETE in SAS)
# ------------------------------------------------------------------
rm(adae, adsl, ae_list, ae_report)

# End of script
