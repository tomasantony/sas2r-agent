```r
# ------------------------------------------------------------
# R translation of P010_sdtm_cm.sas
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(haven)

# ------------------------------------------------------------------
# 0. Environment & paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Project root as defined by the original SAS macro variables
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"

# ------------------------------------------------------------------
# 1. IMPORT RAW CONMEDS CSV (raw_conmeds.csv)
# ------------------------------------------------------------------
raw_cm_path <- file.path(project_path, "data", "raw", "raw_conmeds.csv")
raw_cm <- read_csv(
  raw_cm_path,
  col_names = c("SUBJECT", "MEDICATION", "DOSE", "START_DT", "END_DT", "ONGOING"),
  col_types = cols(
    SUBJECT    = col_character(),
    MEDICATION = col_character(),
    DOSE       = col_character(),
    START_DT   = col_character(),
    END_DT     = col_character(),
    ONGOING    = col_character()
  ),
  na = c("", "NA")
)

# ------------------------------------------------------------------
# 2. BUILD CM DRAFT (equivalent to work.cm_draft)
# ------------------------------------------------------------------
cm_draft <- raw_cm %>%
  mutate(
    # Identifiers
    STUDYID = "CDISC-01",
    DOMAIN  = "CM",
    USUBJID = paste0(STUDYID, "-", str_trim(SUBJECT)),

    # Medication name handling
    CMTRT   = str_trim(MEDICATION),
    CMDECOD = if_else(toupper(CMTRT) == "PARACETAMOL",
                      "ACETAMINOPHEN",
                      toupper(CMTRT)),

    # Dose parsing (first token numeric, second token unit)
    CMDOSE  = as.numeric(str_trim(word(DOSE, 1, sep = " "))),
    CMDOSU  = str_trim(word(DOSE, 2, sep = " ")),

    # Date conversion from DDMMYY10. to ISO 8601 (YYYY‑MM‑DD)
    CMSTDTC = if_else(!is.na(START_DT) & START_DT != "",
                      format(dmy(START_DT), "%Y-%m-%d"),
                      ""),
    CMENDTC = if_else(!is.na(END_DT) & END_DT != "",
                      format(dmy(END_DT), "%Y-%m-%d"),
                      ""),

    # Ongoing flag
    CMENRTPT = if_else(ONGOING == "Y", "ONGOING", "")
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, CMTRT, CMDECOD,
    CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT
  )

# ------------------------------------------------------------------
# 3. SORT cm_draft (by USUBJID, CMSTDTC, CMTRT) – SAS missing values first
# ------------------------------------------------------------------
cm_draft <- cm_draft %>%
  arrange(
    USUBJID,
    is.na(CMSTDTC),   # missing dates first (SAS behaviour)
    CMSTDTC,
    CMTRT
  )

# ------------------------------------------------------------------
# 4. READ DM anchor (RFENDTC) and prepare _CMENTPT_ANCHOR
# ------------------------------------------------------------------
dm_path <- file.path(project_path, "data", "sdtm", "dm.sas7bdat")
dm <- read_sas(dm_path) %>%
  select(USUBJID, RFENDTC) %>%
  arrange(USUBJID)

dm_cm_anchor <- dm %>%
  rename(_CMENTPT_ANCHOR = RFENDTC)

# ------------------------------------------------------------------
# 5. MERGE, compute CMENTPT and CMSEQ (final SDTM.CM)
# ------------------------------------------------------------------
cm_final <- cm_draft %>%
  left_join(dm_cm_anchor, by = "USUBJID") %>%
  mutate(
    CMENTPT = if_else(CMENRTPT == "ONGOING",
                     coalesce(_CMENTPT_ANCHOR, ""),
                     ""),
    # Log SAS‑style error if ongoing but no DM end date
    CMENTPT = if_else(CMENRTPT == "ONGOING" &
                        (is.na(_CMENTPT_ANCHOR) | _CMENTPT_ANCHOR == ""),
                      {
                        message(paste0("ERROR: Ongoing CM record has no DM reference end: USUBJID=", USUBJID))
                        ""
                      },
                      CMENTPT)
  ) %>%
  group_by(USUBJID) %>%
  mutate(CMSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, CMSEQ, CMTRT, CMDECOD,
    CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT, CMENTPT
  )

# ------------------------------------------------------------------
# 6. WRITE OUTPUT DATASET (sdtm.cm)
# ------------------------------------------------------------------
cm_csv_path <- file.path(out_dir, "cm.csv")
write.csv(cm_final, cm_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 7. LISTING – first 15 rows (PROC PRINT analogue)
# ------------------------------------------------------------------
listing_df <- head(cm_final, 15)

# Plain‑text rendering (tab‑delimited, no quotes)
listing_txt_path <- file.path(out_dir, "P010.txt")
write.table(
  listing_df,
  file = listing_txt_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

# CSV version of the same listing (for completeness)
listing_csv_path <- file.path(out_dir, "P010.csv")
write.csv(listing_df, listing_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
```