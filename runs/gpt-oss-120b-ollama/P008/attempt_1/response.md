```r
# ------------------------------------------------------------
# R translation of P008_adam_advs.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ------------------------------------------------------------
# Setup paths
# ------------------------------------------------------------
project_path <- Sys.getenv("project_path")
out_dir       <- Sys.getenv("OUT_DIR")

# Input datasets (assumed .sas7bdat files with same name as dataset)
adsl_path <- file.path(project_path, "data/adam", "adsl.sas7bdat")
vs_path   <- file.path(project_path, "data/sdtm", "vs.sas7bdat")

# ------------------------------------------------------------
# Read raw data
# ------------------------------------------------------------
adsl <- read_sas(adsl_path)
vs   <- read_sas(vs_path)

# ------------------------------------------------------------
# 1. MERGE ADSL AND VS  (create advs draft)
# ------------------------------------------------------------
# Sort ADSL by USUBJID
adsl_sorted <- adsl %>% arrange(USUBJID)

# Sort VS by USUBJID VSDTC VSTESTCD
vs_sorted <- vs %>% arrange(USUBJID, VSDTC, VSTESTCD)

# Merge (inner join on USUBJID, keep only VS records)
advs_draft <- adsl_sorted %>%
  inner_join(vs_sorted, by = "USUBJID") %>%
  # Core analysis variables
  mutate(
    PARAMCD = VSTESTCD,
    PARAM   = VSTEST,
    AVAL    = VSSTRESN,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    SRCDOM  = "VS",
    SRCVAR  = "VSSEQ",
    SRCSEQ  = VSSEQ,
    # Convert ISO Date to numeric SAS date (Date class)
    ASTDT   = if_else(str_length(VSDTC) >= 10,
                      as.Date(substr(VSDTC, 1, 10), "%Y-%m-%d"),
                      as.Date(NA)),
    # Analysis Relative Day (ASTDY)
    ASTDY   = if_else(!is.na(ASTDT) & !is.na(TRTSDT),
                      if_else(ASTDT >= TRTSDT,
                              as.integer(ASTDT - TRTSDT) + 1L,
                              as.integer(ASTDT - TRTSDT)),
                      as.integer(NA))
  ) %>%
  select(STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT,
         PARAMCD, PARAM, AVAL,
         AVISIT, AVISITN, ASTDT, ASTDY,
         SRCDOM, SRCVAR, SRCSEQ)

# ------------------------------------------------------------
# 2. IDENTIFY BASELINE RECORD
# ------------------------------------------------------------
# Candidates: ASTDT <= TRTSDT and AVAL not missing
base_candidates <- advs_draft %>%
  filter(!is.na(AVAL) & !is.na(ASTDT) & ASTDT <= TRTSDT) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

# For each USUBJID & PARAMCD keep the last (max) record
base_flags <- base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    USUBJID,
    PARAMCD,
    BASE    = AVAL,
    BASE_DT = ASTDT,
    BASE_SEQ = SRCSEQ
  )

# ------------------------------------------------------------
# 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE
# ------------------------------------------------------------
advs <- advs_draft %>%
  left_join(base_flags, by = c("USUBJID", "PARAMCD")) %>%
  mutate(
    ABLFL = if_else(!is.na(BASE_SEQ) & SRCSEQ == BASE_SEQ, "Y", ""),
    CHG   = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, as.numeric(NA))
  ) %>%
  select(-BASE_DT, -BASE_SEQ)

# Final sort
advs <- advs %>% arrange(USUBJID, PARAMCD, ASTDT)

# ------------------------------------------------------------
# 4. OUTPUT DATASET
# ------------------------------------------------------------
adv_csv_path <- file.path(out_dir, "advs.csv")
write.csv(advs, adv_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# 5. VISUAL AUDIT LISTING (first 15 rows)
# ------------------------------------------------------------
audit_tbl <- advs %>%
  select(USUBJID, PARAMCD, AVISIT, ASTDT, TRTSDT, AVAL, BASE, CHG, ABLFL) %>%
  slice_head(n = 15)

# Text rendering (plain‑text file)
audit_txt_path <- file.path(out_dir, "P008.txt")
write.table(audit_tbl,
            file = audit_txt_path,
            sep = " ",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

# CSV of the same table body
audit_csv_path <- file.path(out_dir, "P008.csv")
write.csv(audit_tbl, audit_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```