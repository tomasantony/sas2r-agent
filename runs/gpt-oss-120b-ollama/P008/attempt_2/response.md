```r
# ------------------------------------------------------------
# R translation of P008_adam_advs.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(stringr)

# ------------------------------------------------------------
# Setup paths (hard‑coded based on macro values from the SAS run)
# ------------------------------------------------------------
root_dir   <- "D:/meta_hackathon/sas2r-agent"
project_path <- file.path(root_dir, "data/derived/c2c")   # &project_path.
out_dir    <- Sys.getenv("OUT_DIR")                      # where all outputs go

# Input datasets (as defined by the libnames in the SAS program)
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

# Inner merge on USUBJID (keep only VS records)
advs_draft <- adsl_sorted %>%
  inner_join(vs_sorted, by = "USUBJID") %>%
  mutate(
    PARAMCD = VSTESTCD,
    PARAM   = VSTEST,
    AVAL    = VSSTRESN,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    SRCDOM  = "VS",
    SRCVAR  = "VSSEQ",
    SRCSEQ  = VSSEQ,
    # Convert ISO date string (YYYY-MM-DD) to Date class
    ASTDT   = if_else(str_length(VSDTC) >= 10,
                      as.Date(substr(VSDTC, 1, 10), "%Y-%m-%d"),
                      as.Date(NA_real_)),
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
base_candidates <- advs_draft %>%
  filter(!is.na(AVAL) & !is.na(ASTDT) & ASTDT <= TRTSDT) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

# For each USUBJID & PARAMCD keep the last (i.e., highest ASTDT/SRCSEQ) record
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
  select(-BASE_DT, -BASE_SEQ) %>%
  arrange(USUBJID, PARAMCD, ASTDT)

# ------------------------------------------------------------
# 4. OUTPUT ADVS DATASET
# ------------------------------------------------------------
adv_csv_path <- file.path(out_dir, "advs.csv")
write.csv(advs, adv_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# 5. VISUAL AUDIT LISTING (first 15 rows) – produce .lst and .csv
# ------------------------------------------------------------
audit_tbl <- advs %>%
  select(USUBJID, PARAMCD, AVISIT, ASTDT, TRTSDT, AVAL, BASE, CHG, ABLFL) %>%
  slice_head(n = 15)

# Text rendering – name must end with .lst per SAS expectation
audit_lst_path <- file.path(out_dir, "P008.lst")
# Use space‑separated format similar to PROC PRINT default
write.table(audit_tbl,
            file = audit_lst_path,
            sep = " ",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            na = "")

# CSV version of the same table body
audit_csv_path <- file.path(out_dir, "P008.csv")
write.csv(audit_tbl, audit_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```