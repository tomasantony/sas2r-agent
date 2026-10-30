```r
# ------------------------------------------------------------
# R translation of P008_adam_advs.sas  (attempt 3)
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(stringr)

# ------------------------------------------------------------
# 1. Define paths
# ------------------------------------------------------------
# Macro values from the SAS environment
root_dir   <- "D:/meta_hackathon/sas2r-agent"
project_path <- file.path(root_dir, "data/derived/c2c")   # &project_path.
out_dir    <- Sys.getenv("OUT_DIR")                      # directory for all outputs

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Input dataset locations (as defined by the libnames in the SAS program)
adsl_path <- file.path(project_path, "data/adam", "adsl.sas7bdat")
vs_path   <- file.path(project_path, "data/sdtm", "vs.sas7bdat")

# ------------------------------------------------------------
# 2. Read source data
# ------------------------------------------------------------
adsl <- read_sas(adsl_path)
vs   <- read_sas(vs_path)

# ------------------------------------------------------------
# 3. Create ADVS draft (merge ADSL and VS)
# ------------------------------------------------------------
# Sort datasets as SAS would
adsl_sorted <- adsl %>% arrange(USUBJID)
vs_sorted   <- vs   %>% arrange(USUBJID, VSDTC, VSTESTCD)

# Inner join on USUBJID (keep only VS records)
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
  # Keep only the variables required for later steps.
  # Use any_of() so missing columns (e.g., STUDYID) do not cause an error.
  select(
    any_of(c("STUDYID", "USUBJID", "TRT01A", "TRTSDT", "TRTEDT",
             "PARAMCD", "PARAM", "AVAL",
             "AVISIT", "AVISITN", "ASTDT", "ASTDY",
             "SRCDOM", "SRCVAR", "SRCSEQ"))
  )

# ------------------------------------------------------------
# 4. Identify baseline records
# ------------------------------------------------------------
base_candidates <- advs_draft %>%
  filter(!is.na(AVAL) & !is.na(ASTDT) & ASTDT <= TRTSDT) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

# For each USUBJID & PARAMCD keep the last (i.e., most recent) record
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
# 5. Merge baseline back and compute flags / change
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
# 6. Write ADVS dataset
# ------------------------------------------------------------
adv_csv_path <- file.path(out_dir, "advs.csv")
write.csv(advs, adv_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# 7. Visual audit listing (first 15 rows)
# ------------------------------------------------------------
audit_tbl <- advs %>%
  select(USUBJID, PARAMCD, AVISIT, ASTDT, TRTSDT, AVAL, BASE, CHG, ABLFL) %>%
  slice_head(n = 15)

# Write a plain‑text listing (SAS PROC PRINT style)
audit_lst_path <- file.path(out_dir, "P008.lst")
write.table(audit_tbl,
            file = audit_lst_path,
            sep = " ",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            na = "")

# Also write a CSV version of the same table (useful for automated checks)
audit_csv_path <- file.path(out_dir, "P008.csv")
write.csv(audit_tbl, audit_csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```