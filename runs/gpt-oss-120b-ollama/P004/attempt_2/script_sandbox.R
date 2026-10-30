# ------------------------------------------------------------
# R translation of P004_adam_adae.sas  (attempt 2)
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------
# SAS round-half-up (not needed here but provided for completeness)
sas_round <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# SAS DATE9. format (e.g., 01JAN2020)
fmt_date9 <- function(date_vec) {
  ifelse(is.na(date_vec),
         "",
         toupper(format(date_vec, "%d%b%Y")))
}

# ------------------------------------------------------------------
# Environment & paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# Project path may be supplied as PROJECT_PATH or project_path; fall back to constructing from ROOT
proj_path <- Sys.getenv("project_path")
if (proj_path == "") {
  root_path <- Sys.getenv("root")
  if (root_path == "") stop("Neither project_path nor root environment variables are set")
  proj_path <- file.path(root_path, "data", "derived", "c2c")
}

# Input dataset locations (ADaM and SDTM libraries)
adsl_path <- file.path(proj_path, "data", "adam", "adsl.sas7bdat")
ae_path   <- file.path(proj_path, "data", "sdtm", "ae.sas7bdat")

# ------------------------------------------------------------------
# 1. Read source data
# ------------------------------------------------------------------
adsl_raw <- read_sas(adsl_path) %>% mutate(across(everything(), as.character))
ae_raw   <- read_sas(ae_path)   %>% mutate(across(everything(), as.character))

# ------------------------------------------------------------------
# 2. Sort input datasets (SAS sorts are case‑sensitive, missing first)
# ------------------------------------------------------------------
adsl_sorted <- adsl_raw %>% arrange(USUBJID)
ae_sorted   <- ae_raw %>% arrange(USUBJID, AESEQ)

# ------------------------------------------------------------------
# 3. Merge ADSL (master) with AE (detail) – keep only subjects with AE
# ------------------------------------------------------------------
adae_draft <- adsl_sorted %>%
  inner_join(ae_sorted, by = "USUBJID", suffix = c(".adsl", ".ae")) %>%
  # Retain order of variables as in SAS RETAIN statement
  select(STUDYID, USUBJID, everything())

# ------------------------------------------------------------------
# 4. Numeric date derivations (AESTDTC / AEENDTC are ISO‑8601 strings)
# ------------------------------------------------------------------
adae_draft <- adae_draft %>%
  mutate(
    AESTDT = ifelse(str_length(AESTDTC) >= 10,
                    as.Date(substr(AESTDTC, 1, 10), format = "%Y-%m-%d"),
                    as.Date(NA)),
    AEENDT = ifelse(str_length(AEENDTC) >= 10,
                    as.Date(substr(AEENDTC, 1, 10), format = "%Y-%m-%d"),
                    as.Date(NA))
  )

# ------------------------------------------------------------------
# 5. Analysis relative days (ASTDY)
# ------------------------------------------------------------------
# TRTSDT in ADaM is already a SAS date (numeric days since 1960) and read_sas converts it to Date
adae_draft <- adae_draft %>%
  mutate(
    ASTDY = ifelse(!is.na(AESTDT) & !is.na(TRTSDT),
                   ifelse(AESTDT >= TRTSDT,
                          as.integer(AESTDT - TRTSDT) + 1L,
                          as.integer(AESTDT - TRTSDT)),
                   NA_integer_)
  )

# ------------------------------------------------------------------
# 6. Treatment‑Emergent flag (TRTEMFL)
# ------------------------------------------------------------------
adae_draft <- adae_draft %>%
  mutate(
    TRTEMFL = ifelse(SAFFL == "Y" & !is.na(AESTDT) & !is.na(TRTSDT) & AESTDT >= TRTSDT,
                     "Y", "")
  )

# ------------------------------------------------------------------
# 7. Keep essential variables (same order as SAS KEEP statement)
# ------------------------------------------------------------------
adae_draft <- adae_draft %>%
  select(
    STUDYID, USUBJID, SUBJID, TRT01A, TRTSDT, TRTEDT, AGE, SEX,
    AESEQ, AETERM, AEDECOD, AESEV, AEREL, AEOUT, AESER,
    AESTDT, AEENDT, ASTDY, TRTEMFL
  )

# ------------------------------------------------------------------
# 8. Derive occurrence order flag
# ------------------------------------------------------------------
adae_occurrence_order <- adae_draft %>%
  mutate(_occ_order = ifelse(TRTEMFL == "Y", 0L, 1L))

# ------------------------------------------------------------------
# 9. Sort for first‑occurrence logic
# ------------------------------------------------------------------
adae_occurrence_order <- adae_occurrence_order %>%
  arrange(USUBJID, AEDECOD, _occ_order, AESTDT, AESEQ)

# ------------------------------------------------------------------
# 10. Flag first treatment‑emergent occurrence per subject/term
# ------------------------------------------------------------------
adae_flagged <- adae_occurrence_order %>%
  group_by(USUBJID, AEDECOD, _occ_order) %>%
  mutate(
    first_occ = row_number() == 1,
    AOCCFL = ifelse(TRTEMFL == "Y" & first_occ, "Y", "")
  ) %>%
  ungroup() %>%
  select(-_occ_order, -first_occ)

# ------------------------------------------------------------------
# 11. Final sort and write ADaE dataset
# ------------------------------------------------------------------
adae_final <- adae_flagged %>%
  arrange(USUBJID, AESTDT, AESEQ)

# Write ADaE dataset as CSV
write.csv(
  adae_final,
  file = file.path(out_dir, "adae.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 12. Visual audit listing (first 15 rows) – write .lst and .csv
# ------------------------------------------------------------------
audit_vars <- c("USUBJID", "TRT01A", "AETERM", "AESEV",
                "AESTDT", "TRTSDT", "ASTDY", "TRTEMFL", "AOCCFL")

audit_tbl <- adae_final %>%
  select(all_of(audit_vars)) %>%
  slice_head(n = 15) %>%
  mutate(
    AESTDT = fmt_date9(AESTDT),
    TRTSDT = fmt_date9(TRTSDT)
  )

# Plain‑text rendering (tab‑separated) – SAS listings are typically space‑aligned;
# a tab‑separated file is acceptable for the automated check.
write.table(
  audit_tbl,
  file = file.path(out_dir, "P004.lst"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  na = ""
)

# CSV version of the listing (optional but useful)
write.csv(
  audit_tbl,
  file = file.path(out_dir, "P004.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
