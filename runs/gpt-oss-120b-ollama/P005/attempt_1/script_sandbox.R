# ------------------------------------------------------------
# R translation of adam_adlb.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(lubridate)
library(stringr)

# Output directory (set by the environment)
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Read source data
# ------------------------------------------------------------------
adsl_path <- "/bench/data/derived/c2c/data/adam/adsl.sas7bdat"
lb_path   <- "/bench/data/derived/c2c/data/sdtm/lb.sas7bdat"

adsl <- read_sas(adsl_path) %>% 
  rename_with(toupper)   # ensure uppercase variable names

lb   <- read_sas(lb_path) %>% 
  rename_with(toupper)

# ------------------------------------------------------------------
# 2. Sort input tables (SAS PROC SORT equivalent)
# ------------------------------------------------------------------
adsl_sorted <- adsl %>% arrange(USUBJID)
lb_sorted   <- lb %>% arrange(USUBJID, LBDTC, LBTESTCD)

# ------------------------------------------------------------------
# 3. Create ADLB draft (merge ADSL and LB, keep only LB records)
# ------------------------------------------------------------------
adlb_draft <- lb_sorted %>%
  left_join(adsl_sorted, by = "USUBJID") %>%          # merge by USUBJID
  filter(!is.na(LBSEQ)) %>%                         # keep only records that have lab tests (b flag)
  mutate(
    # Core analysis variables
    PARAMCD = LBTESTCD,
    PARAM   = LBTEST,
    AVAL    = LBSTRESN,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    SRCDOM  = "LB",
    SRCVAR  = "LBSEQ",
    SRCSEQ  = LBSEQ,
    # Convert ISO date string to SAS date (numeric days)
    ASTDT   = ifelse(str_length(LBDTC) >= 10,
                     as.Date(substr(LBDTC, 1, 10), "%Y-%m-%d"),
                     as.Date(NA)),
    # Analysis Relative Day (ASTDY)
    ASTDY   = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
                     ifelse(ASTDT >= TRTSDT,
                            as.numeric(ASTDT - TRTSDT) + 1,
                            as.numeric(ASTDT - TRTSDT)),
                     as.numeric(NA))
  ) %>%
  select(STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT,
         PARAMCD, PARAM, AVAL,
         AVISIT, AVISITN, ASTDT, ASTDY,
         LBSTRESU, SRCDOM, SRCVAR, SRCSEQ)

# ------------------------------------------------------------------
# 4. Identify baseline records
# ------------------------------------------------------------------
adlb_base_candidates <- adlb_draft %>%
  filter(!is.na(AVAL) & !is.na(ASTDT) & ASTDT <= TRTSDT) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

base_flags <- adlb_base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  slice_tail(n = 1) %>%               # last observation per USUBJID-PARAMCD
  ungroup() %>%
  transmute(
    USUBJID,
    PARAMCD,
    BASE     = AVAL,
    BASE_DT  = ASTDT,
    BASE_SEQ = SRCSEQ
  )

# ------------------------------------------------------------------
# 5. Merge baseline back and calculate flags / change
# ------------------------------------------------------------------
adlb <- adlb_draft %>%
  left_join(base_flags, by = c("USUBJID", "PARAMCD")) %>%
  mutate(
    # Flag the exact source record
    ABLFL = ifelse(!is.na(BASE_SEQ) & SRCSEQ == BASE_SEQ, "Y", ""),
    # Change from baseline
    CHG  = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, as.numeric(NA)),
    # Abnormality indicator (LBNRIND)
    LBNRIND = case_when(
      !is.na(AVAL) & PARAMCD == "GLUC" ~ {
        val <- ifelse(toupper(LBSTRESU) == "MG/DL", AVAL * 0.0555, AVAL)
        if (val < 3.9) "LOW" else if (val > 5.6) "HIGH" else "NORMAL"
      },
      !is.na(AVAL) & PARAMCD == "HGB" ~ {
        val <- ifelse(toupper(LBSTRESU) == "G/DL", AVAL * 10, AVAL)
        if (val < 120) "LOW" else if (val > 175) "HIGH" else "NORMAL"
      },
      !is.na(AVAL) ~ "UNKNOWN",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-BASE_DT, -BASE_SEQ) %>%   # drop temporary columns as in SAS DROP statement
  arrange(USUBJID, PARAMCD, ASTDT)

# ------------------------------------------------------------------
# 6. Write ADLB dataset
# ------------------------------------------------------------------
write.csv(
  adlb,
  file = file.path(out_dir, "adlb.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 7. Create audit listing (first 15 observations)
# ------------------------------------------------------------------
audit_listing <- adlb %>%
  select(USUBJID, PARAMCD, AVISIT, ASTDT, AVAL,
         LBSTRESU, BASE, CHG, ABLFL, LBNRIND) %>%
  slice_head(n = 15)

# Text rendering (tab‑delimited, similar to PROC PRINT)
write.table(
  audit_listing,
  file = file.path(out_dir, "P005.lst"),
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  na = "",
  quote = FALSE
)

# CSV version of the listing
write.csv(
  audit_listing,
  file = file.path(out_dir, "P005.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
