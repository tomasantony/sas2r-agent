# P085_adlb.R
# Translation of SAS program P085_adlb.sas to R
# Generates ADaM datasets and a listing under the directory specified by OUT_DIR

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

# Helper: round half up (SAS default) - not needed for this program but defined for completeness
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x) * 10^digits
  rounded <- floor(absx + 0.5)
  posneg * rounded / 10^digits
}

# Output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ----------------------------------------------------------------------
# STEP 1: Create mock SDTM LB data
# ----------------------------------------------------------------------
lb_txt <- "
STUDY01|STUDY01-1001|1|ALT|Alanine Aminotransferase|CHEMISTRY|24|U/L|24|U/L|7|56|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|2|AST|Aspartate Aminotransferase|CHEMISTRY|26|U/L|26|U/L|10|40|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|3|CREAT|Creatinine|CHEMISTRY|0.9|mg/dL|0.9|mg/dL|0.7|1.3|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|4|ALT|Alanine Aminotransferase|CHEMISTRY|28|U/L|28|U/L|7|56|Week 4|2|2024-02-12|NORMAL
STUDY01|STUDY01-1001|5|AST|Aspartate Aminotransferase|CHEMISTRY|30|U/L|30|U/L|10|40|Week 4|2|2024-02-12|NORMAL
STUDY01|STUDY01-1001|6|ALT|Alanine Aminotransferase|CHEMISTRY|32|U/L|32|U/L|7|56|Week 8|3|2024-03-11|NORMAL
STUDY01|STUDY01-1002|1|ALT|Alanine Aminotransferase|CHEMISTRY|18|U/L|18|U/L|7|56|Baseline|1|2024-01-18|NORMAL
STUDY01|STUDY01-1002|2|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Week 4|2|2024-02-15|NORMAL
STUDY01|STUDY01-1003|1|ALT|Alanine Aminotransferase|CHEMISTRY|35|U/L|35|U/L|7|56|Baseline|1|2024-01-20|NORMAL
STUDY01|STUDY01-1003|2|ALT|Alanine Aminotransferase|CHEMISTRY|95|U/L|95|U/L|7|56|Week 4|2|2024-02-17|HIGH
STUDY01|STUDY01-1003|3|ALT|Alanine Aminotransferase|CHEMISTRY|180|U/L|180|U/L|7|56|Week 8|3|2024-03-15|HIGH
STUDY01|STUDY01-1004|1|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Baseline|1|2024-01-25|NORMAL
STUDY01|STUDY01-1005|1|ALT|Alanine Aminotransferase|CHEMISTRY|19|U/L|19|U/L|7|56|Baseline|1|2024-01-28|NORMAL
STUDY01|STUDY01-1006|1|ALT|Alanine Aminotransferase|CHEMISTRY|30|U/L|30|U/L|7|56|Baseline|1|2024-01-30|NORMAL
"
lb <- read_delim(
  file = I(str_trim(lb_txt)),
  delim = "|",
  col_names = c(
    "STUDYID", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST", "LBCAT",
    "LBORRES", "LBORRESU", "LBSTRESN", "LBSTRESU", "LBSTNRLO", "LBSTNRHI",
    "VISIT", "VISITNUM", "LBDTC", "LBNRIND"
  ),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    across(c(LBSEQ, LBSTRESN, LBSTNRLO, LBSTNRHI, VISITNUM), as.numeric),
    across(c(LBORRES, LBORRESU, LBSTRESU, LBDTC, LBNRIND, LBTEST, LBTESTCD, LBCAT, VISIT), as.character)
  )

# ----------------------------------------------------------------------
# STEP 2: Mock ADSL
# ----------------------------------------------------------------------
adsl_txt <- "
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024
"
adsl <- read_delim(
  file = I(str_trim(adsl_txt)),
  delim = "|",
  col_names = c(
    "STUDYID", "USUBJID", "TRTP", "TRTPN", "AGE", "SEX", "SAFFL",
    "TRTSDT_raw", "TRTEDT_raw"
  ),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    AGE = as.numeric(AGE),
    TRTSDT = dmy(TRTSDT_raw),
    TRTEDT = dmy(TRTEDT_raw),
    across(c(TRTP, SEX, SAFFL), as.character)
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

# ----------------------------------------------------------------------
# STEP 3: Merge and derive ADLB_BASE variables
# ----------------------------------------------------------------------
adlb_base <- inner_join(lb, adsl, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    # Parameter variables
    PARAMCD = LBTESTCD,
    PARAM = LBTEST,
    PARCAT1 = LBCAT,
    # PARAMN mapping
    PARAMN = case_when(
      PARAMCD == "ALT"   ~ 1,
      PARAMCD == "AST"   ~ 2,
      PARAMCD == "ALP"   ~ 3,
      PARAMCD == "BILI"  ~ 4,
      PARAMCD == "CREAT" ~ 5,
      PARAMCD == "BUN"   ~ 6,
      PARAMCD == "GLUC"  ~ 7,
      PARAMCD == "HGB"   ~ 8,
      PARAMCD == "WBC"   ~ 9,
      PARAMCD == "PLT"   ~ 10,
      TRUE                ~ 99
    ),
    # Analysis value
    AVAL = LBSTRESN,
    AVALU = LBSTRESU,
    # Normal ranges
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    A1LO = ANRLO,
    A1HI = ANRHI,
    # ANRIND
    ANRIND = case_when(
      !is.na(AVAL) & !is.na(ANRLO) & AVAL < ANRLO ~ "LOW",
      !is.na(AVAL) & !is.na(ANRHI) & AVAL > ANRHI ~ "HIGH",
      !is.na(AVAL) & ( !is.na(ANRLO) | !is.na(ANRHI) ) ~ "NORMAL",
      TRUE ~ NA_character_
    ),
    # Analysis date
    ADT = if_else(LBDTC != "", ymd(LBDTC), as.Date(NA)),
    # ADY
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT + 1),
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT  ~ as.numeric(ADT - TRTSDT),
      TRUE                                        ~ NA_real_
    ),
    # Visit
    AVISIT = VISIT,
    AVISITN = VISITNUM
  ) %>%
  select(
    STUDYID, USUBJID, LBSEQ, LBTESTCD, LBTEST, LBCAT,
    everything(),
    -c(LBORRES, LBORRESU, LBSTRESU, LBSTNRLO, LBSTNRHI,
       VISIT, VISITNUM, LBDTC, LBNRIND, LBORRES, LBORRESU,
       LBSTRESN, LBSTRESU, LBSTNRLO, LBSTNRHI)
  ) %>%
  select(-c(LBSEQ, LBTESTCD, LBTEST, LBCAT))

# ----------------------------------------------------------------------
# STEP 4: Derive baseline flag (ABLFL)
# ----------------------------------------------------------------------
adlb_base <- adlb_base %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    # Identify the latest ADT on or before TRTSDT
    max_pre_trt_adt = if (any(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT)) {
      max(ADT[!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    ABLFL = if_else(!is.na(ADT) & !is.na(TRTSDT) & ADT == max_pre_trt_adt, "Y", "")
  ) %>%
  ungroup() %>%
  select(-max_pre_trt_adt)

# ----------------------------------------------------------------------
# STEP 5: Derive BASE, BNRIND, CHG, PCHG, SHIFT, ANL01FL, R2A1HI, BR2A1HI
# ----------------------------------------------------------------------
# Baseline dataset
baseline <- adlb_base %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, PARAMCD, BASE = AVAL, BNRIND = ANRIND)

adlb <- adlb_base %>%
  left_join(baseline, by = c("USUBJID", "PARAMCD")) %>%
  mutate(
    # Change from baseline
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = if_else(!is.na(CHG) & !is.na(BASE) & BASE != 0,
                   (CHG / BASE) * 100, NA_real_),
    # Shift
    SHIFT1 = if_else(!is.na(BNRIND) & !is.na(ANRIND),
                     str_c(BNRIND, " to ", ANRIND), NA_character_),
    # Analysis flag
    ANL01FL = if_else(ABLFL != "Y" & !is.na(AVAL) & !is.na(TRTSDT) & ADT > TRTSDT,
                      "Y", ""),
    # Ratio to ULN
    R2A1HI = if_else(!is.na(AVAL) & !is.na(ANRHI) & ANRHI > 0,
                    AVAL / ANRHI, NA_real_),
    # Baseline ratio to ULN
    BR2A1HI = if_else(!is.na(BASE) & !is.na(A1HI) & A1HI > 0,
                      BASE / A1HI, NA_real_)
  )

# ----------------------------------------------------------------------
# STEP 6: Derive potentially clinically significant (PCS) flags
# ----------------------------------------------------------------------
adlb <- adlb %>%
  mutate(
    ANRIND2 = case_when(
      !is.na(R2A1HI) & R2A1HI > 3 ~ ">3xULN",
      !is.na(R2A1HI) & R2A1HI > 2 ~ ">2xULN",
      ANRIND == "HIGH"               ~ "HIGH",
      ANRIND == "LOW"                ~ "LOW",
      TRUE                           ~ ANRIND
    )
  )

# ----------------------------------------------------------------------
# STEP 7: Add sequence (ASEQ) and final ordering
# ----------------------------------------------------------------------
adlb <- adlb %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number() - 1) %>%
  ungroup()

# ----------------------------------------------------------------------
# Output datasets as CSV
# ----------------------------------------------------------------------
write.csv(adsl, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adlb_base, file.path(out_dir, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adlb, file.path(out_dir, "adlb.csv"), row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# Create listing (PROC PRINT of first 20 rows) and save as text and CSV
# ----------------------------------------------------------------------
listing_df <- adlb %>% slice_head(n = 20)

# Text file (tab‑delimited, similar to PROC PRINT output)
txt_path <- file.path(out_dir, "P085.lst")
write.table(
  listing_df,
  file = txt_path,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same 20‑row table
write.csv(
  listing_df,
  file = file.path(out_dir, "P085.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
