```r
# P085_adlb.R  (attempt 3)
# Translation of SAS program P085_adlb.sas to R
# Generates ADaM datasets and a listing under the directory specified by OUT_DIR

library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ----------------------------------------------------------------------
# Helper: round half up (SAS default) – not used here but kept for completeness
# ----------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ----------------------------------------------------------------------
# Output directory
# ----------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ----------------------------------------------------------------------
# STEP 1 – mock SDTM LB data
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
    across(c(LBORRES, LBORRESU, LBSTRESU, LBDTC, LBNRIND,
             LBTEST, LBTESTCD, LBCAT, VISIT), as.character)
  )

# ----------------------------------------------------------------------
# STEP 2 – mock ADSL
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
# STEP 3 – merge and derive ADLB_BASE (including LBSEQ and AVALC)
# ----------------------------------------------------------------------
adlb_base <- inner_join(lb, adsl, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    # Parameter variables
    PARAMCD = LBTESTCD,
    PARAM   = LBTEST,
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
    # Analysis values
    AVAL  = LBSTRESN,
    AVALU = LBSTRESU,
    AVALC = LBORRES,                     # character result
    # Normal ranges
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    A1LO  = ANRLO,
    A1HI  = ANRHI,
    # ANRIND
    ANRIND = case_when(
      !is.na(AVAL) & !is.na(ANRLO) & AVAL < ANRLO ~ "LOW",
      !is.na(AVAL) & !is.na(ANRHI) & AVAL > ANRHI ~ "HIGH",
      !is.na(AVAL) & ( !is.na(ANRLO) | !is.na(ANRHI) ) ~ "NORMAL",
      TRUE                                      ~ NA_character_
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
    AVISIT  = VISIT,
    AVISITN = VISITNUM
  ) %>%
  # Drop variables that SAS drops
  select(
    -LBTESTCD, -LBTEST, -LBCAT,
    -LBORRES, -LBORRESU, -LBSTRESN, -LBSTRESU,
    -LBSTNRLO, -LBSTNRHI,
    -VISIT, -VISITNUM, -LBDTC, -LBNRIND
  )

# ----------------------------------------------------------------------
# STEP 4 – derive baseline flag (ABLFL) exactly as SAS does
# ----------------------------------------------------------------------
# 4a – flag records on or before TRTSDT
adlb_base <- adlb_base %>%
  mutate(_pre_trt = if_else(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, 1L, 0L))

# 4b – within each subject/parameter, the most recent record (descending ADT)
#       that has _pre_trt = 1 gets ABLFL = 'Y'
adlb_base <- adlb_base %>%
  arrange(USUBJID, PARAMCD, desc(ADT)) %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(ABLFL = if_else(row_number() == 1L & _pre_trt == 1L, "Y", "")) %>%
  ungroup()

# 4c – restore original ascending order
adlb_base <- adlb_base %>%
  arrange(USUBJID, PARAMCD, ADT) %>%
  # Add placeholder columns that exist in the SAS base dataset but are not derived yet
  mutate(
    ANL01FL = "",   # not derived for base dataset
    SHIFT1  = "",   # not derived for base dataset
    BNRIND  = ""    # not derived for base dataset
  )

# ----------------------------------------------------------------------
# STEP 5 – derive BASE, BNRIND, CHG, PCHG, SHIFT, ANL01FL, R2A1HI, BR2A1HI
# ----------------------------------------------------------------------
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
                    str_c(BNRIND, " to ", ANRIND), ""),
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
# STEP 6 – potentially clinically significant flags
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
# STEP 7 – sequence (ASEQ) and final ordering
# ----------------------------------------------------------------------
adlb <- adlb %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number() - 1) %>%
  ungroup()

# ----------------------------------------------------------------------
# Write output datasets
# ----------------------------------------------------------------------
write.csv(adsl, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adlb_base, file.path(out_dir, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adlb, file.path(out_dir, "adlb.csv"), row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# Create listing (first 20 rows) – both text (.lst) and CSV
# ----------------------------------------------------------------------
listing_df <- adlb %>% slice_head(n = 20)

# Text file (tab‑delimited, mimicking PROC PRINT output)
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

# CSV version of the same 20‑row table (optional, not required by spec but kept)
write.csv(
  listing_df,
  file = file.path(out_dir, "P085.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
```