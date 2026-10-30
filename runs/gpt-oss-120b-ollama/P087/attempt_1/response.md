```r
# P087_advs.R
# Translation of SAS program P087_advs.sas to R
# Generates datasets: adsl, advs_base, advs
# Generates listing: P087.txt (plain text) and P087.csv (table body)

library(dplyr)
library(lubridate)
library(readr)

out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

#-----------------------------
# STEP 1: Create mock VS data
#-----------------------------
vs_raw <- read_delim(
  file = I("
STUDY01|STUDY01-1001|1|SYSBP|Systolic Blood Pressure|128|mmHg|128|mmHg|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|DIABP|Diastolic Blood Pressure|82|mmHg|82|mmHg|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|3|PULSE|Pulse Rate|72|beats/min|72|beats/min|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|4|SYSBP|Systolic Blood Pressure|122|mmHg|122|mmHg|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|5|DIABP|Diastolic Blood Pressure|78|mmHg|78|mmHg|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|6|PULSE|Pulse Rate|70|beats/min|70|beats/min|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|7|SYSBP|Systolic Blood Pressure|118|mmHg|118|mmHg|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|8|DIABP|Diastolic Blood Pressure|76|mmHg|76|mmHg|Week 8|3|2024-03-11
STUDY01|STUDY01-1002|1|SYSBP|Systolic Blood Pressure|140|mmHg|140|mmHg|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|DIABP|Diastolic Blood Pressure|90|mmHg|90|mmHg|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|3|SYSBP|Systolic Blood Pressure|138|mmHg|138|mmHg|Week 4|2|2024-02-15
STUDY01|STUDY01-1002|4|DIABP|Diastolic Blood Pressure|88|mmHg|88|mmHg|Week 4|2|2024-02-15
STUDY01|STUDY01-1003|1|SYSBP|Systolic Blood Pressure|120|mmHg|120|mmHg|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|DIABP|Diastolic Blood Pressure|75|mmHg|75|mmHg|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|3|SYSBP|Systolic Blood Pressure|115|mmHg|115|mmHg|Week 4|2|2024-02-17
STUDY01|STUDY01-1004|1|SYSBP|Systolic Blood Pressure|132|mmHg|132|mmHg|Baseline|1|2024-01-25
STUDY01|STUDY01-1005|1|SYSBP|Systolic Blood Pressure|118|mmHg|118|mmHg|Baseline|1|2024-01-28
STUDY01|STUDY01-1006|1|SYSBP|Systolic Blood Pressure|145|mmHg|145|mmHg|Baseline|1|2024-01-30
", 
    locale = locale(encoding = "UTF-8")
  ),
  delim = "|",
  col_names = c(
    "STUDYID","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES",
    "VSORRESU","VSSTRESN","VSSTRESU","VISIT","VISITNUM","VSDTC"
  ),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    VSSEQ = as.integer(VSSEQ),
    VSSTRESN = as.numeric(VSSTRESN),
    VISITNUM = as.integer(VISITNUM),
    VSDTC = as.character(VSDTC)
  )

#-----------------------------
# STEP 2: Create mock ADSL data
#-----------------------------
adsl_raw <- read_delim(
  file = I("
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024
", 
    locale = locale(encoding = "UTF-8")
  ),
  delim = "|",
  col_names = c(
    "STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL",
    "TRTSDT_raw","TRTEDT_raw"
  ),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    TRTPN = as.integer(TRTPN),
    AGE = as.integer(AGE),
    TRTSDT = dmy(TRTSDT_raw),
    TRTEDT = dmy(TRTEDT_raw)
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

#-----------------------------
# STEP 3: Merge to create advs_base
#-----------------------------
advs_base <- vs_raw %>%
  left_join(adsl_raw, by = c("STUDYID", "USUBJID")) %>%
  # keep only records present in both (inner join)
  filter(!is.na(TRTSDT)) %>%
  mutate(
    PARAMCD = VSTESTCD,
    PARAM   = VSTEST,
    PARCAT1 = "VITAL SIGNS",
    PARAMN = case_when(
      PARAMCD == "SYSBP" ~ 1L,
      PARAMCD == "DIABP" ~ 2L,
      PARAMCD == "PULSE" ~ 3L,
      PARAMCD == "TEMP"  ~ 4L,
      PARAMCD == "RESP"  ~ 5L,
      PARAMCD == "WEIGHT"~ 6L,
      PARAMCD == "HEIGHT"~ 7L,
      TRUE               ~ 99L
    ),
    AVAL  = VSSTRESN,
    AVALU = VSSTRESU,
    ADT   = ymd(VSDTC),
    ADY   = if_else(!is.na(ADT) & !is.na(TRTSDT),
                    if_else(ADT >= TRTSDT, ADT - TRTSDT + 1, ADT - TRTSDT),
                    as.integer(NA)),
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    ATPT    = "",
    ATPTN   = NA_integer_
  ) %>%
  select(
    STUDYID, USUBJID, PARAMCD, PARAM, PARAMN, PARCAT1,
    AVAL, AVALU, ADT, ADY, AVISIT, AVISITN, ATPT, ATPTN,
    TRTP, TRTPN, TRTSDT, TRTEDT, SAFFL
  )

# write advs_base dataset
write.csv(advs_base, file.path(out_dir, "advs_base.csv"), row.names = FALSE, na = "")

#-----------------------------
# STEP 4: Derive baseline flag (ABLFL)
#-----------------------------
advs_base <- advs_base %>%
  arrange(USUBJID, PARAMCD, ADT) %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    _pre_trt = if_else(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, 1L, 0L),
    max_pre_adt = if (any(_pre_trt == 1L)) max(ADT[_pre_trt == 1L], na.rm = TRUE) else as.Date(NA),
    ABLFL = if_else(!is.na(ADT) & _pre_trt == 1L & ADT == max_pre_adt, "Y", "")
  ) %>%
  ungroup() %>%
  select(-_pre_trt, -max_pre_adt)

# write updated advs_base (overwrites previous file to include ABLFL)
write.csv(advs_base, file.path(out_dir, "advs_base.csv"), row.names = FALSE, na = "")

#-----------------------------
# STEP 5: Derive BASE, CHG, PCHG, ANL01FL
#-----------------------------
baseline_vals <- advs_base %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, PARAMCD, BASE = AVAL)

advs <- advs_base %>%
  left_join(baseline_vals, by = c("USUBJID", "PARAMCD")) %>%
  mutate(
    CHG  = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = if_else(!is.na(CHG) & BASE != 0, (CHG / BASE) * 100, NA_real_),
    ANL01FL = if_else(ABLFL != "Y" & !is.na(AVAL) & ADT > TRTSDT, "Y", "")
  )

# write advs dataset (pre‑maximum record)
write.csv(advs, file.path(out_dir, "advs.csv"), row.names = FALSE, na = "")

#-----------------------------
# STEP 6: Add maximum post‑baseline derived record
#-----------------------------
max_post <- advs %>%
  filter(ABLFL != "Y", ADT > TRTSDT) %>%
  group_by(STUDYID, USUBJID, PARAMCD) %>%
  summarise(
    AVAL = max(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(baseline_vals, by = c("USUBJID", "PARAMCD")) %>%
  mutate(
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    DTYPE = "MAXIMUM",
    AVISIT = "Maximum Post-Baseline",
    AVISITN = 99L
  ) %>%
  # bring across other needed columns (they will be NA where not defined)
  left_join(
    advs %>%
      select(STUDYID, USUBJID, PARAMCD, PARAM, PARAMN, PARCAT1,
             AVALU, ADT, ADY, ATPT, ATPTN, TRTP, TRTPN, TRTSDT, TRTEDT, SAFFL) %>%
      distinct(),
    by = c("STUDYID", "USUBJID", "PARAMCD")
  ) %>%
  select(
    STUDYID, USUBJID, PARAMCD, PARAM, PARAMN, PARCAT1,
    AVAL, AVALU, BASE, CHG, PCHG = NA_real_, ADT, ADY,
    AVISIT, AVISITN, ABLFL = "", ANL01FL = "", DTYPE,
    ATPT, ATPTN, TRTP, TRTPN, TRTSDT, TRTEDT, SAFFL
  )

# combine original advs with derived maximum record
advs <- bind_rows(advs, max_post)

#-----------------------------
# STEP 7: Add ASEQ sequence number
#-----------------------------
advs <- advs %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number() - 1L) %>%
  ungroup()

# final write of advs (includes maximum record)
write.csv(advs, file.path(out_dir, "advs.csv"), row.names = FALSE, na = "")

#-----------------------------
# STEP 8: Write ADSL dataset (already read)
#-----------------------------
write.csv(adsl_raw, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")

#-----------------------------
# STEP 9: Create listing (first 20 rows) – P087.lst equivalent
#-----------------------------
listing_tbl <- advs %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>%
  slice_head(n = 20)

# plain‑text rendering (space‑separated, similar to PROC PRINT)
txt_path <- file.path(out_dir, "P087.txt")
write.table(
  listing_tbl,
  file = txt_path,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# CSV version of the same table body
write.csv(listing_tbl, file.path(out_dir, "P087.csv"), row.names = FALSE, na = "")

# End of script
```