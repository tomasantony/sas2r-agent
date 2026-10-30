```r
# P086_adtte.R
# Translation of SAS program P086_adtte.sas to R
# Generates ADaM datasets and a listing under OUT_DIR

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."

# ---------- STEP 1: Create mock source data ----------
# ADSL
adsl_txt <- "
STUDY01|STUDY01-1001|Treatment A|1|59|<65|M|WHITE|Y|Y|15JAN2024|15JUN2024|15JUN2024|.|N
STUDY01|STUDY01-1002|Placebo|2|45|<65|F|BLACK|Y|Y|18JAN2024|18JUN2024|18JUN2024|.|N
STUDY01|STUDY01-1003|Treatment A|1|43|<65|M|ASIAN|Y|Y|20JAN2024|01APR2024|01APR2024|15APR2024|Y
STUDY01|STUDY01-1004|Treatment A|1|51|<65|F|WHITE|Y|Y|25JAN2024|25JUN2024|25JUN2024|.|N
STUDY01|STUDY01-1005|Placebo|2|36|<65|M|ASIAN|Y|Y|28JAN2024|.|15JUL2024|.|N
STUDY01|STUDY01-1006|Treatment A|1|68|>=65|F|WHITE|Y|Y|30JAN2024|01JUL2024|01JUL2024|.|N
"
adsl <- read_delim(
  adsl_txt,
  delim = "|",
  col_names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX",
                "RACE","SAFFL","ITTFL","TRTSDT","TRTEDT","EOSDT","DTHDT","DTHFL"),
  na = ".",
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    TRTSDT = dmy(TRTSDT),
    TRTEDT = dmy(TRTEDT),
    EOSDT  = dmy(EOSDT),
    DTHDT  = dmy(DTHDT)
  )

# ADRS
adrs_txt <- "
STUDY01|STUDY01-1001|STABLE DISEASE|2024-03-15
STUDY01|STUDY01-1001|STABLE DISEASE|2024-06-15
STUDY01|STUDY01-1002|STABLE DISEASE|2024-03-18
STUDY01|STUDY01-1002|PROGRESSIVE DISEASE|2024-05-20
STUDY01|STUDY01-1003|PARTIAL RESPONSE|2024-02-20
STUDY01|STUDY01-1003|PROGRESSIVE DISEASE|2024-03-25
STUDY01|STUDY01-1004|PARTIAL RESPONSE|2024-03-25
STUDY01|STUDY01-1005|STABLE DISEASE|2024-04-28
STUDY01|STUDY01-1006|COMPLETE RESPONSE|2024-04-30
"
adrs <- read_delim(
  adrs_txt,
  delim = "|",
  col_names = c("STUDYID","USUBJID","RSSTRESC","RSDTC"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(RSDTC = ymd(RSDTC))

# Write source datasets
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adrs, file.path(out, "adrs.csv"), row.names = FALSE, na = "")

# ---------- STEP 2: Derive PFS ----------
prog <- adrs %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(USUBJID, PROGDT = RSDTC)

adtte_pfs <- adsl %>%
  left_join(prog, by = "USUBJID") %>%
  mutate(
    PARAMCD = "PFS",
    PARAM   = "Progression-Free Survival (Days)",
    PARAMN  = 1L,
    STARTDT = TRTSDT,
    # event/censor logic
    ADT = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ PROGDT,
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT >  DTHDT ~ DTHDT,
      !is.na(PROGDT)                              ~ PROGDT,
      !is.na(DTHDT)                               ~ DTHDT,
      TRUE                                        ~ coalesce(EOSDT, TRTEDT, TRTSDT)
    ),
    CNSR = if_else(
      !is.na(PROGDT) | !is.na(DTHDT), 0L,
      1L
    ),
    EVNTDESC = case_when(
      !is.na(PROGDT) & (is.na(DTHDT) | PROGDT <= DTHDT) ~ "Disease Progression",
      !is.na(DTHDT) & (is.na(PROGDT) | DTHDT < PROGDT)  ~ "Death",
      TRUE                                            ~ ""
    ),
    CNSDTDSC = if_else(CNSR == 1L, "Censored - No progression or death", ""),
    SRCDOM = case_when(
      !is.na(PROGDT) & (is.na(DTHDT) | PROGDT <= DTHDT) ~ "RS",
      !is.na(DTHDT)                                   ~ "ADSL",
      TRUE                                            ~ "ADSL"
    ),
    AVAL = if_else(!is.na(ADT) & !is.na(STARTDT),
                   as.numeric(ADT - STARTDT + 1),
                   NA_real_)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE,
         SAFFL, ITTFL, PARAMCD, PARAM, PARAMN, STARTDT, ADT,
         AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ---------- STEP 3: Derive OS ----------
adtte_os <- adsl %>%
  mutate(
    PARAMCD = "OS",
    PARAM   = "Overall Survival (Days)",
    PARAMN  = 2L,
    STARTDT = TRTSDT,
    ADT = if_else(DTHFL == "Y" & !is.na(DTHDT), DTHDT,
                  coalesce(EOSDT, TRTEDT, TRTSDT)),
    CNSR = if_else(DTHFL == "Y" & !is.na(DTHDT), 0L, 1L),
    EVNTDESC = if_else(DTHFL == "Y" & !is.na(DTHDT), "Death", ""),
    CNSDTDSC = if_else(CNSR == 1L, "Censored - Alive", ""),
    SRCDOM = "ADSL",
    AVAL = as.numeric(ADT - STARTDT + 1)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE,
         SAFFL, ITTFL, PARAMCD, PARAM, PARAMN, STARTDT, ADT,
         AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ---------- STEP 4: Derive TTR ----------
resp <- adrs %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE", "PARTIAL RESPONSE")) %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(USUBJID, RESPDT = RSDTC)

adtte_ttr <- adsl %>%
  left_join(resp, by = "USUBJID") %>%
  mutate(
    PARAMCD = "TTR",
    PARAM   = "Time to Response (Days)",
    PARAMN  = 3L,
    STARTDT = TRTSDT,
    ADT = if_else(!is.na(RESPDT), RESPDT,
                  coalesce(EOSDT, TRTEDT, TRTSDT)),
    CNSR = if_else(!is.na(RESPDT), 0L, 1L),
    EVNTDESC = if_else(!is.na(RESPDT), "Response", ""),
    CNSDTDSC = if_else(CNSR == 1L, "Censored - No response", ""),
    SRCDOM = "ADSL",
    AVAL = as.numeric(ADT - STARTDT + 1)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE,
         SAFFL, ITTFL, PARAMCD, PARAM, PARAMN, STARTDT, ADT,
         AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ---------- STEP 5: Combine all TTE parameters ----------
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr) %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  mutate(
    AVAL_M = if_else(!is.na(AVAL), AVAL / 30.4375, NA_real_)
  )

# ---------- STEP 6: Add sequence ----------
adtte <- adtte %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number() - 1L) %>%
  ungroup()

# Write ADaM datasets
write.csv(adtte, file.path(out, "adtte.csv"), row.names = FALSE, na = "")
write.csv(adtte_os, file.path(out, "adtte_os.csv"), row.names = FALSE, na = "")
write.csv(adtte_pfs, file.path(out, "adtte_pfs.csv"), row.names = FALSE, na = "")
write.csv(adtte_ttr, file.path(out, "adtte_ttr.csv"), row.names = FALSE, na = "")

# ---------- STEP 7: Create listing (PROC PRINT equivalent) ----------
# Helper to format dates as SAS DATE9. (e.g., 15JAN2024)
fmt_sas_date <- function(x) {
  ifelse(is.na(x), "", toupper(format(x, "%d%b%Y")))
}

listing_df <- adtte %>%
  mutate(
    STARTDT = fmt_sas_date(STARTDT),
    ADT     = fmt_sas_date(ADT)
  ) %>%
  # Ensure character columns stay character, numeric keep as is
  select(
    STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE,
    SAFFL, ITTFL, PARAMCD, PARAM, PARAMN, STARTDT, ADT,
    AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM, ASEQ
  )

# Write plain‑text listing
txt_path <- file.path(out, "P086.lst")
write.table(
  listing_df,
  file = txt_path,
  sep = " ",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = ""
)

# Write CSV version of the listing body
write.csv(listing_df, file.path(out, "P086.lst.csv"), row.names = FALSE, na = "")
```