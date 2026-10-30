# P088_adeff.R
# Translation of SAS program P088_adeff.sas to R
# Generates datasets: adeff, adeff_resp, adeff_tumor, adsl
# and a listing (text and CSV) of adeff.
# All outputs are written to the directory specified by OUT_DIR.

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# Helper to write CSV with SAS‑style missing handling
write_sas_csv <- function(df, name) {
  write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
}

# Helper: convert character date (YYYY-MM-DD) to Date
char_to_date <- function(x) {
  ymd(x, quiet = TRUE)
}

# ----------------------------------------------------------------------
# STEP 1 – Mock tumor response data (RS)
# ----------------------------------------------------------------------
rs_txt <- "
STUDY01|STUDY01-1001|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|3|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 16|4|2024-05-06
STUDY01|STUDY01-1001|4|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 24|5|2024-07-01
STUDY01|STUDY01-1002|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-14
STUDY01|STUDY01-1002|3|OVRLRESP|Overall Response|PROGRESSIVE DISEASE|.|Week 16|4|2024-05-09
STUDY01|STUDY01-1003|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 8|3|2024-03-16
STUDY01|STUDY01-1003|3|OVRLRESP|Overall Response|COMPLETE RESPONSE|.|Week 16|4|2024-05-11
STUDY01|STUDY01-1004|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-25
STUDY01|STUDY01-1004|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-21
STUDY01|STUDY01-1005|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-28
STUDY01|STUDY01-1005|2|OVRLRESP|Overall Response|PROGRESSIVE DISEASE|.|Week 8|3|2024-03-24
STUDY01|STUDY01-1006|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-30
STUDY01|STUDY01-1006|2|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 8|3|2024-03-26
"
rs <- read_delim(I(rs_txt), delim = "|", col_names = c(
  "STUDYID","USUBJID","RSSEQ","RSTESTCD","RSTEST","RSSTRESC",
  "RSSTRESN","VISIT","VISITNUM","RSDTC"
), trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(
    RSSEQ = as.integer(RSSEQ),
    VISITNUM = as.integer(VISITNUM)
  )

# ----------------------------------------------------------------------
# STEP 2 – Mock tumor size data (TU)
# ----------------------------------------------------------------------
tu_txt <- "
STUDY01|STUDY01-1001|1|LDIAM|Longest Diameter|45|45|45|mm|LIVER|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|LDIAM|Longest Diameter|42|42|42|mm|LIVER|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|3|LDIAM|Longest Diameter|35|35|35|mm|LIVER|Week 16|4|2024-05-06
STUDY01|STUDY01-1001|4|LDIAM|Longest Diameter|30|30|30|mm|LIVER|Week 24|5|2024-07-01
STUDY01|STUDY01-1002|1|LDIAM|Longest Diameter|38|38|38|mm|LUNG|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|LDIAM|Longest Diameter|40|40|40|mm|LUNG|Week 8|3|2024-03-14
STUDY01|STUDY01-1002|3|LDIAM|Longest Diameter|52|52|52|mm|LUNG|Week 16|4|2024-05-09
STUDY01|STUDY01-1003|1|LDIAM|Longest Diameter|55|55|55|mm|LYMPH NODE|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|LDIAM|Longest Diameter|40|40|40|mm|LYMPH NODE|Week 8|3|2024-03-16
STUDY01|STUDY01-1003|3|LDIAM|Longest Diameter|0|0|0|mm|LYMPH NODE|Week 16|4|2024-05-11
STUDY01|STUDY01-1004|1|LDIAM|Longest Diameter|32|32|32|mm|LIVER|Baseline|1|2024-01-25
STUDY01|STUDY01-1004|2|LDIAM|Longest Diameter|30|30|30|mm|LIVER|Week 8|3|2024-03-21
STUDY01|STUDY01-1005|1|LDIAM|Longest Diameter|28|28|28|mm|LUNG|Baseline|1|2024-01-28
STUDY01|STUDY01-1005|2|LDIAM|Longest Diameter|42|42|42|mm|LUNG|Week 8|3|2024-03-24
STUDY01|STUDY01-1006|1|LDIAM|Longest Diameter|48|48|48|mm|LIVER|Baseline|1|2024-01-30
STUDY01|STUDY01-1006|2|LDIAM|Longest Diameter|36|36|36|mm|LIVER|Week 8|3|2024-03-26
"
tu <- read_delim(I(tu_txt), delim = "|", col_names = c(
  "STUDYID","USUBJID","TUSEQ","TUTESTCD","TUTEST","TUORRES",
  "TUSTRESC","TUSTRESN","TUSTRESU","TULOC","VISIT","VISITNUM","TUDTC"
), trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(
    TUSEQ = as.integer(TUSEQ),
    TUSTRESN = as.numeric(TUSTRESN),
    VISITNUM = as.integer(VISITNUM)
  )

# ----------------------------------------------------------------------
# STEP 3 – Mock ADSL
# ----------------------------------------------------------------------
adsl_txt <- "
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|Y|30JAN2024|01JUL2024
"
adsl <- read_delim(I(adsl_txt), delim = "|", col_names = c(
  "STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","EFFFL",
  "TRTSDT_raw","TRTEDT_raw"
), trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(
    TRTPN = as.integer(TRTPN),
    AGE = as.integer(AGE),
    TRTSDT = dmy(TRTSDT_raw),
    TRTEDT = dmy(TRTEDT_raw)
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

write_sas_csv(adsl, "adsl")

# ----------------------------------------------------------------------
# STEP 4 – ADEFF_RESP
# ----------------------------------------------------------------------
adeff_resp <- rs %>%
  filter(RSTESTCD == "OVRLRESP") %>%
  left_join(adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "OVRLRESP",
    PARAM = "Overall Response",
    PARAMN = 1L,
    AVALC = RSSTRESC,
    AVAL = case_when(
      str_to_upper(RSSTRESC) == "COMPLETE RESPONSE"   ~ 1,
      str_to_upper(RSSTRESC) == "PARTIAL RESPONSE"    ~ 2,
      str_to_upper(RSSTRESC) == "STABLE DISEASE"      ~ 3,
      str_to_upper(RSSTRESC) == "PROGRESSIVE DISEASE" ~ 4,
      str_to_upper(RSSTRESC) == "NOT EVALUABLE"      ~ 5,
      TRUE                                          ~ NA_real_
    ),
    ADT = char_to_date(RSDTC),
    ADY = if_else(!is.na(ADT) & !is.na(TRTSDT),
                  as.integer(if_else(ADT >= TRTSDT,
                                     ADT - TRTSDT + days(1),
                                     ADT - TRTSDT)),
                  NA_integer_),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = if_else(AVISIT == "Baseline", "Y", ""),
    ANL01FL = if_else(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(
    STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL,
    TRTSDT, TRTEDT, AGE, SEX,
    PARAMCD, PARAM, PARAMN,
    AVAL, AVALC,
    ADT, ADY,
    AVISIT, AVISITN,
    ABLFL, ANL01FL
  )

write_sas_csv(adeff_resp, "adeff_resp")

# ----------------------------------------------------------------------
# STEP 5 – ADEFF_TUMOR
# ----------------------------------------------------------------------
adeff_tumor <- tu %>%
  filter(TUTESTCD == "LDIAM") %>%
  left_join(adsl, by = c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "SUMDIAM",
    PARAM = "Sum of Longest Diameters",
    PARAMN = 2L,
    AVAL = TUSTRESN,
    AVALU = TUSTRESU,
    AVALC = NA_character_,
    ADT = char_to_date(TUDTC),
    ADY = if_else(!is.na(ADT) & !is.na(TRTSDT),
                  as.integer(if_else(ADT >= TRTSDT,
                                     ADT - TRTSDT + days(1),
                                     ADT - TRTSDT)),
                  NA_integer_),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = if_else(AVISIT == "Baseline", "Y", ""),
    ANL01FL = if_else(ABLFL != "Y" & !is.na(AVAL), "Y", "")
  ) %>%
  select(
    STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL,
    TRTSDT, TRTEDT, AGE, SEX,
    PARAMCD, PARAM, PARAMN,
    AVAL, AVALC, AVALU,
    ADT, ADY,
    AVISIT, AVISITN,
    ABLFL, ANL01FL
  )

# ----------------------------------------------------------------------
# STEP 5 – Baseline and change from baseline for tumor
# ----------------------------------------------------------------------
base_tumor <- adeff_tumor %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, PARAMCD, BASE = AVAL)

adeff_tumor <- adeff_tumor %>%
  left_join(base_tumor, by = c("USUBJID","PARAMCD")) %>%
  mutate(
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = if_else(!is.na(CHG) & BASE > 0, (CHG / BASE) * 100, NA_real_)
  )

write_sas_csv(adeff_tumor, "adeff_tumor")

# ----------------------------------------------------------------------
# STEP 6 – Best Overall Response (BOR)
# ----------------------------------------------------------------------
bor_source <- adeff_resp %>% filter(ABLFL != "Y")

bor <- bor_source %>%
  group_by(USUBJID) %>%
  slice_min(order_by = AVAL, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    PARAMCD = "BOR",
    PARAM = "Best Overall Response",
    PARAMN = 3L,
    AVISIT = "Best Response",
    AVISITN = 99L,
    ABLFL = "",
    ANL01FL = "Y"
  ) %>%
  select(names(adeff_resp))

# ----------------------------------------------------------------------
# STEP 7 – Responder flag (RESPFL)
# ----------------------------------------------------------------------
respfl <- bor %>%
  mutate(
    RESPFL = if_else(AVAL %in% c(1,2), "Y", "N"),
    PARAMCD = "RESPFL",
    PARAM = "Objective Response Flag",
    PARAMN = 4L,
    AVAL = if_else(RESPFL == "Y", 1, 0),
    AVALC = RESPFL,
    ABLFL = "",
    ANL01FL = ""
  ) %>%
  select(names(adeff_resp))

# ----------------------------------------------------------------------
# STEP 8 – Nadir tumor size (NADIRTUM)
# ----------------------------------------------------------------------
nadir_vals <- adeff_tumor %>%
  filter(ABLFL != "Y") %>%
  group_by(USUBJID) %>%
  summarise(AVAL = min(AVAL, na.rm = TRUE), .groups = "drop")

baseline_info <- adeff_tumor %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, STUDYID, TRTP, TRTPN, SAFFL, EFFFL,
         TRTSDT, TRTEDT, AGE, SEX, BASE)

nadir <- nadir_vals %>%
  left_join(baseline_info, by = "USUBJID") %>%
  mutate(
    PARAMCD = "NADIRTUM",
    PARAM = "Nadir Tumor Size",
    PARAMN = 5L,
    AVALU = "mm",
    AVISIT = "Nadir",
    AVISITN = 98L,
    ABLFL = "",
    ANL01FL = "Y",
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = if_else(!is.na(CHG) & BASE > 0, (CHG / BASE) * 100, NA_real_),
    ADT = as.Date(NA),   # not defined in SAS for nadir
    ADY = NA_integer_
  ) %>%
  select(
    STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL,
    TRTSDT, TRTEDT, AGE, SEX,
    PARAMCD, PARAM, PARAMN,
    AVAL, AVALC = NA_character_, AVALU,
    ADT, ADY,
    AVISIT, AVISITN,
    ABLFL, ANL01FL,
    BASE, CHG, PCHG
  )

# ----------------------------------------------------------------------
# STEP 9 – Combine all efficacy parameters
# ----------------------------------------------------------------------
# Ensure all datasets have the same columns (add missing ones as NA)
all_cols <- c(
  "STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL",
  "TRTSDT","TRTEDT","AGE","SEX",
  "PARAMCD","PARAM","PARAMN",
  "AVAL","AVALC","AVALU","BASE","CHG","PCHG",
  "ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL"
)

fill_missing <- function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  df %>% mutate(across(all_of(missing_cols), ~ NA))
}

adeff <- bind_rows(
  fill_missing(adeff_resp),
  fill_missing(adeff_tumor),
  fill_missing(bor),
  fill_missing(respfl),
  fill_missing(nadir)
) %>%
  select(all_of(all_cols))

# ----------------------------------------------------------------------
# STEP 10 – Add ASEQ and response categories (RSCAT)
# ----------------------------------------------------------------------
adeff <- adeff %>%
  arrange(STUDYID, USUBJID, PARAMN, AVISITN, ADT) %>%
  group_by(USUBJID) %>%
  mutate(
    ASEQ = seq_len(n()) - 1L
  ) %>%
  ungroup() %>%
  mutate(
    RSCAT = case_when(
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE") ~ "RESPONDER",
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC == "STABLE DISEASE"                                 ~ "STABLE",
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC == "PROGRESSIVE DISEASE"                           ~ "PROGRESSOR",
      PARAMCD %in% c("OVRLRESP","BOR")                                                          ~ "OTHER",
      TRUE                                                                                     ~ NA_character_
    )
  )

# ----------------------------------------------------------------------
# Output final datasets
# ----------------------------------------------------------------------
write_sas_csv(adeff, "adeff")
write_sas_csv(adeff_resp, "adeff_resp")
write_sas_csv(adeff_tumor, "adeff_tumor")

# ----------------------------------------------------------------------
# Produce listing (text and CSV) of ADEFF – mimicking PROC PRINT
# ----------------------------------------------------------------------
listing_txt_path <- file.path(out_dir, "P088.lst")
listing_csv_path <- file.path(out_dir, "P088.csv")

# Text rendering (tab‑separated, NA as blank)
write.table(
  adeff,
  file = listing_txt_path,
  sep = "\t",
  row.names = FALSE,
  na = "",
  quote = FALSE
)

# CSV version of the same table
write.csv(adeff, listing_csv_path, row.names = FALSE, na = "")

# End of script
