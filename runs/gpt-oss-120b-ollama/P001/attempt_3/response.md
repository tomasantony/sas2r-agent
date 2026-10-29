```r
# ------------------------------------------------------------
# R translation of P001_adsl.sas (attempt 3)
# Generates ADaM ADSL dataset and writes it as CSV to OUT_DIR
# ------------------------------------------------------------

# Load required packages (pre‑installed in the environment)
library(haven)      # read XPORT files
library(dplyr)      # data manipulation
library(lubridate)  # date‑time parsing
library(stringr)    # string handling
library(tidyr)      # replace_na

# ------------------------------------------------------------------
# 1. Set up paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")

root   <- "D:/meta_hackathon/sas2r-agent"
sdtm   <- file.path(root, "data/raw/cdisc-pilot/sdtm")
qsraw  <- file.path(root, "data/raw/cdisc-pilot/sdtm")

# Input XPT files
dm_path <- file.path(sdtm, "dm.xpt")
ex_path <- file.path(sdtm, "ex.xpt")
ds_path <- file.path(sdtm, "ds.xpt")
ae_path <- file.path(sdtm, "ae.xpt")   # not used downstream but read for completeness
qs_path <- file.path(qsraw, "qs.xpt")

# ------------------------------------------------------------------
# 2. Read source datasets
# ------------------------------------------------------------------
dm <- read_xpt(dm_path)
ex <- read_xpt(ex_path)
ds <- read_xpt(ds_path)
ae <- read_xpt(ae_path)   # not used later
qs <- read_xpt(qs_path)

# ------------------------------------------------------------------
# 3. Base subject record (adsl_base)
# ------------------------------------------------------------------
adsl_base <- dm %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX, RACE,
    ETHNIC, COUNTRY, ARM, ARMCD, ACTARM, ACTARMCD,
    RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, DTHDTC, DTHFL
  )

# ------------------------------------------------------------------
# 4. Treatment dates (adsl_trt)
# ------------------------------------------------------------------
adsl_trt <- adsl_base %>%
  mutate(
    # Convert empty strings to NA
    RFXSTDTC = na_if(RFXSTDTC, ""),
    RFXENDTC = na_if(RFXENDTC, ""),
    DTHDTC   = na_if(DTHDTC,   ""),

    # Date‑only variables (first 10 characters)
    TRTSDT = if_else(!is.na(RFXSTDTC),
                     as.Date(substr(RFXSTDTC, 1, 10), format = "%Y-%m-%d"),
                     as.Date(NA)),
    TRTEDT = if_else(!is.na(RFXENDTC),
                     as.Date(substr(RFXENDTC, 1, 10), format = "%Y-%m-%d"),
                     as.Date(NA)),

    # Full datetime variables – SAS e8601dt19. is ISO‑8601 (e.g. 2020-01-02T13:45:00)
    TRTSDTM = if_else(!is.na(RFXSTDTC),
                      ymd_hms(RFXSTDTC, tz = "UTC"),
                      as.POSIXct(NA)),
    TRTEDTM = if_else(!is.na(RFXENDTC),
                      ymd_hms(RFXENDTC, tz = "UTC"),
                      as.POSIXct(NA)),

    # Date of death
    DTHDT = if_else(!is.na(DTHDTC),
                    as.Date(substr(DTHDTC, 1, 10), format = "%Y-%m-%d"),
                    as.Date(NA)),

    # Treatment duration (days, inclusive)
    TRTDURD = if_else(!is.na(TRTSDT) & !is.na(TRTEDT),
                     as.numeric(TRTEDT - TRTSDT) + 1,
                     as.numeric(NA))
  )

# ------------------------------------------------------------------
# 5. Population flags (SAFFL, ITTFL)
# ------------------------------------------------------------------
# SAFFL = Y if at least one EX record with non‑missing EXTRT
saffl_tbl <- ex %>%
  filter(!is.na(EXTRT) & EXTRT != "") %>%
  distinct(STUDYID, USUBJID) %>%
  mutate(SAFFL = "Y")

adsl_pop <- adsl_trt %>%
  left_join(saffl_tbl, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    SAFFL = if_else(is.na(SAFFL), "N", SAFFL),
    ITTFL = if_else(ARM %in% c("Placebo",
                              "Xanomeline High Dose",
                              "Xanomeline Low Dose"),
                    "Y", "N")
  )

# ------------------------------------------------------------------
# 6. Disposition from DS (latest per subject)
# ------------------------------------------------------------------
disp_latest <- ds %>%
  filter(DSCAT == "DISPOSITION EVENT", DSDECOD != "SCREEN FAILURE") %>%
  arrange(USUBJID, DSDTC) %>%
  group_by(USUBJID) %>%
  slice_tail(n = 1) %>%          # keep last (latest) record per subject
  ungroup() %>%
  mutate(
    EOSSTT = if_else(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"),
    DCSREAS = if_else(DSDECOD == "COMPLETED", "", DSDECOD),
    DCSREASP = if_else(DSDECOD == "OTHER", DSTERM, "")
  ) %>%
  select(USUBJID, EOSSTT, DCSREAS, DCSREASP)

adsl_disp <- adsl_pop %>%
  left_join(disp_latest, by = "USUBJID") %>%
  mutate(
    EOSSTT = if_else(is.na(EOSSTT), "DISCONTINUED", EOSSTT),
    EOTSTT = EOSSTT
  )

# ------------------------------------------------------------------
# 7. Age groups and numeric codes
# ------------------------------------------------------------------
adsl_age <- adsl_disp %>%
  mutate(
    AGEGR1 = case_when(
      AGE < 65                     ~ "<65",
      AGE >= 65 & AGE <= 80        ~ "65-80",
      AGE > 80                     ~ ">80",
      TRUE                         ~ NA_character_
    ),
    AGEGR1N = case_when(
      AGE < 65                     ~ 1L,
      AGE >= 65 & AGE <= 80        ~ 2L,
      AGE > 80                     ~ 3L,
      TRUE                         ~ NA_integer_
    ),
    SEXN = case_when(
      SEX == "M" ~ 1L,
      SEX == "F" ~ 2L,
      TRUE        ~ NA_integer_
    ),
    RACEN = case_when(
      RACE == "WHITE"                                          ~ 1L,
      RACE == "BLACK OR AFRICAN AMERICAN"                       ~ 2L,
      RACE == "ASIAN"                                           ~ 3L,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE"                ~ 4L,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"      ~ 5L,
      RACE == "MULTIPLE"                                        ~ 6L,
      TRUE                                                      ~ NA_integer_
    )
  )

# ------------------------------------------------------------------
# 8. Treatment variables TRT01P / TRT01A
# ------------------------------------------------------------------
adsl_trt01 <- adsl_age %>%
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  )

# ------------------------------------------------------------------
# 9. Efficacy flag from QS (EFFFL)
# ------------------------------------------------------------------
eff_tbl <- qs %>%
  filter(
    QSTESTCD == "ACTOT",
    QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE",
    !is.na(QSSTRESN),
    !str_to_upper(VISIT) %in% c("BASELINE"),
    !str_detect(str_to_upper(VISIT), "SCREENING")
  ) %>%
  distinct(USUBJID) %>%
  mutate(EFFFL = "Y")

adsl_eff <- adsl_trt01 %>%
  left_join(eff_tbl, by = "USUBJID") %>%
  mutate(EFFFL = replace_na(EFFFL, "N")) %>%          # missing -> "N"
  mutate(EFFFL = if_else(SAFFL == "Y" & EFFFL == "Y", "Y", "N"))

# ------------------------------------------------------------------
# 10. Final ADSL dataset – retain order as in SAS retain statement
# ------------------------------------------------------------------
adsl_final <- adsl_eff %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, AGEGR1, AGEGR1N,
    SEX, SEXN, RACE, RACEN, ETHNIC, COUNTRY,
    ARM, ARMCD, ACTARM, ACTARMCD,
    TRT01P, TRT01A,
    RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
    TRTSDT, TRTEDT, TRTSDTM, TRTEDTM,
    TRTDURD, DTHDTC, DTHDT, DTHFL,
    DCSREAS, DCSREASP, EOSSTT, EOTSTT,
    SAFFL, ITTFL, EFFFL
  )

# ------------------------------------------------------------------
# 11. Write output CSV (dates as YYYY‑MM‑DD, datetimes as ISO‑8601)
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "adsl.csv")
write.csv(adsl_final, output_path, row.names = FALSE, na = "")

# End of script
```