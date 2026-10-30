```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

is_sas_missing_chr <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

parse_sas_date <- function(x) {
  v <- substr(as.character(x), 1, 10)
  v[is_sas_missing_chr(x)] <- NA_character_
  as.Date(v, format = "%Y-%m-%d")
}
parse_sas_datetime <- function(x) {
  x_chr <- as.character(x)
  x_chr[is_sas_missing_chr(x_chr)] <- NA_character_
  suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}
format_date_iso <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}
format_datetime_iso <- function(dt) {
  ifelse(is.na(dt), "", format(dt, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

zap_haven <- function(df) {
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  df <- haven::zap_label(df)
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- as.vector(df[[nm]])
    }
  }
  df
}

library(haven)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
qsraw_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

dm <- zap_haven(haven::read_xpt(file.path(sdtm_path, "dm.xpt")))
ex <- zap_haven(haven::read_xpt(file.path(sdtm_path, "ex.xpt")))
ds <- zap_haven(haven::read_xpt(file.path(sdtm_path, "ds.xpt")))
ae <- zap_haven(haven::read_xpt(file.path(sdtm_path, "ae.xpt")))
qs <- zap_haven(haven::read_xpt(file.path(qsraw_path, "qs.xpt")))

dm <- dm %>% mutate(across(where(is.factor), as.character))
ex <- ex %>% mutate(across(where(is.factor), as.character))
ds <- ds %>% mutate(across(where(is.factor), as.character))
ae <- ae %>% mutate(across(where(is.factor), as.character))
qs <- qs %>% mutate(across(where(is.factor), as.character))

adsl_base <- dm %>%
  select(STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX, RACE, ETHNIC, COUNTRY,
         ARM, ARMCD, ACTARM, ACTARMCD, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
         DTHDTC, DTHFL)

adsl_trt <- adsl_base %>%
  mutate(
    TRTSDT = parse_sas_date(RFXSTDTC),
    TRTSDTM = parse_sas_datetime(RFXSTDTC),
    TRTEDT = parse_sas_date(RFXENDTC),
    TRTEDTM = parse_sas_datetime(RFXENDTC),
    DTHDT = parse_sas_date(DTHDTC)
  ) %>%
  mutate(
    TRTDURD = ifelse(!is.na(TRTSDT) & !is.na(TRTEDT),
                     as.numeric(TRTEDT - TRTSDT + 1),
                     NA_real_)
  )

stopifnot("EXTRT" %in% names(ex))
stopifnot("STUDYID" %in% names(ex))
stopifnot("USUBJID" %in% names(ex))

saf <- ex %>%
  filter(!is_sas_missing_chr(EXTRT)) %>%
  distinct(STUDYID, USUBJID) %>%
  mutate(SAFFL = "Y")

adsl_pop <- adsl_trt %>%
  left_join(saf, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    SAFFL = ifelse(is_sas_missing_chr(SAFFL), "N", SAFFL),
    ITTFL = ifelse(ARM %in% c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"), "Y", "N")
  )
stopifnot("SAFFL" %in% names(adsl_pop))
stopifnot("ITTFL" %in% names(adsl_pop))

stopifnot("USUBJID" %in% names(ds))
stopifnot("DSDTC" %in% names(ds))
stopifnot("DSCAT" %in% names(ds))
stopifnot("DSDECOD" %in% names(ds))

ds_sorted <- ds %>%
  arrange(USUBJID, DSDTC)

disp_filtered <- ds_sorted %>%
  filter(DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE")

disp <- disp_filtered %>%
  group_by(USUBJID) %>%
  arrange(DSDTC, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    EOSSTT = ifelse(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"),
    DCSREAS = ifelse(DSDECOD == "COMPLETED", "", DSDECOD),
    DCSREASP = ifelse(DSDECOD == "COMPLETED", "",
                      ifelse(DSDECOD == "OTHER",
                             ifelse(is.na(DSTERM), "", as.character(DSTERM)),
                             ""))
  ) %>%
  select(USUBJID, EOSSTT, DCSREAS, DCSREASP)

adsl_disp <- adsl_pop %>%
  left_join(disp, by = "USUBJID") %>%
  mutate(
    EOSSTT = ifelse(is_sas_missing_chr(EOSSTT), "DISCONTINUED", EOSSTT),
    EOTSTT = EOSSTT
  )
stopifnot("EOSSTT" %in% names(adsl_disp))
stopifnot("EOTSTT" %in% names(adsl_disp))

adsl_age <- adsl_disp %>%
  mutate(
    AGEGR1 = case_when(
      !is.na(AGE) & AGE < 65 ~ "<65",
      !is.na(AGE) & AGE >= 65 & AGE <= 80 ~ "65-80",
      !is.na(AGE) & AGE > 80 ~ ">80",
      TRUE ~ NA_character_
    ),
    AGEGR1N = case_when(
      !is.na(AGE) & AGE < 65 ~ 1,
      !is.na(AGE) & AGE >= 65 & AGE <= 80 ~ 2,
      !is.na(AGE) & AGE > 80 ~ 3,
      TRUE ~ NA_real_
    ),
    SEXN = case_when(
      SEX == "M" ~ 1,
      SEX == "F" ~ 2,
      TRUE ~ NA_real_
    ),
    RACEN = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 4,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
      RACE == "MULTIPLE" ~ 6,
      TRUE ~ NA_real_
    )
  )

adsl_trt01 <- adsl_age %>%
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  )

stopifnot("QSTESTCD" %in% names(qs))
stopifnot("QSCAT" %in% names(qs))
stopifnot("QSSTRESN" %in% names(qs))
stopifnot("VISIT" %in% names(qs))

eff <- qs %>%
  filter(
    QSTESTCD == "ACTOT",
    QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE",
    !is.na(QSSTRESN),
    toupper(as.character(VISIT)) != "BASELINE",
    !grepl("SCREENING", toupper(as.character(VISIT)), fixed = TRUE)
  ) %>%
  distinct(USUBJID) %>%
  mutate(EFFFL = "Y")

adsl_eff <- adsl_trt01 %>%
  left_join(eff, by = "USUBJID") %>%
  mutate(
    EFFFL = ifelse(!is.na(SAFFL) & SAFFL == "Y" & !is.na(EFFFL) & EFFFL == "Y", "Y", "N")
  )
stopifnot("EFFFL" %in% names(adsl_eff))

retain_order <- c(
  "STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
  "SAFFL","ITTFL","EFFFL"
)

for (col in retain_order) {
  if (!col %in% names(adsl_eff)) {
    adsl_eff[[col]] <- NA
  }
}

adsl_final <- adsl_eff %>%
  select(all_of(retain_order)) %>%
  arrange(USUBJID)

adsl_out <- adsl_final %>%
  mutate(
    TRTSDT_chr = format_date_iso(TRTSDT),
    TRTEDT_chr = format_date_iso(TRTEDT),
    DTHDT_chr = format_date_iso(DTHDT),
    TRTSDTM_chr = format_datetime_iso(TRTSDTM),
    TRTEDTM_chr = format_datetime_iso(TRTEDTM)
  ) %>%
  select(-TRTSDT, -TRTEDT, -DTHDT, -TRTSDTM, -TRTEDTM) %>%
  rename(TRTSDT = TRTSDT_chr, TRTEDT = TRTEDT_chr, DTHDT = DTHDT_chr,
         TRTSDTM = TRTSDTM_chr, TRTEDTM = TRTEDTM_chr) %>%
  select(all_of(retain_order))

write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

n_rows <- nrow(adsl_final)
n_vars <- ncol(adsl_final)
message(sprintf("NOTE: R ADSL built: %d subjects, %d variables", n_rows, n_vars))
```