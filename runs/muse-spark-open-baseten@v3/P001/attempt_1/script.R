# P001_adsl.sas -> R translation
# Output: adsl.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# helper: SAS round half away from zero (not needed here but defined)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: clean haven labelled vectors to plain R vectors
clean_haven <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      # zap labels but keep underlying values
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      col <- haven::zap_label(col)
      df[[nm]] <- col
    }
    # also zap formats for other columns
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  df
}

# helper: parse yymmdd10. from first 10 chars (YYYY-MM-DD)
parse_yymmdd10 <- function(x) {
  # x is character vector, may be NA
  # SAS: input(substr(x,1,10), yymmdd10.) -> expects YYYY-MM-DD
  v <- substr(x, 1, 10)
  # strict format %Y-%m-%d
  suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
}

# helper: parse e8601dt19. (YYYY-MM-DDThh:mm:ss)
parse_e8601dt19 <- function(x) {
  # SAS ?? suppresses error -> NA on failure
  # strict: %Y-%m-%dT%H:%M:%S
  # also handle possible trailing spaces
  x <- trimws(as.character(x))
  # if NA or empty -> NA
  res <- rep(as.POSIXct(NA), length(x))
  idx <- !is.na(x) & x != ""
  if (any(idx)) {
    # try primary format
    parsed <- suppressWarnings(as.POSIXct(x[idx], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    # for those still NA, try without seconds? SAS would fail, but keep NA
    res[idx] <- parsed
  }
  res
}

# -------------------------------------------------- paths
root <- "D:/meta_hackathon/sas2r-agent"
sdtm_dir <- file.path(root, "data/raw/cdisc-pilot/sdtm")
qsraw_dir <- file.path(root, "data/raw/cdisc-pilot/sdtm")
# also support forward slashes as in SAS librefs
sdtm_dir2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

# function to find xpt file
find_xpt <- function(dir, name) {
  p1 <- file.path(dir, paste0(name, ".xpt"))
  if (file.exists(p1)) return(p1)
  p2 <- file.path(sdtm_dir2, paste0(name, ".xpt"))
  if (file.exists(p2)) return(p2)
  # fallback: list files case-insensitive
  if (dir.exists(dir)) {
    fl <- list.files(dir, pattern = "\\.xpt$", full.names = TRUE, ignore.case = TRUE)
    cand <- fl[tolower(basename(fl)) == paste0(tolower(name), ".xpt")]
    if (length(cand) > 0) return(cand[1])
  }
  return(p1)
}

dm_path <- find_xpt(sdtm_dir, "dm")
ex_path <- find_xpt(sdtm_dir, "ex")
ds_path <- find_xpt(sdtm_dir, "ds")
ae_path <- find_xpt(sdtm_dir, "ae")
qs_path <- find_xpt(qsraw_dir, "qs")

cat("Reading DM from:", dm_path, "\n")
cat("Reading EX from:", ex_path, "\n")
cat("Reading DS from:", ds_path, "\n")
cat("Reading AE from:", ae_path, "\n")
cat("Reading QS from:", qs_path, "\n")

dm <- haven::read_xpt(dm_path) %>% clean_haven()
ex <- haven::read_xpt(ex_path) %>% clean_haven()
ds <- haven::read_xpt(ds_path) %>% clean_haven()
ae <- haven::read_xpt(ae_path) %>% clean_haven()
qs <- haven::read_xpt(qs_path) %>% clean_haven()

# Ensure character columns are character (haven may import as character already)
# Convert any factors to character
to_char <- function(x) if (is.factor(x)) as.character(x) else x
dm <- dm %>% mutate(across(everything(), ~ to_char(.)))
ex <- ex %>% mutate(across(everything(), ~ to_char(.)))
ds <- ds %>% mutate(across(everything(), ~ to_char(.)))
qs <- qs %>% mutate(across(everything(), ~ to_char(.)))
# numeric columns that should be numeric: AGE, QSSTRESN etc. haven already numeric; if character numeric, coerce where needed
# Ensure AGE numeric
if ("AGE" %in% names(dm)) dm$AGE <- suppressWarnings(as.numeric(dm$AGE))
if ("QSSTRESN" %in% names(qs)) qs$QSSTRESN <- suppressWarnings(as.numeric(qs$QSSTRESN))

# -------------------------------------- 2. Base subject record from DM
keep_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
               "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
               "DTHDTC","DTHFL")
# only keep those that exist
keep_vars <- intersect(keep_vars, names(dm))
adsl_base <- dm %>% select(all_of(keep_vars))

# ------------------------------------------ 3. Treatment dates
adsl_trt <- adsl_base %>%
  mutate(
    TRTSDT = parse_yymmdd10(RFXSTDTC),
    TRTEDT = parse_yymmdd10(RFXENDTC),
    DTHDT = parse_yymmdd10(DTHDTC),
    TRTSDTM = parse_e8601dt19(RFXSTDTC),
    TRTEDTM = parse_e8601dt19(RFXENDTC)
  ) %>%
  mutate(
    TRTDURD = ifelse(!is.na(TRTSDT) & !is.na(TRTEDT), as.numeric(TRTEDT - TRTSDT) + 1, NA_real_)
  )

# ------------------------------ 4. Population flags
# SAFFL = Y if subject has >=1 EX record with non-missing EXTRT
# EXTRT missing means NA or "" or only spaces
ex_saf <- ex %>%
  filter(!is.na(EXTRT) & trimws(EXTRT) != "") %>%
  distinct(STUDYID, USUBJID) %>%
  mutate(SAFFL = "Y")

adsl_pop <- adsl_trt %>%
  left_join(ex_saf, by = c("STUDYID","USUBJID")) %>%
  mutate(
    SAFFL = ifelse(is.na(SAFFL), "N", SAFFL),
    ITTFL = ifelse(ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"), "Y", "N")
  )

# ------------------------------ 5. Disposition from DS
# proc sort data=ds; by USUBJID DSDTC; run;
# where DSCAT="DISPOSITION EVENT" and DSDECOD ne "SCREEN FAILURE"; last.USUBJID
# DSDTC is character ISO datetime; sort lexicographically equals chronological if ISO
ds_disp <- ds %>%
  filter(DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE") %>%
  arrange(USUBJID, DSDTC) %>%
  group_by(USUBJID) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    EOSSTT = ifelse(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"),
    DCSREAS = ifelse(DSDECOD == "COMPLETED", "", DSDECOD),
    DCSREASP = ifelse(DSDECOD == "OTHER", ifelse(is.na(DSTERM), "", DSTERM), "")
  ) %>%
  select(USUBJID, EOSSTT, DCSREAS, DCSREASP)

adsl_disp <- adsl_pop %>%
  left_join(ds_disp, by = "USUBJID") %>%
  mutate(
    EOSSTT = ifelse(is.na(EOSSTT), "DISCONTINUED", EOSSTT),
    EOTSTT = EOSSTT
  )

# ------------------------------ 6. Age groups + numeric codes
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

# ------------------------------ 7. TRT01P / TRT01A
adsl_trt01 <- adsl_age %>%
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  )

# ------------------------------ 8. Efficacy flag from QS
# EFFFL = Y if SAFFL=Y AND >=1 post-baseline ACTOT score in QS.
# where QSTESTCD="ACTOT" and QSCAT="ALZHEIMER'S DISEASE ASSESSMENT SCALE" and not missing QSSTRESN and upcase(VISIT) ne "BASELINE" and not index(upcase(VISIT),"SCREENING")
qs_eff <- qs %>%
  filter(QSTESTCD == "ACTOT",
         QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE",
         !is.na(QSSTRESN),
         toupper(VISIT) != "BASELINE",
         !grepl("SCREENING", toupper(VISIT), fixed = TRUE)) %>%
  distinct(USUBJID) %>%
  mutate(EFFFL = "Y")

adsl_eff <- adsl_trt01 %>%
  left_join(qs_eff, by = "USUBJID") %>%
  mutate(
    EFFFL = ifelse(!is.na(SAFFL) & SAFFL == "Y" & !is.na(EFFFL) & EFFFL == "Y", "Y", "N")
  )

# ------------------------------ 9. Labels, formats, lengths - retain order
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
                  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
                  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
                  "SAFFL","ITTFL","EFFFL")

# Ensure all retain columns exist; create missing as NA
for (col in retain_order) {
  if (!col %in% names(adsl_eff)) {
    adsl_eff[[col]] <- NA
  }
}

adam_adsl <- adsl_eff %>%
  select(all_of(retain_order)) %>%
  arrange(USUBJID)

# Format dates for CSV output: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# Keep numeric TRTDURD etc as numeric
# Convert Date columns to character ISO
adam_adsl_out <- adam_adsl %>%
  mutate(
    TRTSDT = format(TRTSDT, "%Y-%m-%d"),
    TRTEDT = format(TRTEDT, "%Y-%m-%d"),
    DTHDT = format(DTHDT, "%Y-%m-%d"),
    TRTSDTM = format(TRTSDTM, "%Y-%m-%dT%H:%M:%S"),
    TRTEDTM = format(TRTEDTM, "%Y-%m-%dT%H:%M:%S")
  )

# Ensure empty strings remain "" not NA for DCSREAS etc already handled
# Write CSV
write.csv(adam_adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

cat(sprintf("NOTE: R ADSL built: %d subjects, %d variables\n", nrow(adam_adsl_out), ncol(adam_adsl_out)))
