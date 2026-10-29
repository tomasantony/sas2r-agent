```r
# P001_adsl.sas -> R translation
# Output: adsl.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

is_sas_missing_chr <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
is_sas_missing <- function(x) {
  if (is.character(x)) is_sas_missing_chr(x) else is.na(x)
}

parse_sas_yymmdd <- function(x) {
  s <- substr(as.character(x), 1, 10)
  s[is_sas_missing_chr(s)] <- NA_character_
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

parse_sas_e8601dt <- function(x) {
  x_chr <- as.character(x)
  x_chr[is_sas_missing_chr(x_chr)] <- NA_character_
  res <- suppressWarnings(lubridate::ymd_hms(x_chr, quiet = TRUE, tz = "UTC"))
  idx <- is.na(res) & !is.na(x_chr)
  if (any(idx)) {
    res2 <- suppressWarnings(lubridate::ymd_hm(x_chr[idx], quiet = TRUE, tz = "UTC"))
    res[idx] <- res2
  }
  idx2 <- is.na(res) & !is.na(x_chr)
  if (any(idx2)) {
    res3 <- suppressWarnings(lubridate::ymd(x_chr[idx2], quiet = TRUE, tz = "UTC"))
    res[idx2] <- as.POSIXct(res3, tz = "UTC")
  }
  idx3 <- is.na(res) & !is.na(x_chr)
  if (any(idx3)) {
    res4 <- suppressWarnings(lubridate::parse_date_time(x_chr[idx3],
              orders = c("YmdHMS", "YmdHM", "Ymd", "Y-m-d H:M:S", "Y-m-dT H:M:S"),
              quiet = TRUE, tz = "UTC"))
    res[idx3] <- as.POSIXct(res4, tz = "UTC")
  }
  res
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readr)
})

root_default <- "D:/meta_hackathon/sas2r-agent"
env_root <- Sys.getenv("root")
if (env_root != "" && !is.na(env_root)) root_default <- env_root
env_sdtm <- Sys.getenv("sdtm")
env_qsraw <- Sys.getenv("qsraw")
env_rawsdtm <- Sys.getenv("rawsdtm")

sdtm_dir <- file.path(root_default, "data/raw/cdisc-pilot/sdtm")
qsraw_dir <- file.path(root_default, "data/raw/cdisc-pilot/sdtm")
if (env_sdtm != "" && dir.exists(env_sdtm)) sdtm_dir <- env_sdtm
if (env_qsraw != "" && dir.exists(env_qsraw)) qsraw_dir <- env_qsraw
if (env_rawsdtm != "" && dir.exists(env_rawsdtm)) sdtm_dir <- env_rawsdtm

candidate_dirs <- unique(c(sdtm_dir, qsraw_dir,
                           "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
                           "data/raw/cdisc-pilot/sdtm",
                           "./data/raw/cdisc-pilot/sdtm"))

find_xpt <- function(fname, dirs) {
  for (d in dirs) {
    p <- file.path(d, fname)
    if (file.exists(p)) return(p)
    p2 <- file.path(d, tolower(fname))
    if (file.exists(p2)) return(p2)
  }
  if (dir.exists(root_default)) {
    all <- list.files(root_default, pattern = fname, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(all) > 0) return(all[1])
  }
  return(file.path(dirs[1], fname))
}

dm_path <- find_xpt("dm.xpt", candidate_dirs)
ex_path <- find_xpt("ex.xpt", candidate_dirs)
ds_path <- find_xpt("ds.xpt", candidate_dirs)
ae_path <- find_xpt("ae.xpt", candidate_dirs)
qs_path <- find_xpt("qs.xpt", candidate_dirs)

cat("Reading SDTM XPTs...\n")
cat(" dm:", dm_path, "\n")
cat(" ex:", ex_path, "\n")
cat(" ds:", ds_path, "\n")
cat(" ae:", ae_path, "\n")
cat(" qs:", qs_path, "\n")

read_xpt_safe <- function(path) {
  if (!file.exists(path)) stop(paste0("File not found: ", path))
  df <- haven::read_xpt(path)
  df <- haven::zap_formats(df)
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), function(col) {
    if (haven::is.labelled(col)) {
      col <- haven::zap_labels(col)
      col <- haven::zap_label(col)
    }
    col
  }))
  as.data.frame(df, stringsAsFactors = FALSE)
}

dm <- read_xpt_safe(dm_path)
ex <- read_xpt_safe(ex_path)
ds <- read_xpt_safe(ds_path)
ae <- read_xpt_safe(ae_path)
qs <- read_xpt_safe(qs_path)

# -------------------------------------- 2. Base subject record from DM
adsl_base <- dm %>%
  dplyr::select(dplyr::any_of(c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
                                "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
                                "DTHDTC","DTHFL")))

# ------------------------------------------ 3. Treatment dates -------
adsl_trt <- adsl_base %>%
  dplyr::mutate(
    TRTSDT = parse_sas_yymmdd(RFXSTDTC),
    TRTEDT = parse_sas_yymmdd(RFXENDTC),
    DTHDT = parse_sas_yymmdd(DTHDTC),
    TRTSDTM = parse_sas_e8601dt(RFXSTDTC),
    TRTEDTM = parse_sas_e8601dt(RFXENDTC)
  ) %>%
  dplyr::mutate(
    TRTDURD = dplyr::if_else(!is.na(TRTSDT) & !is.na(TRTEDT),
                             as.numeric(TRTEDT - TRTSDT + 1), NA_real_)
  )

# ------------------------------ 4. Population flags ------------------
if (!"EXTRT" %in% names(ex)) {
  ex_names <- names(ex)
  idx <- which(toupper(ex_names) == "EXTRT")
  if (length(idx) > 0) names(ex)[idx[1]] <- "EXTRT"
}
saf_tmp <- ex %>%
  dplyr::filter(!is_sas_missing_chr(EXTRT)) %>%
  dplyr::distinct(STUDYID, USUBJID) %>%
  dplyr::mutate(SAFFL = "Y")

adsl_pop <- adsl_trt %>%
  dplyr::left_join(saf_tmp, by = c("STUDYID","USUBJID")) %>%
  dplyr::mutate(
    SAFFL = dplyr::if_else(is.na(SAFFL) | trimws(SAFFL) == "", "N", SAFFL),
    ITTFL = dplyr::if_else(ARM %in% c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"), "Y", "N")
  )

# ------------------------------ 5. Disposition from DS ---------------
ds_sorted <- ds %>%
  dplyr::arrange(USUBJID, DSDTC)

disp_tmp <- ds_sorted %>%
  dplyr::filter(DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE") %>%
  dplyr::arrange(USUBJID, DSDTC) %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    EOSSTT = dplyr::if_else(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"),
    DCSREAS = dplyr::if_else(DSDECOD == "COMPLETED", "", DSDECOD),
    DCSREASP = dplyr::if_else(DSDECOD == "COMPLETED", "",
                              dplyr::if_else(DSDECOD == "OTHER",
                                             dplyr::coalesce(as.character(DSTERM), ""), "")),
    DCSREAS = dplyr::coalesce(DCSREAS, ""),
    DCSREASP = dplyr::coalesce(DCSREASP, "")
  ) %>%
  dplyr::select(USUBJID, EOSSTT, DCSREAS, DCSREASP)

adsl_disp <- adsl_pop %>%
  dplyr::left_join(disp_tmp, by = "USUBJID") %>%
  dplyr::mutate(
    EOSSTT = dplyr::if_else(is.na(EOSSTT) | trimws(EOSSTT) == "", "DISCONTINUED", EOSSTT),
    EOTSTT = EOSSTT
  )

# ------------------------------ 6. Age groups + numeric codes --------
adsl_age <- adsl_disp %>%
  dplyr::mutate(
    AGEGR1 = dplyr::case_when(
      !is.na(AGE) & AGE < 65 ~ "<65",
      !is.na(AGE) & AGE >= 65 & AGE <= 80 ~ "65-80",
      !is.na(AGE) & AGE > 80 ~ ">80",
      TRUE ~ NA_character_
    ),
    AGEGR1N = dplyr::case_when(
      !is.na(AGE) & AGE < 65 ~ 1,
      !is.na(AGE) & AGE >= 65 & AGE <= 80 ~ 2,
      !is.na(AGE) & AGE > 80 ~ 3,
      TRUE ~ NA_real_
    ),
    SEXN = dplyr::case_when(
      SEX == "M" ~ 1,
      SEX == "F" ~ 2,
      TRUE ~ NA_real_
    ),
    RACEN = dplyr::case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 4,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
      RACE == "MULTIPLE" ~ 6,
      TRUE ~ NA_real_
    )
  )

# ------------------------------ 7. TRT01P / TRT01A -------------------
adsl_trt01 <- adsl_age %>%
  dplyr::mutate(
    TRT01P = as.character(ARM),
    TRT01A = as.character(ACTARM)
  )

# ------------------------------ 8. Efficacy flag from QS -------------
ensure_col <- function(df, target) {
  if (target %in% names(df)) return(df)
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) > 0) {
    names(df)[idx[1]] <- target
  }
  df
}
qs <- ensure_col(qs, "QSTESTCD")
qs <- ensure_col(qs, "QSCAT")
qs <- ensure_col(qs, "QSSTRESN")
qs <- ensure_col(qs, "VISIT")
qs <- ensure_col(qs, "USUBJID")

eff_tmp <- qs %>%
  dplyr::filter(
    QSTESTCD == "ACTOT",
    QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE",
    !is.na(QSSTRESN),
    toupper(as.character(VISIT)) != "BASELINE",
    !grepl("SCREENING", toupper(as.character(VISIT)), fixed = TRUE)
  ) %>%
  dplyr::distinct(USUBJID) %>%
  dplyr::mutate(EFFFL = "Y")

adsl_eff <- adsl_trt01 %>%
  dplyr::left_join(eff_tmp, by = "USUBJID") %>%
  dplyr::mutate(
    EFFFL = dplyr::if_else(!is.na(SAFFL) & SAFFL == "Y" & !is.na(EFFFL) & EFFFL == "Y", "Y", "N")
  )

# ------------------------------ 9. Labels, formats, lengths ----------
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
                  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
                  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
                  "SAFFL","ITTFL","EFFFL")

for (col in retain_order) {
  if (!col %in% names(adsl_eff)) {
    adsl_eff[[col]] <- NA
  }
}

adam_adsl <- adsl_eff %>%
  dplyr::select(dplyr::any_of(retain_order)) %>%
  dplyr::arrange(USUBJID)

adam_adsl_out <- adam_adsl %>%
  dplyr::mutate(
    TRTSDT = format(TRTSDT, "%Y-%m-%d"),
    TRTEDT = format(TRTEDT, "%Y-%m-%d"),
    DTHDT = format(DTHDT, "%Y-%m-%d"),
    TRTSDTM = format(TRTSDTM, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    TRTEDTM = format(TRTEDTM, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  ) %>%
  dplyr::mutate(dplyr::across(c(TRTSDT, TRTEDT, DTHDT, TRTSDTM, TRTEDTM), ~ dplyr::na_if(.x, "NA")))

adam_adsl_out <- adam_adsl_out[, retain_order]

out_path <- file.path(out, "adsl.csv")
write.csv(adam_adsl_out, out_path, row.names = FALSE, na = "")

cat(paste0("NOTE: R ADSL built: ", nrow(adam_adsl_out), " subjects, ", ncol(adam_adsl_out), " variables\n"))
cat(paste0("Written to: ", out_path, "\n"))
```