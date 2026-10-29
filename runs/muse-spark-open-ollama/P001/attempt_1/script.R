# P001_adsl.sas -> R translation
# Output: adsl.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS behavior) - not strictly needed here but included
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS missing check for character
is_sas_missing_chr <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
is_sas_missing <- function(x) {
  if (is.character(x)) is_sas_missing_chr(x) else is.na(x)
}

# helper: parse SAS yymmdd10. (first 10 chars YYYY-MM-DD)
parse_sas_yymmdd <- function(x) {
  # x is character vector, may be NA/""
  # SAS: input(substr(x,1,10), yymmdd10.)
  s <- substr(as.character(x), 1, 10)
  s[is_sas_missing_chr(s)] <- NA_character_
  # use as.Date with format, suppress warnings
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

# helper: parse SAS e8601dt19. (ISO8601 datetime)
parse_sas_e8601dt <- function(x) {
  # SAS: input(x, ?? e8601dt19.) -> returns datetime or missing if invalid
  # Try to parse ISO8601 datetime; return POSIXct or NA
  x_chr <- as.character(x)
  x_chr[is_sas_missing_chr(x_chr)] <- NA_character_
  # Use lubridate parsing with quiet
  # Try ymd_hms, ymd_hm, ymd
  res <- suppressWarnings(lubridate::ymd_hms(x_chr, quiet = TRUE, tz = "UTC"))
  # For those still NA, try ymd_hm
  idx <- is.na(res) & !is.na(x_chr)
  if (any(idx)) {
    res2 <- suppressWarnings(lubridate::ymd_hm(x_chr[idx], quiet = TRUE, tz = "UTC"))
    res[idx] <- res2
  }
  idx2 <- is.na(res) & !is.na(x_chr)
  if (any(idx2)) {
    # try ymd (date only) -> convert to POSIXct at midnight
    res3 <- suppressWarnings(lubridate::ymd(x_chr[idx2], quiet = TRUE, tz = "UTC"))
    # res3 is Date; convert to POSIXct
    res[idx2] <- as.POSIXct(res3, tz = "UTC")
  }
  # Also handle ISO with T separator already handled by ymd_hms
  # If still NA, try parse_date_time with multiple orders
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

# ---------------------------------------------------------- libraries --
# Resolve input directories (absolute paths from SAS program)
root_default <- "D:/meta_hackathon/sas2r-agent"
# Allow override via env vars if present
env_root <- Sys.getenv("root")
if (env_root != "" && !is.na(env_root)) root_default <- env_root
env_sdtm <- Sys.getenv("sdtm")
env_qsraw <- Sys.getenv("qsraw")
env_rawsdtm <- Sys.getenv("rawsdtm")
env_rawadam <- Sys.getenv("rawadam")

sdtm_dir <- file.path(root_default, "data/raw/cdisc-pilot/sdtm")
qsraw_dir <- file.path(root_default, "data/raw/cdisc-pilot/sdtm")
if (env_sdtm != "" && dir.exists(env_sdtm)) sdtm_dir <- env_sdtm
if (env_qsraw != "" && dir.exists(env_qsraw)) qsraw_dir <- env_qsraw
if (env_rawsdtm != "" && dir.exists(env_rawsdtm)) sdtm_dir <- env_rawsdtm

# Fallback: try alternative locations if primary not found
candidate_dirs <- unique(c(sdtm_dir, qsraw_dir,
                           "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
                           "data/raw/cdisc-pilot/sdtm",
                           "./data/raw/cdisc-pilot/sdtm"))

find_xpt <- function(fname, dirs) {
  for (d in dirs) {
    p <- file.path(d, fname)
    if (file.exists(p)) return(p)
    # also try lowercase
    p2 <- file.path(d, tolower(fname))
    if (file.exists(p2)) return(p2)
  }
  # last resort: search recursively under root_default
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
  # zap labels/formats but keep values
  df <- haven::zap_formats(df)
  # haven::zap_labels may remove labels; keep data as is
  # Convert labelled to plain
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), function(col) {
    if (haven::is.labelled(col)) {
      # for labelled, convert to appropriate type
      # haven::as_factor not needed; keep underlying values
      col <- haven::zap_labels(col)
      col <- haven::zap_label(col)
    }
    col
  }))
  # Ensure tibble to data.frame
  as.data.frame(df, stringsAsFactors = FALSE)
}

dm <- read_xpt_safe(dm_path)
ex <- read_xpt_safe(ex_path)
ds <- read_xpt_safe(ds_path)
ae <- read_xpt_safe(ae_path)
qs <- read_xpt_safe(qs_path)

# Ensure character columns are character (haven may give character already)
# Trim not needed; keep as is for SAS semantics (case-sensitive)

# -------------------------------------- 2. Base subject record from DM
# keep STUDYID USUBJID SUBJID SITEID AGE AGEU SEX RACE ETHNIC COUNTRY
#      ARM ARMCD ACTARM ACTARMCD RFSTDTC RFENDTC RFXSTDTC RFXENDTC
#      DTHDTC DTHFL
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
# SAFFL = Y if subject has >=1 EX record with non-missing EXTRT
# (Placebo has EXDOSE=0, so use EXTRT not EXDOSE)
# proc sql create _saf as select distinct STUDYID, USUBJID, "Y" as SAFFL where not missing(EXTRT)
if (!"EXTRT" %in% names(ex)) {
  # fallback: if EXTRT not present, try EXTRT column case-insensitive
  # search for column that matches EXTRT ignoring case
  ex_names <- names(ex)
  idx <- which(toupper(ex_names) == "EXTRT")
  if (length(idx) > 0) names(ex)[idx[1]] <- "EXTRT"
}
_saf <- ex %>%
  dplyr::filter(!is_sas_missing_chr(EXTRT)) %>%
  dplyr::distinct(STUDYID, USUBJID) %>%
  dplyr::mutate(SAFFL = "Y")

adsl_pop <- adsl_trt %>%
  dplyr::left_join(_saf, by = c("STUDYID","USUBJID")) %>%
  dplyr::mutate(
    SAFFL = dplyr::if_else(is.na(SAFFL) | trimws(SAFFL) == "", "N", SAFFL),
    ITTFL = dplyr::if_else(ARM %in% c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"), "Y", "N")
  )

# ------------------------------ 5. Disposition from DS ---------------
# proc sort data=ds; by USUBJID DSDTC; run;
# data _disp; set ds; by USUBJID DSDTC; where DSCAT="DISPOSITION EVENT" and DSDECOD ne "SCREEN FAILURE";
# if last.USUBJID; ...
ds_sorted <- ds %>%
  dplyr::arrange(USUBJID, DSDTC)

_disp <- ds_sorted %>%
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
  dplyr::left_join(_disp, by = "USUBJID") %>%
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
# EFFFL = Y if SAFFL=Y AND >=1 post-baseline ACTOT score in QS.
# where QSTESTCD="ACTOT" and QSCAT="ALZHEIMER'S DISEASE ASSESSMENT SCALE"
# and not missing(QSSTRESN) and upcase(VISIT) ne "BASELINE" and not index(upcase(VISIT),"SCREENING")
# Ensure QS columns exist (case-sensitive)
# Normalize column names to upper case for filtering if needed
# QS variables are expected as QSTESTCD, QSCAT, QSSTRESN, VISIT, USUBJID
qs_cols <- names(qs)
# Ensure we have correct case; if not, try to find case-insensitive
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

_eff <- qs %>%
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
  dplyr::left_join(_eff, by = "USUBJID") %>%
  dplyr::mutate(
    EFFFL = dplyr::if_else(!is.na(SAFFL) & SAFFL == "Y" & !is.na(EFFFL) & EFFFL == "Y", "Y", "N")
  )

# ------------------------------ 9. Labels, formats, lengths ----------
# retain order:
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
                  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
                  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
                  "SAFFL","ITTFL","EFFFL")

# Ensure all retain columns exist; if missing, create as NA
for (col in retain_order) {
  if (!col %in% names(adsl_eff)) {
    adsl_eff[[col]] <- NA
  }
}

adam_adsl <- adsl_eff %>%
  dplyr::select(dplyr::any_of(retain_order)) %>%
  dplyr::arrange(USUBJID)

# Apply SAS-like sorting: character ordering uppercase before lowercase is default in R with C locale
# Ensure USUBJID sorting is stable

# For CSV output, format dates as YYYY-MM-DD and datetimes as YYYY-MM-DDTHH:MM:SS
# Keep original types for internal, but create formatted versions for CSV
adam_adsl_out <- adam_adsl %>%
  dplyr::mutate(
    TRTSDT = format(TRTSDT, "%Y-%m-%d"),
    TRTEDT = format(TRTEDT, "%Y-%m-%d"),
    DTHDT = format(DTHDT, "%Y-%m-%d"),
    TRTSDTM = format(TRTSDTM, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    TRTEDTM = format(TRTEDTM, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  ) %>%
  # Replace NA character strings "NA" from format with NA
  dplyr::mutate(dplyr::across(c(TRTSDT, TRTEDT, DTHDT, TRTSDTM, TRTEDTM), ~ dplyr::na_if(.x, "NA")))

# Ensure column order exactly as retain
adam_adsl_out <- adam_adsl_out[, retain_order]

# Write CSV
out_path <- file.path(out, "adsl.csv")
write.csv(adam_adsl_out, out_path, row.names = FALSE, na = "")

cat(paste0("NOTE: R ADSL built: ", nrow(adam_adsl_out), " subjects, ", ncol(adam_adsl_out), " variables\n"))
cat(paste0("Written to: ", out_path, "\n"))
