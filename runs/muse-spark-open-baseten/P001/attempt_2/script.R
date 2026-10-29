# P001_adsl.sas -> R translation
# Output: adsl.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_yymmdd10 <- function(x) {
  s <- substr(x, 1, 10)
  s[is.na(s) | trimws(s) == ""] <- NA_character_
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

parse_e8601dt19 <- function(x) {
  res <- rep(as.POSIXct(NA), length(x))
  idx <- !is.na(x) & trimws(x) != ""
  if (any(idx)) {
    xx <- x[idx]
    p1 <- suppressWarnings(as.POSIXct(xx, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    need <- is.na(p1)
    if (any(need)) {
      p2 <- suppressWarnings(lubridate::ymd_hms(xx[need], quiet = TRUE, tz = "UTC"))
      need2 <- is.na(p2)
      if (any(need2)) {
        p3 <- suppressWarnings(lubridate::ymd(xx[need][need2], quiet = TRUE, tz = "UTC"))
        p3 <- as.POSIXct(p3, tz = "UTC")
        p2[need2] <- p3
      }
      p1[need] <- p2
    }
    hasT <- grepl("T", xx, fixed = TRUE)
    p1[!hasT & !is.na(p1)] <- as.POSIXct(NA)
    res[idx] <- p1
  }
  res
}

zap_haven <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      df[[nm]] <- col
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

# paths -----------------------------------------------------------------
root_sdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
dm_path <- file.path(root_sdtm, "dm.xpt")
ex_path <- file.path(root_sdtm, "ex.xpt")
ds_path <- file.path(root_sdtm, "ds.xpt")
ae_path <- file.path(root_sdtm, "ae.xpt")
qs_path <- file.path(root_sdtm, "qs.xpt")

# read ------------------------------------------------------------------
dm <- haven::read_xpt(dm_path)
ex <- haven::read_xpt(ex_path)
ds <- haven::read_xpt(ds_path)
ae <- haven::read_xpt(ae_path)
qs <- haven::read_xpt(qs_path)

dm <- zap_haven(dm)
ex <- zap_haven(ex)
ds <- zap_haven(ds)
ae <- zap_haven(ae)
qs <- zap_haven(qs)

# 2. Base subject record from DM ----------------------------------------
keep_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
               "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
               "DTHDTC","DTHFL")
keep_vars <- intersect(keep_vars, names(dm))
adsl_base <- dm[, keep_vars, drop = FALSE]

# 3. Treatment dates ----------------------------------------------------
adsl_trt <- adsl_base
adsl_trt$TRTSDT <- parse_yymmdd10(adsl_trt$RFXSTDTC)
adsl_trt$TRTSDTM <- parse_e8601dt19(adsl_trt$RFXSTDTC)
adsl_trt$TRTEDT <- parse_yymmdd10(adsl_trt$RFXENDTC)
adsl_trt$TRTEDTM <- parse_e8601dt19(adsl_trt$RFXENDTC)
adsl_trt$DTHDT <- parse_yymmdd10(adsl_trt$DTHDTC)
adsl_trt$TRTDURD <- as.numeric(adsl_trt$TRTEDT - adsl_trt$TRTSDT + 1)
adsl_trt$TRTDURD[is.na(adsl_trt$TRTSDT) | is.na(adsl_trt$TRTEDT)] <- NA_real_

# 4. Population flags ---------------------------------------------------
if ("EXTRT" %in% names(ex)) {
  ex_saf <- ex[!is.na(ex$EXTRT) & trimws(as.character(ex$EXTRT)) != "", , drop = FALSE]
} else {
  ex_saf <- ex[0, , drop = FALSE]
}
if (nrow(ex_saf) > 0 && all(c("STUDYID","USUBJID") %in% names(ex_saf))) {
  saf <- unique(ex_saf[, c("STUDYID","USUBJID"), drop = FALSE])
  saf$SAFFL <- "Y"
} else {
  saf <- data.frame(STUDYID = character(0), USUBJID = character(0), SAFFL = character(0), stringsAsFactors = FALSE)
}

adsl_pop <- dplyr::left_join(adsl_trt, saf, by = c("STUDYID","USUBJID"))
adsl_pop$SAFFL[is.na(adsl_pop$SAFFL)] <- "N"
adsl_pop$ITTFL <- ifelse(adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"), "Y", "N")
adsl_pop$ITTFL[is.na(adsl_pop$ITTFL)] <- "N"

# 5. Disposition from DS -----------------------------------------------
if (!"DSDTC" %in% names(ds)) ds$DSDTC <- NA_character_
if (!"DSCAT" %in% names(ds)) ds$DSCAT <- NA_character_
if (!"DSDECOD" %in% names(ds)) ds$DSDECOD <- NA_character_
if (!"DSTERM" %in% names(ds)) ds$DSTERM <- NA_character_

ds_sorted <- ds[order(ds$USUBJID, ifelse(is.na(ds$DSDTC), "", ds$DSDTC), na.last = FALSE), , drop = FALSE]
ds_filt <- ds_sorted[ds_sorted$DSCAT == "DISPOSITION EVENT" & ds_sorted$DSDECOD != "SCREEN FAILURE" & !is.na(ds_sorted$DSCAT) & !is.na(ds_sorted$DSDECOD), , drop = FALSE]

if (nrow(ds_filt) > 0) {
  # sort by USUBJID and DSDTC to mimic SAS BY USUBJID DSDTC and take last.USUBJID
  ds_filt <- ds_filt[order(ds_filt$USUBJID, ds_filt$DSDTC, na.last = TRUE), , drop = FALSE]
  # keep last row per USUBJID
  ds_filt <- ds_filt[!duplicated(ds_filt$USUBJID, fromLast = TRUE), , drop = FALSE]
  ds_filt$EOSSTT <- ifelse(ds_filt$DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED")
  ds_filt$DCSREAS <- ifelse(ds_filt$DSDECOD == "COMPLETED", "", ds_filt$DSDECOD)
  ds_filt$DCSREASP <- ifelse(ds_filt$DSDECOD == "COMPLETED", "",
                             ifelse(ds_filt$DSDECOD == "OTHER", ds_filt$DSTERM, ""))
  ds_filt$DCSREASP[is.na(ds_filt$DCSREASP)] <- ""
  ds_filt$DCSREAS[is.na(ds_filt$DCSREAS)] <- ""
  disp <- ds_filt[, c("USUBJID","EOSSTT","DCSREAS","DCSREASP"), drop = FALSE]
} else {
  disp <- data.frame(USUBJID = character(0), EOSSTT = character(0), DCSREAS = character(0), DCSREASP = character(0), stringsAsFactors = FALSE)
}

adsl_disp <- dplyr::left_join(adsl_pop, disp, by = "USUBJID")
adsl_disp$EOSSTT[is.na(adsl_disp$EOSSTT)] <- "DISCONTINUED"
adsl_disp$EOTSTT <- adsl_disp$EOSSTT

# 6. Age groups + numeric codes ----------------------------------------
adsl_age <- adsl_disp
adsl_age$AGEGR1 <- NA_character_
adsl_age$AGEGR1N <- NA_real_
idx1 <- !is.na(adsl_age$AGE) & adsl_age$AGE < 65
adsl_age$AGEGR1[idx1] <- "<65"
adsl_age$AGEGR1N[idx1] <- 1
idx2 <- !is.na(adsl_age$AGE) & adsl_age$AGE >= 65 & adsl_age$AGE <= 80
adsl_age$AGEGR1[idx2] <- "65-80"
adsl_age$AGEGR1N[idx2] <- 2
idx3 <- !is.na(adsl_age$AGE) & adsl_age$AGE > 80
adsl_age$AGEGR1[idx3] <- ">80"
adsl_age$AGEGR1N[idx3] <- 3

adsl_age$SEXN <- NA_real_
adsl_age$SEXN[adsl_age$SEX == "M"] <- 1
adsl_age$SEXN[adsl_age$SEX == "F"] <- 2

adsl_age$RACEN <- NA_real_
adsl_age$RACEN[adsl_age$RACE == "WHITE"] <- 1
adsl_age$RACEN[adsl_age$RACE == "BLACK OR AFRICAN AMERICAN"] <- 2
adsl_age$RACEN[adsl_age$RACE == "ASIAN"] <- 3
adsl_age$RACEN[adsl_age$RACE == "AMERICAN INDIAN OR ALASKA NATIVE"] <- 4
adsl_age$RACEN[adsl_age$RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"] <- 5
adsl_age$RACEN[adsl_age$RACE == "MULTIPLE"] <- 6

# 7. TRT01P / TRT01A ----------------------------------------------------
adsl_trt01 <- adsl_age
adsl_trt01$TRT01P <- adsl_trt01$ARM
adsl_trt01$TRT01A <- adsl_trt01$ACTARM

# 8. Efficacy flag from QS ---------------------------------------------
if (!"QSTESTCD" %in% names(qs)) qs$QSTESTCD <- NA_character_
if (!"QSCAT" %in% names(qs)) qs$QSCAT <- NA_character_
if (!"QSSTRESN" %in% names(qs)) qs$QSSTRESN <- NA_real_
if (!"VISIT" %in% names(qs)) qs$VISIT <- NA_character_

qs_eff <- qs[qs$QSTESTCD == "ACTOT" &
               qs$QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE" &
               !is.na(qs$QSSTRESN) &
               !is.na(qs$VISIT) &
               toupper(qs$VISIT) != "BASELINE" &
               !grepl("SCREENING", toupper(qs$VISIT), fixed = TRUE), , drop = FALSE]

if (nrow(qs_eff) > 0) {
  eff <- unique(qs_eff[, "USUBJID", drop = FALSE])
  eff$EFFFL <- "Y"
} else {
  eff <- data.frame(USUBJID = character(0), EFFFL = character(0), stringsAsFactors = FALSE)
}

adsl_eff <- dplyr::left_join(adsl_trt01, eff, by = "USUBJID")
adsl_eff$EFFFL <- ifelse(!is.na(adsl_eff$SAFFL) & adsl_eff$SAFFL == "Y" & !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

# 9. Final retain order -------------------------------------------------
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
                  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
                  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
                  "SAFFL","ITTFL","EFFFL")

for (col in retain_order) {
  if (!col %in% names(adsl_eff)) adsl_eff[[col]] <- NA
}

adsl_final <- adsl_eff[, retain_order, drop = FALSE]
adsl_final <- adsl_final[order(adsl_final$USUBJID), , drop = FALSE]

for (dcol in c("TRTSDT","TRTEDT","DTHDT")) {
  if (dcol %in% names(adsl_final)) {
    if (inherits(adsl_final[[dcol]], "Date")) {
      adsl_final[[dcol]] <- format(adsl_final[[dcol]], "%Y-%m-%d")
    } else if (inherits(adsl_final[[dcol]], "POSIXct")) {
      adsl_final[[dcol]] <- format(as.Date(adsl_final[[dcol]]), "%Y-%m-%d")
    }
  }
}
for (dtcol in c("TRTSDTM","TRTEDTM")) {
  if (dtcol %in% names(adsl_final)) {
    if (inherits(adsl_final[[dtcol]], "POSIXct") || inherits(adsl_final[[dtcol]], "POSIXt")) {
      adsl_final[[dtcol]] <- format(adsl_final[[dtcol]], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    }
  }
}

# write CSV -------------------------------------------------------------
write.csv(adsl_final, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

cat(sprintf("NOTE: R ADSL built: %d subjects, %d variables\n", nrow(adsl_final), ncol(adsl_final)))
