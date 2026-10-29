```r
# P001_adsl.sas -> R translation
# Output: adsl.csv

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

dm <- haven::read_xpt(file.path(sdtm_dir, "dm.xpt"))
ex <- haven::read_xpt(file.path(sdtm_dir, "ex.xpt"))
ds <- haven::read_xpt(file.path(sdtm_dir, "ds.xpt"))
ae <- haven::read_xpt(file.path(sdtm_dir, "ae.xpt"))
qs <- haven::read_xpt(file.path(sdtm_dir, "qs.xpt"))

# strip haven labels/formats to plain vectors
zap_all <- function(df) {
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  as.data.frame(df, stringsAsFactors = FALSE)
}
dm <- zap_all(dm)
ex <- zap_all(ex)
ds <- zap_all(ds)
ae <- zap_all(ae)
qs <- zap_all(qs)

is_missing_char <- function(x) {
  is.na(x) | trimws(as.character(x), which = "both") == ""
}

parse_yymmdd_vec <- function(x) {
  suppressWarnings(as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d"))
}

parse_e8601dt19_vec <- function(x) {
  xc <- as.character(x)
  res <- as.POSIXct(rep(NA_real_, length(xc)), origin = "1970-01-01", tz = "UTC")
  ok <- !is.na(xc) & grepl("^[ ]*[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", trimws(xc))
  if (any(ok)) {
    s <- trimws(xc[ok])
    res[ok] <- suppressWarnings(as.POSIXct(substr(s, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  res
}

# -------------------------------------- 2. Base subject record from DM
adsl_base <- dm[, c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE",
                    "ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD",
                    "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","DTHDTC","DTHFL")]

# ------------------------------------------ 3. Treatment dates -------
adsl_trt <- adsl_base
adsl_trt$TRTSDT <- as.Date(NA)
adsl_trt$TRTEDT <- as.Date(NA)
adsl_trt$DTHDT <- as.Date(NA)
adsl_trt$TRTSDTM <- as.POSIXct(rep(NA_real_, nrow(adsl_trt)), origin = "1970-01-01", tz = "UTC")
adsl_trt$TRTEDTM <- as.POSIXct(rep(NA_real_, nrow(adsl_trt)), origin = "1970-01-01", tz = "UTC")
adsl_trt$TRTDURD <- NA_real_

idx1 <- !is_missing_char(adsl_trt$RFXSTDTC)
adsl_trt$TRTSDT[idx1] <- parse_yymmdd_vec(adsl_trt$RFXSTDTC[idx1])
adsl_trt$TRTSDTM[idx1] <- parse_e8601dt19_vec(adsl_trt$RFXSTDTC[idx1])

idx2 <- !is_missing_char(adsl_trt$RFXENDTC)
adsl_trt$TRTEDT[idx2] <- parse_yymmdd_vec(adsl_trt$RFXENDTC[idx2])
adsl_trt$TRTEDTM[idx2] <- parse_e8601dt19_vec(adsl_trt$RFXENDTC[idx2])

idx3 <- !is_missing_char(adsl_trt$DTHDTC)
adsl_trt$DTHDT[idx3] <- parse_yymmdd_vec(adsl_trt$DTHDTC[idx3])

both <- !is.na(adsl_trt$TRTSDT) & !is.na(adsl_trt$TRTEDT)
adsl_trt$TRTDURD[both] <- as.numeric(adsl_trt$TRTEDT[both] - adsl_trt$TRTSDT[both] + 1)

# ------------------------------ 4. Population flags ------------------
# SAFFL = Y if >=1 EX record with non-missing EXTRT
saf_idx <- !is_missing_char(ex$EXTRT)
saf <- ex[saf_idx, c("STUDYID","USUBJID"), drop = FALSE]
saf <- saf[!duplicated(saf[, c("STUDYID","USUBJID")]), , drop = FALSE]
saf$SAFFL <- "Y"

adsl_pop <- merge(adsl_trt, saf, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
adsl_pop$SAFFL[is_missing_char(adsl_pop$SAFFL)] <- "N"
adsl_pop$ITTFL <- ifelse(!is.na(adsl_pop$ARM) &
                           adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"),
                         "Y", "N")

# ------------------------------ 5. Disposition from DS ---------------
ds$USUBJID_c <- ifelse(is.na(ds$USUBJID), "", as.character(ds$USUBJID))
ds$DSDTC_c <- ifelse(is.na(ds$DSDTC), "", as.character(ds$DSDTC))
ord_ds <- order(ds$USUBJID_c, ds$DSDTC_c, method = "radix")
ds_sorted <- ds[ord_ds, , drop = FALSE]

filt <- !is.na(ds_sorted$DSCAT) & ds_sorted$DSCAT == "DISPOSITION EVENT" &
  (is_missing_char(ds_sorted$DSDECOD) | (!is.na(ds_sorted$DSDECOD) & ds_sorted$DSDECOD != "SCREEN FAILURE"))
ds_f <- ds_sorted[filt, , drop = FALSE]

if (nrow(ds_f) > 0) {
  ord_f <- order(ifelse(is.na(ds_f$USUBJID), "", as.character(ds_f$USUBJID)),
                 ifelse(is.na(ds_f$DSDTC), "", as.character(ds_f$DSDTC)),
                 method = "radix")
  ds_f <- ds_f[ord_f, , drop = FALSE]
  keep <- !duplicated(ds_f$USUBJID, fromLast = TRUE)
  disp <- ds_f[keep, , drop = FALSE]
} else {
  disp <- ds_f
}

if (nrow(disp) > 0) {
  disp$EOSSTT <- ifelse(!is.na(disp$DSDECOD) & disp$DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED")
  disp$DCSREAS <- ifelse(!is.na(disp$DSDECOD) & disp$DSDECOD == "COMPLETED", "",
                          ifelse(is.na(disp$DSDECOD), "", as.character(disp$DSDECOD)))
  disp$DCSREASP <- ifelse(!is.na(disp$DSDECOD) & disp$DSDECOD == "COMPLETED", "",
                           ifelse(!is.na(disp$DSDECOD) & disp$DSDECOD == "OTHER",
                                  ifelse(is.na(disp$DSTERM), "", as.character(disp$DSTERM)), ""))
  disp <- disp[, c("USUBJID","EOSSTT","DCSREAS","DCSREASP"), drop = FALSE]
} else {
  disp <- data.frame(USUBJID = character(0), EOSSTT = character(0),
                      DCSREAS = character(0), DCSREASP = character(0),
                      stringsAsFactors = FALSE)
}

adsl_disp <- merge(adsl_pop, disp, by = "USUBJID", all.x = TRUE, sort = FALSE)
adsl_disp$EOSSTT[is_missing_char(adsl_disp$EOSSTT)] <- "DISCONTINUED"
adsl_disp$EOTSTT <- adsl_disp$EOSSTT

# ------------------------------ 6. Age groups + numeric codes --------
adsl_age <- adsl_disp
adsl_age$AGEGR1 <- NA_character_
adsl_age$AGEGR1N <- NA_real_
adsl_age$SEXN <- NA_real_
adsl_age$RACEN <- NA_real_

idx_a1 <- !is.na(adsl_age$AGE) & adsl_age$AGE < 65
idx_a2 <- !is.na(adsl_age$AGE) & adsl_age$AGE >= 65 & adsl_age$AGE <= 80
idx_a3 <- !is.na(adsl_age$AGE) & adsl_age$AGE > 80
adsl_age$AGEGR1[idx_a1] <- "<65"
adsl_age$AGEGR1N[idx_a1] <- 1
adsl_age$AGEGR1[idx_a2] <- "65-80"
adsl_age$AGEGR1N[idx_a2] <- 2
adsl_age$AGEGR1[idx_a3] <- ">80"
adsl_age$AGEGR1N[idx_a3] <- 3

adsl_age$SEXN[!is.na(adsl_age$SEX) & adsl_age$SEX == "M"] <- 1
adsl_age$SEXN[!is.na(adsl_age$SEX) & adsl_age$SEX == "F"] <- 2

adsl_age$RACEN[!is.na(adsl_age$RACE) & adsl_age$RACE == "WHITE"] <- 1
adsl_age$RACEN[!is.na(adsl_age$RACE) & adsl_age$RACE == "BLACK OR AFRICAN AMERICAN"] <- 2
adsl_age$RACEN[!is.na(adsl_age$RACE) & adsl_age$RACE == "ASIAN"] <- 3
adsl_age$RACEN[!is.na(adsl_age$RACE) & adsl_age$RACE == "AMERICAN INDIAN OR ALASKA NATIVE"] <- 4
adsl_age$RACEN[!is.na(adsl_age$RACE) & adsl_age$RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"] <- 5
adsl_age$RACEN[!is.na(adsl_age$RACE) & adsl_age$RACE == "MULTIPLE"] <- 6

# ------------------------------ 7. TRT01P / TRT01A -------------------
adsl_trt01 <- adsl_age
adsl_trt01$TRT01P <- as.character(adsl_trt01$ARM)
adsl_trt01$TRT01A <- as.character(adsl_trt01$ACTARM)

# ------------------------------ 8. Efficacy flag from QS -------------
qs$VISIT_up <- toupper(ifelse(is.na(qs$VISIT), "", as.character(qs$VISIT)))
eff_filt <- !is.na(qs$QSTESTCD) & qs$QSTESTCD == "ACTOT" &
  !is.na(qs$QSCAT) & qs$QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE" &
  !is.na(qs$QSSTRESN) &
  qs$VISIT_up != "BASELINE" &
  !grepl("SCREENING", qs$VISIT_up, fixed = TRUE)
qs_f <- qs[eff_filt & !is.na(qs$USUBJID), , drop = FALSE]
eff <- data.frame(USUBJID = unique(as.character(qs_f$USUBJID)), stringsAsFactors = FALSE)
if (nrow(eff) > 0) eff$EFFFL <- "Y" else eff$EFFFL <- character(0)

adsl_eff <- merge(adsl_trt01, eff, by = "USUBJID", all.x = TRUE, sort = FALSE)
adsl_eff$EFFFL <- ifelse(!is.na(adsl_eff$SAFFL) & adsl_eff$SAFFL == "Y" &
                           !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

# ------------------------------ 9. Labels, formats, lengths ----------
retain_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N",
                 "SEX","SEXN","RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD",
                 "ACTARM","ACTARMCD","TRT01P","TRT01A","RFSTDTC","RFENDTC",
                 "RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                 "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP",
                 "EOSSTT","EOTSTT","SAFFL","ITTFL","EFFFL")

for (v in retain_vars) {
  if (!v %in% names(adsl_eff)) adsl_eff[[v]] <- NA
}
adsl <- adsl_eff[, retain_vars, drop = FALSE]

ord_final <- order(ifelse(is.na(adsl$USUBJID), "", as.character(adsl$USUBJID)), method = "radix")
adsl <- adsl[ord_final, , drop = FALSE]
row.names(adsl) <- NULL

adsl_out <- adsl
adsl_out$TRTSDT <- format(adsl_out$TRTSDT, format = "%Y-%m-%d")
adsl_out$TRTEDT <- format(adsl_out$TRTEDT, format = "%Y-%m-%d")
adsl_out$DTHDT <- format(adsl_out$DTHDT, format = "%Y-%m-%d")
adsl_out$TRTSDTM <- format(adsl_out$TRTSDTM, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
adsl_out$TRTEDTM <- format(adsl_out$TRTEDTM, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
```