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
  # SAS yymmdd10. on substr(x,1,10)
  # x is character ISO date/datetime
  s <- substr(x, 1, 10)
  # treat missing / blank as NA
  s[is.na(s) | trimws(s) == ""] <- NA_character_
  # as.Date with ymd; invalid -> NA
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

parse_e8601dt19 <- function(x) {
  # SAS e8601dt19. with ?? : invalid -> NA, no error
  # Expected "YYYY-MM-DDThh:mm:ss" optionally with timezone
  # Use as.POSIXct with format, fallback to lubridate
  res <- rep(as.POSIXct(NA), length(x))
  idx <- !is.na(x) & trimws(x) != ""
  if (any(idx)) {
    xx <- x[idx]
    # try strict format first
    p1 <- suppressWarnings(as.POSIXct(xx, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    # for those still NA, try lubridate ymd_hms (handles variations)
    need <- is.na(p1)
    if (any(need)) {
      p2 <- suppressWarnings(lubridate::ymd_hms(xx[need], quiet = TRUE, tz = "UTC"))
      # ymd_hms returns POSIXct; if still NA try ymd
      need2 <- is.na(p2)
      if (any(need2)) {
        p3 <- suppressWarnings(lubridate::ymd(xx[need][need2], quiet = TRUE, tz = "UTC"))
        p3 <- as.POSIXct(p3, tz = "UTC")
        p2[need2] <- p3
      }
      p1[need] <- p2
    }
    # also try date-only -> midnight (SAS would treat as missing for datetime, but keep as NA if no time)
    # SAS e8601dt19. requires time; if input is only date, it would be missing.
    # Detect inputs where nchar < 11 and p1 is not NA due to ymd fallback -> set to NA
    # We keep the fallback only if original string contains "T"
    hasT <- grepl("T", xx, fixed = TRUE)
    p1[!hasT & !is.na(p1)] <- as.POSIXct(NA)
    res[idx] <- p1
  }
  res
}

zap_haven <- function(df) {
  # convert haven_labelled to plain R types
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      # zap labels and formats
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      df[[nm]] <- col
    }
    # haven labelled character may still be class haven_labelled; ensure character
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

# paths -----------------------------------------------------------------
root_sdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
# files: dm.xpt, ex.xpt, ds.xpt, ae.xpt, qs.xpt
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

# ensure character columns are character (haven may give character already)
# 2. Base subject record from DM ----------------------------------------
keep_vars <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
               "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
               "DTHDTC","DTHFL")
# keep only those that exist
keep_vars <- intersect(keep_vars, names(dm))
adsl_base <- dm[, keep_vars, drop = FALSE]

# 3. Treatment dates ----------------------------------------------------
adsl_trt <- adsl_base

# TRTSDT / TRTSDTM from RFXSTDTC
adsl_trt$TRTSDT <- parse_yymmdd10(adsl_trt$RFXSTDTC)
adsl_trt$TRTSDTM <- parse_e8601dt19(adsl_trt$RFXSTDTC)

adsl_trt$TRTEDT <- parse_yymmdd10(adsl_trt$RFXENDTC)
adsl_trt$TRTEDTM <- parse_e8601dt19(adsl_trt$RFXENDTC)

adsl_trt$DTHDT <- parse_yymmdd10(adsl_trt$DTHDTC)

# TRTDURD = TRTEDT - TRTSDT + 1 if both non-missing
adsl_trt$TRTDURD <- as.numeric(adsl_trt$TRTEDT - adsl_trt$TRTSDT + 1)
adsl_trt$TRTDURD[is.na(adsl_trt$TRTSDT) | is.na(adsl_trt$TRTEDT)] <- NA_real_

# 4. Population flags ---------------------------------------------------
# SAFFL = Y if subject has >=1 EX record with non-missing EXTRT
# ex$EXTRT may be character
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

# merge adsl_trt with saf by STUDYID USUBJID (left join)
adsl_pop <- dplyr::left_join(adsl_trt, saf, by = c("STUDYID","USUBJID"))
adsl_pop$SAFFL[is.na(adsl_pop$SAFFL)] <- "N"

# ITTFL
adsl_pop$ITTFL <- ifelse(adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"), "Y", "N")
# handle NA ARM -> N (SAS: else ITTFL="N")
adsl_pop$ITTFL[is.na(adsl_pop$ITTFL)] <- "N"

# 5. Disposition from DS -----------------------------------------------
# proc sort data=ds; by USUBJID DSDTC; run;
# filter and take last per USUBJID
# Ensure DSDTC exists
if (!"DSDTC" %in% names(ds)) ds$DSDTC <- NA_character_
if (!"DSCAT" %in% names(ds)) ds$DSCAT <- NA_character_
if (!"DSDECOD" %in% names(ds)) ds$DSDECOD <- NA_character_
if (!"DSTERM" %in% names(ds)) ds$DSTERM <- NA_character_

# SAS missing sorts first; R arrange puts NA last by default.
# To mimic SAS, put NA first: arrange with is.na(DSDTC) desc? Actually SAS missing first means NA smallest.
# We can create sort key where NA -> "" (smallest)
ds_sorted <- ds[order(ds$USUBJID, ifelse(is.na(ds$DSDTC), "", ds$DSDTC), na.last = FALSE), , drop = FALSE]

ds_filt <- ds_sorted[ds_sorted$DSCAT == "DISPOSITION EVENT" & ds_sorted$DSDECOD != "SCREEN FAILURE" & !is.na(ds_sorted$DSCAT) & !is.na(ds_sorted$DSDECOD), , drop = FALSE]
# also need to handle where DSCAT/DSDECOD may be NA -> excluded already
# For each USUBJID, take last row (latest DSDTC)
if (nrow(ds_filt) > 0) {
  # use dplyr to get last per USUBJID preserving order
  ds_filt <- ds_filt %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::arrange(DSDTC, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  # derive EOSSTT etc.
  ds_filt$EOSSTT <- ifelse(ds_filt$DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED")
  ds_filt$DCSREAS <- ifelse(ds_filt$DSDECOD == "COMPLETED", "", ds_filt$DSDECOD)
  ds_filt$DCSREASP <- ifelse(ds_filt$DSDECOD == "COMPLETED", "",
                             ifelse(ds_filt$DSDECOD == "OTHER", ds_filt$DSTERM, ""))
  # handle NA DSTERM
  ds_filt$DCSREASP[is.na(ds_filt$DCSREASP)] <- ""
  ds_filt$DCSREAS[is.na(ds_filt$DCSREAS)] <- ""
  disp <- ds_filt[, c("USUBJID","EOSSTT","DCSREAS","DCSREASP"), drop = FALSE]
} else {
  disp <- data.frame(USUBJID = character(0), EOSSTT = character(0), DCSREAS = character(0), DCSREASP = character(0), stringsAsFactors = FALSE)
}

adsl_disp <- dplyr::left_join(adsl_pop, disp, by = "USUBJID")
adsl_disp$EOSSTT[is.na(adsl_disp$EOSSTT)] <- "DISCONTINUED"
adsl_disp$EOTSTT <- adsl_disp$EOSSTT
# ensure DCSREAS/DCSREASP not NA for those without disp record? SAS would leave missing -> then not set, remains NA.
# But for those with no disp, EOSSTT set to DISCONTINUED, DCSREAS/DCSREASP remain NA.
# Keep as NA (will be written as "" via na="")

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
# EFFFL = Y if SAFFL=Y AND >=1 post-baseline ACTOT score in QS.
# QS filter: QSTESTCD="ACTOT" and QSCAT="ALZHEIMER'S DISEASE ASSESSMENT SCALE"
# and not missing QSSTRESN and upcase(VISIT) != "BASELINE" and not index(upcase(VISIT),"SCREENING")
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
# SAS: if SAFFL="Y" and EFFFL="Y" then EFFFL="Y" else EFFFL="N"
adsl_eff$EFFFL <- ifelse(!is.na(adsl_eff$SAFFL) & adsl_eff$SAFFL == "Y" & !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

# 9. Final retain order -------------------------------------------------
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
                  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
                  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
                  "SAFFL","ITTFL","EFFFL")

# ensure all columns exist (add NA if missing)
for (col in retain_order) {
  if (!col %in% names(adsl_eff)) adsl_eff[[col]] <- NA
}

adsl_final <- adsl_eff[, retain_order, drop = FALSE]

# sort by USUBJID (SAS proc sort)
adsl_final <- adsl_final[order(adsl_final$USUBJID), , drop = FALSE]

# format dates/datetimes for CSV as per spec: dates YYYY-MM-DD, datetimes YYYY-MM-DDTHH:MM:SS
# Keep Date objects for formatting then convert to character
# TRTSDT, TRTEDT, DTHDT are Date
for (dcol in c("TRTSDT","TRTEDT","DTHDT")) {
  if (dcol %in% names(adsl_final)) {
    # ensure Date class
    if (inherits(adsl_final[[dcol]], "Date")) {
      adsl_final[[dcol]] <- format(adsl_final[[dcol]], "%Y-%m-%d")
    } else if (inherits(adsl_final[[dcol]], "POSIXct")) {
      adsl_final[[dcol]] <- format(as.Date(adsl_final[[dcol]]), "%Y-%m-%d")
    } else {
      # already character or numeric; if numeric Date origin? SAS dates are numeric days since 1960-01-01
      # but we already have Date; leave as is
      # try to coerce if numeric
      # do nothing
    }
    # format() turns NA Date into NA_character_
  }
}
for (dtcol in c("TRTSDTM","TRTEDTM")) {
  if (dtcol %in% names(adsl_final)) {
    if (inherits(adsl_final[[dtcol]], "POSIXct") || inherits(adsl_final[[dtcol]], "POSIXt")) {
      adsl_final[[dtcol]] <- format(adsl_final[[dtcol]], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    } else {
      # if already character, keep
    }
  }
}

# write CSV -------------------------------------------------------------
write.csv(adsl_final, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

cat(sprintf("NOTE: R ADSL built: %d subjects, %d variables\n", nrow(adsl_final), ncol(adsl_final)))
