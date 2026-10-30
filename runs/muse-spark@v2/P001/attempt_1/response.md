```r
# P001 ADSL replication in R
# Mirrors SAS program P001_adsl.sas
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

clean_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_labels(col)
    }
    attr(col, "label") <- NULL
    attr(col, "format.sas") <- NULL
    attr(col, "format.spss") <- NULL
    attr(col, "display_width") <- NULL
    attr(col, "width") <- NULL
    attr(col, "labels") <- NULL
    df[[nm]] <- col
  }
  df
}

is_missing_char <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

message("Reading SDTM XPT files ...")
dm <- clean_df(haven::read_xpt(file.path(sdtm_dir, "dm.xpt")))
ex <- clean_df(haven::read_xpt(file.path(sdtm_dir, "ex.xpt")))
ds <- clean_df(haven::read_xpt(file.path(sdtm_dir, "ds.xpt")))
ae <- clean_df(haven::read_xpt(file.path(sdtm_dir, "ae.xpt")))
qs <- clean_df(haven::read_xpt(file.path(sdtm_dir, "qs.xpt")))
message(sprintf("DM=%d EX=%d DS=%d QS=%d", nrow(dm), nrow(ex), nrow(ds), nrow(qs)))

# -------------------------------------- 2. Base subject record from DM
keep_base <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE",
               "ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD",
               "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","DTHDTC","DTHFL")
for (v in keep_base) {
  if (!v %in% names(dm)) {
    stop(sprintf("DM missing expected column %s", v))
  }
}
adsl_base <- dm[, keep_base, drop = FALSE]

# ------------------------------------------ 3. Treatment dates -------
parse_yymmdd10_sub10 <- function(x) {
  # x: character vector (full DTC); mimics input(substr(x,1,10), yymmdd10.)
  sapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    s <- trimws(as.character(v))
    if (s == "") return(as.Date(NA))
    sub <- substr(s, 1, 10)
    if (nchar(sub) < 10) return(as.Date(NA))
    d <- suppressWarnings(as.Date(sub, format = "%Y-%m-%d"))
    d
  }, USE.NAMES = FALSE)
}
# as.Date numeric origin handling: sapply strips Date class -> numeric; restore
parse_yymmdd10_sub10_date <- function(x) {
  res <- parse_yymmdd10_sub10(x)
  # sapply on Date returns numeric days since 1970
  as.Date(res, origin = "1970-01-01")
}

parse_e8601dt19 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(as.numeric(NA))
    s <- trimws(as.character(v))
    if (s == "") return(as.numeric(NA))
    if (nchar(s) < 19) return(as.numeric(NA))
    sub <- substr(s, 1, 19)
    ct <- suppressWarnings(as.POSIXct(sub, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    if (is.na(ct)) return(as.numeric(NA))
    as.numeric(ct)
  }, USE.NAMES = FALSE)
}
parse_e8601dt19_ct <- function(x) {
  res <- parse_e8601dt19(x)
  as.POSIXct(res, origin = "1970-01-01", tz = "UTC")
}

adsl_trt <- adsl_base
adsl_trt$TRTSDT <- parse_yymmdd10_sub10_date(adsl_trt$RFXSTDTC)
adsl_trt$TRTSDTM <- parse_e8601dt19_ct(adsl_trt$RFXSTDTC)
adsl_trt$TRTEDT <- parse_yymmdd10_sub10_date(adsl_trt$RFXENDTC)
adsl_trt$TRTEDTM <- parse_e8601dt19_ct(adsl_trt$RFXENDTC)
adsl_trt$DTHDT <- parse_yymmdd10_sub10_date(adsl_trt$DTHDTC)
# Ensure Date class (sapply path already Date)
if (!inherits(adsl_trt$TRTSDT, "Date")) adsl_trt$TRTSDT <- as.Date(adsl_trt$TRTSDT, origin="1970-01-01")
if (!inherits(adsl_trt$TRTEDT, "Date")) adsl_trt$TRTEDT <- as.Date(adsl_trt$TRTEDT, origin="1970-01-01")
if (!inherits(adsl_trt$DTHDT, "Date")) adsl_trt$DTHDT <- as.Date(adsl_trt$DTHDT, origin="1970-01-01")
adsl_trt$TRTDURD <- ifelse(!is.na(adsl_trt$TRTSDT) & !is.na(adsl_trt$TRTEDT),
                           as.numeric(adsl_trt$TRTEDT - adsl_trt$TRTSDT + 1), NA_real_)

# ------------------------------ 4. Population flags ------------------
stopifnot("EXTRT" %in% names(ex))
stopifnot(all(c("STUDYID","USUBJID") %in% names(ex)))
saf_idx <- !is_missing_char(ex$EXTRT)
saf_sub <- ex[saf_idx, c("STUDYID","USUBJID"), drop = FALSE]
# distinct STUDYID, USUBJID
saf_sub <- saf_sub[!duplicated(saf_sub[, c("STUDYID","USUBJID")]), , drop = FALSE]
saf_sub$SAFFL <- "Y"

# merge adsl_trt(in=a) _saf(in=b) by STUDYID USUBJID ; left join
adsl_pop <- merge(adsl_trt, saf_sub, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
adsl_pop$SAFFL[is.na(adsl_pop$SAFFL)] <- "N"
adsl_pop$ITTFL <- ifelse(!is.na(adsl_pop$ARM) &
                           adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"),
                         "Y", "N")

# ------------------------------ 5. Disposition from DS ---------------
stopifnot(all(c("USUBJID","DSDTC","DSCAT","DSDECOD","DSTERM") %in% names(ds)))
# SAS sort by USUBJID DSDTC (missing first, binary order). Emulate: NA/"" first.
ds_sort <- ds
ds_sort$`_sort_dtc` <- ifelse(is.na(ds_sort$DSDTC), "", as.character(ds_sort$DSDTC))
# binary (C locale) ordering: use xtfrm with radix? Use order on character directly.
# To mimic SAS uppercase-before-lowercase, order by raw bytes via utf8ToInt ranking:
# For these ISO values simple order suffices; use order().
ord <- order(as.character(ds_sort$USUBJID), ds_sort$`_sort_dtc`, na.last = FALSE, method = "radix")
ds_sort <- ds_sort[ord, , drop = FALSE]

# where DSCAT="DISPOSITION EVENT" and DSDECOD ne "SCREEN FAILURE"
# SAS: missing DSDECOD ("") satisfies ne. R NA -> treat as "".
dscat_ok <- !is.na(ds_sort$DSCAT) & ds_sort$DSCAT == "DISPOSITION EVENT"
dsdecod_chr <- ifelse(is.na(ds_sort$DSDECOD), "", as.character(ds_sort$DSDECOD))
dsdecod_ok <- dsdecod_chr != "SCREEN FAILURE"
disp_filt <- ds_sort[dscat_ok & dsdecod_ok, , drop = FALSE]
# last.USUBJID -> latest disposition record per subject (last in sorted order)
# take last row per USUBJID
disp_filt <- disp_filt[order(as.character(disp_filt$USUBJID), disp_filt$`_sort_dtc`, na.last = FALSE, method = "radix"), , drop = FALSE]
# keep last per USUBJID
last_idx <- !duplicated(as.character(disp_filt$USUBJID), fromLast = TRUE)
disp_last <- disp_filt[last_idx, , drop = FALSE]
# build _disp
disp_last$EOSSTT <- ifelse(!is.na(disp_last$DSDECOD) & disp_last$DSDECOD == "COMPLETED",
                           "COMPLETED", "DISCONTINUED")
disp_last$DCSREAS <- ifelse(!is.na(disp_last$DSDECOD) & disp_last$DSDECOD == "COMPLETED",
                            "", ifelse(is.na(disp_last$DSDECOD), "", as.character(disp_last$DSDECOD)))
disp_last$DCSREASP <- ifelse(!is.na(disp_last$DSDECOD) & disp_last$DSDECOD == "OTHER",
                             ifelse(is.na(disp_last$DSTERM), "", as.character(disp_last$DSTERM)), "")
disp_keep <- disp_last[, c("USUBJID","EOSSTT","DCSREAS","DCSREASP"), drop = FALSE]

adsl_disp <- merge(adsl_pop, disp_keep, by = "USUBJID", all.x = TRUE, sort = FALSE)
# if missing(EOSSTT) then DISCONTINUED
need_eos <- is.na(adsl_disp$EOSSTT) | trimws(as.character(adsl_disp$EOSSTT)) == ""
# need to handle factor? ensure character
adsl_disp$EOSSTT <- as.character(adsl_disp$EOSSTT)
adsl_disp$EOSSTT[need_eos] <- "DISCONTINUED"
adsl_disp$EOTSTT <- adsl_disp$EOSSTT

# ------------------------------ 6. Age groups + numeric codes --------
adsl_disp$AGEGR1 <- NA_character_
adsl_disp$AGEGR1N <- NA_real_
age <- suppressWarnings(as.numeric(adsl_disp$AGE))
idx1 <- !is.na(age) & age < 65
idx2 <- !is.na(age) & age >= 65 & age <= 80
idx3 <- !is.na(age) & age > 80
adsl_disp$AGEGR1[idx1] <- "<65"
adsl_disp$AGEGR1N[idx1] <- 1
adsl_disp$AGEGR1[idx2] <- "65-80"
adsl_disp$AGEGR1N[idx2] <- 2
adsl_disp$AGEGR1[idx3] <- ">80"
adsl_disp$AGEGR1N[idx3] <- 3

adsl_disp$SEXN <- NA_real_
adsl_disp$SEXN[!is.na(adsl_disp$SEX) & adsl_disp$SEX == "M"] <- 1
adsl_disp$SEXN[!is.na(adsl_disp$SEX) & adsl_disp$SEX == "F"] <- 2

adsl_disp$RACEN <- NA_real_
race <- as.character(adsl_disp$RACE)
adsl_disp$RACEN[!is.na(race) & race == "WHITE"] <- 1
adsl_disp$RACEN[!is.na(race) & race == "BLACK OR AFRICAN AMERICAN"] <- 2
adsl_disp$RACEN[!is.na(race) & race == "ASIAN"] <- 3
adsl_disp$RACEN[!is.na(race) & race == "AMERICAN INDIAN OR ALASKA NATIVE"] <- 4
adsl_disp$RACEN[!is.na(race) & race == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"] <- 5
adsl_disp$RACEN[!is.na(race) & race == "MULTIPLE"] <- 6

adsl_age <- adsl_disp

# ------------------------------ 7. TRT01P / TRT01A -------------------
adsl_age$TRT01P <- as.character(adsl_age$ARM)
adsl_age$TRT01A <- as.character(adsl_age$ACTARM)
adsl_trt01 <- adsl_age

# ------------------------------ 8. Efficacy flag from QS -------------
stopifnot(all(c("USUBJID","QSTESTCD","QSCAT","QSSTRESN","VISIT") %in% names(qs)))
visit_up <- toupper(ifelse(is.na(qs$VISIT), "", as.character(qs$VISIT)))
qstest_ok <- !is.na(qs$QSTESTCD) & qs$QSTESTCD == "ACTOT"
qscat_ok <- !is.na(qs$QSCAT) & qs$QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE"
qsres_ok <- !is.na(suppressWarnings(as.numeric(qs$QSSTRESN)))
visit_ok1 <- visit_up != "BASELINE"
visit_ok2 <- !grepl("SCREENING", visit_up, fixed = TRUE)
eff_idx <- qstest_ok & qscat_ok & qsres_ok & visit_ok1 & visit_ok2
eff_sub <- qs[eff_idx, "USUBJID", drop = FALSE]
eff_sub <- eff_sub[!duplicated(eff_sub$USUBJID), , drop = FALSE]
eff_sub$EFFFL <- "Y"

adsl_eff <- merge(adsl_trt01, eff_sub, by = "USUBJID", all.x = TRUE, sort = FALSE)
adsl_eff$EFFFL <- as.character(adsl_eff$EFFFL)
adsl_eff$SAFFL <- as.character(adsl_eff$SAFFL)
adsl_eff$EFFFL <- ifelse(!is.na(adsl_eff$SAFFL) & adsl_eff$SAFFL == "Y" &
                           !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

# ------------------------------ 9. Labels, formats, lengths ----------
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N",
                  "SEX","SEXN","RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD",
                  "ACTARM","ACTARMCD","TRT01P","TRT01A","RFSTDTC","RFENDTC",
                  "RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
                  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP",
                  "EOSSTT","EOTSTT","SAFFL","ITTFL","EFFFL")
# ensure all columns exist
for (v in retain_order) {
  if (!v %in% names(adsl_eff)) adsl_eff[[v]] <- NA
}
adsl <- adsl_eff[, retain_order, drop = FALSE]
# sort by USUBJID (SAS binary order)
adsl <- adsl[order(as.character(adsl$USUBJID), na.last = TRUE, method = "radix"), , drop = FALSE]

message(sprintf("ADSL built: %d subjects, %d variables", nrow(adsl), ncol(adsl)))

# Write CSV with ISO dates/datetimes
out_df <- adsl
# Format Date columns
for (cn in c("TRTSDT","TRTEDT","DTHDT")) {
  v <- out_df[[cn]]
  if (inherits(v, "Date")) {
    out_df[[cn]] <- format(v, format = "%Y-%m-%d")
  } else if (is.numeric(v)) {
    # in case numeric days
    out_df[[cn]] <- format(as.Date(v, origin = "1970-01-01"), format = "%Y-%m-%d")
  } else {
    out_df[[cn]] <- as.character(v)
  }
}
for (cn in c("TRTSDTM","TRTEDTM")) {
  v <- out_df[[cn]]
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
    out_df[[cn]] <- format(v, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  } else if (is.numeric(v)) {
    ct <- as.POSIXct(v, origin = "1970-01-01", tz = "UTC")
    out_df[[cn]] <- format(ct, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  } else {
    out_df[[cn]] <- as.character(v)
  }
}

write.csv(out_df, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
message("Wrote adsl.csv")
```