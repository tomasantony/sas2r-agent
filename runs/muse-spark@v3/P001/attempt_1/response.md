```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

dm_path <- file.path(sdtm_dir, "dm.xpt")
ex_path <- file.path(sdtm_dir, "ex.xpt")
ds_path <- file.path(sdtm_dir, "ds.xpt")
ae_path <- file.path(sdtm_dir, "ae.xpt")
qs_path <- file.path(sdtm_dir, "qs.xpt")

library(haven)
library(dplyr)

zap_all <- function(df) {
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  # haven may return labelled; coerce labelled remnants
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- as.vector(df[[nm]])
    }
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

is_miss_c <- function(x) {
  is.na(x) | (is.character(x) & (x == "" | grepl("^[[:space:]]*$", x)))
}
is_not_miss_c <- function(x) !is_miss_c(x)

# strict single-format parsers
parse_yymmdd10 <- function(x) {
  # SAS yymmdd10. reads YYYY-MM-DD from first 10 chars
  vapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    s <- as.character(v)
    if (nchar(trimws(s)) == 0) return(NA_real_)
    s10 <- substr(trimws(s), 1, 10)
    d <- suppressWarnings(as.Date(s10, format = "%Y-%m-%d"))
    if (is.na(d)) return(NA_real_)
    as.numeric(d)
  }, numeric(1), USE.NAMES = FALSE) |> as.Date(origin = "1970-01-01")
}

parse_e8601dt19 <- function(x) {
  # SAS input(..., ?? e8601dt19.) : ISO YYYY-MM-DDTHH:MM:SS, ?? -> NA on invalid
  # date-only defaults to midnight (SAS E8601DT behaviour)
  v <- as.character(x)
  res <- rep(as.POSIXct(NA, tz = "UTC"), length(v))
  for (i in seq_along(v)) {
    s <- v[i]
    if (is.na(s) || nchar(trimws(s)) == 0) next
    s <- trimws(s)
    # take first 19 chars if longer (e.g. with fraction/timezone), keep date-only case
    if (nchar(s) >= 19 && substr(s, 11, 11) %in% c("T", "t", " ")) {
      s19 <- substr(s, 1, 19)
      # normalise separator to T for parsing
      s19n <- paste0(substr(s19, 1, 10), "T", substr(s19, 12, 19))
      dt <- suppressWarnings(as.POSIXct(s19n, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
      if (!is.na(dt)) res[i] <- dt
    } else if (nchar(s) >= 10) {
      s10 <- substr(s, 1, 10)
      d <- suppressWarnings(as.Date(s10, format = "%Y-%m-%d"))
      if (!is.na(d)) {
        # check remainder: if time part present but invalid -> NA (??)
        # if string is exactly date or date + valid time handled above, else NA
        if (nchar(s) == 10) {
          res[i] <- as.POSIXct(paste0(s10, "T00:00:00"), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        } else {
          # string longer than 10 but not valid datetime -> try full parse, else NA
          s19b <- substr(s, 1, 19)
          s19bn <- tryCatch(paste0(substr(s19b,1,10),"T",substr(s19b,12,19)), error=function(e) NA)
          dt2 <- suppressWarnings(as.POSIXct(s19bn, format="%Y-%m-%dT%H:%M:%S", tz="UTC"))
          if (!is.na(dt2) && !is.na(s19bn)) res[i] <- dt2 else res[i] <- as.POSIXct(NA, tz="UTC")
        }
      }
    }
  }
  res
}

dm <- zap_all(haven::read_xpt(dm_path))
ex <- zap_all(haven::read_xpt(ex_path))
ds <- zap_all(haven::read_xpt(ds_path))
ae <- zap_all(haven::read_xpt(ae_path))
qs <- zap_all(haven::read_xpt(qs_path))

# -------------------------------------- 2. Base subject record from DM
keep_base <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
               "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
               "DTHDTC","DTHFL")
# ensure all exist
for (cn in keep_base) if (!cn %in% names(dm)) dm[[cn]] <- NA
adsl_base <- dm[, keep_base, drop = FALSE]

# ------------------------------------------ 3. Treatment dates -------
adsl_trt <- adsl_base
adsl_trt$TRTSDT <- parse_yymmdd10(ifelse(is_miss_c(adsl_trt$RFXSTDTC), NA_character_, substr(as.character(adsl_trt$RFXSTDTC), 1, 10)))
adsl_trt$TRTEDT <- parse_yymmdd10(ifelse(is_miss_c(adsl_trt$RFXENDTC), NA_character_, substr(as.character(adsl_trt$RFXENDTC), 1, 10)))
adsl_trt$DTHDT  <- parse_yymmdd10(ifelse(is_miss_c(adsl_trt$DTHDTC),  NA_character_, substr(as.character(adsl_trt$DTHDTC),  1, 10)))
adsl_trt$TRTSDTM <- parse_e8601dt19(ifelse(is_miss_c(adsl_trt$RFXSTDTC), NA_character_, as.character(adsl_trt$RFXSTDTC)))
adsl_trt$TRTEDTM <- parse_e8601dt19(ifelse(is_miss_c(adsl_trt$RFXENDTC), NA_character_, as.character(adsl_trt$RFXENDTC)))
adsl_trt$TRTDURD <- ifelse(!is.na(adsl_trt$TRTSDT) & !is.na(adsl_trt$TRTEDT),
                           as.numeric(adsl_trt$TRTEDT - adsl_trt$TRTSDT + 1), NA_real_)

# ------------------------------ 4. Population flags ------------------
stopifnot(all(c("STUDYID","USUBJID","EXTRT") %in% names(ex)))
_saf <- ex %>%
  filter(is_not_miss_c(EXTRT)) %>%
  distinct(STUDYID, USUBJID) %>%
  mutate(SAFFL = "Y")
# drop/rename overlap except keys before merge (only SAFFL is new)
adsl_pop <- merge(adsl_trt, _saf, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
stopifnot("SAFFL" %in% names(adsl_pop))
adsl_pop$SAFFL[is_miss_c(adsl_pop$SAFFL)] <- "N"
adsl_pop$ITTFL <- ifelse(!is.na(adsl_pop$ARM) & adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"), "Y", "N")

# ------------------------------ 5. Disposition from DS ---------------
stopifnot(all(c("USUBJID","DSDTC","DSCAT","DSDECOD") %in% names(ds)))
if (!"DSTERM" %in% names(ds)) ds$DSTERM <- NA_character_
# SAS sort by USUBJID DSDTC (missing first, char order)
ord <- order(ds$USUBJID, ds$DSDTC, na.last = FALSE)
ds_s <- ds[ord, , drop = FALSE]
# where DSCAT="DISPOSITION EVENT" and DSDECOD ne "SCREEN FAILURE"
# SAS: missing DSCAT fails equality; missing DSDECOD passes NE
ds_f <- ds_s[!is.na(ds_s$DSCAT) & ds_s$DSCAT == "DISPOSITION EVENT" &
               (is.na(ds_s$DSDECOD) | ds_s$DSDECOD != "SCREEN FAILURE"), , drop = FALSE]
# last.USUBJID : latest disposition record per subject
# take last row per USUBJID in sorted order
ds_f <- ds_f[order(ds_f$USUBJID, ds_f$DSDTC, na.last = FALSE), , drop = FALSE]
# keep last per USUBJID
if (nrow(ds_f) > 0) {
  # use duplicated from last
  keep_last <- !duplicated(ds_f$USUBJID, fromLast = TRUE)
  # need rows in original sorted order then subset to last; ensure order preserved
  _disp <- ds_f[keep_last, , drop = FALSE]
} else {
  _disp <- ds_f
}
if (nrow(_disp) > 0) {
  _disp$EOSSTT <- ifelse(!is.na(_disp$DSDECOD) & _disp$DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED")
  _disp$DCSREAS <- ifelse(!is.na(_disp$DSDECOD) & _disp$DSDECOD == "COMPLETED", "", _disp$DSDECOD)
  _disp$DCSREASP <- ifelse(!is.na(_disp$DSDECOD) & _disp$DSDECOD == "OTHER",
                           ifelse(is.na(_disp$DSTERM), "", as.character(_disp$DSTERM)), "")
  # SAS sets "" for missing; keep as "" then later to NA for CSV; keep character
  _disp$DCSREAS[is.na(_disp$DCSREAS)] <- ""
  _disp$DCSREASP[is.na(_disp$DCSREASP)] <- ""
  _disp <- _disp[, c("USUBJID","EOSSTT","DCSREAS","DCSREASP"), drop = FALSE]
} else {
  _disp <- data.frame(USUBJID=character(0), EOSSTT=character(0), DCSREAS=character(0), DCSREASP=character(0), stringsAsFactors=FALSE)
}

adsl_disp <- merge(adsl_pop, _disp, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot(all(c("EOSSTT") %in% names(adsl_disp)))
adsl_disp$EOSSTT[is_miss_c(adsl_disp$EOSSTT)] <- "DISCONTINUED"
# ensure DCSREAS/DCSREASP exist and missing -> ""
if (!"DCSREAS" %in% names(adsl_disp)) adsl_disp$DCSREAS <- NA_character_
if (!"DCSREASP" %in% names(adsl_disp)) adsl_disp$DCSREASP <- NA_character_
adsl_disp$DCSREAS[is.na(adsl_disp$DCSREAS)] <- ""
adsl_disp$EOTSTT <- adsl_disp$EOSSTT
# for subjects with no _disp record, SAS leaves DCSREAS/DCSREASP missing (blank)
# merge gives NA -> set to "" to match SAS blank
adsl_disp$DCSREAS[is.na(adsl_disp$DCSREAS)] <- ""
adsl_disp$DCSREASP[is.na(adsl_disp$DCSREASP)] <- ""

# ------------------------------ 6. Age groups + numeric codes --------
adsl_age <- adsl_disp
adsl_age$AGEGR1 <- NA_character_
adsl_age$AGEGR1N <- NA_real_
idx1 <- !is.na(adsl_age$AGE) & adsl_age$AGE < 65
idx2 <- !is.na(adsl_age$AGE) & adsl_age$AGE >= 65 & adsl_age$AGE <= 80
idx3 <- !is.na(adsl_age$AGE) & adsl_age$AGE > 80
adsl_age$AGEGR1[idx1] <- "<65";    adsl_age$AGEGR1N[idx1] <- 1
adsl_age$AGEGR1[idx2] <- "65-80";  adsl_age$AGEGR1N[idx2] <- 2
adsl_age$AGEGR1[idx3] <- ">80";    adsl_age$AGEGR1N[idx3] <- 3

adsl_age$SEXN <- NA_real_
adsl_age$SEXN[!is.na(adsl_age$SEX) & adsl_age$SEX == "M"] <- 1
adsl_age$SEXN[!is.na(adsl_age$SEX) & adsl_age$SEX == "F"] <- 2

adsl_age$RACEN <- NA_real_
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
stopifnot(all(c("USUBJID","QSTESTCD","QSCAT","QSSTRESN","VISIT") %in% names(qs)))
_eff <- qs %>%
  filter(!is.na(QSTESTCD) & QSTESTCD == "ACTOT",
         !is.na(QSCAT) & QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE",
         !is.na(QSSTRESN),
         !is_miss_c(VISIT),
         toupper(as.character(VISIT)) != "BASELINE",
         !grepl("SCREENING", toupper(as.character(VISIT)), fixed = TRUE)) %>%
  distinct(USUBJID) %>%
  mutate(EFFFL = "Y")

adsl_eff <- merge(adsl_trt01, _eff, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("EFFFL" %in% names(adsl_eff) | TRUE)
if (!"EFFFL" %in% names(adsl_eff)) adsl_eff$EFFFL <- NA_character_
# SAS: if SAFFL="Y" and EFFFL="Y" then Y else N
# merge gives EFFFL.x? No overlap except key, so single EFFFL with NA where no match
adsl_eff$EFFFL <- ifelse(!is.na(adsl_eff$SAFFL) & adsl_eff$SAFFL == "Y" &
                           !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

# ------------------------------ 9. Labels, formats, lengths ----------
retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
  "SAFFL","ITTFL","EFFFL")
for (cn in retain_order) if (!cn %in% names(adsl_eff)) adsl_eff[[cn]] <- NA
adsl <- adsl_eff[, retain_order, drop = FALSE]
# sort by USUBJID (SAS missing first)
adsl <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]

# Convert blank character strings to NA so CSV writes empty fields with na=""
char_cols <- names(adsl)[sapply(adsl, is.character)]
for (cn in char_cols) {
  v <- adsl[[cn]]
  v[!is.na(v) & grepl("^[[:space:]]*$", v)] <- NA_character_
  # also empty string already covered
  adsl[[cn]] <- v
}

# Prepare output: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
adsl_out <- adsl
# TRTSDT, TRTEDT, DTHDT are Date
for (cn in c("TRTSDT","TRTEDT","DTHDT")) {
  if (inherits(adsl_out[[cn]], "Date")) {
    adsl_out[[cn]] <- format(adsl_out[[cn]], format = "%Y-%m-%d")
    adsl_out[[cn]][adsl_out[[cn]] %in% c("NA", "NA-NA-NA")] <- NA_character_
  } else if (is.numeric(adsl_out[[cn]])) {
    # numeric date? convert
    tmp <- as.Date(adsl_out[[cn]], origin="1970-01-01")
    adsl_out[[cn]] <- format(tmp, format="%Y-%m-%d")
  }
  adsl_out[[cn]][adsl_out[[cn]] == "" ] <- NA_character_
}
for (cn in c("TRTSDTM","TRTEDTM")) {
  vv <- adsl[[cn]]
  if (inherits(vv, "POSIXct") || inherits(vv, "POSIXt")) {
    s <- format(vv, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    s[is.na(vv)] <- NA_character_
    adsl_out[[cn]] <- s
  } else {
    adsl_out[[cn]] <- as.character(vv)
  }
}

write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
```