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

parse_yymmdd10 <- function(x) {
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
  # SAS: input(RFXSTDTC, ?? e8601dt19.) -- strict 19-char ISO datetime.
  # Date-only values (10 chars) fail and yield missing (?? suppresses error).
  v <- as.character(x)
  res <- rep(as.POSIXct(NA, tz = "UTC"), length(v))
  for (i in seq_along(v)) {
    s <- v[i]
    if (is.na(s) || nchar(trimws(s)) == 0) next
    s <- trimws(s)
    if (nchar(s) < 19) next
    if (!(substr(s, 11, 11) %in% c("T"))) next
    s19 <- substr(s, 1, 19)
    dt <- suppressWarnings(as.POSIXct(s19, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    if (!is.na(dt)) res[i] <- dt
  }
  res
}

dm <- zap_all(haven::read_xpt(dm_path))
ex <- zap_all(haven::read_xpt(ex_path))
ds <- zap_all(haven::read_xpt(ds_path))
ae <- zap_all(haven::read_xpt(ae_path))
qs <- zap_all(haven::read_xpt(qs_path))

keep_base <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
               "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
               "DTHDTC","DTHFL")
for (cn in keep_base) if (!cn %in% names(dm)) dm[[cn]] <- NA
adsl_base <- dm[, keep_base, drop = FALSE]

adsl_trt <- adsl_base
adsl_trt$TRTSDT <- parse_yymmdd10(ifelse(is_miss_c(adsl_trt$RFXSTDTC), NA_character_, substr(as.character(adsl_trt$RFXSTDTC), 1, 10)))
adsl_trt$TRTEDT <- parse_yymmdd10(ifelse(is_miss_c(adsl_trt$RFXENDTC), NA_character_, substr(as.character(adsl_trt$RFXENDTC), 1, 10)))
adsl_trt$DTHDT  <- parse_yymmdd10(ifelse(is_miss_c(adsl_trt$DTHDTC),  NA_character_, substr(as.character(adsl_trt$DTHDTC),  1, 10)))
adsl_trt$TRTSDTM <- parse_e8601dt19(ifelse(is_miss_c(adsl_trt$RFXSTDTC), NA_character_, as.character(adsl_trt$RFXSTDTC)))
adsl_trt$TRTEDTM <- parse_e8601dt19(ifelse(is_miss_c(adsl_trt$RFXENDTC), NA_character_, as.character(adsl_trt$RFXENDTC)))
adsl_trt$TRTDURD <- ifelse(!is.na(adsl_trt$TRTSDT) & !is.na(adsl_trt$TRTEDT),
                           as.numeric(adsl_trt$TRTEDT - adsl_trt$TRTSDT + 1), NA_real_)

stopifnot(all(c("STUDYID","USUBJID","EXTRT") %in% names(ex)))
saf_tbl <- ex %>%
  filter(is_not_miss_c(EXTRT)) %>%
  distinct(STUDYID, USUBJID) %>%
  mutate(SAFFL = "Y")

adsl_pop <- merge(adsl_trt, saf_tbl, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
stopifnot("SAFFL" %in% names(adsl_pop))
adsl_pop$SAFFL[is_miss_c(adsl_pop$SAFFL)] <- "N"
adsl_pop$ITTFL <- ifelse(!is.na(adsl_pop$ARM) & adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"), "Y", "N")

stopifnot(all(c("USUBJID","DSDTC","DSCAT","DSDECOD") %in% names(ds)))
if (!"DSTERM" %in% names(ds)) ds$DSTERM <- NA_character_
ord <- order(ds$USUBJID, ds$DSDTC, na.last = FALSE)
ds_s <- ds[ord, , drop = FALSE]
ds_f <- ds_s[!is.na(ds_s$DSCAT) & ds_s$DSCAT == "DISPOSITION EVENT" &
               (is.na(ds_s$DSDECOD) | ds_s$DSDECOD != "SCREEN FAILURE"), , drop = FALSE]
ds_f <- ds_f[order(ds_f$USUBJID, ds_f$DSDTC, na.last = FALSE), , drop = FALSE]
if (nrow(ds_f) > 0) {
  keep_last <- !duplicated(ds_f$USUBJID, fromLast = TRUE)
  disp_tbl <- ds_f[keep_last, , drop = FALSE]
} else {
  disp_tbl <- ds_f
}
if (nrow(disp_tbl) > 0) {
  disp_tbl$EOSSTT <- ifelse(!is.na(disp_tbl$DSDECOD) & disp_tbl$DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED")
  disp_tbl$DCSREAS <- ifelse(!is.na(disp_tbl$DSDECOD) & disp_tbl$DSDECOD == "COMPLETED", "", disp_tbl$DSDECOD)
  disp_tbl$DCSREASP <- ifelse(!is.na(disp_tbl$DSDECOD) & disp_tbl$DSDECOD == "OTHER",
                           ifelse(is.na(disp_tbl$DSTERM), "", as.character(disp_tbl$DSTERM)), "")
  disp_tbl$DCSREAS[is.na(disp_tbl$DCSREAS)] <- ""
  disp_tbl$DCSREASP[is.na(disp_tbl$DCSREASP)] <- ""
  disp_tbl <- disp_tbl[, c("USUBJID","EOSSTT","DCSREAS","DCSREASP"), drop = FALSE]
} else {
  disp_tbl <- data.frame(USUBJID=character(0), EOSSTT=character(0), DCSREAS=character(0), DCSREASP=character(0), stringsAsFactors=FALSE)
}

adsl_disp <- merge(adsl_pop, disp_tbl, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot(all(c("EOSSTT") %in% names(adsl_disp)))
adsl_disp$EOSSTT[is_miss_c(adsl_disp$EOSSTT)] <- "DISCONTINUED"
if (!"DCSREAS" %in% names(adsl_disp)) adsl_disp$DCSREAS <- NA_character_
if (!"DCSREASP" %in% names(adsl_disp)) adsl_disp$DCSREASP <- NA_character_
adsl_disp$DCSREAS[is.na(adsl_disp$DCSREAS)] <- ""
adsl_disp$DCSREASP[is.na(adsl_disp$DCSREASP)] <- ""
adsl_disp$EOTSTT <- adsl_disp$EOSSTT

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

adsl_trt01 <- adsl_age
adsl_trt01$TRT01P <- as.character(adsl_trt01$ARM)
adsl_trt01$TRT01A <- as.character(adsl_trt01$ACTARM)

stopifnot(all(c("USUBJID","QSTESTCD","QSCAT","QSSTRESN","VISIT") %in% names(qs)))
eff_tbl <- qs %>%
  filter(!is.na(QSTESTCD) & QSTESTCD == "ACTOT",
         !is.na(QSCAT) & QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE",
         !is.na(QSSTRESN),
         !is_miss_c(VISIT),
         toupper(as.character(VISIT)) != "BASELINE",
         !grepl("SCREENING", toupper(as.character(VISIT)), fixed = TRUE)) %>%
  distinct(USUBJID) %>%
  mutate(EFFFL = "Y")

adsl_eff <- merge(adsl_trt01, eff_tbl, by = "USUBJID", all.x = TRUE, sort = FALSE)
if (!"EFFFL" %in% names(adsl_eff)) adsl_eff$EFFFL <- NA_character_
adsl_eff$EFFFL <- ifelse(!is.na(adsl_eff$SAFFL) & adsl_eff$SAFFL == "Y" &
                           !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

retain_order <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
  "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
  "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
  "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
  "SAFFL","ITTFL","EFFFL")
for (cn in retain_order) if (!cn %in% names(adsl_eff)) adsl_eff[[cn]] <- NA
adsl <- adsl_eff[, retain_order, drop = FALSE]
adsl <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]

char_cols <- names(adsl)[sapply(adsl, is.character)]
for (cn in char_cols) {
  v <- adsl[[cn]]
  v[!is.na(v) & grepl("^[[:space:]]*$", v)] <- NA_character_
  adsl[[cn]] <- v
}

adsl_out <- adsl
for (cn in c("TRTSDT","TRTEDT","DTHDT")) {
  if (inherits(adsl_out[[cn]], "Date")) {
    adsl_out[[cn]] <- format(adsl_out[[cn]], format = "%Y-%m-%d")
    adsl_out[[cn]][adsl_out[[cn]] %in% c("NA", "NA-NA-NA")] <- NA_character_
  } else if (is.numeric(adsl_out[[cn]])) {
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
