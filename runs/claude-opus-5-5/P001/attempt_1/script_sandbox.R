# P001 - ADSL build (translation of P001_adsl.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out  <- Sys.getenv("OUT_DIR")
sdtm <- "/bench/data/raw/cdisc-pilot/sdtm"
qsraw <- sdtm

# Read an XPT file; convert NA character values to "" (SAS blank)
rd <- function(f) {
  d <- haven::read_xpt(file.path(sdtm, f))
  d <- haven::zap_labels(haven::zap_formats(d))
  d[] <- lapply(d, function(x) if (is.character(x)) { x[is.na(x)] <- ""; trimws(x, "right") } else x)
  as.data.frame(d, stringsAsFactors = FALSE)
}
cat("Reading SDTM...\n")
dm <- rd("dm.xpt")
ex <- rd("ex.xpt")
ds <- rd("ds.xpt")
qs <- haven::read_xpt(file.path(qsraw, "qs.xpt"))
qs <- as.data.frame(haven::zap_labels(haven::zap_formats(qs)))
qs[] <- lapply(qs, function(x) if (is.character(x)) { x[is.na(x)] <- ""; trimws(x, "right") } else x)

miss <- function(x) is.na(x) | x == ""

# ---------------------------------------------- 2. Base record from DM
keepv <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","SEX","RACE","ETHNIC","COUNTRY",
           "ARM","ARMCD","ACTARM","ACTARMCD","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC",
           "DTHDTC","DTHFL")
for (v in keepv) if (!v %in% names(dm)) dm[[v]] <- ""
adsl_base <- dm[, keepv]

# ---------------------------------------------- 3. Treatment dates
sas_date <- function(x) {
  s <- substr(x, 1, 10)
  ok <- !miss(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", s)
  r <- as.Date(rep(NA_character_, length(x)))
  r[ok] <- as.Date(s[ok], format = "%Y-%m-%d")
  r
}
sas_dtm <- function(x) {
  r <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  ok1 <- !miss(x) & grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$", x)
  ok2 <- !miss(x) & grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}$", x)
  r[ok1] <- as.POSIXct(x[ok1], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  r[ok2] <- as.POSIXct(x[ok2], format = "%Y-%m-%dT%H:%M", tz = "UTC")
  r
}
adsl_trt <- adsl_base
adsl_trt$TRTSDT  <- sas_date(adsl_trt$RFXSTDTC)
adsl_trt$TRTSDTM <- sas_dtm(adsl_trt$RFXSTDTC)
adsl_trt$TRTEDT  <- sas_date(adsl_trt$RFXENDTC)
adsl_trt$TRTEDTM <- sas_dtm(adsl_trt$RFXENDTC)
adsl_trt$DTHDT   <- sas_date(adsl_trt$DTHDTC)
adsl_trt$TRTDURD <- ifelse(!is.na(adsl_trt$TRTSDT) & !is.na(adsl_trt$TRTEDT),
                           as.numeric(adsl_trt$TRTEDT - adsl_trt$TRTSDT) + 1, NA_real_)

# ---------------------------------------------- 4. Population flags
saf <- unique(ex[!miss(ex$EXTRT), c("STUDYID","USUBJID")])
saf$SAFFL <- "Y"
adsl_pop <- left_join(adsl_trt, saf, by = c("STUDYID","USUBJID"))
adsl_pop$SAFFL[miss(adsl_pop$SAFFL)] <- "N"
adsl_pop$ITTFL <- ifelse(adsl_pop$ARM %in% c("Placebo","Xanomeline High Dose","Xanomeline Low Dose"), "Y", "N")

# ---------------------------------------------- 5. Disposition
ds <- ds[order(ds$USUBJID, ds$DSDTC, method = "radix"), ]
dsf <- ds[ds$DSCAT == "DISPOSITION EVENT" & ds$DSDECOD != "SCREEN FAILURE", ]
dsf <- dsf[!duplicated(dsf$USUBJID, fromLast = TRUE), ]
disp <- data.frame(USUBJID = dsf$USUBJID, stringsAsFactors = FALSE)
comp <- dsf$DSDECOD == "COMPLETED"
disp$EOSSTT   <- ifelse(comp, "COMPLETED", "DISCONTINUED")
disp$DCSREAS  <- ifelse(comp, "", substr(dsf$DSDECOD, 1, 100))
disp$DCSREASP <- ifelse(!comp & dsf$DSDECOD == "OTHER", substr(dsf$DSTERM, 1, 200), "")
adsl_disp <- left_join(adsl_pop, disp, by = "USUBJID")
for (v in c("EOSSTT","DCSREAS","DCSREASP")) adsl_disp[[v]][is.na(adsl_disp[[v]])] <- ""
adsl_disp$EOSSTT[miss(adsl_disp$EOSSTT)] <- "DISCONTINUED"
adsl_disp$EOTSTT <- adsl_disp$EOSSTT

# ---------------------------------------------- 6. Age groups + codes
a <- adsl_disp$AGE
adsl_age <- adsl_disp
lt65 <- is.na(a) | a < 65          # SAS: missing < 65 is true
adsl_age$AGEGR1  <- ifelse(lt65, "<65", ifelse(a <= 80, "65-80", ">80"))
adsl_age$AGEGR1N <- ifelse(lt65, 1, ifelse(a <= 80, 2, 3))
adsl_age$SEXN <- ifelse(adsl_age$SEX == "M", 1, ifelse(adsl_age$SEX == "F", 2, NA_real_))
racemap <- c("WHITE" = 1, "BLACK OR AFRICAN AMERICAN" = 2, "ASIAN" = 3,
             "AMERICAN INDIAN OR ALASKA NATIVE" = 4,
             "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = 5, "MULTIPLE" = 6)
adsl_age$RACEN <- unname(racemap[adsl_age$RACE])

# ---------------------------------------------- 7. TRT01P / TRT01A
adsl_trt01 <- adsl_age
adsl_trt01$TRT01P <- substr(adsl_trt01$ARM, 1, 40)
adsl_trt01$TRT01A <- substr(adsl_trt01$ACTARM, 1, 40)

# ---------------------------------------------- 8. Efficacy flag
uv <- toupper(qs$VISIT)
effq <- qs[qs$QSTESTCD == "ACTOT" &
           qs$QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE" &
           !is.na(qs$QSSTRESN) &
           uv != "BASELINE" &
           !grepl("SCREENING", uv, fixed = TRUE), ]
eff <- data.frame(USUBJID = unique(effq$USUBJID), EFFFL = "Y", stringsAsFactors = FALSE)
adsl_eff <- left_join(adsl_trt01, eff, by = "USUBJID")
adsl_eff$EFFFL <- ifelse(adsl_eff$SAFFL == "Y" & !is.na(adsl_eff$EFFFL) & adsl_eff$EFFFL == "Y", "Y", "N")

# ---------------------------------------------- 9. Final ordering
ord <- c("STUDYID","USUBJID","SUBJID","SITEID","AGE","AGEU","AGEGR1","AGEGR1N","SEX","SEXN",
         "RACE","RACEN","ETHNIC","COUNTRY","ARM","ARMCD","ACTARM","ACTARMCD","TRT01P","TRT01A",
         "RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","TRTSDT","TRTEDT","TRTSDTM","TRTEDTM",
         "TRTDURD","DTHDTC","DTHDT","DTHFL","DCSREAS","DCSREASP","EOSSTT","EOTSTT",
         "SAFFL","ITTFL","EFFFL")
adsl <- adsl_eff[, c(ord, setdiff(names(adsl_eff), ord))]
adsl <- adsl[order(adsl$USUBJID, method = "radix"), ]

# Output formatting
outdf <- adsl
for (v in c("TRTSDT","TRTEDT","DTHDT")) outdf[[v]] <- ifelse(is.na(adsl[[v]]), "", format(adsl[[v]], "%Y-%m-%d"))
for (v in c("TRTSDTM","TRTEDTM")) outdf[[v]] <- ifelse(is.na(adsl[[v]]), "", format(adsl[[v]], "%Y-%m-%dT%H:%M:%S", tz = "UTC"))

write.csv(outdf, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
cat(sprintf("NOTE: R ADSL built: %d subjects, %d variables\n", nrow(outdf), ncol(outdf)))
