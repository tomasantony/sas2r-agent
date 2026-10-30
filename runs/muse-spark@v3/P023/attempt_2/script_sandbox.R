out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_base <- "/bench/data/raw/faijal-oncology/raw_data"

## helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
to_char <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x)) {
    return(ifelse(is.na(x), "", trimws(format(x, scientific = FALSE, trim = TRUE))))
  }
  ifelse(is.na(x), "", trimws(as.character(x)))
}
title_case_mon <- function(s) {
  s <- trimws(as.character(s))
  out_s <- s
  for (i in seq_along(s)) {
    v <- s[i]
    if (is.na(v) || v == "" || v == "." || toupper(v) == "NA") { out_s[i] <- NA_character_; next }
    m <- regmatches(v, regexec("^\\s*(\\d{1,2})[^A-Za-z0-9]?([A-Za-z]{3})[^A-Za-z0-9]?(\\d{4})\\s*$", v))[[1]]
    if (length(m) == 4) {
      dd <- sprintf("%02d", as.integer(m[2]))
      mon <- paste0(toupper(substr(m[3],1,1)), tolower(substr(m[3],2,3)))
      yy <- m[4]
      out_s[i] <- paste0(dd, "-", mon, "-", yy)
    } else {
      out_s[i] <- v
    }
  }
  out_s
}
parse_date11_vec <- function(x) {
  ## ONE strict format: %d-%b-%Y  (SAS DATE11. DD-MMM-YYYY)
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  cx <- trimws(as.character(x))
  cx[cx == "" | cx == "." | toupper(cx) == "NA"] <- NA_character_
  cx_norm <- title_case_mon(cx)
  as.Date(cx_norm, format = "%d-%b-%Y")
}
fmt_yymmdd <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}
norm_names <- function(df) {
  nms <- trimws(names(df))
  nms <- gsub("\\s+", "_", nms)
  names(df) <- toupper(nms)
  df
}
read_edc <- function(fname) {
  p <- file.path(raw_base, fname)
  df <- as.data.frame(readxl::read_excel(p, sheet = 1), stringsAsFactors = FALSE)
  df <- norm_names(df)
  df
}

## load ---------------------------------------------------------------
edc_dm    <- read_edc("edc_dm.xlsx")
ivrs_rand <- read_edc("ivrs_rand.xlsx")
edc_ex    <- read_edc("edc_ex.xlsx")
edc_ds    <- read_edc("edc_ds.xlsx")
edc_death <- read_edc("edc_death.xlsx")

stopifnot("SUBJID" %in% names(edc_dm))
stopifnot("SUBJID" %in% names(ivrs_rand))
stopifnot("SUBJID" %in% names(edc_ex))
stopifnot("SUBJID" %in% names(edc_ds))
stopifnot("SUBJID" %in% names(edc_death))

for (nm in c("edc_dm","ivrs_rand","edc_ex","edc_ds","edc_death")) {
  df <- get(nm)
  df$SUBJID <- to_char(df$SUBJID)
  df$SUBJID <- trimws(df$SUBJID)
  assign(nm, df)
}

## STEP 1a: ds_enrol ----------------------------------------------------
stopifnot("RAND_DT" %in% names(ivrs_rand))
ds_enrol <- ivrs_rand[, c("SUBJID","RAND_DT"), drop = FALSE]
ds_enrol$ENR_DT <- parse_date11_vec(ds_enrol$RAND_DT)
ds_enrol <- ds_enrol[, c("SUBJID","ENR_DT"), drop = FALSE]
ds_enrol <- ds_enrol[order(ds_enrol$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]

## STEP 1b: ex_dates -----------------------------------------------------
stopifnot("STDT" %in% names(edc_ex))
stopifnot("ENDT" %in% names(edc_ex))
edc_ex$STDT_D <- parse_date11_vec(edc_ex$STDT)
edc_ex$ENDT_D <- parse_date11_vec(edc_ex$ENDT)
subs_ex <- sort(unique(edc_ex$SUBJID))
ex_dates <- data.frame(SUBJID = subs_ex, stringsAsFactors = FALSE)
f_num <- sapply(subs_ex, function(s) {
  v <- edc_ex$STDT_D[edc_ex$SUBJID == s]
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else as.numeric(min(v))
})
l_num <- sapply(subs_ex, function(s) {
  v <- edc_ex$ENDT_D[edc_ex$SUBJID == s]
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else as.numeric(max(v))
})
ex_dates$F_DOSE <- as.Date(f_num, origin = "1970-01-01")
ex_dates$L_DOSE <- as.Date(l_num, origin = "1970-01-01")
ex_dates <- ex_dates[order(ex_dates$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]

## STEP 1c: ds_final -----------------------------------------------------
stopifnot("DS_DT" %in% names(edc_ds))
stopifnot("DS_TERM" %in% names(edc_ds))
ds_final <- edc_ds[, c("SUBJID","DS_DT","DS_TERM"), drop = FALSE]
ds_final$END_DT <- parse_date11_vec(ds_final$DS_DT)
ds_final$DTH_FL <- ifelse(!is.na(ds_final$DS_TERM) & ds_final$DS_TERM == "DEATH", "Y", "N")
ds_final <- ds_final[, c("SUBJID","END_DT","DTH_FL"), drop = FALSE]
ds_final <- ds_final[order(ds_final$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]

## STEP 2: sorts ---------------------------------------------------------
edc_dm_s <- edc_dm[order(edc_dm$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]
ivrs_keep <- ivrs_rand[, intersect(c("SUBJID","TRT_CODE"), names(ivrs_rand)), drop = FALSE]
ivrs_keep <- ivrs_keep[order(ivrs_keep$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]
edc_death_s <- edc_death[order(edc_death$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]

stopifnot("DTH_DT" %in% names(edc_death_s))
death_keep <- edc_death_s[, c("SUBJID","DTH_DT"), drop = FALSE]
names(death_keep)[names(death_keep) == "DTH_DT"] <- "DEATH_DATE_RAW"

## STEP 3: merge (keep only subjects present in DM, i.e. left join) -----
m <- merge(edc_dm_s, ivrs_keep, by = "SUBJID", all.x = TRUE, sort = FALSE)
stopifnot("SUBJID" %in% names(m))
m <- merge(m, ds_enrol, by = "SUBJID", all.x = TRUE, sort = FALSE)
stopifnot("ENR_DT" %in% names(m) | nrow(m) == 0)
m <- merge(m, ex_dates, by = "SUBJID", all.x = TRUE, sort = FALSE)
m <- merge(m, ds_final, by = "SUBJID", all.x = TRUE, sort = FALSE)
m <- merge(m, death_keep, by = "SUBJID", all.x = TRUE, sort = FALSE)
m <- m[order(m$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]

getcol <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))
}
getdate <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(as.Date(NA), nrow(df))
}

SITE_v    <- to_char(getcol(m, "SITE"))
BRTHDT_v  <- to_char(getcol(m, "BRTHDT"))
RFICDT_v  <- to_char(getcol(m, "RFICDT"))
GENDER_v  <- to_char(getcol(m, "GENDER"))
RACE_v    <- to_char(getcol(m, "RACE"))
ETHNIC_v  <- to_char(getcol(m, "ETHNIC"))
TRT_v     <- to_char(getcol(m, "TRT_CODE"))
ENR_DT_v  <- getdate(m, "ENR_DT")
F_DOSE_v  <- getdate(m, "F_DOSE")
L_DOSE_v  <- getdate(m, "L_DOSE")
END_DT_v  <- getdate(m, "END_DT")
DTH_FL_v  <- to_char(getcol(m, "DTH_FL"))
DEATHRAW  <- m$DEATH_DATE_RAW
if (is.null(DEATHRAW)) DEATHRAW <- rep(NA, nrow(m))
if (inherits(DEATHRAW, "Date") || inherits(DEATHRAW, "POSIXt")) {
  DTHDTC_v <- fmt_yymmdd(as.Date(DEATHRAW))
} else {
  DEATHRAW_c <- to_char(DEATHRAW)
  DEATHRAW_c[DEATHRAW_c == ""] <- NA_character_
  dth_d <- parse_date11_vec(DEATHRAW_c)
  DTHDTC_v <- fmt_yymmdd(dth_d)
  DTHDTC_v[is.na(DEATHRAW_c)] <- ""
}

BRTH_D <- parse_date11_vec(ifelse(BRTHDT_v == "", NA_character_, BRTHDT_v))
RFIC_D <- parse_date11_vec(ifelse(RFICDT_v == "", NA_character_, RFICDT_v))
BRTHDTC_v <- fmt_yymmdd(BRTH_D)
RFICDTC_v <- fmt_yymmdd(RFIC_D)

SEX_v <- ifelse(GENDER_v == "", "", substr(GENDER_v, 1, 1))

age_fun <- function(b, r) {
  if (is.na(b) || is.na(r)) return(NA_integer_)
  y <- as.integer(format(r, "%Y")) - as.integer(format(b, "%Y"))
  mb <- as.integer(format(b, "%m")); mr <- as.integer(format(r, "%m"))
  db <- as.integer(format(b, "%d")); dr <- as.integer(format(r, "%d"))
  if (mr < mb || (mr == mb && dr < db)) y <- y - 1L
  y
}
AGE_v <- mapply(age_fun, BRTH_D, RFIC_D)

STUDYID_v <- rep("ONCO-2026", nrow(m))
DOMAIN_v  <- rep("DM", nrow(m))
SUBJID_v  <- m$SUBJID
USUBJID_v <- ifelse(is.na(SUBJID_v) | SUBJID_v == "", STUDYID_v, paste0(STUDYID_v, "-", SUBJID_v))
SITEID_v  <- SITE_v
AGEU_v    <- rep("YEARS", nrow(m))
COUNTRY_v <- rep("IND", nrow(m))

RFSTDTC_v  <- fmt_yymmdd(ENR_DT_v)
RFXSTDTC_v <- fmt_yymmdd(F_DOSE_v)
RFXENDTC_v <- fmt_yymmdd(L_DOSE_v)
RFENDTC_v  <- fmt_yymmdd(END_DT_v)
RFPENDTC_v <- RFENDTC_v

ARMCD_v <- TRT_v
ARMCD_v[ARMCD_v == ""] <- NA_character_
ARM_v <- ifelse(!is.na(ARMCD_v) & ARMCD_v == "DRUGA", "Experimental Drug A",
         ifelse(!is.na(ARMCD_v) & ARMCD_v == "PBO", "Placebo", NA_character_))
ACTARMCD_v <- ARMCD_v
ACTARM_v   <- ARM_v
DTHFL_v <- DTH_FL_v
DTHFL_v[DTHFL_v == ""] <- NA_character_

sdtm_dm <- data.frame(
  STUDYID = STUDYID_v, DOMAIN = DOMAIN_v, USUBJID = USUBJID_v,
  SUBJID = SUBJID_v, SITEID = SITEID_v,
  RFSTDTC = RFSTDTC_v, RFENDTC = RFENDTC_v, RFXSTDTC = RFXSTDTC_v,
  RFXENDTC = RFXENDTC_v, RFICDTC = RFICDTC_v, RFPENDTC = RFPENDTC_v,
  BRTHDTC = BRTHDTC_v, AGE = as.integer(AGE_v), AGEU = AGEU_v,
  SEX = SEX_v, RACE = RACE_v, ETHNIC = ETHNIC_v,
  ARMCD = ARMCD_v, ARM = ARM_v, ACTARMCD = ACTARMCD_v, ACTARM = ACTARM_v,
  COUNTRY = COUNTRY_v, DTHFL = DTHFL_v, DTHDTC = DTHDTC_v,
  stringsAsFactors = FALSE
)
for (cc in names(sdtm_dm)) {
  if (cc == "AGE") next
  sdtm_dm[[cc]][is.na(sdtm_dm[[cc]])] <- ""
}
sdtm_dm$SEX[sdtm_dm$SEX == ""] <- ""
sdtm_dm <- sdtm_dm[order(sdtm_dm$SUBJID, method = "radix", na.last = FALSE), , drop = FALSE]

## STEP 4: outputs -------------------------------------------------------
write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")
write.csv(sdtm_dm, file.path(out, "P023.csv"), row.names = FALSE, na = "")

prt <- data.frame(Obs = seq_len(nrow(sdtm_dm)), sdtm_dm, stringsAsFactors = FALSE, check.names = FALSE)
txt_lines <- capture.output(print(prt, row.names = FALSE, right = TRUE, na.print = ""))
header <- c(
  "SDTM DM DOMAIN (STUDY: ONCO-2026)",
  "PROC PRINT of SDTM_DM",
  paste0("N = ", nrow(sdtm_dm)),
  ""
)
full_lines <- c(header, txt_lines)
writeLines(full_lines, file.path(out, "P023.txt"))
writeLines(full_lines, file.path(out, "P023.lst"))
