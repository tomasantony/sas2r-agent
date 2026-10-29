out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lubridate)
})

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

# ---- helpers to mimic SAS ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_sas_date_one <- function(v) {
  if (is.na(v)) return(as.Date(NA))
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v) && !is.character(v)) {
    # readxl already converts dates; treat pure numeric as missing
    return(as.Date(NA))
  }
  s <- trimws(as.character(v))
  if (is.na(s) || s == "" || toupper(s) %in% c(".", "NA", "NULL", "MISSING")) return(as.Date(NA))
  # try common formats
  fmts <- c("%d-%b-%Y", "%d-%B-%Y", "%d-%b-%y", "%d%b%Y", "%d-%b-%Y", "%Y-%m-%d",
            "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%d-%m-%Y", "%Y%m%d", "%d %b %Y",
            "%d-%B-%Y", "%Y-%m-%dT%H:%M:%S")
  # uppercase month abbrev handling: as.Date is case-insensitive on most platforms for %b, but force title case attempt
  # try as-is
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d)) return(d)
    # try with upper/lower variations for month name
    # convert e.g. 01JAN2020 -> 01-Jan-2020
    s2 <- s
    # if like DDMMMYYYY insert dashes
    if (grepl("^[0-9]{1,2}[A-Za-z]{3}[0-9]{2,4}$", s2)) {
      dd <- sub("^([0-9]{1,2})([A-Za-z]{3})([0-9]{2,4})$", "\\1-\\2-\\3", s2)
      d2 <- suppressWarnings(as.Date(dd, format = f))
      if (!is.na(d2)) return(d2)
    }
  }
  # fallback lubridate
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  return(as.Date(NA))
}

parse_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  # vectorize
  res <- lapply(seq_along(x), function(i) parse_sas_date_one(x[i]))
  do.call(c, res)
}

put_yymmdd10 <- function(d) {
  # d is Date vector; return character YYYY-MM-DD or "" for NA (SAS PUT blank)
  if (inherits(d, "character")) d <- parse_sas_date(d)
  if (!inherits(d, "Date")) d <- as.Date(d, origin = "1970-01-01")
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

sas_substr1 <- function(x) {
  # substr(GENDER,1,1)
  s <- as.character(x)
  out <- ifelse(is.na(s) | trimws(s) == "", "", substr(s, 1, 1))
  out
}

calc_age_intck_c <- function(birth, consent) {
  # birth, consent are Date vectors
  n <- length(birth)
  if (length(consent) != n) consent <- rep(consent, length.out = n)
  vapply(seq_len(n), function(i) {
    b <- birth[i]; c <- consent[i]
    if (is.na(b) || is.na(c)) return(NA_real_)
    yb <- lubridate::year(b); mb <- lubridate::month(b); db <- lubridate::mday(b)
    yc <- lubridate::year(c); mc <- lubridate::month(c); dc <- lubridate::mday(c)
    age <- yc - yb
    if (mc < mb || (mc == mb && dc < db)) age <- age - 1L
    as.numeric(age)
  }, numeric(1))
}

get_col <- function(df, name) {
  # case-insensitive lookup; return vector or NULL
  nm <- names(df)
  idx <- which(toupper(nm) == toupper(name))
  if (length(idx) == 0) return(rep(NA, nrow(df)))
  df[[idx[1]]]
}

read_edc <- function(fname) {
  p <- file.path(edc_dir, fname)
  df <- readxl::read_excel(p, .name_repair = "minimal")
  names(df) <- toupper(trimws(names(df)))
  as.data.frame(df, stringsAsFactors = FALSE)
}

# ---- STEP 1: read sources ----
edc_dm    <- read_edc("edc_dm.xlsx")
ivrs_rand <- read_edc("ivrs_rand.xlsx")
edc_ex    <- read_edc("edc_ex.xlsx")
edc_ds    <- read_edc("edc_ds.xlsx")
edc_death <- read_edc("edc_death.xlsx")

# Ensure SUBJID character trimmed
norm_key <- function(x) {
  if (is.null(x)) return(character(0))
  s <- as.character(x)
  s[is.na(s)] <- NA_character_
  trimws(s)
}

edc_dm$SUBJID    <- norm_key(get_col(edc_dm, "SUBJID"))
ivrs_rand$SUBJID <- norm_key(get_col(ivrs_rand, "SUBJID"))
edc_ex$SUBJID    <- norm_key(get_col(edc_ex, "SUBJID"))
edc_ds$SUBJID    <- norm_key(get_col(edc_ds, "SUBJID"))
edc_death$SUBJID <- norm_key(get_col(edc_death, "SUBJID"))

# ---- ds_enrol: input(RAND_DT, date11.) ----
rand_dt_raw <- get_col(ivrs_rand, "RAND_DT")
ds_enrol <- data.frame(
  SUBJID = ivrs_rand$SUBJID,
  ENR_DT = parse_sas_date(rand_dt_raw),
  stringsAsFactors = FALSE
)
# if duplicate SUBJID keep first (SAS merge would need sort; SQL would return multiples - keep all? deduplicate to first for merge stability, but keep SQL semantics: one row per input row)
# For merge we need one per SUBJID; if multiples, keep first non-missing? Use first row per SUBJID
ds_enrol <- ds_enrol %>% arrange(SUBJID) %>% distinct(SUBJID, .keep_all = TRUE)

# ---- ex_dates: min/max ----
stdt_raw <- get_col(edc_ex, "STDT")
endt_raw <- get_col(edc_ex, "ENDT")
edc_ex$`.STDT_D` <- parse_sas_date(stdt_raw)
edc_ex$`.ENDT_D` <- parse_sas_date(endt_raw)
ex_dates <- edc_ex %>%
  group_by(SUBJID) %>%
  summarise(
    F_DOSE = if (all(is.na(`.STDT_D`))) as.Date(NA) else suppressWarnings(min(`.STDT_D`, na.rm = TRUE)),
    L_DOSE = if (all(is.na(`.ENDT_D`))) as.Date(NA) else suppressWarnings(max(`.ENDT_D`, na.rm = TRUE)),
    .groups = "drop"
  ) %>% as.data.frame()
# ensure Date class
ex_dates$F_DOSE <- as.Date(ex_dates$F_DOSE, origin = "1970-01-01")
ex_dates$L_DOSE <- as.Date(ex_dates$L_DOSE, origin = "1970-01-01")

# ---- ds_final ----
ds_dt_raw <- get_col(edc_ds, "DS_DT")
ds_term_raw <- as.character(get_col(edc_ds, "DS_TERM"))
ds_final <- data.frame(
  SUBJID = edc_ds$SUBJID,
  END_DT = parse_sas_date(ds_dt_raw),
  DTH_FL = ifelse(!is.na(ds_term_raw) & ds_term_raw == "DEATH", "Y", "N"),
  stringsAsFactors = FALSE
)
ds_final <- ds_final %>% arrange(SUBJID) %>% distinct(SUBJID, .keep_all = TRUE)

# ---- ivrs keep ----
trt_raw <- get_col(ivrs_rand, "TRT_CODE")
ivrs_keep <- data.frame(SUBJID = ivrs_rand$SUBJID, TRT_CODE = as.character(trt_raw), stringsAsFactors = FALSE)
ivrs_keep$TRT_CODE[is.na(ivrs_keep$TRT_CODE)] <- NA_character_
ivrs_keep <- ivrs_keep %>% arrange(SUBJID) %>% distinct(SUBJID, .keep_all = TRUE)

# ---- death rename ----
dth_raw <- get_col(edc_death, "DTH_DT")
# preserve raw as character for ne "" logic; if Date object, keep Date
death_keep <- data.frame(SUBJID = edc_death$SUBJID, DEATH_DATE_RAW = dth_raw, stringsAsFactors = FALSE)
# if Date class, convert to character YYYY-MM-DD? keep as is for parsing; store as character representation that parse can handle
# If column was Date, dth_raw is Date; data.frame may convert? Keep original vector
death_keep$DEATH_DATE_RAW <- dth_raw
death_keep <- death_keep %>% arrange(SUBJID) %>% distinct(SUBJID, .keep_all = TRUE)

# ---- STEP 2/3: sort + merge (left join edc_dm) ----
# SAS sorts with missing first, uppercase before lowercase (byte order). Use byte-order sort for SUBJID.
# For left join order, sort edc_dm by SUBJID byte order
byte_order <- function(x) {
  # order by raw bytes: use xtfrm with C locale via order on charToRaw strings
  # simple: order using stringr byte values; fallback to order()
  # Implement rank by converting each string to hex-joined
  sapply(x, function(v) {
    if (is.na(v)) return("\001")
    paste(sprintf("%03d", as.integer(charToRaw(v))), collapse = "-")
  })
}
edc_dm <- edc_dm[order(byte_order(edc_dm$SUBJID)), , drop = FALSE]

merged <- edc_dm %>%
  left_join(ds_enrol, by = "SUBJID") %>%
  left_join(ex_dates, by = "SUBJID") %>%
  left_join(ds_final, by = "SUBJID") %>%
  left_join(ivrs_keep, by = "SUBJID") %>%
  left_join(death_keep, by = "SUBJID")

# Extract source fields (case-insensitive)
SITE_raw    <- get_col(merged, "SITE")
BRTHDT_raw  <- get_col(merged, "BRTHDT")
RFICDT_raw  <- get_col(merged, "RFICDT")
GENDER_raw  <- get_col(merged, "GENDER")
RACE_raw    <- get_col(merged, "RACE")
ETHNIC_raw  <- get_col(merged, "ETHNIC")

# Dates
brth_d  <- parse_sas_date(BRTHDT_raw)
rfic_d  <- parse_sas_date(RFICDT_raw)
enr_dt  <- merged$ENR_DT
if (!inherits(enr_dt, "Date")) enr_dt <- parse_sas_date(enr_dt)
f_dose  <- merged$F_DOSE
if (!inherits(f_dose, "Date")) f_dose <- parse_sas_date(f_dose)
l_dose  <- merged$L_DOSE
if (!inherits(l_dose, "Date")) l_dose <- parse_sas_date(l_dose)
end_dt  <- merged$END_DT
if (!inherits(end_dt, "Date")) end_dt <- parse_sas_date(end_dt)

BRTHDTC  <- put_yymmdd10(brth_d)
RFICDTC  <- put_yymmdd10(rfic_d)
RFSTDTC  <- put_yymmdd10(enr_dt)
RFXSTDTC <- put_yymmdd10(f_dose)
RFXENDTC <- put_yymmdd10(l_dose)
RFENDTC  <- put_yymmdd10(end_dt)
RFPENDTC <- RFENDTC

SEX <- sas_substr1(GENDER_raw)
AGE <- calc_age_intck_c(parse_sas_date(BRTHDTC), parse_sas_date(RFICDTC))

STUDYID <- rep("ONCO-2026", nrow(merged))
DOMAIN  <- rep("DM", nrow(merged))
USUBJID <- paste0("ONCO-2026-", merged$SUBJID)
# catx strips blanks: if SUBJID missing/blank, USUBJID = STUDYID
USUBJID[is.na(merged$SUBJID) | trimws(merged$SUBJID) == ""] <- "ONCO-2026"
SITEID <- as.character(SITE_raw)
AGEU <- rep("YEARS", nrow(merged))
COUNTRY <- rep("IND", nrow(merged))

ARMCD <- as.character(merged$TRT_CODE)
ARM <- ifelse(!is.na(ARMCD) & ARMCD == "DRUGA", "Experimental Drug A",
       ifelse(!is.na(ARMCD) & ARMCD == "PBO", "Placebo", NA_character_))
ACTARMCD <- ARMCD
ACTARM <- ARM

DTHFL <- as.character(merged$DTH_FL)
# death date logic: if death_date_raw ne "" then DTHDTC=put(...)
death_raw_vec <- merged$DEATH_DATE_RAW
DTHDTC <- vapply(seq_len(nrow(merged)), function(i) {
  v <- death_raw_vec[i]
  if (length(v) == 0 || is.na(v)) return("")
  # if Date object
  if (inherits(v, "Date") || inherits(v, "POSIXct")) {
    d <- parse_sas_date(v)
    if (is.na(d)) return("")
    return(format(d, "%Y-%m-%d"))
  }
  s <- trimws(as.character(v))
  if (is.na(s) || s == "") return("")
  d <- parse_sas_date_one(s)
  if (is.na(d)) return("")
  format(d, "%Y-%m-%d")
}, character(1))

RACE <- as.character(RACE_raw)
ETHNIC <- as.character(ETHNIC_raw)
SUBJID <- as.character(merged$SUBJID)

sdtm_dm <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  SUBJID = SUBJID,
  SITEID = SITEID,
  RFSTDTC = RFSTDTC,
  RFENDTC = RFENDTC,
  RFXSTDTC = RFXSTDTC,
  RFXENDTC = RFXENDTC,
  RFICDTC = RFICDTC,
  RFPENDTC = RFPENDTC,
  BRTHDTC = BRTHDTC,
  AGE = AGE,
  AGEU = AGEU,
  SEX = SEX,
  RACE = RACE,
  ETHNIC = ETHNIC,
  ARMCD = ARMCD,
  ARM = ARM,
  ACTARMCD = ACTARMCD,
  ACTARM = ACTARM,
  COUNTRY = COUNTRY,
  DTHFL = DTHFL,
  DTHDTC = DTHDTC,
  stringsAsFactors = FALSE
)

# SAS missing character is "" ; convert NA character to ""? Keep NA for CSV na="" handling, but for display use "".
# Keep NA as NA so write.csv na="" writes blank. For text, show blank.
# Ensure "" stays "" and NA stays NA (both display blank). Normalize empty strings to NA? Keep as "" for put blanks -> use "" not NA for date blanks to match SAS blank.
# AGE stays numeric NA for missing.

# Sort final by SUBJID byte order (PROC PRINT shows sorted merge order)
sdtm_dm <- sdtm_dm[order(byte_order(sdtm_dm$SUBJID)), , drop = FALSE]

# ---- write datasets ----
write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")
write.csv(sdtm_dm, file.path(out, "P023.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT listing -> text rendering ----
# Build print with Obs column
lst_df <- cbind(Obs = seq_len(nrow(sdtm_dm)), sdtm_dm)
# For text, replace NA with "" for character display
disp_df <- lst_df
for (nm in names(disp_df)) {
  if (is.character(disp_df[[nm]])) {
    disp_df[[nm]][is.na(disp_df[[nm]])] <- ""
  }
}
# Format AGE without decimals, blank if NA
# capture print
old_width <- getOption("width")
options(width = 1000)
txt_header <- c(
  "SDTM DM Domain (ONCO-2026)",
  paste0("PROC PRINT data=sdtm_dm  N=", nrow(sdtm_dm)),
  ""
)
txt_body <- capture.output(print(as.data.frame(disp_df), right = TRUE, na.print = "", quote = FALSE, max.print = 1000000))
options(width = old_width)
txt_all <- c(txt_header, txt_body)
writeLines(txt_all, file.path(out, "P023.txt"))
# also write .lst copy for compatibility
writeLines(txt_all, file.path(out, "P023.lst"))
