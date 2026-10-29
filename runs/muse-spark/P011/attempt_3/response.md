```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}

sas_trunc <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(as.character(x), 1, len))
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

cat("Reading raw:", raw_path, "\n")
raw_dm <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                          na = c("", "NA"), trim_ws = FALSE, progress = FALSE,
                          locale = readr::locale(encoding = "UTF-8"))
nm <- names(raw_dm)
nm <- trimws(nm)
nm <- toupper(nm)
nm <- gsub("[^A-Z0-9_]", "_", nm)
names(raw_dm) <- nm
cat("Raw rows:", nrow(raw_dm), " cols:", paste(names(raw_dm), collapse=","), "\n")

get_col <- function(df, primary, alts = c()) {
  if (primary %in% names(df)) return(df[[primary]])
  for (a in alts) if (a %in% names(df)) return(df[[a]])
  return(rep(NA_character_, nrow(df)))
}

SUBJ_ID_raw       <- get_col(raw_dm, "SUBJ_ID", c("SUBJID","SUBJ","USUBJID"))
SITE_raw          <- get_col(raw_dm, "SITE", c("SITEID","SITE_ID"))
GENDER_raw        <- get_col(raw_dm, "GENDER", c("SEX"))
RACE_TXT_raw      <- get_col(raw_dm, "RACE_TXT", c("RACE","RACE_TXT_"))
RANDARM_raw       <- get_col(raw_dm, "RANDOMIZED_ARM", c("RANDOMIZEDARM","ARM","PLANNED_ARM"))
BRTH_DT_raw       <- get_col(raw_dm, "BRTH_DT", c("BRTHDT","BIRTH_DT","BRTHDTC"))
ICF_DAT_raw       <- get_col(raw_dm, "ICF_DAT", c("ICFDAT","ICFDT","CONSENT_DAT","RFICDTC"))
STUDY_END_raw     <- get_col(raw_dm, "STUDY_END_DAT", c("STUDYENDDAT","STUDY_END","END_DAT","RFENDTC"))

n <- nrow(raw_dm)

# SAS ANYDTDTE. in this job resolves ambiguous a/b/year as DMY (day/month/year),
# as evidenced by SAS reference (e.g. 03/04/1956 -> 1956-04-03).
# Implement DMY-first with MDY fallback for unambiguous values.
expand_2digit_year <- function(yy) {
  ifelse(yy >= 0 & yy <= 19, 2000L + yy,
  ifelse(yy >= 20 & yy <= 99, 1900L + yy, yy))
}

try_dmy <- function(a, b, y) {
  # a=first part, b=second part : DMY => d=a, m=b
  d <- a; m <- b
  if (is.na(d) || is.na(m) || is.na(y)) return(as.Date(NA))
  if (m < 1 || m > 12 || d < 1 || d > 31) return(as.Date(NA))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
}
try_mdy <- function(a, b, y) {
  m <- a; d <- b
  if (is.na(d) || is.na(m) || is.na(y)) return(as.Date(NA))
  if (m < 1 || m > 12 || d < 1 || d > 31) return(as.Date(NA))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
}

parse_anydtdte_one_DMY <- function(x) {
  if (is.na(x)) return(as.Date(NA))
  xs <- trimws(as.character(x))
  if (xs == "" || toupper(xs) %in% c(".", "NA", "NULL", "MISSING", "NA/NA/NA", "UNKNOWN")) return(as.Date(NA))
  if (grepl("[A-Za-z]", xs)) {
    dt <- suppressWarnings(lubridate::parse_date_time(xs,
      orders = c("Ymd","YmdHMS","YmdHM","mdY","mdYHMS","mdYHM","dby","dbY","bdY",
                 "b d, Y","B d, Y","d-b-Y","d-b-y","Y-b-d","Y/b/d","d b Y","b d Y",
                 "dmY","dmYHMS","dmYHM"),
      quiet = TRUE, truncated = 0, tz = "UTC"))
    if (length(dt) == 1 && !is.na(dt)) return(as.Date(dt))
    fmts <- c("%d%b%Y","%d-%b-%Y","%d %b %Y","%d%b%y","%d-%b-%y","%b %d, %Y","%B %d, %Y",
              "%d %B %Y","%Y-%b-%d","%Y/%b/%d","%d/%m/%Y","%d-%m-%Y","%Y-%m-%d")
    for (f in fmts) {
      d <- suppressWarnings(as.Date(xs, format = f))
      if (!is.na(d)) return(d)
    }
    d2 <- suppressWarnings(as.Date(xs, tryFormats = fmts))
    if (length(d2)==1 && !is.na(d2)) return(d2)
    return(as.Date(NA))
  }
  xs_date <- xs
  m_iso_dt <- regmatches(xs, regexpr("^\\d{4}[-/\\.]\\d{1,2}[-/\\.]\\d{1,2}[T ]", xs))
  if (length(m_iso_dt)==1 && !is.na(m_iso_dt) && nchar(m_iso_dt)>0) {
    xs_date <- substr(xs, 1, 10)
  } else if (grepl("^\\d{1,2}[-/\\.]\\d{1,2}[-/\\.]\\d{2,4}[ T]+", xs)) {
    xs_date <- strsplit(xs, "[ \tT]+")[[1]][1]
  } else {
    xs_date <- strsplit(xs, "[ \t]+")[[1]][1]
  }
  xs_date <- trimws(xs_date)
  if (grepl("^\\d{4}[-/\\.\\s]\\d{1,2}[-/\\.\\s]\\d{1,2}$", xs_date)) {
    parts <- strsplit(xs_date, "[-/\\.\\s]")[[1]]
    y <- suppressWarnings(as.integer(parts[1])); m <- suppressWarnings(as.integer(parts[2])); d <- suppressWarnings(as.integer(parts[3]))
    if (any(is.na(c(y,m,d)))) return(as.Date(NA))
    if (m < 1 || m > 12 || d < 1 || d > 31) return(as.Date(NA))
    dd <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
    return(dd)
  }
  if (grepl("^\\d{1,2}[-/\\.]\\d{1,2}[-/\\.]\\d{2,4}$", xs_date)) {
    parts <- strsplit(xs_date, "[-/\\.]")[[1]]
    a <- suppressWarnings(as.integer(parts[1])); b <- suppressWarnings(as.integer(parts[2])); c0 <- suppressWarnings(as.integer(parts[3]))
    if (any(is.na(c(a,b,c0)))) return(as.Date(NA))
    y <- c0
    if (nchar(parts[3]) <= 2) y <- expand_2digit_year(c0)
    # DMY first (matches SAS reference for ambiguous dates)
    dd <- try_dmy(a, b, y)
    if (!is.na(dd)) return(dd)
    # fallback to MDY (handles e.g. 03/25/2020 where month would be 25 under DMY)
    dd2 <- try_mdy(a, b, y)
    if (!is.na(dd2)) return(dd2)
    return(as.Date(NA))
  }
  if (grepl("^\\d{8}$", xs_date)) {
    # try YYYYMMDD first, then DDMMYYYY, then MMDDYYYY
    y <- suppressWarnings(as.integer(substr(xs_date,1,4)))
    m <- suppressWarnings(as.integer(substr(xs_date,5,6)))
    d <- suppressWarnings(as.integer(substr(xs_date,7,8)))
    if (!is.na(y) && y >= 1800 && y <= 2100 && m>=1 && m<=12 && d>=1 && d<=31) {
      dd <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m,d)))
      if (!is.na(dd)) return(dd)
    }
    d2a <- suppressWarnings(as.integer(substr(xs_date,1,2)))
    m2a <- suppressWarnings(as.integer(substr(xs_date,3,4)))
    y2a <- suppressWarnings(as.integer(substr(xs_date,5,8)))
    if (m2a>=1 && m2a<=12 && d2a>=1 && d2a<=31 && y2a>=1800 && y2a<=2100) {
      dd <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y2a,m2a,d2a)))
      if (!is.na(dd)) return(dd)
    }
    m2 <- suppressWarnings(as.integer(substr(xs_date,1,2)))
    d2 <- suppressWarnings(as.integer(substr(xs_date,3,4)))
    y2 <- suppressWarnings(as.integer(substr(xs_date,5,8)))
    # MMDDYYYY vs DDMMYYYY ambiguous; prefer DMY per SAS reference
    dd <- try_dmy(d2, m2, y2)
    if (!is.na(dd)) return(dd)
    dd <- try_mdy(m2, d2, y2)
    if (!is.na(dd)) return(dd)
    return(as.Date(NA))
  }
  if (grepl("^\\d{6}$", xs_date)) {
    a <- suppressWarnings(as.integer(substr(xs_date,1,2)))
    b <- suppressWarnings(as.integer(substr(xs_date,3,4)))
    y0 <- suppressWarnings(as.integer(substr(xs_date,5,6)))
    y <- expand_2digit_year(y0)
    dd <- try_dmy(a, b, y)
    if (!is.na(dd)) return(dd)
    dd <- try_mdy(a, b, y)
    if (!is.na(dd)) return(dd)
    return(as.Date(NA))
  }
  d <- suppressWarnings(as.Date(xs_date, tryFormats = c("%Y-%m-%d","%Y/%m/%d","%d/%m/%Y","%d-%m-%Y","%m/%d/%Y","%m-%d-%Y","%Y%m%d")))
  return(d)
}

parse_anydtdte_DMY <- function(v) {
  if (is.numeric(v)) return(as.Date(v, origin = "1960-01-01"))
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  res <- lapply(v, parse_anydtdte_one_DMY)
  do.call(c, res)
}

fmt_is8601da <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

brth_date  <- parse_anydtdte_DMY(BRTH_DT_raw)
rfic_date  <- parse_anydtdte_DMY(ICF_DAT_raw)
rfend_date <- parse_anydtdte_DMY(STUDY_END_raw)

BRTHDTC  <- fmt_is8601da(brth_date)
RFICDTC  <- fmt_is8601da(rfic_date)
RFSTDTC  <- RFICDTC
RFENDTC  <- fmt_is8601da(rfend_date)
RFPENDTC <- RFENDTC

STUDYID <- rep("CDISC-01", n)
DOMAIN  <- rep("DM", n)

SUBJID <- sas_trunc(sas_strip(SUBJ_ID_raw), 20)
USUBJID <- sas_trunc(ifelse(is.na(SUBJID) | SUBJID == "", STUDYID,
                     paste0(STUDYID, "-", SUBJID)), 40)

SITEID <- sas_trunc(sas_strip(SITE_raw), 10)
SITEID[SITEID == ""] <- NA_character_

SEX <- rep("U", n)
for (i in seq_len(n)) {
  g <- GENDER_raw[i]
  if (is.na(g)) { SEX[i] <- "U"; next }
  gs <- as.character(g)
  if (is.na(gs) || gs == "") { SEX[i] <- "U"; next }
  ch1 <- substr(gs, 1, 1)
  uch <- toupper(ch1)
  if (!is.na(uch) && uch == "M") SEX[i] <- "M"
  else if (!is.na(uch) && uch == "F") SEX[i] <- "F"
  else SEX[i] <- "U"
}
SEX <- sas_trunc(SEX, 1)

RACE_up <- toupper(sas_strip(RACE_TXT_raw))
RACE <- rep("UNKNOWN", n)
RACE[!is.na(RACE_up) & RACE_up == "WHITE"] <- "WHITE"
RACE[!is.na(RACE_up) & RACE_up == "BLACK"] <- "BLACK OR AFRICAN AMERICAN"
RACE[!is.na(RACE_up) & RACE_up == "ASIAN"] <- "ASIAN"
RACE[!is.na(RACE_up) & RACE_up == "OTHER"] <- "OTHER"
RACE <- sas_trunc(RACE, 40)

ARM <- rep(NA_character_, n)
ARMCD <- rep(NA_character_, n)
rand_strip <- sas_strip(RANDARM_raw)
ARM[!is.na(rand_strip) & rand_strip == "Not Randomized"] <- "SCREEN FAILURE"
ARMCD[!is.na(rand_strip) & rand_strip == "Not Randomized"] <- "SCRNFAIL"
ARM[!is.na(rand_strip) & rand_strip == "Placebo"] <- "Placebo"
ARMCD[!is.na(rand_strip) & rand_strip == "Placebo"] <- "PBO"
ARM[!is.na(rand_strip) & rand_strip == "Active Drug 50mg"] <- "Active Drug 50mg"
ARMCD[!is.na(rand_strip) & rand_strip == "Active Drug 50mg"] <- "ACT50"
ARM <- sas_trunc(ARM, 20)
ARMCD <- sas_trunc(ARMCD, 8)

sdtm_dm <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  SUBJID = SUBJID,
  SITEID = SITEID,
  SEX = SEX,
  RACE = RACE,
  BRTHDTC = BRTHDTC,
  RFSTDTC = RFSTDTC,
  RFENDTC = RFENDTC,
  RFPENDTC = RFPENDTC,
  RFICDTC = RFICDTC,
  ARM = ARM,
  ARMCD = ARMCD,
  stringsAsFactors = FALSE
)

cat("SDTM DM rows:", nrow(sdtm_dm), "\n")

write.csv(sdtm_dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")

print_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
audit_df <- head(sdtm_dm[, print_vars, drop = FALSE], 10)
audit_df <- cbind(Obs = seq_len(nrow(audit_df)), audit_df)

write.csv(audit_df, file.path(out, "P011.csv"), row.names = FALSE, na = "")

txt_path1 <- file.path(out, "P011.txt")
txt_path2 <- file.path(out, "P011.lst")
con_lines <- c(
  "DM Domain Audit (First 10 Records)",
  "",
  paste0("Dataset: DM  (N=", nrow(sdtm_dm), ", showing first ", nrow(audit_df), " records)"),
  ""
)
hdr <- sprintf("%-4s %-8s %-18s %-6s %-3s %-28s %-10s %-10s %-10s %-10s %-10s %-18s %-8s",
  "Obs","STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
con_lines <- c(con_lines, hdr, sep)
for (i in seq_len(nrow(audit_df))) {
  r <- audit_df[i, ]
  line <- sprintf("%-4d %-8s %-18s %-6s %-3s %-28s %-10s %-10s %-10s %-10s %-10s %-18s %-8s",
    r$Obs,
    ifelse(is.na(r$STUDYID), "", r$STUDYID),
    ifelse(is.na(r$USUBJID), "", r$USUBJID),
    ifelse(is.na(r$SITEID), "", r$SITEID),
    ifelse(is.na(r$SEX), "", r$SEX),
    ifelse(is.na(r$RACE), "", r$RACE),
    ifelse(is.na(r$BRTHDTC), "", r$BRTHDTC),
    ifelse(is.na(r$RFICDTC), "", r$RFICDTC),
    ifelse(is.na(r$RFSTDTC), "", r$RFSTDTC),
    ifelse(is.na(r$RFENDTC), "", r$RFENDTC),
    ifelse(is.na(r$RFPENDTC), "", r$RFPENDTC),
    ifelse(is.na(r$ARM), "", r$ARM),
    ifelse(is.na(r$ARMCD), "", r$ARMCD))
  con_lines <- c(con_lines, line)
}
con_lines <- c(con_lines, "", paste0("Total records in DM: ", nrow(sdtm_dm)))
writeLines(con_lines, txt_path1)
writeLines(con_lines, txt_path2)
cat("Wrote:", file.path(out, "dm.csv"), "\n")
cat("Wrote:", txt_path1, "and", txt_path2, "and", file.path(out, "P011.csv"), "\n")
```