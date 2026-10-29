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

raw_path <- "/bench/data/derived/c2c/data/raw/raw_demog.csv"

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

cat("Reading raw:", raw_path, "\n")
# Read everything as character to mimic PROC IMPORT character handling
# and preserve original strings for ANYDTDTE parsing
raw_dm <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                          na = c("", "NA"), trim_ws = FALSE, progress = FALSE,
                          locale = readr::locale(encoding = "UTF-8"))
# Normalize names to uppercase like VALIDVARNAME=UPCASE
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

parse_anydtdte_one <- function(x) {
  if (is.na(x)) return(as.Date(NA))
  xs <- trimws(as.character(x))
  if (xs == "" || toupper(xs) %in% c(".", "NA", "NULL", "MISSING", "NA/NA/NA", "UNKNOWN")) return(as.Date(NA))
  # Try lubridate parse_date_time with common SAS ANYDTDTE orders
  dt <- suppressWarnings(lubridate::parse_date_time(xs,
    orders = c("Ymd","YmdHMS","YmdHM","YmdH","mdY","mdYHMS","mdYHM","mdYH",
               "dmY","dmYHMS","dmYHM","Ymd HMS","bdY","dbY","b d, Y","B d, Y",
               "d-b-Y","d-b-y","Y-b-d","Y/m/d","m/d/Y","d/m/Y","m-d-Y","Y.m.d"),
    quiet = TRUE, truncated = 0, tz = "UTC"))
  if (length(dt) == 1 && !is.na(dt)) return(as.Date(dt))
  fmts <- c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%m-%d-%Y","%m/%d/%y","%m-%d-%y",
            "%d%b%Y","%d-%b-%Y","%d %b %Y","%d%b%y","%d-%b-%y","%b %d, %Y","%B %d, %Y",
            "%Y%m%d","%Y.%m.%d","%d.%m.%Y","%m.%d.%Y","%Y-%m-%dT%H:%M:%S",
            "%Y-%m-%d %H:%M:%S","%m/%d/%Y %H:%M","%Y/%m/%d %H:%M:%S","%d %B %Y","%B %d %Y")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(xs, format = f))
    if (!is.na(d)) return(d)
  }
  d2 <- suppressWarnings(as.Date(xs, tryFormats = fmts))
  if (!is.na(d2)) return(d2)
  # last resort: strip time part after space/T and retry ISO
  xs2 <- sub("[T ].*$", "", xs)
  if (xs2 != xs) {
    for (f in fmts) {
      d <- suppressWarnings(as.Date(xs2, format = f))
      if (!is.na(d)) return(d)
    }
  }
  return(as.Date(NA))
}
parse_anydtdte <- function(v) {
  # v is character vector (PROC IMPORT char branch)
  # If numeric vector supplied (SAS date numbers), convert via 1960-01-01
  if (is.numeric(v)) {
    return(as.Date(v, origin = "1960-01-01"))
  }
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  res <- lapply(v, parse_anydtdte_one)
  do.call(c, res)
}
fmt_is8601da <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# Detect numeric SAS-date columns that may have been read as character but are truly numeric?
# Since we read as character, char branch applies: input(strip(x), anydtdte.)
brth_date  <- parse_anydtdte(BRTH_DT_raw)
rfic_date  <- parse_anydtdte(ICF_DAT_raw)
rfend_date <- parse_anydtdte(STUDY_END_raw)

BRTHDTC  <- fmt_is8601da(brth_date)
RFICDTC  <- fmt_is8601da(rfic_date)
RFSTDTC  <- RFICDTC
RFENDTC  <- fmt_is8601da(rfend_date)
RFPENDTC <- RFENDTC

# STUDYID / DOMAIN
STUDYID <- rep("CDISC-01", n)
DOMAIN  <- rep("DM", n)

# SUBJID = SUBJ_ID (length $20, truncate)
SUBJID <- sas_trunc(sas_strip(SUBJ_ID_raw), 20)
# USUBJID = catx("-", STUDYID, SUBJID)  (length $40)
USUBJID <- sas_trunc(ifelse(is.na(SUBJID) | SUBJID == "", STUDYID,
                     paste0(STUDYID, "-", SUBJID)), 40)

# SITEID length $10 : strip(SITE) char branch
SITEID <- sas_trunc(sas_strip(SITE_raw), 10)
SITEID[SITEID == ""] <- NA_character_

# SEX length $1 : upcase(char(GENDER,1))
SEX <- rep("U", n)
for (i in seq_len(n)) {
  g <- GENDER_raw[i]
  if (is.na(g)) { SEX[i] <- "U"; next }
  gs <- as.character(g)
  if (nchar(gs, keepNA = TRUE) == 0 || is.na(gs) || trimws(gs) == "") {
    # SAS char(missing,1) = blank -> U ; also blank string -> U
    # check raw first char including blanks
    if (is.na(gs) || gs == "") { SEX[i] <- "U"; next }
  }
  ch1 <- substr(gs, 1, 1)
  uch <- toupper(ch1)
  if (!is.na(uch) && uch == "M") SEX[i] <- "M"
  else if (!is.na(uch) && uch == "F") SEX[i] <- "F"
  else SEX[i] <- "U"
}
SEX <- sas_trunc(SEX, 1)

# RACE length $40
RACE_up <- toupper(sas_strip(RACE_TXT_raw))
RACE <- rep("UNKNOWN", n)
RACE[!is.na(RACE_up) & RACE_up == "WHITE"] <- "WHITE"
RACE[!is.na(RACE_up) & RACE_up == "BLACK"] <- "BLACK OR AFRICAN AMERICAN"
RACE[!is.na(RACE_up) & RACE_up == "ASIAN"] <- "ASIAN"
RACE[!is.na(RACE_up) & RACE_up == "OTHER"] <- "OTHER"
# otherwise UNKNOWN (includes NA, missing, other values)
RACE <- sas_trunc(RACE, 40)

# ARM ($20) / ARMCD ($8)
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

# Write dataset dm.csv
write.csv(sdtm_dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT (First 10 Records) : PROC PRINT
print_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
audit_df <- head(sdtm_dm[, print_vars, drop = FALSE], 10)
audit_df <- cbind(Obs = seq_len(nrow(audit_df)), audit_df)

# CSV of table body (without Obs? keep Obs for traceability - also provide without)
write.csv(audit_df, file.path(out, "P011.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
txt_path1 <- file.path(out, "P011.txt")
txt_path2 <- file.path(out, "P011.lst")
con_lines <- c(
  "DM Domain Audit (First 10 Records)",
  "",
  paste0("Dataset: DM  (N=", nrow(sdtm_dm), ", showing first ", nrow(audit_df), " records)"),
  ""
)
# Build fixed-width style table
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
