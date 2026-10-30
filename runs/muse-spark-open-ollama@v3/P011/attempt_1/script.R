out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Load allowed packages (no install)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

# ------------------------------------------------------------------
# Helper: SAS round half away from zero (not needed here but kept)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}

# Helper: decide date order for a column of n/n/yyyy values
# SAS PROC IMPORT: one informat per column, DDMMYY10. if any first field >12 else MMDDYY10.
decide_order <- function(vec) {
  vals <- vec[!is.na(vec) & trimws(vec) != ""]
  if (length(vals) == 0) return(NA_character_)
  slash_vals <- vals[grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s*$", vals)]
  if (length(slash_vals) == 0) return(NA_character_)
  first_fields <- suppressWarnings(as.integer(sapply(strsplit(trimws(slash_vals), "/"), `[`, 1)))
  first_fields <- first_fields[!is.na(first_fields)]
  if (length(first_fields) == 0) return(NA_character_)
  if (any(first_fields > 12, na.rm = TRUE)) "DMY" else "MDY"
}

# Helper: parse a date column with ONE fixed order (SAS semantics)
parse_date_col <- function(vec, order) {
  n <- length(vec)
  res <- rep(as.Date(NA), n)
  vec_trim <- trimws(as.character(vec))
  idx <- !is.na(vec_trim) & vec_trim != "" & toupper(vec_trim) != "NA"
  if (sum(idx) == 0) return(res)
  v <- vec_trim[idx]
  if (!is.na(order) && order %in% c("DMY", "MDY")) {
    if (order == "DMY") {
      parsed <- suppressWarnings(lubridate::dmy(v, quiet = TRUE))
    } else {
      parsed <- suppressWarnings(lubridate::mdy(v, quiet = TRUE))
    }
    res[idx] <- parsed
  } else {
    # No slash pattern: try ISO ymd, then dmy with month abbrev
    parsed <- suppressWarnings(lubridate::ymd(v, quiet = TRUE))
    need <- is.na(parsed)
    if (any(need)) {
      # try dmy (e.g., 12JAN2000, 12-Jan-2000, 12 Jan 2000)
      parsed2 <- suppressWarnings(lubridate::dmy(v[need], quiet = TRUE))
      parsed[need] <- parsed2
    }
    # still NA try mdy as fallback (SAS ANYDTDTE can handle)
    need2 <- is.na(parsed)
    if (any(need2)) {
      parsed3 <- suppressWarnings(lubridate::mdy(v[need2], quiet = TRUE))
      parsed[need2] <- parsed3
    }
    res[idx] <- parsed
  }
  res
}

# ------------------------------------------------------------------
# Locate raw_demog.csv (primary path from &project_path.)
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv",
  file.path(Sys.getenv("project_path"), "data/raw/raw_demog.csv"),
  "data/raw/raw_demog.csv"
)
raw_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  # last resort: search under root
  root <- Sys.getenv("root")
  if (root == "") root <- "D:/meta_hackathon/sas2r-agent"
  alt <- file.path(root, "data/derived/c2c/data/raw/raw_demog.csv")
  if (file.exists(alt)) raw_path <- alt
}
if (is.null(raw_path)) stop("raw_demog.csv not found in any candidate path")

# Read raw CSV as character (emulate PROC IMPORT scanning)
raw_dm <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                          show_col_types = FALSE, na = c("", "NA"), trim_ws = TRUE)
# SAS validvarname=upcase
names(raw_dm) <- toupper(names(raw_dm))

# Ensure expected columns exist (create NA if missing)
expected_raw <- c("SUBJ_ID","SITE","GENDER","RACE_TXT","RANDOMIZED_ARM","BRTH_DT","ICF_DAT","STUDY_END_DAT")
for (cn in expected_raw) if (!cn %in% names(raw_dm)) raw_dm[[cn]] <- NA_character_

# ------------------------------------------------------------------
# 3. TRANSFORMATION AND SDTM MAPPING
# Decide order once per date column (SAS PROC IMPORT GUESSINGROWS logic)
date_cols <- c("BRTH_DT","ICF_DAT","STUDY_END_DAT")
orders <- sapply(date_cols, function(col) decide_order(raw_dm[[col]]))
names(orders) <- date_cols

brth_date  <- parse_date_col(raw_dm[["BRTH_DT"]], orders["BRTH_DT"])
rfic_date  <- parse_date_col(raw_dm[["ICF_DAT"]], orders["ICF_DAT"])
rfend_date <- parse_date_col(raw_dm[["STUDY_END_DAT"]], orders["STUDY_END_DAT"])

BRTHDTC <- ifelse(is.na(brth_date), NA_character_, format(brth_date, "%Y-%m-%d"))
RFICDTC <- ifelse(is.na(rfic_date), NA_character_, format(rfic_date, "%Y-%m-%d"))
RFENDTC <- ifelse(is.na(rfend_date), NA_character_, format(rfend_date, "%Y-%m-%d"))
RFSTDTC <- RFICDTC
RFPENDTC <- RFENDTC

# SITEID: vtype(SITE)='C' then strip(SITE) else strip(put(SITE,best.))
site_raw <- raw_dm[["SITE"]]
SITEID <- trimws(as.character(site_raw))
SITEID[SITEID == ""] <- NA_character_
SITEID[is.na(site_raw)] <- NA_character_

# STUDYID, DOMAIN, SUBJID, USUBJID
STUDYID <- rep("CDISC-01", nrow(raw_dm))
DOMAIN  <- rep("DM", nrow(raw_dm))
# SUBJ_ID may be numeric in SAS but we have character
subj_raw <- raw_dm[["SUBJ_ID"]]
SUBJID <- trimws(as.character(subj_raw))
SUBJID[SUBJID == ""] <- NA_character_
# USUBJID = catx("-", STUDYID, SUBJID)  (catx trims and skips missing)
USUBJID <- ifelse(is.na(SUBJID), STUDYID, paste0(STUDYID, "-", SUBJID))
# catx would return STUDYID if SUBJID missing; above does that

# SEX: upcase(char(GENDER,1)) = 'M'/'F' else 'U'
gender_raw <- raw_dm[["GENDER"]]
first_char <- substr(trimws(as.character(gender_raw)), 1, 1)
first_up <- toupper(first_char)
SEX <- rep("U", nrow(raw_dm))
SEX[first_up == "M"] <- "M"
SEX[first_up == "F"] <- "F"
# missing stays U (SAS would also go to else)
SEX[is.na(gender_raw) | trimws(as.character(gender_raw)) == ""] <- "U"
# ensure any other char -> U
SEX[!first_up %in% c("M","F") & !is.na(first_up) & first_up != ""] <- "U"

# RACE: select upcase(strip(RACE_TXT))
race_txt_raw <- raw_dm[["RACE_TXT"]]
race_up <- toupper(trimws(as.character(race_txt_raw)))
RACE <- rep("UNKNOWN", nrow(raw_dm))
RACE[race_up == "WHITE"] <- "WHITE"
RACE[race_up == "BLACK"] <- "BLACK OR AFRICAN AMERICAN"
RACE[race_up == "ASIAN"] <- "ASIAN"
RACE[race_up == "OTHER"] <- "OTHER"
# otherwise UNKNOWN (including NA/blank)
RACE[is.na(race_up) | race_up == ""] <- "UNKNOWN"

# ARM / ARMCD
rand_raw <- raw_dm[["RANDOMIZED_ARM"]]
rand_trim <- trimws(as.character(rand_raw))
ARM <- rep(NA_character_, nrow(raw_dm))
ARMCD <- rep(NA_character_, nrow(raw_dm))
ARM[rand_trim == "Not Randomized"] <- "SCREEN FAILURE"
ARMCD[rand_trim == "Not Randomized"] <- "SCRNFAIL"
ARM[rand_trim == "Placebo"] <- "Placebo"
ARMCD[rand_trim == "Placebo"] <- "PBO"
ARM[rand_trim == "Active Drug 50mg"] <- "Active Drug 50mg"
ARMCD[rand_trim == "Active Drug 50mg"] <- "ACT50"

# Build SDTM DM (keep only SDTM variables, retain order)
dm <- data.frame(
  STUDYID = STUDYID,
  DOMAIN  = DOMAIN,
  USUBJID = USUBJID,
  SUBJID  = SUBJID,
  SITEID  = SITEID,
  SEX     = SEX,
  RACE    = RACE,
  BRTHDTC = BRTHDTC,
  RFSTDTC = RFSTDTC,
  RFENDTC = RFENDTC,
  RFPENDTC= RFPENDTC,
  RFICDTC = RFICDTC,
  ARM     = ARM,
  ARMCD   = ARMCD,
  stringsAsFactors = FALSE
)
# Ensure column order exactly as KEEP
dm <- dm[, c("STUDYID","DOMAIN","USUBJID","SUBJID","SITEID","SEX","RACE","BRTHDTC","RFSTDTC","RFENDTC","RFPENDTC","RFICDTC","ARM","ARMCD")]

# Write dataset dm.csv (dates as YYYY-MM-DD, na="")
write.csv(dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 4. VISUAL AUDIT (Quality Check) -> P011.lst
# PROC PRINT data=sdtm.dm(obs=10); var STUDYID USUBJID SITEID SEX RACE BRTHDTC RFICDTC RFSTDTC RFENDTC RFPENDTC ARM ARMCD;
listing_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
n_show <- min(10, nrow(dm))
listing_df <- dm[seq_len(n_show), listing_vars, drop = FALSE]
listing_display <- cbind(Obs = seq_len(n_show), listing_df)

# CSV of listing body (required)
write.csv(listing_df, file.path(out, "P011.csv"), row.names = FALSE, na = "")
write.csv(listing_df, file.path(out, "P011.lst.csv"), row.names = FALSE, na = "")

# Text rendering (PROC PRINT style)
# Build fixed-width text with title and column headers
title_txt <- "DM Domain Audit (First 10 Records)"
# Create header line
hdr <- paste(c("Obs", listing_vars), collapse = " | ")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
lines <- c(title_txt, "", hdr, sep)
for (i in seq_len(nrow(listing_display))) {
  vals <- sapply(listing_display[i, ], function(x) ifelse(is.na(x) || x == "", "", as.character(x)))
  # Obs as integer
  lines <- c(lines, paste(vals, collapse = " | "))
}
lines <- c(lines, "", paste0("N = ", nrow(dm), " total subjects, ", n_show, " displayed"))
# Add footnote blank line to mimic SAS title reset
lines <- c(lines, "")

txt1 <- file.path(out, "P011.txt")
txt2 <- file.path(out, "P011.lst.txt")
writeLines(lines, txt1)
writeLines(lines, txt2)

# Also write generic listing text name expected by some validators
writeLines(lines, file.path(out, "P011.lst"))
