```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file1 <- paste0(project_path, "/data/raw/raw_demog.csv")
raw_file2 <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"
candidates <- c(raw_file1, raw_file2,
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv")
raw_file <- NULL
for (f in candidates) {
  if (file.exists(f)) { raw_file <- f; break }
}
if (is.null(raw_file)) {
  # fallback: search derived c2c raw dir
  dd <- paste0(project_path, "/data/raw")
  if (dir.exists(dd)) {
    fl <- list.files(dd, pattern = "raw_demog.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
    if (length(fl) > 0) raw_file <- fl[1]
  }
}
if (is.null(raw_file)) raw_file <- raw_file1
cat("Reading:", raw_file, "\n")

# SAS round half away from zero helper (not needed here but SAS semantics)
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5) / m
}
sas_strip <- function(x) {
  # SAS STRIP: remove leading/trailing blanks; NA stays NA
  v <- as.character(x)
  v[is.na(v)] <- NA_character_
  trimws(v)
}
sas_upcase <- function(x) {
  v <- as.character(x)
  toupper(v)
}
sas_trunc <- function(x, len) {
  v <- as.character(x)
  isna <- is.na(v)
  v[!isna] <- substr(v[!isna], 1, len)
  v[isna] <- NA_character_
  v
}
sas_catx <- function(delim, ...) {
  # vectorised catx over rows: ... are vectors same length
  args <- list(...)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(args, function(a) trimws(a[i]))
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}
sas_best <- function(x) {
  # SAS BEST. numeric to character, stripped
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (abs(v - round(v)) < 1e-9 && abs(v) < 1e12) {
      formatC(round(v), format = "f", digits = 0, flag = "")
    } else {
      format(v, scientific = FALSE, trim = TRUE)
    }
  }, USE.NAMES = FALSE)
}

# 2. IMPORT RAW EDC DATA (PROC IMPORT DBMS=CSV GETNAMES=YES, VALIDVARNAME=UPCASE)
# Read everything as character to preserve raw strings, then emulate SAS type guessing
raw0 <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"), trim_ws = FALSE, progress = FALSE, show_col_types = FALSE)
raw0 <- as.data.frame(raw0, stringsAsFactors = FALSE)
names(raw0) <- gsub("[^A-Z0-9_]", "_", toupper(trimws(names(raw0))))
cat("Columns:", paste(names(raw0), collapse = ","), "\n")
cat("Rows:", nrow(raw0), "\n")

# Required columns (uppercased)
stopifnot("SUBJ_ID" %in% names(raw0))
stopifnot("SITE" %in% names(raw0))
stopifnot("GENDER" %in% names(raw0))
stopifnot("RACE_TXT" %in% names(raw0))
stopifnot("RANDOMIZED_ARM" %in% names(raw0))
stopifnot("BRTH_DT" %in% names(raw0))
stopifnot("ICF_DAT" %in% names(raw0))
stopifnot("STUDY_END_DAT" %in% names(raw0))

# Helper: decide if a column would be numeric under PROC IMPORT (GUESSINGROWS=20)
is_numeric_import_col <- function(vec) {
  n <- min(20L, length(vec))
  g <- trimws(as.character(vec[seq_len(n)]))
  g <- g[!is.na(g) & g != ""]
  if (length(g) == 0) return(FALSE)
  all(grepl("^-?[0-9]+(\\.[0-9]+)?$", g))
}

# Helper: PROC IMPORT date column -> ISO with ONE fixed order per column
import_date_iso <- function(vec) {
  t <- trimws(as.character(vec))
  t[t == ""] <- NA_character_
  n <- length(t)
  nonmiss <- t[!is.na(t)]
  if (length(nonmiss) == 0) return(rep(NA_character_, n))
  slash_pat <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", nonmiss)
  if (all(slash_pat)) {
    # numeric SAS date branch: decide MMDDYY vs DDMMYY once from all values
    firsts <- suppressWarnings(as.integer(sub("^([0-9]{1,2})/.*$", "\\1", nonmiss)))
    use_dmy <- any(firsts > 12, na.rm = TRUE)
    fmt <- if (isTRUE(use_dmy)) "%d/%m/%Y" else "%m/%d/%Y"
    parsed <- as.Date(t, format = fmt)
    iso <- format(parsed, "%Y-%m-%d")
    iso[is.na(parsed)] <- NA_character_
    return(iso)
  }
  # character branch (ANYDTDTE with one fixed order): detect ONE format once
  if (all(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", nonmiss))) {
    parsed <- as.Date(t, format = "%Y-%m-%d")
    iso <- format(parsed, "%Y-%m-%d")
    iso[is.na(parsed)] <- NA_character_
    return(iso)
  }
  if (all(grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2,4}$", nonmiss))) {
    t_norm <- sapply(t, function(x) {
      if (is.na(x)) return(NA_character_)
      m <- regmatches(x, regexec("^(\\d{1,2})-([A-Za-z]{3})-(\\d{2,4})$", x))[[1]]
      if (length(m) == 4) {
        mon <- paste0(toupper(substr(m[3], 1, 1)), tolower(substr(m[3], 2, 3)))
        paste0(m[2], "-", mon, "-", m[4])
      } else x
    }, USE.NAMES = FALSE)
    parsed <- as.Date(t_norm, format = "%d-%b-%Y")
    iso <- format(parsed, "%Y-%m-%d")
    iso[is.na(parsed)] <- NA_character_
    return(iso)
  }
  # fallback single format: if slash present use MDY, else YMD
  has_slash <- any(grepl("/", nonmiss, fixed = TRUE))
  if (has_slash) {
    parsed <- as.Date(t, format = "%m/%d/%Y")
  } else {
    parsed <- as.Date(t, format = "%Y-%m-%d")
  }
  iso <- format(parsed, "%Y-%m-%d")
  iso[is.na(parsed)] <- NA_character_
  iso
}

# 3. TRANSFORMATION AND SDTM MAPPING -> sdtm.dm
# Emulate VTYPE checks
# SITEID
site_raw <- raw0$SITE
if (is_numeric_import_col(site_raw)) {
  sitenum <- suppressWarnings(as.numeric(trimws(as.character(site_raw))))
  # missing where original blank/NA
  sitenum[is.na(trimws(as.character(site_raw))) | trimws(as.character(site_raw)) == ""] <- NA_real_
  SITEID_raw <- sas_best(sitenum)
} else {
  SITEID_raw <- sas_strip(site_raw)
  SITEID_raw[SITEID_raw == ""] <- NA_character_
}
# SUBJID (SUBJ_ID): SAS SUBJID = SUBJ_ID with auto conversion if numeric
subj_raw <- raw0$SUBJ_ID
if (is_numeric_import_col(subj_raw)) {
  subjnum <- suppressWarnings(as.numeric(trimws(as.character(subj_raw))))
  subjnum[is.na(trimws(as.character(subj_raw))) | trimws(as.character(subj_raw)) == ""] <- NA_real_
  SUBJID_raw <- sas_best(subjnum)
} else {
  SUBJID_raw <- sas_strip(subj_raw)
  SUBJID_raw[SUBJID_raw == ""] <- NA_character_
}

GENDER_raw <- as.character(raw0$GENDER)
RACE_TXT_raw <- as.character(raw0$RACE_TXT)
RANDARM_raw <- as.character(raw0$RANDOMIZED_ARM)

n <- nrow(raw0)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("DM", n)
SUBJID <- sas_trunc(SUBJID_raw, 20)
USUBJID <- sas_catx("-", STUDYID, SUBJID)
USUBJID <- sas_trunc(USUBJID, 40)
SITEID <- sas_trunc(SITEID_raw, 10)

# SEX: if upcase(char(GENDER,1))='M' then M else if 'F' then F else U
first_char <- substr(as.character(GENDER_raw), 1, 1)
first_up <- toupper(first_char)
SEX <- rep("U", n)
SEX[!is.na(first_up) & first_up == "M"] <- "M"
SEX[!is.na(first_up) & first_up == "F"] <- "F"
SEX <- sas_trunc(SEX, 1)

# RACE controlled terminology
race_key <- toupper(trimws(as.character(RACE_TXT_raw)))
RACE <- rep("UNKNOWN", n)
RACE[!is.na(race_key) & race_key == "WHITE"] <- "WHITE"
RACE[!is.na(race_key) & race_key == "BLACK"] <- "BLACK OR AFRICAN AMERICAN"
RACE[!is.na(race_key) & race_key == "ASIAN"] <- "ASIAN"
RACE[!is.na(race_key) & race_key == "OTHER"] <- "OTHER"
RACE <- sas_trunc(RACE, 40)

# ARM / ARMCD
ARM <- rep(NA_character_, n)
ARMCD <- rep(NA_character_, n)
rand_strip <- trimws(as.character(RANDARM_raw))
idx1 <- !is.na(rand_strip) & rand_strip == "Not Randomized"
ARM[idx1] <- "SCREEN FAILURE"
ARMCD[idx1] <- "SCRNFAIL"
idx2 <- !is.na(rand_strip) & rand_strip == "Placebo"
ARM[idx2] <- "Placebo"
ARMCD[idx2] <- "PBO"
idx3 <- !is.na(rand_strip) & rand_strip == "Active Drug 50mg"
ARM[idx3] <- "Active Drug 50mg"
ARMCD[idx3] <- "ACT50"
ARM <- sas_trunc(ARM, 20)
ARMCD <- sas_trunc(ARMCD, 8)

# ISO 8601 dates with dynamic type-checking emulated via import_date_iso
BRTHDTC <- import_date_iso(raw0$BRTH_DT)
RFICDTC <- import_date_iso(raw0$ICF_DAT)
RFSTDTC <- RFICDTC
RFEND_raw_iso <- import_date_iso(raw0$STUDY_END_DAT)
RFENDTC <- RFEND_raw_iso
RFPENDTC <- RFEND_raw_iso

dm <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID, SUBJID = SUBJID,
  SITEID = SITEID, SEX = SEX, RACE = RACE, BRTHDTC = BRTHDTC,
  RFSTDTC = RFSTDTC, RFENDTC = RFENDTC, RFPENDTC = RFPENDTC, RFICDTC = RFICDTC,
  ARM = ARM, ARMCD = ARMCD, stringsAsFactors = FALSE
)

# keep order already as required
dm <- dm[, c("STUDYID","DOMAIN","USUBJID","SUBJID","SITEID","SEX","RACE","BRTHDTC","RFSTDTC","RFENDTC","RFPENDTC","RFICDTC","ARM","ARMCD")]

# Write dataset dm
write.csv(dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT (First 10 Records) : PROC PRINT
audit_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
audit_n <- min(10L, nrow(dm))
audit <- dm[seq_len(audit_n), audit_vars, drop = FALSE]
audit$Obs <- seq_len(audit_n)
audit <- audit[, c("Obs", audit_vars), drop = FALSE]

# CSV of table body (without Obs? keep Obs for traceability; also write without Obs version)
write.csv(audit[, audit_vars, drop = FALSE], file.path(out, "P011.csv"), row.names = FALSE, na = "")
# also write with Obs for reference
write.csv(audit, file.path(out, "P011_body.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
txt_file <- file.path(out, "P011.txt")
lst_file <- file.path(out, "P011.lst")
con <- file(txt_file, open = "wt")
cat("DM Domain Audit (First 10 Records)\n", file = con)
cat("\n", file = con)
# header
hdr <- sprintf("%-4s %-8s %-20s %-6s %-3s %-25s %-10s %-10s %-10s %-10s %-10s %-16s %-8s",
  "Obs","STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
cat(hdr, "\n", file = con, sep = "")
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(audit_n)) {
  r <- audit[i, ]
  disp <- function(x) { if (is.na(x) || x == "") "" else as.character(x) }
  line <- sprintf("%-4s %-8s %-20s %-6s %-3s %-25s %-10s %-10s %-10s %-10s %-10s %-16s %-8s",
    disp(r$Obs), disp(r$STUDYID), disp(r$USUBJID), disp(r$SITEID), disp(r$SEX),
    disp(r$RACE), disp(r$BRTHDTC), disp(r$RFICDTC), disp(r$RFSTDTC),
    disp(r$RFENDTC), disp(r$RFPENDTC), disp(r$ARM), disp(r$ARMCD))
  cat(line, "\n", file = con, sep = "")
}
close(con)
file.copy(txt_file, lst_file, overwrite = TRUE)
cat("Done. dm rows:", nrow(dm), " audit rows:", audit_n, "\n")
```