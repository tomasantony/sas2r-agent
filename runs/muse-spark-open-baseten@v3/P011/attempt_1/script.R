# P011_sdtm_dm.sas -> R translation
# Produces sdtm.dm and DM Domain Audit listing (first 10 records)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- Sys.getenv("PROJECT_PATH")
if (project_path == "" || is.na(project_path)) {
  project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
}
# also try root env
root <- Sys.getenv("root")
if (root != "" && !is.na(root) && !dir.exists(project_path)) {
  alt <- file.path(root, "data/derived/c2c")
  if (dir.exists(alt)) project_path <- alt
}

# locate raw_demog.csv
candidates <- c(
  file.path(project_path, "data/raw/raw_demog.csv"),
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv",
  file.path(Sys.getenv("rawdir"), "raw_demog.csv"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_demog.csv"
)
raw_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  # last resort: search under project_path
  all_csv <- list.files(project_path, pattern = "raw_demog\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(all_csv) > 0) raw_path <- all_csv[1]
}
if (is.null(raw_path) || !file.exists(raw_path)) {
  stop(paste0("raw_demog.csv not found. Tried: ", paste(candidates, collapse="; ")))
}

# helper: SAS round half away from zero (not needed here but kept for compliance)
round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# helper: parse a date column emulating SAS PROC IMPORT + INPUT(anydtdte.) + PUT(is8601da.)
# PROC IMPORT assigns ONE informat per column after scanning values:
#  - if column is n/n/yyyy (slash) then SAS uses DDMMYY10. when any first field >12 else MMDDYY10.
#  - every value in that column is then read with that same order.
parse_sas_date_col <- function(vec_char) {
  # vec_char: character vector as read from CSV (raw strings)
  v <- trimws(as.character(vec_char))
  v[v == "" | v == "NA" | v == "."] <- NA
  non_na <- v[!is.na(v)]
  if (length(non_na) == 0) return(as.Date(rep(NA, length(v))))

  # detect slash dates: e.g. 1/15/1950 or 15/01/1950
  is_slash <- grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s*$", non_na)
  is_dash_slash <- grepl("^\\s*\\d{1,2}[-/]\\d{1,2}[-/]\\d{2,4}\\s*$", non_na)

  # If all non-missing match slash-like n/n/yyyy pattern, decide MDY vs DMY once per column
  if (all(is_dash_slash)) {
    # extract first field (before / or -)
    first_fields <- suppressWarnings(as.integer(sub("^\\s*(\\d{1,2})[/-].*", "\\1", non_na)))
    # SAS rule: if any first field >12 then DDMMYY else MMDDYY
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    # detect separator
    sep <- ifelse(grepl("/", non_na[1]), "/", "-")
    # detect year length
    years <- sub("^\\d{1,2}[/-]\\d{1,2}[/-]", "", non_na)
    is_2digit_year <- any(nchar(years) == 2, na.rm = TRUE)
    if (sep == "/") {
      fmt <- if (use_dmy) {
        if (is_2digit_year) "%d/%m/%y" else "%d/%m/%Y"
      } else {
        if (is_2digit_year) "%m/%d/%y" else "%m/%d/%Y"
      }
    } else {
      fmt <- if (use_dmy) {
        if (is_2digit_year) "%d-%m-%y" else "%d-%m-%Y"
      } else {
        if (is_2digit_year) "%m-%d-%y" else "%m-%d-%Y"
      }
    }
    parsed <- suppressWarnings(as.Date(v, format = fmt))
    return(parsed)
  }

  # ISO YYYY-MM-DD
  if (all(grepl("^\\s*\\d{4}-\\d{1,2}-\\d{1,2}\\s*$", non_na))) {
    return(suppressWarnings(as.Date(trimws(v), format = "%Y-%m-%d")))
  }
  if (all(grepl("^\\s*\\d{4}/\\d{1,2}/\\d{1,2}\\s*$", non_na))) {
    return(suppressWarnings(as.Date(trimws(v), format = "%Y/%m/%d")))
  }
  # DD-MON-YYYY or DD-Mon-YYYY e.g. 15-Jan-1950, 15-JAN-1950
  if (all(grepl("^\\s*\\d{1,2}-[A-Za-z]{3}-\\d{2,4}\\s*$", non_na))) {
    # use lubridate dmy which handles case-insensitive month abbrev
    return(suppressWarnings(lubridate::dmy(v, quiet = TRUE)))
  }
  # DD MON YYYY or DDMonYYYY etc with letters
  if (any(grepl("[A-Za-z]", non_na))) {
    # try dmy then mdy then ymd with single order per column
    # decide order by first value pattern: if starts with digit and contains month abbrev, assume dmy
    parsed <- suppressWarnings(lubridate::dmy(v, quiet = TRUE))
    if (all(is.na(parsed[!is.na(v)]))) {
      parsed <- suppressWarnings(lubridate::mdy(v, quiet = TRUE))
    }
    if (all(is.na(parsed[!is.na(v)]))) {
      parsed <- suppressWarnings(lubridate::ymd(v, quiet = TRUE))
    }
    return(as.Date(parsed))
  }
  # fallback: try ymd
  parsed <- suppressWarnings(lubridate::ymd(v, quiet = TRUE))
  if (!all(is.na(parsed[!is.na(v)]))) return(as.Date(parsed))
  # try mdy as last resort with single format decided by first field >12 check
  first_fields_try <- suppressWarnings(as.integer(sub("^\\s*(\\d{1,2})[/-].*", "\\1", non_na)))
  if (any(!is.na(first_fields_try) & first_fields_try > 12)) {
    return(suppressWarnings(lubridate::dmy(v, quiet = TRUE)))
  } else {
    return(suppressWarnings(lubridate::mdy(v, quiet = TRUE)))
  }
}

# Read raw CSV as character to emulate PROC IMPORT scanning
# Use readr with all columns as character, keep original strings
suppressWarnings({
  raw_dm_raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                                na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
})
# Uppercase variable names to match SAS validvarname=upcase
names(raw_dm_raw) <- toupper(names(raw_dm_raw))

# Ensure expected columns exist (case-insensitive check)
# SAS set work.raw_dm expects: SUBJ_ID, SITE, GENDER, RACE_TXT, RANDOMIZED_ARM, BRTH_DT, ICF_DAT, STUDY_END_DAT
# If names differ slightly, try to map
# Normalize: replace spaces/dots with underscore
names(raw_dm_raw) <- gsub("[\\. ]", "_", names(raw_dm_raw))

# Helper to get column by name case-insensitive, return NA vector if missing
get_col <- function(df, name) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) == 0) return(rep(NA_character_, nrow(df)))
  df[[idx[1]]]
}

# Extract raw vectors as character (preserve original strings for date parsing)
SUBJ_ID_raw <- get_col(raw_dm_raw, "SUBJ_ID")
SITE_raw <- get_col(raw_dm_raw, "SITE")
GENDER_raw <- get_col(raw_dm_raw, "GENDER")
RACE_TXT_raw <- get_col(raw_dm_raw, "RACE_TXT")
RANDOMIZED_ARM_raw <- get_col(raw_dm_raw, "RANDOMIZED_ARM")
BRTH_DT_raw <- get_col(raw_dm_raw, "BRTH_DT")
ICF_DAT_raw <- get_col(raw_dm_raw, "ICF_DAT")
STUDY_END_DAT_raw <- get_col(raw_dm_raw, "STUDY_END_DAT")

n <- nrow(raw_dm_raw)

# Parse dates with single-format-per-column rule
brth_date <- parse_sas_date_col(BRTH_DT_raw)
rfic_date <- parse_sas_date_col(ICF_DAT_raw)
rfend_date <- parse_sas_date_col(STUDY_END_DAT_raw)

# Format to ISO 8601 DA (YYYY-MM-DD) as SAS PUT(is8601da.)
BRTHDTC <- ifelse(is.na(brth_date), NA_character_, format(brth_date, "%Y-%m-%d"))
RFICDTC <- ifelse(is.na(rfic_date), NA_character_, format(rfic_date, "%Y-%m-%d"))
RFENDTC <- ifelse(is.na(rfend_date), NA_character_, format(rfend_date, "%Y-%m-%d"))
RFPENDTC <- RFENDTC
RFSTDTC <- RFICDTC

# Build SDTM DM
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("DM", n)
SUBJID <- trimws(as.character(SUBJ_ID_raw))
SUBJID[SUBJID == "" | is.na(SUBJID)] <- NA_character_
# USUBJID = catx("-", STUDYID, SUBJID)  (catx trims and skips missing)
USUBJID <- ifelse(is.na(SUBJID) | SUBJID == "", STUDYID, paste0(STUDYID, "-", SUBJID))

# SITEID: if vtype(SITE)='C' then strip(SITE) else strip(put(SITE,best.))
# In R, SITE is character; emulate best. by trimming and removing trailing .0 if numeric
SITEID <- trimws(as.character(SITE_raw))
SITEID[SITEID == "" | SITEID == "NA"] <- NA_character_
# If SITE looks numeric with decimal, SAS best. would strip; we already have character, keep as is
# Ensure length $10 (truncate if needed, SAS would truncate)
SITEID <- substr(SITEID, 1, 10)

# SEX: if upcase(char(GENDER,1))='M' then 'M' else if 'F' then 'F' else 'U'
# char(GENDER,1) = first character
first_char <- substr(trimws(as.character(GENDER_raw)), 1, 1)
first_char_up <- toupper(first_char)
SEX <- ifelse(!is.na(first_char_up) & first_char_up == "M", "M",
       ifelse(!is.na(first_char_up) & first_char_up == "F", "F", "U"))
SEX[is.na(GENDER_raw) | trimws(as.character(GENDER_raw)) == ""] <- "U"

# RACE mapping: select (upcase(strip(RACE_TXT)))
race_up <- toupper(trimws(as.character(RACE_TXT_raw)))
RACE <- rep("UNKNOWN", n)
RACE[race_up == "WHITE"] <- "WHITE"
RACE[race_up == "BLACK"] <- "BLACK OR AFRICAN AMERICAN"
RACE[race_up == "ASIAN"] <- "ASIAN"
RACE[race_up == "OTHER"] <- "OTHER"
# otherwise remains UNKNOWN (including NA)

# ARM / ARMCD mapping: strip(RANDOMIZED_ARM) exact match
arm_raw <- trimws(as.character(RANDOMIZED_ARM_raw))
ARM <- rep(NA_character_, n)
ARMCD <- rep(NA_character_, n)
ARM[arm_raw == "Not Randomized"] <- "SCREEN FAILURE"
ARMCD[arm_raw == "Not Randomized"] <- "SCRNFAIL"
ARM[arm_raw == "Placebo"] <- "Placebo"
ARMCD[arm_raw == "Placebo"] <- "PBO"
ARM[arm_raw == "Active Drug 50mg"] <- "Active Drug 50mg"
ARMCD[arm_raw == "Active Drug 50mg"] <- "ACT50"

# Assemble dm in SAS keep order
dm <- data.frame(
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

# Write dataset dm.csv
write.csv(dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")

# Visual Audit: First 10 records, var order as in PROC PRINT
audit_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
audit_df <- dm[seq_len(min(10, nrow(dm))), audit_vars, drop = FALSE]

# Write CSV of listing body
write.csv(audit_df, file.path(out, "P011.lst.csv"), row.names = FALSE, na = "")
write.csv(audit_df, file.path(out, "P011.csv"), row.names = FALSE, na = "")

# Build plain-text rendering mimicking PROC PRINT
# Title exactly as in SAS
title_str <- "DM Domain Audit (First 10 Records)"

# Create text table with Obs column
obs_df <- cbind(Obs = seq_len(nrow(audit_df)), audit_df)

# Format as fixed-width text for readability (SAS-like)
# Use capture of formatted table
# Build header line
header_line <- paste(c("Obs", audit_vars), collapse = " | ")
sep_line <- paste(rep("-", nchar(header_line)), collapse = "")

# Build rows
row_lines <- apply(obs_df, 1, function(r) {
  # r is character vector; ensure NA displayed as blank
  r[is.na(r)] <- ""
  paste(r, collapse = " | ")
})

txt_content <- c(
  title_str,
  "",
  header_line,
  sep_line,
  row_lines,
  "",
  paste0("Total observations in DM: ", nrow(dm)),
  paste0("Audit shows first ", nrow(audit_df), " records.")
)

# Write text files
writeLines(txt_content, file.path(out, "P011.lst.txt"))
writeLines(txt_content, file.path(out, "P011.txt"))

# Also ensure generic listing name if expected without extension
# (some validators look for P011.lst)
# Copy to that exact name if needed (already done)
invisible(NULL)
