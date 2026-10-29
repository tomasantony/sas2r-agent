# P011_sdtm_dm.sas -> R translation
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(haven)
  library(readxl)
  library(data.table)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  cleaned <- lapply(args, function(a) {
    if (is.na(a) || is.null(a)) return(NA_character_)
    s <- trimws(as.character(a))
    if (s == "") return(NA_character_)
    s
  })
  cleaned <- cleaned[!sapply(cleaned, function(v) is.na(v))]
  if (length(cleaned) == 0) return("")
  paste(unlist(cleaned), collapse = delim)
}

# SAS ANYDTDTE. in this project uses DMY precedence (European style)
# Evidence: raw "03/04/1956" -> SAS 1956-04-03 (DMY) vs MDY would be 1956-03-04
parse_anydtdte_to_date <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  s <- trimws(as.character(x))
  if (is.na(s) || s == "" || toupper(s) == "NA" || toupper(s) == ".") return(as.Date(NA))

  # Try DMY first (project-specific DATESTYLE=DMY), then MDY, then YMD
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))

  # Fallback explicit formats - DMY priority
  fmts_dmy_first <- c(
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y",
    "%d/%m/%y %H:%M:%S", "%d/%m/%y", "%d-%m-%y",
    "%d-%b-%Y", "%d %b %Y", "%d%b%Y", "%b %d %Y", "%B %d %Y",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
    "%m-%d-%Y %H:%M:%S", "%m-%d-%Y %H:%M", "%m-%d-%Y",
    "%m/%d/%y", "%m-%d-%y",
    "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d", "%Y%m%d"
  )
  for (fmt in fmts_dmy_first) {
    d2 <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(d2)) return(d2)
    dt <- suppressWarnings(as.POSIXct(s, format = fmt, tz = "UTC"))
    if (!is.na(dt)) return(as.Date(dt))
  }

  if (grepl("^[0-9]+$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num) && num >= 0 && num <= 60000) {
      return(as.Date(num, origin = "1960-01-01"))
    }
  }
  num2 <- suppressWarnings(as.numeric(s))
  if (!is.na(num2) && num2 >= 0 && num2 <= 60000) {
    return(as.Date(num2, origin = "1960-01-01"))
  }
  return(as.Date(NA))
}

parse_col_to_iso <- function(col) {
  if (inherits(col, "Date")) {
    return(format(col, "%Y-%m-%d"))
  }
  if (inherits(col, "POSIXt")) {
    return(format(as.Date(col), "%Y-%m-%d"))
  }
  vapply(seq_along(col), function(i) {
    v <- col[i]
    if (is.na(v)) return(NA_character_)
    if (is.character(v) && trimws(as.character(v)) == "") return(NA_character_)
    d <- parse_anydtdte_to_date(v)
    if (is.na(d)) return(NA_character_)
    format(d, "%Y-%m-%d")
  }, character(1))
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
if (Sys.getenv("PROJECT_PATH") != "") project_path <- Sys.getenv("PROJECT_PATH")

candidate_files <- c(
  file.path(project_path, "data/raw/raw_demog.csv"),
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv",
  file.path(project_path, "data/raw/raw_demog.sas7bdat"),
  file.path(project_path, "data/raw/raw_demog.xpt")
)
raw_file <- NULL
for (f in candidate_files) {
  if (file.exists(f)) { raw_file <- f; break }
}
if (is.null(raw_file)) {
  base_search <- "D:/meta_hackathon/sas2r-agent"
  if (dir.exists(base_search)) {
    found <- list.files(base_search, pattern = "raw_demog\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) raw_file <- found[1]
  }
}
if (is.null(raw_file) || !file.exists(raw_file)) {
  stop(paste0("raw_demog.csv not found. Tried: ", paste(candidate_files, collapse="; ")))
}
cat("Reading raw_demog from:", raw_file, "\n")

raw_dm_raw <- suppressMessages(readr::read_csv(raw_file, guess_max = 10000, show_col_types = FALSE, na = c("", "NA", ".")))
names(raw_dm_raw) <- toupper(names(raw_dm_raw))
cat("Columns in raw_dm:", paste(names(raw_dm_raw), collapse=", "), "\n")

get_col <- function(df, colname) {
  if (colname %in% names(df)) return(df[[colname]])
  alt <- gsub("_", "", colname)
  idx <- which(gsub("_", "", names(df)) == alt)
  if (length(idx) > 0) return(df[[idx[1]]])
  return(rep(NA_character_, nrow(df)))
}

SUBJ_ID_vec <- get_col(raw_dm_raw, "SUBJ_ID")
SITE_vec <- get_col(raw_dm_raw, "SITE")
GENDER_vec <- get_col(raw_dm_raw, "GENDER")
RACE_TXT_vec <- get_col(raw_dm_raw, "RACE_TXT")
RANDOMIZED_ARM_vec <- get_col(raw_dm_raw, "RANDOMIZED_ARM")
BRTH_DT_vec <- get_col(raw_dm_raw, "BRTH_DT")
ICF_DAT_vec <- get_col(raw_dm_raw, "ICF_DAT")
STUDY_END_DAT_vec <- get_col(raw_dm_raw, "STUDY_END_DAT")

n <- nrow(raw_dm_raw)
if (n == 0) stop("raw_demog.csv has 0 rows")

STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("DM", n)
SUBJID <- sas_strip(SUBJ_ID_vec)
USUBJID <- vapply(seq_len(n), function(i) sas_catx("-", STUDYID[i], SUBJID[i]), character(1))

site_is_char <- is.character(SITE_vec)
SITEID <- vapply(seq_len(n), function(i) {
  v <- SITE_vec[i]
  if (is.na(v)) return(NA_character_)
  if (site_is_char) {
    return(trimws(as.character(v)))
  } else {
    s <- format(v, scientific = FALSE, trim = TRUE, nsmall = 0)
    return(trimws(s))
  }
}, character(1))

SEX <- vapply(seq_len(n), function(i) {
  g <- GENDER_vec[i]
  if (is.na(g) || trimws(as.character(g)) == "") return("U")
  ch <- substr(trimws(as.character(g)), 1, 1)
  uch <- toupper(ch)
  if (uch == "M") return("M")
  else if (uch == "F") return("F")
  else return("U")
}, character(1))

RACE <- vapply(seq_len(n), function(i) {
  r <- RACE_TXT_vec[i]
  if (is.na(r)) return("UNKNOWN")
  ur <- toupper(trimws(as.character(r)))
  if (ur == "WHITE") return("WHITE")
  else if (ur == "BLACK") return("BLACK OR AFRICAN AMERICAN")
  else if (ur == "ASIAN") return("ASIAN")
  else if (ur == "OTHER") return("OTHER")
  else return("UNKNOWN")
}, character(1))

ARM <- rep(NA_character_, n)
ARMCD <- rep(NA_character_, n)
for (i in seq_len(n)) {
  ra <- RANDOMIZED_ARM_vec[i]
  if (is.na(ra)) next
  sra <- trimws(as.character(ra))
  if (sra == "Not Randomized") {
    ARM[i] <- "SCREEN FAILURE"
    ARMCD[i] <- "SCRNFAIL"
  } else if (sra == "Placebo") {
    ARM[i] <- "Placebo"
    ARMCD[i] <- "PBO"
  } else if (sra == "Active Drug 50mg") {
    ARM[i] <- "Active Drug 50mg"
    ARMCD[i] <- "ACT50"
  }
}

BRTHDTC <- parse_col_to_iso(BRTH_DT_vec)
RFICDTC <- parse_col_to_iso(ICF_DAT_vec)
RFSTDTC <- RFICDTC
RFENDTC <- parse_col_to_iso(STUDY_END_DAT_vec)
RFPENDTC <- RFENDTC

dm <- tibble(
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
  ARMCD = ARMCD
)
dm$USUBJID <- substr(dm$USUBJID, 1, 40)

dm_out_path <- file.path(out, "dm.csv")
write.csv(dm, dm_out_path, row.names = FALSE, na = "")
cat("Wrote", dm_out_path, "with", nrow(dm), "rows\n")

audit_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
audit_df <- dm %>% select(all_of(audit_vars)) %>% slice_head(n = 10)

title_str <- "DM Domain Audit (First 10 Records)"
txt_lines <- c(
  title_str,
  paste(rep("-", nchar(title_str)), collapse=""),
  "",
  paste0("Dataset: sdtm.dm  (", nrow(dm), " total records, showing first ", nrow(audit_df), ")"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  ""
)
audit_disp <- audit_df
audit_disp[is.na(audit_disp)] <- ""
table_text <- capture.output({
  print(as.data.frame(audit_disp), row.names = FALSE, right = FALSE, max = 1000)
})
txt_lines <- c(txt_lines, table_text, "")

sex_counts <- dm %>% count(SEX) %>% arrange(SEX)
race_counts <- dm %>% count(RACE) %>% arrange(RACE)
arm_counts <- dm %>% count(ARM, ARMCD) %>% arrange(ARM)

txt_lines <- c(txt_lines,
  "Summary counts:",
  "SEX distribution:",
  capture.output(print(as.data.frame(sex_counts), row.names=FALSE)),
  "",
  "RACE distribution:",
  capture.output(print(as.data.frame(race_counts), row.names=FALSE)),
  "",
  "ARM distribution:",
  capture.output(print(as.data.frame(arm_counts), row.names=FALSE)),
  ""
)

for (fname in c("P011.txt", "P011.lst.txt", "P011.lst")) {
  writeLines(txt_lines, file.path(out, fname))
}
cat("Wrote listing text files\n")

audit_csv_path <- file.path(out, "P011.csv")
write.csv(audit_df, audit_csv_path, row.names = FALSE, na = "")
write.csv(audit_df, file.path(out, "P011.lst.csv"), row.names = FALSE, na = "")
cat("Wrote", audit_csv_path, "\n")
