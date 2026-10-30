out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"

suppressMessages(library(readr))

raw_dm <- readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 10000)
names(raw_dm) <- toupper(names(raw_dm))

stopifnot("SUBJ_ID" %in% names(raw_dm))
stopifnot("SITE" %in% names(raw_dm))
stopifnot("GENDER" %in% names(raw_dm))
stopifnot("RACE_TXT" %in% names(raw_dm))
stopifnot("RANDOMIZED_ARM" %in% names(raw_dm))
stopifnot("BRTH_DT" %in% names(raw_dm))
stopifnot("ICF_DAT" %in% names(raw_dm))
stopifnot("STUDY_END_DAT" %in% names(raw_dm))

parse_anydtdte_to_date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(as.Date(v))
  if (is.numeric(v) || is.integer(v)) {
    return(as.Date(v, origin = "1960-01-01"))
  }
  if (is.logical(v)) {
    return(as.Date(rep(NA_real_, length(v)), origin = "1970-01-01"))
  }
  s <- trimws(as.character(v))
  n <- length(s)
  res <- as.Date(rep(NA_character_, n))
  is_missing <- is.na(s) | s == ""
  idx <- !is_missing & is.na(res) & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}[ T].*$", s)
  if (any(idx, na.rm = TRUE)) {
    res[idx] <- as.Date(substr(s[idx], 1, 10), format = "%Y-%m-%d")
  }
  idx <- !is_missing & is.na(res) & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", s)
  if (any(idx, na.rm = TRUE)) res[idx] <- as.Date(s[idx], format = "%Y-%m-%d")
  idx <- !is_missing & is.na(res) & grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", s)
  if (any(idx, na.rm = TRUE)) res[idx] <- as.Date(s[idx], format = "%Y/%m/%d")
  idx <- !is_missing & is.na(res) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", s)
  if (any(idx, na.rm = TRUE)) res[idx] <- as.Date(s[idx], format = "%m/%d/%Y")
  idx <- !is_missing & is.na(res) & grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", s)
  if (any(idx, na.rm = TRUE)) res[idx] <- as.Date(s[idx], format = "%m-%d-%Y")
  idx <- !is_missing & is.na(res) & grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2,4}$", s)
  if (any(idx, na.rm = TRUE)) {
    tmp <- s[idx]
    parts <- strsplit(tmp, "-")
    norm <- vapply(parts, function(p) {
      if (length(p) != 3) return(paste(p, collapse = "-"))
      mon <- paste0(toupper(substr(p[2], 1, 1)), tolower(substr(p[2], 2, 3)))
      paste0(p[1], "-", mon, "-", p[3])
    }, character(1))
    parsed <- as.Date(norm, format = "%d-%b-%Y")
    yrlen <- nchar(sub(".*-", "", norm))
    need2 <- is.na(parsed) & yrlen == 2
    if (any(need2, na.rm = TRUE)) parsed[need2] <- as.Date(norm[need2], format = "%d-%b-%y")
    res[idx] <- parsed
  }
  idx <- !is_missing & is.na(res) & grepl("^\\d{1,2} [A-Za-z]{3} \\d{4}$", s)
  if (any(idx, na.rm = TRUE)) {
    tmp <- s[idx]
    norm <- vapply(strsplit(tmp, " "), function(p) {
      if (length(p) != 3) return(paste(p, collapse = " "))
      mon <- paste0(toupper(substr(p[2], 1, 1)), tolower(substr(p[2], 2, 3)))
      paste(p[1], mon, p[3])
    }, character(1))
    res[idx] <- as.Date(norm, format = "%d %b %Y")
  }
  idx <- !is_missing & is.na(res) & grepl("^[A-Za-z]{3} +\\d{1,2}, +\\d{4}$", s)
  if (any(idx, na.rm = TRUE)) {
    tmp <- trimws(gsub(",", "", s[idx]))
    norm <- vapply(strsplit(tmp, " +"), function(p) {
      if (length(p) != 3) return(paste(p, collapse = " "))
      mon <- paste0(toupper(substr(p[1], 1, 1)), tolower(substr(p[1], 2, 3)))
      paste(mon, p[2], p[3])
    }, character(1))
    res[idx] <- as.Date(norm, format = "%b %d %Y")
  }
  idx <- !is_missing & is.na(res) & grepl("^\\d{8}$", s)
  if (any(idx, na.rm = TRUE)) res[idx] <- as.Date(s[idx], format = "%Y%m%d")
  idx <- !is_missing & is.na(res) & grepl("^\\d{1,5}$", s)
  if (any(idx, na.rm = TRUE)) {
    num <- suppressWarnings(as.numeric(s[idx]))
    ok <- !is.na(num) & num >= -20000 & num <= 60000
    tmp_res <- rep(as.Date(NA), sum(idx))
    tmp_res[ok] <- as.Date(num[ok], origin = "1960-01-01")
    res[idx] <- tmp_res
  }
  res
}

# BRTH_DT: SAS ANYDTDTE with DMY order -> ONE strict format %d/%m/%Y
parse_brth_dmy <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(as.Date(v))
  if (is.numeric(v) || is.integer(v)) {
    return(as.Date(v, origin = "1960-01-01"))
  }
  if (is.logical(v)) {
    return(as.Date(rep(NA_real_, length(v)), origin = "1970-01-01"))
  }
  s <- trimws(as.character(v))
  s[is.na(s)] <- NA_character_
  s[s == ""] <- NA_character_
  # normalise dash/dot separators to slash so ONE format handles them
  s_norm <- gsub("[-.]", "/", s)
  as.Date(s_norm, format = "%d/%m/%Y")
}

n <- nrow(raw_dm)

STUDYID <- rep("CDISC-01", n)
STUDYID <- substr(STUDYID, 1, 8)
DOMAIN <- rep("DM", n)
DOMAIN <- substr(DOMAIN, 1, 2)

subj_raw <- raw_dm$SUBJ_ID
SUBJID <- trimws(as.character(subj_raw))
SUBJID[is.na(subj_raw)] <- NA_character_
SUBJID[SUBJID == ""] <- NA_character_
SUBJID <- substr(SUBJID, 1, 20)

USUBJID <- ifelse(is.na(SUBJID), STUDYID, paste0(STUDYID, "-", SUBJID))
USUBJID <- substr(USUBJID, 1, 40)

site_raw <- raw_dm$SITE
SITEID <- trimws(as.character(site_raw))
SITEID[is.na(site_raw)] <- NA_character_
SITEID[SITEID == ""] <- NA_character_
SITEID[SITEID == "NA"] <- NA_character_
SITEID <- substr(SITEID, 1, 10)

g_raw <- as.character(raw_dm$GENDER)
g_trim <- trimws(g_raw)
first_char <- substr(toupper(g_trim), 1, 1)
SEX <- ifelse(!is.na(first_char) & first_char == "M", "M",
       ifelse(!is.na(first_char) & first_char == "F", "F", "U"))
SEX <- substr(SEX, 1, 1)

rt_raw <- toupper(trimws(as.character(raw_dm$RACE_TXT)))
RACE <- ifelse(!is.na(rt_raw) & rt_raw == "WHITE", "WHITE",
        ifelse(!is.na(rt_raw) & rt_raw == "BLACK", "BLACK OR AFRICAN AMERICAN",
        ifelse(!is.na(rt_raw) & rt_raw == "ASIAN", "ASIAN",
        ifelse(!is.na(rt_raw) & rt_raw == "OTHER", "OTHER", "UNKNOWN"))))
RACE <- substr(RACE, 1, 40)

ra_raw <- trimws(as.character(raw_dm$RANDOMIZED_ARM))
ARM <- ifelse(!is.na(ra_raw) & ra_raw == "Not Randomized", "SCREEN FAILURE",
       ifelse(!is.na(ra_raw) & ra_raw == "Placebo", "Placebo",
       ifelse(!is.na(ra_raw) & ra_raw == "Active Drug 50mg", "Active Drug 50mg", NA_character_)))
ARM <- substr(ARM, 1, 20)
ARMCD <- ifelse(!is.na(ra_raw) & ra_raw == "Not Randomized", "SCRNFAIL",
         ifelse(!is.na(ra_raw) & ra_raw == "Placebo", "PBO",
         ifelse(!is.na(ra_raw) & ra_raw == "Active Drug 50mg", "ACT50", NA_character_)))
ARMCD <- substr(ARMCD, 1, 8)

brth_date <- parse_brth_dmy(raw_dm$BRTH_DT)
rfic_date <- parse_anydtdte_to_date(raw_dm$ICF_DAT)
rfend_date <- parse_anydtdte_to_date(raw_dm$STUDY_END_DAT)

BRTHDTC <- format(brth_date, "%Y-%m-%d")
RFICDTC <- format(rfic_date, "%Y-%m-%d")
RFSTDTC <- RFICDTC
RFENDTC <- format(rfend_date, "%Y-%m-%d")
RFPENDTC <- RFENDTC

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

stopifnot(all(c("STUDYID","DOMAIN","USUBJID","SUBJID","SITEID","SEX","RACE","BRTHDTC","RFSTDTC","RFENDTC","RFPENDTC","RFICDTC","ARM","ARMCD") %in% names(dm)))

write.csv(dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")

nprint <- min(10, nrow(dm))
audit_vars <- c("STUDYID","USUBJID","SITEID","SEX","RACE","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFPENDTC","ARM","ARMCD")
stopifnot(all(audit_vars %in% names(dm)))
print_df <- dm[seq_len(nprint), audit_vars, drop = FALSE]
print_df <- cbind(Obs = seq_len(nprint), print_df)

write.csv(print_df, file.path(out, "P011.csv"), row.names = FALSE, na = "")

fmt_cell <- function(x) ifelse(is.na(x) | x == "", "", as.character(x))
header <- c("Obs", audit_vars)
all_mat <- rbind(header, as.matrix(data.frame(lapply(print_df, fmt_cell), stringsAsFactors = FALSE, check.names = FALSE)))
widths <- apply(nchar(all_mat), 2, function(c) max(c, na.rm = TRUE))
widths <- pmax(widths, nchar(header))
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_left_obs <- function(x, w) sprintf(paste0("%", w, "s"), x)
lines <- c()
lines <- c(lines, "DM Domain Audit (First 10 Records)", "")
hline_parts <- c(pad_left_obs("Obs", widths[1]))
for (j in 2:length(header)) hline_parts <- c(hline_parts, pad_right(header[j], widths[j]))
lines <- c(lines, paste(trimws(hline_parts, which = "right"), collapse = "  "))
lines <- c(lines, paste(vapply(widths, function(w) paste(rep("-", w), collapse = ""), character(1)), collapse = "  "))
for (i in seq_len(nprint)) {
  row_vals <- fmt_cell(as.character(unlist(print_df[i, , drop = FALSE])))
  parts <- c(pad_left_obs(row_vals[1], widths[1]))
  for (j in 2:length(row_vals)) parts <- c(parts, pad_right(row_vals[j], widths[j]))
  lines <- c(lines, paste(parts, collapse = "  "))
}
lines <- c(lines, "", paste0("N = ", nprint, " (first 10 records of ", nrow(dm), " total)"))

writeLines(lines, file.path(out, "P011.txt"))
writeLines(lines, file.path(out, "P011.lst"))

cat("DM rows:", nrow(dm), " cols:", ncol(dm), "\n")
cat("Audit printed:", nprint, "rows\n")
