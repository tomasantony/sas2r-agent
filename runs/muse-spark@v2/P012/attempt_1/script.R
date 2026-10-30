out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  sign_x <- sign(x)
  z <- abs(x) * mult + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  sign_x * z / mult
}

cat("P012: start\n")

# ---- locate raw_demog.csv ----
raw_path_fixed <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"
candidates <- c(
  raw_path_fixed,
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv",
  file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_demog.csv"),
  file.path(Sys.getenv("PROJECT_PATH"), "data/derived/c2c/data/raw/raw_demog.csv"),
  file.path(getwd(), "raw_demog.csv")
)
raw_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  found <- list.files(getwd(), pattern = "^raw_demog\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(found) > 0) raw_path <- found[1]
}
if (is.null(raw_path)) {
  # last resort: search D:/ root if visible
  if (dir.exists("D:/meta_hackathon/sas2r-agent")) {
    found2 <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^raw_demog\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(found2) > 0) raw_path <- found2[1]
  }
}
if (is.null(raw_path)) stop("raw_demog.csv not found")
cat("Using raw file:", raw_path, "\n")

suppressMessages(library(readr))
suppressMessages(library(dplyr))

# PROC IMPORT dbms=csv getnames=yes ; validvarname=upcase -> uppercase names
# Read everything as character to mimic SAS vtype check (C vs N)
raw0 <- suppressMessages(readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()), na = c(""), trim_ws = FALSE, progress = FALSE))
names(raw0) <- toupper(names(raw0))
# strip BOM if present on first name
names(raw0) <- sub("^\ufeff", "", names(raw0))

cat("Columns:", paste(names(raw0), collapse = ","), "\n")

# Resolve expected columns (case-insensitive already upper)
# SAS uses SUBJ_ID, DISPOSITION, DISPOSITION_REASON, STUDY_END_DAT
stopifnot("SUBJ_ID" %in% names(raw0) || "SUBJID" %in% names(raw0))
stopifnot("DISPOSITION" %in% names(raw0))
stopifnot("DISPOSITION_REASON" %in% names(raw0) || "DISP_REASON" %in% names(raw0))
stopifnot("STUDY_END_DAT" %in% names(raw0) || "STUDY_END_DT" %in% names(raw0))

# map alternates to canonical
if (!"SUBJ_ID" %in% names(raw0) && "SUBJID" %in% names(raw0)) raw0$SUBJ_ID <- raw0$SUBJID
if (!"DISPOSITION_REASON" %in% names(raw0) && "DISP_REASON" %in% names(raw0)) raw0$DISPOSITION_REASON <- raw0$DISP_REASON
if (!"STUDY_END_DAT" %in% names(raw0) && "STUDY_END_DT" %in% names(raw0)) raw0$STUDY_END_DAT <- raw0$STUDY_END_DT

n <- nrow(raw0)
STUDYID_v <- rep("CDISC-01", n)
DOMAIN_v <- rep("DS", n)

subj_raw <- as.character(raw0$SUBJ_ID)
# SAS strip + catx("-", STUDYID, SUBJ_ID) : catx ignores missing
subj_stripped <- trimws(subj_raw)
subj_stripped[is.na(subj_stripped)] <- ""
# catx behaviour
USUBJID_v <- ifelse(subj_stripped == "", "CDISC-01", paste0("CDISC-01-", subj_stripped))
USUBJID_v <- substr(USUBJID_v, 1, 40)

DSSEQ_v <- rep(1, n)

DSCAT_v <- rep("DISPOSITION EVENT", n)
DSSCAT_v <- rep("STUDY PARTICIPATION", n)

dr_raw <- as.character(raw0$DISPOSITION_REASON)
dr_stripped <- trimws(dr_raw)
dr_is_miss <- is.na(dr_raw) | is.na(dr_stripped) | dr_stripped == ""
DSTERM_v <- toupper(dr_stripped)
DSTERM_v[dr_is_miss] <- NA_character_
DSTERM_v <- substr(DSTERM_v, 1, 80)
DSDECOD_v <- DSTERM_v

disp_raw <- as.character(raw0$DISPOSITION)
disp_up <- toupper(trimws(disp_raw))
disp_up[is.na(disp_up)] <- ""
EPOCH_v <- ifelse(disp_up == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")
EPOCH_v <- substr(EPOCH_v, 1, 20)

# ---- DSSTDTC : SAS anydtdte. + put is8601da. ----
# SAS: if vtype(STUDY_END_DAT)='C' then _dsdt=input(strip(STUDY_END_DAT),anydtdte.); else _dsdt=STUDY_END_DAT
# We read as character, so character branch. Detect ONE strict format from data (MDY default like SAS DATESTYLE=MDY).
sed_raw <- as.character(raw0$STUDY_END_DAT)
sed_strip <- trimws(sed_raw)
sed_strip[sed_strip == ""] <- NA_character_

# choose single explicit format based on first non-missing value
nonmiss <- sed_strip[!is.na(sed_strip) & sed_strip != ""]
fmt <- "%m/%d/%Y"
if (length(nonmiss) > 0) {
  first <- nonmiss[1]
  first_t <- sub("T.*$", "", first)
  first_t <- trimws(first_t)
  # remove time after space if it looks like datetime with space + colon
  if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} +[0-9]", first_t)) {
    first_t <- trimws(strsplit(first_t, " +")[[1]][1])
  }
  if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", first_t)) {
    fmt <- "%Y-%m-%d"
  } else if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", first_t)) {
    fmt <- "%m/%d/%Y"
  } else if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", first_t)) {
    fmt <- "%m-%d-%Y"
  } else if (grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", first_t)) {
    fmt <- "%Y/%m/%d"
  } else if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", first_t)) {
    fmt <- "%d-%b-%Y"
  } else if (grepl("^[0-9]{1,2}[A-Za-z]{3}[0-9]{2,4}$", first_t)) {
    fmt <- "%d%b%Y"
  } else if (grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$", first_t)) {
    fmt <- "%d.%m.%Y"
  } else {
    fmt <- "%m/%d/%Y"
  }
}
cat("Chosen date format:", fmt, "\n")

parse_one <- function(x, fmt) {
  if (is.na(x) || x == "") return(as.Date(NA))
  s <- trimws(x)
  # pure SAS numeric date (days since 1960-01-01) e.g. 13100
  if (grepl("^[0-9]{1,5}$", s)) {
    v <- suppressWarnings(as.numeric(s))
    # SAS date range roughly 0-30000; treat as SAS date
    if (!is.na(v) && v >= 0 && v <= 40000) {
      return(as.Date(v, origin = "1960-01-01"))
    }
  }
  # isolate date part before T (ISO datetime)
  s2 <- sub("T.*$", "", s)
  s2 <- trimws(s2)
  # if datetime with space + time, take first token when fmt is date-only and remainder looks like time
  if (grepl(" ", s2) && grepl(":", s2)) {
    tok <- trimws(strsplit(s2, " +")[[1]][1])
    # only strip if token matches date pattern length
    if (nchar(tok) >= 8) s2 <- tok
  }
  s2 <- trimws(s2)
  # strict single-format parse
  d <- suppressWarnings(as.Date(s2, format = fmt))
  # handle month abbreviation case-insensitivity for %b
  if (is.na(d) && grepl("%b", fmt, fixed = TRUE)) {
    d <- suppressWarnings(as.Date(toupper(s2), format = gsub("%b", "%b", fmt, fixed = TRUE)))
    if (is.na(d)) {
      # try title case
      s3 <- paste0(toupper(substr(s2, 1, 1)), tolower(substr(s2, 2, nchar(s2))))
      # need manual: use same fmt but with converted string via as.Date on upper? R %b is locale-dependent; try both
      d <- suppressWarnings(as.Date(s3, format = fmt))
    }
  }
  d
}
vec_parse <- function(v, fmt) {
  res <- rep(as.Date(NA), length(v))
  for (i in seq_along(v)) {
    xi <- v[i]
    if (is.na(xi) || trimws(xi) == "") { res[i] <- as.Date(NA) } else { res[i] <- parse_one(xi, fmt) }
  }
  res
}
# SAS anydtdte is MDY by default; our fmt already chosen; apply ONE format to whole column
_dsdt <- vec_parse(sed_strip, fmt)
DSSTDTC_v <- format(_dsdt, "%Y-%m-%d")
DSSTDTC_v[is.na(_dsdt)] <- NA_character_
DSSTDTC_v <- substr(DSSTDTC_v, 1, 10)

# LENGTH enforcement (truncate; SAS would pad but CSV trims)
STUDYID_v <- substr(STUDYID_v, 1, 8)
DOMAIN_v <- substr(DOMAIN_v, 1, 2)
DSCAT_v <- substr(DSCAT_v, 1, 40)
DSSCAT_v <- substr(DSSCAT_v, 1, 40)

ds <- data.frame(
  STUDYID = STUDYID_v,
  DOMAIN = DOMAIN_v,
  USUBJID = USUBJID_v,
  DSSEQ = DSSEQ_v,
  DSCAT = DSCAT_v,
  DSSCAT = DSSCAT_v,
  DSTERM = DSTERM_v,
  DSDECOD = DSDECOD_v,
  EPOCH = EPOCH_v,
  DSSTDTC = DSSTDTC_v,
  stringsAsFactors = FALSE
)

# PROC SORT by USUBJID DSSEQ (duplicates remain); SAS missing first, uppercase before lowercase
# Use radix byte order to approximate SAS binary collation
ord <- order(ds$USUBJID, ds$DSSEQ, na.last = FALSE, method = "radix")
ds <- ds[ord, , drop = FALSE]
rownames(ds) <- NULL

stopifnot(all(c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSCAT","DSSCAT","DSTERM","DSDECOD","EPOCH","DSSTDTC") %in% names(ds)))

# ---- write dataset ds.csv ----
write.csv(ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT obs=12 title "DS Domain Audit" ----
print_n <- min(12, nrow(ds))
pt <- ds[seq_len(print_n), , drop = FALSE]
pt_obs <- cbind(Obs = seq_len(print_n), pt)

# text rendering (P012.txt + P012.lst) with verbatim title
hdr <- c("Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSCAT","DSSCAT","DSTERM","DSDECOD","EPOCH","DSSTDTC")
lines <- c("DS Domain Audit", "")
lines <- c(lines, paste(hdr, collapse = " | "))
for (i in seq_len(print_n)) {
  vals <- sapply(names(pt_obs), function(cc) {
    v <- pt_obs[i, cc]
    if (is.na(v)) v <- ""
    as.character(v)
  })
  lines <- c(lines, paste(vals, collapse = " | "))
}
writeLines(lines, file.path(out, "P012.txt"))
writeLines(lines, file.path(out, "P012.lst"))
# CSV of table body
write.csv(pt_obs, file.path(out, "P012.csv"), row.names = FALSE, na = "")

cat("P012: done, n=", nrow(ds), " printed=", print_n, "\n")
