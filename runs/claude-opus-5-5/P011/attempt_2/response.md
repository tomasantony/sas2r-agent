```r
# P011 - SDTM DM mapping (translation of P011_sdtm_dm.sas)
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40

# ---------------------------------------------------------------------------
# 2. IMPORT RAW EDC DATA (PROC IMPORT emulation: read as text, upcase names)
# ---------------------------------------------------------------------------
raw <- readr::read_csv(file.path(project_path, "data/raw/raw_demog.csv"),
                       col_types = readr::cols(.default = readr::col_character()),
                       na = c("", "NA"), trim_ws = TRUE, progress = FALSE)
raw <- as.data.frame(raw, stringsAsFactors = FALSE)
names(raw) <- toupper(names(raw))
for (v in names(raw)) {
  x <- trimws(raw[[v]])
  x[!is.na(x) & x == ""] <- NA
  raw[[v]] <- x
}
cat("Read raw_demog.csv:", nrow(raw), "rows\n")

best_str <- function(x) {
  r <- formatC(x, digits = 12, format = "g", flag = "")
  r <- trimws(r)
  r[is.na(x)] <- NA
  r
}
is_numeric_col <- function(x) {
  nn <- x[!is.na(x)]
  length(nn) > 0 && all(grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", nn))
}
as_sas_char <- function(x) {
  if (is_numeric_col(x)) best_str(as.numeric(x)) else x
}

# Date parsing; PROC IMPORT detects a column-level date style (DMY if any
# first component > 12, otherwise MDY)
mon_map <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6, JUL = 7,
             AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)
fix_year <- function(y) {
  y <- as.integer(y)
  ifelse(y < 100, ifelse(y >= 26, 1900 + y, 2000 + y), y)
}
mk_date <- function(y, m, d) {
  y <- fix_year(y); m <- as.integer(m); d <- as.integer(d)
  if (is.na(y) || is.na(m) || is.na(d)) return(as.Date(NA))
  s <- sprintf("%04d-%02d-%02d", y, m, d)
  r <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (is.na(r) || format(r, "%Y-%m-%d") != s) return(as.Date(NA))
  r
}
num_re <- "^(\\d{1,2})[-/.](\\d{1,2})[-/.](\\d{2,4})([T ].*)?$"
parse_one <- function(s, dmy = FALSE) {
  if (is.na(s)) return(as.Date(NA))
  s <- trimws(s)
  if (s == "") return(as.Date(NA))
  m <- regmatches(s, regexec("^(\\d{4})[-/.](\\d{1,2})[-/.](\\d{1,2})([T ].*)?$", s))[[1]]
  if (length(m)) return(mk_date(m[2], m[3], m[4]))
  m <- regmatches(s, regexec(num_re, s))[[1]]
  if (length(m)) {
    a <- as.integer(m[2]); b <- as.integer(m[3])
    if (dmy) {
      if (b > 12 && a <= 12) return(mk_date(m[4], a, b))
      return(mk_date(m[4], b, a))
    }
    if (a > 12 && b <= 12) return(mk_date(m[4], b, a))
    return(mk_date(m[4], a, b))
  }
  m <- regmatches(s, regexec("^(\\d{4})(\\d{2})(\\d{2})$", s))[[1]]
  if (length(m)) return(mk_date(m[2], m[3], m[4]))
  m <- regmatches(s, regexec("^(\\d{1,2})[- /]?([A-Za-z]{3,9})[- /,]?(\\d{2,4})(:.*)?$", s))[[1]]
  if (length(m)) {
    mm <- mon_map[toupper(substr(m[3], 1, 3))]
    if (!is.na(mm)) return(mk_date(m[4], mm, m[2]))
  }
  m <- regmatches(s, regexec("^([A-Za-z]{3,9})\\.?[ -](\\d{1,2}),?[ -](\\d{2,4})$", s))[[1]]
  if (length(m)) {
    mm <- mon_map[toupper(substr(m[2], 1, 3))]
    if (!is.na(mm)) return(mk_date(m[4], mm, m[3]))
  }
  as.Date(NA)
}
detect_dmy <- function(x) {
  xx <- trimws(x[!is.na(x)])
  mm <- regmatches(xx, regexec(num_re, xx))
  mm <- mm[lengths(mm) > 0]
  if (!length(mm)) return(FALSE)
  a <- sapply(mm, function(z) as.integer(z[2]))
  b <- sapply(mm, function(z) as.integer(z[3]))
  any(a > 12) && !any(b > 12)
}
anydtdte <- function(x) {
  dmy <- detect_dmy(x)
  as.Date(vapply(x, function(s) as.numeric(parse_one(s, dmy)), numeric(1)),
          origin = "1970-01-01")
}
is8601 <- function(d) ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))

getcol <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA_character_, nrow(raw))

# ---------------------------------------------------------------------------
# 3. TRANSFORMATION AND SDTM MAPPING
# ---------------------------------------------------------------------------
n <- nrow(raw)
STUDYID <- rep("CDISC-01", n)
DOMAIN  <- rep("DM", n)
SUBJID  <- substr(as_sas_char(getcol("SUBJ_ID")), 1, 20)
USUBJID <- ifelse(is.na(SUBJID) | SUBJID == "", STUDYID, paste(STUDYID, SUBJID, sep = "-"))
USUBJID <- substr(USUBJID, 1, usubjid_length)
SITEID  <- substr(trimws(as_sas_char(getcol("SITE"))), 1, 10)

g1 <- toupper(substr(ifelse(is.na(getcol("GENDER")), "", getcol("GENDER")), 1, 1))
SEX <- ifelse(g1 == "M", "M", ifelse(g1 == "F", "F", "U"))

rt <- toupper(trimws(ifelse(is.na(getcol("RACE_TXT")), "", getcol("RACE_TXT"))))
RACE <- dplyr::case_when(
  rt == "WHITE" ~ "WHITE",
  rt == "BLACK" ~ "BLACK OR AFRICAN AMERICAN",
  rt == "ASIAN" ~ "ASIAN",
  rt == "OTHER" ~ "OTHER",
  TRUE ~ "UNKNOWN")

ra <- trimws(ifelse(is.na(getcol("RANDOMIZED_ARM")), "", getcol("RANDOMIZED_ARM")))
ARM <- dplyr::case_when(
  ra == "Not Randomized" ~ "SCREEN FAILURE",
  ra == "Placebo" ~ "Placebo",
  ra == "Active Drug 50mg" ~ "Active Drug 50mg",
  TRUE ~ NA_character_)
ARMCD <- dplyr::case_when(
  ra == "Not Randomized" ~ "SCRNFAIL",
  ra == "Placebo" ~ "PBO",
  ra == "Active Drug 50mg" ~ "ACT50",
  TRUE ~ NA_character_)

brth <- anydtdte(getcol("BRTH_DT"))
rfic <- anydtdte(getcol("ICF_DAT"))
rfend <- anydtdte(getcol("STUDY_END_DAT"))

BRTHDTC <- is8601(brth)

RFICDTC <- RFSTDTC <- RFENDTC <- RFPENDTC <- rep(NA_character_, n)
cur_ic <- NA_character_; cur_end <- NA_character_; cur_pend <- NA_character_
for (i in seq_len(n)) {
  if (!is.na(rfic[i])) cur_ic <- format(rfic[i], "%Y-%m-%d")
  RFICDTC[i] <- cur_ic
  RFSTDTC[i] <- cur_ic
  if (!is.na(rfend[i])) {
    cur_end <- format(rfend[i], "%Y-%m-%d")
    cur_pend <- cur_end
  }
  RFENDTC[i] <- cur_end
  RFPENDTC[i] <- cur_pend
}

dm <- data.frame(STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, SEX, RACE, ARM, ARMCD,
                 RFSTDTC, RFENDTC, RFPENDTC, RFICDTC, BRTHDTC,
                 stringsAsFactors = FALSE)

write.csv(dm, file.path(out, "dm.csv"), row.names = FALSE, na = "")
cat("Wrote dm.csv:", nrow(dm), "rows\n")

# ---------------------------------------------------------------------------
# 4. VISUAL AUDIT (PROC PRINT obs=10)
# ---------------------------------------------------------------------------
vars <- c("STUDYID", "USUBJID", "SITEID", "SEX", "RACE", "BRTHDTC", "RFICDTC",
          "RFSTDTC", "RFENDTC", "RFPENDTC", "ARM", "ARMCD")
lst <- head(dm, 10)[, vars, drop = FALSE]
lst <- cbind(Obs = seq_len(nrow(lst)), lst)
write.csv(lst, file.path(out, "P011.csv"), row.names = FALSE, na = "")

disp <- lst
for (v in names(disp)) {
  x <- as.character(disp[[v]])
  x[is.na(x)] <- ""
  disp[[v]] <- x
}
widths <- sapply(names(disp), function(v) max(nchar(v), nchar(disp[[v]]), 1))
fmt_row <- function(vals) {
  paste(mapply(function(val, w, nm) {
    if (nm == "Obs") formatC(val, width = w) else formatC(val, width = w, flag = "-")
  }, vals, widths, names(disp)), collapse = "  ")
}
lines <- c("DM Domain Audit (First 10 Records)", "",
           fmt_row(names(disp)), "")
for (i in seq_len(nrow(disp))) lines <- c(lines, fmt_row(unlist(disp[i, ])))
writeLines(lines, file.path(out, "P011.txt"))
cat("Wrote P011.txt / P011.csv\n")
```