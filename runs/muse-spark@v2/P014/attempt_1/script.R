out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv"

# SAS round half away from zero (helper, SAS uses ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# ---- read raw exposure (mimic PROC IMPORT DBMS=CSV GETNAMES=YES) ----
# base read.csv guesses numeric vs character like PROC IMPORT, never auto-dates
raw_ex <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE,
                   na.strings = c("", "NA"), strip.white = TRUE, encoding = "unknown")
# SAS VALIDVARNAME=UPCASE -> names uppercase
names(raw_ex) <- toupper(trimws(names(raw_ex)))

# locate columns case-insensitively (already upper)
getcol <- function(nm) {
  idx <- which(names(raw_ex) == toupper(nm))
  if (length(idx) == 0) return(rep(NA, nrow(raw_ex)))
  raw_ex[[idx[1]]]
}

PATIENT_v   <- getcol("PATIENT")
TREATMENT_v <- getcol("TREATMENT")
DOSE_v      <- getcol("DOSE")
UNIT_v      <- getcol("UNIT")
START_DATE_v <- getcol("START_DATE")
END_DATE_v   <- getcol("END_DATE")
stopifnot(!is.null(PATIENT_v), !is.null(TREATMENT_v))

n <- nrow(raw_ex)

# ---- SAS ANYDTDTE with DATESTYLE=MDY (single fixed order MDY) ----
# Parses ISO YYYY-MM-DD, MM/DD/YYYY (MDY), DDMMMYYYY, YYYYMMDD, year-only
sas_yearcutoff <- 1926L
expand_2digit_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(is.na(yy), NA_integer_,
    ifelse(yy >= 26 & yy <= 99, 1900L + yy,
      ifelse(yy >= 0 & yy <= 25, 2000L + yy, yy)))
}
month_from_token <- function(tok) {
  t <- toupper(trimws(tok))
  m3 <- substr(t, 1, 3)
  map <- c(JAN=1L,FEB=2L,MAR=3L,APR=4L,MAY=5L,JUN=6L,
           JUL=7L,AUG=8L,SEP=9L,OCT=10L,NOV=11L,DEC=12L)
  if (m3 %in% names(map)) return(unname(map[m3]))
  # full names
  full <- c(JANUARY=1L,FEBRUARY=2L,MARCH=3L,APRIL=4L,MAY=5L,JUNE=6L,
            JULY=7L,AUGUST=8L,SEPTEMBER=9L,OCTOBER=10L,NOVEMBER=11L,DECEMBER=12L)
  if (t %in% names(full)) return(unname(full[t]))
  suppressWarnings(as.integer(t))
}
valid_ymd <- function(y, m, d) {
  if (is.na(y) || is.na(m) || is.na(d)) return(NA_character_)
  if (m < 1 || m > 12 || d < 1 || d > 31) return(NA_character_)
  dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d), format = "%Y-%m-%d"))
  if (is.na(dt)) return(NA_character_)
  format(dt, "%Y-%m-%d")
}
parse_one_anydtdte_mdy <- function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "" || s == ".") return(NA_character_)
  # datetime with T -> take date part if ISO datetime
  # ISO YYYY-MM-DD[ T ...] with separators - / .
  m <- regmatches(s, regexec("^(\\d{4})[\\-/\\.](\\d{1,2})[\\-/\\.](\\d{1,2})(?:[T ].*)?$", s))[[1]]
  if (length(m) == 4 && m[1] != "") {
    y <- suppressWarnings(as.integer(m[2])); mo <- suppressWarnings(as.integer(m[3])); d <- suppressWarnings(as.integer(m[4]))
    v <- valid_ymd(y, mo, d)
    if (!is.na(v)) return(v)
  }
  # ISO without separators already? YYYY-MM-DD handled; try YYYYMMDD (8 digits)
  m2 <- regmatches(s, regexec("^(\\d{4})(\\d{2})(\\d{2})$", s))[[1]]
  if (length(m2) == 4 && m2[1] != "" && grepl("^[0-9]{8}$", s)) {
    y <- suppressWarnings(as.integer(m2[2])); mo <- suppressWarnings(as.integer(m2[3])); d <- suppressWarnings(as.integer(m2[4]))
    v <- valid_ymd(y, mo, d)
    if (!is.na(v)) return(v)
  }
  # MDY numeric with / - . or space or comma: M/D/Y (1-2/1-2/2or4 digits)
  m3 <- regmatches(s, regexec("^(\\d{1,2})[\\-/\\.\\s]+(\\d{1,2})[\\-/\\.\\s,]+(\\d{2,4})$", s))[[1]]
  if (length(m3) == 4 && m3[1] != "") {
    mo <- suppressWarnings(as.integer(m3[2])); d <- suppressWarnings(as.integer(m3[3])); yraw <- trimws(m3[4])
    y <- suppressWarnings(as.integer(yraw))
    if (nchar(yraw) == 2) y <- expand_2digit_year(y)
    v <- valid_ymd(y, mo, d)
    if (!is.na(v)) return(v)
    else return(NA_character_)
  }
  # DDMMMYYYY variants: 01JAN2020, 01-JAN-2020, 01 JAN 2020, 01/Jan/2020, 1 January 2020
  m4 <- regmatches(s, regexec("^(\\d{1,2})[\\s\\-/\\.]*([A-Za-z]{3,9})[\\s\\-/\\.,]*?(\\d{2,4})$", s))[[1]]
  if (length(m4) == 4 && m4[1] != "") {
    d <- suppressWarnings(as.integer(m4[2])); mo <- month_from_token(m4[3]); yraw <- trimws(m4[4])
    y <- suppressWarnings(as.integer(yraw))
    if (nchar(yraw) == 2) y <- expand_2digit_year(y)
    v <- valid_ymd(y, mo, d)
    if (!is.na(v)) return(v)
    else return(NA_character_)
  }
  # MMM DD YYYY : JAN 01 2020, JAN-01-2020
  m5 <- regmatches(s, regexec("^([A-Za-z]{3,9})[\\s\\-/\\.]+(\\d{1,2})[\\s\\-/\\.,]+(\\d{2,4})$", s))[[1]]
  if (length(m5) == 4 && m5[1] != "") {
    mo <- month_from_token(m5[2]); d <- suppressWarnings(as.integer(m5[3])); yraw <- trimws(m5[4])
    y <- suppressWarnings(as.integer(yraw))
    if (nchar(yraw) == 2) y <- expand_2digit_year(y)
    v <- valid_ymd(y, mo, d)
    if (!is.na(v)) return(v)
    else return(NA_character_)
  }
  # Year only YYYY
  if (grepl("^\\d{4}$", s)) {
    y <- suppressWarnings(as.integer(s))
    if (!is.na(y) && y >= 1582 && y <= 9999) return(sprintf("%04d-01-01", y))
    else return(NA_character_)
  }
  # YYYY-MM (month precision) -> first day (SAS would give month; map to first day for EXSTDTC)
  m6 <- regmatches(s, regexec("^(\\d{4})[\\-/](\\d{1,2})$", s))[[1]]
  if (length(m6) == 3 && m6[1] != "") {
    y <- suppressWarnings(as.integer(m6[2])); mo <- suppressWarnings(as.integer(m6[3]))
    v <- valid_ymd(y, mo, 1L)
    if (!is.na(v)) return(v)
  }
  return(NA_character_)
}
sas_input_anydtdte_vec <- function(vec) {
  # vec is character/factor/logical vector (vtype C branch)
  sapply(vec, parse_one_anydtdte_mdy, USE.NAMES = FALSE)
}
sas_num_to_iso <- function(numvec) {
  # numeric SAS date (days since 1960-01-01) -> ISO, NA stays NA
  sapply(numvec, function(v) {
    if (is.na(v)) return(NA_character_)
    vnum <- suppressWarnings(as.numeric(v))
    if (is.na(vnum)) return(NA_character_)
    dt <- as.Date(vnum, origin = "1960-01-01")
    if (is.na(dt)) return(NA_character_)
    format(dt, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}
sas_vec_to_iso <- function(vec) {
  # replicate: if vtype(START_DATE)='C' then input(anydtdte) else numeric SAS date
  if (inherits(vec, "Date")) {
    iso <- format(vec, "%Y-%m-%d"); iso[is.na(vec)] <- NA_character_; return(iso)
  }
  if (inherits(vec, "POSIXt") || inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")) {
    d <- as.Date(vec); iso <- format(d, "%Y-%m-%d"); iso[is.na(d)] <- NA_character_; return(iso)
  }
  if (is.numeric(vec)) {
    return(sas_num_to_iso(vec))
  }
  # character / factor / logical / integer-as-character already handled; integer type is numeric in R
  if (is.integer(vec)) return(sas_num_to_iso(vec))
  return(sas_input_anydtdte_vec(as.character(vec)))
}

# ---- CORE TRANSFORMATION (work.ex_mapped) ----
sas_strip <- function(x) {
  sapply(as.character(x), function(v) {
    if (is.na(v)) return("")
    trimws(v)
  }, USE.NAMES = FALSE)
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  nmax <- max(sapply(args, length))
  out <- character(nmax)
  for (i in seq_len(nmax)) {
    parts <- sapply(args, function(a) {
      v <- if (length(a) == 1) a[1] else a[i]
      if (is.na(v)) return(NA_character_)
      t <- trimws(as.character(v))
      if (t == "" || t == ".") return(NA_character_)
      t
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) out[i] <- ""
    else out[i] <- paste(parts, collapse = delim)
  }
  out
}
substr_sas <- function(x, len) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    v <- as.character(v)
    if (v == "") return("")
    substr(v, 1, len)
  }, USE.NAMES = FALSE)
}

STUDYID_v <- rep("CDISC-01", n)
DOMAIN_v  <- rep("EX", n)
USUBJID_v <- sas_catx("-", "CDISC-01", as.character(PATIENT_v))
USUBJID_v <- substr_sas(USUBJID_v, 40)
STUDYID_v <- substr_sas(STUDYID_v, 8)
DOMAIN_v  <- substr_sas(DOMAIN_v, 2)

EXTRT_v  <- substr_sas(sas_strip(as.character(TREATMENT_v)), 50)
EXTRT_v[is.na(TREATMENT_v)] <- ""
# EXDOSE numeric
EXDOSE_v <- suppressWarnings(as.numeric(DOSE_v))
EXDOSU_v <- substr_sas(sas_strip(as.character(UNIT_v)), 20)
EXDOSU_v[is.na(UNIT_v)] <- ""

EXSTDTC_iso <- sas_vec_to_iso(START_DATE_v)
EXENDTC_iso <- sas_vec_to_iso(END_DATE_v)
# SAS: if missing then EXSTDTC stays missing (""), EXENDTC = ""
EXSTDTC_v <- ifelse(is.na(EXSTDTC_iso), "", EXSTDTC_iso)
EXENDTC_v <- ifelse(is.na(EXENDTC_iso), "", EXENDTC_iso)
EXSTDTC_v <- substr_sas(EXSTDTC_v, 10)
EXENDTC_v <- substr_sas(EXENDTC_v, 10)
# empty stays ""
EXSTDTC_v[EXSTDTC_v == ""] <- ""
EXENDTC_v[EXENDTC_v == ""] <- ""

ex_mapped <- data.frame(
  STUDYID = STUDYID_v, DOMAIN = DOMAIN_v, USUBJID = USUBJID_v,
  EXTRT = EXTRT_v, EXDOSE = EXDOSE_v, EXDOSU = EXDOSU_v,
  EXSTDTC = EXSTDTC_v, EXENDTC = EXENDTC_v,
  stringsAsFactors = FALSE
)

# ---- PROC SORT by USUBJID EXSTDTC (SAS: missing first, uppercase before lowercase) ----
# Use binary (C-locale) ordering to match SAS: order by raw bytes
rank_bin <- function(x) {
  # transform each string to sortable key using char codes; use xtfrm on factor with C locale via order of utf8 bytes
  # simplest: order via order() on charToRaw collapsed; use sapply to create keys and order
  # For ISO dates and USUBJID, plain order() suffices; enforce missing ("") first explicitly
  x
}
ord <- order(ex_mapped$USUBJID, ex_mapped$EXSTDTC, na.last = FALSE, method = "radix")
ex_mapped <- ex_mapped[ord, , drop = FALSE]

# ---- GENERATE EXSEQ (by USUBJID) ----
EXSEQ_v <- integer(nrow(ex_mapped))
if (nrow(ex_mapped) > 0) {
  cur <- ""; cnt <- 0L
  for (i in seq_len(nrow(ex_mapped))) {
    u <- ex_mapped$USUBJID[i]
    if (is.na(u)) u <- ""
    if (u != cur) { cur <- u; cnt <- 1L } else { cnt <- cnt + 1L }
    EXSEQ_v[i] <- cnt
  }
}
sdtm_ex <- data.frame(
  STUDYID = ex_mapped$STUDYID, DOMAIN = ex_mapped$DOMAIN, USUBJID = ex_mapped$USUBJID,
  EXSEQ = EXSEQ_v,
  EXTRT = ex_mapped$EXTRT, EXDOSE = ex_mapped$EXDOSE, EXDOSU = ex_mapped$EXDOSU,
  EXSTDTC = ex_mapped$EXSTDTC, EXENDTC = ex_mapped$EXENDTC,
  stringsAsFactors = FALSE
)

# ---- write dataset ex ----
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# ---- VISUAL AUDIT: PROC PRINT obs=10 vars USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC ----
audit_n <- min(10L, nrow(sdtm_ex))
audit_df <- sdtm_ex[seq_len(audit_n), c("USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC"), drop = FALSE]
audit_df$Obs <- seq_len(audit_n)
audit_df <- audit_df[, c("Obs","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC"), drop = FALSE]

# CSV of table body
write.csv(audit_df, file.path(out, "P014.csv"), row.names = FALSE, na = "")

# Text rendering like SAS PROC PRINT
title_txt <- "EX Domain Audit (Study Drug Exposure)"
# build fixed-width text
hdr <- sprintf("%-4s %-20s %-6s %-20s %-8s %-10s %-10s %-10s",
               "Obs", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
lines <- c(title_txt, "", hdr, sep)
for (i in seq_len(audit_n)) {
  r <- audit_df[i, ]
  dose_s <- if (is.na(r$EXDOSE)) "" else format(r$EXDOSE, scientific = FALSE, trim = TRUE)
  lines <- c(lines, sprintf("%-4d %-20s %-6d %-20s %-8s %-10s %-10s %-10s",
    r$Obs, r$USUBJID, r$EXSEQ, r$EXTRT, dose_s, r$EXDOSU, r$EXSTDTC, r$EXENDTC))
}
lines <- c(lines, "", sprintf("N = %d (first %d observations shown of %d)", audit_n, audit_n, nrow(sdtm_ex)))
writeLines(lines, file.path(out, "P014.txt"))
# also write .lst counterpart identical
writeLines(lines, file.path(out, "P014.lst"))
