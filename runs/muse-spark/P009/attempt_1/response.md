```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_ae.csv")

# fallback locations if needed
if (!file.exists(raw_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_ae.csv"
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv"
  if (file.exists(alt1)) raw_path <- alt1
  if (file.exists(alt2)) raw_path <- alt2
}

sas_trunc <- function(x, n) {
  # mimic SAS LENGTH truncation; x character vector, NA stays NA for now
  v <- as.character(x)
  isna <- is.na(v)
  v[!isna] <- substr(v[!isna], 1, n)
  v
}
sas_strip <- function(x) {
  v <- as.character(x)
  v[is.na(v)] <- ""
  trimws(v)
}
sas_upcase_strip <- function(x) {
  toupper(sas_strip(x))
}

# parse single character date with ANYDTDTE.-like behaviour -> SAS date number (days since 1960-01-01)
parse_one_anydtdte <- function(s) {
  if (length(s) == 0) return(NA_real_)
  if (is.na(s)) return(NA_real_)
  s <- trimws(as.character(s))
  if (is.na(s) || s == "") return(NA_real_)
  # try lubridate parse_date_time
  dt <- suppressWarnings(lubridate::parse_date_time(s,
    orders = c("ymd","Ymd","mdy","dmy","mdY","dmY","ymd HMS","mdy HMS","dmy HMS","Ymd HMS",
               "dby","dbY","bdy","bdY","dB Y","BdY","Yb d","bd y","db y",
               "mdy HM","dmy HM","ymd HM","ymdT HMS","YmdT HMS","ymd HM","mdyT HMS","dmyT HMS",
               "Ymd HM","Ymd HMS","ymd HMS"),
    quiet = TRUE, truncated = 0, exact = FALSE))
  if (length(dt) == 1 && !is.na(dt)) {
    d <- suppressWarnings(as.Date(dt, tz = "UTC"))
    if (!is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
  }
  fmts <- c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y","%m-%d-%Y","%d-%m-%Y",
            "%Y%m%d","%d%b%Y","%d-%b-%Y","%d %b %Y","%d %B %Y","%b %d, %Y","%B %d, %Y",
            "%Y.%m.%d","%m.%d.%Y","%d.%m.%Y","%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S",
            "%m/%d/%y","%d/%m/%y","%d%b%y","%d-%b-%y","%d %b %y","%b %d, %y",
            "%Y-%m-%dT%H:%M","%Y/%m/%d %H:%M:%S","%m/%d/%Y %H:%M:%S")
  d2 <- suppressWarnings(as.Date(s, tryFormats = fmts))
  if (length(d2) == 1 && !is.na(d2)) return(as.numeric(d2 - as.Date("1960-01-01")))
  for (f in fmts) {
    dd <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(dd)) return(as.numeric(dd - as.Date("1960-01-01")))
  }
  return(NA_real_)
}
parse_anydtdte_vec <- function(v) {
  v <- as.character(v)
  sapply(v, parse_one_anydtdte, USE.NAMES = FALSE)
}
sas_to_iso <- function(num) {
  # PUT(num, IS8601DA.) -> YYYY-MM-DD, missing -> ""
  res <- rep("", length(num))
  ok <- !is.na(num)
  res[ok] <- format(as.Date("1960-01-01") + num[ok], "%Y-%m-%d")
  res
}
get_sas_num <- function(col) {
  if (is.numeric(col)) {
    return(as.numeric(col))
  } else if (inherits(col, "Date")) {
    return(as.numeric(col - as.Date("1960-01-01")))
  } else if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    return(as.numeric(as.Date(col) - as.Date("1960-01-01")))
  } else {
    return(parse_anydtdte_vec(as.character(col)))
  }
}

# 1. IMPORT RAW ADVERSE EVENTS (guessingrows=max -> guess_max large, validvarname=upcase)
raw_ae <- readr::read_csv(raw_path, guess_max = 1000000, show_col_types = FALSE, progress = FALSE)
names(raw_ae) <- toupper(names(raw_ae))

# helper to fetch column case-insensitively (names already upper)
getcol <- function(df, nm) {
  if (nm %in% names(df)) return(df[[nm]])
  return(rep(NA, nrow(df)))
}
ID_raw <- getcol(raw_ae, "ID")
AE_TERM_raw <- getcol(raw_ae, "AE_TERM")
SEV_raw <- getcol(raw_ae, "SEV")
SERIOUS_raw <- getcol(raw_ae, "SERIOUS")
RELATED_raw <- getcol(raw_ae, "RELATED")
START_raw <- getcol(raw_ae, "START")
END_raw <- getcol(raw_ae, "END")

n <- nrow(raw_ae)

# 2. CORE TRANSFORMATION & DERIVATION
STUDYID_v <- rep(sas_trunc("CDISC-01", 8), n)
DOMAIN_v <- rep(sas_trunc("AE", 2), n)

# USUBJID = catx("-", STUDYID, ID)
if (is.numeric(ID_raw)) {
  ID_char <- trimws(format(ID_raw, scientific = FALSE, trim = TRUE))
  ID_char[is.na(ID_raw)] <- ""
  # format may produce "NA" for NA; fix
  ID_char[ID_char == "NA"] <- ""
} else {
  ID_char <- sas_strip(ID_raw)
}
USUBJID_v <- ifelse(ID_char == "", STUDYID_v, paste0(STUDYID_v, "-", ID_char))
USUBJID_v <- sas_trunc(USUBJID_v, 40)

AETERM_v <- sas_trunc(sas_strip(AE_TERM_raw), 60)
# AEDECOD = upcase(AETERM)
AEDECOD_v <- sas_trunc(toupper(AETERM_v), 60)
# handle NA AETERM: sas_strip gave "" so fine; ensure missing stays ""
AETERM_v[is.na(AETERM_v)] <- ""
AEDECOD_v[is.na(AEDECOD_v)] <- ""

AESEV_v <- sas_trunc(sas_upcase_strip(SEV_raw), 10)

SER_up <- sas_upcase_strip(SERIOUS_raw)
AESER_v <- ifelse(SER_up == "Y", "Y", "N")
AESER_v <- sas_trunc(AESER_v, 1)

REL_up <- sas_upcase_strip(RELATED_raw)
AEREL_v <- ifelse(REL_up == "Y", "RELATED",
           ifelse(REL_up == "N", "NOT RELATED", "UNKNOWN"))
AEREL_v <- sas_trunc(AEREL_v, 15)

# ISO 8601 DATE CONVERSION & OUTCOME
# mimic vtype check per column
start_num <- get_sas_num(START_raw)
end_num <- get_sas_num(END_raw)

AESTDTC_v <- sas_to_iso(start_num)
AEENDTC_v <- sas_to_iso(end_num)
# AEOUT based on _end_num missing or not
AEOUT_v <- ifelse(is.na(end_num), "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED")
AEOUT_v <- sas_trunc(AEOUT_v, 30)

# ensure missing character are "" (SAS blank) not NA for sorting/display
AESTDTC_v[is.na(AESTDTC_v)] <- ""
AEENDTC_v[is.na(AEENDTC_v)] <- ""
AETERM_v[is.na(AETERM_v)] <- ""
AEDECOD_v[is.na(AEDECOD_v)] <- ""
AESEV_v[is.na(AESEV_v)] <- ""
AESER_v[is.na(AESER_v)] <- ""
AEREL_v[is.na(AEREL_v)] <- ""
AEOUT_v[is.na(AEOUT_v)] <- ""

ae_mapped <- data.frame(
  STUDYID = STUDYID_v,
  DOMAIN = DOMAIN_v,
  USUBJID = USUBJID_v,
  AETERM = AETERM_v,
  AEDECOD = AEDECOD_v,
  AESEV = AESEV_v,
  AESER = AESER_v,
  AEREL = AEREL_v,
  AESTDTC = AESTDTC_v,
  AEENDTC = AEENDTC_v,
  AEOUT = AEOUT_v,
  stringsAsFactors = FALSE
)

# 3. GENERATE SEQUENCE NUMBER (AESEQ) - Sort by Subject and Start Date first
# SAS sorts: character ordering (uppercase before lowercase), missing first
# AESTDTC "" sorts first naturally in R; USUBJID ascending
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, na.last = FALSE, method = "radix")
ae_mapped <- ae_mapped[ord, , drop = FALSE]

# BY-group processing
AESEQ_v <- integer(nrow(ae_mapped))
if (nrow(ae_mapped) > 0) {
  cur_id <- ""
  cnt <- 0L
  for (i in seq_len(nrow(ae_mapped))) {
    uid <- ae_mapped$USUBJID[i]
    if (is.na(uid)) uid <- ""
    if (i == 1 || uid != cur_id) {
      cnt <- 1L
      cur_id <- uid
    } else {
      cnt <- cnt + 1L
    }
    AESEQ_v[i] <- cnt
  }
}
ae_mapped$AESEQ <- AESEQ_v

# retain STUDYID DOMAIN USUBJID AESEQ; -> order columns
sdtm_ae <- ae_mapped[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]

# write dataset ae
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=12 var USUBJID AESEQ AETERM AESEV AESER AEREL AESTDTC AEENDTC AEOUT
print_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit_n <- min(12, nrow(sdtm_ae))
audit_df <- sdtm_ae[seq_len(audit_n), print_vars, drop = FALSE]
audit_df <- cbind(Obs = seq_len(audit_n), audit_df)

# CSV of table body
write.csv(audit_df, file.path(out, "P009.csv"), row.names = FALSE, na = "")

# plain-text rendering
txt_path <- file.path(out, "P009.txt")
con <- file(txt_path, open = "wt")
writeLines("AE Domain Audit (Adverse Events with Ongoing Logic)", con)
writeLines(paste0("Dataset: SDTM.AE  (N=", nrow(sdtm_ae), ", showing first ", audit_n, " observations sorted by USUBJID AESTDTC)"), con)
writeLines("", con)
hdr <- sprintf("%-4s %-20s %-5s %-30s %-10s %-5s %-13s %-10s %-10s %-25s",
  "Obs","USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)
for (i in seq_len(audit_n)) {
  r <- audit_df[i, ]
  line <- sprintf("%-4d %-20s %-5d %-30s %-10s %-5s %-13s %-10s %-10s %-25s",
    as.integer(r$Obs),
    as.character(r$USUBJID),
    as.integer(r$AESEQ),
    substr(as.character(r$AETERM), 1, 30),
    as.character(r$AESEV),
    as.character(r$AESER),
    as.character(r$AEREL),
    as.character(r$AESTDTC),
    as.character(r$AEENDTC),
    as.character(r$AEOUT))
  writeLines(line, con)
}
close(con)

# aliases to cover expected naming variants
try({ file.copy(file.path(out, "P009.txt"), file.path(out, "P009.lst.txt"), overwrite = TRUE) }, silent = TRUE)
try({ file.copy(file.path(out, "P009.csv"), file.path(out, "P009.lst.csv"), overwrite = TRUE) }, silent = TRUE)
try({
  lst_plain <- file.path(out, "P009.lst")
  file.copy(file.path(out, "P009.txt"), lst_plain, overwrite = TRUE)
}, silent = TRUE)
```