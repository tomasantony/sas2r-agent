```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_ae.csv")
if (!file.exists(raw_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_ae.csv"
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv"
  alt3 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_ae.csv"
  for (a in c(alt1, alt2, alt3)) { if (file.exists(a)) { raw_path <- a; break } }
}

sas_trunc <- function(x, n) {
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

parse_one_anydtdte <- function(s) {
  if (length(s) == 0) return(NA_real_)
  if (is.na(s)) return(NA_real_)
  s <- trimws(as.character(s))
  if (is.na(s) || s == "") return(NA_real_)
  fmts_date <- c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y","%m-%d-%Y","%d-%m-%Y",
    "%Y%m%d","%d%b%Y","%d-%b-%Y","%d %b %Y","%d %B %Y","%b %d, %Y","%B %d, %Y",
    "%Y.%m.%d","%m.%d.%Y","%d.%m.%Y","%m/%d/%y","%d/%m/%y","%d%b%y","%d-%b-%y",
    "%d %b %y","%b %d, %y","%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S",
    "%Y/%m/%d %H:%M:%S","%m/%d/%Y %H:%M:%S")
  d <- suppressWarnings(as.Date(s, tryFormats = fmts_date))
  if (length(d) == 1 && !is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
  fmts_dt <- c("%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S",
    "%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M","%Y-%m-%d %H:%M","%Y-%m-%dT%H:%M",
    "%d-%b-%Y %H:%M:%S","%d %b %Y %H:%M:%S","%d-%b-%Y %H:%M")
  for (f in fmts_dt) {
    dt <- suppressWarnings(as.POSIXct(s, format = f, tz = "UTC"))
    if (length(dt) == 1 && !is.na(dt)) {
      dd <- suppressWarnings(as.Date(dt, tz = "UTC"))
      if (!is.na(dd)) return(as.numeric(dd - as.Date("1960-01-01")))
    }
  }
  s2 <- sub("[T ].*$", "", s)
  if (!identical(s2, s)) {
    d2 <- suppressWarnings(as.Date(s2, tryFormats = fmts_date))
    if (length(d2) == 1 && !is.na(d2)) return(as.numeric(d2 - as.Date("1960-01-01")))
  }
  return(NA_real_)
}
parse_anydtdte_vec <- function(v) {
  v <- as.character(v)
  outv <- rep(NA_real_, length(v))
  for (i in seq_along(v)) outv[i] <- parse_one_anydtdte(v[i])
  outv
}
sas_to_iso <- function(num) {
  res <- rep("", length(num))
  ok <- !is.na(num)
  res[ok] <- format(as.Date("1960-01-01") + num[ok], "%Y-%m-%d")
  res
}
get_sas_num <- function(col) {
  if (is.numeric(col)) return(as.numeric(col))
  if (inherits(col, "Date")) return(as.numeric(col - as.Date("1960-01-01")))
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    return(as.numeric(as.Date(col, tz = "UTC") - as.Date("1960-01-01")))
  }
  parse_anydtdte_vec(as.character(col))
}

# 1. IMPORT RAW ADVERSE EVENTS (guessingrows=max, getnames=yes, validvarname=upcase)
raw_ae <- suppressWarnings(readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  guess_max = 1000000, show_col_types = FALSE, progress = FALSE))
names(raw_ae) <- toupper(names(raw_ae))

getcol <- function(df, nm) {
  if (nm %in% names(df)) return(df[[nm]])
  rep(NA_character_, nrow(df))
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
ID_char <- sas_strip(ID_raw)
USUBJID_v <- ifelse(ID_char == "", STUDYID_v, paste0(STUDYID_v, "-", ID_char))
USUBJID_v <- sas_trunc(USUBJID_v, 40)

AETERM_v <- sas_trunc(sas_strip(AE_TERM_raw), 60)
AEDECOD_v <- sas_trunc(toupper(AETERM_v), 60)
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

start_num <- get_sas_num(START_raw)
end_num <- get_sas_num(END_raw)
AESTDTC_v <- sas_to_iso(start_num)
AEENDTC_v <- sas_to_iso(end_num)
AEOUT_v <- ifelse(is.na(end_num), "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED")
AEOUT_v <- sas_trunc(AEOUT_v, 30)

AESTDTC_v[is.na(AESTDTC_v)] <- ""
AEENDTC_v[is.na(AEENDTC_v)] <- ""
AESEV_v[is.na(AESEV_v)] <- ""
AESER_v[is.na(AESER_v)] <- ""
AEREL_v[is.na(AEREL_v)] <- ""
AEOUT_v[is.na(AEOUT_v)] <- ""

ae_mapped <- data.frame(
  STUDYID = STUDYID_v, DOMAIN = DOMAIN_v, USUBJID = USUBJID_v,
  AETERM = AETERM_v, AEDECOD = AEDECOD_v, AESEV = AESEV_v,
  AESER = AESER_v, AEREL = AEREL_v, AESTDTC = AESTDTC_v,
  AEENDTC = AEENDTC_v, AEOUT = AEOUT_v,
  stringsAsFactors = FALSE
)

# 3. GENERATE SEQUENCE NUMBER (AESEQ) - sort by USUBJID AESTDTC
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, na.last = FALSE, method = "radix")
ae_mapped <- ae_mapped[ord, , drop = FALSE]
AESEQ_v <- integer(nrow(ae_mapped))
if (nrow(ae_mapped) > 0) {
  cur_id <- NA_character_
  cnt <- 0L
  for (i in seq_len(nrow(ae_mapped))) {
    uid <- ae_mapped$USUBJID[i]
    if (is.na(uid)) uid <- ""
    if (i == 1 || !identical(uid, cur_id)) { cnt <- 1L; cur_id <- uid } else { cnt <- cnt + 1L }
    AESEQ_v[i] <- cnt
  }
}
ae_mapped$AESEQ <- AESEQ_v
sdtm_ae <- ae_mapped[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]

write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=12
print_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit_n <- min(12, nrow(sdtm_ae))
audit_df <- sdtm_ae[seq_len(audit_n), print_vars, drop = FALSE]
audit_df <- cbind(Obs = seq_len(audit_n), audit_df)
write.csv(audit_df, file.path(out, "P009.csv"), row.names = FALSE, na = "")

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
    as.integer(r$Obs), as.character(r$USUBJID), as.integer(r$AESEQ),
    substr(as.character(r$AETERM), 1, 30), as.character(r$AESEV),
    as.character(r$AESER), as.character(r$AEREL),
    as.character(r$AESTDTC), as.character(r$AEENDTC), as.character(r$AEOUT))
  writeLines(line, con)
}
close(con)
try({ file.copy(file.path(out, "P009.txt"), file.path(out, "P009.lst"), overwrite = TRUE) }, silent = TRUE)
try({ file.copy(file.path(out, "P009.csv"), file.path(out, "P009.lst.csv"), overwrite = TRUE) }, silent = TRUE)
```