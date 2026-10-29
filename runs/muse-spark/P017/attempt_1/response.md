```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_vitals.csv")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# ---- Read raw (mimic PROC IMPORT dbms=csv getnames=yes) ----
raw_sv <- readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE, progress = FALSE, na = c("", "NA"))
names(raw_sv) <- toupper(names(raw_sv))

# Expected columns: PT_ID, VISIT_NUM, VISIT, VS_DATE
# Ensure they exist (SAS validvarname=upcase already applied)
# No extra handling - keep close to SAS

parse_anydtdte_vec <- function(x) {
  res <- rep(as.Date(NA_character_), length(x))
  for (i in seq_along(x)) {
    s0 <- x[i]
    if (is.na(s0)) next
    s <- trimws(as.character(s0))
    if (is.na(s) || s == "" || s == ".") next
    dt <- suppressWarnings(lubridate::parse_date_time(s,
      orders = c("ymd HMS","ymd HM","ymd","Ymd HMS","Ymd",
                 "dmy HMS","dmy HM","dmy","mdy HMS","mdy HM","mdy",
                 "dBY HMS","dBY","dbY HMS","dbY",
                 "BYd HMS","BYd","bdY HMS","bdY",
                 "Ymd HMS","YmdT HMS","ymdT HMS","ymdT"),
      quiet = TRUE, tz = "UTC", truncated = 3))
    if (length(dt) == 1 && !is.na(dt)) {
      res[i] <- as.Date(dt, tz = "UTC")
      next
    }
    s_title <- suppressWarnings(stringr::str_to_title(s))
    candidates <- unique(c(s, s_title))
    parsed <- as.Date(NA_character_)
    fmts_d <- c("%Y-%m-%d","%Y/%m/%d","%Y.%m.%d",
                "%m/%d/%Y","%d/%m/%Y",
                "%d-%b-%Y","%d-%B-%Y","%d %b %Y","%d %B %Y",
                "%b %d %Y","%B %d %Y","%d%b%Y","%d%B%Y",
                "%m-%d-%Y","%d-%m-%Y","%Y-%m-%dT%H:%M:%S",
                "%Y-%m-%d %H:%M:%S","%d-%b-%y","%m-%d-%y")
    fmts_dt <- c("%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S",
                 "%m/%d/%Y %H:%M","%d-%b-%Y %H:%M",
                 "%d-%b-%Y:%H:%M:%S","%Y/%m/%d %H:%M:%S")
    found <- FALSE
    for (ss in candidates) {
      for (f in fmts_d) {
        d <- suppressWarnings(as.Date(ss, format = f))
        if (!is.na(d)) { parsed <- d; found <- TRUE; break }
      }
      if (found) break
      for (f in fmts_dt) {
        dt2 <- suppressWarnings(as.POSIXct(ss, format = f, tz = "UTC"))
        if (!is.na(dt2)) { parsed <- as.Date(dt2, tz = "UTC"); found <- TRUE; break }
      }
      if (found) break
    }
    if (!is.na(parsed)) res[i] <- parsed
  }
  res
}

# ---- Build SDTM SV (mimic data sdtm.sv) ----
# retain STUDYID DOMAIN USUBJID VISITNUM VISIT VISITDY SVSTDTC SVENDTC
n <- nrow(raw_sv)
STUDYID_v <- rep(substr("CDISC-01", 1, 8), n)
DOMAIN_v <- rep(substr("SV", 1, 2), n)

pt_raw <- raw_sv$PT_ID
pt_trim <- trimws(as.character(pt_raw))
pt_trim[is.na(pt_raw)] <- NA_character_
# CATX("-", STUDYID, PT_ID): strip, skip missing
USUBJID_v <- ifelse(is.na(pt_trim) | pt_trim == "" | pt_trim == ".",
  substr("CDISC-01", 1, 40),
  substr(paste0("CDISC-01", "-", pt_trim), 1, 40))

VISITNUM_v <- suppressWarnings(as.numeric(raw_sv$VISIT_NUM))

VISIT_v <- trimws(as.character(raw_sv$VISIT))
VISIT_v[is.na(raw_sv$VISIT)] <- NA_character_
VISIT_v <- substr(VISIT_v, 1, 40)
VISIT_v[VISIT_v == ""] <- NA_character_

VISITDY_v <- rep(NA_real_, n)
VISITDY_v[!is.na(VISITNUM_v) & VISITNUM_v == 1] <- 1
VISITDY_v[!is.na(VISITNUM_v) & VISITNUM_v == 2] <- 31
VISITDY_v[!is.na(VISITNUM_v) & VISITNUM_v == 3] <- 61
# otherwise missing (already NA)

vs_raw <- as.character(raw_sv$VS_DATE)
svdt <- parse_anydtdte_vec(vs_raw)
SVSTDTC_v <- format(svdt, "%Y-%m-%d")
SVSTDTC_v <- substr(ifelse(is.na(svdt), NA_character_, SVSTDTC_v), 1, 10)
SVENDTC_v <- SVSTDTC_v

sv <- data.frame(
  STUDYID = STUDYID_v,
  DOMAIN = DOMAIN_v,
  USUBJID = USUBJID_v,
  VISITNUM = VISITNUM_v,
  VISIT = VISIT_v,
  VISITDY = VISITDY_v,
  SVSTDTC = SVSTDTC_v,
  SVENDTC = SVENDTC_v,
  stringsAsFactors = FALSE
)

# ---- Sort without deleting records: by USUBJID VISITNUM (SAS: missing first, uppercase before lowercase) ----
ord <- order(sv$USUBJID, sv$VISITNUM, na.last = FALSE, method = "radix")
sv <- sv[ord, , drop = FALSE]
rownames(sv) <- NULL

# ---- Write dataset sv.csv ----
write.csv(sv, file.path(out, "sv.csv"), row.names = FALSE, na = "")

# ---- Listing: PROC PRINT obs=12 with title "SV Domain Audit" ----
lst_n <- min(12, nrow(sv))
lst <- sv[seq_len(lst_n), , drop = FALSE]
lst_out <- data.frame(
  Obs = seq_len(lst_n),
  STUDYID = lst$STUDYID,
  DOMAIN = lst$DOMAIN,
  USUBJID = lst$USUBJID,
  VISITNUM = lst$VISITNUM,
  VISIT = lst$VISIT,
  VISITDY = lst$VISITDY,
  SVSTDTC = lst$SVSTDTC,
  SVENDTC = lst$SVENDTC,
  stringsAsFactors = FALSE
)

# CSV of table body
write.csv(lst_out, file.path(out, "P017.csv"), row.names = FALSE, na = "")

# Text rendering (PROC PRINT style)
txt_file <- file.path(out, "P017.txt")
con <- file(txt_file, open = "wt")
cat("SV Domain Audit\n", file = con)
cat("\n", file = con)
hdr <- sprintf("%-4s %-8s %-6s %-20s %-8s %-25s %-7s %-10s %-10s",
  "Obs", "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "VISITDY", "SVSTDTC", "SVENDTC")
cat(hdr, "\n", file = con, sep = "")
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(lst_out))) {
  visnum_s <- ifelse(is.na(lst_out$VISITNUM[i]), "", format(lst_out$VISITNUM[i], scientific = FALSE, trim = TRUE))
  visdy_s <- ifelse(is.na(lst_out$VISITDY[i]), "", format(lst_out$VISITDY[i], scientific = FALSE, trim = TRUE))
  visit_s <- ifelse(is.na(lst_out$VISIT[i]), "", lst_out$VISIT[i])
  stdtc_s <- ifelse(is.na(lst_out$SVSTDTC[i]), "", lst_out$SVSTDTC[i])
  endtc_s <- ifelse(is.na(lst_out$SVENDTC[i]), "", lst_out$SVENDTC[i])
  line <- sprintf("%-4d %-8s %-6s %-20s %-8s %-25s %-7s %-10s %-10s",
    lst_out$Obs[i],
    ifelse(is.na(lst_out$STUDYID[i]), "", lst_out$STUDYID[i]),
    ifelse(is.na(lst_out$DOMAIN[i]), "", lst_out$DOMAIN[i]),
    ifelse(is.na(lst_out$USUBJID[i]), "", lst_out$USUBJID[i]),
    visnum_s, visit_s, visdy_s, stdtc_s, endtc_s)
  cat(line, "\n", file = con, sep = "")
}
close(con)
```