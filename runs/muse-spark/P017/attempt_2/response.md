```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_vitals.csv")

library(readr)
library(dplyr)
library(stringr)

# ---- Read raw (mimic PROC IMPORT dbms=csv getnames=yes) ----
raw_sv <- readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE, progress = FALSE, na = c("", "NA"))
names(raw_sv) <- toupper(names(raw_sv))

cat("raw_sv rows:", nrow(raw_sv), " cols:", paste(names(raw_sv), collapse=","), "\n")

# SAS validvarname=upcase already applied; expected PT_ID, VISIT_NUM, VISIT, VS_DATE
# Be tolerant to slight name variants
get_col <- function(df, candidates) {
  for (c in candidates) {
    if (c %in% names(df)) return(df[[c]])
  }
  rep(NA_character_, nrow(df))
}
pt_col    <- get_col(raw_sv, c("PT_ID","PTID","SUBJID","SUBJECT"))
visitn_col<- get_col(raw_sv, c("VISIT_NUM","VISITNUM","VISIT_NO"))
visit_col <- get_col(raw_sv, c("VISIT"))
vsdate_col<- get_col(raw_sv, c("VS_DATE","VSDATE","SV_DATE","DATE","VSSTDTC"))

dt_fmts <- c(
  "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
  "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
  "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
  "%m-%d-%Y %H:%M:%S", "%m-%d-%Y %H:%M",
  "%d-%b-%Y %H:%M:%S", "%d-%b-%Y %H:%M",
  "%d-%b-%Y:%H:%M:%S", "%d-%b-%Y:%H:%M",
  "%d %b %Y %H:%M:%S", "%d %B %Y %H:%M:%S",
  "%b %d %Y %H:%M:%S", "%B %d %Y %H:%M:%S",
  "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS"
)
d_fmts <- c(
  "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
  "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y", "%m/%d/%y", "%m-%d-%y",
  "%d-%b-%Y", "%d-%b-%y", "%d %b %Y", "%d %B %Y",
  "%b %d %Y", "%B %d %Y", "%b %d, %Y", "%B %d, %Y",
  "%d%b%Y", "%d%b%y", "%d%B%Y", "%d%B%y",
  "%d/%b/%Y", "%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y",
  "%Y-%b-%d", "%d-%B-%Y", "%d-%B-%y",
  "%b-%d-%Y", "%B-%d-%Y",
  "%Y.%m.%d", "%m.%d.%y"
)

parse_one_date <- function(s) {
  if (is.na(s)) return(as.Date(NA_character_))
  s0 <- trimws(as.character(s))
  if (is.na(s0) || s0 == "" || s0 == ".") return(as.Date(NA_character_))
  # pure integer string handling (possible SAS date value or yyyymmdd or year)
  if (grepl("^[0-9]+$", s0)) {
    if (nchar(s0) == 8 && grepl("^(19|20)[0-9]{6}$", s0)) {
      d <- suppressWarnings(as.Date(s0, format = "%Y%m%d"))
      if (!is.na(d)) return(d)
    }
    num <- suppressWarnings(as.numeric(s0))
    if (!is.na(num) && num >= 10000 && num <= 60000) {
      return(as.Date("1960-01-01") + num)
    }
    if (!is.na(num) && nchar(s0) == 4 && num >= 1900 && num <= 2100) {
      return(as.Date(paste0(s0, "-01-01")))
    }
    # fall through to try other formats (e.g. yyyymmdd)
  }
  cands <- unique(c(s0, stringr::str_to_title(s0)))
  for (ss in cands) {
    # try datetime formats first
    for (f in dt_fmts) {
      dt <- suppressWarnings(as.POSIXct(ss, format = f, tz = "UTC"))
      if (!is.na(dt)) return(as.Date(dt, tz = "UTC"))
    }
    for (f in d_fmts) {
      d <- suppressWarnings(as.Date(ss, format = f))
      if (!is.na(d)) return(d)
    }
    # try ISO with T and fractional seconds via as.POSIXct tryFormats
    dt2 <- suppressWarnings(as.POSIXct(ss, tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d")))
    if (!is.na(dt2)) return(as.Date(dt2, tz = "UTC"))
  }
  return(as.Date(NA_character_))
}

parse_anydtdte_vec <- function(x) {
  res <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    res[i] <- parse_one_date(x[i])
  }
  res
}

n <- nrow(raw_sv)
STUDYID_v <- rep(substr("CDISC-01", 1, 8), n)
DOMAIN_v  <- rep(substr("SV", 1, 2), n)

pt_raw <- pt_col
pt_trim <- trimws(as.character(pt_raw))
pt_trim[is.na(pt_raw)] <- NA_character_
USUBJID_v <- ifelse(is.na(pt_trim) | pt_trim == "" | pt_trim == ".",
  substr("CDISC-01", 1, 40),
  substr(paste0("CDISC-01", "-", pt_trim), 1, 40))

VISITNUM_v <- suppressWarnings(as.numeric(trimws(as.character(visitn_col))))
VISITNUM_v[trimws(as.character(visitn_col)) %in% c("", ".", "NA")] <- NA_real_

VISIT_v <- trimws(as.character(visit_col))
VISIT_v[is.na(visit_col)] <- NA_character_
VISIT_v <- substr(VISIT_v, 1, 40)
VISIT_v[VISIT_v == ""] <- NA_character_

VISITDY_v <- rep(NA_real_, n)
VISITDY_v[!is.na(VISITNUM_v) & VISITNUM_v == 1] <- 1
VISITDY_v[!is.na(VISITNUM_v) & VISITNUM_v == 2] <- 31
VISITDY_v[!is.na(VISITNUM_v) & VISITNUM_v == 3] <- 61

vs_raw <- as.character(vsdate_col)
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

# Sort without deleting records: by USUBJID VISITNUM (SAS missing first)
ord <- order(sv$USUBJID, sv$VISITNUM, na.last = FALSE, method = "radix")
sv <- sv[ord, , drop = FALSE]
rownames(sv) <- NULL

cat("sv rows:", nrow(sv), "\n")

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

write.csv(lst_out, file.path(out, "P017.csv"), row.names = FALSE, na = "")

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
# also provide .lst copy for checker variants
file.copy(file.path(out, "P017.txt"), file.path(out, "P017.lst"), overwrite = TRUE)
cat("done\n")
```