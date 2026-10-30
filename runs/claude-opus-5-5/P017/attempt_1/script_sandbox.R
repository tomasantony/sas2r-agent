# P017 - SDTM SV domain from raw_vitals.csv
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
usubjid_length <- 40

# ---- PROC IMPORT (read as character, then mimic type guessing) ----
raw_sv <- read_csv(file.path(project_path, "data/raw/raw_vitals.csv"),
                   col_types = cols(.default = col_character()),
                   na = c("", "NA"), trim_ws = TRUE, progress = FALSE)
names(raw_sv) <- toupper(names(raw_sv))

is_num_col <- function(x) {
  x <- x[!is.na(x) & x != ""]
  length(x) > 0 && all(grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", x))
}

# SAS BEST12-like numeric to character (as catx would do)
num2char <- function(v) {
  ifelse(is.na(v), NA_character_,
         sapply(v, function(z) {
           if (is.na(z)) return(NA_character_)
           s <- format(z, digits = 12, scientific = FALSE, trim = TRUE)
           sub("\\.?0+$", "", ifelse(grepl("\\.", s), s, paste0(s, ".")))
         }))
}

# PT_ID
pt_id <- raw_sv$PT_ID
if (is_num_col(pt_id)) {
  pt_id <- num2char(suppressWarnings(as.numeric(pt_id)))
} else {
  pt_id <- str_trim(pt_id)
}
pt_id[!is.na(pt_id) & pt_id == ""] <- NA

visit_num <- suppressWarnings(as.numeric(raw_sv$VISIT_NUM))
visit_raw <- if ("VISIT" %in% names(raw_sv)) raw_sv$VISIT else rep(NA_character_, nrow(raw_sv))

# ---- ANYDTDTE-like parser ----
mon_abb <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
mkdate <- function(y, m, d) {
  y <- as.integer(y); m <- as.integer(m); d <- as.integer(d)
  if (any(is.na(c(y, m, d)))) return(NA)
  if (y < 100) y <- ifelse(y < 40, 2000L + y, 1900L + y)   # YEARCUTOFF=1940
  r <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
  if (is.na(r)) NA else r
}
parse_anydt <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  s <- toupper(str_trim(s))
  if (s == "") return(as.Date(NA))
  s <- sub("[T ][0-9]{1,2}:[0-9]{2}.*$", "", s)
  r <- NA
  if (grepl("^[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}$", s)) {
    p <- strsplit(s, "[-/.]")[[1]]; r <- mkdate(p[1], p[2], p[3])
  } else if (grepl("^[0-9]{8}$", s)) {
    r <- mkdate(substr(s,1,4), substr(s,5,6), substr(s,7,8))
  } else if (grepl("^[0-9]{1,2}[-/.]?[A-Z]{3}[-/.]?[0-9]{2,4}$", s)) {
    m <- regmatches(s, regexec("^([0-9]{1,2})[-/.]?([A-Z]{3})[-/.]?([0-9]{2,4})$", s))[[1]]
    mi <- match(m[3], mon_abb); if (!is.na(mi)) r <- mkdate(m[4], mi, m[2])
  } else if (grepl("^[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{2,4}$", s)) {
    p <- strsplit(s, "[-/.]")[[1]]
    r <- mkdate(p[3], p[1], p[2])                     # MDY default
    if (is.na(r)) r <- mkdate(p[3], p[2], p[1])
  } else if (grepl("^[A-Z]{3}[ -]?[0-9]{1,2},? ?[0-9]{4}$", s)) {
    m <- regmatches(s, regexec("^([A-Z]{3})[ -]?([0-9]{1,2}),? ?([0-9]{4})$", s))[[1]]
    mi <- match(m[2], mon_abb); if (!is.na(mi)) r <- mkdate(m[4], mi, m[3])
  }
  if (is.na(r)) as.Date(NA) else as.Date(r, origin = "1970-01-01")
}

vs_date <- raw_sv$VS_DATE
svdt <- do.call(c, lapply(vs_date, parse_anydt))

# ---- DATA step with RETAIN semantics for SVSTDTC/SVENDTC ----
n <- nrow(raw_sv)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("SV", n)
USUBJID <- ifelse(is.na(pt_id), "CDISC-01", paste("CDISC-01", pt_id, sep = "-"))
USUBJID <- substr(USUBJID, 1, usubjid_length)
VISITNUM <- visit_num
VISIT <- substr(ifelse(is.na(visit_raw), "", str_trim(visit_raw)), 1, 40)
VISITDY <- ifelse(is.na(VISITNUM), NA_real_,
           ifelse(VISITNUM == 1, 1, ifelse(VISITNUM == 2, 31, ifelse(VISITNUM == 3, 61, NA_real_))))

SVSTDTC <- character(n); SVENDTC <- character(n)
cur_st <- ""; cur_en <- ""
for (i in seq_len(n)) {
  if (!is.na(svdt[i])) {
    cur_st <- format(svdt[i], "%Y-%m-%d"); cur_en <- cur_st
  }
  SVSTDTC[i] <- cur_st; SVENDTC[i] <- cur_en
}

sv <- data.frame(STUDYID, DOMAIN, USUBJID, VISIT, SVSTDTC, SVENDTC,
                 VISITNUM, VISITDY, stringsAsFactors = FALSE)

# ---- PROC SORT by USUBJID VISITNUM (stable, missing first) ----
ord <- order(sv$USUBJID, !is.na(sv$VISITNUM), sv$VISITNUM, method = "radix")
sv <- sv[ord, , drop = FALSE]
rownames(sv) <- NULL

write.csv(sv, file.path(out, "sv.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT (obs=12) ----
pr <- head(sv, 12)
fmtnum <- function(x) ifelse(is.na(x), ".", num2char(x))
lst <- data.frame(Obs = as.character(seq_len(nrow(pr))),
                  STUDYID = pr$STUDYID, DOMAIN = pr$DOMAIN, USUBJID = pr$USUBJID,
                  VISIT = pr$VISIT, SVSTDTC = pr$SVSTDTC, SVENDTC = pr$SVENDTC,
                  VISITNUM = fmtnum(pr$VISITNUM), VISITDY = fmtnum(pr$VISITDY),
                  stringsAsFactors = FALSE)
write.csv(lst, file.path(out, "P017.csv"), row.names = FALSE, na = "")

right_cols <- c("Obs", "VISITNUM", "VISITDY")
widths <- sapply(names(lst), function(c) max(nchar(c), nchar(lst[[c]]), na.rm = TRUE))
padc <- function(x, w, right) if (right) formatC(x, width = w) else formatC(x, width = w, flag = "-")
hdr <- paste(mapply(function(c, w) padc(c, w, c %in% right_cols), names(lst), widths), collapse = "  ")
body <- apply(lst, 1, function(r) paste(mapply(function(v, c, w) padc(v, w, c %in% right_cols),
                                               r, names(lst), widths), collapse = "  "))
writeLines(c("SV Domain Audit", "", hdr, body), file.path(out, "P017.txt"))

cat("P017 done: sv rows =", nrow(sv), "\n")
