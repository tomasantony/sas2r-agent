```r
# P017_sdtm_sv.sas -> R translation
# Creates SDTM SV from raw_vitals.csv, sorts, prints first 12 obs

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_vitals.csv")

# --- Read raw CSV (PROC IMPORT dbms=csv getnames=yes) ---
# Read everything as character to preserve VVALUE behaviour, then convert types explicitly
raw_sv <- suppressMessages(readr::read_csv(
  raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  trim_ws = TRUE,
  guess_max = 1000000,
  progress = FALSE
))
names(raw_sv) <- toupper(names(raw_sv))

# SAS validvarname=upcase => all vars uppercase
# Expected vars: PT_ID, VISIT_NUM, VISIT, VS_DATE
stopifnot("PT_ID" %in% names(raw_sv))
stopifnot("VISIT_NUM" %in% names(raw_sv))
stopifnot("VISIT" %in% names(raw_sv))
stopifnot("VS_DATE" %in% names(raw_sv))

# Helper: SAS STRIP = trim both + SAS LENGTH truncation
sas_strip <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# Helper: SAS CATX("-", STUDYID, PT_ID) : strip each, skip missing/blank, join, truncate
sas_catx <- function(delim, ...) {
  args <- list(...)
  # vectorised over longest
  n <- max(lengths(args))
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  vapply(seq_len(n), function(i) {
    parts <- vapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return("")
      v <- trimws(v)
      if (v == "" || toupper(v) == "NA") return("")
      v
    }, character(1))
    parts <- parts[parts != ""]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, character(1))
}

# Helper: SAS ANYDTDTE32 for this column.
# Raw values observed as DATE11 e.g. 15-JAN-2023 -> ONE explicit strict format %d-%b-%Y
# (SAS ANYDTDTE with default MDY order; width 32 avoids truncation of DATE11).
parse_sv_date <- function(x) {
  x <- as.character(x)
  vapply(x, function(one) {
    if (is.na(one)) return(as.character(NA))
    s <- trimws(one)
    if (s == "" || toupper(s) == "NA" || s == ".") return(as.character(NA))
    # normalize separator "/" -> "-" so DATE11 with slashes still maps to same format
    s <- gsub("/", "-", s, fixed = TRUE)
    # title-case month abbreviation for R %b (Jan vs JAN)
    prt <- strsplit(s, "-", fixed = TRUE)[[1]]
    if (length(prt) == 3L && grepl("^[A-Za-z]+$", prt[2])) {
      mon <- prt[2]
      mon <- paste0(toupper(substr(mon, 1L, 1L)), tolower(substr(mon, 2L, nchar(mon))))
      s <- paste(prt[1], mon, prt[3], sep = "-")
    }
    d <- as.Date(s, format = "%d-%b-%Y")
    as.character(d)
  }, character(1))
}

n <- nrow(raw_sv)

STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("SV", n)
# USUBJID length $40
USUBJID_raw <- sas_catx("-", STUDYID, sas_strip(raw_sv$PT_ID))
USUBJID <- substr(USUBJID_raw, 1L, 40L)
USUBJID[USUBJID == ""] <- NA_character_

# VISITNUM = VISIT_NUM (numeric); SAS auto-converts char->num
VISITNUM <- suppressWarnings(as.numeric(trimws(as.character(raw_sv$VISIT_NUM))))

# VISIT length $40, strip
VISIT <- substr(sas_strip(raw_sv$VISIT), 1L, 40L)
VISIT[VISIT == ""] <- NA_character_

# VISITDY by VISITNUM
VISITDY <- rep(NA_real_, n)
VISITDY[!is.na(VISITNUM) & VISITNUM == 1] <- 1
VISITDY[!is.na(VISITNUM) & VISITNUM == 2] <- 31
VISITDY[!is.na(VISITNUM) & VISITNUM == 3] <- 61

# _svdt = input(strip(vvalue(VS_DATE)), anydtdte32.)
vs_vvalue <- sas_strip(raw_sv$VS_DATE)
svdt_chr <- parse_sv_date(vs_vvalue)
svdt <- as.Date(svdt_chr, format = "%Y-%m-%d")

SVSTDTC <- ifelse(!is.na(svdt), format(svdt, "%Y-%m-%d"), NA_character_)
SVENDTC <- SVSTDTC
# LENGTH $10 truncation (ISO date is 10)
SVSTDTC <- substr(SVSTDTC, 1L, 10L)
SVENDTC <- substr(SVENDTC, 1L, 10L)

sv <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  VISITNUM = VISITNUM,
  VISIT = VISIT,
  VISITDY = VISITDY,
  SVSTDTC = SVSTDTC,
  SVENDTC = SVENDTC,
  stringsAsFactors = FALSE
)

# --- PROC SORT by USUBJID VISITNUM (keep duplicates, missing first, ASCII order) ---
# SAS character order: uppercase before lowercase (byte order). USUBJID values share
# same case so base order matches; enforce byte order via xtfrm on code points for safety.
byte_key <- vapply(sv$USUBJID, function(v) {
  if (is.na(v)) return("")
  paste(sprintf("%03d", utf8ToInt(v)), collapse = "-")
}, character(1))
ord <- order(is.na(sv$USUBJID), byte_key, is.na(sv$VISITNUM), sv$VISITNUM,
             na.last = FALSE, method = "radix")
sv <- sv[ord, , drop = FALSE]
rownames(sv) <- NULL

# --- Write dataset sv.csv ---
write.csv(sv, file.path(out, "sv.csv"), row.names = FALSE, na = "")

# --- PROC PRINT obs=12 title "SV Domain Audit" ---
print12 <- utils::head(sv, 12)
print12$OBS <- seq_len(nrow(print12))
print12 <- print12[, c("OBS", "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "VISITDY", "SVSTDTC", "SVENDTC")]

# CSV of table body
write.csv(print12, file.path(out, "P017.csv"), row.names = FALSE, na = "")

# Text rendering (PROC PRINT style)
fmt_num <- function(v) {
  ifelse(is.na(v), ".", format(v, scientific = FALSE, trim = TRUE))
}
fmt_chr <- function(v) {
  ifelse(is.na(v), "", v)
}
hdr <- sprintf("%-4s %-8s %-6s %-20s %-8s %-25s %-7s %-10s %-10s",
               "Obs", "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "VISITDY", "SVSTDTC", "SVENDTC")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
lines <- c("SV Domain Audit", "", hdr, sep)
for (i in seq_len(nrow(print12))) {
  r <- print12[i, ]
  lines <- c(lines, sprintf("%-4s %-8s %-6s %-20s %-8s %-25s %-7s %-10s %-10s",
    fmt_num(r$OBS),
    fmt_chr(r$STUDYID),
    fmt_chr(r$DOMAIN),
    fmt_chr(r$USUBJID),
    fmt_num(r$VISITNUM),
    fmt_chr(r$VISIT),
    fmt_num(r$VISITDY),
    fmt_chr(r$SVSTDTC),
    fmt_chr(r$SVENDTC)))
}
lines <- c(lines, "", paste0("NOTE: First 12 observations of SDTM SV (N=", nrow(sv), "). Sorted by USUBJID VISITNUM."))
txt_path <- file.path(out, "P017.txt")
writeLines(lines, txt_path)
# SAS-style .lst counterpart (same content)
writeLines(lines, file.path(out, "P017.lst"))
```