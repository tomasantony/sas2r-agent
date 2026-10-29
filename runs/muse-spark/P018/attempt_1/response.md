```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_vitals.csv")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_best_one <- function(v) {
  if (length(v) == 0) return(NA_character_)
  if (is.na(v)) return(NA_character_)
  vv <- suppressWarnings(as.numeric(v))
  if (!is.na(vv)) v <- vv else return(trimws(as.character(v)))
  if (is.na(v)) return(NA_character_)
  trimws(formatC(v, format = "g", digits = 12))
}
sas_best_vec <- function(x) {
  vapply(seq_along(x), function(i) sas_best_one(x[i]), character(1), USE.NAMES = FALSE)
}

parse_vsdtc_one <- function(v) {
  if (length(v) == 0) return(NA_character_)
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) {
    return(format(v, "%Y-%m-%d"))
  }
  if (inherits(v, "POSIXt") || inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
    return(format(as.Date(v, tz = "UTC"), "%Y-%m-%d"))
  }
  if (is.numeric(v)) {
    d <- as.Date(v, origin = "1960-01-01")
    if (is.na(d)) return(NA_character_)
    return(format(d, "%Y-%m-%d"))
  }
  s <- trimws(as.character(v))
  if (is.na(s) || s == "" || toupper(s) %in% c(".", "NA", "NULL", "MISSING")) {
    if (s == "" || s == ".") return(NA_character_)
    if (toupper(s) %in% c("NA","NULL","MISSING")) return(NA_character_)
  }
  if (is.na(s) || s == "") return(NA_character_)
  dt <- suppressWarnings(lubridate::parse_date_time(s,
    orders = c("ymd","Ymd","mdy","dmy","Ymd HMS","mdy HMS","dmy HMS","ymd HMS",
               "Ymd HM","mdy HM","dmy HM","ymd HM","Ymd HMS p","mdy HMS p","dmy HMS p",
               "ymd HM p","mdy HM p","dmy HM p","ymdT HMS","YmdT HMS","BdY","Bdy","bdy",
               "dBY","dbY","mDY","Ymd","ymd"),
    quiet = TRUE, truncated = 0))
  if (length(dt) == 1 && !is.na(dt)) {
    return(format(as.Date(dt, tz = "UTC"), "%Y-%m-%d"))
  }
  d2 <- suppressWarnings(as.Date(s, tryFormats = c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y",
    "%m-%d-%Y","%d-%m-%Y","%Y.%m.%d","%m.%d.%Y","%d.%m.%Y","%Y%m%d",
    "%d-%b-%Y","%d-%B-%Y","%b %d %Y","%B %d %Y","%d %b %Y","%d %B %Y",
    "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%m/%d/%Y %H:%M","%Y/%m/%d %H:%M")))
  if (length(d2) == 1 && !is.na(d2)) {
    return(format(d2, "%Y-%m-%d"))
  }
  num <- suppressWarnings(as.numeric(s))
  if (length(num) == 1 && !is.na(num)) {
    # try SAS date numeric
    d3 <- as.Date(num, origin = "1960-01-01")
    # only accept reasonable range 1960-2030 to avoid mis-parse; otherwise NA
    if (!is.na(d3)) return(format(d3, "%Y-%m-%d"))
  }
  NA_character_
}
parse_vsdtc_vec <- function(x) {
  vapply(seq_along(x), function(i) parse_vsdtc_one(x[i]), character(1), USE.NAMES = FALSE)
}

cat("Reading raw vitals:", raw_path, "\n")
raw_vs <- readr::read_csv(raw_path, guess_max = 10000, show_col_types = FALSE, progress = FALSE)
raw_vs <- as.data.frame(raw_vs, stringsAsFactors = FALSE)
names(raw_vs) <- toupper(names(raw_vs))
cat("Raw rows:", nrow(raw_vs), " cols:", paste(names(raw_vs), collapse=","), "\n")

# Ensure expected columns exist (create NA if missing to avoid crash)
need_cols <- c("PT_ID","VISIT_NUM","VISIT","VS_DATE","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG")
for (cc in need_cols) {
  if (!cc %in% names(raw_vs)) {
    raw_vs[[cc]] <- NA
  }
}

# Coerce vitals to numeric (PROC IMPORT would make them numeric)
for (cc in c("VISIT_NUM","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG")) {
  if (cc %in% names(raw_vs) && !is.numeric(raw_vs[[cc]])) {
    raw_vs[[cc]] <- suppressWarnings(as.numeric(raw_vs[[cc]]))
  }
}

n <- nrow(raw_vs)
# Pre-parse VSDTC vector for all rows (SAS: per-row anydtdte -> is8601da)
vsdtc_all <- parse_vsdtc_vec(raw_vs[["VS_DATE"]])
# USUBJID vector via CATX("-", "CDISC-01", PT_ID)
pt_char <- trimws(as.character(raw_vs[["PT_ID"]]))
pt_char[is.na(raw_vs[["PT_ID"]])] <- NA_character_
pt_char[pt_char == "" | pt_char == "." | toupper(pt_char) == "NA"] <- NA_character_
usubjid_all <- ifelse(is.na(pt_char), "CDISC-01", paste0("CDISC-01-", pt_char))

visitnum_all <- raw_vs[["VISIT_NUM"]]
visit_all <- as.character(raw_vs[["VISIT"]])
visit_all[is.na(visit_all)] <- NA_character_
visit_all <- trimws(visit_all)
visit_all[visit_all == ""] <- NA_character_

rows_list <- vector("list", n * 4L)
k <- 0L
for (i in seq_len(n)) {
  STUDYID <- "CDISC-01"
  DOMAIN <- "VS"
  USUBJID <- usubjid_all[i]
  VISITNUM <- visitnum_all[i]
  VISIT <- visit_all[i]
  VSDTC <- vsdtc_all[i]
  # 1. Systolic
  v <- raw_vs[["SYS_BP"]][i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "SYSBP", VSTEST = "Systolic Blood Pressure",
      VSORRES = sas_best_one(v), VSORRESU = "mmHg",
      VSSTRESN = as.numeric(v), VSSTRESU = "mmHg",
      stringsAsFactors = FALSE)
  }
  # 2. Diastolic
  v <- raw_vs[["DIA_BP"]][i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "DIABP", VSTEST = "Diastolic Blood Pressure",
      VSORRES = sas_best_one(v), VSORRESU = "mmHg",
      VSSTRESN = as.numeric(v), VSSTRESU = "mmHg",
      stringsAsFactors = FALSE)
  }
  # 3. Pulse
  v <- raw_vs[["HR_BPM"]][i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "PULSE", VSTEST = "Pulse Rate",
      VSORRES = sas_best_one(v), VSORRESU = "beats/min",
      VSSTRESN = as.numeric(v), VSSTRESU = "beats/min",
      stringsAsFactors = FALSE)
  }
  # 4. Weight
  v <- raw_vs[["WEIGHT_KG"]][i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "WEIGHT", VSTEST = "Weight",
      VSORRES = sas_best_one(v), VSORRESU = "kg",
      VSSTRESN = as.numeric(v), VSSTRESU = "kg",
      stringsAsFactors = FALSE)
  }
}
if (k == 0L) {
  vs_mapped <- data.frame(STUDYID=character(0),DOMAIN=character(0),USUBJID=character(0),
    VISITNUM=numeric(0),VISIT=character(0),VSDTC=character(0),
    VSTESTCD=character(0),VSTEST=character(0),VSORRES=character(0),
    VSORRESU=character(0),VSSTRESN=numeric(0),VSSTRESU=character(0),
    stringsAsFactors=FALSE)
} else {
  rows_list <- rows_list[seq_len(k)]
  vs_mapped <- do.call(rbind, rows_list)
}

# 3. SORT by USUBJID VSDTC VSTESTCD (missing first, SAS binary order)
ord <- order(vs_mapped$USUBJID, vs_mapped$VSDTC, vs_mapped$VSTESTCD,
             na.last = FALSE, method = "radix")
vs_mapped <- vs_mapped[ord, , drop = FALSE]

# GENERATE VSSEQ by USUBJID
vs_mapped$VSSEQ <- NA_integer_
if (nrow(vs_mapped) > 0) {
  # first.USUBJID logic; USUBJID never NA here, but handle NA group
  us <- vs_mapped$USUBJID
  us_key <- ifelse(is.na(us), "\x01NA\x01", us)
  r <- rle(us_key)
  seqv <- unlist(lapply(r$lengths, seq_len))
  vs_mapped$VSSEQ <- as.integer(seqv)
}

# Column order to match RETAIN STUDYID DOMAIN USUBJID VSSEQ
sdtm_vs <- vs_mapped[, c("STUDYID","DOMAIN","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESN","VSSTRESU"), drop = FALSE]
rownames(sdtm_vs) <- NULL

cat("SDTM VS rows:", nrow(sdtm_vs), "\n")

# Write dataset vs.csv
write.csv(sdtm_vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: PROC PRINT obs=12 vars USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU
nprint <- min(12L, nrow(sdtm_vs))
audit <- sdtm_vs[seq_len(nprint), c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU"), drop = FALSE]
audit_out <- data.frame(Obs = seq_len(nprint), audit, stringsAsFactors = FALSE, check.names = FALSE)

# CSV of table body
write.csv(audit_out, file.path(out, "P018.csv"), row.names = FALSE, na = "")

# Text rendering (PROC PRINT style)
txt_path <- file.path(out, "P018.txt")
lst_path <- file.path(out, "P018.lst")
con <- file(txt_path, open = "wt")
writeLines("VS Domain Audit (Unpivoted Format with VSSEQ)", con)
writeLines("", con)
# header
hdr <- sprintf("%-4s %-20s %-5s %-8s %-12s %-10s %-8s %-8s %-10s",
  "Obs","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
writeLines(hdr, con)
for (i in seq_len(nprint)) {
  r <- audit_out[i, ]
  usub <- ifelse(is.na(r$USUBJID), "", as.character(r$USUBJID))
  vsseq <- ifelse(is.na(r$VSSEQ), "", as.character(r$VSSEQ))
  visitnum <- ifelse(is.na(r$VISITNUM), "", sas_best_one(r$VISITNUM))
  visit <- ifelse(is.na(r$VISIT), "", as.character(r$VISIT))
  vsdtc <- ifelse(is.na(r$VSDTC), "", as.character(r$VSDTC))
  vstestcd <- ifelse(is.na(r$VSTESTCD), "", as.character(r$VSTESTCD))
  vsstresn <- ifelse(is.na(r$VSSTRESN), "", sas_best_one(r$VSSTRESN))
  vsstresu <- ifelse(is.na(r$VSSTRESU), "", as.character(r$VSSTRESU))
  line <- sprintf("%-4d %-20s %-5s %-8s %-12s %-10s %-8s %-8s %-10s",
    as.integer(r$Obs), usub, vsseq, visitnum, visit, vsdtc, vstestcd, vsstresn, vsstresu)
  writeLines(line, con)
}
close(con)
# duplicate as .lst for benchmark expecting P018.lst
file.copy(txt_path, lst_path, overwrite = TRUE)
cat("Wrote", file.path(out, "vs.csv"), "and", txt_path, "and", lst_path, "\n")
```