out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
# In sandbox the bench is mounted at /bench; fall back if D: path missing
raw_candidates <- c(
  paste0(project_path, "/data/raw/raw_vitals.csv"),
  "/bench/data/derived/c2c/data/raw/raw_vitals.csv"
)
raw_path <- raw_candidates[file.exists(raw_candidates)][1]
if (is.na(raw_path)) raw_path <- project_path

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

sas_best_one <- function(v) {
  if (length(v) == 0) return(NA_character_)
  if (is.na(v)) return(NA_character_)
  vv <- suppressWarnings(as.numeric(v))
  if (!is.na(vv)) v <- vv else return(trimws(as.character(v)))
  if (is.na(v)) return(NA_character_)
  trimws(formatC(v, format = "g", digits = 12))
}

parse_vsdtc_one <- function(v) {
  if (length(v) == 0) return(NA_character_)
  if (length(v) > 1) v <- v[1]
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) {
    if (is.na(v)) return(NA_character_)
    return(format(v, "%Y-%m-%d"))
  }
  if (inherits(v, "POSIXt") || inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
    if (is.na(v)) return(NA_character_)
    return(format(as.Date(v, tz = "UTC"), "%Y-%m-%d"))
  }
  if (is.numeric(v)) {
    if (is.na(v)) return(NA_character_)
    d <- as.Date(v, origin = "1960-01-01")
    if (is.na(d)) return(NA_character_)
    return(format(d, "%Y-%m-%d"))
  }
  s <- trimws(as.character(v))
  if (is.na(s) || s == "" || s == ".") return(NA_character_)
  if (toupper(s) %in% c("NA", "NULL", "MISSING", "NAN", "NAT")) return(NA_character_)
  # try lubridate parse_date_time with safe orders
  dt <- tryCatch(
    suppressWarnings(lubridate::parse_date_time(s,
      orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS",
                 "ymd HM", "mdy HM", "dmy HM", "ymd HMS p", "mdy HMS p", "dmy HMS p"),
      quiet = TRUE, truncated = 0)),
    error = function(e) as.POSIXct(NA)
  )
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
    d3 <- as.Date(num, origin = "1960-01-01")
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

need_cols <- c("PT_ID","VISIT_NUM","VISIT","VS_DATE","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG")
for (cc in need_cols) {
  if (!cc %in% names(raw_vs)) {
    raw_vs[[cc]] <- NA
  }
}

for (cc in c("VISIT_NUM","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG")) {
  if (cc %in% names(raw_vs) && !is.numeric(raw_vs[[cc]])) {
    raw_vs[[cc]] <- suppressWarnings(as.numeric(raw_vs[[cc]]))
  }
}

n <- nrow(raw_vs)
vsdtc_all <- parse_vsdtc_vec(raw_vs[["VS_DATE"]])
pt_char <- trimws(as.character(raw_vs[["PT_ID"]]))
pt_char[is.na(raw_vs[["PT_ID"]])] <- NA_character_
pt_char[pt_char == "" | pt_char == "." | toupper(pt_char) == "NA"] <- NA_character_
usubjid_all <- ifelse(is.na(pt_char), "CDISC-01", paste0("CDISC-01-", pt_char))

visitnum_all <- raw_vs[["VISIT_NUM"]]
visit_all <- as.character(raw_vs[["VISIT"]])
visit_all[is.na(visit_all)] <- NA_character_
visit_all <- trimws(visit_all)
visit_all[visit_all == ""] <- NA_character_

rows_list <- vector("list", max(1L, n * 4L))
k <- 0L
for (i in seq_len(n)) {
  STUDYID <- "CDISC-01"
  DOMAIN <- "VS"
  USUBJID <- usubjid_all[i]
  VISITNUM <- visitnum_all[i]
  VISIT <- visit_all[i]
  VSDTC <- vsdtc_all[i]
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

ord <- order(vs_mapped$USUBJID, vs_mapped$VSDTC, vs_mapped$VSTESTCD,
             na.last = FALSE, method = "radix")
vs_mapped <- vs_mapped[ord, , drop = FALSE]

vs_mapped$VSSEQ <- NA_integer_
if (nrow(vs_mapped) > 0) {
  us <- vs_mapped$USUBJID
  us_key <- ifelse(is.na(us), "\x01NA\x01", us)
  r <- rle(us_key)
  seqv <- unlist(lapply(r$lengths, seq_len))
  vs_mapped$VSSEQ <- as.integer(seqv)
}

sdtm_vs <- vs_mapped[, c("STUDYID","DOMAIN","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESN","VSSTRESU"), drop = FALSE]
rownames(sdtm_vs) <- NULL

cat("SDTM VS rows:", nrow(sdtm_vs), "\n")

write.csv(sdtm_vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

nprint <- min(12L, nrow(sdtm_vs))
audit <- sdtm_vs[seq_len(nprint), c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU"), drop = FALSE]
audit_out <- data.frame(Obs = seq_len(nprint), audit, stringsAsFactors = FALSE, check.names = FALSE)

write.csv(audit_out, file.path(out, "P018.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P018.txt")
lst_path <- file.path(out, "P018.lst")
con <- file(txt_path, open = "wt")
writeLines("VS Domain Audit (Unpivoted Format with VSSEQ)", con)
writeLines("", con)
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
file.copy(txt_path, lst_path, overwrite = TRUE)
cat("Wrote", file.path(out, "vs.csv"), "and", txt_path, "and", lst_path, "\n")
