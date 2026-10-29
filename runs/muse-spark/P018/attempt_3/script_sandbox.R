out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
raw_candidates <- c(
  paste0(project_path, "/data/raw/raw_vitals.csv"),
  "/bench/data/derived/c2c/data/raw/raw_vitals.csv"
)
raw_path <- raw_candidates[file.exists(raw_candidates)][1]
if (is.na(raw_path)) raw_path <- raw_candidates[1]

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

month_num <- function(m) {
  m <- toupper(trimws(m))
  m3 <- substr(m, 1, 3)
  map <- c(JAN=1L,FEB=2L,MAR=3L,APR=4L,MAY=5L,JUN=6L,JUL=7L,AUG=8L,SEP=9L,OCT=10L,NOV=11L,DEC=12L)
  if (m3 %in% names(map)) return(unname(map[m3]))
  NA_integer_
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
  if (toupper(s) %in% c("NA","NULL","MISSING","NAN","NAT")) return(NA_character_)
  # pure SAS date number stored as string
  if (grepl("^[0-9]+(\\.[0-9]+)?$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num)) {
      d <- as.Date(num, origin = "1960-01-01")
      if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    }
    return(NA_character_)
  }
  # DATE9 style: 15JAN2023, 15-JAN-2023, 15 Jan 2023, 15-Jan-23 etc.
  m1 <- regmatches(s, regexec("^([0-9]{1,2})[\\-\\/\\.\\s]*([A-Za-z]{3,9})[\\-\\/\\.\\s]*([0-9]{2,4})$", s))[[1]]
  if (length(m1) == 4 && !is.na(m1[1])) {
    dd <- suppressWarnings(as.integer(m1[2]))
    mm <- month_num(m1[3])
    yy <- suppressWarnings(as.integer(m1[4]))
    if (!is.na(dd) && !is.na(mm) && !is.na(yy)) {
      if (yy < 100) {
        # SAS YEARCUTOFF=1940 default: 00-39 -> 2000s, 40-99 -> 1900s
        yy <- ifelse(yy <= 39, 2000L + yy, 1900L + yy)
      }
      d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd)))
      if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    }
  }
  # Month-first: JAN15,2023 / January 15, 2023 / Jan-15-2023
  m2 <- regmatches(s, regexec("^([A-Za-z]{3,9})[\\-\\/\\.\\s]*([0-9]{1,2})[\\,\\s\\-\\/\\.]+([0-9]{2,4})(\\s+.*)?$", s))[[1]]
  if (length(m2) >= 4 && !is.na(m2[1])) {
    mm <- month_num(m2[2])
    dd <- suppressWarnings(as.integer(m2[3]))
    yy <- suppressWarnings(as.integer(m2[4]))
    if (!is.na(dd) && !is.na(mm) && !is.na(yy)) {
      if (yy < 100) yy <- ifelse(yy <= 39, 2000L + yy, 1900L + yy)
      d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd)))
      if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    }
  }
  # lubridate flexible parse (valid orders only, lowercase d/m, Y/y, b/B)
  orders <- c("Ymd","ymd","mdY","mdy","dmY","dmy",
              "dbY","dby","bdY","bdy","BdY","Bdy",
              "Ymd HMS","ymd HMS","mdY HMS","mdy HMS","dmY HMS","dmy HMS",
              "dbY HMS","dby HMS","bdY HMS","bdy HMS",
              "Ymd HM","ymd HM","mdY HM","mdy HM","dmY HM","dmy HM",
              "dbY HM","dby HM","bdY HM","bdy HM",
              "Ymd HMS p","ymd HMS p","mdY HMS p","mdy HMS p",
              "dbY HMS p","dby HMS p","bdY HMS p","bdy HMS p",
              "Ymd HM p","ymd HM p","mdY HM p","mdy HM p",
              "dbY HM p","dby HM p","bdY HM p","bdy HM p")
  dt <- tryCatch(
    suppressWarnings(lubridate::parse_date_time(s, orders = orders, quiet = TRUE, truncated = 0)),
    error = function(e) as.POSIXct(NA)
  )
  if (length(dt) == 1 && !is.na(dt)) {
    return(format(as.Date(dt, tz = "UTC"), "%Y-%m-%d"))
  }
  d2 <- suppressWarnings(as.Date(s, tryFormats = c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y",
    "%m-%d-%Y","%d-%m-%Y","%Y.%m.%d","%m.%d.%Y","%d.%m.%Y","%Y%m%d",
    "%d-%b-%Y","%d-%B-%Y","%b %d %Y","%B %d %Y","%d %b %Y","%d %B %Y",
    "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%m/%d/%Y %H:%M","%Y/%m/%d %H:%M",
    "%d%b%Y","%d%B%Y","%b%d%Y","%B%d%Y","%Y%b%d","%Y%B%d")))
  if (length(d2) == 1 && !is.na(d2)) {
    return(format(d2, "%Y-%m-%d"))
  }
  NA_character_
}
parse_vsdtc_vec <- function(x) {
  vapply(seq_along(x), function(i) parse_vsdtc_one(x[i]), character(1), USE.NAMES = FALSE)
}

cat("Reading raw vitals:", raw_path, "\n")
# Read as character to preserve original strings like SAS PROC IMPORT sees them
raw_chr <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                           show_col_types = FALSE, progress = FALSE)
raw_chr <- as.data.frame(raw_chr, stringsAsFactors = FALSE)
names(raw_chr) <- toupper(names(raw_chr))
cat("Raw rows:", nrow(raw_chr), " cols:", paste(names(raw_chr), collapse=","), "\n")

need_cols <- c("PT_ID","VISIT_NUM","VISIT","VS_DATE","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG")
for (cc in need_cols) {
  if (!cc %in% names(raw_chr)) raw_chr[[cc]] <- NA_character_
}

n <- nrow(raw_chr)
vsdtc_all <- parse_vsdtc_vec(raw_chr[["VS_DATE"]])

pt_raw <- trimws(as.character(raw_chr[["PT_ID"]]))
pt_raw[is.na(raw_chr[["PT_ID"]])] <- NA_character_
pt_raw[pt_raw == "" | pt_raw == "."] <- NA_character_
# CATX skips missing; numeric-looking PT_ID: strip leading zeros? No - keep as SAS would.
# SAS PROC IMPORT: if PT_ID looks numeric it becomes numeric then CATX uses BEST. (strips leading zeros).
# Detect: if string matches numeric, normalize via BEST. (as.numeric -> format)
pt_norm <- vapply(seq_along(pt_raw), function(i) {
  p <- pt_raw[i]
  if (is.na(p)) return(NA_character_)
  if (grepl("^[0-9]+$", p)) {
    # could be character "001" vs numeric 1: check if has leading zero
    # SAS numeric conversion would strip leading zeros; character would keep.
    # Heuristic: if all digits and has leading zero and length>1, keep? But PROC IMPORT
    # would make it numeric, so strip. Previous run matched USUBJID, so strip.
    num <- suppressWarnings(as.numeric(p))
    if (!is.na(num)) return(sas_best_one(num))
  }
  p
}, character(1), USE.NAMES = FALSE)
usubjid_all <- ifelse(is.na(pt_norm), "CDISC-01", paste0("CDISC-01-", pt_norm))

visitnum_all <- suppressWarnings(as.numeric(raw_chr[["VISIT_NUM"]]))
visit_all <- as.character(raw_chr[["VISIT"]])
visit_all[is.na(visit_all)] <- NA_character_
visit_all <- trimws(visit_all)
visit_all[visit_all == ""] <- NA_character_

sys_all <- suppressWarnings(as.numeric(raw_chr[["SYS_BP"]]))
dia_all <- suppressWarnings(as.numeric(raw_chr[["DIA_BP"]]))
hr_all  <- suppressWarnings(as.numeric(raw_chr[["HR_BPM"]]))
wt_all  <- suppressWarnings(as.numeric(raw_chr[["WEIGHT_KG"]]))

rows_list <- vector("list", max(1L, n * 4L))
k <- 0L
for (i in seq_len(n)) {
  STUDYID <- "CDISC-01"
  DOMAIN <- "VS"
  USUBJID <- usubjid_all[i]
  VISITNUM <- visitnum_all[i]
  VISIT <- visit_all[i]
  VSDTC <- vsdtc_all[i]
  v <- sys_all[i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "SYSBP", VSTEST = "Systolic Blood Pressure",
      VSORRES = sas_best_one(v), VSORRESU = "mmHg",
      VSSTRESN = v, VSSTRESU = "mmHg",
      stringsAsFactors = FALSE)
  }
  v <- dia_all[i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "DIABP", VSTEST = "Diastolic Blood Pressure",
      VSORRES = sas_best_one(v), VSORRESU = "mmHg",
      VSSTRESN = v, VSSTRESU = "mmHg",
      stringsAsFactors = FALSE)
  }
  v <- hr_all[i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "PULSE", VSTEST = "Pulse Rate",
      VSORRES = sas_best_one(v), VSORRESU = "beats/min",
      VSSTRESN = v, VSSTRESU = "beats/min",
      stringsAsFactors = FALSE)
  }
  v <- wt_all[i]
  if (!is.na(v)) {
    k <- k + 1L
    rows_list[[k]] <- data.frame(
      STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
      VISITNUM = VISITNUM, VISIT = VISIT, VSDTC = VSDTC,
      VSTESTCD = "WEIGHT", VSTEST = "Weight",
      VSORRES = sas_best_one(v), VSORRESU = "kg",
      VSSTRESN = v, VSSTRESU = "kg",
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
