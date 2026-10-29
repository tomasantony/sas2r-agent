# P014_sdtm_ex.sas -> R translation
# SDTM EX domain mapping

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_exposure.csv")

# fallback search if not found
if (!file.exists(raw_path)) {
  cand1 <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv"
  cand2 <- file.path("D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw", "raw_exposure.csv")
  for (cc in c(cand1, cand2)) {
    if (file.exists(cc)) { raw_path <- cc; break }
  }
}
if (!file.exists(raw_path)) {
  # last resort: list files
  alt <- list.files(project_path, pattern = "raw_exposure\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(alt) > 0) raw_path <- alt[1]
}
cat("Reading:", raw_path, "\n")

# 1. IMPORT RAW EXPOSURE DATA (PROC IMPORT CSV GETNAMES=YES)
# Read everything as character to mimic flexible SAS import then convert
raw_ex <- suppressMessages(readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("NA"),
  trim_ws = TRUE,
  guess_max = 10000,
  show_col_types = FALSE))
names(raw_ex) <- toupper(names(raw_ex))
# SAS validvarname=upcase already applied; trim whitespace values are trimmed by readr
# Convert empty strings to NA (SAS missing)
for (nm in names(raw_ex)) {
  v <- raw_ex[[nm]]
  v[trimws(as.character(v)) == ""] <- NA_character_
  raw_ex[[nm]] <- v
}
cat("Raw rows:", nrow(raw_ex), " cols:", paste(names(raw_ex), collapse=","), "\n")

# Helper: SAS ANYDTDTE. -> Date, then IS8601DA. -> YYYY-MM-DD
parse_anydtdte_one <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  s <- trimws(as.character(s))
  if (is.na(s) || s == "" || toupper(s) %in% c(".", "NA", "NULL", "MISSING", ".")) return(as.Date(NA))
  # Pure integer string handling
  is_int <- grepl("^[+-]?[0-9]+$", s)
  if (is_int) {
    # 8-digit YYYYMMDD starting 19/20 -> parse as ymd, not SAS date
    if (grepl("^(19|20)[0-9]{6}$", s)) {
      d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
      if (!is.na(d)) return(as.Date(d))
    } else {
      num <- suppressWarnings(as.numeric(s))
      if (!is.na(num) && num >= 5000 && num <= 50000) {
        # plausible SAS date value (days since 1960-01-01)
        return(as.Date(num, origin = "1960-01-01"))
      }
      # small integers are not dates -> fall through to try parsing (will be NA)
    }
  }
  # Try lubridate flexible parse
  d2 <- suppressWarnings(lubridate::parse_date_time(s,
    orders = c("ymd", "Ymd", "mdy", "dmy", "bdy", "dby",
               "ymd HMS", "mdy HMS", "dmy HMS", "bdy HMS", "dby HMS",
               "ymd HM", "mdy HM", "dmy HM", "bdy HM", "dby HM",
               "ymd H", "mdy H", "dmy H",
               "Ymd HMS", "Ymd HM", "dbY", "bdY", "Ybd", "dBY"),
    quiet = TRUE, truncated = 3, tz = "UTC"))
  if (length(d2) == 1 && !is.na(d2)) return(as.Date(d2, tz = "UTC"))
  # Fallback tryFormats with as.Date / as.POSIXct
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y", "%m/%d/%y",
            "%d%b%Y", "%d-%b-%Y", "%d-%b-%y", "%d%b%y", "%d %b %Y", "%d %b %y",
            "%b %d, %Y", "%b %d %Y", "%B %d, %Y", "%Y%m%d",
            "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M",
            "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%d/%m/%Y",
            "%Y-%m-%d %H:%M", "%d-%b-%Y %H:%M:%S")
  for (f in fmts) {
    dd <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(dd)) return(dd)
  }
  # try datetime then date
  for (f in c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S")) {
    pp <- suppressWarnings(as.POSIXct(s, format = f, tz = "UTC"))
    if (!is.na(pp)) return(as.Date(pp, tz = "UTC"))
  }
  # last resort: if numeric string in SAS range, treat as SAS date
  num2 <- suppressWarnings(as.numeric(s))
  if (!is.na(num2) && num2 >= 5000 && num2 <= 50000) {
    return(as.Date(num2, origin = "1960-01-01"))
  }
  return(as.Date(NA))
}
parse_anydtdte <- function(x) {
  # x character vector
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    # handle NA directly
    if (is.na(x[i])) next
    res[i] <- parse_anydtdte_one(x[i])
  }
  res
}

# 2. CORE TRANSFORMATION (work.ex_mapped)
# Expected columns: PATIENT TREATMENT DOSE UNIT START_DATE END_DATE
# Be tolerant to alternative names
get_col <- function(df, candidates) {
  for (c in candidates) if (c %in% names(df)) return(df[[c]])
  return(rep(NA_character_, nrow(df)))
}
PATIENT    <- get_col(raw_ex, c("PATIENT", "SUBJID", "SUBJECT", "PT"))
TREATMENT  <- get_col(raw_ex, c("TREATMENT", "TRT", "EXTRT"))
DOSE       <- get_col(raw_ex, c("DOSE", "EXDOSE"))
UNIT       <- get_col(raw_ex, c("UNIT", "EXDOSU", "DOSU"))
START_DATE <- get_col(raw_ex, c("START_DATE", "STARTDATE", "EXSTDTC", "START"))
END_DATE   <- get_col(raw_ex, c("END_DATE", "ENDDATE", "EXENDTC", "END"))

n <- nrow(raw_ex)
STUDYID <- rep("CDISC-01", n)
DOMAIN  <- rep("EX", n)

# USUBJID = catx("-", STUDYID, PATIENT) : strip + skip missing
pat_trim <- trimws(as.character(PATIENT))
pat_trim[is.na(pat_trim)] <- ""
pat_trim[pat_trim == ""] <- NA_character_
USUBJID <- ifelse(is.na(pat_trim), "CDISC-01", paste0("CDISC-01-", pat_trim))

# EXTRT = strip(TREATMENT)
EXTRT <- trimws(as.character(TREATMENT))
EXTRT[EXTRT == ""] <- NA_character_

# EXDOSE = DOSE (numeric)
EXDOSE <- suppressWarnings(as.numeric(as.character(DOSE)))

# EXDOSU = strip(UNIT)
EXDOSU <- trimws(as.character(UNIT))
EXDOSU[EXDOSU == ""] <- NA_character_

# ISO 8601 DATE CONVERSION
# SAS: if vtype character then input(anydtdte.) else numeric SAS date
# Since we read as character, parse_anydtdte handles both; but also handle
# case where original CSV had true numerics (read as character numbers -> SAS-date logic above)
start_dt <- parse_anydtdte(as.character(START_DATE))
end_dt   <- parse_anydtdte(as.character(END_DATE))

EXSTDTC <- format(start_dt, "%Y-%m-%d")
EXENDTC <- format(end_dt, "%Y-%m-%d")
# SAS missing -> "" ; keep NA for R then write na="" ; but for sorting need NA first
# EXSTDTC/EXENDTC already NA where missing

ex_mapped <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  EXTRT = EXTRT,
  EXDOSE = EXDOSE,
  EXDOSU = EXDOSU,
  EXSTDTC = EXSTDTC,
  EXENDTC = EXENDTC,
  stringsAsFactors = FALSE
)

# 3. GENERATE SEQUENCE NUMBER (EXSEQ)
# proc sort by USUBJID EXSTDTC (SAS: missing first, ASCII uppercase first)
ord <- order(ex_mapped$USUBJID, ex_mapped$EXSTDTC, na.last = FALSE, method = "radix")
ex_mapped <- ex_mapped[ord, , drop = FALSE]

# by USUBJID: if first then 1 else +1
EXSEQ <- integer(nrow(ex_mapped))
if (nrow(ex_mapped) > 0) {
  cur_id <- NA_character_
  cnt <- 0L
  for (i in seq_len(nrow(ex_mapped))) {
    id <- ex_mapped$USUBJID[i]
    if (is.na(id)) id <- "<NA>"
    if (is.na(cur_id) || id != cur_id) {
      cnt <- 1L
      cur_id <- id
    } else {
      cnt <- cnt + 1L
    }
    EXSEQ[i] <- cnt
  }
}
ex_mapped$EXSEQ <- EXSEQ

# retain STUDYID DOMAIN USUBJID EXSEQ -> column order
sdtm_ex <- ex_mapped[, c("STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")]
rownames(sdtm_ex) <- NULL

# Write dataset ex
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: proc print obs=10 var USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC
audit <- head(sdtm_ex[, c("USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")], 10)

# CSV of table body
write.csv(audit, file.path(out, "P014.csv"), row.names = FALSE, na = "")
write.csv(audit, file.path(out, "P014.lst.csv"), row.names = FALSE, na = "")

# Plain-text rendering (P014.lst -> P014.txt)
# Build fixed-width text similar to SAS PROC PRINT
fmt_num <- function(x) ifelse(is.na(x), "", as.character(x))
audit_txt <- audit
for (cc in names(audit_txt)) {
  audit_txt[[cc]][is.na(audit_txt[[cc]])] <- ""
}
# Determine widths
headers <- c("Obs", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")
# Build rows
lines <- c()
lines <- c(lines, "EX Domain Audit (Study Drug Exposure)")
lines <- c(lines, "")
# header line
hdr_line <- sprintf("%-4s %-18s %-5s %-20s %-8s %-10s %-10s %-10s",
  "Obs", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC")
lines <- c(lines, hdr_line)
lines <- c(lines, paste(rep("-", nchar(hdr_line)), collapse = ""))
for (i in seq_len(nrow(audit_txt))) {
  r <- audit_txt[i, , drop = FALSE]
  exdose_s <- ifelse(is.na(audit$EXDOSE[i]), "", as.character(audit$EXDOSE[i]))
  line <- sprintf("%-4d %-18s %-5s %-20s %-8s %-10s %-10s %-10s",
    i,
    as.character(r$USUBJID),
    as.character(r$EXSEQ),
    as.character(r$EXTRT),
    exdose_s,
    as.character(r$EXDOSU),
    as.character(r$EXSTDTC),
    as.character(r$EXENDTC))
  lines <- c(lines, line)
}
lines <- c(lines, "")
lines <- c(lines, sprintf("N = %d (first %d observations shown of %d in EX domain)", nrow(audit), nrow(audit), nrow(sdtm_ex)))

txt_path1 <- file.path(out, "P014.txt")
txt_path2 <- file.path(out, "P014.lst.txt")
txt_path3 <- file.path(out, "P014.lst")
writeLines(lines, txt_path1)
writeLines(lines, txt_path2)
writeLines(lines, txt_path3)

cat("Done. EX rows:", nrow(sdtm_ex), " audit rows:", nrow(audit), "\n")
