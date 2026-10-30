```r
out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv"
message("Reading: ", raw_path)
stopifnot(file.exists(raw_path))

# PROC IMPORT with GETNAMES=YES, GUESSINGROWS=MAX, VALIDVARNAME=UPCASE
# Read everything as character to preserve raw strings; emulate SAS single-informat per column
raw_ae <- readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c(""),
  trim_ws = FALSE,
  progress = FALSE,
  show_col_types = FALSE)
names(raw_ae) <- toupper(names(raw_ae))
message(paste0("raw_ae rows=", nrow(raw_ae), " cols=", paste(names(raw_ae), collapse=",")))

stopifnot(all(c("ID","AE_TERM","SEV","SERIOUS","RELATED","START","END") %in% names(raw_ae)))

# ---- SAS ANYDTDTE emulation: ONE strict format per column ----
# PROC IMPORT decides once per column: n/n/yyyy -> DDMMYY10. if any first field >12 else MMDDYY10.
parse_sas_date_col <- function(x_raw) {
  x_chr <- as.character(x_raw)
  x_trim <- trimws(x_chr)
  x_trim[x_trim == ""] <- NA_character_
  # as.character(NA) stays NA; trimws(NA) is NA
  n <- length(x_trim)
  nonmiss <- x_trim[!is.na(x_trim)]
  if (length(nonmiss) == 0) return(rep(as.Date(NA), n))
  # pure integer strings -> SAS numeric dates (days since 1960-01-01)
  if (all(grepl("^\\d+$", nonmiss))) {
    # heuristic: if values look like SAS date numbers (e.g. 15000-25000) treat as numeric
    # otherwise they are still parsed as numbers; keep conversion
    nums <- suppressWarnings(as.numeric(x_trim))
    # SAS dates for plausible AE dates: 1960-2030 => 0-26000ish; if numbers larger (e.g. IDs) this branch
    # only called for START/END so assume SAS dates
    return(as.Date(nums, origin = "1960-01-01"))
  }
  fmt <- NULL
  if (all(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", nonmiss))) {
    ff <- suppressWarnings(as.integer(sub("/.*$", "", nonmiss)))
    base <- if (any(ff > 12, na.rm = TRUE)) "%d/%m/%Y" else "%m/%d/%Y"
    yr <- sub("^\\d{1,2}/\\d{1,2}/", "", nonmiss)
    if (all(nchar(yr) == 2)) base <- sub("%Y$", "%y", base)
    fmt <- base
  } else if (all(grepl("^\\d{1,2}-\\d{1,2}-\\d{2,4}$", nonmiss))) {
    ff <- suppressWarnings(as.integer(sub("-.*$", "", nonmiss)))
    base <- if (any(ff > 12, na.rm = TRUE)) "%d-%m-%Y" else "%m-%d-%Y"
    yr <- sub("^\\d{1,2}-\\d{1,2}-", "", nonmiss)
    if (all(nchar(yr) == 2)) base <- sub("%Y$", "%y", base)
    fmt <- base
  } else if (all(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}([T ].*)?$", nonmiss))) {
    fmt <- "%Y-%m-%d"
  } else if (all(grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2,4}([ ].*)?$", nonmiss))) {
    yr <- sub("^\\d{1,2}-[A-Za-z]{3}-", "", nonmiss)
    yr <- sub(" .*", "", yr)
    if (all(nchar(yr) == 2)) fmt <- "%d-%b-%y" else fmt <- "%d-%b-%Y"
  } else if (all(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}([ ].*)?$", nonmiss))) {
    ff <- suppressWarnings(as.integer(sub("/.*$", "", nonmiss)))
    fmt <- if (any(ff > 12, na.rm = TRUE)) "%d/%m/%Y" else "%m/%d/%Y"
  } else {
    if (any(grepl("/", nonmiss, fixed = TRUE))) {
      mnum <- regmatches(nonmiss, regexpr("^\\d+", nonmiss))
      ff <- suppressWarnings(as.integer(mnum))
      if (any(ff > 12, na.rm = TRUE)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
    } else if (any(grepl("[A-Za-z]", nonmiss))) {
      fmt <- "%d-%b-%Y"
    } else if (all(grepl("^\\d{4}-", nonmiss))) {
      fmt <- "%Y-%m-%d"
    } else {
      fmt <- "%Y-%m-%d"
    }
  }
  message(paste0("date fmt chosen: ", fmt))
  res <- rep(as.Date(NA), n)
  for (i in seq_along(x_trim)) {
    s <- x_trim[i]
    if (is.na(s)) next
    ss <- s
    if (grepl("%Y-%m-%d", fmt, fixed = TRUE) && grepl("^\\d{4}-\\d{1,2}-\\d{1,2}[T ]", ss)) {
      ss <- regmatches(ss, regexpr("^\\d{4}-\\d{1,2}-\\d{1,2}", ss))
    }
    if (grepl("%b", fmt, fixed = TRUE)) {
      # strip time part
      if (grepl(" ", ss, fixed = TRUE)) ss <- strsplit(ss, " ", fixed = TRUE)[[1]][1]
      if (grepl("T", ss, fixed = TRUE)) ss <- strsplit(ss, "T", fixed = TRUE)[[1]][1]
      mm <- regmatches(ss, regexpr("[A-Za-z]+", ss))
      if (length(mm) > 0) {
        mm_title <- paste0(toupper(substr(mm, 1, 1)), tolower(substr(mm, 2, nchar(mm))))
        ss <- sub("[A-Za-z]+", mm_title, ss)
      }
    } else {
      if (grepl(" ", ss, fixed = TRUE)) ss <- strsplit(ss, " ", fixed = TRUE)[[1]][1]
      # split ISO datetime T
      if (grepl("T", ss, fixed = TRUE) && grepl("^\\d", ss)) {
        # only split if date-like prefix
        ss <- strsplit(ss, "T", fixed = TRUE)[[1]][1]
      }
    }
    res[i] <- suppressWarnings(as.Date(ss, format = fmt))
  }
  res
}

sas_strip <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}
sas_upcase_strip <- function(x) {
  toupper(sas_strip(x))
}

# ---- 2. CORE TRANSFORMATION & DERIVATION ----
ID_chr      <- as.character(raw_ae[["ID"]])
AE_TERM_chr <- as.character(raw_ae[["AE_TERM"]])
SEV_chr     <- as.character(raw_ae[["SEV"]])
SERIOUS_chr <- as.character(raw_ae[["SERIOUS"]])
RELATED_chr <- as.character(raw_ae[["RELATED"]])
START_chr   <- as.character(raw_ae[["START"]])
END_chr     <- as.character(raw_ae[["END"]])

# vtype(START) branch emulation: single-format parse per column (PROC IMPORT guessingrows=max)
start_date <- parse_sas_date_col(START_chr)
end_date   <- parse_sas_date_col(END_chr)

AESTDTC_chr <- format(start_date, "%Y-%m-%d")
AEENDTC_chr <- format(end_date, "%Y-%m-%d")
# format(NA) -> NA ; SAS blank -> NA (written as "")

STUDYID <- rep("CDISC-01", nrow(raw_ae))
DOMAIN  <- rep("AE", nrow(raw_ae))
# USUBJID = catx("-", STUDYID, ID)
ID_stripped <- sas_strip(ID_chr)
ID_stripped[ID_stripped == ""] <- NA_character_
USUBJID <- ifelse(is.na(ID_stripped), STUDYID, paste0(STUDYID, "-", ID_stripped))

AETERM <- sas_strip(AE_TERM_chr)
AEDECOD <- toupper(AETERM)
AESEV <- sas_upcase_strip(SEV_chr)

SER_up <- sas_upcase_strip(SERIOUS_chr)
AESER <- ifelse(SER_up == "Y", "Y", "N")

REL_up <- sas_upcase_strip(RELATED_chr)
AEREL <- ifelse(REL_up == "Y", "RELATED",
         ifelse(REL_up == "N", "NOT RELATED", "UNKNOWN"))

AEOUT <- ifelse(!is.na(end_date), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")
# AEENDTC blank when missing (already NA)
# AESTDTC blank when missing (already NA)

# LENGTH truncation to prevent SAS truncation differences
substr_na <- function(x, len) {
  # SAS length: truncate, keep NA as NA (blank)
  v <- as.character(x)
  isna <- is.na(v)
  v[!isna] <- substr(v[!isna], 1, len)
  # SAS "" vs NA: convert "" to NA for writing? keep "" as "" -> write as blank either way
  # normalize "" to NA for consistency (write.csv na="")
  v[v == ""] <- NA_character_
  v
}
STUDYID <- substr_na(STUDYID, 8)
DOMAIN  <- substr_na(DOMAIN, 2)
USUBJID <- substr_na(USUBJID, 40)
AETERM  <- substr_na(AETERM, 60)
AEDECOD <- substr_na(AEDECOD, 60)
AESEV   <- substr_na(AESEV, 10)
AESER   <- substr_na(AESER, 1)
AEREL   <- substr_na(AEREL, 15)
AEOUT   <- substr_na(AEOUT, 30)
# AESTDTC/AEENDTC already ISO or NA
AESTDTC_chr[AESTDTC_chr == ""] <- NA_character_
AEENDTC_chr[AEENDTC_chr == ""] <- NA_character_

ae_mapped <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  AETERM = AETERM, AEDECOD = AEDECOD, AESEV = AESEV, AESER = AESER, AEREL = AEREL,
  AESTDTC = AESTDTC_chr, AEENDTC = AEENDTC_chr, AEOUT = AEOUT,
  stringsAsFactors = FALSE
)
# keep order as in SAS KEEP
ae_mapped <- ae_mapped[, c("STUDYID","DOMAIN","USUBJID","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]
ae_mapped$`_orig` <- seq_len(nrow(ae_mapped))

# ---- 3. SORT by USUBJID AESTDTC (SAS: missing first, uppercase before lowercase) ----
# AESTDTC lexical ISO == chronological; missing first via na.last=FALSE; stable via _orig
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, ae_mapped$`_orig`, na.last = FALSE)
ae_sorted <- ae_mapped[ord, , drop = FALSE]

# BY USUBJID: AESEQ
ae_sorted$AESEQ <- ave(seq_len(nrow(ae_sorted)), ae_sorted$USUBJID, FUN = function(x) seq_along(x))
# retain order: STUDYID DOMAIN USUBJID AESEQ ...
sdtm_ae <- ae_sorted[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]
# ensure sorted order retained
rownames(sdtm_ae) <- NULL
stopifnot(all(c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT") %in% names(sdtm_ae)))
stopifnot(nrow(sdtm_ae) > 0)

# ---- write dataset ae ----
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")
message(paste0("ae rows=", nrow(sdtm_ae)))

# ---- 4. VISUAL AUDIT: PROC PRINT obs=12 ----
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit_df <- head(sdtm_ae[, audit_vars, drop = FALSE], 12)
stopifnot(all(audit_vars %in% names(sdtm_ae)))

# CSV of table body
write.csv(audit_df, file.path(out, "P009.csv"), row.names = FALSE, na = "")

# Text rendering (PROC PRINT default with Obs column + title)
title_txt <- "AE Domain Audit (Adverse Events with Ongoing Logic)"
# Build fixed text similar to SAS listing
# Use capture.output of print.data.frame for alignment
print_df <- data.frame(Obs = seq_len(nrow(audit_df)), audit_df, stringsAsFactors = FALSE, check.names = FALSE)
txt_body <- capture.output(print(print_df, right = FALSE, na.print = "", row.names = FALSE))
txt_lines <- c(title_txt, "", txt_body, "", paste0("N = ", nrow(audit_df), " rows shown (first 12 of ", nrow(sdtm_ae), ")"))
writeLines(txt_lines, file.path(out, "P009.txt"))
writeLines(txt_lines, file.path(out, "P009.lst"))
message("listing written")
```