out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

raw_path <- "/bench/data/derived/c2c/data/raw/raw_exposure.csv"

# --- helpers ---
sas_trunc <- function(x, n) {
  # emulate SAS LENGTH truncation (character)
  is_na <- is.na(x)
  x2 <- substr(as.character(x), 1, n)
  x2[is_na] <- NA_character_
  # SAS keeps "" as missing; substr(NA) gives NA already
  x2
}
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  # CATX strips each, skips missing/empty
  sapply(seq_len(length(args[[1]])), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v), which = "both")
      if (v == "") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}

guess_single_format <- function(vec) {
  v0 <- trimws(as.character(vec))
  v <- v0[!is.na(v0) & v0 != "" & toupper(v0) != "." & toupper(v0) != "NA" & toupper(v0) != "NAN"]
  if (length(v) == 0) return(NA_character_)
  is_slash   <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", v)
  is_iso     <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", v)
  is_dmony   <- grepl("^[0-9]{1,2}-[A-Za-z]{3,9}-[0-9]{2,4}$", v)
  is_ymdsl   <- grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", v)
  is_mdash   <- grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", v)
  if (all(is_slash)) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", v)))
    dmy <- any(!is.na(first_fields) & first_fields > 12)
    year_part <- sub("^.*/", "", v)
    y2 <- all(nchar(year_part) == 2)
    if (dmy) { if (y2) return("%d/%m/%y") else return("%d/%m/%Y") }
    else     { if (y2) return("%m/%d/%y") else return("%m/%d/%Y") }
  } else if (all(is_iso)) {
    return("%Y-%m-%d")
  } else if (all(is_dmony)) {
    year_part <- sub("^.*-", "", v)
    y2 <- all(nchar(year_part) == 2)
    if (y2) return("%d-%b-%y") else return("%d-%b-%Y")
  } else if (all(is_ymdsl)) {
    return("%Y/%m/%d")
  } else if (all(is_mdash)) {
    # e.g. mm-dd-yyyy ; decide once per column like slash
    first_fields <- suppressWarnings(as.integer(sub("-.*$", "", v)))
    dmy <- any(!is.na(first_fields) & first_fields > 12)
    year_part <- sub("^.*-", "", v)
    y2 <- all(nchar(year_part) == 2)
    if (dmy) { if (y2) return("%d-%m-%y") else return("%d-%m-%Y") }
    else     { if (y2) return("%m-%d-%y") else return("%m-%d-%Y") }
  } else {
    # Mixed column: PROC IMPORT would keep as character; emulate single-order choice.
    # Decide once per column from all values (slash rule takes precedence).
    if (any(is_slash)) {
      sl <- v[is_slash]
      first_fields <- suppressWarnings(as.integer(sub("/.*$", "", sl)))
      dmy <- any(!is.na(first_fields) & first_fields > 12)
      if (dmy) return("%d/%m/%Y") else return("%m/%d/%Y")
    } else if (any(is_iso)) {
      return("%Y-%m-%d")
    } else if (any(is_dmony)) {
      return("%d-%b-%Y")
    } else if (any(is_ymdsl)) {
      return("%Y/%m/%d")
    } else {
      return(NA_character_)
    }
  }
}

parse_single_format <- function(vec, fmt) {
  v0 <- as.character(vec)
  # preserve NA; trim
  vtrim <- trimws(v0, which = "both")
  vtrim[vtrim == "" | toupper(vtrim) == "." | toupper(vtrim) == "NA"] <- NA_character_
  if (is.na(fmt) || is.null(fmt) || fmt == "") {
    return(as.Date(rep(NA_character_, length(vtrim))))
  }
  # ONE explicit strict format for the whole column
  as.Date(vtrim, format = fmt)
}

# --- 1. IMPORT RAW EXPOSURE DATA (emulate PROC IMPORT CSV) ---
# Read everything as character to inspect raw strings (GUESSINGROWS scan)
raw_char <- read.csv(raw_path, colClasses = "character", check.names = FALSE,
                     stringsAsFactors = FALSE, na.strings = c(), blank.lines.skip = TRUE)
names(raw_char) <- toupper(trimws(names(raw_char)))
# SAS validvarname=upcase already; ensure expected columns exist
stopifnot("PATIENT" %in% names(raw_char))
stopifnot("TREATMENT" %in% names(raw_char))
stopifnot("DOSE" %in% names(raw_char))
stopifnot("UNIT" %in% names(raw_char))
stopifnot("START_DATE" %in% names(raw_char))
stopifnot("END_DATE" %in% names(raw_char))

# Decide ONE informat per date column after scanning all values (PROC IMPORT GUESSINGROWS logic)
fmt_start <- guess_single_format(raw_char$START_DATE)
fmt_end   <- guess_single_format(raw_char$END_DATE)

# Parse every value with that single format (never per-value choice)
start_dt <- parse_single_format(raw_char$START_DATE, fmt_start)
end_dt   <- parse_single_format(raw_char$END_DATE, fmt_end)

# --- 2. CORE TRANSFORMATION (work.ex_mapped) ---
n <- nrow(raw_char)
STUDYID <- rep(sas_trunc("CDISC-01", 8), n)
DOMAIN  <- rep(sas_trunc("EX", 2), n)
PATIENT_c <- sas_strip(raw_char$PATIENT)
USUBJID <- sas_catx("-", STUDYID, PATIENT_c)
USUBJID <- sas_trunc(USUBJID, 40)

EXTRT <- sas_trunc(sas_strip(raw_char$TREATMENT), 50)
# EXDOSE numeric; raw DOSE may be character
EXDOSE <- suppressWarnings(as.numeric(trimws(as.character(raw_char$DOSE))))
EXDOSU <- sas_trunc(sas_strip(raw_char$UNIT), 20)

EXSTDTC <- ifelse(is.na(start_dt), NA_character_, format(start_dt, "%Y-%m-%d"))
EXENDTC <- ifelse(is.na(end_dt), NA_character_, format(end_dt, "%Y-%m-%d"))
# SAS: missing end -> "" (ongoing); represent as NA (written as "")
EXSTDTC <- sas_trunc(EXSTDTC, 10)
EXENDTC <- sas_trunc(EXENDTC, 10)
EXSTDTC[is.na(EXSTDTC)] <- NA_character_
EXENDTC[is.na(EXENDTC)] <- NA_character_

ex_mapped <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  EXTRT = EXTRT, EXDOSE = EXDOSE, EXDOSU = EXDOSU,
  EXSTDTC = EXSTDTC, EXENDTC = EXENDTC,
  stringsAsFactors = FALSE
)
# keep order
ex_mapped <- ex_mapped[, c("STUDYID","DOMAIN","USUBJID","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")]

# --- 3. GENERATE SEQUENCE NUMBER (EXSEQ) ---
# PROC SORT by USUBJID EXSTDTC (SAS: missing first, uppercase before lowercase)
# Use order with na.last=FALSE; enforce C-locale byte ordering for fidelity
ord <- order(ex_mapped$USUBJID, ex_mapped$EXSTDTC, na.last = FALSE, method = "radix")
ex_sorted <- ex_mapped[ord, , drop = FALSE]
# BY USUBJID: retain counter
EXSEQ <- integer(nrow(ex_sorted))
if (nrow(ex_sorted) > 0) {
  cur <- NA_character_
  cnt <- 0L
  for (i in seq_len(nrow(ex_sorted))) {
    v <- ex_sorted$USUBJID[i]
    v_cmp <- ifelse(is.na(v), "__NA__", v)
    cur_cmp <- ifelse(is.na(cur), "__NA__", cur)
    if (is.na(cur) && i == 1L) { cnt <- 1L; cur <- v }
    else if (!identical(v_cmp, cur_cmp)) { cnt <- 1L; cur <- v }
    else { cnt <- cnt + 1L }
    # handle first row logic explicitly: first.USUBJID then 1 else +1
    EXSEQ[i] <- cnt
  }
}
sdtm_ex <- data.frame(
  STUDYID = ex_sorted$STUDYID,
  DOMAIN = ex_sorted$DOMAIN,
  USUBJID = ex_sorted$USUBJID,
  EXSEQ = EXSEQ,
  EXTRT = ex_sorted$EXTRT,
  EXDOSE = ex_sorted$EXDOSE,
  EXDOSU = ex_sorted$EXDOSU,
  EXSTDTC = ex_sorted$EXSTDTC,
  EXENDTC = ex_sorted$EXENDTC,
  stringsAsFactors = FALSE
)
rownames(sdtm_ex) <- NULL

# --- write dataset ex.csv ---
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# --- 4. VISUAL AUDIT: PROC PRINT obs=10 vars USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC ---
audit_vars <- c("USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")
stopifnot(all(audit_vars %in% names(sdtm_ex)))
audit_n <- min(10L, nrow(sdtm_ex))
audit_df <- sdtm_ex[seq_len(audit_n), audit_vars, drop = FALSE]
audit_df <- cbind(Obs = seq_len(audit_n), audit_df)

# CSV of table body
write.csv(audit_df, file.path(out, "P014.csv"), row.names = FALSE, na = "")

# Plain-text rendering (PROC PRINT style)
title_line <- "EX Domain Audit (Study Drug Exposure)"
# Build fixed-width text similar to SAS listing
# Format numbers as SAS BEST (no extra decimals)
fmt_cell <- function(x) { if (is.na(x)) "" else as.character(x) }
header <- c("Obs","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")
# compute widths
all_rows <- list(header)
for (i in seq_len(nrow(audit_df))) {
  all_rows[[length(all_rows)+1]] <- sapply(audit_df[i, , drop=FALSE], fmt_cell, USE.NAMES=FALSE)
}
# widths = max nchar per column
ncol_t <- length(header)
widths <- integer(ncol_t)
for (j in seq_len(ncol_t)) {
  widths[j] <- max(sapply(all_rows, function(r) nchar(r[j], type="chars", allowNA=TRUE)), na.rm=TRUE)
  widths[j] <- max(widths[j], nchar(header[j]))
}
pad_r <- function(s, w) { sprintf(paste0("%-", w, "s"), s) }
pad_l <- function(s, w) { sprintf(paste0("%", w, "s"), s) }
# Obs, EXSEQ, EXDOSE right-aligned; others left
right_cols <- c("Obs","EXSEQ","EXDOSE")
lines <- c(title_line, "")
# header line
hline <- paste(mapply(function(h, w) {
  if (h %in% right_cols) pad_l(h, w) else pad_r(h, w)
}, header, widths), collapse="  ")
lines <- c(lines, hline)
dashline <- paste(mapply(function(w) paste(rep("-", w), collapse=""), widths), collapse="  ")
lines <- c(lines, dashline)
for (i in seq_len(nrow(audit_df))) {
  rowv <- sapply(audit_df[i, , drop=FALSE], fmt_cell, USE.NAMES=FALSE)
  names(rowv) <- header
  line <- paste(mapply(function(col, w, val) {
    if (col %in% right_cols) pad_l(val, w) else pad_r(val, w)
  }, header, widths, rowv), collapse="  ")
  lines <- c(lines, line)
}
lines <- c(lines, "")
writeLines(lines, file.path(out, "P014.txt"))
# also write .lst counterpart with identical content
writeLines(lines, file.path(out, "P014.lst"))
