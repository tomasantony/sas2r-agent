```r
out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

## ---- locate raw_mh.csv ----
candidate1 <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_mh.csv"
candidate2 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_mh.csv"
raw_path <- NULL
if (file.exists(candidate1)) {
  raw_path <- candidate1
} else if (file.exists(candidate2)) {
  raw_path <- candidate2
} else {
  roots <- c("D:/meta_hackathon/sas2r-agent")
  found <- character(0)
  for (rt in roots) {
    if (dir.exists(rt)) {
      f <- list.files(rt, pattern = "^raw_mh\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) found <- c(found, f)
    }
  }
  if (length(found) > 0) raw_path <- found[1]
}
if (is.null(raw_path)) stop("raw_mh.csv not found")
message("Reading: ", raw_path)

## ---- PROC IMPORT emulation: read everything as character ----
raw_df <- utils::read.csv(raw_path, colClasses = "character", check.names = FALSE,
                          strip.white = TRUE, na.strings = c("", "NA"))
names(raw_df) <- toupper(names(raw_df))
message("Columns: ", paste(names(raw_df), collapse = ","))
stopifnot(all(c("SUBJECT", "CONDITION", "DIAGNOSIS_DATE") %in% names(raw_df)))

diag_raw <- raw_df$DIAGNOSIS_DATE
## diag_raw is character with NA for missing (PROC IMPORT empty -> missing)

## ---- Decide ONE date order for the whole column (GUESSINGROWS) ----
trimmed <- trimws(as.character(diag_raw))
nonmiss <- trimmed[!is.na(trimmed) & trimmed != ""]
slash_pat4 <- "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"
slash_pat2 <- "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$"
slash_vals <- nonmiss[grepl(slash_pat4, nonmiss) | grepl(slash_pat2, nonmiss)]

slash_order <- "MDY"
if (length(slash_vals) > 0) {
  first_fields <- suppressWarnings(as.integer(sub("/.*$", "", slash_vals)))
  if (any(!is.na(first_fields) & first_fields > 12)) {
    slash_order <- "DMY"
  } else {
    slash_order <- "MDY"
  }
}
message("Slash order decided: ", slash_order)

## Helper: parse one value with the single decided order (plus ISO which ANYDTDTE always reads)
parse_one <- function(x, order) {
  if (is.na(x)) return(as.Date(NA))
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) %in% c("NA", ".")) return(as.Date(NA))
  ## ISO YYYY-MM-DD (ANYDTDTE reads regardless of DATESTYLE)
  if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", s)) {
    return(as.Date(s, format = "%Y-%m-%d"))
  }
  ## slash n/n/yyyy
  if (grepl(slash_pat4, s)) {
    fmt <- if (order == "DMY") "%d/%m/%Y" else "%m/%d/%Y"
    return(as.Date(s, format = fmt))
  }
  ## slash n/n/yy
  if (grepl(slash_pat2, s)) {
    fmt <- if (order == "DMY") "%d/%m/%y" else "%m/%d/%y"
    return(as.Date(s, format = fmt))
  }
  ## DATE9 dd-Mon-yyyy (ANYDTDTE also reads)
  if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", s)) {
    return(as.Date(s, format = "%d-%b-%Y"))
  }
  if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}$", s)) {
    return(as.Date(s, format = "%d-%b-%y"))
  }
  ## fallback: try ISO with time prefix
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}.*", s)) {
    return(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
  }
  return(as.Date(NA))
}

mhdt_list <- lapply(trimmed, parse_one, order = slash_order)
mhdt <- do.call(c, mhdt_list)
## do.call(c,...) on Date objects preserves Date class; ensure Date
if (!inherits(mhdt, "Date")) {
  mhdt <- as.Date(mhdt, origin = "1970-01-01")
}

## ---- Build SDTM MH (first DATA step) ----
n <- nrow(raw_df)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("MH", n)

subject_stripped <- trimws(as.character(raw_df$SUBJECT))
subject_stripped[is.na(subject_stripped)] <- ""
## CATX('-', STUDYID, strip(SUBJECT)): skips empty
USUBJID <- ifelse(subject_stripped == "" | is.na(subject_stripped),
                  "CDISC-01",
                  paste0("CDISC-01-", subject_stripped))

cond_stripped <- trimws(as.character(raw_df$CONDITION))
## LENGTH $60 truncation (SAS truncates)
MHTERM <- substr(cond_stripped, 1, 60)
## keep SAS missing as NA
MHTERM[is.na(cond_stripped)] <- NA_character_
MHTERM[MHTERM == ""] <- NA_character_

MHSTDTC <- ifelse(is.na(mhdt), NA_character_, format(mhdt, "%Y-%m-%d"))

mh <- data.frame(STUDYID = STUDYID,
                 DOMAIN = DOMAIN,
                 USUBJID = USUBJID,
                 MHTERM = MHTERM,
                 MHSTDTC = MHSTDTC,
                 stringsAsFactors = FALSE)

## ---- PROC SORT by USUBJID MHSTDTC (missing first, SAS binary order) ----
## Missing (NA) sorts first; character ordering uppercase before lowercase is
## default byte order - use xtfrm with C locale via order on raw bytes.
## For these values (uppercase/digits/hyphen) base order matches SAS.
ord <- order(mh$USUBJID, mh$MHSTDTC, na.last = FALSE, method = "radix")
mh <- mh[ord, , drop = FALSE]

## ---- Second DATA step: MHSEQ by USUBJID ----
mh$MHSEQ <- NA_integer_
## FIRST./LAST. processing
prev <- ""
seqn <- 0L
for (i in seq_len(nrow(mh))) {
  cur <- mh$USUBJID[i]
  if (is.na(cur)) cur <- NA_character_
  is_first <- (i == 1L) || is.na(cur) != is.na(prev) || (!is.na(cur) && !is.na(prev) && cur != prev) || (is.na(cur) && is.na(prev) && FALSE)
  ## simpler: compare with previous row value handling NA
  if (i == 1L) {
    is_first <- TRUE
  } else {
    p <- mh$USUBJID[i - 1L]
    c <- mh$USUBJID[i]
    if (is.na(c) && is.na(p)) is_first <- FALSE
    else if (is.na(c) || is.na(p)) is_first <- TRUE
    else is_first <- (c != p)
  }
  if (is_first) seqn <- 1L else seqn <- seqn + 1L
  mh$MHSEQ[i] <- seqn
  prev <- mh$USUBJID[i]
}

## retain order
mh <- mh[, c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHSTDTC"), drop = FALSE]
rownames(mh) <- NULL

## ---- Write dataset ----
write.csv(mh, file.path(out, "mh.csv"), row.names = FALSE, na = "")
message("Wrote ", file.path(out, "mh.csv"), " rows=", nrow(mh))
```