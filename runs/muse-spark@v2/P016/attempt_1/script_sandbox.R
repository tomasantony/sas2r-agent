# P016 sdtm_mh.sas -> R translation
# Output: mh.csv

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

raw_path <- "/bench/data/derived/c2c/data/raw/raw_mh.csv"

# SAS: PROC IMPORT DBMS=CSV GETNAMES=YES (guess types)
raw_mh <- readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 10000, progress = FALSE)
names(raw_mh) <- toupper(names(raw_mh))

# validvarname=upcase -> names already upper; ensure expected cols exist
stopifnot(all(c("SUBJECT", "CONDITION", "DIAGNOSIS_DATE") %in% names(raw_mh)))

# Helper: SAS STRIP = trim leading/trailing blanks
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}

# Helper: SAS CATX('-', STUDYID, strip(SUBJECT))
sas_catx_usubjid <- function(studyid, subject) {
  subj <- sas_strip(subject)
  # CATX skips missing: if subj missing/blank -> studyid only
  res <- ifelse(is.na(subj) | subj == "", studyid,
                paste0(studyid, "-", subj))
  substr(res, 1, 40)
}

# Prepare base columns
n <- nrow(raw_mh)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("MH", n)
SUBJECT_chr <- as.character(raw_mh$SUBJECT)
USUBJID <- sas_catx_usubjid(STUDYID, SUBJECT_chr)
USUBJID <- substr(USUBJID, 1, 40)

COND_chr <- sas_strip(as.character(raw_mh$CONDITION))
MHTERM <- substr(ifelse(is.na(COND_chr), NA_character_, COND_chr), 1, 60)
# SAS missing character is blank; keep NA internally, write as "" later
MHTERM[MHTERM == ""] <- NA_character_

# --- MH date: SAS ANYDTDTE (DATESTYLE=MDY) + numeric SAS date branch ---
# SAS numeric date origin 1960-01-01
diag_col <- raw_mh$DIAGNOSIS_DATE

parse_one_char_date <- function(x_trim) {
  if (is.na(x_trim) || x_trim == "") return(as.Date(NA))
  # pure integer string -> treat as SAS date number (PROC IMPORT numeric stored as char)
  if (grepl("^[0-9]+$", x_trim)) {
    num <- suppressWarnings(as.numeric(x_trim))
    # SAS dates are ~ -1000..60000; if huge, still convert; if NA return NA
    if (is.na(num)) return(as.Date(NA))
    return(as.Date(num, origin = "1960-01-01"))
  }
  # Year-first ISO prefix YYYY-MM-DD (unambiguous, ANYDTDTE parses regardless of DATESTYLE)
  # strict single format %Y-%m-%d
  if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", x_trim)) {
    part <- substr(x_trim, 1, 10)
    # strict: require zero-padded? use %Y-%m-%d which requires 2-digit mm/dd in R? R allows 1-digit too.
    # Use explicit strict format
    return(suppressWarnings(as.Date(part, format = "%Y-%m-%d")))
  }
  # Otherwise MDY order (single fixed order MDY, mimics ANYDTDTE with DATESTYLE=MDY)
  # lubridate::mdy with single order handles "/" "-" "." spaces and month names in MDY
  d <- suppressWarnings(lubridate::mdy(x_trim, quiet = TRUE))
  return(as.Date(d))
}

mh_date <- rep(as.Date(NA), n)

if (inherits(diag_col, "Date")) {
  mh_date <- as.Date(diag_col)
} else if (inherits(diag_col, "POSIXct") || inherits(diag_col, "POSIXlt")) {
  mh_date <- as.Date(diag_col)
} else if (is.numeric(diag_col)) {
  # SAS vtype N: _mhdt = DIAGNOSIS_DATE (numeric SAS date)
  mh_date <- as.Date(diag_col, origin = "1960-01-01")
} else {
  # character / factor / logical etc.
  ch <- as.character(diag_col)
  ch_trim <- trimws(ch, which = "both")
  # vectorize parse
  for (i in seq_len(n)) {
    xi <- ch_trim[i]
    if (is.na(xi) || xi == "") {
      mh_date[i] <- as.Date(NA)
    } else {
      mh_date[i] <- parse_one_char_date(xi)
    }
  }
}

MHSTDTC <- format(mh_date, "%Y-%m-%d")
MHSTDTC[is.na(mh_date)] <- NA_character_
MHSTDTC <- substr(ifelse(is.na(MHSTDTC), NA_character_, MHSTDTC), 1, 10)

mh <- data.frame(
  STUDYID = substr(STUDYID, 1, 8),
  DOMAIN = substr(DOMAIN, 1, 2),
  USUBJID = USUBJID,
  MHTERM = MHTERM,
  MHSTDTC = MHSTDTC,
  stringsAsFactors = FALSE
)

# PROC SORT BY USUBJID MHSTDTC (SAS: missing first, ASCII uppercase before lowercase)
# Use radix byte-order to mimic SAS character ordering
ord <- order(mh$USUBJID, mh$MHSTDTC, na.last = FALSE, method = "radix")
mh <- mh[ord, , drop = FALSE]

# DATA step BY USUBJID: MHSEQ
mh$MHSEQ <- ave(seq_len(nrow(mh)), mh$USUBJID, FUN = function(x) seq_along(x))
mh$MHSEQ <- as.numeric(mh$MHSEQ)

# RETAIN / KEEP order
mh <- mh[, c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHSTDTC")]

# Ensure types
rownames(mh) <- NULL

cat("MH rows:", nrow(mh), "\n")

write.csv(mh, file.path(out, "mh.csv"), row.names = FALSE, na = "")
