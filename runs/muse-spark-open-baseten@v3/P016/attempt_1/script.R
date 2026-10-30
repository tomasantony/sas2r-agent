# P016_sdtm_mh.sas -> R translation
# Output: dataset mh -> mh.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find column name case-insensitive
find_col <- function(nms, target) {
  idx <- which(toupper(nms) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  nms[idx[1]]
}

# locate raw_mh.csv (SAS: &project_path./data/raw/raw_mh.csv)
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_mh.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_mh.csv",
  file.path(Sys.getenv("project_path"), "data/raw/raw_mh.csv"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_mh.csv"),
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_mh.csv"
)
# also try OUT_DIR relative
candidates <- unique(candidates)
raw_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  # fallback: search under D:/meta_hackathon
  alt <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_mh.csv", recursive = TRUE, full.names = TRUE)
  if (length(alt) > 0) raw_path <- alt[1]
}
if (is.null(raw_path) || !file.exists(raw_path)) {
  stop(paste0("raw_mh.csv not found. Tried: ", paste(candidates, collapse="; ")))
}
cat("Reading:", raw_path, "\n")

# Read CSV as character to emulate PROC IMPORT scanning
# Use readr if available, otherwise base
if (requireNamespace("readr", quietly = TRUE)) {
  df_raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, na = c("", "NA"))
  # readr keeps column names as in file; SAS validvarname=upcase forces uppercase
  names(df_raw) <- toupper(names(df_raw))
} else {
  df_raw <- read.csv(raw_path, colClasses = "character", na.strings = c("", "NA"), check.names = FALSE)
  names(df_raw) <- toupper(names(df_raw))
}

# Identify columns (SAS: SUBJECT, CONDITION, DIAGNOSIS_DATE)
nms <- names(df_raw)
col_subject <- find_col(nms, "SUBJECT")
col_condition <- find_col(nms, "CONDITION")
col_diag <- find_col(nms, "DIAGNOSIS_DATE")
if (is.na(col_subject)) stop("SUBJECT column not found in raw_mh.csv; cols: ", paste(nms, collapse=","))
if (is.na(col_condition)) stop("CONDITION column not found")
if (is.na(col_diag)) stop("DIAGNOSIS_DATE column not found")

# Extract vectors as character (strip = trim)
subject_vec <- trimws(as.character(df_raw[[col_subject]]))
condition_vec <- trimws(as.character(df_raw[[col_condition]]))
diag_raw_vec <- as.character(df_raw[[col_diag]])  # keep original for parsing; trim later

# Emulate PROC IMPORT GUESSINGROWS single informat decision for DIAGNOSIS_DATE
# SAS: column of n/n/yyyy becomes numeric SAS date read as DDMMYY10. when any first field >12 else MMDDYY10.
# Decide order once per column, then parse every value with that single format.
diag_trim_for_guess <- trimws(diag_raw_vec)
diag_nonmiss <- diag_trim_for_guess[!is.na(diag_trim_for_guess) & diag_trim_for_guess != ""]
# detect slash and dash n/n/yyyy patterns
slash_pat <- "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$"
dash_pat  <- "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$"
slash_vals <- diag_nonmiss[grepl(slash_pat, diag_nonmiss)]
dash_vals  <- diag_nonmiss[grepl(dash_pat, diag_nonmiss)]
order_guess <- "MDY"  # default SAS DATESTYLE MDY
if (length(slash_vals) > 0) {
  first_fields <- suppressWarnings(as.numeric(sub("^([0-9]{1,2})/.*", "\\1", slash_vals)))
  if (any(first_fields > 12, na.rm = TRUE)) order_guess <- "DMY" else order_guess <- "MDY"
} else if (length(dash_vals) > 0) {
  first_fields <- suppressWarnings(as.numeric(sub("^([0-9]{1,2})-.*", "\\1", dash_vals)))
  if (any(first_fields > 12, na.rm = TRUE)) order_guess <- "DMY" else order_guess <- "MDY"
} else {
  # check generic n/n/yyyy with either separator
  generic_pat <- "^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$"
  generic_vals <- diag_nonmiss[grepl(generic_pat, diag_nonmiss)]
  if (length(generic_vals) > 0) {
    first_fields <- suppressWarnings(as.numeric(sub("^([0-9]{1,2})[/-].*", "\\1", generic_vals)))
    if (any(first_fields > 12, na.rm = TRUE)) order_guess <- "DMY" else order_guess <- "MDY"
  }
}
cat("DIAGNOSIS_DATE order guess:", order_guess, "\n")

# Parse function using single order for n/n/yyyy, plus ISO and ANYDTDTE fallbacks
parse_one_date <- function(x, order) {
  if (is.na(x)) return(as.Date(NA))
  x <- trimws(as.character(x))
  if (x == "" || toupper(x) == "NA" || x == ".") return(as.Date(NA))
  # n/n/yyyy with slash
  if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", x)) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1]]
    if (order == "DMY") { d <- as.integer(parts[1]); m <- as.integer(parts[2]) } else { m <- as.integer(parts[1]); d <- as.integer(parts[2]) }
    y <- as.integer(parts[3])
    if (y < 100) y <- ifelse(y >= 70, 1900L + y, 2000L + y)
    dt <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, m, d)), error = function(e) as.Date(NA))
    return(dt)
  }
  if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", x)) {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    # need to distinguish from yyyy-mm-dd which is 4-digit first part; this pattern is 1-2 digits first
    if (order == "DMY") { d <- as.integer(parts[1]); m <- as.integer(parts[2]) } else { m <- as.integer(parts[1]); d <- as.integer(parts[2]) }
    y <- as.integer(parts[3])
    if (y < 100) y <- ifelse(y >= 70, 1900L + y, 2000L + y)
    dt <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, m, d)), error = function(e) as.Date(NA))
    return(dt)
  }
  # ISO yyyy-mm-dd or yyyy/mm/dd
  if (grepl("^[0-9]{4}[/-][0-9]{1,2}[/-][0-9]{1,2}$", x)) {
    x2 <- gsub("/", "-", x, fixed = TRUE)
    # pad month/day if needed via as.Date with %Y-%m-%d handles 1-digit
    dt <- suppressWarnings(as.Date(x2, format = "%Y-%m-%d"))
    if (!is.na(dt)) return(dt)
    # try with single digit
    parts <- strsplit(x2, "-", fixed = TRUE)[[1]]
    y <- as.integer(parts[1]); m <- as.integer(parts[2]); d <- as.integer(parts[3])
    return(tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, m, d)), error = function(e) as.Date(NA)))
  }
  # Try SAS ANYDTDTE common formats (strict, one format at a time)
  fmts <- c("%d-%b-%Y", "%d-%B-%Y", "%d %b %Y", "%d %B %Y",
            "%d%b%Y", "%d%b%y", "%d-%b-%y",
            "%Y-%m-%d", "%Y/%m/%d", "%Y%m%d",
            "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y",
            "%d.%m.%Y", "%m.%d.%Y",
            "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S")
  for (fmt in fmts) {
    dt <- suppressWarnings(as.Date(x, format = fmt))
    if (!is.na(dt)) return(dt)
  }
  # Try lubridate style month abbrev upper case
  # e.g., 15JAN2020
  if (grepl("^[0-9]{1,2}[A-Za-z]{3}[0-9]{2,4}$", x)) {
    dt <- suppressWarnings(as.Date(toupper(x), format = "%d%b%Y"))
    if (!is.na(dt)) return(dt)
    dt <- suppressWarnings(as.Date(toupper(x), format = "%d%b%y"))
    if (!is.na(dt)) return(dt)
  }
  # fallback: try as.Date with anydtdte via lubridate if available
  if (requireNamespace("lubridate", quietly = TRUE)) {
    dt <- suppressWarnings(lubridate::parse_date_time(x, orders = c("ymd", "dmy", "mdy", "dby", "dbY"), exact = FALSE, quiet = TRUE))
    if (!is.na(dt)) return(as.Date(dt))
  }
  return(as.Date(NA))
}

# Vectorized parsing
mh_dates <- vapply(diag_raw_vec, function(v) as.numeric(parse_one_date(v, order_guess)), numeric(1))
mh_dates <- as.Date(mh_dates, origin = "1970-01-01")

# Build SDTM MH step 1 (before sort)
STUDYID <- rep("CDISC-01", nrow(df_raw))
DOMAIN <- rep("MH", nrow(df_raw))
# USUBJID = catx('-', STUDYID, strip(SUBJECT))
USUBJID <- ifelse(is.na(subject_vec) | subject_vec == "",
                  STUDYID,
                  paste0(STUDYID, "-", subject_vec))
# Truncate to lengths as in SAS LENGTH statement
STUDYID <- substr(STUDYID, 1, 8)
DOMAIN <- substr(DOMAIN, 1, 2)
USUBJID <- substr(USUBJID, 1, 40)
MHTERM <- substr(condition_vec, 1, 60)
MHTERM <- ifelse(is.na(MHTERM), "", MHTERM)  # SAS strip of missing is blank
# MHSTDTC = put(_mhdt, is8601da.) => YYYY-MM-DD
MHSTDTC <- ifelse(is.na(mh_dates), NA_character_, format(mh_dates, "%Y-%m-%d"))
MHSTDTC <- substr(ifelse(is.na(MHSTDTC), "", MHSTDTC), 1, 10)

mh_step1 <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  MHTERM = MHTERM,
  MHSTDTC = MHSTDTC,
  stringsAsFactors = FALSE
)

# PROC SORT by USUBJID MHSTDTC (missing first, character order uppercase before lowercase)
# SAS missing character "" sorts first; we emulate by sorting on MHSTDTC with "" for NA
mh_step1$MHSTDTC_sort <- ifelse(is.na(mh_step1$MHSTDTC) | mh_step1$MHSTDTC == "", "", mh_step1$MHSTDTC)
# Use base order with method radix for byte order; ensure character ordering
ord <- order(mh_step1$USUBJID, mh_step1$MHSTDTC_sort, method = "radix", na.last = FALSE)
mh_sorted <- mh_step1[ord, , drop = FALSE]
mh_sorted$MHSTDTC_sort <- NULL

# Second data step: by USUBJID, MHSEQ
# retain STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC; by USUBJID; if first.USUBJID then MHSEQ=1 else MHSEQ+1
mh_sorted$MHSEQ <- NA_integer_
# iterate by USUBJID groups in sorted order
usubjids <- mh_sorted$USUBJID
current <- NA_character_
seqn <- 0L
for (i in seq_len(nrow(mh_sorted))) {
  if (is.na(current) || usubjids[i] != current) {
    current <- usubjids[i]
    seqn <- 1L
  } else {
    seqn <- seqn + 1L
  }
  mh_sorted$MHSEQ[i] <- seqn
}

# Final keep and retain order: STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC
mh_final <- mh_sorted[, c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHSTDTC")]

# Ensure types: MHSEQ integer
mh_final$MHSEQ <- as.integer(mh_final$MHSEQ)

# Write dataset as CSV (SAS dataset counterpart)
# dates as YYYY-MM-DD already, na = ""
write.csv(mh_final, file.path(out, "mh.csv"), row.names = FALSE, na = "")

cat("Wrote", nrow(mh_final), "rows to", file.path(out, "mh.csv"), "\n")
# Verify columns exist
stopifnot(all(c("STUDYID","DOMAIN","USUBJID","MHSEQ","MHTERM","MHSTDTC") %in% names(mh_final)))
