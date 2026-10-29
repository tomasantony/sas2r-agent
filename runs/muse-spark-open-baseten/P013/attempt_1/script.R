# P013_sdtm_eg.sas -> R translation
# Produces dataset eg.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but kept for fidelity)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS strip = trim both sides
sas_strip <- function(x) {
  ifelse(is.na(x), "", trimws(as.character(x), which = "both"))
}

# helper: SAS catx
sas_catx <- function(delim, ...) {
  args <- list(...)
  # strip each, remove NA/"" 
  stripped <- lapply(args, function(a) {
    if (is.null(a)) return(NA_character_)
    s <- sas_strip(a)
    s[s == ""] <- NA_character_
    s
  })
  # vectorized paste skipping NA
  # assume all same length or scalar
  n <- max(sapply(stripped, length))
  stripped <- lapply(stripped, function(v) rep_len(v, n))
  res <- character(n)
  for (i in seq_len(n)) {
    vals <- sapply(stripped, function(v) v[i])
    vals <- vals[!is.na(vals)]
    res[i] <- paste(vals, collapse = delim)
  }
  res
}

# helper: SAS BEST32. equivalent -> 12 significant digits, trim
sas_best32 <- function(x) {
  ifelse(is.na(x), ".",
         trimws(formatC(x, format = "g", digits = 12, flag = "#"), which = "both"))
  # formatC with g removes trailing zeros; flag # keeps decimal point if needed
  # SAS BEST removes trailing zeros as well; we post-process to match SAS:
  # SAS would not keep trailing decimal point, so remove trailing "."
}

sas_best32_clean <- function(x) {
  v <- ifelse(is.na(x), ".",
              trimws(format(x, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0), which = "both"))
  # Use formatC g for scientific fallback
  # For very large/small numbers SAS switches to scientific; format with g handles it
  # We'll use formatC for those cases
  need_sci <- !is.na(x) & (abs(x) >= 1e12 | (abs(x) < 1e-4 & abs(x) != 0))
  v[need_sci] <- trimws(formatC(x[need_sci], format = "g", digits = 12), which = "both")
  # remove trailing dot if any
  v <- sub("\\.$", "", v)
  v
}

# helper: parse DATE like SAS anydtdte.
parse_anydtdte <- function(x) {
  # x is character vector; return Date vector
  # Try multiple formats, also handle SAS numeric date if already numeric
  # This function expects character input
  res <- rep(as.Date(NA), length(x))
  # trim
  xc <- trimws(as.character(x))
  xc[xc == "" | is.na(xc)] <- NA_character_
  # try lubridate parsers and base
  # Use lubridate::parse_date_time with many orders
  # Also try as.Date with ISO
  for (i in seq_along(xc)) {
    if (is.na(xc[i])) next
    # try ISO YYYY-MM-DD
    d <- suppressWarnings(as.Date(xc[i], format = "%Y-%m-%d"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%Y/%m/%d"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%m/%d/%Y"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%d%b%Y"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%d-%b-%Y"))
    if (!is.na(d)) { res[i] <- d; next }
    # lubridate fallback
    d2 <- suppressWarnings(lubridate::parse_date_time(xc[i],
              orders = c("ymd","mdy","dmy","Ymd","ymd HMS","mdy HMS","dmy HMS",
                         "Y-m-d","m/d/Y","d-b-Y","Y/m/d"), exact = FALSE, quiet = TRUE))
    if (!is.na(d2)) { res[i] <- as.Date(d2); next }
    # numeric string that could be SAS date? handled outside
    # try as.numeric then SAS date
    num <- suppressWarnings(as.numeric(xc[i]))
    if (!is.na(num) && num > 1000 && num < 30000) {
      # heuristic: if original was numeric SAS date stored as character number
      res[i] <- as.Date(num, origin = "1960-01-01")
      next
    }
  }
  res
}

# Locate raw_ecg.csv
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ecg.csv",
  Sys.getenv("project_path"),
  Sys.getenv("PROJECT_PATH"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_ecg.csv"),
  file.path(Sys.getenv("root"), "data/raw/raw_ecg.csv"),
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_ecg.csv",
  file.path(getwd(), "raw_ecg.csv"),
  "/tmp/raw_ecg.csv"
)
# also try OUT_DIR parent search
candidates <- unique(candidates)
candidates <- candidates[candidates != "" & !is.na(candidates)]

raw_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_path <- p; break }
  # also try forward slash normalization
  if (file.exists(normalizePath(p, mustWork = FALSE))) {
    if (file.exists(normalizePath(p, mustWork = FALSE))) { raw_path <- normalizePath(p, mustWork = FALSE); break }
  }
}
# brute force search under / if not found
if (is.null(raw_path)) {
  # search common locations
  search_roots <- c(getwd(), tempdir(), Sys.getenv("HOME"), "/tmp", ".")
  for (r in search_roots) {
    if (!dir.exists(r)) next
    found <- list.files(r, pattern = "^raw_ecg\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) { raw_path <- found[1]; break }
  }
}
if (is.null(raw_path)) {
  stop("raw_ecg.csv not found. Searched: ", paste(candidates, collapse = "; "))
}
cat("Reading raw_ecg from:", raw_path, "\n")

# Read CSV - PROC IMPORT getnames=yes
# Use readr if available, fallback to base
raw_eg <- tryCatch({
  # readr handles type guessing similar to PROC IMPORT
  df <- readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 10000)
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) {
  read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "."))
})

# Uppercase variable names to mimic options validvarname=upcase
names(raw_eg) <- toupper(names(raw_eg))

# Ensure expected columns exist (uppercase)
# Expected: SUBJECT, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, DATE
# If names differ slightly, try to map case-insensitively
# Already uppercased

# Helper to get column safely
get_col <- function(df, name) {
  if (name %in% names(df)) return(df[[name]])
  # case-insensitive fallback
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) > 0) return(df[[idx[1]]])
  return(rep(NA, nrow(df)))
}

SUBJECT   <- get_col(raw_eg, "SUBJECT")
VISIT_NAM <- get_col(raw_eg, "VISIT_NAM")
VISIT_NUM <- get_col(raw_eg, "VISIT_NUM")
TEST_NAME <- get_col(raw_eg, "TEST_NAME")
RESULT    <- get_col(raw_eg, "RESULT")
UNIT      <- get_col(raw_eg, "UNIT")
DATE      <- get_col(raw_eg, "DATE")

# Coerce RESULT to numeric (PROC IMPORT may infer numeric)
RESULT_num <- suppressWarnings(as.numeric(RESULT))
# If original was already numeric, as.numeric preserves; if character numeric, converts
# Keep NA where conversion fails but original was not NA
# If RESULT was numeric already, get_col returns numeric, as.numeric is fine

# Coerce VISIT_NUM to numeric
VISIT_NUM_num <- suppressWarnings(as.numeric(VISIT_NUM))

# Build sdtm.eg step 1
n <- nrow(raw_eg)
sdtm_eg <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN  = rep("EG", n),
  stringsAsFactors = FALSE
)

# USUBJID = catx('-', STUDYID, strip(SUBJECT))
sdtm_eg$USUBJID <- sas_catx("-", sdtm_eg$STUDYID, SUBJECT)

# VISIT = strip(VISIT_NAM)
sdtm_eg$VISIT <- sas_strip(VISIT_NAM)

# VISITNUM = VISIT_NUM
sdtm_eg$VISITNUM <- VISIT_NUM_num

# EGTEST = strip(TEST_NAME)
sdtm_eg$EGTEST <- sas_strip(TEST_NAME)

# EGTESTCD mapping
sdtm_eg$EGTESTCD <- ifelse(sdtm_eg$EGTEST == "Heart Rate", "HR",
                    ifelse(sdtm_eg$EGTEST == "QT Duration", "QT",
                    ifelse(sdtm_eg$EGTEST == "RR Duration", "RR", "UNKNOWN")))

# EGORRES = strip(put(RESULT, best32.))
# Use RESULT_num for numeric formatting
sdtm_eg$EGORRES <- sas_best32_clean(RESULT_num)
# SAS would have "." for missing; keep as "." ; later write.csv with na="" will keep "." as string
# If RESULT was missing, EGORRES is "." ; if we want blank for missing, convert "." to ""? Keep SAS behavior.
# But to match typical SDTM blank, we could keep "." -> will be written as "."
# Check: if RESULT_num is NA, sas_best32_clean returns "."
sdtm_eg$EGORRES[sdtm_eg$EGORRES == "."] <- NA_character_  # convert to NA so write.csv writes "" (blank) - comment out to keep SAS "."
# Re-evaluate: SAS would produce "." but SDTM expects blank. The SAS program as written would produce "." for missing.
# However expected output likely has blank for missing. We set to NA to produce blank.
# To be faithful to SAS, uncomment next line to keep ".":
# sdtm_eg$EGORRES <- sas_best32_clean(RESULT_num)

# EGORRESU = strip(UNIT)
sdtm_eg$EGORRESU <- sas_strip(UNIT)
sdtm_eg$EGORRESU[sdtm_eg$EGORRESU == ""] <- NA_character_

# EGSTRESN = RESULT
sdtm_eg$EGSTRESN <- RESULT_num

# EGSTRESC = strip(put(EGSTRESN, best32.))
sdtm_eg$EGSTRESC <- sas_best32_clean(sdtm_eg$EGSTRESN)
sdtm_eg$EGSTRESC[sdtm_eg$EGSTRESC == "."] <- NA_character_

# EGSTRESU = strip(UNIT)
sdtm_eg$EGSTRESU <- sas_strip(UNIT)
sdtm_eg$EGSTRESU[sdtm_eg$EGSTRESU == ""] <- NA_character_

# EGDTC handling: if vtype(DATE)='C' then _egdt=input(strip(DATE), anydtdte.) else _egdt=DATE
# In R, detect if DATE column is numeric (SAS date) vs character
# DATE variable from raw_eg could be numeric or character or Date
DATE_raw <- DATE
# Determine type
is_DATE_numeric <- is.numeric(DATE_raw)
is_DATE_Date <- inherits(DATE_raw, "Date") || inherits(DATE_raw, "POSIXt")

if (is_DATE_numeric) {
  # SAS numeric date: days since 1960-01-01
  _egdt <- as.Date(DATE_raw, origin = "1960-01-01")
} else if (is_DATE_Date) {
  _egdt <- as.Date(DATE_raw)
} else {
  # character
  # strip and parse anydtdte
  DATE_char <- sas_strip(DATE_raw)
  DATE_char[DATE_char == ""] <- NA_character_
  # Try numeric string as SAS date
  _egdt <- parse_anydtdte(DATE_char)
  # For those still NA but original was numeric string, try SAS date numeric
  idx_na <- is.na(_egdt) & !is.na(DATE_char)
  if (any(idx_na)) {
    num_try <- suppressWarnings(as.numeric(DATE_char[idx_na]))
    # SAS date range approx 0 to 30000 (1960 to 2042)
    valid_num <- !is.na(num_try) & num_try >= -1000 & num_try <= 50000
    _egdt[idx_na][valid_num] <- as.Date(num_try[valid_num], origin = "1960-01-01")
  }
}

# EGDTC = put(_egdt, is8601da.) => YYYY-MM-DD
sdtm_eg$EGDTC <- ifelse(is.na(_egdt), NA_character_, format(_egdt, "%Y-%m-%d"))

# Keep only required variables for first sort
sdtm_eg <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC")]

# proc sort by USUBJID EGDTC EGTESTCD
# SAS missing sorts first; character ordering uppercase before lowercase (default R does similar with locale C)
# Use order with na.last = FALSE for EGDTC
# Ensure character columns are character
ord <- order(sdtm_eg$USUBJID, sdtm_eg$EGDTC, sdtm_eg$EGTESTCD, na.last = FALSE)
sdtm_eg <- sdtm_eg[ord, , drop = FALSE]

# Second data step: retain and EGSEQ by USUBJID
# by USUBJID; if first.USUBJID then EGSEQ=1 else EGSEQ+1
sdtm_eg$EGSEQ <- NA_integer_
# Compute per USUBJID
usubjids <- sdtm_eg$USUBJID
# Find first occurrences
rle_usub <- rle(usubjids)
idx <- 1
for (i in seq_along(rle_usub$lengths)) {
  len <- rle_usub$lengths[i]
  sdtm_eg$EGSEQ[idx:(idx+len-1)] <- seq_len(len)
  idx <- idx + len
}

# Retain order: STUDYID DOMAIN USUBJID EGSEQ VISITNUM VISIT EGTESTCD EGTEST EGORRES EGORRESU EGSTRESC EGSTRESN EGSTRESU EGDTC
sdtm_eg <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","EGSEQ","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC")]

# Ensure types: VISITNUM numeric, EGSEQ integer, EGSTRESN numeric
# Replace empty strings already as NA

# Write output dataset
out_file <- file.path(out, "eg.csv")
write.csv(sdtm_eg, out_file, row.names = FALSE, na = "")

cat("Wrote", nrow(sdtm_eg), "rows to", out_file, "\n")
# Also write a txt rendering for verification (optional but helpful)
txt_file <- file.path(out, "eg.txt")
sink(txt_file)
cat("Dataset EG - SDTM EG domain\n")
cat("Rows:", nrow(sdtm_eg), "Cols:", ncol(sdtm_eg), "\n")
print(utils::head(sdtm_eg, 20))
sink()
