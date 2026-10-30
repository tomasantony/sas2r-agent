# P015_sdtm_lb.sas -> R translation
# Outputs: lb.csv + P015.lst text/CSV

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but defined)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS BEST. format emulation (12 significant digits, strip)
sas_best <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_character_)
  # formatC with g uses significant digits, similar to SAS BEST12.
  s <- formatC(x, format = "g", digits = 12, flag = "#")
  # formatC may produce trailing zeros with flag #, remove them like SAS
  # SAS BEST removes trailing zeros and trailing dot
  # flag "#" keeps dot, so strip
  s <- sub("\\.$", "", s)
  # remove trailing zeros after decimal if any (SAS does)
  # e.g. "12.5000000000" -> "12.5" ; but formatC g already does
  # ensure no scientific for moderate numbers: formatC g may use e+
  # keep as is
  trimws(s)
}
sas_best_vec <- function(v) {
  sapply(v, function(xi) if (is.na(xi)) NA_character_ else sas_best(xi), USE.NAMES = FALSE)
}

# helper: find raw_lab.csv
candidates <- c(
  "/bench/data/derived/c2c/data/raw/raw_lab.csv",
  "/bench/data/raw/raw_lab.csv",
  file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_lab.csv"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_lab.csv"),
  "data/derived/c2c/data/raw/raw_lab.csv",
  "data/raw/raw_lab.csv"
)
# also try relative to out
raw_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { raw_path <- p; break }
}
# brute search if not found
if (is.null(raw_path)) {
  # search under D:/meta_hackathon
  base <- "/bench"
  if (dir.exists(base)) {
    found <- list.files(base, pattern = "^raw_lab\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) raw_path <- found[1]
  }
}
if (is.null(raw_path) || !file.exists(raw_path)) {
  stop(paste0("raw_lab.csv not found. Searched: ", paste(candidates, collapse="; ")))
}

# 1. IMPORT RAW LAB DATA (PROC IMPORT getnames=yes)
# Read everything as character to emulate SAS guessing then manual conversion
raw_lb_raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                              na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE, guess_max = 10000)

# SAS validvarname=upcase -> column names uppercased
names(raw_lb_raw) <- toupper(trimws(names(raw_lb_raw)))

# Ensure expected columns exist (case-insensitive check)
# Expected: SUBJ, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, LAB_DAT
required <- c("SUBJ","VISIT_NAM","VISIT_NUM","TEST_NAME","RESULT","UNIT","LAB_DAT")
missing_cols <- setdiff(required, names(raw_lb_raw))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in raw_lab.csv:", paste(missing_cols, collapse=", "), 
             "Found:", paste(names(raw_lb_raw), collapse=", ")))
}

# Convert numeric columns
raw_lb_raw$VISIT_NUM_N <- suppressWarnings(as.numeric(raw_lb_raw$VISIT_NUM))
raw_lb_raw$RESULT_N   <- suppressWarnings(as.numeric(raw_lb_raw$RESULT))

# Keep original LAB_DAT character for SAS anydtdte emulation
# SAS PROC IMPORT logic: decide informat once per column from all values
# If column contains "/" then check first field >12 => DDMMYY else MMDDYY
# We emulate that single-format decision

lab_vec <- trimws(as.character(raw_lb_raw$LAB_DAT))

# Determine if LAB_DAT is SAS numeric date column (all digits) vs character dates
# If any value contains "/" or "-" then treat as character date strings
has_slash <- any(grepl("/", lab_vec, fixed = TRUE) & !is.na(lab_vec), na.rm = TRUE)
has_dash  <- any(grepl("-", lab_vec, fixed = TRUE) & !is.na(lab_vec), na.rm = TRUE)

# Helper to decide MMDDYY vs DDMMYY for slash dates
decide_date_order <- function(vec) {
  # vec: character vector
  vals <- vec[!is.na(vec) & nzchar(trimws(vec))]
  # only consider slash dates
  slash_vals <- vals[grepl("/", vals, fixed = TRUE)]
  if (length(slash_vals) == 0) return(NA_character_)
  first_fields <- suppressWarnings(as.numeric(sapply(strsplit(slash_vals, "/"), function(x) x[1])))
  first_fields <- first_fields[!is.na(first_fields)]
  if (length(first_fields) == 0) return(NA_character_)
  if (any(first_fields > 12, na.rm = TRUE)) "DDMMYY" else "MMDDYY"
}

date_order <- NA_character_
if (has_slash) {
  date_order <- decide_date_order(lab_vec)
}

# Function to parse one LAB_DAT value using the single decided format
parse_lab_one <- function(x, order) {
  if (is.na(x) || !nzchar(trimws(x))) return(as.Date(NA))
  x <- trimws(x)
  # pure integer => SAS date number (days since 1960-01-01)
  if (grepl("^[0-9]+$", x)) {
    n <- suppressWarnings(as.numeric(x))
    # SAS date range approx 0-80000; if plausible treat as SAS date
    # But also RESULT-like numbers could be mistaken; only treat if column was numeric-like
    # Here LAB_DAT pure digits should be SAS date
    if (!is.na(n) && n >= 0 && n <= 100000) {
      return(as.Date(n, origin = "1960-01-01"))
    }
  }
  # slash dates: use single order
  if (grepl("/", x, fixed = TRUE)) {
    fmt <- if (!is.na(order) && order == "DDMMYY") "%d/%m/%Y" else "%m/%d/%Y"
    # also try 2-digit year fallback
    d <- suppressWarnings(as.Date(x, format = fmt))
    if (is.na(d)) {
      fmt2 <- if (!is.na(order) && order == "DDMMYY") "%d/%m/%y" else "%m/%d/%y"
      d <- suppressWarnings(as.Date(x, format = fmt2))
    }
    # also try with single digit month/day (as.Date handles)
    return(d)
  }
  # dash dates: try ISO YYYY-MM-DD first, then DD-MMM-YYYY, etc.
  if (grepl("-", x, fixed = TRUE)) {
    # ISO
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (!is.na(d)) return(d)
    # DD-MMM-YYYY or DD-Mon-YYYY
    # normalize to upper for month
    d <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%Y"))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(toupper(x), format = "%d-%B-%Y"))
    if (!is.na(d)) return(d)
    # YYYY/MM/DD with dashes? already tried
    # Try anydtdte fallback: try lubridate-like but single format: use %d-%m-%Y
    d <- suppressWarnings(as.Date(x, format = "%d-%m-%Y"))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(x, format = "%m-%d-%Y"))
    if (!is.na(d)) return(d)
    return(as.Date(NA))
  }
  # fallback: try as.Date ISO
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # try numeric SAS date as character
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n)) return(as.Date(n, origin = "1960-01-01"))
  return(as.Date(NA))
}

lab_dates <- sapply(lab_vec, parse_lab_one, order = date_order, USE.NAMES = FALSE)
# sapply may return numeric (Date is numeric); ensure Date class
lab_dates <- as.Date(lab_dates, origin = "1970-01-01")

# LBDTC as ISO8601 date (is8601da. => YYYY-MM-DD)
LBDTC_vec <- ifelse(is.na(lab_dates), NA_character_, format(lab_dates, "%Y-%m-%d"))

# 2. CORE TRANSFORMATION & DICTIONARY MAPPING
n <- nrow(raw_lb_raw)
lb_mapped <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN  = rep("LB", n),
  stringsAsFactors = FALSE
)
# USUBJID = catx("-", STUDYID, SUBJ)
lb_mapped$USUBJID <- paste0("CDISC-01-", trimws(as.character(raw_lb_raw$SUBJ)))
# VISIT = strip(VISIT_NAM)
lb_mapped$VISIT <- trimws(as.character(raw_lb_raw$VISIT_NAM))
# VISITNUM = VISIT_NUM (numeric)
lb_mapped$VISITNUM <- raw_lb_raw$VISIT_NUM_N

# TEST mapping
test_up <- toupper(trimws(as.character(raw_lb_raw$TEST_NAME)))
lb_mapped$LBTESTCD <- NA_character_
lb_mapped$LBTEST   <- NA_character_
lb_mapped$LBCAT    <- NA_character_

for (i in seq_len(n)) {
  tn <- test_up[i]
  orig <- trimws(as.character(raw_lb_raw$TEST_NAME[i]))
  if (!is.na(tn) && tn == "GLUCOSE") {
    lb_mapped$LBTESTCD[i] <- "GLUC"
    lb_mapped$LBTEST[i]   <- "Glucose"
    lb_mapped$LBCAT[i]    <- "CHEMISTRY"
  } else if (!is.na(tn) && tn == "HEMOGLOBIN") {
    lb_mapped$LBTESTCD[i] <- "HGB"
    lb_mapped$LBTEST[i]   <- "Hemoglobin"
    lb_mapped$LBCAT[i]    <- "HEMATOLOGY"
  } else if (!is.na(tn) && tn == "ALT") {
    lb_mapped$LBTESTCD[i] <- "ALT"
    lb_mapped$LBTEST[i]   <- "Alanine Aminotransferase"
    lb_mapped$LBCAT[i]    <- "CHEMISTRY"
  } else if (!is.na(tn) && tn == "AST") {
    lb_mapped$LBTESTCD[i] <- "AST"
    lb_mapped$LBTEST[i]   <- "Aspartate Aminotransferase"
    lb_mapped$LBCAT[i]    <- "CHEMISTRY"
  } else {
    lb_mapped$LBTESTCD[i] <- "UNKNOWN"
    lb_mapped$LBTEST[i]   <- orig
    lb_mapped$LBCAT[i]    <- "UNKNOWN"
  }
}

# Results Formatting
# LBORRES = strip(put(RESULT, best.))
lb_mapped$LBORRES <- sas_best_vec(raw_lb_raw$RESULT_N)
lb_mapped$LBSTRESN <- raw_lb_raw$RESULT_N
lb_mapped$LBSTRESC <- lb_mapped$LBORRES

# Standardize units
lb_mapped$LBORRESU <- trimws(as.character(raw_lb_raw$UNIT))
lb_mapped$LBSTRESU <- NA_character_
for (i in seq_len(n)) {
  u <- lb_mapped$LBORRESU[i]
  if (is.na(u) || !nzchar(u)) {
    lb_mapped$LBSTRESU[i] <- u
  } else if (toupper(u) == "MG/DL") {
    lb_mapped$LBSTRESU[i] <- "mg/dL"
  } else if (toupper(u) == "G/DL") {
    lb_mapped$LBSTRESU[i] <- "g/dL"
  } else if (toupper(u) == "U/L") {
    lb_mapped$LBSTRESU[i] <- "U/L"
  } else {
    lb_mapped$LBSTRESU[i] <- u
  }
}

lb_mapped$LBDTC <- LBDTC_vec

# Keep only required columns in SAS keep order
lb_mapped <- lb_mapped[, c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]

# 3. GENERATE SEQUENCE NUMBER (LBSEQ)
# proc sort by USUBJID LBDTC LBTESTCD
# SAS missing sorts first, character uppercase before lowercase (radix)
# Use order with na.last=FALSE, method="radix"
ord <- order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD, na.last = FALSE, method = "radix")
lb_mapped_sorted <- lb_mapped[ord, , drop = FALSE]

# data sdtm.lb; by USUBJID; if first.USUBJID then LBSEQ=1 else LBSEQ+1
lb_mapped_sorted$LBSEQ <- NA_integer_
# retain logic per USUBJID
current_usubjid <- NA_character_
seqn <- 0L
for (i in seq_len(nrow(lb_mapped_sorted))) {
  usub <- lb_mapped_sorted$USUBJID[i]
  if (is.na(current_usubjid) || usub != current_usubjid) {
    current_usubjid <- usub
    seqn <- 1L
  } else {
    seqn <- seqn + 1L
  }
  lb_mapped_sorted$LBSEQ[i] <- seqn
}

# Final sdtm.lb with retain order: STUDYID DOMAIN USUBJID LBSEQ then rest
sdtm_lb <- lb_mapped_sorted[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]

# Ensure types: VISITNUM numeric, LBSTRESN numeric
# Write dataset lb.csv
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=12 vars USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC
audit_vars <- c("USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
audit_df <- head(sdtm_lb[, audit_vars, drop = FALSE], 12)

# Write CSV of table body (required)
write.csv(audit_df, file.path(out, "P015.csv"), row.names = FALSE, na = "")
write.csv(audit_df, file.path(out, "P015.lst.csv"), row.names = FALSE, na = "")

# Build plain-text rendering (120 numbers reference is SAS log count, we reproduce table)
# Title verbatim
title_str <- "LB Domain Audit (Laboratory Test Results)"

# Create text file with title, column headers, rows
# Format similar to SAS PROC PRINT
# Include Obs column
txt_lines <- c(
  title_str,
  paste0(strrep("-", nchar(title_str))),
  "",
  sprintf("Dataset: sdtm.lb  (N=%d, showing first %d obs sorted by USUBJID LBDTC LBTESTCD)", nrow(sdtm_lb), nrow(audit_df)),
  ""
)

# Header
header_fmt <- "%-5s %-20s %-8s %-15s %-8s %-12s %-10s %-8s %-12s"
header <- sprintf(header_fmt, "Obs", "USUBJID", "VISITNUM", "VISIT", "LBTESTCD", "LBCAT", "LBSTRESN", "LBSTRESU", "LBDTC")
sep <- paste0(strrep("-", 5), " ", strrep("-", 20), " ", strrep("-", 8), " ", strrep("-", 15), " ", strrep("-", 8), " ", strrep("-", 12), " ", strrep("-", 10), " ", strrep("-", 8), " ", strrep("-", 12))
txt_lines <- c(txt_lines, header, sep)

for (i in seq_len(nrow(audit_df))) {
  r <- audit_df[i, ]
  # Format LBSTRESN with SAS BEST-like (no extra decimals)
  stresn_str <- ifelse(is.na(r$LBSTRESN), "", sas_best(r$LBSTRESN))
  visitnum_str <- ifelse(is.na(r$VISITNUM), "", as.character(r$VISITNUM))
  line <- sprintf(header_fmt,
                  as.character(i),
                  substr(ifelse(is.na(r$USUBJID), "", r$USUBJID), 1, 20),
                  visitnum_str,
                  substr(ifelse(is.na(r$VISIT), "", r$VISIT), 1, 15),
                  ifelse(is.na(r$LBTESTCD), "", r$LBTESTCD),
                  substr(ifelse(is.na(r$LBCAT), "", r$LBCAT), 1, 12),
                  stresn_str,
                  ifelse(is.na(r$LBSTRESU), "", r$LBSTRESU),
                  ifelse(is.na(r$LBDTC), "", r$LBDTC)
  )
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, "", paste0("Total observations in sdtm.lb: ", nrow(sdtm_lb)))

# Write .txt and .lst (both required)
writeLines(txt_lines, file.path(out, "P015.txt"))
writeLines(txt_lines, file.path(out, "P015.lst"))

# Also ensure no console dump of full data
invisible(NULL)
