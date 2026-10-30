# P009_sdtm_ae.sas -> R translation
# Program id: P009  family: c2c
# Outputs: dataset ae, listing P009.lst (12 obs audit)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_ae_path <- file.path(project_path, "data/raw/raw_ae.csv")
# fallback locations
if (!file.exists(raw_ae_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_ae.csv"
  alt2 <- file.path(project_path, "raw_ae.csv")
  if (file.exists(alt1)) raw_ae_path <- alt1 else if (file.exists(alt2)) raw_ae_path <- alt2
}
cat("Reading raw_ae from:", raw_ae_path, "\n")

# Helper: SAS round half away from zero (not needed here but included)
round_half_up <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS strip = trim both sides
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which="both"))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(sas_strip(x)))
}
# SAS catx: concatenate with delimiter, stripping and skipping missing/blank
sas_catx <- function(delim, ...) {
  args <- list(...)
  # vectorized over first arg length
  n <- length(args[[1]])
  outv <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v), which="both")
      if (v=="") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    outv[i] <- if (length(parts)==0) "" else paste(parts, collapse=delim)
  }
  outv
}
# Truncate to SAS length (characters)
sas_trunc <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(as.character(x), 1, len))
}

# --- SAS PROC IMPORT emulation: read as character, then decide date order per column ---
# Read raw CSV with all columns as character to preserve original strings
# Use readr if available, else base
raw_ae <- NULL
if (requireNamespace("readr", quietly=TRUE)) {
  # read all as character
  raw_ae <- readr::read_csv(raw_ae_path, col_types = readr::cols(.default = readr::col_character()), na = c("", "NA"), show_col_types = FALSE)
  raw_ae <- as.data.frame(raw_ae, stringsAsFactors=FALSE)
} else {
  raw_ae <- read.csv(raw_ae_path, stringsAsFactors=FALSE, colClasses="character", na.strings=c("", "NA"), check.names=FALSE)
}
cat("Columns in raw_ae:", paste(names(raw_ae), collapse=", "), "\n")
cat("Rows in raw_ae:", nrow(raw_ae), "\n")

# Normalize column names to uppercase for case-insensitive matching (SAS validvarname=upcase)
names(raw_ae) <- toupper(names(raw_ae))
# Expected columns: ID, AE_TERM, SEV, SERIOUS, RELATED, START, END
# Ensure they exist
stopifnot("ID" %in% names(raw_ae))
# If AE_TERM etc missing due to different naming, try to map
# Keep original names uppercased

# Helper to decide date order per column (SAS PROC IMPORT guessingrows=max logic)
decide_date_order <- function(col_vals) {
  # col_vals: character vector as read from CSV
  vals <- sas_strip(col_vals)
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals)==0) return("MDY")
  # Look for numeric slash/dash dates like n/n/yyyy or n-n-yyyy
  first_fields <- c()
  for (v in vals) {
    # only consider pure numeric date with / or -
    if (grepl("^\\s*\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}\\s*$", v)) {
      # extract first field before / or -
      m <- regmatches(v, regexpr("^\\s*\\d{1,2}", v))
      num <- suppressWarnings(as.integer(trimws(m)))
      if (!is.na(num)) first_fields <- c(first_fields, num)
    }
  }
  if (length(first_fields)==0) return("MDY")
  if (any(first_fields > 12, na.rm=TRUE)) return("DMY") else return("MDY")
}

parse_sas_date_column <- function(col_vals, order) {
  # Parse each value with ONE fixed order (MDY or DMY) plus handling of other formats
  # Returns Date vector
  n <- length(col_vals)
  out <- rep(as.Date(NA), n)
  for (i in seq_len(n)) {
    v <- col_vals[i]
    if (is.na(v)) next
    v <- trimws(as.character(v), which="both")
    if (v=="" || toupper(v)=="NA" || toupper(v)=="NULL") next
    # Try ISO first: YYYY-MM-DD or YYYY/MM/DD
    if (grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}", v)) {
      # normalize to - and try ymd
      v2 <- gsub("/", "-", v)
      # handle datetime with time
      v2 <- sub("T.*$", "", v2)
      v2 <- sub(" .*", "", v2)
      d <- suppressWarnings(as.Date(v2, format="%Y-%m-%d"))
      if (!is.na(d)) { out[i] <- d; next }
      d <- suppressWarnings(as.Date(v2, format="%Y-%m-%d"))
      if (!is.na(d)) { out[i] <- d; next }
    }
    # Try text month: 15JAN2020, 15-Jan-2020, 15 Jan 2020
    if (grepl("[A-Za-z]{3,}", v)) {
      # try several strict formats
      fmts <- c("%d-%b-%Y", "%d-%b-%y", "%d %b %Y", "%d %b %y",
                "%d-%B-%Y", "%d %B %Y",
                "%b %d %Y", "%b %d, %Y", "%B %d, %Y",
                "%d%b%Y", "%d%b%y")
      parsed <- NA
      for (f in fmts) {
        d <- suppressWarnings(as.Date(toupper(v), format=toupper(f)))
        # as.Date is case-sensitive for %b, so try original and upper
        if (is.na(d)) d <- suppressWarnings(as.Date(v, format=f))
        if (!is.na(d)) { parsed <- d; break }
      }
      if (!is.na(parsed)) { out[i] <- parsed; next }
      # fallback lubridate dmy if allowed (but we try strict)
      # try lubridate parse with single order
      if (requireNamespace("lubridate", quietly=TRUE)) {
        # use lubridate with exact order not tryFormats
      }
    }
    # Numeric slash/dash date
    if (grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}", v)) {
      sep <- ifelse(grepl("/", v), "/", "-")
      # split
      parts <- strsplit(v, "[/-]", perl=TRUE)[[1]]
      if (length(parts)>=3) {
        # determine year part
        yr <- parts[3]
        # handle 2-digit year: SAS anydtdte uses 1960-2059 window? Use 2000 cutoff 69?
        # Use as.Date with %y which handles 00-68 as 2000-2068, 69-99 as 1969-1999
        fmt <- NULL
        if (order=="MDY") {
          if (nchar(yr)==4) fmt <- paste0("%m", sep, "%d", sep, "%Y")
          else fmt <- paste0("%m", sep, "%d", sep, "%y")
        } else {
          if (nchar(yr)==4) fmt <- paste0("%d", sep, "%m", sep, "%Y")
          else fmt <- paste0("%d", sep, "%m", sep, "%y")
        }
        # need to escape sep for format: "/" is literal, "-" is literal
        d <- suppressWarnings(as.Date(v, format=fmt))
        if (!is.na(d)) { out[i] <- d; next }
        # try alternative sep if original had mixed
        # also try with zero-padded
      }
    }
    # Last resort: try anydtdte-like via as.Date with MDY/DMY
    # Use lubridate single order if available
    if (requireNamespace("lubridate", quietly=TRUE)) {
      if (order=="MDY") {
        d <- suppressWarnings(lubridate::mdy(v, quiet=TRUE))
      } else {
        d <- suppressWarnings(lubridate::dmy(v, quiet=TRUE))
      }
      if (!is.na(d)) { out[i] <- as.Date(d); next }
    }
    # if still NA, leave as NA
  }
  out
}

# Determine order for START and END columns (PROC IMPORT single informat per column)
start_order <- decide_date_order(raw_ae$START)
end_order   <- decide_date_order(raw_ae$END)
cat("START order:", start_order, " END order:", end_order, "\n")

# Parse dates
start_dates <- parse_sas_date_column(raw_ae$START, start_order)
end_dates   <- parse_sas_date_column(raw_ae$END, end_order)

# 2. CORE TRANSFORMATION & DERIVATION
# Pre-define lengths: STUDYID $8, DOMAIN $2, USUBJID $40, AETERM AEDECOD $60, AEREL $15, AESEV $10, AEOUT $30, AESER $1
n <- nrow(raw_ae)
ae_mapped <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN  = rep("AE", n),
  stringsAsFactors=FALSE
)
# USUBJID = catx("-", STUDYID, ID)
ae_mapped$USUBJID <- sas_catx("-", ae_mapped$STUDYID, raw_ae$ID)
ae_mapped$USUBJID <- sas_trunc(ae_mapped$USUBJID, 40)

# AETERM = strip(AE_TERM)
ae_mapped$AETERM <- sas_strip(raw_ae$AE_TERM)
ae_mapped$AETERM <- sas_trunc(ae_mapped$AETERM, 60)
# AEDECOD = upcase(AETERM)
ae_mapped$AEDECOD <- sas_upcase(ae_mapped$AETERM)
ae_mapped$AEDECOD <- sas_trunc(ae_mapped$AEDECOD, 60)

# AESEV = upcase(strip(SEV))
if ("SEV" %in% names(raw_ae)) {
  ae_mapped$AESEV <- sas_upcase(raw_ae$SEV)
} else if ("AESEV" %in% names(raw_ae)) {
  ae_mapped$AESEV <- sas_upcase(raw_ae$AESEV)
} else {
  ae_mapped$AESEV <- rep(NA_character_, n)
}
ae_mapped$AESEV <- sas_trunc(ae_mapped$AESEV, 10)

# AESER
if ("SERIOUS" %in% names(raw_ae)) {
  serious_up <- sas_upcase(raw_ae$SERIOUS)
  ae_mapped$AESER <- ifelse(!is.na(serious_up) & serious_up=="Y", "Y", "N")
} else {
  ae_mapped$AESER <- rep("N", n)
}
ae_mapped$AESER <- sas_trunc(ae_mapped$AESER, 1)

# AEREL
if ("RELATED" %in% names(raw_ae)) {
  related_up <- sas_upcase(raw_ae$RELATED)
  ae_mapped$AEREL <- ifelse(!is.na(related_up) & related_up=="Y", "RELATED",
                     ifelse(!is.na(related_up) & related_up=="N", "NOT RELATED", "UNKNOWN"))
} else {
  ae_mapped$AEREL <- rep("UNKNOWN", n)
}
ae_mapped$AEREL <- sas_trunc(ae_mapped$AEREL, 15)

# ISO 8601 date conversion
# AESTDTC = put(_start_num, is8601da.) -> YYYY-MM-DD or blank
ae_mapped$AESTDTC <- ifelse(is.na(start_dates), "", format(start_dates, "%Y-%m-%d"))
ae_mapped$AEENDTC <- ifelse(is.na(end_dates), "", format(end_dates, "%Y-%m-%d"))

# AEOUT
ae_mapped$AEOUT <- ifelse(!is.na(end_dates), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")
ae_mapped$AEOUT <- sas_trunc(ae_mapped$AEOUT, 30)

# Keep only required vars
ae_mapped <- ae_mapped[, c("STUDYID","DOMAIN","USUBJID","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]

# 3. GENERATE SEQUENCE NUMBER (AESEQ)
# Sort by USUBJID AESTDTC (SAS missing first, uppercase before lowercase)
# For character sort, SAS uses ASCII: uppercase (65-90) before lowercase (97-122)
# R's default sort is locale-dependent; use order with xtfrm and ensure "" first
# We will sort with base order on USUBJID and AESTDTC (both character, "" < any)
# Ensure AESTDTC "" sorts first (na.last=FALSE equivalent)
# Use order with method="radix" for byte order
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, na.last=FALSE, method="radix")
ae_mapped <- ae_mapped[ord, , drop=FALSE]

# BY-group processing to create AESEQ
ae_mapped$AESEQ <- NA_integer_
# Use rle on USUBJID
usubjids <- ae_mapped$USUBJID
# Since sorted, increment per group
current_id <- ""
seq <- 0L
for (i in seq_len(nrow(ae_mapped))) {
  if (ae_mapped$USUBJID[i] != current_id) {
    current_id <- ae_mapped$USUBJID[i]
    seq <- 1L
  } else {
    seq <- seq + 1L
  }
  ae_mapped$AESEQ[i] <- seq
}

# Final SDTM AE dataset with retain order: STUDYID DOMAIN USUBJID AESEQ ...
sdtm_ae <- ae_mapped[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]
# Ensure lengths again
sdtm_ae$STUDYID <- sas_trunc(sdtm_ae$STUDYID, 8)
sdtm_ae$DOMAIN  <- sas_trunc(sdtm_ae$DOMAIN, 2)
sdtm_ae$USUBJID <- sas_trunc(sdtm_ae$USUBJID, 40)
sdtm_ae$AETERM  <- sas_trunc(sdtm_ae$AETERM, 60)
sdtm_ae$AEDECOD <- sas_trunc(sdtm_ae$AEDECOD, 60)
sdtm_ae$AESEV   <- sas_trunc(sdtm_ae$AESEV, 10)
sdtm_ae$AESER   <- sas_trunc(sdtm_ae$AESER, 1)
sdtm_ae$AEREL   <- sas_trunc(sdtm_ae$AEREL, 15)
sdtm_ae$AEOUT   <- sas_trunc(sdtm_ae$AEOUT, 30)

# Write dataset ae.csv
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names=FALSE, na="")

# 4. VISUAL AUDIT - PROC PRINT obs=12
audit_title <- "AE Domain Audit (Adverse Events with Ongoing Logic)"
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit_data <- sdtm_ae[seq_len(min(12, nrow(sdtm_ae))), audit_vars, drop=FALSE]

# Create text rendering (P009.lst)
# SAS PROC PRINT default: Obs column, then vars, with titles
# We will produce a plain-text table similar to SAS listing
lst_lines <- c(
  audit_title,
  paste0(rep("-", nchar(audit_title)), collapse=""),
  "",
  paste0("Obs  ", paste(sprintf("%-12s", audit_vars), collapse="  ")),
  paste0(rep("-", 120), collapse="")
)
for (i in seq_len(nrow(audit_data))) {
  row_vals <- sapply(audit_vars, function(v) {
    val <- audit_data[i, v]
    if (is.na(val)) val <- ""
    # truncate for display
    val <- as.character(val)
    if (nchar(val) > 18) val <- substr(val, 1, 18)
    sprintf("%-12s", val)
  })
  lst_lines <- c(lst_lines, sprintf("%3d  %s", i, paste(row_vals, collapse="  ")))
}
lst_lines <- c(lst_lines, "", paste0("Total observations in AE domain: ", nrow(sdtm_ae)))

# Write text files: P009.lst.txt, P009.txt, P009.lst
writeLines(lst_lines, file.path(out, "P009.lst.txt"))
writeLines(lst_lines, file.path(out, "P009.txt"))
writeLines(lst_lines, file.path(out, "P009.lst"))
# Also write CSV of table body (same as audit_data with Obs)
audit_csv <- cbind(Obs=seq_len(nrow(audit_data)), audit_data)
write.csv(audit_csv, file.path(out, "P009.lst.csv"), row.names=FALSE, na="")
write.csv(audit_csv, file.path(out, "P009.csv"), row.names=FALSE, na="")

cat("Wrote ae.csv with", nrow(sdtm_ae), "rows to", file.path(out, "ae.csv"), "\n")
cat("Wrote listing to", file.path(out, "P009.lst.txt"), "\n")
