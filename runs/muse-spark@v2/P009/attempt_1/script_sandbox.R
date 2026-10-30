# P009_sdtm_ae.sas -> R translation
# SDTM AE mapping with AESEQ and ongoing logic

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

project_path <- "/bench/data/derived/c2c"
raw_ae_path <- "/bench/data/derived/c2c/data/raw/raw_ae.csv"

cat("Reading raw AE:", raw_ae_path, "\n")

# PROC IMPORT guessingrows=max, getnames=yes, DBMS=CSV
# Force character to preserve raw strings (mimic SAS character/numeric handling)
raw_ae <- readr::read_csv(raw_ae_path,
  col_types = readr::cols(.default = readr::col_character()),
  guess_max = 1000000, show_col_types = FALSE)

# options validvarname=upcase -> variable names uppercase
names(raw_ae) <- toupper(names(raw_ae))
stopifnot(all(c("ID","AE_TERM","SEV","SERIOUS","RELATED","START","END") %in% names(raw_ae)))

# Helper: SAS STRIP (trim) preserving NA as NA
sstrip <- function(x) {
  ifelse(is.na(x), NA_character_, stringr::str_trim(as.character(x)))
}

# Helper: SAS ANYDTDTE (DATESTYLE=MDY) -> Date, one explicit format per pattern
parse_anydtdte_mdy <- function(x) {
  n <- length(x)
  res <- as.Date(rep(NA_character_, n))
  for (i in seq_len(n)) {
    v <- x[i]
    if (is.na(v)) next
    t <- stringr::str_trim(as.character(v))
    if (is.na(t) || t == "") next
    # Pure integer string 1-5 digits -> SAS date number (days since 1960-01-01)
    if (grepl("^[0-9]+$", t) && nchar(t) <= 5) {
      num <- suppressWarnings(as.numeric(t))
      if (!is.na(num)) res[i] <- as.Date(num, origin = "1960-01-01")
      next
    }
    # Slash dates -> MDY order (single fixed order)
    if (grepl("/", t, fixed = TRUE)) {
      dpart <- strsplit(t, " ", fixed = TRUE)[[1]][1]
      dpart <- strsplit(dpart, "T", fixed = TRUE)[[1]][1]
      parts <- strsplit(dpart, "/", fixed = TRUE)[[1]]
      if (length(parts) == 3) {
        y <- parts[3]
        dpart2 <- paste(parts[1], parts[2], y, sep = "/")
        if (nchar(y) == 2) {
          res[i] <- as.Date(dpart2, format = "%m/%d/%y")
        } else {
          res[i] <- as.Date(dpart2, format = "%m/%d/%Y")
        }
        next
      } else {
        res[i] <- as.Date(dpart, format = "%m/%d/%Y")
        next
      }
    }
    # ISO YYYY-MM-DD or YYYY/MM/DD possibly with time
    if (grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}", t)) {
      dpart <- substr(t, 1, 10)
      dpart <- gsub("/", "-", dpart, fixed = TRUE)
      res[i] <- as.Date(dpart, format = "%Y-%m-%d")
      next
    }
    # YYYYMMDD
    if (grepl("^[0-9]{8}$", t)) {
      res[i] <- as.Date(t, format = "%Y%m%d")
      next
    }
    # DATE9 variants: DDMMMYYYY with dashes/spaces/slashes
    nt <- toupper(gsub("[- /]", "", t))
    nt <- strsplit(nt, "T", fixed = TRUE)[[1]][1]
    nt <- strsplit(nt, " ", fixed = TRUE)[[1]][1]
    if (grepl("^[0-9]{1,2}[A-Z]{3}[0-9]{2,4}$", nt)) {
      m <- regexec("^([0-9]{1,2})([A-Z]{3})([0-9]{2,4})$", nt)
      p <- regmatches(nt, m)[[1]]
      if (length(p) == 4) {
        d <- p[2]; mon <- p[3]; y <- p[4]
        if (nchar(y) == 2) {
          res[i] <- as.Date(paste0(d, mon, y), format = "%d%b%y")
        } else {
          res[i] <- as.Date(paste0(d, mon, y), format = "%d%b%Y")
        }
        next
      }
    }
    # MM-DD-YYYY (MDY with dashes)
    if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}", t)) {
      dpart <- strsplit(t, " ", fixed = TRUE)[[1]][1]
      dpart <- strsplit(dpart, "T", fixed = TRUE)[[1]][1]
      parts <- strsplit(dpart, "-", fixed = TRUE)[[1]]
      if (length(parts) == 3) {
        y <- parts[3]
        if (nchar(y) == 2) res[i] <- as.Date(dpart, format = "%m-%d-%y")
        else res[i] <- as.Date(dpart, format = "%Y-%m-%d")
        if (is.na(res[i])) {
          if (nchar(y) == 2) res[i] <- as.Date(dpart, format = "%m-%d-%y")
          else res[i] <- as.Date(dpart, format = "%m-%d-%Y")
        }
        next
      }
    }
    # Unparseable -> NA (SAS would set missing)
    res[i] <- as.Date(NA)
  }
  res
}

# Numeric SAS date vector -> Date
sasnum_to_date <- function(x) {
  suppressWarnings(as.Date(as.numeric(x), origin = "1960-01-01"))
}

# 2. CORE TRANSFORMATION & DERIVATION
# lengths: STUDYID $8 DOMAIN $2 USUBJID $40 AETERM AEDECOD $60 AEREL $15 AESEV $10 AEOUT $30 AESER $1
ae_mapped <- raw_ae %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "AE",
    .ID_trim = sstrip(ID),
    USUBJID = ifelse(is.na(.ID_trim) | .ID_trim == "", STUDYID, paste0(STUDYID, "-", .ID_trim)),
    .AETERM_raw = sstrip(AE_TERM),
    AETERM = substr(ifelse(is.na(.AETERM_raw), NA_character_, .AETERM_raw), 1, 60),
    AEDECOD = substr(ifelse(is.na(AETERM), NA_character_, toupper(AETERM)), 1, 60),
    .SEV_trim = sstrip(SEV),
    AESEV = substr(ifelse(is.na(.SEV_trim), NA_character_, toupper(.SEV_trim)), 1, 10),
    .SER_trim = sstrip(SERIOUS),
    .SER_up = ifelse(is.na(.SER_trim), "", toupper(.SER_trim)),
    AESER = ifelse(.SER_up == "Y", "Y", "N"),
    .REL_trim = sstrip(RELATED),
    .REL_up = ifelse(is.na(.REL_trim), "", toupper(.REL_trim)),
    AEREL_raw = ifelse(.REL_up == "Y", "RELATED",
                ifelse(.REL_up == "N", "NOT RELATED", "UNKNOWN")),
    AEREL = substr(AEREL_raw, 1, 15)
  )

# ISO date conversion: handle character vs numeric like vtype check
# Here all START/END read as character; numeric strings handled inside parser
# If column were numeric Date, handle separately (not needed but kept for fidelity)
start_chr <- sstrip(ae_mapped$START)
end_chr <- sstrip(ae_mapped$END)

# Detect if values are Date objects (readr auto-parse) - coerce to character ISO first
# (we forced character, so skip)

start_date <- parse_anydtdte_mdy(start_chr)
end_date <- parse_anydtdte_mdy(end_chr)

ae_mapped$AESTDTC <- ifelse(is.na(start_date), NA_character_, format(start_date, "%Y-%m-%d"))
ae_mapped$AEENDTC <- ifelse(is.na(end_date), NA_character_, format(end_date, "%Y-%m-%d"))
ae_mapped$AEOUT_raw <- ifelse(!is.na(end_date), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")
ae_mapped$AEOUT <- substr(ae_mapped$AEOUT_raw, 1, 30)

# Truncate lengths per SAS LENGTH
ae_mapped$STUDYID <- substr(ae_mapped$STUDYID, 1, 8)
ae_mapped$DOMAIN <- substr(ae_mapped$DOMAIN, 1, 2)
ae_mapped$USUBJID <- substr(ae_mapped$USUBJID, 1, 40)
ae_mapped$AESER <- substr(ae_mapped$AESER, 1, 1)

ae_mapped <- ae_mapped %>%
  select(STUDYID, DOMAIN, USUBJID, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)

# 3. SORT by USUBJID AESTDTC (SAS: missing first, uppercase before lowercase)
# Use radix byte order to mimic SAS character ordering
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, na.last = FALSE, method = "radix")
ae_sorted <- ae_mapped[ord, , drop = FALSE]

# BY-group AESEQ
ae_sorted$AESEQ <- ave(seq_len(nrow(ae_sorted)), ae_sorted$USUBJID,
  FUN = function(x) seq_along(x))

# retain STUDYID DOMAIN USUBJID AESEQ ...
sdtm_ae <- ae_sorted[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]

# Ensure AESEQ numeric (integer)
sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)

cat("N AE records:", nrow(sdtm_ae), "\n")

# Write dataset ae.csv
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: PROC PRINT obs=12 vars USUBJID AESEQ AETERM AESEV AESER AEREL AESTDTC AEENDTC AEOUT
audit_title <- "AE Domain Audit (Adverse Events with Ongoing Logic)"
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit12 <- head(sdtm_ae[, audit_vars, drop = FALSE], 12)

# CSV of table body
write.csv(audit12, file.path(out, "P009.csv"), row.names = FALSE, na = "")

# Plain-text rendering mimicking PROC PRINT
# Build fixed-width table
hdr <- c("Obs", audit_vars)
# Compute widths
df_print <- data.frame(Obs = seq_len(nrow(audit12)), audit12, check.names = FALSE, stringsAsFactors = FALSE)
# Convert all to character for display (NA -> "")
df_disp <- df_print
for (nm in names(df_disp)) {
  v <- df_disp[[nm]]
  v <- ifelse(is.na(v), "", as.character(v))
  df_disp[[nm]] <- v
}
widths <- sapply(names(df_disp), function(nm) max(nchar(c(nm, df_disp[[nm]])), na.rm = TRUE))
# Cap very long AETERM for display? PROC PRINT would wrap/truncate by linesize; keep full but limit to 30 for readability?
# Keep full width but ensure at least header width
# Format rows
fmt_row <- function(vals, w) {
  paste(mapply(function(v, ww) sprintf(paste0("%-", ww, "s"), v), vals, w), collapse = "  ")
}
lines <- c(audit_title, "",
  fmt_row(names(df_disp), widths),
  fmt_row(sapply(widths, function(w) paste(rep("-", w), collapse = "")), widths)
)
for (i in seq_len(nrow(df_disp))) {
  lines <- c(lines, fmt_row(as.character(df_disp[i, ]), widths))
}
lines <- c(lines, "", paste0("N = ", nrow(audit12), " rows shown (first 12 of ", nrow(sdtm_ae), ")"))

txt_path <- file.path(out, "P009.txt")
writeLines(lines, txt_path)
# Also write .lst copy for benchmark expecting P009.lst
writeLines(lines, file.path(out, "P009.lst"))

cat("Wrote:", file.path(out, "ae.csv"), "\n")
cat("Wrote:", txt_path, "and P009.csv\n")
