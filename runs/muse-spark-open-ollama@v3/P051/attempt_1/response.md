```r
# P051_medication_listing.sas -> R translation
# Output: Target36 listing (medications)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- helpers ---
`%||%` <- function(a,b) ifelse(is.na(a) | a=="", b, a)

# SAS compbl: compress multiple blanks to single, trim leading/trailing
compbl <- function(x) {
  # vectorized
  x <- trimws(gsub("\\s+", " ", x, perl=TRUE))
  x
}

# SAS round half away from zero helper (not needed here but included)
round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x)*mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# Locate ADCM.xpt
# Paths from SAS macro variables (Windows absolute); try multiple candidates
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adcm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADCM.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adcm.xpt",
  Sys.getenv("RAWADAM"),
  file.path(Sys.getenv("RAWADAM"), "adcm.xpt"),
  file.path(Sys.getenv("RAWADAM"), "ADCM.xpt"),
  "/tmp/adam/adcm.xpt",
  "data/raw/cdisc-pilot/adam/adcm.xpt"
)
# Also try haven search via listing files
find_adcm <- function() {
  for (p in candidates) {
    if (!is.na(p) && p != "" && file.exists(p)) return(p)
  }
  # recursive search under common roots
  roots <- c("D:/meta_hackathon/sas2r-agent", ".", "/tmp", getwd())
  for (r in roots) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern="(?i)^adcm\\.xpt$", recursive=TRUE, full.names=TRUE)
      if (length(f)>0) return(f[1])
    }
  }
  return(NA)
}
adcm_path <- find_adcm()

# If not found, try pharmaversesdtm / admiral test data? But we need file.
# Fallback: try to locate via environment variable rawadam
if (is.na(adcm_path)) {
  # try OUT_DIR parent search
  adcm_path <- list.files(path="/", pattern="adcm.xpt", recursive=FALSE, full.names=TRUE)[1]
}

if (is.na(adcm_path) || !file.exists(adcm_path)) {
  stop(paste("ADCM.xpt not found. Searched candidates:", paste(candidates, collapse="; ")))
}

message("Reading ADCM from: ", adcm_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(tibble)

adcm_raw <- haven::read_xpt(adcm_path)

# Normalize column names to uppercase for SAS semantics, but keep original case for output
# haven preserves case as in XPT; we will work case-insensitive
names(adcm_raw) <- toupper(names(adcm_raw))

# Ensure expected columns exist
# SAS program uses: STUDYID USUBJID TRTA TRTAN SAFFL CMCLAS CMDECOD CMDOSE CMDOSU CMINDC CMSTDTC CMENDTC ADURN AENRF
# and also CMUNIT (numeric?) - handle both
# Check what columns exist
message("Columns in ADCM: ", paste(names(adcm_raw), collapse=", "))

# Helper to get column if exists else NA vector
get_col <- function(df, col, default=NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# Convert haven labelled to character/numeric where needed
# For character columns, haven may have labelled; convert to character
# Use zap_labels then as character for char vars
adcm_raw2 <- adcm_raw %>%
  mutate(across(everything(), ~ haven::zap_labels(.x)))

# Ensure character columns are character (haven may give numeric for some)
# Identify character-like columns
char_cols <- c("STUDYID","USUBJID","TRTA","SAFFL","CMCLAS","CMDECOD","CMDOSU","CMINDC","CMSTDTC","CMENDTC","AENRF","CMTRT","CMUNIT")
for (cc in char_cols) {
  if (cc %in% names(adcm_raw2)) {
    # if numeric, keep numeric; else convert to character
    if (is.numeric(adcm_raw2[[cc]])) {
      # leave numeric
    } else {
      adcm_raw2[[cc]] <- as.character(adcm_raw2[[cc]])
    }
  }
}

# Determine dose unit column: SAS code uses cmunit (numeric check ne .)
# In CDISC pilot ADCM, CMDOSU is character, CMDOSE numeric
# Some versions have CMUNIT? Check
dose_unit_col <- NULL
if ("CMUNIT" %in% names(adcm_raw2)) dose_unit_col <- "CMUNIT"
else if ("CMDOSU" %in% names(adcm_raw2)) dose_unit_col <- "CMDOSU"
else if ("CMDOSU" %in% toupper(names(adcm_raw))) dose_unit_col <- "CMDOSU"

# Also CMTRT may not exist; if not, use CMDECOD? But SAS listing uses CMTRT
# Check if CMTRT exists, if not create from CMDECOD or CMTRT equivalent
if (!"CMTRT" %in% names(adcm_raw2)) {
  # Try to find alternative: CMTRT is reported term, may be same as CMDECOD or CMTRT
  # If missing, set to CMDECOD
  if ("CMDECOD" %in% names(adcm_raw2)) {
    adcm_raw2$CMTRT <- adcm_raw2$CMDECOD
  } else {
    adcm_raw2$CMTRT <- NA_character_
  }
}

# Filter SAFFL='Y' (POPN)
# SAS: WHERE SAFFL='Y'
adcm_filt <- adcm_raw2 %>%
  filter(SAFFL == "Y")

# Sort by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD
# SAS sort: character ordering, missing first. R arrange does similar.
# Need to handle NA vs "" for CMSTDTC etc.
# Convert NA to "" for sorting to mimic SAS missing first? In SAS, missing char "" sorts first.
adcm_filt <- adcm_filt %>%
  mutate(
    CMSTDTC_sort = ifelse(is.na(CMSTDTC), "", CMSTDTC),
    CMENDTC_sort = ifelse(is.na(CMENDTC), "", CMENDTC),
    CMDECOD_sort = ifelse(is.na(CMDECOD), "", CMDECOD),
    TRTA_sort = ifelse(is.na(TRTA), "", TRTA),
    STUDYID_sort = ifelse(is.na(STUDYID), "", STUDYID),
    USUBJID_sort = ifelse(is.na(USUBJID), "", USUBJID)
  ) %>%
  arrange(TRTA_sort, STUDYID_sort, USUBJID_sort, CMSTDTC_sort, CMENDTC_sort, CMDECOD_sort) %>%
  select(-ends_with("_sort"))

# Create ADCM2 as per SAS Data step
# length StartStop $25 Doseunit $50; Period='Study Period';
# if CMSTDTC='' then CMSTDTC='NK';
# if CMENDTC='' then CMENDTC='NK';
# StartStop=compbl(CMSTDTC||'/'||CMENDTC);
# If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE);
# if AENRF='Ongoing' then Ongoing='Y'; else Ongoing='N';

# Need to handle CMDOSE numeric to character conversion as SAS would: default BEST12. ?
# SAS: CMDOSE || '('|| cmunit || ')'  -> if CMDOSE numeric, it is converted with BEST12. (right aligned, 12 width, trimmed by compbl)
# We emulate: if CMDOSE is NA -> "" ; else format without scientific, trim
format_sas_numeric <- function(x) {
  # SAS BEST12. default: if integer, no decimal; if decimal, show as is
  # Use format with minimal representation, similar to SAS PUT with BEST.
  # We'll use as.character with trimming
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # Remove trailing zeros? SAS BEST would keep decimal as needed
    # Use format with 12 significant digits? Simpler: if integer, no decimal
    # Use formatC with max digits
    # Check if v is numeric
    if (is.numeric(v)) {
      # SAS BEST12. would display up to 12 characters, with scientific if needed
      # For typical doses (e.g., 500, 81), simple
      # Use format with trim
      txt <- format(v, scientific=FALSE, trim=TRUE, nsmall=0, digits=12)
      # Remove trailing .0? format may produce "500"
      txt <- trimws(txt)
      # Remove trailing zeros after decimal if any
      # Keep as is
      return(txt)
    } else {
      return(trimws(as.character(v)))
    }
  }, USE.NAMES=FALSE)
}

# Prepare vectors
n <- nrow(adcm_filt)
CMSTDTC2 <- get_col(adcm_filt, "CMSTDTC")
CMENDTC2 <- get_col(adcm_filt, "CMENDTC")
CMDOSE2 <- get_col(adcm_filt, "CMDOSE")
# dose unit values
dose_unit_vals <- if (!is.null(dose_unit_col) && dose_unit_col %in% names(adcm_filt)) get_col(adcm_filt, dose_unit_col) else rep(NA_character_, n)
AENRF2 <- get_col(adcm_filt, "AENRF")

# For SAS logic: if CMSTDTC='' then 'NK' ; need to treat NA as ''
CMSTDTC2_char <- ifelse(is.na(CMSTDTC2), "", as.character(CMSTDTC2))
CMENDTC2_char <- ifelse(is.na(CMENDTC2), "", as.character(CMENDTC2))
CMSTDTC2_char[CMSTDTC2_char == ""] <- "NK"
CMENDTC2_char[CMENDTC2_char == ""] <- "NK"

StartStop <- compbl(paste0(CMSTDTC2_char, "/", CMENDTC2_char))
# Truncate to $25 as SAS length StartStop $25 (SAS would truncate to 25 chars)
StartStop <- substr(StartStop, 1, 25)

# DoseUnit logic: If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE);
# Need to interpret "cmunit ne ." as numeric missing check. If dose_unit_vals is character, check not NA and not ""
# If numeric, check not NA
is_dose_unit_present <- function(v) {
  if (is.numeric(v)) !is.na(v)
  else !is.na(v) & trimws(as.character(v)) != "" & toupper(trimws(as.character(v))) != "NA"
}
dose_present <- is_dose_unit_present(dose_unit_vals)

CMDOSE_char <- format_sas_numeric(CMDOSE2)
# For rows where CMDOSE is NA, CMDOSE_char is ""
# SAS compbl(CMDOSE) would be "" if missing
DoseUnit <- ifelse(dose_present,
                   compbl(paste0(CMDOSE_char, "(", trimws(as.character(dose_unit_vals)), ")")),
                   compbl(CMDOSE_char))
# Truncate to $50
DoseUnit <- substr(DoseUnit, 1, 50)
# If CMDOSE missing and dose unit present, SAS would produce "(unit)"? But our logic gives "(unit)" with empty dose
# Keep as is; compbl will handle

Ongoing <- ifelse(!is.na(AENRF2) & AENRF2 == "Ongoing", "Y", "N")

# Build ADCM2
ADCM2 <- adcm_filt %>%
  mutate(
    CMSTDTC = CMSTDTC2_char,
    CMENDTC = CMENDTC2_char,
    StartStop = StartStop,
    DoseUnit = DoseUnit,
    Ongoing = Ongoing,
    Period = "Study Period"
  )

# Ensure ADURN is numeric; keep as is
# Select columns for report: TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing
# Also keep STUDYID for sorting verification but not displayed
# SAS Columns: TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing
# define TRTA / noprint (used for BY)
# widths are for RTF; we will produce text rendering

# Prepare final dataset for CSV output (dataset)
# As per spec: write every output dataset as CSV with SAS variable names
# The program's dataset is ADCM2 (work.adcm and ADCM2)
# We will output ADCM2 as dataset CSV

# For CSV, keep original SAS variable names (uppercase)
# Include all variables from ADCM2 plus derived
dataset_out <- ADCM2 %>%
  # Ensure dates as YYYY-MM-DD where applicable; CMSTDTC/CMENDTC are already ISO or NK
  # ADURN numeric unrounded
  select(any_of(c("STUDYID","USUBJID","TRTA","TRTAN","SAFFL","CMCLAS","CMDECOD","CMTRT","CMDOSE","CMDOSU","CMUNIT","CMINDC","CMSTDTC","CMENDTC","ADURN","AENRF","Period","StartStop","DoseUnit","Ongoing")))

# Write dataset CSV
write.csv(dataset_out, file.path(out, "Target36_dataset.csv"), row.names=FALSE, na="")

# Also write ADCM work dataset equivalent (sorted filtered)
write.csv(adcm_filt, file.path(out, "ADCM.csv"), row.names=FALSE, na="")

# --- Build table/listing rendering ---
# PROC REPORT with BY TRTA, columns: TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing
# Titles:
# title "Safety Population"
# title2 "Study Period"
# title3 "Treatment: #byval1"  -> per BY group

# Need to produce text rendering and CSV of table body

# Define column headers as in SAS define statements
# USUBJID, CMTRT, cmdecod, cmclas, DoseUnit "Dose (unit)", CMINDC, StartStop "Start Date / Stop Date", ADURN "Dur (Days)", Ongoing
# TRTA is BY variable, not printed as column but as title

# Prepare report data sorted already by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD
report_cols <- c("USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")
# Ensure all exist
for (rc in report_cols) {
  if (!rc %in% names(ADCM2)) ADCM2[[rc]] <- NA_character_
}

# For display, convert NA to "" and handle ADURN formatting
ADCM2_disp <- ADCM2 %>%
  mutate(
    USUBJID = ifelse(is.na(USUBJID), "", as.character(USUBJID)),
    CMTRT = ifelse(is.na(CMTRT), "", as.character(CMTRT)),
    CMDECOD = ifelse(is.na(CMDECOD), "", as.character(CMDECOD)),
    CMCLAS = ifelse(is.na(CMCLAS), "", as.character(CMCLAS)),
    DoseUnit = ifelse(is.na(DoseUnit), "", as.character(DoseUnit)),
    CMINDC = ifelse(is.na(CMINDC), "", as.character(CMINDC)),
    StartStop = ifelse(is.na(StartStop), "", as.character(StartStop)),
    ADURN = ifelse(is.na(ADURN), "", as.character(ADURN)),
    Ongoing = ifelse(is.na(Ongoing), "", as.character(Ongoing))
  )

# Get unique TRTA values in sorted order
trta_levels <- unique(ADCM2_disp$TRTA)
# Ensure order as SAS would: character ordering, but our arrange already sorted
# Keep as encountered

# Build text rendering
# Use fixed-width columns approximating SAS widths (width=11 etc) but ensure readable
# We'll create a plain text file with titles, headers, and rows

# Column widths from define: USUBJID 11, CMTRT 10, cmdecod 10, cmclas 10, DoseUnit 6, CMINDC 10, StartStop 10, ADURN 6, Ongoing 7
# Total linesize 119, pagesize 41 (landscape)
# We'll use formatted table with separators

# Helper to wrap/flow text within width (SAS flow means wrap)
wrap_text <- function(txt, width) {
  if (is.na(txt) || txt=="") return("")
  # Simple: if nchar <= width, return txt, else wrap at width (break words)
  # For rendering, we will not actually wrap to multiple lines per cell to keep CSV simple;
  # Instead truncate/pad for text file
  txt
}

# Create CSV of table body (one row per observation, with TRTA column)
table_body <- ADCM2_disp %>%
  select(TRTA, all_of(report_cols)) %>%
  arrange(TRTA, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

# Write table body CSV
write.csv(table_body, file.path(out, "Target36.csv"), row.names=FALSE, na="")

# Build text file
txt_path <- file.path(out, "Target36.txt")
rtf_path <- file.path(out, "Target36.rtf") # also create rtf placeholder as text

con <- file(txt_path, open="wt")

# Titles
pop_label <- "Safety"
# SAS titles: JUSTIFY=L "&POPNLABEL Population" -> "Safety Population"
# title2 "Study Period"
# title3 "Treatment: #byval1" per BY group

# We'll write overall header then per BY group
cat("Safety Population\n", file=con)
cat("Study Period\n", file=con)
cat("\n", file=con)

# Header line for columns
# Define display headers
headers <- c(
  "USUBJID" = "Subject ID",
  "CMTRT" = "Medication",
  "CMDECOD" = "Generic Name",
  "CMCLAS" = "ATC term(s)",
  "DoseUnit" = "Dose (unit)",
  "CMINDC" = "Primary Indication",
  "StartStop" = "Start Date / Stop Date",
  "ADURN" = "Dur (Days)",
  "Ongoing" = "Cont. (Y/N)"
)
# Use column names as headers for simplicity, but include SAS labels
# We'll print a header row

# Function to format row with fixed widths
col_widths <- c(USUBJID=11, CMTRT=10, CMDECOD=10, CMCLAS=10, DoseUnit=12, CMINDC=15, StartStop=25, ADURN=10, Ongoing=7)
# Adjust to fit linesize 119: sum widths + separators ~ 110

format_row <- function(vals, widths, is_header=FALSE) {
  # vals named vector
  parts <- sapply(names(widths), function(nm) {
    v <- vals[[nm]]
    if (is.null(v)) v <- ""
    v <- as.character(v)
    w <- widths[[nm]]
    # Truncate if longer than width (SAS flow would wrap, but we truncate for single line)
    # For text rendering, we will pad/truncate
    if (nchar(v, type="width") > w) {
      # truncate
      v <- substr(v, 1, w)
    }
    # Pad to width
    sprintf(paste0("%-", w, "s"), v)
  })
  paste(parts, collapse=" | ")
}

# Write per TRTA BY group
for (trta in trta_levels) {
  cat(paste0("Treatment: ", trta, "\n"), file=con)
  cat(paste(rep("-", 119), collapse=""), "\n", file=con, sep="")
  # Header
  hdr_vals <- headers[names(col_widths)]
  # Ensure names match
  names(hdr_vals) <- names(col_widths)
  cat(format_row(as.list(hdr_vals), col_widths, TRUE), "\n", file=con, sep="")
  cat(paste(rep("-", 119), collapse=""), "\n", file=con, sep="")
  sub <- table_body %>% filter(TRTA == trta)
  for (i in seq_len(nrow(sub))) {
    row_vals <- as.list(sub[i, names(col_widths)])
    # Ensure all present
    for (nm in names(col_widths)) if (is.null(row_vals[[nm]])) row_vals[[nm]] <- ""
    cat(format_row(row_vals, col_widths), "\n", file=con, sep="")
  }
  cat("\n", file=con)
  # Footnote separator
  cat("\n", file=con)
}

# Add footer with notes about variables
cat("Notes:\n", file=con)
cat("Subject ID = USUBJID\n", file=con)
cat("Medication ATC term(s) = CMCLAS\n", file=con)
cat("Generic Name = CMDECOD\n", file=con)
cat("Dose (unit) = CMDOSE (CMDOSU/CMUNIT)\n", file=con)
cat("Primary Indication for Use = CMINDC\n", file=con)
cat("Start Date/Stop Date = CMSTDTC/CMENDTC (NK = Not Known)\n", file=con)
cat("Dur. (days) = ADURN\n", file=con)
cat("Cont. (Y/N) = display 'Y' if AENRF='Ongoing'; else display 'N'\n", file=con)
cat(paste0("Total records: ", nrow(table_body), "\n"), file=con)

close(con)

# Also create RTF file as copy of TXT (since RTF rendering requires r2rtf, but we provide text)
# Create a simple RTF wrapper
rtf_con <- file(rtf_path, open="wt")
cat("{\\rtf1\\ansi\\deff0\n", file=rtf_con)
cat("{\\fonttbl{\\f0 Courier;}}\n", file=rtf_con)
cat("\\viewkind4\\uc1\\pard\\f0\\fs18\n", file=rtf_con)
# Read txt and escape RTF special chars
txt_lines <- readLines(txt_path, warn=FALSE)
for (ln in txt_lines) {
  esc <- gsub("\\\\", "\\\\\\\\", ln)
  esc <- gsub("\\{", "\\\\{", esc)
  esc <- gsub("\\}", "\\\\}", esc)
  cat(paste0(esc, "\\par\n"), file=rtf_con)
}
cat("}\n", file=rtf_con)
close(rtf_con)

# Also write PNG placeholder for figure requirement? No figure in this program, but ensure no error
# Write a CSV of plotted data not needed

message("Done. Outputs written to ", out)
message("Rows in ADCM2: ", nrow(ADCM2))
message("TRTA levels: ", paste(trta_levels, collapse=", "))

# Verify counts for log
# Count numbers in output (46030 numbers expected) - just log
message("Table body rows: ", nrow(table_body))
```