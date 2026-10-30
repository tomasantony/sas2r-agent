```r
# P051_medication_listing.sas -> R translation
# Outputs: Target36 (listing of medications)

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("OUTDIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS compbl - compress multiple blanks to single, trim
compbl <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(gsub("\\s+", " ", as.character(x), perl = TRUE)))
}
# helper: SAS round half away from zero (not needed here but defined)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}
# helper: as character handling NA/empty
as_char <- function(x) {
  if (is.null(x)) return(NA_character_)
  as.character(x)
}

# --- locate input ---
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adcm_path <- file.path(rawadam, "adcm.xpt")
# fallback paths
if (!file.exists(adcm_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adcm.xpt"
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adcm.sas7bdat"
  if (file.exists(alt1)) adcm_path <- alt1 else if (file.exists(alt2)) adcm_path <- alt2
}
cat("Reading ADCM from:", adcm_path, "\n")

# --- read ADCM ---
# haven::read_xpt handles .xpt; haven::read_sas handles .sas7bdat
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

if (grepl("\\.sas7bdat$", adcm_path, ignore.case = TRUE)) {
  adcm_raw <- haven::read_sas(adcm_path)
} else {
  adcm_raw <- haven::read_xpt(adcm_path)
}
# remove haven labels/formats, convert to data.frame
adcm_raw <- haven::zap_formats(haven::zap_labels(adcm_raw))
adcm_raw <- as.data.frame(adcm_raw, stringsAsFactors = FALSE)

# Ensure column names are uppercase as in SAS (haven preserves case)
# Normalize names to uppercase for logic, but keep original for output
names(adcm_raw) <- toupper(names(adcm_raw))

# Check expected columns exist
# Variables: STUDYID, USUBJID, TRTA, TRTAN, SAFFL, CMCLAS, CMDECOD, CMDOSE, CMDOSU, CMINDC, CMSTDTC, CMENDTC, ADURN, AENRF, SAFFL, CMTRT, CMUNIT etc.
# Some pilot datasets use CMDOSU, some use CMUNIT; handle both
cat("Columns in ADCM:", paste(names(adcm_raw), collapse=", "), "\n")
cat("Rows in ADCM:", nrow(adcm_raw), "\n")

# --- SAS: proc sort out=work.adcm data=source.ADCM by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD where SAFFL='Y' ---
POPN <- "SAFFL"
# Filter
adcm_filt <- adcm_raw
if (POPN %in% names(adcm_filt)) {
  # SAFFL may be character 'Y'/'N' or 'Y' with blanks
  adcm_filt <- adcm_filt[trimws(as.character(adcm_filt[[POPN]])) == "Y" & !is.na(adcm_filt[[POPN]]), , drop = FALSE]
}
cat("Rows after SAFFL='Y' filter:", nrow(adcm_filt), "\n")

# Ensure sort keys exist; if missing create empty
for (k in c("TRTA","STUDYID","USUBJID","CMSTDTC","CMENDTC","CMDECOD")) {
  if (!k %in% names(adcm_filt)) adcm_filt[[k]] <- NA_character_
  # Convert to character for sorting (SAS character sort)
  # Keep NA as NA (SAS missing sorts first)
}

# SAS sorts with missing first, character ordering uppercase before lowercase (binary)
# Use order with na.last=FALSE
# For character, R order uses locale; to approximate SAS binary, use xtfrm with byte ordering via order()
# We'll use base order on character vectors directly
ord <- order(
  as.character(adcm_filt$TRTA),
  as.character(adcm_filt$STUDYID),
  as.character(adcm_filt$USUBJID),
  as.character(adcm_filt$CMSTDTC),
  as.character(adcm_filt$CMENDTC),
  as.character(adcm_filt$CMDECOD),
  na.last = FALSE, method = "radix"
)
adcm_sorted <- adcm_filt[ord, , drop = FALSE]

# --- Data ADCM2; ---
# length StartStop $25 Doseunit $50;
# Period ='Study Period';
# if CMSTDTC='' then CMSTDTC='NK';
# if CMENDTC='' then CMENDTC='NK';
# StartStop=compbl(CMSTDTC||'/'||CMENDTC);
# If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE);
# if AENRF='Ongoing' then Ongoing='Y'; else Ongoing='N';

ADCM2 <- adcm_sorted

# Ensure needed columns exist
for (col in c("CMSTDTC","CMENDTC","CMDOSE","CMDOSU","CMUNIT","AENRF","ADURN","CMTRT","CMDECOD","CMCLAS","CMINDC","TRTA","USUBJID","STUDYID","TRTAN","SAFFL")) {
  if (!col %in% names(ADCM2)) ADCM2[[col]] <- NA_character_
}

# Helper to normalize missing character: NA or "" -> ""
norm_char <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# Period
ADCM2$PERIOD <- "Study Period"

# Preserve original CMSTDTC/CMENDTC for StartStop logic after NK replacement
# SAS: if CMSTDTC='' then CMSTDTC='NK'; same for CMENDTC
# In R, treat NA or "" or only blanks as missing
is_missing_char <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# Replace
ADCM2$CMSTDTC <- as.character(ADCM2$CMSTDTC)
ADCM2$CMENDTC <- as.character(ADCM2$CMENDTC)
ADCM2$CMSTDTC[is_missing_char(ADCM2$CMSTDTC)] <- "NK"
ADCM2$CMENDTC[is_missing_char(ADCM2$CMENDTC)] <- "NK"

# StartStop = compbl(CMSTDTC||'/'||CMENDTC)
ADCM2$STARTSTOP <- compbl(paste0(ADCM2$CMSTDTC, "/", ADCM2$CMENDTC))
# Truncate to $25 as SAS length (if longer, SAS would truncate)
ADCM2$STARTSTOP <- substr(ADCM2$STARTSTOP, 1, 25)

# DoseUnit logic: SAS uses cmunit (numeric missing check) and CMDOSE
# In pilot ADaM, CMDOSU is character unit, CMDOSE numeric dose
# Implement: if unit exists and not missing then "DOSE (unit)" else "DOSE"
# Determine unit column: prefer CMUNIT if exists and non-empty, else CMDOSU
unit_col <- NULL
if ("CMUNIT" %in% names(ADCM2)) {
  # check if any non-missing non-blank
  tmp <- norm_char(ADCM2$CMUNIT)
  if (any(tmp != "")) unit_col <- "CMUNIT"
}
if (is.null(unit_col) && "CMDOSU" %in% names(ADCM2)) unit_col <- "CMDOSU"
if (is.null(unit_col) && "CMDOSU" %in% names(adcm_raw)) unit_col <- "CMDOSU"

# CMDOSE handling: may be numeric or character
dose_char <- as.character(ADCM2$CMDOSE)
# SAS numeric to character via BEST12. would produce e.g. "200" without extra spaces after compbl
# haven may have numeric; as.character gives "200"
# For NA dose, dose_char is NA -> treat as ""
dose_char[is.na(dose_char) | dose_char == "NA"] <- ""
dose_char <- trimws(dose_char)

if (!is.null(unit_col)) {
  unit_char <- as.character(ADCM2[[unit_col]])
  unit_char[is.na(unit_char)] <- ""
  unit_char <- trimws(unit_char)
  # SAS: If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE);
  # For character unit, equivalent to unit != ""
  has_unit <- unit_char != "" & !is.na(unit_char)
  # Build
  ADCM2$DOSEUNIT <- ifelse(has_unit,
                           compbl(paste0(dose_char, " (", unit_char, ")")),
                           compbl(dose_char))
} else {
  ADCM2$DOSEUNIT <- compbl(dose_char)
}
ADCM2$DOSEUNIT <- substr(ADCM2$DOSEUNIT, 1, 50)
# If dose was missing and no unit, DoseUnit may be "" -> keep as "" (SAS would have "")

# Ongoing
ADCM2$AENRF <- as.character(ADCM2$AENRF)
ADCM2$ONGOING <- ifelse(trimws(ADCM2$AENRF) == "Ongoing" & !is.na(ADCM2$AENRF), "Y", "N")

# Ensure ADURN is numeric (Dur Days)
# Keep as is; if character convert
# ADCM2$ADURN already exists

# --- Prepare report dataset (PROC REPORT columns) ---
# Columns TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing
# Keep order and names as in SAS (case-insensitive)
report_cols <- c("TRTA","USUBJID","CMTRT","CMDECOD","CMCLAS","DOSEUNIT","CMINDC","STARTSTOP","ADURN","ONGOING")
# Ensure all exist
for (c in report_cols) if (!c %in% names(ADCM2)) ADCM2[[c]] <- NA
report_df <- ADCM2[, report_cols, drop = FALSE]
# Rename to match SAS display names (keep uppercase except DoseUnit/StartStop/Ongoing as defined)
# For CSV, use SAS variable names: TRTA, USUBJID, CMTRT, CMDECOD, CMCLAS, DoseUnit, CMINDC, StartStop, ADURN, Ongoing
names(report_df) <- c("TRTA","USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")

# Sort already by TRTA etc., but ensure final order matches SAS by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD
# report_df is already sorted via ADCM2 order

# --- Write dataset CSV (ADCM2) ---
# As per rules, each output dataset as CSV with SAS variable names
# Write report dataset as Target36.csv (table body)
write.csv(report_df, file.path(out, "Target36.csv"), row.names = FALSE, na = "")

# Also write full ADCM2 for traceability as dataset output (optional)
# Use original SAS names (uppercase) for full dataset
# Convert dates to YYYY-MM-DD if Date type (they are character)
full_out <- ADCM2
# Ensure character columns are not factors
write.csv(full_out, file.path(out, "ADCM2.csv"), row.names = FALSE, na = "")

# --- Text rendering (mimic PROC REPORT with BY TRTA) ---
# Titles: Safety Population, Study Period, Treatment: #byval1
# Define column headers with widths as in SAS define statements
# We'll produce a plain-text file with titles and tab-separated rows grouped by TRTA

txt_path <- file.path(out, "Target36.txt")
rtf_path <- file.path(out, "Target36.rtf")  # also write .rtf as plain text for compatibility

# Build text lines
lines <- character(0)
# Get unique TRTA in sorted order (as appears)
trta_levels <- unique(report_df$TRTA)
# If TRTA has NA, handle
trta_levels <- trta_levels[order(trta_levels, na.last = TRUE)]

# Column header labels
header_labels <- c("Subject ID", "Medication", "Generic Name", "ATC term", "Dose (unit)", "Primary Indication", "Start Date / Stop Date", "Dur (Days)", "Cont. (Y/N)")
# Underlying column names for display (excluding TRTA which is BY)
display_cols <- c("USUBJID","CMTRT","CMDECOD","CMCLAS","DoseUnit","CMINDC","StartStop","ADURN","Ongoing")
# Widths from SAS define (approx)
col_widths <- c(11,10,10,10,6,10,10,6,7)

# Helper to format cell for text (truncate/pad)
format_cell <- function(x, width) {
  if (is.na(x) || is.null(x)) x <- ""
  x <- as.character(x)
  # Replace NA string
  x[x == "NA"] <- ""
  # Truncate to width for display (SAS flow would wrap, we truncate)
  # Keep full for CSV, but for txt we show full with tab separation
  x
}

# For each BY group
for (trta_val in trta_levels) {
  sub_df <- report_df[report_df$TRTA == trta_val | (is.na(report_df$TRTA) & is.na(trta_val)), , drop = FALSE]
  # Titles
  lines <- c(lines,
             "Safety Population",
             "Study Period",
             paste0("Treatment: ", ifelse(is.na(trta_val), "", trta_val)),
             paste(rep("-", 119), collapse=""),
             paste(header_labels, collapse=" | "),
             paste(rep("-", 119), collapse="")
  )
  # Rows
  for (i in seq_len(nrow(sub_df))) {
    row_vals <- sapply(display_cols, function(col) format_cell(sub_df[i, col], NA))
    # ADURN formatting: if numeric, show without decimals if integer
    # Keep as is
    line <- paste(row_vals, collapse=" | ")
    lines <- c(lines, line)
  }
  lines <- c(lines, "", "") # blank lines between BY groups
}

# Add footer info
lines <- c(lines,
           paste0("Total records: ", nrow(report_df)),
           paste0("Generated from ADCM where SAFFL='Y' sorted by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD")
)

# Write txt
writeLines(lines, con = txt_path, useBytes = TRUE)
# Also write rtf as same text (SAS produced RTF, we provide text rendering with .rtf extension)
writeLines(lines, con = rtf_path, useBytes = TRUE)

cat("Outputs written to:", out, "\n")
cat(" - Target36.csv rows:", nrow(report_df), "cols:", ncol(report_df), "\n")
cat(" - Target36.txt lines:", length(lines), "\n")
cat(" - ADCM2.csv rows:", nrow(full_out), "\n")
```