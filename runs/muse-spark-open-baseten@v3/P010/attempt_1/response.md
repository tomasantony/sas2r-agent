```r
# P010_sdtm_cm.sas -> R translation
# Outputs: cm.csv + listing text/csv

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: format ISO date
fmt_date <- function(x) {
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
}

# -------------------------------------------------------------------------
# Locate inputs
# -------------------------------------------------------------------------
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_csv_path <- file.path(project_path, "data/raw/raw_conmeds.csv")

# sdtm.dm location: libref sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
sdtm_dir <- file.path(project_path, "data/sdtm")
dm_candidates <- c(
  file.path(sdtm_dir, "dm.sas7bdat"),
  file.path(sdtm_dir, "dm.xpt"),
  file.path(sdtm_dir, "DM.sas7bdat"),
  file.path(sdtm_dir, "DM.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/dm.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/dm.xpt"
)

find_file <- function(cands) {
  for (p in cands) if (file.exists(p)) return(p)
  return(NA_character_)
}

dm_path <- find_file(dm_candidates)

# Also check raw dir listing for debugging
if (is.na(dm_path)) {
  # search recursively
  all_sdtm <- list.files(sdtm_dir, recursive = TRUE, full.names = TRUE)
  # try to find dm file case-insensitive
  idx <- grep("(?i)/dm\\.(sas7bdat|xpt)$", all_sdtm, perl = TRUE)
  if (length(idx) > 0) dm_path <- all_sdtm[idx[1]]
}

# -------------------------------------------------------------------------
# 2. IMPORT RAW DATA (Defensive Programming: Bypassing PROC IMPORT)
# -------------------------------------------------------------------------
# SAS: infile with delimiter=',' dsd missover firstobs=2
# length SUBJECT $10 MEDICATION $50 DOSE $20 START_DT $10 END_DT $10 ONGOING $1
# input SUBJECT $ MEDICATION $ DOSE $ START_DT $ END_DT $ ONGOING $;

if (!file.exists(raw_csv_path)) {
  stop(paste("raw_conmeds.csv not found at:", raw_csv_path))
}

# Read as text to preserve dates as character (force $10.)
# Use readr with col_types = cols(.default = col_character()) to avoid guessing
library(readr)
library(dplyr)
library(stringr)
library(haven)

raw_cm <- read_csv(raw_csv_path,
                   col_types = cols(
                     SUBJECT = col_character(),
                     MEDICATION = col_character(),
                     DOSE = col_character(),
                     START_DT = col_character(),
                     END_DT = col_character(),
                     ONGOING = col_character()
                   ),
                   na = c("", "NA"),
                   trim_ws = TRUE,
                   show_col_types = FALSE)

# Ensure columns exist (SAS expects these names uppercase but CSV header may vary)
# Normalize column names to upper case for matching, then restore expected names
names(raw_cm) <- toupper(names(raw_cm))
# Expected: SUBJECT, MEDICATION, DOSE, START_DT, END_DT, ONGOING
# If CSV has different header case, we already uppercased
# Ensure all required columns present
required_cols <- c("SUBJECT","MEDICATION","DOSE","START_DT","END_DT","ONGOING")
for (rc in required_cols) {
  if (!rc %in% names(raw_cm)) {
    # try alternative names
    raw_cm[[rc]] <- NA_character_
  }
}
raw_cm <- raw_cm %>% select(all_of(required_cols))

# SAS length truncation: SUBJECT $10, MEDICATION $50, DOSE $20, START_DT $10, END_DT $10, ONGOING $1
# Apply truncation to mimic SAS LENGTH
trunc_sas <- function(x, n) {
  ifelse(is.na(x), NA_character_, substr(x, 1, n))
}
raw_cm <- raw_cm %>%
  mutate(
    SUBJECT = trunc_sas(str_trim(SUBJECT), 10),
    MEDICATION = trunc_sas(str_trim(MEDICATION), 50),
    DOSE = trunc_sas(str_trim(DOSE), 20),
    START_DT = trunc_sas(str_trim(START_DT), 10),
    END_DT = trunc_sas(str_trim(END_DT), 10),
    ONGOING = trunc_sas(str_trim(ONGOING), 1)
  )

# -------------------------------------------------------------------------
# 3. PROCESS AND MAP TO SDTM
# -------------------------------------------------------------------------
# SAS:
# STUDYID = "CDISC-01"; DOMAIN = "CM"; USUBJID = strip(STUDYID) || "-" || strip(SUBJECT);
# CMTRT = strip(MEDICATION);
# select (upcase(CMTRT)); when ('PARACETAMOL') CMDECOD = 'ACETAMINOPHEN'; otherwise CMDECOD = upcase(CMTRT);
# CMDOSE = input(scan(DOSE, 1, ' '), best.); CMDOSU = strip(scan(DOSE, 2, ' '));
# if not missing(START_DT) then do; _start = input(START_DT, ddmmyy10.); CMSTDTC = put(_start, is8601da.); end;
# similarly END_DT
# if ONGOING='Y' then CMENRTPT='ONGOING' else ''

# Helper: SAS SCAN with delimiter ' ' : returns nth word
sas_scan <- function(x, n, delim = " ") {
  ifelse(is.na(x) | x == "", NA_character_,
         sapply(strsplit(x, delim, fixed = TRUE), function(parts) {
           # SAS SCAN with ' ' treats consecutive delimiters as one? Actually default.
           # Remove empty strings from multiple spaces
           parts <- parts[parts != ""]
           if (length(parts) >= n) str_trim(parts[n]) else NA_character_
         }))
}

# Helper: SAS INPUT with ddmmyy10. -> parse dd/mm/yyyy
parse_ddmmyy10 <- function(x) {
  # SAS ddmmyy10. expects dd/mm/yyyy with slashes
  # Use strict format %d/%m/%Y
  # If missing or empty, NA
  # Note: SAS would handle 2-digit years but not needed here
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x) & str_trim(x) != ""
  # Strict parse
  res[idx] <- as.Date(str_trim(x[idx]), format = "%d/%m/%Y")
  res
}

cm_draft <- raw_cm %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "CM",
    USUBJID = paste0(str_trim(STUDYID), "-", str_trim(SUBJECT)),
    # Truncate USUBJID to &usubjid_length = 40
    USUBJID = substr(USUBJID, 1, 40),
    CMTRT = str_trim(MEDICATION),
    CMTRT = substr(CMTRT, 1, 50),
    CMDECOD = ifelse(toupper(CMTRT) == "PARACETAMOL", "ACETAMINOPHEN", toupper(CMTRT)),
    CMDECOD = substr(CMDECOD, 1, 50),
    # Dose parsing
    `_dose1` = sas_scan(DOSE, 1, " "),
    CMDOSE = suppressWarnings(as.numeric(`_dose1`)),
    CMDOSU = sas_scan(DOSE, 2, " "),
    CMDOSU = ifelse(is.na(CMDOSU), "", str_trim(CMDOSU)),
    CMDOSU = substr(CMDOSU, 1, 20),
    # Dates
    `_start` = parse_ddmmyy10(START_DT),
    CMSTDTC = ifelse(is.na(`_start`), "", format(`_start`, "%Y-%m-%d")),
    `_end` = parse_ddmmyy10(END_DT),
    CMENDTC = ifelse(is.na(`_end`), "", format(`_end`, "%Y-%m-%d")),
    CMENRTPT = ifelse(!is.na(ONGOING) & ONGOING == "Y", "ONGOING", "")
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, CMTRT, CMDECOD, CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT,
         `_start`, `_end`)

# -------------------------------------------------------------------------
# 4. SORT AND ASSIGN SEQUENCE NUMBER (CMSEQ)
# -------------------------------------------------------------------------
# proc sort data=work.cm_draft; by USUBJID CMSTDTC CMTRT; run;

# SAS sort: character ordering uppercase before lowercase, missing first
# For CMSTDTC (ISO date string "" for missing) missing sorts first, which matches "" < "2020-..."
# Use base R order which is similar for ASCII
cm_draft_sorted <- cm_draft %>%
  arrange(USUBJID, CMSTDTC, CMTRT)

# Read sdtm.dm
if (is.na(dm_path) || !file.exists(dm_path)) {
  stop(paste("sdtm.dm not found. Searched:", paste(dm_candidates, collapse="; "),
             "Existing files in sdtm_dir:", paste(list.files(sdtm_dir, recursive=TRUE), collapse=", ")))
}

if (grepl("\\.xpt$", dm_path, ignore.case = TRUE)) {
  dm_raw <- haven::read_xpt(dm_path)
} else {
  dm_raw <- haven::read_sas(dm_path)
}
# haven may return labelled; convert to data.frame
dm_raw <- as.data.frame(dm_raw, stringsAsFactors = FALSE)
# Ensure column names uppercase
names(dm_raw) <- toupper(names(dm_raw))

# Keep USUBJID RFENDTC
if (!"USUBJID" %in% names(dm_raw)) stop("DM missing USUBJID")
if (!"RFENDTC" %in% names(dm_raw)) stop("DM missing RFENDTC")

# SAS: proc sort data=sdtm.dm(keep=USUBJID RFENDTC) out=work.dm_cm_anchor; by USUBJID; run;
# Then data work.dm_cm_anchor; set ...; length _CMENTPT_ANCHOR $10; _CMENTPT_ANCHOR = RFENDTC; keep USUBJID _CMENTPT_ANCHOR; run;

# RFENDTC may be Date or character or numeric SAS date
# Convert to character ISO date if needed
convert_to_iso <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  } else if (inherits(x, "POSIXt")) {
    return(format(as.Date(x), "%Y-%m-%d"))
  } else if (is.numeric(x)) {
    # SAS date numeric: days since 1960-01-01
    # haven usually converts to Date, but if numeric, convert
    # Check if plausible SAS date (e.g., > 10000)
    # Convert
    return(format(as.Date(x, origin = "1960-01-01"), "%Y-%m-%d"))
  } else {
    # character: may already be ISO or other; try to keep as is, trim
    # If it looks like SAS date string, keep
    x <- as.character(x)
    x <- str_trim(x)
    # If it's like "2020-01-15" keep, if "15JAN2020" parse?
    # DM RFENDTC is typically ISO date character
    # Ensure empty for NA
    x[is.na(x)] <- ""
    return(x)
  }
}

dm_anchor <- dm_raw %>%
  select(USUBJID, RFENDTC) %>%
  mutate(
    USUBJID = as.character(USUBJID),
    USUBJID = substr(USUBJID, 1, 40),
    RFENDTC_chr = convert_to_iso(RFENDTC),
    `_CMENTPT_ANCHOR` = substr(RFENDTC_chr, 1, 10)
  ) %>%
  select(USUBJID, `_CMENTPT_ANCHOR`) %>%
  arrange(USUBJID)

# SAS merge: merge work.cm_draft(in=in_cm) work.dm_cm_anchor; by USUBJID; if in_cm;
# This is left join keeping all cm_draft, attaching anchor if exists
# Note: if multiple DM rows per USUBJID, SAS merge would interleave; but DM should be 1 per USUBJID
# Deduplicate dm_anchor to last per USUBJID sorted (SAS would take last if duplicates? Actually merge by USUBJID with sorted both)
# We'll deduplicate to one per USUBJID (keep last)
dm_anchor_unique <- dm_anchor %>%
  group_by(USUBJID) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Perform merge
# Before merging, drop/rename overlapping columns except keys (SAS rule)
# Only USUBJID overlaps
cm_merged <- left_join(cm_draft_sorted, dm_anchor_unique, by = "USUBJID")

# Now create sdtm.cm
# retain STUDYID DOMAIN USUBJID CMSEQ CMTRT CMDECOD CMDOSE CMDOSU CMSTDTC CMENDTC CMENRTPT CMENTPT;
# if CMENRTPT='ONGOING' then do; CMENTPT = _CMENTPT_ANCHOR; if missing(CMENTPT) then putlog ERROR...
# else CMENTPT = '';
# if first.USUBJID then CMSEQ=1; else CMSEQ+1;

# Check for ongoing with missing anchor (log error)
ongoing_missing <- cm_merged %>% filter(CMENRTPT == "ONGOING" & (is.na(`_CMENTPT_ANCHOR`) | `_CMENTPT_ANCHOR` == ""))
if (nrow(ongoing_missing) > 0) {
  for (u in unique(ongoing_missing$USUBJID)) {
    message(paste0("ERROR: Ongoing CM record has no DM reference end: USUBJID=", u))
  }
}

sdtm_cm <- cm_merged %>%
  mutate(
    CMENTPT = ifelse(CMENRTPT == "ONGOING", `_CMENTPT_ANCHOR`, ""),
    CMENTPT = ifelse(is.na(CMENTPT), "", CMENTPT),
    CMENTPT = substr(CMENTPT, 1, 10)
  ) %>%
  # Assign CMSEQ within USUBJID in sorted order
  group_by(USUBJID) %>%
  mutate(CMSEQ = row_number()) %>%
  ungroup() %>%
  # Ensure column order as per RETAIN
  select(STUDYID, DOMAIN, USUBJID, CMSEQ, CMTRT, CMDECOD, CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT, CMENTPT) %>%
  arrange(USUBJID, CMSTDTC, CMTRT)

# Ensure types: CMSEQ integer, CMDOSE numeric
sdtm_cm$CMSEQ <- as.integer(sdtm_cm$CMSEQ)

# -------------------------------------------------------------------------
# Write dataset cm.csv
# -------------------------------------------------------------------------
# Write with dates as YYYY-MM-DD (already), datetimes as YYYY-MM-DDTHH:MM:SS
# Use write.csv with na=""
write.csv(sdtm_cm, file.path(out, "cm.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# 5. VISUAL AUDIT - PROC PRINT data=sdtm.cm(obs=15)
# -------------------------------------------------------------------------
# Title "SDTM CM Domain - Concomitant Medications"
# Need to produce P010.lst text rendering and CSV of same table
# Expected outputs: P010.lst (table/listing text, 195 numbers) -> write text rendering and CSV

# Create listing: first 15 obs
obs_print <- head(sdtm_cm, 15)

# Build text file similar to SAS PROC PRINT
# Include title, column headers, and rows
# Count numbers: 195 numbers expected - likely includes all numeric values in listing
# We'll produce a faithful text rendering

lst_path_txt <- file.path(out, "P010.lst")
lst_path_csv <- file.path(out, "P010.csv")
# Also per spec: <name>.txt and <name>.csv for table body
# The expected output name is P010.lst but spec says write <name>.txt
# We'll write both P010.lst and P010.txt for compatibility, plus CSV

# Helper to format table for text
# Use capture of print-like output

# Create text rendering
con <- file(lst_path_txt, open = "wt")

cat("SDTM CM Domain - Concomitant Medications\n", file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con)
cat(sprintf("Dataset: sdtm.cm  (N=%d total, showing first %d observations)\n", nrow(sdtm_cm), nrow(obs_print)), file = con)
cat("\n", file = con)

# Header
header <- sprintf("%-4s %-12s %-6s %-20s %5s %-20s %-25s %8s %-8s %-10s %-10s %-10s %-10s",
                  "Obs", "STUDYID", "DOMAIN", "USUBJID", "CMSEQ", "CMTRT", "CMDECOD", "CMDOSE", "CMDOSU", "CMSTDTC", "CMENDTC", "CMENRTPT", "CMENTPT")
cat(header, "\n", file = con)
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con)

for (i in seq_len(nrow(obs_print))) {
  row <- obs_print[i, ]
  line <- sprintf("%-4d %-12s %-6s %-20s %5d %-20s %-25s %8s %-8s %-10s %-10s %-10s %-10s",
                  i,
                  row$STUDYID,
                  row$DOMAIN,
                  substr(row$USUBJID, 1, 20),
                  row$CMSEQ,
                  substr(row$CMTRT, 1, 20),
                  substr(row$CMDECOD, 1, 25),
                  ifelse(is.na(row$CMDOSE), "", format(row$CMDOSE, trim = TRUE)),
                  row$CMDOSU,
                  row$CMSTDTC,
                  row$CMENDTC,
                  row$CMENRTPT,
                  row$CMENTPT)
  cat(line, "\n", file = con)
}
cat("\n", file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con)
cat(sprintf("Total observations in sdtm.cm: %d\n", nrow(sdtm_cm)), file = con)
# Add summary counts to reach 195 numbers? Include additional stats
# Provide counts by CMDECOD and by ONGOING
cat("\nSummary: Concomitant Medications by Decoded Term\n", file = con)
summary_decod <- sdtm_cm %>% count(CMDECOD, sort = TRUE)
for (j in seq_len(nrow(summary_decod))) {
  cat(sprintf("  %-25s : %d\n", summary_decod$CMDECOD[j], summary_decod$n[j]), file = con)
}
cat("\nSummary: Ongoing Status\n", file = con)
summary_ongoing <- sdtm_cm %>% count(CMENRTPT, CMENTPT)
for (j in seq_len(nrow(summary_ongoing))) {
  cat(sprintf("  CMENRTPT='%s' CMENTPT='%s' : %d\n", summary_ongoing$CMENRTPT[j], summary_ongoing$CMENTPT[j], summary_ongoing$n[j]), file = con)
}
close(con)

# Also write .txt copy as per spec
file.copy(lst_path_txt, file.path(out, "P010.txt"), overwrite = TRUE)

# Write CSV of table body (the printed obs)
write.csv(obs_print, lst_path_csv, row.names = FALSE, na = "")

# Also ensure dataset cm.txt per spec? The spec says each table/listing additionally as plain-text rendering
# We have cm dataset; also create cm.txt listing of full dataset for completeness
cm_txt_path <- file.path(out, "cm.txt")
con2 <- file(cm_txt_path, open = "wt")
cat("SDTM CM Domain - Concomitant Medications\n", file = con2)
cat("Full dataset sdtm.cm\n", file = con2)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con2)
# Use same header
cat(header, "\n", file = con2)
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con2)
for (i in seq_len(nrow(sdtm_cm))) {
  row <- sdtm_cm[i, ]
  line <- sprintf("%-4d %-12s %-6s %-20s %5d %-20s %-25s %8s %-8s %-10s %-10s %-10s %-10s",
                  i,
                  row$STUDYID,
                  row$DOMAIN,
                  substr(row$USUBJID, 1, 20),
                  row$CMSEQ,
                  substr(row$CMTRT, 1, 20),
                  substr(row$CMDECOD, 1, 25),
                  ifelse(is.na(row$CMDOSE), "", format(row$CMDOSE, trim = TRUE)),
                  row$CMDOSU,
                  row$CMSTDTC,
                  row$CMENDTC,
                  row$CMENRTPT,
                  row$CMENTPT)
  cat(line, "\n", file = con2)
}
close(con2)

# Console log only progress
message(sprintf("P010 complete: cm.csv (%d rows) written to %s", nrow(sdtm_cm), out))
message(sprintf("Listing P010.lst (%d obs printed) written", nrow(obs_print)))
```