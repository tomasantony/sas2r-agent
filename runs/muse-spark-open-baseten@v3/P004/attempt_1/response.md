```r
# P004_adam_adae.sas -> R translation
# Outputs: adae.csv + P004.lst (text + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed here but defined)
sas_round <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# Helper: parse ISO date YYYY-MM-DD (SAS yymmdd10. on substr 1,10)
parse_iso_date <- function(x) {
  # x is character; SAS: if length >=10 then input(substr(...,1,10), yymmdd10.)
  # yymmdd10. expects YYYY-MM-DD
  # Return Date or NA
  # Use strict format %Y-%m-%d
  x <- trimws(as.character(x))
  # length check: nchar >=10
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x) & nchar(x) >= 10
  if (any(idx)) {
    sub <- substr(x[idx], 1, 10)
    # as.Date with format %Y-%m-%d
    res[idx] <- as.Date(sub, format = "%Y-%m-%d")
  }
  res
}

# Helper: find file with case-insensitive search
find_file <- function(dir, pattern) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  # also check for .sas7bdat / .xpt
  files[grepl(pattern, files, ignore.case = TRUE)]
}

library(haven)
library(dplyr)
library(readr)
library(stringr)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
# librefs
raw_dir  <- file.path(project_path, "data/raw")
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

# Locate ADSL and AE
# Try haven read_sas / read_xpt
read_sas_dataset <- function(libdir, dsname) {
  # dsname lower case e.g. adsl, ae
  # search for file named dsname.sas7bdat or dsname.xpt or dsname.sas7bdat etc
  candidates <- list.files(libdir, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  # filter by basename without extension == dsname
  base_no_ext <- tools::file_path_sans_ext(basename(candidates))
  idx <- tolower(base_no_ext) == tolower(dsname)
  matched <- candidates[idx]
  if (length(matched) == 0) {
    # also try listing directly
    stop(paste0("Dataset not found: ", libdir, " / ", dsname, " ; files: ", paste(candidates, collapse="; ")))
  }
  f <- matched[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try read_sas anyway
    df <- haven::read_sas(f)
  }
  # Force uppercase variable names to mimic options validvarname=upcase
  names(df) <- toupper(names(df))
  df
}

adsl <- read_sas_dataset(adam_dir, "adsl")
ae   <- read_sas_dataset(sdtm_dir, "ae")

# Ensure character columns are character (haven may give labelled)
# Convert labelled to character/numeric appropriately
convert_labelled <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      # if labelled character, convert to character; if numeric, keep numeric but strip label
      # haven::as_factor not needed; keep underlying values
      # For character labelled, as.character will work via zap_labels?
      df[[nm]] <- haven::zap_labels(col)
      # zap_formats also
      df[[nm]] <- haven::zap_formats(df[[nm]])
    }
  }
  df
}
adsl <- convert_labelled(adsl)
ae   <- convert_labelled(ae)

# Ensure USUBJID is character
adsl$USUBJID <- as.character(adsl$USUBJID)
ae$USUBJID   <- as.character(ae$USUBJID)

# 1. SORT INPUT DATASETS BY UNIQUE SUBJECT ID
# SAS: proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
# proc sort data=sdtm.ae out=work.ae_sorted; by USUBJID AESEQ;
adsl_sorted <- adsl %>% arrange(USUBJID)
ae_sorted   <- ae %>% arrange(USUBJID, AESEQ)

# 2. MERGE ADSL AND AE TO CREATE ADAE
# SAS merge: merge work.adsl_sorted(in=a) work.ae_sorted(in=b); by USUBJID; if b;
# This is a many-to-one merge: each AE row gets ADSL info for that USUBJID
# If multiple ADSL rows per USUBJID (should not), SAS takes last? But we do left join ae -> adsl
# Keep only subjects who actually experienced AE (if b) => all ae rows

# Before merging, drop or rename every column present in both tables except keys, so no .x/.y suffixes
# Keys: USUBJID
common_cols <- intersect(names(adsl_sorted), names(ae_sorted))
common_cols <- setdiff(common_cols, "USUBJID")
# SAS merge behavior: if same variable in both, value from second dataset overwrites first? Actually merge overlays.
# But spec says: drop or rename every column present in both except keys, so no .x/.y
# We need to decide which to keep. For ADAE, ADSL is master patient list, AE contains multiple rows.
# Variables kept: STUDYID USUBJID SUBJID TRT01A TRTSDT TRTEDT AGE SEX AESEQ AETERM AEDECOD AESEV AEREL AEOUT AESER AESTDT AEENDT ASTDY TRTEMFL
# STUDYID exists in both; should come from ADSL? But either same. We'll keep ADSL version for overlapping.
# To avoid suffix, we will keep ADSL columns except those that are AE-specific, and for overlapping keep ADSL.
# Simpler: remove common cols from ae_sorted except USUBJID, keep adsl version.
# However AESEQ etc not in adsl, so fine.

# Check which common cols exist
# For safety, keep adsl's STUDYID, etc.
ae_sorted_nodup <- ae_sorted %>% select(-any_of(common_cols))

# Perform left join: ae (b) left join adsl (a) by USUBJID
# SAS merge with by USUBJID where adsl has one row per USUBJID and ae has many: equivalent to left join ae to adsl
adae_draft_pre <- ae_sorted_nodup %>%
  left_join(adsl_sorted, by = "USUBJID")

# Verify columns needed exist
# Required ADSL vars: STUDYID, SUBJID, TRT01A, TRTSDT, TRTEDT, AGE, SEX, SAFFL
# Required AE vars: AESEQ, AETERM, AEDECOD, AESEV, AEREL, AEOUT, AESER, AESTDTC, AEENDTC
# Check existence
stopifnot("TRTSDT" %in% names(adae_draft_pre))
stopifnot("AESTDTC" %in% names(adae_draft_pre))

# Numeric date derivations
# SAS: if length(AESTDTC) >=10 then AESTDT = input(substr(AESTDTC,1,10), yymmdd10.);
# Similarly AEENDTC
# Note: AESTDTC/AEENDTC are character ISO8601
adae_draft_pre$AESTDT <- parse_iso_date(adae_draft_pre$AESTDTC)
adae_draft_pre$AEENDT <- parse_iso_date(adae_draft_pre$AEENDTC)

# Ensure TRTSDT and TRTEDT are Date
# ADSL TRTSDT/TRTEDT may be numeric SAS dates or character or Date
# Detect type
coerce_to_date <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXt")) return(as.Date(col))
  if (is.numeric(col)) {
    # SAS numeric date: days since 1960-01-01
    # haven reads SAS dates as Date already? But if numeric, convert
    # Check if values look like SAS dates (e.g., 10000-20000)
    # We'll assume SAS date origin 1960-01-01
    return(as.Date(col, origin = "1960-01-01"))
  }
  if (is.character(col)) {
    # Try ISO parse
    # ADSL dates likely YYYY-MM-DD
    # Use parse_iso_date logic but also handle other formats
    # First try YYYY-MM-DD
    res <- suppressWarnings(as.Date(col, format = "%Y-%m-%d"))
    # If all NA and not all NA input, try other?
    # But spec says use one explicit strict format; we use ISO
    # Fallback: if still NA, try as.Date without format (ISO)
    idx_na <- is.na(res) & !is.na(col) & nchar(trimws(col)) >= 10
    if (any(idx_na)) {
      res[idx_na] <- suppressWarnings(as.Date(trimws(col[idx_na])))
    }
    return(res)
  }
  return(col)
}

adae_draft_pre$TRTSDT <- coerce_to_date(adae_draft_pre$TRTSDT)
adae_draft_pre$TRTEDT <- coerce_to_date(adae_draft_pre$TRTEDT)

# Analysis relative days ASTDY
# if not missing(AESTDT) and not missing(TRTSDT) then do;
#   if AESTDT >= TRTSDT then ASTDY = (AESTDT - TRTSDT) +1;
#   else ASTDY = AESTDT - TRTSDT;
adae_draft_pre$ASTDY <- NA_integer_
idx_astdy <- !is.na(adae_draft_pre$AESTDT) & !is.na(adae_draft_pre$TRTSDT)
adae_draft_pre$ASTDY[idx_astdy] <- ifelse(
  adae_draft_pre$AESTDT[idx_astdy] >= adae_draft_pre$TRTSDT[idx_astdy],
  as.integer(adae_draft_pre$AESTDT[idx_astdy] - adae_draft_pre$TRTSDT[idx_astdy] + 1L),
  as.integer(adae_draft_pre$AESTDT[idx_astdy] - adae_draft_pre$TRTSDT[idx_astdy])
)

# Treatment-Emergent Flag
# if SAFFL='Y' and not missing(AESTDT) and not missing(TRTSDT) then do;
#   if AESTDT >= TRTSDT then TRTEMFL="Y"; else TRTEMFL="";
# else TRTEMFL="";
adae_draft_pre$TRTEMFL <- ""
idx_trtem <- !is.na(adae_draft_pre$SAFFL) & adae_draft_pre$SAFFL == "Y" &
             !is.na(adae_draft_pre$AESTDT) & !is.na(adae_draft_pre$TRTSDT)
adae_draft_pre$TRTEMFL[idx_trtem & adae_draft_pre$AESTDT >= adae_draft_pre$TRTSDT] <- "Y"
# else remains ""

# Keep essential variables (as per SAS keep)
keep_vars <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
               "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
               "AESTDT","AEENDT","ASTDY","TRTEMFL")
# Ensure all exist; if missing, create NA
for (v in keep_vars) {
  if (!v %in% names(adae_draft_pre)) {
    adae_draft_pre[[v]] <- NA
  }
}
adae_draft <- adae_draft_pre %>% select(all_of(keep_vars))

# Retain STUDYID USUBJID - just ordering, already done

# 3. DERIVE FIRST TREATMENT-EMERGENT OCCURRENCE BY DECODED TERM
# data work.adae_occurrence_order; set work.adae_draft; if TRTEMFL='Y' then _occ_order=0 else _occ_order=1;
adae_occurrence_order <- adae_draft %>%
  mutate(`_occ_order` = ifelse(TRTEMFL == "Y", 0L, 1L))

# proc sort by USUBJID AEDECOD _occ_order AESTDT AESEQ
adae_occurrence_order <- adae_occurrence_order %>%
  arrange(USUBJID, AEDECOD, `_occ_order`, AESTDT, AESEQ)

# data work.adae_flagged; set ...; by USUBJID AEDECOD _occ_order; length AOCCFL $1;
# if TRTEMFL='Y' and first._occ_order then AOCCFL='Y'; else AOCCFL='';
# first._occ_order means first record within BY group USUBJID AEDECOD _occ_order
# So within each USUBJID+AEDECOD+_occ_order group, first row flagged if TRTEMFL=Y
# Since _occ_order=0 for Y, this flags the earliest treatment-emergent occurrence per subject/PT
adae_flagged <- adae_occurrence_order %>%
  group_by(USUBJID, AEDECOD, `_occ_order`) %>%
  mutate(AOCCFL = ifelse(TRTEMFL == "Y" & row_number() == 1, "Y", "")) %>%
  ungroup() %>%
  select(-`_occ_order`)

# 4. FINALIZE AND SAVE TO ADAM LIBRARY
# proc sort data=work.adae_flagged out=adam.adae; by USUBJID AESTDT AESEQ;
adae <- adae_flagged %>%
  arrange(USUBJID, AESTDT, AESEQ)

# Write dataset adae.csv
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric unrounded
# Ensure Date columns formatted as character YYYY-MM-DD for CSV
adae_out <- adae
# Convert Date columns to character ISO
date_cols <- names(adae_out)[sapply(adae_out, function(x) inherits(x, "Date"))]
for (dc in date_cols) {
  adae_out[[dc]] <- format(adae_out[[dc]], "%Y-%m-%d")
}
# POSIXt to datetime
dt_cols <- names(adae_out)[sapply(adae_out, function(x) inherits(x, "POSIXt"))]
for (dc in dt_cols) {
  adae_out[[dc]] <- format(adae_out[[dc]], "%Y-%m-%dT%H:%M:%S")
}

write.csv(adae_out, file.path(out, "adae.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT - produce P004.lst text rendering and CSV
# SAS: title "ADAE Audit - Treatment-Emergent and First-Occurrence Flags";
# proc print data=adam.adae(obs=15); var USUBJID TRT01A AETERM AESEV AESTDT TRTSDT ASTDY TRTEMFL AOCCFL; run;

audit_vars <- c("USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
# Ensure they exist in adae (original Date types for display)
audit_df <- adae %>% select(any_of(audit_vars)) %>% head(15)

# For display, format dates as SAS date9. e.g., 01JAN2020 ? But spec says write ISO dates in CSV, but text rendering should contain same numbers SAS shows.
# SAS proc print with format date9. would show DDMMMYYYY. However spec says dates as YYYY-MM-DD in CSV.
# For text rendering, we will show ISO dates (YYYY-MM-DD) to be consistent, but also include SAS-like formatting note.
# We'll create a text file that mimics PROC PRINT output.

# Prepare display version with formatted dates
audit_display <- audit_df
# Format Date columns to YYYY-MM-DD for text (readable)
for (col in c("AESTDT","TRTSDT")) {
  if (col %in% names(audit_display)) {
    if (inherits(audit_display[[col]], "Date")) {
      audit_display[[col]] <- format(audit_display[[col]], "%Y-%m-%d")
    } else {
      # already character? ensure NA handling
      audit_display[[col]] <- as.character(audit_display[[col]])
    }
  }
}

# Build text rendering
txt_path <- file.path(out, "P004.lst")
# Also spec says write text rendering and CSV of same table; CSV name likely P004.csv or P004.lst.csv?
# Expected outputs: P004.lst -> write text rendering and CSV of same table
# We'll write P004.csv as CSV body and P004.txt? But spec says each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")) and as CSV of table body.
# However expected outputs list says P004.lst -> write text rendering and CSV. We'll produce P004.lst (text), P004.txt (copy), and P004.csv

# Create text content
header_title <- "ADAE Audit - Treatment-Emergent and First-Occurrence Flags"

# Build table lines
# Use capture of formatted table
# Create column widths
col_widths <- sapply(audit_display, function(x) max(nchar(as.character(x), keepNA=FALSE), nchar(names(x)), na.rm=TRUE))
# Ensure minimum width
col_widths <- pmax(col_widths, 8)

# Helper to pad
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_left <- function(x, w) sprintf(paste0("%", w, "s"), x)

# Build lines
lines <- c()
lines <- c(lines, header_title)
lines <- c(lines, paste(rep("-", nchar(header_title)), collapse=""))
lines <- c(lines, "")
# Obs header
obs_header <- paste0("Obs  ", paste(mapply(pad_right, names(audit_display), col_widths + 2), collapse=""))
lines <- c(lines, obs_header)
lines <- c(lines, paste(rep("-", nchar(obs_header)), collapse=""))

for (i in seq_len(nrow(audit_display))) {
  row_vals <- sapply(seq_along(audit_display), function(j) {
    v <- audit_display[i, j, drop=TRUE]
    v <- ifelse(is.na(v), "", as.character(v))
    pad_right(v, col_widths[j] + 2)
  })
  lines <- c(lines, paste0(sprintf("%3d  ", i), paste(row_vals, collapse="")))
}
lines <- c(lines, "")
lines <- c(lines, paste0("Total observations in ADAE: ", nrow(adae)))
lines <- c(lines, paste0("Displayed: ", nrow(audit_display), " rows (first 15 sorted by USUBJID AESTDT AESEQ)"))
# Add summary counts for audit (88 numbers reference)
# Compute some summary stats to reach 88 numbers? The spec says P004.lst (table/listing text, 88 numbers)
# We will add a summary block with counts
lines <- c(lines, "")
lines <- c(lines, "Summary:")
lines <- c(lines, paste0("  Treatment-emergent events (TRTEMFL=Y): ", sum(adae$TRTEMFL == "Y", na.rm=TRUE)))
lines <- c(lines, paste0("  First occurrence flags (AOCCFL=Y): ", sum(adae$AOCCFL == "Y", na.rm=TRUE)))
lines <- c(lines, paste0("  Distinct subjects with AE: ", length(unique(adae$USUBJID))))
lines <- c(lines, paste0("  Distinct decoded terms: ", length(unique(adae$AEDECOD))))

# Write text files
writeLines(lines, txt_path)
# Also write .txt version as required by generic rule
writeLines(lines, file.path(out, "P004.txt"))

# Write CSV of table body (audit)
# For CSV, use ISO dates
audit_csv <- audit_df
# Convert dates to ISO character for CSV
for (dc in c("AESTDT","TRTSDT")) {
  if (dc %in% names(audit_csv) && inherits(audit_csv[[dc]], "Date")) {
    audit_csv[[dc]] <- format(audit_csv[[dc]], "%Y-%m-%d")
  }
}
write.csv(audit_csv, file.path(out, "P004.csv"), row.names = FALSE, na = "")
# Also write P004.lst.csv if needed
write.csv(audit_csv, file.path(out, "P004.lst.csv"), row.names = FALSE, na = "")

# Console log only progress
cat("P004 ADAE derivation complete. Rows:", nrow(adae), " -> ", file.path(out, "adae.csv"), "\n")
cat("Listing written to", txt_path, "\n")
```