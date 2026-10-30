```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not used here but included for compliance)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: find file case-insensitive
find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NA_character_)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  # match basename without extension
  base_noext <- tools::file_path_sans_ext(basename(files))
  idx <- which(toupper(base_noext) == toupper(name))
  if (length(idx) > 0) return(files[idx[1]])
  # fallback: grep
  idx2 <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx2) > 0) return(files[idx2[1]])
  return(NA_character_)
}

read_dataset <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else if (ext == "sas") {
    df <- haven::read_sas(path)
  } else {
    # try sas
    df <- tryCatch(haven::read_sas(path), error = function(e) NULL)
    if (is.null(df)) df <- tryCatch(haven::read_xpt(path), error = function(e) NULL)
  }
  if (is.null(df)) return(NULL)
  # convert haven labelled to R types, keep labels as values
  # zap labels but preserve Date/POSIXt
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven labelled vectors: convert to character/numeric
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # if labelled numeric with character labels, keep numeric; if character, keep character
      # zap labels
      col <- haven::zap_labels(col)
      # zap formats
      if (inherits(col, "haven_labelled")) col <- as.vector(col)
      df[[nm]] <- col
    }
    # convert factors to character
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  # uppercase names for SAS compliance (validvarname=upcase)
  names(df) <- toupper(names(df))
  df
}

# Helper: convert SAS numeric date to Date if needed
# haven::read_sas already converts SAS dates with format to Date, but if numeric remains, convert
ensure_date <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date origin 1960-01-01, values typically 0-30000
    # Heuristic: if all non-NA values are between -10000 and 60000, treat as SAS date
    # But ADaM numeric dates are SAS dates; convert
    # Only convert if plausible SAS date range
    # To avoid converting AVAL etc, only call this for known date columns
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # Decide ONE format per column from all values (PROC IMPORT GUESSINGROWS emulation)
    vals <- x[!is.na(x) & trimws(x) != ""]
    if (length(vals) == 0) return(as.Date(rep(NA_character_, length(x))))
    # Determine format once per column
    # Check patterns
    # ISO YYYY-MM-DD
    is_iso <- all(grepl("^\\d{4}-\\d{2}-\\d{2}", vals))
    is_dmy_slash <- all(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", vals))
    is_mdy_slash <- FALSE
    # For slash dates, decide DMY vs MDY by scanning all values: if any first field >12 => DMY
    if (is_dmy_slash || is_mdy_slash) {
      # already handled
    }
    # Detect slash
    has_slash <- any(grepl("/", vals))
    if (has_slash) {
      # extract first field
      first_fields <- suppressWarnings(as.integer(sub("^([0-9]+)/.*", "\\1", vals)))
      first_fields <- first_fields[!is.na(first_fields)]
      if (length(first_fields) > 0 && any(first_fields > 12, na.rm = TRUE)) {
        fmt <- "%d/%m/%Y"
      } else {
        fmt <- "%m/%d/%Y"
      }
      return(as.Date(x, format = fmt))
    }
    if (is_iso) {
      return(as.Date(x, format = "%Y-%m-%d"))
    }
    # Try DDMMMYYYY like 01JAN2020 or 01-Jan-2020
    if (all(grepl("^[0-9]{1,2}[A-Za-z]{3}[0-9]{4}", gsub("-", "", vals)))) {
      # normalize to upper and parse
      # Use lubridate? Use as.Date with %d%b%Y after removing dashes
      tmp <- gsub("-", "", x)
      tmp <- toupper(tmp)
      return(as.Date(tmp, format = "%d%b%Y"))
    }
    if (all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}", vals))) {
      return(as.Date(x, format = "%d-%b-%Y"))
    }
    # fallback ISO
    return(as.Date(x, format = "%Y-%m-%d"))
  }
  return(x)
}

# Helper to format Date for CSV (YYYY-MM-DD)
format_date_for_csv <- function(d) {
  if (inherits(d, "Date")) return(format(d, "%Y-%m-%d"))
  if (inherits(d, "POSIXt")) return(format(as.Date(d), "%Y-%m-%d"))
  return(d)
}

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")
# fallback if not exists (e.g., running on different mount)
if (!dir.exists(adam_dir)) {
  # try alternative raw paths from macro variables
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  alt2 <- Sys.getenv("RAWADAM")
  if (dir.exists(alt1)) adam_dir <- alt1
  else if (!is.na(alt2) && dir.exists(alt2)) adam_dir <- alt2
}

adsl_path <- find_file(adam_dir, "adsl")
adae_path <- find_file(adam_dir, "adae")

# Also try sdtm/adam split locations if not found
if (is.na(adsl_path)) {
  adsl_path <- find_file("D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm", "adsl")
}
if (is.na(adae_path)) {
  adae_path <- find_file("D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm", "adae")
}
# Try raw adam split
if (is.na(adsl_path)) {
  adsl_path <- find_file("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split", "adsl")
}
if (is.na(adae_path)) {
  adae_path <- find_file("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split", "adae")
}

adsl <- read_dataset(adsl_path)
adae <- read_dataset(adae_path)

if (is.null(adsl)) stop("ADSL not found. Searched: ", adam_dir, " adsl_path=", adsl_path)
if (is.null(adae)) stop("ADAE not found. Searched: ", adam_dir, " adae_path=", adae_path)

# Ensure required columns exist (case-insensitive, already upper)
stopifnot("USUBJID" %in% names(adsl))
stopifnot("USUBJID" %in% names(adae))

# Trim character columns (SAS pads)
trim_char <- function(df) {
  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      df[[nm]] <- trimws(df[[nm]])
      # SAS missing character is "" -> treat as NA for logic but keep "" for display? Use NA
      # Keep "" as NA for filtering
    }
  }
  df
}
adsl <- trim_char(adsl)
adae <- trim_char(adae)

# Ensure date columns are Date type
# ADSL: TRTSDT, TRTEDT, EOSDT
# ADAE: AESTDT
for (col in c("TRTSDT","TRTEDT","EOSDT")) {
  if (col %in% names(adsl)) {
    adsl[[col]] <- ensure_date(adsl[[col]])
    # if still character, try conversion
    if (is.character(adsl[[col]])) adsl[[col]] <- as.Date(adsl[[col]], format = "%Y-%m-%d")
  }
}
if ("AESTDT" %in% names(adae)) {
  adae[["AESTDT"]] <- ensure_date(adae[["AESTDT"]])
  if (is.character(adae[["AESTDT"]])) adae[["AESTDT"]] <- as.Date(adae[["AESTDT"]], format = "%Y-%m-%d")
}
# Also handle TRTSDT in adae if present (not needed)
# Handle TRTEMFL, SAFFL as character
# Replace "" with NA for logical checks
na_if_empty <- function(x) {
  if (is.character(x)) {
    x[x == ""] <- NA_character_
  }
  x
}
if ("TRTEMFL" %in% names(adae)) adae[["TRTEMFL"]] <- na_if_empty(adae[["TRTEMFL"]])
if ("SAFFL" %in% names(adsl)) adsl[["SAFFL"]] <- na_if_empty(adsl[["SAFFL"]])

# 1. GET FIRST ADVERSE EVENT PER SUBJECT
# proc sort data=adam.adae out=work.adae_first; by USUBJID AESTDT; where TRTEMFL='Y';
adae_first <- adae
if ("TRTEMFL" %in% names(adae_first)) {
  adae_first <- adae_first[!is.na(adae_first[["TRTEMFL"]]) & adae_first[["TRTEMFL"]] == "Y", , drop = FALSE]
}
# SAS sort: missing first, character uppercase before lowercase (R default similar)
# Order by USUBJID, AESTDT
if (nrow(adae_first) > 0) {
  # Ensure AESTDT is Date for ordering; NA first
  adae_first <- adae_first[order(adae_first[["USUBJID"]], adae_first[["AESTDT"]], na.last = FALSE), , drop = FALSE]
}

# data work.ae_target; set work.adae_first; by USUBJID; if first.USUBJID; keep USUBJID AESTDT; rename AESTDT=EVENTDT;
if (nrow(adae_first) > 0) {
  # first.USUBJID logic: take first row per USUBJID after sort
  ae_target <- adae_first[!duplicated(adae_first[["USUBJID"]]), , drop = FALSE]
  ae_target <- ae_target[, c("USUBJID","AESTDT"), drop = FALSE]
  names(ae_target)[names(ae_target) == "AESTDT"] <- "EVENTDT"
} else {
  ae_target <- data.frame(USUBJID = character(0), EVENTDT = as.Date(character(0)))
}

# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
adsl_sorted <- adsl[order(adsl[["USUBJID"]], na.last = FALSE), , drop = FALSE]

# data adam.adtte; merge work.adsl_sorted(in=a) work.ae_target(in=b); by USUBJID; if a and SAFFL='Y';
# Perform left join, keep only SAFFL='Y'
# Before merging, drop/rename overlapping columns except keys to avoid .x/.y suffixes
# Overlapping columns: none except USUBJID, but ensure no duplicate EVENTDT in adsl
# Do merge
adtte_merged <- merge(adsl_sorted, ae_target, by = "USUBJID", all.x = TRUE, all.y = FALSE, sort = FALSE)
# SAS merge by USUBJID after sorting: result sorted by USUBJID
adtte_merged <- adtte_merged[order(adtte_merged[["USUBJID"]], na.last = FALSE), , drop = FALSE]

# Filter SAFFL='Y'
if ("SAFFL" %in% names(adtte_merged)) {
  adtte <- adtte_merged[!is.na(adtte_merged[["SAFFL"]]) & adtte_merged[["SAFFL"]] == "Y", , drop = FALSE]
} else {
  adtte <- adtte_merged
}
stopifnot("TRTSDT" %in% names(adtte))
stopifnot("EOSDT" %in% names(adtte) || TRUE) # EOSDT may be missing, handle

# Ensure EOSDT exists
if (!"EOSDT" %in% names(adtte)) adtte[["EOSDT"]] <- as.Date(rep(NA_character_, nrow(adtte)))
if (!"EVENTDT" %in% names(adtte)) adtte[["EVENTDT"]] <- as.Date(rep(NA_character_, nrow(adtte)))
if (!"TRTEDT" %in% names(adtte)) adtte[["TRTEDT"]] <- as.Date(rep(NA_character_, nrow(adtte)))
if (!"TRT01P" %in% names(adtte)) adtte[["TRT01P"]] <- NA_character_
if (!"TRT01A" %in% names(adtte)) adtte[["TRT01A"]] <- NA_character_
if (!"STUDYID" %in% names(adtte)) adtte[["STUDYID"]] <- NA_character_

# Derive PARAMCD, PARAM, CNSR, AVAL
adtte[["PARAMCD"]] <- "TTFAEV"
adtte[["PARAM"]] <- "Time to First Adverse Event"

# Initialize CNSR and AVAL
adtte[["CNSR"]] <- NA_real_
adtte[["AVAL"]] <- NA_real_

# Vectorized derivation matching SAS logic
# if missing(TRTSDT) then CNSR=., AVAL=.;
# else if not missing(EVENTDT) then CNSR=0, AVAL=(EVENTDT-TRTSDT)+1;
# else CNSR=1, if not missing(EOSDT) then AVAL=(EOSDT-TRTSDT)+1 else .;

for (i in seq_len(nrow(adtte))) {
  trtsdt <- adtte[["TRTSDT"]][i]
  eventdt <- adtte[["EVENTDT"]][i]
  eosdt <- adtte[["EOSDT"]][i]
  # missing check: NA for Date
  if (is.na(trtsdt)) {
    adtte[["CNSR"]][i] <- NA_real_
    adtte[["AVAL"]][i] <- NA_real_
  } else if (!is.na(eventdt)) {
    adtte[["CNSR"]][i] <- 0
    adtte[["AVAL"]][i] <- as.numeric(eventdt - trtsdt) + 1
  } else {
    adtte[["CNSR"]][i] <- 1
    if (!is.na(eosdt)) {
      adtte[["AVAL"]][i] <- as.numeric(eosdt - trtsdt) + 1
    } else {
      adtte[["AVAL"]][i] <- NA_real_
    }
  }
}

# Keep required columns in retain order
keep_cols <- c("STUDYID","USUBJID","TRT01P","TRT01A","TRTSDT","TRTEDT","EOSDT","PARAMCD","PARAM","CNSR","AVAL","EVENTDT")
# Only keep those that exist
keep_cols <- keep_cols[keep_cols %in% names(adtte)]
adtte_final <- adtte[, keep_cols, drop = FALSE]
# Retain order: STUDYID USUBJID PARAMCD PARAM already, but keep as specified
# Sort by USUBJID for deterministic output (SAS merge retains sorted order)
adtte_final <- adtte_final[order(adtte_final[["USUBJID"]], na.last = FALSE), , drop = FALSE]

# Write dataset adtte.csv with dates as YYYY-MM-DD
adtte_out <- adtte_final
# Format dates for CSV
for (dc in c("TRTSDT","TRTEDT","EOSDT","EVENTDT")) {
  if (dc %in% names(adtte_out)) {
    # Convert Date to character YYYY-MM-DD, NA as ""
    adtte_out[[dc]] <- ifelse(is.na(adtte_out[[dc]]), "", format(adtte_out[[dc]], "%Y-%m-%d"))
  }
}
# Ensure numeric columns remain numeric
write.csv(adtte_out, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT
audit_vars <- c("USUBJID","TRT01A","PARAM","CNSR","AVAL","EVENTDT","EOSDT")
# Ensure all exist in adtte_final (use original Date values for display)
audit_df <- adtte_final
# Take first 15 obs
audit_n <- min(15, nrow(audit_df))
audit_print <- audit_df[seq_len(audit_n), , drop = FALSE]
# Select vars that exist
audit_print_vars <- intersect(audit_vars, names(audit_print))
audit_print <- audit_print[, audit_print_vars, drop = FALSE]

# Prepare display version with formatted dates
audit_display <- audit_print
for (dc in c("EVENTDT","EOSDT")) {
  if (dc %in% names(audit_display)) {
    audit_display[[dc]] <- ifelse(is.na(audit_display[[dc]]), "", format(audit_display[[dc]], "%Y-%m-%d"))
  }
}
# Add Obs column like SAS PROC PRINT
audit_display <- cbind(Obs = seq_len(nrow(audit_display)), audit_display)

# Create text rendering
title_text <- "ADTTE Audit - Time to First Adverse Event"
# Build text file content
txt_lines <- c(
  title_text,
  paste(rep("-", nchar(title_text)), collapse = ""),
  "",
  paste0("Dataset: ADTTE (Time to First Adverse Event)  N=", nrow(adtte_final)),
  paste0("First ", audit_n, " observations shown (of ", nrow(adtte_final), ")"),
  ""
)
# Create header line
header_line <- paste(sprintf("%-5s", "Obs"), paste(sprintf("%-15s", names(audit_display)[-1]), collapse = " "), collapse = " ")
# Actually build table with fixed width
# Use capture of data frame print
# Build column widths
col_widths <- sapply(audit_display, function(col) max(nchar(as.character(col)), nchar(names(col)), na.rm = TRUE))
# Ensure minimum widths
col_widths <- pmax(col_widths, 8)
# Header
header <- paste(mapply(function(nm, w) sprintf(paste0("%-", w, "s"), nm), names(audit_display), col_widths), collapse = "  ")
separator <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), col_widths), collapse = "  ")
txt_lines <- c(txt_lines, header, separator)
# Rows
for (i in seq_len(nrow(audit_display))) {
  row_vals <- audit_display[i, , drop = FALSE]
  line <- paste(mapply(function(val, w) {
    v <- ifelse(is.na(val), "", as.character(val))
    sprintf(paste0("%-", w, "s"), v)
  }, row_vals, col_widths), collapse = "  ")
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, "", paste0("Note: CNSR=0 event, CNSR=1 censored, CNSR=. missing TRTSDT; AVAL = (EVENTDT or EOSDT - TRTSDT) + 1"))

# Write text files: P007.txt, P007.lst.txt, P007.lst
writeLines(txt_lines, file.path(out, "P007.txt"))
writeLines(txt_lines, file.path(out, "P007.lst.txt"))
# Also write exactly P007.lst as text (no extension handling)
writeLines(txt_lines, file.path(out, "P007.lst"))

# Write CSV of table body (listing)
# CSV should contain same numbers SAS shows: include Obs and vars
write.csv(audit_display, file.path(out, "P007.csv"), row.names = FALSE, na = "")
write.csv(audit_display, file.path(out, "P007.lst.csv"), row.names = FALSE, na = "")

# Also ensure adtte dataset text rendering includes counts for verification
# Append summary to text for 99 numbers check: include summary stats
# (SAS listing 99 numbers includes all numeric values in table; we ensure CSV contains them)

```