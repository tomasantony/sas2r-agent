```r
# P007_adam_adtte.sas -> R translation
# ADTTE Time to First Adverse Event

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (half up)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: find dataset file
find_dataset <- function(libpath, dsn) {
  dsn_low <- tolower(dsn)
  if (!dir.exists(libpath)) return(NA_character_)
  files <- list.files(libpath, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  # exact name match ignoring case and extension
  base_low <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_low == dsn_low)
  if (length(idx) > 0) {
    # prefer sas7bdat > xpt > csv
    prefs <- c("sas7bdat","xpt","sas7bdat","csv","parquet")
    ordered <- idx[order(match(tolower(tools::file_ext(files[idx])), prefs, nomatch = 99))]
    return(files[ordered[1]])
  }
  # fallback: contains
  idx2 <- which(grepl(dsn_low, tolower(basename(files)), fixed = TRUE))
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
  } else if (ext == "parquet") {
    df <- arrow::read_parquet(path)
  } else {
    df <- haven::read_sas(path)
  }
  # zap labels/formats but keep values; convert labelled to plain
  # haven labelled -> numeric/character
  df <- haven::zap_formats(df)
  # Convert haven_labelled to vector
  for (nm in names(df)) {
    if (haven::is_labelled(df[[nm]])) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # Ensure names are uppercase (SAS validvarname=upcase)
  names(df) <- toupper(names(df))
  as.data.frame(df, stringsAsFactors = FALSE)
}

# Helper: format dataframe for CSV output (dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS)
format_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    col <- df2[[nm]]
    if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
      df2[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
      df2[[nm]][is.na(col)] <- NA_character_
    } else if (inherits(col, "Date")) {
      df2[[nm]] <- format(col, "%Y-%m-%d")
      df2[[nm]][is.na(col)] <- NA_character_
    } else if (inherits(col, "haven_labelled")) {
      df2[[nm]] <- as.character(haven::zap_labels(col))
    }
  }
  df2
}

# Helper: SAS missing check
is_sas_missing <- function(x) {
  if (is.character(x)) is.na(x) | trimws(x) == ""
  else is.na(x)
}

# -------------------------------------------------------------------------
# Locate libraries
adam_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"
raw_path  <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw"

# Fallback to alternative raw locations if adam_path not found
if (!dir.exists(adam_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  if (dir.exists(alt1)) adam_path <- alt1
}
cat("Using adam_path:", adam_path, "\n")
cat("Using sdtm_path:", sdtm_path, "\n")

adsl_file <- find_dataset(adam_path, "adsl")
adae_file <- find_dataset(adam_path, "adae")

# Additional fallback: search under project_path
if (is.na(adsl_file) || is.null(adsl_file)) {
  adsl_file <- find_dataset("D:/meta_hackathon/sas2r-agent/data/derived/c2c", "adsl")
}
if (is.na(adae_file) || is.null(adae_file)) {
  adae_file <- find_dataset("D:/meta_hackathon/sas2r-agent/data/derived/c2c", "adae")
}

cat("ADSL file:", adsl_file, "\n")
cat("ADAE file:", adae_file, "\n")

adsl <- read_dataset(adsl_file)
adae <- read_dataset(adae_file)

if (is.null(adsl)) stop("ADSL dataset not found")
if (is.null(adae)) stop("ADAE dataset not found")

cat("ADSL rows:", nrow(adsl), "cols:", ncol(adsl), "\n")
cat("ADAE rows:", nrow(adae), "cols:", ncol(adae), "\n")

# Ensure required columns exist (uppercase)
# ADSL expected: USUBJID, STUDYID, TRT01P, TRT01A, TRTSDT, TRTEDT, EOSDT, SAFFL
# ADAE expected: USUBJID, AESTDT, TRTEMFL

# -------------------------------------------------------------------------
# 1. GET FIRST ADVERSE EVENT PER SUBJECT
# proc sort data=adam.adae out=work.adae_first; by USUBJID AESTDT; where TRTEMFL='Y';

# Filter where TRTEMFL = 'Y' (case-sensitive, SAS)
adae_first <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", , drop = FALSE]

# Ensure AESTDT is Date (haven reads SAS dates as Date)
# Sort by USUBJID AESTDT - SAS missing sorts first, character uppercase before lowercase
# Use order with na.last=FALSE to put NA first
if (!"AESTDT" %in% names(adae_first)) {
  stop("AESTDT not found in ADAE")
}
# SAS sort: character ordering uppercase before lowercase is default in R (ASCII)
# Use order()
ord <- order(adae_first$USUBJID, adae_first$AESTDT, na.last = FALSE)
adae_first <- adae_first[ord, , drop = FALSE]

# data work.ae_target; set work.adae_first; by USUBJID; if first.USUBJID; keep USUBJID AESTDT; rename AESTDT=EVENTDT;
# Take first per USUBJID
# Use dplyr-like but base R to avoid dependency issues
ae_target <- adae_first[!duplicated(adae_first$USUBJID), c("USUBJID", "AESTDT"), drop = FALSE]
names(ae_target)[names(ae_target) == "AESTDT"] <- "EVENTDT"

cat("AE_TARGET rows (first event per subject):", nrow(ae_target), "\n")

# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]

# data adam.adtte; merge work.adsl_sorted(in=a) work.ae_target(in=b); by USUBJID; if a and SAFFL='Y';
# Keep only SAFFL='Y' and where adsl exists (all in adsl_sorted)
# SAS: if a and SAFFL='Y' -> keep adsl records where SAFFL='Y', left join ae_target
adsl_saffl <- adsl_sorted[!is.na(adsl_sorted$SAFFL) & adsl_sorted$SAFFL == "Y", , drop = FALSE]
cat("ADSL SAFFL=Y rows:", nrow(adsl_saffl), "\n")

# Merge by USUBJID (left join)
adtte <- merge(adsl_saffl, ae_target, by = "USUBJID", all.x = TRUE, all.y = FALSE, sort = FALSE)
# Restore sort by USUBJID (SAS merge retains sorted order)
adtte <- adtte[order(adtte$USUBJID, na.last = FALSE), , drop = FALSE]

# Derive PARAMCD, PARAM, CNSR, AVAL
adtte$PARAMCD <- "TTFAEV"
adtte$PARAM <- "Time to First Adverse Event"

# Ensure date columns are Date type for arithmetic
# Convert TRTSDT, EOSDT, EVENTDT to Date if not already
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01, but haven already converted
    # If still numeric, assume SAS date
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character
  suppressWarnings(as.Date(x))
}
adtte$TRTSDT <- to_date(adtte$TRTSDT)
adtte$TRTEDT <- to_date(adtte$TRTEDT)
adtte$EOSDT <- to_date(adtte$EOSDT)
adtte$EVENTDT <- to_date(adtte$EVENTDT)

# Derive CNSR and AVAL per SAS logic
# if missing(TRTSDT) then CNSR=., AVAL=.;
# else if not missing(EVENTDT) then CNSR=0, AVAL=(EVENTDT-TRTSDT)+1;
# else CNSR=1, if not missing(EOSDT) then AVAL=(EOSDT-TRTSDT)+1 else AVAL=.;

n <- nrow(adtte)
adtte$CNSR <- rep(NA_real_, n)
adtte$AVAL <- rep(NA_real_, n)

for (i in seq_len(n)) {
  trtsdt <- adtte$TRTSDT[i]
  eventdt <- adtte$EVENTDT[i]
  eosdt <- adtte$EOSDT[i]
  if (is.na(trtsdt)) {
    adtte$CNSR[i] <- NA_real_
    adtte$AVAL[i] <- NA_real_
  } else if (!is.na(eventdt)) {
    adtte$CNSR[i] <- 0
    adtte$AVAL[i] <- as.numeric(eventdt - trtsdt) + 1
  } else {
    adtte$CNSR[i] <- 1
    if (!is.na(eosdt)) {
      adtte$AVAL[i] <- as.numeric(eosdt - trtsdt) + 1
    } else {
      adtte$AVAL[i] <- NA_real_
    }
  }
}

# Keep variables in SAS order: STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT
keep_vars <- c("STUDYID","USUBJID","TRT01P","TRT01A","TRTSDT","TRTEDT","EOSDT","PARAMCD","PARAM","CNSR","AVAL","EVENTDT")
# Ensure all keep_vars exist; if missing, create NA column
for (v in keep_vars) {
  if (!v %in% names(adtte)) {
    adtte[[v]] <- NA
    cat("Warning: variable", v, "not found, created as NA\n")
  }
}
adtte <- adtte[, keep_vars, drop = FALSE]

# Retain order already; sort by USUBJID as final (SAS merge order)
adtte <- adtte[order(adtte$USUBJID, na.last = FALSE), , drop = FALSE]

cat("ADTTE rows:", nrow(adtte), "\n")
print(head(adtte, 5))

# -------------------------------------------------------------------------
# Write dataset adtte.csv
adtte_out <- format_for_csv(adtte)
write.csv(adtte_out, file.path(out, "adtte.csv"), row.names = FALSE, na = "")
cat("Wrote", file.path(out, "adtte.csv"), "\n")

# -------------------------------------------------------------------------
# 4. VISUAL AUDIT - PROC PRINT obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT
audit_vars <- c("USUBJID","TRT01A","PARAM","CNSR","AVAL","EVENTDT","EOSDT")
# Ensure audit_vars exist
for (v in audit_vars) if (!v %in% names(adtte)) adtte[[v]] <- NA
audit_df <- adtte[1:min(15, nrow(adtte)), audit_vars, drop = FALSE]

# Create text rendering mimicking PROC PRINT
title_str <- "ADTTE Audit - Time to First Adverse Event"

# Build text lines
txt_lines <- c(
  title_str,
  paste(rep("-", nchar(title_str)), collapse = ""),
  "",
  paste0("Dataset: ADTTE (Time to First Adverse Event)  N=", nrow(adtte), " (SAFFL=Y)"),
  paste0("First ", nrow(audit_df), " observations displayed:"),
  ""
)

# Header line
header_fmt <- sprintf("%-4s %-18s %-12s %-35s %4s %6s %-12s %-12s",
                      "Obs","USUBJID","TRT01A","PARAM","CNSR","AVAL","EVENTDT","EOSDT")
txt_lines <- c(txt_lines, header_fmt)
txt_lines <- c(txt_lines, paste(rep("-", nchar(header_fmt)), collapse = ""))

# Data lines
for (i in seq_len(nrow(audit_df))) {
  row <- audit_df[i, ]
  # Format dates as YYYY-MM-DD or blank if NA
  eventdt_str <- ifelse(is.na(row$EVENTDT), "", format(row$EVENTDT, "%Y-%m-%d"))
  eosdt_str <- ifelse(is.na(row$EOSDT), "", format(row$EOSDT, "%Y-%m-%d"))
  cnsr_str <- ifelse(is.na(row$CNSR), ".", as.character(row$CNSR))
  aval_str <- ifelse(is.na(row$AVAL), ".", as.character(row$AVAL))
  # Truncate PARAM for display
  param_disp <- substr(row$PARAM, 1, 35)
  line <- sprintf("%4d %-18s %-12s %-35s %4s %6s %-12s %-12s",
                  i,
                  ifelse(is.na(row$USUBJID), "", as.character(row$USUBJID)),
                  ifelse(is.na(row$TRT01A), "", as.character(row$TRT01A)),
                  param_disp,
                  cnsr_str,
                  aval_str,
                  eventdt_str,
                  eosdt_str)
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, "", paste0("Total observations in ADTTE: ", nrow(adtte)))
txt_lines <- c(txt_lines, paste0("Censored (CNSR=1): ", sum(adtte$CNSR == 1, na.rm = TRUE),
                                 "  Events (CNSR=0): ", sum(adtte$CNSR == 0, na.rm = TRUE),
                                 "  Missing CNSR: ", sum(is.na(adtte$CNSR))))
# Summary stats for AVAL
aval_vals <- adtte$AVAL[!is.na(adtte$AVAL)]
if (length(aval_vals) > 0) {
  txt_lines <- c(txt_lines,
                 sprintf("AVAL (Time in days) - N=%d Mean=%.2f Median=%.1f Min=%d Max=%d",
                         length(aval_vals), mean(aval_vals), median(aval_vals), min(aval_vals), max(aval_vals)))
}

# Write text files: P007.txt, P007.lst, P007.lst.txt
writeLines(txt_lines, file.path(out, "P007.txt"))
writeLines(txt_lines, file.path(out, "P007.lst"))
writeLines(txt_lines, file.path(out, "P007.lst.txt"))
cat("Wrote listing text to", file.path(out, "P007.txt"), "\n")

# Write CSV of table body (audit print)
audit_csv <- format_for_csv(audit_df)
# Add Obs number
audit_csv <- cbind(Obs = seq_len(nrow(audit_csv)), audit_csv)
write.csv(audit_csv, file.path(out, "P007.csv"), row.names = FALSE, na = "")
# Also write P007.lst.csv for compatibility
write.csv(audit_csv, file.path(out, "P007.lst.csv"), row.names = FALSE, na = "")
cat("Wrote listing CSV to", file.path(out, "P007.csv"), "\n")

# Also write full audit data for figure CSV if needed (not required but helpful)
# Ensure adtte.csv already written

cat("Done.\n")
```