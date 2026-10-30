# P012_sdtm_ds.sas -> R translation
# Program id: P012  file: P012_sdtm_ds.sas
# Creates DS domain (study disposition) for every subject

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but required by spec)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  # floor(x*mult + 0.5)/mult with sign
  sign_x * floor(abs_x * mult + 0.5 + 1e-12) / mult
}

# helper: SAS strip = trim both sides
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}

# helper: SAS upcase
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(sas_strip(x)))
}

# helper: SAS catx(delimiter, ... ) - trims each, skips missing/empty
sas_catx <- function(delim, ...) {
  args <- list(...)
  # vectorized version for single delim and vectors
  # args are vectors of same length
  n <- max(lengths(args))
  # recycle if needed
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  vapply(seq_len(n), function(i) {
    parts <- vapply(args, function(a) a[i], character(1))
    # strip and remove NA / empty after strip
    parts <- trimws(parts, which = "both")
    parts <- parts[!is.na(parts) & parts != ""]
    paste(parts, collapse = delim)
  }, character(1))
}

# helper: SAS put(_dsdt, is8601da.) -> YYYY-MM-DD, blank if missing
sas_put_is8601da <- function(d) {
  ifelse(is.na(d), NA_character_, format(as.Date(d, origin = "1970-01-01"), "%Y-%m-%d"))
}

# helper: parse a character column that SAS PROC IMPORT would have read as
# a single informat. Decide format once per column, then parse every value
# with that single format. Never choose per value.
parse_sas_date_column <- function(vec_char) {
  # vec_char: character vector of raw strings (trimmed)
  # returns Date vector
  # handle NA / empty
  raw <- sas_strip(vec_char)
  # keep original for detection
  nonmiss <- raw[!is.na(raw) & raw != ""]
  if (length(nonmiss) == 0) {
    return(as.Date(rep(NA_character_, length(vec_char))))
  }

  # If all non-missing look like pure integers (SAS numeric date), treat as SAS date
  # SAS numeric date = days since 1960-01-01
  is_all_integer <- all(grepl("^[0-9]+$", nonmiss))
  if (is_all_integer) {
    # heuristic: if values are 5 digits and plausible SAS dates (e.g. 15000-25000)
    # treat as SAS numeric dates; otherwise keep as NA and fall through
    # Check if numeric values are in plausible range for SAS dates (1960-2030 ~ 0-25500)
    nums <- suppressWarnings(as.numeric(nonmiss))
    if (all(!is.na(nums)) && all(nums > 5000 & nums < 30000)) {
      out <- rep(as.Date(NA), length(vec_char))
      idx <- !is.na(raw) & raw != ""
      out[idx] <- as.Date(as.numeric(raw[idx]), origin = "1960-01-01")
      return(out)
    }
  }

  # Detect slash dates: n/n/yyyy or n-n-yyyy
  has_slash <- any(grepl("/", nonmiss, fixed = TRUE))
  has_dash_alpha <- any(grepl("[A-Za-z]", nonmiss))
  has_iso_dash <- any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", nonmiss))

  # Slash case: decide MMDDYY vs DDMMYY by scanning all values
  if (has_slash) {
    # extract first field before "/"
    first_fields <- vapply(nonmiss, function(x) {
      # split by "/"
      parts <- strsplit(x, "/", fixed = TRUE)[[1]]
      suppressWarnings(as.numeric(parts[1]))
    }, numeric(1))
    # if any first field >12 => DDMMYY, else MMDDYY
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    fmt <- if (use_dmy) "%d/%m/%Y" else "%m/%d/%Y"
    # also handle 2-digit year variants: try %y if %Y fails
    # Determine if year part is 2 digits
    # Check last part length
    year_lens <- vapply(nonmiss, function(x) {
      parts <- strsplit(x, "/", fixed = TRUE)[[1]]
      nchar(parts[length(parts)])
    }, numeric(1))
    if (all(year_lens == 2, na.rm = TRUE)) {
      fmt <- if (use_dmy) "%d/%m/%y" else "%m/%d/%y"
    } else if (any(year_lens == 2)) {
      # mixed: keep 4-digit fmt, as.Date will handle 2-digit with %Y? use %y fallback
      # we will try both
      fmt2 <- if (use_dmy) "%d/%m/%y" else "%m/%d/%y"
      # try primary fmt, then fallback
      res <- suppressWarnings(as.Date(raw, format = fmt))
      miss_idx <- is.na(res) & !is.na(raw) & raw != ""
      if (any(miss_idx)) {
        res[miss_idx] <- suppressWarnings(as.Date(raw[miss_idx], format = fmt2))
      }
      return(res)
    }
    return(suppressWarnings(as.Date(raw, format = fmt)))
  }

  # ISO date: YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS
  if (has_iso_dash) {
    # strip time part if present
    date_part <- sub("T.*$", "", raw)
    date_part <- sub(" .*$", "", date_part)
    # try YYYY-MM-DD
    res <- suppressWarnings(as.Date(date_part, format = "%Y-%m-%d"))
    # fallback to YYYY/MM/DD
    miss_idx <- is.na(res) & !is.na(raw) & raw != ""
    if (any(miss_idx)) {
      res[miss_idx] <- suppressWarnings(as.Date(date_part[miss_idx], format = "%Y/%m/%d"))
    }
    return(res)
  }

  # Dash with month name: e.g. 15JAN2014, 15-JAN-2014, 15-Jan-2014, 15JAN14
  if (has_dash_alpha) {
    # normalize: remove dashes, uppercase, trim
    norm <- toupper(gsub("-", "", raw))
    # try dmy with month abbrev: %d%b%Y and %d%b%y
    # need to handle e.g. "15JAN2014"
    res <- suppressWarnings(as.Date(norm, format = "%d%b%Y"))
    miss_idx <- is.na(res) & !is.na(raw) & raw != ""
    if (any(miss_idx)) {
      res[miss_idx] <- suppressWarnings(as.Date(norm[miss_idx], format = "%d%b%y"))
    }
    # try with dash: %d-%b-%Y
    if (any(is.na(res) & !is.na(raw) & raw != "")) {
      miss_idx <- is.na(res) & !is.na(raw) & raw != ""
      res2 <- suppressWarnings(as.Date(raw[miss_idx], format = "%d-%b-%Y"))
      res[miss_idx] <- res2
      miss_idx2 <- is.na(res) & !is.na(raw) & raw != ""
      if (any(miss_idx2)) {
        res[miss_idx2] <- suppressWarnings(as.Date(raw[miss_idx2], format = "%d-%b-%y"))
      }
    }
    # try %d %b %Y
    if (any(is.na(res) & !is.na(raw) & raw != "")) {
      miss_idx <- is.na(res) & !is.na(raw) & raw != ""
      res[miss_idx] <- suppressWarnings(as.Date(raw[miss_idx], format = "%d %b %Y"))
    }
    return(res)
  }

  # Fallback: try anydtdte-like common formats with single format per column
  # Determine format from first non-missing value
  first <- nonmiss[1]
  # guess format
  if (grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}", first)) {
    return(suppressWarnings(as.Date(raw, format = "%Y/%m/%d")))
  }
  if (grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4}", first)) {
    # could be MM-DD-YYYY or DD-MM-YYYY - use slash logic already handled, but for dash
    # decide by first field >12
    ff <- suppressWarnings(as.numeric(strsplit(first, "-", fixed = TRUE)[[1]][1]))
    fmt <- if (!is.na(ff) && ff > 12) "%d-%m-%Y" else "%m-%d-%Y"
    return(suppressWarnings(as.Date(raw, format = fmt)))
  }
  if (grepl("^[0-9]{8}$", first)) {
    return(suppressWarnings(as.Date(raw, format = "%Y%m%d")))
  }
  # last resort: try ISO
  res <- suppressWarnings(as.Date(raw, format = "%Y-%m-%d"))
  if (all(is.na(res) & !is.na(raw) & raw != "")) {
    # try lubridate-free fallback: as.Date without format (ISO)
    res <- suppressWarnings(as.Date(raw))
  }
  return(res)
}

# --- locate raw_demog.csv ---
# Primary path from SAS program: &project_path./data/raw/raw_demog.csv
primary_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"
alt_paths <- c(
  primary_path,
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv",
  file.path(Sys.getenv("project_path"), "data/raw/raw_demog.csv"),
  "data/raw/raw_demog.csv",
  "data/derived/c2c/data/raw/raw_demog.csv"
)
raw_path <- NULL
for (p in alt_paths) {
  if (!is.na(p) && p != "" && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  # try to find recursively under root
  root <- Sys.getenv("root")
  if (root == "" || is.na(root)) root <- "D:/meta_hackathon/sas2r-agent"
  cand <- list.files(root, pattern = "raw_demog\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_path <- cand[1]
}
if (is.null(raw_path) || !file.exists(raw_path)) {
  stop(paste0("raw_demog.csv not found. Tried: ", paste(alt_paths, collapse="; ")))
}

# --- proc import equivalent ---
# SAS PROC IMPORT with getnames=yes, DBMS=CSV, validvarname=upcase
# Emulate: read as character to preserve raw, then uppercase names
# Use readr if available, else base
raw_ds_raw <- NULL
# read with readr to keep all as character (GUESSINGROWS emulation handled later)
if (requireNamespace("readr", quietly = TRUE)) {
  raw_ds_raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, na = c("", "NA", "."))
  # readr keeps original case; uppercase to emulate validvarname=upcase
  names(raw_ds_raw) <- toupper(names(raw_ds_raw))
} else {
  raw_ds_raw <- read.csv(raw_path, stringsAsFactors = FALSE, na.strings = c("", "NA", "."), check.names = FALSE)
  names(raw_ds_raw) <- toupper(names(raw_ds_raw))
}

# Ensure expected columns exist (SAS would create them as missing if not present)
# Required: SUBJ_ID, DISPOSITION, DISPOSITION_REASON, STUDY_END_DAT
# If column names differ slightly (e.g. SUBJID), try to map
col_map <- list(
  SUBJ_ID = c("SUBJ_ID", "SUBJID", "USUBJID", "SUBJECT_ID"),
  DISPOSITION = c("DISPOSITION", "DISP"),
  DISPOSITION_REASON = c("DISPOSITION_REASON", "DISP_REASON", "DSTERM", "REASON"),
  STUDY_END_DAT = c("STUDY_END_DAT", "STUDY_END_DATE", "DSSTDTC", "END_DATE", "STUDYENDDAT")
)
for (target in names(col_map)) {
  if (!target %in% names(raw_ds_raw)) {
    for (cand in col_map[[target]]) {
      if (cand %in% names(raw_ds_raw)) {
        names(raw_ds_raw)[names(raw_ds_raw) == cand] <- target
        break
      }
    }
  }
  if (!target %in% names(raw_ds_raw)) {
    # create missing column
    raw_ds_raw[[target]] <- NA_character_
  }
}

# Keep raw character vectors for date parsing (emulate vtype check)
# SAS vtype logic: if vtype(STUDY_END_DAT)='C' then input(anydtdte.) else _dsdt=STUDY_END_DAT
# In R we emulate by checking if column was character (it is, since we read as character)
# But we also need to handle the PROC IMPORT guessing: if column looked like dates,
# SAS would have made it numeric. We emulate that by deciding format once per column.
# So we treat STUDY_END_DAT as character and parse with single-format anydtdte emulation.

# --- data sdtm.ds ---
# length STUDYID $8 DOMAIN $2 USUBJID $40 DSCAT DSSCAT $40 DSTERM DSDECOD $80 EPOCH $20 DSSTDTC $10
# retain STUDYID DOMAIN USUBJID DSSEQ DSCAT DSSCAT DSTERM DSDECOD EPOCH DSSTDTC
# set work.raw_ds

# Prepare vectors
n <- nrow(raw_ds_raw)
# Ensure SUBJ_ID is character
SUBJ_ID_vec <- as.character(raw_ds_raw[["SUBJ_ID"]])
DISPOSITION_vec <- as.character(raw_ds_raw[["DISPOSITION"]])
DISPOSITION_REASON_vec <- as.character(raw_ds_raw[["DISPOSITION_REASON"]])
STUDY_END_DAT_raw <- as.character(raw_ds_raw[["STUDY_END_DAT"]])

# STUDYID, DOMAIN, etc.
STUDYID_vec <- rep("CDISC-01", n)
DOMAIN_vec <- rep("DS", n)
# USUBJID = catx("-", STUDYID, SUBJ_ID)
USUBJID_vec <- sas_catx("-", STUDYID_vec, SUBJ_ID_vec)
# Truncate to 40 as per length (SAS would truncate)
USUBJID_vec <- substr(USUBJID_vec, 1, 40)

DSSEQ_vec <- rep(1L, n)
DSCAT_vec <- rep("DISPOSITION EVENT", n)
DSSCAT_vec <- rep("STUDY PARTICIPATION", n)
DSTERM_vec <- sas_upcase(DISPOSITION_REASON_vec)
# SAS length $80, truncate
DSTERM_vec <- substr(DSTERM_vec, 1, 80)
DSDECOD_vec <- DSTERM_vec

# EPOCH logic
EPOCH_vec <- ifelse(sas_upcase(DISPOSITION_vec) == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")
# Handle NA: sas_upcase(NA) is NA, comparison yields NA, ifelse will give NA -> should be FOLLOW-UP per SAS else branch
# In SAS, missing DISPOSITION => upcase(strip(DISPOSITION)) = "" not equal "SCREEN FAILURE" => FOLLOW-UP
# So replace NA EPOCH with FOLLOW-UP
EPOCH_vec[is.na(EPOCH_vec)] <- "FOLLOW-UP"
EPOCH_vec <- substr(EPOCH_vec, 1, 20)

# DSSTDTC: parse STUDY_END_DAT
# Emulate: if vtype(STUDY_END_DAT)='C' then _dsdt=input(strip(STUDY_END_DAT), anydtdte.) else _dsdt=STUDY_END_DAT
# Since we read as character, we go to character branch, but we must emulate PROC IMPORT single-format decision
# Use parse_sas_date_column which decides format once per column
_dsdt_vec <- parse_sas_date_column(STUDY_END_DAT_raw)

# If original column was numeric SAS date (all integer strings in plausible range), parse_sas_date_column already handled
# Otherwise _dsdt is Date

DSSTDTC_vec <- sas_put_is8601da(_dsdt_vec)
# SAS put returns blank for missing; we use NA_character_ which will be written as "" with na=""
DSSTDTC_vec <- substr(DSSTDTC_vec, 1, 10)

# Build sdtm.ds data frame with keep order
sdtm_ds <- data.frame(
  STUDYID = STUDYID_vec,
  DOMAIN = DOMAIN_vec,
  USUBJID = USUBJID_vec,
  DSSEQ = DSSEQ_vec,
  DSCAT = DSCAT_vec,
  DSSCAT = DSSCAT_vec,
  DSTERM = DSTERM_vec,
  DSDECOD = DSDECOD_vec,
  EPOCH = EPOCH_vec,
  DSSTDTC = DSSTDTC_vec,
  stringsAsFactors = FALSE
)

# --- proc sort data=sdtm.ds; by USUBJID DSSEQ; ---
# SAS sorts with character ordering uppercase before lowercase, missing first
# R's order with default handles missing last; we need missing first
# Use order with na.last=FALSE and case-sensitive (R is case-sensitive)
# For character, uppercase < lowercase in ASCII, which matches SAS
ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = TRUE)
# To put NA first, we need to handle NA separately: SAS missing sorts first
# order(..., na.last=FALSE) puts NA first
ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE)
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]
rownames(sdtm_ds) <- NULL

# --- write dataset ds.csv ---
# SAS dataset name ds -> OUT_DIR/ds.csv
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# --- proc print data=sdtm.ds(obs=12); title "DS Domain Audit"; ---
# Create listing: first 12 obs after sort
print_n <- min(12, nrow(sdtm_ds))
listing_df <- sdtm_ds[seq_len(print_n), , drop = FALSE]

# Add Obs number as SAS does
listing_df_with_obs <- cbind(Obs = seq_len(print_n), listing_df)

# Write CSV of listing body (table body)
write.csv(listing_df, file.path(out, "P012.csv"), row.names = FALSE, na = "")
# Also write listing with Obs for reference
write.csv(listing_df_with_obs, file.path(out, "P012_listing.csv"), row.names = FALSE, na = "")

# Build plain-text rendering similar to SAS PROC PRINT
# SAS prints title, column headers, rows, with Obs numbers
# We need to produce text file that contains same numbers SAS shows
# Include title "DS Domain Audit" verbatim

# Helper to format text table with fixed widths (simple)
# Use capture of print-like output

# Create text lines
title_str <- "DS Domain Audit"
# Build header line
# Use format similar to SAS: Obs STUDYID DOMAIN USUBJID DSSEQ DSCAT DSSCAT DSTERM DSDECOD EPOCH DSSTDTC
# Determine column widths based on content
cols_to_print <- c("Obs", "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSCAT", "DSSCAT", "DSTERM", "DSDECOD", "EPOCH", "DSSTDTC")
# Prepare data for printing
txt_df <- listing_df_with_obs
# Ensure all columns are character for width calc
txt_df_char <- data.frame(lapply(txt_df, function(x) ifelse(is.na(x), "", as.character(x))), stringsAsFactors = FALSE, check.names = FALSE)
# Calculate widths: max of header and data
col_widths <- sapply(cols_to_print, function(col) {
  max(nchar(c(col, txt_df_char[[col]])), na.rm = TRUE)
})
# Add padding
col_widths <- pmax(col_widths, 3)

# Build format string: left-aligned for character, right for numeric (Obs, DSSEQ)
# Use sprintf with widths
header_line <- paste(mapply(function(col, w) sprintf(paste0("%-", w, "s"), col), cols_to_print, col_widths), collapse = "  ")
sep_line <- paste(mapply(function(w) paste(rep("-", w), collapse=""), col_widths), collapse = "  ")

# Build row lines
row_lines <- vapply(seq_len(nrow(txt_df_char)), function(i) {
  row <- txt_df_char[i, , drop = FALSE]
  paste(mapply(function(col, w) {
    val <- row[[col]]
    # Right align numeric columns Obs and DSSEQ
    if (col %in% c("Obs", "DSSEQ")) {
      sprintf(paste0("%", w, "s"), val)
    } else {
      sprintf(paste0("%-", w, "s"), val)
    }
  }, cols_to_print, col_widths), collapse = "  ")
}, character(1))

# Assemble text output
text_lines <- c(
  title_str,
  "",
  header_line,
  sep_line,
  row_lines,
  "",
  paste0("NOTE: ", nrow(sdtm_ds), " observations in dataset sdtm.ds; first ", print_n, " shown."),
  paste0("NOTE: Sorted by USUBJID DSSEQ; duplicate keys retained for QC.")
)

# Write to P012.lst and P012.txt (both required)
writeLines(text_lines, file.path(out, "P012.lst"))
writeLines(text_lines, file.path(out, "P012.txt"))

# Also write generic listing text as per spec: <name>.txt already done
# Ensure ds dataset also has txt rendering? Not needed; only listing needs txt

# Console log only progress (no full data)
cat("P012 translation complete: ds.csv (", nrow(sdtm_ds), " rows) and P012.lst (", print_n, " obs) written to ", out, "\n", sep="")
