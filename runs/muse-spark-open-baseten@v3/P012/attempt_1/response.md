```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_demog.csv")

# fallback locations if primary not found (robustness, still uses absolute paths)
candidates <- c(
  raw_file,
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"
)
raw_file_found <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_file_found <- p; break }
}
if (is.null(raw_file_found)) {
  # last resort: search for raw_demog.csv under project_path
  all <- list.files(project_path, pattern = "raw_demog\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(all) > 0) raw_file_found <- all[1]
}
if (is.null(raw_file_found)) stop(paste("raw_demog.csv not found. Tried:", paste(candidates, collapse="; ")))
raw_file <- raw_file_found

# Read CSV as character to emulate PROC IMPORT scanning
# Use readr if available, otherwise base
df_raw <- NULL
if (requireNamespace("readr", quietly = TRUE)) {
  df_raw <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()), na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
  df_raw <- as.data.frame(df_raw, stringsAsFactors = FALSE)
} else {
  df_raw <- read.csv(raw_file, stringsAsFactors = FALSE, colClasses = "character", na.strings = c("", "NA"), check.names = FALSE)
}
# Force uppercase variable names (options validvarname=upcase)
names(df_raw) <- toupper(names(df_raw))

# Ensure expected columns exist (case-insensitive check already uppercased)
# SAS code uses: SUBJ_ID, DISPOSITION_REASON, DISPOSITION, STUDY_END_DAT
required <- c("SUBJ_ID", "DISPOSITION", "DISPOSITION_REASON", "STUDY_END_DAT")
for (col in required) {
  if (!col %in% names(df_raw)) {
    # try to find case-insensitive match already handled, if still missing create NA column
    df_raw[[col]] <- NA_character_
  }
}

# Helper: trim whitespace like SAS strip()
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(x, which = "both"))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  # catx trims each argument and skips missing/empty
  n <- max(lengths(args))
  args <- lapply(args, function(a) rep(a, length.out = n))
  vapply(seq_len(n), function(i) {
    vals <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v), which="both")
      if (v == "") return(NA_character_)
      v
    })
    vals <- vals[!is.na(vals)]
    if (length(vals)==0) return(NA_character_)
    paste(vals, collapse = sep)
  }, character(1))
}

# Emulate PROC IMPORT GUESSINGROWS for STUDY_END_DAT
# Decide ONE informat for the whole column, then parse every value with that single format
parse_study_end_dat <- function(x_char) {
  x_trim <- sas_strip(x_char)
  # treat empty string as NA (already NA from read)
  x_trim[x_trim == ""] <- NA_character_
  idx <- which(!is.na(x_trim))
  if (length(idx) == 0) return(rep(as.Date(NA), length(x_char)))
  vals <- x_trim[idx]

  # Detect if column is ISO YYYY-MM-DD (or YYYY/MM/DD)
  # If all non-missing match ^\d{4}[-/]\d{1,2}[-/]\d{1,2}
  is_iso <- grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}", vals)
  if (all(is_iso)) {
    # Determine separator
    # Use single format for whole column: if contains "-" use %Y-%m-%d else %Y/%m/%d
    if (any(grepl("-", vals, fixed=TRUE))) {
      return(suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d")))
    } else {
      return(suppressWarnings(as.Date(x_trim, format = "%Y/%m/%d")))
    }
  }

  # Detect slash dates n/n/yyyy
  has_slash <- grepl("/", vals, fixed = TRUE)
  if (any(has_slash)) {
    # For slash dates, SAS PROC IMPORT decides DDMMYY10. vs MMDDYY10. based on first field >12
    # Extract first field before "/"
    first_fields <- suppressWarnings(as.numeric(sub("/.*", "", vals[has_slash])))
    # If any first field >12 => DDMMYY, else MMDDYY
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    if (use_dmy) {
      # DDMMYY10. => %d/%m/%Y (SAS allows 1-2 digit day/month, 4 digit year)
      # Use single format for entire column
      res <- suppressWarnings(as.Date(x_trim, format = "%d/%m/%Y"))
      # Also try %d/%m/%y for 2-digit year if still NA? But SAS would have chosen one informat;
      # keep NA if not matching
      return(res)
    } else {
      res <- suppressWarnings(as.Date(x_trim, format = "%m/%d/%Y"))
      return(res)
    }
  }

  # Detect dash with month name e.g. 12-Jan-2020 or 12-JAN-2020
  has_alpha <- grepl("[A-Za-z]", vals)
  if (any(has_alpha)) {
    # Try %d-%b-%Y (e.g. 12-Jan-2020) - case-insensitive month
    # Normalize to title case for %b
    # R's %b is locale dependent but handles English abbreviations
    # Try upper/lower variations by converting to proper case
    # First try as is with %d-%b-%Y
    res <- suppressWarnings(as.Date(x_trim, format = "%d-%b-%Y"))
    # For remaining NA, try %d-%B-%Y
    miss <- is.na(res) & !is.na(x_trim)
    if (any(miss)) {
      res2 <- suppressWarnings(as.Date(x_trim[miss], format = "%d-%B-%Y"))
      res[miss] <- res2
    }
    # Try with spaces instead of dashes
    miss <- is.na(res) & !is.na(x_trim)
    if (any(miss)) {
      res2 <- suppressWarnings(as.Date(x_trim[miss], format = "%d %b %Y"))
      res[miss] <- res2
    }
    return(res)
  }

  # Detect dash numeric dates like DD-MM-YYYY or MM-DD-YYYY or YYYY-MM-DD already handled
  has_dash <- grepl("-", vals, fixed = TRUE)
  if (any(has_dash)) {
    # Similar logic to slash: check first field >12 => DMY
    first_fields <- suppressWarnings(as.numeric(sub("-.*", "", vals[has_dash])))
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    if (use_dmy) {
      return(suppressWarnings(as.Date(x_trim, format = "%d-%m-%Y")))
    } else {
      return(suppressWarnings(as.Date(x_trim, format = "%m-%d-%Y")))
    }
  }

  # Fallback: try ISO
  return(suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d")))
}

# Build SDTM DS domain
n <- nrow(df_raw)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("DS", n)
# USUBJID = catx("-", STUDYID, SUBJ_ID)
USUBJID <- sas_catx("-", STUDYID, df_raw$SUBJ_ID)
DSSEQ <- rep(1L, n)
DSCAT <- rep("DISPOSITION EVENT", n)
DSSCAT <- rep("STUDY PARTICIPATION", n)
DSTERM <- sas_upcase(sas_strip(df_raw$DISPOSITION_REASON))
DSDECOD <- DSTERM

# EPOCH logic: if upcase(strip(DISPOSITION)) = "SCREEN FAILURE" then "SCREENING" else "FOLLOW-UP"
disp_up <- sas_upcase(sas_strip(df_raw$DISPOSITION))
EPOCH <- ifelse(!is.na(disp_up) & disp_up == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")

# DSSTDTC: emulate vtype check + anydtdte + is8601da.
# In R we have character vector; we apply single-format parsing
_dsdt <- parse_study_end_dat(df_raw$STUDY_END_DAT)
DSSTDTC <- ifelse(is.na(_dsdt), NA_character_, format(_dsdt, "%Y-%m-%d"))

sdtm_ds <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  DSSEQ = DSSEQ,
  DSCAT = DSCAT,
  DSSCAT = DSSCAT,
  DSTERM = DSTERM,
  DSDECOD = DSDECOD,
  EPOCH = EPOCH,
  DSSTDTC = DSSTDTC,
  stringsAsFactors = FALSE
)

# Sort without deleting records: duplicate keys must remain visible to QC.
# SAS sorts with character ordering (uppercase before lowercase) and missing first
# R's order handles character ordering; ensure stable sort
ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE)
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]

# Write dataset ds.csv
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# Create listing: PROC PRINT obs=12 with title "DS Domain Audit"
# Prepare print data (first 12 obs)
print_n <- min(12, nrow(sdtm_ds))
print_df <- sdtm_ds[seq_len(print_n), , drop = FALSE]
# Add Obs column like SAS PROC PRINT
print_df_with_obs <- cbind(Obs = seq_len(print_n), print_df)

# Write CSV of table body (listing)
write.csv(print_df_with_obs, file.path(out, "P012.lst.csv"), row.names = FALSE, na = "")
# Also write generic P012.csv for compatibility
write.csv(print_df_with_obs, file.path(out, "P012.csv"), row.names = FALSE, na = "")

# Build plain-text rendering mimicking SAS PROC PRINT
# SAS PROC PRINT default output includes title, column headers, rows
# We produce a text file with same numbers (DSSEQ etc.)
txt_path <- file.path(out, "P012.lst.txt")
txt_path2 <- file.path(out, "P012.txt")
con <- file(txt_path, open = "wt")
con2 <- file(txt_path2, open = "wt")
header_lines <- c(
  "DS Domain Audit",
  paste(rep("-", 80), collapse = ""),
  ""
)
# Build column header line
col_names <- names(print_df_with_obs)
# Determine column widths (SAS-like: width = max(nchar(colname), max(nchar(values))))
col_widths <- sapply(seq_along(col_names), function(i) {
  col <- col_names[i]
  vals <- as.character(print_df_with_obs[[i]])
  vals[is.na(vals)] <- ""
  max(nchar(col, type="width"), max(nchar(vals, type="width"), na.rm=TRUE), na.rm=TRUE)
})
# Ensure minimum width
col_widths <- pmax(col_widths, 6)

# Helper to format row
format_row <- function(vals, widths) {
  # left align character, right align numeric (Obs, DSSEQ)
  formatted <- mapply(function(v, w, nm) {
    v <- ifelse(is.na(v), "", as.character(v))
    # Right align for Obs and DSSEQ
    if (nm %in% c("Obs", "DSSEQ")) {
      sprintf(paste0("%", w, "s"), v)
    } else {
      sprintf(paste0("%-", w, "s"), v)
    }
  }, vals, widths, col_names, SIMPLIFY = TRUE)
  paste(formatted, collapse = "  ")
}

writeLines(header_lines, con)
writeLines(header_lines, con2)

# Header row
hdr_line <- format_row(col_names, col_widths)
sep_line <- paste(sapply(col_widths, function(w) paste(rep("-", w), collapse="")), collapse="  ")
writeLines(hdr_line, con)
writeLines(sep_line, con)
writeLines(hdr_line, con2)
writeLines(sep_line, con2)

# Data rows
for (i in seq_len(nrow(print_df_with_obs))) {
  row_vals <- as.list(print_df_with_obs[i, , drop=FALSE])
  line <- format_row(row_vals, col_widths)
  writeLines(line, con)
  writeLines(line, con2)
}
writeLines("", con)
writeLines(paste0("NOTE: ", nrow(sdtm_ds), " observations in dataset sdtm.ds; first ", print_n, " displayed."), con)
writeLines("", con2)
writeLines(paste0("NOTE: ", nrow(sdtm_ds), " observations in dataset sdtm.ds; first ", print_n, " displayed."), con2)
close(con)
close(con2)

# Also ensure dataset ds.txt listing exists (optional)
# No further outputs required
```