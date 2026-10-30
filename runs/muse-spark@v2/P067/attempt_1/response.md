```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_dir  <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

find_dataset <- function(dir, dsname) {
  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) stop(paste0("No files in ", dir))
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(dsname))
  if (length(idx) == 0) stop(paste0("Dataset ", dsname, " not found in ", dir, "; files: ", paste(basename(files), collapse=", ")))
  files[idx[1]]
}

read_dataset <- function(path) {
  message("Reading ", path)
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    as.data.frame(haven::read_xpt(path), stringsAsFactors = FALSE)
  } else if (grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    as.data.frame(haven::read_sas(path), stringsAsFactors = FALSE)
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    as.data.frame(readr::read_csv(path, show_col_types = FALSE), stringsAsFactors = FALSE)
  } else if (grepl("\\.xpt\\.gz$|\\.sas7bdat\\.gz$", path, ignore.case = TRUE)) {
    as.data.frame(haven::read_sas(path), stringsAsFactors = FALSE)
  } else {
    as.data.frame(haven::read_sas(path), stringsAsFactors = FALSE)
  }
}

raw_ae_path <- find_dataset(raw_dir, "raw_ae")
dm_path     <- find_dataset(sdtm_dir, "dm")

raw_ae <- read_dataset(raw_ae_path)
dm     <- read_dataset(dm_path)

message(paste0("raw_ae rows=", nrow(raw_ae), " cols=", paste(names(raw_ae), collapse=",")))
message(paste0("dm rows=", nrow(dm), " cols=", paste(names(dm), collapse=",")))

stopifnot("SUBJID" %in% names(raw_ae))
stopifnot("SUBJID" %in% names(dm))

# Write raw_ae counterpart (input dataset copy) as CSV with ISO dates
raw_ae_out <- raw_ae
# Strip haven labels/formats for clean CSV but keep values
for (nm in names(raw_ae_out)) {
  col <- raw_ae_out[[nm]]
  if (inherits(col, "labelled")) col <- haven::zap_labels(col)
  if (inherits(col, "POSIXt")) {
    col <- format(as.POSIXct(col), "%Y-%m-%dT%H:%M:%S")
  } else if (inherits(col, "Date")) {
    col <- format(as.Date(col), "%Y-%m-%d")
  }
  # haven labelled may still have class haven_labelled; coerce
  if (inherits(col, "haven_labelled")) col <- as.vector(col)
  raw_ae_out[[nm]] <- col
}
write.csv(raw_ae_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")
message("Wrote raw_ae.csv")

# ---- SAS sorts: proc sort by SUBJID (missing first, char uppercase before lowercase) ----
sas_order_idx <- function(x) {
  # missing first, then byte order; use xtfrm with C locale via order with radix
  order(x, na.last = FALSE, method = "radix")
}
raw_ae <- raw_ae[sas_order_idx(raw_ae$SUBJID), , drop = FALSE]
dm     <- dm[sas_order_idx(dm$SUBJID), , drop = FALSE]

# ---- Joins: drop overlapping non-key columns so no .x/.y (SAS second overwrites first) ----
common_cols <- intersect(names(raw_ae), names(dm))
common_cols <- setdiff(common_cols, "SUBJID")
if (length(common_cols) > 0) {
  message(paste0("Overlapping cols (dropping from raw_ae to mimic SAS second-wins): ", paste(common_cols, collapse=",")))
  raw_ae <- raw_ae[, setdiff(names(raw_ae), common_cols), drop = FALSE]
}

# Ensure key types match for merge; if different, coerce both to character
if (class(raw_ae$SUBJID) != class(dm$SUBJID)) {
  # only coerce if one is character and other numeric
  if (is.character(raw_ae$SUBJID) || is.character(dm$SUBJID)) {
    raw_ae$SUBJID <- as.character(raw_ae$SUBJID)
    dm$SUBJID <- as.character(dm$SUBJID)
  }
}

stopifnot("SUBJID" %in% names(raw_ae))
stopifnot("SUBJID" %in% names(dm))
# dm must have USUBJID and RFSTDTC for derivation
stopifnot("USUBJID" %in% names(dm))
stopifnot("RFSTDTC" %in% names(dm))
stopifnot(all(c("AETERM","AESER","AESEV","AESTDT","AEENDT") %in% names(raw_ae)))

# merge raw.raw_ae (in=a) sdtm.dm; by SUBJID; if a;  => left join
merged <- merge(raw_ae, dm, by = "SUBJID", all.x = TRUE, sort = FALSE)
# merge with sort=FALSE preserves raw_ae order (already sorted by SUBJID); re-sort by SUBJID to mimic SAS output order
merged <- merged[sas_order_idx(merged$SUBJID), , drop = FALSE]
stopifnot("USUBJID" %in% names(merged))
stopifnot("RFSTDTC" %in% names(merged))
stopifnot("AESTDT" %in% names(merged))

# ---- Helpers to convert SAS values to R Dates ----
to_date_from_sas <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(as.POSIXct(x)))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    xc <- trimws(as.character(x))
    # strict single format YYYY-MM-DD on first 10 chars (mimics INPUT(...,yymmdd10.))
    res <- as.Date(substr(xc, 1, 10), format = "%Y-%m-%d")
    # empty strings -> NA (as.Date gives NA, ok)
    return(res)
  }
  # fallback
  return(as.Date(x))
}

parse_rfstdtc <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(as.POSIXct(x)))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  if (is.character(x)) {
    xc <- trimws(as.character(x))
    xc[xc == ""] <- NA_character_
    return(as.Date(substr(xc, 1, 10), format = "%Y-%m-%d"))
  }
  return(as.Date(x))
}

aest_dt  <- to_date_from_sas(merged$AESTDT)
aeend_dt <- to_date_from_sas(merged$AEENDT)
# RFSTDTC is character ISO; INPUT(RFSTDTC,yymmdd10.) reads first 10 chars
rf_dt <- parse_rfstdtc(merged$RFSTDTC)

# AESTDTC = put(AESTDT, yymmdd10.); AEENDTC = put(AEENDT, yymmdd10.)
AESTDTC <- ifelse(is.na(aest_dt), NA_character_, format(aest_dt, "%Y-%m-%d"))
AEENDTC <- ifelse(is.na(aeend_dt), NA_character_, format(aeend_dt, "%Y-%m-%d"))

# ASTDY / AENDY
n <- nrow(merged)
ASTDY <- rep(NA_real_, n)
AENDY <- rep(NA_real_, n)
for (i in seq_len(n)) {
  if (!is.na(aest_dt[i])) {
    if (is.na(rf_dt[i])) {
      ASTDY[i] <- NA_real_
    } else if (aest_dt[i] >= rf_dt[i]) {
      ASTDY[i] <- as.integer(aest_dt[i] - rf_dt[i] + 1L)
    } else {
      ASTDY[i] <- as.integer(aest_dt[i] - rf_dt[i])
    }
  }
  if (!is.na(aeend_dt[i])) {
    if (is.na(rf_dt[i])) {
      AENDY[i] <- NA_real_
    } else if (aeend_dt[i] >= rf_dt[i]) {
      AENDY[i] <- as.integer(aeend_dt[i] - rf_dt[i] + 1L)
    } else {
      AENDY[i] <- as.integer(aeend_dt[i] - rf_dt[i])
    }
  }
}

merged$STUDYID <- "ABC-301"
merged$AESTDTC <- AESTDTC
merged$AEENDTC <- AEENDTC
merged$ASTDY <- ASTDY
merged$AENDY <- AENDY

# keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY
keep_vars <- c("STUDYID","USUBJID","SUBJID","AETERM","AESER","AESEV","AESTDTC","AEENDTC","ASTDY","AENDY")
for (v in keep_vars) {
  if (!v %in% names(merged)) {
    # if missing (e.g., type mismatch), create NA
    merged[[v]] <- NA
  }
}
ae <- merged[, keep_vars, drop = FALSE]

# Ensure character types for STUDYID/AESTDTC/AEENDTC, strip labels
for (nm in c("STUDYID","USUBJID","SUBJID","AETERM","AESER","AESEV","AESTDTC","AEENDTC")) {
  col <- ae[[nm]]
  if (inherits(col, "labelled")) col <- haven::zap_labels(col)
  if (inherits(col, "Date")) col <- format(as.Date(col), "%Y-%m-%d")
  if (inherits(col, "POSIXt")) col <- format(as.POSIXct(col), "%Y-%m-%dT%H:%M:%S")
  if (is.factor(col)) col <- as.character(col)
  ae[[nm]] <- as.character(col)
}
# ASTDY/AENDY numeric (format 8.)
ae$ASTDY <- as.numeric(ae$ASTDY)
ae$AENDY <- as.numeric(ae$AENDY)
# SUBJID may have been numeric originally; keep original type? SAS keep preserves type.
# If original SUBJID was numeric, restore numeric
orig_subjid <- NULL
# recover original SUBJID type from dm/raw: prefer raw original
# re-read type: if dm$SUBJID numeric and ae$SUBJID character numeric-like, coerce back
# Detect: if both sources numeric, coerce
# We check pre-coercion classes stored? Use dm_path reading again types: if dm SUBJID numeric, convert
# Simpler: if all non-missing ae$SUBJID look numeric and original dm SUBJID was numeric, coerce
# Check original dm object before coercion? dm$SUBJID may have been coerced; re-evaluate from file
# To avoid complexity, if SUBJID in raw_ae_out is numeric, use numeric
if ("SUBJID" %in% names(raw_ae_out) && is.numeric(raw_ae_out$SUBJID)) {
  ae$SUBJID <- suppressWarnings(as.numeric(ae$SUBJID))
}

# Final sort by SUBJID (SAS output sorted)
ae <- ae[sas_order_idx(ae$SUBJID), , drop = FALSE]

message(paste0("ae rows=", nrow(ae)))

write.csv(ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")
message("Wrote ae.csv")
```