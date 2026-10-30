# P067_ae.sas -> R translation
# Purpose: Create SDTM AE dataset
# Outputs: ae.csv, raw_ae.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: round half away from zero (SAS behavior) - not strictly needed here but included
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper to find dataset file in a library directory
find_dataset_file <- function(libpath, dsname) {
  # normalize path separators
  libpath <- gsub("\\\\", "/", libpath)
  candidates <- c(
    file.path(libpath, paste0(dsname, ".sas7bdat")),
    file.path(libpath, paste0(dsname, ".sas7bdat.gz")),
    file.path(libpath, paste0(dsname, ".xpt")),
    file.path(libpath, paste0(dsname, ".XPT")),
    file.path(libpath, paste0(dsname, ".xpt.gz")),
    file.path(libpath, paste0(tolower(dsname), ".sas7bdat")),
    file.path(libpath, paste0(tolower(dsname), ".xpt")),
    file.path(libpath, paste0(toupper(dsname), ".sas7bdat")),
    file.path(libpath, paste0(toupper(dsname), ".xpt")),
    file.path(libpath, paste0(dsname, ".csv")),
    file.path(libpath, paste0(tolower(dsname), ".csv"))
  )
  for (f in candidates) {
    if (file.exists(f)) return(f)
  }
  # also list directory case-insensitive search
  if (dir.exists(libpath)) {
    files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
    idx <- which(tolower(basename(tools::file_path_sans_ext(files))) == tolower(dsname))
    if (length(idx) > 0) {
      # prefer sas7bdat/xpt over csv
      pref <- files[idx]
      # order by extension preference
      ord <- order(match(tolower(tools::file_ext(pref)), c("sas7bdat", "xpt", "csv")))
      return(pref[ord[1]])
    }
  }
  return(NA_character_)
}

read_dataset <- function(libpath, dsname) {
  f <- find_dataset_file(libpath, dsname)
  if (is.na(f)) {
    cat("File not found for", dsname, "in", libpath, "\n")
    return(NULL)
  }
  cat("Reading", f, "\n")
  ext <- tolower(tools::file_ext(f))
  # handle .gz double extension
  if (ext == "gz") {
    ext2 <- tolower(tools::file_ext(tools::file_path_sans_ext(f)))
    ext <- ext2
  }
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try haven as fallback
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <<- haven::read_xpt(f))
  }
  # convert haven labelled to plain and handle tibble
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven may keep labelled class; strip
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::as_factor(df[[nm]], levels = "default")
      # if still labelled, convert to character/numeric
      if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- as.character(df[[nm]])
    }
  }
  return(df)
}

# Resolve library paths
# Primary paths from SAS librefs
raw_path_primary  <- "/bench/data/derived/wxzhai/raw"
sdtm_path_primary <- "/bench/data/derived/wxzhai/sdtm"
adam_path_primary <- "/bench/data/derived/wxzhai/adam"

# Fallback via env vars
root_env <- Sys.getenv("root")
if (root_env != "") {
  root_env <- gsub("\\\\", "/", root_env)
  raw_path_alt  <- file.path(root_env, "data/derived/wxzhai/raw")
  sdtm_path_alt <- file.path(root_env, "data/derived/wxzhai/sdtm")
  adam_path_alt <- file.path(root_env, "data/derived/wxzhai/adam")
} else {
  raw_path_alt <- raw_path_primary
  sdtm_path_alt <- sdtm_path_primary
  adam_path_alt <- adam_path_primary
}

# Also check rawadam/rawsdtm env
rawadam_env <- Sys.getenv("rawadam")
rawsdtm_env <- Sys.getenv("rawsdtm")
rawsplit_env <- Sys.getenv("rawsplit")
rawdir_env  <- Sys.getenv("rawdir")

candidate_raw_paths <- unique(c(raw_path_primary, raw_path_alt, rawadam_env, rawsplit_env, rawdir_env))
candidate_raw_paths <- candidate_raw_paths[candidate_raw_paths != "" & !is.na(candidate_raw_paths)]

candidate_sdtm_paths <- unique(c(sdtm_path_primary, sdtm_path_alt, rawsdtm_env))
candidate_sdtm_paths <- candidate_sdtm_paths[candidate_sdtm_paths != "" & !is.na(candidate_sdtm_paths)]

# Function to try multiple libpaths
read_from_candidates <- function(paths, dsname) {
  for (p in paths) {
    if (dir.exists(p) || file.exists(p)) {
      df <- read_dataset(p, dsname)
      if (!is.null(df)) return(df)
    }
  }
  # last resort: try reading directly from file path if dsname contains path
  return(NULL)
}

raw_ae <- read_from_candidates(candidate_raw_paths, "raw_ae")
if (is.null(raw_ae)) {
  # try alternative name ae in raw?
  raw_ae <- read_from_candidates(candidate_raw_paths, "ae")
}
sdtm_dm <- read_from_candidates(candidate_sdtm_paths, "dm")

if (is.null(raw_ae)) stop("raw.raw_ae dataset not found in any candidate path")
if (is.null(sdtm_dm)) stop("sdtm.dm dataset not found in any candidate path")

cat("raw_ae rows:", nrow(raw_ae), "cols:", ncol(raw_ae), "\n")
cat("sdtm.dm rows:", nrow(sdtm_dm), "cols:", ncol(sdtm_dm), "\n")

# Ensure SUBJID exists
stopifnot("SUBJID" %in% names(raw_ae))
stopifnot("SUBJID" %in% names(sdtm_dm))

# Convert SUBJID to character for consistent join (SAS character comparison case-sensitive)
raw_ae$SUBJID <- as.character(raw_ae$SUBJID)
sdtm_dm$SUBJID <- as.character(sdtm_dm$SUBJID)

# SAS sorts
raw_ae <- raw_ae[order(raw_ae$SUBJID), , drop = FALSE]
sdtm_dm <- sdtm_dm[order(sdtm_dm$SUBJID), , drop = FALSE]

# Before merging, drop or rename every column present in both tables except keys, so no .x/.y suffixes
common_cols <- intersect(names(raw_ae), names(sdtm_dm))
common_cols <- setdiff(common_cols, c("SUBJID"))
if (length(common_cols) > 0) {
  cat("Dropping overlapping columns from sdtm.dm:", paste(common_cols, collapse = ", "), "\n")
  sdtm_dm <- sdtm_dm[, setdiff(names(sdtm_dm), common_cols), drop = FALSE]
}

# Merge raw.raw_ae (in=a) sdtm.dm; by SUBJID; if a;
# Left join keep only subjects who have AE
# Use dplyr left_join
ae_merged <- dplyr::left_join(raw_ae, sdtm_dm, by = "SUBJID")

# Verify join produced rows (SAS would have same row count as raw_ae)
stopifnot(nrow(ae_merged) == nrow(raw_ae))
stopifnot("RFSTDTC" %in% names(ae_merged) || "RFSTDTC" %in% names(sdtm_dm) || "RFSTDTC" %in% names(raw_ae))

# Helper to convert SAS numeric date or character to Date
to_date <- function(x) {
  if (is.null(x)) return(as.Date(rep(NA, length(x))))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    # If values are large (e.g., > 30000) they are SAS dates; if small they might be something else
    # Convert all numeric via SAS origin; NAs remain NA
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # Trim whitespace
    x_trim <- trimws(x)
    x_trim[x_trim == ""] <- NA
    # Take first 10 chars for date part (handles datetime like 2020-01-02T10:00)
    x_datepart <- substr(x_trim, 1, 10)
    # Parse with single strict format %Y-%m-%d (SAS yymmdd10.)
    # Use as.Date with format; invalid will be NA
    res <- as.Date(x_datepart, format = "%Y-%m-%d")
    # For any remaining NA where original not NA, try to handle SAS character date already?
    return(res)
  }
  # fallback
  return(as.Date(as.character(x), format = "%Y-%m-%d"))
}

# Ensure required columns exist; if missing create NA
if (!"AESTDT" %in% names(ae_merged)) {
  if ("AESTDT" %in% names(raw_ae)) ae_merged$AESTDT <- raw_ae$AESTDT else ae_merged$AESTDT <- as.Date(NA)
}
if (!"AEENDT" %in% names(ae_merged)) {
  if ("AEENDT" %in% names(raw_ae)) ae_merged$AEENDT <- raw_ae$AEENDT else ae_merged$AEENDT <- as.Date(NA)
}
if (!"RFSTDTC" %in% names(ae_merged)) ae_merged$RFSTDTC <- as.character(NA)
if (!"USUBJID" %in% names(ae_merged)) ae_merged$USUBJID <- as.character(NA)
if (!"AETERM" %in% names(ae_merged)) ae_merged$AETERM <- as.character(NA)
if (!"AESER" %in% names(ae_merged)) ae_merged$AESER <- as.character(NA)
if (!"AESEV" %in% names(ae_merged)) ae_merged$AESEV <- as.character(NA)

# Convert dates
# AESTDT and AEENDT are numeric SAS dates in SAS; haven may have converted to Date already
AESTDT_date <- to_date(ae_merged$AESTDT)
AEENDT_date <- to_date(ae_merged$AEENDT)
RFSTDTC_date <- to_date(ae_merged$RFSTDTC)

# For debugging, check types
cat("AESTDT sample:", paste(head(AESTDT_date), collapse = ", "), "\n")
cat("RFSTDTC sample:", paste(head(RFSTDTC_date), collapse = ", "), "\n")

# Create ISO 8601 character dates: put(AESTDT, yymmdd10.) -> "YYYY-MM-DD"
# SAS yymmdd10. formats missing as blank
AESTDTC <- ifelse(is.na(AESTDT_date), NA_character_, format(AESTDT_date, "%Y-%m-%d"))
AEENDTC <- ifelse(is.na(AEENDT_date), NA_character_, format(AEENDT_date, "%Y-%m-%d"))

# Convert RFSTDTC to numeric for calculation: input(RFSTDTC, yymmdd10.) -> numeric SAS date
# Equivalent to RFSTDTC_date as Date; numeric difference same
# Compute ASTDY and AENDY
# SAS logic:
# if not missing(AESTDT) then do;
#   if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num + 1;
#   else ASTDY = AESTDT - rfstdt_num;
# end;

# In R, use Date difference in days
ASTDY <- rep(NA_real_, nrow(ae_merged))
AENDY <- rep(NA_real_, nrow(ae_merged))

# Only where AESTDT not missing and RFSTDTC not missing
idx_ast <- !is.na(AESTDT_date) & !is.na(RFSTDTC_date)
if (any(idx_ast)) {
  diff_ast <- as.numeric(AESTDT_date[idx_ast] - RFSTDTC_date[idx_ast])
  # SAS comparison AESTDT >= rfstdt_num is same as diff >=0
  ASTDY[idx_ast] <- ifelse(diff_ast >= 0, diff_ast + 1, diff_ast)
}

idx_aen <- !is.na(AEENDT_date) & !is.na(RFSTDTC_date)
if (any(idx_aen)) {
  diff_aen <- as.numeric(AEENDT_date[idx_aen] - RFSTDTC_date[idx_aen])
  AENDY[idx_aen] <- ifelse(diff_aen >= 0, diff_aen + 1, diff_aen)
}

# Format ASTDY AENDY as 8. (integer, no decimals) - keep numeric
# SAS format 8. just display; we keep numeric integer

# Build final sdtm.ae dataset
# keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY;
STUDYID <- rep("ABC-301", nrow(ae_merged))

sdtm_ae <- data.frame(
  STUDYID = STUDYID,
  USUBJID = as.character(ae_merged$USUBJID),
  SUBJID  = as.character(ae_merged$SUBJID),
  AETERM  = as.character(ae_merged$AETERM),
  AESER   = as.character(ae_merged$AESER),
  AESEV   = as.character(ae_merged$AESEV),
  AESTDTC = AESTDTC,
  AEENDTC = AEENDTC,
  ASTDY   = ASTDY,
  AENDY   = AENDY,
  stringsAsFactors = FALSE
)

# Ensure column order and types
# SAS LENGTH $10 for STUDYID, AESTDTC, AEENDTC - truncate if needed (not needed)
sdtm_ae$STUDYID <- substr(sdtm_ae$STUDYID, 1, 10)
sdtm_ae$AESTDTC <- substr(sdtm_ae$AESTDTC, 1, 10)
sdtm_ae$AEENDTC <- substr(sdtm_ae$AEENDTC, 1, 10)

# Sort by SUBJID to match SAS sort (and maybe AETERM for stable order)
sdtm_ae <- sdtm_ae[order(sdtm_ae$SUBJID, sdtm_ae$AETERM, sdtm_ae$AESTDTC), , drop = FALSE]

cat("sdtm.ae rows:", nrow(sdtm_ae), "\n")
print(head(sdtm_ae))

# Write outputs
# Write ae.csv
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# Write raw_ae.csv - copy of input raw_ae as read, but ensure dates written as YYYY-MM-DD
# For raw_ae output, we should write the original raw_ae data frame with proper date formatting
# Convert any Date columns to character ISO for consistent CSV (write.csv does this automatically)
# But to be explicit, we will convert Date columns to ISO strings where needed, keeping NA as ""
raw_ae_out <- raw_ae
# Convert Date columns to character ISO for writing (write.csv will handle Date, but we ensure)
for (nm in names(raw_ae_out)) {
  if (inherits(raw_ae_out[[nm]], "Date")) {
    raw_ae_out[[nm]] <- format(raw_ae_out[[nm]], "%Y-%m-%d")
  } else if (inherits(raw_ae_out[[nm]], "POSIXt")) {
    raw_ae_out[[nm]] <- format(raw_ae_out[[nm]], "%Y-%m-%dT%H:%M:%S")
  }
}
write.csv(raw_ae_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")

cat("Outputs written to", out, "\n")
cat(" - ae.csv\n")
cat(" - raw_ae.csv\n")
