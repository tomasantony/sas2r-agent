# P064_adlb.sas translation to R
# Purpose: Create ADaM ADLB dataset
# SAS logic:
#  proc sort data=sdtm.lb; by USUBJID PARAM LBDTC; run;
#  proc sort data=adam.adsl; by USUBJID; run;
#  data adam.adlb;
#    merge sdtm.lb (in=a) adam.adsl (in=b keep=USUBJID TRT01P RFSTDTC);
#    by USUBJID; if a and b;
#    retain BASE;
#    if first.PARAM then BASE = .;
#    if input(LBDTC, yymmdd10.) < input(RFSTDTC, yymmdd10.) and not missing(AVAL) then BASE = AVAL;
#    if input(LBDTC, yymmdd10.) < input(RFSTDTC, yymmdd10.) and AVAL = BASE then ABLFL = "Y"; else ABLFL = "";
#    CHG = AVAL - BASE;
#    drop LBDTC RFSTDTC;
#  run;

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find dataset file for libref
find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NA_character_)
  # list files case-insensitive
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match name without extension
  low_name <- tolower(name)
  for (f in files) {
    base <- tolower(tools::file_path_sans_ext(basename(f)))
    if (base == low_name) return(f)
  }
  # also check subdirectories one level? not needed
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
  } else if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else {
    # try haven read_sas as fallback
    tryCatch({ df <- haven::read_sas(path) }, error = function(e) NULL)
    if (is.null(df)) tryCatch({ df <- haven::read_xpt(path) }, error = function(e) NULL)
  }
  if (!is.null(df)) {
    # convert haven_labelled to plain
    df <- haven::zap_formats(df)
    df <- haven::zap_labels(df)
    df <- haven::zap_label(df)
    # haven labelled -> numeric/character
    # convert labelled vectors
    for (nm in names(df)) {
      if (haven::is.labelled(df[[nm]])) {
        df[[nm]] <- haven::zap_labels(df[[nm]])
      }
    }
    # tibble to data.frame
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  }
  df
}

# Resolve libref directories (as defined in family environment)
root <- "/bench"
sdtm_dir <- file.path(root, "data/derived/wxzhai/sdtm")
adam_dir <- file.path(root, "data/derived/wxzhai/adam")
# fallback to raw paths if derived not found
rawadam_dir <- file.path(root, "data/raw/cdisc-pilot/adam")
rawsdtm_dir <- file.path(root, "data/raw/cdisc-pilot/sdtm")

# Try to locate sdtm.lb
lb_path <- find_file(sdtm_dir, "lb")
if (is.na(lb_path)) {
  # try sdtm libref alternative
  lb_path <- find_file(rawsdtm_dir, "lb")
}
if (is.na(lb_path)) {
  # try direct file names
  cand <- c(
    file.path(sdtm_dir, "lb.sas7bdat"),
    file.path(sdtm_dir, "lb.xpt"),
    file.path(sdtm_dir, "lb.csv"),
    file.path(rawsdtm_dir, "lb.xpt"),
    file.path(root, "data/derived/wxzhai/sdtm/lb.sas7bdat")
  )
  for (p in cand) if (file.exists(p)) { lb_path <- p; break }
}

adsl_path <- find_file(adam_dir, "adsl")
if (is.na(adsl_path)) {
  adsl_path <- find_file(rawadam_dir, "adsl")
}
if (is.na(adsl_path)) {
  cand2 <- c(
    file.path(adam_dir, "adsl.sas7bdat"),
    file.path(adam_dir, "adsl.xpt"),
    file.path(adam_dir, "adsl.csv"),
    file.path(rawadam_dir, "adsl.xpt")
  )
  for (p in cand2) if (file.exists(p)) { adsl_path <- p; break }
}

if (is.na(lb_path) || is.na(adsl_path)) {
  stop(paste0("Input files not found. lb_path=", lb_path, " adsl_path=", adsl_path))
}

lb <- read_dataset(lb_path)
adsl <- read_dataset(adsl_path)

if (is.null(lb) || is.null(adsl)) stop("Failed to read input datasets")

# Ensure character columns are character (haven may give factors)
lb[] <- lapply(lb, function(x) if (is.factor(x)) as.character(x) else x)
adsl[] <- lapply(adsl, function(x) if (is.factor(x)) as.character(x) else x)

# Keep only needed columns from adsl: USUBJID TRT01P RFSTDTC
# If columns missing, try case-insensitive match
get_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NULL)
  df[[ idx[1] ]]
}
# Ensure USUBJID exists
if (is.null(get_col(lb, "USUBJID"))) stop("USUBJID not found in lb")
if (is.null(get_col(adsl, "USUBJID"))) stop("USUBJID not found in adsl")

# Standardize column names to original case for output, but work with uppercase for logic
# Create uppercase name mapping
names_upper_lb <- toupper(names(lb))
names_upper_adsl <- toupper(names(adsl))

# Extract needed adsl columns
adsl_keep_cols <- c("USUBJID", "TRT01P", "RFSTDTC")
adsl_sub <- data.frame(USUBJID = get_col(adsl, "USUBJID"), stringsAsFactors = FALSE)
for (col in c("TRT01P", "RFSTDTC")) {
  v <- get_col(adsl, col)
  if (is.null(v)) {
    # if missing, create NA column
    adsl_sub[[col]] <- NA_character_
  } else {
    adsl_sub[[col]] <- v
  }
}
# Ensure column names are exactly as SAS (uppercase)
names(adsl_sub) <- c("USUBJID", "TRT01P", "RFSTDTC")

# For lb, keep all columns as is, but ensure USUBJID, PARAM, LBDTC, AVAL exist
# Convert AVAL to numeric if needed
if (!is.null(get_col(lb, "AVAL"))) {
  # ensure numeric
  lb$AVAL <- suppressWarnings(as.numeric(get_col(lb, "AVAL")))
  # if original column name was different case, ensure AVAL column exists
  # find actual name
  idx_aval <- which(toupper(names(lb)) == "AVAL")
  names(lb)[idx_aval] <- "AVAL"
}
# Ensure USUBJID, PARAM, LBDTC column names standardized to uppercase for logic but keep original for output?
# We'll work with uppercase names internally, then restore original names for output except new vars
# Create a copy with uppercase names for processing
lb_proc <- lb
names(lb_proc) <- toupper(names(lb_proc))
adsl_proc <- adsl_sub
names(adsl_proc) <- toupper(names(adsl_proc))

# SAS sorts: proc sort data=sdtm.lb; by USUBJID PARAM LBDTC; run;
# proc sort data=adam.adsl; by USUBJID; run;
# Do sorts
if (all(c("USUBJID","PARAM","LBDTC") %in% names(lb_proc))) {
  lb_proc <- lb_proc[order(lb_proc$USUBJID, lb_proc$PARAM, lb_proc$LBDTC, na.last = FALSE), , drop = FALSE]
}
adsl_proc <- adsl_proc[order(adsl_proc$USUBJID, na.last = FALSE), , drop = FALSE]

# Merge: merge sdtm.lb (in=a) adam.adsl (in=b keep=USUBJID TRT01P RFSTDTC); by USUBJID; if a and b;
# This is inner join on USUBJID, replicating adsl values for each lb row
# Use dplyr inner_join
merged <- merge(lb_proc, adsl_proc, by = "USUBJID", all = FALSE, sort = FALSE)
# After merge, need to sort again by USUBJID PARAM LBDTC to mimic SAS retain order
# SAS retain processes in order of merged dataset which is by USUBJID (BY group) and within that order of lb (which was sorted)
# So sort by USUBJID PARAM LBDTC
if (all(c("USUBJID","PARAM","LBDTC") %in% names(merged))) {
  merged <- merged[order(merged$USUBJID, merged$PARAM, merged$LBDTC, na.last = FALSE), , drop = FALSE]
}

# Helper to parse yymmdd10. : input(LBDTC, yymmdd10.) -> date from first 10 chars
parse_yymmdd10 <- function(x) {
  # x is character, may be NA or ""
  # SAS yymmdd10. reads YYYY-MM-DD, returns missing if invalid
  # We parse first 10 chars as Date
  res <- rep(as.Date(NA), length(x))
  idx <- !is.na(x) & x != ""
  if (any(idx)) {
    substr10 <- substr(x[idx], 1, 10)
    # try as.Date with YYYY-MM-DD
    parsed <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))
    # also try lubridate ymd as fallback
    need <- is.na(parsed)
    if (any(need)) {
      parsed2 <- suppressWarnings(lubridate::ymd(substr10[need], quiet = TRUE))
      parsed[need] <- as.Date(parsed2)
    }
    res[idx] <- parsed
  }
  res
}

# Compute LBDTC_date and RFSTDTC_date for comparison
merged$LBDTC_date <- parse_yymmdd10(merged$LBDTC)
merged$RFSTDTC_date <- parse_yymmdd10(merged$RFSTDTC)

# Implement retain BASE logic
# SAS: retain BASE; if first.PARAM then BASE = .; if LBDTC < RFSTDTC and not missing(AVAL) then BASE = AVAL;
# ABLFL logic uses updated BASE
# first.PARAM should be per USUBJID-PARAM group (intended). We implement that.
# If PARAM missing, treat as group.

# Initialize columns
merged$BASE <- NA_real_
merged$ABLFL <- ""
merged$CHG <- NA_real_

# Process by groups USUBJID + PARAM
# Use sequential loop to exactly mimic SAS retain
# Ensure AVAL is numeric
merged$AVAL <- suppressWarnings(as.numeric(merged$AVAL))

# Create group identifier
# Use interaction of USUBJID and PARAM
# Need to handle NA PARAM
merged$grp_USUBJID <- merged$USUBJID
merged$grp_PARAM <- merged$PARAM

# Order already sorted, now iterate
# For efficiency, split by groups
# Use unique groups in order of appearance
# Create group key
merged$group_key <- paste0(merged$USUBJID, "||", ifelse(is.na(merged$PARAM), "__NA__", merged$PARAM))

# Iterate groups
unique_keys <- unique(merged$group_key)
# Keep BASE retain across groups? SAS retain BASE is not reset except at first.PARAM, so it carries across groups but reset at each new PARAM
# Our loop will handle reset

for (k in unique_keys) {
  idx <- which(merged$group_key == k)
  # idx is already in sorted order
  base_val <- NA_real_
  for (i in idx) {
    # first.PARAM equivalent: first row of this group => reset BASE to NA
    if (i == idx[1]) {
      base_val <- NA_real_
    }
    lbdtc_d <- merged$LBDTC_date[i]
    rfstdtc_d <- merged$RFSTDTC_date[i]
    aval <- merged$AVAL[i]
    # condition: LBDTC < RFSTDTC and not missing AVAL
    cond_pre <- !is.na(lbdtc_d) && !is.na(rfstdtc_d) && lbdtc_d < rfstdtc_d && !is.na(aval)
    if (cond_pre) {
      base_val <- aval
    }
    merged$BASE[i] <- base_val

    # ABLFL: if LBDTC < RFSTDTC and AVAL = BASE then "Y" else ""
    # SAS missing equality: . = . is true, so need to handle NA == NA as TRUE
    cond_ablfl_date <- !is.na(lbdtc_d) && !is.na(rfstdtc_d) && lbdtc_d < rfstdtc_d
    if (cond_ablfl_date) {
      # SAS equality: AVAL = BASE
      # if both NA => true, if one NA and other not => false (since missing vs non-missing not equal)
      # if both non-missing => numeric equality
      aval_eq_base <- FALSE
      b <- base_val
      if (is.na(aval) && is.na(b)) {
        aval_eq_base <- TRUE
      } else if (!is.na(aval) && !is.na(b) && aval == b) {
        aval_eq_base <- TRUE
      } else {
        aval_eq_base <- FALSE
      }
      if (aval_eq_base) {
        merged$ABLFL[i] <- "Y"
      } else {
        merged$ABLFL[i] <- ""
      }
    } else {
      merged$ABLFL[i] <- ""
    }

    # CHG = AVAL - BASE
    if (!is.na(aval) && !is.na(base_val)) {
      merged$CHG[i] <- aval - base_val
    } else {
      merged$CHG[i] <- NA_real_
    }
  }
}

# Drop helper columns and LBDTC RFSTDTC as per SAS drop
# SAS: drop LBDTC RFSTDTC;
# Note: LBDTC and RFSTDTC are dropped, but we have LBDTC_date etc.
cols_to_drop <- c("LBDTC", "RFSTDTC", "LBDTC_date", "RFSTDTC_date", "grp_USUBJID", "grp_PARAM", "group_key")
merged_final <- merged[, setdiff(names(merged), cols_to_drop), drop = FALSE]

# Restore original column order: lb columns (minus LBDTC) + TRT01P + BASE, ABLFL, CHG
# lb original column names (excluding LBDTC, RFSTDTC)
# Get lb original names in order
lb_orig_names <- names(lb)
# Remove LBDTC (case-insensitive)
lb_keep_names <- lb_orig_names[toupper(lb_orig_names) != "LBDTC"]
# For output, we want to keep original case names from lb, plus TRT01P, plus new vars
# But merged_final currently has uppercase names. Need to map back to original case for lb columns
# Create mapping from uppercase to original case (first occurrence)
map_upper_to_orig <- list()
for (nm in lb_orig_names) {
  up <- toupper(nm)
  if (is.null(map_upper_to_orig[[up]])) map_upper_to_orig[[up]] <- nm
}
# Also for TRT01P, keep as TRT01P
# Build final data frame with original case names where possible
final_df <- data.frame(matrix(nrow = nrow(merged_final), ncol = 0))
# Add lb columns in original order
for (orig in lb_keep_names) {
  up <- toupper(orig)
  if (up %in% names(merged_final)) {
    final_df[[orig]] <- merged_final[[up]]
  }
}
# Add TRT01P if not already present (it comes from adsl)
if ("TRT01P" %in% names(merged_final) && !("TRT01P" %in% names(final_df))) {
  # find original case for TRT01P from adsl
  orig_trt <- "TRT01P"
  # check adsl original names
  adsl_orig_names <- names(adsl)
  idx_trt <- which(toupper(adsl_orig_names) == "TRT01P")
  if (length(idx_trt) > 0) orig_trt <- adsl_orig_names[idx_trt[1]]
  final_df[[orig_trt]] <- merged_final[["TRT01P"]]
}
# Add BASE, ABLFL, CHG at end (as SAS does)
final_df[["BASE"]] <- merged_final[["BASE"]]
final_df[["ABLFL"]] <- merged_final[["ABLFL"]]
final_df[["CHG"]] <- merged_final[["CHG"]]

# Handle any remaining columns from merged_final that were not in lb_keep_names but should be kept
# e.g., if lb had extra columns not in lb_keep_names due to case mismatch, ensure they are included
# Already handled, but check for any uppercase columns not yet added that are not helpers
remaining <- setdiff(names(merged_final), c(toupper(lb_keep_names), "TRT01P", "BASE", "ABLFL", "CHG"))
# remaining should be empty; if not, add them
for (up in remaining) {
  # find original name if exists
  orig <- up
  if (!is.null(map_upper_to_orig[[up]])) orig <- map_upper_to_orig[[up]]
  final_df[[orig]] <- merged_final[[up]]
}

# Ensure column order: lb columns first, then TRT01P, then BASE, ABLFL, CHG
# Reorder to put BASE, ABLFL, CHG last
cols <- names(final_df)
base_cols <- c("BASE", "ABLFL", "CHG")
other_cols <- setdiff(cols, base_cols)
final_df <- final_df[, c(other_cols, base_cols), drop = FALSE]

# Format dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS for CSV output
# Identify Date/DateTime columns (if any remain)
for (nm in names(final_df)) {
  col <- final_df[[nm]]
  if (inherits(col, "Date")) {
    final_df[[nm]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
    final_df[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  }
}

# Write output dataset as CSV
write.csv(final_df, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# Also write a txt rendering for verification (optional, but required for tables/listings; for dataset we provide csv only)
# Create a simple txt with head
txt_path <- file.path(out, "adlb.txt")
sink(txt_path)
cat("ADLB dataset created from P064_adlb.sas translation\n")
cat(paste0("Rows: ", nrow(final_df), " Cols: ", ncol(final_df), "\n"))
cat("Columns: ", paste(names(final_df), collapse = ", "), "\n")
print(utils::head(final_df, 10))
sink()

# Console progress
cat("P064 ADLB translation complete. Output written to", file.path(out, "adlb.csv"), "\n")
cat("Rows:", nrow(final_df), "Columns:", ncol(final_df), "\n")
