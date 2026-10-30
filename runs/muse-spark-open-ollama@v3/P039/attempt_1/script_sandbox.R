# P039 Supp2Par_v1 translation to R
# Merges SUPPAE onto AE (CDISC pilot SDTM) reproducing SAS macro SUPP2PAR_v1

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: find dataset file (xpt / sas7bdat / csv) case-insensitive, recursive
find_dataset_file <- function(dsn, search_dirs) {
  dsn_up <- toupper(dsn)
  for (d in search_dirs) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    files <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
    if (length(files) == 0) next
    # match basename without extension
    base_noext <- toupper(tools::file_path_sans_ext(basename(files)))
    ext <- tolower(tools::file_ext(files))
    idx <- which(base_noext == dsn_up & ext %in% c("xpt","sas7bdat","csv","sas7bdat"))
    if (length(idx) > 0) return(files[idx[1]])
    # also try pattern with underscore/dash
    idx2 <- which(grepl(paste0("^", dsn_up, "\\.(XPT|SAS7BDAT|CSV)$"), toupper(basename(files))))
    if (length(idx2) > 0) return(files[idx2[1]])
  }
  return(NA_character_)
}

# helper: read SAS dataset (xpt/sas7bdat/csv) and normalize
read_sas_dataset <- function(path) {
  if (is.na(path) || !file.exists(path)) stop(paste0("File not found: ", path))
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df_raw <- haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    df_raw <- haven::read_sas(path)
  } else if (ext == "csv") {
    df_raw <- readr::read_csv(path, show_col_types = FALSE)
    df_raw <- as.data.frame(df_raw, stringsAsFactors = FALSE)
    names(df_raw) <- toupper(names(df_raw))
    return(df_raw)
  } else {
    stop(paste0("Unknown extension: ", path))
  }
  # Convert SAS dates where format.sas indicates DATE/DATETIME before zapping
  for (cn in names(df_raw)) {
    fmt <- attr(df_raw[[cn]], "format.sas")
    if (!is.null(fmt) && is.numeric(df_raw[[cn]])) {
      fmt_up <- toupper(fmt)
      if (grepl("DATETIME", fmt_up)) {
        # SAS datetime = seconds since 1960-01-01
        # haven usually already converts, but handle numeric case
        if (all(is.na(df_raw[[cn]]) | df_raw[[cn]] < 1e10)) {
          # heuristic: if values look like SAS datetime (large), convert
          # only if not already POSIXct
          if (!inherits(df_raw[[cn]], "POSIXct")) {
            # check if fmt contains DATETIME
            df_raw[[cn]] <- as.POSIXct(df_raw[[cn]], origin = "1960-01-01", tz = "UTC")
          }
        }
      } else if (grepl("DATE", fmt_up)) {
        if (!inherits(df_raw[[cn]], "Date")) {
          df_raw[[cn]] <- as.Date(df_raw[[cn]], origin = "1960-01-01")
        }
      }
    }
  }
  df <- haven::zap_formats(df_raw)
  df <- haven::zap_labels(df)
  df <- haven::zap_label(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven_labelled may remain
  for (cn in names(df)) {
    if (inherits(df[[cn]], "haven_labelled")) {
      df[[cn]] <- haven::zap_labels(df[[cn]])
    }
  }
  names(df) <- toupper(names(df))
  return(df)
}

# helper: format dates for CSV output
format_dates_for_output <- function(df) {
  for (cn in names(df)) {
    if (inherits(df[[cn]], "Date")) {
      df[[cn]] <- format(df[[cn]], "%Y-%m-%d")
    } else if (inherits(df[[cn]], "POSIXct") || inherits(df[[cn]], "POSIXt")) {
      df[[cn]] <- format(df[[cn]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  return(df)
}

# helper: SAS strip = trim both sides, SAS upcase
sas_strip <- function(x) trimws(x, which = "both")
sas_upcase <- function(x) toupper(x)

# helper: SAS nvalid check (simplified): valid SAS variable name = starts with letter or _, followed by letters/digits/_
is_valid_sas_name <- function(nm) {
  if (is.na(nm) || nm == "") return(FALSE)
  grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm) && nchar(nm) <= 32
}

# ------------------------------------------------------------------
# Locate AE and SUPPAE
root <- Sys.getenv("root")
if (root == "" || is.na(root)) root <- "/bench"
rawadam <- Sys.getenv("rawadam")
if (rawadam == "" || is.na(rawadam)) rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")
rawsdtm <- Sys.getenv("rawsdtm")
if (rawsdtm == "" || is.na(rawsdtm)) rawsdtm <- file.path(root, "data/raw/cdisc-pilot/sdtm")
rawsplit <- Sys.getenv("rawsplit")
if (rawsplit == "" || is.na(rawsplit)) rawsplit <- file.path(root, "data/derived/cdisc-pilot/adam-split")
rawdir <- Sys.getenv("rawdir")
if (rawdir == "" || is.na(rawdir)) rawdir <- file.path(root, "data/raw")
inlib_dir <- file.path(root, "data/derived/cdisc-pilot/adam")
psdtm_dir <- file.path(root, "data/raw/cdisc-pilot/sdtm")
# additional possible locations
search_dirs <- unique(c(
  psdtm_dir,
  rawsdtm,
  inlib_dir,
  rawadam,
  rawsplit,
  rawdir,
  file.path(root, "data/derived/cdisc-pilot/sdtm"),
  file.path(root, "data/raw/cdisc-pilot"),
  file.path(root, "data"),
  "/bench/data/derived/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/sdtm"
))

parent_name <- "AE"
supp_name <- "SUPPAE"
outname <- "AEFULL"

ae_path <- find_dataset_file(parent_name, search_dirs)
supp_path <- find_dataset_file(supp_name, search_dirs)

# fallback: try explicit known paths
if (is.na(ae_path)) {
  cand <- c(
    "/bench/data/raw/cdisc-pilot/sdtm/ae.xpt",
    "/bench/data/raw/cdisc-pilot/sdtm/AE.xpt",
    "/bench/data/derived/cdisc-pilot/adam/ae.sas7bdat"
  )
  for (p in cand) if (file.exists(p)) { ae_path <- p; break }
}
if (is.na(supp_path)) {
  cand <- c(
    "/bench/data/raw/cdisc-pilot/sdtm/suppae.xpt",
    "/bench/data/raw/cdisc-pilot/sdtm/SUPPAE.xpt"
  )
  for (p in cand) if (file.exists(p)) { supp_path <- p; break }
}

if (is.na(ae_path) || !file.exists(ae_path)) stop("Parent dataset AE not found in search_dirs")
message("Reading parent AE from: ", ae_path)
parent_df <- read_sas_dataset(ae_path)

# SUPP may be missing -> then output is simple copy (SAS behavior)
supp_exists <- !is.na(supp_path) && file.exists(supp_path)
if (supp_exists) {
  message("Reading supp SUPPAE from: ", supp_path)
  supp_df <- read_sas_dataset(supp_path)
} else {
  message("SUPP dataset not found, will copy parent")
  supp_df <- NULL
}

# ------------------------------------------------------------------
# Validate parent required vars: STUDYID DOMAIN USUBJID
req_parent <- c("STUDYID","DOMAIN","USUBJID")
missing_parent <- setdiff(req_parent, names(parent_df))
if (length(missing_parent) > 0) {
  stop(paste0("Parent lacks required variables: ", paste(missing_parent, collapse=", ")))
}
if (nrow(parent_df) == 0) stop("Parent dataset is empty")

# If supp missing or empty -> simple copy
do_copy <- FALSE
if (is.null(supp_df)) {
  do_copy <- TRUE
} else if (nrow(supp_df) == 0) {
  message("SUPP empty -> copy parent")
  do_copy <- TRUE
} else {
  # Validate supp required vars
  req_supp <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL")
  missing_supp <- setdiff(req_supp, names(supp_df))
  if (length(missing_supp) > 0) {
    stop(paste0("Supp lacks required variables: ", paste(missing_supp, collapse=", ")))
  }
  # Filter RDOMAIN = upcase(parent) -> S2P_RDOMCHECK
  # SAS: where RDOMAIN = "%upcase(&parent)"  (case-sensitive, but values are uppercase)
  supp_rdom <- supp_df[sas_upcase(sas_strip(supp_df$RDOMAIN)) == sas_upcase(parent_name), , drop = FALSE]
  if (nrow(supp_rdom) == 0) {
    message("SUPP empty after RDOMAIN filter -> copy parent")
    do_copy <- TRUE
  } else {
    supp_df <- supp_rdom
  }
}

if (do_copy) {
  aefull <- parent_df
  # ensure output ordering by STUDYID DOMAIN USUBJID [AESEQ if exists]
  seq_var <- paste0(parent_name, "SEQ") # AESEQ
  if (seq_var %in% names(aefull)) {
    # SAS sorts with missing first, character uppercase before lowercase
    # Use order with na.last=FALSE for missing first
    ord <- do.call(order, c(lapply(c("STUDYID","DOMAIN","USUBJID", seq_var), function(cn) {
      x <- aefull[[cn]]
      if (is.character(x)) x <- xtfrm(x) # respects locale but close
      x
    }), list(na.last = FALSE)))
    aefull <- aefull[ord, , drop = FALSE]
  } else {
    ord <- do.call(order, c(lapply(c("STUDYID","DOMAIN","USUBJID"), function(cn) aefull[[cn]]), list(na.last = FALSE)))
    aefull <- aefull[ord, , drop = FALSE]
  }
  aefull_out <- format_dates_for_output(aefull)
  write.csv(aefull_out, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
  message("Wrote aefull.csv (copy) with ", nrow(aefull_out), " rows")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------------
# Checks on supplemental dataset

# 1) Records where USUBJID and/or RDOMAIN and/or QNAM empty -> ignore
# SAS: strip(RDOMAIN)='' or strip(USUBJID)='' or strip(QNAM)=''
is_empty <- function(x) is.na(x) | sas_strip(as.character(x)) == ""
ignore_idx <- is_empty(supp_df$USUBJID) | is_empty(supp_df$RDOMAIN) | is_empty(supp_df$QNAM)
if (any(ignore_idx)) {
  message("SUPP has ", sum(ignore_idx), " records with empty USUBJID/RDOMAIN/QNAM -> ignored")
  # In SAS these go to WORK.&SUPP._IGNOR, processing continues
  supp_df <- supp_df[!ignore_idx, , drop = FALSE]
  if (nrow(supp_df) == 0) {
    message("SUPP empty after ignoring empty keys -> copy parent")
    aefull <- parent_df
    aefull_out <- format_dates_for_output(aefull)
    write.csv(aefull_out, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
    quit(save = "no", status = 0)
  }
}

# 2) Check uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM
# SAS terminates if duplicates
dup_keys <- c("USUBJID","RDOMAIN","IDVAR","IDVARVAL","QNAM")
# normalize for duplicate check: strip and upcase? SAS BY is case-sensitive, but values are uppercase
# Use exact values after trimming
supp_for_dup <- supp_df
for (k in dup_keys) supp_for_dup[[k]] <- sas_strip(as.character(supp_for_dup[[k]]))
dup_check <- duplicated(supp_for_dup[, dup_keys, drop = FALSE]) | duplicated(supp_for_dup[, dup_keys, drop = FALSE], fromLast = TRUE)
if (any(dup_check)) {
  dup_rows <- supp_for_dup[dup_check, dup_keys, drop = FALSE]
  message("Duplicate records on USUBJID RDOMAIN IDVAR IDVARVAL QNAM found:")
  print(head(dup_rows, 15))
  stop("SUPP not unique on USUBJID RDOMAIN IDVAR IDVARVAL QNAM -> terminate (RC=10)")
}

# 3) Same QNAM occurs with different QLABEL -> terminate
# SAS: proc freq QNAM*QLABEL, then check if same QNAM has >1 QLABEL
# Need to upcase QNAM for check (SAS does QNAM=upcase(QNAM) before check)
supp_df$QNAM_UP <- sas_upcase(sas_strip(supp_df$QNAM))
qlabel_check <- aggregate(QLABEL ~ QNAM_UP, data = supp_df, FUN = function(x) length(unique(sas_strip(as.character(x)))))
colnames(qlabel_check)[2] <- "n_label"
trouble_qnam <- qlabel_check$QNAM_UP[qlabel_check$n_label > 1]
if (length(trouble_qnam) > 0) {
  message("Same QNAM with different QLABEL: ", paste(trouble_qnam, collapse=", "))
  stop("Same QNAM has different labels -> terminate (RC=7)")
}

# ------------------------------------------------------------------
# Creating version in WORK: ensure IDVAR not empty, substitute USUBJID
# Determine USUBJID length (for SAS length handling, not needed in R but keep logic)
# If IDVAR='' and IDVARVAL='' then IDVAR='USUBJID', IDVARVAL=USUBJID
# If IDVAR='' and IDVARVAL ne '' then terminate (RC=8)

# Ensure IDVAR and IDVARVAL are character
supp_df$IDVAR <- as.character(supp_df$IDVAR)
supp_df$IDVARVAL <- as.character(supp_df$IDVARVAL)
# Replace NA with ""
supp_df$IDVAR[is.na(supp_df$IDVAR)] <- ""
supp_df$IDVARVAL[is.na(supp_df$IDVARVAL)] <- ""
supp_df$IDVAR <- sas_strip(supp_df$IDVAR)
supp_df$IDVARVAL <- sas_strip(supp_df$IDVARVAL)

empty_both <- supp_df$IDVAR == "" & supp_df$IDVARVAL == ""
if (any(empty_both)) {
  message("Substituting empty IDVAR/IDVARVAL with USUBJID for ", sum(empty_both), " records")
  supp_df$IDVAR[empty_both] <- "USUBJID"
  supp_df$IDVARVAL[empty_both] <- supp_df$USUBJID[empty_both]
}

empty_idvar_only <- supp_df$IDVAR == "" & supp_df$IDVARVAL != ""
if (any(empty_idvar_only)) {
  stop("IDVAR empty while IDVARVAL not empty -> terminate (RC=8)")
}

# ------------------------------------------------------------------
# Check for QNAM already on parent
# SAS: merge contents NAME with QNAM (rename QNAM to NAME) by NAME, if both then rename QNAM to QNAM_2
parent_vars <- names(parent_df)
# QNAM values (strip, keep original case for variable creation, but check case-insensitive)
qnams_unique <- unique(sas_strip(supp_df$QNAM))
qnams_unique <- qnams_unique[qnams_unique != ""]
# Check illegal names
for (qn in qnams_unique) {
  if (!is_valid_sas_name(qn) || nchar(qn) > 8) {
    message("WARNING: QNAM '", qn, "' has illegal naming or length>8 (SAS will correct, CDISC compliance requires fix)")
  }
}
# Find overlap (case-insensitive, SAS variable names are uppercase)
overlap <- intersect(toupper(parent_vars), toupper(qnams_unique))
if (length(overlap) > 0) {
  message("Parent has variables identical to QNAM, will suffix _2: ", paste(overlap, collapse=", "))
  # For each overlapping QNAM, rename in supp_df to QNAM_2
  # Need to map original QNAM values to new names (preserve case of original QNAM)
  for (ov in overlap) {
    # find original QNAM(s) that match ov case-insensitively
    idx <- which(toupper(sas_strip(supp_df$QNAM)) == ov)
    if (length(idx) > 0) {
      # SAS does QNAM = "NAME_2" where NAME is original QNAM value (as stored)
      # So we append _2 to original QNAM value
      supp_df$QNAM[idx] <- paste0(sas_strip(supp_df$QNAM[idx]), "_2")
    }
  }
}

# ------------------------------------------------------------------
# Determine IDVAR values in supplemental where RDOMAIN=parent
idvar_vals <- unique(sas_strip(supp_df$IDVAR))
idvar_vals <- idvar_vals[idvar_vals != ""]
idvar_vals <- sort(idvar_vals) # SAS proc freq order is sorted by IDVAR
IDMAX <- length(idvar_vals)
if (IDMAX == 0) {
  # No IDVAR -> copy parent
  aefull <- parent_df
  aefull_out <- format_dates_for_output(aefull)
  write.csv(aefull_out, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
  quit(save = "no", status = 0)
}
message("IDVAR values: ", paste(idvar_vals, collapse=", "))

# Determine TYPE for each IDVAR from parent
# SAS: type 1 numeric, 2 character
id_types <- list()
for (i in seq_along(idvar_vals)) {
  idv <- idvar_vals[i]
  if (!(idv %in% names(parent_df))) {
    stop(paste0("IDVAR ", idv, " not present on parent -> terminate (RC=9)"))
  }
  col <- parent_df[[idv]]
  if (is.numeric(col)) {
    id_types[[idv]] <- "N"
  } else {
    id_types[[idv]] <- "C"
  }
}

# ------------------------------------------------------------------
# Core processing: for each IDVAR, transpose and merge

# Helper to create wide supp for one IDVAR
create_wide_supp <- function(supp_sub, idvar_name, id_type) {
  # supp_sub already filtered to IDVAR == idvar_name and RDOMAIN == parent
  # Create variable named idvar_name from IDVARVAL with appropriate type
  # SAS: if numeric then input(trim(left(IDVARVAL)),best.) else IDVARVAL
  if (id_type == "N") {
    # Convert IDVARVAL to numeric (best. informat)
    # SAS best. will parse numeric with possible leading/trailing spaces
    supp_sub[[idvar_name]] <- suppressWarnings(as.numeric(sas_strip(supp_sub$IDVARVAL)))
  } else {
    supp_sub[[idvar_name]] <- supp_sub$IDVARVAL
  }
  # For transpose, need BY variables: STUDYID RDOMAIN USUBJID [IDVAR if not USUBJID]
  # SAS renames RDOMAIN to DOMAIN for transpose
  # Keep only needed columns for transpose
  # QNAM, QVAL, QLABEL, plus BY vars
  # Note: IDCLN is idvar_name unless idvar_name == "USUBJID" then empty
  by_vars <- c("STUDYID","RDOMAIN","USUBJID")
  if (idvar_name != "USUBJID") by_vars <- c(by_vars, idvar_name)
  # Prepare data for transpose: select BY vars + QNAM + QVAL + QLABEL
  # Ensure QNAM is stripped
  supp_sub$QNAM <- sas_strip(supp_sub$QNAM)
  supp_sub$QVAL <- as.character(supp_sub$QVAL)
  # For pivot, we need STUDYID, DOMAIN (from RDOMAIN), USUBJID, [IDVAR]
  supp_sub$DOMAIN <- supp_sub$RDOMAIN
  # Select columns
  keep_cols <- c("STUDYID","DOMAIN","USUBJID", if (idvar_name != "USUBJID") idvar_name, "QNAM","QVAL")
  supp_t <- supp_sub[, keep_cols, drop = FALSE]
  # Sort by BY vars (SAS proc sort before transpose)
  # Use order with na.last=FALSE (missing first)
  ord_cols <- c("STUDYID","DOMAIN","USUBJID", if (idvar_name != "USUBJID") idvar_name)
  # Create ordering
  ord <- do.call(order, c(lapply(ord_cols, function(cn) {
    x <- supp_t[[cn]]
    if (is.character(x)) x <- xtfrm(x)
    x
  }), list(na.last = FALSE)))
  supp_t <- supp_t[ord, , drop = FALSE]
  # Transpose: pivot wider, id = BY vars, names_from = QNAM, values_from = QVAL
  # SAS PROC TRANSPOSE will create one row per BY group, with QNAM values as columns
  # If duplicate BY+QNAM (should not happen due to earlier dup check), take first
  # Use tidyr::pivot_wider
  # Need to handle that QNAM values may not be valid R names, but pivot_wider will handle
  # Ensure QNAM values are unique per BY group (they are due to dup check)
  wide <- tidyr::pivot_wider(supp_t,
                             id_cols = dplyr::all_of(c("STUDYID","DOMAIN","USUBJID", if (idvar_name != "USUBJID") idvar_name)),
                             names_from = QNAM,
                             values_from = QVAL,
                             values_fn = function(x) x[1],
                             values_fill = NA)
  # wide now has BY vars + QNAM columns
  return(wide)
}

# Prepare parent for merging: ensure character columns are not factors
# Determine SEQ var
seq_var <- paste0(parent_name, "SEQ") # AESEQ
has_seq <- seq_var %in% names(parent_df)

# Start with parent as base for final
# We will iteratively left_join each wide supp to parent
# But SAS does: for each IDVAR, merge parent with wide supp to create STEP, then merge STEPS together by SEQ
# Equivalent to left_join parent with each wide supp by appropriate keys, then combine

# Create list of wide supps
wide_list <- list()
for (idv in idvar_vals) {
  supp_sub <- supp_df[sas_strip(supp_df$IDVAR) == idv, , drop = FALSE]
  # Also filter RDOMAIN == parent (already done)
  if (nrow(supp_sub) == 0) next
  id_type <- id_types[[idv]]
  wide <- create_wide_supp(supp_sub, idv, id_type)
  wide_list[[idv]] <- wide
  message("Created wide supp for IDVAR=", idv, " with ", nrow(wide), " rows and ", ncol(wide), " cols")
}

# Now merge parent with each wide
# For each wide, the join keys are STUDYID, DOMAIN, USUBJID, plus IDVAR if not USUBJID
# Parent has DOMAIN, not RDOMAIN
aefull <- parent_df
# Ensure parent DOMAIN is present (it is)
for (idv in names(wide_list)) {
  wide <- wide_list[[idv]]
  by_keys <- c("STUDYID","DOMAIN","USUBJID")
  if (idv != "USUBJID") by_keys <- c(by_keys, idv)
  # Check that by_keys exist in both
  missing_in_parent <- setdiff(by_keys, names(aefull))
  if (length(missing_in_parent) > 0) stop(paste0("Parent missing key ", paste(missing_in_parent, collapse=",")))
  missing_in_wide <- setdiff(by_keys, names(wide))
  if (length(missing_in_wide) > 0) stop(paste0("Wide missing key ", paste(missing_in_wide, collapse=",")))
  # Left join (SAS merge with in=PARENT keeps all parent records)
  # Use dplyr::left_join, but need to handle that wide may have extra columns that already exist in parent (should not, except overlap already handled)
  # Ensure no .x/.y suffixes: drop or rename overlapping non-key columns (SAS would have renamed QNAM_2)
  # Check for overlapping non-key columns
  overlap_cols <- intersect(names(aefull), names(wide))
  overlap_cols <- setdiff(overlap_cols, by_keys)
  if (length(overlap_cols) > 0) {
    # This should have been handled by QNAM_2 renaming, but if still overlap, rename wide cols
    for (oc in overlap_cols) {
      names(wide)[names(wide) == oc] <- paste0(oc, "_2")
    }
  }
  aefull <- dplyr::left_join(aefull, wide, by = by_keys)
  message("Merged IDVAR=", idv, " -> aefull now ", nrow(aefull), " rows, ", ncol(aefull), " cols")
}

# Handle DROP dataset: records in supp that have no parent
# SAS creates S2P_&supp.DROP_&i for each IDVAR where SUPP without PARENT
# We can compute overall drops as anti_join of supp wide vs parent
# For reporting, we just check if any supp record has no matching parent
# Build parent keys for each IDVAR and check
drop_count <- 0
for (idv in names(wide_list)) {
  wide <- wide_list[[idv]]
  by_keys <- c("STUDYID","DOMAIN","USUBJID")
  if (idv != "USUBJID") by_keys <- c(by_keys, idv)
  # Parent keys
  parent_keys <- unique(aefull[, by_keys, drop = FALSE])
  # Actually need original parent keys, not aefull after merges
  parent_keys_orig <- unique(parent_df[, by_keys, drop = FALSE])
  # Find wide rows not in parent
  # Use anti_join
  wide_keys <- wide[, by_keys, drop = FALSE]
  # Create a key string for comparison
  # Use dplyr::anti_join
  not_in_parent <- dplyr::anti_join(wide_keys, parent_keys_orig, by = by_keys)
  drop_count <- drop_count + nrow(not_in_parent)
}
if (drop_count > 0) {
  message("WARNING: ", drop_count, " supp records have no parent (would go to SUPPAE_DROP)")
}

# Final sorting: SAS does proc sort by STUDYID DOMAIN USUBJID [AESEQ] for each STEP, then merge by same
# So final order is by STUDYID DOMAIN USUBJID AESEQ if exists
if (has_seq) {
  sort_keys <- c("STUDYID","DOMAIN","USUBJID", seq_var)
} else {
  sort_keys <- c("STUDYID","DOMAIN","USUBJID")
}
# Ensure sort_keys exist
sort_keys <- intersect(sort_keys, names(aefull))
ord <- do.call(order, c(lapply(sort_keys, function(cn) {
  x <- aefull[[cn]]
  if (is.character(x)) {
    # SAS sorts character with uppercase before lowercase, missing first
    # R's order with xtfrm approximates, but we can use base order
    # Replace NA with "" for character missing (SAS missing is "")
    x[is.na(x)] <- ""
    x
  } else {
    x
  }
}), list(na.last = FALSE)))
aefull <- aefull[ord, , drop = FALSE]

# ------------------------------------------------------------------
# Write output
aefull_out <- format_dates_for_output(aefull)
# Ensure column names are uppercase (SAS)
names(aefull_out) <- toupper(names(aefull_out))
write.csv(aefull_out, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
message("Wrote ", file.path(out, "aefull.csv"), " with ", nrow(aefull_out), " rows and ", ncol(aefull_out), " cols")
# Also write a txt rendering for verification (optional)
# Create a simple summary txt
txt_path <- file.path(out, "aefull.txt")
sink(txt_path)
cat("AEFULL dataset created by R translation of SUPP2PAR_v1\n")
cat("Parent: AE, Supp: SUPPAE, Out: AEFULL\n")
cat("Rows: ", nrow(aefull_out), "\n")
cat("Cols: ", ncol(aefull_out), "\n")
cat("Columns: ", paste(names(aefull_out), collapse=", "), "\n")
cat("\nFirst 5 rows:\n")
print(utils::head(aefull_out, 5))
sink()
