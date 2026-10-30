## P039 Supp2Par_v1 translation
## Merges SUPPAE onto AE (CDISC pilot SDTM)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a,b) if (!is.null(a) && !is.na(a) && nzchar(a)) a else b

# helper: find dataset file
find_file <- function(name) {
  lname <- tolower(name)
  uname <- toupper(name)
  candidates <- c(
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(lname, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(uname, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(lname, ".sas7bdat")),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(uname, ".sas7bdat")),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm", paste0(lname, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", paste0(lname, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", paste0(lname, ".xpt")),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split", paste0(lname, ".xpt")),
    file.path(Sys.getenv("rawsdtm") %||% "", paste0(lname, ".xpt")),
    file.path(Sys.getenv("rawadam") %||% "", paste0(lname, ".xpt")),
    file.path("data/raw/cdisc-pilot/sdtm", paste0(lname, ".xpt")),
    file.path("data/derived/cdisc-pilot/adam", paste0(lname, ".xpt"))
  )
  # also try directory listing for case-insensitive match
  extra_dirs <- unique(c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm"
  ))
  for (d in extra_dirs) {
    if (dir.exists(d)) {
      fl <- list.files(d, full.names = TRUE, ignore.case = FALSE)
      # case-insensitive match
      idx <- which(tolower(basename(fl)) == paste0(lname, ".xpt") | tolower(basename(fl)) == paste0(lname, ".sas7bdat"))
      if (length(idx) > 0) candidates <- c(candidates, fl[idx])
    }
  }
  for (f in candidates) {
    if (!is.na(f) && nzchar(f) && file.exists(f)) return(f)
  }
  return(NA_character_)
}

read_dataset <- function(name) {
  f <- find_file(name)
  if (is.na(f)) {
    cat("WARNING: file for", name, "not found in candidates\n")
    return(NULL)
  }
  cat("Reading", name, "from", f, "\n")
  if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
    df <- haven::read_sas(f)
  } else {
    df <- haven::read_xpt(f)
  }
  # convert haven_labelled to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven may keep labelled class; strip
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
      df[[nm]] <- haven::zap_formats(df[[nm]])
    }
  }
  return(df)
}

# read parent and supp
parent_name <- "AE"
supp_name   <- "SUPPAE"

ae_raw <- read_dataset(parent_name)
supp_raw <- read_dataset(supp_name)

if (is.null(ae_raw)) stop("Parent AE dataset not found")

# helper to uppercase names for logic but keep original for output
# SAS names are case-insensitive; we standardize to uppercase
names(ae_raw) <- toupper(names(ae_raw))
if (!is.null(supp_raw)) names(supp_raw) <- toupper(names(supp_raw))

# function to check empty (SAS strip() == '')
is_empty <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# If supp does not exist or empty -> simple copy
do_copy <- FALSE
if (is.null(supp_raw) || nrow(supp_raw) == 0) {
  do_copy <- TRUE
  cat("SUPP does not exist or empty -> copy parent\n")
}

# Required variables check (parent: STUDYID DOMAIN USUBJID)
if (!do_copy) {
  req_parent <- c("STUDYID","DOMAIN","USUBJID")
  miss_p <- setdiff(req_parent, names(ae_raw))
  if (length(miss_p) > 0) stop(paste("Parent lacks required vars:", paste(miss_p, collapse=",")))
  req_supp <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL")
  miss_s <- setdiff(req_supp, names(supp_raw))
  if (length(miss_s) > 0) stop(paste("Supp lacks required vars:", paste(miss_s, collapse=",")))
}

# Filter supp where RDOMAIN == parent (AE)
if (!do_copy) {
  supp_filt <- supp_raw[toupper(trimws(supp_raw$RDOMAIN)) == toupper(parent_name), , drop=FALSE]
  if (nrow(supp_filt) == 0) {
    cat("SUPP empty after RDOMAIN filter -> copy parent\n")
    do_copy <- TRUE
  } else {
    supp_raw <- supp_filt
  }
}

# If copy, just output parent
if (do_copy) {
  aefull <- ae_raw
  # write output
  # format dates for CSV
  for (nm in names(aefull)) {
    if (inherits(aefull[[nm]], "Date")) {
      aefull[[nm]] <- format(aefull[[nm]], "%Y-%m-%d")
    } else if (inherits(aefull[[nm]], "POSIXct") || inherits(aefull[[nm]], "POSIXt")) {
      aefull[[nm]] <- format(aefull[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  write.csv(aefull, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
  cat("Wrote", file.path(out, "aefull.csv"), "rows", nrow(aefull), "\n")
  quit(save="no", status=0)
}

# Checks on supplemental dataset
# 1) records where USUBJID or RDOMAIN or QNAM empty -> ignore
empty_mask <- is_empty(supp_raw$USUBJID) | is_empty(supp_raw$RDOMAIN) | is_empty(supp_raw$QNAM)
if (any(empty_mask)) {
  cat("Ignoring", sum(empty_mask), "supp records with empty USUBJID/RDOMAIN/QNAM\n")
  supp_raw <- supp_raw[!empty_mask, , drop=FALSE]
  if (nrow(supp_raw) == 0) {
    aefull <- ae_raw
    for (nm in names(aefull)) {
      if (inherits(aefull[[nm]], "Date")) aefull[[nm]] <- format(aefull[[nm]], "%Y-%m-%d")
      else if (inherits(aefull[[nm]], "POSIXct") || inherits(aefull[[nm]], "POSIXt")) aefull[[nm]] <- format(aefull[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
    write.csv(aefull, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
    quit(save="no", status=0)
  }
}

# 2) uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM
# SAS: dupout nodupkey by USUBJID RDOMAIN IDVAR IDVARVAL QNAM
# If duplicates -> terminate; here we check and if found, keep first and warn (but dataset expected to have none)
dup_check <- supp_raw %>%
  mutate(across(everything(), as.character)) %>%
  group_by(USUBJID, RDOMAIN, IDVAR, IDVARVAL, QNAM) %>%
  filter(n() > 1) %>% ungroup()
if (nrow(dup_check) > 0) {
  cat("ERROR: duplicate keys in SUPP:", nrow(dup_check), "records\n")
  print(head(dup_check, 15))
  # For pilot data this should not happen; we deduplicate to continue
  supp_raw <- supp_raw %>% distinct(USUBJID, RDOMAIN, IDVAR, IDVARVAL, QNAM, .keep_all = TRUE)
}

# 3) same QNAM occurs with different QLABEL -> terminate
# Check distinct QNAM (upcase) vs QLABEL
qn_lab <- supp_raw %>% distinct(QNAM, QLABEL) %>% mutate(QNAM_U = toupper(trimws(QNAM)))
qn_counts <- qn_lab %>% group_by(QNAM_U) %>% summarise(n_lab = n_distinct(QLABEL), .groups="drop") %>% filter(n_lab > 1)
if (nrow(qn_counts) > 0) {
  cat("ERROR: same QNAM with different QLABEL:", paste(qn_counts$QNAM_U, collapse=","), "\n")
  stop("QNAM label conflict")
}

# Creating version of supplemental in WORK and ensuring IDVAR not empty
# If IDVAR == '' and IDVARVAL == '' then IDVAR='USUBJID', IDVARVAL=USUBJID
# If IDVAR == '' and IDVARVAL != '' -> error RC=8
idvar_empty <- is_empty(supp_raw$IDVAR)
idvarval_empty <- is_empty(supp_raw$IDVARVAL)
if (any(idvar_empty & !idvarval_empty)) {
  stop("IDVAR empty while IDVARVAL not empty")
}
idx_both_empty <- which(idvar_empty & idvarval_empty)
if (length(idx_both_empty) > 0) {
  supp_raw$IDVAR[idx_both_empty] <- "USUBJID"
  supp_raw$IDVARVAL[idx_both_empty] <- supp_raw$USUBJID[idx_both_empty]
}

# Check for QNAM already on parent
parent_vars <- toupper(names(ae_raw))
supp_qnams <- unique(toupper(trimws(supp_raw$QNAM)))
supp_qnams <- supp_qnams[!is.na(supp_qnams) & supp_qnams != ""]
overlap <- intersect(parent_vars, supp_qnams)
if (length(overlap) > 0) {
  cat("QNAM already on parent, renaming to _2:", paste(overlap, collapse=","), "\n")
  # rename QNAM values: append _2
  for (ov in overlap) {
    mask <- toupper(trimws(supp_raw$QNAM)) == ov
    supp_raw$QNAM[mask] <- paste0(trimws(supp_raw$QNAM[mask]), "_2")
  }
}

# Determine distinct IDVAR values where RDOMAIN == parent
idvar_vals <- unique(trimws(supp_raw$IDVAR))
idvar_vals <- idvar_vals[!is.na(idvar_vals) & idvar_vals != ""]
# keep original case but SAS is case-sensitive? Use as is, but upcase for matching parent var
cat("Distinct IDVAR values:", paste(idvar_vals, collapse=", "), "\n")

# Determine type of each IDVAR as on parent
# For each idvar, check if exists on parent, get type
idvar_info <- list()
for (iv in idvar_vals) {
  iv_up <- toupper(iv)
  if (!(iv_up %in% parent_vars)) {
    stop(paste("IDVAR", iv, "not present on parent"))
  }
  col <- ae_raw[[iv_up]]
  is_num <- is.numeric(col)
  idvar_info[[iv]] <- list(name = iv_up, is_numeric = is_num)
}

# For each IDVAR, create transposed supp and merge
# We will iteratively left_join onto parent
# Keep parent original order columns
parent_cols <- names(ae_raw)

# Prepare final dataset starting from ae_raw
final_df <- ae_raw

# Also collect DROP records (supp without parent)
drop_list <- list()

# Helper to get keys for an idvar
get_keys <- function(iv_up) {
  if (iv_up == "USUBJID") {
    c("STUDYID","DOMAIN","USUBJID")
  } else {
    c("STUDYID","DOMAIN","USUBJID", iv_up)
  }
}

# For each idvar, process
for (iv in idvar_vals) {
  info <- idvar_info[[iv]]
  iv_up <- info$name
  is_num <- info$is_numeric

  # subset supp where IDVAR == iv and RDOMAIN == parent
  sub <- supp_raw[toupper(trimws(supp_raw$IDVAR)) == toupper(iv) & toupper(trimws(supp_raw$RDOMAIN)) == toupper(parent_name), , drop=FALSE]
  if (nrow(sub) == 0) next

  # create variable iv_up from IDVARVAL
  if (is_num) {
    # input(trim(left(IDVARVAL)), best.)
    sub[[iv_up]] <- suppressWarnings(as.numeric(trimws(as.character(sub$IDVARVAL))))
  } else {
    sub[[iv_up]] <- as.character(sub$IDVARVAL)
  }

  # For transpose, need STUDYID, RDOMAIN->DOMAIN, USUBJID, iv_up, QNAM, QVAL, QLABEL
  # Keep only needed
  # Rename RDOMAIN to DOMAIN for transpose
  sub$DOMAIN <- sub$RDOMAIN
  # Ensure keys are character/numeric consistent with parent for join
  # For character keys, trim? SAS merge is exact; we keep as is but ensure type match
  # Convert parent keys to same type for join: if parent key is numeric, ensure sub key numeric (already)
  # For STUDYID, DOMAIN, USUBJID they are character

  # Prepare data for transpose: select keys + QNAM + QVAL
  # QNAM values become column names; QVAL values become cell values
  # Use pivot_wider
  keys <- get_keys(iv_up)
  # keys include DOMAIN not RDOMAIN
  # Ensure sub has DOMAIN column
  # For USUBJID case, keys = STUDYID,DOMAIN,USUBJID (no extra)
  # For other, keys include iv_up

  # Select distinct for transpose
  trans_input <- sub[, c(keys, "QNAM", "QVAL"), drop=FALSE]
  # QNAM should be valid variable name; SAS proc transpose will correct illegal names
  # In R we keep as is, but make syntactically valid if needed? Keep original.
  # Remove rows where QNAM empty (already filtered)
  trans_input <- trans_input[!is_empty(trans_input$QNAM), , drop=FALSE]

  # pivot_wider: if duplicate keys+QNAM (should not happen due to uniqueness), take first
  trans_wide <- trans_input %>%
    # ensure QNAM is character and trimmed
    mutate(QNAM = trimws(as.character(QNAM))) %>%
    group_by(across(all_of(c(keys, "QNAM")))) %>%
    summarise(QVAL = first(QVAL), .groups="drop") %>%
    pivot_wider(id_cols = all_of(keys), names_from = QNAM, values_from = QVAL)

  # Sort trans_wide by keys
  trans_wide <- trans_wide %>% arrange(across(all_of(keys)))

  # Merge parent (final_df) with trans_wide
  # Determine join keys: same as keys
  # For final_df, ensure it has DOMAIN column (it does, parent DOMAIN)
  # Do left_join
  # Before join, check for DROP: supp records with no parent
  # Anti-join trans_wide vs final_df on keys
  # Need to handle that final_df may have multiple rows per key (e.g., multiple AE per subject)
  # For DROP detection, we check distinct parent keys
  parent_keys_distinct <- final_df %>% distinct(across(all_of(keys)))
  drops <- anti_join(trans_wide, parent_keys_distinct, by = keys)
  if (nrow(drops) > 0) {
    drop_list[[iv]] <- drops
  }

  # Left join
  final_df <- left_join(final_df, trans_wide, by = keys)
}

# Determine if parent has --SEQ variable (AESEQ)
seq_var <- paste0(toupper(parent_name), "SEQ")
has_seq <- seq_var %in% names(final_df)

if (has_seq) {
  final_df <- final_df %>% arrange(STUDYID, DOMAIN, USUBJID, .data[[seq_var]])
} else {
  final_df <- final_df %>% arrange(STUDYID, DOMAIN, USUBJID)
}

# Output dataset
aefull <- final_df

# Format dates for CSV output
for (nm in names(aefull)) {
  if (inherits(aefull[[nm]], "Date")) {
    aefull[[nm]] <- format(aefull[[nm]], "%Y-%m-%d")
  } else if (inherits(aefull[[nm]], "POSIXct") || inherits(aefull[[nm]], "POSIXt")) {
    aefull[[nm]] <- format(aefull[[nm]], "%Y-%m-%dT%H:%M:%S")
  }
}

# Ensure column order: parent columns first, then new QNAM columns in order they were added
# Already preserved because left_join appends new columns at end

write.csv(aefull, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
cat("Wrote", file.path(out, "aefull.csv"), "rows", nrow(aefull), "cols", ncol(aefull), "\n")

# Also write a simple txt rendering for verification (optional)
txt_path <- file.path(out, "aefull.txt")
con <- file(txt_path, "w")
writeLines(paste("Dataset AEFULL: merged AE + SUPPAE"), con)
writeLines(paste("Rows:", nrow(aefull), "Cols:", ncol(aefull)), con)
writeLines(paste("Columns:", paste(names(aefull), collapse=", ")), con)
close(con)
