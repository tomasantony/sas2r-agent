```r
# P039 Supp2Par_v1 translation to R
# Produces aefull.csv by merging SUPPAE onto AE (SDTM)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_sas <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: trim and upcase like SAS
sas_trim <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
sas_upcase <- function(x) toupper(sas_trim(x))
sas_empty <- function(x) sas_trim(x) == ""

# helper: find dataset file
find_dataset_file <- function(dirs, dsn){
  dsn_low <- tolower(dsn)
  for (d in dirs){
    if (is.na(d) || d=="" || !dir.exists(d)) next
    # direct files
    files <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
    # also list without recursive for speed
    for (f in files){
      bn <- tolower(basename(f))
      # remove extension
      bn_noext <- sub("\\.[^.]*$", "", bn)
      if (bn_noext == dsn_low){
        # check extension is known
        ext <- tolower(tools::file_ext(f))
        if (ext %in% c("xpt","sas7bdat","csv","sas7bdat")) return(f)
        # also allow no ext? 
      }
    }
  }
  return(NA_character_)
}

# helper: read dataset
read_dataset <- function(path){
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext == "xpt"){
    df <- haven::read_xpt(path)
  } else if (ext == "sas7bdat"){
    df <- haven::read_sas(path)
  } else if (ext == "csv"){
    df <- readr::read_csv(path, show_col_types = FALSE, guess_max = 10000)
  } else {
    # try xpt
    try(df <- haven::read_xpt(path), silent=TRUE)
  }
  if (is.null(df)) return(NULL)
  # convert haven_labelled to plain vectors, keep labels as attribute
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap formats but keep data
  for (nm in names(df)){
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")){
      # keep numeric/character value, drop labels
      df[[nm]] <- haven::zap_labels(col)
    }
    if (inherits(col, "POSIXt")){
      df[[nm]] <- as.POSIXct(col)
    }
  }
  # ensure names are original (haven keeps uppercase)
  names(df) <- toupper(names(df))
  df
}

# candidate directories for psdtm and inlib
cand_dirs <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
  Sys.getenv("RAWSdtm"),
  Sys.getenv("RAWSDTM"),
  Sys.getenv("RAWADAM"),
  Sys.getenv("INLIB")
)
# also add recursive search root
root_cand <- "D:/meta_hackathon/sas2r-agent/data"
if (dir.exists(root_cand)) cand_dirs <- c(cand_dirs, root_cand)

# locate AE and SUPPAE
ae_path <- find_dataset_file(cand_dirs, "AE")
supp_path <- find_dataset_file(cand_dirs, "SUPPAE")

# fallback: brute force search under root
if (is.na(ae_path) || is.na(supp_path)){
  if (dir.exists(root_cand)){
    all_files <- list.files(root_cand, recursive=TRUE, full.names=TRUE)
    if (is.na(ae_path)){
      idx <- which(tolower(tools::file_path_sans_ext(basename(all_files))) == "ae")
      if (length(idx)>0) ae_path <- all_files[idx[1]]
    }
    if (is.na(supp_path)){
      idx <- which(tolower(tools::file_path_sans_ext(basename(all_files))) == "suppae")
      if (length(idx)>0) supp_path <- all_files[idx[1]]
    }
  }
}

if (is.na(ae_path) || !file.exists(ae_path)){
  stop(paste0("AE dataset not found. Searched: ", paste(cand_dirs, collapse="; ")))
}
if (is.na(supp_path) || !file.exists(supp_path)){
  # per macro, if supp does not exist -> copy parent
  message("SUPPAE not found, will copy AE")
}

ae <- read_dataset(ae_path)
supp <- if (!is.na(supp_path) && file.exists(supp_path)) read_dataset(supp_path) else NULL

# ensure required vars exist (case-insensitive already upper)
parent_name <- "AE"
supp_name <- "SUPPAE"

# helper to check existence
has_vars <- function(df, vars){
  all(toupper(vars) %in% toupper(names(df)))
}

# If supp is NULL or empty -> copy
do_copy <- FALSE
if (is.null(supp) || nrow(supp)==0){
  do_copy <- TRUE
} else {
  # check required vars on parent
  req_parent <- c("STUDYID","DOMAIN","USUBJID")
  if (!has_vars(ae, req_parent)){
    stop("Parent missing required vars")
  }
  req_supp <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL")
  if (!has_vars(supp, req_supp)){
    stop("Supp missing required vars")
  }
  # filter RDOMAIN = parent (upcase)
  # ensure RDOMAIN column exists
  supp$RDOMAIN <- sas_upcase(supp$RDOMAIN)
  # keep only where RDOMAIN == "AE"
  supp_filt <- supp[sas_upcase(supp$RDOMAIN) == toupper(parent_name), , drop=FALSE]
  if (nrow(supp_filt)==0){
    do_copy <- TRUE
  } else {
    supp <- supp_filt
  }
}

# function to perform SUPP2PAR merge
supp2par <- function(parent_df, supp_df, parent_nm){
  # parent_df, supp_df have uppercase names
  # Step: ignore records where USUBJID or RDOMAIN or QNAM empty
  is_empty <- function(x) is.na(x) | sas_trim(x)==""
  keep_idx <- !(is_empty(supp_df$USUBJID) | is_empty(supp_df$RDOMAIN) | is_empty(supp_df$QNAM))
  # ignored records would go to _IGNOR, we just drop
  supp_acc <- supp_df[keep_idx, , drop=FALSE]
  if (nrow(supp_acc)==0){
    return(parent_df)
  }
  # check uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM
  # SAS dup check is case-sensitive? Use exact values after trim? Use as is.
  # Create key
  dup_key <- paste(supp_acc$USUBJID, supp_acc$RDOMAIN, supp_acc$IDVAR, supp_acc$IDVARVAL, supp_acc$QNAM, sep="|")
  if (any(duplicated(dup_key))){
    stop("Duplicate keys in SUPP on USUBJID RDOMAIN IDVAR IDVARVAL QNAM")
  }
  # check same QNAM with different QLABEL
  # QNAM upcased for check
  supp_acc$QNAM_UP <- sas_upcase(supp_acc$QNAM)
  # distinct QNAM*QLABEL
  uniq <- unique(supp_acc[, c("QNAM_UP","QLABEL"), drop=FALSE])
  # count per QNAM_UP
  tbl <- table(uniq$QNAM_UP)
  nonuniq <- names(tbl[tbl>1])
  if (length(nonuniq)>0){
    stop(paste("Same QNAM with different QLABEL:", paste(nonuniq, collapse=",")))
  }
  # Determine USUBJID length not needed
  # Create S2P_SUPP: handle IDVAR empty
  # IDVAR and IDVARVAL are character; treat NA as ""
  supp_acc$IDVAR <- ifelse(is.na(supp_acc$IDVAR), "", as.character(supp_acc$IDVAR))
  supp_acc$IDVARVAL <- ifelse(is.na(supp_acc$IDVARVAL), "", as.character(supp_acc$IDVARVAL))
  # trim for check but keep original for value? SAS uses strip comparison
  empty_idvar <- sas_trim(supp_acc$IDVAR)==""
  empty_idvarval <- sas_trim(supp_acc$IDVARVAL)==""
  # case both empty -> set to USUBJID
  both_empty <- empty_idvar & empty_idvarval
  if (any(both_empty)){
    supp_acc$IDVAR[both_empty] <- "USUBJID"
    supp_acc$IDVARVAL[both_empty] <- supp_acc$USUBJID[both_empty]
  }
  # case IDVAR empty while IDVARVAL not empty -> error RC 8
  if (any(empty_idvar & !empty_idvarval)){
    stop("IDVAR empty while IDVARVAL not empty")
  }
  # Check QNAM already on parent
  parent_vars_up <- toupper(names(parent_df))
  supp_qnams <- unique(sas_trim(supp_acc$QNAM))
  supp_qnams <- supp_qnams[supp_qnams!=""]
  # QNAM values are to become variable names; check if any equals parent var
  # SAS check is case-insensitive (NAME vs QNAM)
  qnam_on_parent <- supp_qnams[toupper(supp_qnams) %in% parent_vars_up]
  if (length(qnam_on_parent)>0){
    # rename QNAM to QNAM_2
    for (qn in qnam_on_parent){
      idx <- sas_upcase(supp_acc$QNAM) == toupper(qn)
      supp_acc$QNAM[idx] <- paste0(sas_trim(supp_acc$QNAM[idx]), "_2")
    }
  }
  # Determine distinct IDVAR values where RDOMAIN == parent
  # already filtered
  idvars <- unique(sas_trim(supp_acc$IDVAR))
  idvars <- idvars[idvars!=""]
  # For each IDVAR, check it exists on parent
  for (idv in idvars){
    if (!(toupper(idv) %in% parent_vars_up)){
      stop(paste("IDVAR not on parent:", idv))
    }
  }
  # Determine type of each IDVAR on parent
  idvar_type <- list()
  for (idv in idvars){
    col <- parent_df[[which(toupper(names(parent_df))==toupper(idv))[1]]]
    # numeric if is.numeric and not character
    if (is.numeric(col)) idvar_type[[idv]] <- "N" else idvar_type[[idv]] <- "C"
  }
  # Determine if parent has SEQ variable: parent.SEQ e.g., AESEQ
  seq_var <- paste0(toupper(parent_nm), "SEQ")
  has_seq <- toupper(seq_var) %in% parent_vars_up
  # For each IDVAR, create transposed dataset and merge
  step_list <- list()
  drop_list <- list()
  for (idv in idvars){
    sub <- supp_acc[sas_trim(supp_acc$IDVAR)==idv, , drop=FALSE]
    # create variable named idv with converted IDVARVAL
    # if numeric, input with best.
    if (idvar_type[[idv]]=="N"){
      # SAS input(trim(left(IDVARVAL)),best.) -> numeric
      # trim left
      vals <- suppressWarnings(as.numeric(sas_trim(sub$IDVARVAL)))
      sub[[idv]] <- vals
    } else {
      sub[[idv]] <- sub$IDVARVAL
    }
    # Prepare for transpose: keep STUDYID, RDOMAIN, USUBJID, idv, QNAM, QVAL, QLABEL
    # RDOMAIN will be renamed to DOMAIN
    # For USUBJID case, idv == "USUBJID" -> IDCLN empty, by keys are STUDYID RDOMAIN USUBJID only
    # So we don't need extra idv column beyond USUBJID (but we already have USUBJID)
    # For transpose, by keys are STUDYID DOMAIN USUBJID [idv if not USUBJID]
    # Create DOMAIN column
    sub$DOMAIN <- sub$RDOMAIN
    # Select columns needed
    # Keep QNAM, QVAL, QLABEL, keys
    # Transpose: pivot wider QNAM -> QVAL, keep QLABEL as label
    # Need to handle multiple rows per by-group: each QNAM becomes column
    # Build by keys
    by_keys <- c("STUDYID","DOMAIN","USUBJID")
    if (toupper(idv) != "USUBJID"){
      by_keys <- c(by_keys, toupper(idv))
      # ensure column name uppercase
      # sub already has column named idv (original case may be AESEQ)
      # ensure uppercase name exists
      # rename to uppercase if needed
      if (!(toupper(idv) %in% names(sub))){
        # find case-insensitive
        idx <- which(toupper(names(sub))==toupper(idv))
        if (length(idx)>0) names(sub)[idx[1]] <- toupper(idv)
      }
    }
    # Ensure by_keys exist in sub (uppercase)
    # Convert all names to uppercase for consistency
    names(sub) <- toupper(names(sub))
    by_keys <- toupper(by_keys)
    # For transpose, we need QNAM and QVAL
    # QNAM values may have been renamed with _2
    # Create wide
    # Use tidyr::pivot_wider
    # Need to keep QLABEL for label attribute
    # Get distinct QNAM->QLABEL mapping
    qlabel_map <- unique(sub[, c("QNAM","QLABEL"), drop=FALSE])
    # pivot
    # Ensure QNAM is trimmed
    sub$QNAM <- sas_trim(sub$QNAM)
    # pivot_wider requires unique by_keys+QNAM
    # Use dplyr
    wide <- sub %>%
      dplyr::select(dplyr::all_of(c(by_keys, "QNAM","QVAL"))) %>%
      tidyr::pivot_wider(names_from = QNAM, values_from = QVAL, values_fn = function(x) x[1])
    # Apply labels: for each new QNAM column, set label attribute from QLABEL
    for (qn in unique(qlabel_map$QNAM)){
      qn_trim <- sas_trim(qn)
      if (qn_trim %in% names(wide)){
        lbl <- qlabel_map$QLABEL[qlabel_map$QNAM==qn][1]
        attr(wide[[qn_trim]], "label") <- lbl
      }
    }
    # Sort wide by by_keys
    # SAS sort: missing first, character uppercase before lowercase
    # Use dplyr arrange (will handle NA first)
    wide <- wide %>% dplyr::arrange(dplyr::across(dplyr::all_of(by_keys)))
    # Sort parent for this idv
    parent_sort_keys <- c("STUDYID","DOMAIN","USUBJID")
    if (toupper(idv) != "USUBJID"){
      parent_sort_keys <- c(parent_sort_keys, toupper(idv))
    }
    # Ensure parent has those columns uppercase
    names(parent_df) <- toupper(names(parent_df))
    parent_sorted <- parent_df %>% dplyr::arrange(dplyr::across(dplyr::all_of(parent_sort_keys)))
    # Merge parent and wide
    # Left join parent with wide by by_keys
    merged <- dplyr::left_join(parent_sorted, wide, by = by_keys)
    # Identify drops: supp records with no parent
    # Anti-join wide vs parent on by_keys
    # Need to find wide rows not in parent
    # Use dplyr::anti_join
    drops <- dplyr::anti_join(wide, parent_sorted, by = by_keys)
    step_list[[idv]] <- merged
    drop_list[[idv]] <- drops
  }
  # Now merge step datasets together
  # If only one idvar, final is that step
  # If multiple, merge by STUDYID DOMAIN USUBJID [SEQ if exists]
  if (length(step_list)==1){
    final <- step_list[[1]]
  } else {
    # Determine final by keys
    final_by <- c("STUDYID","DOMAIN","USUBJID")
    if (has_seq) final_by <- c(final_by, toupper(seq_var))
    # Need to sort each step by final_by before merge
    for (nm in names(step_list)){
      step_list[[nm]] <- step_list[[nm]] %>% dplyr::arrange(dplyr::across(dplyr::all_of(final_by)))
    }
    # Merge sequentially via full join? SAS merge with by will interleave
    # Use full_join iteratively
    final <- step_list[[1]]
    for (i in 2:length(step_list)){
      final <- dplyr::full_join(final, step_list[[i]], by = final_by, suffix = c("", ""))
      # For overlapping non-by columns, SAS merge would keep last? But distinct QNAMs so no overlap
    }
    # Sort final by final_by
    final <- final %>% dplyr::arrange(dplyr::across(dplyr::all_of(final_by)))
  }
  # If has_seq, sort by STUDYID DOMAIN USUBJID SEQ else by STUDYID DOMAIN USUBJID
  final_by2 <- c("STUDYID","DOMAIN","USUBJID")
  if (has_seq) final_by2 <- c(final_by2, toupper(seq_var))
  final <- final %>% dplyr::arrange(dplyr::across(dplyr::all_of(final_by2)))
  return(final)
}

if (do_copy){
  aefull <- ae
} else {
  aefull <- supp2par(ae, supp, parent_name)
}

# Ensure output column order: parent columns first in original order, then new QNAM columns
# parent original order
orig_parent_names <- toupper(names(ae))
# aefull names are uppercase
new_cols <- setdiff(names(aefull), orig_parent_names)
# Keep orig order then new cols in order they appear in aefull (which is order of appearance)
aefull <- aefull[, c(intersect(orig_parent_names, names(aefull)), new_cols), drop=FALSE]

# Write CSV
# Convert dates to YYYY-MM-DD, datetimes to YYYY-MM-DDTHH:MM:SS
for (nm in names(aefull)){
  col <- aefull[[nm]]
  if (inherits(col, "Date")){
    aefull[[nm]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXt")){
    aefull[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  }
}

write.csv(aefull, file.path(out, "aefull.csv"), row.names = FALSE, na = "")

# Also write txt rendering for verification (counts)
txt_path <- file.path(out, "aefull.txt")
sink(txt_path)
cat("AEFULL merged dataset\n")
cat(paste0("Rows: ", nrow(aefull), " Cols: ", ncol(aefull), "\n"))
cat("Columns:\n")
cat(paste(names(aefull), collapse=", "), "\n")
if (nrow(aefull)>0){
  cat("\nFirst 5 rows:\n")
  print(utils::head(aefull, 5))
}
sink()

# Also write plotted data CSV already done; no figure required

message("Done. Wrote ", file.path(out, "aefull.csv"))
```