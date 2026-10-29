```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

sas_trim <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
sas_upcase <- function(x) toupper(sas_trim(x))

find_dataset_file <- function(dirs, dsn){
  dsn_low <- tolower(dsn)
  for (d in dirs){
    if (is.na(d) || d=="" || !dir.exists(d)) next
    files <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
    for (f in files){
      bn <- tolower(basename(f))
      bn_noext <- sub("\\.[^.]*$", "", bn)
      if (bn_noext == dsn_low){
        ext <- tolower(tools::file_ext(f))
        if (ext %in% c("xpt","sas7bdat","csv")) return(f)
      }
    }
  }
  return(NA_character_)
}

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
    df <- tryCatch(haven::read_xpt(path), error=function(e) NULL)
  }
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)){
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")){
      df[[nm]] <- haven::zap_labels(col)
    }
  }
  names(df) <- toupper(names(df))
  df
}

arrange_by_keys <- function(df, keys){
  keys <- intersect(keys, names(df))
  if (length(keys)==0) return(df)
  # SAS: missing first -> na.last = FALSE
  ord <- do.call(order, c(lapply(keys, function(k) df[[k]]), list(na.last = FALSE)))
  df[ord, , drop=FALSE]
}

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
root_cand <- "D:/meta_hackathon/sas2r-agent/data"
if (dir.exists(root_cand)) cand_dirs <- c(cand_dirs, root_cand)

ae_path <- find_dataset_file(cand_dirs, "AE")
supp_path <- find_dataset_file(cand_dirs, "SUPPAE")

if (is.na(ae_path) || !file.exists(ae_path)){
  if (dir.exists(root_cand)){
    all_files <- list.files(root_cand, recursive=TRUE, full.names=TRUE)
    idx <- which(tolower(tools::file_path_sans_ext(basename(all_files))) == "ae")
    if (length(idx)>0) ae_path <- all_files[idx[1]]
    if (is.na(supp_path)){
      idx2 <- which(tolower(tools::file_path_sans_ext(basename(all_files))) == "suppae")
      if (length(idx2)>0) supp_path <- all_files[idx2[1]]
    }
  }
}
if (is.na(ae_path) || !file.exists(ae_path)){
  stop(paste0("AE dataset not found. Searched: ", paste(cand_dirs, collapse="; ")))
}

ae <- read_dataset(ae_path)
supp <- if (!is.na(supp_path) && file.exists(supp_path)) read_dataset(supp_path) else NULL

parent_name <- "AE"

has_vars <- function(df, vars){
  all(toupper(vars) %in% toupper(names(df)))
}

do_copy <- FALSE
if (is.null(supp) || nrow(supp)==0){
  do_copy <- TRUE
} else {
  req_parent <- c("STUDYID","DOMAIN","USUBJID")
  if (!has_vars(ae, req_parent)) stop("Parent missing required vars")
  req_supp <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL")
  if (!has_vars(supp, req_supp)) stop("Supp missing required vars")
  supp$RDOMAIN <- sas_upcase(supp$RDOMAIN)
  supp_filt <- supp[sas_upcase(supp$RDOMAIN) == toupper(parent_name), , drop=FALSE]
  if (nrow(supp_filt)==0){
    do_copy <- TRUE
  } else {
    supp <- supp_filt
  }
}

supp2par <- function(parent_df, supp_df, parent_nm){
  is_empty <- function(x) is.na(x) | sas_trim(x)==""
  keep_idx <- !(is_empty(supp_df$USUBJID) | is_empty(supp_df$RDOMAIN) | is_empty(supp_df$QNAM))
  supp_acc <- supp_df[keep_idx, , drop=FALSE]
  if (nrow(supp_acc)==0) return(parent_df)

  dup_key <- paste(supp_acc$USUBJID, supp_acc$RDOMAIN, supp_acc$IDVAR, supp_acc$IDVARVAL, supp_acc$QNAM, sep="|")
  if (any(duplicated(dup_key))){
    stop("Duplicate keys in SUPP on USUBJID RDOMAIN IDVAR IDVARVAL QNAM")
  }

  supp_acc$QNAM_UP <- sas_upcase(supp_acc$QNAM)
  uniq <- unique(supp_acc[, c("QNAM_UP","QLABEL"), drop=FALSE])
  tbl <- table(uniq$QNAM_UP)
  nonuniq <- names(tbl[tbl>1])
  if (length(nonuniq)>0){
    stop(paste("Same QNAM with different QLABEL:", paste(nonuniq, collapse=",")))
  }

  supp_acc$IDVAR <- ifelse(is.na(supp_acc$IDVAR), "", as.character(supp_acc$IDVAR))
  supp_acc$IDVARVAL <- ifelse(is.na(supp_acc$IDVARVAL), "", as.character(supp_acc$IDVARVAL))
  empty_idvar <- sas_trim(supp_acc$IDVAR)==""
  empty_idvarval <- sas_trim(supp_acc$IDVARVAL)==""
  both_empty <- empty_idvar & empty_idvarval
  if (any(both_empty)){
    supp_acc$IDVAR[both_empty] <- "USUBJID"
    supp_acc$IDVARVAL[both_empty] <- supp_acc$USUBJID[both_empty]
  }
  if (any(empty_idvar & !empty_idvarval)){
    stop("IDVAR empty while IDVARVAL not empty")
  }

  parent_vars_up <- toupper(names(parent_df))
  supp_qnams <- unique(sas_trim(supp_acc$QNAM))
  supp_qnams <- supp_qnams[supp_qnams!=""]
  qnam_on_parent <- supp_qnams[toupper(supp_qnams) %in% parent_vars_up]
  if (length(qnam_on_parent)>0){
    for (qn in qnam_on_parent){
      idx <- sas_upcase(supp_acc$QNAM) == toupper(qn)
      supp_acc$QNAM[idx] <- paste0(sas_trim(supp_acc$QNAM[idx]), "_2")
    }
  }

  idvars <- unique(sas_trim(supp_acc$IDVAR))
  idvars <- idvars[idvars!=""]
  for (idv in idvars){
    if (!(toupper(idv) %in% parent_vars_up)){
      stop(paste("IDVAR not on parent:", idv))
    }
  }
  idvar_type <- list()
  for (idv in idvars){
    col <- parent_df[[which(toupper(names(parent_df))==toupper(idv))[1]]]
    if (is.numeric(col)) idvar_type[[idv]] <- "N" else idvar_type[[idv]] <- "C"
  }

  seq_var <- paste0(toupper(parent_nm), "SEQ")
  has_seq <- toupper(seq_var) %in% parent_vars_up

  step_list <- list()
  drop_list <- list()

  for (idv in idvars){
    sub <- supp_acc[sas_trim(supp_acc$IDVAR)==idv, , drop=FALSE]
    if (idvar_type[[idv]]=="N"){
      vals <- suppressWarnings(as.numeric(sas_trim(sub$IDVARVAL)))
      sub[[idv]] <- vals
    } else {
      sub[[idv]] <- sub$IDVARVAL
    }
    sub$DOMAIN <- sub$RDOMAIN
    names(sub) <- toupper(names(sub))
    by_keys <- c("STUDYID","DOMAIN","USUBJID")
    if (toupper(idv) != "USUBJID"){
      by_keys <- c(by_keys, toupper(idv))
      if (!(toupper(idv) %in% names(sub))){
        idx <- which(toupper(names(sub))==toupper(idv))
        if (length(idx)>0) names(sub)[idx[1]] <- toupper(idv)
      }
    }
    by_keys <- toupper(by_keys)
    sub$QNAM <- sas_trim(sub$QNAM)
    qlabel_map <- unique(sub[, c("QNAM","QLABEL"), drop=FALSE])

    # pivot wider without pipe
    sel_cols <- c(by_keys, "QNAM", "QVAL")
    sel_cols <- intersect(sel_cols, names(sub))
    sub_sel <- sub[, sel_cols, drop=FALSE]
    wide <- tidyr::pivot_wider(sub_sel, names_from = QNAM, values_from = QVAL, values_fn = function(x) x[1])

    for (qn in unique(qlabel_map$QNAM)){
      qn_trim <- sas_trim(qn)
      if (qn_trim %in% names(wide)){
        lbl <- qlabel_map$QLABEL[qlabel_map$QNAM==qn][1]
        attr(wide[[qn_trim]], "label") <- lbl
      }
    }
    wide <- arrange_by_keys(wide, by_keys)

    names(parent_df) <- toupper(names(parent_df))
    parent_sort_keys <- c("STUDYID","DOMAIN","USUBJID")
    if (toupper(idv) != "USUBJID"){
      parent_sort_keys <- c(parent_sort_keys, toupper(idv))
    }
    parent_sorted <- arrange_by_keys(parent_df, parent_sort_keys)

    merged <- dplyr::left_join(parent_sorted, wide, by = by_keys)
    drops <- dplyr::anti_join(wide, parent_sorted, by = by_keys)
    step_list[[idv]] <- merged
    drop_list[[idv]] <- drops
  }

  if (length(step_list)==1){
    final <- step_list[[1]]
  } else {
    final_by <- c("STUDYID","DOMAIN","USUBJID")
    if (has_seq) final_by <- c(final_by, toupper(seq_var))
    for (nm in names(step_list)){
      step_list[[nm]] <- arrange_by_keys(step_list[[nm]], final_by)
    }
    final <- step_list[[1]]
    for (i in 2:length(step_list)){
      final <- dplyr::full_join(final, step_list[[i]], by = final_by, suffix = c("", ""))
    }
    final <- arrange_by_keys(final, final_by)
  }
  final_by2 <- c("STUDYID","DOMAIN","USUBJID")
  if (has_seq) final_by2 <- c(final_by2, toupper(seq_var))
  final <- arrange_by_keys(final, final_by2)
  return(final)
}

if (do_copy){
  aefull <- ae
} else {
  aefull <- supp2par(ae, supp, parent_name)
}

orig_parent_names <- toupper(names(ae))
names(aefull) <- toupper(names(aefull))
new_cols <- setdiff(names(aefull), orig_parent_names)
aefull <- aefull[, c(intersect(orig_parent_names, names(aefull)), new_cols), drop=FALSE]

for (nm in names(aefull)){
  col <- aefull[[nm]]
  if (inherits(col, "Date")){
    aefull[[nm]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXt")){
    aefull[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  }
}

write.csv(aefull, file.path(out, "aefull.csv"), row.names = FALSE, na = "")

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
message("Done. Wrote ", file.path(out, "aefull.csv"))
```