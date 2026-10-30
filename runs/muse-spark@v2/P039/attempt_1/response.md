```r
# P039 Supp2Par_v1 -> AE + SUPPAE merge (SUPP2PAR_v1 macro, parent=AE)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

cat("P039 Supp2Par start\n")

root <- "D:/meta_hackathon/sas2r-agent"
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
deriv_adam <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
rawsplit <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"

parent_name <- "AE"
supp_name <- "SUPPAE"
parent_up <- toupper(parent_name)

is_empty_sas <- function(x) {
  if (length(x) == 0) return(logical(0))
  xc <- as.character(x)
  is.na(xc) | trimws(xc, which = "both") == ""
}

find_dataset_file <- function(ds, dirs) {
  ds_low <- tolower(ds)
  for (d in dirs) {
    if (!dir.exists(d)) next
    files <- list.files(d, recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) next
    for (f in files) {
      bn <- tolower(tools::file_path_sans_ext(basename(f)))
      if (bn == ds_low) {
        ext <- tolower(tools::file_ext(f))
        if (ext %in% c("xpt", "sas7bdat", "csv", "xlsx", "xls")) return(f)
      }
    }
  }
  # second pass: any file containing name
  for (d in dirs) {
    if (!dir.exists(d)) next
    files <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    cand <- files[grepl(paste0("(^|/)", ds_low, "\\."), tolower(files))]
    if (length(cand) > 0) return(cand[1])
  }
  return(NULL)
}

read_dataset_file <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE, guess_max = 100000)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(f)
  } else {
    stop(paste0("Unknown extension: ", f))
  }
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}

search_dirs <- c(rawsdtm, deriv_adam, rawadam, rawsplit, rawdir, root)
search_dirs <- unique(search_dirs)

f_parent <- find_dataset_file(parent_name, search_dirs)
f_supp <- find_dataset_file(supp_name, search_dirs)
cat(paste0("parent file: ", ifelse(is.null(f_parent), "NULL", f_parent), "\n"))
cat(paste0("supp file: ", ifelse(is.null(f_supp), "NULL", f_supp), "\n"))
stopifnot(!is.null(f_parent))

parent_df <- read_dataset_file(f_parent)
if (is.null(f_supp)) {
  supp_df <- NULL
} else {
  supp_df <- read_dataset_file(f_supp)
}

# zap haven formats/labels attributes but keep values
zap_df <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # keep underlying values
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      # haven_labelled may remain; coerce via as.vector
      attributes(col) <- attributes(col)[names(attributes(col)) %in% c("names")]
      df[[nm]] <- col
    } else if (inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_labels(col)
      df[[nm]] <- col
    }
  }
  df
}
parent_df <- zap_df(parent_df)
if (!is.null(supp_df)) supp_df <- zap_df(supp_df)

cat(paste0("parent rows=", nrow(parent_df), " cols=", ncol(parent_df), "\n"))
if (!is.null(supp_df)) cat(paste0("supp rows=", nrow(supp_df), " cols=", ncol(supp_df), "\n"))

parent_names <- names(parent_df)
parent_up_names <- toupper(parent_names)

# helper to get column case-insensitively
get_col <- function(df, want) {
  idx <- which(toupper(names(df)) == toupper(want))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}

# ---- checks: parent requires STUDYID DOMAIN USUBJID ----
req_par <- c("STUDYID", "DOMAIN", "USUBJID")
has_par <- toupper(req_par) %in% parent_up_names
if (!all(has_par)) stop("Parent lacks required variables")

ParCopy <- FALSE
if (is.null(supp_df) || nrow(supp_df) == 0) {
  ParCopy <- TRUE
} else {
  supp_names <- names(supp_df)
  supp_up <- toupper(supp_names)
  req_supp <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL")
  has_supp <- toupper(req_supp) %in% supp_up
  if (!all(has_supp)) stop("Supp lacks required variables")
}

final_df <- NULL

if (ParCopy) {
  final_df <- parent_df
} else {
  # RDOMAIN = upcase(parent) check
  RDOMAIN_col <- get_col(supp_df, "RDOMAIN")
  # SAS: where=(RDOMAIN="AE") trailing blanks ignored -> compare trimws
  rdom_trim <- ifelse(is.na(as.character(RDOMAIN_col)), "", trimws(as.character(RDOMAIN_col)))
  supp_rdom <- supp_df[rdom_trim == parent_up, , drop = FALSE]
  if (nrow(supp_rdom) == 0) {
    ParCopy <- TRUE
    final_df <- parent_df
  } else {
    # IGNOR / Accept split: strip(RDOMAIN)=='' or strip(USUBJID)=='' or strip(QNAM)==''
    USUBJID_s <- get_col(supp_df, "USUBJID")
    QNAM_s <- get_col(supp_df, "QNAM")
    e_rdom <- is_empty_sas(RDOMAIN_col)
    e_usub <- is_empty_sas(USUBJID_s)
    e_qnam <- is_empty_sas(QNAM_s)
    is_ignor <- e_rdom | e_usub | e_qnam
    n_ignor <- sum(is_ignor, na.rm = TRUE)
    if (n_ignor > 0) cat(paste0("IGNOR records: ", n_ignor, "\n"))
    accept_df <- supp_df[!is_ignor, , drop = FALSE]
    # uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM
    ukeys <- c("USUBJID","RDOMAIN","IDVAR","IDVARVAL","QNAM")
    key_df <- as.data.frame(lapply(ukeys, function(k) {
      v <- as.character(get_col(accept_df, k))
      v[is.na(v)] <- ""
      trimws(v)
    }), stringsAsFactors = FALSE)
    names(key_df) <- ukeys
    dup_flag <- duplicated(key_df)
    if (any(dup_flag)) {
      stop("Supplemental not unique on USUBJID RDOMAIN IDVAR IDVARVAL QNAM")
    }
    # same QNAM with different labels: group by upcase(QNAM), distinct QLABEL
    QNAM_a <- as.character(get_col(accept_df, "QNAM"))
    QLABEL_a <- as.character(get_col(accept_df, "QLABEL"))
    QNAM_up <- toupper(trimws(ifelse(is.na(QNAM_a), "", QNAM_a)))
    QLABEL_t <- ifelse(is.na(QLABEL_a), "", QLABEL_a)
    chk <- tibble(QNAM_up = QNAM_up, QLABEL = QLABEL_t) %>% distinct() %>%
      group_by(QNAM_up) %>% summarise(nl = n_distinct(QLABEL), .groups = "drop")
    bad_q <- chk$QNAM_up[chk$nl > 1]
    if (length(bad_q) > 0) stop(paste0("Same QNAM different labels: ", paste(bad_q, collapse=",")))

    # IDVAR substitution
    IDVAR_c <- as.character(get_col(accept_df, "IDVAR"))
    IDVARVAL_c <- as.character(get_col(accept_df, "IDVARVAL"))
    USUBJID_c <- as.character(get_col(accept_df, "USUBJID"))
    e_idvar <- is_empty_sas(IDVAR_c)
    e_idvarval <- is_empty_sas(IDVARVAL_c)
    # error if IDVAR empty while IDVARVAL not empty
    if (any(e_idvar & !e_idvarval)) stop("IDVAR empty while IDVARVAL not empty (RC=8)")
    # substitute where both empty
    both_empty <- e_idvar & e_idvarval
    # locate actual column positions (case-insensitive)
    idx_idvar <- which(toupper(names(accept_df)) == "IDVAR")[1]
    idx_idvarval <- which(toupper(names(accept_df)) == "IDVARVAL")[1]
    if (any(both_empty)) {
      accept_df[both_empty, idx_idvar] <- "USUBJID"
      accept_df[both_empty, idx_idvarval] <- USUBJID_c[both_empty]
      cat(paste0("IDVAR substituted: ", sum(both_empty), "\n"))
    }

    # QNAM already on parent check
    # distinct QNAMs
    QNAM_all <- as.character(get_col(accept_df, "QNAM"))
    QNAM_all_t <- trimws(ifelse(is.na(QNAM_all), "", QNAM_all))
    uniq_qnam <- unique(QNAM_all_t[QNAM_all_t != ""])
    # validity warning only
    for (qn in uniq_qnam) {
      valid <- grepl("^[A-Za-z_][A-Za-z0-9_]*$", qn) && nchar(qn) <= 32
      if (!valid || nchar(qn) > 8) {
        cat(paste0("QNAM warning: ", qn, "\n"))
      }
    }
    # exact (case-sensitive) match to parent names after trim
    overlap <- intersect(uniq_qnam, parent_names)
    # SAS merge is case-sensitive on NAME; also try trimmed parent? parent names have no blanks
    if (length(overlap) > 0) {
      cat(paste0("QNAM overlap -> _2: ", paste(overlap, collapse=","), "\n"))
      idx_qn <- which(toupper(names(accept_df)) == "QNAM")[1]
      for (v in overlap) {
        rows_v <- which(trimws(ifelse(is.na(as.character(accept_df[[idx_qn]])), "", as.character(accept_df[[idx_qn]]))) == v)
        accept_df[rows_v, idx_qn] <- paste0(v, "_2")
      }
      # refresh uniq
      QNAM_all <- as.character(get_col(accept_df, "QNAM"))
    }

    # IDVAR values where RDOMAIN = parent
    RDOMAIN_a <- as.character(get_col(accept_df, "RDOMAIN"))
    rdom_a_trim <- trimws(ifelse(is.na(RDOMAIN_a), "", RDOMAIN_a))
    IDVAR_a <- as.character(get_col(accept_df, "IDVAR"))
    IDVAR_a_t <- trimws(ifelse(is.na(IDVAR_a), "", IDVAR_a))
    id_list <- unique(IDVAR_a_t[rdom_a_trim == parent_up])
    id_list <- id_list[id_list != ""]
    cat(paste0("IDVARs: ", paste(id_list, collapse=","), "\n"))
    stopifnot(length(id_list) > 0)

    # check IDVARs present on parent (case-sensitive after trim)
    for (idv in id_list) {
      if (!(idv %in% parent_names)) {
        # SAS uses trim(left(NAME)) = ID ; case-sensitive
        stop(paste0("IDVAR not on parent: ", idv))
      }
    }
    # types
    id_types <- sapply(id_list, function(idv) {
      col <- parent_df[[which(parent_names == idv)[1]]]
      if (is.numeric(col)) "N" else "C"
    })

    # SEQ exists? parent.SEQ case-insensitive
    seq_name <- paste0(parent_up, "SEQ")
    has_seq <- toupper(seq_name) %in% parent_up_names
    seq_actual <- if (has_seq) parent_names[which(parent_up_names == toupper(seq_name))[1]] else NULL

    # build transposes
    trans_list <- list()
    by_list <- list()
    for (ii in seq_along(id_list)) {
      idv <- id_list[ii]
      typ <- id_types[ii]
      idcln <- if (idv != "USUBJID") idv else ""
      # subset
      sel <- which(IDVAR_a_t == idv & rdom_a_trim == parent_up)
      sub <- accept_df[sel, , drop = FALSE]
      # create converted ID column
      IDVARVAL_sub <- as.character(get_col(sub, "IDVARVAL"))
      if (typ == "N") {
        newid <- suppressWarnings(as.numeric(trimws(ifelse(is.na(IDVARVAL_sub), "", IDVARVAL_sub))))
      } else {
        newid <- as.character(IDVARVAL_sub)
        newid[is.na(newid)] <- NA_character_
      }
      # assign to column named idv (overwrite if exists e.g. USUBJID)
      # find position of idv in sub? sub may not have idv column unless idv==USUBJID etc.
      # For merge we need BY columns; create helper vectors
      STUDYID_sub <- as.character(get_col(sub, "STUDYID"))
      RDOMAIN_sub <- as.character(get_col(sub, "RDOMAIN"))
      USUBJID_sub <- as.character(get_col(sub, "USUBJID"))
      if (idcln == "") {
        by_sub <- data.frame(STUDYID = STUDYID_sub, RDOMAIN = RDOMAIN_sub, USUBJID = USUBJID_sub,
                             stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        by_sub <- data.frame(STUDYID = STUDYID_sub, RDOMAIN = RDOMAIN_sub, USUBJID = USUBJID_sub,
                             IDVAL = newid, stringsAsFactors = FALSE, check.names = FALSE)
        names(by_sub)[4] <- idcln
      }
      QNAM_sub <- trimws(ifelse(is.na(as.character(get_col(sub, "QNAM"))), "", as.character(get_col(sub, "QNAM"))))
      QVAL_sub <- as.character(get_col(sub, "QVAL"))
      # transpose via pivot_wider
      # rename RDOMAIN->DOMAIN
      names(by_sub)[names(by_sub) == "RDOMAIN"] <- "DOMAIN"
      tw <- by_sub
      tw$QNAM <- QNAM_sub
      tw$QVAL <- QVAL_sub
      # sort QNAM columns alphabetically for determinism (SAS order ~ appearance; alpha matches typical)
      # pivot
      by_cols <- names(by_sub)
      # use pivot_wider
      # ensure QVAL character
      tw$QVAL <- as.character(tw$QVAL)
      # remove rows with empty QNAM (shouldn't exist)
      tw <- tw[tw$QNAM != "", , drop = FALSE]
      trans <- tw %>% distinct() %>% tidyr::pivot_wider(names_from = QNAM, values_from = QVAL,
                                                        values_fn = function(x) x[1])
      # order new QNAM cols alphabetically
      newcols <- setdiff(names(trans), by_cols)
      trans <- trans[, c(by_cols, sort(newcols)), drop = FALSE]
      trans_list[[idv]] <- as.data.frame(trans, stringsAsFactors = FALSE, check.names = FALSE)
      by_list[[idv]] <- by_cols
    }

    # merge parent with transposes
    # prepare parent BY types: ensure character BY trimmed? keep original but join on trimmed keys
    # Build FINAL by successive left joins
    work <- parent_df
    # need DOMAIN col name actual case: find parent DOMAIN actual
    dom_actual <- parent_names[which(parent_up_names == "DOMAIN")[1]]
    study_actual <- parent_names[which(parent_up_names == "STUDYID")[1]]
    usub_actual <- parent_names[which(parent_up_names == "USUBJID")[1]]
    for (idv in id_list) {
      trans <- trans_list[[idv]]
      byc <- by_list[[idv]]
      # map byc (STUDYID,DOMAIN,USUBJID[,ID]) to actual parent col names
      # byc uses uppercase generic; map case-insensitively
      join_by <- c()
      for (b in byc) {
        # find parent col matching b (for ID col, exact idv)
        if (toupper(b) == "STUDYID") join_by[b] <- study_actual
        else if (toupper(b) == "DOMAIN") join_by[b] <- dom_actual
        else if (toupper(b) == "USUBJID") join_by[b] <- usub_actual
        else join_by[b] <- b  # idv exact
      }
      # dplyr join needs named vector: c("parent_col" = "trans_col")? Use by = setNames(byc, mapped)?
      # trans cols are named byc (generic uppercase). parent cols are actual.
      # Build: by = c("STUDYID_parent" = "STUDYID", ...)
      # Actually names(trans) for BY are byc. So:
      by_arg <- c()
      for (tb in byc) {
        pb <- unname(join_by[tb])
        by_arg[pb] <- tb
      }
      # To avoid .x/.y on non-BY common cols (shouldn't overlap except BY), drop or rename
      # trans only has BY + QNAM cols; QNAM cols are new (except overlap already suffixed)
      # Ensure no duplicate non-BY names cause suffixes: remove any trans col that equals parent non-BY? QNAM _2 ensures no clash.
      # Perform left join; need to handle type mismatch for numeric ID: coerce both to numeric
      # Coerce join keys to same type: for character keys, compare as character trimmed trailing? use exact
      # For safety, coerce both sides BY to character (except numeric ID keep numeric)
      # Check types
      for (k in seq_along(by_arg)) {
        pc <- names(by_arg)[k]; tc <- by_arg[k]
        pv <- work[[pc]]; tv <- trans[[tc]]
        if (is.numeric(pv) && !is.numeric(tv)) trans[[tc]] <- suppressWarnings(as.numeric(tv))
        if (!is.numeric(pv) && is.numeric(tv)) trans[[tc]] <- as.character(tv)
      }
      # drop/rename overlapping non-key cols in trans (should be none)
      nonkey_trans <- setdiff(names(trans), unname(by_arg))
      # if any nonkey_trans %in% names(work), they are QNAM new cols; if clash (shouldn't after _2), keep SAS-like: later overwrites? remove from work? keep work's? In SAS merge, trans value would overwrite? But _2 prevents clash.
      # If clash remains, drop from trans to avoid suffixes and warn
      clash <- intersect(nonkey_trans, names(work))
      if (length(clash) > 0) {
        # rename trans clash to clash_2? Already handled; just drop to avoid .x/.y
        # Actually SAS would have both? Parent var + trans var same name -> trans overwrites? Merge keeps one var with trans value? We'll let trans overwrite by removing work col? Simpler: drop work clash? Keep trans.
        # To avoid suffixes, remove clash from work (will be replaced)
        work <- work[, setdiff(names(work), clash), drop = FALSE]
      }
      work <- dplyr::left_join(work, trans, by = by_arg)
      stopifnot(all(c(study_actual, dom_actual, usub_actual) %in% names(work)))
    }
    # final sort
    if (has_seq) {
      final_by <- c(study_actual, dom_actual, usub_actual, seq_actual)
    } else {
      final_by <- c(study_actual, dom_actual, usub_actual)
    }
    # order with NA first (SAS missing first)
    ord <- do.call(order, c(lapply(final_by, function(cn) {
      v <- work[[cn]]
      # order handles NA.last; use na.last=FALSE
      v
    }), list(na.last = FALSE, method = "radix")))
    work <- work[ord, , drop = FALSE]
    # column order: parent original order + new cols in sorted order
    new_cols <- setdiff(names(work), parent_names)
    # keep parent order as original (minus any dropped clash already handled) then sorted new cols
    par_ord <- intersect(parent_names, names(work))
    work <- work[, c(par_ord, sort(new_cols)), drop = FALSE]
    final_df <- work
    # DROP check (supp rows with no parent) - for log only
    # compute anti-joins count
    drop_n <- 0
    for (idv in id_list) {
      trans <- trans_list[[idv]]
      byc <- by_list[[idv]]
      # parent keys distinct
      if (byc[length(byc)] %in% c("STUDYID","DOMAIN","USUBJID")) {
        # shouldn't happen
      }
      # build parent key df with generic names
      pk <- data.frame(
        STUDYID = as.character(get_col(parent_df, "STUDYID")),
        DOMAIN = as.character(get_col(parent_df, "DOMAIN")),
        USUBJID = as.character(get_col(parent_df, "USUBJID")),
        stringsAsFactors = FALSE
      )
      if (idv != "USUBJID") {
        pk[[idv]] <- parent_df[[which(parent_names == idv)[1]]]
        # coerce to character for comparison? keep type
      }
      # compare: anti_join trans vs pk on byc
      # coerce types for join
      for (b in byc) {
        if (is.numeric(pk[[b]]) && !is.numeric(trans[[b]])) trans[[b]] <- suppressWarnings(as.numeric(trans[[b]]))
        if (!is.numeric(pk[[b]]) && is.numeric(trans[[b]])) trans[[b]] <- as.character(trans[[b]])
      }
      aj <- dplyr::anti_join(trans, pk, by = byc)
      drop_n <- drop_n + nrow(aj)
    }
    if (drop_n > 0) cat(paste0("DROP records (supp w/o parent): ", drop_n, "\n"))
  }
}

# ---- write output ----
# format datetimes as ISO, dates as ISO
for (nm in names(final_df)) {
  col <- final_df[[nm]]
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    col <- format(as.POSIXct(col, tz = "UTC"), "%Y-%m-%dT%H:%M:%S")
    final_df[[nm]] <- col
  } else if (inherits(col, "Date")) {
    # keep Date; write.csv handles YYYY-MM-DD
  } else if (inherits(col, "haven_labelled")) {
    final_df[[nm]] <- haven::zap_labels(col)
  }
}

# ensure haven attributes stripped
final_df <- as.data.frame(final_df, stringsAsFactors = FALSE, check.names = FALSE)

write.csv(final_df, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
cat(paste0("wrote aefull.csv rows=", nrow(final_df), " cols=", ncol(final_df), "\n"))
```