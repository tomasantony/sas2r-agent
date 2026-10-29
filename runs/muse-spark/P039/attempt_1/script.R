# P039 Supp2Par_v1 -> AE + SUPPAE merge (SUPP2PAR)
# Standalone R translation; runs with Rscript on Windows
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

cat("P039 Supp2Par translation starting\n")

root_dir   <- "D:/meta_hackathon/sas2r-agent"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_dir   <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
rawdir_dir  <- "D:/meta_hackathon/sas2r-agent/data/raw"

parent_name <- "AE"
supp_name   <- "SUPPAE"
parent_up   <- toupper(parent_name)

# ---------- helpers ----------
sas_trim <- function(x) {
  # SAS STRIP: trim both ends; NA stays NA
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}
is_empty_char <- function(x) {
  is.na(x) | sas_trim(x) == ""
}
# SAS-like ordering: force C collation for character sorts
sas_arrange <- function(df, cols) {
  # cols: character vector of column names present in df
  oldloc <- Sys.getlocale("LC_COLLATE")
  # try C locale for ASCII (uppercase before lowercase), restore after
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  on.exit(suppressWarnings(try(Sys.setlocale("LC_COLLATE", oldloc), silent = TRUE)), add = TRUE)
  # build order via base::order with radix for stability; NA first like SAS
  args <- lapply(cols, function(cn) df[[cn]])
  # na.last=FALSE => missing first (SAS)
  ord <- do.call(order, c(args, list(na.last = FALSE, method = "radix")))
  df[ord, , drop = FALSE]
}

clean_haven <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
      attr(x, "label") <- NULL
      df[[nm]] <- as.vector(x)
    } else {
      attr(x, "label") <- NULL
      attr(x, "format.sas") <- NULL
      attr(x, "format.spss") <- NULL
      attr(x, "display_width") <- NULL
      df[[nm]] <- x
    }
  }
  names(df) <- toupper(names(df))
  # reset rownames
  rownames(df) <- NULL
  df
}

find_dataset_file <- function(base, dirs) {
  base_low <- tolower(base)
  # direct candidates
  cand <- c()
  for (d in dirs) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    cand <- c(cand, file.path(d, paste0(base, ".xpt")),
                    file.path(d, paste0(base_low, ".xpt")),
                    file.path(d, paste0(toupper(base), ".xpt")),
                    file.path(d, paste0(base, ".sas7bdat")),
                    file.path(d, paste0(base_low, ".sas7bdat")))
  }
  for (f in cand) if (file.exists(f)) return(f)
  # search recursively
  for (d in dirs) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    allf <- try(list.files(d, full.names = TRUE, recursive = TRUE), silent = TRUE)
    if (inherits(allf, "try-error")) next
    for (f in allf) {
      bn <- tolower(basename(f))
      if (bn == paste0(base_low, ".xpt") || bn == paste0(base_low, ".sas7bdat")) return(f)
    }
  }
  return(NULL)
}

read_sas_dataset <- function(f) {
  cat(paste0("Reading ", f, "\n"))
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f, .name_repair = "minimal")
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(f, .name_repair = "minimal")
  } else {
    df <- haven::read_xpt(f, .name_repair = "minimal")
  }
  clean_haven(df)
}

dirs_to_search <- c(rawsdtm_dir, inlib_dir, rawadam_dir, rawdir_dir,
                    file.path(root_dir, "data/raw/cdisc-pilot/sdtm"),
                    file.path(root_dir, "data/derived/cdisc-pilot/adam"))

f_parent <- find_dataset_file(parent_name, dirs_to_search)
f_supp   <- find_dataset_file(supp_name, dirs_to_search)

# fallback: list sdtm dir explicitly
if (is.null(f_parent) && dir.exists(rawsdtm_dir)) {
  cat("Parent not found via search; listing rawsdtm\n")
  print(list.files(rawsdtm_dir, full.names = FALSE))
}
if (is.null(f_supp) && dir.exists(rawsdtm_dir)) {
  cat("Supp not found via search; listing rawsdtm\n")
  print(list.files(rawsdtm_dir, full.names = FALSE))
}
if (is.null(f_parent)) stop("Parent AE dataset file not found")
if (is.null(f_supp)) stop("Supp SUPPAE dataset file not found")

parent_df <- read_sas_dataset(f_parent)
supp_df   <- read_sas_dataset(f_supp)

cat(paste0("Parent rows=", nrow(parent_df), " cols=", ncol(parent_df), "\n"))
cat(paste0("Supp rows=", nrow(supp_df), " cols=", ncol(supp_df), "\n"))

parent_vars_up <- toupper(names(parent_df))
supp_vars_up   <- toupper(names(supp_df))

# ---- Required variables checks ----
req_parent <- c("STUDYID", "DOMAIN", "USUBJID")
req_supp   <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QVAL", "QLABEL")
miss_p <- setdiff(req_parent, parent_vars_up)
if (length(miss_p) > 0) stop(paste("Parent lacks", paste(miss_p, collapse=",")))
miss_s <- setdiff(req_supp, supp_vars_up)
if (length(miss_s) > 0) stop(paste("Supp lacks", paste(miss_s, collapse=",")))

# Ensure needed columns exist as character where appropriate; coerce haven numerics?
# Keep types as read; but ensure key cols are character except SEQ
# IDVAR, IDVARVAL, QNAM, QVAL, QLABEL, RDOMAIN, STUDYID, USUBJID, DOMAIN as character
char_cols_parent <- intersect(c("STUDYID","DOMAIN","USUBJID"), names(parent_df))
for (cn in char_cols_parent) parent_df[[cn]] <- as.character(parent_df[[cn]])
char_cols_supp <- intersect(c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL"), names(supp_df))
for (cn in char_cols_supp) supp_df[[cn]] <- as.character(supp_df[[cn]])

# ---- RDOMAIN = parent check ----
# SAS: where RDOMAIN = "AE" (case-sensitive, blank-padded). Use trimmed exact.
supp_rdom <- supp_df[sas_trim(supp_df$RDOMAIN) == parent_up & !is.na(supp_df$RDOMAIN), , drop = FALSE]
cat(paste0("Supp rows with RDOMAIN=", parent_up, ": ", nrow(supp_rdom), "\n"))

par_copy_flag <- FALSE
if (nrow(supp_rdom) == 0) {
  par_copy_flag <- TRUE
}

# Helper to write final copy
write_final <- function(df) {
  # SAS output dataset: keep variable order as in df; write CSV
  # Ensure dates as YYYY-MM-DD etc: SDTM already character ISO; numerics unrounded
  write.csv(df, file.path(out, "aefull.csv"), row.names = FALSE, na = "")
  cat(paste0("Wrote aefull.csv rows=", nrow(df), " cols=", ncol(df), "\n"))
}

if (par_copy_flag) {
  cat("RDOMAIN selection empty -> simple copy\n")
  write_final(parent_df)
} else {
  # ---- IGNOR check: strip(RDOMAIN)=='' or strip(USUBJID)=='' or strip(QNAM)=='' ----
  is_ignor <- is_empty_char(supp_df$RDOMAIN) | is_empty_char(supp_df$USUBJID) | is_empty_char(supp_df$QNAM)
  is_ignor[is.na(is_ignor)] <- FALSE
  # Note SAS splits using both where clauses on full supp (not just RDOMAIN subset);
  # replicate: ignor from full supp, accept = rest
  supp_ignor  <- supp_df[is_ignor, , drop = FALSE]
  supp_accept <- supp_df[!is_ignor, , drop = FALSE]
  cat(paste0("IGNOR rows=", nrow(supp_ignor), " ACCEPT rows=", nrow(supp_accept), "\n"))

  # ---- Uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM ----
  # SAS PROC SORT NODUPKEY DUPOUT: check duplicates in ACCEPT
  if (nrow(supp_accept) > 0) {
    key_df <- supp_accept[, c("USUBJID","RDOMAIN","IDVAR","IDVARVAL","QNAM"), drop = FALSE]
    # SAS considers missing/blank distinct? Use exact including NA: replace NA with sentinel
    # For duplicate detection, treat NA == NA and "" distinct from NA? SAS missing char = blanks,
    # R NA vs "" : coerce NA to "" for key comparison to mimic SAS blanks
    key_df2 <- as.data.frame(lapply(key_df, function(x) { x[is.na(x)] <- ""; sas_trim(x) }), stringsAsFactors = FALSE)
    # Actually SAS comparison is blank-padded, not trimmed? But keys with trailing blanks equal.
    # Using trimmed is close enough for pilot (no trailing blanks differences).
    dup_flag <- duplicated(key_df2) | duplicated(key_df2, fromLast = TRUE)
    ndup <- sum(dup_flag)
    # SAS DUPOUT contains duplicates beyond first? NOBS_SUPPDUP counts dupout rows.
    # Our ndup counts all involved; SAS dupout counts extra copies. Adjust: if duplicates exist, error.
    # Compute nodupkey extra count:
    ndup_extra <- nrow(key_df2) - nrow(unique(key_df2))
    cat(paste0("Duplicate key extra rows=", ndup_extra, "\n"))
    if (ndup_extra > 0) stop("Supplemental not unique on USUBJID RDOMAIN IDVAR IDVARVAL QNAM")
  }

  # ---- Same QNAM with different QLABEL ----
  freq_tbl <- supp_accept %>%
    dplyr::distinct(QNAM, QLABEL) %>%
    dplyr::mutate(QNAM_U = toupper(QNAM))
  # group by QNAM_U
  qlabel_counts <- freq_tbl %>% dplyr::group_by(QNAM_U) %>% dplyr::summarise(n = dplyr::n(), .groups="drop")
  bad_q <- qlabel_counts %>% dplyr::filter(n > 1)
  if (nrow(bad_q) > 0) stop(paste("Same QNAM different QLABEL:", paste(bad_q$QNAM_U, collapse=",")))

  # ---- Create WORK version with IDVAR substitution ----
  supp_work <- supp_accept
  # lengths ignored in R
  empty_idvar <- is_empty_char(supp_work$IDVAR)
  empty_idvarval <- is_empty_char(supp_work$IDVARVAL)
  # IDVAR empty while IDVARVAL not empty -> terminate RC=8
  if (any(empty_idvar & !empty_idvarval, na.rm = TRUE)) stop("IDVAR empty while IDVARVAL not empty (RC=8)")
  # both empty -> substitute USUBJID
  both_empty <- empty_idvar & empty_idvarval
  if (any(both_empty, na.rm = TRUE)) {
    supp_work$IDVAR[both_empty] <- "USUBJID"
    supp_work$IDVARVAL[both_empty] <- supp_work$USUBJID[both_empty]
    cat(paste0("Substituted USUBJID for ", sum(both_empty), " rows\n"))
  }

  # ---- Check QNAM already on parent ----
  parent_names <- toupper(names(parent_df))
  supp_qnams <- unique(supp_work$QNAM)
  supp_qnams <- supp_qnams[!is.na(supp_qnams) & sas_trim(supp_qnams) != ""]
  # SAS nvalid check (inform only)
  is_valid_sas <- function(nm) grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm)
  for (qn in supp_qnams) {
    qns <- sas_trim(qn)
    if (!is_valid_sas(qns) || nchar(qns) > 8) {
      cat(paste0("WARNING QNAM illegal/len>8: ", qns, "\n"))
    }
  }
  overlap <- intersect(toupper(sas_trim(supp_qnams)), parent_names)
  # SAS merge is case-sensitive on NAME vs QNAM; but QNAMs upper, parent upper -> same
  # Replicate case-sensitive overlap as well: intersect exact
  overlap_exact <- intersect(supp_qnams, names(parent_df))
  # Use uppercase overlap to be safe (SAS contents NAME is uppercase)
  if (length(overlap) > 0) {
    cat(paste0("QNAM overlap, suffix _2: ", paste(overlap, collapse=","), "\n"))
    # rename those QNAM values to QNAM_2 (preserve case? use original + _2)
    for (ov in overlap) {
      idx <- which(toupper(sas_trim(supp_work$QNAM)) == ov)
      # SAS: if QNAM = "NAME" then QNAM = "NAME_2" (exact match, case-sensitive)
      # Implement: append _2 to original trimmed value's uppercase? Use original value + _2
      supp_work$QNAM[idx] <- paste0(sas_trim(supp_work$QNAM[idx]), "_2")
    }
    # refresh qnams
    supp_qnams <- unique(supp_work$QNAM)
  }

  # ---- Determine IDVAR values where RDOMAIN=parent ----
  supp_for_ids <- supp_work[sas_trim(supp_work$RDOMAIN) == parent_up, , drop = FALSE]
  id_vals <- sort(unique(supp_for_ids$IDVAR))
  # SAS PROC FREQ order is by IDVAR value (ascending, SAS collation). Use C-locale sort
  # Use sas ordering via C locale
  oldloc2 <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  id_vals <- sort(id_vals)
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", oldloc2), silent = TRUE))
  IDMAX <- length(id_vals)
  cat(paste0("IDMAX=", IDMAX, " IDs=", paste(id_vals, collapse=","), "\n"))
  if (IDMAX == 0) {
    write_final(parent_df)
  } else {
    # Check IDVARs present on parent
    for (idv in id_vals) {
      if (!(toupper(sas_trim(idv)) %in% parent_names)) stop(paste("IDVAR not on parent:", idv))
    }
    # Determine type per ID
    id_types <- sapply(id_vals, function(idv) {
      cn <- names(parent_df)[toupper(names(parent_df)) == toupper(sas_trim(idv))][1]
      if (is.numeric(parent_df[[cn]])) "N" else "C"
    })
    # SEQ existence: parent.SEQ e.g., AESEQ
    seq_name <- paste0(parent_up, "SEQ")
    has_seq <- seq_name %in% parent_names
    seq_col <- names(parent_df)[toupper(names(parent_df)) == seq_name][1]
    if (is.na(seq_col)) has_seq <- FALSE
    cat(paste0("has_seq=", has_seq, "\n"))

    step_list <- list()
    # keep transposed col order for final ordering
    for (i in seq_along(id_vals)) {
      IDi <- id_vals[i]
      IDTYP <- id_types[i]
      IDCLN <- if (toupper(sas_trim(IDi)) == "USUBJID") "" else IDi
      # actual parent col name for ID
      parent_id_col <- names(parent_df)[toupper(names(parent_df)) == toupper(sas_trim(IDi))][1]
      cat(paste0("Processing ID ", i, ": ", IDi, " type=", IDTYP, " parent_col=", parent_id_col, "\n"))
      sub <- supp_work[supp_work$IDVAR == IDi & sas_trim(supp_work$RDOMAIN) == parent_up, , drop = FALSE]
      # create variable identical as parent
      if (toupper(sas_trim(IDi)) == "USUBJID") {
        # IDVAR is USUBJID: ensure USUBJID = IDVARVAL (should already match); do not overwrite if same
        # SAS does USUBJID = IDVARVAL; replicate
        sub$USUBJID <- sub$IDVARVAL
      } else {
        if (IDTYP == "N") {
          # input(trim(left(IDVARVAL)),best.) -> numeric
          sub[[parent_id_col]] <- suppressWarnings(as.numeric(sas_trim(sub$IDVARVAL)))
        } else {
          sub[[parent_id_col]] <- sub$IDVARVAL
        }
      }
      # sort sub (drop IDVARVAL IDVAR) by STUDYID RDOMAIN USUBJID [IDCLN]
      # keep all cols except IDVARVAL IDVAR for transpose input (SAS drops them in SORT out)
      sort_keys_supp <- c("STUDYID", "RDOMAIN", "USUBJID")
      if (IDCLN != "") {
        # map IDCLN to actual col name (parent_id_col)
        sort_keys_supp <- c(sort_keys_supp, parent_id_col)
      }
      # drop IDVARVAL IDVAR
      sub_sort_in <- sub[, setdiff(names(sub), c("IDVARVAL", "IDVAR")), drop = FALSE]
      sub_sorted <- sas_arrange(sub_sort_in, intersect(sort_keys_supp, names(sub_sort_in)))
      # Transpose: by STUDYID DOMAIN(=RDOMAIN) USUBJID [IDCLN], var QVAL id QNAM
      # rename RDOMAIN to DOMAIN for transpose
      sub_sorted$DOMAIN <- sub_sorted$RDOMAIN
      by_cols <- c("STUDYID", "DOMAIN", "USUBJID")
      if (IDCLN != "") by_cols <- c(by_cols, parent_id_col)
      # pivot_wider; need id_cols + names_from QNAM values_from QVAL
      # Handle potential duplicate (already checked) -> one value per cell
      # Use names_sort=FALSE to preserve appearance order
      # Ensure QNAM/QVAL character
      sub_sorted$QNAM <- as.character(sub_sorted$QNAM)
      sub_sorted$QVAL <- as.character(sub_sorted$QVAL)
      # pivot
      # tidyr requires unique id+names; if multiple rows same BY+QNAM -> error (already checked)
      trans <- sub_sorted %>%
        dplyr::select(dplyr::all_of(c(by_cols, "QNAM", "QVAL"))) %>%
        tidyr::pivot_wider(id_cols = dplyr::all_of(by_cols),
                           names_from = QNAM, values_from = QVAL,
                           values_fn = function(x) x[1],
                           names_sort = FALSE)
      # trans is tibble; convert to data.frame
      trans <- as.data.frame(trans, stringsAsFactors = FALSE, check.names = FALSE)
      cat(paste0("Transposed cols: ", paste(names(trans), collapse=","), "\n"))
      # Sort parent for merge: by STUDYID DOMAIN USUBJID [IDCLN]
      parent_sort_keys <- c("STUDYID", "DOMAIN", "USUBJID")
      if (IDCLN != "") parent_sort_keys <- c(parent_sort_keys, parent_id_col)
      parent_sorted <- sas_arrange(parent_df, intersect(parent_sort_keys, names(parent_df)))
      # Merge parent + transposed (left join), DROP = supp-only
      # BY keys: STUDYID DOMAIN USUBJID [IDCLN col]
      # Note trans DOMAIN came from RDOMAIN; parent DOMAIN same domain value
      by_join <- c("STUDYID", "DOMAIN", "USUBJID")
      if (IDCLN != "") by_join <- c(by_join, parent_id_col)
      # Ensure join key types match: if numeric ID, both numeric; if character, both character
      # For character keys, compare exact? SAS pads blanks; use as-is (haven trimmed)
      # Perform left join
      # Preserve parent row order after sort (sas_arrange already), left_join preserves left order
      step <- dplyr::left_join(parent_sorted, trans, by = by_join)
      step <- as.data.frame(step, stringsAsFactors = FALSE, check.names = FALSE)
      # Sort STEP by STUDYID DOMAIN USUBJID [SEQ if exists]
      if (has_seq) {
        step <- sas_arrange(step, c("STUDYID", "DOMAIN", "USUBJID", seq_col))
      } else {
        step <- sas_arrange(step, c("STUDYID", "DOMAIN", "USUBJID"))
      }
      step_list[[i]] <- step
    }
    # Merge subsets together
    if (length(step_list) == 1) {
      final_df <- step_list[[1]]
    } else {
      # SAS MERGE ... BY STUDYID DOMAIN USUBJID [SEQ]: full outer combine
      by_final <- c("STUDYID", "DOMAIN", "USUBJID")
      if (has_seq) by_final <- c(by_final, seq_col)
      final_df <- step_list[[1]]
      for (k in 2:length(step_list)) {
        final_df <- dplyr::full_join(final_df, step_list[[k]], by = by_final)
        final_df <- as.data.frame(final_df, stringsAsFactors = FALSE, check.names = FALSE)
      }
      if (has_seq) final_df <- sas_arrange(final_df, c("STUDYID", "DOMAIN", "USUBJID", seq_col))
      else final_df <- sas_arrange(final_df, c("STUDYID", "DOMAIN", "USUBJID"))
    }
    # Ensure variable order: parent vars first in original order, then new QNAM cols in order encountered
    parent_order <- names(parent_df)
    extra_cols <- setdiff(names(final_df), parent_order)
    # SAS transpose order is appearance order; our trans already appearance order; keep as is
    final_df <- final_df[, c(intersect(parent_order, names(final_df)), extra_cols), drop = FALSE]
    write_final(final_df)
  }
}

cat("P039 done\n")
