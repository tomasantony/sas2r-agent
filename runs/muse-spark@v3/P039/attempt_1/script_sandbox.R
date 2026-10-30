out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

root <- "/bench"
dirs_to_search <- c(
  "/bench/data/raw/cdisc-pilot/sdtm",
  "/bench/data/derived/cdisc-pilot/sdtm",
  "/bench/data/derived/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/raw",
  file.path(root, "data/raw/cdisc-pilot/sdtm")
)
dirs_to_search <- unique(dirs_to_search)

find_candidate <- function(base) {
  # base like "AE" or "SUPPAE"
  exts <- c(".xpt", ".sas7bdat", ".xpt.gz")
  for (d in dirs_to_search) {
    if (!is.na(d) && dir.exists(d)) {
      fl <- tryCatch(list.files(d, full.names = TRUE, recursive = FALSE), error=function(e) character(0))
      if (length(fl)==0) next
      # case-insensitive match base + ext
      for (f in fl) {
        bf <- basename(f)
        # strip extension
        low <- tolower(bf)
        if (low %in% paste0(tolower(base), c(".xpt",".sas7bdat",".xpt.gz",".csv"))) {
          return(f)
        }
      }
      # also try recursive one level for sdtm subdirs
    }
  }
  # recursive search in root data/raw/cdisc-pilot/sdtm if exists
  for (d in dirs_to_search) {
    if (!is.na(d) && dir.exists(d)) {
      fl2 <- tryCatch(list.files(d, full.names = TRUE, recursive = TRUE), error=function(e) character(0))
      for (f in fl2) {
        low <- tolower(basename(f))
        if (low %in% paste0(tolower(base), c(".xpt",".sas7bdat"))) {
          return(f)
        }
      }
    }
  }
  return(NULL)
}

read_any <- function(path) {
  low <- tolower(path)
  if (grepl("\\.sas7bdat$", low)) {
    df <- haven::read_sas(path)
  } else if (grepl("\\.xpt(\\.gz)?$", low)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.csv$", low)) {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("unknown ext")
  }
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}

ae_path <- find_candidate("AE")
supp_path <- find_candidate("SUPPAE")
cat("AE path:", ifelse(is.null(ae_path),"NULL",ae_path), "\n")
cat("SUPPAE path:", ifelse(is.null(supp_path),"NULL",supp_path), "\n")

if (is.null(ae_path) || is.null(supp_path)) {
  # fallback to pharmaversesdtm package if available
  cat("Trying pharmaversesdtm fallback\n")
  try({
    suppressPackageStartupMessages(library(pharmaversesdtm))
  }, silent=TRUE)
  # list objects
  if (is.null(ae_path)) {
    if (exists("ae")) {
      parent_df <- as.data.frame(ae, stringsAsFactors=FALSE, check.names=FALSE)
    } else {
      ae2 <- tryCatch({data(list="ae", package="pharmaversesdtm"); get("ae")}, error=function(e) NULL)
      if (!is.null(ae2)) parent_df <- as.data.frame(ae2, stringsAsFactors=FALSE, check.names=FALSE)
    }
  }
  if (is.null(supp_path)) {
    if (exists("suppae")) {
      supp_df <- as.data.frame(suppae, stringsAsFactors=FALSE, check.names=FALSE)
    } else {
      s2 <- tryCatch({data(list="suppae", package="pharmaversesdtm"); get("suppae")}, error=function(e) NULL)
      if (!is.null(s2)) supp_df <- as.data.frame(s2, stringsAsFactors=FALSE, check.names=FALSE)
      else {
        s3 <- tryCatch({data(list="supp_ae", package="pharmaversesdtm"); get("supp_ae")}, error=function(e) NULL)
        if (!is.null(s3)) supp_df <- as.data.frame(s3, stringsAsFactors=FALSE, check.names=FALSE)
      }
    }
  }
}
if (!exists("parent_df")) {
  stopifnot(!is.null(ae_path))
  parent_df <- read_any(ae_path)
}
if (!exists("supp_df")) {
  stopifnot(!is.null(supp_path))
  supp_df <- read_any(supp_path)
}

cat("parent rows:", nrow(parent_df), "cols:", ncol(parent_df), "\n")
cat("supp rows:", nrow(supp_df), "cols:", ncol(supp_df), "\n")

# helper: uppercase name map
pnames <- names(parent_df)
snames <- names(supp_df)
p_up <- toupper(pnames)
s_up <- toupper(snames)
get_col <- function(df, target) {
  # return actual column name matching target case-insensitive
  idx <- which(toupper(names(df))==toupper(target))
  if (length(idx)==0) return(NULL)
  names(df)[idx[1]]
}

# Required vars check
parent_req <- c("STUDYID","DOMAIN","USUBJID")
supp_req <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QVAL","QLABEL")
for (v in parent_req) stopifnot(!is.null(get_col(parent_df, v)))
for (v in supp_req) stopifnot(!is.null(get_col(supp_df, v)))

# Normalize: create working copies with uppercase canonical names for logic,
# but keep original columns for output. We'll add canonical columns.
# Convert all character haven labelled to plain character; keep numeric.
strip_char <- function(x) {
  if (is.character(x)) return(x)
  if (is.factor(x)) return(as.character(x))
  if (inherits(x, "labelled")) {
    # haven labelled: convert to character if character-like else numeric?
    return(haven::as_factor(x, levels="default"))
  }
  return(x)
}

# Ensure data.frame with check.names FALSE
# Map canonical
canon_parent <- parent_df
canon_supp <- supp_df
# rename helper: add uppercase aliases? Instead create new cols STUDYID_, etc.
# Simpler: rename all columns to uppercase for processing, remember original order/names.
orig_parent_names <- names(parent_df)
orig_parent_order <- seq_along(orig_parent_names)
# For processing use uppercase names
names(canon_parent) <- toupper(names(canon_parent))
names(canon_supp) <- toupper(names(canon_supp))

# Ensure required cols exist uppercase now
# Convert factors/labelled to character/numeric plain
for (cn in names(canon_parent)) {
  col <- canon_parent[[cn]]
  if (inherits(col, "labelled")) {
    # if all values character-like, haven::read_xpt already gives appropriate type
    # Use as_factor only if needed? Better: if label only, convert via as.vector
    canon_parent[[cn]] <- haven::zap_labels(haven::zap_formats(col))
  }
}
for (cn in names(canon_supp)) {
  col <- canon_supp[[cn]]
  if (inherits(col, "labelled")) {
    canon_supp[[cn]] <- haven::zap_labels(haven::zap_formats(col))
  }
}
# Ensure character columns are character (not factor)
for (cn in names(canon_parent)) {
  if (is.factor(canon_parent[[cn]])) canon_parent[[cn]] <- as.character(canon_parent[[cn]])
}
for (cn in names(canon_supp)) {
  if (is.factor(canon_supp[[cn]])) canon_supp[[cn]] <- as.character(canon_supp[[cn]])
}

parent_name <- "AE"
supp_name <- "SUPPAE"

# RDOMAIN = upcase(parent) filter check
# SAS: data S2P_RDOMCHECK; set supp (where=(RDOMAIN="%upcase(&parent)")); run;
# exact case-sensitive match to "AE"
rdom_vals <- canon_supp[["RDOMAIN"]]
# RDOMAIN may have NA; treat NA as not matching
match_rdom <- !is.na(rdom_vals) & rdom_vals=="AE"
n_match <- sum(match_rdom, na.rm=TRUE)
cat("SUPP rows with RDOMAIN=AE:", n_match, "\n")

is_empty_sas <- function(x) {
  is.na(x) | trimws(as.character(x))=="" 
}

# If zero -> simple copy
do_copy <- FALSE
if (n_match==0) do_copy <- TRUE

# Checks on supplemental: IGNOR where strip(RDOMAIN)=='' or strip(USUBJID)=='' or strip(QNAM)==''
if (!do_copy) {
  ignor_flag <- is_empty_sas(canon_supp[["RDOMAIN"]]) | is_empty_sas(canon_supp[["USUBJID"]]) | is_empty_sas(canon_supp[["QNAM"]])
  ignor_flag[is.na(ignor_flag)] <- TRUE
  supp_ignor <- canon_supp[ignor_flag, , drop=FALSE]
  supp_accept <- canon_supp[!ignor_flag, , drop=FALSE]
  cat("IGNOR rows:", nrow(supp_ignor), " ACCEPT rows:", nrow(supp_accept), "\n")

  # uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM
  # SAS PROC SORT NODUPKEY DUPOUT: detects duplicates (keeps first, outputs extras)
  # If any duplicates -> terminate RC=10. Here we check.
  key_cols <- c("USUBJID","RDOMAIN","IDVAR","IDVARVAL","QNAM")
  # SAS compares with trailing blanks padded? Use trimmed? Use exact with NA handling: NA==NA? In SAS missing==missing for sort? Two missings would be dup but we removed empties for 3 cols; IDVAR/IDVARVAL may be empty.
  # Normalize NA to "" and trim? For dup detection SAS uses actual values with padding. We'll use exact as stored with NA->"".
  tmpk <- supp_accept[, key_cols, drop=FALSE]
  for (k in key_cols) {
    v <- tmpk[[k]]
    v[is.na(v)] <- ""
    tmpk[[k]] <- v
  }
  dup_flag <- duplicated(tmpk)
  n_dup <- sum(dup_flag)
  cat("dup check n_dup(extra):", n_dup, "\n")
  if (n_dup != 0) {
    stop("SUPP duplicates on keys - SAS would terminate RC=10")
  }

  # Same QNAM with different labels: group by upcase(QNAM)
  supp_accept[["QNAM_UP"]] <- toupper(supp_accept[["QNAM"]])
  # distinct QNAM_UP x QLABEL
  fq <- unique(supp_accept[, c("QNAM_UP","QLABEL"), drop=FALSE])
  # count per QNAM_UP
  cnt <- table(fq[["QNAM_UP"]])
  nonuniq <- names(cnt[cnt>1])
  cat("nonuniq QNAM:", length(nonuniq), "\n")
  if (length(nonuniq)>0) {
    print(nonuniq)
    stop("Same QNAM different QLABEL - SAS would terminate RC=7")
  }
  # drop helper
  supp_accept[["QNAM_UP"]] <- NULL

  # Creating version with IDVAR substitution
  # if IDVAR=='' and IDVARVAL=='' then IDVAR='USUBJID', IDVARVAL=USUBJID
  # else if IDVAR=='' and IDVARVAL!='' then terminate RC=8
  idvar_empty <- is_empty_sas(supp_accept[["IDVAR"]])
  idvarval_empty <- is_empty_sas(supp_accept[["IDVARVAL"]])
  # check error condition
  if (any(idvar_empty & !idvarval_empty, na.rm=TRUE)) {
    stop("IDVAR empty while IDVARVAL not empty - SAS RC=8")
  }
  need_sub <- idvar_empty & idvarval_empty
  cat("IDVAR substitution rows:", sum(need_sub, na.rm=TRUE), "\n")
  if (any(need_sub)) {
    supp_accept[need_sub, "IDVAR"] <- "USUBJID"
    # IDVARVAL = USUBJID (character, handle NA)
    supp_accept[need_sub, "IDVARVAL"] <- supp_accept[need_sub, "USUBJID"]
  }

  # Check QNAM already on parent
  parent_varnames <- toupper(names(canon_parent))  # NAME from contents is uppercase
  supp_qnams <- unique(supp_accept[["QNAM"]])
  supp_qnams <- supp_qnams[!is.na(supp_qnams) & trimws(supp_qnams)!=""]
  # SAS merge by NAME case-sensitive: NAME is uppercase, QNAM as is. So match exact.
  # We'll do exact match (case-sensitive) to mimic SAS.
  overlap_exact <- intersect(unique(canon_parent %>% names()), supp_qnams)  # names are uppercase already
  # Actually canon_parent names are uppercase, so this is exact vs uppercase QNAMs.
  # For safety also check case-insensitive? SAS would be case-sensitive, but QNAMs are uppercase so same.
  cat("QNAM overlap with parent:", length(overlap_exact), "\n")
  if (length(overlap_exact)>0) {
    print(overlap_exact)
    # rename those QNAMs to QNAM_2
    for (qn in overlap_exact) {
      idx <- !is.na(supp_accept[["QNAM"]]) & supp_accept[["QNAM"]]==qn
      supp_accept[idx, "QNAM"] <- paste0(qn, "_2")
    }
  }

  # Determine IDVAR values where RDOMAIN="AE"
  supp_rdom <- supp_accept[!is.na(supp_accept[["RDOMAIN"]]) & supp_accept[["RDOMAIN"]]=="AE", , drop=FALSE]
  cat("supp_rdom rows:", nrow(supp_rdom), "\n")
  if (nrow(supp_rdom)==0) {
    do_copy <- TRUE
  } else {
    ids_raw <- unique(trimws(as.character(supp_rdom[["IDVAR"]])))
    ids_raw <- ids_raw[!is.na(ids_raw) & ids_raw!=""]
    # PROC FREQ out order is ascending; emulate SAS sort (binary, uppercase first)
    # Use base sort with C locale? Simple sort()
    # SAS sorts character with uppercase before lowercase (ASCII). R sort in C locale does same.
    # We'll sort using order with method="radix" which is byte-wise? Use sort().
    ids_sorted <- sort(ids_raw)
    cat("IDVAR values:", paste(ids_sorted, collapse=","), "\n")
    IDMAX <- length(ids_sorted)
    # Check each ID present on parent, determine type
    id_types <- character(IDMAX)
    for (i in seq_len(IDMAX)) {
      idn <- ids_sorted[i]
      # find parent var exact (uppercase names, idn as is trimmed; but idn may be uppercase)
      # SAS compares trim(left(NAME)) = ID -> case-sensitive
      found <- idn %in% names(canon_parent)
      if (!found) {
        # try case-insensitive? SAS would terminate; report
        # check uppercase version
        if (toupper(idn) %in% toupper(names(canon_parent))) {
          # case differs -> SAS would still terminate because case-sensitive, but data is uppercase so unlikely
          stop(paste0("IDVAR ",idn," not present on parent (case-sensitive) - SAS RC=9"))
        } else {
          stop(paste0("IDVAR ",idn," not present on parent - SAS RC=9"))
        }
      }
      col <- canon_parent[[idn]]
      if (is.numeric(col)) id_types[i] <- "N" else id_types[i] <- "C"
    }
    cat("id_types:", paste(id_types, collapse=","), "\n")

    # Determine if AESEQ exists on parent
    seq_var <- paste0(parent_name, "SEQ")  # "AESEQ"
    has_seq <- seq_var %in% names(canon_parent)
    cat("has_seq:", has_seq, "\n")

    step_list <- list()
    drop_list <- list()
    for (i in seq_len(IDMAX)) {
      idn <- ids_sorted[i]
      idtyp <- id_types[i]
      idcln <- if (idn=="USUBJID") character(0) else idn
      # subset supp where IDVAR==idn and RDOMAIN=="AE"
      # IDVAR comparison: SAS where=(IDVAR="&&ID&i") is case-sensitive exact (with padding). Use trimmed exact?
      # Use exact as stored vs idn (trimmed). Since IDVAR values may have trailing blanks, compare trimmed.
      sub <- supp_accept[!is.na(supp_accept[["IDVAR"]]) & trimws(supp_accept[["IDVAR"]])==idn & !is.na(supp_accept[["RDOMAIN"]]) & supp_accept[["RDOMAIN"]]=="AE", , drop=FALSE]
      # create merge variable idn
      if (idn=="USUBJID") {
        # USUBJID already; ensure IDVARVAL equals? No new var. But need to ensure sub USUBJID stays.
        # nothing
      } else {
        if (idtyp=="N") {
          # input(trim(left(IDVARVAL)),best.)
          vv <- trimws(as.character(sub[["IDVARVAL"]]))
          # best. informat: allow numbers; use suppressWarnings as.numeric
          num <- suppressWarnings(as.numeric(vv))
          sub[[idn]] <- num
        } else {
          sub[[idn]] <- as.character(sub[["IDVARVAL"]])
        }
      }
      # sort sub by STUDYID RDOMAIN USUBJID [idcln], drop IDVARVAL IDVAR
      # For transpose BY: STUDYID DOMAIN USUBJID [idcln]
      # Create DOMAIN from RDOMAIN
      sub[["DOMAIN"]] <- sub[["RDOMAIN"]]
      # Select cols for transpose: BY + QNAM QVAL QLABEL
      by_vars <- c("STUDYID","DOMAIN","USUBJID", idcln)
      # Ensure by_vars exist in sub (idn created)
      # Drop IDVARVAL IDVAR for transpose input (keep BY + QNAM QVAL QLABEL)
      # Transpose: by STUDYID DOMAIN USUBJID [idcln]; var qval; id qnam; idlabel qlabel
      # pivot_wider
      # Need to handle QVAL as character
      sub[["QVAL"]] <- as.character(sub[["QVAL"]])
      sub[["QNAM"]] <- as.character(sub[["QNAM"]])
      # pivot
      # Use tidyr
      wide_input <- sub[, c(by_vars, "QNAM","QVAL"), drop=FALSE]
      # In case of duplicate BY+QNAM (should not happen due to uniqueness, but after IDVAR substitution + RDOMAIN filter, uniqueness holds)
      # pivot_wider
      transposed <- wide_input %>%
        tidyr::pivot_wider(names_from = QNAM, values_from = QVAL, values_fn = function(x) x[1])
      cat("i=",i," id=",idn," sub rows:",nrow(sub)," transposed rows:",nrow(transposed)," new cols:",ncol(transposed)-length(by_vars),"\n")
      # Sort parent for merge: by STUDYID DOMAIN USUBJID [idcln]
      # Parent BY vars: STUDYID DOMAIN USUBJID [idcln] ; note parent DOMAIN is DOMAIN, supp DOMAIN from RDOMAIN
      # Ensure types match for join: for character keys, compare trimmed? Create join keys normalized?
      # For numeric id, both numeric.
      # For character keys, SAS pads blanks, so trimmed equality. We'll join on trimmed values by creating temp keys?
      # Simplest: create normalized join keys (trimmed, NA->"") for character, then join, then drop temp.
      # But need to preserve original parent values in output.
      # Create copies for join
      par_sort <- canon_parent
      # Normalize function
      norm_char <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }
      # Build join key data for parent and transposed
      # For each by_var, create _jk column
      for (bv in by_vars) {
        # bv exists in both?
        if (!bv %in% names(par_sort)) stop(paste0("parent missing BY ",bv))
        if (!bv %in% names(transposed)) {
          # transposed may lack id column if all NA? Create NA
          transposed[[bv]] <- NA
        }
        pv <- par_sort[[bv]]
        tv <- transposed[[bv]]
        if (is.numeric(pv) || is.numeric(tv)) {
          # ensure numeric both
          par_sort[[paste0(bv,"_JK")]] <- suppressWarnings(as.numeric(pv))
          transposed[[paste0(bv,"_JK")]] <- suppressWarnings(as.numeric(tv))
        } else {
          par_sort[[paste0(bv,"_JK")]] <- norm_char(pv)
          transposed[[paste0(bv,"_JK")]] <- norm_char(tv)
        }
      }
      jk_vars <- paste0(by_vars, "_JK")
      # Left join parent with transposed on jk_vars, bringing new QNAM cols
      new_cols <- setdiff(names(transposed), c(by_vars, jk_vars))
      # transposed join table: jk + new_cols
      trans_join <- transposed[, c(jk_vars, new_cols), drop=FALSE]
      # To avoid duplicate column name conflicts, new_cols should not overlap parent names (except BY which we excluded).
      # But if QNAM_2 overlap handled; if still overlap (should not after rename), SAS would overwrite? Actually SAS merge would overwrite with supp value? In SAS merge, if same variable in both, second dataset's value overwrites? For overlapping QNAM already renamed, no overlap. So new_cols disjoint from parent except BY.
      # Remove any new_cols that already in parent (should be none after rename) – but if illegal, keep supp?
      # Keep as is; if overlap, suffix? SAS merge would take supp value for overlapping? We'll let supp overwrite via coalesce? Simpler: drop overlapping from parent? But after rename none.
      overlap2 <- intersect(new_cols, names(par_sort))
      # remove JK cols from overlap check
      overlap2 <- setdiff(overlap2, jk_vars)
      if (length(overlap2)>0) {
        cat("overlap after rename:", paste(overlap2,collapse=","), "\n")
        # SAS merge: value from supp (second) overwrites if both non-missing? Actually merge takes value from rightmost dataset where non-missing? For our left join, we want supp value to overwrite? But parent already has that var; supp transposed var same name would be from QNAM same as parent var – but we renamed, so shouldn't happen.
        # If still happens, drop parent version and keep supp?
        par_sort <- par_sort[, setdiff(names(par_sort), overlap2), drop=FALSE]
      }
      # left join
      # Use dplyr left_join on jk_vars
      # Ensure unique trans_join keys? Should be unique per BY (one row per BY). If duplicates, take first.
      trans_join_dedup <- trans_join[!duplicated(trans_join[, jk_vars, drop=FALSE]), , drop=FALSE]
      merged <- dplyr::left_join(par_sort, trans_join_dedup, by=jk_vars)
      # drops: supp rows with no parent (anti-join)
      # SAS: if SUPP and not PARENT then output DROP. PARENT here means parent key exists.
      # Build parent keys set
      par_keys <- par_sort[, jk_vars, drop=FALSE]
      # anti-join transposed vs parent
      # Create key strings
      key_par <- do.call(paste, c(par_keys, sep="\r"))
      key_trans <- do.call(paste, c(transposed[, jk_vars, drop=FALSE], sep="\r"))
      drop_idx <- !(key_trans %in% key_par)
      drop_df <- transposed[drop_idx, , drop=FALSE]
      # drop JK cols from merged and drop_df for step storage? Keep merged with JK for now, will remove later.
      # For STEP sorting: sort by STUDYID DOMAIN USUBJID [AESEQ if has_seq]
      # Use SAS order: missing first, character uppercase-first.
      # We'll order via order() with na.last=FALSE. For character, R order may be locale-dependent; force C locale via xtfrm? Use stringr? We'll just use order.
      # Remove JK? Keep for sorting? Sort using original cols.
      # First remove JK cols from merged for step? Keep them temporarily for final merge across steps? Final merge needs BY STUDYID DOMAIN USUBJID AESEQ.
      # Store merged without JK? But need to merge steps later on final BY. We'll store merged without JK plus keep original BY cols.
      # Drop JK cols
      merged_njk <- merged[, setdiff(names(merged), jk_vars), drop=FALSE]
      # drop_df keep as is (without JK)
      drop_njk <- drop_df[, setdiff(names(drop_df), jk_vars), drop=FALSE]
      # Sort merged_njk by final sort keys (STUDYID DOMAIN USUBJID [AESEQ])
      if (has_seq) {
        sk <- c("STUDYID","DOMAIN","USUBJID", seq_var)
      } else {
        sk <- c("STUDYID","DOMAIN","USUBJID")
      }
      # order
      # Build order args: for each key, use xtfrm for character? Use order with na.last=FALSE
      ord_args <- list()
      for (k in sk) {
        v <- merged_njk[[k]]
        # For character, SAS sorts with trailing blanks ignored? Use trimmed? Use as is.
        # Missing (NA or "")? SAS missing char is "" (blank). In R, NA and "" both considered missing? SAS "" sorts first. We'll map NA->"" and order.
        if (is.character(v)) {
          v2 <- v; v2[is.na(v2)] <- ""
          # To mimic SAS uppercase-before-lowercase, use C locale ordering via xtfrm with LC_COLLATE=C?
          # R order uses locale; we can transform via utf8 code points: order by charToRaw? Simpler: use order(v2) – in most locales uppercase < lowercase anyway.
          ord_args[[k]] <- v2
        } else {
          ord_args[[k]] <- v
        }
      }
      # do order
      o <- do.call(order, c(ord_args, list(na.last=FALSE, method="radix")))
      merged_sorted <- merged_njk[o, , drop=FALSE]
      step_list[[i]] <- merged_sorted
      drop_list[[i]] <- drop_njk
    }

    # Merging subsets together: merge steps by STUDYID DOMAIN USUBJID [AESEQ]
    if (IDMAX==1) {
      final_df <- step_list[[1]]
    } else {
      if (has_seq) fby <- c("STUDYID","DOMAIN","USUBJID", seq_var) else fby <- c("STUDYID","DOMAIN","USUBJID")
      # full join steps successively
      final_df <- step_list[[1]]
      for (i in 2:IDMAX) {
        # full_join on fby
        # To avoid .x/.y, ensure non-key overlapping cols are disjoint? They should be disjoint (different QNAMs). If overlap, coalesce?
        # Use full_join then coalesce overlapping?
        nxt <- step_list[[i]]
        # find overlapping non-key cols
        ov <- intersect(names(final_df), names(nxt))
        ov <- setdiff(ov, fby)
        if (length(ov)>0) {
          # coalesce: full join with suffix then coalesce
          tmp <- dplyr::full_join(final_df, nxt, by=fby, suffix=c("","_Y"))
          for (c in ov) {
            yc <- paste0(c,"_Y")
            if (yc %in% names(tmp)) {
              # coalesce: take first non-NA (SAS merge takes last non-missing? Actually merge of two steps both having parent rows: both have same parent values for overlapping? They would be same. For new cols, no overlap. So just coalesce.
              a <- tmp[[c]]; b <- tmp[[yc]]
              # for character, "" is missing in SAS? SAS merge: if one is missing (blank) the other fills? Actually SAS merge overwrites with blank? No, missing values do not overwrite? In DATA merge, missing values overwrite? Let's recall: In SAS merge, if variable in both, value from second dataset overwrites even if missing? No, I think missing does not overwrite? Actually in MERGE, if variable comes from multiple datasets, the value is taken from the dataset that contributes the observation? Hmm. For our case parent values same, so fine.
              # coalesce NA; also treat "" as missing for character?
              if (is.character(a) || is.character(b)) {
                a[is.na(a)] <- ""; b[is.na(b)] <- ""
                tmp[[c]] <- ifelse(a!="" , a, b)
              } else {
                tmp[[c]] <- dplyr::coalesce(a,b)
              }
              tmp[[yc]] <- NULL
            }
          }
          final_df <- tmp
        } else {
          final_df <- dplyr::full_join(final_df, nxt, by=fby)
        }
        # re-sort
        ord_args2 <- list()
        for (k in fby) {
          v <- final_df[[k]]
          if (is.character(v)) { v2 <- v; v2[is.na(v2)] <- ""; ord_args2[[k]] <- v2 } else ord_args2[[k]] <- v
        }
        o2 <- do.call(order, c(ord_args2, list(na.last=FALSE, method="radix")))
        final_df <- final_df[o2, , drop=FALSE]
      }
    }
  }
}

if (do_copy) {
  cat("Simple copy of parent\n")
  final_df <- canon_parent
} else {
  # final_df already built
  cat("final rows:", nrow(final_df), "cols:", ncol(final_df), "\n")
}

# Output dataset AEFULL: set S2P_FINAL
# Column order: parent original order first, then new cols in alphabetical? Preserve SAS: parent order then transposed order (order of appearance = sorted QNAM?).
# We'll order: parent cols in original order (uppercase names), then extra cols sorted alphabetically for determinism.
parent_upper_order <- toupper(orig_parent_names)
extra_cols <- setdiff(names(final_df), c(parent_upper_order, paste0(parent_upper_order,"_JK")))
# extra includes DOMAIN? DOMAIN already in parent. So extra are QNAM-derived.
extra_sorted <- sort(extra_cols)
ordered_names <- c(parent_upper_order[parent_upper_order %in% names(final_df)], extra_sorted)
# any remaining (e.g., DOMAIN if renamed?) add
remaining <- setdiff(names(final_df), ordered_names)
ordered_names <- c(ordered_names, remaining)
final_df <- final_df[, ordered_names, drop=FALSE]

# Restore original case? Requirement says same case is fine. We'll output uppercase (SAS default).
# But to match SAS dataset variable names, uppercase is correct (AE vars are uppercase).
# Format dates: look for Date/POSIX columns -> ISO
for (cn in names(final_df)) {
  col <- final_df[[cn]]
  if (inherits(col, "Date")) {
    final_df[[cn]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, c("POSIXct","POSIXt"))) {
    final_df[[cn]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  } else if (inherits(col, "labelled")) {
    final_df[[cn]] <- haven::zap_labels(haven::zap_formats(col))
  }
}
# Ensure data.frame
final_df <- as.data.frame(final_df, stringsAsFactors=FALSE, check.names=FALSE)

# Write CSV
write.csv(final_df, file.path(out, "aefull.csv"), row.names=FALSE, na="")
cat("Wrote", file.path(out, "aefull.csv"), "rows", nrow(final_df), "\n")
