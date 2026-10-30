```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
trunc_str <- function(x, len) {
  sapply(as.character(x), function(v) {
    if (is.na(v)) return(NA_character_)
    substr(v, 1, len)
  }, USE.NAMES = FALSE)
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  sapply(seq_along(args[[1]]), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v))
      if (v == "") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}
get_colname <- function(df, target) {
  nm <- names(df)
  idx <- which(toupper(nm) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  nm[idx[1]]
}
get_col <- function(df, target) {
  cn <- get_colname(df, target)
  if (is.na(cn)) return(rep(NA_character_, nrow(df)))
  df[[cn]]
}
to_subjid_char <- function(v) {
  sapply(v, function(x) {
    if (is.na(x)) return(NA_character_)
    if (is.numeric(x)) {
      # emulate SAS BEST12. stripped by CATX
      s <- format(x, scientific = FALSE, trim = TRUE, nsmall = 0)
      s <- trimws(s)
      return(s)
    }
    trimws(as.character(x))
  }, USE.NAMES = FALSE)
}
# SAS DATE11. -> YYYY-MM-DD : one strict format after removing dashes/spaces
parse_date11_to_iso <- function(v) {
  sapply(v, function(x) {
    if (is.na(x)) return(NA_character_)
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
    if (is.numeric(x) && !is.na(suppressWarnings(as.numeric(x)))) {
      # numeric excel serial unlikely; return NA
      return(NA_character_)
    }
    s <- trimws(as.character(x))
    if (is.na(s) || s == "" || toupper(s) %in% c(".", "NA", "NULL")) return(NA_character_)
    s_up <- toupper(s)
    s_nodash <- gsub("-", "", s_up, fixed = TRUE)
    s_nodash <- gsub(" ", "", s_nodash, fixed = TRUE)
    s_nodash <- gsub("/", "", s_nodash, fixed = TRUE)
    # single strict format %d%b%Y
    d <- suppressWarnings(as.Date(s_nodash, format = "%d%b%Y"))
    if (is.na(d)) return(NA_character_)
    format(d, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}
# generic char->Date with ONE format decided once per column (PROC IMPORT guessing)
parse_char_col_to_date <- function(vec) {
  # vec: character vector (or mixed); returns Date vector
  n <- length(vec)
  outd <- rep(as.Date(NA), n)
  # handle Date/POSIXct input separately by caller
  chr <- sapply(vec, function(x) {
    if (is.na(x)) return(NA_character_)
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
    trimws(as.character(x))
  }, USE.NAMES = FALSE)
  is_missing <- is.na(chr) | chr == "" | toupper(chr) %in% c(".", "NA", "NULL")
  vals <- chr[!is_missing]
  if (length(vals) == 0) return(outd)
  # detect type once
  is_iso <- all(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", vals))
  is_slash <- all(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", vals))
  is_ymd_slash <- all(grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", vals))
  is_date11dash <- all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", vals))
  is_date11nodash <- all(grepl("^[0-9]{1,2}[A-Za-z]{3}[0-9]{4}$", vals))
  is_date11 <- all(grepl("^[0-9]{1,2}-?[A-Za-z]{3}-?[0-9]{4}$", toupper(vals)))
  fmt <- NULL
  if (is_iso) fmt <- "%Y-%m-%d"
  else if (is_ymd_slash) fmt <- "%Y/%m/%d"
  else if (is_slash) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", vals)))
    # PROC IMPORT logic: if any first field > 12 then DDMMYY else MMDDYY
    if (any(!is.na(first_fields) & first_fields > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
  } else if (is_date11) {
    # normalize then single format
    chr_norm <- toupper(gsub("-", "", chr, fixed = TRUE))
    chr_norm <- gsub(" ", "", chr_norm, fixed = TRUE)
    d <- suppressWarnings(as.Date(chr_norm, format = "%d%b%Y"))
    outd <- d
    return(outd)
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}.*", vals))) {
    # datetime, take date part
    d <- suppressWarnings(as.Date(substr(chr, 1, 10), format = "%Y-%m-%d"))
    outd <- d
    return(outd)
  } else {
    # fallback: try ISO
    d <- suppressWarnings(as.Date(chr, format = "%Y-%m-%d"))
    # try excel numeric strings? leave NA
    outd <- d
    return(outd)
  }
  d <- suppressWarnings(as.Date(chr, format = fmt))
  outd <- d
  outd
}
col_to_date <- function(df, colname) {
  v <- df[[colname]]
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    # if numeric excel dates already would be Date; treat as NA
    return(rep(as.Date(NA), length(v)))
  }
  parse_char_col_to_date(as.character(v))
}
find_date_col <- function(df, prefer_patterns) {
  nms <- names(df)
  # try prefer patterns in order
  for (pat in prefer_patterns) {
    cand <- nms[grepl(pat, nms, ignore.case = TRUE)]
    for (c in cand) {
      d <- try(col_to_date(df, c), silent = TRUE)
      if (!inherits(d, "try-error") && any(!is.na(d))) return(c)
    }
  }
  # any Date class column
  for (c in nms) {
    v <- df[[c]]
    if (inherits(v, "Date") || inherits(v, "POSIXct")) return(c)
  }
  # any column with DT/DATE/DTC in name that parses
  for (c in nms[grepl("DT|DATE|DTC|DAY|TIME", nms, ignore.case = TRUE)]) {
    d <- try(col_to_date(df, c), silent = TRUE)
    if (!inherits(d, "try-error") && any(!is.na(d))) return(c)
  }
  # any column that parses as date
  for (c in nms) {
    if (toupper(c) %in% c("SUBJID", "USUBJID", "STUDYID")) next
    v <- df[[c]]
    if (is.character(v) || is.numeric(v)) {
      d <- try(col_to_date(df, c), silent = TRUE)
      if (!inherits(d, "try-error") && any(!is.na(d))) {
        # require at least half non-missing parse? just any
        return(c)
      }
    }
  }
  NA_character_
}

cat("Reading raw EDC files...\n")
edc_tu <- as.data.frame(readxl::read_excel(file.path(rawdir, "edc_tu.xlsx")), stringsAsFactors = FALSE)
edc_dm_raw <- as.data.frame(readxl::read_excel(file.path(rawdir, "edc_dm.xlsx")), stringsAsFactors = FALSE)
edc_ex_raw <- as.data.frame(readxl::read_excel(file.path(rawdir, "edc_ex.xlsx")), stringsAsFactors = FALSE)
ivrs_rand_raw <- as.data.frame(readxl::read_excel(file.path(rawdir, "ivrs_rand.xlsx")), stringsAsFactors = FALSE)
cat(paste0("edc_tu rows=", nrow(edc_tu), " cols=", paste(names(edc_tu), collapse=","), "\n"))
cat(paste0("edc_dm cols=", paste(names(edc_dm_raw), collapse=","), "\n"))
cat(paste0("edc_ex cols=", paste(names(edc_ex_raw), collapse=","), "\n"))
cat(paste0("ivrs cols=", paste(names(ivrs_rand_raw), collapse=","), "\n"))

# 1. Prepare DM Reference Data : try to reuse existing derived DM if present
dm_for_tu <- NULL
dm_files <- try(list.files("D:/meta_hackathon/sas2r-agent/output/sas/datasets", pattern = "(?i)\\bdm\\.(xlsx|csv)$", recursive = TRUE, full.names = TRUE), silent = TRUE)
if (!inherits(dm_files, "try-error") && length(dm_files) > 0) {
  cat(paste0("Found DM file(s): ", paste(dm_files, collapse=";"), "\n"))
  for (f in dm_files) {
    dmt <- try({
      if (grepl("\\.xlsx$", f, ignore.case = TRUE)) as.data.frame(readxl::read_excel(f), stringsAsFactors = FALSE)
      else read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    }, silent = TRUE)
    if (!inherits(dmt, "try-error") && nrow(dmt) > 0) {
      nms_up <- toupper(names(dmt))
      if ("USUBJID" %in% nms_up && "RFSTDTC" %in% nms_up) {
        usub_c <- names(dmt)[nms_up == "USUBJID"][1]
        rf_c <- names(dmt)[nms_up == "RFSTDTC"][1]
        dm_for_tu <- data.frame(USUBJID = as.character(dmt[[usub_c]]), RFSTDTC = as.character(dmt[[rf_c]]), stringsAsFactors = FALSE)
        # normalize RFSTDTC to ISO YYYY-MM-DD (take first 10 if datetime)
        dm_for_tu$RFSTDTC <- sapply(dm_for_tu$RFSTDTC, function(x) {
          if (is.na(x)) return(NA_character_)
          if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
          s <- trimws(as.character(x))
          if (s == "" || s == ".") return(NA_character_)
          if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) return(substr(s, 1, 10))
          # try parse then format
          d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
          if (!is.na(d)) return(format(d, "%Y-%m-%d"))
          # DATE11?
          iso <- parse_date11_to_iso(s)
          if (!is.na(iso)) return(iso)
          s
        }, USE.NAMES = FALSE)
        cat(paste0("Using DM file ", f, " rows=", nrow(dm_for_tu), "\n"))
        break
      }
    }
  }
}
if (is.null(dm_for_tu)) {
  cat("Deriving RFSTDTC from raw EDC/IVRS (standard CDISC reference start)...\n")
  # locate SUBJID columns
  subj_dm_c <- get_colname(edc_dm_raw, "SUBJID")
  subj_ex_c <- get_colname(edc_ex_raw, "SUBJID")
  subj_rs_c <- get_colname(ivrs_rand_raw, "SUBJID")
  if (is.na(subj_dm_c)) {
    # fallback: first column containing SUBJ
    cand <- names(edc_dm_raw)[grepl("SUBJ", names(edc_dm_raw), ignore.case = TRUE)]
    if (length(cand) > 0) subj_dm_c <- cand[1]
  }
  if (is.na(subj_ex_c)) {
    cand <- names(edc_ex_raw)[grepl("SUBJ", names(edc_ex_raw), ignore.case = TRUE)]
    if (length(cand) > 0) subj_ex_c <- cand[1]
  }
  if (is.na(subj_rs_c)) {
    cand <- names(ivrs_rand_raw)[grepl("SUBJ", names(ivrs_rand_raw), ignore.case = TRUE)]
    if (length(cand) > 0) subj_rs_c <- cand[1]
  }
  # check for RFSTDTC already in edc_dm
  rf_c_existing <- get_colname(edc_dm_raw, "RFSTDTC")
  rf_map <- NULL
  if (!is.na(rf_c_existing)) {
    d <- col_to_date(edc_dm_raw, rf_c_existing)
    subjv <- to_subjid_char(edc_dm_raw[[subj_dm_c]])
    rf_map <- data.frame(SUBJ = subjv, RF = d, stringsAsFactors = FALSE)
    rf_map <- aggregate(RF ~ SUBJ, data = rf_map, FUN = function(x) suppressWarnings(min(x, na.rm = TRUE)))
    rf_map$RF[is.infinite(rf_map$RF)] <- NA
  } else {
    # EX first exposure date
    ex_date_c <- find_date_col(edc_ex_raw, c("EXSTDTC", "EXSTDT", "EXSTD", "EX_START", "^EX.*DT", "^EX.*DATE"))
    cat(paste0("EX date col=", ex_date_c, "\n"))
    ex_min <- NULL
    if (!is.na(ex_date_c) && !is.na(subj_ex_c)) {
      d <- col_to_date(edc_ex_raw, ex_date_c)
      subjv <- to_subjid_char(edc_ex_raw[[subj_ex_c]])
      tmp <- data.frame(SUBJ = subjv, DT = d, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$SUBJ) & tmp$SUBJ != "", ]
      if (nrow(tmp) > 0) {
        ex_min <- aggregate(DT ~ SUBJ, data = tmp, FUN = function(x) suppressWarnings(min(x, na.rm = TRUE)))
        names(ex_min)[2] <- "EXDT"
        ex_min$EXDT[is.infinite(ex_min$EXDT)] <- NA
        # min returns numeric if Date? ensure Date
        ex_min$EXDT <- as.Date(ex_min$EXDT, origin = "1970-01-01")
      }
    }
    # RAND date
    rand_date_c <- find_date_col(ivrs_rand_raw, c("RANDDTC", "RANDDT", "RAND.*DT", "RAND.*DATE", "RANDOM.*DT"))
    cat(paste0("RAND date col=", rand_date_c, "\n"))
    rand_min <- NULL
    if (!is.na(rand_date_c) && !is.na(subj_rs_c)) {
      d <- col_to_date(ivrs_rand_raw, rand_date_c)
      subjv <- to_subjid_char(ivrs_rand_raw[[subj_rs_c]])
      tmp <- data.frame(SUBJ = subjv, DT = d, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$SUBJ) & tmp$SUBJ != "", ]
      if (nrow(tmp) > 0) {
        rand_min <- aggregate(DT ~ SUBJ, data = tmp, FUN = function(x) suppressWarnings(min(x, na.rm = TRUE)))
        names(rand_min)[2] <- "RANDDT"
        rand_min$RANDDT <- as.Date(rand_min$RANDDT, origin = "1970-01-01")
      }
    }
    # DM date fallback (e.g., enrollment)
    dm_date_c <- find_date_col(edc_dm_raw, c("RFSTDTC", "RANDDTC", "RANDDT", "ENR.*DT", "DM.*DT"))
    cat(paste0("DM date col=", dm_date_c, "\n"))
    dm_min <- NULL
    if (!is.na(dm_date_c) && !is.na(subj_dm_c)) {
      d <- col_to_date(edc_dm_raw, dm_date_c)
      subjv <- to_subjid_char(edc_dm_raw[[subj_dm_c]])
      tmp <- data.frame(SUBJ = subjv, DT = d, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$SUBJ) & tmp$SUBJ != "", ]
      if (nrow(tmp) > 0) {
        dm_min <- aggregate(DT ~ SUBJ, data = tmp, FUN = function(x) suppressWarnings(min(x, na.rm = TRUE)))
        names(dm_min)[2] <- "DMDT"
        dm_min$DMDT <- as.Date(dm_min$DMDT, origin = "1970-01-01")
      }
    }
    # union of subjects
    all_subj <- unique(c(
      if (!is.null(ex_min)) ex_min$SUBJ,
      if (!is.null(rand_min)) rand_min$SUBJ,
      if (!is.null(dm_min)) dm_min$SUBJ,
      to_subjid_char(edc_dm_raw[[subj_dm_c]]),
      to_subjid_char(get_col(edc_tu, "SUBJID"))
    ))
    all_subj <- all_subj[!is.na(all_subj) & all_subj != ""]
    rf_map <- data.frame(SUBJ = all_subj, stringsAsFactors = FALSE)
    if (!is.null(ex_min)) rf_map <- merge(rf_map, ex_min, by = "SUBJ", all.x = TRUE)
    else rf_map$EXDT <- as.Date(NA)
    if (!is.null(rand_min)) rf_map <- merge(rf_map, rand_min, by = "SUBJ", all.x = TRUE)
    else rf_map$RANDDT <- as.Date(NA)
    if (!is.null(dm_min)) rf_map <- merge(rf_map, dm_min, by = "SUBJ", all.x = TRUE)
    else rf_map$DMDT <- as.Date(NA)
    # RFSTDTC = first exposure if present else randomization else DM date (standard)
    rf_map$RF <- as.Date(ifelse(!is.na(rf_map$EXDT), rf_map$EXDT,
                        ifelse(!is.na(rf_map$RANDDT), rf_map$RANDDT, rf_map$DMDT)),
                        origin = "1970-01-01")
  }
  # build dm_for_tu with USUBJID + RFSTDTC ISO
  if (!is.null(rf_map) && "RF" %in% names(rf_map)) {
    rf_map$USUBJID <- trunc_str(sas_catx("-", rep("ONCO-2026", nrow(rf_map)), rf_map$SUBJ), 25)
    rf_map$RFSTDTC <- format(rf_map$RF, "%Y-%m-%d")
    rf_map$RFSTDTC[is.na(rf_map$RFSTDTC)] <- NA_character_
    dm_for_tu <- rf_map[, c("USUBJID", "RFSTDTC")]
  } else {
    # fallback from SUBJ list with missing RFSTDTC
    subjv <- unique(to_subjid_char(get_col(edc_tu, "SUBJID")))
    dm_for_tu <- data.frame(USUBJID = trunc_str(paste0("ONCO-2026-", subjv), 25), RFSTDTC = NA_character_, stringsAsFactors = FALSE)
  }
  # ensure sorted by USUBJID (SAS sort)
  dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID), , drop = FALSE]
}
stopifnot(all(c("USUBJID", "RFSTDTC") %in% names(dm_for_tu)))
cat(paste0("dm_for_tu rows=", nrow(dm_for_tu), "\n"))

# 2. Prepare Raw Data : tu_prep
# resolve edc_tu columns case-insensitively
cn_SUBJID <- get_colname(edc_tu, "SUBJID")
cn_LOC <- get_colname(edc_tu, "LOC")
cn_METH <- get_colname(edc_tu, "METH")
cn_LNKID <- get_colname(edc_tu, "LNKID")
cn_TYPE <- get_colname(edc_tu, "TYPE")
cn_VISIT <- get_colname(edc_tu, "VISIT")
cn_TUDT <- get_colname(edc_tu, "TUDT")
stopifnot(!is.na(cn_SUBJID))
tu_prep <- edc_tu
SUBJID_char <- to_subjid_char(edc_tu[[cn_SUBJID]])
tu_prep$USUBJID <- trunc_str(sas_catx("-", rep("ONCO-2026", nrow(tu_prep)), SUBJID_char), 25)
loc_raw <- if (!is.na(cn_LOC)) edc_tu[[cn_LOC]] else rep(NA, nrow(tu_prep))
meth_raw <- if (!is.na(cn_METH)) edc_tu[[cn_METH]] else rep(NA, nrow(tu_prep))
tu_prep$TULOC <- sapply(loc_raw, function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "") return(NA_character_)
  trunc_str(toupper(gsub("_", " ", s, fixed = TRUE)), 20)
}, USE.NAMES = FALSE)
tu_prep$TUMETHOD <- sapply(meth_raw, function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "") return(NA_character_)
  trunc_str(toupper(gsub("_", " ", s, fixed = TRUE)), 20)
}, USE.NAMES = FALSE)
# sort by USUBJID (SAS)
tu_prep <- tu_prep[order(tu_prep$USUBJID), , drop = FALSE]

# 3. Map TU Domain : merge tu_prep(in=t) dm_for_tu by USUBJID, if t
# drop/rename overlapping cols except keys before merging (avoid .x/.y)
overlap <- intersect(names(tu_prep), names(dm_for_tu))
overlap <- setdiff(overlap, "USUBJID")
if (length(overlap) > 0) {
  # dm_for_tu only has USUBJID RFSTDTC; if tu_prep has RFSTDTC drop from tu_prep
  tu_prep <- tu_prep[, setdiff(names(tu_prep), overlap), drop = FALSE]
}
sdtm_tu1 <- merge(tu_prep, dm_for_tu, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot(all(c("USUBJID", "RFSTDTC") %in% names(sdtm_tu1)))

n1 <- nrow(sdtm_tu1)
sdtm_tu1$STUDYID <- trunc_str(rep("ONCO-2026", n1), 10)
sdtm_tu1$DOMAIN <- trunc_str(rep("TU", n1), 2)
lnk_raw <- if (!is.na(cn_LNKID)) sdtm_tu1[[cn_LNKID]] else rep(NA, n1)
sdtm_tu1$TULNKID <- sapply(lnk_raw, function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA") return(NA_character_)
  # emulate SAS numeric->char BEST stripped: already as.character
  trunc_str(s, 5)
}, USE.NAMES = FALSE)
sdtm_tu1$TUTESTCD <- trunc_str(rep("TUMIDENT", n1), 8)
sdtm_tu1$TUTEST <- trunc_str(rep("Tumor Identification", n1), 40)
type_raw <- if (!is.na(cn_TYPE)) sdtm_tu1[[cn_TYPE]] else rep(NA, n1)
sdtm_tu1$TUORRES <- sapply(type_raw, function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "") return(NA_character_)
  trunc_str(toupper(s), 15)
}, USE.NAMES = FALSE)
sdtm_tu1$TUSTRESC <- sdtm_tu1$TUORRES
sdtm_tu1$TUEVAL <- trunc_str(rep("INVESTIGATOR", n1), 15)
visit_raw <- if (!is.na(cn_VISIT)) sdtm_tu1[[cn_VISIT]] else rep(NA, n1)
sdtm_tu1$VISIT <- sapply(visit_raw, function(x) {
  if (is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (s == "") return(NA_character_)
  trunc_str(toupper(gsub("_", " ", s, fixed = TRUE)), 20)
}, USE.NAMES = FALSE)
tudt_raw <- if (!is.na(cn_TUDT)) sdtm_tu1[[cn_TUDT]] else rep(NA, n1)
# TUDTC = put(input(TUDT, date11.), yymmdd10.) ; single strict DATE11. format
sdtm_tu1$TUDTC <- parse_date11_to_iso(tudt_raw)
sdtm_tu1$TUDTC <- trunc_str(sdtm_tu1$TUDTC, 19)
# Study Day : input(TUDTC,yymmdd10.) and input(RFSTDTC,yymmdd10.)
sdtm_tu1$TUSTDY <- sapply(seq_len(n1), function(i) {
  a <- sdtm_tu1$TUDTC[i]; b <- sdtm_tu1$RFSTDTC[i]
  if (is.na(a) || is.na(b)) return(NA_real_)
  a <- trimws(as.character(a)); b <- trimws(as.character(b))
  if (a == "" || b == "") return(NA_real_)
  stdt <- suppressWarnings(as.Date(a, format = "%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(b, format = "%Y-%m-%d"))
  if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
  diff <- as.integer(stdt - rfdt)
  if (diff >= 0) diff + 1 else diff
}, USE.NAMES = FALSE)

# 4. Derive TUSEQ : sort by USUBJID TUDTC (missing first), then by USUBJID retain
# SAS missing sorts first: use "" for NA in ordering
ord_tudtc <- ifelse(is.na(sdtm_tu1$TUDTC), "", sdtm_tu1$TUDTC)
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, ord_tudtc), , drop = FALSE]
sdtm_tu1$TUSEQ <- NA_real_
prev_usub <- NA_character_
seqn <- 0
for (i in seq_len(nrow(sdtm_tu1))) {
  u <- sdtm_tu1$USUBJID[i]
  if (is.na(prev_usub) || is.na(u) || u != prev_usub) {
    seqn <- 1
  } else {
    seqn <- seqn + 1
  }
  sdtm_tu1$TUSEQ[i] <- seqn
  prev_usub <- u
}
sdtm_tu <- sdtm_tu1[, c("STUDYID", "DOMAIN", "USUBJID", "TUSEQ", "TULNKID", "TUTESTCD", "TUTEST", "TUORRES", "TUSTRESC", "TULOC", "TUMETHOD", "TUEVAL", "TUDTC", "TUSTDY", "VISIT")]
# enforce lengths (truncate already) and types
sdtm_tu$TUSEQ <- as.numeric(sdtm_tu$TUSEQ)
sdtm_tu$TUSTDY <- as.numeric(sdtm_tu$TUSTDY)

cat(paste0("sdtm_tu rows=", nrow(sdtm_tu), "\n"))

# 5. View Final Dataset -> listing text (PROC PRINT)
print_df <- cbind(Obs = seq_len(nrow(sdtm_tu)), sdtm_tu)
# text rendering
hdr <- paste(c("Obs", "STUDYID", "DOMAIN", "USUBJID", "TUSEQ", "TULNKID", "TUTESTCD", "TUTEST", "TUORRES", "TUSTRESC", "TULOC", "TUMETHOD", "TUEVAL", "TUDTC", "TUSTDY", "VISIT"), collapse = " | ")
lines <- c("PROC PRINT DATA=SDTM_TU;", "", hdr, paste(rep("-", nchar(hdr)), collapse = ""))
fmt_cell <- function(x) { if (is.na(x)) "" else as.character(x) }
for (i in seq_len(nrow(print_df))) {
  r <- print_df[i, ]
  vals <- sapply(r, fmt_cell, USE.NAMES = FALSE)
  lines <- c(lines, paste(vals, collapse = " | "))
}
lines <- c(lines, "", paste0("N = ", nrow(sdtm_tu)))
writeLines(lines, file.path(out, "P029.txt"))
# CSV of table body
write.csv(sdtm_tu, file.path(out, "P029.csv"), row.names = FALSE, na = "")
# 6. Export Final Dataset
write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")
# plotted data placeholder not needed (no figure); still ensure listing csv has ISO dates
cat("Done.\n")
```