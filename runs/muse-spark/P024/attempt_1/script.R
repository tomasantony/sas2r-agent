out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

cat("P024 DS mapping - start\n")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

## ---------- SAS helpers ----------
sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  # strip leading/trailing blanks, skip missing/blank (SAS CATX strips and ignores blanks? actually ignores missing)
  # SAS CATX ignores missing values
  n <- max(lengths(args))
  args <- lapply(args, function(a) {
    a <- as.character(a)
    if (length(a) == 1L) a <- rep(a, n)
    a
  })
  vapply(seq_len(n), function(i) {
    parts <- vapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(v)
      if (v == "") return(NA_character_)
      v
    }, character(1))
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, character(1))
}
sas_tranwrd <- function(s, from, to) {
  ifelse(is.na(s), NA_character_, gsub(from, to, s, fixed = TRUE))
}
sas_upcase <- function(s) {
  ifelse(is.na(s), NA_character_, toupper(s))
}
sas_substr_len <- function(s, len) {
  ifelse(is.na(s), NA_character_, substr(s, 1, len))
}
# Parse SAS DATE11. (DDMMMYYYY with optional dashes, case-insensitive, 2-digit year allowed)
sas_input_date11 <- function(x) {
  # x character vector; return Date vector
  vapply(x, function(v) {
    if (is.na(v)) return(as.numeric(NA))
    v <- trimws(as.character(v))
    if (v == "") return(as.numeric(NA))
    # remove dashes and slashes and spaces
    # SAS DATE. allows DD-MMM-YY(YY)
    vu <- toupper(v)
    vu <- gsub("[/ ]", "-", vu)
    # try formats: %d-%b-%Y, %d-%b-%y, %d%b%Y, %d%b%y
    vu2 <- gsub("-", "", vu, fixed = TRUE)
    # vu2 like 15JAN2026 or 15JAN26
    d <- suppressWarnings(as.Date(NA))
    # try with dashes
    for (fmt in c("%d-%b-%Y", "%d-%b-%y", "%d%b%Y", "%d%b%y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")) {
      dd <- suppressWarnings(as.Date(vu, format = fmt))
      if (!is.na(dd)) { d <- dd; break }
      # also try original v
      dd2 <- suppressWarnings(as.Date(v, format = fmt))
      if (!is.na(dd2)) { d <- dd2; break }
    }
    if (is.na(d)) {
      # try lubridate-ish via as.Date with tryFormats
      dd <- suppressWarnings(as.Date(vu, tryFormats = c("%d-%b-%Y","%d-%b-%y","%d%b%Y","%d%b%y","%Y-%m-%d")))
      if (!is.na(dd)) d <- dd
    }
    as.numeric(d)
  }, numeric(1)) |> suppressWarnings() |> as.Date(origin = "1970-01-01")
}
sas_input_yymmdd10 <- function(x) {
  suppressWarnings(as.Date(trimws(as.character(x)), format = "%Y-%m-%d"))
}
sas_format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}
parse_any_to_iso <- function(x) {
  # x can be Date, POSIX, numeric, character -> ISO YYYY-MM-DD or NA
  if (inherits(x, "Date")) return(sas_format_yymmdd10(x))
  if (inherits(x, "POSIXt")) return(sas_format_yymmdd10(as.Date(x)))
  if (is.numeric(x) && !is.character(x)) {
    # Could be Excel numeric date? SAS numeric date is days since 1960-01-01
    # readxl already converts to Date, so numeric here unlikely; treat as NA
    return(rep(NA_character_, length(x)))
  }
  xc <- as.character(x)
  res <- rep(NA_character_, length(xc))
  for (i in seq_along(xc)) {
    v <- xc[i]
    if (is.na(v) || trimws(v) == "") { res[i] <- NA_character_; next }
    v <- trimws(v)
    # if already ISO
    d <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
    if (!is.na(d)) { res[i] <- format(d, "%Y-%m-%d"); next }
    # try datetime prefix
    d2 <- suppressWarnings(as.Date(substr(v, 1, 10), format = "%Y-%m-%d"))
    if (!is.na(d2) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) { res[i] <- format(d2, "%Y-%m-%d"); next }
    d3 <- sas_input_date11(v)
    if (!is.na(d3)) { res[i] <- format(d3, "%Y-%m-%d"); next }
    # try m/d/y
    for (fmt in c("%m/%d/%Y","%m/%d/%y","%d/%m/%Y","%Y/%m/%d","%d.%m.%Y")) {
      dd <- suppressWarnings(as.Date(v, format = fmt))
      if (!is.na(dd)) { res[i] <- format(dd, "%Y-%m-%d"); break }
    }
  }
  res
}
find_col <- function(df, candidates) {
  nms <- names(df)
  unms <- toupper(trimws(nms))
  for (cand in candidates) {
    idx <- which(unms == toupper(cand))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  # contains search
  for (cand in candidates) {
    idx <- grep(cand, nms, ignore.case = TRUE)
    if (length(idx) > 0) return(nms[idx[1]])
  }
  NA_character_
}
sas_order_idx <- function(...) {
  # order with missing first, binary (C locale) ordering via radix
  order(..., na.last = FALSE, method = "radix")
}

## ---------- paths ----------
raw_faijal <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
rawsdtm    <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
datasets_root <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets"

edc_ds_path <- file.path(raw_faijal, "edc_ds.xlsx")
edc_dm_path <- file.path(raw_faijal, "edc_dm.xlsx")
edc_death_path <- file.path(raw_faijal, "edc_death.xlsx")
edc_ex_path <- file.path(raw_faijal, "edc_ex.xlsx")

## ---------- read edc_ds ----------
cat("Reading edc_ds\n")
edc_ds_raw <- as.data.frame(readxl::read_excel(edc_ds_path, .name_repair = "minimal"), stringsAsFactors = FALSE)
cat(paste0("edc_ds cols: ", paste(names(edc_ds_raw), collapse = ", "), "\n"))

subj_col_ds <- find_col(edc_ds_raw, c("SUBJID","SUBJ","PTNO","PATIENT"))
term_col <- find_col(edc_ds_raw, c("DS_TERM","DSTERM","TERM","DS_TERM_"))
# DECOD column does not exist per comment; program uses DS_TERM for DECOD
decod_src_col <- term_col
cat_col <- find_col(edc_ds_raw, c("DS_CAT","DSCAT","CAT","DSCATEGORY"))
dt_col <- find_col(edc_ds_raw, c("DS_DT","DSSTDTC","STDT","DS_DATE","DSDT","DSSTDAT"))

if (is.na(subj_col_ds)) subj_col_ds <- names(edc_ds_raw)[1]
if (is.na(term_col)) term_col <- names(edc_ds_raw)[2]
if (is.na(cat_col)) cat_col <- names(edc_ds_raw)[3]
if (is.na(dt_col)) dt_col <- names(edc_ds_raw)[4]

cat(paste0("Using cols SUBJID=", subj_col_ds, " TERM=", term_col, " CAT=", cat_col, " DT=", dt_col, "\n"))

## helper to convert raw cell to character for TERM/CAT/STDT
cell_to_char <- function(v) {
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) return(format(v, "%d-%b-%Y"))
  if (inherits(v, "POSIXt")) return(format(as.Date(v), "%d-%b-%Y"))
  as.character(v)
}

n_ds <- nrow(edc_ds_raw)
TERM_v <- vapply(seq_len(n_ds), function(i) cell_to_char(edc_ds_raw[i, term_col]), character(1))
CAT_v  <- vapply(seq_len(n_ds), function(i) cell_to_char(edc_ds_raw[i, cat_col]), character(1))
# STDT: if Date, format as DD-MON-YYYY (11 chars) to mimic SAS $11
STDT_v <- vapply(seq_len(n_ds), function(i) {
  v <- edc_ds_raw[i, dt_col][[1]]
  if (is.null(v) || length(v) == 0) return(NA_character_)
  if (is.na(v[1])) return(NA_character_)
  if (inherits(v, "Date")) return(toupper(format(v, "%d-%b-%Y")))
  if (inherits(v, "POSIXt")) return(toupper(format(as.Date(v), "%d-%b-%Y")))
  s <- trimws(as.character(v[1]))
  if (s == "" || toupper(s) %in% c("NA","NAN","NAT")) return(NA_character_)
  # truncate to 11 like SAS LENGTH $11 (but keep full for parsing? SAS would truncate)
  # SAS: STDT = DS_DT with LENGTH $11 truncates to 11 chars
  if (nchar(s) > 11) s <- substr(s, 1, 11)
  s
}, character(1))
DECOD_v <- TERM_v  # per program DECOD = DS_TERM

# truncate TERM/DECOD/CAT to $50
TERM_v <- sas_substr_len(TERM_v, 50)
DECOD_v <- sas_substr_len(DECOD_v, 50)
CAT_v <- sas_substr_len(CAT_v, 50)
# STDT already $11
STDT_v[STDT_v == ""] <- NA_character_

SUBJID_raw <- edc_ds_raw[[subj_col_ds]]
# format SUBJID as SAS CATX would: numeric -> BEST12. stripped
subj_char <- vapply(SUBJID_raw, function(v) {
  if (is.na(v)) return(NA_character_)
  if (is.numeric(v)) return(trimws(format(v, scientific = FALSE, trim = TRUE)))
  trimws(as.character(v))
}, character(1))

USUBJID_v <- paste0("ONCO-2026-", trimws(subj_char))
# SAS CATX strips blanks; if SUBJID missing, CATX would give "ONCO-2026"? mimic
USUBJID_v[is.na(subj_char) | trimws(subj_char) == ""] <- "ONCO-2026"

edc_ds_prep <- data.frame(
  USUBJID = USUBJID_v,
  TERM = TERM_v,
  DECOD = DECOD_v,
  CAT = CAT_v,
  STDT = STDT_v,
  stringsAsFactors = FALSE
)
# sort by USUBJID (SAS sort)
edc_ds_prep <- edc_ds_prep[sas_order_idx(edc_ds_prep$USUBJID), , drop = FALSE]

## ---------- dm_for_ds ----------
cat("Locating sdtm_dm\n")
dm_for_ds <- NULL

try_read_file <- function(f) {
  ext <- tolower(tools::file_ext(f))
  tryCatch({
    if (ext == "xlsx" || ext == "xls") as.data.frame(readxl::read_excel(f, .name_repair = "minimal"), stringsAsFactors = FALSE)
    else if (ext == "csv") as.data.frame(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE), stringsAsFactors = FALSE)
    else if (ext == "xpt") as.data.frame(haven::read_xpt(f), stringsAsFactors = FALSE)
    else if (ext == "sas7bdat") as.data.frame(haven::read_sas(f), stringsAsFactors = FALSE)
    else NULL
  }, error = function(e) NULL)
}

# search for oncology DM in datasets_root
candidates <- character(0)
if (dir.exists(datasets_root)) {
  allf <- list.files(datasets_root, recursive = TRUE, full.names = TRUE)
  # prefer dm files
  is_dm <- grepl("(?i)(^|/|\\\\)(sdtm_)?dm\\.(xlsx|csv|xpt|sas7bdat)$", allf, perl = TRUE)
  candidates <- c(candidates, allf[is_dm])
  # also dm_for_ds
  is_dm2 <- grepl("(?i)dm_for_ds\\.", allf)
  candidates <- c(candidates, allf[is_dm2])
}
# also check rawsdtm DM
if (dir.exists(rawsdtm)) {
  rf <- list.files(rawsdtm, recursive = TRUE, full.names = TRUE)
  candidates <- c(candidates, rf[grepl("(?i)dm\\.", rf)])
}

found_dm <- NULL
for (f in candidates) {
  df <- try_read_file(f)
  if (is.null(df) || nrow(df) == 0) next
  nms_u <- toupper(names(df))
  if (!("USUBJID" %in% nms_u)) next
  # check for ONCO-2026
  usub <- df[[which(nms_u == "USUBJID")[1]]]
  usub_c <- as.character(usub)
  if (any(grepl("ONCO-2026", usub_c, fixed = TRUE), na.rm = TRUE)) {
    cat(paste0("Using DM file: ", f, "\n"))
    found_dm <- df
    break
  }
}
if (!is.null(found_dm)) {
  nms <- names(found_dm)
  nms_u <- toupper(nms)
  getc <- function(want, alt = c()) {
    for (w in c(want, alt)) {
      idx <- which(nms_u == toupper(w))
      if (length(idx) > 0) return(as.character(found_dm[[idx[1]]]))
    }
    rep(NA_character_, nrow(found_dm))
  }
  # RFSTDTC may be Date object
  rf_raw <- found_dm[[which(nms_u == "RFSTDTC")[1]]]
  if (!is.null(rf_raw)) {
    if (inherits(rf_raw, "Date") || inherits(rf_raw, "POSIXt")) RFSTDTC_c <- sas_format_yymmdd10(as.Date(rf_raw))
    else RFSTDTC_c <- parse_any_to_iso(as.character(rf_raw))
  } else {
    RFSTDTC_c <- getc("RFSTDTC")
  }
  dth_raw_idx <- which(nms_u %in% c("DTHDTC"))
  if (length(dth_raw_idx) > 0) {
    dth_raw <- found_dm[[dth_raw_idx[1]]]
    if (inherits(dth_raw, "Date") || inherits(dth_raw, "POSIXt")) DTHDTC_c <- sas_format_yymmdd10(as.Date(dth_raw))
    else DTHDTC_c <- parse_any_to_iso(as.character(dth_raw))
  } else DTHDTC_c <- rep(NA_character_, nrow(found_dm))
  usub_c <- as.character(found_dm[[which(nms_u == "USUBJID")[1]]])
  subj_c <- if ("SUBJID" %in% nms_u) as.character(found_dm[[which(nms_u == "SUBJID")[1]]]) else rep(NA_character_, nrow(found_dm))
  dm_for_ds <- data.frame(USUBJID = trimws(usub_c), SUBJID = trimws(subj_c),
                          RFSTDTC = RFSTDTC_c, DTHDTC = DTHDTC_c, stringsAsFactors = FALSE)
  dm_for_ds$RFSTDTC[dm_for_ds$RFSTDTC == ""] <- NA_character_
  dm_for_ds$DTHDTC[dm_for_ds$DTHDTC == ""] <- NA_character_
} else {
  cat("No oncology DM found - deriving dm_for_ds from raw edc files\n")
  # read edc_dm, edc_death, edc_ex
  edc_dm_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_dm_path, .name_repair = "minimal"), stringsAsFactors = FALSE), error = function(e) NULL)
  edc_death_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_death_path, .name_repair = "minimal"), stringsAsFactors = FALSE), error = function(e) NULL)
  edc_ex_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_ex_path, .name_repair = "minimal"), stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(edc_dm_raw)) cat(paste0("edc_dm cols: ", paste(names(edc_dm_raw), collapse = ", "), "\n"))
  if (!is.null(edc_death_raw)) cat(paste0("edc_death cols: ", paste(names(edc_death_raw), collapse = ", "), "\n"))
  if (!is.null(edc_ex_raw)) cat(paste0("edc_ex cols: ", paste(names(edc_ex_raw), collapse = ", "), "\n"))

  # collect all SUBJIDs
  all_subj <- subj_char
  if (!is.null(edc_dm_raw)) {
    sc <- find_col(edc_dm_raw, c("SUBJID","SUBJ"))
    if (!is.na(sc)) {
      v <- edc_dm_raw[[sc]]
      vc <- vapply(v, function(x) { if (is.na(x)) NA_character_ else if (is.numeric(x)) trimws(format(x, scientific=FALSE, trim=TRUE)) else trimws(as.character(x)) }, character(1))
      all_subj <- unique(c(all_subj, vc))
    }
  }
  all_subj <- unique(all_subj[!is.na(all_subj) & trimws(all_subj) != ""])
  all_usub <- paste0("ONCO-2026-", trimws(all_subj))

  # RFSTDTC derivation
  rf_map <- setNames(rep(NA_character_, length(all_subj)), all_subj)
  # try edc_dm date cols
  if (!is.null(edc_dm_raw)) {
    sc <- find_col(edc_dm_raw, c("SUBJID","SUBJ"))
    # candidate RF columns in priority
    rf_candidates <- c("RFSTDTC","RFSTDT","TRTSDT","RANDDT","RAND_DT","ENROLLDT","ENROLDT","STARTDT","STDT","EXSTDTC","TRT_DT","FIRST_DOSE")
    rf_col <- NA_character_
    nms_dm <- names(edc_dm_raw)
    for (cand in rf_candidates) {
      idx <- which(toupper(nms_dm) == toupper(cand))
      if (length(idx) > 0) { rf_col <- nms_dm[idx[1]]; break }
    }
    if (is.na(rf_col)) {
      # any column containing RFST, RAND, ENROLL, TRTSD, START, DOSE, EX
      for (p in c("RFST","RAND","ENROLL","ENROL","TRTSD","START","DOSE","EXDT","EX_DT")) {
        idx <- grep(p, nms_dm, ignore.case = TRUE)
        # exclude death cols
        idx <- idx[!grepl("DTH|DEATH", nms_dm[idx], ignore.case = TRUE)]
        if (length(idx) > 0) { rf_col <- nms_dm[idx[1]]; break }
      }
    }
    # fallback: any date-like column
    if (is.na(rf_col)) {
      for (cn in nms_dm) {
        if (grepl("DT|DATE|DTC", cn, ignore.case = TRUE) && !grepl("DTH|DEATH|BIRTH|BRTH", cn, ignore.case = TRUE)) { rf_col <- cn; break }
      }
    }
    cat(paste0("edc_dm RF col: ", rf_col, "\n"))
    if (!is.na(rf_col) && !is.na(sc)) {
      for (i in seq_len(nrow(edc_dm_raw))) {
        s <- edc_dm_raw[i, sc][[1]]
        s_c <- if (is.na(s)) NA_character_ else if (is.numeric(s)) trimws(format(s, scientific=FALSE, trim=TRUE)) else trimws(as.character(s))
        if (is.na(s_c) || !(s_c %in% names(rf_map))) next
        rv <- edc_dm_raw[i, rf_col][[1]]
        iso <- NA_character_
        if (!is.na(rv)) {
          if (inherits(rv, "Date") || inherits(rv, "POSIXt")) iso <- sas_format_yymmdd10(as.Date(rv))
          else iso <- parse_any_to_iso(as.character(rv))
        }
        if (!is.na(iso) && iso != "") rf_map[[s_c]] <- iso
      }
    }
  }
  # supplement from edc_ex min date if still missing
  if (!is.null(edc_ex_raw)) {
    sc_ex <- find_col(edc_ex_raw, c("SUBJID","SUBJ"))
    dt_ex_col <- find_col(edc_ex_raw, c("EX_DT","EXSTDTC","EXSTDT","EXDATE","STDT","DATE","DT"))
    if (is.na(dt_ex_col)) {
      # pick first date-like
      for (cn in names(edc_ex_raw)) if (grepl("DT|DATE|DTC", cn, ignore.case=TRUE)) { dt_ex_col <- cn; break }
    }
    cat(paste0("edc_ex cols used: subj=", sc_ex, " dt=", dt_ex_col, "\n"))
    if (!is.na(sc_ex) && !is.na(dt_ex_col)) {
      for (i in seq_len(nrow(edc_ex_raw))) {
        s <- edc_ex_raw[i, sc_ex][[1]]
        s_c <- if (is.na(s)) NA_character_ else if (is.numeric(s)) trimws(format(s, scientific=FALSE, trim=TRUE)) else trimws(as.character(s))
        if (is.na(s_c) || !(s_c %in% names(rf_map))) next
        rv <- edc_ex_raw[i, dt_ex_col][[1]]
        iso <- NA_character_
        if (!is.na(rv)) {
          if (inherits(rv, "Date") || inherits(rv, "POSIXt")) iso <- sas_format_yymmdd10(as.Date(rv))
          else iso <- parse_any_to_iso(as.character(rv))
        }
        if (!is.na(iso) && iso != "") {
          cur <- rf_map[[s_c]]
          if (is.na(cur) || iso < cur) rf_map[[s_c]] <- iso
        }
      }
    }
  }
  # DTHDTC derivation from edc_death and/or edc_dm
  dth_map <- setNames(rep(NA_character_, length(all_subj)), all_subj)
  if (!is.null(edc_death_raw)) {
    sc_d <- find_col(edc_death_raw, c("SUBJID","SUBJ"))
    dt_d_col <- find_col(edc_death_raw, c("DTHDTC","DEATHDT","DEATH_DT","DTHDT","DT","DATE","DTC"))
    if (is.na(dt_d_col)) {
      for (cn in names(edc_death_raw)) if (grepl("DT|DATE|DTC", cn, ignore.case=TRUE)) { dt_d_col <- cn; break }
    }
    cat(paste0("edc_death cols used: subj=", sc_d, " dt=", dt_d_col, "\n"))
    if (!is.na(dt_d_col)) {
      if (is.na(sc_d)) {
        # death file may have one row per subject in same order? assume all subjects with death?
        # if no SUBJID, try to map by row? skip
      } else {
        for (i in seq_len(nrow(edc_death_raw))) {
          s <- edc_death_raw[i, sc_d][[1]]
          s_c <- if (is.na(s)) NA_character_ else if (is.numeric(s)) trimws(format(s, scientific=FALSE, trim=TRUE)) else trimws(as.character(s))
          if (is.na(s_c) || !(s_c %in% names(dth_map))) next
          rv <- edc_death_raw[i, dt_d_col][[1]]
          iso <- NA_character_
          if (!is.na(rv)) {
            if (inherits(rv, "Date") || inherits(rv, "POSIXt")) iso <- sas_format_yymmdd10(as.Date(rv))
            else iso <- parse_any_to_iso(as.character(rv))
          }
          if (!is.na(iso) && iso != "") dth_map[[s_c]] <- iso
        }
      }
    }
  }
  # also check edc_dm for DTH columns
  if (!is.null(edc_dm_raw)) {
    sc <- find_col(edc_dm_raw, c("SUBJID","SUBJ"))
    dth_col_dm <- NA_character_
    for (cn in names(edc_dm_raw)) if (grepl("DTH|DEATH", cn, ignore.case=TRUE)) { dth_col_dm <- cn; break }
    if (!is.na(dth_col_dm) && !is.na(sc)) {
      cat(paste0("edc_dm DTH col: ", dth_col_dm, "\n"))
      for (i in seq_len(nrow(edc_dm_raw))) {
        s <- edc_dm_raw[i, sc][[1]]
        s_c <- if (is.na(s)) NA_character_ else if (is.numeric(s)) trimws(format(s, scientific=FALSE, trim=TRUE)) else trimws(as.character(s))
        if (is.na(s_c) || !(s_c %in% names(dth_map))) next
        if (!is.na(dth_map[[s_c]])) next
        rv <- edc_dm_raw[i, dth_col_dm][[1]]
        # could be Y/N flag; only parse dates
        iso <- NA_character_
        if (!is.na(rv)) {
          if (inherits(rv, "Date") || inherits(rv, "POSIXt")) iso <- sas_format_yymmdd10(as.Date(rv))
          else {
            s2 <- trimws(as.character(rv))
            if (grepl("[0-9]{4}|[0-9]{2}.*[A-Z]{3}|[0-9]{2}/|[0-9]{4}-", s2)) iso <- parse_any_to_iso(s2)
            else iso <- NA_character_
          }
        }
        if (!is.na(iso) && iso != "") dth_map[[s_c]] <- iso
      }
    }
  }

  dm_for_ds <- data.frame(
    USUBJID = all_usub,
    SUBJID = all_subj,
    RFSTDTC = unname(rf_map[all_subj]),
    DTHDTC = unname(dth_map[all_subj]),
    stringsAsFactors = FALSE
  )
}
# sort dm_for_ds by USUBJID, keep only needed
dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]
# dedupe by USUBJID (keep first)
dm_for_ds <- dm_for_ds[!duplicated(dm_for_ds$USUBJID), , drop = FALSE]
cat(paste0("dm_for_ds n=", nrow(dm_for_ds), "\n"))

## ---------- 4. Create SDTM DS1 (merge) ----------
# left join edc_ds_prep with dm_for_ds
sdtm_ds1 <- merge(edc_ds_prep, dm_for_ds, by = "USUBJID", all.x = TRUE, sort = FALSE)
# SAS merge keeps SUBJID from dm? edc_ds_prep has no SUBJID col (only USUBJID); dm has SUBJID - keep dm SUBJID but not needed
# Identifier
sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"
# DS vars with lengths
sdtm_ds1$DSTERM <- sas_substr_len(sas_tranwrd(sdtm_ds1$TERM, "_", " "), 50)
sdtm_ds1$DSDECOD <- sas_substr_len(sas_upcase(sdtm_ds1$DECOD), 30)
sdtm_ds1$DSCAT <- sas_substr_len(sas_tranwrd(sdtm_ds1$CAT, "_", " "), 30)
# DTHFL
sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(as.character(sdtm_ds1$DTHDTC)) != "", "Y", NA_character_)
# DSSTDTC: if STDT ne "" then put(input(STDT,date11.),yymmdd10.)
sdtm_ds1$DSSTDTC <- rep(NA_character_, nrow(sdtm_ds1))
for (i in seq_len(nrow(sdtm_ds1))) {
  st <- sdtm_ds1$STDT[i]
  if (is.na(st) || trimws(st) == "") { sdtm_ds1$DSSTDTC[i] <- NA_character_; next }
  d <- sas_input_date11(st)
  # if DATE11 parse fails, try already ISO
  if (is.na(d)) {
    d2 <- suppressWarnings(as.Date(trimws(st), format = "%Y-%m-%d"))
    if (!is.na(d2)) d <- d2
  }
  if (is.na(d)) sdtm_ds1$DSSTDTC[i] <- NA_character_
  else sdtm_ds1$DSSTDTC[i] <- format(d, "%Y-%m-%d")
}
# DSSTDY
sdtm_ds1$DSSTDY <- rep(NA_real_, nrow(sdtm_ds1))
for (i in seq_len(nrow(sdtm_ds1))) {
  a <- sdtm_ds1$DSSTDTC[i]; b <- sdtm_ds1$RFSTDTC[i]
  if (is.na(a) || is.na(b) || trimws(a) == "" || trimws(b) == "") next
  stdt <- sas_input_yymmdd10(a)
  rfdt <- sas_input_yymmdd10(b)
  # RFSTDTC might be datetime or DATE11; fallback parse
  if (is.na(rfdt)) {
    rf_iso <- parse_any_to_iso(b)
    if (!is.na(rf_iso)) rfdt <- as.Date(rf_iso)
  }
  if (is.na(stdt) || is.na(rfdt)) next
  diff <- as.numeric(stdt - rfdt)
  if (stdt >= rfdt) sdtm_ds1$DSSTDY[i] <- diff + 1
  else sdtm_ds1$DSSTDY[i] <- diff
}
# consistency check subject 103
idx103 <- which(sdtm_ds1$USUBJID == "ONCO-2026-103")
for (i in idx103) {
  a <- sdtm_ds1$DSSTDTC[i]; b <- sdtm_ds1$DTHDTC[i]
  an <- ifelse(is.na(a), "", a); bn <- ifelse(is.na(b), "", b)
  if (an != bn) cat(paste0("WARNING: Date mismatch! DS=", an, " DM=", bn, "\n"))
}

## ---------- 5. Sort final DS1 by USUBJID DSSTDTC ----------
ord <- sas_order_idx(sdtm_ds1$USUBJID, sdtm_ds1$DSSTDTC)
sdtm_ds1 <- sdtm_ds1[ord, , drop = FALSE]

## ---------- 6. Derive DSSEQ ----------
sdtm_ds1$DSSEQ <- NA_real_
# by USUBJID
us <- sdtm_ds1$USUBJID
# handle NA USUBJID as own group
r <- rle(ifelse(is.na(us), "__NA__", us))
ends <- cumsum(r$lengths)
starts <- c(1, head(ends, -1) + 1)
for (k in seq_along(starts)) {
  idx <- seq(starts[k], ends[k])
  sdtm_ds1$DSSEQ[idx] <- seq_along(idx)
}

sdtm_ds <- data.frame(
  STUDYID = sdtm_ds1$STUDYID,
  DOMAIN = sdtm_ds1$DOMAIN,
  USUBJID = sdtm_ds1$USUBJID,
  DSSEQ = sdtm_ds1$DSSEQ,
  DSTERM = sdtm_ds1$DSTERM,
  DSDECOD = sdtm_ds1$DSDECOD,
  DSCAT = sdtm_ds1$DSCAT,
  DSSTDTC = sdtm_ds1$DSSTDTC,
  DSSTDY = sdtm_ds1$DSSTDY,
  DTHFL = sdtm_ds1$DTHFL,
  stringsAsFactors = FALSE
)

cat(paste0("sdtm_ds n=", nrow(sdtm_ds), "\n"))

## ---------- 7/8. Outputs ----------
# dataset ds.csv
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# listing: PROC PRINT equivalent
# Build display data frame with Obs
print_df <- cbind(Obs = seq_len(nrow(sdtm_ds)), sdtm_ds)
# text rendering
txt_path <- file.path(out, "P024.txt")
lst_path <- file.path(out, "P024.lst")
csv_path <- file.path(out, "P024.csv")

# format for txt: fixed-width columns
fmt_cell <- function(v) { if (is.na(v) || v == "") "" else as.character(v) }
header <- c("Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")
# compute widths
widths <- sapply(header, nchar)
for (j in header) {
  vals <- if (j == "Obs") as.character(print_df$Obs) else as.character(print_df[[j]])
  vals[is.na(vals)] <- ""
  widths[j] <- max(widths[j], max(nchar(vals), na.rm = TRUE))
}
widths <- pmin(widths + 2, 30)
# for DSTERM/DSDECOD/DSCAT allow wider
line_sep <- paste0(rep("-", sum(widths) + length(widths) - 1), collapse = "")

con <- file(txt_path, open = "wt")
writeLines("Listing of SDTM DS (P024)", con)
writeLines(paste0("Study: ONCO-2026  Domain=DS  N=", nrow(sdtm_ds)), con)
writeLines(line_sep, con)
# header line
hdr_line <- paste(mapply(function(h, w) sprintf(paste0("%-", w, "s"), h), header, widths), collapse = " ")
writeLines(trimws(hdr_line, which = "right"), con)
writeLines(line_sep, con)
for (i in seq_len(nrow(print_df))) {
  row_vals <- sapply(header, function(h) {
    v <- print_df[i, h]
    if (is.na(v)) "" else as.character(v)
  })
  line <- paste(mapply(function(v, w) sprintf(paste0("%-", w, "s"), v), row_vals, widths), collapse = " ")
  writeLines(trimws(line, which = "right"), con)
}
writeLines(line_sep, con)
close(con)
# copy to .lst
file.copy(txt_path, lst_path, overwrite = TRUE)
# CSV of table body
write.csv(print_df, csv_path, row.names = FALSE, na = "")

cat("Done\n")
