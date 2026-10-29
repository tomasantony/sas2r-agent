```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

cat("P028 TR mapping started\n")

sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}

sas_catx <- function(delim, ...) {
  args <- list(...)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(v, which = "both")
      if (v == "") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}

sas_tranwrd <- function(source, from, to) {
  sapply(source, function(s) {
    if (is.na(s)) return(NA_character_)
    stringr::str_replace_all(s, stringr::fixed(from), to)
  }, USE.NAMES = FALSE)
}

sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}

sas_substr <- function(x, len) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- as.character(v)
    substr(v, 1, len)
  }, USE.NAMES = FALSE)
}

parse_date11_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(as.Date(NA))
    vc <- trimws(as.character(v), which = "both")
    if (vc == "" || toupper(vc) == "NA") return(as.Date(NA))
    clean <- gsub("[-/ ]", "", vc)
    d <- suppressWarnings(as.Date(clean, format = "%d%b%Y"))
    if (!is.na(d)) return(d)
    d2 <- suppressWarnings(as.Date(toupper(clean), format = "%d%b%Y"))
    if (!is.na(d2)) return(d2)
    d3 <- suppressWarnings(as.Date(vc, format = "%d-%b-%Y"))
    if (!is.na(d3)) return(d3)
    d4 <- suppressWarnings(as.Date(toupper(vc), format = "%d-%b-%Y"))
    if (!is.na(d4)) return(d4)
    d5 <- suppressWarnings(lubridate::dmy(vc, quiet = TRUE))
    if (!is.na(d5)) return(as.Date(d5))
    return(as.Date(NA))
  }, USE.NAMES = FALSE) |> as.Date(origin = "1970-01-01")
}

format_yymmdd10 <- function(d) {
  sapply(d, function(v) {
    if (is.na(v)) return(NA_character_)
    if (inherits(v, "character")) {
      dd <- suppressWarnings(as.Date(substr(v, 1, 10), format = "%Y-%m-%d"))
      if (is.na(dd)) return(NA_character_)
      return(format(dd, "%Y-%m-%d"))
    }
    format(as.Date(v, origin = "1970-01-01"), "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}

# flexible ISO / any date parser returning Date
parse_any_date <- function(x) {
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.null(v) || length(v) == 0) return(as.Date(NA))
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    if (is.numeric(v) && !is.na(v)) {
      # could be SAS date (days since 1960) or Excel date; try both
      # haven already converts SAS dates, so numeric here likely Excel or SAS raw
      # Try SAS origin first if plausible range, else Excel
      # We try ISO parse of character first; if v is numeric like 20240115 skip
      return(as.Date(NA))
    }
    if (is.na(v)) return(as.Date(NA))
    vc <- trimws(as.character(v), which = "both")
    if (vc == "" || toupper(vc) == "NA" || vc == ".") return(as.Date(NA))
    # try YYYY-MM-DD (first 10 chars)
    d <- suppressWarnings(as.Date(substr(vc, 1, 10), format = "%Y-%m-%d"))
    if (!is.na(d)) return(d)
    # try YYYY/MM/DD
    d <- suppressWarnings(as.Date(substr(vc, 1, 10), format = "%Y/%m/%d"))
    if (!is.na(d)) return(d)
    # try datetime YYYY-MM-DDTHH:MM:SS
    d <- suppressWarnings(as.Date(substr(vc, 1, 10)))
    if (!is.na(d)) return(d)
    # try date11
    clean <- gsub("[-/ ]", "", vc)
    d <- suppressWarnings(as.Date(clean, format = "%d%b%Y"))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(toupper(clean), format = "%d%b%Y"))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(lubridate::dmy(vc, quiet = TRUE))
    if (!is.na(d)) return(as.Date(d))
    d <- suppressWarnings(lubridate::ymd(vc, quiet = TRUE))
    if (!is.na(d)) return(as.Date(d))
    return(as.Date(NA))
  }, USE.NAMES = FALSE) |> as.Date(origin = "1970-01-01")
}

read_edc <- function(name) {
  p <- file.path(edc_dir, paste0(name, ".xlsx"))
  if (!file.exists(p)) {
    cat(paste0("Missing file: ", p, "\n"))
    return(tibble::tibble())
  }
  df <- readxl::read_excel(p, .name_repair = "minimal")
  names(df) <- toupper(trimws(names(df)))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  tibble::as_tibble(df)
}

get_col <- function(df, col) {
  if (nrow(df) == 0) return(rep(NA_character_, 0))
  idx <- which(toupper(names(df)) == toupper(col))
  if (length(idx) == 0) return(rep(NA_character_, nrow(df)))
  v <- df[[idx[1]]]
  if (inherits(v, "Date")) return(as.character(format(v, "%d%b%Y")))
  if (inherits(v, "POSIXt")) return(as.character(format(as.Date(v), "%d%b%Y")))
  as.character(v)
}
get_col_raw <- function(df, col) {
  if (nrow(df) == 0) return(vector())
  idx <- which(toupper(names(df)) == toupper(col))
  if (length(idx) == 0) return(rep(NA, nrow(df)))
  df[[idx[1]]]
}

norm_subj_part <- function(usub) {
  sapply(usub, function(v) {
    if (is.na(v)) return(NA_character_)
    vc <- trimws(as.character(v))
    if (vc == "") return("")
    # part after last hyphen
    pos <- max(gregexpr("-", vc, fixed = TRUE)[[1]])
    part <- if (pos > 0) substr(vc, pos + 1, nchar(vc)) else vc
    part <- trimws(part)
    # remove leading zeros for numeric comparison, keep case-insensitive
    stripped <- sub("^0+", "", part)
    if (stripped == "") stripped <- "0"
    toupper(stripped)
  }, USE.NAMES = FALSE)
}

edc_tr <- read_edc("edc_tr")
cat(paste0("edc_tr rows: ", nrow(edc_tr), " cols: ", paste(names(edc_tr), collapse=","), "\n"))

n_tr <- nrow(edc_tr)
subj_raw <- if (n_tr > 0) get_col(edc_tr, "SUBJID") else character(0)
usubjid_prep <- if (n_tr > 0) sas_catx("-", rep("ONCO-2026", n_tr), ifelse(is.na(subj_raw), "", subj_raw)) else character(0)
usubjid_prep <- sas_substr(usubjid_prep, 25)
lnk_raw   <- if (n_tr > 0) get_col(edc_tr, "LNKID") else character(0)
testcd_raw<- if (n_tr > 0) get_col(edc_tr, "TESTCD") else character(0)
orres_raw <- if (n_tr > 0) get_col(edc_tr, "ORRES") else character(0)
unit_raw  <- if (n_tr > 0) get_col(edc_tr, "UNIT") else character(0)
visit_raw <- if (n_tr > 0) get_col(edc_tr, "VISIT") else character(0)
trdt_raw_vec <- if (n_tr > 0) get_col_raw(edc_tr, "TRDT") else vector()

tr_prep <- tibble::tibble(
  USUBJID = usubjid_prep,
  LNKID = as.character(lnk_raw),
  TESTCD = as.character(testcd_raw),
  ORRES = as.character(orres_raw),
  UNIT = as.character(unit_raw),
  VISIT_RAW = as.character(visit_raw)
)
tr_prep$TRDT_RAW <- trdt_raw_vec
if (nrow(tr_prep) > 0) {
  tr_prep$USUBJID[is.na(tr_prep$USUBJID)] <- ""
  ord <- order(tr_prep$USUBJID, na.last = FALSE, method = "radix")
  tr_prep <- tr_prep[ord, , drop = FALSE]
}

# ---- DM reference ----
dm_for_tr <- NULL
search_dirs <- c(file.path(root, "output/sas/datasets"),
                 file.path(root, "output/sas"),
                 "D:/meta_hackathon/sas2r-agent/output/sas/datasets")
found_dm <- NULL
for (sd in search_dirs) {
  if (dir.exists(sd)) {
    cand <- list.files(sd, pattern = "(?i)(^|/|\\\\)dm\\.(csv|xlsx|xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE)
    if (length(cand) > 0) {
      found_dm <- cand[1]
      break
    }
  }
}
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
if (is.null(found_dm) && dir.exists(rawsdtm_dir)) {
  cand2 <- list.files(rawsdtm_dir, pattern = "(?i)^dm\\.", full.names = TRUE)
  if (length(cand2) > 0) found_dm <- cand2[1]
}

if (!is.null(found_dm) && file.exists(found_dm)) {
  cat(paste0("Found DM file: ", found_dm, "\n"))
  ext <- tolower(tools::file_ext(found_dm))
  dm_raw <- NULL
  try({
    if (ext == "csv") dm_raw <- readr::read_csv(found_dm, show_col_types = FALSE, name_repair = "minimal")
    else if (ext == "xlsx") dm_raw <- readxl::read_excel(found_dm, .name_repair = "minimal")
    else if (ext == "xpt") dm_raw <- haven::read_xpt(found_dm)
    else if (ext == "sas7bdat") dm_raw <- haven::read_sas(found_dm)
  }, silent = TRUE)
  if (!is.null(dm_raw)) {
    names(dm_raw) <- toupper(trimws(names(dm_raw)))
    # zap labels for robust conversion
    dm_raw_nl <- tryCatch(haven::zap_labels(dm_raw), error = function(e) dm_raw)
    # also zap formats
    dm_raw_nl <- tryCatch(haven::zap_formats(dm_raw_nl), error = function(e) dm_raw_nl)
    df2 <- as.data.frame(dm_raw_nl, stringsAsFactors = FALSE)
    names(df2) <- toupper(trimws(names(df2)))
    ucol <- which(names(df2) == "USUBJID")
    rcol <- which(names(df2) == "RFSTDTC")
    # fallback: RFSTDT if RFSTDTC missing
    use_rcol <- rcol
    if (length(ucol) > 0 && length(use_rcol) == 0) {
      alt <- which(names(df2) == "RFSTDT")
      if (length(alt) > 0) use_rcol <- alt
    }
    if (length(ucol) > 0 && length(use_rcol) > 0) {
      cu <- df2[[ucol[1]]]
      cr <- df2[[use_rcol[1]]]
      # robust conversion
      if (inherits(cu, "Date")) uu <- format(cu, "%Y-%m-%d")
      else if (inherits(cu, "POSIXt") || inherits(cu, "POSIXct")) uu <- format(as.Date(cu), "%Y-%m-%d")
      else {
        # numeric SAS date?
        if (is.numeric(cu) && !is.character(cu)) uu <- as.character(cu)
        else uu <- as.character(cu)
      }
      if (inherits(cr, "Date")) rr <- format(cr, "%Y-%m-%d")
      else if (inherits(cr, "POSIXt") || inherits(cr, "POSIXct")) rr <- format(as.Date(cr), "%Y-%m-%d")
      else if (is.numeric(cr) && !is.character(cr)) {
        # assume SAS date numeric (days since 1960-01-01)
        # haven should have converted, but handle raw numeric
        rr_try <- suppressWarnings(as.Date(cr, origin = "1960-01-01"))
        rr <- format(rr_try, "%Y-%m-%d")
        rr[is.na(rr_try)] <- ""
      } else {
        rr <- as.character(cr)
        # if datetime with time, keep full then later substr; ensure Date objects formatted
      }
      uu[is.na(uu)] <- ""
      rr[is.na(rr)] <- ""
      uu <- trimws(uu); rr <- trimws(rr)
      # if rr looks like date11 (contains letters), convert to yymmdd10
      need_conv <- grepl("[A-Za-z]", rr)
      if (any(need_conv, na.rm = TRUE)) {
        conv <- format_yymmdd10(parse_date11_vec(rr))
        idxc <- which(need_conv & !is.na(conv) & conv != "")
        if (length(idxc) > 0) rr[idxc] <- conv[idxc]
      }
      # if rr is datetime with T, keep as is (substr later handles)
      dm_for_tr <- tibble::tibble(USUBJID = sas_substr(trimws(uu), 25),
                                  RFSTDTC = trimws(rr))
      dm_for_tr$RFSTDTC[is.na(dm_for_tr$RFSTDTC)] <- ""
      dm_for_tr$USUBJID[is.na(dm_for_tr$USUBJID)] <- ""
    }
  }
}

if (is.null(dm_for_tr)) {
  cat("DM file not found or unusable; deriving dm_for_tr from edc_dm\n")
  edc_dm <- read_edc("edc_dm")
  cat(paste0("edc_dm rows: ", nrow(edc_dm), " cols: ", paste(names(edc_dm), collapse=","), "\n"))
  if (nrow(edc_dm) > 0) {
    dm_subj <- get_col(edc_dm, "SUBJID")
    dm_usub <- sas_catx("-", rep("ONCO-2026", length(dm_subj)), ifelse(is.na(dm_subj), "", dm_subj))
    dm_usub <- sas_substr(dm_usub, 25)
    rfcol_idx <- which(toupper(names(edc_dm)) == "RFSTDTC")
    rf_vals <- NULL
    if (length(rfcol_idx) > 0) {
      rv <- edc_dm[[rfcol_idx[1]]]
      if (inherits(rv, "Date")) rf_vals <- format(rv, "%Y-%m-%d")
      else if (inherits(rv, "POSIXt")) rf_vals <- format(as.Date(rv), "%Y-%m-%d")
      else {
        rf_vals <- trimws(as.character(rv))
        need_conv <- grepl("[A-Za-z]", rf_vals)
        if (any(need_conv, na.rm = TRUE)) {
          conv <- format_yymmdd10(parse_date11_vec(rf_vals))
          rf_vals[need_conv] <- ifelse(is.na(conv[need_conv]), rf_vals[need_conv], conv[need_conv])
        }
      }
    } else {
      alt_names <- c("TRTSDT", "TRTSTDTC", "RFSTDT", "STDTC", "EXSTDTC", "STARTDT", "STDT")
      found <- FALSE
      for (an in alt_names) {
        ai <- which(toupper(names(edc_dm)) == an)
        if (length(ai) > 0) {
          rv <- edc_dm[[ai[1]]]
          if (inherits(rv, "Date")) rf_vals <- format(rv, "%Y-%m-%d")
          else if (inherits(rv, "POSIXt")) rf_vals <- format(as.Date(rv), "%Y-%m-%d")
          else {
            rf_vals <- trimws(as.character(rv))
            need_conv <- grepl("[A-Za-z]", rf_vals)
            if (any(need_conv, na.rm = TRUE)) {
              conv <- format_yymmdd10(parse_date11_vec(rf_vals))
              rf_vals[need_conv] <- ifelse(is.na(conv[need_conv]), rf_vals[need_conv], conv[need_conv])
            }
          }
          found <- TRUE
          break
        }
      }
      if (!found) rf_vals <- rep("", length(dm_usub))
    }
    rf_vals[is.na(rf_vals)] <- ""
    dm_for_tr <- tibble::tibble(USUBJID = dm_usub, RFSTDTC = rf_vals)
  } else {
    dm_for_tr <- tibble::tibble(USUBJID = character(0), RFSTDTC = character(0))
  }
}

if (nrow(dm_for_tr) > 0) {
  dm_for_tr$USUBJID[is.na(dm_for_tr$USUBJID)] <- ""
  dm_for_tr$RFSTDTC[is.na(dm_for_tr$RFSTDTC)] <- ""
  dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]
  dm_for_tr <- dm_for_tr[!duplicated(dm_for_tr$USUBJID), , drop = FALSE]
}
cat(paste0("dm_for_tr rows: ", nrow(dm_for_tr), "\n"))

# ---- Map TR Domain ----
if (nrow(tr_prep) == 0) {
  sdtm_tr1_base <- tr_prep
  sdtm_tr1_base$RFSTDTC <- character(0)
} else {
  sdtm_tr1_base <- dplyr::left_join(tr_prep, dm_for_tr, by = "USUBJID")
  sdtm_tr1_base$RFSTDTC[is.na(sdtm_tr1_base$RFSTDTC)] <- ""
  # Fallback normalized join for rows with missing RFSTDTC (handles leading-zero differences)
  miss_idx <- which(sdtm_tr1_base$RFSTDTC == "")
  if (length(miss_idx) > 0 && nrow(dm_for_tr) > 0) {
    dm_norm <- norm_subj_part(dm_for_tr$USUBJID)
    tr_norm <- norm_subj_part(sdtm_tr1_base$USUBJID)
    for (ii in miss_idx) {
      tn <- tr_norm[ii]
      if (is.na(tn) || tn == "") next
      hit <- which(!is.na(dm_norm) & dm_norm == tn)
      if (length(hit) > 0) {
        # prefer exact prefix match if multiple? take first with non-missing RFSTDTC
        cands <- dm_for_tr$RFSTDTC[hit]
        cands <- cands[!is.na(cands) & trimws(cands) != ""]
        if (length(cands) > 0) sdtm_tr1_base$RFSTDTC[ii] <- cands[1]
      }
    }
    # Second fallback: derive RFSTDTC from edc_dm by normalized SUBJID if still missing
    still_miss <- which(sdtm_tr1_base$RFSTDTC == "")
    if (length(still_miss) > 0) {
      edc_dm2 <- tryCatch(read_edc("edc_dm"), error = function(e) tibble::tibble())
      if (nrow(edc_dm2) > 0) {
        dm2_subj <- get_col(edc_dm2, "SUBJID")
        dm2_norm <- sapply(dm2_subj, function(v) {
          if (is.na(v)) return(NA_character_)
          vc <- trimws(as.character(v))
          st <- sub("^0+", "", vc); if (st == "") st <- "0"; toupper(st)
        }, USE.NAMES = FALSE)
        # find a date column for RFSTDTC
        rf_candidates <- c("RFSTDTC","TRTSDT","TRTSTDTC","RFSTDT","STDTC","EXSTDTC","STARTDT","STDT","BRTHDTC")
        rf_vals2 <- rep("", length(dm2_subj))
        for (cn in rf_candidates) {
          ci <- which(toupper(names(edc_dm2)) == cn)
          if (length(ci) > 0) {
            rv <- edc_dm2[[ci[1]]]
            if (inherits(rv, "Date")) rf_vals2 <- format(rv, "%Y-%m-%d")
            else if (inherits(rv, "POSIXt")) rf_vals2 <- format(as.Date(rv), "%Y-%m-%d")
            else {
              rf_vals2 <- trimws(as.character(rv))
              nc <- grepl("[A-Za-z]", rf_vals2)
              if (any(nc, na.rm = TRUE)) {
                conv <- format_yymmdd10(parse_date11_vec(rf_vals2))
                rf_vals2[nc] <- ifelse(is.na(conv[nc]), rf_vals2[nc], conv[nc])
              }
            }
            break
          }
        }
        for (ii in still_miss) {
          tn <- tr_norm[ii]
          hit2 <- which(!is.na(dm2_norm) & dm2_norm == tn)
          if (length(hit2) > 0) {
            c2 <- rf_vals2[hit2]
            c2 <- c2[!is.na(c2) & trimws(c2) != ""]
            if (length(c2) > 0) sdtm_tr1_base$RFSTDTC[ii] <- c2[1]
          }
        }
      }
    }
  }
}

if (nrow(tr_prep) == 0) {
  sdtm_tr1 <- tibble::tibble()
} else {
  n1 <- nrow(sdtm_tr1_base)
  STUDYID <- rep("ONCO-2026", n1)
  DOMAIN <- rep("TR", n1)
  TRLNKID <- sas_substr(sas_trim(sdtm_tr1_base$LNKID), 5)
  TRTESTCD <- sas_substr(sas_trim(sdtm_tr1_base$TESTCD), 8)
  TRTEST <- ifelse(!is.na(TRTESTCD) & TRTESTCD == "LDIAM", "Longest Diameter", NA_character_)
  TRTEST <- sas_substr(TRTEST, 40)
  TRORRES <- sas_substr(sas_trim(sdtm_tr1_base$ORRES), 10)
  TRORRESU <- sas_substr(sas_trim(sdtm_tr1_base$UNIT), 10)
  TRSTRESC <- sas_substr(sas_trim(sdtm_tr1_base$ORRES), 10)
  TRSTRESN <- suppressWarnings(as.numeric(trimws(as.character(sdtm_tr1_base$ORRES))))
  TRSTRESU <- sas_substr(sas_trim(sdtm_tr1_base$UNIT), 10)
  TREVAL <- sas_substr(rep("INVESTIGATOR", n1), 15)
  vis_tmp <- sas_tranwrd(sdtm_tr1_base$VISIT_RAW, "_", " ")
  vis_tmp <- sas_upcase(vis_tmp)
  VISIT <- sas_substr(vis_tmp, 20)
  VISIT[VISIT == ""] <- NA_character_
  TRDTC <- rep(NA_character_, n1)
  trdt_raw <- sdtm_tr1_base$TRDT_RAW
  for (i in seq_len(n1)) {
    v <- trdt_raw[[i]]
    if (is.null(v) || length(v) == 0) { TRDTC[i] <- NA_character_; next }
    if (length(v) > 1) v <- v[1]
    if (is.na(v)) { TRDTC[i] <- NA_character_; next }
    if (inherits(v, "Date")) {
      TRDTC[i] <- format(v, "%Y-%m-%d")
    } else if (inherits(v, "POSIXt")) {
      TRDTC[i] <- format(as.Date(v), "%Y-%m-%d")
    } else if (is.numeric(v) && !is.character(v)) {
      # Excel numeric date
      dd <- suppressWarnings(as.Date(v, origin = "1899-12-30"))
      if (!is.na(dd)) TRDTC[i] <- format(dd, "%Y-%m-%d")
      else TRDTC[i] <- NA_character_
    } else {
      vc <- trimws(as.character(v))
      if (vc == "" || toupper(vc) == "NA") {
        TRDTC[i] <- NA_character_
      } else {
        if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", vc)) {
          dd <- suppressWarnings(as.Date(substr(vc, 1, 10), format = "%Y-%m-%d"))
          if (!is.na(dd)) TRDTC[i] <- format(dd, "%Y-%m-%d")
          else TRDTC[i] <- NA_character_
        } else {
          dd <- parse_any_date(vc)
          if (!is.na(dd)) TRDTC[i] <- format(dd, "%Y-%m-%d")
          else TRDTC[i] <- NA_character_
        }
      }
    }
  }
  TRDTC <- sas_substr(TRDTC, 19)
  # Study Day with robust parsing (SAS: input(yymmdd10.) then diff)
  TRSTDY <- rep(NA_real_, n1)
  rf_vec <- sdtm_tr1_base$RFSTDTC
  stdt_all <- parse_any_date(TRDTC)
  rf_all <- parse_any_date(rf_vec)
  for (i in seq_len(n1)) {
    s <- stdt_all[i]; r <- rf_all[i]
    if (is.na(s) || is.na(r)) next
    diff <- as.integer(s - r)
    if (s >= r) TRSTDY[i] <- diff + 1
    else TRSTDY[i] <- diff
  }
  sdtm_tr1 <- tibble::tibble(
    STUDYID = sas_substr(STUDYID, 10),
    DOMAIN = sas_substr(DOMAIN, 2),
    USUBJID = sas_substr(sdtm_tr1_base$USUBJID, 25),
    TRLNKID = TRLNKID,
    TRTESTCD = TRTESTCD,
    TRTEST = TRTEST,
    TRORRES = TRORRES,
    TRORRESU = TRORRESU,
    TRSTRESC = TRSTRESC,
    TRSTRESN = TRSTRESN,
    TRSTRESU = TRSTRESU,
    TREVAL = TREVAL,
    TRDTC = TRDTC,
    TRSTDY = TRSTDY,
    VISIT = VISIT
  )
  sdtm_tr1$TRTEST[sdtm_tr1$TRTEST == ""] <- NA_character_
}

if (exists("sdtm_tr1") && nrow(sdtm_tr1) > 0) {
  sdtm_tr1$._u <- ifelse(is.na(sdtm_tr1$USUBJID), "", sdtm_tr1$USUBJID)
  sdtm_tr1$._t <- ifelse(is.na(sdtm_tr1$TRDTC), "", sdtm_tr1$TRDTC)
  o <- order(sdtm_tr1$._u, sdtm_tr1$._t, na.last = FALSE, method = "radix")
  sdtm_tr1 <- sdtm_tr1[o, , drop = FALSE]
  sdtm_tr1$._u <- NULL
  sdtm_tr1$._t <- NULL
  grp <- ifelse(is.na(sdtm_tr1$USUBJID), "<NA>", sdtm_tr1$USUBJID)
  sdtm_tr1$TRSEQ <- ave(seq_len(nrow(sdtm_tr1)), grp, FUN = seq_along)
  sdtm_tr1$TRSEQ <- as.numeric(sdtm_tr1$TRSEQ)
  sdtm_tr <- sdtm_tr1[, c("STUDYID","DOMAIN","USUBJID","TRSEQ","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT")]
} else {
  sdtm_tr <- tibble::tibble(STUDYID=character(0),DOMAIN=character(0),USUBJID=character(0),TRSEQ=numeric(0),TRLNKID=character(0),TRTESTCD=character(0),TRTEST=character(0),TRORRES=character(0),TRORRESU=character(0),TRSTRESC=character(0),TRSTRESN=numeric(0),TRSTRESU=character(0),TREVAL=character(0),TRDTC=character(0),TRSTDY=numeric(0),VISIT=character(0))
}

cat(paste0("sdtm_tr rows: ", nrow(sdtm_tr), "\n"))

write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

fmt_num <- function(x) {
  ifelse(is.na(x), ".", as.character(x))
}
fmt_char <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

print_df <- sdtm_tr
obs <- seq_len(nrow(print_df))
disp <- data.frame(
  Obs = obs,
  STUDYID = fmt_char(print_df$STUDYID),
  DOMAIN = fmt_char(print_df$DOMAIN),
  USUBJID = fmt_char(print_df$USUBJID),
  TRSEQ = fmt_num(print_df$TRSEQ),
  TRLNKID = fmt_char(print_df$TRLNKID),
  TRTESTCD = fmt_char(print_df$TRTESTCD),
  TRTEST = fmt_char(print_df$TRTEST),
  TRORRES = fmt_char(print_df$TRORRES),
  TRORRESU = fmt_char(print_df$TRORRESU),
  TRSTRESC = fmt_char(print_df$TRSTRESC),
  TRSTRESN = fmt_num(print_df$TRSTRESN),
  TRSTRESU = fmt_char(print_df$TRSTRESU),
  TREVAL = fmt_char(print_df$TREVAL),
  TRDTC = fmt_char(print_df$TRDTC),
  TRSTDY = fmt_num(print_df$TRSTDY),
  VISIT = fmt_char(print_df$VISIT),
  stringsAsFactors = FALSE, check.names = FALSE
)

write.csv(disp, file.path(out, "P028.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P028.txt")
lst_path <- file.path(out, "P028.lst")
con_lines <- c(
  "P028 TR Listing",
  "PROC PRINT of SDTM TR",
  "",
  paste0("Observations: ", nrow(disp))
)
hdr <- paste(names(disp), collapse = " | ")
con_lines <- c(con_lines, "", hdr, paste(rep("-", nchar(hdr)), collapse = ""))
if (nrow(disp) > 0) {
  for (i in seq_len(nrow(disp))) {
    rowvals <- sapply(disp[i, ], as.character, USE.NAMES = FALSE)
    con_lines <- c(con_lines, paste(rowvals, collapse = " | "))
  }
} else {
  con_lines <- c(con_lines, "(no observations)")
}
con_lines <- c(con_lines, "", paste0("End of listing. N=", nrow(disp)))
writeLines(con_lines, txt_path)
writeLines(con_lines, lst_path)

cat("Outputs written to OUT_DIR\n")
```