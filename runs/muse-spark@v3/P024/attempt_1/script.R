out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, {
    m <- 10^digits
    sign(x) * floor(abs(x) * m + 0.5) / m
  })
}

sas_tranwrd <- function(s, from, to) {
  vapply(s, function(x) {
    if (is.na(x)) return(NA_character_)
    gsub(from, to, x, fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

parse_date11_vec <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    s <- trimws(as.character(v))
    if (s == "" || s == "." ) return(as.Date(NA))
    m <- regmatches(s, regexec("^\\s*(\\d{1,2})-([A-Za-z]{3})-(\\d{4})\\s*$", s))[[1]]
    if (length(m) != 4) return(as.Date(NA))
    d <- suppressWarnings(as.integer(m[2]))
    mon <- toupper(m[3])
    y <- suppressWarnings(as.integer(m[4]))
    months <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
    if (!mon %in% names(months) || is.na(d) || is.na(y)) return(as.Date(NA))
    mm <- months[[mon]]
    dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mm, d), format="%Y-%m-%d"))
    if (is.na(dt)) return(as.Date(NA))
    # validate day/month (e.g. 31-FEB invalid -> as.Date rolls? check components)
    lt <- as.POSIXlt(dt)
    if (lt$mday != d || (lt$mon+1) != mm || (lt$year+1900) != y) return(as.Date(NA))
    dt
  }, as.Date(NA), USE.NAMES = FALSE)
}
parse_date11_vec <- function(x) {
  res <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) next
    s <- trimws(as.character(v))
    if (s == "" || s == ".") next
    m <- regmatches(s, regexec("^\\s*(\\d{1,2})-([A-Za-z]{3})-(\\d{4})\\s*$", s))[[1]]
    if (length(m) != 4) next
    d <- suppressWarnings(as.integer(m[2]))
    mon <- toupper(m[3])
    y <- suppressWarnings(as.integer(m[4]))
    months <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
    if (!mon %in% names(months) || is.na(d) || is.na(y)) next
    mm <- unname(months[mon])
    dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mm, d), format="%Y-%m-%d"))
    if (is.na(dt)) next
    lt <- as.POSIXlt(dt)
    if (lt$mday != d || (lt$mon+1) != mm || (lt$year+1900) != y) next
    res[i] <- dt
  }
  res
}

parse_yymmdd_vec <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    s <- trimws(as.character(v))
    if (s == "" ) return(as.Date(NA))
    s10 <- substr(s, 1, 10)
    dt <- suppressWarnings(as.Date(s10, format="%Y-%m-%d"))
    dt
  }, as.Date(NA), USE.NAMES = FALSE)
}
parse_yymmdd_vec <- function(x) {
  res <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) next
    s <- trimws(as.character(v))
    if (s == "") next
    s10 <- substr(s, 1, 10)
    dt <- suppressWarnings(as.Date(s10, format="%Y-%m-%d"))
    if (length(dt)==0 || is.na(dt)) next
    # strict check: reformat must match
    if (format(dt, "%Y-%m-%d") != s10) next
    res[i] <- dt
  }
  res
}

find_col <- function(df, candidates) {
  un <- toupper(names(df))
  for (cand in candidates) {
    idx <- which(un == toupper(cand))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}
find_col_contains <- function(df, patterns, exclude = NULL) {
  un <- toupper(names(df))
  if (!is.null(exclude)) {
    exc_idx <- which(un %in% toupper(exclude))
  } else exc_idx <- integer(0)
  for (pat in patterns) {
    for (j in seq_along(names(df))) {
      if (j %in% exc_idx) next
      if (grepl(pat, un[j], fixed = TRUE)) return(names(df)[j])
    }
  }
  return(NA_character_)
}

# Convert an arbitrary Excel-read vector to ISO YYYY-MM-DD character (or NA)
# Decides slash order once per column (MDY vs DMY) per PROC IMPORT rule
to_iso_char <- function(v) {
  n <- length(v)
  if (n == 0) return(character(0))
  if (inherits(v, "Date")) {
    return(ifelse(is.na(v), NA_character_, format(v, "%Y-%m-%d")))
  }
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
    d <- as.Date(v)
    return(ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d")))
  }
  if (is.numeric(v)) {
    # numeric SUBJID-like or Excel serial? If looks like date serial (30000-60000) could be date,
    # but we cannot distinguish; treat as character (caller should handle dates as Date type).
    # For date conversion, return NA to avoid misinterpretation.
    return(rep(NA_character_, n))
  }
  # character / factor
  cvec <- trimws(as.character(v))
  cvec[cvec == "" | cvec == "." | toupper(cvec) == "NA" | toupper(cvec) == "NULL"] <- NA_character_
  # determine slash order once per column
  slash_vals <- cvec[!is.na(cvec) & grepl("/", cvec, fixed = TRUE)]
  slash_order <- "MDY"
  if (length(slash_vals) > 0) {
    first_fields <- suppressWarnings(as.integer(sub("^\\s*(\\d{1,4}).*$", "\\1", slash_vals)))
    # only consider values matching n/n/yyyy pattern
    is_nny <- grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s*$", slash_vals)
    if (any(is_nny, na.rm = TRUE)) {
      ff <- suppressWarnings(as.integer(sub("^\\s*(\\d{1,2})/.*$", "\\1", slash_vals[is_nny])))
      if (any(ff > 12, na.rm = TRUE)) slash_order <- "DMY" else slash_order <- "MDY"
    }
  }
  out <- rep(NA_character_, n)
  for (i in seq_along(cvec)) {
    s <- cvec[i]
    if (is.na(s)) next
    # already ISO?
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) {
      cand <- substr(s, 1, 10)
      dt <- suppressWarnings(as.Date(cand, format="%Y-%m-%d"))
      if (!is.na(dt) && format(dt, "%Y-%m-%d") == cand) { out[i] <- cand; next }
    }
    # date11 ?
    if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", trimws(s))) {
      dt <- parse_date11_vec(s)
      if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    }
    # slash n/n/yyyy ?
    if (grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s*$", s)) {
      parts <- strsplit(trimws(s), "/")[[1]]
      a <- suppressWarnings(as.integer(parts[1])); b <- suppressWarnings(as.integer(parts[2])); y <- suppressWarnings(as.integer(parts[3]))
      if (any(is.na(c(a,b,y)))) next
      if (y < 100) y <- ifelse(y <= 69, 2000+y, 1900+y)
      if (slash_order == "MDY") { m <- a; d <- b } else { d <- a; m <- b }
      dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d), format="%Y-%m-%d"))
      if (!is.na(dt)) {
        lt <- as.POSIXlt(dt)
        if (lt$mday==d && (lt$mon+1)==m && (lt$year+1900)==y) { out[i] <- format(dt, "%Y-%m-%d"); next }
      }
      next
    }
    # try yyyymmdd without dashes?
    if (grepl("^\\d{8}$", trimws(s))) {
      dt <- suppressWarnings(as.Date(trimws(s), format="%Y%m%d"))
      if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    }
    # else leave NA
  }
  out
}

subj_to_char <- function(v) {
  if (is.numeric(v)) {
    vapply(v, function(x) if (is.na(x)) NA_character_ else trimws(format(x, scientific=FALSE, trim=TRUE)), character(1), USE.NAMES=FALSE)
  } else {
    vapply(as.character(v), function(x) if (is.na(x)) NA_character_ else trimws(x), character(1), USE.NAMES=FALSE)
  }
}
substr_sas <- function(x, len) {
  vapply(x, function(v) if (is.na(v)) NA_character_ else substr(v, 1, len), character(1), USE.NAMES=FALSE)
}

base_raw <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
edc_ds_path <- file.path(base_raw, "edc_ds.xlsx")
edc_dm_path <- file.path(base_raw, "edc_dm.xlsx")
edc_death_path <- file.path(base_raw, "edc_death.xlsx")
edc_ex_path <- file.path(base_raw, "edc_ex.xlsx")
ivrs_path <- file.path(base_raw, "ivrs_rand.xlsx")

edc_ds_raw <- as.data.frame(readxl::read_excel(edc_ds_path, guess_max = 10000), stringsAsFactors = FALSE)
edc_dm_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_dm_path, guess_max = 10000), stringsAsFactors = FALSE), error=function(e) NULL)
edc_death_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_death_path, guess_max = 10000), stringsAsFactors = FALSE), error=function(e) NULL)
edc_ex_raw <- tryCatch(as.data.frame(readxl::read_excel(edc_ex_path, guess_max = 10000), stringsAsFactors = FALSE), error=function(e) NULL)
ivrs_raw <- tryCatch(as.data.frame(readxl::read_excel(ivrs_path, guess_max = 10000), stringsAsFactors = FALSE), error=function(e) NULL)

# ---- 2. Prepare Raw DS Data ----
subj_ds_col <- find_col(edc_ds_raw, c("SUBJID"))
if (is.na(subj_ds_col)) subj_ds_col <- find_col_contains(edc_ds_raw, c("SUBJ"))
stopifnot(!is.na(subj_ds_col))
ds_term_col <- find_col(edc_ds_raw, c("DS_TERM"))
if (is.na(ds_term_col)) ds_term_col <- find_col(edc_ds_raw, c("DSTERM","TERM"))
stopifnot(!is.na(ds_term_col))
ds_cat_col <- find_col(edc_ds_raw, c("DS_CAT"))
if (is.na(ds_cat_col)) ds_cat_col <- find_col(edc_ds_raw, c("DSCAT","CAT"))
ds_dt_col <- find_col(edc_ds_raw, c("DS_DT"))
if (is.na(ds_dt_col)) ds_dt_col <- find_col(edc_ds_raw, c("DSSTDTC","DSSTDT","STDT","DT"))
stopifnot(!is.na(ds_dt_col))

SUBJID_ds <- subj_to_char(edc_ds_raw[[subj_ds_col]])
TERM_raw <- as.character(edc_ds_raw[[ds_term_col]])
CAT_raw <- if (!is.na(ds_cat_col)) as.character(edc_ds_raw[[ds_dt_col]] * 0 + edc_ds_raw[[ds_cat_col]]) else rep(NA_character_, nrow(edc_ds_raw))
# above trick to keep length; simpler:
CAT_raw <- if (!is.na(ds_cat_col)) as.character(edc_ds_raw[[ds_cat_col]]) else rep(NA_character_, nrow(edc_ds_raw))
DS_DT_raw <- edc_ds_raw[[ds_dt_col]]
# STDT $11: if DS_DT is Date/POSIX, format as date11-like? SAS would numeric-to-char; emulate SAS: convert Date to "" (so DSSTDTC missing)?
# But to preserve dates, if Date type, convert to date11 string first then parse (round-trip preserves ISO).
if (inherits(DS_DT_raw, "Date")) {
  STDT_raw <- ifelse(is.na(DS_DT_raw), NA_character_, format(DS_DT_raw, "%d-%b-%Y"))
  STDT_raw <- toupper(STDT_raw)
} else if (inherits(DS_DT_raw, "POSIXct") || inherits(DS_DT_raw, "POSIXt")) {
  d <- as.Date(DS_DT_raw)
  STDT_raw <- ifelse(is.na(d), NA_character_, format(d, "%d-%b-%Y"))
  STDT_raw <- toupper(STDT_raw)
} else {
  STDT_raw <- trimws(as.character(DS_DT_raw))
  STDT_raw[STDT_raw=="" ] <- NA_character_
}
TERM <- substr_sas(TERM_raw, 50)
DECOD <- substr_sas(TERM_raw, 50)
CATv <- substr_sas(CAT_raw, 50)
STDT <- substr_sas(STDT_raw, 11)
USUBJID_ds <- paste0("ONCO-2026-", trimws(SUBJID_ds))
USUBJID_ds[grepl("NA$", USUBJID_ds) & is.na(SUBJID_ds)] <- NA_character_

edc_ds_prep <- data.frame(
  SUBJID = SUBJID_ds,
  TERM = TERM,
  DECOD = DECOD,
  CAT = CATv,
  STDT = STDT,
  USUBJID = USUBJID_ds,
  stringsAsFactors = FALSE
)
# sort by USUBJID (SAS sorts missing first; character ordering)
edc_ds_prep <- edc_ds_prep[order(edc_ds_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# ---- 1/3. dm_for_ds : derive RFSTDTC / DTHDTC per CDISC ----
# helper to get SUBJID vector from a df
get_subj_vec <- function(df) {
  if (is.null(df) || nrow(df)==0) return(NULL)
  cname <- find_col(df, c("SUBJID"))
  if (is.na(cname)) cname <- find_col_contains(df, c("SUBJ"))
  if (is.na(cname)) return(NULL)
  list(col=cname, vec=subj_to_char(df[[cname]]))
}
dm_subj <- get_subj_vec(edc_dm_raw)

rf_iso_dm <- NULL
dth_iso_dm <- NULL
if (!is.null(edc_dm_raw)) {
  rfcol <- find_col(edc_dm_raw, c("RFSTDTC"))
  if (is.na(rfcol)) rfcol <- find_col(edc_dm_raw, c("RFSTD","RANDDTC","RANDDT"))
  if (!is.na(rfcol)) {
    v <- edc_dm_raw[[rfcol]]
    if (inherits(v,"Date")||inherits(v,"POSIXct")||inherits(v,"POSIXt")) {
      d <- if (inherits(v,"Date")) v else as.Date(v)
      rf_iso_dm <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
    } else {
      rf_iso_dm <- to_iso_char(v)
    }
  }
  dcol <- find_col(edc_dm_raw, c("DTHDTC"))
  if (is.na(dcol)) dcol <- find_col(edc_dm_raw, c("DTHDT","DTHDTC","DEATHDTC","DEATHDT","DTH_DT","DEATH_DT"))
  if (is.na(dcol)) dcol <- find_col_contains(edc_dm_raw, c("DTH","DEATH"), exclude=c("DTHFL"))
  if (!is.na(dcol)) {
    v <- edc_dm_raw[[dcol]]
    # avoid flag columns (single Y/N)
    uniq <- unique(trimws(as.character(v[!is.na(v)])))
    if (!(length(uniq)<=2 && all(toupper(uniq) %in% c("Y","N","")))) {
      if (inherits(v,"Date")||inherits(v,"POSIXct")||inherits(v,"POSIXt")) {
        d <- if (inherits(v,"Date")) v else as.Date(v)
        dth_iso_dm <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else {
        dth_iso_dm <- to_iso_char(v)
      }
    }
  }
}

# derive RF from ivrs_rand + edc_ex if needed
rf_from_rand <- NULL
if (!is.null(ivrs_raw) && nrow(ivrs_raw)>0) {
  si <- get_subj_vec(ivrs_raw)
  if (!is.null(si)) {
    # find date col: prefer RAND*, else first date-like col
    dc <- find_col(ivrs_raw, c("RANDDTC","RANDDT","RAND_DT","RNDDT","RANDOMDT","RFSTDTC","RANDDTC"))
    if (is.na(dc)) dc <- find_col_contains(ivrs_raw, c("RAND","RND","RFSTD","DTC","_DT","DATE"), exclude=c(si$col))
    if (!is.na(dc)) {
      v <- ivrs_raw[[dc]]
      if (inherits(v,"Date")||inherits(v,"POSIXct")||inherits(v,"POSIXt")) {
        d <- if (inherits(v,"Date")) v else as.Date(v)
        iso <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else iso <- to_iso_char(v)
      rf_from_rand <- data.frame(SUBJID=si$vec, ISO=iso, stringsAsFactors=FALSE)
      rf_from_rand <- rf_from_rand[!is.na(rf_from_rand$SUBJID) & rf_from_rand$SUBJID!="", , drop=FALSE]
    }
  }
}
rf_from_ex <- NULL
if (!is.null(edc_ex_raw) && nrow(edc_ex_raw)>0) {
  si <- get_subj_vec(edc_ex_raw)
  if (!is.null(si)) {
    dc <- find_col(edc_ex_raw, c("EXSTDTC","EXSTDT","EX_DT","EXSTDAT","EXSTDT","RFSTDTC"))
    if (is.na(dc)) dc <- find_col_contains(edc_ex_raw, c("EXSTD","EXST","START","STDTC","_DT","DATE","DTC"), exclude=c(si$col))
    if (!is.na(dc)) {
      v <- edc_ex_raw[[dc]]
      if (inherits(v,"Date")||inherits(v,"POSIXct")||inherits(v,"POSIXt")) {
        d <- if (inherits(v,"Date")) v else as.Date(v)
        iso <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else iso <- to_iso_char(v)
      tmp <- data.frame(SUBJID=si$vec, ISO=iso, stringsAsFactors=FALSE)
      tmp <- tmp[!is.na(tmp$SUBJID) & tmp$SUBJID!="" & !is.na(tmp$ISO), , drop=FALSE]
      if (nrow(tmp)>0) {
        # min per subject
        tmp$D <- as.Date(tmp$ISO, format="%Y-%m-%d")
        agg <- aggregate(D ~ SUBJID, data=tmp, FUN=min)
        rf_from_ex <- data.frame(SUBJID=agg$SUBJID, ISO=format(agg$D, "%Y-%m-%d"), stringsAsFactors=FALSE)
      }
    }
  }
}
# derive DTH from edc_death if needed
dth_from_death <- NULL
if (!is.null(edc_death_raw) && nrow(edc_death_raw)>0) {
  si <- get_subj_vec(edc_death_raw)
  if (!is.null(si)) {
    dc <- find_col(edc_death_raw, c("DTHDTC","DTHDT","DEATHDTC","DEATHDT","DTH_DT","DEATH_DT","DEATH_DATE","DTH_DATE"))
    if (is.na(dc)) dc <- find_col_contains(edc_death_raw, c("DTH","DEATH","DTC","_DT","DATE"), exclude=c(si$col))
    if (!is.na(dc)) {
      v <- edc_death_raw[[dc]]
      if (inherits(v,"Date")||inherits(v,"POSIXct")||inherits(v,"POSIXt")) {
        d <- if (inherits(v,"Date")) v else as.Date(v)
        iso <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else iso <- to_iso_char(v)
      dth_from_death <- data.frame(SUBJID=si$vec, ISO=iso, stringsAsFactors=FALSE)
      dth_from_death <- dth_from_death[!is.na(dth_from_death$SUBJID) & dth_from_death$SUBJID!="", , drop=FALSE]
      # if multiple rows per subject, keep max (latest death date) or first non-missing
      dth_from_death <- dth_from_death[order(dth_from_death$SUBJID, dth_from_death$ISO, na.last=TRUE), , drop=FALSE]
      dth_from_death <- dth_from_death[!duplicated(dth_from_death$SUBJID, fromLast=TRUE), , drop=FALSE]
    }
  }
}

# build master subject list
all_subj <- unique(c(
  if (!is.null(dm_subj)) dm_subj$vec else NULL,
  subj_to_char(edc_ds_prep$SUBJID),
  if (!is.null(rf_from_rand)) rf_from_rand$SUBJID else NULL,
  if (!is.null(rf_from_ex)) rf_from_ex$SUBJID else NULL,
  if (!is.null(dth_from_death)) dth_from_death$SUBJID else NULL
))
all_subj <- all_subj[!is.na(all_subj) & all_subj!=""]
dm_base <- data.frame(SUBJID=all_subj, stringsAsFactors=FALSE)
dm_base$USUBJID <- paste0("ONCO-2026-", trimws(dm_base$SUBJID))
# attach RFSTDTC from DM if available
if (!is.null(dm_subj) && !is.null(rf_iso_dm)) {
  map <- data.frame(SUBJID=dm_subj$vec, RF_DM=rf_iso_dm, stringsAsFactors=FALSE)
  map <- map[!duplicated(map$SUBJID), , drop=FALSE]
  dm_base <- merge(dm_base, map, by="SUBJID", all.x=TRUE, sort=FALSE)
} else dm_base$RF_DM <- NA_character_
if (!is.null(dm_subj) && !is.null(dth_iso_dm)) {
  map2 <- data.frame(SUBJID=dm_subj$vec, DTH_DM=dth_iso_dm, stringsAsFactors=FALSE)
  map2 <- map2[!duplicated(map2$SUBJID), , drop=FALSE]
  dm_base <- merge(dm_base, map2, by="SUBJID", all.x=TRUE, sort=FALSE)
} else dm_base$DTH_DM <- NA_character_

# attach rand/ex
if (!is.null(rf_from_rand)) {
  r2 <- rf_from_rand[!duplicated(rf_from_rand$SUBJID), , drop=FALSE]
  names(r2)[2] <- "RF_RAND"
  dm_base <- merge(dm_base, r2, by="SUBJID", all.x=TRUE, sort=FALSE)
} else dm_base$RF_RAND <- NA_character_
if (!is.null(rf_from_ex)) {
  dm_base <- merge(dm_base, rf_from_ex, by="SUBJID", all.x=TRUE, sort=FALSE)
  names(dm_base)[names(dm_base)=="ISO"] <- "RF_EX"
} else dm_base$RF_EX <- NA_character_
if (!is.null(dth_from_death)) {
  d2 <- dth_from_death
  names(d2)[2] <- "DTH_DEATH"
  dm_base <- merge(dm_base, d2, by="SUBJID", all.x=TRUE, sort=FALSE)
} else dm_base$DTH_DEATH <- NA_character_

# coalesce: RF = RF_DM else min(RF_RAND, RF_EX)
dm_base$RFSTDTC <- dm_base$RF_DM
need <- is.na(dm_base$RFSTDTC)
if (any(need)) {
  for (i in which(need)) {
    cands <- c(dm_base$RF_RAND[i], dm_base$RF_EX[i])
    cands <- cands[!is.na(cands)]
    if (length(cands)==0) next
    ds <- suppressWarnings(as.Date(cands, format="%Y-%m-%d"))
    ds <- ds[!is.na(ds)]
    if (length(ds)>0) dm_base$RFSTDTC[i] <- format(min(ds), "%Y-%m-%d")
  }
}
dm_base$DTHDTC <- dm_base$DTH_DM
need2 <- is.na(dm_base$DTHDTC)
if (any(need2)) {
  dm_base$DTHDTC[need2] <- dm_base$DTH_DEATH[need2]
}
dm_for_ds <- dm_base[, c("USUBJID","SUBJID","RFSTDTC","DTHDTC"), drop=FALSE]
dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last=FALSE), , drop=FALSE]

# ---- 4. Create SDTM DS1 (merge edc_ds_prep + dm_for_ds) ----
# drop overlapping non-key cols from dm side (SUBJID) to avoid suffixes
dm_join <- dm_for_ds[, c("USUBJID","RFSTDTC","DTHDTC"), drop=FALSE]
sdtm_ds1 <- merge(edc_ds_prep, dm_join, by="USUBJID", all.x=TRUE, sort=FALSE)
stopifnot(all(c("TERM","DECOD","CAT","STDT","RFSTDTC","DTHDTC") %in% names(sdtm_ds1)))

sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"
sdtm_ds1$DSTERM <- sas_tranwrd(sdtm_ds1$TERM, "_", " ")
sdtm_ds1$DSTERM <- substr_sas(sdtm_ds1$DSTERM, 50)
tmp_decod <- toupper(ifelse(is.na(sdtm_ds1$DECOD), NA_character_, sdtm_ds1$DECOD))
sdtm_ds1$DSDECOD <- substr_sas(tmp_decod, 30)
sdtm_ds1$DSCAT <- substr_sas(sas_tranwrd(sdtm_ds1$CAT, "_", " "), 30)
sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(sdtm_ds1$DTHDTC)!="", "Y", NA_character_)

# DSSTDTC = put(input(STDT,date11.), yymmdd10.)
stdt_parsed <- parse_date11_vec(sdtm_ds1$STDT)
sdtm_ds1$DSSTDTC <- ifelse(is.na(sdtm_ds1$STDT) | trimws(sdtm_ds1$STDT)=="" | is.na(stdt_parsed),
  NA_character_, format(stdt_parsed, "%Y-%m-%d"))

# DSSTDY
rf_parsed <- parse_yymmdd_vec(sdtm_ds1$RFSTDTC)
st_parsed2 <- parse_yymmdd_vec(sdtm_ds1$DSSTDTC)
sdtm_ds1$DSSTDY <- rep(NA_real_, nrow(sdtm_ds1))
for (i in seq_len(nrow(sdtm_ds1))) {
  if (!is.na(st_parsed2[i]) && !is.na(rf_parsed[i])) {
    diff <- as.integer(st_parsed2[i] - rf_parsed[i])
    sdtm_ds1$DSSTDY[i] <- if (st_parsed2[i] >= rf_parsed[i]) diff + 1 else diff
  }
}

# consistency check subject 103
chk <- sdtm_ds1[sdtm_ds1$USUBJID=="ONCO-2026-103", , drop=FALSE]
if (nrow(chk)>0) {
  for (i in seq_len(nrow(chk))) {
    a <- chk$DSSTDTC[i]; b <- chk$DTHDTC[i]
    na_a <- is.na(a); na_b <- is.na(b)
    mismatch <- FALSE
    if (na_a && na_b) mismatch <- FALSE
    else if (na_a || na_b) mismatch <- TRUE
    else if (a != b) mismatch <- TRUE
    if (mismatch) cat(sprintf("WARNING: Date mismatch! DS=%s DM=%s\n",
      ifelse(na_a,"",a), ifelse(na_b,"",b)))
  }
}

# ---- 5. Sort final ----
ord <- order(sdtm_ds1$USUBJID, ifelse(is.na(sdtm_ds1$DSSTDTC),"",sdtm_ds1$DSSTDTC), na.last=FALSE)
sdtm_ds1 <- sdtm_ds1[ord, , drop=FALSE]

# ---- 6. Derive DSSEQ ----
sdtm_ds1 <- sdtm_ds1[order(sdtm_ds1$USUBJID, ifelse(is.na(sdtm_ds1$DSSTDTC),"",sdtm_ds1$DSSTDTC), na.last=FALSE), , drop=FALSE]
sdtm_ds1$DSSEQ <- ave(seq_len(nrow(sdtm_ds1)), sdtm_ds1$USUBJID, FUN=function(x) seq_along(x))

sdtm_ds <- sdtm_ds1[, c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL"), drop=FALSE]
# ensure types
sdtm_ds$DSSEQ <- as.numeric(sdtm_ds$DSSEQ)
sdtm_ds$DSSTDY <- as.numeric(sdtm_ds$DSSTDY)

# ---- 7/8. Print + Export ----
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names=FALSE, na="")

# PROC PRINT text rendering
print_df <- cbind(Obs=seq_len(nrow(sdtm_ds)), sdtm_ds)
fmt_cell <- function(v) ifelse(is.na(v), "", as.character(v))
# build fixed-width-ish text
hdr <- c("Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")
lines <- c("PROC PRINT DATA=SDTM_DS;", "", paste(hdr, collapse=" | "))
for (i in seq_len(nrow(print_df))) {
  rowv <- sapply(print_df[i, hdr, drop=FALSE], function(x) if (is.na(x)) "" else as.character(x))
  lines <- c(lines, paste(rowv, collapse=" | "))
}
lines <- c(lines, "", sprintf("N = %d", nrow(sdtm_ds)))
writeLines(lines, file.path(out, "P024.txt"))
writeLines(lines, file.path(out, "P024.lst"))
write.csv(print_df, file.path(out, "P024.csv"), row.names=FALSE, na="")
