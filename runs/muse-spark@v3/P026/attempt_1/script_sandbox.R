out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_base <- "/bench/data/raw/faijal-oncology/raw_data"
f_lb   <- file.path(raw_base, "edc_lb.xlsx")
f_dm   <- file.path(raw_base, "edc_dm.xlsx")
f_ex   <- file.path(raw_base, "edc_ex.xlsx")
f_rand <- file.path(raw_base, "ivrs_rand.xlsx")

round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}
sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, x, fixed = TRUE))
}
to_char_trim <- function(x) {
  if (is.null(x)) return(rep(NA_character_, 0))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x)) {
    s <- ifelse(is.na(x), NA_character_, trimws(format(x, scientific = FALSE, trim = TRUE)))
    return(s)
  }
  s <- as.character(x)
  s <- trimws(s)
  s[s == ""] <- NA_character_
  # keep NA as NA; caller decides "" vs NA
  s
}
get_col <- function(df, target) {
  nm <- names(df)
  idx <- which(toupper(nm) == toupper(target))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
get_col_name <- function(df, target) {
  nm <- names(df)
  idx <- which(toupper(nm) == toupper(target))
  if (length(idx) == 0) return(NULL)
  nm[idx[1]]
}
sas_substr <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(x, 1, len))
}
sas_input_best <- function(x) {
  # emulate SAS INPUT(..., best.): read numeric prefix
  v <- trimws(as.character(x))
  v[v == "" | is.na(v)] <- NA_character_
  suppressWarnings(as.numeric(v))
}
norm_mon <- function(x) {
  # normalize DD-MON-YYYY month case to Title case for %b parsing
  ifelse(is.na(x), NA_character_, gsub("([0-9]{1,2}-)([A-Za-z]{3})(-.*)",
    "\\1\\U\\2\\E\\3", x, perl = FALSE))
  # fallback manual: use regex with tolower/toupper per element
}
title_mon <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || v == "") return(v)
    m <- regmatches(v, regexec("^([0-9]{1,2})-([A-Za-z]{3})-(.*)$", v))[[1]]
    if (length(m) == 4) {
      mon <- paste0(toupper(substr(m[3],1,1)), tolower(substr(m[3],2,3)))
      paste0(m[2], "-", mon, "-", m[4])
    } else v
  }, USE.NAMES = FALSE)
}
parse_lbdt_single <- function(x) {
  # ONE strict format: DATE11. = %d-%b-%Y ; x may be Date/POSIXct/numeric/character
  n <- length(x)
  res <- rep(as.Date(NA), n)
  for (i in seq_len(n)) {
    v <- x[i]
    if (is.na(v)) next
    if (inherits(v, "Date")) { res[i] <- v; next }
    if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) { res[i] <- as.Date(v); next }
    if (is.numeric(v)) {
      # Excel serial unlikely; treat as days since 1899-12-30 if plausible
      if (!is.na(v) && v > 20000 && v < 60000) res[i] <- as.Date(v, origin = "1899-12-30")
      next
    }
    s <- trimws(as.character(v))
    if (is.na(s) || s == "" || s == "NA") next
    s2 <- title_mon(s)
    d <- suppressWarnings(as.Date(s2, format = "%d-%b-%Y"))
    res[i] <- d
  }
  res
}
parse_iso_single <- function(x) {
  n <- length(x)
  res <- rep(as.Date(NA), n)
  for (i in seq_len(n)) {
    v <- x[i]
    if (is.na(v)) next
    if (inherits(v, "Date")) { res[i] <- v; next }
    if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) { res[i] <- as.Date(v); next }
    if (is.numeric(v)) {
      if (!is.na(v) && v > 20000 && v < 60000) res[i] <- as.Date(v, origin = "1899-12-30")
      next
    }
    s <- trimws(as.character(v))
    if (is.na(s) || s == "") next
    d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
    res[i] <- d
  }
  res
}
# Generic single-format character date parser for RFSTDTC source columns:
# decide order once per column from all values, then parse every value with that format
parse_char_col_single_format <- function(vec) {
  # vec: character vector (may contain NA/"")
  # returns Date vector
  n <- length(vec)
  res <- rep(as.Date(NA), n)
  # handle Date/POSIX/numeric upstream before calling; here only character
  vals <- trimws(as.character(vec))
  vals_nonmiss <- vals[!is.na(vals) & vals != ""]
  if (length(vals_nonmiss) == 0) return(res)
  # detect YYYY-MM-DD
  if (all(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", vals_nonmiss))) {
    for (i in seq_len(n)) {
      s <- trimws(as.character(vec[i]))
      if (is.na(s) || s == "") next
      res[i] <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
    }
    return(res)
  }
  # detect DD-MON-YYYY
  if (all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", vals_nonmiss))) {
    s2 <- title_mon(trimws(as.character(vec)))
    for (i in seq_len(n)) {
      if (is.na(s2[i]) || s2[i] == "") next
      # decide 2 vs 4 digit year: use %y if 2-digit
      yr <- sub("^[0-9]{1,2}-[A-Za-z]{3}-", "", s2[i])
      fmt <- if (nchar(yr) == 2) "%d-%b-%y" else "%d-%b-%Y"
      res[i] <- suppressWarnings(as.Date(s2[i], format = fmt))
    }
    return(res)
  }
  # detect slash dates n/n/yyyy
  slash_idx <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", vals_nonmiss)
  if (any(slash_idx)) {
    # decide order once per column: if any first field > 12 => DMY else MDY (PROC IMPORT rule)
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", vals_nonmiss[slash_idx])))
    use_dmy <- any(!is.na(first_fields) & first_fields > 12)
    # decide year digits
    yr2 <- any(nchar(sub("^.*/","", vals_nonmiss[slash_idx])) == 2)
    fmt <- if (use_dmy) { if (yr2) "%d/%m/%y" else "%d/%m/%Y" } else { if (yr2) "%m/%d/%y" else "%m/%d/%Y" }
    for (i in seq_len(n)) {
      s <- trimws(as.character(vec[i]))
      if (is.na(s) || s == "") next
      if (!grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", s)) next
      res[i] <- suppressWarnings(as.Date(s, format = fmt))
    }
    return(res)
  }
  # detect dash numeric n-n-yyyy
  dash_idx <- grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", vals_nonmiss)
  if (any(dash_idx)) {
    first_fields <- suppressWarnings(as.integer(sub("-.*$", "", vals_nonmiss[dash_idx])))
    use_dmy <- any(!is.na(first_fields) & first_fields > 12)
    yr2 <- any(nchar(sub("^.*-","", vals_nonmiss[dash_idx])) == 2)
    fmt <- if (use_dmy) { if (yr2) "%d-%m-%y" else "%d-%m-%Y" } else { if (yr2) "%m-%d-%y" else "%m-%d-%Y" }
    for (i in seq_len(n)) {
      s <- trimws(as.character(vec[i]))
      if (is.na(s) || s == "") next
      if (!grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", s)) next
      res[i] <- suppressWarnings(as.Date(s, format = fmt))
    }
    return(res)
  }
  # fallback: try ISO
  for (i in seq_len(n)) {
    s <- trimws(as.character(vec[i]))
    if (is.na(s) || s == "") next
    res[i] <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  }
  res
}
col_to_date <- function(col) {
  if (is.null(col)) return(NULL)
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) return(as.Date(col))
  if (is.numeric(col)) {
    # if looks like Excel serial, convert; else NA
    d <- rep(as.Date(NA), length(col))
    idx <- !is.na(col) & col > 20000 & col < 60000
    d[idx] <- as.Date(col[idx], origin = "1899-12-30")
    # if not serial range, leave NA (numeric best. dates not expected)
    return(d)
  }
  parse_char_col_single_format(as.character(col))
}
find_best_date_col <- function(df, exclude = NULL) {
  # find column (excluding SUBJID-like) with highest parseable date rate
  nms <- names(df)
  subj_nm <- get_col_name(df, "SUBJID")
  cand <- setdiff(nms, c(subj_nm, exclude))
  best_nm <- NULL; best_rate <- 0; best_dates <- NULL
  for (nm in cand) {
    col <- df[[nm]]
    if (inherits(col, "Date") || inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
      d <- col_to_date(col)
      rate <- mean(!is.na(d))
      if (rate > best_rate) { best_rate <- rate; best_nm <- nm; best_dates <- d }
    } else if (is.character(col) || is.numeric(col) || is.factor(col)) {
      d <- col_to_date(col)
      rate <- mean(!is.na(d))
      if (!is.na(rate) && rate > best_rate) { best_rate <- rate; best_nm <- nm; best_dates <- d }
    }
  }
  if (is.null(best_nm) || best_rate == 0) return(list(name = NULL, dates = NULL, rate = 0))
  list(name = best_nm, dates = best_dates, rate = best_rate)
}
min_date_by_subj <- function(df, subj_vec, date_vec) {
  # subj_vec character, date_vec Date
  ok <- !is.na(subj_vec) & subj_vec != "" & !is.na(date_vec)
  if (!any(ok)) return(data.frame(SUBJID = character(0), MINDATE = as.Date(character(0)), stringsAsFactors = FALSE))
  agg <- aggregate(date_vec[ok], by = list(SUBJID = subj_vec[ok]), FUN = min)
  names(agg)[2] <- "MINDATE"
  agg
}

message("Reading inputs")
edc_lb <- as.data.frame(readxl::read_excel(f_lb), stringsAsFactors = FALSE)
edc_dm_raw <- NULL; edc_ex_raw <- NULL; rand_raw <- NULL
if (file.exists(f_dm)) edc_dm_raw <- as.data.frame(readxl::read_excel(f_dm), stringsAsFactors = FALSE)
if (file.exists(f_ex)) edc_ex_raw <- as.data.frame(readxl::read_excel(f_ex), stringsAsFactors = FALSE)
if (file.exists(f_rand)) rand_raw <- as.data.frame(readxl::read_excel(f_rand), stringsAsFactors = FALSE)

# ---- 1. Prepare DM Reference Data (derive sdtm_dm RFSTDTC per CDISC) ----
# RFSTDTC = first exposure date if available else randomization date else DM date
ex_min <- NULL; rand_min <- NULL; dm_min <- NULL
if (!is.null(edc_ex_raw)) {
  sc <- get_col(edc_ex_raw, "SUBJID")
  if (!is.null(sc)) {
    subj_c <- trimws(as.character(sc)); subj_c[is.na(subj_c)] <- ""
    # prefer explicit EX start columns if present
    pref <- NULL
    for (pn in c("EXSTDTC","EXSTDT","EXSTDAT","EXSTART","EXDAT","EXDT") ) {
      cn <- get_col_name(edc_ex_raw, pn)
      if (!is.null(cn)) { pref <- cn; break }
    }
    if (!is.null(pref)) {
      d <- col_to_date(edc_ex_raw[[pref]])
    } else {
      fb <- find_best_date_col(edc_ex_raw)
      d <- fb$dates
    }
    if (!is.null(d)) ex_min <- min_date_by_subj(edc_ex_raw, subj_c, as.Date(d, origin="1970-01-01"))
  }
}
if (!is.null(rand_raw)) {
  sc <- get_col(rand_raw, "SUBJID")
  # ivrs_rand may use different subj id name: try SUBJID, SUBJECT, SUBJ, USUBJID
  if (is.null(sc)) {
    for (alt in c("SUBJECT","SUBJ","USUBJID","PATIENT")) {
      sc <- get_col(rand_raw, alt); if (!is.null(sc)) break
    }
  }
  if (!is.null(sc)) {
    subj_c <- trimws(as.character(sc)); subj_c[is.na(subj_c)] <- ""
    # strip ONCO-2026- prefix if USUBJID used
    subj_c <- sub("^ONCO-2026-", "", subj_c)
    pref <- NULL
    for (pn in c("RANDDTC","RANDDT","RANDDAT","RANDOMDT","RAND_DATE","RANDONDT")) {
      cn <- get_col_name(rand_raw, pn); if (!is.null(cn)) { pref <- cn; break }
    }
    if (!is.null(pref)) d <- col_to_date(rand_raw[[pref]]) else {
      fb <- find_best_date_col(rand_raw); d <- fb$dates
    }
    if (!is.null(d)) rand_min <- min_date_by_subj(rand_raw, subj_c, as.Date(d, origin="1970-01-01"))
  }
}
if (!is.null(edc_dm_raw)) {
  sc <- get_col(edc_dm_raw, "SUBJID")
  if (!is.null(sc)) {
    subj_c <- trimws(as.character(sc)); subj_c[is.na(subj_c)] <- ""
    subj_c <- sub("^ONCO-2026-", "", subj_c)
    pref <- NULL
    for (pn in c("RFSTDTC","RANDDT","RANDDTC","ENROLLDT","ENROLDT","BRTHDTC")) {
      cn <- get_col_name(edc_dm_raw, pn); if (!is.null(cn) && toupper(pn) != "BRTHDTC") { pref <- cn; break }
    }
    # avoid birth date: use best date col excluding birth-like
    if (!is.null(pref)) d <- col_to_date(edc_dm_raw[[pref]]) else {
      # exclude birth columns
      excl <- NULL
      for (bn in c("BRTHDT","BRTHDTC","DOB","BIRTH")) {
        cn <- get_col_name(edc_dm_raw, bn); if (!is.null(cn)) excl <- c(excl, cn)
      }
      fb <- find_best_date_col(edc_dm_raw, exclude = excl); d <- fb$dates
    }
    if (!is.null(d)) dm_min <- min_date_by_subj(edc_dm_raw, subj_c, as.Date(d, origin="1970-01-01"))
  }
}
# union subjects
all_subj <- unique(c(if (!is.null(ex_min)) ex_min$SUBJID,
                     if (!is.null(rand_min)) rand_min$SUBJID,
                     if (!is.null(dm_min)) dm_min$SUBJID))
# also include lb subjects to ensure coverage
lb_subj_raw <- get_col(edc_lb, "SUBJID")
lb_subj_c <- trimws(as.character(lb_subj_raw)); lb_subj_c[is.na(lb_subj_c)] <- ""
all_subj <- unique(c(all_subj, lb_subj_c[lb_subj_c != ""]))
dm_ref <- data.frame(SUBJID = all_subj, stringsAsFactors = FALSE)
if (!is.null(ex_min)) dm_ref <- merge(dm_ref, ex_min, by = "SUBJID", all.x = TRUE)
else dm_ref$ MINDATE <- as.Date(NA)
if (!is.null(rand_min)) {
  names(rand_min)[2] <- "RANDDATE"
  dm_ref <- merge(dm_ref, rand_min, by = "SUBJID", all.x = TRUE)
} else dm_ref$RANDDATE <- as.Date(NA)
if (!is.null(dm_min)) {
  names(dm_min)[2] <- "DMDATE"
  dm_ref <- merge(dm_ref, dm_min, by = "SUBJID", all.x = TRUE)
} else dm_ref$DMDATE <- as.Date(NA)
# normalize Date classes
for (cc in c("MINDATE","RANDDATE","DMDATE")) {
  if (cc %in% names(dm_ref)) dm_ref[[cc]] <- as.Date(dm_ref[[cc]], origin = "1970-01-01")
}
# RFSTDTC = coalesce(exposure, randomization, dm) per CDISC reference start
dm_ref$RFSTDTC_DATE <- as.Date(NA, origin = "1970-01-01")
for (i in seq_len(nrow(dm_ref))) {
  v <- NA
  if ("MINDATE" %in% names(dm_ref) && !is.na(dm_ref$MINDATE[i])) v <- dm_ref$MINDATE[i]
  else if ("RANDDATE" %in% names(dm_ref) && !is.na(dm_ref$RANDDATE[i])) v <- dm_ref$RANDDATE[i]
  else if ("DMDATE" %in% names(dm_ref) && !is.na(dm_ref$DMDATE[i])) v <- dm_ref$DMDATE[i]
  dm_ref$RFSTDTC_DATE[i] <- v
}
dm_ref$USUBJID <- paste0("ONCO-2026-", dm_ref$SUBJID)
dm_ref$RFSTDTC <- ifelse(is.na(dm_ref$RFSTDTC_DATE), NA_character_, format(dm_ref$RFSTDTC_DATE, "%Y-%m-%d"))
dm_for_lb <- dm_ref[, c("USUBJID","RFSTDTC"), drop = FALSE]
dm_for_lb <- dm_for_lb[order(dm_for_lb$USUBJID, method = "radix"), , drop = FALSE]
stopifnot("USUBJID" %in% names(dm_for_lb)); stopifnot("RFSTDTC" %in% names(dm_for_lb))

# ---- 2. Prepare Raw Data with USUBJID ----
c_SUBJID <- to_char_trim(get_col(edc_lb, "SUBJID"))
c_TESTCD <- to_char_trim(get_col(edc_lb, "TESTCD"))
c_CAT    <- to_char_trim(get_col(edc_lb, "CAT"))
c_ORRES  <- to_char_trim(get_col(edc_lb, "ORRES"))
c_UNIT   <- to_char_trim(get_col(edc_lb, "UNIT"))
c_LOW    <- to_char_trim(get_col(edc_lb, "LOW"))
c_HIGH   <- to_char_trim(get_col(edc_lb, "HIGH"))
c_VISIT  <- to_char_trim(get_col(edc_lb, "VISIT"))
raw_LBDT_col <- get_col(edc_lb, "LBDT")
if (is.null(raw_LBDT_col)) {
  # fallback: try LBDTC / DATE
  raw_LBDT_col <- get_col(edc_lb, "LBDTC")
  if (is.null(raw_LBDT_col)) raw_LBDT_col <- get_col(edc_lb, "DATE")
  if (is.null(raw_LBDT_col)) raw_LBDT_col <- rep(NA_character_, nrow(edc_lb))
}
n_lb <- nrow(edc_lb)
lb_prep <- data.frame(
  SUBJID = if (is.null(c_SUBJID)) rep(NA_character_, n_lb) else c_SUBJID,
  TESTCD = if (is.null(c_TESTCD)) rep(NA_character_, n_lb) else c_TESTCD,
  CAT    = if (is.null(c_CAT)) rep(NA_character_, n_lb) else c_CAT,
  ORRES  = if (is.null(c_ORRES)) rep(NA_character_, n_lb) else c_ORRES,
  UNIT   = if (is.null(c_UNIT)) rep(NA_character_, n_lb) else c_UNIT,
  LOW    = if (is.null(c_LOW)) rep(NA_character_, n_lb) else c_LOW,
  HIGH   = if (is.null(c_HIGH)) rep(NA_character_, n_lb) else c_HIGH,
  VISIT  = if (is.null(c_VISIT)) rep(NA_character_, n_lb) else c_VISIT,
  stringsAsFactors = FALSE
)
lb_prep$LBDT_raw <- raw_LBDT_col
# USUBJID = catx("-", "ONCO-2026", SUBJID)
lb_prep$USUBJID <- sapply(lb_prep$SUBJID, function(s) {
  if (is.na(s) || trimws(s) == "") "ONCO-2026" else paste0("ONCO-2026-", trimws(s))
}, USE.NAMES = FALSE)
lb_prep$USUBJID <- sas_substr(lb_prep$USUBJID, 25)
# sort by USUBJID (SAS character ordering)
lb_prep <- lb_prep[order(lb_prep$USUBJID, method = "radix"), , drop = FALSE]

# ---- 3. Map LB Domain (merge lb_prep(in=l) dm_for_lb by USUBJID; if l) ----
# drop/rename overlapping except keys (dm only has USUBJID,RFSTDTC)
m <- merge(lb_prep, dm_for_lb, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("RFSTDTC" %in% names(m))
stopifnot("USUBJID" %in% names(m))
# lengths
STUDYID <- rep(sas_substr("ONCO-2026", 10), nrow(m))
DOMAIN <- rep("LB", nrow(m))
LBTESTCD <- sas_substr(m$TESTCD, 8)
LBCAT <- sas_substr(m$CAT, 15)
LBTEST <- rep(NA_character_, nrow(m))
LBTEST[m$LBTESTCD == "HGB" & !is.na(m$LBTESTCD)] <- "Hemoglobin"
LBTEST[m$LBTESTCD == "ALT" & !is.na(m$LBTESTCD)] <- "Alanine Aminotransferase"
LBTEST <- sas_substr(LBTEST, 40)
LBORRES <- sas_substr(m$ORRES, 10)
LBORRESU <- sas_substr(m$UNIT, 20)
LBSTRESC <- sas_substr(m$ORRES, 10)
LBSTRESN <- sas_input_best(m$ORRES)
LBSTRESU <- sas_substr(m$UNIT, 20)
LBORNRLO <- sas_substr(m$LOW, 10)
LBORNRHI <- sas_substr(m$HIGH, 10)
VISIT <- sas_substr(sas_tranwrd(m$VISIT, "_", " "), 20)
LOWN <- sas_input_best(m$LOW)
HIGHN <- sas_input_best(m$HIGH)
LBNRIND <- rep(NA_character_, nrow(m))
for (i in seq_len(nrow(m))) {
  v <- LBSTRESN[i]; lo <- LOWN[i]; hi <- HIGHN[i]
  if (is.na(v)) next
  # SAS missing ordering: missing < any non-missing
  cond_low <- FALSE
  if (!is.na(lo)) cond_low <- v < lo
  else cond_low <- FALSE  # v < . is FALSE
  if (cond_low) { LBNRIND[i] <- "LOW"; next }
  cond_high <- FALSE
  if (!is.na(hi)) cond_high <- v > hi
  else cond_high <- TRUE  # v > . is TRUE when v non-missing
  if (cond_high) { LBNRIND[i] <- "HIGH"; next }
  LBNRIND[i] <- "NORMAL"
}
LBNRIND <- sas_substr(LBNRIND, 8)
# Date conversion: if LBDT ne "" then LBDTC = put(input(LBDT,date11.), yymmdd10.)
lbdt_dates <- parse_lbdt_single(m$LBDT_raw)
LBDTC <- ifelse(is.na(lbdt_dates), NA_character_, format(lbdt_dates, "%Y-%m-%d"))
# blank for missing to emulate SAS "" (keep NA for now, convert later for sort/display)
# Study Day
rf_dates <- parse_iso_single(m$RFSTDTC)
stdt <- parse_iso_single(LBDTC)
LBSTDY <- rep(NA_real_, nrow(m))
for (i in seq_len(nrow(m))) {
  if (is.na(stdt[i]) || is.na(rf_dates[i])) next
  diff <- as.integer(stdt[i] - rf_dates[i])
  if (stdt[i] >= rf_dates[i]) LBSTDY[i] <- diff + 1 else LBSTDY[i] <- diff
}
sdtm_lb1 <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = sas_substr(m$USUBJID, 25),
  LBTESTCD = LBTESTCD, LBTEST = LBTEST, LBCAT = LBCAT,
  LBORRES = LBORRES, LBORRESU = LBORRESU,
  LBSTRESC = LBSTRESC, LBSTRESN = LBSTRESN, LBSTRESU = LBSTRESU,
  LBORNRLO = LBORNRLO, LBORNRHI = LBORNRHI, LBNRIND = LBNRIND,
  LBDTC = LBDTC, LBSTDY = LBSTDY, VISIT = VISIT,
  stringsAsFactors = FALSE
)
# sort by USUBJID LBDTC (missing first: "" sorts first; NA -> "")
sdtm_lb1$._sort_dtc <- ifelse(is.na(sdtm_lb1$LBDTC), "", sdtm_lb1$LBDTC)
sdtm_lb1$._ord <- seq_len(nrow(sdtm_lb1))
sdtm_lb1 <- sdtm_lb1[order(sdtm_lb1$USUBJID, sdtm_lb1$._sort_dtc, sdtm_lb1$._ord, method = "radix"), , drop = FALSE]
sdtm_lb1$._sort_dtc <- NULL; sdtm_lb1$._ord <- NULL

# ---- 4. Derive LBSEQ (by USUBJID) ----
sdtm_lb1 <- sdtm_lb1[order(sdtm_lb1$USUBJID, ifelse(is.na(sdtm_lb1$LBDTC),"",sdtm_lb1$LBDTC), method = "radix"), , drop = FALSE]
LBSEQ <- rep(NA_real_, nrow(sdtm_lb1))
cur <- NA_character_; seqn <- 0
for (i in seq_len(nrow(sdtm_lb1))) {
  if (is.na(cur) || sdtm_lb1$USUBJID[i] != cur) { cur <- sdtm_lb1$USUBJID[i]; seqn <- 1 }
  else seqn <- seqn + 1
  LBSEQ[i] <- seqn
}
sdtm_lb <- data.frame(
  STUDYID = sdtm_lb1$STUDYID, DOMAIN = sdtm_lb1$DOMAIN, USUBJID = sdtm_lb1$USUBJID,
  LBSEQ = LBSEQ, LBTESTCD = sdtm_lb1$LBTESTCD, LBTEST = sdtm_lb1$LBTEST,
  LBCAT = sdtm_lb1$LBCAT, LBORRES = sdtm_lb1$LBORRES, LBORRESU = sdtm_lb1$LBORRESU,
  LBSTRESC = sdtm_lb1$LBSTRESC, LBSTRESN = sdtm_lb1$LBSTRESN, LBSTRESU = sdtm_lb1$LBSTRESU,
  LBORNRLO = sdtm_lb1$LBORNRLO, LBORNRHI = sdtm_lb1$LBORNRHI, LBNRIND = sdtm_lb1$LBNRIND,
  LBDTC = sdtm_lb1$LBDTC, LBSTDY = sdtm_lb1$LBSTDY, VISIT = sdtm_lb1$VISIT,
  stringsAsFactors = FALSE
)
# keep order as specified
sdtm_lb <- sdtm_lb[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI",
  "LBNRIND","LBDTC","LBSTDY","VISIT")]

# ---- 5/6. View + Export ----
# write dataset lb.csv (dates ISO, na="")
lb_out <- sdtm_lb
write.csv(lb_out, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# PROC PRINT rendering -> P026.txt and P026.csv
prt <- lb_out
prt$Obs <- seq_len(nrow(prt))
prt <- prt[, c("Obs","STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI",
  "LBNRIND","LBDTC","LBSTDY","VISIT")]
# CSV of table body
write.csv(prt, file.path(out, "P026.csv"), row.names = FALSE, na = "")
# text rendering
fmt_cell <- function(v) { if (is.na(v)) "" else as.character(v) }
hdr <- paste(sprintf("%-5s %-10s %-6s %-15s %-5s %-8s %-25s %-12s %-8s %-10s %-8s %-8s %-10s %-8s %-8s %-8s %-10s %-6s %-12s",
  "Obs","STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI","LBNRIND","LBDTC","LBSTDY","VISIT"))
lines <- c("PROC PRINT DATA=SDTM_LB;", "", hdr, paste(rep("-", nchar(hdr)), collapse=""))
for (i in seq_len(nrow(prt))) {
  r <- prt[i,]
  lines <- c(lines, sprintf("%-5s %-10s %-6s %-15s %-5s %-8s %-25s %-12s %-8s %-10s %-8s %-8s %-10s %-8s %-8s %-8s %-10s %-6s %-12s",
    fmt_cell(r$Obs), fmt_cell(r$STUDYID), fmt_cell(r$DOMAIN), fmt_cell(r$USUBJID),
    fmt_cell(r$LBSEQ), fmt_cell(r$LBTESTCD), fmt_cell(r$LBTEST), fmt_cell(r$LBCAT),
    fmt_cell(r$LBORRES), fmt_cell(r$LBORRESU), fmt_cell(r$LBSTRESC), fmt_cell(r$LBSTRESN),
    fmt_cell(r$LBSTRESU), fmt_cell(r$LBORNRLO), fmt_cell(r$LBORNRHI), fmt_cell(r$LBNRIND),
    fmt_cell(r$LBDTC), fmt_cell(r$LBSTDY), fmt_cell(r$VISIT)))
}
writeLines(lines, file.path(out, "P026.txt"))
# alias with .lst extension for benchmark
writeLines(lines, file.path(out, "P026.lst"))
message("Done")
