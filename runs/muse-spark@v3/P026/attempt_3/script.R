out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
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
  v <- trimws(as.character(x))
  v[v == "" | is.na(v)] <- NA_character_
  suppressWarnings(as.numeric(v))
}
title_mon <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || v == "") return(v)
    m <- regmatches(v, regexec("^([0-9]{1,2})-([A-Za-z]{3})-(.*)$", v))[[1]]
    if (length(m) == 4) {
      mon <- paste0(toupper(substr(m[3],1,1)), tolower(substr(m[3],2,3)))
      paste0(m[2], "-", mon, "-", m[4])
    } else { v }
  }, USE.NAMES = FALSE)
}
parse_lbdt_single <- function(x) {
  n <- length(x)
  res <- rep(as.Date(NA), n)
  for (i in seq_len(n)) {
    v <- x[i]
    if (length(v) == 0) next
    if (is.na(v)) next
    if (inherits(v, "Date")) { res[i] <- v; next }
    if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) { res[i] <- as.Date(v); next }
    if (is.numeric(v)) {
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
    if (length(v) == 0) next
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
parse_char_col_single_format <- function(vec) {
  n <- length(vec)
  res <- rep(as.Date(NA), n)
  vals <- trimws(as.character(vec))
  vals_nonmiss <- vals[!is.na(vals) & vals != ""]
  if (length(vals_nonmiss) == 0) return(res)
  if (all(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", vals_nonmiss))) {
    for (i in seq_len(n)) {
      s <- trimws(as.character(vec[i]))
      if (is.na(s) || s == "") next
      res[i] <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
    }
    return(res)
  }
  if (all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", vals_nonmiss))) {
    s2 <- title_mon(trimws(as.character(vec)))
    for (i in seq_len(n)) {
      if (is.na(s2[i]) || s2[i] == "") next
      yr <- sub("^[0-9]{1,2}-[A-Za-z]{3}-", "", s2[i])
      fmt <- if (nchar(yr) == 2) "%d-%b-%y" else "%d-%b-%Y"
      res[i] <- suppressWarnings(as.Date(s2[i], format = fmt))
    }
    return(res)
  }
  slash_idx <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", vals_nonmiss)
  if (any(slash_idx)) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", vals_nonmiss[slash_idx])))
    use_dmy <- any(!is.na(first_fields) & first_fields > 12)
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
    d <- rep(as.Date(NA), length(col))
    idx <- !is.na(col) & col > 20000 & col < 60000
    d[idx] <- as.Date(col[idx], origin = "1899-12-30")
    return(d)
  }
  parse_char_col_single_format(as.character(col))
}
find_best_date_col <- function(df, exclude = NULL) {
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
min_date_by_subj <- function(subj_vec, date_vec) {
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

# ---- 1. Prepare DM Reference Data (sdtm_dm RFSTDTC) ----
# Standard: RFSTDTC = randomization/first-exposure reference start.
# Prefer explicit RFSTDTC in edc_dm if present, else randomization, else first exposure.
ex_min <- NULL; rand_min <- NULL; dm_rf_direct <- NULL
if (!is.null(edc_ex_raw)) {
  sc <- get_col(edc_ex_raw, "SUBJID")
  if (!is.null(sc)) {
    subj_c <- trimws(as.character(sc)); subj_c[is.na(subj_c)] <- ""
    pref <- NULL
    for (pn in c("EXSTDTC","EXSTDT","EXSTDAT","EXSTART","EXDAT","EXDT","EXSTD")) {
      cn <- get_col_name(edc_ex_raw, pn)
      if (!is.null(cn)) { pref <- cn; break }
    }
    # prefer columns with STD/START in name among date-like columns
    if (is.null(pref)) {
      # search for start-like column names
      for (nm in names(edc_ex_raw)) {
        up <- toupper(nm)
        if (grepl("STD|START", up)) {
          d_try <- col_to_date(edc_ex_raw[[nm]])
          if (!is.null(d_try) && mean(!is.na(d_try)) > 0.3) { pref <- nm; break }
        }
      }
    }
    if (!is.null(pref)) { d <- col_to_date(edc_ex_raw[[pref]]) } else { fb <- find_best_date_col(edc_ex_raw); d <- fb$dates }
    if (!is.null(d)) {
      ex_min <- min_date_by_subj(subj_c, as.Date(d, origin="1970-01-01"))
    }
  }
}
if (!is.null(rand_raw)) {
  sc <- get_col(rand_raw, "SUBJID")
  if (is.null(sc)) {
    for (alt in c("SUBJECT","SUBJ","USUBJID","PATIENT")) {
      sc <- get_col(rand_raw, alt)
      if (!is.null(sc)) break
    }
  }
  if (!is.null(sc)) {
    subj_c <- trimws(as.character(sc)); subj_c[is.na(subj_c)] <- ""
    subj_c <- sub("^ONCO-2026-", "", subj_c)
    pref <- NULL
    for (pn in c("RANDDTC","RANDDT","RANDDAT","RANDOMDT","RAND_DATE","RANDONDT","RANDDTM")) {
      cn <- get_col_name(rand_raw, pn)
      if (!is.null(cn)) { pref <- cn; break }
    }
    if (is.null(pref)) {
      for (nm in names(rand_raw)) {
        up <- toupper(nm)
        if (grepl("RAND", up)) {
          d_try <- col_to_date(rand_raw[[nm]])
          if (!is.null(d_try) && mean(!is.na(d_try)) > 0.3) { pref <- nm; break }
        }
      }
    }
    if (!is.null(pref)) { d <- col_to_date(rand_raw[[pref]]) } else { fb <- find_best_date_col(rand_raw); d <- fb$dates }
    if (!is.null(d)) {
      rand_min <- min_date_by_subj(subj_c, as.Date(d, origin="1970-01-01"))
      if (!is.null(rand_min) && nrow(rand_min) > 0) names(rand_min)[2] <- "MINDATE"
    }
  }
}
if (!is.null(edc_dm_raw)) {
  rf_cn <- NULL
  for (pn in c("RFSTDTC","RFSTDT","RFSTDAT","RFSTART")) {
    cn <- get_col_name(edc_dm_raw, pn)
    if (!is.null(cn)) { rf_cn <- cn; break }
  }
  if (!is.null(rf_cn)) {
    sc <- get_col(edc_dm_raw, "SUBJID")
    if (!is.null(sc)) {
      subj_c <- trimws(as.character(sc)); subj_c[is.na(subj_c)] <- ""
      subj_c <- sub("^ONCO-2026-", "", subj_c)
      d <- col_to_date(edc_dm_raw[[rf_cn]])
      if (!is.null(d)) {
        dm_rf_direct <- min_date_by_subj(subj_c, as.Date(d, origin="1970-01-01"))
        if (!is.null(dm_rf_direct) && nrow(dm_rf_direct) > 0) names(dm_rf_direct)[2] <- "MINDATE"
      }
    }
  }
}
# Build reference: priority dm_rf_direct > rand_min > ex_min
lb_subj_raw <- get_col(edc_lb, "SUBJID")
lb_subj_c <- trimws(as.character(lb_subj_raw)); lb_subj_c[is.na(lb_subj_c)] <- ""
all_subj <- unique(c(if (!is.null(dm_rf_direct)) dm_rf_direct$SUBJID,
                     if (!is.null(rand_min)) rand_min$SUBJID,
                     if (!is.null(ex_min)) ex_min$SUBJID,
                     lb_subj_c[lb_subj_c != ""]))
dm_ref <- data.frame(SUBJID = all_subj, stringsAsFactors = FALSE)
dm_ref$RFDATE <- as.Date(NA, origin="1970-01-01")
# helper to fill coalesce in priority order
fill_from <- function(ref, src) {
  if (is.null(src) || nrow(src) == 0) return(ref)
  m2 <- match(ref$SUBJID, src$SUBJID)
  srcd <- src$MINDATE[m2]
  need <- is.na(ref$RFDATE) & !is.na(srcd)
  ref$RFDATE[need] <- srcd[need]
  ref
}
# priority: direct RF first, then randomization, then exposure
dm_ref <- fill_from(dm_ref, dm_rf_direct)
dm_ref <- fill_from(dm_ref, rand_min)
dm_ref <- fill_from(dm_ref, ex_min)
dm_ref$USUBJID <- paste0("ONCO-2026-", dm_ref$SUBJID)
dm_ref$RFSTDTC <- ifelse(is.na(dm_ref$RFDATE), NA_character_, format(dm_ref$RFDATE, "%Y-%m-%d"))
dm_for_lb <- dm_ref[, c("USUBJID","RFSTDTC"), drop = FALSE]
dm_for_lb <- dm_for_lb[order(dm_for_lb$USUBJID, method = "radix"), , drop = FALSE]
stopifnot("USUBJID" %in% names(dm_for_lb)); stopifnot("RFSTDTC" %in% names(dm_for_lb))

c_SUBJID <- to_char_trim(get_col(edc_lb, "SUBJID"))
# Robust TESTCD source: prefer TESTCD, else TEST, LBTESTCD, etc. or column containing HGB/ALT
test_src_col <- get_col(edc_lb, "TESTCD")
test_src_name <- get_col_name(edc_lb, "TESTCD")
if (is.null(test_src_col)) {
  for (alt in c("LBTESTCD","TEST","LBTEST","PARAMCD","PARAM")) {
    test_src_col <- get_col(edc_lb, alt)
    if (!is.null(test_src_col)) { test_src_name <- get_col_name(edc_lb, alt); break }
  }
}
if (is.null(test_src_col)) {
  # search any column with HGB/ALT values
  for (nm in names(edc_lb)) {
    v <- trimws(as.character(edc_lb[[nm]]))
    if (any(toupper(v) %in% c("HGB","ALT"), na.rm = TRUE)) { test_src_col <- edc_lb[[nm]]; test_src_name <- nm; break }
  }
}
if (is.null(test_src_col)) test_src_col <- rep(NA_character_, nrow(edc_lb))
c_TESTCD <- to_char_trim(test_src_col)
c_CAT    <- to_char_trim(get_col(edc_lb, "CAT"))
if (is.null(c_CAT) || length(c_CAT) == 0) {
  altcat <- NULL
  for (alt in c("LBCAT","CATEGORY","CAT2")) { altcat <- get_col(edc_lb, alt); if (!is.null(altcat)) break }
  if (!is.null(altcat)) c_CAT <- to_char_trim(altcat) else c_CAT <- rep(NA_character_, nrow(edc_lb))
}
c_ORRES  <- to_char_trim(get_col(edc_lb, "ORRES"))
if (is.null(c_ORRES) || length(c_ORRES)==0) {
  for (alt in c("LBORRES","RESULT","VALUE","RES")) { tmp <- get_col(edc_lb, alt); if (!is.null(tmp)) { c_ORRES <- to_char_trim(tmp); break } }
}
c_UNIT   <- to_char_trim(get_col(edc_lb, "UNIT"))
if (is.null(c_UNIT) || length(c_UNIT)==0) {
  for (alt in c("LBORRESU","UNITS","ORRESU")) { tmp <- get_col(edc_lb, alt); if (!is.null(tmp)) { c_UNIT <- to_char_trim(tmp); break } }
}
c_LOW    <- to_char_trim(get_col(edc_lb, "LOW"))
if (is.null(c_LOW) || length(c_LOW)==0) {
  for (alt in c("LBORNRLO","LOWER","NRLO","REFLOW")) { tmp <- get_col(edc_lb, alt); if (!is.null(tmp)) { c_LOW <- to_char_trim(tmp); break } }
}
c_HIGH   <- to_char_trim(get_col(edc_lb, "HIGH"))
if (is.null(c_HIGH) || length(c_HIGH)==0) {
  for (alt in c("LBORNRHI","UPPER","NRHI","REFHIGH")) { tmp <- get_col(edc_lb, alt); if (!is.null(tmp)) { c_HIGH <- to_char_trim(tmp); break } }
}
c_VISIT  <- to_char_trim(get_col(edc_lb, "VISIT"))
raw_LBDT_col <- get_col(edc_lb, "LBDT")
if (is.null(raw_LBDT_col)) {
  raw_LBDT_col <- get_col(edc_lb, "LBDTC")
  if (is.null(raw_LBDT_col)) raw_LBDT_col <- get_col(edc_lb, "DATE")
  if (is.null(raw_LBDT_col)) raw_LBDT_col <- get_col(edc_lb, "LBDAT")
  if (is.null(raw_LBDT_col)) raw_LBDT_col <- rep(NA_character_, nrow(edc_lb))
}
n_lb <- nrow(edc_lb)
lb_prep <- data.frame(
  SUBJID = if (is.null(c_SUBJID) || length(c_SUBJID)==0) rep(NA_character_, n_lb) else c_SUBJID,
  TESTCD = if (is.null(c_TESTCD) || length(c_TESTCD)==0) rep(NA_character_, n_lb) else c_TESTCD,
  CAT    = if (is.null(c_CAT) || length(c_CAT)==0) rep(NA_character_, n_lb) else c_CAT,
  ORRES  = if (is.null(c_ORRES) || length(c_ORRES)==0) rep(NA_character_, n_lb) else c_ORRES,
  UNIT   = if (is.null(c_UNIT) || length(c_UNIT)==0) rep(NA_character_, n_lb) else c_UNIT,
  LOW    = if (is.null(c_LOW) || length(c_LOW)==0) rep(NA_character_, n_lb) else c_LOW,
  HIGH   = if (is.null(c_HIGH) || length(c_HIGH)==0) rep(NA_character_, n_lb) else c_HIGH,
  VISIT  = if (is.null(c_VISIT) || length(c_VISIT)==0) rep(NA_character_, n_lb) else c_VISIT,
  stringsAsFactors = FALSE
)
lb_prep$LBDT_raw <- raw_LBDT_col
lb_prep$USUBJID <- sapply(lb_prep$SUBJID, function(s) {
  if (is.na(s) || trimws(s) == "") "ONCO-2026" else paste0("ONCO-2026-", trimws(s))
}, USE.NAMES = FALSE)
lb_prep$USUBJID <- sas_substr(lb_prep$USUBJID, 25)
lb_prep <- lb_prep[order(lb_prep$USUBJID, method = "radix"), , drop = FALSE]

m <- merge(lb_prep, dm_for_lb, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("RFSTDTC" %in% names(m))
stopifnot("USUBJID" %in% names(m))
STUDYID <- rep(sas_substr("ONCO-2026", 10), nrow(m))
DOMAIN <- rep("LB", nrow(m))
LBTESTCD <- sas_substr(m$TESTCD, 8)
LBCAT <- sas_substr(m$CAT, 15)
# Map Test Names from LBTESTCD vector (SAS: if LBTESTCD="HGB" ... else if "ALT")
LBTEST <- rep(NA_character_, nrow(m))
idx_hgb <- !is.na(LBTESTCD) & LBTESTCD == "HGB"
idx_alt <- !is.na(LBTESTCD) & LBTESTCD == "ALT"
LBTEST[idx_hgb] <- "Hemoglobin"
LBTEST[idx_alt] <- "Alanine Aminotransferase"
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
  cond_low <- FALSE
  if (!is.na(lo)) { cond_low <- v < lo } else { cond_low <- FALSE }
  if (cond_low) { LBNRIND[i] <- "LOW"; next }
  cond_high <- FALSE
  if (!is.na(hi)) { cond_high <- v > hi } else { cond_high <- TRUE }
  if (cond_high) { LBNRIND[i] <- "HIGH"; next }
  LBNRIND[i] <- "NORMAL"
}
LBNRIND <- sas_substr(LBNRIND, 8)
lbdt_dates <- parse_lbdt_single(m$LBDT_raw)
LBDTC <- ifelse(is.na(lbdt_dates), NA_character_, format(lbdt_dates, "%Y-%m-%d"))
rf_dates <- parse_iso_single(m$RFSTDTC)
stdt <- parse_iso_single(LBDTC)
LBSTDY <- rep(NA_real_, nrow(m))
for (i in seq_len(nrow(m))) {
  if (is.na(stdt[i]) || is.na(rf_dates[i])) next
  diff <- as.integer(stdt[i] - rf_dates[i])
  if (stdt[i] >= rf_dates[i]) { LBSTDY[i] <- diff + 1 } else { LBSTDY[i] <- diff }
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
sdtm_lb1$._sort_dtc <- ifelse(is.na(sdtm_lb1$LBDTC), "", sdtm_lb1$LBDTC)
sdtm_lb1$._ord <- seq_len(nrow(sdtm_lb1))
sdtm_lb1 <- sdtm_lb1[order(sdtm_lb1$USUBJID, sdtm_lb1$._sort_dtc, sdtm_lb1$._ord, method = "radix"), , drop = FALSE]
sdtm_lb1$._sort_dtc <- NULL; sdtm_lb1$._ord <- NULL

sdtm_lb1 <- sdtm_lb1[order(sdtm_lb1$USUBJID, ifelse(is.na(sdtm_lb1$LBDTC),"",sdtm_lb1$LBDTC), method = "radix"), , drop = FALSE]
LBSEQ <- rep(NA_real_, nrow(sdtm_lb1))
cur <- NA_character_; seqn <- 0
for (i in seq_len(nrow(sdtm_lb1))) {
  if (is.na(cur) || sdtm_lb1$USUBJID[i] != cur) { cur <- sdtm_lb1$USUBJID[i]; seqn <- 1 } else { seqn <- seqn + 1 }
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
sdtm_lb <- sdtm_lb[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI",
  "LBNRIND","LBDTC","LBSTDY","VISIT")]

lb_out <- sdtm_lb
write.csv(lb_out, file.path(out, "lb.csv"), row.names = FALSE, na = "")

prt <- lb_out
prt$Obs <- seq_len(nrow(prt))
prt <- prt[, c("Obs","STUDYID","DOMAIN","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT",
  "LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBORNRLO","LBORNRHI",
  "LBNRIND","LBDTC","LBSTDY","VISIT")]
write.csv(prt, file.path(out, "P026.csv"), row.names = FALSE, na = "")
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
writeLines(lines, file.path(out, "P026.lst"))
message("Done")
