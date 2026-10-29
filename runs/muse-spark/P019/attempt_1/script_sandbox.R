out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
raw_dir  <- file.path(project_path, "data/raw")
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
})

cat("P019 QC core translation starting\n")

# ---------- helpers ----------
sas_strip <- function(x) {
  # SAS STRIP: remove leading/trailing blanks; NA -> ""
  if (is.null(x)) return(character(0))
  xc <- as.character(x)
  res <- trimws(xc, which = "both")
  res[is.na(xc)] <- ""
  # trimws(NA) is NA, already handled; also NaN?
  res[is.na(res)] <- ""
  res
}
sas_up_strip <- function(x) toupper(sas_strip(x))

is_miss <- function(x) {
  if (is.null(x)) return(logical(0))
  if (inherits(x, "Date") || inherits(x, "POSIXt") || is.numeric(x) || is.integer(x) || is.logical(x)) {
    return(is.na(x))
  } else {
    xc <- as.character(x)
    return(is.na(xc) | trimws(xc) == "")
  }
}
is_not_miss <- function(x) !is_miss(x)

valid_iso_date <- function(x) {
  # prxmatch('/^\\d{4}-\\d{2}-\\d{2}$/', strip(x)) != 0  and not missing
  xc <- as.character(x)
  out <- rep(FALSE, length(xc))
  idx <- !is.na(xc) & trimws(xc) != ""
  if (any(idx)) {
    s <- trimws(xc[idx])
    out[idx] <- grepl("^\\d{4}-\\d{2}-\\d{2}$", s)
  }
  # NA -> FALSE (SAS missing gives false for =0 comparison except missing() branch)
  out[is.na(out)] <- FALSE
  out
}

parse_yymmdd10 <- function(x) {
  # input(substr(RFPENDTC,1,10), yymmdd10.)
  xc <- as.character(x)
  res <- rep(as.Date(NA), length(xc))
  idx <- !is.na(xc) & trimws(xc) != ""
  if (any(idx)) {
    s <- substr(trimws(xc[idx]), 1, 10)
    suppressWarnings(res[idx] <- as.Date(s, format = "%Y-%m-%d"))
  }
  res
}

completed_years <- function(brth, rfic) {
  # floor(yrdif(brth, rfic, 'AGE')) = completed years
  n <- length(brth)
  res <- rep(NA_integer_, n)
  br <- as.Date(brth); rf <- as.Date(rfic)
  ok <- !is.na(br) & !is.na(rf)
  if (any(ok)) {
    yb <- as.integer(format(br[ok], "%Y")); mb <- as.integer(format(br[ok], "%m")); db <- as.integer(format(br[ok], "%d"))
    yr <- as.integer(format(rf[ok], "%Y")); mr <- as.integer(format(rf[ok], "%m")); dr <- as.integer(format(rf[ok], "%d"))
    age <- yr - yb - ((mr < mb) | ((mr == mb) & (dr < db)))
    res[ok] <- age
  }
  res
}

scan_first_dash <- function(x) {
  # scan(strip(SUBJID),1,'-')
  xc <- sas_strip(x)
  vapply(xc, function(s) {
    if (s == "") return("")
    parts <- strsplit(s, "-", fixed = TRUE)[[1]]
    trimws(parts[1])
  }, character(1), USE.NAMES = FALSE)
}

get_col <- function(df, nm) {
  if (is.null(df) || nrow(df) == 0 && ncol(df) == 0) return(NULL)
  cn <- names(df)
  idx <- which(toupper(cn) == toupper(nm))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}

clean_df <- function(df) {
  if (is.null(df)) return(NULL)
  # zap haven labels/formats if present
  if (inherits(df, "data.frame")) {
    # convert haven_labelled to plain
    for (j in seq_along(df)) {
      v <- df[[j]]
      if (inherits(v, "haven_labelled") || inherits(v, "haven_labelled_spss")) {
        # zap labels
        v <- haven::zap_labels(v)
        df[[j]] <- v
      }
      if (is.factor(df[[j]])) df[[j]] <- as.character(df[[j]])
      if (inherits(df[[j]], "POSIXct") || inherits(df[[j]], "POSIXlt")) {
        # keep as Date if time midnight? keep POSIX -> Date for day comparisons
        # convert to Date
        df[[j]] <- as.Date(df[[j]])
      }
    }
    names(df) <- toupper(names(df))
  }
  df
}

read_lib_mem <- function(libdir, mem) {
  # find file case-insensitive with extensions
  if (!dir.exists(libdir)) return(NULL)
  files <- list.files(libdir, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  # match basename without extension
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(mem))
  if (length(idx) == 0) {
    # try direct paths
    cand <- c(file.path(libdir, paste0(mem, ".sas7bdat")),
              file.path(libdir, paste0(tolower(mem), ".sas7bdat")),
              file.path(libdir, paste0(toupper(mem), ".sas7bdat")),
              file.path(libdir, paste0(mem, ".xpt")),
              file.path(libdir, paste0(mem, ".csv")))
    cand <- cand[file.exists(cand)]
    if (length(cand) == 0) return(NULL)
    f <- cand[1]
  } else {
    f <- files[idx[1]]
  }
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "sas7bdat" || ext == "sas7dat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
    df <- as.data.frame(df)
  } else if (ext == "parquet") {
    df <- as.data.frame(arrow::read_parquet(f))
  } else {
    # try sas then xpt
    try({ df <- haven::read_sas(f) }, silent = TRUE)
    if (is.null(df)) try({ df <- haven::read_xpt(f) }, silent = TRUE)
  }
  clean_df(as.data.frame(df))
}

# ---------- load ----------
adsl  <- read_lib_mem(adam_dir, "adsl")
adae  <- read_lib_mem(adam_dir, "adae")
advs  <- read_lib_mem(adam_dir, "advs")
adlb  <- read_lib_mem(adam_dir, "adlb")
adtte <- read_lib_mem(adam_dir, "adtte")
dm    <- read_lib_mem(sdtm_dir, "dm")
ae    <- read_lib_mem(sdtm_dir, "ae")
ex    <- read_lib_mem(sdtm_dir, "ex")
lb    <- read_lib_mem(sdtm_dir, "lb")
vs    <- read_lib_mem(sdtm_dir, "vs")
eg    <- read_lib_mem(sdtm_dir, "eg")
mh    <- read_lib_mem(sdtm_dir, "mh")
sv    <- read_lib_mem(sdtm_dir, "sv")
ds    <- read_lib_mem(sdtm_dir, "ds")
cm    <- read_lib_mem(sdtm_dir, "cm")

cat("datasets loaded\n")
# helper to safely get nrow
nrow0 <- function(d) if (is.null(d)) 0 else nrow(d)

# ---------- RULE 1 ADSL-001 ----------
n_adsl01 <- 0
if (!is.null(adsl) && "USUBJID" %in% names(adsl)) {
  u <- sas_strip(get_col(adsl, "USUBJID"))
  # SAS group by: missing groups together; count>1
  # use table; NA/"" treated as "" already
  tb <- table(u, useNA = "no")
  n_adsl01 <- sum(tb > 1)
}

# ---------- RULE 2 ADAE-001 ----------
n_adae01 <- 0
if (!is.null(adae)) {
  aendt <- get_col(adae, "AEENDT"); aestdt <- get_col(adae, "AESTDT")
  if (!is.null(aendt) && !is.null(aestdt)) {
    ae_end <- as.Date(aendt); ae_st <- as.Date(aestdt)
    # haven may already be Date; as.Date on Date is identity
    cond <- !is.na(ae_end) & !is.na(ae_st) & (ae_end < ae_st)
    cond[is.na(cond)] <- FALSE
    n_adae01 <- sum(cond)
  }
}

# ---------- RULE 3 ADVS-001 ----------
n_advs01 <- 0
if (!is.null(advs)) {
  abl <- get_col(advs, "ABLFL"); us <- sas_strip(get_col(advs, "USUBJID")); pc <- sas_strip(get_col(advs, "PARAMCD"))
  if (!is.null(abl)) {
    abl_s <- sas_strip(abl)
    idx <- abl_s == "Y"
    idx[is.na(idx)] <- FALSE
    if (any(idx)) {
      key <- paste0(us[idx], "\x01", pc[idx])
      tb <- table(key, useNA = "no")
      n_advs01 <- sum(tb > 1)
    }
  }
}

# ---------- RULE 4 ADLB-001 ----------
n_adlb01 <- 0
if (!is.null(adlb)) {
  abl <- get_col(adlb, "ABLFL"); us <- sas_strip(get_col(adlb, "USUBJID")); pc <- sas_strip(get_col(adlb, "PARAMCD"))
  if (!is.null(abl)) {
    abl_s <- sas_strip(abl)
    idx <- abl_s == "Y"
    idx[is.na(idx)] <- FALSE
    if (any(idx)) {
      key <- paste0(us[idx], "\x01", pc[idx])
      tb <- table(key, useNA = "no")
      n_adlb01 <- sum(tb > 1)
    }
  }
}

# ---------- RULE 5 DM-001 ----------
n_dm01 <- 0
if (!is.null(dm) && "USUBJID" %in% names(dm)) {
  u <- sas_strip(get_col(dm, "USUBJID"))
  tb <- table(u, useNA = "no")
  n_dm01 <- sum(tb > 1)
}

# helper referential integrity: count rows in child with no DM match
ref_fail <- function(child, dm_df) {
  if (is.null(child) || is.null(dm_df)) return(0L)
  cu <- sas_strip(get_col(child, "USUBJID"))
  du <- sas_strip(get_col(dm_df, "USUBJID"))
  if (is.null(cu) || is.null(du)) return(0L)
  dset <- unique(du)
  sum(!(cu %in% dset))
}
n_ae01 <- ref_fail(ae, dm)
n_ex01 <- ref_fail(ex, dm)
n_lb01 <- ref_fail(lb, dm)
n_vs01 <- ref_fail(vs, dm)
n_eg01 <- ref_fail(eg, dm)
n_mh01 <- ref_fail(mh, dm)
n_sv01 <- ref_fail(sv, dm)

# ---------- RULE 10 ADSL-002 ----------
n_adsl02 <- 0
if (!is.null(adsl) && !is.null(ex)) {
  saff <- sas_strip(get_col(adsl, "SAFFL")); aus <- sas_strip(get_col(adsl, "USUBJID"))
  eus <- unique(sas_strip(get_col(ex, "USUBJID")))
  idx <- saff == "Y"
  idx[is.na(idx)] <- FALSE
  if (any(idx)) n_adsl02 <- sum(!(aus[idx] %in% eus))
} else if (!is.null(adsl)) {
  saff <- sas_strip(get_col(adsl, "SAFFL")); aus <- sas_strip(get_col(adsl, "USUBJID"))
  idx <- saff == "Y"
  n_adsl02 <- sum(idx, na.rm = TRUE)  # no EX => all fail if EX missing? but EX should exist
}

# ---------- RULE 11 ADTTE-001 ----------
n_adtte01 <- 0
if (!is.null(adtte)) {
  aval <- suppressWarnings(as.numeric(get_col(adtte, "AVAL")))
  cond <- is.na(aval) | aval < 1
  cond[is.na(cond)] <- TRUE  # if aval NA already TRUE
  n_adtte01 <- sum(cond)
}

# ---------- ADTTE-002 ----------
n_adtte02 <- 0
if (!is.null(adtte)) {
  trtsdt <- as.Date(get_col(adtte, "TRTSDT"))
  cnsr <- suppressWarnings(as.numeric(get_col(adtte, "CNSR")))
  eventdt <- as.Date(get_col(adtte, "EVENTDT"))
  eosdt <- as.Date(get_col(adtte, "EOSDT"))
  aval <- suppressWarnings(as.numeric(get_col(adtte, "AVAL")))
  n <- nrow(adtte)
  fail <- rep(FALSE, n)
  for (i in seq_len(n)) {
    t <- trtsdt[i]; c <- cnsr[i]; e <- eventdt[i]; s <- eosdt[i]; a <- aval[i]
    miss_t <- is.na(t); miss_c <- is.na(c); miss_e <- is.na(e); miss_s <- is.na(s); miss_a <- is.na(a)
    if (miss_t) { fail[i] <- TRUE; next }
    if (miss_c) { fail[i] <- TRUE; next }
    if (!(c %in% c(0,1))) { fail[i] <- TRUE; next }
    if (c == 0) {
      if (miss_e) { fail[i] <- TRUE; next }
      exp <- as.numeric(e - t) + 1
      if (miss_a || is.na(exp) || a != exp) { fail[i] <- TRUE; next }
    } else if (c == 1) {
      if (!miss_e) { fail[i] <- TRUE; next }
      if (miss_s) { fail[i] <- TRUE; next }
      exp <- as.numeric(s - t) + 1
      if (miss_a || is.na(exp) || a != exp) { fail[i] <- TRUE; next }
    }
  }
  n_adtte02 <- sum(fail)
}

# ---------- ADTTE-003 ----------
n_adtte03 <- 0
if (!is.null(adtte)) {
  trtsdt <- as.Date(get_col(adtte, "TRTSDT"))
  eosdt <- as.Date(get_col(adtte, "EOSDT"))
  eventdt <- as.Date(get_col(adtte, "EVENTDT"))
  n <- nrow(adtte)
  fail <- rep(FALSE, n)
  for (i in seq_len(n)) {
    t <- trtsdt[i]; s <- eosdt[i]; e <- eventdt[i]
    if (is.na(s)) { fail[i] <- TRUE; next }
    # EOSDT < TRTSDT : SAS missing comparison false if t missing
    if (!is.na(t) && !is.na(s) && s < t) { fail[i] <- TRUE; next }
    if (!is.na(e)) {
      cond1 <- (!is.na(t) && !is.na(e) && e < t)
      cond2 <- (!is.na(e) && !is.na(s) && e > s)
      if (isTRUE(cond1) || isTRUE(cond2)) { fail[i] <- TRUE; next }
    }
  }
  n_adtte03 <- sum(fail)
}

# ---------- ADSL-003 ----------
n_adsl03 <- 0
if (!is.null(adsl)) {
  aus <- sas_strip(get_col(adsl, "USUBJID"))
  aeos <- as.Date(get_col(adsl, "EOSDT"))
  if (!is.null(dm)) {
    dus <- sas_strip(get_col(dm, "USUBJID"))
    rfp <- as.character(get_col(dm, "RFPENDTC"))
    # build lookup
    lookup <- data.frame(US = dus, RFP = rfp, stringsAsFactors = FALSE)
    # if duplicates in DM, take first
    lookup <- lookup[!duplicated(lookup$US), ]
    m <- match(aus, lookup$US)
    parsed <- parse_yymmdd10(lookup$RFP[m])
    # d.USUBJID is null => m is NA => fail
    fail <- is.na(m) | is.na(aeos) | is.na(parsed) | (aeos != parsed)
    fail[is.na(fail)] <- TRUE
    # SAS: a.EOSDT ne input(...) : if parsed missing and aeos non-missing => true (fail); if both missing, missing(a.EOSDT) already true
    n_adsl03 <- sum(fail)
  } else {
    n_adsl03 <- nrow(adsl)
  }
}

# ---------- ADSL-004 ----------
n_adsl04 <- 0
if (!is.null(adsl)) {
  brth <- as.Date(get_col(adsl, "BRTHDT"))
  rfic <- as.Date(get_col(adsl, "RFICDT"))
  age <- suppressWarnings(as.numeric(get_col(adsl, "AGE")))
  exp <- completed_years(brth, rfic)
  fail <- is.na(brth) | is.na(rfic) | is.na(age) | is.na(exp) | (age != exp)
  fail[is.na(fail)] <- TRUE
  n_adsl04 <- sum(fail)
}

# ---------- ADVS-002 / ADLB-002 ----------
baseline_dup <- function(d) {
  if (is.null(d)) return(0L)
  us <- sas_strip(get_col(d, "USUBJID")); pc <- sas_strip(get_col(d, "PARAMCD"))
  astdt <- as.Date(get_col(d, "ASTDT")); trtsdt <- as.Date(get_col(d, "TRTSDT"))
  aval <- suppressWarnings(as.numeric(get_col(d, "AVAL")))
  if (is.null(us) || is.null(pc) || is.null(astdt) || is.null(trtsdt) || is.null(aval)) return(0L)
  n <- length(us)
  # eligible for BASE_DT: ASTDT<=TRTSDT and not missing AVAL, both dates non-missing
  elig <- !is.na(astdt) & !is.na(trtsdt) & !is.na(aval) & (astdt <= trtsdt)
  elig[is.na(elig)] <- FALSE
  if (!any(elig)) return(0L)
  df_e <- data.frame(US=us[elig], PC=pc[elig], DT=astdt[elig], stringsAsFactors=FALSE)
  # max per US,PC
  agg <- aggregate(DT ~ US + PC, data=df_e, FUN=max)
  names(agg)[3] <- "BASE_DT"
  # join back: a.ASTDT = BASE_DT and not missing AVAL
  has_aval <- !is.na(aval) & !is.na(astdt)
  # map base
  key_all <- paste0(us, "\x01", pc)
  key_agg <- paste0(agg$US, "\x01", agg$PC)
  m <- match(key_all, key_agg)
  base_dt <- agg$BASE_DT[m]
  cand <- has_aval & !is.na(m) & !is.na(base_dt) & (astdt == base_dt)
  cand[is.na(cand)] <- FALSE
  if (!any(cand)) return(0L)
  keys_cand <- key_all[cand]
  tb <- table(keys_cand, useNA="no")
  sum(tb > 1)
}
n_advs02 <- baseline_dup(advs)
n_adlb02 <- baseline_dup(adlb)

# ---------- EG-02 / MH-02 / EG-04 / MH-04 dictionary ----------
req_eg <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST','EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
req_mh <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
raw_eg <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
raw_mh <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')

dict_count_present <- function(df, lst) {
  if (is.null(df)) return(0L)
  up <- toupper(names(df))
  sum(lst %in% up)
}
n_eg02 <- 12 - dict_count_present(eg, req_eg)
n_mh02 <- 6 - dict_count_present(mh, req_mh)
n_eg04 <- dict_count_present(eg, raw_eg)  # count(*) where name in list
# careful: count(*) counts rows matching; distinct not needed since names unique; same
n_mh04 <- dict_count_present(mh, raw_mh)

# ---------- EG-03 ----------
n_eg03 <- 0
if (!is.null(eg)) {
  egdtc <- get_col(eg, "EGDTC"); egstresn <- suppressWarnings(as.numeric(get_col(eg, "EGSTRESN"))); egstresc <- get_col(eg, "EGSTRESC")
  miss_dtc <- is_miss(egdtc)
  bad_dtc <- !valid_iso_date(egdtc)
  # for non-missing valid? valid_iso FALSE includes missing; but missing already counted; keep OR logic
  miss_n <- is.na(egstresn)
  miss_c <- is_miss(egstresc)
  fail <- miss_dtc | bad_dtc | miss_n | miss_c
  # Note: for missing EGDTC, bad_dtc is TRUE as well, still fail once
  # But SAS: missing(EGDTC) or prxmatch=0 ... prxmatch for missing is missing (=not 0?) In R bad_dtc for missing is TRUE; still OR => fail. fine.
  # For non-missing valid, bad_dtc FALSE.
  fail[is.na(fail)] <- TRUE
  n_eg03 <- sum(fail)
}

# ---------- MH-03 ----------
n_mh03 <- 0
if (!is.null(mh)) {
  mhs <- get_col(mh, "MHSTDTC"); mht <- get_col(mh, "MHTERM")
  fail <- is_miss(mhs) | (!valid_iso_date(mhs)) | is_miss(mht)
  fail[is.na(fail)] <- TRUE
  n_mh03 <- sum(fail)
}

# ---------- DM-02 ----------
n_dm02 <- 0
if (!is.null(dm)) {
  rfic <- get_col(dm, "RFICDTC"); rfs <- get_col(dm, "RFSTDTC"); rfe <- get_col(dm, "RFENDTC"); rfp <- get_col(dm, "RFPENDTC")
  miss_any <- is_miss(rfic) | is_miss(rfs) | is_miss(rfe) | is_miss(rfp)
  bad_any <- (!valid_iso_date(rfic)) | (!valid_iso_date(rfs)) | (!valid_iso_date(rfe)) | (!valid_iso_date(rfp))
  # string ordered comparisons, only when both non-missing
  s_rfic <- sas_strip(rfic); s_rfs <- sas_strip(rfs); s_rfe <- sas_strip(rfe); s_rfp <- sas_strip(rfp)
  c1 <- !is_miss(rfic) & !is_miss(rfs) & (s_rfs < s_rfic)
  c2 <- !is_miss(rfs) & !is_miss(rfe) & (s_rfe < s_rfs)
  c3 <- !is_miss(rfp) & !is_miss(rfe) & (s_rfp != s_rfe)
  c1[is.na(c1)] <- FALSE; c2[is.na(c2)] <- FALSE; c3[is.na(c3)] <- FALSE
  fail <- miss_any | bad_any | c1 | c2 | c3
  fail[is.na(fail)] <- TRUE
  n_dm02 <- sum(fail)
}

# ---------- SV-02 ----------
n_sv02 <- 0
if (!is.null(sv)) {
  us <- sas_strip(get_col(sv, "USUBJID"))
  vn <- get_col(sv, "VISITNUM")
  # VISITNUM may be numeric or char; normalize to string with NA marker
  if (is.numeric(vn)) {
    vn_s <- ifelse(is.na(vn), "<MISSING>", as.character(vn))
  } else {
    vn_c <- as.character(vn)
    vn_s <- ifelse(is.na(vn_c) | trimws(vn_c)=="", "<MISSING>", trimws(vn_c))
  }
  key <- paste0(us, "\x01", vn_s)
  tb <- table(key, useNA="no")
  n_sv02 <- sum(tb != 1)
}

# ---------- SV-03 ----------
n_sv03 <- 0
if (!is.null(sv) && !is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID"))
  drfs <- sas_strip(get_col(dm, "RFSTDTC")); drfe <- sas_strip(get_col(dm, "RFENDTC"))
  dmap <- data.frame(US=dus, RFS=drfs, RFE=drfe, stringsAsFactors=FALSE)
  dmap <- dmap[!duplicated(dmap$US), ]
  sus <- sas_strip(get_col(sv, "USUBJID"))
  m <- match(sus, dmap$US)
  # inner join: only matched
  keep <- !is.na(m)
  if (any(keep)) {
    vn <- get_col(sv, "VISITNUM"); vst <- get_col(sv, "VISIT")
    sstd <- get_col(sv, "SVSTDTC"); send <- get_col(sv, "SVENDTC")
    # subset to inner
    vn <- vn[keep]; vst <- vst[keep]; sstd <- sstd[keep]; send <- send[keep]
    rfs <- dmap$RFS[m[keep]]; rfe <- dmap$RFE[m[keep]]
    s_sstd <- sas_strip(sstd); s_send <- sas_strip(send)
    miss_vn <- is_miss(vn); miss_v <- is_miss(vst); miss_s <- is_miss(sstd); miss_e <- is_miss(send)
    bad_s <- !valid_iso_date(sstd); bad_e <- !valid_iso_date(send)
    lt <- !is_miss(send) & !is_miss(sstd) & (s_send < s_sstd)
    lt[is.na(lt)] <- FALSE
    c_low <- !is_miss(sstd) & s_sstd != "" & rfs != "" & !is.na(rfs) & (s_sstd < rfs)
    c_high <- !is_miss(send) & s_send != "" & rfe != "" & !is.na(rfe) & (s_send > rfe)
    c_low[is.na(c_low)] <- FALSE; c_high[is.na(c_high)] <- FALSE
    fail <- miss_vn | miss_v | miss_s | miss_e | bad_s | bad_e | lt | c_low | c_high
    fail[is.na(fail)] <- TRUE
    n_sv03 <- sum(fail)
  } else n_sv03 <- 0
} else if (!is.null(sv)) {
  # no DM => inner join yields 0 rows => 0? SAS inner join with no DM => 0 fails. keep 0.
  n_sv03 <- 0
}

# ---------- DS-01 ----------
n_ds01 <- 0
if (!is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID"))
  if (!is.null(ds)) {
    sus <- sas_strip(get_col(ds, "USUBJID"))
    # count DS per DM subject
    # N_DS = count(s.USUBJID)
    # use table of sus
    tb_ds <- table(sus, useNA="no")
    n_ds_for_dm <- vapply(dus, function(u) if (u %in% names(tb_ds)) as.integer(tb_ds[[u]]) else 0L, integer(1))
    part1 <- sum(n_ds_for_dm != 1)
    # orphans
    orph <- sus[!(sus %in% dus)]
    part2 <- length(unique(orph))
    # edge: if sus contains ""? still counts; SAS would group missing together. Our method same.
    n_ds01 <- part1 + part2
  } else {
    n_ds01 <- length(dus)  # all have 0
  }
}

# ---------- DS-02 ----------
n_ds02 <- 0
if (!is.null(ds) && !is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID")); drfp <- sas_strip(get_col(dm, "RFPENDTC"))
  dmap <- data.frame(US=dus, RFP=drfp, stringsAsFactors=FALSE)
  dmap <- dmap[!duplicated(dmap$US), ]
  sus <- sas_strip(get_col(ds, "USUBJID"))
  m <- match(sus, dmap$US)
  keep <- !is.na(m)
  if (any(keep)) {
    dec <- get_col(ds, "DSDECOD"); sdt <- get_col(ds, "DSSTDTC")
    dec <- dec[keep]; sdt <- sdt[keep]; rfp_m <- dmap$RFP[m[keep]]
    s_sdt <- sas_strip(sdt)
    fail <- is_miss(dec) | is_miss(sdt) | (!valid_iso_date(sdt)) | (s_sdt != rfp_m)
    # for NA rfp_m, s_sdt != NA => NA => treat as TRUE (fail) since DM missing? but inner join ensures DM exists; rfp could be missing => then != => fail
    fail[is.na(fail)] <- TRUE
    n_ds02 <- sum(fail)
  }
} else if (!is.null(ds)) {
  n_ds02 <- nrow(ds)
}

# ---------- DM-03 ----------
n_dm03 <- 0
if (!is.null(dm)) {
  race <- sas_up_strip(get_col(dm, "RACE"))
  allowed <- c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","OTHER","UNKNOWN")
  fail <- (race == "") | !(race %in% allowed)
  fail[is.na(fail)] <- TRUE
  n_dm03 <- sum(fail)
}

# ---------- DM-04 ----------
n_dm04 <- 0
if (!is.null(dm)) {
  site <- get_col(dm, "SITEID"); subj <- get_col(dm, "SUBJID")
  s_site <- sas_strip(site)
  s_subj <- sas_strip(subj)
  tok <- scan_first_dash(subj)
  fail <- (s_site == "") | (s_site != tok)
  fail[is.na(fail)] <- TRUE
  n_dm04 <- sum(fail)
}

# ---------- ADAE-02 ----------
n_adae02 <- 0
if (!is.null(adae)) {
  aocc <- sas_strip(get_col(adae, "AOCCFL")); trt <- sas_strip(get_col(adae, "TRTEMFL"))
  miss_a <- (aocc == "")
  c1 <- (!miss_a) & (aocc != "Y")
  c2 <- (aocc == "Y") & (trt != "Y")
  c1[is.na(c1)] <- FALSE; c2[is.na(c2)] <- FALSE
  n_adae02 <- sum(c1 | c2)
}

# ---------- ADAE-03 ----------
n_adae03 <- 0
if (!is.null(adae)) {
  us <- sas_strip(get_col(adae, "USUBJID")); pt <- sas_strip(get_col(adae, "AEDECOD"))
  trt <- sas_strip(get_col(adae, "TRTEMFL")); aocc <- sas_strip(get_col(adae, "AOCCFL"))
  df <- data.frame(US=us, PT=pt, TRT=(trt=="Y"), AOC=(aocc=="Y"), stringsAsFactors=FALSE)
  df$TRT[is.na(df$TRT)] <- FALSE; df$AOC[is.na(df$AOC)] <- FALSE
  agg <- aggregate(cbind(TRT, AOC) ~ US + PT, data=df, FUN=sum)
  fail <- (agg$TRT > 0) & (agg$AOC != 1)
  n_adae03 <- sum(fail)
}

# ---------- CM-01 ----------
n_cm01 <- 0
if (!is.null(cm)) {
  cus <- sas_strip(get_col(cm, "USUBJID"))
  dus <- if (!is.null(dm)) sas_strip(get_col(dm, "USUBJID")) else character(0)
  trt <- get_col(cm, "CMTRT"); dec <- get_col(cm, "CMDECOD")
  dose <- get_col(cm, "CMDOSE"); dosu <- get_col(cm, "CMDOSU")
  sdtc <- get_col(cm, "CMSTDTC"); endtc <- get_col(cm, "CMENDTC")
  enrt <- sas_strip(get_col(cm, "CMENRTPT"))  # "" for missing, case-sensitive per SAS (no upcase here)
  # SAS uses c.CMENRTPT = 'ONGOING' exact case
  s_trt_up <- sas_up_strip(trt); s_dec_up <- sas_up_strip(dec)
  s_sdt <- sas_strip(sdtc); s_end <- sas_strip(endtc)
  n <- nrow(cm)
  fail <- rep(FALSE, n)
  for (i in seq_len(n)) {
    if (!(cus[i] %in% dus)) { fail[i] <- TRUE; next }
    if (sas_strip(trt[i]) == "") { fail[i] <- TRUE; next }
    if (sas_strip(dec[i]) == "") { fail[i] <- TRUE; next }
    if (s_trt_up[i] == "PARACETAMOL" && s_dec_up[i] != "ACETAMINOPHEN") { fail[i] <- TRUE; next }
    if (is_miss(dose[i])) { fail[i] <- TRUE; next }
    if (is_miss(dosu[i])) { fail[i] <- TRUE; next }
    if (is_miss(sdtc[i])) { fail[i] <- TRUE; next }
    if (!valid_iso_date(sdtc[i])) { fail[i] <- TRUE; next }
    if (!is_miss(endtc[i]) && !valid_iso_date(endtc[i])) { fail[i] <- TRUE; next }
    if (!is_miss(endtc[i]) && !is_miss(sdtc[i]) && s_end[i] < s_sdt[i]) { fail[i] <- TRUE; next }
    if (enrt[i] == "ONGOING" && !is_miss(endtc[i])) { fail[i] <- TRUE; next }
    if (enrt[i] != "ONGOING" && is_miss(endtc[i])) { fail[i] <- TRUE; next }
  }
  n_cm01 <- sum(fail)
}

# ---------- DS-03 ----------
n_ds03 <- 0
if (!is.null(ds)) {
  dec_up <- sas_up_strip(get_col(ds, "DSDECOD"))
  allowed <- c("COMPLETED","LOST TO FOLLOW-UP","WITHDRAWAL BY SUBJECT","ADVERSE EVENT","SCREEN FAILURE")
  fail <- !(dec_up %in% allowed)
  fail[is.na(fail)] <- TRUE
  n_ds03 <- sum(fail)
}

# ---------- SEQ-01 ----------
seq_fail_count <- function(df, seqnm) {
  if (is.null(df)) return(0L)
  us <- sas_strip(get_col(df, "USUBJID")); sq <- get_col(df, seqnm)
  if (is.null(us) || is.null(sq)) return(0L)
  # SEQ numeric; NA groups together? SAS groups missing together
  sq_s <- if (is.numeric(sq)) ifelse(is.na(sq), "<MISSING>", as.character(sq)) else {
    xc <- as.character(sq); xc[is.na(xc)] <- "<MISSING>"; xc[trimws(xc)==""] <- "<MISSING>"; xc
  }
  key <- paste0(us, "\x01", sq_s)
  tb <- table(key, useNA="no")
  sum(tb > 1)
}
n_seq01 <- seq_fail_count(ae,"AESEQ") + seq_fail_count(ex,"EXSEQ") + seq_fail_count(lb,"LBSEQ") +
           seq_fail_count(vs,"VSSEQ") + seq_fail_count(cm,"CMSEQ") + seq_fail_count(mh,"MHSEQ") +
           seq_fail_count(eg,"EGSEQ")

# ---------- CM-02 ----------
n_cm02 <- 0
if (!is.null(cm) && !is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID")); drfe <- sas_strip(get_col(dm, "RFENDTC"))
  dmap <- data.frame(US=dus, RFE=drfe, stringsAsFactors=FALSE)
  dmap <- dmap[!duplicated(dmap$US), ]
  cus <- sas_strip(get_col(cm, "USUBJID"))
  m <- match(cus, dmap$US)
  # inner join only
  keep <- !is.na(m)
  if (any(keep)) {
    enrt_up <- sas_up_strip(get_col(cm, "CMENRTPT"))[keep]
    endtc <- get_col(cm, "CMENDTC")[keep]
    entpt <- get_col(cm, "CMENTPT")[keep]
    rfe_m <- dmap$RFE[m[keep]]
    s_ent <- sas_strip(entpt)
    fail <- rep(FALSE, sum(keep))
    for (i in seq_along(fail)) {
      e <- enrt_up[i]
      if (!(e %in% c("", "ONGOING"))) { fail[i] <- TRUE; next }
      if (e == "ONGOING") {
        if (!is_miss(endtc[i])) { fail[i] <- TRUE; next }
        if (is_miss(entpt[i])) { fail[i] <- TRUE; next }
        if (!valid_iso_date(entpt[i])) { fail[i] <- TRUE; next }
        if (s_ent[i] != rfe_m[i]) { fail[i] <- TRUE; next }
      } else { # not ONGOING (includes "")
        if (!is_miss(entpt[i])) { fail[i] <- TRUE; next }
      }
    }
    n_cm02 <- sum(fail)
  }
}

# ---------- consolidate ----------
checks <- data.frame(
  CHECK_ID = c("ADSL-001","ADAE-001","ADVS-001","ADLB-001","ADVS-002","ADLB-002","SDTM-001",
               "SDTM-002","SDTM-003","SDTM-004","SDTM-005","ADSL-002","ADSL-003","ADSL-004",
               "ADTTE-001","ADTTE-002","ADTTE-003",
               "SDTM-006","SDTM-007","SDTM-008","SDTM-009",
               "SDTM-010","SDTM-011","SDTM-012","SDTM-013","SDTM-014",
               "SDTM-015","SDTM-016","SDTM-017","SDTM-018","SDTM-019",
               "SDTM-020","SDTM-021","ADAE-002","ADAE-003",
               "SDTM-022","SDTM-023","SDTM-024","SDTM-025"),
  DOMAIN = c("ADSL","ADAE","ADVS","ADLB","ADVS","ADLB","DM",
             "AE","EX","LB","VS","ADSL","ADSL","ADSL",
             "ADTTE","ADTTE","ADTTE",
             "EG","EG","EG","EG",
             "MH","MH","MH","MH","DM",
             "SV","SV","SV","DS","DS",
             "DM","DM","ADAE","ADAE",
             "CM","DS","SDTM","CM"),
  RULE = c("USUBJID must be perfectly unique","AEENDT must be >= AESTDT","Max 1 Baseline (ABLFL=Y) per Param","Max 1 Baseline (ABLFL=Y) per Param",
           "Baseline date has exactly one eligible source record","Baseline date has exactly one eligible source record","USUBJID must be perfectly unique",
           "Every AE.USUBJID exists in DM","Every EX.USUBJID exists in DM","Every LB.USUBJID exists in DM","Every VS.USUBJID exists in DM",
           "SAFFL=Y requires exposure in EX","EOSDT must agree with DM.RFPENDTC","AGE must equal completed years at informed consent",
           "Survival time (AVAL) must be >= 1","AVAL and CNSR agree with event or subject follow-up date","Event and follow-up dates must be chronologically valid",
           "Every EG.USUBJID exists in DM","Required EG variables must exist","EG date/value contract must be valid","Raw EG variables must not leak to SDTM",
           "Every MH.USUBJID exists in DM","Required MH variables must exist","MH date/term contract must be valid","Raw MH variables must not leak to SDTM",
           "DM reference dates complete and ordered",
           "Every SV.USUBJID exists in DM","USUBJID and VISITNUM must be unique","SV dates valid and inside reference period",
           "Exactly one final DS record per DM subject","DS date matches DM participation end",
           "RACE uses supported CDISC terminology","SITEID present and consistent with SUBJID","AOCCFL valid and restricted to TRTEMFL=Y","One AOCCFL=Y per subject and TEAE term",
           "CM subject, medication, dose and dates must be valid","DSDECOD uses supported disposition terminology","Domain sequence keys must be unique per subject","Ongoing CM must be anchored to the DM reference end"),
  N_FAIL = c(n_adsl01,n_adae01,n_advs01,n_adlb01,n_advs02,n_adlb02,n_dm01,
             n_ae01,n_ex01,n_lb01,n_vs01,n_adsl02,n_adsl03,n_adsl04,
             n_adtte01,n_adtte02,n_adtte03,
             n_eg01,n_eg02,n_eg03,n_eg04,
             n_mh01,n_mh02,n_mh03,n_mh04,n_dm02,
             n_sv01,n_sv02,n_sv03,n_ds01,n_ds02,
             n_dm03,n_dm04,n_adae02,n_adae03,
             n_cm01,n_ds03,n_seq01,n_cm02),
  stringsAsFactors = FALSE
)
checks$N_FAIL <- as.integer(checks$N_FAIL)
checks$STATUS <- ifelse(checks$N_FAIL == 0, "PASS", "FAIL")

qc_report <- checks

cat(sprintf("QC complete: %d checks, %d failed\n", nrow(qc_report), sum(qc_report$STATUS=="FAIL")))

# ---------- write outputs ----------
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names = FALSE, na = "")

# text rendering (P019.lst counterpart)
txt_path1 <- file.path(out, "qc_report.txt")
txt_path2 <- file.path(out, "P019.txt")
csv_body2 <- file.path(out, "P019.csv")
con_lines <- c(
  "Automated Quality Control (QC) Validation Report",
  "",
  sprintf("%-10s %-6s %-60s %6s %-6s", "CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS"),
  paste(rep("-", 95), collapse="")
)
for (i in seq_len(nrow(qc_report))) {
  con_lines <- c(con_lines, sprintf("%-10s %-6s %-60s %6d %-6s",
    qc_report$CHECK_ID[i], qc_report$DOMAIN[i],
    substr(qc_report$RULE[i], 1, 60),
    qc_report$N_FAIL[i], qc_report$STATUS[i]))
}
con_lines <- c(con_lines, "",
  sprintf("Total checks: %d   Failed: %d", nrow(qc_report), sum(qc_report$STATUS=="FAIL")))
writeLines(con_lines, txt_path1)
writeLines(con_lines, txt_path2)
write.csv(qc_report, csv_body2, row.names = FALSE, na = "")

cat("outputs written\n")
