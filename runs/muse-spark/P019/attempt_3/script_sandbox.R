out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
})

cat("P019 QC core translation starting\n")

sas_strip <- function(x) {
  if (is.null(x)) return(character(0))
  xc <- as.character(x)
  res <- trimws(xc, which = "both")
  res[is.na(xc)] <- ""
  res[is.na(res)] <- ""
  res
}
sas_up_strip <- function(x) toupper(sas_strip(x))
is_miss <- function(x) {
  if (is.null(x)) return(logical(0))
  if (inherits(x, "Date") || inherits(x, "POSIXt") || is.numeric(x) || is.integer(x) || is.logical(x)) return(is.na(x))
  xc <- as.character(x)
  return(is.na(xc) | trimws(xc) == "")
}
valid_iso_date <- function(x) {
  xc <- as.character(x)
  out <- rep(FALSE, length(xc))
  idx <- !is.na(xc) & trimws(xc) != ""
  if (any(idx)) {
    s <- trimws(xc[idx])
    out[idx] <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)
  }
  out[is.na(out)] <- FALSE
  out
}
parse_yymmdd10 <- function(x) {
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
  br <- as.Date(brth); rf <- as.Date(rfic)
  n <- length(br)
  res <- rep(NA_integer_, n)
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
  xc <- sas_strip(x)
  vapply(xc, function(s) {
    if (s == "") return("")
    parts <- strsplit(s, "-", fixed = TRUE)[[1]]
    trimws(parts[1])
  }, character(1), USE.NAMES = FALSE)
}
get_col <- function(df, nm) {
  if (is.null(df)) return(NULL)
  cn <- names(df)
  idx <- which(toupper(cn) == toupper(nm))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
get_vec <- function(df, nm, n) {
  # return vector length n, NA-filled if column missing
  v <- get_col(df, nm)
  if (is.null(v)) return(rep(NA, n))
  if (length(v) != n) return(rep(NA, n))
  v
}
clean_df <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (inherits(v, "haven_labelled") || inherits(v, "haven_labelled_spss")) v <- haven::zap_labels(v)
    if (is.factor(v)) v <- as.character(v)
    if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) v <- as.Date(v)
    df[[j]] <- v
  }
  names(df) <- toupper(names(df))
  df
}
read_lib_mem <- function(libdir, mem) {
  if (!dir.exists(libdir)) return(NULL)
  files <- list.files(libdir, full.names = TRUE, recursive = FALSE)
  if (length(files) == 0) return(NULL)
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(mem))
  f <- NULL
  if (length(idx) > 0) f <- files[idx[1]]
  else {
    cand <- c(file.path(libdir, paste0(mem, ".sas7bdat")),
              file.path(libdir, paste0(tolower(mem), ".sas7bdat")),
              file.path(libdir, paste0(toupper(mem), ".sas7bdat")),
              file.path(libdir, paste0(mem, ".xpt")),
              file.path(libdir, paste0(tolower(mem), ".xpt")))
    cand <- cand[file.exists(cand)]
    if (length(cand) == 0) return(NULL)
    f <- cand[1]
  }
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext %in% c("sas7bdat","sas7dat")) df <- haven::read_sas(f)
  else if (ext == "xpt") df <- haven::read_xpt(f)
  else if (ext == "csv") df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
  else if (ext == "parquet") df <- as.data.frame(arrow::read_parquet(f))
  else try({ df <- haven::read_sas(f) }, silent = TRUE)
  if (is.null(df)) return(NULL)
  clean_df(as.data.frame(df))
}

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

n_adsl01 <- 0
if (!is.null(adsl) && "USUBJID" %in% names(adsl)) {
  u <- sas_strip(get_col(adsl, "USUBJID"))
  tb <- table(u, useNA = "no")
  n_adsl01 <- sum(tb > 1)
}
n_adae01 <- 0
if (!is.null(adae)) {
  aendt <- as.Date(get_col(adae, "AEENDT")); aestdt <- as.Date(get_col(adae, "AESTDT"))
  if (!is.null(aendt) && !is.null(aestdt)) {
    cond <- !is.na(aendt) & !is.na(aestdt) & (aendt < aestdt)
    cond[is.na(cond)] <- FALSE
    n_adae01 <- sum(cond)
  }
}
n_advs01 <- 0
if (!is.null(advs)) {
  abl_s <- sas_strip(get_col(advs, "ABLFL")); us <- sas_strip(get_col(advs, "USUBJID")); pc <- sas_strip(get_col(advs, "PARAMCD"))
  idx <- abl_s == "Y"; idx[is.na(idx)] <- FALSE
  if (any(idx)) { tb <- table(paste0(us[idx], "\x01", pc[idx]), useNA="no"); n_advs01 <- sum(tb > 1) }
}
n_adlb01 <- 0
if (!is.null(adlb)) {
  abl_s <- sas_strip(get_col(adlb, "ABLFL")); us <- sas_strip(get_col(adlb, "USUBJID")); pc <- sas_strip(get_col(adlb, "PARAMCD"))
  idx <- abl_s == "Y"; idx[is.na(idx)] <- FALSE
  if (any(idx)) { tb <- table(paste0(us[idx], "\x01", pc[idx]), useNA="no"); n_adlb01 <- sum(tb > 1) }
}
n_dm01 <- 0
if (!is.null(dm) && "USUBJID" %in% names(dm)) {
  u <- sas_strip(get_col(dm, "USUBJID"))
  tb <- table(u, useNA = "no")
  n_dm01 <- sum(tb > 1)
}
ref_fail <- function(child, dm_df) {
  if (is.null(child) || is.null(dm_df)) return(0L)
  cu <- sas_strip(get_col(child, "USUBJID")); du <- sas_strip(get_col(dm_df, "USUBJID"))
  if (is.null(cu) || is.null(du)) return(0L)
  dset <- unique(du)
  sum(!(cu %in% dset))
}
n_ae01 <- ref_fail(ae, dm); n_ex01 <- ref_fail(ex, dm); n_lb01 <- ref_fail(lb, dm)
n_vs01 <- ref_fail(vs, dm); n_eg01 <- ref_fail(eg, dm); n_mh01 <- ref_fail(mh, dm)
n_sv01 <- ref_fail(sv, dm)

n_adsl02 <- 0
if (!is.null(adsl)) {
  saff <- sas_strip(get_col(adsl, "SAFFL")); aus <- sas_strip(get_col(adsl, "USUBJID"))
  idx <- saff == "Y"; idx[is.na(idx)] <- FALSE
  if (any(idx)) {
    if (!is.null(ex)) { eus <- unique(sas_strip(get_col(ex, "USUBJID"))); n_adsl02 <- sum(!(aus[idx] %in% eus)) }
    else n_adsl02 <- sum(idx)
  }
}
n_adtte01 <- 0
if (!is.null(adtte)) {
  aval <- suppressWarnings(as.numeric(get_col(adtte, "AVAL")))
  cond <- is.na(aval) | aval < 1; cond[is.na(cond)] <- TRUE
  n_adtte01 <- sum(cond)
}
n_adtte02 <- 0
if (!is.null(adtte)) {
  trtsdt <- as.Date(get_col(adtte, "TRTSDT")); cnsr <- suppressWarnings(as.numeric(get_col(adtte, "CNSR")))
  eventdt <- as.Date(get_col(adtte, "EVENTDT")); eosdt <- as.Date(get_col(adtte, "EOSDT"))
  aval <- suppressWarnings(as.numeric(get_col(adtte, "AVAL")))
  n <- nrow(adtte); fail <- rep(FALSE, n)
  for (i in seq_len(n)) {
    t <- trtsdt[i]; c <- cnsr[i]; e <- eventdt[i]; s <- eosdt[i]; a <- aval[i]
    if (is.na(t) || is.na(c) || !(c %in% c(0,1))) { fail[i] <- TRUE; next }
    if (c == 0) {
      if (is.na(e)) { fail[i] <- TRUE; next }
      exp <- as.numeric(e - t) + 1
      if (is.na(a) || is.na(exp) || a != exp) fail[i] <- TRUE
    } else {
      if (!is.na(e) || is.na(s)) { fail[i] <- TRUE; next }
      exp <- as.numeric(s - t) + 1
      if (is.na(a) || is.na(exp) || a != exp) fail[i] <- TRUE
    }
  }
  n_adtte02 <- sum(fail)
}
n_adtte03 <- 0
if (!is.null(adtte)) {
  trtsdt <- as.Date(get_col(adtte, "TRTSDT")); eosdt <- as.Date(get_col(adtte, "EOSDT")); eventdt <- as.Date(get_col(adtte, "EVENTDT"))
  n <- nrow(adtte); fail <- rep(FALSE, n)
  for (i in seq_len(n)) {
    t <- trtsdt[i]; s <- eosdt[i]; e <- eventdt[i]
    if (is.na(s)) { fail[i] <- TRUE; next }
    if (!is.na(t) && s < t) { fail[i] <- TRUE; next }
    if (!is.na(e) && ((!is.na(t) && e < t) || e > s)) fail[i] <- TRUE
  }
  n_adtte03 <- sum(fail)
}
n_adsl03 <- 0
if (!is.null(adsl)) {
  aus <- sas_strip(get_col(adsl, "USUBJID")); aeos <- as.Date(get_col(adsl, "EOSDT"))
  if (!is.null(dm)) {
    dus <- sas_strip(get_col(dm, "USUBJID")); rfp <- as.character(get_col(dm, "RFPENDTC"))
    lookup <- data.frame(US=dus, RFP=rfp, stringsAsFactors=FALSE)
    lookup <- lookup[!duplicated(lookup$US), ]
    m <- match(aus, lookup$US)
    parsed <- parse_yymmdd10(lookup$RFP[m])
    fail <- is.na(m) | is.na(aeos) | is.na(parsed) | (aeos != parsed)
    fail[is.na(fail)] <- TRUE
    n_adsl03 <- sum(fail)
  } else n_adsl03 <- nrow(adsl)
}
n_adsl04 <- 0
if (!is.null(adsl)) {
  brth <- as.Date(get_col(adsl, "BRTHDT")); rfic <- as.Date(get_col(adsl, "RFICDT"))
  age <- suppressWarnings(as.numeric(get_col(adsl, "AGE")))
  exp <- completed_years(brth, rfic)
  fail <- is.na(brth) | is.na(rfic) | is.na(age) | is.na(exp) | (age != exp)
  fail[is.na(fail)] <- TRUE
  n_adsl04 <- sum(fail)
}
baseline_dup <- function(d) {
  if (is.null(d)) return(0L)
  us <- sas_strip(get_col(d, "USUBJID")); pc <- sas_strip(get_col(d, "PARAMCD"))
  astdt <- as.Date(get_col(d, "ASTDT")); trtsdt <- as.Date(get_col(d, "TRTSDT"))
  aval <- suppressWarnings(as.numeric(get_col(d, "AVAL")))
  if (is.null(us) || is.null(pc) || is.null(astdt) || is.null(trtsdt) || is.null(aval)) return(0L)
  elig <- !is.na(astdt) & !is.na(trtsdt) & !is.na(aval) & (astdt <= trtsdt)
  elig[is.na(elig)] <- FALSE
  if (!any(elig)) return(0L)
  df_e <- data.frame(US=us[elig], PC=pc[elig], DT=astdt[elig], stringsAsFactors=FALSE)
  agg <- aggregate(DT ~ US + PC, data=df_e, FUN=max)
  names(agg)[3] <- "BASE_DT"
  key_all <- paste0(us, "\x01", pc); key_agg <- paste0(agg$US, "\x01", agg$PC)
  m <- match(key_all, key_agg)
  base_dt <- agg$BASE_DT[m]
  cand <- !is.na(aval) & !is.na(astdt) & !is.na(m) & !is.na(base_dt) & (astdt == base_dt)
  cand[is.na(cand)] <- FALSE
  if (!any(cand)) return(0L)
  tb <- table(key_all[cand], useNA="no")
  sum(tb > 1)
}
n_advs02 <- baseline_dup(advs); n_adlb02 <- baseline_dup(adlb)

req_eg <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST','EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
req_mh <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
raw_eg <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
raw_mh <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')
dict_present <- function(df, lst) { if (is.null(df)) return(0L); sum(lst %in% toupper(names(df))) }
n_eg02 <- 12 - dict_present(eg, req_eg)
n_mh02 <- 6 - dict_present(mh, req_mh)
n_eg04 <- dict_present(eg, raw_eg)
n_mh04 <- dict_present(mh, raw_mh)

n_eg03 <- 0
if (!is.null(eg)) {
  egdtc <- get_col(eg, "EGDTC"); egstresn <- suppressWarnings(as.numeric(get_col(eg, "EGSTRESN"))); egstresc <- get_col(eg, "EGSTRESC")
  fail <- is_miss(egdtc) | (!valid_iso_date(egdtc)) | is.na(egstresn) | is_miss(egstresc)
  fail[is.na(fail)] <- TRUE
  n_eg03 <- sum(fail)
}
n_mh03 <- 0
if (!is.null(mh)) {
  fail <- is_miss(get_col(mh, "MHSTDTC")) | (!valid_iso_date(get_col(mh, "MHSTDTC"))) | is_miss(get_col(mh, "MHTERM"))
  fail[is.na(fail)] <- TRUE
  n_mh03 <- sum(fail)
}
n_dm02 <- 0
if (!is.null(dm)) {
  rfic <- get_col(dm, "RFICDTC"); rfs <- get_col(dm, "RFSTDTC"); rfe <- get_col(dm, "RFENDTC"); rfp <- get_col(dm, "RFPENDTC")
  miss_any <- is_miss(rfic) | is_miss(rfs) | is_miss(rfe) | is_miss(rfp)
  bad_any <- (!valid_iso_date(rfic)) | (!valid_iso_date(rfs)) | (!valid_iso_date(rfe)) | (!valid_iso_date(rfp))
  s_rfic <- sas_strip(rfic); s_rfs <- sas_strip(rfs); s_rfe <- sas_strip(rfe); s_rfp <- sas_strip(rfp)
  c1 <- !is_miss(rfic) & !is_miss(rfs) & (s_rfs < s_rfic)
  c2 <- !is_miss(rfs) & !is_miss(rfe) & (s_rfe < s_rfs)
  c3 <- !is_miss(rfp) & !is_miss(rfe) & (s_rfp != s_rfe)
  c1[is.na(c1)] <- FALSE; c2[is.na(c2)] <- FALSE; c3[is.na(c3)] <- FALSE
  fail <- miss_any | bad_any | c1 | c2 | c3
  fail[is.na(fail)] <- TRUE
  n_dm02 <- sum(fail)
}
n_sv02 <- 0
if (!is.null(sv)) {
  us <- sas_strip(get_col(sv, "USUBJID")); vn <- get_col(sv, "VISITNUM")
  if (is.numeric(vn)) vn_s <- ifelse(is.na(vn), "<MISSING>", as.character(vn))
  else { xc <- as.character(vn); vn_s <- ifelse(is.na(xc) | trimws(xc)=="", "<MISSING>", trimws(xc)) }
  tb <- table(paste0(us, "\x01", vn_s), useNA="no")
  n_sv02 <- sum(tb != 1)
}

# SDTM-017 SV03: inner join, SAS character semantics, robust to missing cols
n_sv03 <- 0
sv_exists <- !is.null(sv); dm_exists <- !is.null(dm)
if (sv_exists && dm_exists) {
  nsv <- nrow(sv)
  s_us <- sas_strip(get_vec(sv, "USUBJID", nsv))
  s_vn <- get_vec(sv, "VISITNUM", nsv)
  s_vi <- get_vec(sv, "VISIT", nsv)
  s_ss <- get_vec(sv, "SVSTDTC", nsv)
  s_se <- get_vec(sv, "SVENDTC", nsv)
  d_us <- sas_strip(get_col(dm, "USUBJID")); d_rfs <- sas_strip(get_col(dm, "RFSTDTC")); d_rfe <- sas_strip(get_col(dm, "RFENDTC"))
  dmap <- data.frame(US=d_us, RFS=d_rfs, RFE=d_rfe, stringsAsFactors=FALSE)
  dmap <- dmap[!duplicated(dmap$US), ]
  sv_df <- data.frame(US=s_us, VN=I(as.list(s_vn)), VISIT=I(as.list(s_vi)),
                       SSTD=I(as.list(s_ss)), SEND=I(as.list(s_se)), stringsAsFactors=FALSE)
  # expand list-cols back to vectors preserving NA
  sv_df2 <- data.frame(US=s_us, stringsAsFactors=FALSE)
  sv_df2$VN <- s_vn; sv_df2$VISIT <- s_vi; sv_df2$SSTD <- s_ss; sv_df2$SEND <- s_se
  mrg <- merge(sv_df2, dmap, by="US", all=FALSE, sort=FALSE)
  if (nrow(mrg) > 0) {
    s_sstd <- sas_strip(mrg$SSTD); s_send <- sas_strip(mrg$SEND)
    miss_vn <- is_miss(mrg$VN); miss_v <- is_miss(mrg$VISIT)
    miss_s <- is_miss(mrg$SSTD); miss_e <- is_miss(mrg$SEND)
    bad_s <- !valid_iso_date(mrg$SSTD); bad_e <- !valid_iso_date(mrg$SEND)
    lt <- !is_miss(mrg$SEND) & !is_miss(mrg$SSTD) & (s_send < s_sstd); lt[is.na(lt)] <- FALSE
    c_low <- !is_miss(mrg$SSTD) & (s_sstd < mrg$RFS); c_low[is.na(c_low)] <- FALSE
    c_high <- !is_miss(mrg$SEND) & (s_send > mrg$RFE); c_high[is.na(c_high)] <- FALSE
    fail <- miss_vn | miss_v | miss_s | miss_e | bad_s | bad_e | lt | c_low | c_high
    fail[is.na(fail)] <- TRUE
    n_sv03 <- sum(fail)
  }
} else if (sv_exists && !dm_exists) {
  n_sv03 <- 0
}

n_ds01 <- 0
if (!is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID"))
  if (!is.null(ds)) {
    sus <- sas_strip(get_col(ds, "USUBJID"))
    tb_ds <- table(sus, useNA="no")
    n_ds_for_dm <- vapply(dus, function(u) if (u %in% names(tb_ds)) as.integer(tb_ds[[u]]) else 0L, integer(1))
    part1 <- sum(n_ds_for_dm != 1)
    part2 <- length(unique(sus[!(sus %in% dus)]))
    n_ds01 <- part1 + part2
  } else n_ds01 <- length(dus)
}
# SDTM-019 DS02 inner join
n_ds02 <- 0
if (!is.null(ds) && !is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID")); drfp <- sas_strip(get_col(dm, "RFPENDTC"))
  dmap <- data.frame(US=dus, RFP=drfp, stringsAsFactors=FALSE)
  dmap <- dmap[!duplicated(dmap$US), ]
  nds <- nrow(ds)
  ds_df <- data.frame(US=sas_strip(get_vec(ds, "USUBJID", nds)), stringsAsFactors=FALSE)
  ds_df$DECOD <- get_vec(ds, "DSDECOD", nds)
  ds_df$SDT <- get_vec(ds, "DSSTDTC", nds)
  mrg <- merge(ds_df, dmap, by="US", all=FALSE, sort=FALSE)
  if (nrow(mrg) > 0) {
    s_sdt <- sas_strip(mrg$SDT)
    fail <- is_miss(mrg$DECOD) | is_miss(mrg$SDT) | (!valid_iso_date(mrg$SDT)) | (s_sdt != mrg$RFP)
    fail[is.na(fail)] <- TRUE
    n_ds02 <- sum(fail)
  }
}
n_dm03 <- 0
if (!is.null(dm)) {
  race <- sas_up_strip(get_col(dm, "RACE"))
  allowed <- c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","OTHER","UNKNOWN")
  fail <- (race == "") | !(race %in% allowed); fail[is.na(fail)] <- TRUE
  n_dm03 <- sum(fail)
}
n_dm04 <- 0
if (!is.null(dm)) {
  s_site <- sas_strip(get_col(dm, "SITEID")); tok <- scan_first_dash(get_col(dm, "SUBJID"))
  fail <- (s_site == "") | (s_site != tok); fail[is.na(fail)] <- TRUE
  n_dm04 <- sum(fail)
}
n_adae02 <- 0
if (!is.null(adae)) {
  aocc <- sas_strip(get_col(adae, "AOCCFL")); trt <- sas_strip(get_col(adae, "TRTEMFL"))
  c1 <- (aocc != "") & (aocc != "Y"); c2 <- (aocc == "Y") & (trt != "Y")
  c1[is.na(c1)] <- FALSE; c2[is.na(c2)] <- FALSE
  n_adae02 <- sum(c1 | c2)
}
n_adae03 <- 0
if (!is.null(adae)) {
  us <- sas_strip(get_col(adae, "USUBJID")); pt <- sas_strip(get_col(adae, "AEDECOD"))
  trt <- sas_strip(get_col(adae, "TRTEMFL")); aocc <- sas_strip(get_col(adae, "AOCCFL"))
  df <- data.frame(US=us, PT=pt, TRT=(trt=="Y"), AOC=(aocc=="Y"), stringsAsFactors=FALSE)
  df$TRT[is.na(df$TRT)] <- FALSE; df$AOC[is.na(df$AOC)] <- FALSE
  agg <- aggregate(cbind(TRT, AOC) ~ US + PT, data=df, FUN=sum)
  n_adae03 <- sum((agg$TRT > 0) & (agg$AOC != 1))
}
n_cm01 <- 0
if (!is.null(cm)) {
  ncm <- nrow(cm)
  cus <- sas_strip(get_vec(cm, "USUBJID", ncm))
  dus <- if (!is.null(dm)) sas_strip(get_col(dm, "USUBJID")) else character(0)
  s_trt_up <- sas_up_strip(get_vec(cm, "CMTRT", ncm)); s_dec_up <- sas_up_strip(get_vec(cm, "CMDECOD", ncm))
  dose <- get_vec(cm, "CMDOSE", ncm); dosu <- get_vec(cm, "CMDOSU", ncm)
  sdtc <- get_vec(cm, "CMSTDTC", ncm); endtc <- get_vec(cm, "CMENDTC", ncm)
  enrt <- sas_strip(get_vec(cm, "CMENRTPT", ncm))
  s_sdt <- sas_strip(sdtc); s_end <- sas_strip(endtc)
  trt_raw <- get_vec(cm, "CMTRT", ncm); dec_raw <- get_vec(cm, "CMDECOD", ncm)
  fail <- rep(FALSE, ncm)
  for (i in seq_len(ncm)) {
    if (!(cus[i] %in% dus)) { fail[i] <- TRUE; next }
    if (sas_strip(trt_raw[i]) == "") { fail[i] <- TRUE; next }
    if (sas_strip(dec_raw[i]) == "") { fail[i] <- TRUE; next }
    if (s_trt_up[i] == "PARACETAMOL" && s_dec_up[i] != "ACETAMINOPHEN") { fail[i] <- TRUE; next }
    if (is_miss(dose[i])) { fail[i] <- TRUE; next }
    if (is_miss(dosu[i])) { fail[i] <- TRUE; next }
    if (is_miss(sdtc[i]) || !valid_iso_date(sdtc[i])) { fail[i] <- TRUE; next }
    if (!is_miss(endtc[i]) && !valid_iso_date(endtc[i])) { fail[i] <- TRUE; next }
    if (!is_miss(endtc[i]) && !is_miss(sdtc[i]) && s_end[i] < s_sdt[i]) { fail[i] <- TRUE; next }
    if (enrt[i] == "ONGOING" && !is_miss(endtc[i])) { fail[i] <- TRUE; next }
    if (enrt[i] != "ONGOING" && is_miss(endtc[i])) { fail[i] <- TRUE; next }
  }
  n_cm01 <- sum(fail)
}
n_ds03 <- 0
if (!is.null(ds)) {
  dec_up <- sas_up_strip(get_col(ds, "DSDECOD"))
  allowed <- c("COMPLETED","LOST TO FOLLOW-UP","WITHDRAWAL BY SUBJECT","ADVERSE EVENT","SCREEN FAILURE")
  fail <- !(dec_up %in% allowed); fail[is.na(fail)] <- TRUE
  n_ds03 <- sum(fail)
}
seq_fail_count <- function(df, seqnm) {
  if (is.null(df)) return(0L)
  us <- sas_strip(get_col(df, "USUBJID")); sq <- get_col(df, seqnm)
  if (is.null(us) || is.null(sq)) return(0L)
  if (is.numeric(sq)) sq_s <- ifelse(is.na(sq), "<MISSING>", as.character(sq))
  else { xc <- as.character(sq); xc[is.na(xc)] <- "<MISSING>"; xc[trimws(xc)==""] <- "<MISSING>"; sq_s <- xc }
  tb <- table(paste0(us, "\x01", sq_s), useNA="no")
  sum(tb > 1)
}
n_seq01 <- seq_fail_count(ae,"AESEQ") + seq_fail_count(ex,"EXSEQ") + seq_fail_count(lb,"LBSEQ") +
           seq_fail_count(vs,"VSSEQ") + seq_fail_count(cm,"CMSEQ") + seq_fail_count(mh,"MHSEQ") +
           seq_fail_count(eg,"EGSEQ")
# SDTM-025 CM02 inner join
n_cm02 <- 0
if (!is.null(cm) && !is.null(dm)) {
  dus <- sas_strip(get_col(dm, "USUBJID")); drfe <- sas_strip(get_col(dm, "RFENDTC"))
  dmap <- data.frame(US=dus, RFE=drfe, stringsAsFactors=FALSE)
  dmap <- dmap[!duplicated(dmap$US), ]
  ncm <- nrow(cm)
  cm_df <- data.frame(US=sas_strip(get_vec(cm, "USUBJID", ncm)), stringsAsFactors=FALSE)
  cm_df$ENRT <- get_vec(cm, "CMENRTPT", ncm)
  cm_df$ENDT <- get_vec(cm, "CMENDTC", ncm)
  cm_df$ENTPT <- get_vec(cm, "CMENTPT", ncm)
  mrg <- merge(cm_df, dmap, by="US", all=FALSE, sort=FALSE)
  if (nrow(mrg) > 0) {
    enrt_up <- sas_up_strip(mrg$ENRT); s_ent <- sas_strip(mrg$ENTPT)
    fail <- rep(FALSE, nrow(mrg))
    for (i in seq_len(nrow(mrg))) {
      e <- enrt_up[i]
      if (!(e %in% c("", "ONGOING"))) { fail[i] <- TRUE; next }
      if (e == "ONGOING") {
        if (!is_miss(mrg$ENDT[i])) { fail[i] <- TRUE; next }
        if (is_miss(mrg$ENTPT[i]) || !valid_iso_date(mrg$ENTPT[i]) || s_ent[i] != mrg$RFE[i]) fail[i] <- TRUE
      } else {
        if (!is_miss(mrg$ENTPT[i])) fail[i] <- TRUE
      }
    }
    n_cm02 <- sum(fail)
  }
}

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
  N_FAIL = as.integer(c(n_adsl01,n_adae01,n_advs01,n_adlb01,n_advs02,n_adlb02,n_dm01,
             n_ae01,n_ex01,n_lb01,n_vs01,n_adsl02,n_adsl03,n_adsl04,
             n_adtte01,n_adtte02,n_adtte03,
             n_eg01,n_eg02,n_eg03,n_eg04,
             n_mh01,n_mh02,n_mh03,n_mh04,n_dm02,
             n_sv01,n_sv02,n_sv03,n_ds01,n_ds02,
             n_dm03,n_dm04,n_adae02,n_adae03,
             n_cm01,n_ds03,n_seq01,n_cm02)),
  stringsAsFactors = FALSE
)
checks$STATUS <- ifelse(checks$N_FAIL == 0, "PASS", "FAIL")
qc_report <- checks
cat(sprintf("QC complete: %d checks, %d failed\n", nrow(qc_report), sum(qc_report$STATUS=="FAIL")))

# Write dataset exactly as SAS adam.qc_report (39 rows, 5 cols)
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names = FALSE, na = "")

lines <- c("Automated Quality Control (QC) Validation Report", "",
  sprintf("%-10s %-6s %-60s %6s %-6s", "CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS"),
  paste(rep("-", 95), collapse=""))
for (i in seq_len(nrow(qc_report))) {
  lines <- c(lines, sprintf("%-10s %-6s %-60s %6d %-6s",
    qc_report$CHECK_ID[i], qc_report$DOMAIN[i], substr(qc_report$RULE[i],1,60),
    qc_report$N_FAIL[i], qc_report$STATUS[i]))
}
lines <- c(lines, "", sprintf("Total checks: %d   Failed: %d", nrow(qc_report), sum(qc_report$STATUS=="FAIL")))
writeLines(lines, file.path(out, "qc_report.txt"))
writeLines(lines, file.path(out, "P019.txt"))
write.csv(qc_report, file.path(out, "P019.csv"), row.names = FALSE, na = "")
cat("outputs written\n")
