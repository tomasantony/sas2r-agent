```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

adam_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"
raw_path  <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw"

round_half_up <- function(x, d = 0) {
  p <- 10^d
  sign(x) * floor(abs(x) * p + 0.5) / p
}
strip <- function(x) {
  if (is.null(x)) return(x)
  trimws(as.character(x), which = "both")
}
up_strip <- function(x) toupper(strip(x))
is_miss_c <- function(x) {
  if (is.null(x)) return(rep(TRUE, 1))
  is.na(x) | trimws(as.character(x)) == ""
}
is_miss_n <- function(x) {
  is.na(x)
}
to_Date <- function(x) {
  if (is.null(x)) return(as.Date(rep(NA, 1)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  if (is.character(x)) {
    suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  } else {
    suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
  }
}
parse_yymmdd10 <- function(x) {
  suppressWarnings(as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d"))
}
iso_ok <- function(x) {
  # SAS: prxmatch('/^\\d{4}-\\d{2}-\\d{2}$/', strip(x)) ne 0
  s <- strip(x)
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", s)
  ok[is.na(ok)] <- FALSE
  ok[is_miss_c(x)] <- FALSE
  ok
}
sas_age_completed <- function(brth, rfic) {
  # floor(yrdif(BRTHDT, RFICDT, 'AGE')) = completed years
  b <- to_Date(brth)
  r <- to_Date(rfic)
  yb <- as.integer(format(b, "%Y")); mb <- as.integer(format(b, "%m")); db <- as.integer(format(b, "%d"))
  yr <- as.integer(format(r, "%Y")); mr <- as.integer(format(r, "%m")); dr <- as.integer(format(r, "%d"))
  age <- yr - yb - as.integer((mr < mb) | (mr == mb & dr < db))
  age[is.na(b) | is.na(r)] <- NA_integer_
  age
}

read_lib <- function(mem, libpath) {
  memU <- toupper(mem)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  if (length(files) == 0) stop(paste0("No files in ", libpath))
  base <- toupper(tools::file_path_sans_ext(basename(files)))
  idx <- which(base == memU)
  if (length(idx) == 0) stop(paste0("Dataset not found: ", mem, " in ", libpath))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "sas7bdat") df <- haven::read_sas(f)
  else if (ext == "xpt") df <- haven::read_xpt(f)
  else if (ext == "csv") df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  else stop(paste0("Unknown ext: ", f))
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- toupper(names(df))
  # zap haven labels/formats
  for (nm in names(df)) {
    cl <- class(df[[nm]])
    if (any(cl %in% c("haven_labelled", "haven_labelled_spss"))) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # convert haven Date/POSIX already; keep
  # ensure character factors to character
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

message("Reading ADaM/SDTM...")
adsl  <- read_lib("ADSL", adam_path)
adae  <- read_lib("ADAE", adam_path)
advs  <- read_lib("ADVS", adam_path)
adlb  <- read_lib("ADLB", adam_path)
adtte <- read_lib("ADTTE", adam_path)
dm <- read_lib("DM", sdtm_path)
ae <- read_lib("AE", sdtm_path)
ex <- read_lib("EX", sdtm_path)
lb <- read_lib("LB", sdtm_path)
vs <- read_lib("VS", sdtm_path)
eg <- read_lib("EG", sdtm_path)
mh <- read_lib("MH", sdtm_path)
sv <- read_lib("SV", sdtm_path)
ds <- read_lib("DS", sdtm_path)
cm <- read_lib("CM", sdtm_path)

# helper to get column or NA vector
getcol <- function(df, nm) {
  if (toupper(nm) %in% names(df)) df[[toupper(nm)]] else rep(NA, nrow(df))
}

# RULE 1 ADSL-001
n_adsl01 <- 0
if ("USUBJID" %in% names(adsl)) {
  t <- table(strip(getcol(adsl,"USUBJID")), useNA = "no")
  # SAS missing sorts first and groups; blank USUBJID would group too. table excludes NA; handle blanks as group
  n_adsl01 <- sum(t > 1)
}
chk_adsl01 <- data.frame(CHECK_ID="ADSL-001", DOMAIN="ADSL", RULE="USUBJID must be perfectly unique", N_FAIL=as.integer(n_adsl01), stringsAsFactors=FALSE)

# RULE 2 ADAE-001
aest <- to_Date(getcol(adae,"AESTDT")); aeend <- to_Date(getcol(adae,"AEENDT"))
n_adae01 <- sum(!is.na(aest) & !is.na(aeend) & aeend < aest)
chk_adae01 <- data.frame(CHECK_ID="ADAE-001", DOMAIN="ADAE", RULE="AEENDT must be >= AESTDT", N_FAIL=as.integer(n_adae01), stringsAsFactors=FALSE)

# RULE 3 ADVS-001
n_advs01 <- 0L
if (all(c("USUBJID","PARAMCD","ABLFL") %in% names(advs))) {
  sub <- advs[strip(advs$ABLFL)=="Y" & !is.na(strip(advs$ABLFL)), , drop=FALSE]
  if (nrow(sub)>0) {
    g <- aggregate(list(CNT=rep(1,nrow(sub))), by=list(USUBJID=strip(sub$USUBJID), PARAMCD=strip(sub$PARAMCD)), FUN=length)
    n_advs01 <- sum(g$CNT > 1)
  }
}
chk_advs01 <- data.frame(CHECK_ID="ADVS-001", DOMAIN="ADVS", RULE="Max 1 Baseline (ABLFL=Y) per Param", N_FAIL=as.integer(n_advs01), stringsAsFactors=FALSE)

# RULE 4 ADLB-001
n_adlb01 <- 0L
if (all(c("USUBJID","PARAMCD","ABLFL") %in% names(adlb))) {
  sub <- adlb[strip(adlb$ABLFL)=="Y" & !is.na(strip(adlb$ABLFL)), , drop=FALSE]
  if (nrow(sub)>0) {
    g <- aggregate(list(CNT=rep(1,nrow(sub))), by=list(USUBJID=strip(sub$USUBJID), PARAMCD=strip(sub$PARAMCD)), FUN=length)
    n_adlb01 <- sum(g$CNT > 1)
  }
}
chk_adlb01 <- data.frame(CHECK_ID="ADLB-001", DOMAIN="ADLB", RULE="Max 1 Baseline (ABLFL=Y) per Param", N_FAIL=as.integer(n_adlb01), stringsAsFactors=FALSE)

# RULE 5 DM-001
n_dm01 <- 0L
if ("USUBJID" %in% names(dm)) {
  t <- table(strip(getcol(dm,"USUBJID")), useNA="no")
  n_dm01 <- sum(t > 1)
}
chk_dm01 <- data.frame(CHECK_ID="SDTM-001", DOMAIN="DM", RULE="USUBJID must be perfectly unique", N_FAIL=as.integer(n_dm01), stringsAsFactors=FALSE)

# Referential helper
ref_fail <- function(child, parent, key="USUBJID") {
  ck <- strip(getcol(child, key))
  pk <- strip(getcol(parent, key))
  # SAS left join on exact (case-sensitive, trailing blanks ignored via padding -> use strip? use exact trimmed trailing? use strip both trailing? We'll use strip trailing+leading via trimws? Keep strip)
  # missing child key cannot match -> fail
  sum(!(ck %in% pk) | is_miss_c(getcol(child,key)))
}

chk_ae01 <- data.frame(CHECK_ID="SDTM-002", DOMAIN="AE", RULE="Every AE.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(ae, dm)), stringsAsFactors=FALSE)
chk_ex01 <- data.frame(CHECK_ID="SDTM-003", DOMAIN="EX", RULE="Every EX.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(ex, dm)), stringsAsFactors=FALSE)
chk_lb01 <- data.frame(CHECK_ID="SDTM-004", DOMAIN="LB", RULE="Every LB.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(lb, dm)), stringsAsFactors=FALSE)
chk_vs01 <- data.frame(CHECK_ID="SDTM-005", DOMAIN="VS", RULE="Every VS.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(vs, dm)), stringsAsFactors=FALSE)

# RULE 10 ADSL-002
n_adsl02 <- 0L
{
  saff <- strip(getcol(adsl,"SAFFL"))=="Y"
  saff[is.na(saff)] <- FALSE
  adsl_saf <- strip(getcol(adsl,"USUBJID"))[saff]
  ex_ids <- unique(strip(getcol(ex,"USUBJID")))
  n_adsl02 <- sum(!(adsl_saf %in% ex_ids))
}
chk_adsl02 <- data.frame(CHECK_ID="ADSL-002", DOMAIN="ADSL", RULE="SAFFL=Y requires exposure in EX", N_FAIL=as.integer(n_adsl02), stringsAsFactors=FALSE)

# RULE 11 ADTTE-001
{
  aval <- suppressWarnings(as.numeric(getcol(adtte,"AVAL")))
  n1 <- sum(is.na(aval) | aval < 1)
}
chk_adtte01 <- data.frame(CHECK_ID="ADTTE-001", DOMAIN="ADTTE", RULE="Survival time (AVAL) must be >= 1", N_FAIL=as.integer(n1), stringsAsFactors=FALSE)

# ADTTE-002
{
  trtsdt <- to_Date(getcol(adtte,"TRTSDT"))
  eventdt <- to_Date(getcol(adtte,"EVENTDT"))
  eosdt <- to_Date(getcol(adtte,"EOSDT"))
  aval <- suppressWarnings(as.numeric(getcol(adtte,"AVAL")))
  cnsr <- suppressWarnings(as.numeric(getcol(adtte,"CNSR")))
  miss_trt <- is.na(trtsdt)
  miss_cnsr <- is.na(cnsr)
  bad_cnsr <- !is.na(cnsr) & !(cnsr %in% c(0,1))
  # CNSR=0 branch
  exp0 <- suppressWarnings(as.integer(eventdt - trtsdt) + 1L)
  cond0 <- !is.na(cnsr) & cnsr==0 & (is.na(eventdt) | is.na(aval) | is.na(exp0) | aval != exp0)
  # CNSR=1 branch
  exp1 <- suppressWarnings(as.integer(eosdt - trtsdt) + 1L)
  cond1 <- !is.na(cnsr) & cnsr==1 & (!is.na(eventdt) | is.na(eosdt) | is.na(aval) | is.na(exp1) | aval != exp1)
  # need to handle NA in comparisons: aval != exp when one NA -> already covered by is.na checks
  fail <- miss_trt | miss_cnsr | bad_cnsr | cond0 | cond1
  fail[is.na(fail)] <- TRUE
  n2 <- sum(fail)
}
chk_adtte02 <- data.frame(CHECK_ID="ADTTE-002", DOMAIN="ADTTE", RULE="AVAL and CNSR agree with event or subject follow-up date", N_FAIL=as.integer(n2), stringsAsFactors=FALSE)

# ADTTE-003
{
  trtsdt <- to_Date(getcol(adtte,"TRTSDT"))
  eventdt <- to_Date(getcol(adtte,"EVENTDT"))
  eosdt <- to_Date(getcol(adtte,"EOSDT"))
  fail <- is.na(eosdt) | (!is.na(trtsdt) & eosdt < trtsdt) | (!is.na(eventdt) & ((!is.na(trtsdt) & eventdt < trtsdt) | (!is.na(eosdt) & eventdt > eosdt)))
  # SAS: missing(EOSDT) or EOSDT<TRTSDT ... if TRTSDT missing, EOSDT<missing? In SAS missing is smallest, so EOSDT<. is false when EOSDT non-missing. Our code: only compare when trtsdt non-missing. If trtsdt missing, EOSDT<TRTSDT false. But SAS where missing(EOSDT) or EOSDT<TRTSDT ... if TRTSDT missing and EOSDT non-missing, EOSDT<. = false. So keep as above.
  # If TRTSDT missing and EOSDT present, first false, second false. That's SAS.
  # For strict SAS when TRTSDT missing: EOSDT<TRTSDT uses missing value -> false. Good.
  # Also need case TRTSDT missing -> treat as not fail on that part.
  # Adjust: if trtsdt NA, second term FALSE
  fail[is.na(fail)] <- FALSE
  # but missing EOSDT already TRUE
  fail[is.na(eosdt)] <- TRUE
  n3 <- sum(fail)
}
chk_adtte03 <- data.frame(CHECK_ID="ADTTE-003", DOMAIN="ADTTE", RULE="Event and follow-up dates must be chronologically valid", N_FAIL=as.integer(n3), stringsAsFactors=FALSE)

# ADSL-003
{
  a_ids <- strip(getcol(adsl,"USUBJID"))
  d_ids <- strip(getcol(dm,"USUBJID"))
  a_eos <- to_Date(getcol(adsl,"EOSDT"))
  d_rfpend <- parse_yymmdd10(getcol(dm,"RFPENDTC"))
  # left join
  m <- match(a_ids, d_ids)
  rfp_m <- d_rfpend[m]
  orphan <- is.na(m)
  fail <- orphan | is.na(a_eos) | is.na(rfp_m) | (a_eos != rfp_m)
  # SAS: d.USUBJID is null or missing(a.EOSDT) or a.EOSDT ne input(...). If input missing and EOSDT non-missing -> ne true -> fail. Covered.
  # If both missing? missing EOSDT already true.
  n4 <- sum(fail)
}
chk_adsl03 <- data.frame(CHECK_ID="ADSL-003", DOMAIN="ADSL", RULE="EOSDT must agree with DM.RFPENDTC", N_FAIL=as.integer(n4), stringsAsFactors=FALSE)

# ADSL-004
{
  brth <- to_Date(getcol(adsl,"BRTHDT"))
  rfic <- to_Date(getcol(adsl,"RFICDT"))
  age <- suppressWarnings(as.numeric(getcol(adsl,"AGE")))
  exp_age <- sas_age_completed(brth, rfic)
  fail <- is.na(brth) | is.na(rfic) | is.na(age) | is.na(exp_age) | (age != exp_age)
  n5 <- sum(fail)
}
chk_adsl04 <- data.frame(CHECK_ID="ADSL-004", DOMAIN="ADSL", RULE="AGE must equal completed years at informed consent", N_FAIL=as.integer(n5), stringsAsFactors=FALSE)

# ADVS-002
advs02_fail <- function(df) {
  if (!all(c("USUBJID","PARAMCD","ASTDT","TRTSDT","AVAL") %in% names(df))) return(0L)
  astd <- to_Date(df$ASTDT); trts <- to_Date(df$TRTSDT); aval <- suppressWarnings(as.numeric(df$AVAL))
  elig <- !is.na(astd) & !is.na(trts) & !is.na(aval) & astd <= trts
  # SAS: where ASTDT<=TRTSDT and not missing(AVAL). If either date missing, comparison missing -> false -> excluded. Our elig handles.
  if (sum(elig)==0) return(0L)
  sub <- data.frame(USUBJID=strip(df$USUBJID), PARAMCD=strip(df$PARAMCD), ASTDT=astd, AVAL=aval, stringsAsFactors=FALSE)
  sub_e <- sub[elig, , drop=FALSE]
  # max ASTDT per group
  agg <- aggregate(ASTDT ~ USUBJID + PARAMCD, data=sub_e, FUN=function(x) max(x, na.rm=TRUE))
  names(agg)[3] <- "BASE_DT"
  agg$BASE_DT <- as.Date(agg$BASE_DT, origin="1970-01-01")
  # join back: candidates where ASTDT==BASE_DT and not missing AVAL
  # need original a rows with not missing AVAL
  cand <- sub[!is.na(sub$AVAL), , drop=FALSE]
  m <- merge(cand, agg, by=c("USUBJID","PARAMCD"))
  m <- m[m$ASTDT == m$BASE_DT, , drop=FALSE]
  if (nrow(m)==0) return(0L)
  g <- aggregate(list(CNT=rep(1,nrow(m))), by=list(USUBJID=m$USUBJID, PARAMCD=m$PARAMCD), FUN=length)
  sum(g$CNT > 1)
}
chk_advs02 <- data.frame(CHECK_ID="ADVS-002", DOMAIN="ADVS", RULE="Baseline date has exactly one eligible source record", N_FAIL=as.integer(advs02_fail(advs)), stringsAsFactors=FALSE)
chk_adlb02 <- data.frame(CHECK_ID="ADLB-002", DOMAIN="ADLB", RULE="Baseline date has exactly one eligible source record", N_FAIL=as.integer(advs02_fail(adlb)), stringsAsFactors=FALSE)

# EG checks
chk_eg01 <- data.frame(CHECK_ID="SDTM-006", DOMAIN="EG", RULE="Every EG.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(eg, dm)), stringsAsFactors=FALSE)
{
  req <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST','EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
  present <- intersect(toupper(names(eg)), req)
  n_eg02 <- 12 - length(unique(present))
}
chk_eg02 <- data.frame(CHECK_ID="SDTM-007", DOMAIN="EG", RULE="Required EG variables must exist", N_FAIL=as.integer(n_eg02), stringsAsFactors=FALSE)
{
  egdtc <- getcol(eg,"EGDTC"); stn <- suppressWarnings(as.numeric(getcol(eg,"EGSTRESN"))); stc <- getcol(eg,"EGSTRESC")
  fail <- is_miss_c(egdtc) | !iso_ok(egdtc) | is.na(stn) | is_miss_c(stc)
  n_eg03 <- sum(fail)
}
chk_eg03 <- data.frame(CHECK_ID="SDTM-008", DOMAIN="EG", RULE="EG date/value contract must be valid", N_FAIL=as.integer(n_eg03), stringsAsFactors=FALSE)
{
  leak <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
  n_eg04 <- sum(leak %in% toupper(names(eg)))
}
chk_eg04 <- data.frame(CHECK_ID="SDTM-009", DOMAIN="EG", RULE="Raw EG variables must not leak to SDTM", N_FAIL=as.integer(n_eg04), stringsAsFactors=FALSE)

chk_mh01 <- data.frame(CHECK_ID="SDTM-010", DOMAIN="MH", RULE="Every MH.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(mh, dm)), stringsAsFactors=FALSE)
{
  req <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
  n_mh02 <- 6 - length(intersect(toupper(names(mh)), req))
}
chk_mh02 <- data.frame(CHECK_ID="SDTM-011", DOMAIN="MH", RULE="Required MH variables must exist", N_FAIL=as.integer(n_mh02), stringsAsFactors=FALSE)
{
  mhs <- getcol(mh,"MHSTDTC"); term <- getcol(mh,"MHTERM")
  fail <- is_miss_c(mhs) | !iso_ok(mhs) | is_miss_c(term)
  n_mh03 <- sum(fail)
}
chk_mh03 <- data.frame(CHECK_ID="SDTM-012", DOMAIN="MH", RULE="MH date/term contract must be valid", N_FAIL=as.integer(n_mh03), stringsAsFactors=FALSE)
{
  leak <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')
  n_mh04 <- sum(leak %in% toupper(names(mh)))
}
chk_mh04 <- data.frame(CHECK_ID="SDTM-013", DOMAIN="MH", RULE="Raw MH variables must not leak to SDTM", N_FAIL=as.integer(n_mh04), stringsAsFactors=FALSE)

# DM-014
{
  rfic <- getcol(dm,"RFICDTC"); rfs <- getcol(dm,"RFSTDTC"); rfe <- getcol(dm,"RFENDTC"); rfp <- getcol(dm,"RFPENDTC")
  fail <- is_miss_c(rfic) | is_miss_c(rfs) | is_miss_c(rfe) | is_miss_c(rfp) |
    !iso_ok(rfic) | !iso_ok(rfs) | !iso_ok(rfe) | !iso_ok(rfp) |
    (strip(rfs) < strip(rfic)) | (strip(rfe) < strip(rfs)) | (strip(rfp) != strip(rfe))
  # SAS character comparison: need to handle NA -> comparison NA; but missing already true. For non-missing, compare strings.
  fail[is.na(fail)] <- TRUE
  n_dm02 <- sum(fail)
}
chk_dm02 <- data.frame(CHECK_ID="SDTM-014", DOMAIN="DM", RULE="DM reference dates complete and ordered", N_FAIL=as.integer(n_dm02), stringsAsFactors=FALSE)

chk_sv01 <- data.frame(CHECK_ID="SDTM-015", DOMAIN="SV", RULE="Every SV.USUBJID exists in DM", N_FAIL=as.integer(ref_fail(sv, dm)), stringsAsFactors=FALSE)
{
  if (all(c("USUBJID","VISITNUM") %in% names(sv))) {
    # SAS groups missing together; count(*) ne 1 means >1 (since min 1). Use paste key with NA handling.
    key <- paste0(strip(getcol(sv,"USUBJID")), "|", ifelse(is.na(suppressWarnings(as.numeric(getcol(sv,"VISITNUM")))), "_MISS_", as.character(suppressWarnings(as.numeric(getcol(sv,"VISITNUM"))))))
    # better: use raw VISITNUM string? SAS numeric missing groups. Use is.na.
    t <- table(key, useNA="no")
    n_sv02 <- sum(t != 1)
  } else n_sv02 <- nrow(sv)
}
chk_sv02 <- data.frame(CHECK_ID="SDTM-016", DOMAIN="SV", RULE="USUBJID and VISITNUM must be unique", N_FAIL=as.integer(n_sv02), stringsAsFactors=FALSE)
{
  # inner join sv to dm
  sv_ids <- strip(getcol(sv,"USUBJID")); dm_ids <- strip(getcol(dm,"USUBJID"))
  m <- match(sv_ids, dm_ids)
  keep <- !is.na(m)
  sv2 <- sv[keep, , drop=FALSE]; m2 <- m[keep]
  d_rfs <- strip(getcol(dm,"RFSTDTC"))[m2]; d_rfe <- strip(getcol(dm,"RFENDTC"))[m2]
  vn <- suppressWarnings(as.numeric(getcol(sv2,"VISITNUM")))
  vst <- getcol(sv2,"VISIT"); sstd <- getcol(sv2,"SVSTDTC"); send <- getcol(sv2,"SVENDTC")
  fail <- is.na(vn) | is_miss_c(vst) | is_miss_c(sstd) | is_miss_c(send) |
    !iso_ok(sstd) | !iso_ok(send) |
    (strip(send) < strip(sstd)) |
    (strip(sstd) < d_rfs) | (strip(send) > d_rfe)
  fail[is.na(fail)] <- TRUE
  # rows with no DM match are excluded (inner join)
  n_sv03 <- sum(fail)
}
chk_sv03 <- data.frame(CHECK_ID="SDTM-017", DOMAIN="SV", RULE="SV dates valid and inside reference period", N_FAIL=as.integer(n_sv03), stringsAsFactors=FALSE)

# DS-018
{
  d_ids <- unique(strip(getcol(dm,"USUBJID")))
  s_ids <- strip(getcol(ds,"USUBJID"))
  # count per DM subject
  tab_s <- table(s_ids, useNA="no")
  n_ds_fail <- 0L
  for (id in d_ids) {
    c <- ifelse(id %in% names(tab_s), as.integer(tab_s[id]), 0L)
    if (is.na(c)) c <- 0L
    if (c != 1L) n_ds_fail <- n_ds_fail + 1L
  }
  # orphans: distinct s_ids not in d_ids
  orph <- setdiff(unique(s_ids[!is_miss_c(s_ids)]), d_ids)
  # SAS second select groups orphans by USUBJID -> each orphan id counts 1
  # also what about blank s USUBJID? blank not in DM -> would be orphan group? In SAS, blank join? d.USUBJID is null when s blank? blank not in DM (DM blanks unlikely) -> counts. Our orph excludes blanks via is_miss. Need to include? SAS: select s.USUBJID ... where d null group by s.USUBJID -> blank group counts. So include blank as one.
  blank_orph <- any(is_miss_c(s_ids))
  # if blank exists and blank not in d_ids (d_ids likely not blank), then blank group is orphan
  if (blank_orph) {
    # check if blank in d_ids
    if (!("" %in% d_ids) && !any(is.na(d_ids))) orph <- c(orph, "_BLANK_")
  }
  n_ds01 <- n_ds_fail + length(orph)
}
chk_ds01 <- data.frame(CHECK_ID="SDTM-018", DOMAIN="DS", RULE="Exactly one final DS record per DM subject", N_FAIL=as.integer(n_ds01), stringsAsFactors=FALSE)
{
  sv_ids <- strip(getcol(ds,"USUBJID")); dm_ids <- strip(getcol(dm,"USUBJID"))
  m <- match(sv_ids, dm_ids)
  keep <- !is.na(m)
  ds2 <- ds[keep, , drop=FALSE]; m2 <- m[keep]
  d_rfp <- strip(getcol(dm,"RFPENDTC"))[m2]
  decod <- getcol(ds2,"DSDECOD"); dsdtc <- getcol(ds2,"DSSTDTC")
  fail <- is_miss_c(decod) | is_miss_c(dsdtc) | !iso_ok(dsdtc) | (strip(dsdtc) != d_rfp)
  fail[is.na(fail)] <- TRUE
  n_ds02 <- sum(fail)
}
chk_ds02 <- data.frame(CHECK_ID="SDTM-019", DOMAIN="DS", RULE="DS date matches DM participation end", N_FAIL=as.integer(n_ds02), stringsAsFactors=FALSE)
{
  race <- up_strip(getcol(dm,"RACE"))
  allowed <- c('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')
  fail <- is_miss_c(getcol(dm,"RACE")) | !(race %in% allowed)
  n_dm03 <- sum(fail)
}
chk_dm03 <- data.frame(CHECK_ID="SDTM-020", DOMAIN="DM", RULE="RACE uses supported CDISC terminology", N_FAIL=as.integer(n_dm03), stringsAsFactors=FALSE)
{
  site <- strip(getcol(dm,"SITEID")); subj <- strip(getcol(dm,"SUBJID"))
  first_tok <- sapply(strsplit(subj, "-", fixed=TRUE), function(x) if(length(x)>=1) x[1] else "")
  fail <- is_miss_c(site) | (site != first_tok)
  fail[is.na(fail)] <- TRUE
  n_dm04 <- sum(fail)
}
chk_dm04 <- data.frame(CHECK_ID="SDTM-021", DOMAIN="DM", RULE="SITEID present and consistent with SUBJID", N_FAIL=as.integer(n_dm04), stringsAsFactors=FALSE)
{
  aocc <- strip(getcol(adae,"AOCCFL")); trt <- strip(getcol(adae,"TRTEMFL"))
  # (not missing AOCCFL and AOCCFL ne 'Y') or (AOCCFL='Y' and TRTEMFL ne 'Y')
  cond1 <- !is_miss_c(getcol(adae,"AOCCFL")) & (aocc != "Y")
  cond1[is.na(cond1)] <- FALSE
  cond2 <- !is.na(aocc) & aocc=="Y" & (is_miss_c(getcol(adae,"TRTEMFL")) | trt != "Y")
  cond2[is.na(cond2)] <- FALSE
  # need to handle NA aocc: is_miss true -> cond1 false; aocc==Y with NA -> NA -> false
  n_ae02 <- sum(cond1 | cond2)
}
chk_adae02 <- data.frame(CHECK_ID="ADAE-002", DOMAIN="ADAE", RULE="AOCCFL valid and restricted to TRTEMFL=Y", N_FAIL=as.integer(n_ae02), stringsAsFactors=FALSE)
{
  if (all(c("USUBJID","AEDECOD","TRTEMFL","AOCCFL") %in% names(adae))) {
    df2 <- data.frame(USUBJID=strip(adae$USUBJID), AEDECOD=strip(adae$AEDECOD),
                      TRTEMFL=strip(adae$TRTEMFL), AOCCFL=strip(adae$AOCCFL), stringsAsFactors=FALSE)
    df2$TRTEMFL[is.na(df2$TRTEMFL)] <- ""
    df2$AOCCFL[is.na(df2$AOCCFL)] <- ""
    agg <- aggregate(cbind(TE=df2$TRTEMFL=="Y", AO=df2$AOCCFL=="Y") ~ USUBJID + AEDECOD, data=df2, FUN=sum)
    fail_g <- agg$TE > 0 & agg$AO != 1
    n_ae03 <- sum(fail_g)
  } else n_ae03 <- 0L
}
chk_adae03 <- data.frame(CHECK_ID="ADAE-003", DOMAIN="ADAE", RULE="One AOCCFL=Y per subject and TEAE term", N_FAIL=as.integer(n_ae03), stringsAsFactors=FALSE)
{
  c_ids <- strip(getcol(cm,"USUBJID")); d_ids <- strip(getcol(dm,"USUBJID"))
  m <- match(c_ids, d_ids)
  in_dm <- !is.na(m)
  trt <- getcol(cm,"CMTRT"); dec <- getcol(cm,"CMDECOD")
  dose <- suppressWarnings(as.numeric(getcol(cm,"CMDOSE"))); dosu <- getcol(cm,"CMDOSU")
  sdtc <- getcol(cm,"CMSTDTC"); endtc <- getcol(cm,"CMENDTC"); enrt <- getcol(cm,"CMENRTPT")
  # SAS exact 'ONGOING' case-sensitive
  enrt_raw <- as.character(enrt); enrt_raw[is.na(enrt_raw)] <- ""
  fail <- (!in_dm) |
    is_miss_c(trt) | is_miss_c(dec) |
    (toupper(strip(trt))=="PARACETAMOL" & toupper(strip(dec))!="ACETAMINOPHEN") |
    is.na(dose) | is_miss_c(dosu) |
    is_miss_c(sdtc) | !iso_ok(sdtc) |
    (!is_miss_c(endtc) & !iso_ok(endtc)) |
    (!is_miss_c(endtc) & (strip(endtc) < strip(sdtc))) |
    (enrt_raw=="ONGOING" & !is_miss_c(endtc)) |
    (enrt_raw!="ONGOING" & is_miss_c(endtc))
  fail[is.na(fail)] <- TRUE
  n_cm01 <- sum(fail)
}
chk_cm01 <- data.frame(CHECK_ID="SDTM-022", DOMAIN="CM", RULE="CM subject, medication, dose and dates must be valid", N_FAIL=as.integer(n_cm01), stringsAsFactors=FALSE)
{
  allowed <- c('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT','ADVERSE EVENT','SCREEN FAILURE')
  dec <- up_strip(getcol(ds,"DSDECOD"))
  fail <- !(dec %in% allowed)
  fail[is.na(fail)] <- TRUE
  # SAS: where upcase(strip(DSDECOD)) not in (...) ; missing (blank->"") not in -> fail. Covered ("" not in allowed)
  n_ds03 <- sum(fail)
}
chk_ds03 <- data.frame(CHECK_ID="SDTM-023", DOMAIN="DS", RULE="DSDECOD uses supported disposition terminology", N_FAIL=as.integer(n_ds03), stringsAsFactors=FALSE)
{
  dup_count <- function(df, seqnm) {
    if (!all(c("USUBJID",seqnm) %in% names(df))) return(0L)
    u <- strip(getcol(df,"USUBJID"))
    s <- suppressWarnings(as.numeric(getcol(df,seqnm)))
    # SAS groups numeric missing together; keep NA as group
    key <- paste0(u, "|", ifelse(is.na(s), "_MISS_", as.character(s)))
    # blank USUBJID? keep as is
    t <- table(key, useNA="no")
    sum(t > 1)
  }
  n_seq <- dup_count(ae,"AESEQ") + dup_count(ex,"EXSEQ") + dup_count(lb,"LBSEQ") + dup_count(vs,"VSSEQ") + dup_count(cm,"CMSEQ") + dup_count(mh,"MHSEQ") + dup_count(eg,"EGSEQ")
}
chk_seq01 <- data.frame(CHECK_ID="SDTM-024", DOMAIN="SDTM", RULE="Domain sequence keys must be unique per subject", N_FAIL=as.integer(n_seq), stringsAsFactors=FALSE)
{
  c_ids <- strip(getcol(cm,"USUBJID")); d_ids <- strip(getcol(dm,"USUBJID"))
  m <- match(c_ids, d_ids)
  keep <- !is.na(m)
  cm2 <- cm[keep, , drop=FALSE]; m2 <- m[keep]
  d_rfe <- strip(getcol(dm,"RFENDTC"))[m2]
  enrt <- up_strip(getcol(cm2,"CMENRTPT"))
  endtc <- getcol(cm2,"CMENDTC"); entpt <- getcol(cm2,"CMENTPT")
  # if CMENTPT column missing, treat as missing
  if (!("CMENTPT" %in% names(cm2))) entpt <- rep(NA, nrow(cm2))
  cond_badval <- !(enrt %in% c("","ONGOING"))
  cond_badval[is.na(cond_badval)] <- TRUE
  cond_ong <- (enrt=="ONGOING") & (!is_miss_c(endtc) | is_miss_c(entpt) | !iso_ok(entpt) | (strip(entpt) != d_rfe))
  cond_ong[is.na(cond_ong)] <- TRUE
  # need to handle NA enrt: enrt NA -> up_strip NA -> cond_badval? up_strip(NA)="NA"? Actually strip(NA)="NA"? Let's fix: is_miss -> treat as ""
  miss_enrt <- is_miss_c(getcol(cm2,"CMENRTPT"))
  cond_badval[miss_enrt] <- FALSE  # "" is allowed
  cond_ong[miss_enrt] <- FALSE
  # for NA enrt, third condition: (upcase ne 'ONGOING' and not missing CMENTPT) -> NA ne ONGOING? In SAS missing ne ONGOING true. So if enrt missing and CMENTPT non-missing -> fail.
  cond_non <- (!miss_enrt & enrt!="ONGOING" & !is_miss_c(entpt)) | (miss_enrt & !is_miss_c(entpt))
  cond_non[is.na(cond_non)] <- FALSE
  # also for enrt NA that equals? handle
  fail <- cond_badval | cond_ong | cond_non
  n_cm02 <- sum(fail)
}
chk_cm02 <- data.frame(CHECK_ID="SDTM-025", DOMAIN="CM", RULE="Ongoing CM must be anchored to the DM reference end", N_FAIL=as.integer(n_cm02), stringsAsFactors=FALSE)

qc_report <- bind_rows(chk_adsl01, chk_adae01, chk_advs01, chk_adlb01, chk_advs02, chk_adlb02, chk_dm01,
  chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04,
  chk_adtte01, chk_adtte02, chk_adtte03,
  chk_eg01, chk_eg02, chk_eg03, chk_eg04,
  chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02,
  chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02,
  chk_dm03, chk_dm04, chk_adae02, chk_adae03,
  chk_cm01, chk_ds03, chk_seq01, chk_cm02)
qc_report$STATUS <- ifelse(qc_report$N_FAIL==0, "PASS", "FAIL")

n_checks <- nrow(qc_report)
n_failed <- sum(qc_report$STATUS=="FAIL")
message(paste0("QC checks: ", n_checks, " failed: ", n_failed))
if (n_failed > 0) message("ERROR: QC PIPELINE FAILED. CRITICAL DATA ERRORS DETECTED.") else message("NOTE: QC VALIDATION COMPLETE. ALL CHECKS PASSED.")

write.csv(qc_report, file.path(out, "qc_report.csv"), row.names=FALSE, na="")

# Text rendering (PROC PRINT)
hdr <- "Automated Quality Control (QC) Validation Report"
hdr2 <- "Automated Quality Control (QC) Report"
lines <- c(hdr, hdr2, "",
  sprintf("%-10s %-10s %-60s %6s %-6s", "CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS"),
  paste(rep("-", 95), collapse=""))
for (i in seq_len(nrow(qc_report))) {
  lines <- c(lines, sprintf("%-10s %-10s %-60s %6d %-6s",
    qc_report$CHECK_ID[i], qc_report$DOMAIN[i],
    substr(qc_report$RULE[i],1,60),
    qc_report$N_FAIL[i], qc_report$STATUS[i]))
}
lines <- c(lines, "", paste0("Checks: ", n_checks, "  Failed: ", n_failed))
writeLines(lines, file.path(out, "qc_report.txt"))
writeLines(lines, file.path(out, "P019.txt"))
writeLines(lines, file.path(out, "P019.lst"))
write.csv(qc_report, file.path(out, "P019.csv"), row.names=FALSE, na="")
```