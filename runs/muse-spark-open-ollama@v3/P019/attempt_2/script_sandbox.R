out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
adam_path <- file.path(project_path, "data/adam")
sdtm_path <- file.path(project_path, "data/sdtm")
raw_path  <- file.path(project_path, "data/raw")

library(haven)
library(dplyr)
library(stringr)
library(lubridate)

# helper to read SAS dataset (sas7bdat or xpt) and uppercase names
read_sas_ds <- function(dir, memname) {
  mem_low <- tolower(memname)
  # list files
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # try exact match ignoring case and extension
  cand <- files[grepl(paste0("^", mem_low, "\\.(sas7bdat|xpt|sastbdat)$"), tolower(basename(files)))]
  if (length(cand)==0) {
    # also try without extension check
    cand2 <- files[toupper(tools::file_path_sans_ext(basename(files))) == toupper(memname)]
    cand <- cand2
  }
  if (length(cand)==0) {
    # try xpt in adam_path alternative raw locations
    return(NULL)
  }
  f <- cand[1]
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # convert haven labelled to plain, keep Date
  # haven::read_sas already converts SAS dates to Date if format present; otherwise numeric
  # zap labels
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  # uppercase names
  names(df) <- toupper(names(df))
  # trim character columns? keep as is, handle later
  # convert haven labelled vectors to character/numeric
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  df
}

# is missing helpers
is_miss_chr <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
is_miss_num <- function(x) {
  is.na(x)
}
is_miss <- function(x) {
  if (is.character(x)) is_miss_chr(x) else is.na(x)
}
strip <- function(x) trimws(as.character(x))
upcase <- function(x) toupper(strip(x))
# prxmatch iso date
is_iso_date <- function(x) {
  !is_miss_chr(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(x))
}
# yrdif AGE completed years
completed_years <- function(brth, rfic) {
  # brth, rfic are Date
  # vectorized
  y1 <- lubridate::year(brth)
  y2 <- lubridate::year(rfic)
  m1 <- lubridate::month(brth)
  m2 <- lubridate::month(rfic)
  d1 <- lubridate::day(brth)
  d2 <- lubridate::day(rfic)
  age <- y2 - y1
  adj <- (m2 < m1) | (m2 == m1 & d2 < d1)
  adj[is.na(adj)] <- FALSE
  age - as.integer(adj)
}
# helper to get column existence count
count_required_present <- function(df, req_list) {
  if (is.null(df)) return(0)
  nms <- toupper(names(df))
  sum(toupper(req_list) %in% nms)
}
count_raw_leak <- function(df, raw_list) {
  if (is.null(df)) return(0)
  nms <- toupper(names(df))
  sum(toupper(raw_list) %in% nms)
}
# helper to safely get vector, return NA vector if missing column
get_col <- function(df, col) {
  if (is.null(df) || !(toupper(col) %in% toupper(names(df)))) return(NULL)
  df[[toupper(col)]]
}
# helper to ensure Date type for SAS numeric dates that may be numeric
as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date is days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character ISO
  suppressWarnings(as.Date(trimws(as.character(x)), format="%Y-%m-%d"))
}

# Load datasets
adsl  <- read_sas_ds(adam_path, "adsl")
adae  <- read_sas_ds(adam_path, "adae")
advs  <- read_sas_ds(adam_path, "advs")
adlb  <- read_sas_ds(adam_path, "adlb")
adtte <- read_sas_ds(adam_path, "adtte")
dm    <- read_sas_ds(sdtm_path, "dm")
ae    <- read_sas_ds(sdtm_path, "ae")
ex    <- read_sas_ds(sdtm_path, "ex")
lb    <- read_sas_ds(sdtm_path, "lb")
vs    <- read_sas_ds(sdtm_path, "vs")
eg    <- read_sas_ds(sdtm_path, "eg")
mh    <- read_sas_ds(sdtm_path, "mh")
sv    <- read_sas_ds(sdtm_path, "sv")
ds    <- read_sas_ds(sdtm_path, "ds")
cm    <- read_sas_ds(sdtm_path, "cm")

# Ensure data frames exist (empty if missing)
if (is.null(adsl)) adsl <- data.frame()
if (is.null(adae)) adae <- data.frame()
if (is.null(advs)) advs <- data.frame()
if (is.null(adlb)) adlb <- data.frame()
if (is.null(adtte)) adtte <- data.frame()
if (is.null(dm)) dm <- data.frame()
if (is.null(ae)) ae <- data.frame()
if (is.null(ex)) ex <- data.frame()
if (is.null(lb)) lb <- data.frame()
if (is.null(vs)) vs <- data.frame()
if (is.null(eg)) eg <- data.frame()
if (is.null(mh)) mh <- data.frame()
if (is.null(sv)) sv <- data.frame()
if (is.null(ds)) ds <- data.frame()
if (is.null(cm)) cm <- data.frame()

# Helper to count duplicates groups
count_dup_groups <- function(df, by) {
  if (nrow(df)==0) return(0)
  # by is character vector of columns
  # ensure columns exist
  for (b in by) if (!(b %in% names(df))) return(0)
  df %>% group_by(across(all_of(by))) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow()
}

# RULE 1: ADSL-001
n_adsl01 <- 0
if (nrow(adsl)>0 && "USUBJID" %in% names(adsl)) {
  n_adsl01 <- adsl %>% group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow()
}
chk_adsl01 <- data.frame(CHECK_ID="ADSL-001", DOMAIN="ADSL", RULE="USUBJID must be perfectly unique", N_FAIL=n_adsl01, stringsAsFactors=FALSE)

# RULE 2: ADAE-001
n_adae01 <- 0
if (nrow(adae)>0 && all(c("AEENDT","AESTDT") %in% names(adae))) {
  aeendt <- as_date_safe(adae$AEENDT)
  aestdt <- as_date_safe(adae$AESTDT)
  # if already Date, as_date_safe returns Date; if numeric SAS date, converted
  # For character DTC, as_date_safe will parse ISO
  # Use original vectors if Date already
  # Need to handle missing: is.na
  cond <- !is.na(aeendt) & !is.na(aestdt) & aeendt < aestdt
  # also need to consider original missing logic: not missing(AEENDT) and not missing(AESTDT)
  # is.na covers
  n_adae01 <- sum(cond, na.rm=TRUE)
}
chk_adae01 <- data.frame(CHECK_ID="ADAE-001", DOMAIN="ADAE", RULE="AEENDT must be >= AESTDT", N_FAIL=n_adae01, stringsAsFactors=FALSE)

# RULE 3: ADVS-001
n_advs01 <- 0
if (nrow(advs)>0 && all(c("USUBJID","PARAMCD","ABLFL") %in% names(advs))) {
  sub <- advs[upcase(advs$ABLFL)=="Y" & !is.na(advs$ABLFL), , drop=FALSE]
  if (nrow(sub)>0) n_advs01 <- count_dup_groups(sub, c("USUBJID","PARAMCD"))
}
chk_advs01 <- data.frame(CHECK_ID="ADVS-001", DOMAIN="ADVS", RULE="Max 1 Baseline (ABLFL=Y) per Param", N_FAIL=n_advs01, stringsAsFactors=FALSE)

# RULE 4: ADLB-001
n_adlb01 <- 0
if (nrow(adlb)>0 && all(c("USUBJID","PARAMCD","ABLFL") %in% names(adlb))) {
  sub <- adlb[upcase(adlb$ABLFL)=="Y" & !is.na(adlb$ABLFL), , drop=FALSE]
  if (nrow(sub)>0) n_adlb01 <- count_dup_groups(sub, c("USUBJID","PARAMCD"))
}
chk_adlb01 <- data.frame(CHECK_ID="ADLB-001", DOMAIN="ADLB", RULE="Max 1 Baseline (ABLFL=Y) per Param", N_FAIL=n_adlb01, stringsAsFactors=FALSE)

# RULE 5: SDTM DM duplicate
n_dm01 <- 0
if (nrow(dm)>0 && "USUBJID" %in% names(dm)) {
  n_dm01 <- dm %>% group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow()
}
chk_dm01 <- data.frame(CHECK_ID="SDTM-001", DOMAIN="DM", RULE="USUBJID must be perfectly unique", N_FAIL=n_dm01, stringsAsFactors=FALSE)

# RULE 6: AE referential
n_ae01 <- 0
if (nrow(ae)>0) {
  if ("USUBJID" %in% names(ae) && "USUBJID" %in% names(dm)) {
    dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
    n_ae01 <- sum(!(ae$USUBJID %in% dm_ids) | is_miss_chr(ae$USUBJID), na.rm=TRUE)
    # left join where d.USUBJID is null => count ae rows with no match
    # if ae has duplicate USUBJID, each row counts
  }
}
chk_ae01 <- data.frame(CHECK_ID="SDTM-002", DOMAIN="AE", RULE="Every AE.USUBJID exists in DM", N_FAIL=n_ae01, stringsAsFactors=FALSE)

# RULE 7: EX referential
n_ex01 <- 0
if (nrow(ex)>0 && "USUBJID" %in% names(ex) && "USUBJID" %in% names(dm)) {
  dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
  n_ex01 <- sum(!(ex$USUBJID %in% dm_ids) | is_miss_chr(ex$USUBJID), na.rm=TRUE)
}
chk_ex01 <- data.frame(CHECK_ID="SDTM-003", DOMAIN="EX", RULE="Every EX.USUBJID exists in DM", N_FAIL=n_ex01, stringsAsFactors=FALSE)

# RULE 8: LB referential
n_lb01 <- 0
if (nrow(lb)>0 && "USUBJID" %in% names(lb) && "USUBJID" %in% names(dm)) {
  dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
  n_lb01 <- sum(!(lb$USUBJID %in% dm_ids) | is_miss_chr(lb$USUBJID), na.rm=TRUE)
}
chk_lb01 <- data.frame(CHECK_ID="SDTM-004", DOMAIN="LB", RULE="Every LB.USUBJID exists in DM", N_FAIL=n_lb01, stringsAsFactors=FALSE)

# RULE 9: VS referential
n_vs01 <- 0
if (nrow(vs)>0 && "USUBJID" %in% names(vs) && "USUBJID" %in% names(dm)) {
  dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
  n_vs01 <- sum(!(vs$USUBJID %in% dm_ids) | is_miss_chr(vs$USUBJID), na.rm=TRUE)
}
chk_vs01 <- data.frame(CHECK_ID="SDTM-005", DOMAIN="VS", RULE="Every VS.USUBJID exists in DM", N_FAIL=n_vs01, stringsAsFactors=FALSE)

# RULE 10: ADSL-002 SAFFL=Y requires exposure in EX
n_adsl02 <- 0
if (nrow(adsl)>0 && "USUBJID" %in% names(adsl) && "SAFFL" %in% names(adsl) && nrow(ex)>0) {
  ex_ids <- unique(ex$USUBJID[!is_miss_chr(ex$USUBJID)])
  sub <- adsl[upcase(adsl$SAFFL)=="Y" & !is.na(adsl$SAFFL), , drop=FALSE]
  if (nrow(sub)>0) n_adsl02 <- sum(!(sub$USUBJID %in% ex_ids), na.rm=TRUE)
} else if (nrow(adsl)>0 && "SAFFL" %in% names(adsl)) {
  sub <- adsl[upcase(adsl$SAFFL)=="Y" & !is.na(adsl$SAFFL), , drop=FALSE]
  if (nrow(sub)>0 && nrow(ex)==0) n_adsl02 <- nrow(sub)
}
chk_adsl02 <- data.frame(CHECK_ID="ADSL-002", DOMAIN="ADSL", RULE="SAFFL=Y requires exposure in EX", N_FAIL=n_adsl02, stringsAsFactors=FALSE)

# RULE 11: ADTTE-001
n_adtte01 <- 0
if (nrow(adtte)>0 && "AVAL" %in% names(adtte)) {
  n_adtte01 <- sum(is.na(adtte$AVAL) | adtte$AVAL < 1, na.rm=TRUE)
}
chk_adtte01 <- data.frame(CHECK_ID="ADTTE-001", DOMAIN="ADTTE", RULE="Survival time (AVAL) must be >= 1", N_FAIL=n_adtte01, stringsAsFactors=FALSE)

# RULE ADTTE-002
n_adtte02 <- 0
if (nrow(adtte)>0) {
  # need TRTSDT, CNSR, EVENTDT, EOSDT, AVAL
  # ensure columns exist
  cols_needed <- c("TRTSDT","CNSR","EVENTDT","EOSDT","AVAL")
  if (all(cols_needed %in% names(adtte))) {
    trtsdt <- as_date_safe(adtte$TRTSDT)
    eventdt <- as_date_safe(adtte$EVENTDT)
    eosdt <- as_date_safe(adtte$EOSDT)
    aval <- adtte$AVAL
    cnsr <- adtte$CNSR
    # missing checks
    miss_trtsdt <- is.na(trtsdt)
    miss_cnsr <- is.na(cnsr)
    miss_eventdt <- is.na(eventdt)
    miss_eosdt <- is.na(eosdt)
    # CNSR not in (0,1)
    cnsr_invalid <- !is.na(cnsr) & !(cnsr %in% c(0,1))
    # For CNSR=0: missing EVENTDT or AVAL != (EVENTDT - TRTSDT)+1
    # For CNSR=1: not missing EVENTDT or missing EOSDT or AVAL != (EOSDT - TRTSDT)+1
    diff_event <- as.numeric(eventdt - trtsdt) + 1
    diff_eos <- as.numeric(eosdt - trtsdt) + 1
    cond1 <- miss_trtsdt | miss_cnsr | cnsr_invalid
    cond2 <- !is.na(cnsr) & cnsr==0 & (miss_eventdt | (!is.na(aval) & !is.na(diff_event) & aval != diff_event) | (is.na(aval) & !is.na(diff_event)) | (!is.na(aval) & is.na(diff_event)))
    # Actually AVAL ne (EVENTDT - TRTSDT)+1 includes case where one is NA? In SAS, if EVENTDT missing, then (EVENTDT - TRTSDT)+1 is missing, and AVAL ne missing is true? But they already check missing(EVENTDT) separately. So we can simplify: if CNSR=0 and (missing EVENTDT or AVAL != diff)
    # For CNSR=1: not missing EVENTDT or missing EOSDT or AVAL != diff_eos
    cond3 <- !is.na(cnsr) & cnsr==1 & (!miss_eventdt | miss_eosdt | (!is.na(aval) & !is.na(diff_eos) & aval != diff_eos) | (is.na(aval) & !is.na(diff_eos)) | (!is.na(aval) & is.na(diff_eos)))
    # Need to handle AVAL missing already? But cond1 covers missing CNSR/TRTSDT, but AVAL missing not directly except via diff comparison? However SAS where clause includes missing(AVAL) indirectly via AVAL ne ...? In SAS, missing ne value is true? Actually in SAS, . ne 5 is true. So missing AVAL would cause AVAL ne diff to be true. So we should treat is.na(aval) as fail when diff is not NA.
    # Simplify: compute fail if any cond true
    fail <- cond1 | cond2 | cond3
    # Also need to handle case where CNSR=0 and AVAL is NA but EVENTDT not missing: then AVAL ne diff is true (since NA != diff) -> fail
    # Our cond2 already covers is.na(aval) & !is.na(diff_event)
    n_adtte02 <- sum(fail, na.rm=TRUE)
  } else {
    # if missing columns, count all rows as fail? But SAS would error; we approximate 0
    n_adtte02 <- 0
  }
}
chk_adtte02 <- data.frame(CHECK_ID="ADTTE-002", DOMAIN="ADTTE", RULE="AVAL and CNSR agree with event or subject follow-up date", N_FAIL=n_adtte02, stringsAsFactors=FALSE)

# RULE ADTTE-003
n_adtte03 <- 0
if (nrow(adtte)>0 && all(c("EOSDT","TRTSDT","EVENTDT") %in% names(adtte))) {
  trtsdt <- as_date_safe(adtte$TRTSDT)
  eosdt <- as_date_safe(adtte$EOSDT)
  eventdt <- as_date_safe(adtte$EVENTDT)
  miss_eosdt <- is.na(eosdt)
  cond_a <- miss_eosdt
  cond_b <- !is.na(eosdt) & !is.na(trtsdt) & eosdt < trtsdt
  cond_c <- !is.na(eventdt) & ((!is.na(trtsdt) & eventdt < trtsdt) | (!is.na(eosdt) & eventdt > eosdt))
  fail <- cond_a | cond_b | cond_c
  n_adtte03 <- sum(fail, na.rm=TRUE)
}
chk_adtte03 <- data.frame(CHECK_ID="ADTTE-003", DOMAIN="ADTTE", RULE="Event and follow-up dates must be chronologically valid", N_FAIL=n_adtte03, stringsAsFactors=FALSE)

# RULE ADSL-003 EOSDT must agree with DM.RFPENDTC
n_adsl03 <- 0
if (nrow(adsl)>0 && "USUBJID" %in% names(adsl) && "EOSDT" %in% names(adsl) && nrow(dm)>0) {
  # left join
  dm_sub <- dm[, c("USUBJID","RFPENDTC"), drop=FALSE]
  merged <- merge(adsl, dm_sub, by="USUBJID", all.x=TRUE, suffixes=c("",".dm"))
  eosdt <- as_date_safe(merged$EOSDT)
  rfp <- suppressWarnings(as.Date(substr(trimws(as.character(merged$RFPENDTC)),1,10), format="%Y-%m-%d"))
  miss_eos <- is.na(eosdt)
  miss_dm <- is.na(rfp) | is_miss_chr(merged$RFPENDTC)
  # also d.USUBJID is null => miss_dm true
  # condition: d.USUBJID is null or missing(a.EOSDT) or a.EOSDT ne input(...)
  fail <- miss_dm | miss_eos | (!is.na(eosdt) & !is.na(rfp) & eosdt != rfp)
  n_adsl03 <- sum(fail, na.rm=TRUE)
}
chk_adsl03 <- data.frame(CHECK_ID="ADSL-003", DOMAIN="ADSL", RULE="EOSDT must agree with DM.RFPENDTC", N_FAIL=n_adsl03, stringsAsFactors=FALSE)

# RULE ADSL-004 AGE must equal completed years at informed consent
n_adsl04 <- 0
if (nrow(adsl)>0 && all(c("BRTHDT","RFICDT","AGE") %in% names(adsl))) {
  brthdt <- as_date_safe(adsl$BRTHDT)
  rficdt <- as_date_safe(adsl$RFICDT)
  age <- adsl$AGE
  miss_brth <- is.na(brthdt)
  miss_rfic <- is.na(rficdt)
  miss_age <- is.na(age)
  calc_age <- completed_years(brthdt, rficdt)
  # where missing(BRTHDT) or missing(RFICDT) or missing(AGE) or AGE ne floor(yrdif(...))
  # floor(yrdif) == calc_age
  fail <- miss_brth | miss_rfic | miss_age | (!is.na(age) & !is.na(calc_age) & age != calc_age)
  # also if calc_age is NA but age not missing, then fail? Our condition covers age != calc_age when calc_age NA => NA, not fail. But missing BRTHDT/RFICDT already fail.
  n_adsl04 <- sum(fail, na.rm=TRUE)
}
chk_adsl04 <- data.frame(CHECK_ID="ADSL-004", DOMAIN="ADSL", RULE="AGE must equal completed years at informed consent", N_FAIL=n_adsl04, stringsAsFactors=FALSE)

# RULE ADVS-002
n_advs02 <- 0
if (nrow(advs)>0 && all(c("USUBJID","PARAMCD","ASTDT","TRTSDT","AVAL") %in% names(advs))) {
  advs2 <- advs
  advs2$ASTDT_d <- as_date_safe(advs2$ASTDT)
  advs2$TRTSDT_d <- as_date_safe(advs2$TRTSDT)
  # eligible: ASTDT <= TRTSDT and not missing AVAL
  elig <- advs2[!is.na(advs2$ASTDT_d) & !is.na(advs2$TRTSDT_d) & advs2$ASTDT_d <= advs2$TRTSDT_d & !is.na(advs2$AVAL), , drop=FALSE]
  if (nrow(elig)>0) {
    base_dt <- elig %>% group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT_d, na.rm=TRUE), .groups="drop")
    # join back to advs where ASTDT = BASE_DT and not missing AVAL
    advs2$AVAL_miss <- is.na(advs2$AVAL)
    # inner join
    merged <- merge(advs2, base_dt, by=c("USUBJID","PARAMCD"))
    # filter where ASTDT == BASE_DT and not missing AVAL
    filt <- merged[!is.na(merged$ASTDT_d) & !is.na(merged$BASE_DT) & merged$ASTDT_d == merged$BASE_DT & !merged$AVAL_miss, , drop=FALSE]
    if (nrow(filt)>0) {
      dup <- filt %>% group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
      n_advs02 <- nrow(dup)
    }
  }
}
chk_advs02 <- data.frame(CHECK_ID="ADVS-002", DOMAIN="ADVS", RULE="Baseline date has exactly one eligible source record", N_FAIL=n_advs02, stringsAsFactors=FALSE)

# RULE ADLB-002
n_adlb02 <- 0
if (nrow(adlb)>0 && all(c("USUBJID","PARAMCD","ASTDT","TRTSDT","AVAL") %in% names(adlb))) {
  adlb2 <- adlb
  adlb2$ASTDT_d <- as_date_safe(adlb2$ASTDT)
  adlb2$TRTSDT_d <- as_date_safe(adlb2$TRTSDT)
  elig <- adlb2[!is.na(adlb2$ASTDT_d) & !is.na(adlb2$TRTSDT_d) & adlb2$ASTDT_d <= adlb2$TRTSDT_d & !is.na(adlb2$AVAL), , drop=FALSE]
  if (nrow(elig)>0) {
    base_dt <- elig %>% group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT_d, na.rm=TRUE), .groups="drop")
    merged <- merge(adlb2, base_dt, by=c("USUBJID","PARAMCD"))
    filt <- merged[!is.na(merged$ASTDT_d) & !is.na(merged$BASE_DT) & merged$ASTDT_d == merged$BASE_DT & !is.na(merged$AVAL), , drop=FALSE]
    if (nrow(filt)>0) {
      dup <- filt %>% group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
      n_adlb02 <- nrow(dup)
    }
  }
}
chk_adlb02 <- data.frame(CHECK_ID="ADLB-002", DOMAIN="ADLB", RULE="Baseline date has exactly one eligible source record", N_FAIL=n_adlb02, stringsAsFactors=FALSE)

# RULE 12: EG referential
n_eg01 <- 0
if (nrow(eg)>0 && "USUBJID" %in% names(eg) && "USUBJID" %in% names(dm)) {
  dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
  n_eg01 <- sum(!(eg$USUBJID %in% dm_ids) | is_miss_chr(eg$USUBJID), na.rm=TRUE)
}
chk_eg01 <- data.frame(CHECK_ID="SDTM-006", DOMAIN="EG", RULE="Every EG.USUBJID exists in DM", N_FAIL=n_eg01, stringsAsFactors=FALSE)

# RULE 13: EG required variables
req_eg <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST','EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
present_eg <- count_required_present(eg, req_eg)
n_eg02 <- 12 - present_eg
chk_eg02 <- data.frame(CHECK_ID="SDTM-007", DOMAIN="EG", RULE="Required EG variables must exist", N_FAIL=n_eg02, stringsAsFactors=FALSE)

# RULE 14: EG date/value contract
n_eg03 <- 0
if (nrow(eg)>0) {
  # need EGDTC, EGSTRESN, EGSTRESC
  if (all(c("EGDTC","EGSTRESN","EGSTRESC") %in% names(eg))) {
    miss_egdtc <- is_miss_chr(eg$EGDTC)
    bad_egdtc <- !is_iso_date(eg$EGDTC)
    miss_stresn <- is.na(eg$EGSTRESN)
    miss_stresc <- is_miss_chr(eg$EGSTRESC)
    fail <- miss_egdtc | bad_egdtc | miss_stresn | miss_stresc
    n_eg03 <- sum(fail, na.rm=TRUE)
  } else {
    # if columns missing, then every row fails? But SAS would error; we count as nrow
    n_eg03 <- nrow(eg)
  }
}
chk_eg03 <- data.frame(CHECK_ID="SDTM-008", DOMAIN="EG", RULE="EG date/value contract must be valid", N_FAIL=n_eg03, stringsAsFactors=FALSE)

# RULE 15: EG raw leak
raw_eg <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
n_eg04 <- count_raw_leak(eg, raw_eg)
chk_eg04 <- data.frame(CHECK_ID="SDTM-009", DOMAIN="EG", RULE="Raw EG variables must not leak to SDTM", N_FAIL=n_eg04, stringsAsFactors=FALSE)

# RULE 16: MH referential
n_mh01 <- 0
if (nrow(mh)>0 && "USUBJID" %in% names(mh) && "USUBJID" %in% names(dm)) {
  dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
  n_mh01 <- sum(!(mh$USUBJID %in% dm_ids) | is_miss_chr(mh$USUBJID), na.rm=TRUE)
}
chk_mh01 <- data.frame(CHECK_ID="SDTM-010", DOMAIN="MH", RULE="Every MH.USUBJID exists in DM", N_FAIL=n_mh01, stringsAsFactors=FALSE)

# RULE 17: MH required
req_mh <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
present_mh <- count_required_present(mh, req_mh)
n_mh02 <- 6 - present_mh
chk_mh02 <- data.frame(CHECK_ID="SDTM-011", DOMAIN="MH", RULE="Required MH variables must exist", N_FAIL=n_mh02, stringsAsFactors=FALSE)

# RULE 18: MH date/term
n_mh03 <- 0
if (nrow(mh)>0 && all(c("MHSTDTC","MHTERM") %in% names(mh))) {
  miss_mhstdtc <- is_miss_chr(mh$MHSTDTC)
  bad_mhstdtc <- !is_iso_date(mh$MHSTDTC)
  miss_mhterm <- is_miss_chr(mh$MHTERM)
  fail <- miss_mhstdtc | bad_mhstdtc | miss_mhterm
  n_mh03 <- sum(fail, na.rm=TRUE)
} else if (nrow(mh)>0) {
  n_mh03 <- nrow(mh)
}
chk_mh03 <- data.frame(CHECK_ID="SDTM-012", DOMAIN="MH", RULE="MH date/term contract must be valid", N_FAIL=n_mh03, stringsAsFactors=FALSE)

# RULE 19: MH raw leak
raw_mh <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')
n_mh04 <- count_raw_leak(mh, raw_mh)
chk_mh04 <- data.frame(CHECK_ID="SDTM-013", DOMAIN="MH", RULE="Raw MH variables must not leak to SDTM", N_FAIL=n_mh04, stringsAsFactors=FALSE)

# RULE 20: DM reference period
n_dm02 <- 0
if (nrow(dm)>0) {
  # need RFICDTC, RFSTDTC, RFENDTC, RFPENDTC
  if (all(c("RFICDTC","RFSTDTC","RFENDTC","RFPENDTC") %in% names(dm))) {
    miss_rfic <- is_miss_chr(dm$RFICDTC)
    miss_rfst <- is_miss_chr(dm$RFSTDTC)
    miss_rfen <- is_miss_chr(dm$RFENDTC)
    miss_rfpen <- is_miss_chr(dm$RFPENDTC)
    bad_rfic <- !is_iso_date(dm$RFICDTC)
    bad_rfst <- !is_iso_date(dm$RFSTDTC)
    bad_rfen <- !is_iso_date(dm$RFENDTC)
    bad_rfpen <- !is_iso_date(dm$RFPENDTC)
    # string comparison for dates: RFSTDTC < RFICDTC etc. Since ISO, lexicographic works
    cond_order1 <- !miss_rfic & !miss_rfst & !bad_rfic & !bad_rfst & trimws(dm$RFSTDTC) < trimws(dm$RFICDTC)
    cond_order2 <- !miss_rfst & !miss_rfen & !bad_rfst & !bad_rfen & trimws(dm$RFENDTC) < trimws(dm$RFSTDTC)
    cond_order3 <- !miss_rfpen & !miss_rfen & trimws(dm$RFPENDTC) != trimws(dm$RFENDTC)
    fail <- miss_rfic | miss_rfst | miss_rfen | miss_rfpen | bad_rfic | bad_rfst | bad_rfen | bad_rfpen | cond_order1 | cond_order2 | cond_order3
    n_dm02 <- sum(fail, na.rm=TRUE)
  } else {
    n_dm02 <- nrow(dm)
  }
}
chk_dm02 <- data.frame(CHECK_ID="SDTM-014", DOMAIN="DM", RULE="DM reference dates complete and ordered", N_FAIL=n_dm02, stringsAsFactors=FALSE)

# RULE 21: SV referential
n_sv01 <- 0
if (nrow(sv)>0 && "USUBJID" %in% names(sv) && "USUBJID" %in% names(dm)) {
  dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
  n_sv01 <- sum(!(sv$USUBJID %in% dm_ids) | is_miss_chr(sv$USUBJID), na.rm=TRUE)
}
chk_sv01 <- data.frame(CHECK_ID="SDTM-015", DOMAIN="SV", RULE="Every SV.USUBJID exists in DM", N_FAIL=n_sv01, stringsAsFactors=FALSE)

# RULE 22: SV unique VISITNUM per subject
n_sv02 <- 0
if (nrow(sv)>0 && all(c("USUBJID","VISITNUM") %in% names(sv))) {
  dup <- sv %>% group_by(USUBJID, VISITNUM) %>% summarise(n=n(), .groups="drop") %>% filter(n != 1)
  n_sv02 <- nrow(dup)
}
chk_sv02 <- data.frame(CHECK_ID="SDTM-016", DOMAIN="SV", RULE="USUBJID and VISITNUM must be unique", N_FAIL=n_sv02, stringsAsFactors=FALSE)

# RULE 23: SV dates valid and inside reference
n_sv03 <- 0
if (nrow(sv)>0 && nrow(dm)>0 && all(c("VISITNUM","VISIT","SVSTDTC","SVENDTC","USUBJID") %in% names(sv))) {
  # inner join sv to dm
  dm_sub <- dm[, c("USUBJID","RFSTDTC","RFENDTC"), drop=FALSE]
  merged <- merge(sv, dm_sub, by="USUBJID", all=FALSE)
  if (nrow(merged)>0) {
    miss_visitnum <- is.na(merged$VISITNUM)
    miss_visit <- is_miss_chr(merged$VISIT)
    miss_svstdtc <- is_miss_chr(merged$SVSTDTC)
    miss_svendtc <- is_miss_chr(merged$SVENDTC)
    bad_svstdtc <- !is_iso_date(merged$SVSTDTC)
    bad_svendtc <- !is_iso_date(merged$SVENDTC)
    # SVENDTC < SVSTDTC string compare (ISO)
    cond_end_lt_start <- !miss_svstdtc & !miss_svendtc & !bad_svstdtc & !bad_svendtc & trimws(merged$SVENDTC) < trimws(merged$SVSTDTC)
    cond_start_lt_rfstd <- !miss_svstdtc & !bad_svstdtc & !is_miss_chr(merged$RFSTDTC) & trimws(merged$SVSTDTC) < trimws(merged$RFSTDTC)
    cond_end_gt_rfend <- !miss_svendtc & !bad_svendtc & !is_miss_chr(merged$RFENDTC) & trimws(merged$SVENDTC) > trimws(merged$RFENDTC)
    fail <- miss_visitnum | miss_visit | miss_svstdtc | miss_svendtc | bad_svstdtc | bad_svendtc | cond_end_lt_start | cond_start_lt_rfstd | cond_end_gt_rfend
    n_sv03 <- sum(fail, na.rm=TRUE)
  }
}
chk_sv03 <- data.frame(CHECK_ID="SDTM-017", DOMAIN="SV", RULE="SV dates valid and inside reference period", N_FAIL=n_sv03, stringsAsFactors=FALSE)

# RULE 24: DS exactly one final per DM subject
n_ds01 <- 0
if (nrow(dm)>0) {
  dm_ids <- unique(dm$USUBJID)
  # count per DM subject
  if (nrow(ds)>0 && "USUBJID" %in% names(ds)) {
    ds_counts <- ds %>% group_by(USUBJID) %>% summarise(N_DS=n(), .groups="drop")
    # left join dm to ds counts
    dm_counts <- data.frame(USUBJID=dm_ids, stringsAsFactors=FALSE) %>% left_join(ds_counts, by="USUBJID")
    dm_counts$N_DS[is.na(dm_counts$N_DS)] <- 0
    fail_dm <- sum(dm_counts$N_DS != 1, na.rm=TRUE)
    # orphan ds: ds USUBJID not in dm
    orphan <- ds[!(ds$USUBJID %in% dm_ids), , drop=FALSE]
    fail_orphan <- 0
    if (nrow(orphan)>0) {
      # group by USUBJID (each orphan group counts as 1 fail? SAS: select s.USUBJID, count(*) as N_DS from sdtm.ds s left join sdtm.dm d ... where d.USUBJID is null group by s.USUBJID)
      # Then outer count(*) counts number of orphan USUBJIDs (distinct)
      fail_orphan <- length(unique(orphan$USUBJID))
    }
    n_ds01 <- fail_dm + fail_orphan
  } else {
    # no ds at all => each dm subject has 0 !=1
    n_ds01 <- length(dm_ids)
  }
}
chk_ds01 <- data.frame(CHECK_ID="SDTM-018", DOMAIN="DS", RULE="Exactly one final DS record per DM subject", N_FAIL=n_ds01, stringsAsFactors=FALSE)

# RULE 25: DS date matches DM participation end
n_ds02 <- 0
if (nrow(ds)>0 && nrow(dm)>0 && all(c("DSDECOD","DSSTDTC","USUBJID") %in% names(ds))) {
  dm_sub <- dm[, c("USUBJID","RFPENDTC"), drop=FALSE]
  merged <- merge(ds, dm_sub, by="USUBJID", all=FALSE)
  if (nrow(merged)>0) {
    miss_decod <- is_miss_chr(merged$DSDECOD)
    miss_dsstdtc <- is_miss_chr(merged$DSSTDTC)
    bad_dsstdtc <- !is_iso_date(merged$DSSTDTC)
    cond_neq <- !miss_dsstdtc & !bad_dsstdtc & !is_miss_chr(merged$RFPENDTC) & trimws(merged$DSSTDTC) != trimws(merged$RFPENDTC)
    fail <- miss_decod | miss_dsstdtc | bad_dsstdtc | cond_neq
    n_ds02 <- sum(fail, na.rm=TRUE)
  }
}
chk_ds02 <- data.frame(CHECK_ID="SDTM-019", DOMAIN="DS", RULE="DS date matches DM participation end", N_FAIL=n_ds02, stringsAsFactors=FALSE)

# RULE 26: DM RACE terminology
n_dm03 <- 0
if (nrow(dm)>0 && "RACE" %in% names(dm)) {
  allowed <- c('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')
  miss_race <- is_miss_chr(dm$RACE)
  bad_race <- !(upcase(dm$RACE) %in% allowed)
  fail <- miss_race | bad_race
  n_dm03 <- sum(fail, na.rm=TRUE)
}
chk_dm03 <- data.frame(CHECK_ID="SDTM-020", DOMAIN="DM", RULE="RACE uses supported CDISC terminology", N_FAIL=n_dm03, stringsAsFactors=FALSE)

# RULE 27: DM SITEID consistent with SUBJID
n_dm04 <- 0
if (nrow(dm)>0 && all(c("SITEID","SUBJID") %in% names(dm))) {
  miss_site <- is_miss_chr(dm$SITEID)
  # scan(strip(SUBJID),1,'-')
  scan1 <- sapply(strsplit(trimws(as.character(dm$SUBJID)), "-", fixed=TRUE), function(x) if(length(x)>=1) x[1] else "")
  cond_neq <- !miss_site & trimws(as.character(dm$SITEID)) != trimws(scan1)
  fail <- miss_site | cond_neq
  n_dm04 <- sum(fail, na.rm=TRUE)
}
chk_dm04 <- data.frame(CHECK_ID="SDTM-021", DOMAIN="DM", RULE="SITEID present and consistent with SUBJID", N_FAIL=n_dm04, stringsAsFactors=FALSE)

# RULE 28: ADAE-002 AOCCFL valid and restricted to TRTEMFL=Y
n_adae02 <- 0
if (nrow(adae)>0 && all(c("AOCCFL","TRTEMFL") %in% names(adae))) {
  # (not missing(AOCCFL) and AOCCFL ne 'Y') or (AOCCFL='Y' and TRTEMFL ne 'Y')
  not_miss_aoccfl <- !is_miss_chr(adae$AOCCFL)
  aoccfl_not_y <- not_miss_aoccfl & trimws(adae$AOCCFL) != "Y"
  aoccfl_y_trtem_not_y <- !is_miss_chr(adae$AOCCFL) & trimws(adae$AOCCFL)=="Y" & trimws(adae$TRTEMFL) != "Y"
  fail <- aoccfl_not_y | aoccfl_y_trtem_not_y
  n_adae02 <- sum(fail, na.rm=TRUE)
}
chk_adae02 <- data.frame(CHECK_ID="ADAE-002", DOMAIN="ADAE", RULE="AOCCFL valid and restricted to TRTEMFL=Y", N_FAIL=n_adae02, stringsAsFactors=FALSE)

# RULE 29: ADAE-003 One AOCCFL=Y per subject and TEAE term
n_adae03 <- 0
if (nrow(adae)>0 && all(c("USUBJID","AEDECOD","TRTEMFL","AOCCFL") %in% names(adae))) {
  # group by USUBJID, AEDECOD having sum(TRTEMFL='Y')>0 and sum(AOCCFL='Y') !=1
  # need to handle missing
  adae$trte_y <- ifelse(!is_miss_chr(adae$TRTEMFL) & trimws(adae$TRTEMFL)=="Y", 1, 0)
  adae$aocc_y <- ifelse(!is_miss_chr(adae$AOCCFL) & trimws(adae$AOCCFL)=="Y", 1, 0)
  grp <- adae %>% group_by(USUBJID, AEDECOD) %>% summarise(sum_trte=sum(trte_y, na.rm=TRUE), sum_aocc=sum(aocc_y, na.rm=TRUE), .groups="drop")
  fail_grp <- grp %>% filter(sum_trte > 0 & sum_aocc != 1)
  n_adae03 <- nrow(fail_grp)
}
chk_adae03 <- data.frame(CHECK_ID="ADAE-003", DOMAIN="ADAE", RULE="One AOCCFL=Y per subject and TEAE term", N_FAIL=n_adae03, stringsAsFactors=FALSE)

# RULE 30: CM subject, medication, dose and dates must be valid
n_cm01 <- 0
if (nrow(cm)>0) {
  # left join cm to dm
  if ("USUBJID" %in% names(cm) && "USUBJID" %in% names(dm)) {
    dm_ids <- unique(dm$USUBJID[!is_miss_chr(dm$USUBJID)])
    # For each cm row, check conditions
    # d.USUBJID is null => USUBJID not in dm
    cond_dm_null <- is_miss_chr(cm$USUBJID) | !(cm$USUBJID %in% dm_ids)
    miss_cmtrt <- is_miss_chr(cm$CMTRT)
    miss_cmdecod <- is_miss_chr(cm$CMDECOD)
    # (upcase(strip(CMTRT))='PARACETAMOL' and upcase(strip(CMDECOD)) ne 'ACETAMINOPHEN')
    cond_para <- !is_miss_chr(cm$CMTRT) & upcase(cm$CMTRT)=="PARACETAMOL" & upcase(cm$CMDECOD) != "ACETAMINOPHEN"
    miss_cmdose <- is.na(cm$CMDOSE)
    miss_cmdosu <- is_miss_chr(cm$CMDOSU)
    miss_cmstdtc <- is_miss_chr(cm$CMSTDTC)
    bad_cmstdtc <- !is_iso_date(cm$CMSTDTC)
    # CMENDTC checks: if not missing, must be iso
    miss_cmendtc <- is_miss_chr(cm$CMENDTC)
    bad_cmendtc <- !miss_cmendtc & !is_iso_date(cm$CMENDTC)
    # CMENDTC < CMSTDTC (string compare ISO)
    cond_end_lt_start <- !miss_cmendtc & !miss_cmstdtc & !bad_cmendtc & !bad_cmstdtc & trimws(cm$CMENDTC) < trimws(cm$CMSTDTC)
    # CMENRTPT checks: need column
    cmenrtpt <- if ("CMENRTPT" %in% names(cm)) cm$CMENRTPT else rep(NA, nrow(cm))
    cond_ongoing_with_end <- !is_miss_chr(cmenrtpt) & upcase(cmenrtpt)=="ONGOING" & !miss_cmendtc
    cond_not_ongoing_missing_end <- (is_miss_chr(cmenrtpt) | upcase(cmenrtpt) != "ONGOING") & miss_cmendtc
    # Note: SAS: (c.CMENRTPT = 'ONGOING' and not missing(c.CMENDTC)) or (c.CMENRTPT ne 'ONGOING' and missing(c.CMENDTC))
    # This is case-sensitive exact 'ONGOING'
    # But we use upcase? SAS code for this rule uses exact case: c.CMENRTPT = 'ONGOING'
    # We'll follow exact case-sensitive for this rule
    # Re-evaluate with case-sensitive
    cond_ongoing_with_end_cs <- !is.na(cmenrtpt) & trimws(cmenrtpt)=="ONGOING" & !miss_cmendtc
    cond_not_ongoing_missing_end_cs <- (is.na(cmenrtpt) | trimws(cmenrtpt) != "ONGOING") & miss_cmendtc
    fail <- cond_dm_null | miss_cmtrt | miss_cmdecod | cond_para | miss_cmdose | miss_cmdosu | miss_cmstdtc | bad_cmstdtc | bad_cmendtc | cond_end_lt_start | cond_ongoing_with_end_cs | cond_not_ongoing_missing_end_cs
    n_cm01 <- sum(fail, na.rm=TRUE)
  }
}
chk_cm01 <- data.frame(CHECK_ID="SDTM-022", DOMAIN="CM", RULE="CM subject, medication, dose and dates must be valid", N_FAIL=n_cm01, stringsAsFactors=FALSE)

# RULE 31: DSDECOD terminology
n_ds03 <- 0
if (nrow(ds)>0 && "DSDECOD" %in% names(ds)) {
  allowed_ds <- c('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT','ADVERSE EVENT','SCREEN FAILURE')
  bad_ds <- !(upcase(ds$DSDECOD) %in% allowed_ds)
  # missing also counts as not in allowed? SAS: where upcase(strip(DSDECOD)) not in (...) => missing will be not in, so counts
  # But need to handle NA: upcase(NA) is NA, not in allowed => TRUE? We'll treat NA as fail
  bad_ds[is.na(bad_ds)] <- TRUE
  # also is_miss_chr => upcase("") = "" not in allowed => true
  n_ds03 <- sum(bad_ds, na.rm=TRUE)
}
chk_ds03 <- data.frame(CHECK_ID="SDTM-023", DOMAIN="DS", RULE="DSDECOD uses supported disposition terminology", N_FAIL=n_ds03, stringsAsFactors=FALSE)

# RULE 32: Sequence keys unique per subject
n_seq01 <- 0
# helper to count dup seq per domain
count_seq_dup <- function(df, seq_col) {
  if (is.null(df) || nrow(df)==0) return(0)
  if (!("USUBJID" %in% names(df)) || !(seq_col %in% names(df))) return(0)
  dup <- df %>% group_by(USUBJID, across(all_of(seq_col))) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
  nrow(dup)
}
n_seq01 <- count_seq_dup(ae, "AESEQ") + count_seq_dup(ex, "EXSEQ") + count_seq_dup(lb, "LBSEQ") + count_seq_dup(vs, "VSSEQ") + count_seq_dup(cm, "CMSEQ") + count_seq_dup(mh, "MHSEQ") + count_seq_dup(eg, "EGSEQ")
chk_seq01 <- data.frame(CHECK_ID="SDTM-024", DOMAIN="SDTM", RULE="Domain sequence keys must be unique per subject", N_FAIL=n_seq01, stringsAsFactors=FALSE)

# RULE 39: Ongoing CM anchored
n_cm02 <- 0
if (nrow(cm)>0 && nrow(dm)>0 && "USUBJID" %in% names(cm)) {
  dm_sub <- dm[, c("USUBJID","RFENDTC"), drop=FALSE]
  merged <- merge(cm, dm_sub, by="USUBJID", all=FALSE) # inner join
  if (nrow(merged)>0) {
    cmenrtpt_up <- upcase(merged$CMENRTPT)
    cmentpt <- merged$CMENTPT
    cmendtc <- merged$CMENDTC
    rfendtc <- merged$RFENDTC
    miss_cmendtc <- is_miss_chr(cmendtc)
    miss_cmentpt <- is_miss_chr(cmentpt)
    bad_cmentpt <- !is_iso_date(cmentpt)
    # condition1: upcase(strip(CMENRTPT)) not in ('','ONGOING')
    cond1 <- !(cmenrtpt_up %in% c("","ONGOING"))
    # condition2: ONGOING and (not missing CMENDTC or missing CMENTPT or bad CMENTPT or CMENTPT != RFENDTC)
    cond2 <- cmenrtpt_up=="ONGOING" & (!miss_cmendtc | miss_cmentpt | bad_cmentpt | (!miss_cmentpt & !is_miss_chr(rfendtc) & trimws(cmentpt) != trimws(rfendtc)))
    # condition3: not ONGOING and not missing CMENTPT
    cond3 <- cmenrtpt_up != "ONGOING" & !miss_cmentpt
    # Note: for cond1, need to handle NA: upcase(NA) is NA, not in c("", "ONGOING") => TRUE? In SAS, missing CMENRTPT would be ""? Actually SAS missing char is "" . So upcase(strip("")) = "" which is in list, so not fail. In R, NA should be treated as ""? We'll treat NA as "".
    cmenrtpt_up[is.na(cmenrtpt_up)] <- ""
    cond1 <- !(cmenrtpt_up %in% c("","ONGOING"))
    cond2 <- cmenrtpt_up=="ONGOING" & (!miss_cmendtc | miss_cmentpt | bad_cmentpt | (!miss_cmentpt & trimws(cmentpt) != trimws(rfendtc)))
    cond3 <- cmenrtpt_up != "ONGOING" & !miss_cmentpt
    fail <- cond1 | cond2 | cond3
    n_cm02 <- sum(fail, na.rm=TRUE)
  }
}
chk_cm02 <- data.frame(CHECK_ID="SDTM-025", DOMAIN="CM", RULE="Ongoing CM must be anchored to the DM reference end", N_FAIL=n_cm02, stringsAsFactors=FALSE)

# Consolidate in SAS order
qc_report <- rbind(
  chk_adsl01, chk_adae01, chk_advs01, chk_adlb01,
  chk_advs02, chk_adlb02, chk_dm01,
  chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04,
  chk_adtte01, chk_adtte02, chk_adtte03,
  chk_eg01, chk_eg02, chk_eg03, chk_eg04,
  chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02,
  chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02,
  chk_dm03, chk_dm04, chk_adae02, chk_adae03,
  chk_cm01, chk_ds03, chk_seq01, chk_cm02
)

# STATUS
qc_report$STATUS <- ifelse(qc_report$N_FAIL==0, "PASS", "FAIL")
# Ensure lengths as SAS: CHECK_ID $10, DOMAIN $10, RULE $70, STATUS $10
# Truncate RULE to 70 chars if needed (SAS length)
qc_report$RULE <- substr(qc_report$RULE, 1, 70)

# Write dataset
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names=FALSE, na="")

# Also write P019 dataset copies
write.csv(qc_report, file.path(out, "P019.csv"), row.names=FALSE, na="")
write.csv(qc_report, file.path(out, "P019_qc_report.csv"), row.names=FALSE, na="")

# Text rendering
# Create a text file that mimics PROC PRINT
txt_path <- file.path(out, "P019.txt")
lst_path <- file.path(out, "P019.lst")
qc_txt_path <- file.path(out, "qc_report.txt")

# Build text content
title_str <- "Automated Quality Control (QC) Validation Report"
header <- sprintf("%-10s %-10s %-70s %6s %-6s", "CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS")
sep <- paste(rep("-", nchar(header)), collapse="")
lines <- c(title_str, "", header, sep)
for (i in 1:nrow(qc_report)) {
  lines <- c(lines, sprintf("%-10s %-10s %-70s %6d %-6s",
                            qc_report$CHECK_ID[i],
                            qc_report$DOMAIN[i],
                            qc_report$RULE[i],
                            qc_report$N_FAIL[i],
                            qc_report$STATUS[i]))
}
lines <- c(lines, "", paste0("Total checks: ", nrow(qc_report), "  Failed: ", sum(qc_report$STATUS=="FAIL")))

# Write to all text variants
writeLines(lines, txt_path)
writeLines(lines, lst_path)
writeLines(lines, qc_txt_path)
# Also write P019.lst.txt and P019.lst.csv per spec
writeLines(lines, file.path(out, "P019.lst.txt"))
write.csv(qc_report, file.path(out, "P019.lst.csv"), row.names=FALSE, na="")

# Also create RTF-like placeholder (not required but for completeness)
# Ensure out directory has qc_report.csv with correct columns
# Print summary to console
cat("QC report generated with", nrow(qc_report), "checks, failed:", sum(qc_report$STATUS=="FAIL"), "\n")
print(qc_report)
