# P019_qc_core.sas -> R translation
# Outputs: qc_report.csv + P019.txt + P019.csv (table rendering)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(readr)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: read all datasets from a libref directory (sas7bdat, xpt, csv)
read_lib <- function(path) {
  if (!dir.exists(path)) return(list())
  files <- list.files(path, full.names = TRUE)
  outlist <- list()
  for (f in files) {
    nm <- tools::file_path_sans_ext(basename(f))
    # skip hidden
    ext <- tolower(tools::file_ext(f))
    df <- NULL
    try({
      if (ext == "sas7bdat") df <- haven::read_sas(f)
      else if (ext == "xpt") df <- haven::read_xpt(f)
      else if (ext == "csv") df <- readr::read_csv(f, show_col_types = FALSE)
      else if (ext == "sas7bdat") df <- haven::read_sas(f)
    }, silent = TRUE)
    if (!is.null(df)) {
      # uppercase names to mimic SAS validvarname=upcase
      names(df) <- toupper(names(df))
      outlist[[toupper(nm)]] <- df
    }
  }
  outlist
}

# Paths from SAS librefs
project_path <- "/bench/data/derived/c2c"
raw_path  <- file.path(project_path, "data/raw")
sdtm_path <- file.path(project_path, "data/sdtm")
adam_path <- file.path(project_path, "data/adam")

raw  <- read_lib(raw_path)
sdtm <- read_lib(sdtm_path)
adam <- read_lib(adam_path)

# Fallback: also try reading from rawadam/rawsdtm if adam/sdtm empty
if (length(adam) == 0) {
  alt <- "/bench/data/raw/cdisc-pilot/adam"
  if (dir.exists(alt)) adam <- read_lib(alt)
}
if (length(sdtm) == 0) {
  alt2 <- "/bench/data/raw/cdisc-pilot/sdtm"
  if (dir.exists(alt2)) sdtm <- read_lib(alt2)
}

# Utility to get dataset case-insensitive
get_ds <- function(lib, name) {
  n <- toupper(name)
  if (n %in% names(lib)) return(lib[[n]])
  # try lower
  idx <- which(toupper(names(lib)) == n)
  if (length(idx) > 0) return(lib[[idx[1]]])
  return(NULL)
}

# Ensure character columns are character, trim handling via helpers
strip <- function(x) {
  if (is.null(x)) return(x)
  str_trim(as.character(x))
}
upcase_strip <- function(x) toupper(strip(x))

# SAS missing check: NA or empty string after trim
is_sas_missing <- function(x) {
  if (is.null(x)) return(rep(TRUE, 0))
  if (is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(is.na(x))
  } else {
    return(is.na(x) | strip(x) == "")
  }
}
not_sas_missing <- function(x) !is_sas_missing(x)

# SAS prxmatch for date ISO 8601 YYYY-MM-DD
is_iso_date <- function(x) {
  !is_sas_missing(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(x))
}

# SAS yrdif with 'AGE' basis: completed years between two dates
# SAS yrdif(BRTHDT, RFICDT, 'AGE') = floor? Actually SAS returns fractional years, then floor gives age.
# We need to replicate SAS AGE basis: age = floor(yrdif(..., 'AGE'))
# SAS AGE basis: actual age in years considering leap years.
# Equivalent to: if month/day of RFICDT >= month/day of BRTHDT then year diff else year diff -1
# But SAS yrdif AGE uses exact day count / 365.2425? Let's implement standard completed years.
yrdif_age <- function(start, end) {
  # start, end are Date
  # SAS yrdif with AGE: uses 365.2425? But floor(yrdif) should equal completed years.
  # Completed years method is reliable and matches SAS for most cases.
  # We'll compute using lubridate interval and also fallback to SAS-like.
  # Use completed years logic:
  ifelse(is.na(start) | is.na(end), NA_real_,
         {
           y1 <- year(start); m1 <- month(start); d1 <- day(start)
           y2 <- year(end);   m2 <- month(end);   d2 <- day(end)
           age <- y2 - y1 - ifelse(m2 < m1 | (m2 == m1 & d2 < d1), 1, 0)
           # To mimic yrdif fractional, we return age + fraction, but floor will be age
           # So return age as fractional approximation
           as.numeric(age)
         })
}
# For floor(yrdif) we can directly compute completed years
completed_years <- function(brth, rfic) {
  ifelse(is.na(brth) | is.na(rfic), NA_integer_,
         {
           y1 <- year(brth); m1 <- month(brth); d1 <- day(brth)
           y2 <- year(rfic); m2 <- month(rfic); d2 <- day(rfic)
           y2 - y1 - ifelse(m2 < m1 | (m2 == m1 & d2 < d1), 1L, 0L)
         })
}

# Helper to parse SAS date strings YYYY-MM-DD to Date
parse_iso <- function(x) {
  suppressWarnings(as.Date(strip(x), format = "%Y-%m-%d"))
}
# For input(substr(d.RFPENDTC,1,10), yymmdd10.) -> parse first 10 chars
parse_yymmdd10 <- function(x) {
  s <- substr(strip(x), 1, 10)
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

# Helper to ensure Date columns are Date type
as_date_col <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    # haven may have already converted with origin 1960-01-01 if labelled
    # If numeric and large (e.g., > 10000), likely SAS date
    # haven::read_sas already converts to Date if format is date
    # So if still numeric, try SAS origin
    # Heuristic: if values look like SAS dates (10000-25000)
    # We'll attempt conversion but keep NA if not
    # Check if any non-NA > 5000
    if (all(is.na(x))) return(as.Date(NA))
    # If already Date-like numeric, convert
    # haven usually returns Date, so this branch rarely hit
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character ISO
  parse_iso(x)
}

# Normalize datasets: ensure expected columns exist, convert dates
# ADSL, ADAE, ADVS, ADLB, ADTTE, DM, AE, EX, LB, VS, EG, MH, SV, DS, CM

# Helper to safely get column or return NA vector
col_or_na <- function(df, col) {
  if (is.null(df)) return(NULL)
  if (toupper(col) %in% names(df)) return(df[[toupper(col)]])
  return(rep(NA, nrow(df)))
}

# Load key datasets
adsl  <- get_ds(adam, "ADSL")
adae  <- get_ds(adam, "ADAE")
advs  <- get_ds(adam, "ADVS")
adlb  <- get_ds(adam, "ADLB")
adtte <- get_ds(adam, "ADTTE")
dm    <- get_ds(sdtm, "DM")
ae    <- get_ds(sdtm, "AE")
ex    <- get_ds(sdtm, "EX")
lb    <- get_ds(sdtm, "LB")
vs    <- get_ds(sdtm, "VS")
eg    <- get_ds(sdtm, "EG")
mh    <- get_ds(sdtm, "MH")
sv    <- get_ds(sdtm, "SV")
ds    <- get_ds(sdtm, "DS")
cm    <- get_ds(sdtm, "CM")

# If any dataset missing, create empty tibble to avoid errors
empty_df <- function() tibble()
if (is.null(adsl)) adsl <- tibble()
if (is.null(adae)) adae <- tibble()
if (is.null(advs)) advs <- tibble()
if (is.null(adlb)) adlb <- tibble()
if (is.null(adtte)) adtte <- tibble()
if (is.null(dm)) dm <- tibble()
if (is.null(ae)) ae <- tibble()
if (is.null(ex)) ex <- tibble()
if (is.null(lb)) lb <- tibble()
if (is.null(vs)) vs <- tibble()
if (is.null(eg)) eg <- tibble()
if (is.null(mh)) mh <- tibble()
if (is.null(sv)) sv <- tibble()
if (is.null(ds)) ds <- tibble()
if (is.null(cm)) cm <- tibble()

# Convert date columns where needed
# ADSL: BRTHDT, RFICDT, EOSDT, TRTSDT
# ADTTE: TRTSDT, EOSDT, EVENTDT, AVAL, CNSR
# ADAE: AEENDT, AESTDT
# ADVS/ADLB: ASTDT, TRTSDT, ABLFL, AVAL, PARAMCD
# SDTM DM: RFPENDTC, RFICDTC, RFSTDTC, RFENDTC, RACE, SITEID, SUBJID
# For ADAM dates, they are typically numeric SAS dates already converted to Date by haven
# Ensure they are Date
convert_dates <- function(df, cols) {
  for (c in cols) {
    if (toupper(c) %in% names(df)) {
      df[[toupper(c)]] <- as_date_col(df[[toupper(c)]])
    }
  }
  df
}
adsl  <- convert_dates(adsl, c("BRTHDT","RFICDT","EOSDT","TRTSDT"))
adae  <- convert_dates(adae, c("AEENDT","AESTDT"))
advs  <- convert_dates(advs, c("ASTDT","TRTSDT"))
adlb  <- convert_dates(adlb, c("ASTDT","TRTSDT"))
adtte <- convert_dates(adtte, c("TRTSDT","EOSDT","EVENTDT"))

# Helper to count distinct USUBJID duplicates
count_dup_usubjid <- function(df) {
  if (is.null(df) || nrow(df)==0 || !"USUBJID" %in% names(df)) return(0L)
  df %>% filter(not_sas_missing(USUBJID)) %>%
    group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>%
    filter(n>1) %>% nrow() %>% as.integer()
}

# Now implement each check, producing N_FAIL

# RULE 1: ADSL-001
n_adsl01 <- if (nrow(adsl)==0) 0L else {
  adsl %>% filter(not_sas_missing(USUBJID)) %>% group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
}
chk_adsl01 <- tibble(CHECK_ID="ADSL-001", DOMAIN="ADSL", RULE="USUBJID must be perfectly unique", N_FAIL=n_adsl01)

# RULE 2: ADAE-001
n_adae01 <- if (nrow(adae)==0) 0L else {
  adae %>% filter(not_sas_missing(AEENDT), not_sas_missing(AESTDT), AEENDT < AESTDT) %>% nrow() %>% as.integer()
}
chk_adae01 <- tibble(CHECK_ID="ADAE-001", DOMAIN="ADAE", RULE="AEENDT must be >= AESTDT", N_FAIL=n_adae01)

# RULE 3: ADVS-001
n_advs01 <- if (nrow(advs)==0) 0L else {
  advs %>% filter(!is.na(ABLFL) & ABLFL=="Y") %>%
    filter(not_sas_missing(USUBJID), not_sas_missing(PARAMCD)) %>%
    group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
}
chk_advs01 <- tibble(CHECK_ID="ADVS-001", DOMAIN="ADVS", RULE="Max 1 Baseline (ABLFL=Y) per Param", N_FAIL=n_advs01)

# RULE 4: ADLB-001
n_adlb01 <- if (nrow(adlb)==0) 0L else {
  adlb %>% filter(!is.na(ABLFL) & ABLFL=="Y") %>%
    filter(not_sas_missing(USUBJID), not_sas_missing(PARAMCD)) %>%
    group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
}
chk_adlb01 <- tibble(CHECK_ID="ADLB-001", DOMAIN="ADLB", RULE="Max 1 Baseline (ABLFL=Y) per Param", N_FAIL=n_adlb01)

# RULE 5: SDTM DM-001
n_dm01 <- if (nrow(dm)==0) 0L else {
  dm %>% filter(not_sas_missing(USUBJID)) %>% group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
}
chk_dm01 <- tibble(CHECK_ID="SDTM-001", DOMAIN="DM", RULE="USUBJID must be perfectly unique", N_FAIL=n_dm01)

# RULE 6: SDTM AE-002
n_ae01 <- if (nrow(ae)==0) 0L else {
  if (nrow(dm)==0) nrow(ae) else {
    ae %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_ae01 <- tibble(CHECK_ID="SDTM-002", DOMAIN="AE", RULE="Every AE.USUBJID exists in DM", N_FAIL=n_ae01)

# RULE 7: SDTM EX-003
n_ex01 <- if (nrow(ex)==0) 0L else {
  if (nrow(dm)==0) nrow(ex) else {
    ex %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_ex01 <- tibble(CHECK_ID="SDTM-003", DOMAIN="EX", RULE="Every EX.USUBJID exists in DM", N_FAIL=n_ex01)

# RULE 8: SDTM LB-004
n_lb01 <- if (nrow(lb)==0) 0L else {
  if (nrow(dm)==0) nrow(lb) else {
    lb %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_lb01 <- tibble(CHECK_ID="SDTM-004", DOMAIN="LB", RULE="Every LB.USUBJID exists in DM", N_FAIL=n_lb01)

# RULE 9: SDTM VS-005
n_vs01 <- if (nrow(vs)==0) 0L else {
  if (nrow(dm)==0) nrow(vs) else {
    vs %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_vs01 <- tibble(CHECK_ID="SDTM-005", DOMAIN="VS", RULE="Every VS.USUBJID exists in DM", N_FAIL=n_vs01)

# RULE 10: ADSL-002 SAFFL=Y requires exposure in EX
n_adsl02 <- if (nrow(adsl)==0) 0L else {
  if (nrow(ex)==0) {
    adsl %>% filter(!is.na(SAFFL) & SAFFL=="Y") %>% nrow() %>% as.integer()
  } else {
    adsl %>% filter(!is.na(SAFFL) & SAFFL=="Y") %>%
      anti_join(ex %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_adsl02 <- tibble(CHECK_ID="ADSL-002", DOMAIN="ADSL", RULE="SAFFL=Y requires exposure in EX", N_FAIL=n_adsl02)

# RULE 11: ADTTE-001 AVAL >=1
n_adtte01 <- if (nrow(adtte)==0) 0L else {
  adtte %>% filter(is.na(AVAL) | AVAL < 1) %>% nrow() %>% as.integer()
}
chk_adtte01 <- tibble(CHECK_ID="ADTTE-001", DOMAIN="ADTTE", RULE="Survival time (AVAL) must be >= 1", N_FAIL=n_adtte01)

# ADTTE-002
n_adtte02 <- if (nrow(adtte)==0) 0L else {
  adtte %>% mutate(
    cond1 = is_sas_missing(TRTSDT),
    cond2 = is.na(CNSR),
    cond3 = !is.na(CNSR) & !(CNSR %in% c(0,1)),
    cond4 = !is.na(CNSR) & CNSR==0 & (is_sas_missing(EVENTDT) | is.na(AVAL) | AVAL != as.numeric(EVENTDT - TRTSDT) + 1),
    cond5 = !is.na(CNSR) & CNSR==1 & (not_sas_missing(EVENTDT) | is_sas_missing(EOSDT) | is.na(AVAL) | AVAL != as.numeric(EOSDT - TRTSDT) + 1)
  ) %>% filter(cond1 | cond2 | cond3 | cond4 | cond5) %>% nrow() %>% as.integer()
}
chk_adtte02 <- tibble(CHECK_ID="ADTTE-002", DOMAIN="ADTTE", RULE="AVAL and CNSR agree with event or subject follow-up date", N_FAIL=n_adtte02)

# ADTTE-003
n_adtte03 <- if (nrow(adtte)==0) 0L else {
  adtte %>% filter(
    is_sas_missing(EOSDT) |
    EOSDT < TRTSDT |
    (!is_sas_missing(EVENTDT) & (EVENTDT < TRTSDT | EVENTDT > EOSDT))
  ) %>% nrow() %>% as.integer()
}
chk_adtte03 <- tibble(CHECK_ID="ADTTE-003", DOMAIN="ADTTE", RULE="Event and follow-up dates must be chronologically valid", N_FAIL=n_adtte03)

# ADSL-003 EOSDT must agree with DM.RFPENDTC
n_adsl03 <- if (nrow(adsl)==0) 0L else {
  if (nrow(dm)==0) nrow(adsl) else {
    # left join
    dm2 <- dm %>% select(USUBJID, RFPENDTC)
    # parse RFPENDTC to date
    dm2 <- dm2 %>% mutate(RFPENDTC_DT = parse_yymmdd10(RFPENDTC))
    adsl %>% left_join(dm2, by="USUBJID") %>%
      filter(is.na(USUBJID) | is.na(RFPENDTC) | is_sas_missing(EOSDT) | is.na(RFPENDTC_DT) | EOSDT != RFPENDTC_DT) %>%
      # Note: SAS condition: d.USUBJID is null or missing(a.EOSDT) or a.EOSDT ne input(...)
      # So if no DM match, count as fail
      nrow() %>% as.integer()
  }
}
chk_adsl03 <- tibble(CHECK_ID="ADSL-003", DOMAIN="ADSL", RULE="EOSDT must agree with DM.RFPENDTC", N_FAIL=n_adsl03)

# ADSL-004 AGE must equal completed years at informed consent
n_adsl04 <- if (nrow(adsl)==0) 0L else {
  adsl %>% mutate(
    calc_age = completed_years(BRTHDT, RFICDT)
  ) %>% filter(is_sas_missing(BRTHDT) | is_sas_missing(RFICDT) | is.na(AGE) | is.na(calc_age) | AGE != calc_age) %>% nrow() %>% as.integer()
}
chk_adsl04 <- tibble(CHECK_ID="ADSL-004", DOMAIN="ADSL", RULE="AGE must equal completed years at informed consent", N_FAIL=n_adsl04)

# ADVS-002 Baseline date has exactly one eligible source record
n_advs02 <- if (nrow(advs)==0) 0L else {
  # inner join logic
  # First find max ASTDT per USUBJID,PARAMCD where ASTDT <= TRTSDT and not missing AVAL
  eligible <- advs %>% filter(not_sas_missing(ASTDT), not_sas_missing(TRTSDT), !is.na(AVAL), ASTDT <= TRTSDT)
  if (nrow(eligible)==0) 0L else {
    base_dt <- eligible %>% group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT, na.rm=TRUE), .groups="drop")
    # join back to advs where AVAL not missing and ASTDT == BASE_DT
    advs %>% filter(!is.na(AVAL)) %>%
      inner_join(base_dt, by=c("USUBJID","PARAMCD")) %>%
      filter(ASTDT == BASE_DT) %>%
      group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
  }
}
chk_advs02 <- tibble(CHECK_ID="ADVS-002", DOMAIN="ADVS", RULE="Baseline date has exactly one eligible source record", N_FAIL=n_advs02)

# ADLB-002 similarly
n_adlb02 <- if (nrow(adlb)==0) 0L else {
  eligible <- adlb %>% filter(not_sas_missing(ASTDT), not_sas_missing(TRTSDT), !is.na(AVAL), ASTDT <= TRTSDT)
  if (nrow(eligible)==0) 0L else {
    base_dt <- eligible %>% group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT, na.rm=TRUE), .groups="drop")
    adlb %>% filter(!is.na(AVAL)) %>%
      inner_join(base_dt, by=c("USUBJID","PARAMCD")) %>%
      filter(ASTDT == BASE_DT) %>%
      group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
  }
}
chk_adlb02 <- tibble(CHECK_ID="ADLB-002", DOMAIN="ADLB", RULE="Baseline date has exactly one eligible source record", N_FAIL=n_adlb02)

# RULE 12: SDTM EG-006
n_eg01 <- if (nrow(eg)==0) 0L else {
  if (nrow(dm)==0) nrow(eg) else {
    eg %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_eg01 <- tibble(CHECK_ID="SDTM-006", DOMAIN="EG", RULE="Every EG.USUBJID exists in DM", N_FAIL=n_eg01)

# RULE 13: SDTM EG-007 required variables must exist
required_eg <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST','EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
n_eg02 <- if (nrow(eg)==0) {
  # if dataset missing, then count distinct =0 => N_FAIL=12
  # But if eg exists but empty, still check columns
  if (is.null(eg) || length(names(eg))==0) 12L else {
    cnt <- sum(toupper(required_eg) %in% toupper(names(eg)))
    12L - as.integer(cnt)
  }
} else {
  cnt <- sum(toupper(required_eg) %in% toupper(names(eg)))
  12L - as.integer(cnt)
}
chk_eg02 <- tibble(CHECK_ID="SDTM-007", DOMAIN="EG", RULE="Required EG variables must exist", N_FAIL=n_eg02)

# RULE 14: SDTM EG-008
n_eg03 <- if (nrow(eg)==0) 0L else {
  eg %>% filter(
    is_sas_missing(EGDTC) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(EGDTC)) |
    is.na(EGSTRESN) |
    is_sas_missing(EGSTRESC)
  ) %>% nrow() %>% as.integer()
}
chk_eg03 <- tibble(CHECK_ID="SDTM-008", DOMAIN="EG", RULE="EG date/value contract must be valid", N_FAIL=n_eg03)

# RULE 15: SDTM EG-009 raw leak
raw_leak_eg <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
n_eg04 <- if (nrow(eg)==0) 0L else {
  sum(toupper(raw_leak_eg) %in% toupper(names(eg))) %>% as.integer()
}
chk_eg04 <- tibble(CHECK_ID="SDTM-009", DOMAIN="EG", RULE="Raw EG variables must not leak to SDTM", N_FAIL=n_eg04)

# RULE 16: SDTM MH-010
n_mh01 <- if (nrow(mh)==0) 0L else {
  if (nrow(dm)==0) nrow(mh) else {
    mh %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_mh01 <- tibble(CHECK_ID="SDTM-010", DOMAIN="MH", RULE="Every MH.USUBJID exists in DM", N_FAIL=n_mh01)

# RULE 17: SDTM MH-011
required_mh <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
n_mh02 <- if (is.null(mh) || length(names(mh))==0) 6L else {
  cnt <- sum(toupper(required_mh) %in% toupper(names(mh)))
  6L - as.integer(cnt)
}
chk_mh02 <- tibble(CHECK_ID="SDTM-011", DOMAIN="MH", RULE="Required MH variables must exist", N_FAIL=n_mh02)

# RULE 18: SDTM MH-012
n_mh03 <- if (nrow(mh)==0) 0L else {
  mh %>% filter(
    is_sas_missing(MHSTDTC) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(MHSTDTC)) |
    is_sas_missing(MHTERM)
  ) %>% nrow() %>% as.integer()
}
chk_mh03 <- tibble(CHECK_ID="SDTM-012", DOMAIN="MH", RULE="MH date/term contract must be valid", N_FAIL=n_mh03)

# RULE 19: SDTM MH-013 raw leak
raw_leak_mh <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')
n_mh04 <- if (nrow(mh)==0) 0L else {
  sum(toupper(raw_leak_mh) %in% toupper(names(mh))) %>% as.integer()
}
chk_mh04 <- tibble(CHECK_ID="SDTM-013", DOMAIN="MH", RULE="Raw MH variables must not leak to SDTM", N_FAIL=n_mh04)

# RULE 20: DM-014 reference period
n_dm02 <- if (nrow(dm)==0) 0L else {
  dm %>% filter(
    is_sas_missing(RFICDTC) | is_sas_missing(RFSTDTC) | is_sas_missing(RFENDTC) | is_sas_missing(RFPENDTC) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(RFICDTC)) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(RFSTDTC)) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(RFENDTC)) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(RFPENDTC)) |
    strip(RFSTDTC) < strip(RFICDTC) | strip(RFENDTC) < strip(RFSTDTC) | strip(RFPENDTC) != strip(RFENDTC)
  ) %>% nrow() %>% as.integer()
}
chk_dm02 <- tibble(CHECK_ID="SDTM-014", DOMAIN="DM", RULE="DM reference dates complete and ordered", N_FAIL=n_dm02)

# RULE 21: SV-015
n_sv01 <- if (nrow(sv)==0) 0L else {
  if (nrow(dm)==0) nrow(sv) else {
    sv %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>% nrow() %>% as.integer()
  }
}
chk_sv01 <- tibble(CHECK_ID="SDTM-015", DOMAIN="SV", RULE="Every SV.USUBJID exists in DM", N_FAIL=n_sv01)

# RULE 22: SV-016 unique USUBJID VISITNUM
n_sv02 <- if (nrow(sv)==0) 0L else {
  sv %>% filter(not_sas_missing(USUBJID), !is.na(VISITNUM)) %>%
    group_by(USUBJID, VISITNUM) %>% summarise(n=n(), .groups="drop") %>% filter(n != 1) %>% nrow() %>% as.integer()
}
chk_sv02 <- tibble(CHECK_ID="SDTM-016", DOMAIN="SV", RULE="USUBJID and VISITNUM must be unique", N_FAIL=n_sv02)

# RULE 23: SV-017
n_sv03 <- if (nrow(sv)==0 || nrow(dm)==0) 0L else {
  sv %>% inner_join(dm %>% select(USUBJID, RFSTDTC, RFENDTC), by="USUBJID") %>%
    filter(
      is.na(VISITNUM) | is_sas_missing(VISIT) | is_sas_missing(SVSTDTC) | is_sas_missing(SVENDTC) |
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(SVSTDTC)) |
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(SVENDTC)) |
      strip(SVENDTC) < strip(SVSTDTC) |
      strip(SVSTDTC) < strip(RFSTDTC) | strip(SVENDTC) > strip(RFENDTC)
    ) %>% nrow() %>% as.integer()
}
chk_sv03 <- tibble(CHECK_ID="SDTM-017", DOMAIN="SV", RULE="SV dates valid and inside reference period", N_FAIL=n_sv03)

# RULE 24: DS-018 exactly one final DS per DM subject
n_ds01 <- if (nrow(dm)==0) 0L else {
  # left join count
  dm_counts <- dm %>% select(USUBJID) %>% distinct() %>%
    left_join(ds %>% group_by(USUBJID) %>% summarise(N_DS=n(), .groups="drop"), by="USUBJID") %>%
    mutate(N_DS = ifelse(is.na(N_DS), 0, N_DS)) %>%
    filter(N_DS != 1) %>% nrow()
  # plus DS subjects not in DM
  extra <- if (nrow(ds)==0) 0L else {
    ds %>% anti_join(dm %>% select(USUBJID) %>% distinct(), by="USUBJID") %>%
      group_by(USUBJID) %>% summarise(N_DS=n(), .groups="drop") %>% nrow()
  }
  as.integer(dm_counts + extra)
}
chk_ds01 <- tibble(CHECK_ID="SDTM-018", DOMAIN="DS", RULE="Exactly one final DS record per DM subject", N_FAIL=n_ds01)

# RULE 25: DS-019 DS date matches DM participation end
n_ds02 <- if (nrow(ds)==0 || nrow(dm)==0) 0L else {
  ds %>% inner_join(dm %>% select(USUBJID, RFPENDTC), by="USUBJID") %>%
    filter(
      is_sas_missing(DSDECOD) | is_sas_missing(DSSTDTC) |
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(DSSTDTC)) |
      strip(DSSTDTC) != strip(RFPENDTC)
    ) %>% nrow() %>% as.integer()
}
chk_ds02 <- tibble(CHECK_ID="SDTM-019", DOMAIN="DS", RULE="DS date matches DM participation end", N_FAIL=n_ds02)

# RULE 26: DM-020 RACE terminology
valid_race <- c('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')
n_dm03 <- if (nrow(dm)==0) 0L else {
  dm %>% filter(is_sas_missing(RACE) | !(toupper(strip(RACE)) %in% valid_race)) %>% nrow() %>% as.integer()
}
chk_dm03 <- tibble(CHECK_ID="SDTM-020", DOMAIN="DM", RULE="RACE uses supported CDISC terminology", N_FAIL=n_dm03)

# RULE 27: DM-021 SITEID
n_dm04 <- if (nrow(dm)==0) 0L else {
  dm %>% mutate(
    site_from_subjid = sapply(strip(SUBJID), function(x) {
      if (is.na(x) || x=="") return(NA_character_)
      parts <- strsplit(x, "-", fixed=TRUE)[[1]]
      if (length(parts)>=1) parts[1] else NA_character_
    })
  ) %>% filter(is_sas_missing(SITEID) | strip(SITEID) != site_from_subjid) %>% nrow() %>% as.integer()
}
chk_dm04 <- tibble(CHECK_ID="SDTM-021", DOMAIN="DM", RULE="SITEID present and consistent with SUBJID", N_FAIL=n_dm04)

# RULE 28: ADAE-002 AOCCFL
n_adae02 <- if (nrow(adae)==0) 0L else {
  adae %>% filter(
    (!is_sas_missing(AOCCFL) & AOCCFL != "Y") |
    (AOCCFL == "Y" & (is.na(TRTEMFL) | TRTEMFL != "Y"))
  ) %>% nrow() %>% as.integer()
}
chk_adae02 <- tibble(CHECK_ID="ADAE-002", DOMAIN="ADAE", RULE="AOCCFL valid and restricted to TRTEMFL=Y", N_FAIL=n_adae02)

# RULE 29: ADAE-003 One AOCCFL=Y per subject and TEAE term
n_adae03 <- if (nrow(adae)==0) 0L else {
  adae %>% group_by(USUBJID, AEDECOD) %>%
    summarise(
      n_teae = sum(TRTEMFL=="Y", na.rm=TRUE),
      n_aocc = sum(AOCCFL=="Y", na.rm=TRUE),
      .groups="drop"
    ) %>% filter(n_teae > 0 & n_aocc != 1) %>% nrow() %>% as.integer()
}
chk_adae03 <- tibble(CHECK_ID="ADAE-003", DOMAIN="ADAE", RULE="One AOCCFL=Y per subject and TEAE term", N_FAIL=n_adae03)

# RULE 30: CM-022
n_cm01 <- if (nrow(cm)==0) 0L else {
  cm %>% left_join(dm %>% select(USUBJID) %>% distinct() %>% mutate(in_dm=1), by="USUBJID") %>%
    filter(
      is.na(in_dm) |
      is_sas_missing(CMTRT) | is_sas_missing(CMDECOD) |
      (toupper(strip(CMTRT))=="PARACETAMOL" & toupper(strip(CMDECOD)) != "ACETAMINOPHEN") |
      is.na(CMDOSE) | is_sas_missing(CMDOSU) |
      is_sas_missing(CMSTDTC) |
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(CMSTDTC)) |
      (!is_sas_missing(CMENDTC) & !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(CMENDTC))) |
      (!is_sas_missing(CMENDTC) & strip(CMENDTC) < strip(CMSTDTC)) |
      (toupper(strip(CMENRTPT))=="ONGOING" & !is_sas_missing(CMENDTC)) |
      (toupper(strip(CMENRTPT))!="ONGOING" & is_sas_missing(CMENDTC))
    ) %>% nrow() %>% as.integer()
}
chk_cm01 <- tibble(CHECK_ID="SDTM-022", DOMAIN="CM", RULE="CM subject, medication, dose and dates must be valid", N_FAIL=n_cm01)

# RULE 31: DS-023 DSDECOD terminology
valid_dsdecod <- c('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT','ADVERSE EVENT','SCREEN FAILURE')
n_ds03 <- if (nrow(ds)==0) 0L else {
  ds %>% filter(!(toupper(strip(DSDECOD)) %in% valid_dsdecod)) %>% nrow() %>% as.integer()
}
chk_ds03 <- tibble(CHECK_ID="SDTM-023", DOMAIN="DS", RULE="DSDECOD uses supported disposition terminology", N_FAIL=n_ds03)

# RULE 32: SEQ-024 unique per subject
count_dup_seq <- function(df, seqcol) {
  if (is.null(df) || nrow(df)==0 || !("USUBJID" %in% names(df)) || !(toupper(seqcol) %in% names(df))) return(0L)
  df %>% filter(not_sas_missing(USUBJID), !is.na(.data[[toupper(seqcol)]])) %>%
    group_by(USUBJID, SEQ = .data[[toupper(seqcol)]]) %>% summarise(n=n(), .groups="drop") %>% filter(n>1) %>% nrow() %>% as.integer()
}
n_seq <- 0L
n_seq <- n_seq + count_dup_seq(ae, "AESEQ")
n_seq <- n_seq + count_dup_seq(ex, "EXSEQ")
n_seq <- n_seq + count_dup_seq(lb, "LBSEQ")
n_seq <- n_seq + count_dup_seq(vs, "VSSEQ")
n_seq <- n_seq + count_dup_seq(cm, "CMSEQ")
n_seq <- n_seq + count_dup_seq(mh, "MHSEQ")
n_seq <- n_seq + count_dup_seq(eg, "EGSEQ")
chk_seq01 <- tibble(CHECK_ID="SDTM-024", DOMAIN="SDTM", RULE="Domain sequence keys must be unique per subject", N_FAIL=n_seq)

# RULE 39: CM-025 ongoing anchored
n_cm02 <- if (nrow(cm)==0 || nrow(dm)==0) 0L else {
  cm %>% inner_join(dm %>% select(USUBJID, RFENDTC), by="USUBJID") %>%
    filter(
      !(toupper(strip(CMENRTPT)) %in% c("","ONGOING")) |
      (toupper(strip(CMENRTPT))=="ONGOING" & (
        !is_sas_missing(CMENDTC) |
        is_sas_missing(CMENTPT) |
        !grepl("^\\d{4}-\\d{2}-\\d{2}$", strip(CMENTPT)) |
        strip(CMENTPT) != strip(RFENDTC)
      )) |
      (toupper(strip(CMENRTPT))!="ONGOING" & !is_sas_missing(CMENTPT))
    ) %>% nrow() %>% as.integer()
}
chk_cm02 <- tibble(CHECK_ID="SDTM-025", DOMAIN="CM", RULE="Ongoing CM must be anchored to the DM reference end", N_FAIL=n_cm02)

# Consolidate in SAS order
qc_report <- bind_rows(
  chk_adsl01, chk_adae01, chk_advs01, chk_adlb01,
  chk_advs02, chk_adlb02, chk_dm01,
  chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04,
  chk_adtte01, chk_adtte02, chk_adtte03,
  chk_eg01, chk_eg02, chk_eg03, chk_eg04,
  chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02,
  chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02,
  chk_dm03, chk_dm04, chk_adae02, chk_adae03,
  chk_cm01, chk_ds03, chk_seq01, chk_cm02
) %>%
  mutate(
    STATUS = ifelse(N_FAIL==0, "PASS", "FAIL"),
    # enforce lengths as in SAS (not needed but keep)
    CHECK_ID = as.character(CHECK_ID),
    DOMAIN = as.character(DOMAIN),
    RULE = as.character(RULE)
  )

# Write dataset
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names=FALSE, na="")

# Create text rendering (P019.lst)
# SAS proc print noobs var CHECK_ID DOMAIN RULE N_FAIL STATUS
# Build plain text table
header <- "Automated Quality Control (QC) Validation Report"
col_names <- c("CHECK_ID","DOMAIN","RULE","N_FAIL","STATUS")
# Format table as fixed width
# Create txt file
txt_path <- file.path(out, "P019.txt")
# Also need P019.lst? spec says P019.lst -> write text rendering and CSV
# We'll write both P019.txt and P019.lst as same
lst_path <- file.path(out, "P019.lst")

# Build text content
lines <- c(header, paste(rep("-", nchar(header)), collapse=""), "")
# header row
fmt_header <- sprintf("%-10s %-10s %-60s %6s %-6s", "CHECK_ID","DOMAIN","RULE","N_FAIL","STATUS")
lines <- c(lines, fmt_header)
lines <- c(lines, paste(rep("-", nchar(fmt_header)), collapse=""))
for (i in seq_len(nrow(qc_report))) {
  r <- qc_report[i,]
  # truncate RULE to 60 for display but keep full in CSV
  rule_disp <- str_trunc(r$RULE, 60, side="right", ellipsis="")
  lines <- c(lines, sprintf("%-10s %-10s %-60s %6d %-6s", r$CHECK_ID, r$DOMAIN, rule_disp, r$N_FAIL, r$STATUS))
}
lines <- c(lines, "", paste0("Total checks: ", nrow(qc_report), "  Failed: ", sum(qc_report$STATUS=="FAIL")))
# Count numbers in text: ensure 156 numbers? The spec says 156 numbers - we include N_FAIL values
writeLines(lines, txt_path)
writeLines(lines, lst_path)

# Also write CSV of table body (P019.csv)
write.csv(qc_report, file.path(out, "P019.csv"), row.names=FALSE, na="")

# Log progress
cat("QC report generated:", nrow(qc_report), "checks\n")
cat("Failed:", sum(qc_report$STATUS=="FAIL"), "\n")
