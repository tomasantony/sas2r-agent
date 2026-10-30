out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path  <- file.path(project_path, "data/raw")
sdtm_path <- file.path(project_path, "data/sdtm")
adam_path <- file.path(project_path, "data/adam")

library(haven)
library(dplyr)
library(stringr)
library(readr)

# helper: list files in a directory
list_files <- function(p) {
  if (!dir.exists(p)) return(character(0))
  list.files(p, recursive = TRUE, full.names = TRUE)
}

# helper: read SAS dataset (.sas7bdat) or .xpt if present
read_sas_dir <- function(dir, memname) {
  # memname case-insensitive
  files <- list_files(dir)
  # try exact match
  pat <- paste0("(?i)/", memname, "\\.(sas7bdat|xpt)$")
  idx <- grep(pat, files)
  if (length(idx) == 0) {
    # also try without extension search
    pat2 <- paste0("(?i)", memname)
    idx <- grep(pat2, files)
  }
  if (length(idx) == 0) return(NULL)
  f <- files[idx[1]]
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # uppercase names to mimic SAS validvarname=upcase
  names(df) <- toupper(names(df))
  df
}

# helper: get column names for dictionary.columns emulation
get_colnames <- function(df) {
  if (is.null(df)) character(0) else toupper(names(df))
}

# helper: SAS missing check
is_sas_missing <- function(x) {
  if (is.character(x)) {
    is.na(x) | trimws(x) == ""
  } else {
    is.na(x)
  }
}
not_sas_missing <- function(x) !is_sas_missing(x)

# helper: SAS strip = trim both
sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(trimws(as.character(x))))
}

# helper: SAS prxmatch for ^\d{4}-\d{2}-\d{2}$ on strip
is_iso_date <- function(x) {
  s <- sas_strip(x)
  !is.na(s) & grepl("^\\d{4}-\\d{2}-\\d{2}$", s)
}

# helper: SAS yymmdd10. input of substr(RFPENDTC,1,10)
parse_yymmdd10 <- function(x) {
  # x is character, take first 10 chars
  s <- substr(as.character(x), 1, 10)
  # SAS yymmdd10. expects YYYY-MM-DD or YYYY/MM/DD etc; here ISO
  as.Date(s, format = "%Y-%m-%d")
}

# helper: SAS yrdif with 'AGE' -> completed years
# SAS docs: YRDIF(start,end,'AGE') computes age in years, considering leap years
# floor of that equals completed years = year diff minus 1 if birthday not yet reached
completed_years <- function(brthdt, rficdt) {
  # both Date
  # vectorized
  yb <- as.integer(format(brthdt, "%Y"))
  mb <- as.integer(format(brthdt, "%m"))
  db <- as.integer(format(brthdt, "%d"))
  yr <- as.integer(format(rficdt, "%Y"))
  mr <- as.integer(format(rficdt, "%m"))
  dr <- as.integer(format(rficdt, "%d"))
  age <- yr - yb - ifelse(mr < mb | (mr == mb & dr < db), 1L, 0L)
  age
}

# helper: SAS scan(string,1,'-')
sas_scan1 <- function(x, delim = "-") {
  s <- sas_strip(x)
  vapply(s, function(one) {
    if (is.na(one) || one == "") return(NA_character_)
    parts <- strsplit(one, delim, fixed = TRUE)[[1]]
    trimws(parts[1])
  }, character(1))
}

# helper: SAS floor(yrdif(...)) already done

# Load all needed datasets
adsl  <- read_sas_dir(adam_path, "adsl")
adae  <- read_sas_dir(adam_path, "adae")
advs  <- read_sas_dir(adam_path, "advs")
adlb  <- read_sas_dir(adam_path, "adlb")
adtte <- read_sas_dir(adam_path, "adtte")

dm <- read_sas_dir(sdtm_path, "dm")
ae <- read_sas_dir(sdtm_path, "ae")
ex <- read_sas_dir(sdtm_path, "ex")
lb <- read_sas_dir(sdtm_path, "lb")
vs <- read_sas_dir(sdtm_path, "vs")
eg <- read_sas_dir(sdtm_path, "eg")
mh <- read_sas_dir(sdtm_path, "mh")
sv <- read_sas_dir(sdtm_path, "sv")
ds <- read_sas_dir(sdtm_path, "ds")
cm <- read_sas_dir(sdtm_path, "cm")

# Ensure data frames exist (empty if missing)
empty_df <- function() data.frame()
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

# Normalize names already upper
# Convert date-like numeric SAS dates to Date if needed
# In ADaM, dates like BRTHDT, RFICDT, TRTSDT, EOSDT, AESTDT, AEENDT, ASTDT, EVENTDT etc may be numeric SAS dates or Date
# haven reads SAS dates as Date already (with origin 1960-01-01) if format is date. Check class.
# For safety, if column is numeric and looks like SAS date (large integer), convert? But haven already does.
# We'll treat any Date or numeric date as Date; if numeric and not Date, try to convert if column name ends with DT
convert_sas_numeric_dates <- function(df) {
  if (nrow(df)==0) return(df)
  for (nm in names(df)) {
    if (grepl("DT$", nm)) {
      # if numeric and not Date, and values are plausible SAS date numbers (e.g., 10000-25000)
      if (is.numeric(df[[nm]]) && !inherits(df[[nm]], "Date")) {
        # check if values are integers in SAS date range
        vals <- df[[nm]][!is.na(df[[nm]])]
        if (length(vals)>0 && all(vals > 5000 & vals < 30000, na.rm=TRUE)) {
          df[[nm]] <- as.Date(df[[nm]], origin = "1960-01-01")
        }
      }
      # if character that looks like YYYY-MM-DD, convert to Date for comparisons where needed
      # But keep original character for DTC checks; for DT numeric checks we need Date
      # For ADaM DT columns, they should be Date; if character ISO, convert
      if (is.character(df[[nm]])) {
        # try ISO parse, but keep character if fails? For ADaM we want Date
        # Only convert if all non-missing match ISO
        nonmiss <- df[[nm]][!is_sas_missing(df[[nm]])]
        if (length(nonmiss)>0 && all(grepl("^\\d{4}-\\d{2}-\\d{2}", nonmiss))) {
          df[[nm]] <- as.Date(substr(df[[nm]],1,10), format="%Y-%m-%d")
        }
      }
    }
  }
  df
}

adsl  <- convert_sas_numeric_dates(adsl)
adae  <- convert_sas_numeric_dates(adae)
advs  <- convert_sas_numeric_dates(advs)
adlb  <- convert_sas_numeric_dates(adlb)
adtte <- convert_sas_numeric_dates(adtte)
# SDTM DTC columns stay character; don't convert DM RF*DTC etc
# But for DM we need to keep DTC as character for regex checks

# Helper to count N_FAIL for each rule
# RULE 1: ADSL-001
chk_adsl01_n <- 0
if ("USUBJID" %in% names(adsl) && nrow(adsl)>0) {
  dup <- adsl %>% group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
  chk_adsl01_n <- nrow(dup)
}

# RULE 2: ADAE-001
chk_adae01_n <- 0
if (all(c("AEENDT","AESTDT") %in% names(adae)) && nrow(adae)>0) {
  chk_adae01_n <- adae %>% filter(!is_sas_missing(AEENDT), !is_sas_missing(AESTDT), AEENDT < AESTDT) %>% nrow()
}

# RULE 3: ADVS-001
chk_advs01_n <- 0
if (all(c("USUBJID","PARAMCD","ABLFL") %in% names(advs)) && nrow(advs)>0) {
  tmp <- advs %>% filter(ABLFL=="Y") %>% group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
  chk_advs01_n <- nrow(tmp)
}

# RULE 4: ADLB-001
chk_adlb01_n <- 0
if (all(c("USUBJID","PARAMCD","ABLFL") %in% names(adlb)) && nrow(adlb)>0) {
  tmp <- adlb %>% filter(ABLFL=="Y") %>% group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
  chk_adlb01_n <- nrow(tmp)
}

# RULE 5: SDTM-001 DM duplicate
chk_dm01_n <- 0
if ("USUBJID" %in% names(dm) && nrow(dm)>0) {
  dup <- dm %>% group_by(USUBJID) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
  chk_dm01_n <- nrow(dup)
}

# RULE 6: SDTM-002 AE referential
chk_ae01_n <- 0
if (all(c("USUBJID") %in% names(ae)) && nrow(ae)>0) {
  if ("USUBJID" %in% names(dm)) {
    dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
    chk_ae01_n <- sum(!ae$USUBJID %in% dm_ids, na.rm=TRUE)
    # also count where ae USUBJID is missing? SAS left join where d.USUBJID is null includes missing? But missing not in dm, so counted
    # handle NA
    chk_ae01_n <- chk_ae01_n + sum(is.na(ae$USUBJID))
  } else {
    chk_ae01_n <- nrow(ae)
  }
}

# RULE 7: SDTM-003 EX referential
chk_ex01_n <- 0
if ("USUBJID" %in% names(ex) && nrow(ex)>0) {
  if ("USUBJID" %in% names(dm)) {
    dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
    chk_ex01_n <- sum(!ex$USUBJID %in% dm_ids, na.rm=TRUE) + sum(is.na(ex$USUBJID))
  } else chk_ex01_n <- nrow(ex)
}

# RULE 8: SDTM-004 LB referential
chk_lb01_n <- 0
if ("USUBJID" %in% names(lb) && nrow(lb)>0) {
  dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  chk_lb01_n <- sum(!lb$USUBJID %in% dm_ids, na.rm=TRUE) + sum(is.na(lb$USUBJID))
}

# RULE 9: SDTM-005 VS referential
chk_vs01_n <- 0
if ("USUBJID" %in% names(vs) && nrow(vs)>0) {
  dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  chk_vs01_n <- sum(!vs$USUBJID %in% dm_ids, na.rm=TRUE) + sum(is.na(vs$USUBJID))
}

# RULE 10: ADSL-002 SAFFL=Y requires EX
chk_adsl02_n <- 0
if ("USUBJID" %in% names(adsl) && "SAFFL" %in% names(adsl) && nrow(adsl)>0) {
  if ("USUBJID" %in% names(ex) && nrow(ex)>0) {
    ex_ids <- unique(ex$USUBJID[!is_sas_missing(ex$USUBJID)])
    tmp <- adsl %>% filter(SAFFL=="Y")
    chk_adsl02_n <- sum(!tmp$USUBJID %in% ex_ids, na.rm=TRUE) + sum(is.na(tmp$USUBJID))
  } else {
    chk_adsl02_n <- adsl %>% filter(SAFFL=="Y") %>% nrow()
  }
}

# RULE 11: ADTTE-001 AVAL >=1
chk_adtte01_n <- 0
if ("AVAL" %in% names(adtte) && nrow(adtte)>0) {
  chk_adtte01_n <- adtte %>% filter(is_sas_missing(AVAL) | AVAL < 1) %>% nrow()
}

# RULE ADTTE-002
chk_adtte02_n <- 0
if (nrow(adtte)>0) {
  # ensure columns exist
  for (col in c("TRTSDT","CNSR","EVENTDT","EOSDT","AVAL")) if (!col %in% names(adtte)) adtte[[col]] <- NA
  # convert dates if needed (already)
  chk_adtte02_n <- adtte %>% filter(
    is_sas_missing(TRTSDT) |
    is_sas_missing(CNSR) |
    !CNSR %in% c(0,1) |
    (CNSR==0 & (is_sas_missing(EVENTDT) | AVAL != as.numeric(EVENTDT - TRTSDT) + 1)) |
    (CNSR==1 & (!is_sas_missing(EVENTDT) | is_sas_missing(EOSDT) | AVAL != as.numeric(EOSDT - TRTSDT) + 1))
  ) %>% nrow()
}

# RULE ADTTE-003
chk_adtte03_n <- 0
if (nrow(adtte)>0) {
  for (col in c("EOSDT","TRTSDT","EVENTDT")) if (!col %in% names(adtte)) adtte[[col]] <- NA
  chk_adtte03_n <- adtte %>% filter(
    is_sas_missing(EOSDT) |
    EOSDT < TRTSDT |
    (!is_sas_missing(EVENTDT) & (EVENTDT < TRTSDT | EVENTDT > EOSDT))
  ) %>% nrow()
}

# RULE ADSL-003 EOSDT must agree with DM.RFPENDTC
chk_adsl03_n <- 0
if (nrow(adsl)>0 && nrow(dm)>0) {
  for (col in c("EOSDT","USUBJID")) if (!col %in% names(adsl)) adsl[[col]] <- NA
  if (!"RFPENDTC" %in% names(dm)) dm$RFPENDTC <- NA_character_
  # left join
  tmp <- adsl %>% select(USUBJID, EOSDT) %>% left_join(dm %>% select(USUBJID, RFPENDTC), by="USUBJID", suffix=c("",".dm"))
  # SAS: where d.USUBJID is null or missing(a.EOSDT) or a.EOSDT ne input(substr(d.RFPENDTC,1,10), yymmdd10.)
  # d.USUBJID is null means no match
  # For matched, compare EOSDT (Date) vs parsed RFPENDTC
  tmp$RFPEND_DT <- parse_yymmdd10(tmp$RFPENDTC)
  chk_adsl03_n <- tmp %>% filter(
    is.na(RFPEND_DT) | is_sas_missing(RFPENDTC) | # d.USUBJID is null -> RFPENDTC NA
    is_sas_missing(EOSDT) |
    EOSDT != RFPEND_DT
  ) %>% nrow()
  # Note: if RFPENDTC is NA, then d.USUBJID is null case already counted; but also if RFPENDTC present but parse fails, treat as mismatch
}

# RULE ADSL-004 AGE must equal completed years at informed consent
chk_adsl04_n <- 0
if (nrow(adsl)>0) {
  for (col in c("BRTHDT","RFICDT","AGE")) if (!col %in% names(adsl)) adsl[[col]] <- NA
  # BRTHDT and RFICDT should be Date
  # Compute expected age
  # Need to handle missing
  tmp <- adsl
  # Ensure Date type
  # If character, parse
  # Already converted
  expected <- rep(NA_integer_, nrow(tmp))
  valid <- !is_sas_missing(tmp$BRTHDT) & !is_sas_missing(tmp$RFICDT)
  if (any(valid, na.rm=TRUE)) {
    expected[valid] <- completed_years(tmp$BRTHDT[valid], tmp$RFICDT[valid])
  }
  chk_adsl04_n <- tmp %>% mutate(EXP = expected) %>% filter(
    is_sas_missing(BRTHDT) | is_sas_missing(RFICDT) | is_sas_missing(AGE) | AGE != EXP
  ) %>% nrow()
}

# RULE ADVS-002
chk_advs02_n <- 0
if (nrow(advs)>0 && all(c("USUBJID","PARAMCD","ASTDT","TRTSDT","AVAL") %in% names(advs))) {
  # inner join logic
  # b: max ASTDT per USUBJID,PARAMCD where ASTDT <= TRTSDT and not missing AVAL
  b <- advs %>% filter(!is_sas_missing(ASTDT), !is_sas_missing(TRTSDT), ASTDT <= TRTSDT, !is_sas_missing(AVAL)) %>%
    group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT, na.rm=TRUE), .groups="drop")
  if (nrow(b)>0) {
    a2 <- advs %>% filter(!is_sas_missing(AVAL)) %>% inner_join(b, by=c("USUBJID","PARAMCD"))
    a2 <- a2 %>% filter(ASTDT == BASE_DT)
    cnt <- a2 %>% group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
    chk_advs02_n <- nrow(cnt)
  }
}

# RULE ADLB-002
chk_adlb02_n <- 0
if (nrow(adlb)>0 && all(c("USUBJID","PARAMCD","ASTDT","TRTSDT","AVAL") %in% names(adlb))) {
  b <- adlb %>% filter(!is_sas_missing(ASTDT), !is_sas_missing(TRTSDT), ASTDT <= TRTSDT, !is_sas_missing(AVAL)) %>%
    group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT, na.rm=TRUE), .groups="drop")
  if (nrow(b)>0) {
    a2 <- adlb %>% filter(!is_sas_missing(AVAL)) %>% inner_join(b, by=c("USUBJID","PARAMCD"))
    a2 <- a2 %>% filter(ASTDT == BASE_DT)
    cnt <- a2 %>% group_by(USUBJID, PARAMCD) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
    chk_adlb02_n <- nrow(cnt)
  }
}

# RULE 12: SDTM-006 EG referential
chk_eg01_n <- 0
if (nrow(eg)>0 && "USUBJID" %in% names(eg)) {
  dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  chk_eg01_n <- sum(!eg$USUBJID %in% dm_ids, na.rm=TRUE) + sum(is.na(eg$USUBJID))
}

# RULE 13: SDTM-007 EG required variables must exist
chk_eg02_n <- 0
{
  required <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST','EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
  present <- intersect(toupper(required), get_colnames(eg))
  chk_eg02_n <- 12 - length(unique(present))
  if (chk_eg02_n <0) chk_eg02_n <- 0
}

# RULE 14: SDTM-008 EG date/value contract
chk_eg03_n <- 0
if (nrow(eg)>0) {
  for (col in c("EGDTC","EGSTRESN","EGSTRESC")) if (!col %in% names(eg)) eg[[col]] <- NA
  chk_eg03_n <- eg %>% filter(
    is_sas_missing(EGDTC) |
    !is_iso_date(EGDTC) |
    is_sas_missing(EGSTRESN) |
    is_sas_missing(EGSTRESC)
  ) %>% nrow()
}

# RULE 15: SDTM-009 Raw EG variables must not leak
chk_eg04_n <- 0
{
  leak <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
  present <- intersect(toupper(leak), get_colnames(eg))
  chk_eg04_n <- length(present)
}

# RULE 16: SDTM-010 MH referential
chk_mh01_n <- 0
if (nrow(mh)>0 && "USUBJID" %in% names(mh)) {
  dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  chk_mh01_n <- sum(!mh$USUBJID %in% dm_ids, na.rm=TRUE) + sum(is.na(mh$USUBJID))
}

# RULE 17: SDTM-011 MH required variables
chk_mh02_n <- 0
{
  required <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
  present <- intersect(toupper(required), get_colnames(mh))
  chk_mh02_n <- 6 - length(unique(present))
  if (chk_mh02_n <0) chk_mh02_n <- 0
}

# RULE 18: SDTM-012 MH date/term contract
chk_mh03_n <- 0
if (nrow(mh)>0) {
  for (col in c("MHSTDTC","MHTERM")) if (!col %in% names(mh)) mh[[col]] <- NA
  chk_mh03_n <- mh %>% filter(
    is_sas_missing(MHSTDTC) |
    !is_iso_date(MHSTDTC) |
    is_sas_missing(MHTERM)
  ) %>% nrow()
}

# RULE 19: SDTM-013 MH raw leak
chk_mh04_n <- 0
{
  leak <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')
  present <- intersect(toupper(leak), get_colnames(mh))
  chk_mh04_n <- length(present)
}

# RULE 20: SDTM-014 DM reference period
chk_dm02_n <- 0
if (nrow(dm)>0) {
  for (col in c("RFICDTC","RFSTDTC","RFENDTC","RFPENDTC")) if (!col %in% names(dm)) dm[[col]] <- NA_character_
  chk_dm02_n <- dm %>% filter(
    is_sas_missing(RFICDTC) | is_sas_missing(RFSTDTC) | is_sas_missing(RFENDTC) | is_sas_missing(RFPENDTC) |
    !is_iso_date(RFICDTC) | !is_iso_date(RFSTDTC) | !is_iso_date(RFENDTC) | !is_iso_date(RFPENDTC) |
    RFSTDTC < RFICDTC | RFENDTC < RFSTDTC | RFPENDTC != RFENDTC
  ) %>% nrow()
}

# RULE 21: SDTM-015 SV referential
chk_sv01_n <- 0
if (nrow(sv)>0 && "USUBJID" %in% names(sv)) {
  dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  chk_sv01_n <- sum(!sv$USUBJID %in% dm_ids, na.rm=TRUE) + sum(is.na(sv$USUBJID))
}

# RULE 22: SDTM-016 SV unique
chk_sv02_n <- 0
if (nrow(sv)>0 && all(c("USUBJID","VISITNUM") %in% names(sv))) {
  tmp <- sv %>% group_by(USUBJID, VISITNUM) %>% summarise(n=n(), .groups="drop") %>% filter(n != 1)
  chk_sv02_n <- nrow(tmp)
}

# RULE 23: SDTM-017 SV dates valid and inside reference period
chk_sv03_n <- 0
if (nrow(sv)>0 && nrow(dm)>0) {
  for (col in c("VISITNUM","VISIT","SVSTDTC","SVENDTC","USUBJID")) if (!col %in% names(sv)) sv[[col]] <- NA
  for (col in c("RFSTDTC","RFENDTC","USUBJID")) if (!col %in% names(dm)) dm[[col]] <- NA
  tmp <- sv %>% inner_join(dm %>% select(USUBJID, RFSTDTC, RFENDTC), by="USUBJID")
  chk_sv03_n <- tmp %>% filter(
    is_sas_missing(VISITNUM) | is_sas_missing(VISIT) | is_sas_missing(SVSTDTC) | is_sas_missing(SVENDTC) |
    !is_iso_date(SVSTDTC) | !is_iso_date(SVENDTC) |
    SVENDTC < SVSTDTC |
    SVSTDTC < RFSTDTC | SVENDTC > RFENDTC
  ) %>% nrow()
}

# RULE 24: SDTM-018 DS exactly one per DM subject
chk_ds01_n <- 0
if (nrow(dm)>0) {
  # left join counts
  # part1: DM left join DS
  dm_ids <- unique(dm$USUBJID)
  ds_counts <- ds %>% group_by(USUBJID) %>% summarise(N_DS=n(), .groups="drop")
  # for each DM subject, count DS
  dm_ds <- data.frame(USUBJID=dm_ids) %>% left_join(ds_counts, by="USUBJID") %>% mutate(N_DS=ifelse(is.na(N_DS),0,N_DS))
  part1 <- sum(dm_ds$N_DS != 1)
  # part2: DS left join DM where DM null
  dm_set <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  orphan <- ds %>% filter(!USUBJID %in% dm_set | is.na(USUBJID)) %>% group_by(USUBJID) %>% summarise(N_DS=n(), .groups="drop")
  part2 <- nrow(orphan)
  chk_ds01_n <- part1 + part2
}

# RULE 25: SDTM-019 DS date matches DM participation end
chk_ds02_n <- 0
if (nrow(ds)>0 && nrow(dm)>0) {
  for (col in c("DSDECOD","DSSTDTC","USUBJID")) if (!col %in% names(ds)) ds[[col]] <- NA
  tmp <- ds %>% inner_join(dm %>% select(USUBJID, RFPENDTC), by="USUBJID")
  chk_ds02_n <- tmp %>% filter(
    is_sas_missing(DSDECOD) | is_sas_missing(DSSTDTC) |
    !is_iso_date(DSSTDTC) |
    DSSTDTC != RFPENDTC
  ) %>% nrow()
}

# RULE 26: SDTM-020 RACE terminology
chk_dm03_n <- 0
if (nrow(dm)>0 && "RACE" %in% names(dm)) {
  allowed <- c('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')
  chk_dm03_n <- dm %>% filter(is_sas_missing(RACE) | !toupper(trimws(RACE)) %in% allowed) %>% nrow()
}

# RULE 27: SDTM-021 SITEID present and consistent with SUBJID
chk_dm04_n <- 0
if (nrow(dm)>0) {
  for (col in c("SITEID","SUBJID")) if (!col %in% names(dm)) dm[[col]] <- NA
  chk_dm04_n <- dm %>% filter(is_sas_missing(SITEID) | sas_strip(SITEID) != sas_scan1(SUBJID)) %>% nrow()
}

# RULE 28: ADAE-002 AOCCFL valid and restricted to TRTEMFL=Y
chk_adae02_n <- 0
if (nrow(adae)>0) {
  for (col in c("AOCCFL","TRTEMFL")) if (!col %in% names(adae)) adae[[col]] <- NA
  chk_adae02_n <- adae %>% filter(
    (!is_sas_missing(AOCCFL) & AOCCFL != "Y") |
    (AOCCFL=="Y" & TRTEMFL != "Y")
  ) %>% nrow()
}

# RULE 29: ADAE-003 One AOCCFL=Y per subject and TEAE term
chk_adae03_n <- 0
if (nrow(adae)>0 && all(c("USUBJID","AEDECOD","TRTEMFL","AOCCFL") %in% names(adae))) {
  tmp <- adae %>% group_by(USUBJID, AEDECOD) %>%
    summarise(
      n_trtem = sum(TRTEMFL=="Y", na.rm=TRUE),
      n_aocc = sum(AOCCFL=="Y", na.rm=TRUE),
      .groups="drop"
    ) %>% filter(n_trtem >0 & n_aocc !=1)
  chk_adae03_n <- nrow(tmp)
}

# RULE 30: SDTM-022 CM subject, medication, dose and dates must be valid
chk_cm01_n <- 0
if (nrow(cm)>0) {
  for (col in c("USUBJID","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT")) if (!col %in% names(cm)) cm[[col]] <- NA
  dm_ids <- unique(dm$USUBJID[!is_sas_missing(dm$USUBJID)])
  chk_cm01_n <- cm %>% filter(
    !USUBJID %in% dm_ids | is.na(USUBJID) |
    is_sas_missing(CMTRT) | is_sas_missing(CMDECOD) |
    (toupper(trimws(CMTRT))=="PARACETAMOL" & toupper(trimws(CMDECOD)) != "ACETAMINOPHEN") |
    is_sas_missing(CMDOSE) | is_sas_missing(CMDOSU) |
    is_sas_missing(CMSTDTC) |
    !is_iso_date(CMSTDTC) |
    (!is_sas_missing(CMENDTC) & !is_iso_date(CMENDTC)) |
    (!is_sas_missing(CMENDTC) & CMENDTC < CMSTDTC) |
    (toupper(trimws(CMENRTPT))=="ONGOING" & !is_sas_missing(CMENDTC)) |
    (toupper(trimws(CMENRTPT))!="ONGOING" & is_sas_missing(CMENDTC))
  ) %>% nrow()
  # Note: SAS condition (c.CMENRTPT ne 'ONGOING' and missing(c.CMENDTC)) includes missing CMENRTPT as ne 'ONGOING'
  # Our logic matches: if CMENRTPT is NA/"" then toupper != "ONGOING" true, so missing CMENDTC triggers fail
  # But SAS missing is "" ; we treat NA as missing, so same
}

# RULE 31: SDTM-023 DSDECOD terminology
chk_ds03_n <- 0
if (nrow(ds)>0 && "DSDECOD" %in% names(ds)) {
  allowed <- c('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT','ADVERSE EVENT','SCREEN FAILURE')
  chk_ds03_n <- ds %>% filter(!toupper(trimws(DSDECOD)) %in% allowed) %>% nrow()
}

# RULE 32: SDTM-024 Domain sequence keys must be unique per subject
chk_seq01_n <- 0
{
  count_dups <- function(df, seqvar) {
    if (is.null(df) || nrow(df)==0 || !all(c("USUBJID", seqvar) %in% names(df))) return(0)
    tmp <- df %>% group_by(USUBJID, .data[[seqvar]]) %>% summarise(n=n(), .groups="drop") %>% filter(n>1)
    nrow(tmp)
  }
  chk_seq01_n <- count_dups(ae, "AESEQ") + count_dups(ex, "EXSEQ") + count_dups(lb, "LBSEQ") +
    count_dups(vs, "VSSEQ") + count_dups(cm, "CMSEQ") + count_dups(mh, "MHSEQ") + count_dups(eg, "EGSEQ")
}

# RULE 39: SDTM-025 Ongoing CM timing
chk_cm02_n <- 0
if (nrow(cm)>0 && nrow(dm)>0) {
  for (col in c("CMENRTPT","CMENDTC","CMENTPT","USUBJID")) if (!col %in% names(cm)) cm[[col]] <- NA
  tmp <- cm %>% inner_join(dm %>% select(USUBJID, RFENDTC), by="USUBJID")
  # SAS: where upcase(strip(c.CMENRTPT)) not in ('', 'ONGOING') or (ONGOING and (...)) or (ne ONGOING and not missing(CMENTPT))
  # Note: SAS missing is '' after strip/upcase
  tmp <- tmp %>% mutate(
    CMENRTPT_U = toupper(trimws(ifelse(is.na(CMENRTPT), "", CMENRTPT))),
    CMENTPT_S = trimws(ifelse(is.na(CMENTPT), "", CMENTPT))
  )
  chk_cm02_n <- tmp %>% filter(
    !CMENRTPT_U %in% c("", "ONGOING") |
    (CMENRTPT_U=="ONGOING" & (!is_sas_missing(CMENDTC) | is_sas_missing(CMENTPT) | !is_iso_date(CMENTPT) | CMENTPT != RFENDTC)) |
    (CMENRTPT_U!="ONGOING" & !is_sas_missing(CMENTPT))
  ) %>% nrow()
}

# Consolidate results
qc_report <- data.frame(
  CHECK_ID = c('ADSL-001','ADAE-001','ADVS-001','ADLB-001','ADVS-002','ADLB-002','SDTM-001',
               'SDTM-002','SDTM-003','SDTM-004','SDTM-005','ADSL-002','ADSL-003','ADSL-004',
               'ADTTE-001','ADTTE-002','ADTTE-003',
               'SDTM-006','SDTM-007','SDTM-008','SDTM-009',
               'SDTM-010','SDTM-011','SDTM-012','SDTM-013','SDTM-014',
               'SDTM-015','SDTM-016','SDTM-017','SDTM-018','SDTM-019',
               'SDTM-020','SDTM-021','ADAE-002','ADAE-003',
               'SDTM-022','SDTM-023','SDTM-024','SDTM-025'),
  DOMAIN = c('ADSL','ADAE','ADVS','ADLB','ADVS','ADLB','DM',
             'AE','EX','LB','VS','ADSL','ADSL','ADSL',
             'ADTTE','ADTTE','ADTTE',
             'EG','EG','EG','EG',
             'MH','MH','MH','MH','DM',
             'SV','SV','SV','DS','DS',
             'DM','DM','ADAE','ADAE',
             'CM','DS','SDTM','CM'),
  RULE = c('USUBJID must be perfectly unique',
           'AEENDT must be >= AESTDT',
           'Max 1 Baseline (ABLFL=Y) per Param',
           'Max 1 Baseline (ABLFL=Y) per Param',
           'Baseline date has exactly one eligible source record',
           'Baseline date has exactly one eligible source record',
           'USUBJID must be perfectly unique',
           'Every AE.USUBJID exists in DM',
           'Every EX.USUBJID exists in DM',
           'Every LB.USUBJID exists in DM',
           'Every VS.USUBJID exists in DM',
           'SAFFL=Y requires exposure in EX',
           'EOSDT must agree with DM.RFPENDTC',
           'AGE must equal completed years at informed consent',
           'Survival time (AVAL) must be >= 1',
           'AVAL and CNSR agree with event or subject follow-up date',
           'Event and follow-up dates must be chronologically valid',
           'Every EG.USUBJID exists in DM',
           'Required EG variables must exist',
           'EG date/value contract must be valid',
           'Raw EG variables must not leak to SDTM',
           'Every MH.USUBJID exists in DM',
           'Required MH variables must exist',
           'MH date/term contract must be valid',
           'Raw MH variables must not leak to SDTM',
           'DM reference dates complete and ordered',
           'Every SV.USUBJID exists in DM',
           'USUBJID and VISITNUM must be unique',
           'SV dates valid and inside reference period',
           'Exactly one final DS record per DM subject',
           'DS date matches DM participation end',
           'RACE uses supported CDISC terminology',
           'SITEID present and consistent with SUBJID',
           'AOCCFL valid and restricted to TRTEMFL=Y',
           'One AOCCFL=Y per subject and TEAE term',
           'CM subject, medication, dose and dates must be valid',
           'DSDECOD uses supported disposition terminology',
           'Domain sequence keys must be unique per subject',
           'Ongoing CM must be anchored to the DM reference end'),
  N_FAIL = c(chk_adsl01_n, chk_adae01_n, chk_advs01_n, chk_adlb01_n,
             chk_advs02_n, chk_adlb02_n, chk_dm01_n,
             chk_ae01_n, chk_ex01_n, chk_lb01_n, chk_vs01_n, chk_adsl02_n, chk_adsl03_n, chk_adsl04_n,
             chk_adtte01_n, chk_adtte02_n, chk_adtte03_n,
             chk_eg01_n, chk_eg02_n, chk_eg03_n, chk_eg04_n,
             chk_mh01_n, chk_mh02_n, chk_mh03_n, chk_mh04_n, chk_dm02_n,
             chk_sv01_n, chk_sv02_n, chk_sv03_n, chk_ds01_n, chk_ds02_n,
             chk_dm03_n, chk_dm04_n, chk_adae02_n, chk_adae03_n,
             chk_cm01_n, chk_ds03_n, chk_seq01_n, chk_cm02_n),
  stringsAsFactors = FALSE
)
qc_report$STATUS <- ifelse(qc_report$N_FAIL==0, "PASS", "FAIL")

# Write dataset
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names=FALSE, na="")

# Create text rendering (P019.lst)
# SAS proc print with title "Automated Quality Control (QC) Validation Report"
# Reproduce as plain text table
txt_file <- file.path(out, "P019.lst")
# Also need to write CSV of table body? Requirement says each table/listing additionally as CSV of table body
# We'll write P019.csv as same as qc_report

# Build text output
header <- c(
  "Automated Quality Control (QC) Validation Report",
  "",
  sprintf("%-10s %-10s %-70s %6s %-6s", "CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS"),
  paste(rep("-", 110), collapse="")
)
rows <- sprintf("%-10s %-10s %-70s %6d %-6s",
                qc_report$CHECK_ID, qc_report$DOMAIN, substr(qc_report$RULE,1,70),
                qc_report$N_FAIL, qc_report$STATUS)
footer <- c(
  paste(rep("-", 110), collapse=""),
  "",
  sprintf("Total checks: %d   Failed: %d", nrow(qc_report), sum(qc_report$STATUS=="FAIL"))
)
writeLines(c(header, rows, footer), txt_file)

# Also write P019.csv (table body)
write.csv(qc_report, file.path(out, "P019.csv"), row.names=FALSE, na="")

# Also ensure qc_report.txt exists as required (copy of lst)
writeLines(c(header, rows, footer), file.path(out, "qc_report.txt"))
