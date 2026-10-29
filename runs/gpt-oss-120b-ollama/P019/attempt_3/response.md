```r
# ------------------------------------------------------------
# QC Core Validation translated from SAS to R
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# Helper functions ------------------------------------------------
is_missing <- function(x) {
  is.na(x) | (is.character(x) & str_trim(x) == "")
}

# Environment variables ------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"

# Build paths to source data ------------------------------------------------
adam_path <- file.path(project_path, "data", "adam")
sdtm_path <- file.path(project_path, "data", "sdtm")

read_adam <- function(name) {
  read_sas(file.path(adam_path, paste0(tolower(name), ".sas7bdat")))
}
read_sdtm <- function(name) {
  read_sas(file.path(sdtm_path, paste0(tolower(name), ".sas7bdat")))
}

# Load datasets ------------------------------------------------
adsl  <- read_adam("adsl")
adae  <- read_adam("adae")
advs  <- read_adam("advs")
adlb  <- read_adam("adlb")
adtte <- read_adam("adtte")

dm    <- read_sdtm("dm")
ae    <- read_sdtm("ae")
ex    <- read_sdtm("ex")
lb    <- read_sdtm("lb")
vs    <- read_sdtm("vs")
eg    <- read_sdtm("eg")
mh    <- read_sdtm("mh")
sv    <- read_sdtm("sv")
ds    <- read_sdtm("ds")
cm    <- read_sdtm("cm")

# Trim character columns ------------------------------------------------
trim_all <- function(df) {
  df %>% mutate(across(where(is.character), ~ str_trim(.x, side = "both")))
}
adsl  <- trim_all(adsl)
adae  <- trim_all(adae)
advs  <- trim_all(advs)
adlb  <- trim_all(adlb)
adtte <- trim_all(adtte)
dm    <- trim_all(dm)
ae    <- trim_all(ae)
ex    <- trim_all(ex)
lb    <- trim_all(lb)
vs    <- trim_all(vs)
eg    <- trim_all(eg)
mh    <- trim_all(mh)
sv    <- trim_all(sv)
ds    <- trim_all(ds)
cm    <- trim_all(cm)

# ------------------------------------------------------------
# 1. DEFINE VALIDATION RULES
# ------------------------------------------------------------

# 1) ADSL - USUBJID must be perfectly unique
chk_adsl01 <- adsl %>%
  group_by(USUBJID) %>%
  filter(n() > 1) %>%
  distinct(USUBJID) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADSL-001", DOMAIN = "ADSL",
         RULE = "USUBJID must be perfectly unique")

# 2) ADAE - AEENDT >= AESTDT
chk_adae01 <- adae %>%
  filter(!is_missing(AEENDT) & !is_missing(AESTDT) & AEENDT < AESTDT) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADAE-001", DOMAIN = "ADAE",
         RULE = "AEENDT must be >= AESTDT")

# 3) ADVS - Max 1 Baseline per Param per Subject
chk_advs01 <- advs %>%
  filter(ABLFL == "Y") %>%
  group_by(USUBJID, PARAMCD) %>%
  filter(n() > 1) %>%
  distinct(USUBJID, PARAMCD) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADVS-001", DOMAIN = "ADVS",
         RULE = "Max 1 Baseline (ABLFL=Y) per Param")

# 4) ADLB - Max 1 Baseline per Param per Subject
chk_adlb01 <- adlb %>%
  filter(ABLFL == "Y") %>%
  group_by(USUBJID, PARAMCD) %>%
  filter(n() > 1) %>%
  distinct(USUBJID, PARAMCD) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADLB-001", DOMAIN = "ADLB",
         RULE = "Max 1 Baseline (ABLFL=Y) per Param")

# 5) SDTM DM - No duplicate USUBJID
chk_dm01 <- dm %>%
  group_by(USUBJID) %>%
  filter(n() > 1) %>%
  distinct(USUBJID) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-001", DOMAIN = "DM",
         RULE = "USUBJID must be perfectly unique")

# 6) SDTM AE - Referential Integrity
chk_ae01 <- anti_join(ae, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-002", DOMAIN = "AE",
         RULE = "Every AE.USUBJID exists in DM")

# 7) SDTM EX - Referential Integrity
chk_ex01 <- anti_join(ex, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-003", DOMAIN = "EX",
         RULE = "Every EX.USUBJID exists in DM")

# 8) SDTM LB - Referential Integrity
chk_lb01 <- anti_join(lb, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-004", DOMAIN = "LB",
         RULE = "Every LB.USUBJID exists in DM")

# 9) SDTM VS - Referential Integrity
chk_vs01 <- anti_join(vs, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-005", DOMAIN = "VS",
         RULE = "Every VS.USUBJID exists in DM")

# 10) ADSL - SAFFL=Y requires exposure in EX
chk_adsl02 <- anti_join(filter(adsl, SAFFL == "Y"), ex, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADSL-002", DOMAIN = "ADSL",
         RULE = "SAFFL=Y requires exposure in EX")

# 11) ADTTE - Survival time must be >=1
chk_adtte01 <- adtte %>%
  filter(is_missing(AVAL) | AVAL < 1) %>%
  summarise(N_FAIL = n())) %>%
  mutate(CHECK_ID = "ADTTE-001", DOMAIN = "ADTTE",
         RULE = "Survival time (AVAL) must be >= 1")

# 12) ADTTE - Event and censoring formula
chk_adtte02 <- adtte %>%
  filter(
    is_missing(TRTSDT) |
    is_missing(CNSR) |
    !(CNSR %in% c(0,1)) |
    (CNSR == 0 & (is_missing(EVENTDT) |
                  AVAL != (as.numeric(EVENTDT - TRTSDT) + 1))) |
    (CNSR == 1 & ( !is_missing(EVENTDT) |
                   is_missing(EOSDT) |
                   AVAL != (as.numeric(EOSDT - TRTSDT) + 1)))
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADTTE-002", DOMAIN = "ADTTE",
         RULE = "AVAL and CNSR agree with event or subject follow-up date")

# 13) ADTTE - Dates chronology
chk_adtte03 <- adtte %>%
  filter(
    is_missing(EOSDT) |
    EOSDT < TRTSDT |
    (!is_missing(EVENTDT) & (EVENTDT < TRTSDT | EVENTDT > EOSDT))
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADTTE-003", DOMAIN = "ADTTE",
         RULE = "Event and follow-up dates must be chronologically valid")

# 14) ADSL-003 EOSDT vs DM.RFPENDTC
chk_adsl03 <- adsl %>%
  left_join(dm, by = "USUBJID") %>%
  mutate(RFPENDTC_date = as.Date(substr(RFPENDTC, 1, 10), "%Y-%m-%d")) %>%
  filter(is_missing(EOSDT) |
         is.na(RFPENDTC_date) |
         EOSDT != RFPENDTC_date) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADSL-003", DOMAIN = "ADSL",
         RULE = "EOSDT must agree with DM.RFPENDTC")

# 15) ADSL-004 AGE calculation
chk_adsl04 <- adsl %>%
  filter(is_missing(BRTHDT) | is_missing(RFICDT) | is_missing(AGE) |
         AGE != floor(time_length(interval(BRTHDT, RFICDT), "years"))) ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADSL-004", DOMAIN = "ADSL",
         RULE = "AGE must equal completed years at informed consent")

# 16) ADVS-002 baseline date uniqueness
baseline_advs <- advs %>%
  filter(!is_missing(ASTDT) & !is_missing(TRTSDT) & !is_missing(AVAL) & ASTDT <= TRTSDT) %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(BASE_DT = max(ASTDT), .groups = "drop")
chk_advs02 <- advs %>%
  inner_join(baseline_advs, by = c("USUBJID","PARAMCD")) %>%
  filter(ASTDT == BASE_DT, !is_missing(AVAL)) %>%
  group_by(USUBJID, PARAMCD) %>%
  filter(n() > 1) %>%
  distinct(USUBJID, PARAMCD) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADVS-002", DOMAIN = "ADVS",
         RULE = "Baseline date has exactly one eligible source record")

# 17) ADLB-002 baseline date uniqueness
baseline_adlb <- adlb %>%
  filter(!is_missing(ASTDT) & !is_missing(TRTSDT) & !is_missing(AVAL) & ASTDT <= TRTSDT) %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(BASE_DT = max(ASTDT), .groups = "drop")
chk_adlb02 <- adlb %>%
  inner_join(baseline_adlb, by = c("USUBJID","PARAMCD")) %>%
  filter(ASTDT == BASE_DT, !is_missing(AVAL)) %>%
  group_by(USUBJID, PARAMCD) %>%
  filter(n() > 1) %>%
  distinct(USUBJID, PARAMCD) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADLB-002", DOMAIN = "ADLB",
         RULE = "Baseline date has exactly one eligible source record")

# 18) EG referential integrity
chk_eg01 <- anti_join(eg, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-006", DOMAIN = "EG",
         RULE = "Every EG.USUBJID exists in DM")

# 19) EG required variables existence
required_eg_vars <- c('STUDYID','DOMAIN','USUBJID','EGSEQ','EGTESTCD','EGTEST',
                     'EGORRES','EGORRESU','EGSTRESC','EGSTRESN','EGSTRESU','EGDTC')
present_eg_vars <- names(eg) %>% toupper()
chk_eg02 <- tibble(
  N_FAIL = length(required_eg_vars) - length(intersect(required_eg_vars, present_eg_vars))
) %>%
  mutate(CHECK_ID = "SDTM-007", DOMAIN = "EG",
         RULE = "Required EG variables must exist")

# 20) EG date/value contract
date_regex <- "^\\d{4}-\\d{2}-\\d{2}$"
chk_eg03 <- eg %>%
  filter(is_missing(EGDTC) |
         !str_detect(str_trim(EGDTC), date_regex) |
         is_missing(EGSTRESN) |
         is_missing(EGSTRESC)) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-008", DOMAIN = "EG",
         RULE = "EG date/value contract must be valid")

# 21) EG raw import variables leak
raw_eg_vars <- c('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')
chk_eg04 <- tibble(
  N_FAIL = sum(toupper(names(eg)) %in% raw_eg_vars)
) %>%
  mutate(CHECK_ID = "SDTM-009", DOMAIN = "EG",
         RULE = "Raw EG variables must not leak to SDTM")

# 22) MH referential integrity
chk_mh01 <- anti_join(mh, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-010", DOMAIN = "MH",
         RULE = "Every MH.USUBJID exists in DM")

# 23) MH required variables
required_mh_vars <- c('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')
present_mh_vars <- names(mh) %>% toupper()
chk_mh02 <- tibble(
  N_FAIL = length(required_mh_vars) - length(intersect(required_mh_vars, present_mh_vars))
) %>%
  mutate(CHECK_ID = "SDTM-011", DOMAIN = "MH",
         RULE = "Required MH variables must exist")

# 24) MH date/term contract
chk_mh03 <- mh %>%
  filter(is_missing(MHSTDTC) |
         !str_detect(str_trim(MHSTDTC), date_regex) |
         is_missing(MHTERM)) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-012", DOMAIN = "MH",
         RULE = "MH date/term contract must be valid")

# 25) MH raw import variables leak
raw_mh_vars <- c('SUBJECT','CONDITION','DIAGNOSIS_DATE')
chk_mh04 <- tibble(
  N_FAIL = sum(toupper(names(mh)) %in% raw_mh_vars)
) %>%
  mutate(CHECK_ID = "SDTM-013", DOMAIN = "MH",
         RULE = "Raw MH variables must not leak to SDTM")

# 26) DM reference period completeness & order
chk_dm02 <- dm %>%
  mutate(
    RFICDTC_ok   = !is_missing(RFICDTC)   & str_detect(str_trim(RFICDTC), date_regex),
    RFSTDTC_ok   = !is_missing(RFSTDTC)   & str_detect(str_trim(RFSTDTC), date_regex),
    RFENDTC_ok   = !is_missing(RFENDTC)   & str_detect(str_trim(RFENDTC), date_regex),
    RFPENDTC_ok  = !is_missing(RFPENDTC)  & str_detect(str_trim(RFPENDTC), date_regex),
    RFSTDTC_lt   = RFSTDTC_ok & RFICDTC_ok & (as.Date(RFSTDTC) < as.Date(RFICDTC)),
    RFENDTC_lt   = RFENDTC_ok & RFSTDTC_ok & (as.Date(RFENDTC) < as.Date(RFSTDTC)),
    RFPENDTC_eq  = RFPENDTC_ok & RFENDTC_ok & (as.Date(RFPENDTC) != as.Date(RFENDTC))
  ) %>%
  filter(
    is_missing(RFICDTC) | is_missing(RFSTDTC) | is_missing(RFENDTC) | is_missing(RFPENDTC) |
    !RFICDTC_ok | !RFSTDTC_ok | !RFENDTC_ok | !RFPENDTC_ok |
    RFSTDTC_lt | RFENDTC_lt | RFPENDTC_eq
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-014", DOMAIN = "DM",
         RULE = "DM reference dates complete and ordered")

# 27) SV referential integrity
chk_sv01 <- anti_join(sv, dm, by = "USUBJID") %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-015", DOMAIN = "SV",
         RULE = "Every SV.USUBJID exists in DM")

# 28) SV unique USUBJID+VISITNUM
chk_sv02 <- sv %>%
  group_by(USUBJID, VISITNUM) %>%
  filter(n() != 1) %>%
  distinct(USUBJID, VISITNUM) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-016", DOMAIN = "SV",
         RULE = "USUBJID and VISITNUM must be unique")

# 29) SV dates validity & inside DM reference period
sv_join <- sv %>%
  inner_join(dm, by = "USUBJID") %>%
  mutate(
    SVSTDTC_ok = !is_missing(SVSTDTC) & str_detect(str_trim(SVSTDTC), date_regex),
    SVENDTC_ok = !is_missing(SVENDTC) & str_detect(str_trim(SVENDTC), date_regex),
    SVEND_before_SVSTD = SVENDTC_ok & SVSTDTC_ok & (as.Date(SVENDTC) < as.Date(SVSTDTC)),
    SVSTD_before_DM = SVSTDTC_ok & (as.Date(SVSTDTC) < as.Date(RFSTDTC)),
    SVEND_after_DM  = SVENDTC_ok & (as.Date(SVENDTC) > as.Date(RFENDTC))
  )
chk_sv03 <- sv_join %>%
  filter(
    is_missing(VISITNUM) | is_missing(VISIT) |
    !SVSTDTC_ok | !SVENDTC_ok |
    SVEND_before_SVSTD |
    SVSTD_before_DM |
    SVEND_after_DM
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-017", DOMAIN = "SV",
         RULE = "SV dates valid and inside reference period")

# 30) DS exactly one final disposition per DM subject
ds_per_dm <- dm %>%
  left_join(ds, by = "USUBJID") %>%
  group_by(USUBJID) %>%
  summarise(N_DS = n_distinct(USUBJID.y), .groups = "drop") %>%
  filter(N_DS != 1)
orphan_ds <- ds %>%
  left_join(dm, by = "USUBJID") %>%
  filter(is.na(USUBJID.y)) %>%
  distinct(USUBJID) %>%
  mutate(N_DS = NA_integer_)
chk_ds01 <- bind_rows(ds_per_dm, orphan_ds) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-018", DOMAIN = "DS",
         RULE = "Exactly one final DS record per DM subject")

# 31) DS date matches DM participation end
ds_join <- ds %>%
  inner_join(dm, by = "USUBJID") %>%
  mutate(
    DSSTDTC_ok = !is_missing(DSSTDTC) & str_detect(str_trim(DSSTDTC), date_regex))
  )
chk_ds02 <- ds_join %>%
  filter(
    is_missing(DSDECOD) |
    !DSSTDTC_ok |
    as.Date(DSSTDTC) != as.Date(RFPENDTC)
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-019", DOMAIN = "DS",
         RULE = "DS date matches DM participation end")

# 32) DM RACE terminology
valid_race <- c('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')
chk_dm03 <- dm %>%
  filter(is_missing(RACE) | !(toupper(str_trim(RACE)) %in% valid_race)) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-020", DOMAIN = "DM",
         RULE = "RACE uses supported CDISC terminology")

# 33) DM SITEID consistency with SUBJID
chk_dm04 <- dm %>%
  mutate(SUBJID_part = str_split_fixed(str_trim(SUBJID), "-", 2)[,1]) %>%
  filter(is_missing(SITEID) | str_trim(SITEID) != SUBJID_part) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-021", DOMAIN = "DM",
         RULE = "SITEID present and consistent with SUBJID")

# 34) ADAE AOCCFL validity
chk_adae02 <- adae %>%
  filter(
    (!is_missing(AOCCFL) & AOCCFL != "Y") |
    (AOCCFL == "Y" & TRTEMFL != "Y")
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADAE-002", DOMAIN = "ADAE",
         RULE = "AOCCFL valid and restricted to TRTEMFL=Y")

# 35) ADAE one AOCCFL=Y per subject/term
adae_group <- adae %>%
  group_by(USUBJID, AEDECOD) %>%
  summarise(
    trtemfl_y = sum(TRTEMFL == "Y", na.rm = TRUE),
    aoccfl_y  = sum(AOCCFL == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(trtemfl_y > 0 & aoccfl_y != 1)
chk_adae03 <- adae_group %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "ADAE-003", DOMAIN = "ADAE",
         RULE = "One AOCCFL=Y per subject and TEAE term")

# 36) CM validation
chk_cm01 <- cm %>%
  anti_join(dm, by = "USUBJID") %>%
  filter(
    is_missing(CMTRT) | is_missing(CMDECOD) |
    (toupper(str_trim(CMTRT)) == "PARACETAMOL" & toupper(str_trim(CMDECOD)) != "ACETAMINOPHEN") |
    is_missing(CMDOSE) | is_missing(CMDOSU) |
    is_missing(CMSTDTC) | !str_detect(str_trim(CMSTDTC), date_regex) |
    (!is_missing(CMENDTC) & !str_detect(str_trim(CMENDTC), date_regex)) |
    (!is_missing(CMENDTC) & as.Date(CMENDTC) < as.Date(CMSTDTC)) |
    (CMENRTPT == "ONGOING" & !is_missing(CMENDTC)) |
    (CMENRTPT != "ONGOING" & is_missing(CMENDTC))
  ) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-022", DOMAIN = "CM",
         RULE = "CM subject, medication, dose and dates must be valid")

# 37) DSDECOD terminology
valid_dsdecod <- c('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT',
                   'ADVERSE EVENT','SCREEN FAILURE')
chk_ds03 <- ds %>%
  filter(!(toupper(str_trim(DSDECOD)) %in% valid_dsdecod)) %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-023", DOMAIN = "DS",
         RULE = "DSDECOD uses supported disposition terminology")

# 38) Sequence uniqueness across domains
seq_dup <- bind_rows(
  ae %>% mutate(SOURCE = "AE") %>% select(SOURCE, USUBJID, SEQ = AESEQ),
  ex %>% mutate(SOURCE = "EX") %>% select(SOURCE, USUBJID, SEQ = EXSEQ),
  lb %>% mutate(SOURCE = "LB") %>% select(SOURCE, USUBJID, SEQ = LBSEQ),
  vs %>% mutate(SOURCE = "VS") %>% select(SOURCE, USUBJID, SEQ = VSSEQ),
  cm %>% mutate(SOURCE = "CM") %>% select(SOURCE, USUBJID, SEQ = CMSEQ),
  mh %>% mutate(SOURCE = "MH") %>% select(SOURCE, USUBJID, SEQ = MHSEQ),
  eg %>% mutate(SOURCE = "EG") %>% select(SOURCE, USUBJID, SEQ = EGSEQ)
) %>%
  group_by(USUBJID, SEQ) %>%
  filter(n() > 1) %>%
  distinct()
chk_seq01 <- seq_dup %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-024", DOMAIN = "SDTM",
         RULE = "Domain sequence keys must be unique per subject")

# 39) CM ongoing timing anchor
cm_ongoing <- cm %>%
  inner_join(dm, by = "USUBJID") %>%
  mutate(CMENRTPT_up = toupper(str_trim(CMENRTPT))) %>%
  filter(
    !(CMENRTPT_up %in% c("", "ONGOING")) |
    (CMENRTPT_up == "ONGOING" & (
        !is_missing(CMENDTC) |
        is_missing(CMENTPT) |
        !str_detect(str_trim(CMENTPT), date_regex) |
        as.Date(CMENTPT) != as.Date(RFENDTC)
    )) |
    (CMENRTPT_up != "ONGOING" & !is_missing(CMENTPT))
  )
chk_cm02 <- cm_ongoing %>%
  summarise(N_FAIL = n()) %>%
  mutate(CHECK_ID = "SDTM-025", DOMAIN = "CM",
         RULE = "Ongoing CM must be anchored to the DM reference end")

# ------------------------------------------------------------
# 2. CONSOLIDATE RESULTS
# ------------------------------------------------------------
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
    N_FAIL = as.integer(N_FAIL),
    STATUS = ifelse(N_FAIL == 0, "PASS", "FAIL")
  ) %>%
  select(CHECK_ID, DOMAIN, RULE, N_FAIL, STATUS)

# ------------------------------------------------------------
# 3. WRITE OUTPUTS
# ------------------------------------------------------------
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Dataset output
write.csv(qc_report, file.path(out_dir, "qc_report.csv"), row.names = FALSE, na = "")

# Listing text rendering (SAS .lst equivalent)
write.table(qc_report, file.path(out_dir, "P019.txt"),
            sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Listing CSV (same content)
write.csv(qc_report, file.path(out_dir, "P019.csv"), row.names = FALSE, na = "")

# End of script
```