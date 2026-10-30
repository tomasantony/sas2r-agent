out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
proj <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
suppressPackageStartupMessages({library(haven);library(dplyr)})
read_lib_ds <- function(lib, mem) {
  d <- file.path(proj, "data", lib)
  mem_up <- toupper(mem)
  files <- list.files(d, full.names = TRUE)
  if (length(files) == 0) stop(paste0("No files in ", d))
  bn <- tools::file_path_sans_ext(basename(files))
  idx <- which(toupper(bn) == mem_up)
  if (length(idx) == 0) stop(paste0("Dataset ", lib, ".", mem, " not found"))
  cand <- sort(files[idx])
  pref <- function(f) {
    e <- tolower(tools::file_ext(f))
    if (e == "sas7bdat") return(1L)
    if (e == "xpt") return(2L)
    if (e == "csv") return(3L)
    return(9L)
  }
  ord <- order(sapply(cand, pref), nchar(cand))
  f <- cand[ord[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") df <- haven::read_sas(f)
  else if (ext == "xpt") df <- haven::read_xpt(f)
  else if (ext == "csv") df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  else df <- haven::read_sas(f)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- toupper(names(df))
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) { x <- haven::zap_labels(x); x <- haven::zap_formats(x); df[[nm]] <- x }
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) df[[nm]] <- as.Date(x)
  }
  df
}
is_miss_c <- function(x) { cx <- as.character(x); is.na(x) | trimws(cx) == "" }
strip_c <- function(x) { cx <- as.character(x); cx[is.na(cx)] <- ""; trimws(cx) }
up_strip <- function(x) toupper(strip_c(x))
iso_ok <- function(x) { s <- strip_c(x); grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s) }
to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  suppressWarnings(as.Date(as.character(x)))
}
scan_first <- function(x) {
  sapply(as.character(x), function(s) {
    if (is.na(s) || trimws(s) == "") return("")
    p <- strsplit(trimws(s), "-", fixed = TRUE)[[1]]
    if (length(p) == 0) return("")
    p[1]
  }, USE.NAMES = FALSE)
}
calc_age_completed <- function(brth, rfic) {
  brth <- to_Date(brth); rfic <- to_Date(rfic)
  y1 <- as.integer(format(brth, "%Y")); m1 <- as.integer(format(brth, "%m")); d1 <- as.integer(format(brth, "%d"))
  y2 <- as.integer(format(rfic, "%Y")); m2 <- as.integer(format(rfic, "%m")); d2 <- as.integer(format(rfic, "%d"))
  age <- y2 - y1 - as.integer((m2 < m1) | (m2 == m1 & d2 < d1))
  age[is.na(brth) | is.na(rfic)] <- NA_integer_
  age
}
adsl <- read_lib_ds("adam", "adsl"); adae <- read_lib_ds("adam", "adae")
advs <- read_lib_ds("adam", "advs"); adlb <- read_lib_ds("adam", "adlb")
adtte <- read_lib_ds("adam", "adtte"); dm <- read_lib_ds("sdtm", "dm")
ae <- read_lib_ds("sdtm", "ae"); ex <- read_lib_ds("sdtm", "ex")
lb <- read_lib_ds("sdtm", "lb"); vs <- read_lib_ds("sdtm", "vs")
eg <- read_lib_ds("sdtm", "eg"); mh <- read_lib_ds("sdtm", "mh")
sv <- read_lib_ds("sdtm", "sv"); ds <- read_lib_ds("sdtm", "ds"); cm <- read_lib_ds("sdtm", "cm")
mk <- function(id, dom, rule, n) data.frame(CHECK_ID = id, DOMAIN = dom, RULE = rule, N_FAIL = as.integer(n), stringsAsFactors = FALSE)
n1 <- 0
if ("USUBJID" %in% names(adsl)) { g <- adsl %>% group_by(USUBJID) %>% summarise(n = n(), .groups = "drop"); n1 <- sum(g$n > 1, na.rm = TRUE) }
chk_adsl01 <- mk("ADSL-001", "ADSL", "USUBJID must be perfectly unique", n1)
n2 <- 0
if (all(c("AEENDT", "AESTDT") %in% names(adae))) { aendt <- to_Date(adae$AEENDT); ast <- to_Date(adae$AESTDT); n2 <- sum(!is.na(aendt) & !is.na(ast) & aendt < ast, na.rm = TRUE) }
chk_adae01 <- mk("ADAE-001", "ADAE", "AEENDT must be >= AESTDT", n2)
n3 <- 0
if (all(c("USUBJID", "PARAMCD", "ABLFL") %in% names(advs))) { sub <- advs %>% filter(!is.na(ABLFL) & ABLFL == "Y"); if (nrow(sub) > 0) { g <- sub %>% group_by(USUBJID, PARAMCD) %>% summarise(n = n(), .groups = "drop"); n3 <- sum(g$n > 1, na.rm = TRUE) } }
chk_advs01 <- mk("ADVS-001", "ADVS", "Max 1 Baseline (ABLFL=Y) per Param", n3)
n4 <- 0
if (all(c("USUBJID", "PARAMCD", "ABLFL") %in% names(adlb))) { sub <- adlb %>% filter(!is.na(ABLFL) & ABLFL == "Y"); if (nrow(sub) > 0) { g <- sub %>% group_by(USUBJID, PARAMCD) %>% summarise(n = n(), .groups = "drop"); n4 <- sum(g$n > 1, na.rm = TRUE) } }
chk_adlb01 <- mk("ADLB-001", "ADLB", "Max 1 Baseline (ABLFL=Y) per Param", n4)
n5 <- 0
if ("USUBJID" %in% names(dm)) { g <- dm %>% group_by(USUBJID) %>% summarise(n = n(), .groups = "drop"); n5 <- sum(g$n > 1, na.rm = TRUE) }
chk_dm01 <- mk("SDTM-001", "DM", "USUBJID must be perfectly unique", n5)
ref_fail <- function(child, parent_ids, key = "USUBJID") {
  if (!(key %in% names(child))) return(0L)
  cv <- as.character(child[[key]])
  has_na_parent <- any(is.na(parent_ids))
  bad <- sapply(seq_along(cv), function(i) { v <- cv[i]; if (is.na(v)) return(!has_na_parent); !(v %in% parent_ids[!is.na(parent_ids)]) })
  sum(bad, na.rm = TRUE)
}
dm_ids <- as.character(dm$USUBJID)
chk_ae01 <- mk("SDTM-002", "AE", "Every AE.USUBJID exists in DM", ref_fail(ae, dm_ids))
chk_ex01 <- mk("SDTM-003", "EX", "Every EX.USUBJID exists in DM", ref_fail(ex, dm_ids))
chk_lb01 <- mk("SDTM-004", "LB", "Every LB.USUBJID exists in DM", ref_fail(lb, dm_ids))
chk_vs01 <- mk("SDTM-005", "VS", "Every VS.USUBJID exists in DM", ref_fail(vs, dm_ids))
n10 <- 0
if (all(c("USUBJID", "SAFFL") %in% names(adsl))) {
  ex_ids <- unique(as.character(ex$USUBJID)); sub <- adsl %>% filter(!is.na(SAFFL) & SAFFL == "Y")
  if (nrow(sub) > 0) n10 <- sum(sapply(as.character(sub$USUBJID), function(v) { if (is.na(v)) return(!any(is.na(ex_ids))); !(v %in% ex_ids[!is.na(ex_ids)]) }))
}
chk_adsl02 <- mk("ADSL-002", "ADSL", "SAFFL=Y requires exposure in EX", n10)
n11 <- sum(is.na(adtte$AVAL) | (!is.na(adtte$AVAL) & adtte$AVAL < 1), na.rm = TRUE)
chk_adtte01 <- mk("ADTTE-001", "ADTTE", "Survival time (AVAL) must be >= 1", n11)
TRTSDT <- to_Date(adtte$TRTSDT); CNSR <- adtte$CNSR; EVENTDT <- to_Date(adtte$EVENTDT); EOSDT <- to_Date(adtte$EOSDT); AVAL <- adtte$AVAL
miss_TRT <- is.na(TRTSDT); miss_CNSR <- is.na(CNSR); bad_CNSR <- !is.na(CNSR) & !(CNSR %in% c(0, 1))
dE <- suppressWarnings(as.numeric(EVENTDT - TRTSDT)) + 1; dO <- suppressWarnings(as.numeric(EOSDT - TRTSDT)) + 1
cond0 <- !is.na(CNSR) & CNSR == 0 & (is.na(EVENTDT) | is.na(AVAL) | is.na(dE) | AVAL != dE); cond0[is.na(cond0)] <- FALSE
cond1 <- !is.na(CNSR) & CNSR == 1 & (!is.na(EVENTDT) | is.na(EOSDT) | is.na(AVAL) | is.na(dO) | AVAL != dO); cond1[is.na(cond1)] <- FALSE
chk_adtte02 <- mk("ADTTE-002", "ADTTE", "AVAL and CNSR agree with event or subject follow-up date", sum(miss_TRT | miss_CNSR | bad_CNSR | cond0 | cond1, na.rm = TRUE))
TRT2 <- to_Date(adtte$TRTSDT); EV2 <- to_Date(adtte$EVENTDT); EO2 <- to_Date(adtte$EOSDT)
miss_EOS <- is.na(EO2); cond_lt <- !is.na(EO2) & !is.na(TRT2) & EO2 < TRT2; cond_ev <- !is.na(EV2) & ((!is.na(TRT2) & EV2 < TRT2) | (!is.na(EO2) & EV2 > EO2)); cond_ev[is.na(cond_ev)] <- FALSE; cond_lt[is.na(cond_lt)] <- FALSE
chk_adtte03 <- mk("ADTTE-003", "ADTTE", "Event and follow-up dates must be chronologically valid", sum(miss_EOS | cond_lt | cond_ev, na.rm = TRUE))
adsl_eos <- adsl %>% select(USUBJID, EOSDT); dm_sub <- dm %>% select(USUBJID, RFPENDTC)
m03 <- merge(adsl_eos, dm_sub, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot(all(c("USUBJID", "EOSDT", "RFPENDTC") %in% names(m03)))
has_na_dm <- any(is.na(as.character(dm$USUBJID)))
dm_missing <- sapply(as.character(m03$USUBJID), function(v) { if (is.na(v)) return(!has_na_dm); !(v %in% as.character(dm$USUBJID[!is.na(dm$USUBJID)])) })
eos_d <- to_Date(m03$EOSDT); parsed <- suppressWarnings(as.Date(substr(as.character(m03$RFPENDTC), 1, 10), format = "%Y-%m-%d"))
fail03 <- dm_missing | is.na(eos_d) | is.na(parsed) | eos_d != parsed; fail03[is.na(fail03)] <- TRUE
chk_adsl03 <- mk("ADSL-003", "ADSL", "EOSDT must agree with DM.RFPENDTC", sum(fail03, na.rm = TRUE))
br <- to_Date(adsl$BRTHDT); rfic <- to_Date(adsl$RFICDT); age <- adsl$AGE; exp_age <- calc_age_completed(br, rfic)
chk_adsl04 <- mk("ADSL-004", "ADSL", "AGE must equal completed years at informed consent", sum(is.na(br) | is.na(rfic) | is.na(age) | is.na(exp_age) | age != exp_age, na.rm = TRUE))
baseline_dup <- function(d) {
  req <- c("USUBJID", "PARAMCD", "ASTDT", "TRTSDT", "AVAL")
  if (!all(req %in% names(d))) return(0L)
  dd <- d; dd$ASTDT_D <- to_Date(dd$ASTDT); dd$TRTSDT_D <- to_Date(dd$TRTSDT)
  elig <- dd %>% filter(!is.na(AVAL) & !is.na(ASTDT_D) & !is.na(TRTSDT_D) & ASTDT_D <= TRTSDT_D)
  if (nrow(elig) == 0) return(0L)
  base <- elig %>% group_by(USUBJID, PARAMCD) %>% summarise(BASE_DT = max(ASTDT_D), .groups = "drop")
  cand <- dd %>% filter(!is.na(AVAL) & !is.na(ASTDT_D)) %>% inner_join(base, by = c("USUBJID", "PARAMCD")) %>% filter(ASTDT_D == BASE_DT)
  if (nrow(cand) == 0) return(0L)
  g <- cand %>% group_by(USUBJID, PARAMCD) %>% summarise(n = n(), .groups = "drop")
  sum(g$n > 1, na.rm = TRUE)
}
chk_advs02 <- mk("ADVS-002", "ADVS", "Baseline date has exactly one eligible source record", baseline_dup(advs))
chk_adlb02 <- mk("ADLB-002", "ADLB", "Baseline date has exactly one eligible source record", baseline_dup(adlb))
chk_eg01 <- mk("SDTM-006", "EG", "Every EG.USUBJID exists in DM", ref_fail(eg, dm_ids))
req_eg <- c("STUDYID", "DOMAIN", "USUBJID", "EGSEQ", "EGTESTCD", "EGTEST", "EGORRES", "EGORRESU", "EGSTRESC", "EGSTRESN", "EGSTRESU", "EGDTC")
chk_eg02 <- mk("SDTM-007", "EG", "Required EG variables must exist", 12 - sum(req_eg %in% unique(toupper(names(eg)))))
if (all(c("EGDTC", "EGSTRESN", "EGSTRESC") %in% names(eg))) n_eg03 <- sum(is_miss_c(eg$EGDTC) | !iso_ok(eg$EGDTC) | is.na(eg$EGSTRESN) | is_miss_c(eg$EGSTRESC), na.rm = TRUE) else n_eg03 <- nrow(eg)
chk_eg03 <- mk("SDTM-008", "EG", "EG date/value contract must be valid", n_eg03)
chk_eg04 <- mk("SDTM-009", "EG", "Raw EG variables must not leak to SDTM", sum(unique(toupper(names(eg))) %in% c("SUBJECT", "TEST_NAME", "RESULT", "UNIT", "DATE")))
chk_mh01 <- mk("SDTM-010", "MH", "Every MH.USUBJID exists in DM", ref_fail(mh, dm_ids))
req_mh <- c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHSTDTC")
chk_mh02 <- mk("SDTM-011", "MH", "Required MH variables must exist", 6 - sum(req_mh %in% unique(toupper(names(mh)))))
chk_mh03 <- mk("SDTM-012", "MH", "MH date/term contract must be valid", sum(is_miss_c(mh$MHSTDTC) | !iso_ok(mh$MHSTDTC) | is_miss_c(mh$MHTERM), na.rm = TRUE))
chk_mh04 <- mk("SDTM-013", "MH", "Raw MH variables must not leak to SDTM", sum(unique(toupper(names(mh))) %in% c("SUBJECT", "CONDITION", "DIAGNOSIS_DATE")))
chk_dm02 <- mk("SDTM-014", "DM", "DM reference dates complete and ordered", sum(is_miss_c(dm$RFICDTC) | is_miss_c(dm$RFSTDTC) | is_miss_c(dm$RFENDTC) | is_miss_c(dm$RFPENDTC) | !iso_ok(dm$RFICDTC) | !iso_ok(dm$RFSTDTC) | !iso_ok(dm$RFENDTC) | !iso_ok(dm$RFPENDTC) | strip_c(dm$RFSTDTC) < strip_c(dm$RFICDTC) | strip_c(dm$RFENDTC) < strip_c(dm$RFSTDTC) | strip_c(dm$RFPENDTC) != strip_c(dm$RFENDTC), na.rm = TRUE))
chk_sv01 <- mk("SDTM-015", "SV", "Every SV.USUBJID exists in DM", ref_fail(sv, dm_ids))
g_sv <- sv %>% group_by(USUBJID, VISITNUM) %>% summarise(n = n(), .groups = "drop")
chk_sv02 <- mk("SDTM-016", "SV", "USUBJID and VISITNUM must be unique", sum(g_sv$n != 1, na.rm = TRUE))
dm_ref_sv <- dm %>% select(USUBJID, RFSTDTC, RFENDTC)
m_sv <- merge(sv, dm_ref_sv, by = "USUBJID", all = FALSE, sort = FALSE)
stopifnot(all(c("VISITNUM", "VISIT", "SVSTDTC", "SVENDTC") %in% names(m_sv)))
fail_sv03 <- is_miss_c(m_sv$VISITNUM) | is_miss_c(m_sv$VISIT) | is_miss_c(m_sv$SVSTDTC) | is_miss_c(m_sv$SVENDTC) | !iso_ok(m_sv$SVSTDTC) | !iso_ok(m_sv$SVENDTC) | strip_c(m_sv$SVENDTC) < strip_c(m_sv$SVSTDTC) | strip_c(m_sv$SVSTDTC) < strip_c(m_sv$RFSTDTC) | strip_c(m_sv$SVENDTC) > strip_c(m_sv$RFENDTC)
chk_sv03 <- mk("SDTM-017", "SV", "SV dates valid and inside reference period", sum(fail_sv03, na.rm = TRUE))
dm_ids_u <- unique(as.character(dm$USUBJID))
ds_by <- ds %>% group_by(USUBJID) %>% summarise(N_DS = n(), .groups = "drop")
dm_ds <- data.frame(USUBJID = dm_ids_u, stringsAsFactors = FALSE) %>% left_join(ds_by, by = "USUBJID") %>% mutate(N_DS = ifelse(is.na(N_DS), 0, N_DS))
fail1 <- sum(dm_ds$N_DS != 1, na.rm = TRUE)
orph <- ds %>% anti_join(data.frame(USUBJID = dm_ids_u, stringsAsFactors = FALSE), by = "USUBJID") %>% distinct(USUBJID) %>% nrow()
chk_ds01 <- mk("SDTM-018", "DS", "Exactly one final DS record per DM subject", fail1 + orph)
dm_ref_ds <- dm %>% select(USUBJID, RFPENDTC)
m_ds <- merge(ds, dm_ref_ds, by = "USUBJID", all = FALSE, sort = FALSE)
stopifnot(all(c("DSDECOD", "DSSTDTC", "RFPENDTC") %in% names(m_ds)))
fail_ds02 <- is_miss_c(m_ds$DSDECOD) | is_miss_c(m_ds$DSSTDTC) | !iso_ok(m_ds$DSSTDTC) | strip_c(m_ds$DSSTDTC) != strip_c(m_ds$RFPENDTC)
chk_ds02 <- mk("SDTM-019", "DS", "DS date matches DM participation end", sum(fail_ds02, na.rm = TRUE))
chk_dm03 <- mk("SDTM-020", "DM", "RACE uses supported CDISC terminology", sum(is_miss_c(dm$RACE) | !(up_strip(dm$RACE) %in% c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER", "UNKNOWN")), na.rm = TRUE))
site <- strip_c(dm$SITEID); first_tok <- scan_first(as.character(dm$SUBJID))
chk_dm04 <- mk("SDTM-021", "DM", "SITEID present and consistent with SUBJID", sum(is_miss_c(dm$SITEID) | site != first_tok, na.rm = TRUE))
aocc <- as.character(adae$AOCCFL); trt <- as.character(adae$TRTEMFL); aocc_miss <- is_miss_c(adae$AOCCFL)
c1a <- !aocc_miss & !is.na(aocc) & aocc != "Y"; c1a[is.na(c1a)] <- FALSE
c2a <- !is.na(aocc) & aocc == "Y" & (is.na(trt) | trt != "Y"); c2a[is.na(c2a)] <- FALSE
chk_adae02 <- mk("ADAE-002", "ADAE", "AOCCFL valid and restricted to TRTEMFL=Y", sum(c1a | c2a, na.rm = TRUE))
g_ae <- adae %>% group_by(USUBJID, AEDECOD) %>% summarise(n_te = sum(!is.na(TRTEMFL) & TRTEMFL == "Y", na.rm = TRUE), n_occ = sum(!is.na(AOCCFL) & AOCCFL == "Y", na.rm = TRUE), .groups = "drop")
chk_adae03 <- mk("ADAE-003", "ADAE", "One AOCCFL=Y per subject and TEAE term", sum(g_ae$n_te > 0 & g_ae$n_occ != 1, na.rm = TRUE))
in_dm_cm <- sapply(as.character(cm$USUBJID), function(v) { if (is.na(v)) return(any(is.na(dm_ids))); v %in% dm_ids[!is.na(dm_ids)] })
c_trt <- as.character(cm$CMTRT); c_dec <- as.character(cm$CMDECOD); c_enr <- as.character(cm$CMENRTPT)
cond_par <- up_strip(c_trt) == "PARACETAMOL" & up_strip(c_dec) != "ACETAMINOPHEN"; cond_par[is.na(cond_par)] <- FALSE
std_miss <- is_miss_c(cm$CMSTDTC); std_bad <- !std_miss & !iso_ok(cm$CMSTDTC)
end_miss <- is_miss_c(cm$CMENDTC); end_bad <- !end_miss & !iso_ok(cm$CMENDTC)
end_lt <- !end_miss & !std_miss & strip_c(cm$CMENDTC) < strip_c(cm$CMSTDTC)
is_ong <- !is.na(c_enr) & c_enr == "ONGOING"; is_ong[is.na(is_ong)] <- FALSE
not_ong <- is.na(c_enr) | c_enr != "ONGOING"
fail_cm01 <- !in_dm_cm | is_miss_c(cm$CMTRT) | is_miss_c(cm$CMDECOD) | cond_par | is.na(cm$CMDOSE) | is_miss_c(cm$CMDOSU) | std_miss | std_bad | end_bad | end_lt | (is_ong & !end_miss) | (not_ong & end_miss)
fail_cm01[is.na(fail_cm01)] <- TRUE
chk_cm01 <- mk("SDTM-022", "CM", "CM subject, medication, dose and dates must be valid", sum(fail_cm01, na.rm = TRUE))
chk_ds03 <- mk("SDTM-023", "DS", "DSDECOD uses supported disposition terminology", sum(!(up_strip(ds$DSDECOD) %in% c("COMPLETED", "LOST TO FOLLOW-UP", "WITHDRAWAL BY SUBJECT", "ADVERSE EVENT", "SCREEN FAILURE")), na.rm = TRUE))
fseq <- function(df, seq) { if (!all(c("USUBJID", seq) %in% names(df))) return(0L); sub <- df %>% select(USUBJID, SEQV = all_of(seq)); g <- sub %>% group_by(USUBJID, SEQV) %>% summarise(n = n(), .groups = "drop"); sum(g$n > 1, na.rm = TRUE) }
chk_seq01 <- mk("SDTM-024", "SDTM", "Domain sequence keys must be unique per subject", fseq(ae, "AESEQ") + fseq(ex, "EXSEQ") + fseq(lb, "LBSEQ") + fseq(vs, "VSSEQ") + fseq(cm, "CMSEQ") + fseq(mh, "MHSEQ") + fseq(eg, "EGSEQ"))
dm_ref_cm <- dm %>% select(USUBJID, RFENDTC)
m_cm <- merge(cm, dm_ref_cm, by = "USUBJID", all = FALSE, sort = FALSE)
stopifnot(all(c("CMENRTPT", "CMENDTC", "CMENTPT", "RFENDTC") %in% names(m_cm)))
enr_up <- up_strip(m_cm$CMENRTPT)
cc1 <- !(enr_up %in% c("", "ONGOING"))
cc2 <- enr_up == "ONGOING" & (!is_miss_c(m_cm$CMENDTC) | is_miss_c(m_cm$CMENTPT) | !iso_ok(m_cm$CMENTPT) | strip_c(m_cm$CMENTPT) != strip_c(m_cm$RFENDTC))
cc3 <- enr_up != "ONGOING" & !is_miss_c(m_cm$CMENTPT)
fail_cm02 <- cc1 | cc2 | cc3; fail_cm02[is.na(fail_cm02)] <- TRUE
chk_cm02 <- mk("SDTM-025", "CM", "Ongoing CM must be anchored to the DM reference end", sum(fail_cm02, na.rm = TRUE))
qc_report <- bind_rows(chk_adsl01, chk_adae01, chk_advs01, chk_adlb01, chk_advs02, chk_adlb02, chk_dm01, chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04, chk_adtte01, chk_adtte02, chk_adtte03, chk_eg01, chk_eg02, chk_eg03, chk_eg04, chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02, chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02, chk_dm03, chk_dm04, chk_adae02, chk_adae03, chk_cm01, chk_ds03, chk_seq01, chk_cm02)
qc_report$STATUS <- ifelse(qc_report$N_FAIL == 0, "PASS", "FAIL")
stopifnot(nrow(qc_report) == 39, all(c("CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS") %in% names(qc_report)))
stopifnot(qc_report$CHECK_ID[29] == "SDTM-017", qc_report$CHECK_ID[31] == "SDTM-019", qc_report$CHECK_ID[39] == "SDTM-025")
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names = FALSE, na = "")
hdr <- "Automated Quality Control (QC) Validation Report"
lines <- c(hdr, paste(rep("-", nchar(hdr)), collapse = ""), "", sprintf("%-10s %-6s %-55s %6s %-6s", "CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS"), sprintf("%-10s %-6s %-55s %6s %-6s", "----------", "------", paste(rep("-", 55), collapse = ""), "------", "------"))
for (i in seq_len(nrow(qc_report))) lines <- c(lines, sprintf("%-10s %-6s %-55s %6d %-6s", qc_report$CHECK_ID[i], qc_report$DOMAIN[i], substr(qc_report$RULE[i], 1, 55), qc_report$N_FAIL[i], qc_report$STATUS[i]))
lines <- c(lines, "", paste0("Number of checks: ", nrow(qc_report)), paste0("Number failed: ", sum(qc_report$STATUS == "FAIL")))
writeLines(lines, file.path(out, "P019.txt"))
write.csv(qc_report, file.path(out, "P019.csv"), row.names = FALSE, na = "")
cat("QC done:", nrow(qc_report), "checks,", sum(qc_report$STATUS == "FAIL"), "failed\n")
