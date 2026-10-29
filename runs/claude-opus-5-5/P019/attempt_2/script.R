# P019 - Automated QC Validation Framework (translation of P019_qc_core.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})
invisible(Sys.setlocale("LC_COLLATE", "C"))

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
lib_dir <- list(
  sdtm = file.path(project_path, "data/sdtm"),
  adam = file.path(project_path, "data/adam")
)

# Read a SAS dataset (.sas7bdat preferred, .xpt fallback); upcase names,
# turn dates into numeric day counts, drop labels.
read_ds <- function(lib, name) {
  d <- lib_dir[[lib]]
  fl <- list.files(d, full.names = TRUE)
  b <- tolower(basename(fl))
  f <- fl[b == paste0(tolower(name), ".sas7bdat")]
  if (length(f) > 0) {
    df <- haven::read_sas(f[1])
  } else {
    f <- fl[b == paste0(tolower(name), ".xpt")]
    if (length(f) == 0) stop("Dataset not found: ", lib, ".", name)
    df <- haven::read_xpt(f[1])
  }
  df <- haven::zap_labels(haven::zap_formats(df))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  for (v in names(df)) {
    if (inherits(df[[v]], "Date")) {
      df[[v]] <- as.numeric(df[[v]])
    } else if (inherits(df[[v]], "POSIXt")) {
      df[[v]] <- as.numeric(df[[v]])
    }
  }
  df
}

# ---------------- SAS-semantic helpers ----------------
nc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  sub(" +$", "", x)
}
miss <- function(x) {
  if (is.character(x)) is.na(x) | trimws(x) == "" else is.na(x)
}
s_eq <- function(a, b) nc(a) == nc(b)
s_ne <- function(a, b) nc(a) != nc(b)
s_lt <- function(a, b) nc(a) < nc(b)
s_gt <- function(a, b) nc(a) > nc(b)
upst <- function(x) toupper(trimws(nc(x)))

lowna <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -Inf
  x
}
n_lt <- function(a, b) lowna(a) < lowna(b)
n_le <- function(a, b) lowna(a) <= lowna(b)
n_gt <- function(a, b) lowna(a) > lowna(b)
n_ne <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
}

iso_ok <- function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", trimws(nc(x)), perl = TRUE)

scan1 <- function(x) {
  x <- nc(x)
  vapply(strsplit(x, "-", fixed = TRUE), function(p) {
    p <- p[p != ""]
    if (length(p) == 0) "" else p[1]
  }, character(1))
}

compl_years <- function(b, e) {
  res <- rep(NA_real_, length(b))
  ok <- !is.na(b) & !is.na(e)
  bd <- as.Date(b[ok], origin = "1970-01-01")
  ed <- as.Date(e[ok], origin = "1970-01-01")
  yb <- as.integer(format(bd, "%Y")); ye <- as.integer(format(ed, "%Y"))
  mdb <- as.integer(format(bd, "%m")) * 100 + as.integer(format(bd, "%d"))
  mde <- as.integer(format(ed, "%m")) * 100 + as.integer(format(ed, "%d"))
  res[ok] <- (ye - yb) - as.integer(mde < mdb)
  res
}

n_dup_groups <- function(df, keys, cond = function(n) n > 1) {
  if (nrow(df) == 0) return(0)
  k <- do.call(paste, c(lapply(keys, function(v) {
    x <- df[[v]]
    if (is.character(x)) nc(x) else ifelse(is.na(x), "<NA>", format(x, digits = 15))
  }), sep = "\r"))
  tb <- table(k)
  sum(cond(as.integer(tb)))
}

mk <- function(id, dom, rule, n) {
  data.frame(CHECK_ID = id, DOMAIN = dom, RULE = rule, N_FAIL = as.numeric(n),
             stringsAsFactors = FALSE)
}
# PROC SQL inner join with no qualifying rows: count(*)=0 but the
# constant columns come back missing (SAS remerge behaviour on empty join)
mk_inner <- function(id, dom, rule, n) {
  if (n == 0) mk("", "", "", 0) else mk(id, dom, rule, n)
}

# ---------------- Read data ----------------
message("Reading data ...")
adsl  <- read_ds("adam", "adsl")
adae  <- read_ds("adam", "adae")
advs  <- read_ds("adam", "advs")
adlb  <- read_ds("adam", "adlb")
adtte <- read_ds("adam", "adtte")
dm <- read_ds("sdtm", "dm")
ae <- read_ds("sdtm", "ae")
ex <- read_ds("sdtm", "ex")
lb <- read_ds("sdtm", "lb")
vs <- read_ds("sdtm", "vs")
eg <- read_ds("sdtm", "eg")
mh <- read_ds("sdtm", "mh")
sv <- read_ds("sdtm", "sv")
ds <- read_ds("sdtm", "ds")
cm <- read_ds("sdtm", "cm")

dm_ids <- nc(dm$USUBJID)
not_in_dm <- function(x) sum(!(nc(x$USUBJID) %in% dm_ids))

join_dm <- function(x, dmcols, all.x = FALSE) {
  x$.K <- nc(x$USUBJID)
  d <- dm[, dmcols, drop = FALSE]
  names(d) <- paste0("D_", names(d))
  d$.K <- nc(dm$USUBJID)
  d$.DM <- 1L
  merge(x, d, by = ".K", all.x = all.x, sort = FALSE)
}

# ---------------- 1. Rules ----------------
message("Running checks ...")
chk <- list()

chk$adsl01 <- mk("ADSL-001", "ADSL", "USUBJID must be perfectly unique",
                 n_dup_groups(adsl, "USUBJID"))

chk$adae01 <- mk("ADAE-001", "ADAE", "AEENDT must be >= AESTDT",
                 sum(!is.na(adae$AEENDT) & !is.na(adae$AESTDT) & adae$AEENDT < adae$AESTDT))

chk$advs01 <- mk("ADVS-001", "ADVS", "Max 1 Baseline (ABLFL=Y) per Param",
                 n_dup_groups(advs[s_eq(advs$ABLFL, "Y"), ], c("USUBJID", "PARAMCD")))

chk$adlb01 <- mk("ADLB-001", "ADLB", "Max 1 Baseline (ABLFL=Y) per Param",
                 n_dup_groups(adlb[s_eq(adlb$ABLFL, "Y"), ], c("USUBJID", "PARAMCD")))

chk$dm01 <- mk("SDTM-001", "DM", "USUBJID must be perfectly unique",
               n_dup_groups(dm, "USUBJID"))

chk$ae01 <- mk("SDTM-002", "AE", "Every AE.USUBJID exists in DM", not_in_dm(ae))
chk$ex01 <- mk("SDTM-003", "EX", "Every EX.USUBJID exists in DM", not_in_dm(ex))
chk$lb01 <- mk("SDTM-004", "LB", "Every LB.USUBJID exists in DM", not_in_dm(lb))
chk$vs01 <- mk("SDTM-005", "VS", "Every VS.USUBJID exists in DM", not_in_dm(vs))

chk$adsl02 <- mk("ADSL-002", "ADSL", "SAFFL=Y requires exposure in EX",
                 sum(s_eq(adsl$SAFFL, "Y") & !(nc(adsl$USUBJID) %in% nc(ex$USUBJID))))

chk$adtte01 <- mk("ADTTE-001", "ADTTE", "Survival time (AVAL) must be >= 1",
                  sum(is.na(adtte$AVAL) | n_lt(adtte$AVAL, 1)))

with(adtte, {
  cn <- CNSR
  f <- is.na(TRTSDT) | is.na(cn) | !(cn %in% c(0, 1)) |
    (cn %in% 0 & (is.na(EVENTDT) | n_ne(AVAL, (EVENTDT - TRTSDT) + 1))) |
    (cn %in% 1 & (!is.na(EVENTDT) | is.na(EOSDT) | n_ne(AVAL, (EOSDT - TRTSDT) + 1)))
  chk$adtte02 <<- mk("ADTTE-002", "ADTTE",
                     "AVAL and CNSR agree with event or subject follow-up date", sum(f))
  f3 <- is.na(EOSDT) | n_lt(EOSDT, TRTSDT) |
    (!is.na(EVENTDT) & (n_lt(EVENTDT, TRTSDT) | n_gt(EVENTDT, EOSDT)))
  chk$adtte03 <<- mk("ADTTE-003", "ADTTE",
                     "Event and follow-up dates must be chronologically valid", sum(f3))
})

j <- join_dm(adsl[, c("USUBJID", "EOSDT")], c("USUBJID", "RFPENDTC"), all.x = TRUE)
rfp <- suppressWarnings(as.numeric(as.Date(substr(nc(j$D_RFPENDTC), 1, 10), format = "%Y-%m-%d")))
f <- is.na(j$.DM) | is.na(j$EOSDT) | n_ne(j$EOSDT, rfp)
chk$adsl03 <- mk("ADSL-003", "ADSL", "EOSDT must agree with DM.RFPENDTC", sum(f))

f <- is.na(adsl$BRTHDT) | is.na(adsl$RFICDT) | is.na(adsl$AGE) |
  n_ne(adsl$AGE, compl_years(adsl$BRTHDT, adsl$RFICDT))
chk$adsl04 <- mk("ADSL-004", "ADSL", "AGE must equal completed years at informed consent", sum(f))

base_chk <- function(d) {
  b <- d[n_le(d$ASTDT, d$TRTSDT) & !is.na(d$AVAL), ]
  if (nrow(b) == 0) return(0)
  b <- b %>% mutate(K1 = nc(USUBJID), K2 = nc(PARAMCD)) %>%
    group_by(K1, K2) %>%
    summarise(BASE_DT = if (all(is.na(ASTDT))) NA_real_ else max(ASTDT, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(K3 = lowna(BASE_DT)) %>% select(K1, K2, K3)
  a <- d[!is.na(d$AVAL), ] %>%
    mutate(K1 = nc(USUBJID), K2 = nc(PARAMCD), K3 = lowna(ASTDT)) %>% select(K1, K2, K3)
  jj <- inner_join(a, b, by = c("K1", "K2", "K3"), relationship = "many-to-many")
  if (nrow(jj) == 0) return(0)
  jj %>% count(K1, K2) %>% filter(n > 1) %>% nrow()
}
chk$advs02 <- mk("ADVS-002", "ADVS", "Baseline date has exactly one eligible source record", base_chk(advs))
chk$adlb02 <- mk("ADLB-002", "ADLB", "Baseline date has exactly one eligible source record", base_chk(adlb))

chk$eg01 <- mk("SDTM-006", "EG", "Every EG.USUBJID exists in DM", not_in_dm(eg))
egreq <- c("STUDYID", "DOMAIN", "USUBJID", "EGSEQ", "EGTESTCD", "EGTEST",
           "EGORRES", "EGORRESU", "EGSTRESC", "EGSTRESN", "EGSTRESU", "EGDTC")
chk$eg02 <- mk("SDTM-007", "EG", "Required EG variables must exist",
               12 - length(unique(intersect(toupper(names(eg)), egreq))))
f <- miss(eg$EGDTC) | !iso_ok(eg$EGDTC) | miss(eg$EGSTRESN) | miss(eg$EGSTRESC)
chk$eg03 <- mk("SDTM-008", "EG", "EG date/value contract must be valid", sum(f))
chk$eg04 <- mk("SDTM-009", "EG", "Raw EG variables must not leak to SDTM",
               sum(toupper(names(eg)) %in% c("SUBJECT", "TEST_NAME", "RESULT", "UNIT", "DATE")))

chk$mh01 <- mk("SDTM-010", "MH", "Every MH.USUBJID exists in DM", not_in_dm(mh))
mhreq <- c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHSTDTC")
chk$mh02 <- mk("SDTM-011", "MH", "Required MH variables must exist",
               6 - length(unique(intersect(toupper(names(mh)), mhreq))))
f <- miss(mh$MHSTDTC) | !iso_ok(mh$MHSTDTC) | miss(mh$MHTERM)
chk$mh03 <- mk("SDTM-012", "MH", "MH date/term contract must be valid", sum(f))
chk$mh04 <- mk("SDTM-013", "MH", "Raw MH variables must not leak to SDTM",
               sum(toupper(names(mh)) %in% c("SUBJECT", "CONDITION", "DIAGNOSIS_DATE")))

f <- with(dm, miss(RFICDTC) | miss(RFSTDTC) | miss(RFENDTC) | miss(RFPENDTC) |
            !iso_ok(RFICDTC) | !iso_ok(RFSTDTC) | !iso_ok(RFENDTC) | !iso_ok(RFPENDTC) |
            s_lt(RFSTDTC, RFICDTC) | s_lt(RFENDTC, RFSTDTC) | s_ne(RFPENDTC, RFENDTC))
chk$dm02 <- mk("SDTM-014", "DM", "DM reference dates complete and ordered", sum(f))

chk$sv01 <- mk("SDTM-015", "SV", "Every SV.USUBJID exists in DM", not_in_dm(sv))
chk$sv02 <- mk("SDTM-016", "SV", "USUBJID and VISITNUM must be unique",
               n_dup_groups(sv, c("USUBJID", "VISITNUM"), function(n) n != 1))
j <- join_dm(sv, c("RFSTDTC", "RFENDTC"))
f <- with(j, miss(VISITNUM) | miss(VISIT) | miss(SVSTDTC) | miss(SVENDTC) |
            !iso_ok(SVSTDTC) | !iso_ok(SVENDTC) | s_lt(SVENDTC, SVSTDTC) |
            s_lt(SVSTDTC, D_RFSTDTC) | s_gt(SVENDTC, D_RFENDTC))
chk$sv03 <- mk_inner("SDTM-017", "SV", "SV dates valid and inside reference period", sum(f))

dmk <- nc(dm$USUBJID); dsk <- nc(ds$USUBJID)
ndm <- table(dmk); nds <- table(dsk[dsk != ""])
ids <- names(ndm)
nds_v <- as.integer(nds[ids]); nds_v[is.na(nds_v)] <- 0
n_ds <- as.integer(ndm[ids]) * nds_v
n_ds[ids == ""] <- 0
part1 <- sum(n_ds != 1)
part2 <- length(unique(dsk[!(dsk %in% dmk)]))
chk$ds01 <- mk("SDTM-018", "DS", "Exactly one final DS record per DM subject", part1 + part2)

j <- join_dm(ds, c("RFPENDTC"))
f <- with(j, miss(DSDECOD) | miss(DSSTDTC) | !iso_ok(DSSTDTC) | s_ne(DSSTDTC, D_RFPENDTC))
chk$ds02 <- mk_inner("SDTM-019", "DS", "DS date matches DM participation end", sum(f))

f <- miss(dm$RACE) | !(upst(dm$RACE) %in% c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER", "UNKNOWN"))
chk$dm03 <- mk("SDTM-020", "DM", "RACE uses supported CDISC terminology", sum(f))
f <- miss(dm$SITEID) | (trimws(nc(dm$SITEID)) != scan1(trimws(nc(dm$SUBJID))))
chk$dm04 <- mk("SDTM-021", "DM", "SITEID present and consistent with SUBJID", sum(f))

f <- (!miss(adae$AOCCFL) & s_ne(adae$AOCCFL, "Y")) |
  (s_eq(adae$AOCCFL, "Y") & s_ne(adae$TRTEMFL, "Y"))
chk$adae02 <- mk("ADAE-002", "ADAE", "AOCCFL valid and restricted to TRTEMFL=Y", sum(f))
a3 <- adae %>% mutate(K1 = nc(USUBJID), K2 = nc(AEDECOD),
                      T = as.integer(s_eq(TRTEMFL, "Y")), O = as.integer(s_eq(AOCCFL, "Y"))) %>%
  group_by(K1, K2) %>% summarise(ST = sum(T), SO = sum(O), .groups = "drop") %>%
  filter(ST > 0 & SO != 1)
chk$adae03 <- mk("ADAE-003", "ADAE", "One AOCCFL=Y per subject and TEAE term", nrow(a3))

j <- join_dm(cm, c("USUBJID"), all.x = TRUE)
f <- with(j, is.na(.DM) | miss(CMTRT) | miss(CMDECOD) |
            (upst(CMTRT) == "PARACETAMOL" & upst(CMDECOD) != "ACETAMINOPHEN") |
            miss(CMDOSE) | miss(CMDOSU) | miss(CMSTDTC) | !iso_ok(CMSTDTC) |
            (!miss(CMENDTC) & !iso_ok(CMENDTC)) |
            (!miss(CMENDTC) & s_lt(CMENDTC, CMSTDTC)) |
            (s_eq(CMENRTPT, "ONGOING") & !miss(CMENDTC)) |
            (s_ne(CMENRTPT, "ONGOING") & miss(CMENDTC)))
chk$cm01 <- mk("SDTM-022", "CM", "CM subject, medication, dose and dates must be valid", sum(f))

f <- !(upst(ds$DSDECOD) %in% c("COMPLETED", "LOST TO FOLLOW-UP", "WITHDRAWAL BY SUBJECT",
                               "ADVERSE EVENT", "SCREEN FAILURE"))
chk$ds03 <- mk("SDTM-023", "DS", "DSDECOD uses supported disposition terminology", sum(f))

nseq <- n_dup_groups(ae, c("USUBJID", "AESEQ")) + n_dup_groups(ex, c("USUBJID", "EXSEQ")) +
  n_dup_groups(lb, c("USUBJID", "LBSEQ")) + n_dup_groups(vs, c("USUBJID", "VSSEQ")) +
  n_dup_groups(cm, c("USUBJID", "CMSEQ")) + n_dup_groups(mh, c("USUBJID", "MHSEQ")) +
  n_dup_groups(eg, c("USUBJID", "EGSEQ"))
chk$seq01 <- mk("SDTM-024", "SDTM", "Domain sequence keys must be unique per subject", nseq)

j <- join_dm(cm, c("RFENDTC"))
f <- with(j, !(upst(CMENRTPT) %in% c("", "ONGOING")) |
            (upst(CMENRTPT) == "ONGOING" & (!miss(CMENDTC) | miss(CMENTPT) |
                                             !iso_ok(CMENTPT) | s_ne(CMENTPT, D_RFENDTC))) |
            (upst(CMENRTPT) != "ONGOING" & !miss(CMENTPT)))
chk$cm02 <- mk_inner("SDTM-025", "CM", "Ongoing CM must be anchored to the DM reference end", sum(f))

# ---------------- 2. Consolidate ----------------
ord <- c("adsl01", "adae01", "advs01", "adlb01", "advs02", "adlb02", "dm01",
         "ae01", "ex01", "lb01", "vs01", "adsl02", "adsl03", "adsl04",
         "adtte01", "adtte02", "adtte03", "eg01", "eg02", "eg03", "eg04",
         "mh01", "mh02", "mh03", "mh04", "dm02", "sv01", "sv02", "sv03", "ds01", "ds02",
         "dm03", "dm04", "adae02", "adae03", "cm01", "ds03", "seq01", "cm02")
qc <- do.call(rbind, chk[ord])
qc$STATUS <- ifelse(!is.na(qc$N_FAIL) & qc$N_FAIL == 0, "PASS", "FAIL")
qc_report <- qc[, c("CHECK_ID", "DOMAIN", "RULE", "STATUS", "N_FAIL")]
rownames(qc_report) <- NULL
write.csv(qc_report, file.path(out, "qc_report.csv"), row.names = FALSE, na = "")

# ---------------- 3. Print report ----------------
render <- function(df, title) {
  cols <- names(df)
  cell <- lapply(cols, function(v) {
    x <- df[[v]]
    if (is.numeric(x)) format(ifelse(is.na(x), ".", as.character(x)), justify = "right")
    else as.character(x)
  })
  w <- mapply(function(v, x) max(nchar(v), nchar(x)), cols, cell)
  pad <- function(s, wd, right) if (right) formatC(s, width = wd) else formatC(s, width = -wd)
  isnum <- sapply(df, is.numeric)
  hdr <- paste(mapply(pad, cols, w, isnum), collapse = "  ")
  rows <- sapply(seq_len(nrow(df)), function(i)
    paste(mapply(function(x, wd, r) pad(x[i], wd, r), cell, w, isnum), collapse = "  "))
  c(title, "", hdr, "", rows, "")
}

print1 <- qc_report[, c("CHECK_ID", "DOMAIN", "RULE", "N_FAIL", "STATUS")]
txt <- render(print1, "Automated Quality Control (QC) Validation Report")

n_checks <- nrow(qc_report)
n_failed <- sum(qc_report$STATUS == "FAIL")
if (n_failed > 0) {
  message("ERROR: QC PIPELINE FAILED. ", n_failed, " VALIDATION RULES FAILED. EXECUTION ABORTED.")
} else {
  message("NOTE: QC VALIDATION COMPLETE. ALL ", n_checks, " CHECKS PASSED SUCCESSFULLY.")
  txt <- c(txt, render(qc_report, "Automated Quality Control (QC) Report"))
}
writeLines(txt, file.path(out, "P019.txt"))
write.csv(print1, file.path(out, "P019.csv"), row.names = FALSE, na = "")
message("Done.")
