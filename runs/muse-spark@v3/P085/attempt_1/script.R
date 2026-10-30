# P085_adlb.sas -> R translation
# ADLB BDS creation + PROC CONTENTS / PRINT rendering
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------- helpers ----------
parse_date9_vec <- function(x) {
  # ONE strict format: DDMMMYYYY (SAS date9.), "." = missing
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v) || v == "" || v == ".") next
    # strict DDMMMYYYY
    m <- regmatches(v, regexec("^([0-9]{1,2})([A-Za-z]{3})([0-9]{4})$", v))[[1]]
    if (length(m) != 4) next
    d <- suppressWarnings(as.integer(m[2]))
    monraw <- toupper(m[3])
    y <- suppressWarnings(as.integer(m[4]))
    mon <- match(monraw, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
    if (is.na(d) || is.na(mon) || is.na(y)) next
    res[i] <- as.Date(sprintf("%04d-%02d-%02d", y, mon, d))
  }
  res
}
fmt_date9 <- function(x) {
  # SAS DATE9. display e.g. 15JAN2024, missing -> "."
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    toupper(format(v, "%d%b%Y"))
  }, USE.NAMES = FALSE)
}

# ---------- STEP 1: mock SDTM LB ----------
lb_txt <- paste(
"STUDY01|STUDY01-1001|1|ALT|Alanine Aminotransferase|CHEMISTRY|24|U/L|24|U/L|7|56|Baseline|1|2024-01-15|NORMAL",
"STUDY01|STUDY01-1001|2|AST|Aspartate Aminotransferase|CHEMISTRY|26|U/L|26|U/L|10|40|Baseline|1|2024-01-15|NORMAL",
"STUDY01|STUDY01-1001|3|CREAT|Creatinine|CHEMISTRY|0.9|mg/dL|0.9|mg/dL|0.7|1.3|Baseline|1|2024-01-15|NORMAL",
"STUDY01|STUDY01-1001|4|ALT|Alanine Aminotransferase|CHEMISTRY|28|U/L|28|U/L|7|56|Week 4|2|2024-02-12|NORMAL",
"STUDY01|STUDY01-1001|5|AST|Aspartate Aminotransferase|CHEMISTRY|30|U/L|30|U/L|10|40|Week 4|2|2024-02-12|NORMAL",
"STUDY01|STUDY01-1001|6|ALT|Alanine Aminotransferase|CHEMISTRY|32|U/L|32|U/L|7|56|Week 8|3|2024-03-11|NORMAL",
"STUDY01|STUDY01-1002|1|ALT|Alanine Aminotransferase|CHEMISTRY|18|U/L|18|U/L|7|56|Baseline|1|2024-01-18|NORMAL",
"STUDY01|STUDY01-1002|2|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Week 4|2|2024-02-15|NORMAL",
"STUDY01|STUDY01-1003|1|ALT|Alanine Aminotransferase|CHEMISTRY|35|U/L|35|U/L|7|56|Baseline|1|2024-01-20|NORMAL",
"STUDY01|STUDY01-1003|2|ALT|Alanine Aminotransferase|CHEMISTRY|95|U/L|95|U/L|7|56|Week 4|2|2024-02-17|HIGH",
"STUDY01|STUDY01-1003|3|ALT|Alanine Aminotransferase|CHEMISTRY|180|U/L|180|U/L|7|56|Week 8|3|2024-03-15|HIGH",
"STUDY01|STUDY01-1004|1|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Baseline|1|2024-01-25|NORMAL",
"STUDY01|STUDY01-1005|1|ALT|Alanine Aminotransferase|CHEMISTRY|19|U/L|19|U/L|7|56|Baseline|1|2024-01-28|NORMAL",
"STUDY01|STUDY01-1006|1|ALT|Alanine Aminotransferase|CHEMISTRY|30|U/L|30|U/L|7|56|Baseline|1|2024-01-30|NORMAL",
sep = "\n")
lb <- read.table(text = lb_txt, sep = "|", header = FALSE, stringsAsFactors = FALSE,
  col.names = c("STUDYID","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESN","LBSTRESU","LBSTNRLO","LBSTNRHI","VISIT","VISITNUM","LBDTC","LBNRIND"),
  colClasses = c("character","character","numeric","character","character","character","character","character","numeric","character","numeric","numeric","character","numeric","character","character"))
# ensure types
lb$LBSEQ <- as.numeric(lb$LBSEQ)
lb$LBSTRESN <- as.numeric(lb$LBSTRESN)
lb$LBSTNRLO <- as.numeric(lb$LBSTNRLO)
lb$LBSTNRHI <- as.numeric(lb$LBSTNRHI)
lb$VISITNUM <- as.numeric(lb$VISITNUM)

# ---------- STEP 2: mock ADSL ----------
adsl_txt <- paste(
"STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024",
"STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024",
"STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024",
"STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024",
"STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.",
"STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024",
sep = "\n")
adsl_raw <- read.table(text = adsl_txt, sep = "|", header = FALSE, stringsAsFactors = FALSE,
  col.names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT_c","TRTEDT_c"),
  colClasses = c("character","character","character","numeric","numeric","character","character","character","character"))
adsl <- adsl_raw
adsl$TRTPN <- as.numeric(adsl$TRTPN)
adsl$AGE <- as.numeric(adsl$AGE)
adsl$TRTSDT <- parse_date9_vec(adsl$TRTSDT_c)
adsl$TRTEDT <- parse_date9_vec(adsl$TRTEDT_c)
adsl$TRTSDT_c <- NULL
adsl$TRTEDT_c <- NULL
# order columns as SAS
adsl <- adsl[, c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")]
# sort by keys (SAS proc sort)
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

# ---------- STEP 3: merge and derive adlb_base ----------
lb <- lb[order(lb$STUDYID, lb$USUBJID), ]
# inner join (if a and b), no overlapping cols except keys -> no suffixes
stopifnot(all(c("STUDYID","USUBJID") %in% names(lb)))
stopifnot(all(c("STUDYID","USUBJID") %in% names(adsl)))
adlb_base <- merge(lb, adsl, by = c("STUDYID","USUBJID"), all = FALSE, sort = FALSE)

# derive
n <- nrow(adlb_base)
adlb_base$PARAMCD <- adlb_base$LBTESTCD
adlb_base$PARAM <- adlb_base$LBTEST
adlb_base$PARCAT1 <- adlb_base$LBCAT
adlb_base$PARAMN <- ifelse(adlb_base$PARAMCD=="ALT",1,
  ifelse(adlb_base$PARAMCD=="AST",2,
  ifelse(adlb_base$PARAMCD=="ALP",3,
  ifelse(adlb_base$PARAMCD=="BILI",4,
  ifelse(adlb_base$PARAMCD=="CREAT",5,
  ifelse(adlb_base$PARAMCD=="BUN",6,
  ifelse(adlb_base$PARAMCD=="GLUC",7,
  ifelse(adlb_base$PARAMCD=="HGB",8,
  ifelse(adlb_base$PARAMCD=="WBC",9,
  ifelse(adlb_base$PARAMCD=="PLT",10,99))))))))))
adlb_base$AVAL <- adlb_base$LBSTRESN
adlb_base$AVALU <- adlb_base$LBSTRESU
adlb_base$ANRLO <- adlb_base$LBSTNRLO
adlb_base$ANRHI <- adlb_base$LBSTNRHI
adlb_base$A1LO <- adlb_base$ANRLO
adlb_base$A1HI <- adlb_base$ANRHI
adlb_base$ANRIND <- ""
for (i in seq_len(n)) {
  av <- adlb_base$AVAL[i]; lo <- adlb_base$ANRLO[i]; hi <- adlb_base$ANRHI[i]
  if (!is.na(av)) {
    if (!is.na(lo) && av < lo) adlb_base$ANRIND[i] <- "LOW"
    else if (!is.na(hi) && av > hi) adlb_base$ANRIND[i] <- "HIGH"
    else if (!is.na(lo) || !is.na(hi)) adlb_base$ANRIND[i] <- "NORMAL"
    else adlb_base$ANRIND[i] <- ""
  } else adlb_base$ANRIND[i] <- ""
}
# ADT from LBDTC yymmdd10. - ONE strict format
adlb_base$ADT <- as.Date(adlb_base$LBDTC, format = "%Y-%m-%d")
adlb_base$ADY <- rep(NA_real_, n)
for (i in seq_len(n)) {
  adt <- adlb_base$ADT[i]; ref <- adlb_base$TRTSDT[i]
  if (!is.na(adt) && !is.na(ref)) {
    if (adt >= ref) adlb_base$ADY[i] <- as.integer(adt - ref + 1)
    else adlb_base$ADY[i] <- as.integer(adt - ref)
  }
}
adlb_base$AVISIT <- adlb_base$VISIT
adlb_base$AVISITN <- adlb_base$VISITNUM
adlb_base$ABLFL <- ""
adlb_base$ANL01FL <- ""
adlb_base$SHIFT1 <- ""
adlb_base$BNRIND <- ""
# drop SDTM vars as in SAS DROP
adlb_base$LBTESTCD <- NULL; adlb_base$LBTEST <- NULL; adlb_base$LBCAT <- NULL
adlb_base$LBORRES <- NULL; adlb_base$LBORRESU <- NULL; adlb_base$LBSTRESN <- NULL
adlb_base$LBSTRESU <- NULL; adlb_base$LBSTNRLO <- NULL; adlb_base$LBSTNRHI <- NULL
adlb_base$VISIT <- NULL; adlb_base$VISITNUM <- NULL; adlb_base$LBDTC <- NULL; adlb_base$LBNRIND <- NULL
# column order for adlb_base
adlb_base <- adlb_base[, c("STUDYID","USUBJID","LBSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
  "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALU","ADT","ADY","AVISIT","AVISITN",
  "ANRLO","ANRHI","ANRIND","ABLFL","ANL01FL","A1LO","A1HI","SHIFT1","BNRIND")]

# ---------- STEP 4: baseline flag (faithful SAS logic) ----------
# sort by USUBJID PARAMCD ADT
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT, na.last = FALSE), ]
adlb_base$`_pre_trt` <- NA_real_
for (i in seq_len(nrow(adlb_base))) {
  adt <- adlb_base$ADT[i]; ref <- adlb_base$TRTSDT[i]
  if (!is.na(adt) && !is.na(ref)) {
    adlb_base$`_pre_trt`[i] <- ifelse(adt <= ref, 1, 0)
  } else {
    adlb_base$`_pre_trt`[i] <- NA_real_
  }
}
# sort by USUBJID PARAMCD descending ADT
ord <- order(adlb_base$USUBJID, adlb_base$PARAMCD, -as.numeric(adlb_base$ADT), na.last = TRUE)
adlb_base <- adlb_base[ord, ]
# BY processing first.PARAMCD
adlb_base$ABLFL <- ""
grp <- paste(adlb_base$USUBJID, adlb_base$PARAMCD, sep = "|")
first_flag <- c(TRUE, grp[-1] != grp[-length(grp)])
for (i in seq_len(nrow(adlb_base))) {
  pre <- adlb_base$`_pre_trt`[i]
  cond <- isTRUE(first_flag[i]) && !is.na(pre) && pre == 1
  adlb_base$ABLFL[i] <- ifelse(cond, "Y", "")
}
adlb_base$`_pre_trt` <- NULL
# sort back by USUBJID PARAMCD ADT
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD, adlb_base$ADT, na.last = FALSE), ]

# keep copy for output dataset adlb_base (before BASE merge)
adlb_base_out <- adlb_base

# ---------- STEP 5: BASE, BNRIND, CHG, PCHG, SHIFT ----------
baseline <- adlb_base[adlb_base$ABLFL == "Y", ]
# _baseline_ with BASE=AVAL, BNRIND=ANRIND
if (nrow(baseline) > 0) {
  base2 <- data.frame(USUBJID = baseline$USUBJID, PARAMCD = baseline$PARAMCD,
    BASE = baseline$AVAL, BNRIND_new = baseline$ANRIND, stringsAsFactors = FALSE)
} else {
  base2 <- data.frame(USUBJID = character(0), PARAMCD = character(0), BASE = numeric(0), BNRIND_new = character(0), stringsAsFactors = FALSE)
}
# sort both
adlb_base <- adlb_base[order(adlb_base$USUBJID, adlb_base$PARAMCD), ]
base2 <- base2[order(base2$USUBJID, base2$PARAMCD), ]
# drop BNRIND from left to avoid .x/.y, then left join
adlb_base$BNRIND <- NULL
stopifnot(all(c("USUBJID","PARAMCD") %in% names(adlb_base)))
stopifnot(all(c("USUBJID","PARAMCD") %in% names(base2)))
adlb <- merge(adlb_base, base2, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)
# restore BNRIND name: BNRIND_new -> BNRIND, missing -> ""
adlb$BNRIND <- adlb$BNRIND_new
adlb$BNRIND_new <- NULL
adlb$BNRIND[is.na(adlb$BNRIND)] <- ""
# CHG PCHG
adlb$CHG <- rep(NA_real_, nrow(adlb))
adlb$PCHG <- rep(NA_real_, nrow(adlb))
for (i in seq_len(nrow(adlb))) {
  av <- adlb$AVAL[i]; bs <- adlb$BASE[i]
  if (!is.na(av) && !is.na(bs)) {
    adlb$CHG[i] <- av - bs
    if (bs != 0) adlb$PCHG[i] <- (adlb$CHG[i] / bs) * 100
  }
}
# SHIFT1
# ensure SHIFT1 exists (it does)
for (i in seq_len(nrow(adlb))) {
  b <- adlb$BNRIND[i]; a <- adlb$ANRIND[i]
  if (!is.na(b) && b != "" && !is.na(a) && a != "") {
    adlb$SHIFT1[i] <- paste(b, a, sep = " to ")
  } else {
    adlb$SHIFT1[i] <- ""
  }
}
# ANL01FL
for (i in seq_len(nrow(adlb))) {
  abl <- adlb$ABLFL[i]; av <- adlb$AVAL[i]; adt <- adlb$ADT[i]; ref <- adlb$TRTSDT[i]
  if (!is.na(abl) && abl != "Y" && !is.na(av) && !is.na(adt) && !is.na(ref) && adt > ref) adlb$ANL01FL[i] <- "Y"
  else adlb$ANL01FL[i] <- ""
}
# R2A1HI, BR2A1HI
adlb$R2A1HI <- rep(NA_real_, nrow(adlb))
adlb$BR2A1HI <- rep(NA_real_, nrow(adlb))
for (i in seq_len(nrow(adlb))) {
  av <- adlb$AVAL[i]; hi <- adlb$ANRHI[i]
  if (!is.na(av) && !is.na(hi) && hi > 0) adlb$R2A1HI[i] <- av / hi
  bs <- adlb$BASE[i]; a1 <- adlb$A1HI[i]
  if (!is.na(bs) && !is.na(a1) && a1 > 0) adlb$BR2A1HI[i] <- bs / a1
}

# ---------- STEP 6: PCS flags ----------
adlb$ANRIND2 <- ""
for (i in seq_len(nrow(adlb))) {
  r <- adlb$R2A1HI[i]; ind <- adlb$ANRIND[i]
  if (is.na(ind)) ind <- ""
  if (!is.na(r) && r > 3) adlb$ANRIND2[i] <- ">3xULN"
  else if (!is.na(r) && r > 2) adlb$ANRIND2[i] <- ">2xULN"
  else if (!is.na(ind) && ind == "HIGH") adlb$ANRIND2[i] <- "HIGH"
  else if (!is.na(ind) && ind == "LOW") adlb$ANRIND2[i] <- "LOW"
  else adlb$ANRIND2[i] <- ind
}

# ---------- STEP 7: sequence ----------
adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN, adlb$ADT, na.last = FALSE), ]
adlb$ASEQ <- NA_real_
# BY STUDYID USUBJID
grp2 <- paste(adlb$STUDYID, adlb$USUBJID, sep = "|")
first2 <- c(TRUE, grp2[-1] != grp2[-length(grp2)])
cnt <- 0
for (i in seq_len(nrow(adlb))) {
  if (first2[i]) cnt <- 0
  cnt <- cnt + 1
  adlb$ASEQ[i] <- cnt
}
# final column order (varnum)
adlb <- adlb[, c("STUDYID","USUBJID","LBSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
  "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALU","ADT","ADY","AVISIT","AVISITN",
  "ANRLO","ANRHI","ANRIND","ABLFL","ANL01FL","A1LO","A1HI","SHIFT1","BNRIND",
  "BASE","CHG","PCHG","R2A1HI","BR2A1HI","ANRIND2","ASEQ")]

# labels (for contents display)
lbls <- c(STUDYID='Study Identifier',USUBJID='Unique Subject Identifier',ASEQ='Analysis Sequence Number',
PARAMCD='Parameter Code',PARAM='Parameter',PARAMN='Parameter (N)',PARCAT1='Parameter Category 1',
AVAL='Analysis Value',AVALU='Analysis Value Unit',BASE='Baseline Value',CHG='Change from Baseline',
PCHG='Percent Change from Baseline',ADT='Analysis Date',ADY='Analysis Relative Day',
AVISIT='Analysis Visit',AVISITN='Analysis Visit (N)',ABLFL='Baseline Record Flag',ANL01FL='Analysis Record Flag 01',
ANRLO='Analysis Normal Range Lower Limit',ANRHI='Analysis Normal Range Upper Limit',
ANRIND='Analysis Normal Range Indicator',BNRIND='Baseline Normal Range Indicator',SHIFT1='Shift 1',
R2A1HI='Ratio to Analysis Range 1 Upper Limit',BR2A1HI='Baseline Ratio to Analysis Range 1 Upper Limit',
TRTP='Planned Treatment',TRTPN='Planned Treatment (N)',SAFFL='Safety Population Flag')

# ---------- write datasets (CSV, ISO dates) ----------
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adlb_base_out, file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
# CSV of table body (print body)
write.csv(adlb, file.path(out, "P085.csv"), row.names = FALSE, na = "")

# ---------- text rendering: CONTENTS + PRINT ----------
# Build contents info
varnames <- names(adlb)
vartypes <- sapply(adlb, function(c) if (inherits(c, "Date")) "Num" else if (is.numeric(c)) "Num" else "Char")
varlens <- sapply(adlb, function(c) {
  if (inherits(c, "Date")) 8
  else if (is.numeric(c)) 8
  else max(nchar(as.character(c), keepNA = FALSE), 1)
})
varfmts <- sapply(varnames, function(v) if (v %in% c("TRTSDT","TRTEDT","ADT")) "DATE9." else "")
varlabels <- sapply(varnames, function(v) if (v %in% names(lbls)) lbls[[v]] else "")

con <- file(file.path(out, "P085.txt"), open = "wt")
sink(con, split = FALSE)
cat("The CONTENTS Procedure\n")
cat("\n")
cat(paste0("Data Set Name: WORK.ADLB   Observations: ", nrow(adlb), "\n"))
cat(paste0("Variables: ", ncol(adlb), "\n"))
cat("\n")
cat("Variables in Creation Order\n")
cat(sprintf("%-4s %-10s %-5s %-4s %-8s %s\n", "#", "Variable", "Type", "Len", "Format", "Label"))
for (i in seq_along(varnames)) {
  cat(sprintf("%-4d %-10s %-5s %-4d %-8s %s\n", i, varnames[i], vartypes[i], varlens[i], varfmts[i], varlabels[i]))
}
cat("\n")
cat("PROC PRINT of ADLB (OBS=20) NOOBS\n")
cat("\n")
# header
cat(paste(varnames, collapse = " | "))
cat("\n")
# rows: SAS-style display (dates DATE9., numeric missing ".", char missing blank)
for (i in seq_len(min(20, nrow(adlb)))) {
  vals <- character(length(varnames))
  for (j in seq_along(varnames)) {
    v <- varnames[j]
    x <- adlb[[v]][i]
    if (inherits(x, "Date")) {
      vals[j] <- ifelse(is.na(x), ".", fmt_date9(x))
    } else if (is.numeric(x)) {
      vals[j] <- ifelse(is.na(x), ".", as.character(x))
    } else {
      xx <- as.character(x)
      vals[j] <- ifelse(is.na(xx), "", xx)
    }
  }
  cat(paste(vals, collapse = " | "))
  cat("\n")
}
sink()
close(con)
# also write .lst copy
invisible(file.copy(file.path(out, "P085.txt"), file.path(out, "P085.lst"), overwrite = TRUE))
message("Done P085")
