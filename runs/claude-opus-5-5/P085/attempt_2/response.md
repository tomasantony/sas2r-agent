```r
# P085 - ADLB (Laboratory Analysis Dataset) - translation of P085_adlb.sas
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## ---------------- STEP 1: mock SDTM LB ----------------
lb_txt <- "STUDY01|STUDY01-1001|1|ALT|Alanine Aminotransferase|CHEMISTRY|24|U/L|24|U/L|7|56|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|2|AST|Aspartate Aminotransferase|CHEMISTRY|26|U/L|26|U/L|10|40|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|3|CREAT|Creatinine|CHEMISTRY|0.9|mg/dL|0.9|mg/dL|0.7|1.3|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|4|ALT|Alanine Aminotransferase|CHEMISTRY|28|U/L|28|U/L|7|56|Week 4|2|2024-02-12|NORMAL
STUDY01|STUDY01-1001|5|AST|Aspartate Aminotransferase|CHEMISTRY|30|U/L|30|U/L|10|40|Week 4|2|2024-02-12|NORMAL
STUDY01|STUDY01-1001|6|ALT|Alanine Aminotransferase|CHEMISTRY|32|U/L|32|U/L|7|56|Week 8|3|2024-03-11|NORMAL
STUDY01|STUDY01-1002|1|ALT|Alanine Aminotransferase|CHEMISTRY|18|U/L|18|U/L|7|56|Baseline|1|2024-01-18|NORMAL
STUDY01|STUDY01-1002|2|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Week 4|2|2024-02-15|NORMAL
STUDY01|STUDY01-1003|1|ALT|Alanine Aminotransferase|CHEMISTRY|35|U/L|35|U/L|7|56|Baseline|1|2024-01-20|NORMAL
STUDY01|STUDY01-1003|2|ALT|Alanine Aminotransferase|CHEMISTRY|95|U/L|95|U/L|7|56|Week 4|2|2024-02-17|HIGH
STUDY01|STUDY01-1003|3|ALT|Alanine Aminotransferase|CHEMISTRY|180|U/L|180|U/L|7|56|Week 8|3|2024-03-15|HIGH
STUDY01|STUDY01-1004|1|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Baseline|1|2024-01-25|NORMAL
STUDY01|STUDY01-1005|1|ALT|Alanine Aminotransferase|CHEMISTRY|19|U/L|19|U/L|7|56|Baseline|1|2024-01-28|NORMAL
STUDY01|STUDY01-1006|1|ALT|Alanine Aminotransferase|CHEMISTRY|30|U/L|30|U/L|7|56|Baseline|1|2024-01-30|NORMAL"
lb <- read.table(text = lb_txt, sep = "|", stringsAsFactors = FALSE, quote = "",
                 colClasses = c("character","character","numeric","character","character",
                                "character","character","character","numeric","character",
                                "numeric","numeric","character","numeric","character","character"),
                 col.names = c("STUDYID","USUBJID","LBSEQ","LBTESTCD","LBTEST","LBCAT","LBORRES",
                               "LBORRESU","LBSTRESN","LBSTRESU","LBSTNRLO","LBSTNRHI","VISIT",
                               "VISITNUM","LBDTC","LBNRIND"))

## ---------------- STEP 2: mock ADSL ----------------
adsl_txt <- "STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024"
adsl_raw <- read.table(text = adsl_txt, sep = "|", stringsAsFactors = FALSE, quote = "",
                       colClasses = "character",
                       col.names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL",
                                     "TRTSDT","TRTEDT"))
mon <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
d9 <- function(x) {
  x <- toupper(trimws(x))
  res <- rep(as.Date(NA), length(x))
  ok <- grepl("^[0-9]{2}[A-Z]{3}[0-9]{4}$", x)
  res[ok] <- as.Date(sprintf("%s-%02d-%s", substr(x[ok],6,9), mon[substr(x[ok],3,5)], substr(x[ok],1,2)))
  res
}
adsl <- data.frame(
  STUDYID = adsl_raw$STUDYID, USUBJID = adsl_raw$USUBJID, TRTP = adsl_raw$TRTP,
  TRTPN = as.numeric(adsl_raw$TRTPN), AGE = as.numeric(adsl_raw$AGE), SEX = adsl_raw$SEX,
  TRTSDT = d9(adsl_raw$TRTSDT), TRTEDT = d9(adsl_raw$TRTEDT), SAFFL = adsl_raw$SAFFL,
  stringsAsFactors = FALSE)
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID, method = "radix"), ]

## ---------------- STEP 3: merge and derive ----------------
lb <- lb[order(lb$STUDYID, lb$USUBJID, method = "radix"), ]
m <- inner_join(lb, adsl, by = c("STUDYID","USUBJID"))

paramn_map <- c(ALT=1, AST=2, ALP=3, BILI=4, CREAT=5, BUN=6, GLUC=7, HGB=8, WBC=9, PLT=10)
ab <- data.frame(
  STUDYID = m$STUDYID, USUBJID = m$USUBJID, LBSEQ = m$LBSEQ,
  TRTP = m$TRTP, TRTPN = m$TRTPN, AGE = m$AGE, SEX = m$SEX,
  TRTSDT = m$TRTSDT, TRTEDT = m$TRTEDT, SAFFL = m$SAFFL,
  PARAMCD = m$LBTESTCD, PARAM = m$LBTEST,
  PARAMN = ifelse(m$LBTESTCD %in% names(paramn_map), paramn_map[m$LBTESTCD], 99),
  PARCAT1 = m$LBCAT, AVAL = m$LBSTRESN, AVALC = "", AVALU = m$LBSTRESU,
  ADT = as.Date(ifelse(m$LBDTC != "", m$LBDTC, NA)), ADY = NA_real_,
  AVISIT = m$VISIT, AVISITN = m$VISITNUM,
  ANRLO = m$LBSTNRLO, ANRHI = m$LBSTNRHI, ANRIND = "",
  ABLFL = "", ANL01FL = "", A1LO = m$LBSTNRLO, A1HI = m$LBSTNRHI,
  SHIFT1 = "", BNRIND = "", stringsAsFactors = FALSE)
ab$PARAMN <- as.numeric(ab$PARAMN)
for (i in seq_len(nrow(ab))) {
  a <- ab$AVAL[i]; lo <- ab$ANRLO[i]; hi <- ab$ANRHI[i]
  if (!is.na(a)) {
    if (!is.na(lo) && a < lo) ab$ANRIND[i] <- "LOW"
    else if (!is.na(hi) && a > hi) ab$ANRIND[i] <- "HIGH"
    else if (!is.na(lo) || !is.na(hi)) ab$ANRIND[i] <- "NORMAL"
  }
  if (!is.na(ab$ADT[i]) && !is.na(ab$TRTSDT[i])) {
    d <- as.numeric(ab$ADT[i] - ab$TRTSDT[i])
    ab$ADY[i] <- if (d >= 0) d + 1 else d
  }
}

## ---------------- STEP 4: baseline flag ----------------
ab <- ab[order(ab$USUBJID, ab$PARAMCD, as.numeric(ab$ADT), method = "radix", na.last = FALSE), ]
pre <- ifelse(!is.na(ab$ADT) & !is.na(ab$TRTSDT), ifelse(ab$ADT <= ab$TRTSDT, 1, 0), NA)
ab$.pre <- pre
ab <- ab[order(ab$USUBJID, ab$PARAMCD, -as.numeric(ab$ADT), method = "radix", na.last = TRUE), ]
key <- paste(ab$USUBJID, ab$PARAMCD, sep = "\r")
first <- !duplicated(key)
ab$ABLFL <- ifelse(first & !is.na(ab$.pre) & ab$.pre == 1, "Y", "")
ab$.pre <- NULL
ab <- ab[order(ab$USUBJID, ab$PARAMCD, as.numeric(ab$ADT), method = "radix", na.last = FALSE), ]
rownames(ab) <- NULL
adlb_base <- ab

## ---------------- STEP 5: BASE, BNRIND, CHG, PCHG, SHIFT ----------------
bl <- adlb_base[adlb_base$ABLFL == "Y", c("USUBJID","PARAMCD","AVAL","ANRIND")]
names(bl) <- c("USUBJID","PARAMCD","BASE","BNRIND_B")
bl <- bl[!duplicated(paste(bl$USUBJID, bl$PARAMCD)), ]
adlb <- left_join(adlb_base, bl, by = c("USUBJID","PARAMCD"))
key <- paste(adlb$USUBJID, adlb$PARAMCD, sep = "\r")
firstg <- !duplicated(key)
hasb <- firstg & !is.na(adlb$BASE)
adlb$BNRIND[hasb] <- adlb$BNRIND_B[hasb]
adlb$BNRIND_B <- NULL

n <- nrow(adlb)
adlb$CHG <- NA_real_; adlb$PCHG <- NA_real_; adlb$R2A1HI <- NA_real_; adlb$BR2A1HI <- NA_real_
for (i in seq_len(n)) {
  if (!is.na(adlb$AVAL[i]) && !is.na(adlb$BASE[i])) {
    adlb$CHG[i] <- adlb$AVAL[i] - adlb$BASE[i]
    if (adlb$BASE[i] != 0) adlb$PCHG[i] <- adlb$CHG[i] / adlb$BASE[i] * 100
  }
  if (adlb$BNRIND[i] != "" && adlb$ANRIND[i] != "")
    adlb$SHIFT1[i] <- paste(adlb$BNRIND[i], adlb$ANRIND[i], sep = " to ")
  adt_gt <- if (is.na(adlb$ADT[i])) FALSE else if (is.na(adlb$TRTSDT[i])) TRUE else adlb$ADT[i] > adlb$TRTSDT[i]
  adlb$ANL01FL[i] <- if (adlb$ABLFL[i] != "Y" && !is.na(adlb$AVAL[i]) && adt_gt) "Y" else ""
  if (!is.na(adlb$AVAL[i]) && !is.na(adlb$ANRHI[i]) && adlb$ANRHI[i] > 0)
    adlb$R2A1HI[i] <- adlb$AVAL[i] / adlb$ANRHI[i]
  if (!is.na(adlb$BASE[i]) && !is.na(adlb$A1HI[i]) && adlb$A1HI[i] > 0)
    adlb$BR2A1HI[i] <- adlb$BASE[i] / adlb$A1HI[i]
}

## ---------------- STEP 6: PCS flags ----------------
adlb$ANRIND2 <- ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 3, ">3xULN",
                ifelse(!is.na(adlb$R2A1HI) & adlb$R2A1HI > 2, ">2xULN",
                ifelse(adlb$ANRIND == "HIGH", "HIGH",
                ifelse(adlb$ANRIND == "LOW", "LOW", adlb$ANRIND))))

## ---------------- STEP 7: sequence ----------------
adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN, as.numeric(adlb$ADT),
                   method = "radix", na.last = FALSE), ]
adlb$ASEQ <- ave(seq_len(nrow(adlb)), adlb$STUDYID, adlb$USUBJID, FUN = seq_along)
rownames(adlb) <- NULL

## ---------------- Output datasets ----------------
fmt_date_csv <- function(df) {
  for (v in names(df)) if (inherits(df[[v]], "Date")) df[[v]] <- format(df[[v]], "%Y-%m-%d")
  df
}
write.csv(fmt_date_csv(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(fmt_date_csv(adlb_base), file.path(out, "adlb_base.csv"), row.names = FALSE, na = "")
write.csv(fmt_date_csv(adlb), file.path(out, "adlb.csv"), row.names = FALSE, na = "")

## ---------------- PROC CONTENTS (varnum) ----------------
# SAS variable order / attributes
meta <- data.frame(
  var = c("STUDYID","USUBJID","LBSEQ","TRTP","TRTPN","AGE","SEX","TRTSDT","TRTEDT","SAFFL",
          "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","AVISIT",
          "AVISITN","ANRLO","ANRHI","ANRIND","ABLFL","ANL01FL","A1LO","A1HI","SHIFT1","BNRIND",
          "BASE","CHG","PCHG","R2A1HI","BR2A1HI","ANRIND2","ASEQ"),
  type = c("Char","Char","Num","Char","Num","Num","Char","Num","Num","Char",
           "Char","Char","Num","Char","Num","Char","Char","Num","Num","Char",
           "Num","Num","Num","Char","Char","Char","Num","Num","Char","Char",
           "Num","Num","Num","Num","Num","Char","Num"),
  len = c(12,20,8,40,8,8,1,8,8,1, 8,200,8,50,8,200,20,8,8,40, 8,8,8,20,1,1,8,8,40,10, 8,8,8,8,8,20,8),
  fmt = c("","","","","","","","DATE9.","DATE9.","", "","","","","","","","DATE9.","","",
          "","","","","","","","","","", "","","","","","",""),
  label = c("Study Identifier","Unique Subject Identifier","","Planned Treatment","Planned Treatment (N)",
            "","","","","Safety Population Flag",
            "Parameter Code","Parameter","Parameter (N)","Parameter Category 1","Analysis Value","",
            "Analysis Value Unit","Analysis Date","Analysis Relative Day","Analysis Visit",
            "Analysis Visit (N)","Analysis Normal Range Lower Limit","Analysis Normal Range Upper Limit",
            "Analysis Normal Range Indicator","Baseline Record Flag","Analysis Record Flag 01","","",
            "Shift 1","Baseline Normal Range Indicator",
            "Baseline Value","Change from Baseline","Percent Change from Baseline",
            "Ratio to Analysis Range 1 Upper Limit","Baseline Ratio to Analysis Range 1 Upper Limit","",
            "Analysis Sequence Number"),
  stringsAsFactors = FALSE)
obslen <- ceiling(sum(meta$len) / 8) * 8
pagesize <- 69632
maxobs <- floor((pagesize - 128) / (obslen + 1))  # approx SAS
adlb_out <- adlb[, meta$var]

## ---------------- PROC PRINT (obs=20) ----------------
date9 <- function(d) ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    s <- formatC(v, digits = 10, format = "fg")
    s <- trimws(s)
    if (nchar(s) > 12) s <- formatC(v, digits = 9, format = "fg")
    trimws(s)
  })
}
pr <- head(adlb_out, 20)
disp <- pr
for (v in names(disp)) {
  if (inherits(disp[[v]], "Date")) disp[[v]] <- date9(disp[[v]])
  else if (is.numeric(disp[[v]])) disp[[v]] <- best12(disp[[v]])
  disp[[v]] <- trimws(disp[[v]])
}
write.csv(fmt_date_csv(pr), file.path(out, "P085.csv"), row.names = FALSE, na = "")

con <- file(file.path(out, "P085.txt"), "w")
writeLines("The CONTENTS Procedure", con)
writeLines("", con)
writeLines(sprintf("Data Set Name        WORK.ADLB                Observations          %d", nrow(adlb_out)), con)
writeLines(sprintf("Member Type          DATA                     Variables             %d", ncol(adlb_out)), con)
writeLines(sprintf("Engine               V9                       Indexes               0"), con)
writeLines(sprintf("Created              %s", format(Sys.time(), "%d/%m/%Y %H:%M:%S")), con)
writeLines(sprintf("Observation Length   %d", obslen), con)
writeLines("Deleted Observations 0", con)
writeLines("", con)
writeLines("Engine/Host Dependent Information", con)
writeLines(sprintf("Data Set Page Size          %d", pagesize), con)
writeLines("Number of Data Set Pages    1", con)
writeLines("First Data Page             1", con)
writeLines(sprintf("Max Obs per Page            %d", maxobs), con)
writeLines(sprintf("Obs in First Data Page      %d", nrow(adlb_out)), con)
writeLines("Number of Data Set Repairs  0", con)
writeLines("Release Created             9.0401M7", con)
writeLines("", con)
writeLines("Variables in Creation Order", con)
writeLines(sprintf("%3s  %-10s %-5s %4s  %-8s %s", "#", "Variable", "Type", "Len", "Format", "Label"), con)
writeLines(sprintf("%3d  %-10s %-5s %4d  %-8s %s", seq_len(nrow(meta)), meta$var, meta$type,
                   as.integer(meta$len), meta$fmt, meta$label), con)
writeLines("", con)
writeLines("PROC PRINT data=adlb(obs=20) noobs", con)
writeLines(capture.output(print(disp, row.names = FALSE, right = TRUE)), con)
close(con)

cat("P085 done:", nrow(adlb), "ADLB records\n")
```