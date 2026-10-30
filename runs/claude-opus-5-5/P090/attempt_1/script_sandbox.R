suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."

## STEP 1: mock SDTM CM
cm_txt <- "STUDY01|STUDY01-1001|1|ASPIRIN|ASPIRIN|ANALGESICS|2023-06-15|.|81|mg|ORAL|QD|CARDIOVASCULAR PROTECTION|Y
STUDY01|STUDY01-1001|2|LISINOPRIL|LISINOPRIL|ACE INHIBITORS|2023-01-10|.|10|mg|ORAL|QD|HYPERTENSION|Y
STUDY01|STUDY01-1001|3|ACETAMINOPHEN|PARACETAMOL|ANALGESICS|2024-02-01|2024-02-03|500|mg|ORAL|PRN|HEADACHE|N
STUDY01|STUDY01-1002|1|METFORMIN|METFORMIN|ANTIDIABETICS|2022-03-20|.|500|mg|ORAL|BID|DIABETES|Y
STUDY01|STUDY01-1002|2|ATORVASTATIN|ATORVASTATIN|STATINS|2023-08-01|.|20|mg|ORAL|QD|HYPERLIPIDEMIA|Y
STUDY01|STUDY01-1003|1|OMEPRAZOLE|OMEPRAZOLE|PROTON PUMP INHIBITORS|2024-01-15|.|20|mg|ORAL|QD|GERD|Y
STUDY01|STUDY01-1003|2|IBUPROFEN|IBUPROFEN|NSAIDS|2024-02-20|2024-02-22|400|mg|ORAL|TID|BACK PAIN|N
STUDY01|STUDY01-1004|1|AMLODIPINE|AMLODIPINE|CALCIUM CHANNEL BLOCKERS|2023-05-10|.|5|mg|ORAL|QD|HYPERTENSION|Y
STUDY01|STUDY01-1005|1|LORATADINE|LORATADINE|ANTIHISTAMINES|2024-03-01|2024-03-15|10|mg|ORAL|QD|ALLERGIES|N
STUDY01|STUDY01-1006|1|LEVOTHYROXINE|LEVOTHYROXINE|THYROID HORMONES|2020-01-01|.|75|mcg|ORAL|QD|HYPOTHYROIDISM|Y
STUDY01|STUDY01-1006|2|CALCIUM|CALCIUM CARBONATE|SUPPLEMENTS|2023-09-01|.|600|mg|ORAL|QD|BONE HEALTH|Y"
cm <- read.table(text = cm_txt, sep = "|", quote = "", stringsAsFactors = FALSE,
                 colClasses = "character",
                 col.names = c("STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS",
                               "CMSTDTC","CMENDTC","CMDOSE","CMDOSU","CMROUTE","CMFRQ",
                               "CMINDC","CMONGO"))
# SAS list input: a lone '.' for character is missing
cm[] <- lapply(cm, function(x) ifelse(x == ".", "", x))
cm$CMSEQ <- as.numeric(cm$CMSEQ)
cm$CMDOSE <- as.numeric(cm$CMDOSE)

## STEP 2: mock ADSL
adsl_txt <- "STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024"
a <- read.table(text = adsl_txt, sep = "|", quote = "", stringsAsFactors = FALSE,
                colClasses = "character",
                col.names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT"))
pdate9 <- function(x) {
  m <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
  res <- rep(as.Date(NA), length(x))
  ok <- nchar(x) == 9
  res[ok] <- as.Date(sprintf("%s-%02d-%s", substr(x[ok],6,9), m[substr(x[ok],3,5)], substr(x[ok],1,2)))
  res
}
adsl <- data.frame(STUDYID = a$STUDYID, USUBJID = a$USUBJID, TRTP = a$TRTP,
                   TRTPN = as.numeric(a$TRTPN), AGE = as.numeric(a$AGE), SEX = a$SEX,
                   TRTSDT = pdate9(a$TRTSDT), TRTEDT = pdate9(a$TRTEDT), SAFFL = a$SAFFL,
                   stringsAsFactors = FALSE)
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

## STEP 3: merge and derive
cm <- cm[order(cm$STUDYID, cm$USUBJID), ]
adcm <- inner_join(cm, adsl, by = c("STUDYID","USUBJID"))

n <- nrow(adcm)
adcm$ASTDT <- as.Date(ifelse(adcm$CMSTDTC != "", adcm$CMSTDTC, NA))
adcm$AENDT <- as.Date(ifelse(adcm$CMENDTC != "", adcm$CMENDTC, NA))
st <- as.numeric(adcm$ASTDT); en <- as.numeric(adcm$AENDT)
ts <- as.numeric(adcm$TRTSDT); te <- as.numeric(adcm$TRTEDT)
adcm$ASTDY <- ifelse(!is.na(st) & !is.na(ts), ifelse(st >= ts, st - ts + 1, st - ts), NA)
adcm$AENDY <- ifelse(!is.na(en) & !is.na(ts), ifelse(en >= ts, en - ts + 1, en - ts), NA)

PREFL <- ifelse(!is.na(st) & !is.na(ts), ifelse(st < ts, "Y", "N"), "")
today <- as.numeric(Sys.Date())
trt_end <- ifelse(is.na(te), today, te)
ONTRTFL <- rep("", n)
for (i in seq_len(n)) {
  if (!is.na(ts[i]) && !is.na(st[i])) {
    if (is.na(en[i])) {
      if (st[i] <= trt_end[i]) ONTRTFL[i] <- "Y"
    } else {
      if (st[i] <= trt_end[i] && en[i] >= ts[i]) ONTRTFL[i] <- "Y"
    }
  }
}
ONTRTFL[ONTRTFL == ""] <- "N"
adcm$ONTRTFL <- ONTRTFL
adcm$PREFL <- PREFL
adcm$CONCOMFL <- ifelse(ONTRTFL == "Y" | PREFL == "Y", "Y", "N")

atc <- data.frame(
  k = c("ANALGESICS","ACE INHIBITORS","ANTIDIABETICS","STATINS","PROTON PUMP INHIBITORS",
        "NSAIDS","CALCIUM CHANNEL BLOCKERS","ANTIHISTAMINES","THYROID HORMONES","SUPPLEMENTS"),
  cd = c("N02","C09A","A10","C10AA","A02BC","M01A","C08CA","R06A","H03AA","A12AA"),
  tx = c("ANALGESICS","ACE INHIBITORS, PLAIN","DRUGS USED IN DIABETES","HMG COA REDUCTASE INHIBITORS",
         "PROTON PUMP INHIBITORS","ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS",
         "DIHYDROPYRIDINE DERIVATIVES","ANTIHISTAMINES FOR SYSTEMIC USE","THYROID HORMONES","CALCIUM"),
  stringsAsFactors = FALSE)
CMCLAS_UP <- toupper(adcm$CMCLAS)
mi <- match(CMCLAS_UP, atc$k)
adcm$ATCCD1 <- ifelse(is.na(mi), "", atc$cd[mi])
adcm$ATCCD2 <- ""
adcm$ATC1 <- ifelse(is.na(mi), "", atc$tx[mi])
adcm$ATC2 <- ""
adcm$CMDECOD_UP <- toupper(adcm$CMDECOD)
adcm$CMCLAS_UP <- CMCLAS_UP
adcm$ADTEFFL <- ifelse(adcm$CMONGO == "Y" | is.na(en), "Y", "N")
adcm$ADTEFN <- ifelse(adcm$ADTEFFL == "Y", 1, 0)
adcm$CMDURN <- ifelse(!is.na(st) & !is.na(en), en - st + 1, NA)

## STEP 4: sort and ASEQ
adcm <- adcm[order(adcm$STUDYID, adcm$USUBJID, as.numeric(adcm$ASTDT), adcm$CMSEQ, na.last = FALSE), ]
adcm <- adcm %>% group_by(STUDYID, USUBJID) %>% mutate(ASEQ = row_number()) %>% ungroup() %>% as.data.frame()

ord <- c("STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS","CMSTDTC","CMENDTC","CMDOSE",
         "CMDOSU","CMROUTE","CMFRQ","CMINDC","CMONGO","TRTP","TRTPN","AGE","SEX","TRTSDT","TRTEDT",
         "SAFFL","ASTDT","AENDT","ASTDY","AENDY","ONTRTFL","PREFL","CONCOMFL","ATCCD1","ATCCD2",
         "ATC1","ATC2","CMDECOD_UP","CMCLAS_UP","ADTEFFL","ADTEFN","CMDURN","ASEQ")
adcm <- adcm[, ord]

fmtd <- function(df) {
  for (v in names(df)) if (inherits(df[[v]], "Date")) df[[v]] <- format(df[[v]], "%Y-%m-%d")
  df
}
write.csv(fmtd(adcm), file.path(out, "adcm.csv"), row.names = FALSE, na = "")
write.csv(fmtd(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")

## Listing (PROC CONTENTS varnum + PROC PRINT obs=15)
date9 <- function(d) ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
pr <- head(adcm, 15)
for (v in c("TRTSDT","TRTEDT","ASTDT","AENDT")) pr[[v]] <- date9(pr[[v]])
pr[] <- lapply(pr, function(x) { x <- as.character(x); x[is.na(x)] <- "."; x })
write.csv(pr, file.path(out, "P090.csv"), row.names = FALSE)

con <- file(file.path(out, "P090.txt"), "w")
writeLines("The CONTENTS Procedure", con)
writeLines(sprintf("Data Set Name WORK.ADCM   Observations %d   Variables %d", nrow(adcm), ncol(adcm)), con)
writeLines("Variables in Creation Order", con)
for (i in seq_along(ord)) writeLines(sprintf("%3d  %s", i, ord[i]), con)
writeLines("", con)
writeLines("PROC PRINT (obs=15)", con)
w <- sapply(names(pr), function(v) max(nchar(v), nchar(pr[[v]])))
writeLines(paste(mapply(function(v, ww) formatC(v, width = ww), names(pr), w), collapse = "  "), con)
for (i in seq_len(nrow(pr))) {
  writeLines(paste(mapply(function(x, ww) formatC(x, width = ww), unlist(pr[i, ]), w), collapse = "  "), con)
}
close(con)
cat("P090 done\n")
