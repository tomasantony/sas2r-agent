```r
# P087 - ADVS (Vital Signs Analysis Dataset) translated from SAS
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# STEP 1: mock SDTM VS
# ---------------------------------------------------------------------------
vs_txt <- "STUDY01|STUDY01-1001|1|SYSBP|Systolic Blood Pressure|128|mmHg|128|mmHg|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|DIABP|Diastolic Blood Pressure|82|mmHg|82|mmHg|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|3|PULSE|Pulse Rate|72|beats/min|72|beats/min|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|4|SYSBP|Systolic Blood Pressure|122|mmHg|122|mmHg|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|5|DIABP|Diastolic Blood Pressure|78|mmHg|78|mmHg|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|6|PULSE|Pulse Rate|70|beats/min|70|beats/min|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|7|SYSBP|Systolic Blood Pressure|118|mmHg|118|mmHg|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|8|DIABP|Diastolic Blood Pressure|76|mmHg|76|mmHg|Week 8|3|2024-03-11
STUDY01|STUDY01-1002|1|SYSBP|Systolic Blood Pressure|140|mmHg|140|mmHg|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|DIABP|Diastolic Blood Pressure|90|mmHg|90|mmHg|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|3|SYSBP|Systolic Blood Pressure|138|mmHg|138|mmHg|Week 4|2|2024-02-15
STUDY01|STUDY01-1002|4|DIABP|Diastolic Blood Pressure|88|mmHg|88|mmHg|Week 4|2|2024-02-15
STUDY01|STUDY01-1003|1|SYSBP|Systolic Blood Pressure|120|mmHg|120|mmHg|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|DIABP|Diastolic Blood Pressure|75|mmHg|75|mmHg|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|3|SYSBP|Systolic Blood Pressure|115|mmHg|115|mmHg|Week 4|2|2024-02-17
STUDY01|STUDY01-1004|1|SYSBP|Systolic Blood Pressure|132|mmHg|132|mmHg|Baseline|1|2024-01-25
STUDY01|STUDY01-1005|1|SYSBP|Systolic Blood Pressure|118|mmHg|118|mmHg|Baseline|1|2024-01-28
STUDY01|STUDY01-1006|1|SYSBP|Systolic Blood Pressure|145|mmHg|145|mmHg|Baseline|1|2024-01-30"

vs <- read.table(text = vs_txt, sep = "|", colClasses = "character",
                 col.names = c("STUDYID","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES",
                               "VSORRESU","VSSTRESN","VSSTRESU","VISIT","VISITNUM","VSDTC"),
                 stringsAsFactors = FALSE, quote = "")
vs$VSSEQ <- as.numeric(vs$VSSEQ)
vs$VSSTRESN <- as.numeric(vs$VSSTRESN)
vs$VISITNUM <- as.numeric(vs$VISITNUM)

# ---------------------------------------------------------------------------
# STEP 2: mock ADSL
# ---------------------------------------------------------------------------
adsl_txt <- "STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024"
adsl_raw <- read.table(text = adsl_txt, sep = "|", colClasses = "character",
                       col.names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL",
                                     "TRTSDT","TRTEDT"),
                       stringsAsFactors = FALSE, quote = "")
d9 <- function(x) {
  x[x == "."] <- NA
  as.Date(x, format = "%d%b%Y")
}
old_loc <- Sys.getlocale("LC_TIME")
invisible(Sys.setlocale("LC_TIME", "C"))
adsl <- data.frame(
  STUDYID = adsl_raw$STUDYID, USUBJID = adsl_raw$USUBJID, TRTP = adsl_raw$TRTP,
  TRTPN = as.numeric(adsl_raw$TRTPN), AGE = as.numeric(adsl_raw$AGE), SEX = adsl_raw$SEX,
  TRTSDT = d9(adsl_raw$TRTSDT), TRTEDT = d9(adsl_raw$TRTEDT), SAFFL = adsl_raw$SAFFL,
  stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# STEP 3: merge & base structure
# ---------------------------------------------------------------------------
vs <- vs[order(vs$STUDYID, vs$USUBJID, method = "radix"), ]
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID, method = "radix"), ]

ab <- inner_join(vs, adsl, by = c("STUDYID", "USUBJID"))
ab$PARAMCD <- ab$VSTESTCD
ab$PARAM <- ab$VSTEST
ab$PARAMN <- dplyr::case_when(
  ab$PARAMCD == "SYSBP" ~ 1, ab$PARAMCD == "DIABP" ~ 2, ab$PARAMCD == "PULSE" ~ 3,
  ab$PARAMCD == "TEMP" ~ 4, ab$PARAMCD == "RESP" ~ 5, ab$PARAMCD == "WEIGHT" ~ 6,
  ab$PARAMCD == "HEIGHT" ~ 7, TRUE ~ 99)
ab$PARCAT1 <- "VITAL SIGNS"
ab$AVAL <- ab$VSSTRESN
ab$AVALC <- ""
ab$AVALU <- ab$VSSTRESU
ab$ADT <- as.Date(ifelse(ab$VSDTC != "", ab$VSDTC, NA), format = "%Y-%m-%d")
ab$ADY <- ifelse(!is.na(ab$ADT) & !is.na(ab$TRTSDT),
                 ifelse(ab$ADT >= ab$TRTSDT,
                        as.numeric(ab$ADT - ab$TRTSDT) + 1,
                        as.numeric(ab$ADT - ab$TRTSDT)), NA_real_)
ab$ATPT <- ""
ab$ATPTN <- NA_real_
ab$AVISIT <- ab$VISIT
ab$AVISITN <- ab$VISITNUM
ab$ABLFL <- ""
ab$ANL01FL <- ""

base_cols <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","TRTSDT","TRTEDT","SAFFL",
               "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY",
               "ATPT","ATPTN","AVISIT","AVISITN","ABLFL","ANL01FL")
advs_base <- ab[, base_cols]

# ---------------------------------------------------------------------------
# STEP 4: baseline flag
# ---------------------------------------------------------------------------
advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, advs_base$ADT,
                             method = "radix", na.last = FALSE), ]
advs_base$`_pre_trt` <- ifelse(!is.na(advs_base$ADT) & !is.na(advs_base$TRTSDT),
                               ifelse(advs_base$ADT <= advs_base$TRTSDT, 1, 0), NA)

advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, -as.numeric(advs_base$ADT),
                             method = "radix", na.last = TRUE), ]
key <- paste(advs_base$USUBJID, advs_base$PARAMCD, sep = "\r")
firstp <- !duplicated(key)
advs_base$ABLFL <- ifelse(firstp & !is.na(advs_base$`_pre_trt`) & advs_base$`_pre_trt` == 1, "Y", "")
advs_base$`_pre_trt` <- NULL

advs_base <- advs_base[order(advs_base$USUBJID, advs_base$PARAMCD, advs_base$ADT,
                             method = "radix", na.last = FALSE), ]
rownames(advs_base) <- NULL

# ---------------------------------------------------------------------------
# STEP 5: BASE / CHG / PCHG
# ---------------------------------------------------------------------------
baseline <- advs_base[advs_base$ABLFL == "Y", c("USUBJID","PARAMCD","AVAL")]
names(baseline)[3] <- "BASE"
baseline <- baseline[!duplicated(paste(baseline$USUBJID, baseline$PARAMCD)), ]

advs <- left_join(advs_base, baseline, by = c("USUBJID","PARAMCD"))
advs$CHG <- ifelse(!is.na(advs$AVAL) & !is.na(advs$BASE), advs$AVAL - advs$BASE, NA_real_)
advs$PCHG <- ifelse(!is.na(advs$CHG) & advs$BASE != 0, (advs$CHG / advs$BASE) * 100, NA_real_)
advs$ANL01FL <- ifelse(advs$ABLFL != "Y" & !is.na(advs$AVAL) &
                         !is.na(advs$ADT) & (is.na(advs$TRTSDT) | advs$ADT > advs$TRTSDT),
                       "Y", "")
# SAS: missing TRTSDT is smallest, so ADT > . is true when ADT non-missing

# ---------------------------------------------------------------------------
# STEP 6: maximum post-baseline records
# ---------------------------------------------------------------------------
gt_sas <- function(a, b) {  # SAS comparison with missing as -Inf
  a2 <- ifelse(is.na(a), -Inf, as.numeric(a)); b2 <- ifelse(is.na(b), -Inf, as.numeric(b))
  a2 > b2
}
sub <- advs[advs$ABLFL != "Y" & gt_sas(advs$ADT, advs$TRTSDT), ]
max_post <- sub %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(AVAL = if (all(is.na(AVAL))) NA_real_ else max(AVAL, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()
bl <- advs[advs$ABLFL == "Y", c("ABLFL","USUBJID","PARAMCD","BASE")]
max_post <- left_join(max_post, bl, by = c("USUBJID","PARAMCD"))
max_post$ABLFL[is.na(max_post$ABLFL)] <- ""
max_post$DTYPE <- "MAXIMUM"
max_post$AVISIT <- "Maximum Post-Baseline"
max_post$AVISITN <- 99
max_post$CHG <- ifelse(!is.na(max_post$AVAL) & !is.na(max_post$BASE),
                       max_post$AVAL - max_post$BASE, NA_real_)

advs$DTYPE <- ""
advs <- bind_rows(advs, max_post)
chr_cols <- c("STUDYID","USUBJID","TRTP","SEX","SAFFL","PARAMCD","PARAM","PARCAT1","AVALC",
              "AVALU","ATPT","AVISIT","ABLFL","ANL01FL","DTYPE")
for (cc in chr_cols) advs[[cc]][is.na(advs[[cc]])] <- ""

# ---------------------------------------------------------------------------
# STEP 7: sort and ASEQ
# ---------------------------------------------------------------------------
advs <- advs[order(advs$STUDYID, advs$USUBJID, advs$PARAMCD, advs$AVISITN, advs$ADT,
                   method = "radix", na.last = FALSE), ]
grp <- paste(advs$STUDYID, advs$USUBJID, sep = "\r")
advs$ASEQ <- ave(seq_along(grp), grp, FUN = seq_along)
final_cols <- c(base_cols, "BASE","CHG","PCHG","DTYPE","ASEQ")
advs <- advs[, final_cols]
rownames(advs) <- NULL

# ---------------------------------------------------------------------------
# Output datasets
# ---------------------------------------------------------------------------
fmt_dates <- function(df) {
  for (v in names(df)) if (inherits(df[[v]], "Date")) df[[v]] <- format(df[[v]], "%Y-%m-%d")
  df
}
write.csv(fmt_dates(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(fmt_dates(advs_base), file.path(out, "advs_base.csv"), row.names = FALSE, na = "")
write.csv(fmt_dates(advs), file.path(out, "advs.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------------------
# Listing: PROC CONTENTS (varnum) + PROC PRINT obs=20
# ---------------------------------------------------------------------------
lbls <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
          ASEQ="Analysis Sequence Number", PARAMCD="Parameter Code", PARAM="Parameter",
          PARAMN="Parameter (N)", PARCAT1="Parameter Category 1", AVAL="Analysis Value",
          AVALU="Analysis Value Unit", BASE="Baseline Value", CHG="Change from Baseline",
          PCHG="Percent Change from Baseline", ADT="Analysis Date", ADY="Analysis Relative Day",
          AVISIT="Analysis Visit", AVISITN="Analysis Visit (N)", ABLFL="Baseline Record Flag",
          ANL01FL="Analysis Record Flag 01", DTYPE="Derivation Type", TRTP="Planned Treatment",
          TRTPN="Planned Treatment (N)", TRTSDT="Date of First Exposure to Treatment",
          SAFFL="Safety Population Flag")
lens <- c(STUDYID=12, USUBJID=20, VSSEQ=8, TRTP=40, TRTPN=8, AGE=8, SEX=1, TRTSDT=8, TRTEDT=8,
          SAFFL=1, PARAMCD=8, PARAM=200, PARAMN=8, PARCAT1=50, AVAL=8, AVALC=200, AVALU=20,
          ADT=8, ADY=8, ATPT=20, ATPTN=8, AVISIT=40, AVISITN=8, ABLFL=1, ANL01FL=1, BASE=8,
          CHG=8, PCHG=8, DTYPE=20, ASEQ=8)
contents <- data.frame(
  Num = seq_along(final_cols), Variable = final_cols,
  Type = ifelse(final_cols %in% chr_cols, "Char", "Num"),
  Len = unname(lens[final_cols]),
  Format = ifelse(final_cols %in% c("TRTSDT","TRTEDT","ADT"), "DATE9.", ""),
  Label = ifelse(final_cols %in% names(lbls), unname(lbls[final_cols]), ""),
  stringsAsFactors = FALSE)

pr <- advs[seq_len(min(20, nrow(advs))), ]
for (v in c("TRTSDT","TRTEDT","ADT")) {
  pr[[v]] <- ifelse(is.na(pr[[v]]), ".", toupper(format(pr[[v]], "%d%b%Y")))
}
for (v in names(pr)) {
  if (is.numeric(pr[[v]])) {
    x <- pr[[v]]
    pr[[v]] <- ifelse(is.na(x), ".", format(signif(x, 12), trim = TRUE, drop0trailing = TRUE))
  }
}
write.csv(pr, file.path(out, "P087.csv"), row.names = FALSE, na = "")

con <- file(file.path(out, "P087.txt"), "w")
writeLines("The CONTENTS Procedure", con)
writeLines(sprintf("Data Set Name WORK.ADVS   Observations %d   Variables %d", nrow(advs), ncol(advs)), con)
writeLines("", con)
writeLines(capture.output(print(contents, row.names = FALSE, right = FALSE)), con)
writeLines("", con)
old_w <- options(width = 250)
writeLines(capture.output(print(pr, row.names = FALSE, right = FALSE)), con)
options(old_w)
close(con)
invisible(Sys.setlocale("LC_TIME", old_loc))

cat("P087 done: advs rows =", nrow(advs), "\n")
```