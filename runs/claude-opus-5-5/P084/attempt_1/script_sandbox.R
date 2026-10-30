# P084 - ADAE creation (translation of P084_adae.sas)
suppressPackageStartupMessages({
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Character ordering like SAS (byte/C locale)
sas_order <- function(...) {
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old))
  Sys.setlocale("LC_COLLATE", "C")
  order(..., na.last = FALSE, method = "radix")
}

# ---------------- STEP 1: mock SDTM AE ----------------
ae_txt <- c(
"STUDY01|STUDY01-1001|1|Headache|Headache|Nervous system disorders|2024-02-01|2024-02-03|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1",
"STUDY01|STUDY01-1001|2|Nausea|Nausea|Gastrointestinal disorders|2024-02-15|2024-02-18|MODERATE|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|2",
"STUDY01|STUDY01-1002|1|Fatigue|Fatigue|General disorders|2024-02-10|2024-03-01|MILD|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|1",
"STUDY01|STUDY01-1003|1|Dizziness|Dizziness|Nervous system disorders|2024-02-20|2024-02-22|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1",
"STUDY01|STUDY01-1003|2|Rash|Rash|Skin disorders|2024-03-05|2024-03-15|MODERATE|N|PROBABLY RELATED|DOSE REDUCED|RECOVERED|2",
"STUDY01|STUDY01-1003|3|Hepatotoxicity|Hepatotoxicity|Hepatobiliary disorders|2024-03-25|.|SEVERE|Y|RELATED|DRUG WITHDRAWN|NOT RECOVERED|3",
"STUDY01|STUDY01-1004|1|Insomnia|Insomnia|Psychiatric disorders|2024-02-25|2024-03-10|MILD|N|UNLIKELY|DOSE NOT CHANGED|RECOVERED|1",
"STUDY01|STUDY01-1005|1|Vomiting|Vomiting|Gastrointestinal disorders|2024-03-01|2024-03-02|MODERATE|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|2",
"STUDY01|STUDY01-1006|1|Back pain|Back pain|Musculoskeletal disorders|2024-02-28|2024-03-05|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1")
ae_cols <- c("STUDYID","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESTDTC",
             "AEENDTC","AESEV","AESER","AEREL","AEACN","AEOUT","AETOXGR")
ae <- as.data.frame(do.call(rbind, strsplit(ae_txt, "|", fixed = TRUE)),
                    stringsAsFactors = FALSE)
names(ae) <- ae_cols
# SAS list input: a lone '.' for a character variable is read as blank
for (v in setdiff(ae_cols, "AESEQ")) ae[[v]][ae[[v]] == "."] <- ""
ae$AESEQ <- as.numeric(ae$AESEQ)

# ---------------- STEP 2: mock ADSL ----------------
adsl_txt <- c(
"STUDY01|STUDY01-1001|101|Treatment A|1|59|<65|M|WHITE|Y|15JAN2024|15JUN2024",
"STUDY01|STUDY01-1002|101|Placebo|2|45|<65|F|BLACK OR AFRICAN AMERICAN|Y|18JAN2024|18JUN2024",
"STUDY01|STUDY01-1003|102|Treatment A|1|43|<65|M|ASIAN|Y|20JAN2024|01APR2024",
"STUDY01|STUDY01-1004|102|Treatment A|1|51|<65|F|WHITE|Y|25JAN2024|25JUN2024",
"STUDY01|STUDY01-1005|103|Placebo|2|36|<65|M|ASIAN|Y|28JAN2024|.",
"STUDY01|STUDY01-1006|103|Treatment A|1|68|>=65|F|WHITE|Y|30JAN2024|01JUL2024")
adsl_cols <- c("STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1","SEX",
               "RACE","SAFFL","TRTSDT","TRTEDT")
adsl <- as.data.frame(do.call(rbind, strsplit(adsl_txt, "|", fixed = TRUE)),
                      stringsAsFactors = FALSE)
names(adsl) <- adsl_cols
adsl$TRTPN <- as.numeric(adsl$TRTPN)
adsl$AGE   <- as.numeric(adsl$AGE)
pdate9 <- function(x) {
  mon <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
  res <- as.Date(rep(NA_character_, length(x)))
  ok <- nchar(x) == 9
  res[ok] <- as.Date(sprintf("%s-%02d-%s", substr(x[ok],6,9),
                             mon[substr(x[ok],3,5)], substr(x[ok],1,2)))
  res
}
adsl$TRTSDT <- pdate9(adsl$TRTSDT)
adsl$TRTEDT <- pdate9(adsl$TRTEDT)

# ---------------- STEP 3: merge and derive ----------------
ae   <- ae[sas_order(ae$STUDYID, ae$USUBJID), ]
adsl <- adsl[sas_order(adsl$STUDYID, adsl$USUBJID), ]

adae <- inner_join(ae, adsl, by = c("STUDYID","USUBJID"))

adae <- adae %>% mutate(
  ASTDT = as.Date(ifelse(AESTDTC != "", AESTDTC, NA)),
  AENDT = as.Date(ifelse(AEENDTC != "" & AEENDTC != ".", AEENDTC, NA)),
  ASTDY = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
                 ifelse(ASTDT >= TRTSDT, as.numeric(ASTDT - TRTSDT) + 1,
                        as.numeric(ASTDT - TRTSDT)), NA_real_),
  AENDY = ifelse(!is.na(AENDT) & !is.na(TRTSDT),
                 ifelse(AENDT >= TRTSDT, as.numeric(AENDT - TRTSDT) + 1,
                        as.numeric(AENDT - TRTSDT)), NA_real_),
  TRTEMFL = ifelse(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT &
                     (is.na(TRTEDT) | ASTDT <= TRTEDT + 30), "Y", "N"),
  TRTEMFL = ifelse(is.na(TRTEMFL), "N", TRTEMFL),
  AESER_FL = AESER,
  AOCCFL = "", AOCCSFL = "", AOCCPFL = "",
  ASEV = AESEV,
  ASEVN = c(MILD = 1, MODERATE = 2, SEVERE = 3)[toupper(AESEV)],
  AREL = AEREL,
  ARELN = c("NOT RELATED" = 1, "UNLIKELY" = 2, "POSSIBLY RELATED" = 3,
            "PROBABLY RELATED" = 4, "RELATED" = 5)[toupper(AEREL)],
  AESOC = AEBODSYS,
  AEDECOD_UP = toupper(AEDECOD),
  AEDUR = ifelse(!is.na(ASTDT) & !is.na(AENDT),
                 as.numeric(AENDT - ASTDT) + 1, NA_real_)
)
adae$ASEVN <- unname(as.numeric(adae$ASEVN))
adae$ARELN <- unname(as.numeric(adae$ARELN))

# ---------------- STEP 4: occurrence flags ----------------
adae <- adae[sas_order(adae$USUBJID, adae$AEBODSYS, adae$AEDECOD, adae$ASTDT), ]
n <- nrow(adae)
fU <- c(TRUE, adae$USUBJID[-1] != adae$USUBJID[-n])
fS <- fU | c(TRUE, adae$AEBODSYS[-1] != adae$AEBODSYS[-n])
fD <- fS | c(TRUE, adae$AEDECOD[-1] != adae$AEDECOD[-n])
adae$AOCCFL  <- ifelse(fU, "Y", "")
adae$AOCCSFL <- ifelse(fS, "Y", "")
adae$AOCCPFL <- ifelse(fD, "Y", "")

# ---------------- STEP 5: sequence ----------------
adae <- adae[sas_order(adae$STUDYID, adae$USUBJID, adae$ASTDT, adae$AESEQ), ]
adae$ASEQ <- ave(seq_len(nrow(adae)), adae$STUDYID, adae$USUBJID, FUN = seq_along)
adae$ASEQ <- as.numeric(adae$ASEQ)
rownames(adae) <- NULL

var_order <- c(ae_cols, setdiff(adsl_cols, c("STUDYID","USUBJID")),
               "ASTDT","AENDT","ASTDY","AENDY","TRTEMFL","AESER_FL","AOCCFL",
               "AOCCSFL","AOCCPFL","ASEV","ASEVN","AREL","ARELN","AESOC",
               "AEDECOD_UP","AEDUR","ASEQ")
adae <- adae[, var_order]

# ---------------- Output datasets ----------------
fmt_iso <- function(df) {
  for (v in names(df)) if (inherits(df[[v]], "Date")) df[[v]] <- format(df[[v]], "%Y-%m-%d")
  df
}
write.csv(fmt_iso(adae), file.path(out, "adae.csv"), row.names = FALSE, na = "")
write.csv(fmt_iso(adsl), file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ---------------- Listing: PROC CONTENTS varnum + PROC PRINT ----------------
lens <- c(STUDYID=12, USUBJID=20, AESEQ=8, AETERM=60, AEDECOD=60, AEBODSYS=60,
          AESTDTC=10, AEENDTC=10, AESEV=10, AESER=1, AEREL=20, AEACN=25, AEOUT=40,
          AETOXGR=2, SITEID=4, TRTP=40, TRTPN=8, AGE=8, AGEGR1=10, SEX=1, RACE=50,
          SAFFL=1, TRTSDT=8, TRTEDT=8, ASTDT=8, AENDT=8, ASTDY=8, AENDY=8,
          TRTEMFL=1, AESER_FL=1, AOCCFL=1, AOCCSFL=1, AOCCPFL=1, ASEV=10, ASEVN=8,
          AREL=20, ARELN=8, AESOC=60, AEDECOD_UP=60, AEDUR=8, ASEQ=8)
labs <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier',
          AESEQ='Sequence Number', ASEQ='Analysis Sequence Number',
          AETERM='Reported Term for the Adverse Event', AEDECOD='Dictionary-Derived Term',
          AEBODSYS='Body System or Organ Class', AESOC='Primary System Organ Class',
          ASTDT='Analysis Start Date', AENDT='Analysis End Date',
          ASTDY='Analysis Start Relative Day', AENDY='Analysis End Relative Day',
          ASEV='Analysis Severity/Intensity', ASEVN='Analysis Severity/Intensity (N)',
          AREL='Analysis Causality', ARELN='Analysis Causality (N)',
          TRTEMFL='Treatment Emergent Analysis Flag', AESER_FL='Serious Event Flag',
          AOCCFL='First Occurrence of Any AE Flag', AOCCSFL='First Occurrence Within SOC Flag',
          AOCCPFL='First Occurrence of Preferred Term Flag', AEDUR='AE Duration (Days)',
          TRTP='Planned Treatment', TRTPN='Planned Treatment (N)',
          TRTSDT='Date of First Exposure to Treatment', TRTEDT='Date of Last Exposure to Treatment',
          SAFFL='Safety Population Flag')
contents <- data.frame(
  Num = seq_along(var_order), Variable = var_order,
  Type = ifelse(sapply(adae, function(x) is.numeric(x) || inherits(x, "Date")), "Num", "Char"),
  Len = unname(lens[var_order]),
  Format = ifelse(var_order %in% c("TRTSDT","TRTEDT","ASTDT","AENDT"), "DATE9.", ""),
  Label = ifelse(is.na(labs[var_order]), "", unname(labs[var_order])),
  stringsAsFactors = FALSE)

d9 <- function(x) ifelse(is.na(x), ".", toupper(format(x, "%d%b%Y")))
prt <- head(adae, 10)
for (v in names(prt)) {
  if (inherits(prt[[v]], "Date")) prt[[v]] <- d9(prt[[v]])
  else if (is.numeric(prt[[v]])) prt[[v]] <- ifelse(is.na(prt[[v]]), ".", format(prt[[v]], trim = TRUE))
}
write.csv(prt, file.path(out, "P084.csv"), row.names = FALSE, na = "")
write.csv(contents, file.path(out, "P084_contents.csv"), row.names = FALSE, na = "")

sink(file.path(out, "P084.txt"))
cat("The CONTENTS Procedure\n\n")
cat("Data Set Name  WORK.ADAE\nObservations   ", nrow(adae), "\nVariables      ", ncol(adae), "\n\n")
cat("Variables in Creation Order\n\n")
print(contents, row.names = FALSE, right = FALSE)
cat("\n\nPROC PRINT (obs=10)\n\n")
cols <- names(prt)
chunks <- split(cols, ceiling(seq_along(cols) / 8))
for (ch in chunks) {
  print(prt[, ch, drop = FALSE], row.names = FALSE, right = FALSE)
  cat("\n")
}
sink()

cat("P084 done\n")
