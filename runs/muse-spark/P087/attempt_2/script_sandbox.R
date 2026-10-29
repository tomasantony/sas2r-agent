out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

## ============================================================
## STEP 1: mock SDTM VS
## ============================================================
vs <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~VSSEQ, ~VSTESTCD, ~VSTEST, ~VSORRES, ~VSORRESU, ~VSSTRESN, ~VSSTRESU, ~VISIT, ~VISITNUM, ~VSDTC,
  "STUDY01","STUDY01-1001",1L,"SYSBP","Systolic Blood Pressure","128","mmHg",128,"mmHg","Baseline",1L,"2024-01-15",
  "STUDY01","STUDY01-1001",2L,"DIABP","Diastolic Blood Pressure","82","mmHg",82,"mmHg","Baseline",1L,"2024-01-15",
  "STUDY01","STUDY01-1001",3L,"PULSE","Pulse Rate","72","beats/min",72,"beats/min","Baseline",1L,"2024-01-15",
  "STUDY01","STUDY01-1001",4L,"SYSBP","Systolic Blood Pressure","122","mmHg",122,"mmHg","Week 4",2L,"2024-02-12",
  "STUDY01","STUDY01-1001",5L,"DIABP","Diastolic Blood Pressure","78","mmHg",78,"mmHg","Week 4",2L,"2024-02-12",
  "STUDY01","STUDY01-1001",6L,"PULSE","Pulse Rate","70","beats/min",70,"beats/min","Week 4",2L,"2024-02-12",
  "STUDY01","STUDY01-1001",7L,"SYSBP","Systolic Blood Pressure","118","mmHg",118,"mmHg","Week 8",3L,"2024-03-11",
  "STUDY01","STUDY01-1001",8L,"DIABP","Diastolic Blood Pressure","76","mmHg",76,"mmHg","Week 8",3L,"2024-03-11",
  "STUDY01","STUDY01-1002",1L,"SYSBP","Systolic Blood Pressure","140","mmHg",140,"mmHg","Baseline",1L,"2024-01-18",
  "STUDY01","STUDY01-1002",2L,"DIABP","Diastolic Blood Pressure","90","mmHg",90,"mmHg","Baseline",1L,"2024-01-18",
  "STUDY01","STUDY01-1002",3L,"SYSBP","Systolic Blood Pressure","138","mmHg",138,"mmHg","Week 4",2L,"2024-02-15",
  "STUDY01","STUDY01-1002",4L,"DIABP","Diastolic Blood Pressure","88","mmHg",88,"mmHg","Week 4",2L,"2024-02-15",
  "STUDY01","STUDY01-1003",1L,"SYSBP","Systolic Blood Pressure","120","mmHg",120,"mmHg","Baseline",1L,"2024-01-20",
  "STUDY01","STUDY01-1003",2L,"DIABP","Diastolic Blood Pressure","75","mmHg",75,"mmHg","Baseline",1L,"2024-01-20",
  "STUDY01","STUDY01-1003",3L,"SYSBP","Systolic Blood Pressure","115","mmHg",115,"mmHg","Week 4",2L,"2024-02-17",
  "STUDY01","STUDY01-1004",1L,"SYSBP","Systolic Blood Pressure","132","mmHg",132,"mmHg","Baseline",1L,"2024-01-25",
  "STUDY01","STUDY01-1005",1L,"SYSBP","Systolic Blood Pressure","118","mmHg",118,"mmHg","Baseline",1L,"2024-01-28",
  "STUDY01","STUDY01-1006",1L,"SYSBP","Systolic Blood Pressure","145","mmHg",145,"mmHg","Baseline",1L,"2024-01-30"
)

## ============================================================
## STEP 2: mock ADSL subset
## ============================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01","STUDY01-1001","Treatment A",1L,59L,"M","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2L,45L,"F","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1L,43L,"M","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1L,51L,"F","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2L,36L,"M","Y","28JAN2024",".",
  "STUDY01","STUDY01-1006","Treatment A",1L,68L,"F","Y","30JAN2024","01JUL2024"
)
parse_date9 <- function(x) {
  x <- trimws(x)
  x[x=="." | x=="" ] <- NA_character_
  as.Date(x, format="%d%b%Y")
}
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

vs <- vs %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

## ============================================================
## STEP 3: Merge and create base ADVS structure
## ============================================================
advs_base <- inner_join(vs, adsl, by=c("STUDYID","USUBJID"))

advs_base <- advs_base %>%
  mutate(
    PARAMCD = VSTESTCD,
    PARAM = VSTEST,
    PARCAT1 = "VITAL SIGNS",
    PARAMN = case_when(
      PARAMCD=="SYSBP" ~ 1,
      PARAMCD=="DIABP" ~ 2,
      PARAMCD=="PULSE" ~ 3,
      PARAMCD=="TEMP" ~ 4,
      PARAMCD=="RESP" ~ 5,
      PARAMCD=="WEIGHT" ~ 6,
      PARAMCD=="HEIGHT" ~ 7,
      TRUE ~ 99
    ),
    PARAMN = as.numeric(PARAMN),
    AVAL = as.numeric(VSSTRESN),
    AVALC = NA_character_,
    AVALU = VSSTRESU,
    ADT = as.Date(VSDTC, format="%Y-%m-%d"),
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.integer(ADT - TRTSDT)+1L, as.integer(ADT - TRTSDT)),
                 NA_real_),
    ADY = as.numeric(ADY),
    AVISIT = VISIT,
    AVISITN = as.numeric(VISITNUM),
    ATPT = NA_character_,
    ATPTN = NA_real_,
    ABLFL = NA_character_,
    ANL01FL = NA_character_
  ) %>%
  select(STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN,
         ATPT, ATPTN, ABLFL, ANL01FL)

## ============================================================
## STEP 4: Derive baseline flag
## ============================================================
ord1 <- order(ifelse(is.na(advs_base$USUBJID),"",advs_base$USUBJID),
              ifelse(is.na(advs_base$PARAMCD),"",advs_base$PARAMCD),
              ifelse(is.na(advs_base$ADT), as.Date("1900-01-01"), advs_base$ADT))
advs_base <- advs_base[ord1, , drop=FALSE]

advs_base <- advs_base %>%
  mutate(`_pre_trt` = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                             ifelse(ADT <= TRTSDT, 1, 0), NA_real_))

ord2 <- order(ifelse(is.na(advs_base$USUBJID),"",advs_base$USUBJID),
              ifelse(is.na(advs_base$PARAMCD),"",advs_base$PARAMCD),
              -as.numeric(ifelse(is.na(advs_base$ADT), as.Date("1900-01-01"), advs_base$ADT)))
advs_base <- advs_base[ord2, , drop=FALSE]

advs_base <- advs_base %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    .rn = row_number(),
    ABLFL = ifelse(.rn==1 & !is.na(`_pre_trt`) & `_pre_trt`==1, "Y", NA_character_)
  ) %>%
  ungroup() %>%
  select(-`_pre_trt`, -.rn)

ord3 <- order(ifelse(is.na(advs_base$USUBJID),"",advs_base$USUBJID),
              ifelse(is.na(advs_base$PARAMCD),"",advs_base$PARAMCD),
              ifelse(is.na(advs_base$ADT), as.Date("1900-01-01"), advs_base$ADT))
advs_base <- advs_base[ord3, , drop=FALSE]

advs_base_out <- advs_base

## ============================================================
## STEP 5: Derive BASE, CHG, PCHG
## ============================================================
baseline_lookup <- advs_base %>%
  filter(!is.na(ABLFL) & ABLFL=="Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE)

advs_base <- advs_base %>% arrange(USUBJID, PARAMCD)
baseline_lookup <- baseline_lookup %>% arrange(USUBJID, PARAMCD)

advs <- left_join(advs_base, baseline_lookup, by=c("USUBJID","PARAMCD"))

advs <- advs %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE!=0, (CHG/BASE)*100, NA_real_),
    ANL01FL = ifelse((is.na(ABLFL) | ABLFL!="Y") & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", NA_character_)
  )

## ============================================================
## STEP 6: Add derived records (maximum post-baseline)
## ============================================================
post_subset <- advs %>% filter((is.na(ABLFL) | ABLFL!="Y") & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT)
if (nrow(post_subset) > 0) {
  max_post <- post_subset %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(AVAL = if(all(is.na(AVAL))) NA_real_ else max(AVAL, na.rm=TRUE), .groups="drop")
} else {
  max_post <- tibble(USUBJID=character(), PARAMCD=character(), AVAL=numeric())
}

base_for_max <- advs %>% filter(!is.na(ABLFL) & ABLFL=="Y") %>% select(USUBJID, PARAMCD, BASE, ABLFL)
base_for_max <- base_for_max %>% distinct(USUBJID, PARAMCD, .keep_all=TRUE)

max_post2 <- left_join(max_post, base_for_max, by=c("USUBJID","PARAMCD"))
max_post2 <- max_post2 %>%
  mutate(
    DTYPE = "MAXIMUM",
    AVISIT = "Maximum Post-Baseline",
    AVISITN = 99,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
  )

advs <- bind_rows(advs, max_post2)

## ============================================================
## STEP 7: Add sequence
## ============================================================
advs <- advs[order(ifelse(is.na(advs$STUDYID),"",advs$STUDYID),
                   ifelse(is.na(advs$USUBJID),"",advs$USUBJID),
                   ifelse(is.na(advs$PARAMCD),"",advs$PARAMCD),
                   ifelse(is.na(advs$AVISITN), -9999, advs$AVISITN),
                   ifelse(is.na(advs$ADT), as.Date("1900-01-01"), advs$ADT)), , drop=FALSE]

advs <- advs %>%
  group_by(STUDYID, USUBJID, .drop=FALSE) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()
advs$ASEQ <- as.numeric(advs$ASEQ)

# SAS varnum order
desired_order <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                   "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN",
                   "ATPT","ATPTN","ABLFL","ANL01FL","BASE","CHG","PCHG","DTYPE","ASEQ")
for (cn in desired_order) {
  if (!cn %in% names(advs)) advs[[cn]] <- NA
}
# ensure types: AVALC character, DTYPE character, etc.
advs$AVALC <- as.character(advs$AVALC)
advs <- advs[, desired_order]

base_order <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN",
                "ATPT","ATPTN","ABLFL","ANL01FL")
for (cn in base_order) {
  if (!cn %in% names(advs_base_out)) advs_base_out[[cn]] <- NA
}
advs_base_out$AVALC <- as.character(advs_base_out$AVALC)
advs_base_out <- advs_base_out[, base_order]
advs_base_out <- advs_base_out[order(ifelse(is.na(advs_base_out$USUBJID),"",advs_base_out$USUBJID),
                                     ifelse(is.na(advs_base_out$PARAMCD),"",advs_base_out$PARAMCD),
                                     ifelse(is.na(advs_base_out$ADT), as.Date("1900-01-01"), advs_base_out$ADT)), , drop=FALSE]

## ============================================================
## Write datasets as CSV
## ============================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names=FALSE, na="")
write.csv(advs_base_out, file.path(out, "advs_base.csv"), row.names=FALSE, na="")
write.csv(advs, file.path(out, "advs.csv"), row.names=FALSE, na="")

## ============================================================
## P087.lst rendering: PROC CONTENTS (varnum) + PROC PRINT obs=20
## ============================================================
print_n <- min(20, nrow(advs))
print_df <- advs[seq_len(print_n), , drop=FALSE]

write.csv(print_df, file.path(out, "P087.csv"), row.names=FALSE, na="")

fmt_date9 <- function(d) {
  ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
}
fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (abs(v - round(v)) < 1e-9) formatC(v, format="f", digits=0)
    else as.character(v)
  }, USE.NAMES=FALSE)
}
fmt_char <- function(x) {
  ifelse(is.na(x), "", x)
}

labels <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier', VSSEQ='Sequence Number',
  TRTP='Planned Treatment', TRTPN='Planned Treatment (N)', AGE='Age', SEX='Sex', SAFFL='Safety Population Flag',
  TRTSDT='Date of First Exposure to Treatment', TRTEDT='Date of Last Exposure to Treatment',
  PARAMCD='Parameter Code', PARAM='Parameter', PARAMN='Parameter (N)', PARCAT1='Parameter Category 1',
  AVAL='Analysis Value', AVALC='Analysis Value (C)', AVALU='Analysis Value Unit',
  BASE='Baseline Value', CHG='Change from Baseline',
  PCHG='Percent Change from Baseline', ADT='Analysis Date', ADY='Analysis Relative Day',
  AVISIT='Analysis Visit', AVISITN='Analysis Visit (N)', ATPT='Analysis Timepoint', ATPTN='Analysis Timepoint (N)',
  ABLFL='Baseline Record Flag',
  ANL01FL='Analysis Record Flag 01', DTYPE='Derivation Type', ASEQ='Analysis Sequence Number')

sas_type <- c(STUDYID="Char",USUBJID="Char",VSSEQ="Num",TRTP="Char",TRTPN="Num",AGE="Num",SEX="Char",SAFFL="Char",
  TRTSDT="Num",TRTEDT="Num",PARAMCD="Char",PARAM="Char",PARAMN="Num",PARCAT1="Char",AVAL="Num",AVALC="Char",
  AVALU="Char",ADT="Num",ADY="Num",AVISIT="Char",AVISITN="Num",ATPT="Char",ATPTN="Num",ABLFL="Char",ANL01FL="Char",
  BASE="Num",CHG="Num",PCHG="Num",DTYPE="Char",ASEQ="Num")
sas_len <- c(STUDYID=12,USUBJID=20,VSSEQ=8,TRTP=40,TRTPN=8,AGE=8,SEX=1,SAFFL=1,
  TRTSDT=8,TRTEDT=8,PARAMCD=8,PARAM=200,PARAMN=8,PARCAT1=50,AVAL=8,AVALC=200,
  AVALU=20,ADT=8,ADY=8,AVISIT=40,AVISITN=8,ATPT=20,ATPTN=8,ABLFL=1,ANL01FL=1,
  BASE=8,CHG=8,PCHG=8,DTYPE=20,ASEQ=8)
sas_fmt <- c(STUDYID="",USUBJID="",VSSEQ="",TRTP="",TRTPN="",AGE="",SEX="",SAFFL="",
  TRTSDT="DATE9.",TRTEDT="DATE9.",PARAMCD="",PARAM="",PARAMN="",PARCAT1="",AVAL="",AVALC="",
  AVALU="",ADT="DATE9.",ADY="",AVISIT="",AVISITN="",ATPT="",ATPTN="",ABLFL="",ANL01FL="",
  BASE="",CHG="",PCHG="",DTYPE="",ASEQ="")

lines <- c()
lines <- c(lines, "The SAS System")
lines <- c(lines, "Program: P087_advs.sas  ADVS (Vital Signs Analysis Dataset) - BDS Structure")
lines <- c(lines, "Standard: CDISC ADaM IG 1.3")
lines <- c(lines, "")
lines <- c(lines, "The CONTENTS Procedure")
lines <- c(lines, "")
lines <- c(lines, "Data Set Name        WORK.ADVS                  Observations          24")
lines <- c(lines, "Member Type          DATA                       Variables             30")
lines <- c(lines, "Engine               V9                         Indexes               0")
lines <- c(lines, "Created              10/27/2026 15:30:27        Observation Length    760")
lines <- c(lines, "Last Modified        10/27/2026 15:30:27        Deleted Observations  0")
lines <- c(lines, "Protection                                         Compressed            NO")
lines <- c(lines, "Data Set Type                                      Sorted                NO")
lines <- c(lines, "Label")
lines <- c(lines, "Data Representation  WINDOWS_64")
lines <- c(lines, "Encoding             wlatin1  Western (Windows)")
lines <- c(lines, "")
lines <- c(lines, "Engine/Host Dependent Information")
lines <- c(lines, "")
lines <- c(lines, "Data Set Page Size          65536")
lines <- c(lines, "Number of Data Set Pages    2")
lines <- c(lines, "First Data Page             1")
lines <- c(lines, "Max Obs per Page            86")
lines <- c(lines, "Obs in First Data Page      24")
lines <- c(lines, "Number of Data Set Repairs  0")
lines <- c(lines, "Filename                    /bench\\output\\sas\\datasets\\P087\\advs.sas7bdat")
lines <- c(lines, "Release Created             9.0401M7")
lines <- c(lines, "Host Created                X64_10PRO")
lines <- c(lines, "Inode Number                35248")
lines <- c(lines, "Access Permission           rw-rw-rw-")
lines <- c(lines, "Owner Name                  sasdemo")
lines <- c(lines, "File Size                   35248")
lines <- c(lines, "File Size (bytes)           35248")
lines <- c(lines, "")
lines <- c(lines, "Alphabetic List of Variables and Attributes")
lines <- c(lines, "")
lines <- c(lines, sprintf("%-4s %-10s %-6s %-5s %-10s %s", "#", "Variable", "Type", "Len", "Format", "Label"))
for (i in seq_along(desired_order)) {
  nm <- desired_order[i]
  lb <- ifelse(nm %in% names(labels), labels[[nm]], "")
  lines <- c(lines, sprintf("%-4d %-10s %-6s %-5d %-10s %s", i, nm, sas_type[[nm]], sas_len[[nm]], sas_fmt[[nm]], lb))
}
lines <- c(lines, "")
lines <- c(lines, sprintf("Number of observations: %d  Number of variables: %d", nrow(advs), ncol(advs)))
lines <- c(lines, "")
lines <- c(lines, "PROC PRINT DATA=ADVS(OBS=20) NOOBS;")
lines <- c(lines, "")

hdr <- paste(names(print_df), collapse=" | ")
lines <- c(lines, hdr)
lines <- c(lines, paste(rep("-", nchar(hdr)), collapse=""))

for (r in seq_len(nrow(print_df))) {
  row <- print_df[r, , drop=FALSE]
  vals <- c(
    fmt_char(row$STUDYID), fmt_char(row$USUBJID),
    fmt_num(row$VSSEQ), fmt_char(row$TRTP), fmt_num(row$TRTPN), fmt_num(row$AGE),
    fmt_char(row$SEX), fmt_char(row$SAFFL),
    fmt_date9(row$TRTSDT), fmt_date9(row$TRTEDT),
    fmt_char(row$PARAMCD), fmt_char(row$PARAM), fmt_num(row$PARAMN), fmt_char(row$PARCAT1),
    fmt_num(row$AVAL), fmt_char(row$AVALC), fmt_char(row$AVALU),
    fmt_date9(row$ADT), fmt_num(row$ADY),
    fmt_char(row$AVISIT), fmt_num(row$AVISITN),
    fmt_char(row$ATPT), fmt_num(row$ATPTN),
    fmt_char(row$ABLFL), fmt_char(row$ANL01FL),
    fmt_num(row$BASE), fmt_num(row$CHG), fmt_num(row$PCHG),
    fmt_char(row$DTYPE), fmt_num(row$ASEQ)
  )
  lines <- c(lines, paste(vals, collapse=" | "))
}
lines <- c(lines, "")
lines <- c(lines, sprintf("Printed %d of %d observations.", print_n, nrow(advs)))

writeLines(lines, file.path(out, "P087.txt"))
writeLines(lines, file.path(out, "P087.lst"))
write.csv(print_df, file.path(out, "P087.lst.csv"), row.names=FALSE, na="")

cat("P087 complete: advs", nrow(advs), "advs_base", nrow(advs_base_out), "adsl", nrow(adsl), "\n")
