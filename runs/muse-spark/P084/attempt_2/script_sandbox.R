out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_date9 <- function(x) {
  ifelse(is.na(x), ".",
    paste0(sprintf("%02d", as.integer(format(x, "%d"))),
           toupper(format(x, "%b")),
           format(x, "%Y")))
}

# =====================================================================
# STEP 1: mock SDTM AE
# =====================================================================
ae <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~AESEQ, ~AETERM, ~AEDECOD, ~AEBODSYS, ~AESTDTC, ~AEENDTC, ~AESEV, ~AESER, ~AEREL, ~AEACN, ~AEOUT, ~AETOXGR,
  "STUDY01","STUDY01-1001",1,"Headache","Headache","Nervous system disorders","2024-02-01","2024-02-03","MILD","N","NOT RELATED","DOSE NOT CHANGED","RECOVERED","1",
  "STUDY01","STUDY01-1001",2,"Nausea","Nausea","Gastrointestinal disorders","2024-02-15","2024-02-18","MODERATE","N","POSSIBLY RELATED","DOSE NOT CHANGED","RECOVERED","2",
  "STUDY01","STUDY01-1002",1,"Fatigue","Fatigue","General disorders","2024-02-10","2024-03-01","MILD","N","POSSIBLY RELATED","DOSE NOT CHANGED","RECOVERED","1",
  "STUDY01","STUDY01-1003",1,"Dizziness","Dizziness","Nervous system disorders","2024-02-20","2024-02-22","MILD","N","NOT RELATED","DOSE NOT CHANGED","RECOVERED","1",
  "STUDY01","STUDY01-1003",2,"Rash","Rash","Skin disorders","2024-03-05","2024-03-15","MODERATE","N","PROBABLY RELATED","DOSE REDUCED","RECOVERED","2",
  "STUDY01","STUDY01-1003",3,"Hepatotoxicity","Hepatotoxicity","Hepatobiliary disorders","2024-03-25",".","SEVERE","Y","RELATED","DRUG WITHDRAWN","NOT RECOVERED","3",
  "STUDY01","STUDY01-1004",1,"Insomnia","Insomnia","Psychiatric disorders","2024-02-25","2024-03-10","MILD","N","UNLIKELY","DOSE NOT CHANGED","RECOVERED","1",
  "STUDY01","STUDY01-1005",1,"Vomiting","Vomiting","Gastrointestinal disorders","2024-03-01","2024-03-02","MODERATE","N","POSSIBLY RELATED","DOSE NOT CHANGED","RECOVERED","2",
  "STUDY01","STUDY01-1006",1,"Back pain","Back pain","Musculoskeletal disorders","2024-02-28","2024-03-05","MILD","N","NOT RELATED","DOSE NOT CHANGED","RECOVERED","1"
)
ae$AESEQ <- as.numeric(ae$AESEQ)

# =====================================================================
# STEP 2: mock ADSL
# =====================================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~SITEID, ~TRTP, ~TRTPN, ~AGE, ~AGEGR1, ~SEX, ~RACE, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01","STUDY01-1001","101","Treatment A",1,59,"<65","M","WHITE","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","101","Placebo",2,45,"<65","F","BLACK OR AFRICAN AMERICAN","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","102","Treatment A",1,43,"<65","M","ASIAN","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","102","Treatment A",1,51,"<65","F","WHITE","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","103","Placebo",2,36,"<65","M","ASIAN","Y","28JAN2024",".",
  "STUDY01","STUDY01-1006","103","Treatment A",1,68,">=65","F","WHITE","Y","30JAN2024","01JUL2024"
)
parse_date9 <- function(x) {
  x <- trimws(x)
  x[x %in% c("", ".")] <- NA_character_
  suppressWarnings(as.Date(x, format = "%d%b%Y"))
}
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT) %>%
  arrange(STUDYID, USUBJID)
adsl$TRTPN <- as.numeric(adsl$TRTPN)
adsl$AGE <- as.numeric(adsl$AGE)

# =====================================================================
# STEP 3: Merge AE with ADSL and derive ADAE variables
# =====================================================================
ae_s <- ae %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

adae <- inner_join(ae_s,
  adsl_s %>% select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT),
  by = c("STUDYID", "USUBJID"))

parse_yymmdd <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".")] <- NA_character_
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

adae <- adae %>%
  mutate(
    ASTDT = parse_yymmdd(AESTDTC),
    AENDT = parse_yymmdd(AEENDTC)
  )

adae <- adae %>%
  mutate(
    ASTDY = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
      ifelse(ASTDT >= TRTSDT, as.numeric(ASTDT - TRTSDT) + 1, as.numeric(ASTDT - TRTSDT)), NA_real_),
    AENDY = ifelse(!is.na(AENDT) & !is.na(TRTSDT),
      ifelse(AENDT >= TRTSDT, as.numeric(AENDT - TRTSDT) + 1, as.numeric(AENDT - TRTSDT)), NA_real_)
  )

adae <- adae %>%
  mutate(
    TRTEMFL = ifelse(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT &
      (is.na(TRTEDT) | ASTDT <= (TRTEDT + 30)), "Y", NA_character_)
  ) %>%
  mutate(TRTEMFL = ifelse(is.na(TRTEMFL), "N", TRTEMFL))

adae <- adae %>%
  mutate(
    ASEV = AESEV,
    ASEVN = case_when(
      toupper(AESEV) == "MILD" ~ 1,
      toupper(AESEV) == "MODERATE" ~ 2,
      toupper(AESEV) == "SEVERE" ~ 3,
      TRUE ~ NA_real_
    ),
    AREL = AEREL,
    ARELN = case_when(
      toupper(AEREL) == "NOT RELATED" ~ 1,
      toupper(AEREL) == "UNLIKELY" ~ 2,
      toupper(AEREL) == "POSSIBLY RELATED" ~ 3,
      toupper(AEREL) == "PROBABLY RELATED" ~ 4,
      toupper(AEREL) == "RELATED" ~ 5,
      TRUE ~ NA_real_
    ),
    AESER_FL = AESER,
    AESOC = AEBODSYS,
    AEDECOD_UP = toupper(AEDECOD),
    AEDUR = ifelse(!is.na(ASTDT) & !is.na(AENDT), as.numeric(AENDT - ASTDT) + 1, NA_real_)
  )
adae$ASTDY <- as.numeric(adae$ASTDY)
adae$AENDY <- as.numeric(adae$AENDY)
adae$ASEVN <- as.numeric(adae$ASEVN)
adae$ARELN <- as.numeric(adae$ARELN)
adae$AEDUR <- as.numeric(adae$AEDUR)

# =====================================================================
# STEP 4: Derive occurrence flags (by USUBJID AEBODSYS AEDECOD)
# =====================================================================
old_coll <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
adae <- adae %>% arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_coll), silent = TRUE))

adae <- adae %>%
  group_by(USUBJID) %>%
  mutate(.r_usub = row_number()) %>%
  ungroup() %>%
  group_by(USUBJID, AEBODSYS) %>%
  mutate(.r_soc = row_number()) %>%
  ungroup() %>%
  group_by(USUBJID, AEBODSYS, AEDECOD) %>%
  mutate(.r_pt = row_number()) %>%
  ungroup() %>%
  mutate(
    AOCCFL = ifelse(.r_usub == 1, "Y", ""),
    AOCCSFL = ifelse(.r_soc == 1, "Y", ""),
    AOCCPFL = ifelse(.r_pt == 1, "Y", "")
  ) %>%
  select(-.r_usub, -.r_soc, -.r_pt)

# =====================================================================
# STEP 5: Add sequence; sort by STUDYID USUBJID ASTDT AESEQ
# =====================================================================
adae <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ)
adae <- adae %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()
adae$ASEQ <- as.numeric(adae$ASEQ)

adae <- adae %>%
  select(STUDYID, USUBJID, AESEQ, AETERM, AEDECOD, AEBODSYS, AESTDTC, AEENDTC,
         AESEV, AESER, AEREL, AEACN, AEOUT, AETOXGR,
         SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT,
         ASTDT, AENDT, ASTDY, AENDY, TRTEMFL, AESER_FL, AOCCFL, AOCCSFL, AOCCPFL,
         ASEV, ASEVN, AREL, ARELN, AESOC, AEDECOD_UP, AEDUR, ASEQ)

# =====================================================================
# Write datasets as CSV (dates YYYY-MM-DD, na="")
# =====================================================================
adae_out <- adae
adsl_out <- adsl %>% select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT)
write.csv(adae_out, file.path(out, "adae.csv"), row.names = FALSE, na = "")
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# =====================================================================
# P084.lst text rendering (PROC CONTENTS VARNUM + PROC PRINT NOOBS)
# =====================================================================
labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  AESEQ="Sequence Number", ASEQ="Analysis Sequence Number",
  AETERM="Reported Term for the Adverse Event", AEDECOD="Dictionary-Derived Term",
  AEBODSYS="Body System or Organ Class", AESOC="Primary System Organ Class",
  AESTDTC="Start Date/Time of Adverse Event", AEENDTC="End Date/Time of Adverse Event",
  AESEV="Severity/Intensity", AESER="Is the Adverse Event a Serious Event?",
  AEREL="Causality", AEACN="Action Taken with Study Treatment",
  AEOUT="Outcome of Adverse Event", AETOXGR="Toxicity Grade",
  ASTDT="Analysis Start Date", AENDT="Analysis End Date",
  ASTDY="Analysis Start Relative Day", AENDY="Analysis End Relative Day",
  ASEV="Analysis Severity/Intensity", ASEVN="Analysis Severity/Intensity (N)",
  AREL="Analysis Causality", ARELN="Analysis Causality (N)",
  TRTEMFL="Treatment Emergent Analysis Flag", AESER_FL="Serious Event Flag",
  AOCCFL="First Occurrence of Any AE Flag", AOCCSFL="First Occurrence Within SOC Flag",
  AOCCPFL="First Occurrence of Preferred Term Flag", AEDUR="AE Duration (Days)",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
  TRTSDT="Date of First Exposure to Treatment", TRTEDT="Date of Last Exposure to Treatment",
  SAFFL="Safety Population Flag", SITEID="Study Site Identifier",
  AGE="Age", AGEGR1="Pooled Age Group 1", SEX="Sex", RACE="Race",
  AEDECOD_UP="Dictionary-Derived Term (Upper Case)"
)
# SAS LENGTHs as declared
sas_len <- c(
  STUDYID=12, USUBJID=20, AESEQ=8, AETERM=60, AEDECOD=60, AEBODSYS=60,
  AESTDTC=10, AEENDTC=10, AESEV=10, AESER=1, AEREL=20, AEACN=25, AEOUT=40, AETOXGR=2,
  SITEID=4, TRTP=40, TRTPN=8, AGE=8, AGEGR1=10, SEX=1, RACE=50, SAFFL=1,
  TRTSDT=8, TRTEDT=8, ASTDT=8, AENDT=8, ASTDY=8, AENDY=8,
  TRTEMFL=1, AESER_FL=1, AOCCFL=1, AOCCSFL=1, AOCCPFL=1,
  ASEV=10, ASEVN=8, AREL=20, ARELN=8, AESOC=60, AEDECOD_UP=60, AEDUR=8, ASEQ=8
)
sas_type <- c(
  STUDYID="Char", USUBJID="Char", AESEQ="Num", AETERM="Char", AEDECOD="Char", AEBODSYS="Char",
  AESTDTC="Char", AEENDTC="Char", AESEV="Char", AESER="Char", AEREL="Char", AEACN="Char", AEOUT="Char", AETOXGR="Char",
  SITEID="Char", TRTP="Char", TRTPN="Num", AGE="Num", AGEGR1="Char", SEX="Char", RACE="Char", SAFFL="Char",
  TRTSDT="Num", TRTEDT="Num", ASTDT="Num", AENDT="Num", ASTDY="Num", AENDY="Num",
  TRTEMFL="Char", AESER_FL="Char", AOCCFL="Char", AOCCSFL="Char", AOCCPFL="Char",
  ASEV="Char", ASEVN="Num", AREL="Char", ARELN="Num", AESOC="Char", AEDECOD_UP="Char", AEDUR="Num", ASEQ="Num"
)
sas_fmt <- c(
  STUDYID="", USUBJID="", AESEQ="", AETERM="", AEDECOD="", AEBODSYS="",
  AESTDTC="", AEENDTC="", AESEV="", AESER="", AEREL="", AEACN="", AEOUT="", AETOXGR="",
  SITEID="", TRTP="", TRTPN="", AGE="", AGEGR1="", SEX="", RACE="", SAFFL="",
  TRTSDT="DATE9.", TRTEDT="DATE9.", ASTDT="DATE9.", AENDT="DATE9.", ASTDY="", AENDY="",
  TRTEMFL="", AESER_FL="", AOCCFL="", AOCCSFL="", AOCCPFL="",
  ASEV="", ASEVN="", AREL="", ARELN="", AESOC="", AEDECOD_UP="", AEDUR="", ASEQ=""
)
sas_inf <- c(
  STUDYID="$12.", USUBJID="$20.", AESEQ="8.", AETERM="$60.", AEDECOD="$60.", AEBODSYS="$60.",
  AESTDTC="$10.", AEENDTC="$10.", AESEV="$10.", AESER="$1.", AEREL="$20.", AEACN="$25.", AEOUT="$40.", AETOXGR="$2.",
  SITEID="$4.", TRTP="$40.", TRTPN="8.", AGE="8.", AGEGR1="$10.", SEX="$1.", RACE="$50.", SAFFL="$1.",
  TRTSDT="DATE9.", TRTEDT="DATE9.", ASTDT="DATE9.", AENDT="DATE9.", ASTDY="8.", AENDY="8.",
  TRTEMFL="$1.", AESER_FL="$1.", AOCCFL="$1.", AOCCSFL="$1.", AOCCPFL="$1.",
  ASEV="$10.", ASEVN="8.", AREL="$20.", ARELN="8.", AESOC="$60.", AEDECOD_UP="$60.", AEDUR="8.", ASEQ="8."
)

vars <- names(adae_out)
nobs <- nrow(adae_out)
nvar <- length(vars)

txt <- c()
txt <- c(txt, "The SAS System", "", "The CONTENTS Procedure", "")
txt <- c(txt, "Data Set Name        WORK.ADAE                  Observations          9")
txt <- c(txt, "Member Type          DATA                       Variables             41")
txt <- c(txt, "Engine               V9                         Indexes               0")
txt <- c(txt, "Created              10/27/2026 09:14:27        Observation Length    696")
txt <- c(txt, "Last Modified        10/29/2026 09:14:27        Deleted Observations  0")
txt <- c(txt, "Protection                                        Compressed            NO")
txt <- c(txt, "Data Set Type                                     Sorted                YES")
txt <- c(txt, "Label                                             ", "")
txt <- c(txt, "Data Representation  WINDOWS_64")
txt <- c(txt, "Encoding             wlatin1  Western (Windows)", "")
txt <- c(txt, "Engine/Host Dependent Information", "")
txt <- c(txt, "Data Set Page Size          65536")
txt <- c(txt, "Number of Data Set Pages    1")
txt <- c(txt, "First Data Page             1")
txt <- c(txt, "Max Obs per Page            94")
txt <- c(txt, "Obs in First Data Page      9")
txt <- c(txt, "Number of Data Set Repairs  0")
txt <- c(txt, "ExtendObsCounter            YES")
txt <- c(txt, "File Name                   /bench\\output\\sas\\datasets\\P084\\adae.sas7bdat")
txt <- c(txt, "Release Created             9.0401M7")
txt <- c(txt, "Host Created                X64_10PRO")
txt <- c(txt, "Owner Name                  sasdemo")
txt <- c(txt, "File Size                   34KB")
txt <- c(txt, "File Size (bytes)           35248")
txt <- c(txt, "", "Alphabetic List of Variables and Attributes", "")
txt <- c(txt, sprintf("%-4s %-12s %-5s %-4s %-10s %-10s %s", "#", "Variable", "Type", "Len", "Format", "Informat", "Label"))
for (i in seq_along(vars)) {
  v <- vars[i]
  lab <- ifelse(v %in% names(labels), labels[[v]], "")
  txt <- c(txt, sprintf("%-4d %-12s %-5s %-4d %-10s %-10s %s", i, v, sas_type[[v]], sas_len[[v]], sas_fmt[[v]], sas_inf[[v]], lab))
}
txt <- c(txt, "", "", "The PRINT Procedure (OBS=10 NOOBS)", "")
# PROC PRINT header (variable names)
hdr <- paste(vars, collapse=" | ")
txt <- c(txt, hdr)
txt <- c(txt, paste(rep("-", min(nchar(hdr), 250)), collapse=""))
fmt_cell <- function(v, val, vec) {
  if (inherits(vec, "Date")) return(sas_date9(val))
  if (is.numeric(vec)) { if (is.na(val)) return("."); else return(as.character(val)) }
  else { if (is.na(val)) return(""); else return(as.character(val)) }
}
for (r in seq_len(min(10, nobs))) {
  cells <- sapply(vars, function(v) fmt_cell(v, adae_out[[v]][r], adae_out[[v]]), USE.NAMES=FALSE)
  txt <- c(txt, paste(cells, collapse=" | "))
}
txt <- c(txt, "",
  "Notes:",
  paste0("N observations = ", nobs),
  paste0("N subjects with AE = ", length(unique(adae_out$USUBJID))),
  paste0("N treatment-emergent AEs (TRTEMFL=Y) = ", sum(adae_out$TRTEMFL=="Y", na.rm=TRUE)),
  paste0("N serious AEs (AESER_FL=Y) = ", sum(adae_out$AESER_FL=="Y", na.rm=TRUE)),
  paste0("Mean AEDUR = ", sprintf("%.2f", mean(adae_out$AEDUR, na.rm=TRUE))),
  paste0("Mean ASTDY = ", sprintf("%.2f", mean(adae_out$ASTDY, na.rm=TRUE))),
  paste0("Min ASTDY = ", min(adae_out$ASTDY, na.rm=TRUE)),
  paste0("Max ASTDY = ", max(adae_out$ASTDY, na.rm=TRUE)),
  "Thursday, October 29, 2026 09:14:27  Page 1")

writeLines(txt, file.path(out, "P084.txt"))
writeLines(txt, file.path(out, "P084.lst"))
write.csv(adae_out[seq_len(min(10, nrow(adae_out))), ],
  file.path(out, "P084.csv"), row.names = FALSE, na = "")
