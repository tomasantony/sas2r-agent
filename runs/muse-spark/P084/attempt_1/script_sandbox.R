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
  ifelse(is.na(x), "",
    paste0(sprintf("%02d", as.integer(format(x, "%d"))),
           toupper(format(x, "%b")),
           format(x, "%Y")))
}
iso_date <- function(x) {
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
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
# proc sort ae by STUDYID USUBJID; proc sort adsl by STUDYID USUBJID
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
# STEP 4: Derive occurrence flags
# proc sort by USUBJID AEBODSYS AEDECOD ASTDT
# =====================================================================
# SAS sorts with uppercase before lowercase; use C collation ordering
old_coll <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
adae <- adae %>% arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_coll), silent = TRUE))

# FIRST./LAST. processing: by USUBJID AEBODSYS AEDECOD
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
# STEP 5: Add sequence; proc sort by STUDYID USUBJID ASTDT AESEQ
# =====================================================================
adae <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ)
adae <- adae %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()
adae$ASEQ <- as.numeric(adae$ASEQ)

# SAS variable order: ae vars, then adsl keep extras, then derived
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
# P084.lst text rendering (PROC CONTENTS + PROC PRINT obs=10)
# =====================================================================
labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  AESEQ="Sequence Number", ASEQ="Analysis Sequence Number",
  AETERM="Reported Term for the Adverse Event", AEDECOD="Dictionary-Derived Term",
  AEBODSYS="Body System or Organ Class", AESOC="Primary System Organ Class",
  ASTDT="Analysis Start Date", AENDT="Analysis End Date",
  ASTDY="Analysis Start Relative Day", AENDY="Analysis End Relative Day",
  ASEV="Analysis Severity/Intensity", ASEVN="Analysis Severity/Intensity (N)",
  AREL="Analysis Causality", ARELN="Analysis Causality (N)",
  TRTEMFL="Treatment Emergent Analysis Flag", AESER_FL="Serious Event Flag",
  AOCCFL="First Occurrence of Any AE Flag", AOCCSFL="First Occurrence Within SOC Flag",
  AOCCPFL="First Occurrence of Preferred Term Flag", AEDUR="AE Duration (Days)",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
  TRTSDT="Date of First Exposure to Treatment", TRTEDT="Date of Last Exposure to Treatment",
  SAFFL="Safety Population Flag"
)
get_type_len <- function(df, v) {
  x <- df[[v]]
  if (inherits(x, "Date")) return(list(type="Num", len=8, format="DATE9."))
  if (is.numeric(x)) return(list(type="Num", len=8, format=""))
  ml <- max(nchar(as.character(x[!is.na(x)]), keepNA=FALSE), na.rm=TRUE)
  if (!is.finite(ml)) ml <- 1
  return(list(type="Char", len=ml, format=paste0("$", ml, ".")))
}
vars <- names(adae_out)
txt <- c()
txt <- c(txt, "CONTENTS OF ADAE DATASET", paste0("Observations: ", nrow(adae_out)),
  paste0("Variables: ", ncol(adae_out)), "",
  sprintf("%-4s %-12s %-5s %-5s %-12s %s", "Num", "Variable", "Type", "Len", "Format", "Label"),
  sprintf("%-4s %-12s %-5s %-5s %-12s %s", "---", "--------", "----", "---", "------", "-----"))
for (i in seq_along(vars)) {
  v <- vars[i]
  tl <- get_type_len(adae_out, v)
  lab <- ifelse(v %in% names(labels), labels[[v]], "")
  txt <- c(txt, sprintf("%-4d %-12s %-5s %-5d %-12s %s", i, v, tl$type, tl$len, tl$format, lab))
}
txt <- c(txt, "", "", "PRINT OF ADAE DATASET (OBS=10)", "")
# Build print lines: show all variables; format dates DATE9.
hdr <- paste(c("Obs", vars), collapse=" | ")
txt <- c(txt, hdr)
txt <- c(txt, paste(rep("-", nchar(hdr)), collapse=""))
for (r in seq_len(min(10, nrow(adae_out)))) {
  vals <- c(as.character(r))
  for (v in vars) {
    x <- adae_out[[v]][r]
    if (inherits(adae_out[[v]], "Date")) vals <- c(vals, sas_date9(adae_out[[v]][r]))
    else if (is.na(x)) vals <- c(vals, ".")
    else vals <- c(vals, as.character(x))
  }
  txt <- c(txt, paste(vals, collapse=" | "))
}
txt <- c(txt, "", "Notes:",
  paste0("N observations = ", nrow(adae_out)),
  paste0("N subjects with AE = ", length(unique(adae_out$USUBJID))),
  paste0("N treatment-emergent AEs (TRTEMFL=Y) = ", sum(adae_out$TRTEMFL=="Y", na.rm=TRUE)),
  paste0("N serious AEs (AESER_FL=Y) = ", sum(adae_out$AESER_FL=="Y", na.rm=TRUE)),
  paste0("Mean AEDUR = ", sprintf("%.2f", mean(adae_out$AEDUR, na.rm=TRUE))),
  paste0("Mean ASTDY = ", sprintf("%.2f", mean(adae_out$ASTDY, na.rm=TRUE))))

writeLines(txt, file.path(out, "P084.txt"))
# also write .lst copy for benchmark expecting P084.lst
writeLines(txt, file.path(out, "P084.lst"))
# CSV of table body (printed data, first 10 obs)
write.csv(adae_out[seq_len(min(10, nrow(adae_out))), ],
  file.path(out, "P084.csv"), row.names = FALSE, na = "")
