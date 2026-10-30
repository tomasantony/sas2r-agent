# P088_adeff.sas -> R translation
# Outputs: adsl.csv, adeff_resp.csv, adeff_tumor.csv, adeff.csv, P088.lst text+csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helpers
`%||%` <- function(a,b) ifelse(is.na(a) | a=="", b, a)
parse_yymmdd10 <- function(x) as.Date(x, format="%Y-%m-%d")
parse_date9 <- function(x) {
  x <- trimws(x)
  x[x=="" | x=="."] <- NA
  as.Date(toupper(x), format="%d%b%Y")
}
fmt_date <- function(d) format(d, "%Y-%m-%d")
round_half_up <- function(x, digits=0){
  mult <- 10^digits
  sign(x) * floor(abs(x)*mult + 0.5) / mult
}

library(dplyr)
library(tidyr)
library(readr)

#==========================================================================
# STEP 1: mock RS
#==========================================================================
rs <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~RSSEQ, ~RSTESTCD, ~RSTEST, ~RSSTRESC, ~RSSTRESN, ~VISIT, ~VISITNUM, ~RSDTC,
  "STUDY01","STUDY01-1001",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",2,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Week 8",3,"2024-03-11",
  "STUDY01","STUDY01-1001",3,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA,"Week 16",4,"2024-05-06",
  "STUDY01","STUDY01-1001",4,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA,"Week 24",5,"2024-07-01",
  "STUDY01","STUDY01-1002",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Baseline",1,"2024-01-18",
  "STUDY01","STUDY01-1002",2,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Week 8",3,"2024-03-14",
  "STUDY01","STUDY01-1002",3,"OVRLRESP","Overall Response","PROGRESSIVE DISEASE",NA,"Week 16",4,"2024-05-09",
  "STUDY01","STUDY01-1003",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Baseline",1,"2024-01-20",
  "STUDY01","STUDY01-1003",2,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA,"Week 8",3,"2024-03-16",
  "STUDY01","STUDY01-1003",3,"OVRLRESP","Overall Response","COMPLETE RESPONSE",NA,"Week 16",4,"2024-05-11",
  "STUDY01","STUDY01-1004",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Baseline",1,"2024-01-25",
  "STUDY01","STUDY01-1004",2,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Week 8",3,"2024-03-21",
  "STUDY01","STUDY01-1005",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Baseline",1,"2024-01-28",
  "STUDY01","STUDY01-1005",2,"OVRLRESP","Overall Response","PROGRESSIVE DISEASE",NA,"Week 8",3,"2024-03-24",
  "STUDY01","STUDY01-1006",1,"OVRLRESP","Overall Response","STABLE DISEASE",NA,"Baseline",1,"2024-01-30",
  "STUDY01","STUDY01-1006",2,"OVRLRESP","Overall Response","PARTIAL RESPONSE",NA,"Week 8",3,"2024-03-26"
)

tu <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TUSEQ, ~TUTESTCD, ~TUTEST, ~TUORRES, ~TUSTRESC, ~TUSTRESN, ~TUSTRESU, ~TULOC, ~VISIT, ~VISITNUM, ~TUDTC,
  "STUDY01","STUDY01-1001",1,"LDIAM","Longest Diameter","45","45",45,"mm","LIVER","Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",2,"LDIAM","Longest Diameter","42","42",42,"mm","LIVER","Week 8",3,"2024-03-11",
  "STUDY01","STUDY01-1001",3,"LDIAM","Longest Diameter","35","35",35,"mm","LIVER","Week 16",4,"2024-05-06",
  "STUDY01","STUDY01-1001",4,"LDIAM","Longest Diameter","30","30",30,"mm","LIVER","Week 24",5,"2024-07-01",
  "STUDY01","STUDY01-1002",1,"LDIAM","Longest Diameter","38","38",38,"mm","LUNG","Baseline",1,"2024-01-18",
  "STUDY01","STUDY01-1002",2,"LDIAM","Longest Diameter","40","40",40,"mm","LUNG","Week 8",3,"2024-03-14",
  "STUDY01","STUDY01-1002",3,"LDIAM","Longest Diameter","52","52",52,"mm","LUNG","Week 16",4,"2024-05-09",
  "STUDY01","STUDY01-1003",1,"LDIAM","Longest Diameter","55","55",55,"mm","LYMPH NODE","Baseline",1,"2024-01-20",
  "STUDY01","STUDY01-1003",2,"LDIAM","Longest Diameter","40","40",40,"mm","LYMPH NODE","Week 8",3,"2024-03-16",
  "STUDY01","STUDY01-1003",3,"LDIAM","Longest Diameter","0","0",0,"mm","LYMPH NODE","Week 16",4,"2024-05-11",
  "STUDY01","STUDY01-1004",1,"LDIAM","Longest Diameter","32","32",32,"mm","LIVER","Baseline",1,"2024-01-25",
  "STUDY01","STUDY01-1004",2,"LDIAM","Longest Diameter","30","30",30,"mm","LIVER","Week 8",3,"2024-03-21",
  "STUDY01","STUDY01-1005",1,"LDIAM","Longest Diameter","28","28",28,"mm","LUNG","Baseline",1,"2024-01-28",
  "STUDY01","STUDY01-1005",2,"LDIAM","Longest Diameter","42","42",42,"mm","LUNG","Week 8",3,"2024-03-24",
  "STUDY01","STUDY01-1006",1,"LDIAM","Longest Diameter","48","48",48,"mm","LIVER","Baseline",1,"2024-01-30",
  "STUDY01","STUDY01-1006",2,"LDIAM","Longest Diameter","36","36",36,"mm","LIVER","Week 8",3,"2024-03-26"
)

#==========================================================================
# STEP 2: mock ADSL
#==========================================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~EFFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01","STUDY01-1001","Treatment A",1,59,"M","Y","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2,45,"F","Y","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1,43,"M","Y","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1,51,"F","Y","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2,36,"M","Y","Y","28JAN2024",".",
  "STUDY01","STUDY01-1006","Treatment A",1,68,"F","Y","Y","30JAN2024","01JUL2024"
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, EFFFL, TRTSDT, TRTEDT)

#==========================================================================
# STEP 3: tumor response parameter OVRLRESP
#==========================================================================
rs <- rs %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

adeff_resp <- rs %>%
  filter(RSTESTCD=="OVRLRESP") %>%
  inner_join(adsl_s, by=c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "OVRLRESP",
    PARAM = "Overall Response",
    PARAMN = 1,
    AVALC = RSSTRESC,
    AVAL = case_when(
      toupper(RSSTRESC)=="COMPLETE RESPONSE" ~ 1,
      toupper(RSSTRESC)=="PARTIAL RESPONSE" ~ 2,
      toupper(RSSTRESC)=="STABLE DISEASE" ~ 3,
      toupper(RSSTRESC)=="PROGRESSIVE DISEASE" ~ 4,
      toupper(RSSTRESC)=="NOT EVALUABLE" ~ 5,
      TRUE ~ NA_real_
    ),
    ADT = parse_yymmdd10(RSDTC),
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.numeric(ADT - TRTSDT) + 1, as.numeric(ADT - TRTSDT)),
                 NA_real_),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT=="Baseline","Y",""),
    ANL01FL = ifelse(ABLFL!="Y" & !is.na(AVAL),"Y","")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)

#==========================================================================
# STEP 4: tumor size parameters
#==========================================================================
tu <- tu %>% arrange(STUDYID, USUBJID)
adeff_tumor <- tu %>%
  filter(TUTESTCD=="LDIAM") %>%
  inner_join(adsl_s, by=c("STUDYID","USUBJID")) %>%
  mutate(
    PARAMCD = "SUMDIAM",
    PARAM = "Sum of Longest Diameters",
    PARAMN = 2,
    AVAL = TUSTRESN,
    AVALU = TUSTRESU,
    AVALC = NA_character_,
    ADT = parse_yymmdd10(TUDTC),
    ADY = ifelse(!is.na(ADT) & !is.na(TRTSDT),
                 ifelse(ADT >= TRTSDT, as.numeric(ADT - TRTSDT) + 1, as.numeric(ADT - TRTSDT)),
                 NA_real_),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ABLFL = ifelse(AVISIT=="Baseline","Y",""),
    ANL01FL = ifelse(ABLFL!="Y" & !is.na(AVAL),"Y","")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL)

#==========================================================================
# STEP 5: Derive baseline and change from baseline for tumor
#==========================================================================
base_tumor <- adeff_tumor %>%
  filter(ABLFL=="Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE)

adeff_tumor <- adeff_tumor %>%
  arrange(USUBJID, PARAMCD) %>%
  left_join(base_tumor, by=c("USUBJID","PARAMCD")) %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE > 0, (AVAL - BASE)/BASE*100, NA_real_)
  )

#==========================================================================
# STEP 6: Derive best overall response (BOR)
#==========================================================================
resp_sorted <- adeff_resp %>%
  filter(ABLFL != "Y") %>%
  arrange(USUBJID, AVAL)

bor <- resp_sorted %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    PARAMCD = "BOR",
    PARAM = "Best Overall Response",
    PARAMN = 3,
    AVISIT = "Best Response",
    AVISITN = 99,
    ABLFL = "",
    ANL01FL = "Y"
  )

#==========================================================================
# STEP 7: Derive responder flag - keep RESPFL as in SAS
#==========================================================================
respfl <- bor %>%
  mutate(
    RESPFL = ifelse(AVAL %in% c(1, 2), "Y", "N"),
    PARAMCD = "RESPFL",
    PARAM = "Objective Response Flag",
    PARAMN = 4,
    AVAL = ifelse(RESPFL == "Y", 1, 0),
    AVALC = RESPFL
  )
# do NOT drop RESPFL - SAS retains it

#==========================================================================
# STEP 8: Derive minimum tumor size (nadir)
#==========================================================================
nadir_min <- adeff_tumor %>%
  filter(ABLFL != "Y") %>%
  group_by(USUBJID) %>%
  summarise(AVAL = min(AVAL, na.rm=TRUE), .groups="drop")

base_info <- adeff_tumor %>%
  filter(ABLFL=="Y") %>%
  select(USUBJID, STUDYID, TRTP, TRTPN, SAFFL, EFFFL, TRTSDT, TRTEDT, AGE, SEX, BASE, ABLFL)

nadir <- nadir_min %>%
  left_join(base_info, by="USUBJID") %>%
  mutate(
    PARAMCD = "NADIRTUM",
    PARAM = "Nadir Tumor Size",
    PARAMN = 5,
    AVALU = "mm",
    AVISIT = "Nadir",
    AVISITN = 98,
    ABLFL = "",
    ANL01FL = "Y",
    AVALC = NA_character_,
    ADT = as.Date(NA),
    ADY = NA_real_,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE > 0, (AVAL - BASE)/BASE*100, NA_real_)
  )

#==========================================================================
# STEP 9: Combine all efficacy parameters
#==========================================================================
all_cols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","EFFFL","TRTSDT","TRTEDT","AGE","SEX",
              "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","RESPFL")

ensure_cols <- function(df){
  for(c in all_cols) if(!c %in% names(df)) df[[c]] <- NA_character_
  # ensure numeric columns stay numeric where needed - coerce back if needed
  df
}

adeff_resp2 <- ensure_cols(adeff_resp)
adeff_tumor2 <- ensure_cols(adeff_tumor)
bor2 <- ensure_cols(bor)
respfl2 <- ensure_cols(respfl)
nadir2 <- ensure_cols(nadir)

# Fix types after ensure_cols added character NAs to numeric cols
fix_numeric <- function(df){
  num_cols <- c("TRTPN","AGE","PARAMN","AVAL","BASE","CHG","PCHG","ADY","AVISITN")
  for(cn in num_cols) if(cn %in% names(df)) df[[cn]] <- as.numeric(df[[cn]])
  # dates
  for(cn in c("TRTSDT","TRTEDT","ADT")) if(cn %in% names(df)) df[[cn]] <- as.Date(df[[cn]])
  df
}
adeff_resp2 <- fix_numeric(adeff_resp2)
adeff_tumor2 <- fix_numeric(adeff_tumor2)
bor2 <- fix_numeric(bor2)
respfl2 <- fix_numeric(respfl2)
nadir2 <- fix_numeric(nadir2)

adeff <- bind_rows(adeff_resp2, adeff_tumor2, bor2, respfl2, nadir2)

#==========================================================================
# STEP 10: Add sequence and labels
#==========================================================================
adeff <- adeff %>%
  arrange(STUDYID, USUBJID, PARAMN, AVISITN, ADT) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  mutate(
    RSCAT = case_when(
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE") ~ "RESPONDER",
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC=="STABLE DISEASE" ~ "STABLE",
      PARAMCD %in% c("OVRLRESP","BOR") & AVALC=="PROGRESSIVE DISEASE" ~ "PROGRESSOR",
      PARAMCD %in% c("OVRLRESP","BOR") ~ "OTHER",
      TRUE ~ NA_character_
    )
  ) %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, BASE, CHG, PCHG,
         ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL, RESPFL, RSCAT, TRTP, TRTPN, EFFFL, SAFFL, TRTSDT, TRTEDT, AGE, SEX)

#==========================================================================
# Write datasets
#==========================================================================
write_out <- function(df, name){
  df_out <- df
  for(cn in names(df_out)){
    if(inherits(df_out[[cn]], "Date")){
      df_out[[cn]] <- format(df_out[[cn]], "%Y-%m-%d")
    }
  }
  write.csv(df_out, file.path(out, paste0(name,".csv")), row.names=FALSE, na="")
}

write_out(adsl, "adsl")
write_out(adeff_resp, "adeff_resp")
write_out(adeff_tumor, "adeff_tumor")
write_out(adeff, "adeff")

#==========================================================================
# P088.lst text rendering and CSV
#==========================================================================
txt_path <- file.path(out, "P088.txt")
lst_path <- file.path(out, "P088.lst")
csv_path <- file.path(out, "P088.csv")

contents_lines <- c(
  "Program: adeff.sas",
  "Purpose: Create ADEFF (Efficacy Analysis Dataset) - BDS Structure",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "PROC CONTENTS data=adeff varnum;",
  paste0("Observations: ", nrow(adeff)),
  paste0("Variables: ", ncol(adeff)),
  ""
)
var_labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier", ASEQ="Analysis Sequence Number",
  PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)", AVAL="Analysis Value",
  AVALC="Analysis Value (C)", AVALU="Analysis Value Unit", BASE="Baseline Value",
  CHG="Change from Baseline", PCHG="Percent Change from Baseline", ADT="Analysis Date",
  ADY="Analysis Relative Day", AVISIT="Analysis Visit", AVISITN="Analysis Visit (N)",
  ABLFL="Baseline Record Flag", ANL01FL="Analysis Record Flag 01", RESPFL="Responder Flag", RSCAT="Response Category",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)", EFFFL="Efficacy Population Flag",
  SAFFL="Safety Population Flag", TRTSDT="Treatment Start Date", TRTEDT="Treatment End Date",
  AGE="Age", SEX="Sex"
)
contents_lines <- c(contents_lines, "Variables in creation order:")
for(i in seq_along(names(adeff))){
  nm <- names(adeff)[i]
  lab <- ifelse(nm %in% names(var_labels), var_labels[[nm]], "")
  contents_lines <- c(contents_lines, sprintf("%2d  %-10s  %s", i, nm, lab))
}
contents_lines <- c(contents_lines, "", "PROC PRINT data=adeff noobs;", "")

adeff_print <- adeff
adeff_print$ADT <- format(adeff_print$ADT, "%Y-%m-%d")
adeff_print$TRTSDT <- format(adeff_print$TRTSDT, "%Y-%m-%d")
adeff_print$TRTEDT <- format(adeff_print$TRTEDT, "%Y-%m-%d")

print_out <- capture.output(print(as.data.frame(adeff_print), row.names=FALSE, max=1000))

all_lines <- c(contents_lines, print_out)

writeLines(all_lines, txt_path)
writeLines(all_lines, lst_path)

write_out(adeff, "P088")
if (!file.exists(csv_path)) {
  write.csv(adeff_print, csv_path, row.names=FALSE, na="")
}
