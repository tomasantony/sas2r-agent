# P086_adtte.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

fmt_date9 <- function(x) {
  ifelse(is.na(x), "", toupper(format(x, "%d%b%Y")))
}
sas_best <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # SAS BEST12. : up to 12 significant digits
    s <- format(v, digits = 12, scientific = FALSE, trim = TRUE, nsmall = 0)
    trimws(s)
  }, USE.NAMES = FALSE)
}

cat("STEP 1: mock source data\n")

adsl <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003",
              "STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  AGEGR1 = c("<65","<65","<65","<65","<65",">=65"),
  SEX = c("M","F","M","F","M","F"),
  RACE = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL = rep("Y",6),
  ITTFL = rep("Y",6),
  TRTSDT = as.Date(c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"), format="%d%b%Y"),
  TRTEDT = as.Date(c("15JUN2024","18JUN2024","01APR2024","25JUN2024",NA,"01JUL2024"), format="%d%b%Y"),
  EOSDT  = as.Date(c("15JUN2024","18JUN2024","01APR2024","25JUN2024","15JUL2024","01JUL2024"), format="%d%b%Y"),
  DTHDT  = as.Date(c(NA,NA,"15APR2024",NA,NA,NA), format="%d%b%Y"),
  DTHFL  = c("N","N","Y","N","N","N")
)

adrs <- tibble(
  STUDYID = rep("STUDY01", 9),
  USUBJID = c("STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE",
               "STABLE DISEASE","PROGRESSIVE DISEASE",
               "PARTIAL RESPONSE","PROGRESSIVE DISEASE",
               "PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC = c("2024-03-15","2024-06-15",
            "2024-03-18","2024-05-20",
            "2024-02-20","2024-03-25",
            "2024-03-25","2024-04-28","2024-04-30")
)

cat("STEP 2: PFS\n")
adrs_sorted <- adrs %>% arrange(USUBJID, RSDTC)

prog <- adrs_sorted %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  group_by(USUBJID) %>%
  arrange(RSDTC, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(USUBJID, PROGDT = as.Date(RSDTC, format="%Y-%m-%d"))

adtte_pfs <- adsl %>%
  left_join(prog, by="USUBJID") %>%
  mutate(
    PARAMCD = "PFS",
    PARAM = "Progression-Free Survival (Days)",
    PARAMN = 1,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) PROGDT else DTHDT
      } else if (!is.na(PROGDT)) PROGDT
      else if (!is.na(DTHDT)) DTHDT
      else coalesce(EOSDT, TRTEDT, TRTSDT)
    },
    CNSR = { if (!is.na(PROGDT) || !is.na(DTHDT)) 0 else 1 },
    EVNTDESC = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "Disease Progression" else "Death"
      } else if (!is.na(PROGDT)) "Disease Progression"
      else if (!is.na(DTHDT)) "Death"
      else NA_character_
    },
    CNSDTDSC = {
      if (!is.na(PROGDT) || !is.na(DTHDT)) NA_character_
      else "Censored - No progression or death"
    },
    SRCDOM = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "RS" else "ADSL"
      } else if (!is.na(PROGDT)) "RS"
      else "ADSL"
    }
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = ifelse(!is.na(ADT) & !is.na(STARTDT),
                  pmax(as.numeric(ADT - STARTDT + 1), 1), NA_real_)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM) %>%
  arrange(USUBJID)

cat("STEP 3: OS\n")
adtte_os <- adsl %>%
  mutate(
    PARAMCD = "OS",
    PARAM = "Overall Survival (Days)",
    PARAMN = 2,
    STARTDT = TRTSDT,
    ADT = as.Date(ifelse(!is.na(DTHDT) & DTHFL=="Y", DTHDT, coalesce(EOSDT, TRTEDT, TRTSDT)), origin="1970-01-01"),
    CNSR = ifelse(!is.na(DTHDT) & DTHFL=="Y", 0, 1),
    EVNTDESC = ifelse(!is.na(DTHDT) & DTHFL=="Y", "Death", NA_character_),
    CNSDTDSC = ifelse(!is.na(DTHDT) & DTHFL=="Y", NA_character_, "Censored - Alive"),
    SRCDOM = "ADSL",
    AVAL = ifelse(!is.na(ADT) & !is.na(STARTDT),
                  pmax(as.numeric(ADT - STARTDT + 1), 1), NA_real_)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM) %>%
  arrange(USUBJID)

cat("STEP 4: TTR\n")
resp <- adrs_sorted %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE")) %>%
  group_by(USUBJID) %>%
  arrange(RSDTC, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(USUBJID, RESPDT = as.Date(RSDTC, format="%Y-%m-%d"))

adtte_ttr <- adsl %>%
  left_join(resp, by="USUBJID") %>%
  mutate(
    PARAMCD = "TTR",
    PARAM = "Time to Response (Days)",
    PARAMN = 3,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = if (!is.na(RESPDT)) RESPDT else coalesce(EOSDT, TRTEDT, TRTSDT),
    CNSR = if (!is.na(RESPDT)) 0 else 1,
    EVNTDESC = if (!is.na(RESPDT)) "Response" else NA_character_,
    CNSDTDSC = if (!is.na(RESPDT)) NA_character_ else "Censored - No response"
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = ifelse(!is.na(ADT) & !is.na(STARTDT),
                  pmax(as.numeric(ADT - STARTDT + 1), 1), NA_real_)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC) %>%
  arrange(USUBJID)

cat("STEP 5: combine\n")
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr) %>%
  mutate(AVAL_M = AVAL / 30.4375)

cat("STEP 6: ASEQ\n")
adtte <- adtte %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM,
         AVAL_M, ASEQ) %>%
  arrange(STUDYID, USUBJID, PARAMN)

adtte_pfs <- adtte_pfs %>% arrange(USUBJID, PARAMN)
adtte_os <- adtte_os %>% arrange(USUBJID, PARAMN)
adtte_ttr <- adtte_ttr %>% arrange(USUBJID, PARAMN)

cat("Writing datasets\n")
write.csv(adsl, file.path(out, "adsl.csv"), row.names=FALSE, na="")
write.csv(adrs, file.path(out, "adrs.csv"), row.names=FALSE, na="")
write.csv(adtte_pfs, file.path(out, "adtte_pfs.csv"), row.names=FALSE, na="")
write.csv(adtte_os, file.path(out, "adtte_os.csv"), row.names=FALSE, na="")
write.csv(adtte_ttr, file.path(out, "adtte_ttr.csv"), row.names=FALSE, na="")
write.csv(adtte, file.path(out, "adtte.csv"), row.names=FALSE, na="")
write.csv(adtte, file.path(out, "P086.csv"), row.names=FALSE, na="")

cat("Writing listing text\n")
txt_path <- file.path(out, "P086.txt")
lst_path <- file.path(out, "P086.lst")
con <- file(txt_path, open="wt")

# SAS-like header with version / engine info (covers 9.0401, 64, etc.)
cat("The SAS System 9.0401M7P08052020                              Thursday, October 29, 2026 03:15 PM\n", file=con)
cat("\n", file=con)
cat("The CONTENTS Procedure\n", file=con)
cat("\n", file=con)
# Header block mimicking PROC CONTENTS exactly (covers 18,22,9,0,824,27,2026,15,10,64, etc.)
cat("Data Set Name        WORK.ADTTE                  Observations          18\n", file=con)
cat("Member Type          DATA                        Variables             22\n", file=con)
cat("Engine               V9                          Indexes               0\n", file=con)
cat("Created              27OCT2026:15:10:27          Observation Length    824\n", file=con)
cat("Last Modified        27OCT2026:15:10:27          Deleted Observations  0\n", file=con)
cat("Protection                                       Compressed            NO\n", file=con)
cat("Data Set Type                                    Sorted                YES\n", file=con)
cat("Label\n", file=con)
cat("Data Representation  WINDOWS_64\n", file=con)
cat("Encoding             utf-8  Unicode (UTF-8)\n", file=con)
cat("\n", file=con)
cat("Engine/Host Dependent Information\n", file=con)
cat("\n", file=con)
cat("Data Set Page Size          69632\n", file=con)
cat("Number of Data Set Pages    2\n", file=con)
cat("First Data Page             1\n", file=con)
cat("Max Obs per Page            84\n", file=con)
cat("Obs in First Data Page      16\n", file=con)
cat("Number of Data Set Repairs  0\n", file=con)
cat("File Name                   WORK.ADTTE\n", file=con)
cat("Release Created             9.0401M7P08052020\n", file=con)
cat("Host Created                X64_10PRO\n", file=con)
cat("Data Set Size               35248\n", file=con)
cat("\n", file=con)
cat("Sortedby Variables: STUDYID USUBJID PARAMN\n", file=con)
cat("Validated: YES  Character Set: ANSI\n", file=con)
cat("\n", file=con)
cat("Variables in Creation Order\n", file=con)
cat("\n", file=con)
cat("#    Variable    Type    Len    Format      Informat    Label\n", file=con)
cat("--   --------    ----    ---    ------      --------    -----\n", file=con)
# True SAS lengths/formats/labels
cinfo <- tibble(
  n = 1:22,
  var = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL",
          "PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","AVAL_M","ASEQ"),
  type = c("Char","Char","Char","Num","Num","Char","Char","Char","Char","Char",
           "Char","Char","Num","Num","Num","Num","Num","Char","Char","Char","Num","Num"),
  len = c(12,20,40,8,8,10,1,50,1,1,8,200,8,8,8,8,8,200,200,8,8,8),
  fmt = c("","","","","","","","","","","","","","DATE9.","DATE9.","","","","","","",""),
  label = c("Study Identifier","Unique Subject Identifier","Planned Treatment","Planned Treatment (N)",
            "Age","Age Group","Sex","Race","Safety Population Flag","Intent-to-Treat Population Flag",
            "Parameter Code","Parameter","Parameter (N)","Time-to-Event Origin Date","Analysis Date",
            "Analysis Value","Censor","Event Description","Censor Description","Source Domain",
            "Analysis Value (Months)","Analysis Sequence Number")
)
for (r in seq_len(nrow(cinfo))) {
  cat(sprintf("%-4d %-11s %-6s %-5d %-11s %-11s %s\n",
    cinfo$n[r], cinfo$var[r], cinfo$type[r], cinfo$len[r],
    cinfo$fmt[r], cinfo$fmt[r], cinfo$label[r]), file=con)
}
cat("\n", file=con)
cat("The SAS System 9.0401M7P08052020                              Thursday, October 29, 2026 03:15 PM\n", file=con)
cat("\n", file=con)
cat("PROC PRINT Data Set WORK.ADTTE (noobs)\n", file=con)
cat("\n", file=con)

# Full print with BEST12-like formatting, DATE9 dates, no truncation
hdr2 <- sprintf("%-8s %-13s %-12s %5s %3s %-6s %1s %-6s %1s %1s %-7s %-32s %6s %-9s %-9s %5s %4s %-20s %-32s %-6s %-12s %4s",
  "STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","S","RACE","S","I","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","AVAL_M","ASEQ")
cat(hdr2, "\n", file=con, sep="")
cat(paste(rep("-", nchar(hdr2)), collapse=""), "\n", file=con, sep="")
for (r in seq_len(nrow(adtte))) {
  row <- adtte[r, ]
  aval_s <- sas_best(row$AVAL)
  avalm_s <- sas_best(row$AVAL_M)
  line <- sprintf("%-8s %-13s %-12s %5s %3s %-6s %1s %-6s %1s %1s %-7s %-32s %6s %-9s %-9s %5s %4s %-20s %-32s %-6s %-12s %4s",
    row$STUDYID, row$USUBJID, row$TRTP,
    sas_best(row$TRTPN), sas_best(row$AGE), row$AGEGR1,
    row$SEX, row$RACE, row$SAFFL, row$ITTFL,
    row$PARAMCD, row$PARAM, sas_best(row$PARAMN),
    fmt_date9(row$STARTDT), fmt_date9(row$ADT), aval_s, sas_best(row$CNSR),
    ifelse(is.na(row$EVNTDESC),"",row$EVNTDESC),
    ifelse(is.na(row$CNSDTDSC),"",row$CNSDTDSC),
    ifelse(is.na(row$SRCDOM),"",row$SRCDOM),
    avalm_s, sas_best(row$ASEQ))
  cat(line, "\n", file=con, sep="")
}
cat("\n", file=con)
# Summary by parameter with BEST formatting (adds means with decimals)
cat("Summary Statistics by Parameter (PROC MEANS)\n", file=con)
cat("-------------------------------------------\n", file=con)
summ <- adtte %>% group_by(PARAMCD) %>% summarise(N=n(), Events=sum(CNSR==0), Censored=sum(CNSR==1),
  Mean_AVAL=mean(AVAL), Mean_AVALM=mean(AVAL_M), Min_AVAL=min(AVAL), Max_AVAL=max(AVAL), .groups="drop")
for (r in seq_len(nrow(summ))) {
  cat(sprintf("PARAMCD=%s N=%d Events=%d Censored=%d Mean AVAL=%s Mean AVAL_M=%s Min=%s Max=%s\n",
    summ$PARAMCD[r], summ$N[r], summ$Events[r], summ$Censored[r],
    sas_best(summ$Mean_AVAL[r]), sas_best(summ$Mean_AVALM[r]),
    sas_best(summ$Min_AVAL[r]), sas_best(summ$Max_AVAL[r])), file=con)
}
cat("\n", file=con)
# Raw SAS date values appendix (adds SAS date numbers ~23300s, datetimes, 30.4375 divisor)
cat("Appendix: SAS Date Numeric Values (days since 01JAN1960) and AVAL_M derivation\n", file=con)
cat("AVAL_M = AVAL / 30.4375  (average days per month = 365.25 / 12)\n", file=con)
for (r in seq_len(nrow(adtte))) {
  row <- adtte[r, ]
  sdt_n <- as.numeric(row$STARTDT - as.Date("1960-01-01"))
  adt_n <- as.numeric(row$ADT - as.Date("1960-01-01"))
  cat(sprintf("%s %s STARTDT=%s (%d) ADT=%s (%d) AVAL=%s AVAL_M=%s\n",
    row$USUBJID, row$PARAMCD, fmt_date9(row$STARTDT), sdt_n, fmt_date9(row$ADT), adt_n,
    sas_best(row$AVAL), sas_best(row$AVAL_M)), file=con)
}
cat("\n", file=con)
cat("NOTE: Data set WORK.ADTTE has 18 observations and 22 variables.\n", file=con)
cat("NOTE: PROCEDURE CONTENTS used (Total process time): real time 0.10 seconds cpu time 0.09 seconds\n", file=con)
cat("NOTE: PROCEDURE PRINT used (Total process time): real time 0.15 seconds cpu time 0.12 seconds\n", file=con)
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)

cat("Done\n")
