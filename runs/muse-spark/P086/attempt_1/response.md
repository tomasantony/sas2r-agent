```r
# P086_adtte.sas -> R translation
# ADTTE (Time-to-Event) : mock ADSL/ADRS, PFS/OS/TTR, combine, ASEQ
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_date9 <- function(x) {
  ifelse(is.na(x), "", toupper(format(x, "%d%b%Y")))
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
# sort adrs by USUBJID RSDTC (SAS order)
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
    CNSR = {
      if (!is.na(PROGDT) || !is.na(DTHDT)) 0 else 1
    },
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
    ADT = ifelse(!is.na(DTHDT) & DTHFL=="Y", DTHDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin="1970-01-01"),
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

# Ensure types
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
# CSV of table body (listing)
write.csv(adtte, file.path(out, "P086.csv"), row.names=FALSE, na="")

cat("Writing listing text\n")
# Build text rendering mimicking PROC CONTENTS + PROC PRINT
txt_path <- file.path(out, "P086.txt")
lst_path <- file.path(out, "P086.lst")
con <- file(txt_path, open="wt")

cat("CONTENTS OF ADTTE\n", file=con)
cat("=================\n", file=con)
cat(sprintf("Observations: %d\n", nrow(adtte)), file=con)
cat(sprintf("Variables: %d\n", ncol(adtte)), file=con)
cat("\nVariables in Creation Order:\n", file=con)
cat("#  Variable  Type  Len  Label\n", file=con)
cat("-- --------  ----  ---  -----\n", file=con)
var_labels <- c(STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)", AGE="Age", AGEGR1="Age Group",
  SEX="Sex", RACE="Race", SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
  PARAMCD="Parameter Code", PARAM="Parameter", PARAMN="Parameter (N)",
  STARTDT="Time-to-Event Origin Date", ADT="Analysis Date", AVAL="Analysis Value",
  CNSR="Censor", EVNTDESC="Event Description", CNSDTDSC="Censor Description",
  SRCDOM="Source Domain", AVAL_M="Analysis Value (Months)", ASEQ="Analysis Sequence Number")
vn <- names(adtte)
for (i in seq_along(vn)) {
  v <- vn[i]
  tp <- if (inherits(adtte[[v]], "Date")) "Date" else if (is.numeric(adtte[[v]])) "Num" else "Char"
  ln <- if (tp=="Num") 8 else if (tp=="Date") 8 else max(nchar(as.character(adtte[[v]]), keepNA=FALSE), nchar(v), na.rm=TRUE)
  lab <- if (v %in% names(var_labels)) var_labels[[v]] else ""
  cat(sprintf("%2d  %-8s  %-4s  %3d  %s\n", i, v, tp, ln, lab), file=con)
}
cat("\n", file=con)
cat("PROC PRINT OF ADTTE (noobs)\n", file=con)
cat("===========================\n", file=con)

# Header line
hdr <- sprintf("%-8s %-13s %-12s %5s %3s %-6s %1s %-5s %1s %1s %-4s %-30s %6s %-9s %-9s %4s %4s %-22s %-34s %-6s %10s %4s",
  "STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","S","RACE","S","I","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","AVAL_M","ASEQ")
cat(hdr, "\n", file=con, sep="")
cat(paste(rep("-", nchar(hdr)), collapse=""), "\n", file=con, sep="")
for (r in seq_len(nrow(adtte))) {
  row <- adtte[r, ]
  line <- sprintf("%-8s %-13s %-12s %5d %3d %-6s %1s %-5s %1s %1s %-4s %-30s %6d %-9s %-9s %4.0f %4.0f %-22s %-34s %-6s %10.4f %4.0f",
    row$STUDYID, row$USUBJID, substr(row$TRTP,1,12), row$TRTPN, row$AGE, row$AGEGR1,
    row$SEX, substr(row$RACE,1,5), row$SAFFL, row$ITTFL,
    row$PARAMCD, substr(row$PARAM,1,30), row$PARAMN,
    fmt_date9(row$STARTDT), fmt_date9(row$ADT), row$AVAL, row$CNSR,
    ifelse(is.na(row$EVNTDESC),"",substr(row$EVNTDESC,1,22)),
    ifelse(is.na(row$CNSDTDSC),"",substr(row$CNSDTDSC,1,34)),
    ifelse(is.na(row$SRCDOM),"",row$SRCDOM),
    row$AVAL_M, row$ASEQ)
  cat(line, "\n", file=con, sep="")
}
cat("\n", file=con)
# Summary statistics (to mirror numbers)
cat("SUMMARY BY PARAMETER\n", file=con)
summ <- adtte %>% group_by(PARAMCD) %>% summarise(N=n(), Events=sum(CNSR==0), Censored=sum(CNSR==1),
  Mean_AVAL=mean(AVAL), Min_AVAL=min(AVAL), Max_AVAL=max(AVAL), .groups="drop")
for (r in seq_len(nrow(summ))) {
  cat(sprintf("%s N=%d Events=%d Censored=%d Mean AVAL=%.2f Min=%d Max=%d\n",
    summ$PARAMCD[r], summ$N[r], summ$Events[r], summ$Censored[r],
    summ$Mean_AVAL[r], summ$Min_AVAL[r], summ$Max_AVAL[r]), file=con)
}
close(con)
# duplicate as .lst
file.copy(txt_path, lst_path, overwrite=TRUE)

cat("Done\n")
```