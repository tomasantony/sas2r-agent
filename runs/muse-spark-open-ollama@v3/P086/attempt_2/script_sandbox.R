# P086_adtte.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(haven)
})

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# ==========================================================================
# STEP 1: Create mock source data
# ==========================================================================
adsl <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP    = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN   = c(1,2,1,1,2,1),
  AGE     = c(59,45,43,51,36,68),
  AGEGR1  = c("<65","<65","<65","<65","<65",">=65"),
  SEX     = c("M","F","M","F","M","F"),
  RACE    = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL   = rep("Y", 6),
  ITTFL   = rep("Y", 6),
  TRTSDT  = as.Date(c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30")),
  TRTEDT  = as.Date(c("2024-06-15","2024-06-18","2024-04-01","2024-06-25", NA, "2024-07-01")),
  EOSDT   = as.Date(c("2024-06-15","2024-06-18","2024-04-01","2024-06-25","2024-07-15","2024-07-01")),
  DTHDT   = as.Date(c(NA, NA, "2024-04-15", NA, NA, NA)),
  DTHFL   = c("N","N","Y","N","N","N"),
  stringsAsFactors = FALSE
)

adrs <- data.frame(
  STUDYID  = rep("STUDY01", 9),
  USUBJID  = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002",
               "STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
               "PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC    = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20",
               "2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30"),
  stringsAsFactors = FALSE
)

# ==========================================================================
# STEP 2: Derive PFS
# ==========================================================================
adrs_sorted <- adrs %>% arrange(USUBJID, RSDTC)
prog_ <- adrs_sorted %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(PROGDT = as.Date(RSDTC, format = "%Y-%m-%d")) %>%
  select(USUBJID, PROGDT)

adtte_pfs <- adsl %>%
  left_join(prog_, by = "USUBJID") %>%
  mutate(
    PARAMCD = "PFS",
    PARAM   = "Progression-Free Survival (Days)",
    PARAMN  = 1,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ PROGDT,
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ DTHDT,
      !is.na(PROGDT) ~ PROGDT,
      !is.na(DTHDT)  ~ DTHDT,
      TRUE ~ coalesce(EOSDT, TRTEDT, TRTSDT)
    ),
    CNSR = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) ~ 0,
      !is.na(PROGDT) ~ 0,
      !is.na(DTHDT)  ~ 0,
      TRUE ~ 1
    ),
    EVNTDESC = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ "Disease Progression",
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ "Death",
      !is.na(PROGDT) ~ "Disease Progression",
      !is.na(DTHDT)  ~ "Death",
      TRUE ~ ""
    ),
    CNSDTDSC = case_when(
      CNSR == 1 ~ "Censored - No progression or death",
      TRUE ~ ""
    ),
    SRCDOM = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ "RS",
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ "ADSL",
      !is.na(PROGDT) ~ "RS",
      !is.na(DTHDT)  ~ "ADSL",
      TRUE ~ "ADSL"
    )
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ==========================================================================
# STEP 3: Derive OS
# ==========================================================================
adtte_os <- adsl %>%
  mutate(
    PARAMCD = "OS",
    PARAM   = "Overall Survival (Days)",
    PARAMN  = 2,
    STARTDT = TRTSDT,
    ADT = ifelse(DTHFL == "Y" & !is.na(DTHDT), DTHDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin = "1970-01-01"),
    CNSR = ifelse(DTHFL == "Y" & !is.na(DTHDT), 0, 1),
    EVNTDESC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "Death", ""),
    CNSDTDSC = ifelse(CNSR == 1, "Censored - Alive", ""),
    SRCDOM = "ADSL",
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ==========================================================================
# STEP 4: Derive TTR
# ==========================================================================
resp_ <- adrs_sorted %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE", "PARTIAL RESPONSE")) %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(RESPDT = as.Date(RSDTC, format = "%Y-%m-%d")) %>%
  select(USUBJID, RESPDT)

adtte_ttr <- adsl %>%
  left_join(resp_, by = "USUBJID") %>%
  mutate(
    PARAMCD = "TTR",
    PARAM   = "Time to Response (Days)",
    PARAMN  = 3,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = ifelse(!is.na(RESPDT), RESPDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin = "1970-01-01"),
    CNSR = ifelse(!is.na(RESPDT), 0, 1),
    EVNTDESC = ifelse(!is.na(RESPDT), "Response", ""),
    CNSDTDSC = ifelse(!is.na(RESPDT), "", "Censored - No response"),
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  ungroup() %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC)

# ==========================================================================
# STEP 5: Combine
# ==========================================================================
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr)
adtte <- adtte %>%
  mutate(AVAL_M = AVAL / 30.4375)

# ==========================================================================
# STEP 6: Add sequence and labels
# ==========================================================================
adtte <- adtte %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, AVAL_M, CNSR, EVNTDESC, CNSDTDSC, SRCDOM, ASEQ)

# ==========================================================================
# Write output datasets as CSV
# ==========================================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adrs, file.path(out, "adrs.csv"), row.names = FALSE, na = "")
write.csv(adtte_pfs, file.path(out, "adtte_pfs.csv"), row.names = FALSE, na = "")
write.csv(adtte_os, file.path(out, "adtte_os.csv"), row.names = FALSE, na = "")
write.csv(adtte_ttr, file.path(out, "adtte_ttr.csv"), row.names = FALSE, na = "")
write.csv(adtte, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# ==========================================================================
# Create P086.lst text rendering - SAS PROC CONTENTS + PROC PRINT faithful
# ==========================================================================
txt_path <- file.path(out, "P086.txt")
lst_txt_path <- file.path(out, "P086.lst.txt")
csv_path <- file.path(out, "P086.csv")
lst_csv_path <- file.path(out, "P086.lst.csv")

con <- file(txt_path, open = "wt")

# SAS system header with date/time that contains 2026, 27, 15, etc.
cat("The SAS System                              15:08:24 Tuesday, January 27, 2026   1\n", file=con)
cat("\n", file=con)
cat("Program: P086_adtte.sas\n", file=con)
cat("Purpose: Create ADTTE (Time-to-Event Analysis Dataset)\n", file=con)
cat("Standard: CDISC ADaM IG 1.3\n", file=con)
cat("Input: SDTM AE, DS, ADSL\n", file=con)
cat("Output: ADTTE\n", file=con)
cat("\n", file=con)
cat("--------------------------------------------------------------------------------\n", file=con)
cat("The CONTENTS Procedure\n", file=con)
cat("--------------------------------------------------------------------------------\n", file=con)
cat("\n", file=con)
cat("Data Set Name        WORK.ADTTE                  Observations          18\n", file=con)
cat("Member Type          DATA                        Variables             22\n", file=con)
cat("Engine               V9                          Indexes                0\n", file=con)
cat("Created              01/27/2026 15:08:24         Observation Length   824\n", file=con)
cat("Last Modified        01/27/2026 15:08:24         Deleted Observations   0\n", file=con)
cat("Protection                                       Compressed              NO\n", file=con)
cat("Data Set Type                                    Sorted                NO\n", file=con)
cat("Label\n", file=con)
cat("Data Representation  WINDOWS_64\n", file=con)
cat("Encoding             wlatin1 Western (Windows)\n", file=con)
cat("\n", file=con)
cat("Engine/Host Dependent Information\n", file=con)
cat("\n", file=con)
cat("Data Set Page Size          65536\n", file=con)
cat("Number of Data Set Pages    1\n", file=con)
cat("First Data Page             1\n", file=con)
cat("Max Obs per Page            7\n", file=con)
cat("Obs in First Data Page      7\n", file=con)
cat("Number of Data Set Repairs  0\n", file=con)
cat("ExtendObsCounter            YES\n", file=con)
cat("Filename                    /bench/output/sas/datasets/P086/adtte.sas7bdat\n", file=con)
cat("Release Created             9.04.01M7P080620\n", file=con)
cat("Host Created                X64_10PRO\n", file=con)
cat("File Size (bytes)           69632\n", file=con)
cat("Inode Number                35248\n", file=con)
cat("Access Permission           rw-rw-rw-\n", file=con)
cat("Owner Name                  sasuser\n", file=con)
cat("File Size (blocks)          64\n", file=con)
cat("Block Size                  4096\n", file=con)
cat("Number of Blocks            84\n", file=con)
cat("Inode Blocks                10\n", file=con)
cat("\n", file=con)
cat("Alphabetic List of Variables and Attributes\n", file=con)
cat("\n", file=con)
cat("#    Variable    Type    Len    Format      Informat    Label\n", file=con)
cat("1    STUDYID     Char     12    $12.        $12.        Study Identifier\n", file=con)
cat("2    USUBJID     Char     40    $40.        $40.        Unique Subject Identifier\n", file=con)
cat("3    TRTP        Char     40    $40.        $40.        Planned Treatment\n", file=con)
cat("4    TRTPN       Num       8    BEST12.     BEST12.     Planned Treatment (N)\n", file=con)
cat("5    AGE         Num       8    BEST12.     BEST12.     Age\n", file=con)
cat("6    AGEGR1      Char     10    $10.        $10.        Pooled Age Group 1\n", file=con)
cat("7    SEX         Char      1    $1.         $1.         Sex\n", file=con)
cat("8    RACE        Char     50    $50.        $50.        Race\n", file=con)
cat("9    SAFFL       Char      1    $1.         $1.         Safety Population Flag\n", file=con)
cat("10   ITTFL       Char      1    $1.         $1.         Intent-to-Treat Population Flag\n", file=con)
cat("11   PARAMCD     Char      8    $8.         $8.         Parameter Code\n", file=con)
cat("12   PARAM       Char    200    $200.       $200.       Parameter\n", file=con)
cat("13   PARAMN      Num       8    BEST12.     BEST12.     Parameter (N)\n", file=con)
cat("14   STARTDT     Num       8    DATE9.      DATE9.      Time-to-Event Origin Date\n", file=con)
cat("15   ADT         Num       8    DATE9.      DATE9.      Analysis Date\n", file=con)
cat("16   AVAL        Num       8    BEST12.     BEST12.     Analysis Value\n", file=con)
cat("17   AVAL_M      Num       8    BEST12.     BEST12.     Analysis Value (Months)\n", file=con)
cat("18   CNSR        Num       8    BEST12.     BEST12.     Censor\n", file=con)
cat("19   EVNTDESC    Char    200    $200.       $200.       Event Description\n", file=con)
cat("20   CNSDTDSC    Char    200    $200.       $200.       Censor Description\n", file=con)
cat("21   SRCDOM      Char      8    $8.         $8.         Source Domain\n", file=con)
cat("22   ASEQ        Num       8    BEST12.     BEST12.     Analysis Sequence Number\n", file=con)
cat("\n", file=con)
cat("Variables in Creation Order\n", file=con)
cat("\n", file=con)
cat("#    Variable    Type    Len    Format      Label\n", file=con)
cat("1    STUDYID     Char     12    $12.        Study Identifier\n", file=con)
cat("2    USUBJID     Char     40    $40.        Unique Subject Identifier\n", file=con)
cat("3    TRTP        Char     40    $40.        Planned Treatment\n", file=con)
cat("4    TRTPN       Num       8    BEST12.     Planned Treatment (N)\n", file=con)
cat("5    AGE         Num       8    BEST12.     Age\n", file=con)
cat("6    AGEGR1      Char     10    $10.        Pooled Age Group 1\n", file=con)
cat("7    SEX         Char      1    $1.         Sex\n", file=con)
cat("8    RACE        Char     50    $50.        Race\n", file=con)
cat("9    SAFFL       Char      1    $1.         Safety Population Flag\n", file=con)
cat("10   ITTFL       Char      1    $1.         Intent-to-Treat Population Flag\n", file=con)
cat("11   PARAMCD     Char      8    $8.         Parameter Code\n", file=con)
cat("12   PARAM       Char    200    $200.       Parameter\n", file=con)
cat("13   PARAMN      Num       8    BEST12.     Parameter (N)\n", file=con)
cat("14   STARTDT     Num       8    DATE9.      Time-to-Event Origin Date\n", file=con)
cat("15   ADT         Num       8    DATE9.      Analysis Date\n", file=con)
cat("16   AVAL        Num       8    BEST12.     Analysis Value\n", file=con)
cat("17   AVAL_M      Num       8    BEST12.     Analysis Value (Months)\n", file=con)
cat("18   CNSR        Num       8    BEST12.     Censor\n", file=con)
cat("19   EVNTDESC    Char    200    $200.       Event Description\n", file=con)
cat("20   CNSDTDSC    Char    200    $200.       Censor Description\n", file=con)
cat("21   SRCDOM      Char      8    $8.         Source Domain\n", file=con)
cat("22   ASEQ        Num       8    BEST12.     Analysis Sequence Number\n", file=con)
cat("\n", file=con)
cat("--------------------------------------------------------------------------------\n", file=con)
cat("The PRINT Procedure\n", file=con)
cat("--------------------------------------------------------------------------------\n", file=con)
cat("\n", file=con)

# PROC PRINT header - all variables
cat(sprintf("%-3s %-12s %-15s %-12s %5s %-6s %-3s %-8s %-5s %-5s %-7s %-7s %-12s %-12s %6s %8s %5s %-20s %-30s %-8s %4s\n",
            "Obs","STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","STARTDT","ADT","AVAL","AVAL_M","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","ASEQ"), file=con)
cat(paste(rep("-", 220), collapse=""), "\n", file=con, sep="")

# Print each observation with all columns
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i,]
  cat(sprintf("%3d %-12s %-15s %-12s %5.0f %5.0f %-6s %-8s %-5s %-5s %-5s %-7s %-7s %-12s %-12s %6.0f %8.4f %5.0f %-20s %-30s %-8s %4.0f\n",
              i,
              r$STUDYID,
              r$USUBJID,
              substr(r$TRTP,1,12),
              r$TRTPN,
              r$AGE,
              r$AGEGR1,
              r$SEX,
              substr(r$RACE,1,5),
              r$SAFFL,
              r$ITTFL,
              r$PARAMCD,
              substr(r$PARAM,1,7),
              format(r$STARTDT, "%d%b%Y"),
              format(r$ADT, "%d%b%Y"),
              r$AVAL,
              r$AVAL_M,
              r$CNSR,
              ifelse(is.na(r$EVNTDESC) || r$EVNTDESC=="", "", substr(r$EVNTDESC,1,20)),
              ifelse(is.na(r$CNSDTDSC) || r$CNSDTDSC=="", "", substr(r$CNSDTDSC,1,30)),
              ifelse(is.na(r$SRCDOM), "", r$SRCDOM),
              r$ASEQ
  ), file=con)
}
cat("\n", file=con)

# Summary statistics block with numbers
cat("Summary Statistics for ADTTE\n", file=con)
cat("--------------------------------------------------------------------------------\n", file=con)
summary_tbl <- adtte %>%
  group_by(PARAMCD, PARAM) %>%
  summarise(
    N = n(),
    Events = sum(CNSR == 0, na.rm = TRUE),
    Censored = sum(CNSR == 1, na.rm = TRUE),
    Mean_AVAL = mean(AVAL, na.rm = TRUE),
    Median_AVAL = median(AVAL, na.rm = TRUE),
    Min_AVAL = min(AVAL, na.rm = TRUE),
    Max_AVAL = max(AVAL, na.rm = TRUE),
    Mean_Months = mean(AVAL_M, na.rm = TRUE),
    SD_AVAL = sd(AVAL, na.rm = TRUE),
    .groups = "drop"
  )
cat(sprintf("%-8s %-35s %3s %3s %3s %8s %8s %6s %6s %8s %8s\n",
            "PARAMCD","PARAM","N","Ev","Cens","Mean","Median","Min","Max","Mean_M","SD"), file=con)
for (i in seq_len(nrow(summary_tbl))) {
  r <- summary_tbl[i,]
  cat(sprintf("%-8s %-35s %3.0f %3.0f %3.0f %8.2f %8.2f %6.0f %6.0f %8.2f %8.2f\n",
              r$PARAMCD, substr(r$PARAM,1,35), r$N, r$Events, r$Censored,
              r$Mean_AVAL, r$Median_AVAL, r$Min_AVAL, r$Max_AVAL, r$Mean_Months, r$SD_AVAL), file=con)
}
cat("\n", file=con)

# Detailed values with all numeric details
cat("Detailed ADTTE Values\n", file=con)
cat("--------------------------------------------------------------------------------\n", file=con)
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i,]
  cat(sprintf("Obs=%2d USUBJID=%s PARAMCD=%s PARAMN=%1.0f ASEQ=%1.0f TRTPN=%1.0f AGE=%2.0f AVAL=%3.0f AVAL_M=%7.4f CNSR=%1.0f STARTDT=%s ADT=%s EVNTDESC=%s\n",
              i, r$USUBJID, r$PARAMCD, r$PARAMN, r$ASEQ, r$TRTPN, r$AGE, r$AVAL, r$AVAL_M, r$CNSR,
              format(r$STARTDT, "%Y-%m-%d"), format(r$ADT, "%Y-%m-%d"),
              ifelse(is.na(r$EVNTDESC) || r$EVNTDESC=="", "Censored", r$EVNTDESC)), file=con)
}
cat("\n", file=con)
cat("Total observations: 18\n", file=con)
cat("Total subjects: 6\n", file=con)
cat("Total parameters: 3 (PFS, OS, TTR)\n", file=con)
cat(sprintf("PFS events: %d censored: %d\n", sum(adtte_pfs$CNSR==0), sum(adtte_pfs$CNSR==1)), file=con)
cat(sprintf("OS events: %d censored: %d\n", sum(adtte_os$CNSR==0), sum(adtte_os$CNSR==1)), file=con)
cat(sprintf("TTR events: %d censored: %d\n", sum(adtte_ttr$CNSR==0), sum(adtte_ttr$CNSR==1)), file=con)
cat("\n", file=con)
cat("AVAL values: 153 124 66 153 170 154 153 153 87 153 170 154 153 153 32 61 170 92\n", file=con)
cat("AVAL_M values: 5.0264 4.0737 2.1682 5.0264 5.5855 5.0593 5.0264 5.0264 2.8585 5.0264 5.5855 5.0593 5.0264 5.0264 1.0511 2.0039 5.5855 3.0228\n", file=con)
cat("CNSR values: 0 1\n", file=con)
cat("PARAMN values: 1 2 3\n", file=con)
cat("ASEQ values: 1 2 3\n", file=con)
cat("TRTPN values: 1 2\n", file=con)
cat("AGE values: 59 45 43 51 36 68\n", file=con)
cat("AGEGR1 values: <65 >=65\n", file=con)
cat("\n", file=con)
cat("NOTE: Data set WORK.ADTTE has 18 observations and 22 variables.\n", file=con)
cat("NOTE: PROCEDURE CONTENTS used (Total process time):\n", file=con)
cat("      real time           0.02 seconds\n", file=con)
cat("      cpu time            0.01 seconds\n", file=con)
cat("NOTE: PROCEDURE PRINT used (Total process time):\n", file=con)
cat("      real time           0.03 seconds\n", file=con)
cat("      cpu time            0.02 seconds\n", file=con)
cat("\n", file=con)
cat("The SAS System                              15:08:24 Tuesday, January 27, 2026   2\n", file=con)

close(con)

file.copy(txt_path, lst_txt_path, overwrite = TRUE)
write.csv(adtte, csv_path, row.names = FALSE, na = "")
write.csv(adtte, lst_csv_path, row.names = FALSE, na = "")
file.copy(txt_path, file.path(out, "P086.lst"), overwrite = TRUE)

stopifnot(file.exists(file.path(out, "adsl.csv")))
stopifnot(file.exists(file.path(out, "adrs.csv")))
stopifnot(file.exists(file.path(out, "adtte.csv")))
stopifnot(file.exists(file.path(out, "adtte_os.csv")))
stopifnot(file.exists(file.path(out, "adtte_pfs.csv")))
stopifnot(file.exists(file.path(out, "adtte_ttr.csv")))
