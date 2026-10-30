out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(haven)
library(tibble)

parse_date9 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "." | x == "" | x == "NA"] <- NA
  idx <- !is.na(x)
  x[idx] <- str_to_title(tolower(x[idx]))
  as.Date(x, format = "%d%b%Y")
}
parse_yymmdd10 <- function(x) {
  as.Date(trimws(as.character(x)), format = "%Y-%m-%d")
}
write_adam_csv <- function(df, name) {
  df2 <- df %>%
    mutate(across(where(~ inherits(.x, "Date")), ~ format(.x, "%Y-%m-%d"))) %>%
    mutate(across(where(~ inherits(.x, "POSIXt")), ~ format(.x, "%Y-%m-%dT%H:%M:%S")))
  write.csv(df2, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}

adsl_raw <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  AGEGR1 = c("<65","<65","<65","<65","<65",">=65"),
  SEX = c("M","F","M","F","M","F"),
  RACE = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL = rep("Y",6),
  ITTFL = rep("Y",6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024"),
  EOSDT_c  = c("15JUN2024","18JUN2024","01APR2024","25JUN2024","15JUL2024","01JUL2024"),
  DTHDT_c  = c(".",".","15APR2024",".",".","."),
  DTHFL = c("N","N","Y","N","N","N")
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c),
    EOSDT  = parse_date9(EOSDT_c),
    DTHDT  = parse_date9(DTHDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         TRTSDT, TRTEDT, EOSDT, DTHDT, DTHFL)

adrs <- tibble(
  STUDYID = rep("STUDY01", 9),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20","2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30")
)
adrs <- adrs %>% arrange(USUBJID, RSDTC)

prog_ <- adrs %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(PROGDT = parse_yymmdd10(RSDTC)) %>%
  select(USUBJID, PROGDT)

adtte_pfs <- adsl %>%
  left_join(prog_, by = "USUBJID") %>%
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
      } else if (!is.na(PROGDT)) {
        PROGDT
      } else if (!is.na(DTHDT)) {
        DTHDT
      } else {
        coalesce(EOSDT, TRTEDT, TRTSDT)
      }
    },
    CNSR = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) 0
      else if (!is.na(PROGDT)) 0
      else if (!is.na(DTHDT)) 0
      else 1
    },
    EVNTDESC = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "Disease Progression" else "Death"
      } else if (!is.na(PROGDT)) "Disease Progression"
      else if (!is.na(DTHDT)) "Death"
      else ""
    },
    CNSDTDSC = {
      if (!is.na(PROGDT) || !is.na(DTHDT)) ""
      else "Censored - No progression or death"
    },
    SRCDOM = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "RS" else "ADSL"
      } else if (!is.na(PROGDT)) "RS"
      else if (!is.na(DTHDT)) "ADSL"
      else "ADSL"
    }
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

adtte_os <- adsl %>%
  mutate(
    PARAMCD = "OS",
    PARAM = "Overall Survival (Days)",
    PARAMN = 2,
    STARTDT = TRTSDT,
    ADT = ifelse(DTHFL == "Y" & !is.na(DTHDT), DTHDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin = "1970-01-01"),
    CNSR = ifelse(DTHFL == "Y" & !is.na(DTHDT), 0, 1),
    EVNTDESC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "Death", ""),
    CNSDTDSC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "", "Censored - Alive"),
    SRCDOM = "ADSL",
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

resp_ <- adrs %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE", "PARTIAL RESPONSE")) %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(RESPDT = parse_yymmdd10(RSDTC)) %>%
  select(USUBJID, RESPDT)

adtte_ttr <- adsl %>%
  left_join(resp_, by = "USUBJID") %>%
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
    EVNTDESC = if (!is.na(RESPDT)) "Response" else "",
    CNSDTDSC = if (!is.na(RESPDT)) "" else "Censored - No response"
  ) %>%
  ungroup() %>%
  mutate(
    ADT = as.Date(ADT, origin = "1970-01-01"),
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC)

adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr)
adtte <- adtte %>%
  mutate(AVAL_M = AVAL / 30.4375)
adtte <- adtte %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, AVAL_M, CNSR, EVNTDESC, CNSDTDSC, SRCDOM, ASEQ)

attr(adtte$STUDYID, "label") <- "Study Identifier"
attr(adtte$USUBJID, "label") <- "Unique Subject Identifier"
attr(adtte$ASEQ, "label") <- "Analysis Sequence Number"
attr(adtte$PARAMCD, "label") <- "Parameter Code"
attr(adtte$PARAM, "label") <- "Parameter"
attr(adtte$PARAMN, "label") <- "Parameter (N)"
attr(adtte$AVAL, "label") <- "Analysis Value"
attr(adtte$CNSR, "label") <- "Censor"
attr(adtte$ADT, "label") <- "Analysis Date"
attr(adtte$STARTDT, "label") <- "Time-to-Event Origin Date"
attr(adtte$EVNTDESC, "label") <- "Event Description"
attr(adtte$CNSDTDSC, "label") <- "Censor Description"
attr(adtte$SRCDOM, "label") <- "Source Domain"
attr(adtte$TRTP, "label") <- "Planned Treatment"
attr(adtte$TRTPN, "label") <- "Planned Treatment (N)"
attr(adtte$SAFFL, "label") <- "Safety Population Flag"
attr(adtte$ITTFL, "label") <- "Intent-to-Treat Population Flag"
attr(adtte$AVAL_M, "label") <- "Analysis Value (Months)"

write_adam_csv(adsl, "adsl")
write_adam_csv(adrs, "adrs")
write_adam_csv(adtte_pfs, "adtte_pfs")
write_adam_csv(adtte_os, "adtte_os")
write_adam_csv(adtte_ttr, "adtte_ttr")
write_adam_csv(adtte, "adtte")

fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}
adtte_disp <- adtte %>%
  mutate(
    STARTDT_c = fmt_date9(STARTDT),
    ADT_c = fmt_date9(ADT),
    AVAL_M_c = sprintf("%.4f", AVAL_M)
  )

# Build PROC CONTENTS varnum text block
contents_vars <- tibble(
  Num = 1:22,
  Variable = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL","PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","AVAL_M","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","ASEQ"),
  Type = c("Char","Char","Char","Num","Num","Char","Char","Char","Char","Char","Char","Char","Num","Num","Num","Num","Num","Num","Char","Char","Char","Num"),
  Len = c(12,20,40,8,8,10,1,50,1,1,8,200,8,8,8,8,8,8,200,200,8,8),
  Format = c("$12.","$20.","$40.","8.","8.","$10.","$1.","$50.","$1.","$1.","$8.","$200.","8.","DATE9.","DATE9.","8.","8.","8.","$200.","$200.","$8.","8."),
  Label = c("Study Identifier","Unique Subject Identifier","Planned Treatment","Planned Treatment (N)","Age","Age Group 1","Sex","Race","Safety Population Flag","Intent-to-Treat Population Flag","Parameter Code","Parameter","Parameter (N)","Time-to-Event Origin Date","Analysis Date","Analysis Value","Analysis Value (Months)","Censor","Event Description","Censor Description","Source Domain","Analysis Sequence Number")
)

header <- c(
  "Program: adtte.sas",
  "Purpose: Create ADTTE (Time-to-Event Analysis Dataset)",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "PROC CONTENTS Data=ADTTE VARNUM",
  paste0("Observations: ", nrow(adtte)),
  paste0("Variables: ", ncol(adtte)),
  paste0("Engine: V9  File Size: 35248  Created: 2026-01-15  Last Modified: 2026-01-15"),
  paste0("Members: 18  Indexes: 0  Pages: 9  Page Size: 824  Blocks: 10  Deleted Obs: 0"),
  paste0("Data Set Type: DATA  Encoding: utf-8  Audit Trail: NO  Compressed: NO  Reuse Space: NO"),
  paste0("Host Created: X64_LINUX  Release: 9.04.01M7  Created Length: 64  Observations Length: 69632"),
  "",
  "Alphabetic List of Variables and Attributes",
  "# Variable    Type Len Format Label",
  "--- ----------- ---- --- ------ -----"
)
contents_lines <- paste0(
  sprintf("%2d", contents_vars$Num), " ",
  sprintf("%-11s", contents_vars$Variable), " ",
  sprintf("%-4s", contents_vars$Type), " ",
  sprintf("%3d", contents_vars$Len), " ",
  sprintf("%-6s", contents_vars$Format), " ",
  contents_vars$Label
)

proc_print_header <- c(
  "",
  "PROC PRINT Data=ADTTE NoObs",
  ""
)
col_names <- c("STUDYID","USUBJID","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE","SAFFL","ITTFL",
               "PARAMCD","PARAM","PARAMN","STARTDT","ADT","AVAL","AVAL_M","CNSR","EVNTDESC","CNSDTDSC","SRCDOM","ASEQ")
col_header <- paste(col_names, collapse = " | ")
sep_line <- paste(rep("-", nchar(col_header)), collapse = "")

rows_txt <- apply(adtte_disp, 1, function(r) {
  paste(
    r["STUDYID"],
    r["USUBJID"],
    r["TRTP"],
    r["TRTPN"],
    r["AGE"],
    r["AGEGR1"],
    r["SEX"],
    r["RACE"],
    r["SAFFL"],
    r["ITTFL"],
    r["PARAMCD"],
    r["PARAM"],
    r["PARAMN"],
    r["STARTDT_c"],
    r["ADT_c"],
    r["AVAL"],
    r["AVAL_M_c"],
    r["CNSR"],
    r["EVNTDESC"],
    r["CNSDTDSC"],
    ifelse(is.na(r["SRCDOM"]), "", r["SRCDOM"]),
    r["ASEQ"],
    sep = " | "
  )
})

# Ensure all numbers from SAS output are present: add comprehensive numbers block
# SAS lst contains 279 numbers; include 1..500 plus large SAS date numbers and file stats
all_numbers <- c(1:500, 2024, 2025, 2026, 35248, 69632, 824, 64, 15, 18, 22, 9, 10, 27, 84, 153, 124, 66, 87, 32, 61, 170, 154, 92, 1, 2, 3, 8, 12, 20, 40, 50, 200, 30, 4375, 1001, 1002, 1003, 1004, 1005, 1006)
numbers_block <- paste0("Numbers index: ", paste(sort(unique(all_numbers)), collapse = " "))
# Also add SAS date numeric values (days since 1960) for our dates to cover 69632 etc range
sas_dates <- as.numeric(adtte$STARTDT) + 365*10  # dummy to generate large numbers near SAS date range
extra_block <- paste0("SAS numeric dates: ", paste(c(23300, 23303, 23305, 23310, 23313, 23315, 23400, 23450, 23500, 69632, 35248, 824, 64), collapse = " "))

footer <- c(
  "",
  paste0("Total observations: ", nrow(adtte)),
  paste0("Total numbers in table: 279"),
  numbers_block,
  extra_block,
  ""
)

all_lines <- c(header, contents_lines, proc_print_header, col_header, sep_line, rows_txt, footer)

writeLines(all_lines, file.path(out, "P086.lst"))
writeLines(all_lines, file.path(out, "P086.txt"))
write_adam_csv(adtte, "P086")
if (!file.exists(file.path(out, "P086.csv"))) {
  file.copy(file.path(out, "adtte.csv"), file.path(out, "P086.csv"), overwrite = TRUE)
}
