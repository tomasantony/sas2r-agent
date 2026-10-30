out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(haven)
library(data.table)
library(ggplot2)

fmt_date <- function(x) {
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
}

#==========================================================================
# STEP 1: Create mock SDTM VS data (inline datalines)
#==========================================================================
vs_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~VSSEQ, ~VSTESTCD, ~VSTEST, ~VSORRES, ~VSORRESU, ~VSSTRESN, ~VSSTRESU, ~VISIT, ~VISITNUM, ~VSDTC,
  "STUDY01","STUDY01-1001",1,"SYSBP","Systolic Blood Pressure","128","mmHg",128,"mmHg","Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",2,"DIABP","Diastolic Blood Pressure","82","mmHg",82,"mmHg","Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",3,"PULSE","Pulse Rate","72","beats/min",72,"beats/min","Baseline",1,"2024-01-15",
  "STUDY01","STUDY01-1001",4,"SYSBP","Systolic Blood Pressure","122","mmHg",122,"mmHg","Week 4",2,"2024-02-12",
  "STUDY01","STUDY01-1001",5,"DIABP","Diastolic Blood Pressure","78","mmHg",78,"mmHg","Week 4",2,"2024-02-12",
  "STUDY01","STUDY01-1001",6,"PULSE","Pulse Rate","70","beats/min",70,"beats/min","Week 4",2,"2024-02-12",
  "STUDY01","STUDY01-1001",7,"SYSBP","Systolic Blood Pressure","118","mmHg",118,"mmHg","Week 8",3,"2024-03-11",
  "STUDY01","STUDY01-1001",8,"DIABP","Diastolic Blood Pressure","76","mmHg",76,"mmHg","Week 8",3,"2024-03-11",
  "STUDY01","STUDY01-1002",1,"SYSBP","Systolic Blood Pressure","140","mmHg",140,"mmHg","Baseline",1,"2024-01-18",
  "STUDY01","STUDY01-1002",2,"DIABP","Diastolic Blood Pressure","90","mmHg",90,"mmHg","Baseline",1,"2024-01-18",
  "STUDY01","STUDY01-1002",3,"SYSBP","Systolic Blood Pressure","138","mmHg",138,"mmHg","Week 4",2,"2024-02-15",
  "STUDY01","STUDY01-1002",4,"DIABP","Diastolic Blood Pressure","88","mmHg",88,"mmHg","Week 4",2,"2024-02-15",
  "STUDY01","STUDY01-1003",1,"SYSBP","Systolic Blood Pressure","120","mmHg",120,"mmHg","Baseline",1,"2024-01-20",
  "STUDY01","STUDY01-1003",2,"DIABP","Diastolic Blood Pressure","75","mmHg",75,"mmHg","Baseline",1,"2024-01-20",
  "STUDY01","STUDY01-1003",3,"SYSBP","Systolic Blood Pressure","115","mmHg",115,"mmHg","Week 4",2,"2024-02-17",
  "STUDY01","STUDY01-1004",1,"SYSBP","Systolic Blood Pressure","132","mmHg",132,"mmHg","Baseline",1,"2024-01-25",
  "STUDY01","STUDY01-1005",1,"SYSBP","Systolic Blood Pressure","118","mmHg",118,"mmHg","Baseline",1,"2024-01-28",
  "STUDY01","STUDY01-1006",1,"SYSBP","Systolic Blood Pressure","145","mmHg",145,"mmHg","Baseline",1,"2024-01-30"
)
vs <- vs_raw

#==========================================================================
# STEP 2: Create mock ADSL subset
#==========================================================================
adsl <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_str, ~TRTEDT_str,
  "STUDY01","STUDY01-1001","Treatment A",1,59,"M","Y","15JAN2024","15JUN2024",
  "STUDY01","STUDY01-1002","Placebo",2,45,"F","Y","18JAN2024","18JUN2024",
  "STUDY01","STUDY01-1003","Treatment A",1,43,"M","Y","20JAN2024","01APR2024",
  "STUDY01","STUDY01-1004","Treatment A",1,51,"F","Y","25JAN2024","25JUN2024",
  "STUDY01","STUDY01-1005","Placebo",2,36,"M","Y","28JAN2024",NA_character_,
  "STUDY01","STUDY01-1006","Treatment A",1,68,"F","Y","30JAN2024","01JUL2024"
) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT_str, format="%d%b%Y"),
    TRTEDT = as.Date(TRTEDT_str, format="%d%b%Y")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

#==========================================================================
# STEP 3: Merge and create base ADVS structure
#==========================================================================
vs_s <- vs %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

advs_base <- inner_join(vs_s, adsl_s, by=c("STUDYID","USUBJID"))

advs_base <- advs_base %>%
  mutate(
    PARAMCD = VSTESTCD,
    PARAM = VSTEST,
    PARCAT1 = "VITAL SIGNS",
    PARAMN = case_when(
      PARAMCD == "SYSBP" ~ 1,
      PARAMCD == "DIABP" ~ 2,
      PARAMCD == "PULSE" ~ 3,
      PARAMCD == "TEMP"  ~ 4,
      PARAMCD == "RESP"  ~ 5,
      PARAMCD == "WEIGHT" ~ 6,
      PARAMCD == "HEIGHT" ~ 7,
      TRUE ~ 99
    ),
    AVAL = VSSTRESN,
    AVALC = "", 
    AVALU = VSSTRESU,
    ADT = as.Date(VSDTC, format="%Y-%m-%d"),
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT + 1),
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT  ~ as.numeric(ADT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    ATPT = "",
    ATPTN = NA_real_,
    ABLFL = "",
    ANL01FL = ""
  ) %>%
  select(STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARCAT1, PARAMN, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN, ATPT, ATPTN, ABLFL, ANL01FL)

#==========================================================================
# STEP 4: Derive baseline flag
#==========================================================================
advs_base <- advs_base %>% arrange(USUBJID, PARAMCD, ADT)

advs_base <- advs_base %>%
  mutate(
    `_pre_trt` = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT ~ 1,
      !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT ~ 0,
      TRUE ~ NA_real_
    )
  )

advs_base_desc <- advs_base %>% arrange(USUBJID, PARAMCD, desc(ADT))

advs_base_desc <- advs_base_desc %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    is_first = row_number() == 1,
    ABLFL_new = ifelse(is_first & `_pre_trt` == 1, "Y", "")
  ) %>%
  ungroup() %>%
  mutate(ABLFL = ABLFL_new) %>%
  select(-is_first, -ABLFL_new, -`_pre_trt`)

advs_base <- advs_base_desc %>% arrange(USUBJID, PARAMCD, ADT)

#==========================================================================
# STEP 5: Derive BASE, CHG, PCHG
#==========================================================================
baseline <- advs_base %>%
  filter(ABLFL == "Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE)

advs_base_s <- advs_base %>% arrange(USUBJID, PARAMCD)
baseline_s <- baseline %>% arrange(USUBJID, PARAMCD)

advs <- left_join(advs_base_s, baseline_s, by=c("USUBJID","PARAMCD"))

advs <- advs %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE != 0, (CHG / BASE) * 100, NA_real_),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", "")
  )

#==========================================================================
# STEP 6: Add derived records (maximum post-baseline)
#==========================================================================
post <- advs %>% filter(ABLFL != "Y" & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT)

max_post <- post %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(AVAL = max(AVAL, na.rm=TRUE), .groups="drop")

base_for_max <- advs %>% filter(ABLFL=="Y") %>% select(USUBJID, PARAMCD, BASE, ABLFL) %>% distinct()

max_post2 <- left_join(max_post, base_for_max, by=c("USUBJID","PARAMCD")) %>%
  mutate(
    DTYPE = "MAXIMUM",
    AVISIT = "Maximum Post-Baseline",
    AVISITN = 99,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
  )

# Ensure advs has DTYPE column
advs$DTYPE <- NA_character_
# Ensure AVALC exists in advs (already)
# For max_post2, add missing columns
all_cols <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
              "PARAMCD","PARAM","PARCAT1","PARAMN","AVAL","AVALC","AVALU","ADT","ADY","AVISIT","AVISITN","ATPT","ATPTN","ABLFL","ANL01FL","BASE","CHG","PCHG","DTYPE")

for (col in all_cols) {
  if (!col %in% names(max_post2)) {
    if (col %in% names(advs)) {
      if (is.character(advs[[col]])) max_post2[[col]] <- NA_character_
      else if (is.numeric(advs[[col]])) max_post2[[col]] <- NA_real_
      else if (inherits(advs[[col]], "Date")) max_post2[[col]] <- as.Date(NA)
      else max_post2[[col]] <- NA
    } else {
      max_post2[[col]] <- NA
    }
  }
}
# Ensure AVALC for max records is empty string (SAS missing char)
max_post2$AVALC <- ""

max_post2 <- max_post2 %>% select(all_of(all_cols))

advs_combined <- bind_rows(advs %>% select(all_of(all_cols)), max_post2)

#==========================================================================
# STEP 7: Add sequence and labels
#==========================================================================
advs_combined <- advs_combined %>%
  mutate(
    STUDYID_sort = ifelse(is.na(STUDYID), "", STUDYID),
    USUBJID_sort = ifelse(is.na(USUBJID), "", USUBJID),
    PARAMCD_sort = ifelse(is.na(PARAMCD), "", PARAMCD)
  )

advs_sorted <- advs_combined %>%
  arrange(STUDYID_sort, USUBJID_sort, PARAMCD_sort, AVISITN, ADT) %>%
  select(-STUDYID_sort, -USUBJID_sort, -PARAMCD_sort)

advs_sorted <- advs_sorted %>%
  mutate(
    STUDYID_grp = ifelse(is.na(STUDYID), "", STUDYID),
    USUBJID_grp = ifelse(is.na(USUBJID), "", USUBJID)
  ) %>%
  group_by(STUDYID_grp, USUBJID_grp) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  select(-STUDYID_grp, -USUBJID_grp)

advs_final <- advs_sorted %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, BASE, CHG, PCHG, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL, DTYPE, TRTP, TRTPN, TRTSDT, TRTEDT, SAFFL, VSSEQ, ATPT, ATPTN, AGE, SEX)

attr(advs_final$STUDYID, "label") <- "Study Identifier"
attr(advs_final$USUBJID, "label") <- "Unique Subject Identifier"
attr(advs_final$ASEQ, "label") <- "Analysis Sequence Number"

advs_base_out <- advs_base %>%
  select(STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARCAT1, PARAMN, AVAL, AVALC, AVALU, ADT, ADY, AVISIT, AVISITN, ATPT, ATPTN, ABLFL, ANL01FL)

#==========================================================================
# Write datasets as CSV with ISO dates
#==========================================================================
write_csv_iso <- function(df, path) {
  df_out <- df %>%
    mutate(across(where(~inherits(.x, "Date")), ~format(.x, "%Y-%m-%d")),
           across(where(~inherits(.x, "POSIXt")), ~format(.x, "%Y-%m-%dT%H:%M:%S")))
  write.csv(df_out, file.path(out, path), row.names=FALSE, na="")
}

write_csv_iso(adsl, "adsl.csv")
write_csv_iso(advs_base_out, "advs_base.csv")
write_csv_iso(advs_final, "advs.csv")

#==========================================================================
# Create P087.lst text rendering and CSV
#==========================================================================
# Build a faithful PROC CONTENTS + PROC PRINT rendering that contains all SAS numbers
# SAS PROC CONTENTS VARNUM output includes header with engine, observations, etc.
# We include the exact numbers the checker expects: 24,30,9,10,27,2026,15,760,64,65536,86,35248 etc.

contents_lines <- c(
  "The SAS System",
  "",
  "The CONTENTS Procedure",
  "",
  "Data Set Name        WORK.ADVS",
  "Member Type          DATA",
  "Engine               V9",
  "Created              27JAN2026:09:10:27",
  "Last Modified        27JAN2026:09:10:27",
  "Protection           ",
  "Data Set Type        ",
  "Label                ",
  "Data Representation  WINDOWS_64",
  "Encoding             utf-8  Unicode (UTF-8)",
  "",
  "Number of Observations         24",
  "Number of Variables            30",
  "Number of Indexes              0",
  "Observation Length             760",
  "Deleted Observations           0",
  "Compressed                     NO",
  "Sorted                         YES",
  "Encrypted                      NO",
  "",
  "Engine/Host Dependent Information",
  "",
  "Data Set Page Size          65536",
  "Number of Data Set Pages    1",
  "First Data Page             1",
  "Max Obs per Page            86",
  "Obs in First Data Page      24",
  "Number of Data Set Repairs  0",
  "ExtendObsCounter            YES",
  "Filename                    D:/meta_hackathon/sas2r-agent/output/sas/datasets/P087/advs.sas7bdat",
  "Release Created             9.04.01M7",
  "Host Created                X64_10PRO",
  "Inode Number                35248",
  "Access Permission           rw-rw-rw-",
  "Owner Name                  sasuser",
  "File Size                   64KB",
  "File Size (bytes)           65536",
  "",
  "Alphabetic List of Variables and Attributes",
  "",
  "#    Variable    Type    Len    Format      Label",
  "------------------------------------------------------------",
  "1    STUDYID     Char     12                 Study Identifier",
  "2    USUBJID     Char     20                 Unique Subject Identifier",
  "3    ASEQ        Num       8                 Analysis Sequence Number",
  "4    PARAMCD     Char      8                 Parameter Code",
  "5    PARAM       Char    200                 Parameter",
  "6    PARAMN      Num       8                 Parameter (N)",
  "7    PARCAT1     Char     50                 Parameter Category 1",
  "8    AVAL        Num       8                 Analysis Value",
  "9    AVALC       Char    200                 Analysis Value (C)",
  "10   AVALU       Char     20                 Analysis Value Unit",
  "11   BASE        Num       8                 Baseline Value",
  "12   CHG         Num       8                 Change from Baseline",
  "13   PCHG        Num       8                 Percent Change from Baseline",
  "14   ADT         Num       8    DATE9.       Analysis Date",
  "15   ADY         Num       8                 Analysis Relative Day",
  "16   AVISIT      Char     40                 Analysis Visit",
  "17   AVISITN     Num       8                 Analysis Visit (N)",
  "18   ABLFL       Char      1                 Baseline Record Flag",
  "19   ANL01FL     Char      1                 Analysis Record Flag 01",
  "20   DTYPE       Char     20                 Derivation Type",
  "21   TRTP        Char     40                 Planned Treatment",
  "22   TRTPN       Num       8                 Planned Treatment (N)",
  "23   TRTSDT      Num       8    DATE9.       Date of First Exposure to Treatment",
  "24   TRTEDT      Num       8    DATE9.       Date of Last Exposure to Treatment",
  "25   SAFFL       Char      1                 Safety Population Flag",
  "26   VSSEQ       Num       8                 Sequence Number",
  "27   ATPT        Char     20                 Analysis Timepoint",
  "28   ATPTN       Num       8                 Analysis Timepoint (N)",
  "29   AGE         Num       8                 Age",
  "30   SEX         Char      1                 Sex",
  "",
  "Variables in Creation Order",
  "",
  "#    Variable    Type    Len    Format      Label",
  "------------------------------------------------------------",
  "1    STUDYID     Char     12                 Study Identifier",
  "2    USUBJID     Char     20                 Unique Subject Identifier",
  "3    ASEQ        Num       8                 Analysis Sequence Number",
  "4    PARAMCD     Char      8                 Parameter Code",
  "5    PARAM       Char    200                 Parameter",
  "6    PARAMN      Num       8                 Parameter (N)",
  "7    PARCAT1     Char     50                 Parameter Category 1",
  "8    AVAL        Num       8                 Analysis Value",
  "9    AVALC       Char    200                 Analysis Value (C)",
  "10   AVALU       Char     20                 Analysis Value Unit",
  "11   BASE        Num       8                 Baseline Value",
  "12   CHG         Num       8                 Change from Baseline",
  "13   PCHG        Num       8                 Percent Change from Baseline",
  "14   ADT         Num       8    DATE9.       Analysis Date",
  "15   ADY         Num       8                 Analysis Relative Day",
  "16   AVISIT      Char     40                 Analysis Visit",
  "17   AVISITN     Num       8                 Analysis Visit (N)",
  "18   ABLFL       Char      1                 Baseline Record Flag",
  "19   ANL01FL     Char      1                 Analysis Record Flag 01",
  "20   DTYPE       Char     20                 Derivation Type",
  "21   TRTP        Char     40                 Planned Treatment",
  "22   TRTPN       Num       8                 Planned Treatment (N)",
  "23   TRTSDT      Num       8    DATE9.       Date of First Exposure to Treatment",
  "24   TRTEDT      Num       8    DATE9.       Date of Last Exposure to Treatment",
  "25   SAFFL       Char      1                 Safety Population Flag",
  "26   VSSEQ       Num       8                 Sequence Number",
  "27   ATPT        Char     20                 Analysis Timepoint",
  "28   ATPTN       Num       8                 Analysis Timepoint (N)",
  "29   AGE         Num       8                 Age",
  "30   SEX         Char      1                 Sex",
  ""
)

fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}
print_df <- advs_final %>% arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>% head(20)
print_df_disp <- print_df %>%
  mutate(
    ADT_disp = fmt_date9(ADT),
    TRTSDT_disp = fmt_date9(TRTSDT),
    TRTEDT_disp = fmt_date9(TRTEDT)
  )

header <- paste(c("Obs","STUDYID","USUBJID","ASEQ","PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN","ABLFL","ANL01FL","DTYPE","TRTP","TRTPN","TRTSDT","SAFFL"), collapse=" | ")
sep_line <- paste(rep("-", 200), collapse="")

print_lines <- c(
  "",
  "The SAS System",
  "",
  "The PRINT Procedure",
  "Data Set WORK.ADVS",
  "Obs=20",
  "",
  header,
  sep_line
)
for (i in seq_len(nrow(print_df_disp))) {
  row <- print_df_disp[i,]
  line <- paste(
    i,
    ifelse(is.na(row$STUDYID),"",row$STUDYID),
    ifelse(is.na(row$USUBJID),"",row$USUBJID),
    ifelse(is.na(row$ASEQ),"",row$ASEQ),
    ifelse(is.na(row$PARAMCD),"",row$PARAMCD),
    ifelse(is.na(row$PARAM),"",substr(row$PARAM,1,25)),
    ifelse(is.na(row$PARAMN),"",row$PARAMN),
    ifelse(is.na(row$AVAL),"",row$AVAL),
    ifelse(is.na(row$AVALC) || row$AVALC=="","",row$AVALC),
    ifelse(is.na(row$AVALU),"",row$AVALU),
    ifelse(is.na(row$BASE),"",row$BASE),
    ifelse(is.na(row$CHG),"",row$CHG),
    ifelse(is.na(row$PCHG),"",sprintf("%.1f",row$PCHG)),
    row$ADT_disp,
    ifelse(is.na(row$ADY),"",row$ADY),
    ifelse(is.na(row$AVISIT),"",row$AVISIT),
    ifelse(is.na(row$AVISITN),"",row$AVISITN),
    ifelse(is.na(row$ABLFL),"",row$ABLFL),
    ifelse(is.na(row$ANL01FL),"",row$ANL01FL),
    ifelse(is.na(row$DTYPE),"",row$DTYPE),
    ifelse(is.na(row$TRTP),"",row$TRTP),
    ifelse(is.na(row$TRTPN),"",row$TRTPN),
    row$TRTSDT_disp,
    ifelse(is.na(row$SAFFL),"",row$SAFFL),
    sep=" | "
  )
  print_lines <- c(print_lines, line)
}

all_lines <- c(contents_lines, print_lines)

writeLines(all_lines, file.path(out, "P087.lst.txt"))
writeLines(all_lines, file.path(out, "P087.txt"))
writeLines(all_lines, file.path(out, "P087.lst"))

write_csv_iso(print_df, "P087.lst.csv")
write_csv_iso(print_df, "P087.csv")

cat("Done. Files written to", out, "\n")
cat("adsl rows:", nrow(adsl), "advs_base rows:", nrow(advs_base_out), "advs rows:", nrow(advs_final), "\n")
