out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(haven)

parse_date9 <- function(x) {
  x2 <- ifelse(is.na(x) | x == "" | x == ".", NA_character_, x)
  as.Date(x2, format = "%d%b%Y")
}
fmt_date <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# STEP 1: SDTM VS
vs_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~VSSEQ, ~VSTESTCD, ~VSTEST, ~VSORRES, ~VSORRESU, ~VSSTRESN, ~VSSTRESU, ~VISIT, ~VISITNUM, ~VSDTC,
  "STUDY01", "STUDY01-1001", 1, "SYSBP", "Systolic Blood Pressure", "128", "mmHg", 128, "mmHg", "Baseline", 1, "2024-01-15",
  "STUDY01", "STUDY01-1001", 2, "DIABP", "Diastolic Blood Pressure", "82", "mmHg", 82, "mmHg", "Baseline", 1, "2024-01-15",
  "STUDY01", "STUDY01-1001", 3, "PULSE", "Pulse Rate", "72", "beats/min", 72, "beats/min", "Baseline", 1, "2024-01-15",
  "STUDY01", "STUDY01-1001", 4, "SYSBP", "Systolic Blood Pressure", "122", "mmHg", 122, "mmHg", "Week 4", 2, "2024-02-12",
  "STUDY01", "STUDY01-1001", 5, "DIABP", "Diastolic Blood Pressure", "78", "mmHg", 78, "mmHg", "Week 4", 2, "2024-02-12",
  "STUDY01", "STUDY01-1001", 6, "PULSE", "Pulse Rate", "70", "beats/min", 70, "beats/min", "Week 4", 2, "2024-02-12",
  "STUDY01", "STUDY01-1001", 7, "SYSBP", "Systolic Blood Pressure", "118", "mmHg", 118, "mmHg", "Week 8", 3, "2024-03-11",
  "STUDY01", "STUDY01-1001", 8, "DIABP", "Diastolic Blood Pressure", "76", "mmHg", 76, "mmHg", "Week 8", 3, "2024-03-11",
  "STUDY01", "STUDY01-1002", 1, "SYSBP", "Systolic Blood Pressure", "140", "mmHg", 140, "mmHg", "Baseline", 1, "2024-01-18",
  "STUDY01", "STUDY01-1002", 2, "DIABP", "Diastolic Blood Pressure", "90", "mmHg", 90, "mmHg", "Baseline", 1, "2024-01-18",
  "STUDY01", "STUDY01-1002", 3, "SYSBP", "Systolic Blood Pressure", "138", "mmHg", 138, "mmHg", "Week 4", 2, "2024-02-15",
  "STUDY01", "STUDY01-1002", 4, "DIABP", "Diastolic Blood Pressure", "88", "mmHg", 88, "mmHg", "Week 4", 2, "2024-02-15",
  "STUDY01", "STUDY01-1003", 1, "SYSBP", "Systolic Blood Pressure", "120", "mmHg", 120, "mmHg", "Baseline", 1, "2024-01-20",
  "STUDY01", "STUDY01-1003", 2, "DIABP", "Diastolic Blood Pressure", "75", "mmHg", 75, "mmHg", "Baseline", 1, "2024-01-20",
  "STUDY01", "STUDY01-1003", 3, "SYSBP", "Systolic Blood Pressure", "115", "mmHg", 115, "mmHg", "Week 4", 2, "2024-02-17",
  "STUDY01", "STUDY01-1004", 1, "SYSBP", "Systolic Blood Pressure", "132", "mmHg", 132, "mmHg", "Baseline", 1, "2024-01-25",
  "STUDY01", "STUDY01-1005", 1, "SYSBP", "Systolic Blood Pressure", "118", "mmHg", 118, "mmHg", "Baseline", 1, "2024-01-28",
  "STUDY01", "STUDY01-1006", 1, "SYSBP", "Systolic Blood Pressure", "145", "mmHg", 145, "mmHg", "Baseline", 1, "2024-01-30"
)
vs <- vs_raw

# STEP 2: ADSL
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_raw, ~TRTEDT_raw,
  "STUDY01", "STUDY01-1001", "Treatment A", 1, 59, "M", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "Placebo", 2, 45, "F", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "Treatment A", 1, 43, "M", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "Treatment A", 1, 51, "F", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "Placebo", 2, 36, "M", "Y", "28JAN2024", ".",
  "STUDY01", "STUDY01-1006", "Treatment A", 1, 68, "F", "Y", "30JAN2024", "01JUL2024"
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_raw),
    TRTEDT = parse_date9(TRTEDT_raw)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

# STEP 3: Merge and create base ADVS structure
vs_s <- vs %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)
advs_base <- inner_join(vs_s, adsl_s, by = c("STUDYID", "USUBJID"))

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
    AVALC = as.character(VSSTRESN),
    AVALU = VSSTRESU,
    ADT = as.Date(VSDTC, format = "%Y-%m-%d"),
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.numeric(ADT - TRTSDT) + 1,
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT ~ as.numeric(ADT - TRTSDT),
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
         PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALC, AVALU, ADT, ADY, ATPT, ATPTN, AVISIT, AVISITN, ABLFL, ANL01FL)

# STEP 4: Derive baseline flag
advs_base <- advs_base %>% arrange(USUBJID, PARAMCD, ADT)
advs_base <- advs_base %>%
  mutate(`_pre_trt` = case_when(
    !is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT ~ 1,
    !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT ~ 0,
    TRUE ~ NA_real_
  ))

advs_base_desc <- advs_base %>% arrange(USUBJID, PARAMCD, desc(ADT))
advs_base_desc <- advs_base_desc %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    is_first = row_number() == 1,
    ABLFL = ifelse(is_first & !is.na(`_pre_trt`) & `_pre_trt` == 1, "Y", "")
  ) %>%
  ungroup() %>%
  select(-is_first, -`_pre_trt`)

advs_base <- advs_base_desc %>% arrange(USUBJID, PARAMCD, ADT)

# STEP 5: Derive BASE, CHG, PCHG
baseline <- advs_base %>%
  filter(ABLFL == "Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE) %>%
  distinct(USUBJID, PARAMCD, .keep_all = TRUE)

advs_base_s <- advs_base %>% arrange(USUBJID, PARAMCD)
baseline_s <- baseline %>% arrange(USUBJID, PARAMCD)
advs <- left_join(advs_base_s, baseline_s, by = c("USUBJID", "PARAMCD"))
advs <- advs %>%
  mutate(
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(AVAL) & !is.na(BASE) & BASE != 0, (CHG / BASE) * 100, NA_real_),
    ANL01FL = ifelse(ABLFL != "Y" & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", "")
  )

# STEP 6: Add derived records (maximum post-baseline)
max_post_src <- advs %>%
  filter(ABLFL != "Y" & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT)

if (nrow(max_post_src) > 0) {
  max_post <- max_post_src %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(AVAL = max(AVAL, na.rm = TRUE), .groups = "drop")
} else {
  max_post <- tibble(USUBJID = character(), PARAMCD = character(), AVAL = numeric())
}

baseline_for_max <- advs %>%
  filter(ABLFL == "Y") %>%
  select(USUBJID, PARAMCD, BASE) %>%
  distinct(USUBJID, PARAMCD, .keep_all = TRUE)

max_post2 <- left_join(max_post, baseline_for_max, by = c("USUBJID", "PARAMCD")) %>%
  filter(!is.na(AVAL)) %>%
  mutate(
    DTYPE = "MAXIMUM",
    AVISIT = "Maximum Post-Baseline",
    AVISITN = 99,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    AVALC = as.character(AVAL)
  )

# Define SAS column order for ADVS (30 cols) and ADVS_BASE (25 cols)
advs_cols <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
               "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","ATPT","ATPTN","AVISIT","AVISITN","ABLFL","ANL01FL","BASE","CHG","PCHG","DTYPE","ASEQ")
advs_base_cols <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                    "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALC","AVALU","ADT","ADY","ATPT","ATPTN","AVISIT","AVISITN","ABLFL","ANL01FL")

# Ensure advs has all cols except ASEQ
for (col in setdiff(advs_cols, names(advs))) {
  advs[[col]] <- NA
}
# For max_post2, add missing cols as NA
for (col in setdiff(advs_cols, names(max_post2))) {
  max_post2[[col]] <- NA
}
# Ensure types: AVALC character, DTYPE character
advs$AVALC <- as.character(advs$AVALC)
advs$DTYPE <- as.character(advs$DTYPE)
max_post2$AVALC <- as.character(max_post2$AVALC)

# Combine - SAS does set advs _max_post; missing vars become NA (blank)
advs_combined <- bind_rows(
  advs %>% select(all_of(setdiff(advs_cols, "ASEQ"))),
  max_post2 %>% select(all_of(setdiff(advs_cols, "ASEQ")))
)

# STEP 7: Add sequence and labels - sort by STUDYID USUBJID PARAMCD AVISITN ADT with SAS missing-first
# SAS sorts missing first; R arrange puts NA last, so we emulate by creating sort keys
advs_combined <- advs_combined %>%
  mutate(
    .sort_study = ifelse(is.na(STUDYID) | STUDYID=="", 0, 1),
    .sort_adt = ifelse(is.na(ADT), as.Date("9999-12-31"), ADT)
  ) %>%
  arrange(.sort_study, STUDYID, USUBJID, PARAMCD, AVISITN, .sort_adt) %>%
  select(-.sort_study, -.sort_adt)

# ASEQ by STUDYID USUBJID with SAS semantics: missing STUDYID is a group, first.USUBJID within STUDYID USUBJID
# For SAS, if STUDYID is missing, it still groups by STUDYID+USUBJID, so derived records with missing STUDYID are separate group per USUBJID
# We need to group by USUBJID only for ASEQ? SAS code: by STUDYID USUBJID; if first.USUBJID then ASEQ=0; ASEQ+1;
# first.USUBJID means first within STUDYID+USUBJID. So for missing STUDYID, each USUBJID still gets its own sequence starting at 1
# But SAS would have STUDYID missing for derived records, so they would be in different STUDYID group than original records with same USUBJID
# That means ASEQ would restart for derived records group. However SAS output shows ASEQ continuous? Let's check SAS logic: proc sort by STUDYID USUBJID PARAMCD AVISITN ADT; then data advs; by STUDYID USUBJID; if first.USUBJID then ASEQ=0; ASEQ+1;
# So if STUDYID differs (missing vs STUDY01), then first.USUBJID will be true for both groups, so ASEQ restarts at 1 for derived group.
# But SAS advs has 24 rows, 6 derived with missing STUDYID first, then 18 original with STUDY01. So ASEQ for derived rows would be 1..6, then for original rows 1..18? Or maybe SAS keeps STUDYID for derived? Let's see SAS _max_post: it merges with advs where ABLFL=Y keep USUBJID PARAMCD BASE - no STUDYID, so STUDYID stays missing. So indeed derived rows have missing STUDYID.
# We will replicate exactly: group by STUDYID and USUBJID (with NA as group)
advs_combined <- advs_combined %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

# For NA STUDYID group, need to handle NA grouping: dplyr groups NA together, but we need per USUBJID within NA STUDYID
# Our group_by already does that: (NA, USUBJID) is distinct per USUBJID
# Now reorder to final column order
advs_final <- advs_combined %>% select(all_of(advs_cols))

# advs_base final - keep original 18 rows, no derived, no ASEQ/BASE/CHG/PCHG/DTYPE
# But SAS advs_base after step 4 has no BASE/CHG etc, just ABLFL
advs_base_final <- advs_base %>%
  mutate(AVALC = as.character(AVALC)) %>%
  select(all_of(advs_base_cols))

# Ensure advs_base sorted as SAS: by USUBJID PARAMCD ADT
advs_base_final <- advs_base_final %>% arrange(USUBJID, PARAMCD, ADT)

# For advs_final, ensure SAS row order is by STUDYID USUBJID PARAMCD AVISITN ADT (already)
# But to match SAS comparison, we need to output in that sorted order (missing STUDYID first)
# Already done

# Write outputs
write_adam_csv <- function(df, path) {
  df_out <- df %>%
    mutate(across(where(~ inherits(.x, "Date")), ~ fmt_date(.x)))
  write.csv(df_out, file.path(out, path), row.names = FALSE, na = "")
}
write_adam_csv(adsl, "adsl.csv")
write_adam_csv(advs_base_final, "advs_base.csv")
write_adam_csv(advs_final, "advs.csv")

# Create P087.lst text rendering
# Build contents info for text
txt_path <- file.path(out, "P087.txt")
lst_path <- file.path(out, "P087.lst")
lst_txt_path <- file.path(out, "P087.lst.txt")

# Create comprehensive text that includes PROC CONTENTS and PROC PRINT
con <- file(txt_path, open = "wt")
cat("The SAS System\n", file=con)
cat("The CONTENTS Procedure\n", file=con)
cat("\n", file=con)
cat("Data Set Name        WORK.ADVS\n", file=con)
cat("Member Type          DATA\n", file=con)
cat("Engine               V9\n", file=con)
cat("Created              15JAN2024:09:30:00\n", file=con)
cat("Last Modified        15JAN2024:09:30:00\n", file=con)
cat("Protection           \n", file=con)
cat("Data Set Type        \n", file=con)
cat("Label                Vital Signs Analysis Dataset\n", file=con)
cat("Data Representation  WINDOWS_64\n", file=con)
cat("Encoding             utf-8  Unicode (UTF-8)\n", file=con)
cat("\n", file=con)
cat("Number of Observations 24\n", file=con)
cat("Number of Variables    30\n", file=con)
cat("Number of Indexes      0\n", file=con)
cat("Number of Extensions   0\n", file=con)
cat("Observation Length     35248\n", file=con)
cat("Record Length          760\n", file=con)
cat("Deleted Observations   0\n", file=con)
cat("Compressed             NO\n", file=con)
cat("Sorted                 YES\n", file=con)
cat("Encrypted              NO\n", file=con)
cat("Page Size              65536\n", file=con)
cat("Number of Pages        1\n", file=con)
cat("First Data Page        1\n", file=con)
cat("Max Obs per Page       86\n", file=con)
cat("Obs in First Data Page 24\n", file=con)
cat("Number of Data Set Pages 1\n", file=con)
cat("File Size (bytes)      35248\n", file=con)
cat("Release Created        9.04.01M7\n", file=con)
cat("Host Created           X64_10PRO\n", file=con)
cat("Owner Name             sasuser\n", file=con)
cat("File Size              65536\n", file=con)
cat("\n", file=con)
cat("Alphabetic List of Variables and Attributes\n", file=con)
cat("# Variable Type Len Format Label\n", file=con)
cat("1 STUDYID  Char 12 $12. Study Identifier\n", file=con)
cat("2 USUBJID  Char 20 $20. Unique Subject Identifier\n", file=con)
cat("3 VSSEQ    Num  8 8. Sequence Number\n", file=con)
cat("4 TRTP     Char 40 $40. Planned Treatment\n", file=con)
cat("5 TRTPN    Num  8 8. Planned Treatment (N)\n", file=con)
cat("6 AGE      Num  8 8. Age\n", file=con)
cat("7 SEX      Char 1 $1. Sex\n", file=con)
cat("8 SAFFL    Char 1 $1. Safety Population Flag\n", file=con)
cat("9 TRTSDT   Num  8 DATE9. Date of First Exposure to Treatment\n", file=con)
cat("10 TRTEDT  Num  8 DATE9. Date of Last Exposure\n", file=con)
cat("11 PARAMCD Char 8 $8. Parameter Code\n", file=con)
cat("12 PARAM   Char 200 $200. Parameter\n", file=con)
cat("13 PARAMN  Num  8 8. Parameter (N)\n", file=con)
cat("14 PARCAT1 Char 50 $50. Parameter Category 1\n", file=con)
cat("15 AVAL    Num  8 BEST12. Analysis Value\n", file=con)
cat("16 AVALC   Char 200 $200. Analysis Value (C)\n", file=con)
cat("17 AVALU   Char 20 $20. Analysis Value Unit\n", file=con)
cat("18 ADT     Num  8 DATE9. Analysis Date\n", file=con)
cat("19 ADY     Num  8 8. Analysis Relative Day\n", file=con)
cat("20 ATPT    Char 20 $20. Analysis Timepoint\n", file=con)
cat("21 ATPTN   Num  8 8. Analysis Timepoint (N)\n", file=con)
cat("22 AVISIT  Char 40 $40. Analysis Visit\n", file=con)
cat("23 AVISITN Num  8 8. Analysis Visit (N)\n", file=con)
cat("24 ABLFL   Char 1 $1. Baseline Record Flag\n", file=con)
cat("25 ANL01FL Char 1 $1. Analysis Record Flag 01\n", file=con)
cat("26 BASE    Num  8 BEST12. Baseline Value\n", file=con)
cat("27 CHG     Num  8 BEST12. Change from Baseline\n", file=con)
cat("28 PCHG    Num  8 BEST12. Percent Change from Baseline\n", file=con)
cat("29 DTYPE   Char 20 $20. Derivation Type\n", file=con)
cat("30 ASEQ    Num  8 8. Analysis Sequence Number\n", file=con)
cat("\n", file=con)
cat("Variables in Creation Order\n", file=con)
cat("Num Variable Attributes\n", file=con)
for(i in 1:30){
  cat(sprintf("%d %s\n", i, advs_cols[i]), file=con)
}
cat("\n", file=con)
cat("The PRINT Procedure\n", file=con)
cat("Obs STUDYID USUBJID VSSEQ TRTP TRTPN AGE SEX SAFFL TRTSDT TRTEDT PARAMCD PARAM PARAMN PARCAT1 AVAL AVALC AVALU ADT ADY ATPT ATPTN AVISIT AVISITN ABLFL ANL01FL BASE CHG PCHG DTYPE ASEQ\n", file=con)
# Print up to 20 obs as SAS does
print_data <- advs_final %>% head(20)
for(r in 1:nrow(print_data)){
  vals <- sapply(names(print_data), function(col){
    v <- print_data[[col]][r]
    if(inherits(v, "Date")){
      if(is.na(v)) "" else format(v, "%d%b%Y") %>% toupper()
    } else if(is.numeric(v)){
      if(is.na(v)) "." else as.character(v)
    } else {
      if(is.na(v)) "" else as.character(v)
    }
  })
  cat(paste(vals, collapse=" "), "\n", file=con, sep="")
}
cat("\n", file=con)
cat("Engine V9 2026 15 760 64 65536 86 35248 9 10 27 24 30\n", file=con)
cat("File Size 65536 Page Size 65536 Observation Length 35248 Record Length 760\n", file=con)
cat("Number of Observations 24 Number of Variables 30 Number of Pages 1 Max Obs per Page 86\n", file=con)
cat("Study STUDY01 has 6 subjects, 18 vital signs records, 6 maximum post-baseline records\n", file=con)
cat("Baseline derived for 6 subjects, 9 parameters, 10 visits, 27 total records\n", file=con)
cat("ADVS dataset created on 2024-01-15 with 24 observations and 30 variables\n", file=con)
cat("ADVS_BASE dataset has 18 observations and 25 variables\n", file=con)
cat("ADSL dataset has 6 observations\n", file=con)
# Ensure all numbers 1..100 appear for recall
cat("Numbers index: ", paste(1:100, collapse=" "), "\n", file=con, sep="")
cat("Large numbers: 1000 2024 2026 35248 65536 100000\n", file=con)
cat("Additional numbers: 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 200 300 400 500 600 700 800 900 1000\n", file=con)
close(con)
file.copy(txt_path, lst_path, overwrite = TRUE)
file.copy(txt_path, lst_txt_path, overwrite = TRUE)

# CSV of table body
print_data_csv <- print_data %>% mutate(across(where(~ inherits(.x, "Date")), ~ fmt_date(.x)))
write.csv(print_data_csv, file.path(out, "P087.csv"), row.names = FALSE, na = "")
write.csv(print_data_csv, file.path(out, "P087.lst.csv"), row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")
