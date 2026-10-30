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

# Helper: SAS date9. to Date
# Helper to format dates for CSV as YYYY-MM-DD
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
# SAS: proc sort by STUDYID USUBJID then merge vs(in=a) adsl(in=b) by STUDYID USUBJID if a and b
vs_s <- vs %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

advs_base <- inner_join(vs_s, adsl_s, by=c("STUDYID","USUBJID"))

# Map to ADaM PARAM structure
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
         PARAMCD, PARAM, PARCAT1, PARAMN, AVAL, AVALU, ADT, ADY, AVISIT, AVISITN, ATPT, ATPTN, ABLFL, ANL01FL)

#==========================================================================
# STEP 4: Derive baseline flag
#==========================================================================
# SAS logic:
# sort by USUBJID PARAMCD ADT, then create _pre_trt = 1 if ADT <= TRTSDT else 0
# then sort descending ADT, then if first.PARAMCD and _pre_trt=1 then ABLFL='Y' else ''

# First sort ascending
advs_base <- advs_base %>% arrange(USUBJID, PARAMCD, ADT)

advs_base <- advs_base %>%
  mutate(
    `_pre_trt` = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT ~ 1,
      !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT ~ 0,
      TRUE ~ NA_real_
    )
  )

# Sort descending ADT within USUBJID PARAMCD
advs_base_desc <- advs_base %>% arrange(USUBJID, PARAMCD, desc(ADT))

# Identify first.PARAMCD
advs_base_desc <- advs_base_desc %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    is_first = row_number() == 1,
    ABLFL_new = ifelse(is_first & `_pre_trt` == 1, "Y", "")
  ) %>%
  ungroup() %>%
  mutate(ABLFL = ABLFL_new) %>%
  select(-is_first, -ABLFL_new, -`_pre_trt`)

# Sort back ascending
advs_base <- advs_base_desc %>% arrange(USUBJID, PARAMCD, ADT)

#==========================================================================
# STEP 5: Derive BASE, CHG, PCHG
#==========================================================================
baseline <- advs_base %>%
  filter(ABLFL == "Y") %>%
  mutate(BASE = AVAL) %>%
  select(USUBJID, PARAMCD, BASE)

# Merge baseline - SAS proc sort then merge by USUBJID PARAMCD
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
# proc means where ABLFL ne 'Y' and ADT > TRTSDT by USUBJID PARAMCD max AVAL
post <- advs %>% filter(ABLFL != "Y" & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT)

max_post <- post %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(AVAL = max(AVAL, na.rm=TRUE), .groups="drop")

# Merge with baseline to get BASE and ABLFL
# SAS: merge _max_post(in=a) advs(where=(ABLFL='Y') keep=ABLFL USUBJID PARAMCD BASE) by USUBJID PARAMCD if a
base_for_max <- advs %>% filter(ABLFL=="Y") %>% select(USUBJID, PARAMCD, BASE, ABLFL) %>% distinct()

max_post2 <- left_join(max_post, base_for_max, by=c("USUBJID","PARAMCD")) %>%
  mutate(
    DTYPE = "MAXIMUM",
    AVISIT = "Maximum Post-Baseline",
    AVISITN = 99,
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
    # ABLFL remains as from base_for_max (Y) - replicate SAS
  )

# SAS sets advs = advs _max_post (concatenate)
# Need to align columns: advs has many columns, max_post2 has subset. Missing columns will be NA/"".
# Ensure all columns exist in max_post2 for rbind
# Columns in advs: STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
# PARAMCD, PARAM, PARCAT1, PARAMN, AVAL, AVALU, ADT, ADY, AVISIT, AVISITN, ATPT, ATPTN, ABLFL, ANL01FL, BASE, CHG, PCHG
# For max_post2, fill missing with NA

# Add missing columns to max_post2
all_cols <- c("STUDYID","USUBJID","VSSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
              "PARAMCD","PARAM","PARCAT1","PARAMN","AVAL","AVALU","ADT","ADY","AVISIT","AVISITN","ATPT","ATPTN","ABLFL","ANL01FL","BASE","CHG","PCHG","DTYPE")

# Ensure advs has DTYPE column (initially NA)
advs$DTYPE <- NA_character_

# Create max_post2 with all columns
for (col in all_cols) {
  if (!col %in% names(max_post2)) {
    # determine type from advs
    if (col %in% names(advs)) {
      # create NA of appropriate type
      if (is.character(advs[[col]])) max_post2[[col]] <- NA_character_
      else if (is.numeric(advs[[col]])) max_post2[[col]] <- NA_real_
      else if (inherits(advs[[col]], "Date")) max_post2[[col]] <- as.Date(NA)
      else max_post2[[col]] <- NA
    } else {
      max_post2[[col]] <- NA
    }
  }
}
# For character columns that should be "" not NA? SAS missing char is "".
# But for max rows, SAS will have missing char as ""? Actually after merge, missing char is ""? In R we use NA then later write as "".
# Keep as NA for now, will be written as "".

# Reorder max_post2 columns to match advs
max_post2 <- max_post2 %>% select(all_of(all_cols))

# Combine
advs_combined <- bind_rows(advs %>% select(all_of(all_cols)), max_post2)

#==========================================================================
# STEP 7: Add sequence and labels
#==========================================================================
# SAS: proc sort by STUDYID USUBJID PARAMCD AVISITN ADT
# Note: STUDYID missing (NA) sorts first. In R, NA sorts last by default, need to emulate SAS missing first.
# SAS missing character '' sorts before 'STUDY01'. So we need to treat NA as "" and sort accordingly.
advs_combined <- advs_combined %>%
  mutate(
    STUDYID_sort = ifelse(is.na(STUDYID), "", STUDYID),
    USUBJID_sort = ifelse(is.na(USUBJID), "", USUBJID),
    PARAMCD_sort = ifelse(is.na(PARAMCD), "", PARAMCD)
  )

advs_sorted <- advs_combined %>%
  arrange(STUDYID_sort, USUBJID_sort, PARAMCD_sort, AVISITN, ADT) %>%
  select(-STUDYID_sort, -USUBJID_sort, -PARAMCD_sort)

# SAS: by STUDYID USUBJID; if first.USUBJID then ASEQ=0; ASEQ+1;
# Need to emulate BY group with missing STUDYID as distinct group.
# In SAS, BY STUDYID USUBJID means groups defined by both. So we group by STUDYID (with NA as "") and USUBJID
advs_sorted <- advs_sorted %>%
  mutate(
    STUDYID_grp = ifelse(is.na(STUDYID), "", STUDYID),
    USUBJID_grp = ifelse(is.na(USUBJID), "", USUBJID)
  ) %>%
  group_by(STUDYID_grp, USUBJID_grp) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  select(-STUDYID_grp, -USUBJID_grp)

# Reorder columns for final advs: include ASEQ at end
advs_final <- advs_sorted %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, BASE, CHG, PCHG, ADT, ADY, AVISIT, AVISITN, ABLFL, ANL01FL, DTYPE, TRTP, TRTPN, TRTSDT, TRTEDT, SAFFL, VSSEQ, ATPT, ATPTN, AGE, SEX)

# For CSV output, ensure column order matches SAS varnum (approx). Use the order above but also include all.
# Create advs_base for output (before max records, before ASEQ)
advs_base_out <- advs_base %>%
  select(STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARCAT1, PARAMN, AVAL, AVALU, ADT, ADY, AVISIT, AVISITN, ATPT, ATPTN, ABLFL, ANL01FL)

# For advs.csv, use advs_final but also ensure dates formatted later
# Add labels as attributes (optional)
attr(advs_final$STUDYID, "label") <- "Study Identifier"
attr(advs_final$USUBJID, "label") <- "Unique Subject Identifier"
attr(advs_final$ASEQ, "label") <- "Analysis Sequence Number"

#==========================================================================
# Write datasets as CSV with ISO dates
#==========================================================================
write_csv_iso <- function(df, path) {
  df_out <- df %>%
    mutate(across(where(~inherits(.x, "Date")), ~format(.x, "%Y-%m-%d")),
           across(where(~inherits(.x, "POSIXt")), ~format(.x, "%Y-%m-%dT%H:%M:%S")))
  # Replace NA with "" for character columns will be handled by write.csv na=""
  write.csv(df_out, file.path(out, path), row.names=FALSE, na="")
}

# adsl
write_csv_iso(adsl, "adsl.csv")
# advs_base
write_csv_iso(advs_base_out, "advs_base.csv")
# advs
write_csv_iso(advs_final, "advs.csv")

#==========================================================================
# Create P087.lst text rendering and CSV
#==========================================================================
# SAS does proc contents varnum and proc print obs=20
# Build text file containing contents and print

# Build contents info
contents_lines <- c(
  "Procedure Contents for ADVS",
  "----------------------------------------",
  paste0("Observations: ", nrow(advs_final)),
  paste0("Variables: ", ncol(advs_final)),
  "",
  "Variable  Label",
  "----------------------------------------"
)
var_labels <- c(
  "STUDYID  = Study Identifier",
  "USUBJID  = Unique Subject Identifier",
  "ASEQ     = Analysis Sequence Number",
  "PARAMCD  = Parameter Code",
  "PARAM    = Parameter",
  "PARAMN   = Parameter (N)",
  "PARCAT1  = Parameter Category 1",
  "AVAL     = Analysis Value",
  "AVALU    = Analysis Value Unit",
  "BASE     = Baseline Value",
  "CHG      = Change from Baseline",
  "PCHG     = Percent Change from Baseline",
  "ADT      = Analysis Date",
  "ADY      = Analysis Relative Day",
  "AVISIT   = Analysis Visit",
  "AVISITN  = Analysis Visit (N)",
  "ABLFL    = Baseline Record Flag",
  "ANL01FL  = Analysis Record Flag 01",
  "DTYPE    = Derivation Type",
  "TRTP     = Planned Treatment",
  "TRTPN    = Planned Treatment (N)",
  "TRTSDT   = Date of First Exposure to Treatment",
  "SAFFL    = Safety Population Flag"
)
contents_lines <- c(contents_lines, var_labels, "")

# Build print lines (obs=20)
print_df <- advs_final %>% arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>% head(20)
# For display, format dates as DATE9. like 15JAN2024
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}
print_df_disp <- print_df %>%
  mutate(
    ADT_disp = fmt_date9(ADT),
    TRTSDT_disp = fmt_date9(TRTSDT),
    TRTEDT_disp = fmt_date9(TRTEDT)
  )

# Create text table header
header <- paste(c("STUDYID","USUBJID","ASEQ","PARAMCD","PARAM","AVAL","BASE","CHG","PCHG","ADT","ADY","AVISIT","ABLFL","ANL01FL","DTYPE"), collapse=" | ")
sep_line <- paste(rep("-", nchar(header)), collapse="")

print_lines <- c(
  "Procedure Print for ADVS (obs=20)",
  "----------------------------------------",
  header,
  sep_line
)
for (i in seq_len(nrow(print_df_disp))) {
  row <- print_df_disp[i,]
  line <- paste(
    ifelse(is.na(row$STUDYID),"",row$STUDYID),
    ifelse(is.na(row$USUBJID),"",row$USUBJID),
    ifelse(is.na(row$ASEQ),"",row$ASEQ),
    ifelse(is.na(row$PARAMCD),"",row$PARAMCD),
    ifelse(is.na(row$PARAM),"",substr(row$PARAM,1,20)),
    ifelse(is.na(row$AVAL),"",row$AVAL),
    ifelse(is.na(row$BASE),"",row$BASE),
    ifelse(is.na(row$CHG),"",row$CHG),
    ifelse(is.na(row$PCHG),"",sprintf("%.2f",row$PCHG)),
    row$ADT_disp,
    ifelse(is.na(row$ADY),"",row$ADY),
    ifelse(is.na(row$AVISIT),"",row$AVISIT),
    ifelse(is.na(row$ABLFL),"",row$ABLFL),
    ifelse(is.na(row$ANL01FL),"",row$ANL01FL),
    ifelse(is.na(row$DTYPE),"",row$DTYPE),
    sep=" | "
  )
  print_lines <- c(print_lines, line)
}

all_lines <- c(contents_lines, print_lines)

# Write text rendering
writeLines(all_lines, file.path(out, "P087.lst.txt"))
writeLines(all_lines, file.path(out, "P087.txt"))
# Also write with .lst extension without .txt for compatibility
writeLines(all_lines, file.path(out, "P087.lst"))

# Write CSV of table body (print data)
write_csv_iso(print_df, "P087.lst.csv")
write_csv_iso(print_df, "P087.csv")

# Also ensure dataset CSVs are written with correct names (already done)
# Verify files exist
cat("Done. Files written to", out, "\n")
cat("adsl rows:", nrow(adsl), "advs_base rows:", nrow(advs_base_out), "advs rows:", nrow(advs_final), "\n")
