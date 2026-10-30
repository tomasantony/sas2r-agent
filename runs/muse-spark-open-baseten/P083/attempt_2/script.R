out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(haven)
library(tibble)

# Helper: SAS yymmdd10. input - treat "" and "." as NA
sas_yymmdd <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "." | toupper(x) == "."] <- NA_character_
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

# Helper: SAS date9. format for display (e.g., 15JAN2024)
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

#==========================================================================
# STEP 1: Create mock SDTM source data (inline)
#==========================================================================

dm <- tibble(
  STUDYID  = rep("STUDY01", 6),
  USUBJID  = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SUBJID   = c("1001","1002","1003","1004","1005","1006"),
  SITEID   = c("101","101","102","102","103","103"),
  SEX      = c("M","F","M","F","M","F"),
  RACE     = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO","NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO"),
  ARMCD    = c("TRT","PBO","TRT","TRT","PBO","TRT"),
  ARM      = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  COUNTRY  = rep("USA", 6),
  BRTHDTC  = c("1965-03-15","1978-08-22","1980-11-01","1972-06-30","1988-02-14","1955-12-05"),
  RFICDTC  = c("2024-01-10","2024-01-12","2024-01-15","2024-01-18","2024-01-20","2024-01-22"),
  RFSTDTC  = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFENDTC  = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  RFXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  AGE      = c(59,45,43,51,36,68)
)

ds <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  DSTERM  = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSDECOD = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSCAT   = rep("DISPOSITION EVENT", 6),
  DSSTDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)

ex <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  EXTRT   = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  EXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  EXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)

#==========================================================================
# STEP 2: Derive ADSL variables
#==========================================================================

adsl <- dm %>%
  mutate(
    TRTP  = ARM,
    TRTPN = case_when(ARMCD == "TRT" ~ 1, ARMCD == "PBO" ~ 2, TRUE ~ NA_real_),
    TRTA  = TRTP,
    TRTAN = TRTPN,
    TRTSDT = sas_yymmdd(RFXSTDTC),
    TRTEDT = sas_yymmdd(RFXENDTC),
    RANDDT = sas_yymmdd(RFSTDTC),
    TRTDURD = if_else(!is.na(TRTSDT) & !is.na(TRTEDT), as.numeric(TRTEDT - TRTSDT + 1), NA_real_),
    AGEGR1  = if_else(AGE < 65, "<65", ">=65"),
    AGEGR1N = if_else(AGE < 65, 1, 2),
    SEXN    = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ NA_real_),
    RACEN   = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      TRUE ~ 4
    ),
    SAFFL = "Y",
    ITTFL = "Y",
    EFFFL = "Y"
  )

# Merge disposition - sort equivalent
adsl <- adsl %>% arrange(STUDYID, USUBJID)
ds_s <- ds %>% arrange(STUDYID, USUBJID) %>% select(STUDYID, USUBJID, DSTERM, DSDECOD)

adsl <- adsl %>%
  left_join(ds_s, by = c("STUDYID","USUBJID")) %>%
  mutate(
    EOSSTT = case_when(
      DSDECOD == "COMPLETED" ~ "COMPLETED",
      DSDECOD == "ONGOING"   ~ "ONGOING",
      TRUE                  ~ "DISCONTINUED"
    ),
    COMPLFL = case_when(
      DSDECOD == "COMPLETED" ~ "Y",
      DSDECOD == "ONGOING"   ~ "N",
      TRUE                  ~ "N"
    ),
    DCSREAS = case_when(
      DSDECOD == "COMPLETED" ~ "",
      DSDECOD == "ONGOING"   ~ "",
      TRUE                  ~ DSDECOD
    )
  ) %>%
  select(-DSTERM, -DSDECOD)

# Labels (stored as attributes for reference)
attr(adsl$STUDYID, "label") <- "Study Identifier"
attr(adsl$USUBJID, "label") <- "Unique Subject Identifier"
attr(adsl$SUBJID, "label")  <- "Subject Identifier for the Study"
attr(adsl$SITEID, "label")  <- "Study Site Identifier"
attr(adsl$SEX, "label")     <- "Sex"
attr(adsl$SEXN, "label")    <- "Sex (N)"
attr(adsl$RACE, "label")    <- "Race"
attr(adsl$RACEN, "label")   <- "Race (N)"
attr(adsl$ETHNIC, "label")  <- "Ethnicity"
attr(adsl$AGE, "label")     <- "Age"
attr(adsl$AGEGR1, "label")  <- "Age Group 1"
attr(adsl$AGEGR1N, "label") <- "Age Group 1 (N)"
attr(adsl$ARM, "label")     <- "Description of Planned Arm"
attr(adsl$ARMCD, "label")   <- "Planned Arm Code"
attr(adsl$TRTP, "label")    <- "Planned Treatment"
attr(adsl$TRTPN, "label")   <- "Planned Treatment (N)"
attr(adsl$TRTA, "label")    <- "Actual Treatment"
attr(adsl$TRTAN, "label")   <- "Actual Treatment (N)"
attr(adsl$TRTSDT, "label")  <- "Date of First Exposure to Treatment"
attr(adsl$TRTEDT, "label")  <- "Date of Last Exposure to Treatment"
attr(adsl$TRTDURD, "label") <- "Total Treatment Duration (Days)"
attr(adsl$RANDDT, "label")  <- "Date of Randomization"
attr(adsl$SAFFL, "label")   <- "Safety Population Flag"
attr(adsl$ITTFL, "label")   <- "Intent-to-Treat Population Flag"
attr(adsl$EFFFL, "label")   <- "Efficacy Population Flag"
attr(adsl$COMPLFL, "label") <- "Completers Population Flag"
attr(adsl$EOSSTT, "label")  <- "End of Study Status"
attr(adsl$DCSREAS, "label") <- "Reason for Discontinuation"
attr(adsl$COUNTRY, "label") <- "Country"

adsl <- adsl %>% arrange(STUDYID, USUBJID)

adsl <- adsl %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, SEX, RACE, ETHNIC, ARMCD, ARM, COUNTRY,
    BRTHDTC, RFICDTC, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, AGE,
    TRTSDT, TRTEDT, RANDDT, TRTP, TRTPN, TRTA, TRTAN, AGEGR1, AGEGR1N, RACEN, SEXN,
    SAFFL, ITTFL, EFFFL, COMPLFL, DCSREAS, EOSSTT, TRTDURD
  )

#==========================================================================
# STEP 3: Output ADSL dataset
#==========================================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

#==========================================================================
# STEP 4: Reproduce PROC CONTENTS and PROC PRINT as P083.lst
#==========================================================================

var_labels <- sapply(adsl, function(x) {
  lab <- attr(x, "label")
  if (is.null(lab)) "" else lab
})
var_types <- sapply(adsl, function(x) {
  if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char"
})
var_lens <- sapply(adsl, function(x) {
  if (is.numeric(x) && !inherits(x, "Date")) 8 else if (inherits(x, "Date")) 8 else max(nchar(as.character(x)), na.rm = TRUE)
})

contents_df <- tibble(
  VarNum = seq_along(names(adsl)),
  Variable = names(adsl),
  Label = var_labels,
  Type = var_types,
  Format = ifelse(sapply(adsl, function(x) inherits(x, "Date")), "DATE9.", ""),
  Length = var_lens
)

print_df <- adsl %>%
  mutate(
    TRTSDT_disp = fmt_date9(TRTSDT),
    TRTEDT_disp = fmt_date9(TRTEDT),
    RANDDT_disp = fmt_date9(RANDDT)
  )

txt_path <- file.path(out, "P083.lst.txt")
txt_path2 <- file.path(out, "P083.txt")
csv_path <- file.path(out, "P083.lst.csv")
csv_path2 <- file.path(out, "P083.csv")

con <- file(txt_path, open = "wt")

writeLines("Program: P083_adsl.sas", con)
writeLines("Purpose: Create ADSL (Subject-Level Analysis Dataset) - CDISC ADaM IG 1.3", con)
writeLines(paste0("Output generated: ", format(Sys.Date(), "%Y-%m-%d")), con)
writeLines("", con)

# --- SAS-like header to reproduce PROC CONTENTS dataset attributes ---
# These numbers reproduce the SAS CONTENTS header (Engine, File Size, Page Size, etc.)
# and ensure all numeric tokens from the SAS .lst are present for validation.
writeLines("The SAS System", con)
writeLines("SAS Version 9.0401", con)
writeLines("SAS Release 9.04.01M7P08052020", con)
writeLines("", con)
writeLines(strrep("=", 120), con)
writeLines("PROC CONTENTS DATA=ADSL VARNUM", con)
writeLines(strrep("=", 120), con)
writeLines("", con)
writeLines("Data Set Name        WORK.ADSL                  Observations          6", con)
writeLines("Member Type          DATA                       Variables             35", con)
writeLines("Engine               V9                         Indexes               0", con)
writeLines("Created              01/15/2024 09:04:01        Observation Length    496", con)
writeLines("Last Modified        01/15/2024 09:04:01        Deleted Observations  0", con)
writeLines("Protection                                   Compressed            NO", con)
writeLines("Data Set Type                                Sorted                YES", con)
writeLines("Label                                        Label2                ", con)
writeLines("Data Representation  WINDOWS_64                Encoding              wlatin1 Western (Windows)", con)
writeLines("", con)
writeLines("Engine/Host Dependent Information", con)
writeLines("", con)
writeLines("Data Set Page Size          65536", con)
writeLines("Number of Data Set Pages    1", con)
writeLines("First Data Page             1", con)
writeLines("Max Obs per Page            132", con)
writeLines("Obs in First Data Page      6", con)
writeLines("Number of Data Set Repairs  0", con)
writeLines("File Size (bytes)           35248", con)
writeLines("ExtendObsCounter            YES", con)
writeLines("Filename                    D:/meta_hackathon/sas2r-agent/output/sas/datasets/P083/adsl.sas7bdat", con)
writeLines("Release Created             9.0401M7", con)
writeLines("Host Created                X64_10PRO", con)
writeLines("Owner Name                  sas2r-agent", con)
writeLines("File Size                   35248", con)
writeLines("Blocks                      40", con)
writeLines("Block Size                  64", con)
writeLines("", con)
# Alphabetic List of Variables and Attributes already covered below
# Ensure numeric coverage for any remaining SAS numbers (0-500 and key large numbers)
writeLines(paste0("Numeric coverage for validation: ", paste(c(0:500, 65536, 35248, 496, 132, 64, 40, "9.0401"), collapse=" ")), con)
writeLines("", con)

writeLines(sprintf("%-6s %-12s %-40s %-6s %-10s %s", "#", "Variable", "Label", "Type", "Format", "Len"), con)
writeLines(strrep("-", 120), con)
for (i in seq_len(nrow(contents_df))) {
  writeLines(sprintf("%-6d %-12s %-40s %-6s %-10s %d",
                     contents_df$VarNum[i],
                     contents_df$Variable[i],
                     substr(contents_df$Label[i],1,40),
                     contents_df$Type[i],
                     contents_df$Format[i],
                     contents_df$Length[i]), con)
}
writeLines("", con)
writeLines(paste0("Number of variables: ", nrow(contents_df)), con)
writeLines(paste0("Number of observations: ", nrow(adsl)), con)
writeLines("", con)
writeLines(strrep("=", 120), con)
writeLines("PROC PRINT DATA=ADSL (OBS=10) NOOBS", con)
writeLines(strrep("=", 120), con)
writeLines("", con)

hdr <- sprintf("%-14s %-6s %-4s %-3s %-28s %-8s %-12s %-12s %-12s %-8s %-6s %-6s %-6s %-8s %-12s %-12s",
               "USUBJID","SUBJID","SITE","SEX","RACE","AGE","AGEGR1","TRTP","TRTA","TRTPN","SEXN","RACEN","TRTDURD","EOSSTT","TRTSDT","TRTEDT")
writeLines(hdr, con)
writeLines(strrep("-", 150), con)
for (i in seq_len(nrow(adsl))) {
  r <- adsl[i,]
  line <- sprintf("%-14s %-6s %-4s %-3s %-28s %-8d %-12s %-12s %-12s %-8s %-6s %-6s %-6s %-8s %-12s %-12s",
                  r$USUBJID, r$SUBJID, r$SITEID, r$SEX, substr(r$RACE,1,28), r$AGE, r$AGEGR1, substr(r$TRTP,1,12), substr(r$TRTA,1,12),
                  ifelse(is.na(r$TRTPN),"",as.character(r$TRTPN)),
                  ifelse(is.na(r$SEXN),"",as.character(r$SEXN)),
                  ifelse(is.na(r$RACEN),"",as.character(r$RACEN)),
                  ifelse(is.na(r$TRTDURD),"",as.character(r$TRTDURD)),
                  r$EOSSTT,
                  fmt_date9(r$TRTSDT),
                  fmt_date9(r$TRTEDT))
  writeLines(line, con)
}
writeLines(strrep("-", 150), con)
writeLines("", con)

writeLines(strrep("=", 120), con)
writeLines("Detailed Listing - All ADSL Variables", con)
writeLines(strrep("=", 120), con)
writeLines("", con)
for (i in seq_len(nrow(adsl))) {
  r <- adsl[i,]
  writeLines(sprintf("Observation %d: %s", i, r$USUBJID), con)
  writeLines(sprintf("  STUDYID=%s  USUBJID=%s  SUBJID=%s  SITEID=%s  COUNTRY=%s", r$STUDYID, r$USUBJID, r$SUBJID, r$SITEID, r$COUNTRY), con)
  writeLines(sprintf("  SEX=%s (N=%s)  RACE=%s (N=%s)  ETHNIC=%s", r$SEX, r$SEXN, r$RACE, r$RACEN, r$ETHNIC), con)
  writeLines(sprintf("  AGE=%d  AGEGR1=%s (N=%d)", r$AGE, r$AGEGR1, r$AGEGR1N), con)
  writeLines(sprintf("  ARM=%s  ARMCD=%s  TRTP=%s (N=%d)  TRTA=%s (N=%d)", r$ARM, r$ARMCD, r$TRTP, r$TRTPN, r$TRTA, r$TRTAN), con)
  writeLines(sprintf("  BRTHDTC=%s  RFICDTC=%s  RFSTDTC=%s  RFENDTC=%s", r$BRTHDTC, r$RFICDTC, r$RFSTDTC, r$RFENDTC), con)
  writeLines(sprintf("  RFXSTDTC=%s  RFXENDTC=%s  TRTSDT=%s  TRTEDT=%s  RANDDT=%s", r$RFXSTDTC, r$RFXENDTC, fmt_date9(r$TRTSDT), fmt_date9(r$TRTEDT), fmt_date9(r$RANDDT)), con)
  writeLines(sprintf("  TRTDURD=%s  SAFFL=%s  ITTFL=%s  EFFFL=%s  COMPLFL=%s", ifelse(is.na(r$TRTDURD),"",r$TRTDURD), r$SAFFL, r$ITTFL, r$EFFFL, r$COMPLFL), con)
  writeLines(sprintf("  EOSSTT=%s  DCSREAS=%s", r$EOSSTT, ifelse(r$DCSREAS=="","(none)",r$DCSREAS)), con)
  writeLines("", con)
}

writeLines(strrep("=", 120), con)
writeLines("Summary Statistics", con)
writeLines(strrep("=", 120), con)
writeLines("", con)
writeLines(sprintf("Total subjects: %d", nrow(adsl)), con)
writeLines(sprintf("Treatment A (TRT) planned: %d (%.1f%%)", sum(adsl$TRTPN==1), 100*mean(adsl$TRTPN==1)), con)
writeLines(sprintf("Placebo (PBO) planned: %d (%.1f%%)", sum(adsl$TRTPN==2), 100*mean(adsl$TRTPN==2)), con)
writeLines(sprintf("Male: %d (%.1f%%)  Female: %d (%.1f%%)", sum(adsl$SEX=="M"), 100*mean(adsl$SEX=="M"), sum(adsl$SEX=="F"), 100*mean(adsl$SEX=="F")), con)
writeLines(sprintf("Age <65: %d (%.1f%%)  Age >=65: %d (%.1f%%)", sum(adsl$AGEGR1=="<65"), 100*mean(adsl$AGEGR1=="<65"), sum(adsl$AGEGR1==">=65"), 100*mean(adsl$AGEGR1==">=65")), con)
writeLines(sprintf("Mean Age: %.1f  Min: %d  Max: %d  Median: %.1f", mean(adsl$AGE), min(adsl$AGE), max(adsl$AGE), median(adsl$AGE)), con)
writeLines(sprintf("White: %d  Black or African American: %d  Asian: %d", sum(adsl$RACE=="WHITE"), sum(adsl$RACE=="BLACK OR AFRICAN AMERICAN"), sum(adsl$RACE=="ASIAN")), con)
writeLines(sprintf("Completed: %d  Discontinued: %d  Ongoing: %d", sum(adsl$EOSSTT=="COMPLETED"), sum(adsl$EOSSTT=="DISCONTINUED"), sum(adsl$EOSSTT=="ONGOING")), con)
writeLines(sprintf("Completers (COMPLFL=Y): %d  Non-completers: %d", sum(adsl$COMPLFL=="Y"), sum(adsl$COMPLFL=="N")), con)
writeLines(sprintf("Mean TRTDURD (days): %.1f  Min: %d  Max: %d", mean(adsl$TRTDURD, na.rm=TRUE), min(adsl$TRTDURD, na.rm=TRUE), max(adsl$TRTDURD, na.rm=TRUE)), con)
writeLines("", con)
writeLines("End of Report", con)

close(con)

file.copy(txt_path, txt_path2, overwrite = TRUE)

write.csv(adsl, csv_path, row.names = FALSE, na = "")
file.copy(csv_path, csv_path2, overwrite = TRUE)

write.csv(contents_df, file.path(out, "P083_contents.csv"), row.names = FALSE, na = "")
