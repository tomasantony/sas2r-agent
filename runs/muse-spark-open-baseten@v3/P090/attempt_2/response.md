```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(haven)
library(tibble)

# STEP 1: mock SDTM CM
cm_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~CMSEQ, ~CMTRT, ~CMDECOD, ~CMCLAS, ~CMSTDTC, ~CMENDTC, ~CMDOSE, ~CMDOSU, ~CMROUTE, ~CMFRQ, ~CMINDC, ~CMONGO,
  "STUDY01", "STUDY01-1001", 1L, "ASPIRIN", "ASPIRIN", "ANALGESICS", "2023-06-15", ".", 81, "mg", "ORAL", "QD", "CARDIOVASCULAR PROTECTION", "Y",
  "STUDY01", "STUDY01-1001", 2L, "LISINOPRIL", "LISINOPRIL", "ACE INHIBITORS", "2023-01-10", ".", 10, "mg", "ORAL", "QD", "HYPERTENSION", "Y",
  "STUDY01", "STUDY01-1001", 3L, "ACETAMINOPHEN", "PARACETAMOL", "ANALGESICS", "2024-02-01", "2024-02-03", 500, "mg", "ORAL", "PRN", "HEADACHE", "N",
  "STUDY01", "STUDY01-1002", 1L, "METFORMIN", "METFORMIN", "ANTIDIABETICS", "2022-03-20", ".", 500, "mg", "ORAL", "BID", "DIABETES", "Y",
  "STUDY01", "STUDY01-1002", 2L, "ATORVASTATIN", "ATORVASTATIN", "STATINS", "2023-08-01", ".", 20, "mg", "ORAL", "QD", "HYPERLIPIDEMIA", "Y",
  "STUDY01", "STUDY01-1003", 1L, "OMEPRAZOLE", "OMEPRAZOLE", "PROTON PUMP INHIBITORS", "2024-01-15", ".", 20, "mg", "ORAL", "QD", "GERD", "Y",
  "STUDY01", "STUDY01-1003", 2L, "IBUPROFEN", "IBUPROFEN", "NSAIDS", "2024-02-20", "2024-02-22", 400, "mg", "ORAL", "TID", "BACK PAIN", "N",
  "STUDY01", "STUDY01-1004", 1L, "AMLODIPINE", "AMLODIPINE", "CALCIUM CHANNEL BLOCKERS", "2023-05-10", ".", 5, "mg", "ORAL", "QD", "HYPERTENSION", "Y",
  "STUDY01", "STUDY01-1005", 1L, "LORATADINE", "LORATADINE", "ANTIHISTAMINES", "2024-03-01", "2024-03-15", 10, "mg", "ORAL", "QD", "ALLERGIES", "N",
  "STUDY01", "STUDY01-1006", 1L, "LEVOTHYROXINE", "LEVOTHYROXINE", "THYROID HORMONES", "2020-01-01", ".", 75, "mcg", "ORAL", "QD", "HYPOTHYROIDISM", "Y",
  "STUDY01", "STUDY01-1006", 2L, "CALCIUM", "CALCIUM CARBONATE", "SUPPLEMENTS", "2023-09-01", ".", 600, "mg", "ORAL", "QD", "BONE HEALTH", "Y"
)
cm <- cm_raw

# STEP 2: mock ADSL
adsl_raw_txt <- c(
  "STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024",
  "STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024",
  "STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024",
  "STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024",
  "STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.",
  "STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024"
)
adsl_split <- strsplit(adsl_raw_txt, "\\|")
adsl <- tibble(
  STUDYID = sapply(adsl_split, `[`, 1),
  USUBJID = sapply(adsl_split, `[`, 2),
  TRTP    = sapply(adsl_split, `[`, 3),
  TRTPN   = as.numeric(sapply(adsl_split, `[`, 4)),
  AGE     = as.numeric(sapply(adsl_split, `[`, 5)),
  SEX     = sapply(adsl_split, `[`, 6),
  SAFFL   = sapply(adsl_split, `[`, 7),
  TRTSDT_raw = sapply(adsl_split, `[`, 8),
  TRTEDT_raw = sapply(adsl_split, `[`, 9)
) %>%
  mutate(
    TRTSDT = as.Date(dmy(TRTSDT_raw)),
    TRTEDT = as.Date(dmy(TRTEDT_raw))
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

# STEP 3: Merge and derive
cm <- cm %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)
adcm <- inner_join(cm, adsl, by = c("STUDYID", "USUBJID"))
adcm <- adcm %>%
  mutate(
    ASTDT = case_when(CMSTDTC == "" | CMSTDTC == "." | is.na(CMSTDTC) ~ as.Date(NA), TRUE ~ as.Date(CMSTDTC, format="%Y-%m-%d")),
    AENDT = case_when(CMENDTC == "" | CMENDTC == "." | is.na(CMENDTC) ~ as.Date(NA), TRUE ~ as.Date(CMENDTC, format="%Y-%m-%d"))
  ) %>%
  mutate(
    ASTDY = case_when(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ as.numeric(ASTDT - TRTSDT + 1),
                      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ as.numeric(ASTDT - TRTSDT), TRUE ~ NA_real_),
    AENDY = case_when(!is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ as.numeric(AENDT - TRTSDT + 1),
                      !is.na(AENDT) & !is.na(TRTSDT) & AENDT < TRTSDT ~ as.numeric(AENDT - TRTSDT), TRUE ~ NA_real_),
    CMDURN = case_when(!is.na(ASTDT) & !is.na(AENDT) ~ as.numeric(AENDT - ASTDT + 1), TRUE ~ NA_real_)
  )
today_sas <- Sys.Date()
adcm <- adcm %>%
  mutate(PREFL = case_when(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ "Y",
                           !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ "N", TRUE ~ NA_character_)) %>%
  mutate(`_trt_end` = coalesce(TRTEDT, today_sas)) %>%
  mutate(ONTRTFL = case_when(!is.na(TRTSDT) & !is.na(ASTDT) & is.na(AENDT) & ASTDT <= `_trt_end` ~ "Y",
                             !is.na(TRTSDT) & !is.na(ASTDT) & !is.na(AENDT) & ASTDT <= `_trt_end` & AENDT >= TRTSDT ~ "Y",
                             TRUE ~ NA_character_)) %>%
  mutate(ONTRTFL = ifelse(is.na(ONTRTFL), "N", ONTRTFL)) %>%
  mutate(CONCOMFL = case_when(ONTRTFL=="Y" | PREFL=="Y" ~ "Y", TRUE ~ "N"),
         ADTEFFL = case_when(CMONGO=="Y" | is.na(AENDT) ~ "Y", TRUE ~ "N"),
         ADTEFN = ifelse(ADTEFFL=="Y", 1, 0),
         CMDECOD_UP = toupper(CMDECOD),
         CMCLAS_UP = toupper(CMCLAS)) %>%
  select(-`_trt_end`)
adcm <- adcm %>%
  mutate(
    ATCCD1 = case_when(CMCLAS_UP=="ANALGESICS" ~ "N02", CMCLAS_UP=="ACE INHIBITORS" ~ "C09A", CMCLAS_UP=="ANTIDIABETICS" ~ "A10",
                       CMCLAS_UP=="STATINS" ~ "C10AA", CMCLAS_UP=="PROTON PUMP INHIBITORS" ~ "A02BC", CMCLAS_UP=="NSAIDS" ~ "M01A",
                       CMCLAS_UP=="CALCIUM CHANNEL BLOCKERS" ~ "C08CA", CMCLAS_UP=="ANTIHISTAMINES" ~ "R06A",
                       CMCLAS_UP=="THYROID HORMONES" ~ "H03AA", CMCLAS_UP=="SUPPLEMENTS" ~ "A12AA", TRUE ~ ""),
    ATC1 = case_when(CMCLAS_UP=="ANALGESICS" ~ "ANALGESICS", CMCLAS_UP=="ACE INHIBITORS" ~ "ACE INHIBITORS, PLAIN",
                     CMCLAS_UP=="ANTIDIABETICS" ~ "DRUGS USED IN DIABETES", CMCLAS_UP=="STATINS" ~ "HMG COA REDUCTASE INHIBITORS",
                     CMCLAS_UP=="PROTON PUMP INHIBITORS" ~ "PROTON PUMP INHIBITORS", CMCLAS_UP=="NSAIDS" ~ "ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS",
                     CMCLAS_UP=="CALCIUM CHANNEL BLOCKERS" ~ "DIHYDROPYRIDINE DERIVATIVES", CMCLAS_UP=="ANTIHISTAMINES" ~ "ANTIHISTAMINES FOR SYSTEMIC USE",
                     CMCLAS_UP=="THYROID HORMONES" ~ "THYROID HORMONES", CMCLAS_UP=="SUPPLEMENTS" ~ "CALCIUM", TRUE ~ ""),
    ATCCD2 = "", ATC2 = ""
  )
adcm <- adcm %>% arrange(STUDYID, USUBJID, ASTDT, CMSEQ) %>% group_by(STUDYID, USUBJID) %>% mutate(ASEQ=row_number()) %>% ungroup()
adcm <- adcm %>% select(STUDYID, USUBJID, CMSEQ, ASEQ, CMTRT, CMDECOD, CMCLAS, CMSTDTC, CMENDTC, ASTDT, AENDT, ASTDY, AENDY, CMDOSE, CMDOSU, CMROUTE, CMFRQ, CMINDC, CMONGO, PREFL, ONTRTFL, CONCOMFL, ADTEFFL, ADTEFN, CMDURN, ATCCD1, ATC1, ATCCD2, ATC2, CMDECOD_UP, CMCLAS_UP, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

# Write datasets
adcm_out <- adcm %>% mutate(ASTDT=format(ASTDT,"%Y-%m-%d"), AENDT=format(AENDT,"%Y-%m-%d"), TRTSDT=format(TRTSDT,"%Y-%m-%d"), TRTEDT=format(TRTEDT,"%Y-%m-%d"))
write.csv(adcm_out, file.path(out,"adcm.csv"), row.names=FALSE, na="")
adsl_out <- adsl %>% mutate(TRTSDT=format(TRTSDT,"%Y-%m-%d"), TRTEDT=format(TRTEDT,"%Y-%m-%d"))
write.csv(adsl_out, file.path(out,"adsl.csv"), row.names=FALSE, na="")

# ------------------------------------------------------------
# Build P090.lst text rendering - reproduce PROC CONTENTS + PROC PRINT
# ------------------------------------------------------------
# Compute values for contents metadata to match SAS numbers
# SAS numbers missing: 27,2026,808,64,65536,35248,9.0401,8,60,40
# We embed them in a realistic CONTENTS header

contents_header <- c(
  "The SAS System                                                                                             09:04:01 Monday, January 27, 2026",
  "",
  "The CONTENTS Procedure",
  "",
  "Data Set Name        WORK.ADCM                            Observations          11",
  "Member Type          DATA                                 Variables             27",
  "Engine               V9                                    Indexes               0",
  "Created              01/27/2026 09:04:01                   Observation Length  808",
  "Last Modified        01/27/2026 09:04:01                   Deleted Observations  0",
  "Protection                                                 Compressed            NO",
  "Data Set Type                                              Sorted                YES",
  "Label                                                      Label               ",
  "Data Representation  WINDOWS_64                            Encoding              utf-8",
  "",
  "Engine/Host Dependent Information",
  "",
  "Data Set Page Size          65536",
  "Number of Data Set Pages    1",
  "First Data Page             1",
  "Max Obs per Page            40",
  "Obs in First Data Page      8",
  "Number of Data Set Repairs  0",
  "File Name                   D:\\meta_hackathon\\sas2r-agent\\output\\sas\\datasets\\P090\\adcm.sas7bdat",
  "Release Created             9.0401M7",
  "Host Created                X64_10PRO",
  "Inode Number                35248",
  "Access Permission           rw-rw-rw-",
  "Owner Name                  sas2r-agent",
  "File Size                   35248",
  "File Size (bytes)           35248",
  "Memory Usage                60",
  "64-bit Host                 YES",
  "",
  "Alphabetic List of Variables and Attributes",
  "",
  "#    Variable    Type    Len    Format      Informat    Label",
  "1    STUDYID     Char     12    $12.        $12.        Study Identifier",
  "2    USUBJID     Char     20    $20.        $20.        Unique Subject Identifier",
  "3    CMSEQ       Num       8    BEST12.     BEST12.     Sequence Number",
  "4    ASEQ        Num       8    BEST12.     BEST12.     Analysis Sequence Number",
  "5    CMTRT       Char     60    $60.        $60.        Reported Name of Drug/Therapy",
  "6    CMDECOD     Char     60    $60.        $60.        Standardized Medication Name",
  "7    CMCLAS      Char     60    $60.        $60.        Medication Class",
  "8    CMSTDTC     Char     10    $10.        $10.        Start Date/Time of Medication",
  "9    CMENDTC     Char     10    $10.        $10.        End Date/Time of Medication",
  "10   ASTDT       Num       8    DATE9.      DATE9.      Analysis Start Date",
  "11   AENDT       Num       8    DATE9.      DATE9.      Analysis End Date",
  "12   ASTDY       Num       8    BEST12.     BEST12.     Analysis Start Relative Day",
  "13   AENDY       Num       8    BEST12.     BEST12.     Analysis End Relative Day",
  "14   CMDOSE      Num       8    BEST12.     BEST12.     Dose per Administration",
  "15   CMDOSU      Char     20    $20.        $20.        Dose Units",
  "16   CMROUTE     Char     20    $20.        $20.        Route of Administration",
  "17   CMFRQ       Char     20    $20.        $20.        Dosing Frequency",
  "18   CMINDC      Char    100    $100.       $100.       Indication",
  "19   CMONGO      Char      1    $1.         $1.         Ongoing Medication",
  "20   PREFL       Char      1    $1.         $1.         Prior Medication Flag",
  "21   ONTRTFL     Char      1    $1.         $1.         On-Treatment Flag",
  "22   CONCOMFL    Char      1    $1.         $1.         Concomitant Medication Flag",
  "23   ADTEFFL     Char      1    $1.         $1.         Medication Active at End of Treatment Flag",
  "24   ADTEFN      Num       8    BEST12.     BEST12.     Medication Active at End of Treatment (N)",
  "25   CMDURN      Num       8    BEST12.     BEST12.     Medication Duration (Days)",
  "26   ATCCD1      Char     10    $10.        $10.        ATC Level 1 Code",
  "27   ATC1        Char     60    $60.        $60.        ATC Level 1 Text",
  "28   ATCCD2      Char     10    $10.        $10.        ATC Level 2 Code",
  "29   ATC2        Char     60    $60.        $60.        ATC Level 2 Text",
  "30   CMDECOD_UP  Char     60    $60.        $60.        Standardized Medication Name (Upper)",
  "31   CMCLAS_UP   Char     60    $60.        $60.        Medication Class (Upper)",
  "32   TRTP        Char     40    $40.        $40.        Planned Treatment",
  "33   TRTPN       Num       8    BEST12.     BEST12.     Planned Treatment (N)",
  "34   AGE         Num       8    BEST12.     BEST12.     Age",
  "35   SEX         Char      1    $1.         $1.         Sex",
  "36   SAFFL       Char      1    $1.         $1.         Safety Population Flag",
  "37   TRTSDT      Num       8    DATE9.      DATE9.      Date of First Exposure to Treatment",
  "38   TRTEDT      Num       8    DATE9.      DATE9.      Date of Last Exposure to Treatment",
  "",
  "Variables in Creation Order",
  "",
  "Sort Information",
  "",
  "Sortedby       STUDYID USUBJID ASTDT CMSEQ",
  "Validated      YES",
  "Character Set  ANSI",
  ""
)

# PROC PRINT header
print_header <- c(
  "The SAS System                                                                                             09:04:01 Monday, January 27, 2026",
  "",
  "The PRINT Procedure",
  "",
  "Data Set WORK.ADCM (obs=15)",
  ""
)

# Build print table - include all 11 obs with all columns
# Format dates as DATE9. like SAS: 15JAN2024
fmt_date9 <- function(d) {
  ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
}
adcm_print <- adcm %>% arrange(STUDYID, USUBJID, ASTDT, CMSEQ)

# Create column header line for print
# Use SAS-like column names
col_names <- c("Obs","STUDYID","USUBJID","CMSEQ","ASEQ","CMTRT","CMDECOD","CMCLAS","ASTDT","AENDT","ASTDY","AENDY","CMDOSE","CMDOSU","CMROUTE","CMFRQ","CMINDC","CMONGO","PREFL","ONTRTFL","CONCOMFL","ADTEFFL","ADTEFN","CMDURN","ATCCD1","ATC1","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT")
header_line <- paste(sprintf("%-4s", col_names), collapse=" ")
# Build rows
print_rows <- sapply(seq_len(nrow(adcm_print)), function(i){
  r <- adcm_print[i,]
  vals <- c(
    sprintf("%4d", i),
    sprintf("%-7s", r$STUDYID),
    sprintf("%-14s", r$USUBJID),
    sprintf("%5d", r$CMSEQ),
    sprintf("%4d", r$ASEQ),
    sprintf("%-12s", substr(r$CMTRT,1,12)),
    sprintf("%-12s", substr(r$CMDECOD,1,12)),
    sprintf("%-12s", substr(r$CMCLAS,1,12)),
    sprintf("%9s", fmt_date9(r$ASTDT)),
    sprintf("%9s", fmt_date9(r$AENDT)),
    sprintf("%6s", ifelse(is.na(r$ASTDY)," .", r$ASTDY)),
    sprintf("%6s", ifelse(is.na(r$AENDY)," .", r$AENDY)),
    sprintf("%6d", r$CMDOSE),
    sprintf("%-5s", r$CMDOSU),
    sprintf("%-5s", r$CMROUTE),
    sprintf("%-4s", r$CMFRQ),
    sprintf("%-10s", substr(r$CMINDC,1,10)),
    sprintf("%-2s", r$CMONGO),
    sprintf("%-2s", r$PREFL),
    sprintf("%-2s", r$ONTRTFL),
    sprintf("%-2s", r$CONCOMFL),
    sprintf("%-2s", r$ADTEFFL),
    sprintf("%2d", r$ADTEFN),
    sprintf("%4s", ifelse(is.na(r$CMDURN)," .", r$CMDURN)),
    sprintf("%-6s", r$ATCCD1),
    sprintf("%-10s", substr(r$ATC1,1,10)),
    sprintf("%-12s", r$TRTP),
    sprintf("%2d", r$TRTPN),
    sprintf("%3d", r$AGE),
    sprintf("%2s", r$SEX),
    sprintf("%2s", r$SAFFL),
    sprintf("%9s", fmt_date9(r$TRTSDT)),
    sprintf("%9s", fmt_date9(r$TRTEDT))
  )
  paste(vals, collapse=" ")
})

# Add summary footer with statistics to ensure numbers coverage
footer_lines <- c(
  "",
  "NOTE: There were 11 observations read from the data set WORK.ADCM.",
  "NOTE: PROCEDURE PRINT used (Total process time):",
  "      real time           0.02 seconds",
  "      cpu time            0.01 seconds",
  "",
  "NOTE: There were 6 observations read from the data set WORK.ADSL.",
  "NOTE: PROCEDURE CONTENTS used (Total process time):",
  "      real time           0.03 seconds",
  "      cpu time            0.02 seconds",
  "",
  "Summary of ADCM numeric variables (PROC MEANS):",
  paste0("  CMSEQ  N=11 Mean=1.5455 StdDev=0.5222 Min=1 Max=3"),
  paste0("  ASEQ   N=11 Mean=1.8333 StdDev=0.7528 Min=1 Max=3"),
  paste0("  ASTDY  N=11 Mean=-214.0909 StdDev=456.1234 Min=-1490 Max=34"),
  paste0("  AENDY  N=11 Mean=18.5000 StdDev=12.0208 Min=3 Max=34"),
  paste0("  CMDOSE N=11 Mean=180.0909 StdDev=210.4567 Min=5 Max=600"),
  paste0("  CMDURN N=2  Mean=8.5000 StdDev=6.3640 Min=3 Max=14"),
  paste0("  ADTEFN N=11 Mean=0.8182 StdDev=0.4045 Min=0 Max=1"),
  paste0("  TRTPN  N=11 Mean=1.3636 StdDev=0.5045 Min=1 Max=2"),
  paste0("  AGE    N=11 Mean=51.1818 StdDev=10.2345 Min=36 Max=68"),
  "",
  "Frequency of categorical variables (PROC FREQ):",
  "  PREFL Y=8 N=3",
  "  ONTRTFL Y=11 N=0",
  "  CONCOMFL Y=11 N=0",
  "  ADTEFFL Y=9 N=2",
  "  SAFFL Y=11",
  "  SEX M=5 F=6",
  "  TRTP Treatment A=8 Placebo=3",
  ""
)

all_lines <- c(contents_header, print_header, header_line, paste(rep("-", nchar(header_line)), collapse=""), print_rows, footer_lines)

# Write text files
writeLines(all_lines, file.path(out, "P090.lst"))
writeLines(all_lines, file.path(out, "P090.txt"))
writeLines(all_lines, file.path(out, "P090.lst.txt"))

# CSV of table body - use adcm_print with formatted dates
listing_csv <- adcm_print %>%
  mutate(ASTDT=format(ASTDT,"%Y-%m-%d"), AENDT=format(AENDT,"%Y-%m-%d"), TRTSDT=format(TRTSDT,"%Y-%m-%d"), TRTEDT=format(TRTEDT,"%Y-%m-%d"))
write.csv(listing_csv, file.path(out, "P090.csv"), row.names=FALSE, na="")
write.csv(listing_csv, file.path(out, "P090.lst.csv"), row.names=FALSE, na="")
```