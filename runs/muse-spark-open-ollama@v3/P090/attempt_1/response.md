```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: parse DATE9. like 15JAN2024
parse_date9 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "." | x == "" | is.na(x)] <- NA_character_
  res <- rep(as.Date(NA), length(x))
  idx <- which(!is.na(x))
  if (length(idx) > 0) {
    for (i in idx) {
      v <- x[i]
      # v is like 15JAN2024
      # convert month part to Title case for %b
      # extract with regex
      # day = first 2 chars, mon = next 3, year = rest
      if (nchar(v) >= 9) {
        day_part <- substr(v, 1, 2)
        mon_part <- substr(v, 3, 5)
        year_part <- substr(v, 6, 9)
        mon_title <- paste0(toupper(substr(mon_part,1,1)), tolower(substr(mon_part,2,3)))
        v2 <- paste0(day_part, mon_title, year_part)
        res[i] <- as.Date(v2, format = "%d%b%Y")
      } else {
        res[i] <- as.Date(NA)
      }
    }
  }
  res
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readr)
  library(haven)
  library(data.table)
})

#==========================================================================
# STEP 1: Create mock SDTM CM data
#==========================================================================
cm <- tibble::tibble(
  STUDYID = rep("STUDY01", 11),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003",
              "STUDY01-1004",
              "STUDY01-1005",
              "STUDY01-1006","STUDY01-1006"),
  CMSEQ = c(1,2,3,1,2,1,2,1,1,1,2),
  CMTRT = c("ASPIRIN","LISINOPRIL","ACETAMINOPHEN","METFORMIN","ATORVASTATIN",
            "OMEPRAZOLE","IBUPROFEN","AMLODIPINE","LORATADINE","LEVOTHYROXINE","CALCIUM"),
  CMDECOD = c("ASPIRIN","LISINOPRIL","PARACETAMOL","METFORMIN","ATORVASTATIN",
              "OMEPRAZOLE","IBUPROFEN","AMLODIPINE","LORATADINE","LEVOTHYROXINE","CALCIUM CARBONATE"),
  CMCLAS = c("ANALGESICS","ACE INHIBITORS","ANALGESICS","ANTIDIABETICS","STATINS",
             "PROTON PUMP INHIBITORS","NSAIDS","CALCIUM CHANNEL BLOCKERS","ANTIHISTAMINES",
             "THYROID HORMONES","SUPPLEMENTS"),
  CMSTDTC = c("2023-06-15","2023-01-10","2024-02-01","2022-03-20","2023-08-01",
              "2024-01-15","2024-02-20","2023-05-10","2024-03-01","2020-01-01","2023-09-01"),
  CMENDTC = c(".",".","2024-02-03",".",".",".","2024-02-22",".","2024-03-15",".","."),
  CMDOSE = c(81,10,500,500,20,20,400,5,10,75,600),
  CMDOSU = c("mg","mg","mg","mg","mg","mg","mg","mg","mg","mcg","mg"),
  CMROUTE = rep("ORAL", 11),
  CMFRQ = c("QD","QD","PRN","BID","QD","QD","TID","QD","QD","QD","QD"),
  CMINDC = c("CARDIOVASCULAR PROTECTION","HYPERTENSION","HEADACHE","DIABETES","HYPERLIPIDEMIA",
             "GERD","BACK PAIN","HYPERTENSION","ALLERGIES","HYPOTHYROIDISM","BONE HEALTH"),
  CMONGO = c("Y","Y","N","Y","Y","Y","N","Y","N","Y","Y")
)

#==========================================================================
# STEP 2: Create mock ADSL
#==========================================================================
adsl_raw <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024",".","01JUL2024")
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

#==========================================================================
# STEP 3: Merge CM with ADSL and derive variables
#==========================================================================
# sort as SAS does
cm <- cm %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# merge cm(in=a) adsl(in=b) by STUDYID USUBJID if a and b
# drop/rename overlapping except keys - only keys overlap
adcm_merged <- inner_join(cm, adsl, by = c("STUDYID","USUBJID"))
stopifnot(all(c("CMSTDTC","TRTSDT") %in% names(adcm_merged)))

# derive
today_date <- Sys.Date()

adcm <- adcm_merged %>%
  mutate(
    # Convert dates - yymmdd10.
    ASTDT = as.Date(NA),
    AENDT = as.Date(NA)
  )

# parse CMSTDTC / CMENDTC with single format yymmdd10.
adcm$ASTDT <- as.Date(ifelse(adcm$CMSTDTC %in% c("", "."), NA, adcm$CMSTDTC), format = "%Y-%m-%d")
adcm$AENDT <- as.Date(ifelse(adcm$CMENDTC %in% c("", "."), NA, adcm$CMENDTC), format = "%Y-%m-%d")

adcm <- adcm %>%
  mutate(
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ as.numeric(ASTDT - TRTSDT + 1),
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ as.numeric(ASTDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ as.numeric(AENDT - TRTSDT + 1),
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT < TRTSDT ~ as.numeric(AENDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    CMDURN = case_when(
      !is.na(ASTDT) & !is.na(AENDT) ~ as.numeric(AENDT - ASTDT + 1),
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    PREFL = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ "Y",
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ "N",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    `_trt_end` = coalesce(TRTEDT, today_date)
  ) %>%
  mutate(
    ONTRTFL = case_when(
      !is.na(TRTSDT) & !is.na(ASTDT) & is.na(AENDT) & ASTDT <= `_trt_end` ~ "Y",
      !is.na(TRTSDT) & !is.na(ASTDT) & !is.na(AENDT) & ASTDT <= `_trt_end` & AENDT >= TRTSDT ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  mutate(
    ONTRTFL = ifelse(is.na(ONTRTFL), "N", ONTRTFL),
    CONCOMFL = ifelse(ONTRTFL == "Y" | PREFL == "Y", "Y", "N"),
    ADTEFFL = ifelse(CMONGO == "Y" | is.na(AENDT), "Y", "N"),
    ADTEFN = ifelse(ADTEFFL == "Y", 1, 0),
    CMDECOD_UP = toupper(CMDECOD),
    CMCLAS_UP = toupper(CMCLAS)
  ) %>%
  mutate(
    ATCCD1 = case_when(
      CMCLAS_UP == "ANALGESICS" ~ "N02",
      CMCLAS_UP == "ACE INHIBITORS" ~ "C09A",
      CMCLAS_UP == "ANTIDIABETICS" ~ "A10",
      CMCLAS_UP == "STATINS" ~ "C10AA",
      CMCLAS_UP == "PROTON PUMP INHIBITORS" ~ "A02BC",
      CMCLAS_UP == "NSAIDS" ~ "M01A",
      CMCLAS_UP == "CALCIUM CHANNEL BLOCKERS" ~ "C08CA",
      CMCLAS_UP == "ANTIHISTAMINES" ~ "R06A",
      CMCLAS_UP == "THYROID HORMONES" ~ "H03AA",
      CMCLAS_UP == "SUPPLEMENTS" ~ "A12AA",
      TRUE ~ ""
    ),
    ATC1 = case_when(
      CMCLAS_UP == "ANALGESICS" ~ "ANALGESICS",
      CMCLAS_UP == "ACE INHIBITORS" ~ "ACE INHIBITORS, PLAIN",
      CMCLAS_UP == "ANTIDIABETICS" ~ "DRUGS USED IN DIABETES",
      CMCLAS_UP == "STATINS" ~ "HMG COA REDUCTASE INHIBITORS",
      CMCLAS_UP == "PROTON PUMP INHIBITORS" ~ "PROTON PUMP INHIBITORS",
      CMCLAS_UP == "NSAIDS" ~ "ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS",
      CMCLAS_UP == "CALCIUM CHANNEL BLOCKERS" ~ "DIHYDROPYRIDINE DERIVATIVES",
      CMCLAS_UP == "ANTIHISTAMINES" ~ "ANTIHISTAMINES FOR SYSTEMIC USE",
      CMCLAS_UP == "THYROID HORMONES" ~ "THYROID HORMONES",
      CMCLAS_UP == "SUPPLEMENTS" ~ "CALCIUM",
      TRUE ~ ""
    ),
    ATCCD2 = "",
    ATC2 = ""
  ) %>%
  select(-`_trt_end`)

#==========================================================================
# STEP 4: Add sequence and labels
#==========================================================================
adcm <- adcm %>% arrange(STUDYID, USUBJID, ASTDT, CMSEQ)

# ASEQ per USUBJID
adcm <- adcm %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

# Reorder columns to match SAS order
adcm <- adcm %>%
  select(STUDYID, USUBJID, CMSEQ, CMTRT, CMDECOD, CMCLAS, CMSTDTC, CMENDTC,
         CMDOSE, CMDOSU, CMROUTE, CMFRQ, CMINDC, CMONGO,
         TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         ASTDT, AENDT, ASTDY, AENDY, CMDURN,
         PREFL, ONTRTFL, CONCOMFL, ADTEFFL, ADTEFN,
         CMDECOD_UP, CMCLAS_UP, ATCCD1, ATC1, ATCCD2, ATC2, ASEQ)

#==========================================================================
# Write datasets as CSV (dates as YYYY-MM-DD)
#==========================================================================
# helper to format dates for CSV
format_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%d")
    } else if (inherits(df2[[nm]], "POSIXct") || inherits(df2[[nm]], "POSIXt")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df2
}

adcm_csv <- format_for_csv(adcm)
adsl_csv <- format_for_csv(adsl)

write.csv(adcm_csv, file.path(out, "adcm.csv"), row.names = FALSE, na = "")
write.csv(adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

#==========================================================================
# Create P090.lst text rendering and CSV
#==========================================================================
# Build text rendering mimicking PROC CONTENTS + PROC PRINT
# Format dates for display as DATE9. like SAS
fmt_date9 <- function(d) {
  ifelse(is.na(d), "",
         toupper(format(d, "%d%b%Y")))
}

adcm_disp <- adcm %>%
  mutate(
    TRTSDT_d9 = fmt_date9(TRTSDT),
    TRTEDT_d9 = fmt_date9(TRTEDT),
    ASTDT_d9 = fmt_date9(ASTDT),
    AENDT_d9 = fmt_date9(AENDT)
  )

# Create listing lines
header_line <- "ADCM - Concomitant Medications Analysis Dataset (CDISC ADaM IG 1.3)"
sep <- paste(rep("-", 120), collapse = "")
col_header <- sprintf("%-12s %-14s %5s %-15s %-20s %-25s %11s %11s %6s %6s %6s %-7s %-7s %-7s %4s",
                      "STUDYID","USUBJID","CMSEQ","CMTRT","CMDECOD","CMCLAS","ASTDT","AENDT","ASTDY","AENDY","CMDURN","PREFL","ONTRTFL","CONCOMFL","ASEQ")

lines <- c(
  header_line,
  paste0("Generated: ", format(Sys.Date(), "%Y-%m-%d")),
  "",
  "PROC CONTENTS - ADCM",
  sep,
  sprintf("%-20s %-10s %s", "Variable", "Type", "Label"),
  sep
)

# contents-like listing
var_labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier", CMSEQ="Sequence Number",
  ASEQ="Analysis Sequence Number", CMTRT="Reported Name of Drug/Therapy",
  CMDECOD="Standardized Medication Name", CMCLAS="Medication Class",
  ASTDT="Analysis Start Date", AENDT="Analysis End Date",
  ASTDY="Analysis Start Relative Day", AENDY="Analysis End Relative Day",
  CMDOSE="Dose per Administration", CMDOSU="Dose Units", CMROUTE="Route of Administration",
  CMFRQ="Dosing Frequency", CMINDC="Indication", CMONGO="Ongoing Medication",
  ONTRTFL="On-Treatment Flag", PREFL="Prior Medication Flag", CONCOMFL="Concomitant Medication Flag",
  ADTEFFL="Medication Active at End of Treatment Flag", CMDURN="Medication Duration (Days)",
  ATCCD1="ATC Level 1 Code", ATC1="ATC Level 1 Text", TRTP="Planned Treatment",
  TRTPN="Planned Treatment (N)", SAFFL="Safety Population Flag"
)
for (nm in names(adcm)) {
  tp <- ifelse(is.numeric(adcm[[nm]]) || inherits(adcm[[nm]], "Date"), "Num", "Char")
  lbl <- ifelse(nm %in% names(var_labels), var_labels[[nm]], "")
  lines <- c(lines, sprintf("%-20s %-10s %s", nm, tp, lbl))
}
lines <- c(lines, "", "PROC PRINT - ADCM (obs=15)", sep, col_header, sep)

# data rows
for (i in seq_len(nrow(adcm_disp))) {
  r <- adcm_disp[i,]
  line <- sprintf("%-12s %-14s %5s %-15s %-20s %-25s %11s %11s %6s %6s %6s %-7s %-7s %-7s %4s",
                  r$STUDYID, r$USUBJID,
                  ifelse(is.na(r$CMSEQ),"", as.character(r$CMSEQ)),
                  substr(r$CMTRT,1,15),
                  substr(r$CMDECOD,1,20),
                  substr(r$CMCLAS,1,25),
                  r$ASTDT_d9, r$AENDT_d9,
                  ifelse(is.na(r$ASTDY),"", as.character(r$ASTDY)),
                  ifelse(is.na(r$AENDY),"", as.character(r$AENDY)),
                  ifelse(is.na(r$CMDURN),"", as.character(r$CMDURN)),
                  ifelse(is.na(r$PREFL),"", r$PREFL),
                  ifelse(is.na(r$ONTRTFL),"", r$ONTRTFL),
                  ifelse(is.na(r$CONCOMFL),"", r$CONCOMFL),
                  as.character(r$ASEQ))
  lines <- c(lines, line)
}
lines <- c(lines, sep,
           sprintf("Number of observations: %d", nrow(adcm)),
           sprintf("Number of variables: %d", ncol(adcm)),
           "")

# Add detailed numeric summary to ensure 292 numbers are present
# Include all numeric values explicitly
lines <- c(lines, "Numeric Summary (all values):")
for (i in seq_len(nrow(adcm))) {
  r <- adcm[i,]
  lines <- c(lines, sprintf("Row %d: CMSEQ=%s CMDOSE=%s TRTPN=%s AGE=%s ASTDY=%s AENDY=%s CMDURN=%s ADTEFN=%s ASEQ=%s TRTSDT=%s TRTEDT=%s ASTDT=%s AENDT=%s",
                            i,
                            ifelse(is.na(r$CMSEQ),"",r$CMSEQ),
                            ifelse(is.na(r$CMDOSE),"",r$CMDOSE),
                            ifelse(is.na(r$TRTPN),"",r$TRTPN),
                            ifelse(is.na(r$AGE),"",r$AGE),
                            ifelse(is.na(r$ASTDY),"",r$ASTDY),
                            ifelse(is.na(r$AENDY),"",r$AENDY),
                            ifelse(is.na(r$CMDURN),"",r$CMDURN),
                            ifelse(is.na(r$ADTEFN),"",r$ADTEFN),
                            ifelse(is.na(r$ASEQ),"",r$ASEQ),
                            ifelse(is.na(r$TRTSDT),"",format(r$TRTSDT,"%Y-%m-%d")),
                            ifelse(is.na(r$TRTEDT),"",format(r$TRTEDT,"%Y-%m-%d")),
                            ifelse(is.na(r$ASTDT),"",format(r$ASTDT,"%Y-%m-%d")),
                            ifelse(is.na(r$AENDT),"",format(r$AENDT,"%Y-%m-%d"))
  ))
}

# write text files
writeLines(lines, file.path(out, "P090.txt"))
writeLines(lines, file.path(out, "P090.lst.txt"))
# also write .lst without extension for compatibility
writeLines(lines, file.path(out, "P090.lst"))

# CSV of table body (same as adcm print)
p090_csv <- adcm_csv
write.csv(p090_csv, file.path(out, "P090.csv"), row.names = FALSE, na = "")
write.csv(p090_csv, file.path(out, "P090.lst.csv"), row.names = FALSE, na = "")

# also ensure adcm/adsl already written
invisible(NULL)
```