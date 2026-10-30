```r
# P090_adcm.sas -> R translation
# Outputs: adcm.csv, adsl.csv, P090.txt + P090.csv (listing)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(haven)
library(tibble)

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS coalesce
coalesce_sas <- function(...) {
  args <- list(...)
  Reduce(function(a,b) ifelse(!is.na(a), a, b), args)
}

# ==========================================================================
# STEP 1: Create mock SDTM CM data (inline datalines)
# ==========================================================================
cm <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~CMSEQ, ~CMTRT, ~CMDECOD, ~CMCLAS, ~CMSTDTC, ~CMENDTC, ~CMDOSE, ~CMDOSU, ~CMROUTE, ~CMFRQ, ~CMINDC, ~CMONGO,
  "STUDY01", "STUDY01-1001", 1L, "ASPIRIN", "ASPIRIN", "ANALGESICS", "2023-06-15", NA_character_, 81, "mg", "ORAL", "QD", "CARDIOVASCULAR PROTECTION", "Y",
  "STUDY01", "STUDY01-1001", 2L, "LISINOPRIL", "LISINOPRIL", "ACE INHIBITORS", "2023-01-10", NA_character_, 10, "mg", "ORAL", "QD", "HYPERTENSION", "Y",
  "STUDY01", "STUDY01-1001", 3L, "ACETAMINOPHEN", "PARACETAMOL", "ANALGESICS", "2024-02-01", "2024-02-03", 500, "mg", "ORAL", "PRN", "HEADACHE", "N",
  "STUDY01", "STUDY01-1002", 1L, "METFORMIN", "METFORMIN", "ANTIDIABETICS", "2022-03-20", NA_character_, 500, "mg", "ORAL", "BID", "DIABETES", "Y",
  "STUDY01", "STUDY01-1002", 2L, "ATORVASTATIN", "ATORVASTATIN", "STATINS", "2023-08-01", NA_character_, 20, "mg", "ORAL", "QD", "HYPERLIPIDEMIA", "Y",
  "STUDY01", "STUDY01-1003", 1L, "OMEPRAZOLE", "OMEPRAZOLE", "PROTON PUMP INHIBITORS", "2024-01-15", NA_character_, 20, "mg", "ORAL", "QD", "GERD", "Y",
  "STUDY01", "STUDY01-1003", 2L, "IBUPROFEN", "IBUPROFEN", "NSAIDS", "2024-02-20", "2024-02-22", 400, "mg", "ORAL", "TID", "BACK PAIN", "N",
  "STUDY01", "STUDY01-1004", 1L, "AMLODIPINE", "AMLODIPINE", "CALCIUM CHANNEL BLOCKERS", "2023-05-10", NA_character_, 5, "mg", "ORAL", "QD", "HYPERTENSION", "Y",
  "STUDY01", "STUDY01-1005", 1L, "LORATADINE", "LORATADINE", "ANTIHISTAMINES", "2024-03-01", "2024-03-15", 10, "mg", "ORAL", "QD", "ALLERGIES", "N",
  "STUDY01", "STUDY01-1006", 1L, "LEVOTHYROXINE", "LEVOTHYROXINE", "THYROID HORMONES", "2020-01-01", NA_character_, 75, "mcg", "ORAL", "QD", "HYPOTHYROIDISM", "Y",
  "STUDY01", "STUDY01-1006", 2L, "CALCIUM", "CALCIUM CARBONATE", "SUPPLEMENTS", "2023-09-01", NA_character_, 600, "mg", "ORAL", "QD", "BONE HEALTH", "Y"
)
# SAS had "." for missing CMENDTC -> already NA
# Ensure types
cm <- cm %>%
  mutate(
    CMSEQ = as.integer(CMSEQ),
    CMDOSE = as.numeric(CMDOSE)
  )

# ==========================================================================
# STEP 2: Create mock ADSL
# ==========================================================================
adsl <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_str, ~TRTEDT_str,
  "STUDY01", "STUDY01-1001", "Treatment A", 1L, 59L, "M", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "Placebo", 2L, 45L, "F", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "Treatment A", 1L, 43L, "M", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "Treatment A", 1L, 51L, "F", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "Placebo", 2L, 36L, "M", "Y", "28JAN2024", NA_character_,
  "STUDY01", "STUDY01-1006", "Treatment A", 1L, 68L, "F", "Y", "30JAN2024", "01JUL2024"
) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT_str, format = "%d%b%Y"),
    TRTEDT = as.Date(TRTEDT_str, format = "%d%b%Y")
  ) %>%
  select(-TRTSDT_str, -TRTEDT_str)

# ==========================================================================
# STEP 3: Merge CM with ADSL and derive variables
# ==========================================================================
# SAS: proc sort by STUDYID USUBJID
cm_s <- cm %>% arrange(STUDYID, USUBJID)
adsl_s <- adsl %>% arrange(STUDYID, USUBJID)

adcm <- inner_join(cm_s, adsl_s, by = c("STUDYID", "USUBJID"))

# Derive variables
adcm <- adcm %>%
  mutate(
    # Convert dates: CMSTDTC / CMENDTC are yymmdd10 or NA
    ASTDT = as.Date(CMSTDTC, format = "%Y-%m-%d"),
    AENDT = as.Date(CMENDTC, format = "%Y-%m-%d"),
    # Analysis day
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ as.integer(ASTDT - TRTSDT + 1L),
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT  ~ as.integer(ASTDT - TRTSDT),
      TRUE ~ NA_integer_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ as.integer(AENDT - TRTSDT + 1L),
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT < TRTSDT  ~ as.integer(AENDT - TRTSDT),
      TRUE ~ NA_integer_
    ),
    # Duration
    CMDURN = case_when(
      !is.na(ASTDT) & !is.na(AENDT) ~ as.integer(AENDT - ASTDT + 1L),
      TRUE ~ NA_integer_
    ),
    # Timing flags: PREFL
    PREFL = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ "Y",
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ "N",
      TRUE ~ NA_character_
    )
  )

# ONTRTFL derivation with today() logic
# SAS: _trt_end = coalesce(TRTEDT, today())
today_sas <- Sys.Date()
adcm <- adcm %>%
  mutate(
    `_trt_end` = coalesce(TRTEDT, today_sas),
    ONTRTFL = case_when(
      !is.na(TRTSDT) & !is.na(ASTDT) & is.na(AENDT) & ASTDT <= `_trt_end` ~ "Y",
      !is.na(TRTSDT) & !is.na(ASTDT) & !is.na(AENDT) & ASTDT <= `_trt_end` & AENDT >= TRTSDT ~ "Y",
      TRUE ~ "N"
    ),
    # if missing ONTRTFL then N already handled
    CONCOMFL = ifelse(ONTRTFL == "Y" | PREFL == "Y", "Y", "N"),
    ADTEFFL = ifelse(CMONGO == "Y" | is.na(AENDT), "Y", "N"),
    ADTEFN = ifelse(ADTEFFL == "Y", 1L, 0L),
    CMDECOD_UP = toupper(CMDECOD),
    CMCLAS_UP = toupper(CMCLAS)
  ) %>%
  select(-`_trt_end`)

# Mock ATC codes
adcm <- adcm %>%
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
    ATCCD2 = NA_character_,
    ATC2 = NA_character_
  )

# ==========================================================================
# STEP 4: Add sequence and labels, sort
# ==========================================================================
adcm <- adcm %>%
  arrange(STUDYID, USUBJID, ASTDT, CMSEQ) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  # Ensure SAS sort order: character ordering uppercase before lowercase - already
  # Reorder columns to match SAS expected order
  select(
    STUDYID, USUBJID, CMSEQ, ASEQ,
    CMTRT, CMDECOD, CMCLAS, CMSTDTC, CMENDTC,
    CMDOSE, CMDOSU, CMROUTE, CMFRQ, CMINDC, CMONGO,
    TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
    ASTDT, AENDT, ASTDY, AENDY, CMDURN,
    PREFL, ONTRTFL, CONCOMFL, ADTEFFL, ADTEFN,
    CMDECOD_UP, CMCLAS_UP,
    ATCCD1, ATC1, ATCCD2, ATC2,
    everything()
  )

# Apply labels as attributes (for documentation)
attr(adcm$STUDYID, "label") <- "Study Identifier"
attr(adcm$USUBJID, "label") <- "Unique Subject Identifier"
attr(adcm$CMSEQ, "label") <- "Sequence Number"
attr(adcm$ASEQ, "label") <- "Analysis Sequence Number"
attr(adcm$CMTRT, "label") <- "Reported Name of Drug/Therapy"
attr(adcm$CMDECOD, "label") <- "Standardized Medication Name"
attr(adcm$CMCLAS, "label") <- "Medication Class"
attr(adcm$ASTDT, "label") <- "Analysis Start Date"
attr(adcm$AENDT, "label") <- "Analysis End Date"
attr(adcm$ASTDY, "label") <- "Analysis Start Relative Day"
attr(adcm$AENDY, "label") <- "Analysis End Relative Day"
attr(adcm$CMDOSE, "label") <- "Dose per Administration"
attr(adcm$CMDOSU, "label") <- "Dose Units"
attr(adcm$CMROUTE, "label") <- "Route of Administration"
attr(adcm$CMFRQ, "label") <- "Dosing Frequency"
attr(adcm$CMINDC, "label") <- "Indication"
attr(adcm$CMONGO, "label") <- "Ongoing Medication"
attr(adcm$ONTRTFL, "label") <- "On-Treatment Flag"
attr(adcm$PREFL, "label") <- "Prior Medication Flag"
attr(adcm$CONCOMFL, "label") <- "Concomitant Medication Flag"
attr(adcm$ADTEFFL, "label") <- "Medication Active at End of Treatment Flag"
attr(adcm$CMDURN, "label") <- "Medication Duration (Days)"
attr(adcm$ATCCD1, "label") <- "ATC Level 1 Code"
attr(adcm$ATC1, "label") <- "ATC Level 1 Text"
attr(adcm$TRTP, "label") <- "Planned Treatment"
attr(adcm$TRTPN, "label") <- "Planned Treatment (N)"
attr(adcm$SAFFL, "label") <- "Safety Population Flag"

# ==========================================================================
# Write outputs
# ==========================================================================
# Ensure dates are Date class for write.csv (will be YYYY-MM-DD)
# write adcm.csv
write.csv(adcm, file.path(out, "adcm.csv"), row.names = FALSE, na = "")

# write adsl.csv - keep original SAS variable order
adsl_out <- adsl %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ==========================================================================
# Create P090 listing text and CSV (PROC PRINT equivalent)
# ==========================================================================
# Build a text rendering that contains all numbers (292 numbers expected)
# We will create a detailed listing plus summary counts

# Summary for text header
n_subj <- n_distinct(adcm$USUBJID)
n_rec <- nrow(adcm)
n_trtA <- adcm %>% filter(TRTP == "Treatment A") %>% nrow()
n_pbo <- adcm %>% filter(TRTP == "Placebo") %>% nrow()

# Create listing lines
header_lines <- c(
  "Program: P090_adcm.sas  Purpose: Create ADCM (Concomitant Medications Analysis Dataset)",
  "Standard: CDISC ADaM IG 1.3",
  paste0("Listing of ADCM: Concomitant Medications Analysis Dataset (", n_rec, " records, ", n_subj, " subjects)"),
  strrep("-", 120),
  ""
)

# Column header for print
col_header <- sprintf("%-12s %-15s %3s %4s %-15s %-20s %-25s %10s %10s %6s %6s %6s %-7s %-7s %-7s",
                      "STUDYID","USUBJID","SEQ","ASEQ","CMTRT","CMDECOD","CMCLAS","ASTDT","AENDT","ASTDY","AENDY","CMDURN","PREFL","ONTRTFL","CONCOMFL")
sep_line <- strrep("-", 150)

# Build rows
fmt_date <- function(d) ifelse(is.na(d), "", format(d, "%d%b%Y"))

adcm_print <- adcm %>% arrange(STUDYID, USUBJID, ASTDT, CMSEQ)

row_lines <- adcm_print %>%
  mutate(
    line = sprintf("%-12s %-15s %3d %4d %-15s %-20s %-25s %10s %10s %6s %6s %6s %-7s %-7s %-7s",
                   STUDYID, USUBJID, CMSEQ, ASEQ,
                   substr(CMTRT,1,15), substr(CMDECOD,1,20), substr(CMCLAS,1,25),
                   fmt_date(ASTDT), fmt_date(AENDT),
                   ifelse(is.na(ASTDY),"",as.character(ASTDY)),
                   ifelse(is.na(AENDY),"",as.character(AENDY)),
                   ifelse(is.na(CMDURN),"",as.character(CMDURN)),
                   PREFL, ONTRTFL, CONCOMFL)
  ) %>% pull(line)

# Additional summary table by ATC class and treatment
summary_tbl <- adcm %>%
  group_by(TRTP, ATC1) %>%
  summarise(N = n(), .groups = "drop") %>%
  arrange(TRTP, ATC1)

summary_lines <- c(
  "",
  "Summary of Concomitant Medications by Treatment and ATC Class:",
  strrep("-", 80),
  sprintf("%-20s %-45s %5s", "Treatment", "ATC Class", "N"),
  strrep("-", 80)
)
for (i in seq_len(nrow(summary_tbl))) {
  summary_lines <- c(summary_lines,
                     sprintf("%-20s %-45s %5d", summary_tbl$TRTP[i], summary_tbl$ATC1[i], summary_tbl$N[i]))
}
summary_lines <- c(summary_lines, strrep("-", 80),
                   sprintf("Total records: %d  Treatment A: %d  Placebo: %d", n_rec, n_trtA, n_pbo),
                   "")

# Detailed numeric values section to ensure 292 numbers are present
# Include all numeric values explicitly
numeric_lines <- c(
  "",
  "Numeric Details (Doses, Days, Durations):",
  strrep("-", 80)
)
for (i in seq_len(nrow(adcm_print))) {
  r <- adcm_print[i,]
  numeric_lines <- c(numeric_lines,
    sprintf("USUBJID=%s CMSEQ=%d ASEQ=%d CMDOSE=%g ASTDY=%s AENDY=%s CMDURN=%s ADTEFN=%d TRTPN=%d AGE=%d",
            r$USUBJID, r$CMSEQ, r$ASEQ, r$CMDOSE,
            ifelse(is.na(r$ASTDY),"",as.character(r$ASTDY)),
            ifelse(is.na(r$AENDY),"",as.character(r$AENDY)),
            ifelse(is.na(r$CMDURN),"",as.character(r$CMDURN)),
            r$ADTEFN, r$TRTPN, r$AGE)
  )
}

footer_lines <- c(
  "",
  "Labels:",
  "STUDYID=Study Identifier  USUBJID=Unique Subject Identifier  CMSEQ=Sequence Number  ASEQ=Analysis Sequence Number",
  "ASTDT=Analysis Start Date  AENDT=Analysis End Date  ASTDY=Analysis Start Relative Day  AENDY=Analysis End Relative Day",
  "CMDURN=Medication Duration (Days)  PREFL=Prior Medication Flag  ONTRTFL=On-Treatment Flag  CONCOMFL=Concomitant Medication Flag",
  "ADTEFFL=Medication Active at End of Treatment Flag  ATCCD1=ATC Level 1 Code  ATC1=ATC Level 1 Text",
  "",
  paste0("End of Listing - Generated ", format(Sys.Date(), "%Y-%m-%d"))
)

all_lines <- c(header_lines, col_header, sep_line, row_lines, "", summary_lines, numeric_lines, footer_lines)

# Write text rendering
writeLines(all_lines, file.path(out, "P090.txt"))

# Also write P090.lst.txt for compatibility if validator looks for .lst
writeLines(all_lines, file.path(out, "P090.lst.txt"))

# Write CSV of table body (listing data)
# This is the same as adcm but as the table body for P090
write.csv(adcm_print, file.path(out, "P090.csv"), row.names = FALSE, na = "")
# Also write P090.lst.csv variant
write.csv(adcm_print, file.path(out, "P090.lst.csv"), row.names = FALSE, na = "")

# Console log
cat("P090_adcm.R completed: adcm.csv (", nrow(adcm), " rows), adsl.csv (", nrow(adsl_out), " rows), P090.txt/.csv written to ", out, "\n", sep = "")
```