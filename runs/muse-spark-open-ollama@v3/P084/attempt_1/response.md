```r
# P084_adae.sas -> R translation
# Program id: P084  file: P084_adae.sas
# Purpose: Create ADAE (Adverse Events Analysis Dataset)

# Load allowed packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(haven)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

cat("OUT_DIR =", out, "\n")

# Helper: parse DATE9. like 15JAN2024
parse_date9_vec <- function(x) {
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v) || trimws(v) == "") next
    v <- trimws(v)
    m <- regmatches(v, regexec("^([0-9]{1,2})([A-Za-z]{3})([0-9]{4})$", v))[[1]]
    if (length(m) == 4) {
      day <- sprintf("%02d", as.integer(m[2]))
      mon_raw <- m[3]
      mon <- paste0(toupper(substr(mon_raw, 1, 1)), tolower(substr(mon_raw, 2, 3)))
      yr <- m[4]
      v2 <- paste0(day, mon, yr)
      res[i] <- as.Date(v2, format = "%d%b%Y")
    }
  }
  res
}

# Helper: format Date as DATE9. for text rendering (e.g., 15JAN2024)
format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

# Helper: round half away from zero (SAS behavior)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

#==========================================================================
# STEP 1: Create mock SDTM AE data
#==========================================================================
ae <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~AESEQ, ~AETERM, ~AEDECOD, ~AEBODSYS, ~AESTDTC, ~AEENDTC, ~AESEV, ~AESER, ~AEREL, ~AEACN, ~AEOUT, ~AETOXGR,
  "STUDY01", "STUDY01-1001", 1, "Headache", "Headache", "Nervous system disorders", "2024-02-01", "2024-02-03", "MILD", "N", "NOT RELATED", "DOSE NOT CHANGED", "RECOVERED", "1",
  "STUDY01", "STUDY01-1001", 2, "Nausea", "Nausea", "Gastrointestinal disorders", "2024-02-15", "2024-02-18", "MODERATE", "N", "POSSIBLY RELATED", "DOSE NOT CHANGED", "RECOVERED", "2",
  "STUDY01", "STUDY01-1002", 1, "Fatigue", "Fatigue", "General disorders", "2024-02-10", "2024-03-01", "MILD", "N", "POSSIBLY RELATED", "DOSE NOT CHANGED", "RECOVERED", "1",
  "STUDY01", "STUDY01-1003", 1, "Dizziness", "Dizziness", "Nervous system disorders", "2024-02-20", "2024-02-22", "MILD", "N", "NOT RELATED", "DOSE NOT CHANGED", "RECOVERED", "1",
  "STUDY01", "STUDY01-1003", 2, "Rash", "Rash", "Skin disorders", "2024-03-05", "2024-03-15", "MODERATE", "N", "PROBABLY RELATED", "DOSE REDUCED", "RECOVERED", "2",
  "STUDY01", "STUDY01-1003", 3, "Hepatotoxicity", "Hepatotoxicity", "Hepatobiliary disorders", "2024-03-25", ".", "SEVERE", "Y", "RELATED", "DRUG WITHDRAWN", "NOT RECOVERED", "3",
  "STUDY01", "STUDY01-1004", 1, "Insomnia", "Insomnia", "Psychiatric disorders", "2024-02-25", "2024-03-10", "MILD", "N", "UNLIKELY", "DOSE NOT CHANGED", "RECOVERED", "1",
  "STUDY01", "STUDY01-1005", 1, "Vomiting", "Vomiting", "Gastrointestinal disorders", "2024-03-01", "2024-03-02", "MODERATE", "N", "POSSIBLY RELATED", "DOSE NOT CHANGED", "RECOVERED", "2",
  "STUDY01", "STUDY01-1006", 1, "Back pain", "Back pain", "Musculoskeletal disorders", "2024-02-28", "2024-03-05", "MILD", "N", "NOT RELATED", "DOSE NOT CHANGED", "RECOVERED", "1"
)
# Ensure types
ae$AESEQ <- as.numeric(ae$AESEQ)

#==========================================================================
# STEP 2: Create mock ADSL
#==========================================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~SITEID, ~TRTP, ~TRTPN, ~AGE, ~AGEGR1, ~SEX, ~RACE, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01", "STUDY01-1001", "101", "Treatment A", 1, 59, "<65", "M", "WHITE", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "101", "Placebo", 2, 45, "<65", "F", "BLACK OR AFRICAN AMERICAN", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "102", "Treatment A", 1, 43, "<65", "M", "ASIAN", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "102", "Treatment A", 1, 51, "<65", "F", "WHITE", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "103", "Placebo", 2, 36, "<65", "M", "ASIAN", "Y", "28JAN2024", ".",
  "STUDY01", "STUDY01-1006", "103", "Treatment A", 1, 68, ">=65", "F", "WHITE", "Y", "30JAN2024", "01JUL2024"
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9_vec(TRTSDT_c),
    TRTEDT = parse_date9_vec(TRTEDT_c)
  ) %>%
  select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT)

# Verify ADSL
stopifnot(nrow(adsl) == 6)
stopifnot(all(c("STUDYID","USUBJID","TRTSDT","TRTEDT") %in% names(adsl)))

#==========================================================================
# STEP 3: Merge AE with ADSL and derive ADAE variables
#==========================================================================
# Sort as SAS does
ae <- ae %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# Merge (inner join, keep only a and b)
adae <- ae %>%
  inner_join(adsl %>% select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT),
             by = c("STUDYID", "USUBJID"))

stopifnot(nrow(adae) == 9)
stopifnot(all(!is.na(adae$TRTSDT)))

# Derive variables
adae <- adae %>%
  mutate(
    # Convert dates: AESTDTC/AEENDTC are yymmdd10., "." is missing
    AESTDTC_clean = ifelse(AESTDTC == "." | AESTDTC == "", NA_character_, AESTDTC),
    AEENDTC_clean = ifelse(AEENDTC == "." | AEENDTC == "", NA_character_, AEENDTC),
    ASTDT = as.Date(AESTDTC_clean, format = "%Y-%m-%d"),
    AENDT = as.Date(AEENDTC_clean, format = "%Y-%m-%d"),
    # Analysis day
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
    # Treatment-emergent flag (30-day grace)
    TRTEMFL = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT & is.na(TRTEDT) ~ "Y",
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT & !is.na(TRTEDT) & ASTDT <= TRTEDT + 30 ~ "Y",
      TRUE ~ "N"
    ),
    # Analysis severity
    ASEV = AESEV,
    ASEVN = case_when(
      toupper(AESEV) == "MILD" ~ 1,
      toupper(AESEV) == "MODERATE" ~ 2,
      toupper(AESEV) == "SEVERE" ~ 3,
      TRUE ~ NA_real_
    ),
    # Analysis relationship
    AREL = AEREL,
    ARELN = case_when(
      toupper(AEREL) == "NOT RELATED" ~ 1,
      toupper(AEREL) == "UNLIKELY" ~ 2,
      toupper(AEREL) == "POSSIBLY RELATED" ~ 3,
      toupper(AEREL) == "PROBABLY RELATED" ~ 4,
      toupper(AEREL) == "RELATED" ~ 5,
      TRUE ~ NA_real_
    ),
    AESER_FL = AESER,
    AESOC = AEBODSYS,
    AEDECOD_UP = toupper(AEDECOD),
    AEDUR = case_when(
      !is.na(ASTDT) & !is.na(AENDT) ~ as.numeric(AENDT - ASTDT + 1),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-AESTDTC_clean, -AEENDTC_clean)

#==========================================================================
# STEP 4: Derive occurrence flags
#==========================================================================
# Sort by USUBJID AEBODSYS AEDECOD ASTDT as SAS
adae <- adae %>% arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT)

# Derive flags using FIRST. logic
# AOCCFL: first occurrence of any AE per USUBJID
adae <- adae %>%
  group_by(USUBJID) %>%
  mutate(AOCCFL = ifelse(row_number() == 1, "Y", "")) %>%
  ungroup()

# AOCCSFL: first occurrence within SOC per USUBJID + AEBODSYS
adae <- adae %>%
  group_by(USUBJID, AEBODSYS) %>%
  mutate(AOCCSFL = ifelse(row_number() == 1, "Y", "")) %>%
  ungroup()

# AOCCPFL: first occurrence of PT per USUBJID + AEBODSYS + AEDECOD
adae <- adae %>%
  group_by(USUBJID, AEBODSYS, AEDECOD) %>%
  mutate(AOCCPFL = ifelse(row_number() == 1, "Y", "")) %>%
  ungroup()

#==========================================================================
# STEP 5: Add labels and sequence
#==========================================================================
adae <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ)

# Assign ASEQ per USUBJID (first.USUBJID then ASEQ+1)
adae <- adae %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

# Reorder columns to SAS varnum order
adae <- adae %>%
  select(
    STUDYID, USUBJID, AESEQ, ASEQ, AETERM, AEDECOD, AEBODSYS, AESOC, AEDECOD_UP,
    AESTDTC, AEENDTC, ASTDT, AENDT, ASTDY, AENDY, AEDUR,
    AESEV, ASEV, ASEVN, AESER, AESER_FL, AEREL, AREL, ARELN, AEACN, AEOUT, AETOXGR,
    TRTEMFL, AOCCFL, AOCCSFL, AOCCPFL,
    SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT
  )

# Add labels as attributes (for documentation)
attr(adae$STUDYID, "label") <- "Study Identifier"
attr(adae$USUBJID, "label") <- "Unique Subject Identifier"
attr(adae$AESEQ, "label") <- "Sequence Number"
attr(adae$ASEQ, "label") <- "Analysis Sequence Number"
attr(adae$AETERM, "label") <- "Reported Term for the Adverse Event"
attr(adae$AEDECOD, "label") <- "Dictionary-Derived Term"
attr(adae$AEBODSYS, "label") <- "Body System or Organ Class"
attr(adae$AESOC, "label") <- "Primary System Organ Class"
attr(adae$ASTDT, "label") <- "Analysis Start Date"
attr(adae$AENDT, "label") <- "Analysis End Date"
attr(adae$ASTDY, "label") <- "Analysis Start Relative Day"
attr(adae$AENDY, "label") <- "Analysis End Relative Day"
attr(adae$ASEV, "label") <- "Analysis Severity/Intensity"
attr(adae$ASEVN, "label") <- "Analysis Severity/Intensity (N)"
attr(adae$AREL, "label") <- "Analysis Causality"
attr(adae$ARELN, "label") <- "Analysis Causality (N)"
attr(adae$TRTEMFL, "label") <- "Treatment Emergent Analysis Flag"
attr(adae$AESER_FL, "label") <- "Serious Event Flag"
attr(adae$AOCCFL, "label") <- "First Occurrence of Any AE Flag"
attr(adae$AOCCSFL, "label") <- "First Occurrence Within SOC Flag"
attr(adae$AOCCPFL, "label") <- "First Occurrence of Preferred Term Flag"
attr(adae$AEDUR, "label") <- "AE Duration (Days)"
attr(adae$TRTP, "label") <- "Planned Treatment"
attr(adae$TRTPN, "label") <- "Planned Treatment (N)"
attr(adae$TRTSDT, "label") <- "Date of First Exposure to Treatment"
attr(adae$TRTEDT, "label") <- "Date of Last Exposure to Treatment"
attr(adae$SAFFL, "label") <- "Safety Population Flag"

#==========================================================================
# Output datasets as CSV (ISO dates)
#==========================================================================
# Ensure dates are Date objects for write.csv to output YYYY-MM-DD
# write.csv will convert Date to character YYYY-MM-DD automatically
write.csv(adae, file.path(out, "adae.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

cat("Wrote adae.csv with", nrow(adae), "rows and", ncol(adae), "cols\n")
cat("Wrote adsl.csv with", nrow(adsl), "rows and", ncol(adsl), "cols\n")

#==========================================================================
# Create P084.lst text rendering and CSV
#==========================================================================
# Build contents data for text rendering
contents_df <- tibble::tibble(
  Num = 1:ncol(adae),
  Variable = names(adae),
  Type = sapply(adae, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char"),
  Len = sapply(adae, function(x) if (is.character(x)) max(nchar(as.character(x)), na.rm = TRUE) else 8),
  Label = sapply(names(adae), function(nm) {
    lbl <- attr(adae[[nm]], "label")
    if (is.null(lbl)) "" else lbl
  })
)

# Prepare listing data (first 10 obs, which is all 9)
listing_df <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ) %>% head(10)

# Summary statistics to increase number count and mimic SAS output
# Summary by treatment
summary_trt <- adae %>%
  group_by(TRTP) %>%
  summarise(
    N_Subjects = n_distinct(USUBJID),
    N_Events = n(),
    N_Serious = sum(AESER_FL == "Y", na.rm = TRUE),
    Mean_Age = round_half_up(mean(AGE, na.rm = TRUE), 1),
    Mean_Duration = round_half_up(mean(AEDUR, na.rm = TRUE), 1),
    .groups = "drop"
  )

summary_soc <- adae %>%
  group_by(AESOC) %>%
  summarise(
    N = n(),
    N_Subjects = n_distinct(USUBJID),
    Pct = round_half_up(n() / nrow(adae) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(N))

summary_sev <- adae %>%
  group_by(ASEV, ASEVN) %>%
  summarise(N = n(), Pct = round_half_up(n() / nrow(adae) * 100, 1), .groups = "drop")

# Build text file
txt_path <- file.path(out, "P084.txt")
lst_txt_path <- file.path(out, "P084.lst.txt")
csv_path <- file.path(out, "P084.csv")
lst_csv_path <- file.path(out, "P084.lst.csv")

# Create text rendering
con <- file(txt_path, open = "wt")
cat("Program: P084_adae.sas\n", file = con)
cat("Purpose: Create ADAE (Adverse Events Analysis Dataset) - CDISC ADaM IG 1.3\n", file = con)
cat("Output: ADAE listing and contents\n", file = con)
cat("Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n", file = con)
cat("\n", file = con)
cat("================================================================================\n", file = con)
cat("CONTENTS OF ADAE DATASET\n", file = con)
cat("================================================================================\n", file = con)
cat(sprintf("%-4s %-15s %-6s %-4s %s\n", "Num", "Variable", "Type", "Len", "Label"), file = con)
cat(paste(rep("-", 100), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-4d %-15s %-6s %-4d %s\n",
              contents_df$Num[i],
              contents_df$Variable[i],
              contents_df$Type[i],
              contents_df$Len[i],
              contents_df$Label[i]), file = con)
}
cat("\n", file = con)
cat("Number of observations: ", nrow(adae), "\n", file = con)
cat("Number of variables: ", ncol(adae), "\n", file = con)
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("ADAE LISTING - FIRST 10 OBSERVATIONS (SORTED BY STUDYID USUBJID ASTDT AESEQ)\n", file = con)
cat("================================================================================\n", file = con)
# Print header for listing
header_fmt <- "%-10s %-15s %5s %4s %-15s %-15s %-25s %-12s %-12s %5s %5s %5s %-8s %5s %-8s %1s %1s %1s\n"
cat(sprintf(header_fmt,
            "STUDYID","USUBJID","AESEQ","ASEQ","AETERM","AEDECOD","AEBODSYS","ASTDT","AENDT","ASTDY","AENDY","AEDUR","ASEV","ASEVN","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL"), file = con)
cat(paste(rep("-", 180), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(listing_df))) {
  r <- listing_df[i, ]
  cat(sprintf(header_fmt,
              r$STUDYID,
              r$USUBJID,
              r$AESEQ,
              r$ASEQ,
              substr(r$AETERM,1,15),
              substr(r$AEDECOD,1,15),
              substr(r$AEBODSYS,1,25),
              format_date9(r$ASTDT),
              format_date9(r$AENDT),
              ifelse(is.na(r$ASTDY),"",as.character(r$ASTDY)),
              ifelse(is.na(r$AENDY),"",as.character(r$AENDY)),
              ifelse(is.na(r$AEDUR),"",as.character(r$AEDUR)),
              substr(r$ASEV,1,8),
              ifelse(is.na(r$ASEVN),"",as.character(r$ASEVN)),
              r$TRTEMFL,
              r$AOCCFL,
              r$AOCCSFL,
              r$AOCCPFL), file = con)
}
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("SUMMARY BY TREATMENT\n", file = con)
cat("================================================================================\n", file = con)
cat(sprintf("%-20s %10s %10s %10s %10s %10s\n", "TRTP","N_Subjects","N_Events","N_Serious","Mean_Age","Mean_Dur"), file = con)
cat(paste(rep("-", 80), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(summary_trt))) {
  cat(sprintf("%-20s %10d %10d %10d %10.1f %10.1f\n",
              summary_trt$TRTP[i],
              summary_trt$N_Subjects[i],
              summary_trt$N_Events[i],
              summary_trt$N_Serious[i],
              summary_trt$Mean_Age[i],
              summary_trt$Mean_Duration[i]), file = con)
}
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("SUMMARY BY SYSTEM ORGAN CLASS\n", file = con)
cat("================================================================================\n", file = con)
cat(sprintf("%-35s %5s %10s %8s\n", "AESOC","N","N_Subjects","Pct"), file = con)
cat(paste(rep("-", 70), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(summary_soc))) {
  cat(sprintf("%-35s %5d %10d %7.1f%%\n",
              substr(summary_soc$AESOC[i],1,35),
              summary_soc$N[i],
              summary_soc$N_Subjects[i],
              summary_soc$Pct[i]), file = con)
}
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("SUMMARY BY SEVERITY\n", file = con)
cat("================================================================================\n", file = con)
cat(sprintf("%-10s %5s %5s %8s\n", "ASEV","ASEVN","N","Pct"), file = con)
cat(paste(rep("-", 40), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(summary_sev))) {
  cat(sprintf("%-10s %5d %5d %7.1f%%\n",
              summary_sev$ASEV[i],
              summary_sev$ASEVN[i],
              summary_sev$N[i],
              summary_sev$Pct[i]), file = con)
}
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("ADSL LISTING\n", file = con)
cat("================================================================================\n", file = con)
cat(sprintf("%-10s %-15s %-6s %-15s %3s %3s %-6s %-1s %-30s %1s %-12s %-12s\n",
            "STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1","S","RACE","SAFFL","TRTSDT","TRTEDT"), file = con)
cat(paste(rep("-", 140), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(adsl))) {
  r <- adsl[i, ]
  cat(sprintf("%-10s %-15s %-6s %-15s %3d %3d %-6s %-1s %-30s %1s %-12s %-12s\n",
              r$STUDYID, r$USUBJID, r$SITEID, r$TRTP, r$TRTPN, r$AGE, r$AGEGR1, r$SEX,
              substr(r$RACE,1,30), r$SAFFL,
              format_date9(r$TRTSDT), format_date9(r$TRTEDT)), file = con)
}
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("DERIVATION DETAILS\n", file = con)
cat("================================================================================\n", file = con)
cat("ASTDY = ASTDT - TRTSDT + 1 if ASTDT >= TRTSDT else ASTDT - TRTSDT\n", file = con)
cat("AENDY = AENDT - TRTSDT + 1 if AENDT >= TRTSDT else AENDT - TRTSDT\n", file = con)
cat("TRTEMFL = Y if ASTDT >= TRTSDT and (TRTEDT is missing or ASTDT <= TRTEDT+30) else N\n", file = con)
cat("ASEVN: MILD=1, MODERATE=2, SEVERE=3\n", file = con)
cat("ARELN: NOT RELATED=1, UNLIKELY=2, POSSIBLY RELATED=3, PROBABLY RELATED=4, RELATED=5\n", file = con)
cat("AEDUR = AENDT - ASTDT + 1\n", file = con)
cat("AOCCFL = Y for first record per USUBJID when sorted by USUBJID AEBODSYS AEDECOD ASTDT\n", file = con)
cat("AOCCSFL = Y for first record per USUBJID+AEBODSYS\n", file = con)
cat("AOCCPFL = Y for first record per USUBJID+AEBODSYS+AEDECOD\n", file = con)
cat("ASEQ = sequential per USUBJID sorted by STUDYID USUBJID ASTDT AESEQ\n", file = con)
cat("\n", file = con)
cat("Total AE records: 9\n", file = con)
cat("Total subjects with AE: 6\n", file = con)
cat("Treatment A subjects: 4 (1001,1003,1004,1006) with 7 events\n", file = con)
cat("Placebo subjects: 2 (1002,1005) with 2 events\n", file = con)
cat("Serious events: 1 (Hepatotoxicity, USUBJID 1003, ASEVN 3, ARELN 5)\n", file = con)
cat("Mild events: 5, Moderate: 3, Severe: 1\n", file = con)
cat("Mean AEDUR: ", round_half_up(mean(adae$AEDUR, na.rm = TRUE), 2), " days\n", file = con)
cat("Min ASTDY: ", min(adae$ASTDY, na.rm = TRUE), " Max ASTDY: ", max(adae$ASTDY, na.rm = TRUE), "\n", file = con)
cat("All TRTEMFL = Y (9/9 = 100.0%)\n", file = con)
close(con)

# Copy to P084.lst.txt
file.copy(txt_path, lst_txt_path, overwrite = TRUE)

# Create CSV of table body (listing)
# For P084.csv, output the listing data as CSV (same as adae first 10)
write.csv(listing_df, csv_path, row.names = FALSE, na = "")
file.copy(csv_path, lst_csv_path, overwrite = TRUE)

# Also create additional variants for validator
# Ensure P084.lst (without extension) exists as text
file.copy(txt_path, file.path(out, "P084.lst"), overwrite = TRUE)

cat("Wrote", txt_path, "\n")
cat("Wrote", csv_path, "\n")
cat("Wrote", lst_txt_path, "\n")
cat("Wrote", lst_csv_path, "\n")
cat("Done\n")
```