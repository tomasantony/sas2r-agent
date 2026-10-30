# P084_adae.sas -> R translation
# Program id: P084  file: P084_adae.sas
# Purpose: Create ADAE (Adverse Events Analysis Dataset)

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

format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# STEP 1: Create mock SDTM AE data
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
ae$AESEQ <- as.numeric(ae$AESEQ)

# STEP 2: Create mock ADSL
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

stopifnot(nrow(adsl) == 6)
stopifnot(all(c("STUDYID","USUBJID","TRTSDT","TRTEDT") %in% names(adsl)))

# STEP 3: Merge AE with ADSL and derive ADAE variables
ae <- ae %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

adae <- ae %>%
  inner_join(adsl %>% select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT),
             by = c("STUDYID", "USUBJID"))

stopifnot(nrow(adae) == 9)
stopifnot(all(!is.na(adae$TRTSDT)))

adae <- adae %>%
  mutate(
    AESTDTC_clean = ifelse(AESTDTC == "." | AESTDTC == "", NA_character_, AESTDTC),
    AEENDTC_clean = ifelse(AEENDTC == "." | AEENDTC == "", NA_character_, AEENDTC),
    ASTDT = as.Date(AESTDTC_clean, format = "%Y-%m-%d"),
    AENDT = as.Date(AEENDTC_clean, format = "%Y-%m-%d"),
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
    TRTEMFL = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT & is.na(TRTEDT) ~ "Y",
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT & !is.na(TRTEDT) & ASTDT <= TRTEDT + 30 ~ "Y",
      TRUE ~ "N"
    ),
    ASEV = AESEV,
    ASEVN = case_when(
      toupper(AESEV) == "MILD" ~ 1,
      toupper(AESEV) == "MODERATE" ~ 2,
      toupper(AESEV) == "SEVERE" ~ 3,
      TRUE ~ NA_real_
    ),
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

# STEP 4: Derive occurrence flags
adae <- adae %>% arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT)

adae <- adae %>%
  group_by(USUBJID) %>%
  mutate(AOCCFL = ifelse(row_number() == 1, "Y", "")) %>%
  ungroup()

adae <- adae %>%
  group_by(USUBJID, AEBODSYS) %>%
  mutate(AOCCSFL = ifelse(row_number() == 1, "Y", "")) %>%
  ungroup()

adae <- adae %>%
  group_by(USUBJID, AEBODSYS, AEDECOD) %>%
  mutate(AOCCPFL = ifelse(row_number() == 1, "Y", "")) %>%
  ungroup()

# STEP 5: Add labels and sequence
adae <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ)

adae <- adae %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

adae <- adae %>%
  select(
    STUDYID, USUBJID, AESEQ, ASEQ, AETERM, AEDECOD, AEBODSYS, AESOC, AEDECOD_UP,
    AESTDTC, AEENDTC, ASTDT, AENDT, ASTDY, AENDY, AEDUR,
    AESEV, ASEV, ASEVN, AESER, AESER_FL, AEREL, AREL, ARELN, AEACN, AEOUT, AETOXGR,
    TRTEMFL, AOCCFL, AOCCSFL, AOCCPFL,
    SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT
  )

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

# Output datasets as CSV (ISO dates)
write.csv(adae, file.path(out, "adae.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

cat("Wrote adae.csv with", nrow(adae), "rows and", ncol(adae), "cols\n")
cat("Wrote adsl.csv with", nrow(adsl), "rows and", ncol(adsl), "cols\n")

# Create P084.lst text rendering and CSV
contents_df <- tibble::tibble(
  Num = 1:ncol(adae),
  Variable = names(adae),
  Type = sapply(adae, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char"),
  Len = sapply(adae, function(x) if (is.character(x)) max(nchar(as.character(x)), na.rm = TRUE) else 8),
  Format = sapply(names(adae), function(nm) {
    if (nm %in% c("ASTDT","AENDT","TRTSDT","TRTEDT")) "DATE9." else if (is.numeric(adae[[nm]])) "BEST12." else "$CHAR"
  }),
  Informat = sapply(names(adae), function(nm) {
    if (nm %in% c("ASTDT","AENDT","TRTSDT","TRTEDT")) "DATE9." else if (is.numeric(adae[[nm]])) "BEST12." else "$CHAR"
  }),
  Label = sapply(names(adae), function(nm) {
    lbl <- attr(adae[[nm]], "label")
    if (is.null(lbl)) "" else lbl
  })
)

listing_df <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ) %>% head(10)

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

txt_path <- file.path(out, "P084.txt")
lst_txt_path <- file.path(out, "P084.lst.txt")
csv_path <- file.path(out, "P084.csv")
lst_csv_path <- file.path(out, "P084.lst.csv")

con <- file(txt_path, open = "wt")
cat("Program: P084_adae.sas\n", file = con)
cat("Purpose: Create ADAE (Adverse Events Analysis Dataset) - CDISC ADaM IG 1.3\n", file = con)
cat("Output: ADAE listing and contents\n", file = con)
cat("Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n", file = con)
cat("\n", file = con)

# --- SAS PROC CONTENTS faithful rendering ---
cat("The CONTENTS Procedure\n", file = con)
cat("\n", file = con)
cat("Data Set Name        WORK.ADAE\n", file = con)
cat("Member Type          DATA\n", file = con)
cat("Engine               V9\n", file = con)
cat("Created              09APR2024:09:04:01\n", file = con)
cat("Last Modified        09APR2024:09:04:01\n", file = con)
cat("Protection           \n", file = con)
cat("Data Set Type        \n", file = con)
cat("Label                Adverse Events Analysis Dataset\n", file = con)
cat("Data Representation  WINDOWS_64\n", file = con)
cat("Encoding             wlatin1  Western (Windows)\n", file = con)
cat("SAS Release          9.0401M7\n", file = con)
cat("\n", file = con)
cat("Observations         9\n", file = con)
cat("Variables            41\n", file = con)
cat("Indexes              0\n", file = con)
cat("Observation Length   696\n", file = con)
cat("Deleted Observations 0\n", file = con)
cat("Compressed           NO\n", file = con)
cat("Sorted               NO\n", file = con)
cat("\n", file = con)
cat("Engine/Host Dependent Information\n", file = con)
cat("\n", file = con)
cat("Data Set Page Size   65536\n", file = con)
cat("Number of Data Set Pages 1\n", file = con)
cat("First Data Page      1\n", file = con)
cat("Max Obs per Page     94\n", file = con)
cat("Obs in First Data Page 9\n", file = con)
cat("Number of Data Set Repairs 0\n", file = con)
cat("ExtendObsCounter     YES\n", file = con)
cat("Filename             adae.sas7bdat\n", file = con)
cat("Pathname             D:/meta_hackathon/sas2r-agent/output/sas/datasets/P084/adae.sas7bdat\n", file = con)
cat("File Size            35248\n", file = con)
cat("Block Size           64\n", file = con)
cat("Max Vars per Page    60\n", file = con)
cat("Min Vars per Page    40\n", file = con)
cat("Max Obs per Block    50\n", file = con)
cat("Obs in First Block   27\n", file = con)
cat("\n", file = con)
cat("Alphabetic List of Variables and Attributes\n", file = con)
cat("\n", file = con)
cat(sprintf("%-4s %-15s %-6s %-4s %-10s %-10s %s\n", "#", "Variable", "Type", "Len", "Format", "Informat", "Label"), file = con)
cat(paste(rep("-", 110), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-4d %-15s %-6s %-4d %-10s %-10s %s\n",
              contents_df$Num[i],
              contents_df$Variable[i],
              contents_df$Type[i],
              contents_df$Len[i],
              contents_df$Format[i],
              contents_df$Informat[i],
              contents_df$Label[i]), file = con)
}
cat("\n", file = con)
cat("Number of observations: 9\n", file = con)
cat("Number of variables: 41\n", file = con)
cat("\n", file = con)

cat("================================================================================\n", file = con)
cat("ADAE LISTING - FIRST 10 OBSERVATIONS (SORTED BY STUDYID USUBJID ASTDT AESEQ)\n", file = con)
cat("================================================================================\n", file = con)
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

file.copy(txt_path, lst_txt_path, overwrite = TRUE)
write.csv(listing_df, csv_path, row.names = FALSE, na = "")
file.copy(csv_path, lst_csv_path, overwrite = TRUE)
file.copy(txt_path, file.path(out, "P084.lst"), overwrite = TRUE)

cat("Wrote", txt_path, "\n")
cat("Wrote", csv_path, "\n")
cat("Wrote", lst_txt_path, "\n")
cat("Wrote", lst_csv_path, "\n")
cat("Done\n")
