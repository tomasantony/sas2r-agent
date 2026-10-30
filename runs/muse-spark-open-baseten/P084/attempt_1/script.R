out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: parse SAS DATE9. like 15JAN2024
parse_date9 <- function(x) {
  # x is character like "15JAN2024"
  as.Date(toupper(x), format = "%d%b%Y")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
  library(ggplot2)
})

# ==========================================================================
# STEP 1: Create mock SDTM AE data
# ==========================================================================
ae <- tibble::tibble(
  STUDYID  = c("STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01"),
  USUBJID  = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  AESEQ    = c(1,2,1,1,2,3,1,1,1),
  AETERM   = c("Headache","Nausea","Fatigue","Dizziness","Rash","Hepatotoxicity","Insomnia","Vomiting","Back pain"),
  AEDECOD  = c("Headache","Nausea","Fatigue","Dizziness","Rash","Hepatotoxicity","Insomnia","Vomiting","Back pain"),
  AEBODSYS = c("Nervous system disorders","Gastrointestinal disorders","General disorders","Nervous system disorders","Skin disorders","Hepatobiliary disorders","Psychiatric disorders","Gastrointestinal disorders","Musculoskeletal disorders"),
  AESTDTC  = c("2024-02-01","2024-02-15","2024-02-10","2024-02-20","2024-03-05","2024-03-25","2024-02-25","2024-03-01","2024-02-28"),
  AEENDTC  = c("2024-02-03","2024-02-18","2024-03-01","2024-02-22","2024-03-15",".","2024-03-10","2024-03-02","2024-03-05"),
  AESEV    = c("MILD","MODERATE","MILD","MILD","MODERATE","SEVERE","MILD","MODERATE","MILD"),
  AESER    = c("N","N","N","N","N","Y","N","N","N"),
  AEREL    = c("NOT RELATED","POSSIBLY RELATED","POSSIBLY RELATED","NOT RELATED","PROBABLY RELATED","RELATED","UNLIKELY","POSSIBLY RELATED","NOT RELATED"),
  AEACN    = c("DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE REDUCED","DRUG WITHDRAWN","DOSE NOT CHANGED","DOSE NOT CHANGED","DOSE NOT CHANGED"),
  AEOUT    = c("RECOVERED","RECOVERED","RECOVERED","RECOVERED","RECOVERED","NOT RECOVERED","RECOVERED","RECOVERED","RECOVERED"),
  AETOXGR  = c("1","2","1","1","2","3","1","2","1")
)

# ==========================================================================
# STEP 2: Create mock ADSL
# ==========================================================================
adsl_raw <- tibble::tibble(
  STUDYID = c("STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01"),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SITEID  = c("101","101","102","102","103","103"),
  TRTP    = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN   = c(1,2,1,1,2,1),
  AGE     = c(59,45,43,51,36,68),
  AGEGR1  = c("<65","<65","<65","<65","<65",">=65"),
  SEX     = c("M","F","M","F","M","F"),
  RACE    = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL   = c("Y","Y","Y","Y","Y","Y"),
  TRTSDT_c = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_c = c("15JUN2024","18JUN2024","01APR2024","25JUN2024","","01JUL2024")
)

adsl <- adsl_raw %>%
  mutate(
    TRTSDT = parse_date9(TRTSDT_c),
    TRTEDT = parse_date9(TRTEDT_c)
  ) %>%
  select(-TRTSDT_c, -TRTEDT_c)

# Ensure SAS sort order: character ordering uppercase before lowercase, missing first
# For these data, simple arrange is sufficient
ae <- ae %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# ==========================================================================
# STEP 3: Merge AE with ADSL and derive ADAE variables
# ==========================================================================
# SAS: merge ae(in=a) adsl(in=b keep=...) by STUDYID USUBJID; if a and b;
adae <- inner_join(ae, adsl %>% select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT),
                   by = c("STUDYID","USUBJID"))

adae <- adae %>%
  mutate(
    ASTDT = as.Date(AESTDTC, format = "%Y-%m-%d"),
    AENDT = case_when(
      AEENDTC == "" | AEENDTC == "." | is.na(AEENDTC) ~ as.Date(NA),
      TRUE ~ as.Date(AEENDTC, format = "%Y-%m-%d")
    )
  ) %>%
  mutate(
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ as.numeric(ASTDT - TRTSDT + 1),
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT  ~ as.numeric(ASTDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ as.numeric(AENDT - TRTSDT + 1),
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT < TRTSDT  ~ as.numeric(AENDT - TRTSDT),
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
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
  )

# Format dates for display (SAS date9. equivalent stored as Date)
# Keep as Date objects; write.csv will output YYYY-MM-DD

# ==========================================================================
# STEP 4: Derive occurrence flags
# ==========================================================================
# proc sort data=adae; by USUBJID AEBODSYS AEDECOD ASTDT; run;
adae <- adae %>% arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT)

# SAS BY processing: by USUBJID AEBODSYS AEDECOD;
# first.USUBJID, first.AEBODSYS, first.AEDECOD
adae <- adae %>%
  group_by(USUBJID) %>%
  mutate(
    .rn_usubjid = row_number()
  ) %>%
  ungroup() %>%
  group_by(USUBJID, AEBODSYS) %>%
  mutate(
    .rn_soc = row_number()
  ) %>%
  ungroup() %>%
  group_by(USUBJID, AEBODSYS, AEDECOD) %>%
  mutate(
    .rn_pt = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    AOCCFL  = ifelse(.rn_usubjid == 1, "Y", ""),
    AOCCSFL = ifelse(.rn_soc == 1, "Y", ""),
    AOCCPFL = ifelse(.rn_pt == 1, "Y", "")
  ) %>%
  select(-.rn_usubjid, -.rn_soc, -.rn_pt)

# ==========================================================================
# STEP 5: Add labels and sequence
# ==========================================================================
# proc sort data=adae; by STUDYID USUBJID ASTDT AESEQ; run;
adae <- adae %>% arrange(STUDYID, USUBJID, ASTDT, AESEQ)

adae <- adae %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(
    ASEQ = row_number()
  ) %>%
  ungroup()

# Reorder columns to match SAS order as closely as possible
# Original AE vars + ADSL vars + derived
adae <- adae %>%
  select(
    STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT,
    AESEQ, ASEQ, AETERM, AEDECOD, AEBODSYS, AESOC, AEDECOD_UP,
    AESTDTC, AEENDTC, AESEV, AESER, AEREL, AEACN, AEOUT, AETOXGR,
    ASTDT, AENDT, ASTDY, AENDY, AEDUR,
    ASEV, ASEVN, AREL, ARELN, TRTEMFL, AESER_FL, AOCCFL, AOCCSFL, AOCCPFL
  )

# Attach labels as attributes (for documentation)
attr(adae$STUDYID, "label")  <- "Study Identifier"
attr(adae$USUBJID, "label")  <- "Unique Subject Identifier"
attr(adae$AESEQ, "label")    <- "Sequence Number"
attr(adae$ASEQ, "label")     <- "Analysis Sequence Number"
attr(adae$AETERM, "label")   <- "Reported Term for the Adverse Event"
attr(adae$AEDECOD, "label")  <- "Dictionary-Derived Term"
attr(adae$AEBODSYS, "label") <- "Body System or Organ Class"
attr(adae$AESOC, "label")    <- "Primary System Organ Class"
attr(adae$ASTDT, "label")    <- "Analysis Start Date"
attr(adae$AENDT, "label")    <- "Analysis End Date"
attr(adae$ASTDY, "label")    <- "Analysis Start Relative Day"
attr(adae$AENDY, "label")    <- "Analysis End Relative Day"
attr(adae$ASEV, "label")     <- "Analysis Severity/Intensity"
attr(adae$ASEVN, "label")    <- "Analysis Severity/Intensity (N)"
attr(adae$AREL, "label")     <- "Analysis Causality"
attr(adae$ARELN, "label")    <- "Analysis Causality (N)"
attr(adae$TRTEMFL, "label")  <- "Treatment Emergent Analysis Flag"
attr(adae$AESER_FL, "label")<- "Serious Event Flag"
attr(adae$AOCCFL, "label")   <- "First Occurrence of Any AE Flag"
attr(adae$AOCCSFL, "label")  <- "First Occurrence Within SOC Flag"
attr(adae$AOCCPFL, "label")  <- "First Occurrence of Preferred Term Flag"
attr(adae$AEDUR, "label")    <- "AE Duration (Days)"
attr(adae$TRTP, "label")     <- "Planned Treatment"
attr(adae$TRTPN, "label")    <- "Planned Treatment (N)"
attr(adae$TRTSDT, "label")   <- "Date of First Exposure to Treatment"
attr(adae$TRTEDT, "label")   <- "Date of Last Exposure to Treatment"
attr(adae$SAFFL, "label")    <- "Safety Population Flag"

# Ensure adsl column order matches SAS
adsl <- adsl %>%
  select(STUDYID, USUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, TRTSDT, TRTEDT) %>%
  arrange(STUDYID, USUBJID)

# ==========================================================================
# Output datasets as CSV
# ==========================================================================
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, na = ""
write.csv(adae, file.path(out, "adae.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ==========================================================================
# Create P084.lst text rendering and CSV
# ==========================================================================
# Build a listing similar to PROC PRINT obs=10 and PROC CONTENTS
# Text file should contain same numbers SAS shows

# Prepare display data for listing (first 10 obs, but we have 9)
adae_print <- adae

# Create text rendering
txt_lines <- c(
  "Program: P084_adae.sas",
  "Purpose: Create ADAE (Adverse Events Analysis Dataset) - CDISC ADaM IG 1.3",
  "Output: ADAE listing",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  "",
  "==========================================================================",
  "PROC CONTENTS - ADAE",
  "==========================================================================",
  sprintf("%-12s %-40s %-10s", "Variable", "Label", "Type"),
  paste(rep("-", 70), collapse = ""),
  sprintf("%-12s %-40s %-10s", "STUDYID", "Study Identifier", "Char"),
  sprintf("%-12s %-40s %-10s", "USUBJID", "Unique Subject Identifier", "Char"),
  sprintf("%-12s %-40s %-10s", "SITEID", "Site Identifier", "Char"),
  sprintf("%-12s %-40s %-10s", "TRTP", "Planned Treatment", "Char"),
  sprintf("%-12s %-40s %-10s", "TRTPN", "Planned Treatment (N)", "Num"),
  sprintf("%-12s %-40s %-10s", "AGE", "Age", "Num"),
  sprintf("%-12s %-40s %-10s", "AGEGR1", "Age Group", "Char"),
  sprintf("%-12s %-40s %-10s", "SEX", "Sex", "Char"),
  sprintf("%-12s %-40s %-10s", "RACE", "Race", "Char"),
  sprintf("%-12s %-40s %-10s", "SAFFL", "Safety Population Flag", "Char"),
  sprintf("%-12s %-40s %-10s", "TRTSDT", "Date of First Exposure to Treatment", "Date"),
  sprintf("%-12s %-40s %-10s", "TRTEDT", "Date of Last Exposure to Treatment", "Date"),
  sprintf("%-12s %-40s %-10s", "AESEQ", "Sequence Number", "Num"),
  sprintf("%-12s %-40s %-10s", "ASEQ", "Analysis Sequence Number", "Num"),
  sprintf("%-12s %-40s %-10s", "AETERM", "Reported Term for the Adverse Event", "Char"),
  sprintf("%-12s %-40s %-10s", "AEDECOD", "Dictionary-Derived Term", "Char"),
  sprintf("%-12s %-40s %-10s", "AEBODSYS", "Body System or Organ Class", "Char"),
  sprintf("%-12s %-40s %-10s", "AESOC", "Primary System Organ Class", "Char"),
  sprintf("%-12s %-40s %-10s", "ASTDT", "Analysis Start Date", "Date"),
  sprintf("%-12s %-40s %-10s", "AENDT", "Analysis End Date", "Date"),
  sprintf("%-12s %-40s %-10s", "ASTDY", "Analysis Start Relative Day", "Num"),
  sprintf("%-12s %-40s %-10s", "AENDY", "Analysis End Relative Day", "Num"),
  sprintf("%-12s %-40s %-10s", "AEDUR", "AE Duration (Days)", "Num"),
  sprintf("%-12s %-40s %-10s", "ASEV", "Analysis Severity/Intensity", "Char"),
  sprintf("%-12s %-40s %-10s", "ASEVN", "Analysis Severity/Intensity (N)", "Num"),
  sprintf("%-12s %-40s %-10s", "AREL", "Analysis Causality", "Char"),
  sprintf("%-12s %-40s %-10s", "ARELN", "Analysis Causality (N)", "Num"),
  sprintf("%-12s %-40s %-10s", "TRTEMFL", "Treatment Emergent Analysis Flag", "Char"),
  sprintf("%-12s %-40s %-10s", "AESER_FL", "Serious Event Flag", "Char"),
  sprintf("%-12s %-40s %-10s", "AOCCFL", "First Occurrence of Any AE Flag", "Char"),
  sprintf("%-12s %-40s %-10s", "AOCCSFL", "First Occurrence Within SOC Flag", "Char"),
  sprintf("%-12s %-40s %-10s", "AOCCPFL", "First Occurrence of Preferred Term Flag", "Char"),
  "",
  sprintf("Number of observations: %d", nrow(adae)),
  sprintf("Number of variables: %d", ncol(adae)),
  "",
  "==========================================================================",
  "PROC PRINT - ADAE (obs=10)",
  "==========================================================================",
  ""
)

# Build column header for print
# Use a compact listing showing key variables
header_fmt <- "%-14s %5s %5s %-20s %-28s %-28s %-12s %-12s %6s %6s %6s %-10s %5s %-18s %5s %1s %1s %1s %1s"
header_vals <- sprintf(header_fmt,
  "USUBJID","AESEQ","ASEQ","AETERM","AEDECOD","AEBODSYS","ASTDT","AENDT","ASTDY","AENDY","AEDUR","ASEV","ASEVN","AREL","ARELN","TRT","SER","OCC","SOC","PT")
txt_lines <- c(txt_lines, header_vals)
txt_lines <- c(txt_lines, paste(rep("-", 220), collapse=""))

# Add rows
for (i in seq_len(nrow(adae_print))) {
  r <- adae_print[i, ]
  line <- sprintf(header_fmt,
    substr(r$USUBJID,1,14),
    format(r$AESEQ, width=5),
    format(r$ASEQ, width=5),
    substr(r$AETERM,1,20),
    substr(r$AEDECOD,1,28),
    substr(r$AEBODSYS,1,28),
    ifelse(is.na(r$ASTDT), "", format(r$ASTDT, "%Y-%m-%d")),
    ifelse(is.na(r$AENDT), "", format(r$AENDT, "%Y-%m-%d")),
    ifelse(is.na(r$ASTDY), "", format(r$ASTDY, width=6)),
    ifelse(is.na(r$AENDY), "", format(r$AENDY, width=6)),
    ifelse(is.na(r$AEDUR), "", format(r$AEDUR, width=6)),
    substr(r$ASEV,1,10),
    ifelse(is.na(r$ASEVN), "", format(r$ASEVN, width=5)),
    substr(r$AREL,1,18),
    ifelse(is.na(r$ARELN), "", format(r$ARELN, width=5)),
    substr(r$TRTEMFL,1,1),
    substr(r$AESER_FL,1,1),
    substr(r$AOCCFL,1,1),
    substr(r$AOCCSFL,1,1),
    substr(r$AOCCPFL,1,1)
  )
  txt_lines <- c(txt_lines, line)
}

txt_lines <- c(txt_lines,
  "",
  "--------------------------------------------------------------------------",
  "Summary statistics",
  "--------------------------------------------------------------------------",
  sprintf("Total AEs: %d", nrow(adae)),
  sprintf("Total subjects with AE: %d", length(unique(adae$USUBJID))),
  sprintf("Total subjects in ADSL: %d", nrow(adsl)),
  sprintf("Treatment-emergent AEs (TRTEMFL=Y): %d", sum(adae$TRTEMFL=="Y", na.rm=TRUE)),
  sprintf("Serious AEs (AESER=Y): %d", sum(adae$AESER_FL=="Y", na.rm=TRUE)),
  sprintf("Mild (ASEVN=1): %d", sum(adae$ASEVN==1, na.rm=TRUE)),
  sprintf("Moderate (ASEVN=2): %d", sum(adae$ASEVN==2, na.rm=TRUE)),
  sprintf("Severe (ASEVN=3): %d", sum(adae$ASEVN==3, na.rm=TRUE)),
  sprintf("Mean AEDUR: %.2f", mean(adae$AEDUR, na.rm=TRUE)),
  sprintf("Mean ASTDY: %.2f", mean(adae$ASTDY, na.rm=TRUE)),
  sprintf("Mean AENDY: %.2f", mean(adae$AENDY, na.rm=TRUE)),
  sprintf("Mean AGE: %.2f", mean(adsl$AGE, na.rm=TRUE)),
  sprintf("Min AGE: %d", min(adsl$AGE, na.rm=TRUE)),
  sprintf("Max AGE: %d", max(adsl$AGE, na.rm=TRUE)),
  sprintf("Treatment A (TRTPN=1): %d subjects", sum(adsl$TRTPN==1, na.rm=TRUE)),
  sprintf("Placebo (TRTPN=2): %d subjects", sum(adsl$TRTPN==2, na.rm=TRUE)),
  "",
  "AE by SOC:",
  paste0(capture.output(adae %>% count(AEBODSYS, name="n") %>% arrange(AEBODSYS) %>% mutate(txt=sprintf("  %s: %d", AEBODSYS, n)) %>% pull(txt)), collapse="\n"),
  "",
  "AE by Severity:",
  paste0(capture.output(adae %>% count(ASEV, ASEVN) %>% arrange(ASEVN) %>% mutate(txt=sprintf("  %s (%d): %d", ASEV, ASEVN, n)) %>% pull(txt)), collapse="\n"),
  "",
  "AE by Relationship:",
  paste0(capture.output(adae %>% count(AREL, ARELN) %>% arrange(ARELN) %>% mutate(txt=sprintf("  %s (%d): %d", AREL, ARELN, n)) %>% pull(txt)), collapse="\n"),
  "",
  "--------------------------------------------------------------------------",
  "End of listing",
  "--------------------------------------------------------------------------"
)

# Write text files (both naming conventions)
writeLines(txt_lines, file.path(out, "P084.txt"))
writeLines(txt_lines, file.path(out, "P084.lst.txt"))

# CSV of table body (same as adae listing)
write.csv(adae_print, file.path(out, "P084.csv"), row.names = FALSE, na = "")
write.csv(adae_print, file.path(out, "P084.lst.csv"), row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")
cat(sprintf("adae: %d rows, %d cols\n", nrow(adae), ncol(adae)))
cat(sprintf("adsl: %d rows, %d cols\n", nrow(adsl), ncol(adsl)))
