# P079_l_sae.sas -> R translation
# Listing 16.2.7.2 - Listing of Serious Adverse Events

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
  library(purrr)
})

# Helper: read SAS dataset from libref directory (adam / sdtm)
read_sas_lib <- function(libpath, dsname) {
  # libpath is directory containing .sas7bdat or .xpt files
  # dsname lower case without extension
  if (!dir.exists(libpath)) return(NULL)
  # try sas7bdat
  f1 <- file.path(libpath, paste0(dsname, ".sas7bdat"))
  f2 <- file.path(libpath, paste0(toupper(dsname), ".sas7bdat"))
  f3 <- file.path(libpath, paste0(dsname, ".xpt"))
  f4 <- file.path(libpath, paste0(toupper(dsname), ".xpt"))
  # also search case-insensitive
  allfiles <- list.files(libpath, full.names = TRUE, ignore.case = TRUE)
  cand <- allfiles[grepl(paste0("^", dsname, "\\.(sas7bdat|xpt)$"), basename(allfiles), ignore.case = TRUE)]
  if (length(cand) > 0) {
    fp <- cand[1]
    if (grepl("\\.sas7bdat$", fp, ignore.case = TRUE)) {
      return(haven::read_sas(fp))
    } else {
      return(haven::read_xpt(fp))
    }
  }
  if (file.exists(f1)) return(haven::read_sas(f1))
  if (file.exists(f2)) return(haven::read_sas(f2))
  if (file.exists(f3)) return(haven::read_xpt(f3))
  if (file.exists(f4)) return(haven::read_xpt(f4))
  return(NULL)
}

# Resolve adam path: prefer synthetic path used in librefs, fallback to env vars
adam_paths <- c(
  "/bench/data/raw/own-synthetic/adam",
  Sys.getenv("rawadam"),
  "/bench/data/raw/cdisc-pilot/adam",
  file.path(Sys.getenv("root"), "data/raw/own-synthetic/adam"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam")
)
adam_paths <- unique(adam_paths[adam_paths != "" & !is.na(adam_paths)])

adsl <- NULL
adae <- NULL
for (p in adam_paths) {
  if (is.null(adsl)) adsl <- read_sas_lib(p, "adsl")
  if (is.null(adae)) adae <- read_sas_lib(p, "adae")
  if (!is.null(adsl) && !is.null(adae)) break
}

if (is.null(adsl)) stop("ADSL not found in any adam path: ", paste(adam_paths, collapse="; "))
if (is.null(adae)) stop("ADAE not found in any adam path: ", paste(adam_paths, collapse="; "))

# Standardize column names to uppercase for matching SAS (haven preserves case)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Ensure required columns exist
# ADSL: USUBJID, TRTA, AGE, SEX, RACE, TRTSDT, SAFFL
# ADAE: USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC, AETOXGR, AEREL, AEACN, AEOUT, AESER, AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE

# Helper to convert SAS numeric date to Date if needed
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS origin 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # try yymmdd10. strict: YYYY-MM-DD
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    return(d)
  }
  return(as.Date(NA))
}

# TRTSDT may be numeric SAS date or Date
# Ensure TRTSDT is Date
if ("TRTSDT" %in% names(adsl)) {
  adsl$TRTSDT_DATE <- to_date(adsl$TRTSDT)
  # if haven already converted to Date, to_date returns same
  # if numeric, converted
} else {
  stop("TRTSDT not found in ADSL")
}

# Step 1: Prepare SAE listing data (PROC SQL)
# select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, b.TRTSDT as TRT_START, a.AEBODSYS as SOC, etc.
# where b.SAFFL='Y' and a.AESER='Y' order by b.TRTA, a.USUBJID, a.AESTDTC

# Filter ADSL to SAFFL='Y'
adsl_f <- adsl %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID, TRTA, AGE, SEX, RACE, TRTSDT_DATE)

# Filter ADAE to AESER='Y'
adae_f <- adae %>%
  filter(AESER == "Y") %>%
  select(USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC, AETOXGR, AEREL, AEACN, AEOUT,
         AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE)

# Inner join
sae_list <- inner_join(adae_f, adsl_f, by = "USUBJID") %>%
  transmute(
    USUBJID = USUBJID,
    TRTA = TRTA,
    AGE = AGE,
    SEX = SEX,
    RACE = RACE,
    TRT_START = TRTSDT_DATE,
    SOC = AEBODSYS,
    PT = AEDECOD,
    VERBATIM = AETERM,
    START_DATE = as.character(AESTDTC),
    END_DATE = as.character(AEENDTC),
    GRADE = AETOXGR,
    RELATIONSHIP = AEREL,
    ACTION_TAKEN = AEACN,
    OUTCOME = AEOUT,
    AESDTH = AESDTH,
    AESLIFE = AESLIFE,
    AESHOSP = AESHOSP,
    AESDISAB = AESDISAB,
    AESCONG = AESCONG,
    AESMIE = AESMIE
  ) %>%
  arrange(TRTA, USUBJID, START_DATE)

# Step 2: Derive SAE criteria display
# SAS catx(', ', ...) skips empty strings
catx <- function(sep, ...) {
  vals <- c(...)
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return("")
  paste(vals, collapse = sep)
}

sae_report <- sae_list %>%
  rowwise() %>%
  mutate(
    sae_criteria = {
      sc <- ""
      if (!is.na(AESDTH) && AESDTH == "Y") sc <- catx(", ", sc, "Death")
      if (!is.na(AESLIFE) && AESLIFE == "Y") sc <- catx(", ", sc, "Life-threatening")
      if (!is.na(AESHOSP) && AESHOSP == "Y") sc <- catx(", ", sc, "Hospitalization")
      if (!is.na(AESDISAB) && AESDISAB == "Y") sc <- catx(", ", sc, "Disability")
      if (!is.na(AESCONG) && AESCONG == "Y") sc <- catx(", ", sc, "Congenital anomaly")
      if (!is.na(AESMIE) && AESMIE == "Y") sc <- catx(", ", sc, "Medically important")
      sc
    }
  ) %>%
  ungroup() %>%
  mutate(
    # Days from first dose to onset: input(START_DATE, yymmdd10.) - TRT_START + 1
    # Strict yymmdd10. = %Y-%m-%d
    START_DT = suppressWarnings(as.Date(START_DATE, format = "%Y-%m-%d")),
    `_onset` = as.numeric(START_DT - TRT_START + 1),
    onset_day = ifelse(!is.na(TRT_START) & !is.na(START_DT),
                       as.character(as.integer(`_onset`)),
                       ""),
    # Demographics string: catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1)))
    demog = {
      age_str <- ifelse(is.na(AGE), "", trimws(format(AGE, scientific = FALSE, trim = TRUE)))
      # SAS best. for integer ages gives no decimal; format above may give " 45"
      # Remove trailing .0 if any
      age_str <- sub("\\.0+$", "", age_str)
      age_str <- trimws(age_str)
      sex_str <- ifelse(is.na(SEX), "", trimws(as.character(SEX)))
      race_str <- ifelse(is.na(RACE), "", trimws(substr(as.character(RACE), 1, 1)))
      # catx('/', ...)
      parts <- c(age_str, sex_str, race_str)
      parts <- parts[parts != ""]
      if (length(parts) == 0) "" else paste(parts, collapse = "/")
    }
  ) %>%
  select(-START_DT, -`_onset`)

# Order columns as in PROC REPORT
sae_report_final <- sae_report %>%
  select(TRTA, USUBJID, demog, SOC, PT, START_DATE, END_DATE, onset_day,
         GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME, sae_criteria, VERBATIM, TRT_START, AGE, SEX, RACE) %>%
  arrange(TRTA, USUBJID, START_DATE)

# For CSV output, keep report columns plus key identifiers
# Write CSV with SAS variable names, dates as YYYY-MM-DD, na=""
sae_csv <- sae_report_final %>%
  mutate(
    TRT_START = ifelse(is.na(TRT_START), "", format(TRT_START, "%Y-%m-%d"))
  ) %>%
  select(TRTA, USUBJID, demog, SOC, PT, START_DATE, END_DATE, onset_day,
         GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME, sae_criteria)

# Ensure character columns are not factors and handle NA
sae_csv_out <- sae_csv %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))

write.csv(sae_csv_out, file.path(out, "l_sae.csv"), row.names = FALSE, na = "")

# Also write full dataset for traceability (optional, but required: each output dataset as CSV)
# Already done as l_sae.csv

# Step 3: Generate listing text rendering
# Titles from u_titles.sas for L_SAE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

title1 <- paste0(sponsor, "    Page 1 of N")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Listing 16.2.7.2"
title5 <- "Listing of Serious Adverse Events"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
# Program path and run date
prog_name <- "P079_l_sae.sas"
run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")
footnote3 <- paste0("Program: ", prog_name, " | Run: ", run_date, " ", run_time)

# Build text file
txt_path <- file.path(out, "l_sae.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(paste0("Study: ", studyid), con)
writeLines("", con)

# Column headers
header1 <- sprintf("%-12s %-12s %-10s %-18s %-18s %-12s %-12s %-8s %-6s %-12s %-12s %-12s %-20s",
                   "Treatment", "Subject", "Age/Sex/", "SOC", "Preferred", "Start", "End", "Study", "Grade", "Relation-", "Action", "Outcome", "SAE")
header2 <- sprintf("%-12s %-12s %-10s %-18s %-18s %-12s %-12s %-8s %-6s %-12s %-12s %-12s %-20s",
                   "", "ID", "Race", "", "Term", "Date", "Date", "Day", "", "ship", "Taken", "", "Criteria")
sep_line <- paste(rep("-", 180), collapse = "")

writeLines(sep_line, con)
writeLines(header1, con)
writeLines(header2, con)
writeLines(sep_line, con)

# Break after TRTA / page - emulate by grouping
# Sort already by TRTA, USUBJID, START_DATE
current_trta <- ""
for (i in seq_len(nrow(sae_report_final))) {
  row <- sae_report_final[i, ]
  # Page break after TRTA
  if (row$TRTA != current_trta) {
    if (current_trta != "") {
      writeLines("", con)
      writeLines(paste0("--- Page break after Treatment: ", current_trta, " ---"), con)
      writeLines("", con)
      writeLines(sep_line, con)
      writeLines(header1, con)
      writeLines(header2, con)
      writeLines(sep_line, con)
    }
    current_trta <- row$TRTA
  }
  # Truncate fields to fit widths similar to SAS style widths
  trta_disp <- substr(ifelse(is.na(row$TRTA), "", row$TRTA), 1, 12)
  usubjid_disp <- substr(ifelse(is.na(row$USUBJID), "", row$USUBJID), 1, 12)
  demog_disp <- substr(ifelse(is.na(row$demog), "", row$demog), 1, 10)
  soc_disp <- substr(ifelse(is.na(row$SOC), "", row$SOC), 1, 18)
  pt_disp <- substr(ifelse(is.na(row$PT), "", row$PT), 1, 18)
  start_disp <- substr(ifelse(is.na(row$START_DATE), "", row$START_DATE), 1, 12)
  end_disp <- substr(ifelse(is.na(row$END_DATE), "", row$END_DATE), 1, 12)
  onset_disp <- substr(ifelse(is.na(row$onset_day), "", row$onset_day), 1, 8)
  grade_disp <- substr(ifelse(is.na(row$GRADE), "", row$GRADE), 1, 6)
  rel_disp <- substr(ifelse(is.na(row$RELATIONSHIP), "", row$RELATIONSHIP), 1, 12)
  act_disp <- substr(ifelse(is.na(row$ACTION_TAKEN), "", row$ACTION_TAKEN), 1, 12)
  out_disp <- substr(ifelse(is.na(row$OUTCOME), "", row$OUTCOME), 1, 12)
  crit_disp <- substr(ifelse(is.na(row$sae_criteria), "", row$sae_criteria), 1, 20)

  line <- sprintf("%-12s %-12s %-10s %-18s %-18s %-12s %-12s %-8s %-6s %-12s %-12s %-12s %-20s",
                  trta_disp, usubjid_disp, demog_disp, soc_disp, pt_disp,
                  start_disp, end_disp, onset_disp, grade_disp, rel_disp, act_disp, out_disp, crit_disp)
  writeLines(line, con)
}

writeLines(sep_line, con)
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
writeLines("", con)
writeLines(paste0("Total Serious Adverse Events: ", nrow(sae_report_final)), con)
# Count distinct subjects
n_subj <- length(unique(sae_report_final$USUBJID))
writeLines(paste0("Total Subjects with SAE: ", n_subj), con)
# Summary by treatment
if (nrow(sae_report_final) > 0) {
  writeLines("", con)
  writeLines("Summary by Treatment:", con)
  trt_summary <- sae_report_final %>%
    group_by(TRTA) %>%
    summarise(N_events = n(), N_subjects = n_distinct(USUBJID), .groups = "drop")
  for (j in seq_len(nrow(trt_summary))) {
    writeLines(sprintf("  %s: %d events in %d subjects", trt_summary$TRTA[j], trt_summary$N_events[j], trt_summary$N_subjects[j]), con)
  }
}

close(con)

# Also create a CSV of the table body for verification (same as l_sae.csv already)
# Ensure l_sae.rtf placeholder text file exists as well for compatibility (copy txt)
rtf_path <- file.path(out, "l_sae.rtf")
if (!file.exists(rtf_path)) {
  # Write minimal RTF wrapper around txt content
  txt_content <- readLines(txt_path, warn = FALSE)
  rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fswiss Arial;}}\\viewkind4\\uc1\\pard\\f0\\fs16"
  rtf_footer <- "\\par}"
  rtf_body <- paste(txt_content, collapse = "\\par\n")
  writeLines(c(rtf_header, rtf_body, rtf_footer), rtf_path)
}

# Clean up work datasets (SAS proc datasets delete) - not needed in R but log
message("NOTE: l_sae.sas completed successfully.")
message(paste0("Output written to: ", out))
message(paste0("  - ", file.path(out, "l_sae.csv"), " (", nrow(sae_csv_out), " rows)"))
message(paste0("  - ", txt_path))
