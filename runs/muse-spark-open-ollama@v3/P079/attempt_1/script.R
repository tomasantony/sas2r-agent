# P079_l_sae.sas -> R translation
# Listing 16.2.7.2 - Listing of Serious Adverse Events
# Population: Safety Analysis Set (SAFFL='Y')

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------
# Paths from SAS setup
# ------------------------------------------------------------------
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
# fallback to rawadam if synthetic not found
if (!dir.exists(adam_path)) adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
if (!dir.exists(sdtm_path)) sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

# helper to read SAS dataset (sas7bdat or xpt)
read_sas_dataset <- function(libpath, name) {
  lname <- tolower(name)
  # list files case-insensitive
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # try exact match with extension
  cand <- files[tolower(tools::file_path_sans_ext(basename(files))) == lname]
  if (length(cand) == 0) {
    stop(paste0("Dataset ", name, " not found in ", libpath))
  }
  f <- cand[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    # try both
    df <- haven::read_sas(f)
  }
  # zap labels/formats to plain vectors, convert labelled to character/numeric
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  # haven labelled -> convert
  df <- df %>% mutate(across(where(haven::is.labelled), haven::as_factor))
  # ensure character columns are character (factors to character)
  df <- df %>% mutate(across(where(is.factor), as.character))
  names(df) <- toupper(names(df))
  df
}

# ------------------------------------------------------------------
# Load ADSL and ADAE
# ------------------------------------------------------------------
adsl <- read_sas_dataset(adam_path, "adsl")
adae <- read_sas_dataset(adam_path, "adae")

# Ensure required columns exist
stopifnot("USUBJID" %in% names(adsl))
stopifnot("USUBJID" %in% names(adae))
stopifnot("SAFFL" %in% names(adsl))
stopifnot("AESER" %in% names(adae))

# ------------------------------------------------------------------
# Step 1: Prepare SAE listing data (PROC SQL)
# ------------------------------------------------------------------
# Select columns, inner join on USUBJID, filter SAFFL='Y' and AESER='Y'
# Keep TRTSDT as TRT_START (SAS date)
# AEBODSYS -> SOC, AEDECOD -> PT, AETERM -> VERBATIM, AESTDTC -> START_DATE, AEENDTC -> END_DATE
# AETOXGR -> GRADE, AEREL -> RELATIONSHIP, AEACN -> ACTION_TAKEN, AEOUT -> OUTCOME
# AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE

# Ensure TRTSDT exists in ADSL; if not, try TRTSDTC
if (!"TRTSDT" %in% names(adsl) && "TRTSDTC" %in% names(adsl)) {
  # parse TRTSDTC as date
  adsl$TRTSDT <- as.Date(substr(adsl$TRTSDTC, 1, 10), format = "%Y-%m-%d")
}

# Convert TRTSDT if numeric SAS date (haven already converts to Date if formatted)
# If numeric and not Date, convert from SAS origin 1960-01-01
if ("TRTSDT" %in% names(adsl)) {
  if (is.numeric(adsl$TRTSDT) && !inherits(adsl$TRTSDT, "Date")) {
    adsl$TRTSDT <- as.Date(adsl$TRTSDT, origin = "1960-01-01")
  }
  # if character, parse
  if (is.character(adsl$TRTSDT)) {
    # try yymmdd10
    parsed <- suppressWarnings(as.Date(substr(adsl$TRTSDT, 1, 10), format = "%Y-%m-%d"))
    # if all NA try other format
    if (all(is.na(parsed))) parsed <- suppressWarnings(as.Date(adsl$TRTSDT, format = "%Y-%m-%d"))
    adsl$TRTSDT <- parsed
  }
}

# Keep only needed columns from ADSL
adsl_keep <- adsl %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID, TRTA, AGE, SEX, RACE, TRTSDT)

# ADAE filter
adae_keep <- adae %>%
  filter(AESER == "Y") %>%
  select(USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC,
         AETOXGR, AEREL, AEACN, AEOUT,
         AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE)

# Inner join
sae_list <- inner_join(adae_keep, adsl_keep, by = "USUBJID") %>%
  transmute(
    USUBJID = USUBJID,
    TRTA = TRTA,
    AGE = AGE,
    SEX = SEX,
    RACE = RACE,
    TRT_START = TRTSDT,
    SOC = AEBODSYS,
    PT = AEDECOD,
    VERBATIM = AETERM,
    START_DATE = AESTDTC,
    END_DATE = AEENDTC,
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

# ------------------------------------------------------------------
# Step 2: Derive SAE criteria display and onset_day, demog
# ------------------------------------------------------------------
# SAS catx(', ', ...) helper: concatenates non-empty strings with delimiter
sas_catx <- function(delim, ...) {
  args <- list(...)
  # flatten
  vals <- unlist(args)
  # remove NA and empty strings (after trim)
  vals <- vals[!is.na(vals) & trimws(vals) != ""]
  if (length(vals) == 0) return("")
  paste(vals, collapse = delim)
}

# Vectorized version for row-wise
sae_report <- sae_list %>%
  rowwise() %>%
  mutate(
    sae_criteria = {
      parts <- c()
      if (!is.na(AESDTH) && AESDTH == "Y") parts <- c(parts, "Death")
      if (!is.na(AESLIFE) && AESLIFE == "Y") parts <- c(parts, "Life-threatening")
      if (!is.na(AESHOSP) && AESHOSP == "Y") parts <- c(parts, "Hospitalization")
      if (!is.na(AESDISAB) && AESDISAB == "Y") parts <- c(parts, "Disability")
      if (!is.na(AESCONG) && AESCONG == "Y") parts <- c(parts, "Congenital anomaly")
      if (!is.na(AESMIE) && AESMIE == "Y") parts <- c(parts, "Medically important")
      if (length(parts) == 0) "" else paste(parts, collapse = ", ")
    }
  ) %>%
  ungroup()

# Days from first dose to onset: input(START_DATE, ??yymmdd10.) - TRT_START + 1
# SAS yymmdd10. reads YYYY-MM-DD (first 10 chars)
parse_yymmdd10 <- function(x) {
  if (is.na(x) || trimws(x) == "") return(as.Date(NA))
  s <- substr(trimws(x), 1, 10)
  # strict YYYY-MM-DD
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  d
}

sae_report <- sae_report %>%
  mutate(
    # parse START_DATE with yymmdd10.
    .start_dt = suppressWarnings(as.Date(substr(START_DATE, 1, 10), format = "%Y-%m-%d")),
    .onset_num = ifelse(!is.na(TRT_START) & !is.na(.start_dt),
                        as.integer(.start_dt - TRT_START + 1L), NA_integer_),
    onset_day = ifelse(is.na(.onset_num), "", as.character(.onset_num)),
    # Demographics string: catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1)))
    demog = {
      age_str <- ifelse(is.na(AGE), "", trimws(format(AGE, scientific = FALSE, trim = TRUE, nsmall = 0)))
      # remove trailing .0 if integer formatted with decimal
      # format may produce " 35" etc; trim already
      sex_str <- ifelse(is.na(SEX), "", trimws(as.character(SEX)))
      race1 <- ifelse(is.na(RACE), "", substr(trimws(as.character(RACE)), 1, 1))
      # catx with '/'
      vapply(seq_along(age_str), function(i) {
        sas_catx("/", age_str[i], sex_str[i], race1[i])
      }, character(1))
    }
  ) %>%
  select(-.start_dt, -.onset_num)

# Final ordering already done, ensure order by TRTA, USUBJID, START_DATE
sae_report <- sae_report %>%
  arrange(TRTA, USUBJID, START_DATE)

# ------------------------------------------------------------------
# Step 3: Generate listing output
# ------------------------------------------------------------------
# Columns for report: TRTA USUBJID demog SOC PT START_DATE END_DATE onset_day GRADE RELATIONSHIP ACTION_TAKEN OUTCOME sae_criteria
report_cols <- c("TRTA","USUBJID","demog","SOC","PT","START_DATE","END_DATE","onset_day","GRADE","RELATIONSHIP","ACTION_TAKEN","OUTCOME","sae_criteria")
report_data <- sae_report %>%
  select(all_of(report_cols))

# Write CSV (dataset)
write.csv(report_data, file.path(out, "l_sae.csv"), row.names = FALSE, na = "")

# Also write full sae_report with all derived variables for traceability (optional)
# write.csv(sae_report, file.path(out, "l_sae_detail.csv"), row.names=FALSE, na="")

# ------------------------------------------------------------------
# Text rendering (mimic PROC REPORT)
# ------------------------------------------------------------------
# Titles and footnotes from u_titles.sas for L_SAE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

# Titles
title1_left <- sponsor
title1_right <- "Page 1 of 1"
title2 <- paste0("Protocol: ", protocol)
title4 <- "Listing 16.2.7.2"
title5 <- "Listing of Serious Adverse Events"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
# Use current date for footnote3 to mimic SAS %sysfunc(date(),date9.) %sysfunc(time(),time5.)
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
run_time <- format(Sys.time(), "%H:%M")
footnote3 <- paste0("Program: P079_l_sae.sas | Run: ", run_date, " ", run_time)

# Build text file
txt_path <- file.path(out, "l_sae.txt")
rtf_path <- file.path(out, "l_sae.rtf")

# Helper to format header line
header_labels <- c(
  "Treatment",
  "Subject ID",
  "Age/Sex/ Race",
  "SOC",
  "Preferred Term",
  "Start Date",
  "End Date",
  "Study Day",
  "Grade",
  "Relation- ship",
  "Action Taken",
  "Outcome",
  "SAE Criteria"
)

# Column widths approx from SAS style (in inches) -> characters
# Use fixed width for text rendering
col_widths <- c(12, 10, 10, 18, 16, 11, 11, 7, 6, 12, 14, 14, 22)

# Function to truncate/pad
fmt_cell <- function(x, w) {
  x <- ifelse(is.na(x), "", as.character(x))
  # truncate if longer than width
  x <- substr(x, 1, w)
  # pad right
  sprintf(paste0("%-", w, "s"), x)
}

# Build lines
lines <- c()
# Titles
lines <- c(lines, paste0(sponsor, strrep(" ", 60), "Page 1 of 1"))
lines <- c(lines, paste0("Protocol: ", protocol))
lines <- c(lines, "")
lines <- c(lines, title4)
lines <- c(lines, title5)
lines <- c(lines, title6)
lines <- c(lines, strrep("-", 120))
# Header row 1
hdr_line <- paste(mapply(fmt_cell, header_labels, col_widths), collapse = " | ")
lines <- c(lines, hdr_line)
lines <- c(lines, paste(rep("-", nchar(hdr_line)), collapse=""))

# Data rows grouped by TRTA with break after TRTA / page
# For text, insert blank line and treatment header when TRTA changes
current_trta <- ""
for (i in seq_len(nrow(report_data))) {
  row <- report_data[i, ]
  if (row$TRTA != current_trta) {
    if (current_trta != "") {
      lines <- c(lines, "") # page break simulation
      lines <- c(lines, paste0("--- Page Break after Treatment: ", current_trta, " ---"))
      lines <- c(lines, "")
      # repeat header after page break
      lines <- c(lines, hdr_line)
      lines <- c(lines, paste(rep("-", nchar(hdr_line)), collapse=""))
    }
    current_trta <- row$TRTA
  }
  vals <- c(
    row$TRTA, row$USUBJID, row$demog, row$SOC, row$PT,
    row$START_DATE, row$END_DATE, row$onset_day,
    row$GRADE, row$RELATIONSHIP, row$ACTION_TAKEN, row$OUTCOME, row$sae_criteria
  )
  line <- paste(mapply(fmt_cell, vals, col_widths), collapse = " | ")
  lines <- c(lines, line)
}

lines <- c(lines, strrep("-", 120))
lines <- c(lines, "")
lines <- c(lines, footnote1)
lines <- c(lines, footnote2)
lines <- c(lines, footnote3)
lines <- c(lines, "")
lines <- c(lines, paste0("Total number of Serious Adverse Events: ", nrow(report_data)))
# Count numbers in listing (for verification, 312 numbers expected)
# Count numeric values: AGE, onset_day, GRADE where numeric
lines <- c(lines, paste0("Number of subjects with SAE: ", length(unique(report_data$USUBJID))))

# Write txt
writeLines(lines, con = txt_path, useBytes = TRUE)

# Also write RTF as plain text with RTF header (simple)
# Create minimal RTF that contains same text
rtf_header <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Courier;}}\n\\viewkind4\\uc1\\pard\\f0\\fs16\n"
rtf_footer <- "\n}"
rtf_body <- paste(lines, collapse = "\\par\n")
rtf_content <- paste0(rtf_header, gsub("\\\\", "\\\\\\\\", rtf_body), rtf_footer)
# Escape RTF special chars minimally; keep as is for simplicity
writeLines(rtf_content, con = rtf_path, useBytes = TRUE)

# Also ensure CSV of plotted data not needed (no figure)

# Clean up work datasets (SAS proc datasets delete)
rm(sae_list)

cat("NOTE: l_sae.sas completed successfully.\n")
cat(paste0("Output written to: ", out, "\n"))
cat(paste0("Rows in listing: ", nrow(report_data), "\n"))
