# P079_l_sae.sas -> R translation
# Listing 16.2.7.2 - Listing of Serious Adverse Events
# Population: Safety Analysis Set (SAFFL='Y')

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS catx
catx <- function(sep, ...) {
  args <- c(...)
  args <- args[!is.na(args) & trimws(args) != ""]
  if (length(args) == 0) return("")
  paste(args, collapse = sep)
}

# Helper: SAS round half away from zero (not strictly needed here but kept for fidelity)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: parse SAS yymmdd10. (YYYY-MM-DD)
parse_yymmdd10 <- function(x) {
  # SAS input(..., ??yymmdd10.) returns missing on invalid without error
  # We mimic by parsing first 10 chars as YYYY-MM-DD
  if (is.na(x) || trimws(x) == "") return(as.Date(NA))
  s <- substr(trimws(as.character(x)), 1, 10)
  # try YYYY-MM-DD
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # try YYYY/MM/DD
  d <- suppressWarnings(as.Date(s, format = "%Y/%m/%d"))
  if (!is.na(d)) return(d)
  # try with lubridate fallback
  d2 <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d2)) return(as.Date(d2))
  as.Date(NA)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
})

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# Function to read SAS dataset from directory (sas7bdat or xpt)
read_adam <- function(libpath, name) {
  # name lower case e.g. "adae"
  # try various extensions
  candidates <- list.files(libpath, pattern = paste0("^", name, "\\."), ignore.case = TRUE, full.names = TRUE)
  if (length(candidates) == 0) {
    # also try without extension pattern
    candidates <- list.files(libpath, ignore.case = TRUE, full.names = TRUE)
    candidates <- candidates[grepl(paste0("^", name, "\\."), basename(candidates), ignore.case = TRUE)]
  }
  if (length(candidates) == 0) stop(paste0("Dataset not found: ", name, " in ", libpath))
  f <- candidates[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    haven::read_sas(f)
  } else if (ext == "xpt") {
    haven::read_xpt(f)
  } else if (ext == "xpt2" || ext == "xpt") {
    haven::read_xpt(f)
  } else {
    # try read_sas anyway, fallback to read_xpt
    tryCatch(haven::read_sas(f), error = function(e) haven::read_xpt(f))
  }
}

# Load ADSL and ADAE
adsl <- read_adam(adam_path, "adsl")
adae <- read_adam(adam_path, "adae")

# Ensure character columns are character (haven may give labelled)
adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
adae <- adae %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

# Standardize column names to uppercase for matching SAS (case-insensitive)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Check required columns exist
# ADSL: USUBJID, TRTA, AGE, SEX, RACE, TRTSDT, SAFFL
# ADAE: USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC, AETOXGR, AEREL, AEACN, AEOUT, AESER, AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE

# Convert TRTSDT to Date if numeric SAS date (haven usually does)
# If numeric and not Date, convert from SAS origin 1960-01-01
if ("TRTSDT" %in% names(adsl)) {
  if (is.numeric(adsl$TRTSDT) && !inherits(adsl$TRTSDT, "Date")) {
    # SAS date is days since 1960-01-01
    adsl$TRTSDT <- as.Date(adsl$TRTSDT, origin = "1960-01-01")
  } else {
    adsl$TRTSDT <- as.Date(adsl$TRTSDT)
  }
}

# Step 1: Prepare SAE listing data (PROC SQL)
# select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, b.TRTSDT as TRT_START, ...
# where b.SAFFL='Y' and a.AESER='Y' order by b.TRTA, a.USUBJID, a.AESTDTC

# Filter ADSL to SAFFL='Y'
adsl_f <- adsl %>% filter(SAFFL == "Y")

# Filter ADAE to AESER='Y'
adae_f <- adae %>% filter(AESER == "Y")

# Inner join
sae_list <- adae_f %>%
  inner_join(adsl_f %>% select(USUBJID, TRTA, AGE, SEX, RACE, TRTSDT),
             by = "USUBJID") %>%
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
  # SAS sorts with character ordering; arrange accordingly
  arrange(TRTA, USUBJID, START_DATE)

# Step 2: Derive SAE criteria display
# Use rowwise for catx building
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
    },
    # Days from first dose to onset
    onset_day = {
      if (!is.na(TRT_START) && !is.na(START_DATE) && trimws(START_DATE) != "") {
        d <- parse_yymmdd10(START_DATE)
        if (!is.na(d) && !is.na(TRT_START)) {
          onset <- as.integer(d - as.Date(TRT_START) + 1L)
          # SAS: strip(put(_onset, 8.))
          as.character(onset)
        } else {
          ""
        }
      } else {
        ""
      }
    },
    # Demographics string: catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1)))
    demog = {
      age_str <- ifelse(is.na(AGE), "", trimws(format(AGE, trim = TRUE, scientific = FALSE)))
      # Remove trailing .0 if integer-like? format already does
      # SAS best. for integer gives no decimal
      # Ensure no scientific
      sex_str <- ifelse(is.na(SEX), "", trimws(as.character(SEX)))
      race_str <- ifelse(is.na(RACE), "", substr(trimws(as.character(RACE)), 1, 1))
      catx("/", age_str, sex_str, race_str)
    }
  ) %>%
  ungroup()

# Ensure TRT_START is Date for output consistency (convert to character YYYY-MM-DD for CSV if needed)
# Keep TRT_START as Date internally, but for display we don't show it directly

# Select final columns for report (as in PROC REPORT)
# columns TRTA USUBJID demog SOC PT START_DATE END_DATE onset_day GRADE RELATIONSHIP ACTION_TAKEN OUTCOME sae_criteria
final_cols <- c("TRTA","USUBJID","demog","SOC","PT","START_DATE","END_DATE","onset_day","GRADE","RELATIONSHIP","ACTION_TAKEN","OUTCOME","sae_criteria")
sae_report_final <- sae_report %>%
  select(all_of(final_cols)) %>%
  # Ensure character columns are not NA for CSV (SAS missing -> blank)
  mutate(across(everything(), ~as.character(.)))

# Replace NA with "" for display (SAS missing)
sae_report_final[is.na(sae_report_final)] <- ""

# Order already by TRTA, USUBJID, START_DATE (as SAS)
# Ensure SAS sort stability: TRTA character order
# Already arranged

# Write CSV dataset (SAS dataset counterpart)
write.csv(sae_report_final, file.path(out, "l_sae.csv"), row.names = FALSE, na = "")

# Also write intermediate datasets as CSV for traceability (optional)
# write.csv(sae_list, file.path(out, "sae_list.csv"), row.names=FALSE, na="")
# write.csv(sae_report, file.path(out, "sae_report.csv"), row.names=FALSE, na="")

# Step 3: Generate listing text rendering
# Titles from u_titles.sas for L_SAE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
studyid <- "[STUDYID]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Listing 16.2.7.2"
title5 <- "Listing of Serious Adverse Events"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate)
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P079_l_sae.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

# Build text file
txt_file <- file.path(out, "l_sae.txt")
rtf_file <- file.path(out, "l_sae.rtf")

# Create header lines
header_lines <- c(
  title1,
  title2,
  "",
  title4,
  title5,
  title6,
  "",
  paste0(rep("-", 140), collapse = ""),
  paste(
    sprintf("%-12s", "Treatment"),
    sprintf("%-12s", "Subject ID"),
    sprintf("%-10s", "Age/Sex/"),
    sprintf("%-18s", "SOC"),
    sprintf("%-16s", "Preferred"),
    sprintf("%-12s", "Start"),
    sprintf("%-12s", "End"),
    sprintf("%-6s", "Study"),
    sprintf("%-6s", "Grade"),
    sprintf("%-12s", "Relation-"),
    sprintf("%-12s", "Action"),
    sprintf("%-12s", "Outcome"),
    sprintf("%-20s", "SAE"),
    sep = " | "
  ),
  paste(
    sprintf("%-12s", ""),
    sprintf("%-12s", ""),
    sprintf("%-10s", "Race"),
    sprintf("%-18s", ""),
    sprintf("%-16s", "Term"),
    sprintf("%-12s", "Date"),
    sprintf("%-12s", "Date"),
    sprintf("%-6s", "Day"),
    sprintf("%-6s", ""),
    sprintf("%-12s", "ship"),
    sprintf("%-12s", "Taken"),
    sprintf("%-12s", ""),
    sprintf("%-20s", "Criteria"),
    sep = " | "
  ),
  paste0(rep("-", 140), collapse = "")
)

# Build body lines grouped by TRTA with break after TRTA / page
body_lines <- c()
# Get unique TRTA in order
trt_order <- unique(sae_report_final$TRTA)
for (trt in trt_order) {
  sub <- sae_report_final %>% filter(TRTA == trt)
  # Add treatment header
  body_lines <- c(body_lines, paste0("Treatment: ", trt), paste0(rep("-", 80), collapse = ""))
  for (i in seq_len(nrow(sub))) {
    row <- sub[i, ]
    line <- paste(
      sprintf("%-12s", substr(row$TRTA, 1, 12)),
      sprintf("%-12s", substr(row$USUBJID, 1, 12)),
      sprintf("%-10s", substr(row$demog, 1, 10)),
      sprintf("%-18s", substr(row$SOC, 1, 18)),
      sprintf("%-16s", substr(row$PT, 1, 16)),
      sprintf("%-12s", substr(row$START_DATE, 1, 12)),
      sprintf("%-12s", substr(row$END_DATE, 1, 12)),
      sprintf("%-6s", substr(row$onset_day, 1, 6)),
      sprintf("%-6s", substr(row$GRADE, 1, 6)),
      sprintf("%-12s", substr(row$RELATIONSHIP, 1, 12)),
      sprintf("%-12s", substr(row$ACTION_TAKEN, 1, 12)),
      sprintf("%-12s", substr(row$OUTCOME, 1, 12)),
      sprintf("%-20s", substr(row$sae_criteria, 1, 20)),
      sep = " | "
    )
    body_lines <- c(body_lines, line)
  }
  body_lines <- c(body_lines, "", paste0("[Page break after ", trt, "]"), "")
}

footer_lines <- c(
  paste0(rep("-", 140), collapse = ""),
  footnote1,
  footnote2,
  footnote3,
  ""
)

all_lines <- c(header_lines, body_lines, footer_lines)

# Write txt
writeLines(all_lines, con = txt_file, useBytes = TRUE)

# Write rtf as plain text RTF wrapper (simple)
# For compatibility, also write same content to .rtf
# Create minimal RTF header
rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Courier;}}\n\\viewkind4\\uc1\\pard\\f0\\fs16\n"
rtf_footer <- "\n}"
# Escape RTF special chars
escape_rtf <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x
}
rtf_body <- paste(escape_rtf(all_lines), collapse = "\\par\n")
writeLines(paste0(rtf_header, rtf_body, rtf_footer), con = rtf_file, useBytes = TRUE)

# Also write CSV of table body (same as l_sae.csv already)
# Ensure l_sae.csv is the table body CSV (already done)

# Log progress
cat("P079_l_sae.R completed successfully.\n")
cat(paste0("Rows in SAE listing: ", nrow(sae_report_final), "\n"))
cat(paste0("Output written to: ", out, "\n"))
