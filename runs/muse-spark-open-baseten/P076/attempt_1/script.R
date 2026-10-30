# P076_l_ae.sas -> R translation
# Listing 16.2.7.1 - Listing of Adverse Events
# Population: Safety Analysis Set (SAFFL='Y'), TRTEMFL='Y'

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# --- libraries ---
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# --- helpers ---
`%||%` <- function(a,b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

round_half_up <- function(x, digits=0) {
  # SAS round half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

propcase <- function(x) {
  # SAS PROPCASE: first letter of each word uppercase, rest lower
  # Use str_to_title on lowercased input to mimic SAS
  ifelse(is.na(x) | x=="", x, stringr::str_to_title(tolower(x), locale="en"))
}

read_adam <- function(name, base="D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam") {
  # try common extensions and case variations
  candidates <- c(
    file.path(base, paste0(name, ".sas7bdat")),
    file.path(base, paste0(name, ".xpt")),
    file.path(base, paste0(tolower(name), ".sas7bdat")),
    file.path(base, paste0(tolower(name), ".xpt")),
    file.path(base, paste0(toupper(name), ".sas7bdat")),
    file.path(base, paste0(toupper(name), ".xpt"))
  )
  for (f in candidates) {
    if (file.exists(f)) {
      if (grepl("\\.sas7bdat$", f, ignore.case=TRUE)) return(haven::read_sas(f))
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) return(haven::read_xpt(f))
    }
  }
  # fallback: list files case-insensitive
  if (dir.exists(base)) {
    files <- list.files(base, full.names=TRUE)
    idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case=TRUE)
    if (length(idx) > 0) {
      f <- files[idx[1]]
      if (grepl("\\.sas7bdat$", f, ignore.case=TRUE)) return(haven::read_sas(f))
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) return(haven::read_xpt(f))
      # try read_sas anyway
      return(haven::read_sas(f))
    }
  }
  stop(paste0("Cannot find dataset ", name, " in ", base))
}

zap_all <- function(df) {
  # Convert haven_labelled to plain R types, keep character
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  # haven may still leave labelled; convert via as_factor where needed then to character
  # For each column that is haven_labelled, convert to character
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      df[[nm]] <- as.character(haven::as_factor(df[[nm]], levels="labels"))
    }
  }
  # Convert factors to character
  df <- df %>% mutate(across(where(is.factor), as.character))
  df
}

# --- global study parameters (from u_setup.sas) ---
studyid  <- "[STUDYID]"
protocol <- "[Protocol Number]"
sponsor  <- "[Sponsor Name]"
cutdate  <- "[DDMONYYYY]"

# --- Step 1: Prepare listing data ---
cat("Reading ADaM datasets...\n")
adae_raw <- read_adam("adae")
adsl_raw <- read_adam("adsl")

adae <- zap_all(adae_raw)
adsl <- zap_all(adsl_raw)

# Ensure expected columns exist (case-insensitive handling)
# haven preserves original case; SAS variables are uppercase
names(adae) <- toupper(names(adae))
names(adsl) <- toupper(names(adsl))

# Select needed columns, inner join on USUBJID, filter SAFFL='Y' and TRTEMFL='Y'
# SAS: select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, a.AEBODSYS as SOC, etc.
# Keep only required for listing but retain join keys

# Check column existence and create missing as NA
ensure_col <- function(df, col) {
  if (!col %in% names(df)) df[[col]] <- NA_character_
  df
}
for (c in c("USUBJID","TRTA","AGE","SEX","RACE")) adsl <- ensure_col(adsl, c)
for (c in c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC","AESEV","AEREL","AESER","AEACN","AEOUT","AETOXGR","TRTEMFL")) adae <- ensure_col(adae, c)

ae_list <- adae %>%
  inner_join(adsl %>% select(USUBJID, TRTA, AGE, SEX, RACE), by="USUBJID") %>%
  filter(SAFFL <- adsl$SAFFL[match(USUBJID, adsl$USUBJID)] , TRUE) # placeholder, will filter properly below

# Proper filter using adsl SAFFL: need to join SAFFL as well
adsl_saffl <- adsl %>% select(USUBJID, TRTA, AGE, SEX, RACE, SAFFL)
ae_list <- adae %>%
  inner_join(adsl_saffl, by="USUBJID") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y") %>%
  transmute(
    USUBJID = USUBJID,
    TRTA = TRTA,
    AGE = AGE,
    SEX = SEX,
    RACE = RACE,
    SOC = AEBODSYS,
    PT = AEDECOD,
    VERBATIM = AETERM,
    START_DATE = AESTDTC,
    END_DATE = AEENDTC,
    SEVERITY = AESEV,
    RELATIONSHIP = AEREL,
    SERIOUS = AESER,
    ACTION_TAKEN = AEACN,
    OUTCOME = AEOUT,
    CTCAE_GRADE = AETOXGR,
    TRTEMFL = TRTEMFL
  )

# SAS order by b.TRTA, a.USUBJID, a.AESTDTC, a.AEBODSYS, a.AEDECOD
# Use order with na.last=FALSE to mimic SAS missing first
# Character ordering in R is locale-dependent; enforce byte ordering via xtfrm or base order
# Use dplyr arrange which respects character ordering; set na handling via arrange with is.na
ae_list <- ae_list %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

cat(paste0("ae_list rows: ", nrow(ae_list), "\n"))

# --- Step 2: Format display values ---
ae_report <- ae_list %>%
  mutate(
    severity_d = propcase(SEVERITY),
    rel_d = propcase(RELATIONSHIP),
    serious_d = ifelse(!is.na(SERIOUS) & SERIOUS == "Y", "Yes", "No"),
    # ACTION_TAKEN mapping
    action_d = case_when(
      toupper(ACTION_TAKEN) == "DRUG WITHDRAWN"   ~ "Drug Withdrawn",
      toupper(ACTION_TAKEN) == "DOSE REDUCED"     ~ "Dose Reduced",
      toupper(ACTION_TAKEN) == "DRUG INTERRUPTED" ~ "Drug Interrupted",
      toupper(ACTION_TAKEN) == "DOSE NOT CHANGED" ~ "Dose Not Changed",
      toupper(ACTION_TAKEN) == "NOT APPLICABLE"   ~ "Not Applicable",
      is.na(ACTION_TAKEN)                         ~ NA_character_,
      ACTION_TAKEN == ""                          ~ "",
      TRUE                                        ~ propcase(ACTION_TAKEN)
    ),
    outcome_d = case_when(
      toupper(OUTCOME) == "RECOVERED/RESOLVED"               ~ "Resolved",
      toupper(OUTCOME) == "RECOVERING/RESOLVING"             ~ "Resolving",
      toupper(OUTCOME) == "NOT RECOVERED/NOT RESOLVED"       ~ "Not Resolved",
      toupper(OUTCOME) == "RECOVERED/RESOLVED WITH SEQUELAE" ~ "Resolved w/ Sequelae",
      toupper(OUTCOME) == "FATAL"                            ~ "Fatal",
      is.na(OUTCOME)                                         ~ NA_character_,
      OUTCOME == ""                                          ~ "",
      TRUE                                                   ~ propcase(OUTCOME)
    )
  )

# Ensure length constraints (SAS length) - truncate if needed (not strictly required for CSV)
ae_report <- ae_report %>%
  mutate(
    severity_d = substr(severity_d, 1, 10),
    rel_d = substr(rel_d, 1, 15),
    serious_d = substr(serious_d, 1, 3),
    action_d = substr(action_d, 1, 30),
    outcome_d = substr(outcome_d, 1, 30)
  )

# Re-order after formatting (SAS proc report order by TRTA, USUBJID)
ae_report <- ae_report %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# Select final columns for report (as in PROC REPORT)
ae_report_final <- ae_report %>%
  select(TRTA, USUBJID, SOC, PT, VERBATIM, START_DATE, END_DATE,
         CTCAE_GRADE, severity_d, serious_d, rel_d, action_d, outcome_d)

# --- Step 3: Generate listing outputs ---
# Write CSV (SAS dataset counterpart)
csv_path <- file.path(out, "l_ae.csv")
write.csv(ae_report_final, csv_path, row.names=FALSE, na="")

cat(paste0("CSV written to ", csv_path, " rows=", nrow(ae_report_final), "\n"))

# --- Text rendering (RTF-like plain text) ---
# Titles and footnotes as per u_titles.sas for L_AE
title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Listing 16.2.7.1"
title5 <- "Listing of Adverse Events"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P076_l_ae.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

# Build text file
txt_path <- file.path(out, "l_ae.txt")
rtf_path <- file.path(out, "l_ae.rtf")

# Create formatted text table
# Use fixed-width style for readability; also include tab-separated for exact values
con <- file(txt_path, open="wt", encoding="UTF-8")
on.exit(try(close(con), silent=TRUE), add=TRUE)

writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(paste(rep("-", 140), collapse=""), con)

# Header row with split '~' replaced by space
header_labels <- c("Treatment", "Subject ID", "System Organ Class", "Preferred Term", "Verbatim Term",
                   "Start Date", "End Date", "CTCAE Grade", "Severity", "SAE", "Relationship", "Action Taken", "Outcome")
# Write header
writeLines(paste(header_labels, collapse=" | "), con)
writeLines(paste(rep("-", 140), collapse=""), con)

# Write data rows, with page break after TRTA
# SAS break after TRTA / page -> insert page break marker
current_trta <- NA_character_
for (i in seq_len(nrow(ae_report_final))) {
  row <- ae_report_final[i, ]
  # Detect TRTA change for page break
  if (!is.na(row$TRTA) && !identical(row$TRTA, current_trta)) {
    if (!is.na(current_trta)) {
      writeLines("", con)
      writeLines(paste0("--- Page Break after Treatment: ", current_trta, " ---"), con)
      writeLines("", con)
    }
    current_trta <- row$TRTA
  }
  # Replace NA with "" for display, keep dates as YYYY-MM-DD (already)
  vals <- sapply(row, function(x) ifelse(is.na(x), "", as.character(x)))
  # Escape pipe in values
  vals <- gsub("\\|", "/", vals)
  writeLines(paste(vals, collapse=" | "), con)
}
writeLines(paste(rep("-", 140), collapse=""), con)
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
writeLines("", con)
writeLines(paste0("N = ", nrow(ae_report_final), " adverse event records (Safety Analysis Set, Treatment-Emergent)"), con)
close(con)

# Also create RTF file as copy of TXT (plain text RTF wrapper)
# For compatibility, write same content to .rtf
file.copy(txt_path, rtf_path, overwrite=TRUE)

cat(paste0("Text listing written to ", txt_path, "\n"))
cat(paste0("RTF listing written to ", rtf_path, "\n"))

# --- Also write a CSV of the table body for verification (same as l_ae.csv) ---
# Already done

# Clean up (SAS proc datasets delete)
rm(adae_raw, adsl_raw, adae, adsl, adsl_saffl, ae_list, ae_report)

cat("P076_l_ae.sas completed successfully.\n")
