```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

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

propcase <- function(x) {
  ifelse(is.na(x) | x=="", x, stringr::str_to_title(tolower(x), locale="en"))
}

read_adam <- function(name, base="D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam") {
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
  if (dir.exists(base)) {
    files <- list.files(base, full.names=TRUE)
    idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case=TRUE)
    if (length(idx) > 0) {
      f <- files[idx[1]]
      if (grepl("\\.sas7bdat$", f, ignore.case=TRUE)) return(haven::read_sas(f))
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) return(haven::read_xpt(f))
      return(haven::read_sas(f))
    }
  }
  stop(paste0("Cannot find dataset ", name, " in ", base))
}

zap_all <- function(df) {
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      df[[nm]] <- as.character(haven::as_factor(df[[nm]], levels="labels"))
    }
  }
  df <- df %>% mutate(across(where(is.factor), as.character))
  df
}

studyid  <- "[STUDYID]"
protocol <- "[Protocol Number]"
sponsor  <- "[Sponsor Name]"
cutdate  <- "[DDMONYYYY]"

cat("Reading ADaM datasets...\n")
adae_raw <- read_adam("adae")
adsl_raw <- read_adam("adsl")

adae <- zap_all(adae_raw)
adsl <- zap_all(adsl_raw)

names(adae) <- toupper(names(adae))
names(adsl) <- toupper(names(adsl))

ensure_col <- function(df, col) {
  if (!col %in% names(df)) df[[col]] <- NA_character_
  df
}
for (c in c("USUBJID","TRTA","AGE","SEX","RACE","SAFFL")) adsl <- ensure_col(adsl, c)
for (c in c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC","AESEV","AEREL","AESER","AEACN","AEOUT","AETOXGR","TRTEMFL")) adae <- ensure_col(adae, c)

# Filter ADSL to Safety population first to avoid duplicate SAFFL column issues
adsl_saffl <- adsl %>%
  filter(!is.na(SAFFL) & SAFFL == "Y") %>%
  select(USUBJID, TRTA, AGE, SEX, RACE)

# Inner join and filter TRTEMFL='Y' - do filter before transmute and keep SAFFL logic already applied
# Use base filtering for TRTEMFL to avoid dplyr scoping issues
adae_filt <- adae[ !is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", , drop=FALSE]

ae_list <- adae_filt %>%
  inner_join(adsl_saffl, by="USUBJID") %>%
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
  ) %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

cat(paste0("ae_list rows: ", nrow(ae_list), "\n"))

# Step 2: Format display values
ae_report <- ae_list %>%
  mutate(
    severity_d = propcase(SEVERITY),
    rel_d = propcase(RELATIONSHIP),
    serious_d = ifelse(!is.na(SERIOUS) & SERIOUS == "Y", "Yes", "No"),
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
  ) %>%
  mutate(
    severity_d = substr(severity_d, 1, 10),
    rel_d = substr(rel_d, 1, 15),
    serious_d = substr(serious_d, 1, 3),
    action_d = substr(action_d, 1, 30),
    outcome_d = substr(outcome_d, 1, 30)
  ) %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

ae_report_final <- ae_report %>%
  select(TRTA, USUBJID, SOC, PT, VERBATIM, START_DATE, END_DATE,
         CTCAE_GRADE, severity_d, serious_d, rel_d, action_d, outcome_d)

format_date_col <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%S"))
  return(as.character(x))
}
ae_report_final$START_DATE <- format_date_col(ae_report_final$START_DATE)
ae_report_final$END_DATE <- format_date_col(ae_report_final$END_DATE)

# Write CSV
csv_path <- file.path(out, "l_ae.csv")
write.csv(ae_report_final, csv_path, row.names=FALSE, na="")
cat(paste0("CSV written to ", csv_path, " rows=", nrow(ae_report_final), "\n"))

# Text rendering
title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Listing 16.2.7.1"
title5 <- "Listing of Adverse Events"
title6 <- "Safety Analysis Set"
footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P076_l_ae.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

txt_path <- file.path(out, "l_ae.txt")
rtf_path <- file.path(out, "l_ae.rtf")

con <- file(txt_path, open="wt", encoding="UTF-8")
writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(paste(rep("-", 140), collapse=""), con)

header_labels <- c("Treatment", "Subject ID", "System Organ Class", "Preferred Term", "Verbatim Term",
                   "Start Date", "End Date", "CTCAE Grade", "Severity", "SAE", "Relationship", "Action Taken", "Outcome")
writeLines(paste(header_labels, collapse=" | "), con)
writeLines(paste(rep("-", 140), collapse=""), con)

current_trta <- NA_character_
for (i in seq_len(nrow(ae_report_final))) {
  row <- ae_report_final[i, ]
  if (!is.na(row$TRTA) && !identical(row$TRTA, current_trta)) {
    if (!is.na(current_trta)) {
      writeLines("", con)
      writeLines(paste0("--- Page Break after Treatment: ", current_trta, " ---"), con)
      writeLines("", con)
    }
    current_trta <- row$TRTA
  }
  vals <- sapply(row, function(x) ifelse(is.na(x), "", as.character(x)))
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

file.copy(txt_path, rtf_path, overwrite=TRUE)

cat(paste0("Text listing written to ", txt_path, "\n"))
cat(paste0("RTF listing written to ", rtf_path, "\n"))
cat("P076_l_ae.sas completed successfully.\n")
```