# P076_l_ae.sas -> R translation
# Listing 16.2.7.1 - Listing of Adverse Events
# Population: Safety Analysis Set (SAFFL='Y'), TRTEMFL='Y'

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS propcase (first letter of each word uppercase, rest lower)
# SAS PROPCASE capitalizes first letter of each word delimited by non-alphanumeric
propcase_sas <- function(x) {
  ifelse(is.na(x), NA_character_, {
    sapply(x, function(s) {
      if (is.na(s) || s == "") return(s)
      # to lower first
      low <- tolower(s)
      # capitalize after word boundaries: start, or after space, hyphen, slash, etc.
      # SAS propcase uses any non-letter/digit as delimiter
      # Use gsub with perl
      gsub("(^|[^A-Za-z0-9])([a-z])", "\\1\\U\\2", low, perl = TRUE) |>
        # gsub with \U not supported in R; do manual
        identity()
    }, USE.NAMES = FALSE)
  })
}
# Implement propcase correctly without \U
propcase_sas <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    if (s == "") return("")
    low <- tolower(s)
    chars <- strsplit(low, "")[[1]]
    n <- length(chars)
    # capitalize first char if letter
    cap_next <- TRUE
    for (i in seq_len(n)) {
      ch <- chars[i]
      if (grepl("[A-Za-z0-9]", ch)) {
        if (cap_next) {
          chars[i] <- toupper(ch)
          cap_next <- FALSE
        }
      } else {
        cap_next <- TRUE
      }
    }
    paste(chars, collapse = "")
  }, character(1))
}

# Paths
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# List adam files
adam_files <- list.files(adam_path, full.names = TRUE)
# cat adam files for debugging
# print(adam_files)

library(haven)
library(dplyr)
library(stringr)
library(readr)

# Read ADSL and ADAE
# Try .sas7bdat, .xpt, .csv
find_adam_file <- function(name) {
  # name lower
  pat <- paste0("^", name, "\\.")
  f <- list.files(adam_path, pattern = pat, ignore.case = TRUE, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  f[1]
}

read_adam <- function(name) {
  f <- find_adam_file(name)
  if (is.null(f)) stop(paste("File not found for", name))
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    haven::read_sas(f)
  } else if (ext == "xpt") {
    haven::read_xpt(f)
  } else if (ext == "csv") {
    readr::read_csv(f, show_col_types = FALSE)
  } else {
    haven::read_sas(f)
  }
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")

# Ensure column names uppercase for consistency (SAS is case-insensitive)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Check required columns exist
# ADSL: USUBJID, TRTA, AGE, SEX, RACE, SAFFL
# ADAE: USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC, AESEV, AEREL, AESER, AEACN, AEOUT, AETOXGR, TRTEMFL
# If TRTA not in ADSL, try TRT01A or TRTP etc. But spec says TRTA
# Inspect
# print(names(adsl))
# print(names(adae))

# Coerce character columns: haven labelled -> convert to character where needed
to_char <- function(x) {
  if (inherits(x, "haven_labelled")) {
    # keep label values as character if character, else as is
    haven::as_factor(x, levels = "labels") |> as.character()
    # Actually for treatment, we want the formatted value
    # But haven::as_factor gives label; better to use zap_labels then as.character
  } else x
}

# Convert all haven_labelled to character/numeric appropriately
convert_haven <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # If labels exist, use labels where available, else original
      # For numeric with labels, keep numeric but also create character?
      # For our purposes, TRTA is likely labelled numeric; we want display label
      # Use as_factor then as.character for character-like
      # Check if underlying is character
      if (is.character(col) || is.character(haven::zap_labels(col))) {
        df[[nm]] <- as.character(haven::zap_labels(col))
        # If labelled character, zap_labels keeps character
      } else {
        # numeric with labels: convert to label if exists
        # Use haven::as_factor to get label, but keep as character
        df[[nm]] <- as.character(haven::as_factor(col, levels = "labels"))
      }
    }
  }
  df
}

adsl <- convert_haven(adsl)
adae <- convert_haven(adae)

# Ensure SAFFL, TRTEMFL are character
adsl$SAFFL <- as.character(adsl$SAFFL)
adae$TRTEMFL <- as.character(adae$TRTEMFL)

# Step 1: Prepare listing data - inner join
# SAS: select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, a.AEBODSYS as SOC, etc.
# where b.SAFFL='Y' and a.TRTEMFL='Y' order by b.TRTA, a.USUBJID, a.AESTDTC, a.AEBODSYS, a.AEDECOD

# Filter ADSL to SAFFL='Y'
adsl_f <- adsl %>% filter(SAFFL == "Y")

# Filter ADAE to TRTEMFL='Y'
adae_f <- adae %>% filter(TRTEMFL == "Y")

# Join
ae_list <- adae_f %>%
  inner_join(adsl_f %>% select(USUBJID, TRTA, AGE, SEX, RACE), by = "USUBJID") %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTA = as.character(TRTA),
    AGE = AGE,
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    SOC = as.character(AEBODSYS),
    PT = as.character(AEDECOD),
    VERBATIM = as.character(AETERM),
    START_DATE = as.character(AESTDTC),
    END_DATE = as.character(AEENDTC),
    SEVERITY = as.character(AESEV),
    RELATIONSHIP = as.character(AEREL),
    SERIOUS = as.character(AESER),
    ACTION_TAKEN = as.character(AEACN),
    OUTCOME = as.character(AEOUT),
    CTCAE_GRADE = as.character(AETOXGR),
    TRTEMFL = as.character(TRTEMFL)
  )

# Order by TRTA, USUBJID, AESTDTC, AEBODSYS, AEDECOD
# SAS sorts character with uppercase before lowercase; R default is similar but locale dependent
# Use base order with character ordering
ae_list <- ae_list %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# Step 2: Format display values
ae_report <- ae_list %>%
  mutate(
    severity_d = propcase_sas(SEVERITY),
    rel_d = propcase_sas(RELATIONSHIP),
    serious_d = ifelse(SERIOUS == "Y", "Yes", "No"),
    # ACTION_TAKEN mapping
    action_d = case_when(
      toupper(ACTION_TAKEN) == "DRUG WITHDRAWN" ~ "Drug Withdrawn",
      toupper(ACTION_TAKEN) == "DOSE REDUCED" ~ "Dose Reduced",
      toupper(ACTION_TAKEN) == "DRUG INTERRUPTED" ~ "Drug Interrupted",
      toupper(ACTION_TAKEN) == "DOSE NOT CHANGED" ~ "Dose Not Changed",
      toupper(ACTION_TAKEN) == "NOT APPLICABLE" ~ "Not Applicable",
      is.na(ACTION_TAKEN) ~ NA_character_,
      TRUE ~ propcase_sas(ACTION_TAKEN)
    ),
    outcome_d = case_when(
      toupper(OUTCOME) == "RECOVERED/RESOLVED" ~ "Resolved",
      toupper(OUTCOME) == "RECOVERING/RESOLVING" ~ "Resolving",
      toupper(OUTCOME) == "NOT RECOVERED/NOT RESOLVED" ~ "Not Resolved",
      toupper(OUTCOME) == "RECOVERED/RESOLVED WITH SEQUELAE" ~ "Resolved w/ Sequelae",
      toupper(OUTCOME) == "FATAL" ~ "Fatal",
      is.na(OUTCOME) ~ NA_character_,
      TRUE ~ propcase_sas(OUTCOME)
    )
  )

# Ensure ordering still by TRTA, USUBJID, START_DATE, SOC, PT
ae_report <- ae_report %>%
  arrange(TRTA, USUBJID, START_DATE, SOC, PT)

# For CSV output, keep all columns as in SAS ae_report
# SAS ae_report has: USUBJID, TRTA, AGE, SEX, RACE, SOC, PT, VERBATIM, START_DATE, END_DATE,
# SEVERITY, RELATIONSHIP, SERIOUS, ACTION_TAKEN, OUTCOME, CTCAE_GRADE, TRTEMFL,
# severity_d, serious_d, rel_d, action_d, outcome_d
# But final report columns are: TRTA USUBJID SOC PT VERBATIM START_DATE END_DATE CTCAE_GRADE severity_d serious_d rel_d action_d outcome_d
# We will output the report dataset as CSV with those display columns plus maybe all

# Prepare display dataset for report
report_cols <- c("TRTA","USUBJID","SOC","PT","VERBATIM","START_DATE","END_DATE",
                 "CTCAE_GRADE","severity_d","serious_d","rel_d","action_d","outcome_d")
ae_display <- ae_report %>% select(all_of(report_cols))

# Write CSV of table body (as per spec: each output dataset as CSV)
write.csv(ae_display, file.path(out, "l_ae.csv"), row.names = FALSE, na = "")

# Also write full ae_report for traceability
write.csv(ae_report, file.path(out, "l_ae_full.csv"), row.names = FALSE, na = "")

# Text rendering
# Titles and footnotes as per u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
# sysin and run date
sysin <- "P076_l_ae.sas"
run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")

# Build text file
txt_path <- file.path(out, "l_ae.txt")
# Also need to produce .rtf text rendering? Spec says l_ae.rtf -> write text rendering and CSV
# We will write both .txt and .rtf (rtf as plain text with same content)
rtf_path <- file.path(out, "l_ae.rtf")

# Helper to format header
header_lines <- c(
  paste0(sponsor, strrep(" ", 80), "Page 1 of X"),
  paste0("Protocol: ", protocol),
  "",
  "Listing 16.2.7.1",
  "Listing of Adverse Events",
  "Safety Analysis Set",
  ""
)

footer_lines <- c(
  "",
  paste0("Data cutoff: ", cutdate),
  "Source: ADaM datasets",
  paste0("Program: ", sysin, " | Run: ", run_date, " ", substr(run_time,1,5))
)

# Column headers with split='~' meaning line break
# Define widths approx as in SAS: TRTA 0.7in, USUBJID 0.8in, SOC 1.2in, PT 1.0in, VERBATIM 1.0in,
# START_DATE 0.7in, END_DATE 0.7in, CTCAE_GRADE 0.4in, severity_d 0.6in, serious_d 0.3in, rel_d 0.7in, action_d 0.8in, outcome_d 0.8in
# Total 10in landscape
# For text, we will create fixed-width table

# Create column labels
col_labels <- c(
  "Treatment",
  "Subject ID",
  "System Organ Class",
  "Preferred Term",
  "Verbatim Term",
  "Start Date",
  "End Date",
  "CTCAE Grade",
  "Severity",
  "SAE",
  "Relationship",
  "Action Taken",
  "Outcome"
)

# Function to truncate/pad for text
pad_str <- function(x, width, align = "left") {
  if (is.na(x)) x <- ""
  x <- as.character(x)
  # truncate if longer than width
  if (nchar(x) > width) {
    x <- substr(x, 1, width)
  }
  if (align == "left") {
    sprintf(paste0("%-", width, "s"), x)
  } else if (align == "center") {
    pad <- width - nchar(x)
    left <- floor(pad/2)
    right <- pad - left
    paste0(strrep(" ", left), x, strrep(" ", right))
  } else {
    sprintf(paste0("%", width, "s"), x)
  }
}

# Define text widths (characters) proportional to inches
# 10in ~ 130 chars at 7pt landscape; allocate
widths <- c(12, 12, 18, 16, 16, 11, 11, 6, 10, 4, 12, 13, 13)
aligns <- c("left","left","left","left","left","left","left","center","left","center","left","left","left")

# Build table header lines
sep_line <- paste0(paste(sapply(widths, function(w) strrep("-", w)), collapse = " "), collapse="")
header_row1 <- paste(mapply(pad_str, col_labels, widths, aligns), collapse = " ")
# Underline
header_row2 <- sep_line

# Build data rows
# For break after TRTA / page, we will insert page break markers and repeat header
# Count numbers: spec says 2771 numbers - we just ensure all rows printed

# Prepare rows
rows_text <- c()
current_trta <- ""
page_num <- 1

# For text file, we will paginate by TRTA
# Get unique TRTA in order
trta_levels <- unique(ae_display$TRTA)

# Function to write block for one TRTA
all_lines <- c(header_lines, header_row1, header_row2)

for (trt in trta_levels) {
  sub <- ae_display %>% filter(TRTA == trt)
  # Add TRTA group header if needed
  # In PROC REPORT with ORDER, TRTA only prints on first row of group, but we will print each row with TRTA
  # To mimic SAS, we will print TRTA only on first occurrence per group, but for text we print all
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    vals <- c(
      r$TRTA, r$USUBJID, r$SOC, r$PT, r$VERBATIM, r$START_DATE, r$END_DATE,
      r$CTCAE_GRADE, r$severity_d, r$serious_d, r$rel_d, r$action_d, r$outcome_d
    )
    # Replace NA with ""
    vals[is.na(vals)] <- ""
    row_str <- paste(mapply(pad_str, vals, widths, aligns), collapse = " ")
    all_lines <- c(all_lines, row_str)
  }
  # Break after TRTA / page - add page break marker except after last
  if (trt != tail(trta_levels, 1)) {
    all_lines <- c(all_lines, "", paste0("--- Page break after Treatment: ", trt, " ---"), "")
    # Repeat header for next page
    all_lines <- c(all_lines, header_row1, header_row2)
    page_num <- page_num + 1
  }
}

# Add footer
all_lines <- c(all_lines, footer_lines)

# Add summary counts
all_lines <- c(all_lines, "",
               paste0("Total number of adverse event records: ", nrow(ae_display)),
               paste0("Number of subjects with adverse events: ", length(unique(ae_display$USUBJID))),
               paste0("Number of treatment groups: ", length(trta_levels))
)

# Write txt
writeLines(all_lines, txt_path)

# Write rtf as same content but with RTF header (simple)
# Create minimal RTF
rtf_header <- "{\\rtf1\\ansi\\deff0\\viewkind4\\uc1\\pard\\landscape\\f0\\fs16\n"
rtf_footer <- "}"
# Escape RTF special chars
escape_rtf <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\\{", "\\\\{", s)
  s <- gsub("\\}", "\\\\}", s)
  s
}
rtf_body <- paste(sapply(all_lines, function(l) paste0(escape_rtf(l), "\\par\n")), collapse = "")
writeLines(paste0(rtf_header, rtf_body, rtf_footer), rtf_path)

# Also write plotted data CSV already done; no figure for this listing

# Verify outputs
cat("P076 l_ae completed. Rows:", nrow(ae_display), "Subjects:", length(unique(ae_display$USUBJID)), "\n")
cat("Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
