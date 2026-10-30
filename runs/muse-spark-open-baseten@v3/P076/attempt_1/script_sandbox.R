# P076_l_ae.sas -> R translation
# Listing 16.2.7.1 - Listing of Adverse Events
# Population: Safety Analysis Set (SAFFL='Y'), TRTEMFL='Y'

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
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adampath <- "/bench/data/raw/own-synthetic/adam"
sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"

# helper: SAS propcase (proper case) - first letter upper, rest lower per word
prop_case <- function(x) {
  ifelse(is.na(x) | x == "", x,
         stringr::str_to_title(tolower(as.character(x)), locale = "en"))
}

# helper: read adam dataset (sas7bdat or xpt, case-insensitive)
read_adam <- function(name) {
  # try exact lower
  candidates <- c(
    file.path(adampath, paste0(name, ".sas7bdat")),
    file.path(adampath, paste0(tolower(name), ".sas7bdat")),
    file.path(adampath, paste0(toupper(name), ".sas7bdat")),
    file.path(adampath, paste0(name, ".xpt")),
    file.path(adampath, paste0(tolower(name), ".xpt")),
    file.path(adampath, paste0(toupper(name), ".xpt"))
  )
  # also list files in directory for case-insensitive match
  if (dir.exists(adampath)) {
    fl <- list.files(adampath, full.names = TRUE)
    # find by pattern
    pat <- paste0("^", name, "\\.(sas7bdat|xpt)$")
    m <- fl[grepl(pat, basename(fl), ignore.case = TRUE)]
    if (length(m) > 0) candidates <- c(m, candidates)
  }
  f <- candidates[file.exists(candidates)][1]
  if (is.na(f) || !file.exists(f)) {
    stop(paste0("Dataset not found: ", name, " in ", adampath))
  }
  cat("Reading:", f, "\n")
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # convert haven_labelled to character/factor labels
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      # as_factor gives labels where available
      col2 <- haven::as_factor(col, levels = "labels")
      # haven::as_factor returns factor; convert to character
      df[[nm]] <- as.character(col2)
    } else if (is.factor(col)) {
      df[[nm]] <- as.character(col)
    }
  }
  # make names uppercase to mimic SAS (case-insensitive matching)
  # keep original case but also ensure we can find columns case-insensitively
  df
}

# helper to find column name case-insensitively
find_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}

# Read inputs
adsl_raw <- read_adam("adsl")
adae_raw <- read_adam("adae")

cat("ADSL rows:", nrow(adsl_raw), "cols:", ncol(adsl_raw), "\n")
cat("ADAE rows:", nrow(adae_raw), "cols:", ncol(adae_raw), "\n")

# Ensure required columns exist (case-insensitive)
# ADSL: USUBJID, TRTA, AGE, SEX, RACE, SAFFL
# ADAE: USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC, AESEV, AEREL, AESER, AEACN, AEOUT, AETOXGR, TRTEMFL
stopifnot(!is.na(find_col(adsl_raw, "USUBJID")))
stopifnot(!is.na(find_col(adae_raw, "USUBJID")))

# Standardize column names to upper for processing, then restore
names(adsl_raw) <- toupper(names(adsl_raw))
names(adae_raw) <- toupper(names(adae_raw))

# Keep only needed columns to avoid .x/.y suffixes (SAS SELECT)
# ADSL needed: USUBJID, TRTA, AGE, SEX, RACE, SAFFL
adsl_keep <- intersect(c("USUBJID","TRTA","AGE","SEX","RACE","SAFFL"), names(adsl_raw))
adsl <- adsl_raw[, adsl_keep, drop = FALSE]

# ADAE needed: USUBJID, AEBODSYS, AEDECOD, AETERM, AESTDTC, AEENDTC, AESEV, AEREL, AESER, AEACN, AEOUT, AETOXGR, TRTEMFL
adae_keep <- intersect(c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC","AESEV","AEREL","AESER","AEACN","AEOUT","AETOXGR","TRTEMFL"), names(adae_raw))
adae <- adae_raw[, adae_keep, drop = FALSE]

# Step 1: Prepare listing data - inner join and filter
# SAS: inner join on USUBJID where b.SAFFL='Y' and a.TRTEMFL='Y' order by b.TRTA, a.USUBJID, a.AESTDTC, a.AEBODSYS, a.AEDECOD
# Do filtering before join to mimic SAS
adsl_f <- adsl %>% filter(SAFFL == "Y")
adae_f <- adae %>% filter(TRTEMFL == "Y")

stopifnot("SAFFL" %in% names(adsl_f))
stopifnot("TRTEMFL" %in% names(adae_f))

ae_list <- inner_join(adae_f, adsl_f, by = "USUBJID")

# Create SAS SELECT aliases
ae_list <- ae_list %>%
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

# Order by TRTA, USUBJID, AESTDTC, AEBODSYS, AEDECOD
# SAS sorts character with uppercase before lowercase, missing first
# Use order with na.last=FALSE for missing first
# Ensure character columns are character
ae_list <- ae_list %>%
  mutate(across(c(TRTA, USUBJID, START_DATE, SOC, PT), ~ as.character(.x)))

# SAS missing sorts first: for character, "" or NA should be first
# Replace NA with "" for ordering to mimic SAS missing first (NA considered missing)
ord_df <- ae_list %>%
  mutate(
    .trta = ifelse(is.na(TRTA), "", TRTA),
    .usubjid = ifelse(is.na(USUBJID), "", USUBJID),
    .start = ifelse(is.na(START_DATE), "", START_DATE),
    .soc = ifelse(is.na(SOC), "", SOC),
    .pt = ifelse(is.na(PT), "", PT)
  )
o <- order(ord_df$.trta, ord_df$.usubjid, ord_df$.start, ord_df$.soc, ord_df$.pt, na.last = FALSE)
ae_list <- ae_list[o, , drop = FALSE]

cat("ae_list rows after join/filter/sort:", nrow(ae_list), "\n")

# Step 2: Format display values
ae_report <- ae_list %>%
  mutate(
    severity_d = prop_case(SEVERITY),
    rel_d = prop_case(RELATIONSHIP),
    serious_d = ifelse(!is.na(SERIOUS) & SERIOUS == "Y", "Yes", "No"),
    # ACTION_TAKEN mapping
    action_d = case_when(
      toupper(ACTION_TAKEN) == "DRUG WITHDRAWN" ~ "Drug Withdrawn",
      toupper(ACTION_TAKEN) == "DOSE REDUCED" ~ "Dose Reduced",
      toupper(ACTION_TAKEN) == "DRUG INTERRUPTED" ~ "Drug Interrupted",
      toupper(ACTION_TAKEN) == "DOSE NOT CHANGED" ~ "Dose Not Changed",
      toupper(ACTION_TAKEN) == "NOT APPLICABLE" ~ "Not Applicable",
      is.na(ACTION_TAKEN) ~ NA_character_,
      TRUE ~ prop_case(ACTION_TAKEN)
    ),
    outcome_d = case_when(
      toupper(OUTCOME) == "RECOVERED/RESOLVED" ~ "Resolved",
      toupper(OUTCOME) == "RECOVERING/RESOLVING" ~ "Resolving",
      toupper(OUTCOME) == "NOT RECOVERED/NOT RESOLVED" ~ "Not Resolved",
      toupper(OUTCOME) == "RECOVERED/RESOLVED WITH SEQUELAE" ~ "Resolved w/ Sequelae",
      toupper(OUTCOME) == "FATAL" ~ "Fatal",
      is.na(OUTCOME) ~ NA_character_,
      TRUE ~ prop_case(OUTCOME)
    )
  )

# Ensure lengths (SAS LENGTH) - truncate if needed (not strictly required but mimic)
ae_report$severity_d <- substr(ae_report$severity_d, 1, 10)
ae_report$rel_d <- substr(ae_report$rel_d, 1, 15)
ae_report$serious_d <- substr(ae_report$serious_d, 1, 3)
ae_report$action_d <- substr(ae_report$action_d, 1, 30)
ae_report$outcome_d <- substr(ae_report$outcome_d, 1, 30)

# Final listing columns in SAS PROC REPORT order
listing_cols <- c("TRTA","USUBJID","SOC","PT","VERBATIM","START_DATE","END_DATE","CTCAE_GRADE","severity_d","serious_d","rel_d","action_d","outcome_d")
# Ensure all exist
missing_cols <- setdiff(listing_cols, names(ae_report))
if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse=",")))

ae_final <- ae_report[, listing_cols, drop = FALSE]

# Write CSV of listing (table body)
csv_path <- file.path(out, "l_ae.csv")
write.csv(ae_final, csv_path, row.names = FALSE, na = "")
cat("Wrote CSV:", csv_path, "rows:", nrow(ae_final), "\n")

# Also write full ae_report dataset as CSV for traceability (optional)
# write.csv(ae_report, file.path(out, "ae_report.csv"), row.names=FALSE, na="")

# Step 3: Generate text rendering (RTF-like plain text)
# Titles and footnotes from u_titles.sas for L_AE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

# Titles
title1 <- paste0(sponsor, "  Page ^{pageof}")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Listing 16.2.7.1"
title5 <- "Listing of Adverse Events"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
# SAS footnote3: Program: &sysin. | Run: date9. time5.
run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")
footnote3 <- paste0("Program: l_ae.sas | Run: ", run_date, " ", run_time)

# Column headers with SAS split='~' handling
# Define headers: TRTA -> Treatment, USUBJID -> Subject ID, SOC -> System Organ Class, etc.
headers <- c(
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

# Build text file
txt_path <- file.path(out, "l_ae.txt")
rtf_path <- file.path(out, "l_ae.rtf")  # also write .rtf as plain text for compatibility

# Helper to format row for text
format_cell <- function(x, width = 20) {
  if (is.na(x) || is.null(x)) return("")
  as.character(x)
}

# Determine unique TRTA groups for page breaks (break after TRTA / page)
trta_groups <- unique(ae_final$TRTA)
# If TRTA is NA, keep as group
if (length(trta_groups) == 0) trta_groups <- "ALL"

# Count pages = number of TRTA groups (or 1 if no data)
n_pages <- max(1, length(trta_groups))

con <- file(txt_path, open = "wt")
# Also open rtf_path as text (same content)
con_rtf <- file(rtf_path, open = "wt")

write_block <- function(con, page_num, trta_val, data_sub) {
  # Titles with page numbers
  cat(paste0(sponsor, strrep(" ", 60), "Page ", page_num, " of ", n_pages, "\n"), file = con)
  cat(paste0("Protocol: ", protocol, "\n"), file = con)
  cat("\n", file = con)
  cat(paste0(title4, "\n"), file = con)
  cat(paste0(title5, "\n"), file = con)
  cat(paste0(title6, "\n"), file = con)
  cat("\n", file = con)
  if (!is.na(trta_val) && trta_val != "") {
    cat(paste0("Treatment: ", trta_val, "\n"), file = con)
    cat("\n", file = con)
  }
  # Header row
  hdr_line <- paste(headers, collapse = " | ")
  cat(hdr_line, "\n", file = con, sep = "")
  cat(paste(rep("-", nchar(hdr_line)), collapse = ""), "\n", file = con, sep = "")
  # Data rows
  if (nrow(data_sub) > 0) {
    for (i in seq_len(nrow(data_sub))) {
      row_vals <- sapply(data_sub[i, , drop = FALSE], function(v) ifelse(is.na(v), "", as.character(v)))
      # Replace any pipe in data to avoid breaking delimiter
      row_vals <- gsub("\\|", "/", row_vals)
      line <- paste(row_vals, collapse = " | ")
      cat(line, "\n", file = con, sep = "")
    }
  } else {
    cat("No data to report.\n", file = con)
  }
  cat("\n", file = con)
  # Footnotes at bottom of each page (SAS prints footnotes each page)
  cat(paste0(footnote1, "\n"), file = con)
  cat(paste0(footnote2, "\n"), file = con)
  cat(paste0(footnote3, "\n"), file = con)
  cat("\n", file = con)
  cat(paste(rep("=", 80), collapse = ""), "\n", file = con)
  cat("\n", file = con)
}

# If no data, write one page
if (nrow(ae_final) == 0) {
  write_block(con, 1, "", ae_final)
  write_block(con_rtf, 1, "", ae_final)
} else {
  # Split by TRTA
  # Ensure TRTA order is same as sorted order
  # Use split preserving order
  ae_final$TRTA <- as.character(ae_final$TRTA)
  # Get ordered unique TRTA as they appear
  ordered_trta <- unique(ae_final$TRTA)
  page_num <- 1
  for (trt in ordered_trta) {
    sub <- ae_final[ae_final$TRTA == trt | (is.na(ae_final$TRTA) & is.na(trt)), , drop = FALSE]
    write_block(con, page_num, trt, sub)
    write_block(con_rtf, page_num, trt, sub)
    page_num <- page_num + 1
  }
}

close(con)
close(con_rtf)
cat("Wrote TXT:", txt_path, "\n")
cat("Wrote RTF (text):", rtf_path, "\n")

# Also write a summary log
cat("P076 l_ae completed. Rows:", nrow(ae_final), "Pages:", n_pages, "\n")
cat("Outputs in:", out, "\n")
print(head(ae_final, 3))

# Clean up work datasets (SAS proc datasets delete)
rm(ae_list, ae_report, ae_final, adsl, adae, adsl_raw, adae_raw, adsl_f, adae_f)
gc()

cat("NOTE: l_ae.sas completed successfully (R translation).\n")
