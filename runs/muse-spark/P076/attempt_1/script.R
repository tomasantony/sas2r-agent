# P076_l_ae.sas -> R translation
# Listing 16.2.7.1 - Listing of Adverse Events (Safety, TRTEMFL='Y')

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# SAS round half away from zero (helper, not strictly needed here)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# SAS PROPCASE: first letter of each word uppercase, rest lowercase;
# word boundary = start or previous char is not alphanumeric
sas_propcase <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- as.character(s)
    if (is.na(s) || nchar(s) == 0) return(s)
    low <- tolower(s)
    chars <- strsplit(low, "", fixed = TRUE)[[1]]
    prev_alnum <- FALSE
    for (i in seq_along(chars)) {
      ch <- chars[i]
      is_alnum <- grepl("[A-Za-z0-9]", ch)
      if (i == 1 || !prev_alnum) {
        if (grepl("[a-z]", ch)) chars[i] <- toupper(ch)
      }
      prev_alnum <- is_alnum
    }
    paste0(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# SAS IFC for serious
sas_serious <- function(x) {
  ifelse(!is.na(x) & x == "Y", "Yes", "No")
}

find_file <- function(dir, base) {
  pat <- paste0("(?i)^", base, "\\.")
  f <- list.files(dir, pattern = pat, full.names = TRUE)
  # prefer sas7bdat, then xpt, then others
  if (length(f) == 0) return(NULL)
  # order preference
  pref <- function(p) {
    if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) return(1)
    if (grepl("\\.xpt$", p, ignore.case = TRUE)) return(2)
    if (grepl("\\.sas$", p, ignore.case = TRUE)) return(3)
    return(4)
  }
  o <- order(vapply(f, pref, numeric(1)))
  f[o[1]]
}

read_sas_any <- function(path) {
  if (is.null(path) || !file.exists(path)) stop(paste0("File not found: ", path))
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    df <- haven::read_xpt(path)
  } else {
    df <- haven::read_sas(path)
  }
  # normalize names to uppercase for SAS semantics
  names(df) <- toupper(names(df))
  # zap labels/formats but keep values; convert labelled to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      # character labelled -> character, numeric labelled -> numeric
      if (is.character(col) || is.character(haven::as_factor(col, levels = "labels"))) {
        # use underlying character values, not labels
        col <- haven::zap_labels(col)
        col <- as.character(col)
      } else {
        col <- haven::zap_labels(col)
        # keep numeric
        if (is.factor(col)) col <- as.character(col)
      }
      df[[nm]] <- col
    }
  }
  tibble::as_tibble(df)
}

adae_file <- find_file(adam_path, "adae")
adsl_file <- find_file(adam_path, "adsl")
cat("NOTE: adae file:", adae_file, "\n")
cat("NOTE: adsl file:", adsl_file, "\n")

adae <- read_sas_any(adae_file)
adsl <- read_sas_any(adsl_file)

# Ensure needed columns exist (uppercase)
# Step 1: SQL join + filter + order
# select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, SOC, PT, VERBATIM, START/END, etc.
# where b.SAFFL='Y' and a.TRTEMFL='Y'  (case-sensitive, trailing blanks ignored)

trim_chr <- function(x) {
  if (is.character(x)) trimws(x) else x
}

# Prepare join keys as trimmed character
adae$USUBJID <- trim_chr(as.character(adae$USUBJID))
adsl$USUBJID <- trim_chr(as.character(adsl$USUBJID))

# Keep only needed cols; guard if missing
get_col <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))
}

# Build adsl subset
adsl_sub <- adsl %>%
  mutate(
    SAFFL_c = trim_chr(as.character(SAFFL)),
    TRTA_c = trim_chr(as.character(TRTA))
  ) %>%
  filter(!is.na(SAFFL_c) & SAFFL_c == "Y") %>%
  select(USUBJID, TRTA = TRTA_c, AGE, SEX, RACE)

adae_sub <- adae %>%
  mutate(
    TRTEMFL_c = trim_chr(as.character(TRTEMFLFL <- TRTEMFL)),
    USUBJID_c = USUBJID
  )

# TRTEMFL filter (case-sensitive 'Y')
adae_sub <- adae_sub %>%
  filter(!is.na(TRTEMFL_c) & TRTEMFL_c == "Y")

# Inner join
ae_list <- inner_join(adae_sub, adsl_sub, by = "USUBJID", suffix = c("", ".sl"))

# Select/rename per SQL
# a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE,
# SOC=AEBODSYS, PT=AEDECOD, VERBATIM=AETERM, START_DATE=AESTDTC, END_DATE=AEENDTC,
# SEVERITY=AESEV, RELATIONSHIP=AEREL, SERIOUS=AESER, ACTION_TAKEN=AEACN, OUTCOME=AEOUT,
# CTCAE_GRADE=AETOXGR, TRTEMFL
ae_list <- tibble(
  USUBJID = as.character(get_col(ae_list, "USUBJID")),
  TRTA = trim_chr(as.character(get_col(ae_list, "TRTA"))),
  AGE = get_col(ae_list, "AGE"),
  SEX = as.character(get_col(ae_list, "SEX")),
  RACE = as.character(get_col(ae_list, "RACE")),
  SOC = as.character(get_col(ae_list, "AEBODSYS")),
  PT = as.character(get_col(ae_list, "AEDECOD")),
  VERBATIM = as.character(get_col(ae_list, "AETERM")),
  START_DATE = as.character(get_col(ae_list, "AESTDTC")),
  END_DATE = as.character(get_col(ae_list, "AEENDTC")),
  SEVERITY = as.character(get_col(ae_list, "AESEV")),
  RELATIONSHIP = as.character(get_col(ae_list, "AEREL")),
  SERIOUS = as.character(get_col(ae_list, "AESER")),
  ACTION_TAKEN = as.character(get_col(ae_list, "AEACN")),
  OUTCOME = as.character(get_col(ae_list, "AEOUT")),
  CTCAE_GRADE = as.character(get_col(ae_list, "AETOXGR")),
  TRTEMFL = as.character(get_col(ae_list, "TRTEMFL"))
)

# Trim character display fields (SAS would keep trailing blanks; trim for R)
for (cc in c("USUBJID","TRTA","SOC","PT","VERBATIM","START_DATE","END_DATE",
             "SEVERITY","RELATIONSHIP","SERIOUS","ACTION_TAKEN","OUTCOME","CTCAE_GRADE","TRTEMFL")) {
  ae_list[[cc]] <- trim_chr(ae_list[[cc]])
  # SAS missing = "" ; convert "" to NA? Keep "" as NA for CSV na="" handling.
  # haven may give "" for missing; normalize: empty string -> NA
  ae_list[[cc]][!is.na(ae_list[[cc]]) & ae_list[[cc]] == ""] <- NA_character_
}

# SAS sort: order by TRTA, USUBJID, AESTDTC(START_DATE), AEBODSYS(SOC), AEDECOD(PT)
# Missing sorts first; character binary collation (uppercase before lowercase).
# Implement binary collation by ordering on raw bytes via xtfrm on UTF-8 bytes.
sas_key <- function(x) {
  # NA first, then binary order
  x2 <- ifelse(is.na(x), "\001", x)
  # Use order with C locale emulation: rank by raw bytes
  # Create sortable key by converting each string to hex-joined? Use base order on
  # transformed values with sort method "radix" which is byte-based? radix is fast and
  # uses C collation-like ordering. We'll just return x2 for order().
  x2
}
# Use order() with na.last=FALSE; to force binary (uppercase<lowercase), temporarily set LC_COLLATE=C
old_coll <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord <- order(sas_key(ae_list$TRTA), sas_key(ae_list$USUBJID),
             sas_key(ae_list$START_DATE), sas_key(ae_list$SOC), sas_key(ae_list$PT),
             na.last = FALSE, method = "radix")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_coll), silent = TRUE))
ae_list <- ae_list[ord, , drop = FALSE]

cat("NOTE: ae_list rows:", nrow(ae_list), "\n")

# Step 2: Format display values
ae_report <- ae_list
ae_report$severity_d <- sas_propcase(ae_report$SEVERITY)
ae_report$rel_d <- sas_propcase(ae_report$RELATIONSHIP)
ae_report$serious_d <- sas_serious(trim_chr(ae_report$SERIOUS))

# ACTION_TAKEN mapping on UPCASE
up_act <- toupper(trim_chr(ae_report$ACTION_TAKEN))
action_d <- character(nrow(ae_report))
for (i in seq_len(nrow(ae_report))) {
  u <- up_act[i]
  if (is.na(u)) {
    action_d[i] <- NA_character_
  } else if (u == "DRUG WITHDRAWN") {
    action_d[i] <- "Drug Withdrawn"
  } else if (u == "DOSE REDUCED") {
    action_d[i] <- "Dose Reduced"
  } else if (u == "DRUG INTERRUPTED") {
    action_d[i] <- "Drug Interrupted"
  } else if (u == "DOSE NOT CHANGED") {
    action_d[i] <- "Dose Not Changed"
  } else if (u == "NOT APPLICABLE") {
    action_d[i] <- "Not Applicable"
  } else {
    action_d[i] <- sas_propcase(ae_report$ACTION_TAKEN[i])
  }
}
ae_report$action_d <- action_d

up_out <- toupper(trim_chr(ae_report$OUTCOME))
outcome_d <- character(nrow(ae_report))
for (i in seq_len(nrow(ae_report))) {
  u <- up_out[i]
  if (is.na(u)) {
    outcome_d[i] <- NA_character_
  } else if (u == "RECOVERED/RESOLVED") {
    outcome_d[i] <- "Resolved"
  } else if (u == "RECOVERING/RESOLVING") {
    outcome_d[i] <- "Resolving"
  } else if (u == "NOT RECOVERED/NOT RESOLVED") {
    outcome_d[i] <- "Not Resolved"
  } else if (u == "RECOVERED/RESOLVED WITH SEQUELAE") {
    outcome_d[i] <- "Resolved w/ Sequelae"
  } else if (u == "FATAL") {
    outcome_d[i] <- "Fatal"
  } else {
    outcome_d[i] <- sas_propcase(ae_report$OUTCOME[i])
  }
}
ae_report$outcome_d <- outcome_d

# Final listing columns in PROC REPORT order
listing_df <- ae_report %>%
  select(TRTA, USUBJID, SOC, PT, VERBATIM, START_DATE, END_DATE,
         CTCAE_GRADE, severity_d, serious_d, rel_d, action_d, outcome_d)

# For CSV: keep SAS variable names; dates as YYYY-MM-DD (already ISO substrings)
# Ensure CTCAE_GRADE etc as character; write with na=""
# Truncate dates to date part if datetime present (keep YYYY-MM-DD if longer)
fmt_date <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- as.character(s)
    # AESTDTC may be YYYY-MM-DD or YYYY-MM-DDTHH:MM; keep as-is but normalize T to date?
    # SAS displays raw AESTDTC string; keep full string. For CSV dates spec: dates YYYY-MM-DD.
    # If string starts with YYYY-MM-DD, keep full original (contains numbers anyway).
    s
  }, character(1), USE.NAMES = FALSE)
}
listing_df$START_DATE <- fmt_date(listing_df$START_DATE)
listing_df$END_DATE <- fmt_date(listing_df$END_DATE)

# Write CSV of table body
write.csv(listing_df, file.path(out, "l_ae.csv"), row.names = FALSE, na = "")

cat("NOTE: listing rows written:", nrow(listing_df), "\n")

# --- Text rendering (l_ae.txt) mimicking PROC REPORT RTF ---
# Titles from u_titles.sas for L_AE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
t4 <- "Listing 16.2.7.1"
t5 <- "Listing of Adverse Events"
t6 <- "Safety Analysis Set"

# Column headers: replace ~ with space
headers <- c("Treatment", "Subject ID", "System Organ Class", "Preferred Term",
             "Verbatim Term", "Start Date", "End Date", "CTCAE Grade",
             "Severity", "SAE", "Relationship", "Action Taken", "Outcome")
colnames_txt <- c("TRTA","USUBJID","SOC","PT","VERBATIM","START_DATE","END_DATE",
                  "CTCAE_GRADE","severity_d","serious_d","rel_d","action_d","outcome_d")

# Build text file with page break after each TRTA (like BREAK AFTER TRTA / PAGE)
# PROC REPORT ORDER blanks repeated TRTA/USUBJID; we emulate blanking for display
# but keep full values in CSV. For TXT, blank repeats to match SAS appearance
# while preserving all numbers.

txt_path <- file.path(out, "l_ae.txt")
con <- file(txt_path, open = "wt")

trt_levels <- unique(listing_df$TRTA)
# Handle NA TRTA group
if (length(trt_levels) == 0) trt_levels <- NA_character_
page_no <- 0
total_pages <- length(trt_levels)

fmt_cell <- function(x) {
  if (is.na(x)) return("")
  as.character(x)
}

for (ti in seq_along(trt_levels)) {
  trt <- trt_levels[ti]
  page_no <- page_no + 1
  sub <- listing_df[(!is.na(listing_df$TRTA) & !is.na(trt) & listing_df$TRTA == trt) |
                    (is.na(listing_df$TRTA) & is.na(trt)), , drop = FALSE]
  # Titles
  cat(paste0(sponsor, strrep(" ", 4), "Page ", page_no, " of ", total_pages, "\n"), file = con)
  cat(paste0("Protocol: ", protocol, "\n"), file = con)
  cat("\n", file = con)
  cat(paste0(t4, "\n"), file = con)
  cat(paste0(t5, "\n"), file = con)
  cat(paste0(t6, "\n"), file = con)
  cat("\n", file = con)
  # Header row (tab-separated for readability; numbers preserved)
  cat(paste(headers, collapse = " | "), "\n", file = con, sep = "")
  cat(paste(rep("---", length(headers)), collapse = " | "), "\n", file = con, sep = "")
  # ORDER display: blank repeated TRTA and USUBJID
  prev_trt <- NA_character_
  prev_sub <- NA_character_
  for (r in seq_len(nrow(sub))) {
    row <- sub[r, , drop = FALSE]
    disp_trt <- fmt_cell(row$TRTA)
    disp_sub <- fmt_cell(row$USUBJID)
    # ORDER logic: within whole listing, TRTA repeats blanked; USUBJID repeats blanked within TRTA
    cur_trt_raw <- fmt_cell(row$TRTA)
    cur_sub_raw <- fmt_cell(row$USUBJID)
    if (!is.na(prev_trt) && cur_trt_raw == prev_trt) disp_trt <- "" else prev_trt <- cur_trt_raw
    # reset sub when trt changes? prev_sub tracks within trt; if trt displayed (new group) reset
    if (disp_trt != "") {
      # new treatment group: show subject
      prev_sub <- cur_sub_raw
    } else {
      if (!is.na(prev_sub) && cur_sub_raw == prev_sub) disp_sub <- "" else prev_sub <- cur_sub_raw
    }
    vals <- c(disp_trt, disp_sub,
              fmt_cell(row$SOC), fmt_cell(row$PT), fmt_cell(row$VERBATIM),
              fmt_cell(row$START_DATE), fmt_cell(row$END_DATE),
              fmt_cell(row$CTCAE_GRADE), fmt_cell(row$severity_d),
              fmt_cell(row$serious_d), fmt_cell(row$rel_d),
              fmt_cell(row$action_d), fmt_cell(row$outcome_d))
    # Replace any pipe/newline in values to keep structure
    vals <- gsub("[\r\n]+", " ", vals)
    vals <- gsub("\\|", "/", vals)
    cat(paste(vals, collapse = " | "), "\n", file = con, sep = "")
  }
  cat("\n", file = con)
  cat(paste0("Data cutoff: ", cutdate, ".", "\n"), file = con)
  cat("Source: ADaM datasets\n", file = con)
  runstamp <- format(Sys.Date(), "%d%b%Y")
  cat(paste0("Program: l_ae.sas | Run: ", runstamp, "\n"), file = con)
  if (ti < length(trt_levels)) cat("\n--- Page Break ---\n\n", file = con)
}

close(con)
cat("NOTE: l_ae.txt written\n")
cat("NOTE: l_ae.sas completed successfully.\n")
