```r
# P076 l_ae.sas -> R translation
# Listing 16.2.7.1 - Listing of Adverse Events (Safety, TRTEMFL=Y)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_dataset <- function(dir, base) {
  if (!dir.exists(dir)) stop(paste0("Directory not found: ", dir))
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match basename without extension, case-insensitive
  bn <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(bn == tolower(base))
  if (length(idx) == 0) {
    # try recursive
    files2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
    bn2 <- tolower(tools::file_path_sans_ext(basename(files2)))
    idx2 <- which(bn2 == tolower(base))
    if (length(idx2) == 0) stop(paste0("Dataset not found: ", base, " in ", dir))
    return(files2[idx2[1]])
  }
  files[idx[1]]
}

read_sas_dataset <- function(path) {
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    df <- haven::read_xpt(path)
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap labels/formats but keep underlying values
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  # convert any remaining haven_labelled columns
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # keep numeric if numeric, else character
      if (is.numeric(col)) {
        df[[nm]] <- as.numeric(col)
      } else {
        df[[nm]] <- as.character(col)
      }
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  # normalize names to uppercase for matching (SAS case-insensitive)
  names(df) <- toupper(names(df))
  df
}

adae_path <- find_dataset(adam_dir, "adae")
adsl_path <- find_dataset(adam_dir, "adsl")

adae <- read_sas_dataset(adae_path)
adsl <- read_sas_dataset(adsl_path)

# Ensure required columns exist (uppercase)
need_adae <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC",
               "AESEV","AEREL","AESER","AEACN","AEOUT","AETOXGR","TRTEMFL")
need_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","SAFFL")
stopifnot(all(need_adae %in% names(adae)))
stopifnot(all(need_adsl %in% names(adsl)))

# Keep only needed columns + key to avoid .x/.y suffixes (SAS PROC SQL semantics)
# Keys
key <- "USUBJID"
adae_keep <- adae[, need_adae, drop = FALSE]
adsl_keep <- adsl[, need_adsl, drop = FALSE]

# Character comparison is case-sensitive; missing fails equality
# Inner join on USUBJID
ae_list <- merge(adsl_keep, adae_keep, by = key, all = FALSE)
# After merge, stopifnot filter columns exist
stopifnot("SAFFL" %in% names(ae_list))
stopifnot("TRTEMFL" %in% names(ae_list))

# Where SAFFL='Y' and TRTEMFL='Y'
ae_list <- ae_list[!is.na(ae_list$SAFFL) & ae_list$SAFFL == "Y" &
                   !is.na(ae_list$TRTEMFL) & ae_list$TRTEMFL == "Y", , drop = FALSE]

# Rename to listing names (SAS aliases)
# a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE,
# SOC=AEBODSYS, PT=AEDECOD, VERBATIM=AETERM, START_DATE=AESTDTC, END_DATE=AEENDTC,
# SEVERITY=AESEV, RELATIONSHIP=AEREL, SERIOUS=AESER, ACTION_TAKEN=AEACN, OUTCOME=AEOUT,
# CTCAE_GRADE=AETOXGR, TRTEMFL
ae_list$SOC <- ae_list$AEBODSYS
ae_list$PT <- ae_list$AEDECOD
ae_list$VERBATIM <- ae_list$AETERM
# START_DATE / END_DATE: keep display as character ISO; handle Date/POSIX if needed
fmt_iso_date <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%S"))
  if (is.numeric(x)) return(as.character(x))
  return(as.character(x))
}
ae_list$START_DATE <- fmt_iso_date(ae_list$AESTDTC)
ae_list$END_DATE <- fmt_iso_date(ae_list$AEENDTC)
ae_list$SEVERITY <- as.character(ae_list$AESEV)
ae_list$RELATIONSHIP <- as.character(ae_list$AEREL)
ae_list$SERIOUS <- as.character(ae_list$AESER)
ae_list$ACTION_TAKEN <- as.character(ae_list$AEACN)
ae_list$OUTCOME <- as.character(ae_list$AEOUT)
ae_list$CTCAE_GRADE <- as.character(ae_list$AETOXGR)
ae_list$TRTA <- as.character(ae_list$TRTA)
ae_list$USUBJID <- as.character(ae_list$USUBJID)

# Order by TRTA, USUBJID, AESTDTC(START_DATE), AEBODSYS(SOC), AEDECOD(PT)
# SAS: missing first, uppercase before lowercase (ASCII byte order)
# Use order with na.last=FALSE and radix (bytewise)
ord <- order(ae_list$TRTA, ae_list$USUBJID, ae_list$START_DATE,
             ae_list$SOC, ae_list$PT,
             na.last = FALSE, method = "radix")
ae_list <- ae_list[ord, , drop = FALSE]

# --- Step 2: Format display values ---
# SAS PROPCASE: first char and any char after non-alphanumeric -> upper, rest lower
propcase_vec <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    if (is.null(s) || length(s) == 0) return(NA_character_)
    s <- as.character(s)
    if (s == "") return("")
    low <- tolower(s)
    ch <- strsplit(low, "", fixed = TRUE)[[1]]
    for (i in seq_along(ch)) {
      if (i == 1 || !grepl("[A-Za-z0-9]", ch[i-1], perl = TRUE)) {
        ch[i] <- toupper(ch[i])
      }
    }
    paste0(ch, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

sas_trunc <- function(x, n) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- as.character(s)
    if (nchar(s, type = "chars") > n) substr(s, 1, n) else s
  }, character(1), USE.NAMES = FALSE)
}

n <- nrow(ae_list)
severity_d <- propcase_vec(ae_list$SEVERITY)
rel_d <- propcase_vec(ae_list$RELATIONSHIP)
serious_d <- ifelse(!is.na(ae_list$SERIOUS) & ae_list$SERIOUS == "Y", "Yes", "No")

# ACTION_TAKEN mapping on UPCASE
up_act <- toupper(ae_list$ACTION_TAKEN)
action_d <- character(n)
for (i in seq_len(n)) {
  a <- ae_list$ACTION_TAKEN[i]
  u <- up_act[i]
  if (is.na(u) || is.na(a)) {
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
    action_d[i] <- propcase_vec(a)
  }
}

up_out <- toupper(ae_list$OUTCOME)
outcome_d <- character(n)
for (i in seq_len(n)) {
  o <- ae_list$OUTCOME[i]
  u <- up_out[i]
  if (is.na(u) || is.na(o)) {
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
    outcome_d[i] <- propcase_vec(o)
  }
}

# LENGTH truncation: severity_d $10 rel_d $15 serious_d $3 action_d $30 outcome_d $30
severity_d <- sas_trunc(severity_d, 10)
rel_d <- sas_trunc(rel_d, 15)
serious_d <- sas_trunc(serious_d, 3)
action_d <- sas_trunc(action_d, 30)
outcome_d <- sas_trunc(outcome_d, 30)

ae_report <- data.frame(
  TRTA = ae_list$TRTA,
  USUBJID = ae_list$USUBJID,
  SOC = as.character(ae_list$SOC),
  PT = as.character(ae_list$PT),
  VERBATIM = as.character(ae_list$VERBATIM),
  START_DATE = as.character(ae_list$START_DATE),
  END_DATE = as.character(ae_list$END_DATE),
  CTCAE_GRADE = as.character(ae_list$CTCAE_GRADE),
  severity_d = as.character(severity_d),
  serious_d = as.character(serious_d),
  rel_d = as.character(rel_d),
  action_d = as.character(action_d),
  outcome_d = as.character(outcome_d),
  stringsAsFactors = FALSE
)
# Convert "NA" strings from fmt? Ensure real NA stays NA
for (nm in names(ae_report)) {
  ae_report[[nm]][ae_report[[nm]] == "NA" & is.na(ae_list$USUBJID)] <- NA
}
# Re-fix: fmt_iso_date may have produced "NA" string for NA; correct
for (nm in c("START_DATE","END_DATE")) {
  v <- ae_report[[nm]]
  # where original was NA, set NA
  orig <- if (nm == "START_DATE") ae_list$AESTDTC else ae_list$AEENDTC
  v[is.na(orig)] <- NA_character_
  # haven may give "" -> keep ""
  ae_report[[nm]] <- v
}

# --- Write CSV of table body ---
csv_path <- file.path(out, "l_ae.csv")
write.csv(ae_report, csv_path, row.names = FALSE, na = "")

# --- Text rendering (RTF counterpart) ---
# Titles/footnotes from u_titles.sas for L_AE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
run_date9 <- toupper(format(Sys.Date(), "%d%b%Y"))
run_time5 <- format(Sys.time(), "%H:%M")

trt_levels <- unique(ae_report$TRTA)
# unique preserves first-appearance order which equals sorted order
n_pages <- length(trt_levels)
if (n_pages == 0) n_pages <- 1

header_labels <- c("Treatment","Subject ID","System Organ Class","Preferred Term",
                   "Verbatim Term","Start Date","End Date","CTCAE Grade",
                   "Severity","SAE","Relationship","Action Taken","Outcome")

disp_value <- function(x) {
  if (is.na(x)) return("")
  as.character(x)
}

txt_path <- file.path(out, "l_ae.txt")
con <- file(txt_path, open = "wt")

for (pg in seq_len(max(1, length(trt_levels)))) {
  cur_trt <- if (length(trt_levels) >= 1) trt_levels[pg] else NA_character_
  # Titles
  # title1 j=l sponsor j=r Page pageof
  page_str <- paste0("Page ", pg, " of ", n_pages)
  # left + right on one line
  line1 <- paste0(sponsor, "    ", page_str)
  cat(line1, "\n", sep = "", file = con)
  cat(paste0("Protocol: ", protocol), "\n", sep = "", file = con)
  cat(" \n", sep = "", file = con)
  cat("Listing 16.2.7.1\n", file = con)
  cat("Listing of Adverse Events\n", file = con)
  cat("Safety Analysis Set\n", file = con)
  cat("\n", file = con)

  if (nrow(ae_report) > 0) {
    sub <- ae_report[ae_report$TRTA == cur_trt, , drop = FALSE]
    # Header
    cat(paste(header_labels, collapse = " | "), "\n", sep = "", file = con)
    cat(paste(rep("----", length(header_labels)), collapse = " | "), "\n", sep = "", file = con)
    # ORDER suppression: TRTA shown only first row of page, USUBJID only first row per subject
    prev_subj <- NA_character_
    for (r in seq_len(nrow(sub))) {
      trta_disp <- if (r == 1) disp_value(sub$TRTA[r]) else ""
      usub_disp <- if (is.na(prev_subj) || (!is.na(sub$USUBJID[r]) && !is.na(prev_subj) && sub$USUBJID[r] != prev_subj) || (is.na(sub$USUBJID[r]) != is.na(prev_subj))) disp_value(sub$USUBJID[r]) else ""
      # handle first row NA case
      if (r == 1) usub_disp <- disp_value(sub$USUBJID[r])
      else {
        if (!identical(sub$USUBJID[r], prev_subj)) usub_disp <- disp_value(sub$USUBJID[r]) else usub_disp <- ""
        # identical handles NA==NA
        if (is.na(sub$USUBJID[r]) && is.na(prev_subj)) usub_disp <- ""
      }
      row_vals <- c(trta_disp, usub_disp,
                    disp_value(sub$SOC[r]), disp_value(sub$PT[r]),
                    disp_value(sub$VERBATIM[r]),
                    disp_value(sub$START_DATE[r]), disp_value(sub$END_DATE[r]),
                    disp_value(sub$CTCAE_GRADE[r]),
                    disp_value(sub$severity_d[r]), disp_value(sub$serious_d[r]),
                    disp_value(sub$rel_d[r]), disp_value(sub$action_d[r]),
                    disp_value(sub$outcome_d[r]))
      # Replace any "|" inside values to avoid breaking columns
      row_vals <- gsub("\\|", "/", row_vals, perl = TRUE)
      cat(paste(row_vals, collapse = " | "), "\n", sep = "", file = con)
      prev_subj <- sub$USUBJID[r]
    }
  } else {
    cat(paste(header_labels, collapse = " | "), "\n", sep = "", file = con)
  }
  cat("\n", file = con)
  cat(paste0("Data cutoff: ", cutdate, "."), "\n", sep = "", file = con)
  cat("Source: ADaM datasets\n", file = con)
  cat(paste0("Program: P076_l_ae.sas | Run: ", run_date9, " ", run_time5), "\n", sep = "", file = con)
  if (pg < n_pages) cat("\n--- Page Break ---\n\n", file = con)
}
close(con)

# progress only
cat("l_ae completed: rows=", nrow(ae_report), " pages=", n_pages, "\n", sep = "")
```