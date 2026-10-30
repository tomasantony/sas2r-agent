```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_propcase_vec <- function(x) {
  x <- as.character(x)
  is_miss <- is.na(x) | trimws(x) == ""
  # SAS PROPCASE: first letter upper, rest lower for each word
  res <- stringr::str_to_title(tolower(x), locale = "en")
  res[is_miss] <- NA_character_
  res
}

read_lib_dataset <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) stop(paste0("No files in ", dir))
  base_noext <- toupper(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == toupper(name))
  if (length(idx) == 0) stop(paste0("Dataset ", name, " not found in ", dir))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    df <- haven::read_sas(f)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

cat("Reading ADAE and ADSL...\n")
adae <- read_lib_dataset(adam_dir, "ADAE")
adsl <- read_lib_dataset(adam_dir, "ADSL")

stopifnot("USUBJID" %in% names(adae))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("SAFFL" %in% names(adsl))
stopifnot("TRTEMFL" %in% names(adae))

# Keep only needed columns before join to avoid .x/.y suffixes
# SAS: select a.USUBJID, b.TRTA,b.AGE,b.SEX,b.RACE, a.AEBODSYS,a.AEDECOD,a.AETERM,
#      a.AESTDTC,a.AEENDTC,a.AESEV,a.AEREL,a.AESER,a.AEACN,a.AEOUT,a.AETOXGR,a.TRTEMFL
need_adae <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC",
               "AESEV","AEREL","AESER","AEACN","AEOUT","AETOXGR","TRTEMFL")
need_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","SAFFL")
for (v in need_adae) if (!(v %in% names(adae))) stop(paste0("ADAE missing ", v))
for (v in need_adsl) if (!(v %in% names(adsl))) stop(paste0("ADSL missing ", v))

adae_k <- adae[, need_adae, drop = FALSE]
adsl_k <- adsl[, need_adsl, drop = FALSE]

# Character-compare helper: SAS is case-sensitive, missing is "" or NA
to_chr <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%S"))
  as.character(x)
}
adae_k$USUBJID <- to_chr(adae_k$USUBJID)
adsl_k$USUBJID <- to_chr(adsl_k$USUBJID)
adae_k$TRTEMFL <- to_chr(adae_k$TRTEMFL)
adsl_k$SAFFL <- to_chr(adsl_k$SAFFL)

# Inner join + where b.SAFFL='Y' and a.TRTEMFL='Y'
ae_list <- merge(adae_k, adsl_k, by = "USUBJID", all = FALSE)
stopifnot("TRTA" %in% names(ae_list))
stopifnot("SAFFL" %in% names(ae_list))
stopifnot("TRTEMFL" %in% names(ae_list))

ae_list <- ae_list[!is.na(ae_list$SAFFL) & ae_list$SAFFL == "Y" &
                   !is.na(ae_list$TRTEMFL) & ae_list$TRTEMFL == "Y", , drop = FALSE]
stopifnot(nrow(ae_list) > 0)

# Rename to listing names (SAS aliases)
# a.AEBODSYS as SOC, a.AEDECOD as PT, a.AETERM as VERBATIM,
# a.AESTDTC as START_DATE, a.AEENDTC as END_DATE,
# a.AESEV as SEVERITY, a.AEREL as RELATIONSHIP, a.AESER as SERIOUS,
# a.AEACN as ACTION_TAKEN, a.AEOUT as OUTCOME, a.AETOXGR as CTCAE_GRADE
ae_list$SOC <- to_chr(ae_list$AEBODSYS)
ae_list$PT <- to_chr(ae_list$AEDECOD)
ae_list$VERBATIM <- to_chr(ae_list$AETERM)
ae_list$START_DATE <- to_chr(ae_list$AESTDTC)
ae_list$END_DATE <- to_chr(ae_list$AEENDTC)
ae_list$SEVERITY <- to_chr(ae_list$AESEV)
ae_list$RELATIONSHIP <- to_chr(ae_list$AEREL)
ae_list$SERIOUS <- to_chr(ae_list$AESER)
ae_list$ACTION_TAKEN <- to_chr(ae_list$AEACN)
ae_list$OUTCOME <- to_chr(ae_list$AEOUT)
ae_list$CTCAE_GRADE <- to_chr(ae_list$AETOXGR)
ae_list$TRTA <- to_chr(ae_list$TRTA)
ae_list$USUBJID <- to_chr(ae_list$USUBJID)

# order by b.TRTA, a.USUBJID, a.AESTDTC, a.AEBODSYS, a.AEDECOD
# SAS: missing first, character ASCII (uppercase before lowercase)
ord <- order(ae_list$TRTA, ae_list$USUBJID, ae_list$START_DATE,
             ae_list$SOC, ae_list$PT,
             na.last = FALSE, method = "radix")
ae_list <- ae_list[ord, , drop = FALSE]

# Step 2: Format display values
ae_report <- ae_list
ae_report$severity_d <- sas_propcase_vec(ae_report$SEVERITY)
ae_report$rel_d <- sas_propcase_vec(ae_report$RELATIONSHIP)
# IFC(SERIOUS='Y','Yes','No') : missing -> missing (4th arg absent)
ae_report$serious_d <- ifelse(is.na(ae_report$SERIOUS) | trimws(ae_report$SERIOUS) == "",
                              NA_character_,
                              ifelse(ae_report$SERIOUS == "Y", "Yes", "No"))
# ACTION_TAKEN mapping on UPCASE
up_act <- toupper(ae_report$ACTION_TAKEN)
ae_report$action_d <- ifelse(!is.na(up_act) & up_act == "DRUG WITHDRAWN", "Drug Withdrawn",
                      ifelse(!is.na(up_act) & up_act == "DOSE REDUCED", "Dose Reduced",
                      ifelse(!is.na(up_act) & up_act == "DRUG INTERRUPTED", "Drug Interrupted",
                      ifelse(!is.na(up_act) & up_act == "DOSE NOT CHANGED", "Dose Not Changed",
                      ifelse(!is.na(up_act) & up_act == "NOT APPLICABLE", "Not Applicable",
                             sas_propcase_vec(ae_report$ACTION_TAKEN))))))
# OUTCOME mapping on UPCASE
up_out <- toupper(ae_report$OUTCOME)
ae_report$outcome_d <- ifelse(!is.na(up_out) & up_out == "RECOVERED/RESOLVED", "Resolved",
                       ifelse(!is.na(up_out) & up_out == "RECOVERING/RESOLVING", "Resolving",
                       ifelse(!is.na(up_out) & up_out == "NOT RECOVERED/NOT RESOLVED", "Not Resolved",
                       ifelse(!is.na(up_out) & up_out == "RECOVERED/RESOLVED WITH SEQUELAE", "Resolved w/ Sequelae",
                       ifelse(!is.na(up_out) & up_out == "FATAL", "Fatal",
                              sas_propcase_vec(ae_report$OUTCOME))))))

# PROC REPORT columns
rep_cols <- c("TRTA","USUBJID","SOC","PT","VERBATIM","START_DATE","END_DATE",
              "CTCAE_GRADE","severity_d","serious_d","rel_d","action_d","outcome_d")
for (v in rep_cols) if (!(v %in% names(ae_report))) ae_report[[v]] <- NA_character_
report_df <- ae_report[, rep_cols, drop = FALSE]

# Ensure character, ISO dates already; convert any Date-like just in case
for (v in rep_cols) {
  report_df[[v]] <- to_chr(report_df[[v]])
  report_df[[v]][is.na(report_df[[v]])] <- ""
}

# Write CSV of table body (full values, SAS variable names)
csv_out <- report_df
names(csv_out) <- rep_cols
write.csv(csv_out, file.path(out, "l_ae.csv"), row.names = FALSE, na = "")
cat(paste0("Wrote l_ae.csv with ", nrow(csv_out), " rows\n"))

# Build text rendering mimicking PROC REPORT with ORDER + BREAK AFTER TRTA / PAGE
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
title4 <- "Listing 16.2.7.1"
title5 <- "Listing of Adverse Events"
title6 <- "Safety Analysis Set"
fn1 <- paste0("Data cutoff: ", cutdate, ".")
fn2 <- "Source: ADaM datasets"
fn3 <- paste0("Program: l_ae.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

trt_levels <- unique(report_df$TRTA)
# SAS ORDER BY already sorted; unique preserves order
n_pages <- length(trt_levels)
if (n_pages == 0) n_pages <- 1

header_labels <- c("Treatment","Subject ID","System Organ Class","Preferred Term",
                   "Verbatim Term","Start Date","End Date","CTCAE Grade",
                   "Severity","SAE","Relationship","Action Taken","Outcome")

# ORDER display: blank repeated TRTA/USUBJID within consecutive rows
disp_TRTA <- report_df$TRTA
disp_USUBJID <- report_df$USUBJID
for (i in seq_len(nrow(report_df))) {
  if (i > 1 && disp_TRTA[i] == report_df$TRTA[i-1]) disp_TRTA[i] <- ""
  if (i > 1 && report_df$USUBJID[i] == report_df$USUBJID[i-1]) disp_USUBJID[i] <- ""
}

lines <- c()
for (pg in seq_along(trt_levels)) {
  trt <- trt_levels[pg]
  lines <- c(lines,
    paste0(sponsor, "                                                  Page ", pg, " of ", n_pages),
    paste0("Protocol: ", protocol),
    "",
    title4,
    title5,
    title6,
    "")
  lines <- c(lines, paste(header_labels, collapse = " | "))
  lines <- c(lines, paste(rep("----", length(header_labels)), collapse = " | "))
  idx <- which(report_df$TRTA == trt)
  for (i in idx) {
    row_vals <- c(disp_TRTA[i], disp_USUBJID[i],
                  report_df$SOC[i], report_df$PT[i], report_df$VERBATIM[i],
                  report_df$START_DATE[i], report_df$END_DATE[i],
                  report_df$CTCAE_GRADE[i], report_df$severity_d[i],
                  report_df$serious_d[i], report_df$rel_d[i],
                  report_df$action_d[i], report_df$outcome_d[i])
    row_vals[is.na(row_vals)] <- ""
    # avoid breaking pipe by replacing any pipe in text
    row_vals <- gsub("\\|", "/", row_vals)
    lines <- c(lines, paste(row_vals, collapse = " | "))
  }
  lines <- c(lines, "",
    fn1, fn2, fn3)
  if (pg < length(trt_levels)) lines <- c(lines, "", "\f", "")
}

writeLines(lines, file.path(out, "l_ae.txt"), useBytes = TRUE)
cat(paste0("Wrote l_ae.txt with ", length(lines), " lines\n"))
```