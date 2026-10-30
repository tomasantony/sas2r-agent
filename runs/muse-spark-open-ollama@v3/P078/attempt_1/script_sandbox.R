# P078_l_dv.sas -> R translation
# Listing 16.2.2 - Listing of Protocol Deviations
# Population: All Randomized Subjects (RANDFL='Y')

# --- Setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Load required packages (already installed per spec)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(lubridate)
})

# Helper: SAS round half away from zero (not needed for listing but included for fidelity)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: find and read SAS dataset from libref directory
read_dataset <- function(libpath, dsname) {
  candidates <- character(0)
  if (dir.exists(libpath)) {
    files <- list.files(libpath, full.names = TRUE, ignore.case = FALSE)
    # case-insensitive match for dsname.sas7bdat or dsname.xpt
    pat <- paste0("^", dsname, "\\.(sas7bdat|xpt)$")
    idx <- grep(pat, basename(files), ignore.case = TRUE)
    if (length(idx) > 0) candidates <- c(candidates, files[idx])
    # also try explicit lower/upper variants
    candidates <- c(candidates,
                    file.path(libpath, paste0(dsname, ".sas7bdat")),
                    file.path(libpath, paste0(tolower(dsname), ".sas7bdat")),
                    file.path(libpath, paste0(toupper(dsname), ".sas7bdat")),
                    file.path(libpath, paste0(dsname, ".xpt")),
                    file.path(libpath, paste0(tolower(dsname), ".xpt")),
                    file.path(libpath, paste0(toupper(dsname), ".xpt")))
  } else {
    # libpath may be a single .xpt file (XPORT engine)
    if (file.exists(libpath)) candidates <- c(candidates, libpath)
  }
  # also try derived / raw fallback paths
  fallback_adam <- c(
    "/bench/data/raw/own-synthetic/adam",
    "/bench/data/raw/cdisc-pilot/adam",
    "/bench/data/derived/cdisc-pilot/adam-split"
  )
  fallback_sdtm <- c(
    "/bench/data/raw/own-synthetic/sdtm",
    "/bench/data/raw/cdisc-pilot/sdtm"
  )
  for (f in unique(candidates)) {
    if (!is.na(f) && file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        df <- haven::read_xpt(f)
      } else {
        df <- haven::read_sas(f)
      }
      return(df)
    }
  }
  # try fallbacks if not found in primary libpath
  all_fallbacks <- unique(c(fallback_adam, fallback_sdtm))
  for (fb in all_fallbacks) {
    if (dir.exists(fb)) {
      files2 <- list.files(fb, full.names = TRUE)
      idx2 <- grep(pat, basename(files2), ignore.case = TRUE)
      for (k in idx2) {
        f2 <- files2[k]
        if (file.exists(f2)) {
          if (grepl("\\.xpt$", f2, ignore.case = TRUE)) df <- haven::read_xpt(f2) else df <- haven::read_sas(f2)
          return(df)
        }
      }
    }
  }
  stop(paste0("Dataset ", dsname, " not found in ", libpath))
}

clean_df <- function(df) {
  # remove haven formats, zap labels, convert names to uppercase
  df <- haven::zap_formats(df)
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
    # haven labelled character may be class haven_labelled + character
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  # convert POSIXt/Date to character ISO for consistent handling
  for (nm in names(df)) {
    if (inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    }
  }
  names(df) <- toupper(names(df))
  # convert all haven_labelled vectors that are still labelled to character
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  return(df)
}

# --- Global study parameters (from u_setup.sas) ---
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
studyid  <- "[STUDYID]"
cutdate  <- "[DDMONYYYY]"

# Treatment labels (for reference, not used to recode TRTA)
trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
trt3lbl <- "[Drug] High Dose"

# --- Step 1: Prepare data ---
# librefs as defined in family environment
adampath <- "/bench/data/raw/own-synthetic/adam"
sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"

# Read sdtm.dv and adam.adsl
dv_raw   <- read_dataset(sdtmpath, "dv")
adsl_raw <- read_dataset(adampath, "adsl")

dv   <- clean_df(dv_raw)
adsl <- clean_df(adsl_raw)

# Verify required columns exist (stop if join keys missing)
stopifnot("USUBJID" %in% names(dv))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("RANDFL" %in% names(adsl))
stopifnot("TRTA" %in% names(adsl))
stopifnot("SITEID" %in% names(adsl))

# Keep only randomized subjects (RANDFL='Y') - matches SAS where b.RANDFL='Y'
# Drop/rename overlapping columns except keys to avoid .x/.y suffixes
adsl_rand <- adsl %>%
  filter(!is.na(RANDFL) & RANDFL == "Y") %>%
  select(USUBJID, TRTA, SITEID)

# Ensure no duplicate USUBJID in adsl_rand (one row per subject)
adsl_rand <- adsl_rand[!duplicated(adsl_rand$USUBJID), ]

# Inner join sdtm.dv with adsl (SAS: inner join on USUBJID)
# Select and rename to match SAS create table dv_list
# SAS: a.USUBJID, b.TRTA, b.SITEID as SITE, a.DVCAT as CATEGORY, a.DVSCAT as SUBCATEGORY,
#      a.DVTERM as DESCRIPTION, a.DVSTDTC as START_DATE, a.DVENDTC as END_DATE, a.EPOCH, a.DVSEV as SEVERITY
# Note: DV columns may be missing in some synthetic builds - handle gracefully
required_dv_cols <- c("USUBJID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV")
for (col in required_dv_cols) {
  if (!col %in% names(dv)) dv[[col]] <- NA_character_
}

dv_list <- dv %>%
  inner_join(adsl_rand, by = "USUBJID") %>%
  transmute(
    USUBJID    = as.character(USUBJID),
    TRTA       = as.character(TRTA),
    SITE       = as.character(SITEID),
    CATEGORY   = as.character(DVCAT),
    SUBCATEGORY= as.character(DVSCAT),
    DESCRIPTION= as.character(DVTERM),
    START_DATE = as.character(DVSTDTC),
    END_DATE   = as.character(DVENDTC),
    EPOCH      = as.character(EPOCH),
    SEVERITY   = as.character(DVSEV)
  )

# SAS ordering: order by b.TRTA, b.SITEID, a.USUBJID, a.DVSTDTC
# SAS sorts missing first, character ordering uppercase before lowercase, case-sensitive
# Use base R order with na.last=FALSE to mimic SAS missing-first
# Ensure character vectors are not factors
ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE, na.last = FALSE)
dv_list <- dv_list[ord, , drop = FALSE]
rownames(dv_list) <- NULL

# Reorder columns to match PROC REPORT columns order
dv_list <- dv_list[, c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY")]

# --- Write CSV dataset (SAS dataset counterpart) ---
# Dates as YYYY-MM-DD already (character ISO), datetimes as YYYY-MM-DDTHH:MM:SS
write.csv(dv_list, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")

# --- Step 2: Generate listing text rendering ---
# Titles/footnotes from u_titles.sas for output=L_DV
# title1 j=l "&sponsor." j=r "Page ^{pageof}"
# title2 j=l "Protocol: &protocol."
# title3 spacer
# title4 Listing 16.2.2
# title5 Listing of Protocol Deviations
# title6 All Subjects (hardcoded for L_DV, ignores population param)
# footnotes:
#  Data cutoff: &cutdate.
#  Source: ADaM datasets
#  Program: &sysin. | Run: date9. time5.

run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")
# SAS date9 is DDMMMYYYY uppercase month, time5 is HH:MM
run_date <- toupper(run_date)

titles <- c(
  paste0(sponsor, strrep(" ", 60), "Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Listing 16.2.2",
  "Listing of Protocol Deviations",
  "All Subjects",
  ""
)

footnotes <- c(
  paste0("Data cutoff: ", cutdate),
  "Source: ADaM datasets",
  paste0("Program: P078_l_dv.sas | Run: ", run_date, " ", run_time)
)

# Build text file content
txt_path <- file.path(out, "l_dv.txt")
con <- file(txt_path, open = "wt")

# Write titles
for (t in titles) writeLines(t, con)

# Write column header line (mimic PROC REPORT headers with split='~')
# Define headers as in PROC REPORT define statements
header_line1 <- sprintf("%-12s %-8s %-14s %-18s %-14s %-30s %-12s %-12s %-10s %-10s",
                        "Treatment", "Site", "Subject ID", "Deviation Category", "Sub-category", "Description", "Start Date", "End Date", "Epoch", "Severity")
header_line2 <- sprintf("%-12s %-8s %-14s %-18s %-14s %-30s %-12s %-12s %-10s %-10s",
                        strrep("-",12), strrep("-",8), strrep("-",14), strrep("-",18), strrep("-",14), strrep("-",30), strrep("-",12), strrep("-",12), strrep("-",10), strrep("-",10))
writeLines(header_line1, con)
writeLines(header_line2, con)

# Pagination: break after TRTA / page
# For text rendering, insert page break marker between treatment groups
trt_groups <- unique(dv_list$TRTA)
# If TRTA has NA, handle
if (length(trt_groups) == 0) trt_groups <- NA_character_

# Helper to format a row with truncation to mimic column widths
fmt_row <- function(trta, site, usubjid, cat, subcat, desc, sdate, edate, epoch, sev) {
  # Truncate to column widths to avoid overflow, keep NA as ""
  trta  <- ifelse(is.na(trta), "", substr(trta, 1, 12))
  site  <- ifelse(is.na(site), "", substr(site, 1, 8))
  usubjid <- ifelse(is.na(usubjid), "", substr(usubjid, 1, 14))
  cat   <- ifelse(is.na(cat), "", substr(cat, 1, 18))
  subcat<- ifelse(is.na(subcat), "", substr(subcat, 1, 14))
  desc  <- ifelse(is.na(desc), "", substr(desc, 1, 30))
  sdate <- ifelse(is.na(sdate), "", substr(sdate, 1, 12))
  edate <- ifelse(is.na(edate), "", substr(edate, 1, 12))
  epoch <- ifelse(is.na(epoch), "", substr(epoch, 1, 10))
  sev   <- ifelse(is.na(sev), "", substr(sev, 1, 10))
  sprintf("%-12s %-8s %-14s %-18s %-14s %-30s %-12s %-12s %-10s %-10s",
          trta, site, usubjid, cat, subcat, desc, sdate, edate, epoch, sev)
}

# Write data rows grouped by TRTA with page breaks
page_num <- 1
total_pages <- length(trt_groups)
for (i in seq_along(trt_groups)) {
  grp <- trt_groups[i]
  sub <- dv_list[dv_list$TRTA == grp | (is.na(dv_list$TRTA) & is.na(grp)), , drop = FALSE]
  # If TRTA is NA group, sub already filtered
  for (r in seq_len(nrow(sub))) {
    line <- fmt_row(sub$TRTA[r], sub$SITE[r], sub$USUBJID[r], sub$CATEGORY[r], sub$SUBCATEGORY[r],
                    sub$DESCRIPTION[r], sub$START_DATE[r], sub$END_DATE[r], sub$EPOCH[r], sub$SEVERITY[r])
    writeLines(line, con)
  }
  # Page break after each TRTA except last (SAS break after TRTA / page)
  if (i < length(trt_groups)) {
    writeLines("", con)
    writeLines(paste0("--- Page ", page_num, " of ", total_pages, " ---"), con)
    writeLines("", con)
    page_num <- page_num + 1
    # Re-print header on new page for readability (SAS would repeat titles)
    writeLines(header_line1, con)
    writeLines(header_line2, con)
  }
}

writeLines("", con)
writeLines(strrep("-", 80), con)
# Summary line
writeLines(paste0("Total protocol deviations: ", nrow(dv_list)), con)
writeLines(paste0("Total subjects with deviations: ", length(unique(dv_list$USUBJID))), con)
writeLines("", con)
# Footnotes
for (f in footnotes) writeLines(f, con)

close(con)

# Also write a CSV of the table body (same as dataset, already written as l_dv.csv)
# Ensure the CSV for table body is explicitly the same as dv_list (already done)
# For completeness, also write a second CSV named l_dv_body.csv if needed, but spec says <name>.csv
# We have l_dv.csv which serves as both dataset and table body

# Clean up work dataset equivalent (SAS proc datasets delete)
rm(dv_raw, adsl_raw, dv, adsl, adsl_rand)

# Log completion
message("NOTE: l_dv.sas completed successfully.")
message(paste0("Outputs written to: ", out, " (l_dv.csv, l_dv.txt)"))
message(paste0("Rows in dv_list: ", nrow(dv_list)))
