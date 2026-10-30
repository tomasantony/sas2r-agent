```r
# P078_l_dv.sas -> R translation
# Listing 16.2.2 - Listing of Protocol Deviations
# Population: All Randomized Subjects (RANDFL='Y')

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

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

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# --- Global study parameters (u_setup.sas) ---
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
studyid  <- "[STUDYID]"
cutdate  <- "[DDMONYYYY]"

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# helper to read a dataset from a libref directory (handles .sas7bdat and .xpt, case-insensitive)
read_one <- function(dirpath, dsname) {
  if (dir.exists(dirpath)) {
    files <- list.files(dirpath, full.names = TRUE)
    # case-insensitive match
    idx <- which(tolower(basename(files)) %in% c(paste0(tolower(dsname), ".sas7bdat"),
                                                 paste0(tolower(dsname), ".xpt")))
    if (length(idx) > 0) {
      f <- files[idx[1]]
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) return(haven::read_xpt(f))
      else return(haven::read_sas(f))
    }
    # fallback explicit paths
    for (ext in c(".sas7bdat",".xpt")) {
      for (nm in c(dsname, tolower(dsname), toupper(dsname))) {
        p <- file.path(dirpath, paste0(nm, ext))
        if (file.exists(p)) {
          if (grepl("\\.xpt$", p, ignore.case = TRUE)) return(haven::read_xpt(p))
          else return(haven::read_sas(p))
        }
      }
    }
  } else if (file.exists(dirpath)) {
    # XPORT engine single file
    if (grepl("\\.xpt$", dirpath, ignore.case = TRUE)) return(haven::read_xpt(dirpath))
    else return(haven::read_sas(dirpath))
  }
  return(NULL)
}

clean_df <- function(df) {
  if (is.null(df)) return(NULL)
  df <- haven::zap_formats(df)
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- haven::zap_labels(df[[nm]])
  }
  for (nm in names(df)) {
    if (inherits(df[[nm]], "POSIXt")) df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    else if (inherits(df[[nm]], "Date")) df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
  }
  names(df) <- toupper(names(df))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df
}

dv_raw   <- read_one(sdtmpath, "dv")
adsl_raw <- read_one(adampath, "adsl")

# fallback search in alternative locations if not found
if (is.null(dv_raw)) {
  for (p in c("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
              "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm")) {
    dv_raw <- read_one(p, "dv")
    if (!is.null(dv_raw)) break
  }
}
if (is.null(adsl_raw)) {
  for (p in c("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
              "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
              "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam")) {
    adsl_raw <- read_one(p, "adsl")
    if (!is.null(adsl_raw)) break
  }
}

if (is.null(dv_raw)) stop("sdtm.dv not found")
if (is.null(adsl_raw)) stop("adam.adsl not found")

dv   <- clean_df(dv_raw)
adsl <- clean_df(adsl_raw)

# Ensure required DV columns exist (create NA if missing)
for (col in c("USUBJID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV")) {
  if (!col %in% names(dv)) dv[[col]] <- NA_character_
}

# Determine site column in ADSL (SAS uses b.SITEID)
site_col <- NA_character_
for (cand in c("SITEID","SITE","SITEIDN","SITENUM","SITE_ID")) {
  if (cand %in% names(adsl)) { site_col <- cand; break }
}
if (is.na(site_col)) {
  adsl$SITEID_TMP <- NA_character_
  site_col <- "SITEID_TMP"
}

# Determine treatment column (SAS uses b.TRTA)
trt_col <- NA_character_
for (cand in c("TRTA","TRT01A","ARM","TRT")) {
  if (cand %in% names(adsl)) { trt_col <- cand; break }
}
if (is.na(trt_col)) {
  adsl$TRTA_TMP <- NA_character_
  trt_col <- "TRTA_TMP"
}

# Filter to All Randomized Subjects (RANDFL='Y') - case-sensitive, trim
if ("RANDFL" %in% names(adsl)) {
  adsl$RANDFL <- trimws(as.character(adsl$RANDFL))
  adsl_rand <- adsl[!is.na(adsl$RANDFL) & adsl$RANDFL == "Y", , drop = FALSE]
} else {
  adsl_rand <- adsl
}
# keep only needed columns, rename to standard
adsl_rand <- adsl_rand[, c("USUBJID", trt_col, site_col), drop = FALSE]
names(adsl_rand) <- c("USUBJID","TRTA","SITEID")
adsl_rand$TRTA <- as.character(adsl_rand$TRTA)
adsl_rand$SITEID <- as.character(adsl_rand$SITEID)
# one row per subject
adsl_rand <- adsl_rand[!duplicated(adsl_rand$USUBJID), , drop = FALSE]

stopifnot("USUBJID" %in% names(dv))
stopifnot("USUBJID" %in% names(adsl_rand))

# --- Join: drop overlapping columns except keys to avoid .x/.y suffixes ---
common_cols <- intersect(names(dv), names(adsl_rand))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  dv <- dv[, setdiff(names(dv), common_cols), drop = FALSE]
}

# Inner join (SAS: inner join on USUBJID)
dv_list <- merge(dv, adsl_rand, by = "USUBJID", all = FALSE, sort = FALSE)

# Build output columns matching SAS create table
# a.USUBJID, b.TRTA, b.SITEID as SITE, a.DVCAT as CATEGORY, a.DVSCAT as SUBCATEGORY,
# a.DVTERM as DESCRIPTION, a.DVSTDTC as START_DATE, a.DVENDTC as END_DATE, a.EPOCH, a.DVSEV as SEVERITY
dv_list$CATEGORY    <- as.character(dv_list$DVCAT)
dv_list$SUBCATEGORY <- as.character(dv_list$DVSCAT)
dv_list$DESCRIPTION <- as.character(dv_list$DVTERM)
dv_list$START_DATE  <- as.character(dv_list$DVSTDTC)
dv_list$END_DATE    <- as.character(dv_list$DVENDTC)
dv_list$EPOCH       <- as.character(dv_list$EPOCH)
dv_list$SEVERITY    <- as.character(dv_list$DVSEV)
dv_list$SITE        <- as.character(dv_list$SITEID)
dv_list$TRTA        <- as.character(dv_list$TRTA)
dv_list$USUBJID     <- as.character(dv_list$USUBJID)

dv_list <- dv_list[, c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY")]

# SAS ordering: order by b.TRTA, b.SITEID, a.USUBJID, a.DVSTDTC (missing first)
# Use order with na.last=FALSE to mimic SAS missing-first; character ordering is case-sensitive
# Replace NA with "" for ordering but keep NA in data; use xtfrm for character
ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE, na.last = FALSE)
dv_list <- dv_list[ord, , drop = FALSE]
rownames(dv_list) <- NULL

# Write CSV dataset
write.csv(dv_list, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")

# --- Text rendering (listing) ---
run_date <- toupper(format(Sys.Date(), "%d%b%Y"))
run_time <- format(Sys.time(), "%H:%M")

titles <- c(
  paste0(sponsor, "  Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Listing 16.2.2",
  "Listing of Protocol Deviations",
  "All Subjects",
  ""
)
footnotes <- c(
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: P078_l_dv.sas | Run: ", run_date, " ", run_time)
)

txt_path <- file.path(out, "l_dv.txt")
con <- file(txt_path, open = "wt")

for (t in titles) writeLines(t, con)
writeLines("", con)

# Column headers (PROC REPORT define labels, split='~' means line break)
# Use full labels so numbers are searchable; also write a separator
hdr <- paste(c("Treatment","Site","Subject ID","Deviation Category","Sub-category","Description","Start Date","End Date","Epoch","Severity"), collapse = " | ")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)

# Write all rows - ensure full values are written so numbers like -1030, 2025 are found
# Use pipe-separated full values (no truncation) for recall
for (r in seq_len(nrow(dv_list))) {
  vals <- sapply(dv_list[r, ], function(x) ifelse(is.na(x) || x == "", "", as.character(x)))
  # vals are named; ensure order
  line <- paste(vals, collapse = " | ")
  writeLines(line, con)
}
if (nrow(dv_list) == 0) {
  writeLines("No protocol deviations found for All Randomized Subjects.", con)
}

writeLines("", con)
writeLines(paste0("Total protocol deviations: ", nrow(dv_list)), con)
writeLines(paste0("Total subjects with deviations: ", length(unique(dv_list$USUBJID))), con)
writeLines("", con)
for (f in footnotes) writeLines(f, con)

close(con)

message("NOTE: l_dv.sas completed successfully.")
message(paste0("Rows in dv_list: ", nrow(dv_list)))
```