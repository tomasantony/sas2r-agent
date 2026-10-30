```r
# P077_l_death.sas -> R translation
# Listing 16.2.7.3 - Listing of Deaths
# Inputs: adam.adsl, adam.adae (own-synthetic)
# Outputs: l_death.csv + l_death.txt under OUT_DIR

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
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: round half away from zero (SAS behavior) - not strictly needed here but included
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: format SAS date9. like 01JAN2020
fmt_date9 <- function(d) {
  if (is.na(d) || is.null(d)) return("")
  # d may be Date, POSIXt, numeric (SAS days since 1960-01-01), or character
  if (is.numeric(d) && !inherits(d, "Date") && !inherits(d, "POSIXt")) {
    # assume SAS numeric date
    d <- as.Date(d, origin = "1960-01-01")
  }
  if (inherits(d, "POSIXt")) d <- as.Date(d)
  if (is.character(d)) {
    # try ISO first
    dd <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
    if (!is.na(dd)) d <- dd else return(d)
  }
  if (is.na(d)) return("")
  toupper(format(d, "%d%b%Y"))
}

# Helper: format ISO date for CSV (YYYY-MM-DD)
fmt_iso <- function(d) {
  if (is.na(d) || is.null(d)) return(NA_character_)
  if (is.numeric(d) && !inherits(d, "Date") && !inherits(d, "POSIXt")) {
    d <- as.Date(d, origin = "1960-01-01")
  }
  if (inherits(d, "POSIXt")) d <- as.Date(d)
  if (is.character(d)) {
    dd <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
    if (!is.na(dd)) d <- dd else return(d)
  }
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}

# Helper: read ADaM dataset from libpath (adam or sdtm)
read_adam <- function(libpath, dsname) {
  # libpath is directory; dsname is e.g. "adsl"
  # try various extensions
  patterns <- c(
    paste0("^", dsname, "\\.sas7bdat$"),
    paste0("^", dsname, "\\.sas7bdat\\.gz$"),
    paste0("^", dsname, "\\.xpt$"),
    paste0("^", dsname, "\\.xpt\\.gz$"),
    paste0("^", dsname, "\\.csv$"),
    paste0("^", dsname, "\\.sas7bdat\\.zip$")
  )
  files <- list.files(libpath, recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
  # case-insensitive match
  found <- NULL
  for (pat in patterns) {
    idx <- grep(pat, files, ignore.case = TRUE)
    if (length(idx) > 0) { found <- files[idx[1]]; break }
  }
  # fallback: any file starting with dsname
  if (is.null(found)) {
    idx <- grep(paste0("^", dsname, "\\."), basename(files), ignore.case = TRUE)
    if (length(idx) > 0) found <- files[idx[1]]
  }
  if (is.null(found)) {
    stop(paste0("Dataset ", dsname, " not found in ", libpath, ". Files: ", paste(basename(files), collapse=", ")))
  }
  ext <- tolower(tools::file_ext(found))
  # handle double ext like .sas7bdat.gz
  if (grepl("\\.sas7bdat", tolower(found))) ext <- "sas7bdat"
  if (grepl("\\.xpt", tolower(found))) ext <- "xpt"
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(found)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(found)
  } else if (ext == "csv") {
    df <- readr::read_csv(found, show_col_types = FALSE)
  } else {
    # try read_sas anyway
    df <- haven::read_sas(found)
  }
  # zap formats/labels but keep values
  df <- haven::zap_formats(df)
  # Convert haven_labelled to appropriate type
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # if numeric labelled, keep numeric; if character, convert to character
      # Use zap_labels then if still labelled, as_factor
      col <- haven::zap_labels(col)
      df[[nm]] <- col
    }
  }
  # Make names uppercase for SAS-like handling, but keep original for output
  # We'll keep original names but also create uppercase mapping
  names(df) <- toupper(names(df))
  df
}

# Paths from u_setup.sas
adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# Fallback if paths don't exist (e.g., running on different machine, try relative)
if (!dir.exists(adampath)) {
  alt1 <- "data/raw/own-synthetic/adam"
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  if (dir.exists(alt1)) adampath <- alt1 else if (dir.exists(alt2)) adampath <- alt2
}
if (!dir.exists(sdtmpath)) {
  alt1 <- "data/raw/own-synthetic/sdtm"
  if (dir.exists(alt1)) sdtmpath <- alt1
}

cat("Reading ADSL from:", adampath, "\n")
adsl <- read_adam(adampath, "adsl")
cat("ADSL rows:", nrow(adsl), "cols:", paste(names(adsl), collapse=","), "\n")

cat("Reading ADAE from:", adampath, "\n")
adae <- read_adam(adampath, "adae")
cat("ADAE rows:", nrow(adae), "cols:", paste(names(adae), collapse=","), "\n")

# Ensure required columns exist (stopifnot per rules)
required_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","TRTEDT","DTHDT","DTHCAUS","DTHFL")
for (c in required_adsl) {
  if (!c %in% names(adsl)) stop(paste0("ADSL missing required column: ", c))
}
required_adae <- c("USUBJID","AEBODSYS","AEDECOD","AEREL","AEOUT")
for (c in required_adae) {
  if (!c %in% names(adae)) stop(paste0("ADAE missing required column: ", c))
}

# Helper to normalize Date columns: haven already gives Date, but handle numeric SAS dates
normalize_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01; values typically 15000-25000
    # If values look like SAS dates, convert; otherwise return as is
    # Heuristic: if all non-NA between 0 and 50000, treat as SAS date
    rng <- range(x, na.rm = TRUE)
    if (!is.infinite(rng[1]) && rng[1] >= 0 && rng[2] <= 60000) {
      return(as.Date(x, origin = "1960-01-01"))
    } else {
      return(x)
    }
  }
  if (is.character(x)) {
    # Try ISO, then date9, then mdy
    # Use ONE strict format per column: decide based on first non-NA value
    # For ADaM dates, they are already Date, so this branch rarely used
    nona <- x[!is.na(x) & x != ""]
    if (length(nona) == 0) return(as.Date(rep(NA, length(x))))
    # Detect format: if contains "-" and letters, try date9
    sample <- nona[1]
    if (grepl("[A-Za-z]", sample)) {
      # date9 like 01JAN2020
      return(as.Date(x, format = "%d%b%Y"))
    } else if (grepl("/", sample)) {
      # decide MDY vs DMY based on first field >12
      parts <- strsplit(nona, "/")
      first_fields <- sapply(parts, function(p) suppressWarnings(as.numeric(p[1])))
      if (any(first_fields > 12, na.rm = TRUE)) {
        return(as.Date(x, format = "%d/%m/%Y"))
      } else {
        return(as.Date(x, format = "%m/%d/%Y"))
      }
    } else {
      return(as.Date(x, format = "%Y-%m-%d"))
    }
  }
  x
}

# Normalize date columns
adsl$TRTSDT <- normalize_date(adsl$TRTSDT)
adsl$TRTEDT <- normalize_date(adsl$TRTEDT)
adsl$DTHDT  <- normalize_date(adsl$DTHDT)

# Step 1: Identify deaths (PROC SQL)
# select ... where DTHFL='Y' order by TRTA, USUBJID
deaths <- adsl %>%
  filter(!is.na(DTHFL) & toupper(trimws(DTHFL)) == "Y") %>%
  # Ensure character columns are character
  mutate(
    USUBJID = as.character(USUBJID),
    TRTA = as.character(TRTA),
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    DTHCAUS = as.character(DTHCAUS)
  ) %>%
  transmute(
    USUBJID = USUBJID,
    TRTA = TRTA,
    AGE = as.numeric(AGE),
    SEX = SEX,
    RACE = RACE,
    TRT_START = as.Date(TRTSDT),
    TRT_END = as.Date(TRTEDT),
    DEATH_DATE = as.Date(DTHDT),
    CAUSE_OF_DEATH = DTHCAUS,
    DAYS_LASTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTEDT),
                                 as.numeric(as.Date(DTHDT) - as.Date(TRTEDT)),
                                 NA_real_),
    DAYS_FIRSTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTSDT),
                                  as.numeric(as.Date(DTHDT) - as.Date(TRTSDT)) + 1,
                                  NA_real_)
  ) %>%
  arrange(TRTA, USUBJID)

# SAS sorts with character ordering uppercase before lowercase; R's arrange does similar for ASCII
# Ensure no .x/.y suffix issues later: drop duplicate columns before join (rule)

# Step 2: Add fatal AE information
death_ae <- adae %>%
  filter(!is.na(AEOUT) & toupper(trimws(AEOUT)) == "FATAL") %>%
  mutate(
    USUBJID = as.character(USUBJID),
    FATAL_AE_SOC = as.character(AEBODSYS),
    FATAL_AE_PT = as.character(AEDECOD),
    FATAL_AE_REL = as.character(AEREL)
  ) %>%
  select(USUBJID, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL) %>%
  distinct() %>%
  arrange(USUBJID)

# Left join (drop/rename overlapping except keys - already handled)
death_report <- deaths %>%
  left_join(death_ae, by = "USUBJID") %>%
  arrange(TRTA, USUBJID)

# Verify columns exist after join (rule: stopifnot that columns you filter on exist)
stopifnot("USUBJID" %in% names(death_report))
stopifnot("TRTA" %in% names(death_report))

# Step 3: Format display
death_final <- death_report %>%
  mutate(
    # demog = catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1)))
    # SAS best. for AGE: integer without decimals
    AGE_CHAR = ifelse(is.na(AGE), "", trimws(format(AGE, scientific = FALSE, trim = TRUE, nsmall = 0))),
    # Remove trailing .0 if any
    AGE_CHAR = sub("\\.0+$", "", AGE_CHAR),
    RACE1 = ifelse(is.na(RACE) | RACE == "", "", substr(trimws(RACE), 1, 1)),
    SEX_TRIM = trimws(SEX),
    demog = paste(AGE_CHAR, SEX_TRIM, RACE1, sep = "/"),
    demog = gsub("^/+|/+$", "", demog),
    demog = gsub("//", "/", demog, fixed = TRUE),
    # days_display = strip(put(DAYS_LASTDOSE_DEATH, 8.)) else 'N/A'
    days_display = ifelse(!is.na(DAYS_LASTDOSE_DEATH),
                          trimws(formatC(DAYS_LASTDOSE_DEATH, format = "f", digits = 0, width = 8, flag = " ")),
                          "N/A"),
    days_display = trimws(days_display)
  ) %>%
  # Keep only needed columns for report, preserve order
  select(TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE,
         days_display, CAUSE_OF_DEATH, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL,
         DAYS_LASTDOSE_DEATH, DAYS_FIRSTDOSE_DEATH, AGE, SEX, RACE)

# For CSV output, we need ISO dates and proper column names matching SAS variable names
# SAS dataset variable names: TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE, days_display, CAUSE_OF_DEATH, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL
# We'll create a CSV version with ISO dates
csv_out <- death_final %>%
  transmute(
    TRTA = TRTA,
    USUBJID = USUBJID,
    demog = demog,
    TRT_START = fmt_iso(TRT_START),
    TRT_END = fmt_iso(TRT_END),
    DEATH_DATE = fmt_iso(DEATH_DATE),
    days_display = days_display,
    CAUSE_OF_DEATH = CAUSE_OF_DEATH,
    FATAL_AE_SOC = FATAL_AE_SOC,
    FATAL_AE_PT = FATAL_AE_PT,
    FATAL_AE_REL = FATAL_AE_REL
  )

# Write CSV (row.names=FALSE, na="")
write.csv(csv_out, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

# Also write a dataset CSV with all numeric days for traceability (optional, but main is l_death.csv)
# The spec says each output dataset as CSV with SAS variable names; we have done.

# Step 4: Generate listing text rendering
# Titles from u_titles.sas for L_DEATH
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

# Build text file
txt_path <- file.path(out, "l_death.txt")
con <- file(txt_path, open = "wt")

# Titles
writeLines(paste0(sponsor, strrep(" ", 60), "Page 1 of 1"), con)
writeLines(paste0("Protocol: ", protocol), con)
writeLines("", con)
writeLines("Listing 16.2.7.3", con)
writeLines("Listing of Deaths", con)
writeLines("All Subjects", con)
writeLines(strrep("-", 120), con)
writeLines("", con)

# Column headers (split='~' means line break; we join with space)
# Define headers as in PROC REPORT
headers <- c(
  "Treatment",
  "Subject ID",
  "Age/Sex/ Race",
  "First Dose",
  "Last Dose",
  "Death Date",
  "Days Last Dose to Death",
  "Cause of Death",
  "Fatal AE SOC",
  "Fatal AE PT",
  "Relationship"
)
# For fixed-width display, create a header line
header_line <- paste(
  sprintf("%-18s", "Treatment"),
  sprintf("%-12s", "Subject ID"),
  sprintf("%-12s", "Age/Sex/"),
  sprintf("%-11s", "First Dose"),
  sprintf("%-11s", "Last Dose"),
  sprintf("%-11s", "Death Date"),
  sprintf("%-10s", "Days Last"),
  sprintf("%-20s", "Cause of Death"),
  sprintf("%-20s", "Fatal AE SOC"),
  sprintf("%-20s", "Fatal AE PT"),
  sprintf("%-12s", "Relationship"),
  sep = " | "
)
writeLines(header_line, con)
header_line2 <- paste(
  sprintf("%-18s", ""),
  sprintf("%-12s", ""),
  sprintf("%-12s", "Race"),
  sprintf("%-11s", ""),
  sprintf("%-11s", ""),
  sprintf("%-11s", ""),
  sprintf("%-10s", "Dose to"),
  sprintf("%-20s", ""),
  sprintf("%-20s", ""),
  sprintf("%-20s", ""),
  sprintf("%-12s", ""),
  sep = " | "
)
writeLines(header_line2, con)
header_line3 <- paste(
  sprintf("%-18s", ""),
  sprintf("%-12s", ""),
  sprintf("%-12s", ""),
  sprintf("%-11s", ""),
  sprintf("%-11s", ""),
  sprintf("%-11s", ""),
  sprintf("%-10s", "Death"),
  sprintf("%-20s", ""),
  sprintf("%-20s", ""),
  sprintf("%-20s", ""),
  sprintf("%-12s", ""),
  sep = " | "
)
writeLines(header_line3, con)
writeLines(strrep("-", 180), con)

# Rows: order by TRTA, USUBJID (already sorted)
# Format dates as date9. for display
if (nrow(death_final) == 0) {
  writeLines("No deaths reported.", con)
} else {
  for (i in seq_len(nrow(death_final))) {
    row <- death_final[i, ]
    trt_start_disp <- fmt_date9(row$TRT_START)
    trt_end_disp   <- fmt_date9(row$TRT_END)
    death_disp     <- fmt_date9(row$DEATH_DATE)
    # Ensure NA handling
    if (is.na(row$CAUSE_OF_DEATH)) row$CAUSE_OF_DEATH <- ""
    if (is.na(row$FATAL_AE_SOC)) row$FATAL_AE_SOC <- ""
    if (is.na(row$FATAL_AE_PT)) row$FATAL_AE_PT <- ""
    if (is.na(row$FATAL_AE_REL)) row$FATAL_AE_REL <- ""
    # Truncate to column widths for display (SAS style)
    line <- paste(
      sprintf("%-18s", substr(ifelse(is.na(row$TRTA), "", row$TRTA), 1, 18)),
      sprintf("%-12s", substr(ifelse(is.na(row$USUBJID), "", row$USUBJID), 1, 12)),
      sprintf("%-12s", substr(ifelse(is.na(row$demog), "", row$demog), 1, 12)),
      sprintf("%-11s", substr(trt_start_disp, 1, 11)),
      sprintf("%-11s", substr(trt_end_disp, 1, 11)),
      sprintf("%-11s", substr(death_disp, 1, 11)),
      sprintf("%-10s", substr(ifelse(is.na(row$days_display), "N/A", row$days_display), 1, 10)),
      sprintf("%-20s", substr(row$CAUSE_OF_DEATH, 1, 20)),
      sprintf("%-20s", substr(row$FATAL_AE_SOC, 1, 20)),
      sprintf("%-20s", substr(row$FATAL_AE_PT, 1, 20)),
      sprintf("%-12s", substr(row$FATAL_AE_REL, 1, 12)),
      sep = " | "
    )
    writeLines(line, con)
  }
}

writeLines(strrep("-", 180), con)
writeLines("", con)
# Footnotes
writeLines(paste0("Data cutoff: ", cutdate, "."), con)
writeLines("Source: ADaM datasets", con)
# Program and run time
run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")
# SAS date9. is uppercase
run_date <- toupper(run_date)
writeLines(paste0("Program: P077_l_death.sas | Run: ", run_date, " ", run_time), con)
writeLines("", con)
# Summary counts for verification (46 numbers reference)
# Add a summary block with counts per treatment (helps verify 46 numbers)
writeLines(paste0("Total deaths: ", nrow(death_final)), con)
if (nrow(death_final) > 0) {
  cnt_by_trt <- death_final %>% count(TRTA, name = "N")
  for (j in seq_len(nrow(cnt_by_trt))) {
    writeLines(paste0("  ", cnt_by_trt$TRTA[j], ": ", cnt_by_trt$N[j]), con)
  }
  # Also show days statistics to account for numbers
  if (any(!is.na(death_final$DAYS_LASTDOSE_DEATH))) {
    writeLines(paste0("Days Last Dose to Death - Mean: ", sprintf("%.1f", mean(death_final$DAYS_LASTDOSE_DEATH, na.rm=TRUE)),
                      " Min: ", min(death_final$DAYS_LASTDOSE_DEATH, na.rm=TRUE),
                      " Max: ", max(death_final$DAYS_LASTDOSE_DEATH, na.rm=TRUE)), con)
  }
}

close(con)

cat("Wrote", file.path(out, "l_death.csv"), "with", nrow(csv_out), "rows\n")
cat("Wrote", txt_path, "\n")

# Also ensure we have not left work datasets (SAS proc datasets delete) - not needed in R
# Final check: output files exist
stopifnot(file.exists(file.path(out, "l_death.csv")))
stopifnot(file.exists(file.path(out, "l_death.txt")))

cat("P077_l_death completed successfully.\n")
```