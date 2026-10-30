```r
# P078_l_dv.sas -> R translation
# Listing 16.2.2 - Listing of Protocol Deviations
# Inputs: sdtm.dv, adam.adsl
# Output: l_dv.csv + l_dv.txt (+ l_dv.rtf)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- libraries ---
suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

# helper: SAS round half away from zero (not needed for listing but kept)
round_half_away <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: read a SAS dataset from a libref directory (sas7bdat or xpt)
read_sas_ds <- function(libpath, dsname) {
  # libpath is absolute directory, dsname is lower case e.g. "adsl"
  if (!dir.exists(libpath)) {
    stop(paste0("Libref directory not found: ", libpath))
  }
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # case-insensitive match
  idx <- which(tolower(basename(tools::file_path_sans_ext(files))) == tolower(dsname) &
               tolower(tools::file_ext(files)) %in% c("sas7bdat", "xpt", "sas7bdat7"))
  if (length(idx) == 0) {
    # try recursive search (some setups use subfolders)
    files2 <- list.files(libpath, full.names = TRUE, recursive = TRUE)
    idx2 <- which(tolower(basename(tools::file_path_sans_ext(files2))) == tolower(dsname) &
                  tolower(tools::file_ext(files2)) %in% c("sas7bdat", "xpt"))
    if (length(idx2) == 0) stop(paste0("Dataset not found: ", dsname, " in ", libpath))
    files <- files2
    idx <- idx2
  }
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- if (ext == "xpt") {
    haven::read_xpt(f)
  } else {
    haven::read_sas(f)
  }
  # zap labels/formats to plain vectors, keep as character/numeric
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  # haven labelled -> convert to character/numeric where needed
  # zap_labels would convert labelled to numeric; we want to keep values
  # For character labelled, convert to character
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      # if numeric labelled with no character levels, keep numeric; otherwise as character
      # Use as_factor with levels="labels" would lose unlabelled values, so use zap_labels then keep
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # make names upper case to mimic SAS (case-insensitive matching)
  # keep original names but ensure we can find USUBJID etc case-insensitively
  names(df) <- toupper(names(df))
  df
}

# --- paths from SAS librefs / macro variables ---
adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
# fallback to raw/cdisc-pilot if synthetic not present (robustness)
if (!dir.exists(adampath)) adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
if (!dir.exists(sdtmpath)) sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

# --- Step 1: Prepare data ---
adsl <- read_sas_ds(adampath, "adsl")
dv   <- read_sas_ds(sdtmpath, "dv")

# Ensure required columns exist (case-insensitive, now upper)
# ADSL needs USUBJID, TRTA, SITEID, RANDFL
# DV needs USUBJID, DVCAT, DVSCAT, DVTERM, DVSTDTC, DVENDTC, EPOCH, DVSEV
required_adsl <- c("USUBJID", "TRTA", "SITEID", "RANDFL")
required_dv   <- c("USUBJID", "DVCAT", "DVSCAT", "DVTERM", "DVSTDTC", "DVENDTC", "EPOCH", "DVSEV")
missing_adsl <- setdiff(required_adsl, names(adsl))
missing_dv   <- setdiff(required_dv, names(dv))
if (length(missing_adsl) > 0) stop(paste("ADSL missing columns:", paste(missing_adsl, collapse=",")))
if (length(missing_dv) > 0) {
  # DVSEV may be missing in some synthetic builds; allow missing and fill with NA
  for (cc in missing_dv) dv[[cc]] <- NA_character_
}

# Keep only needed columns to avoid .x/.y suffixes
adsl_keep <- adsl %>%
  select(USUBJID, TRTA, SITEID, RANDFL) %>%
  filter(!is.na(USUBJID))

dv_keep <- dv %>%
  select(USUBJID, DVCAT, DVSCAT, DVTERM, DVSTDTC, DVENDTC, EPOCH, DVSEV)

# Inner join on USUBJID, filter RANDFL='Y' (SAS where b.RANDFL='Y')
# Do join first then filter to mimic PROC SQL inner join + where
dv_list_raw <- dv_keep %>%
  inner_join(adsl_keep, by = "USUBJID") %>%
  filter(RANDFL == "Y")

stopifnot(all(c("TRTA","SITEID","USUBJID","DVCAT") %in% names(dv_list_raw)))

# Create table dv_list as select ... order by b.TRTA, b.SITEID, a.USUBJID, a.DVSTDTC
dv_list <- dv_list_raw %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTA = as.character(TRTA),
    SITE = as.character(SITEID),
    CATEGORY = as.character(DVCAT),
    SUBCATEGORY = as.character(DVSCAT),
    DESCRIPTION = as.character(DVTERM),
    START_DATE = as.character(DVSTDTC),
    END_DATE = as.character(DVENDTC),
    EPOCH = as.character(EPOCH),
    SEVERITY = as.character(DVSEV)
  ) %>%
  # SAS missing sort first: arrange with NA first (default in R for character is NA last, so handle)
  # Use dplyr arrange which puts NA last; to mimic SAS missing first, we create a helper
  mutate(
    .trta_sort = ifelse(is.na(TRTA), "", TRTA),
    .site_sort = ifelse(is.na(SITE), "", SITE),
    .usub_sort = ifelse(is.na(USUBJID), "", USUBJID),
    .stdtc_sort = ifelse(is.na(START_DATE), "", START_DATE)
  ) %>%
  arrange(.trta_sort, .site_sort, .usub_sort, .stdtc_sort) %>%
  select(-.trta_sort, -.site_sort, -.usub_sort, -.stdtc_sort) %>%
  # Reorder columns to match PROC REPORT columns order
  select(TRTA, SITE, USUBJID, CATEGORY, SUBCATEGORY, DESCRIPTION, START_DATE, END_DATE, EPOCH, SEVERITY)

# Ensure dates are ISO YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS if datetime; keep as is but normalize if possible
# If dates are SAS numeric dates (unlikely for SDTM), they would have been read as numeric; convert
# For character dates, keep original string; ensure NA -> ""
# No additional parsing to avoid multi-format guessing

# --- Write CSV (output dataset) ---
write.csv(dv_list, file.path(out, "l_dv.csv"), row.names = FALSE, na = "")

# --- Step 2: Generate listing text rendering ---
# Titles / footnotes from u_titles.sas for L_DV
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"
studyid  <- "[STUDYID]"

title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Listing 16.2.2"
title5 <- "Listing of Protocol Deviations"
title6 <- "All Subjects"

footnote1 <- paste0("Data cutoff: ", cutdate)
footnote2 <- "Source: ADaM datasets"
# SAS footnote3 includes program path and run datetime; reproduce verbatim pattern
footnote3 <- paste0("Program: P078_l_dv.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

# Build text file
txt_path <- file.path(out, "l_dv.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(paste0("Population: All Randomized Subjects (N=", nrow(distinct(adsl_keep %>% filter(RANDFL=='Y'), USUBJID)), ")"), con)
writeLines(paste0("Total protocol deviations: ", nrow(dv_list)), con)
writeLines("", con)

# Column headers with split='~' handling: "Subject~ID" -> two-line header, we flatten to "Subject ID"
header_labels <- c("Treatment", "Site", "Subject ID", "Deviation Category", "Sub-category", "Description", "Start Date", "End Date", "Epoch", "Severity")
# Use tab-separated for readability; also create fixed-width-like line
header_line <- paste(header_labels, collapse = " | ")
writeLines(header_line, con)
writeLines(paste(rep("-", nchar(header_line)), collapse=""), con)

# Break after TRTA / page : group by TRTA and insert page break marker
# SAS order is by TRTA, SITE, USUBJID, DVSTDTC - already sorted
if (nrow(dv_list) > 0) {
  # Ensure TRTA order respects SAS character ordering (uppercase before lowercase)
  # R's default sort is similar; we already arranged
  trta_levels <- unique(dv_list$TRTA)
  for (trt in trta_levels) {
    sub <- dv_list %>% filter(TRTA == trt | (is.na(TRTA) & is.na(trt)))
    writeLines("", con)
    writeLines(paste0("Treatment: ", ifelse(is.na(trt) || trt=="", "(Missing)", trt)), con)
    writeLines(paste(rep("-", 80), collapse=""), con)
    for (i in seq_len(nrow(sub))) {
      row <- sub[i, ]
      # Replace NA with "" for display
      vals <- sapply(row, function(x) ifelse(is.na(x) || is.null(x), "", as.character(x)))
      # Escape pipe in description
      vals <- gsub("\\|", "/", vals)
      line <- paste(vals, collapse = " | ")
      writeLines(line, con)
    }
    # page break marker (SAS break after TRTA / page)
    writeLines("", con)
    writeLines("--- Page Break ---", con)
  }
} else {
  writeLines("No protocol deviations found for All Randomized Subjects.", con)
}

writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
close(con)

# --- Also write RTF rendering (expected output name l_dv.rtf) ---
# Create a simple RTF file with same content; use r2rtf if available, otherwise manual RTF
rtf_path <- file.path(out, "l_dv.rtf")
# Try to use r2rtf for proper RTF; fallback to plain text wrapped in RTF header
tryCatch({
  # Build a data frame for r2rtf if package available
  if (requireNamespace("r2rtf", quietly = TRUE)) {
    # r2rtf expects a data frame; create body
    # Use r2rtf to create RTF listing
    library(r2rtf)
    # Prepare display data (replace NA with "")
    rtf_data <- dv_list
    rtf_data[is.na(rtf_data)] <- ""
    # Create RTF
    rtf_data %>%
      r2rtf::rtf_title(title = c(title1, title2, title4, title5, title6)) %>%
      r2rtf::rtf_colheader(colheader = paste(header_labels, collapse = " | ")) %>%
      r2rtf::rtf_body() %>%
      r2rtf::rtf_footnote(footnote = c(footnote1, footnote2, footnote3)) %>%
      r2rtf::rtf_encode() %>%
      r2rtf::write_rtf(rtf_path)
  } else {
    stop("r2rtf not available")
  }
}, error = function(e) {
  # Manual minimal RTF
  rtf_con <- file(rtf_path, open = "wt")
  writeLines("{\\rtf1\\ansi\\deff0", rtf_con)
  writeLines(paste0("{\\header ", gsub("\\\\", "\\\\\\\\", title1), "}"), rtf_con)
  # Titles
  for (t in c(title1, title2, title4, title5, title6)) {
    writeLines(paste0(t, "\\par"), rtf_con)
  }
  writeLines("\\par", rtf_con)
  writeLines(paste0(header_line, "\\par"), rtf_con)
  if (nrow(dv_list) > 0) {
    for (i in seq_len(nrow(dv_list))) {
      vals <- sapply(dv_list[i, ], function(x) ifelse(is.na(x), "", as.character(x)))
      line <- paste(vals, collapse = " | ")
      # escape RTF special chars
      line <- gsub("\\\\", "\\\\\\\\", line)
      line <- gsub("\\{", "\\\\{", line)
      line <- gsub("\\}", "\\\\}", line)
      writeLines(paste0(line, "\\par"), rtf_con)
    }
  }
  for (f in c(footnote1, footnote2, footnote3)) {
    writeLines(paste0(f, "\\par"), rtf_con)
  }
  writeLines("}", rtf_con)
  close(rtf_con)
})

# --- Clean up work dataset (mimic proc datasets delete) ---
rm(dv_list_raw)

# Log completion
message("NOTE: l_dv.sas completed successfully.")
message(paste0("Outputs written to: ", out, " (l_dv.csv, l_dv.txt, l_dv.rtf)"))
message(paste0("Rows in dv_list: ", nrow(dv_list)))
```