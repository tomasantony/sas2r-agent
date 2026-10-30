```r
# P078_l_dv.sas -> R translation
# Listing 16.2.2 - Listing of Protocol Deviations
# Inputs: sdtm.dv, adam.adsl
# Output: l_dv (CSV + TXT)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# Helper: SAS round half away from zero (not needed for listing but included)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: read SAS dataset by name from a directory (handles .sas7bdat and .xpt)
read_dataset <- function(dir, dsname) {
  dsname_lc <- tolower(dsname)
  if (!dir.exists(dir)) stop(paste0("Directory not found: ", dir))
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # try exact match ignoring case and extension
  # look for .sas7bdat, .xpt, .sas7bdat.gz etc
  pattern <- paste0("^", dsname_lc, "\\.(sas7bdat|xpt)$")
  idx <- grep(pattern, tolower(basename(files)))
  if (length(idx) == 0) {
    # also try without extension check (XPORT engine single file)
    # if dir itself is a file
    if (file.exists(dir) && grepl("\\.xpt$", tolower(dir))) {
      files <- dir
      idx <- 1
    } else {
      stop(paste0("Dataset ", dsname, " not found in ", dir, ". Files: ", paste(basename(files), collapse=", ")))
    }
  }
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    stop(paste0("Unknown extension for ", f))
  }
  # convert haven_labelled to plain vectors, keep character as character
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap labels but preserve values
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      # if numeric labelled, keep numeric; if character labelled, keep character
      # haven::zap_labels keeps type
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      df[[nm]] <- col
    }
    # convert labelled character that may be factor-like
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  # normalize column names to uppercase for case-insensitive matching (SAS is case-insensitive)
  names(df) <- toupper(names(df))
  df
}

# Helper: format date/datetime to YYYY-MM-DD / YYYY-MM-DDTHH:MM:SS
format_sas_date <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  # haven may import dates as numeric with SAS date origin? haven::read_sas already converts
  # If character, return as is (DVSTDTC is character ISO)
  if (is.character(x)) return(x)
  if (is.numeric(x)) return(as.character(x))
  as.character(x)
}

# Helper: convert NA to "" for display, keep character
disp <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

cat("Reading ADSL...\n")
adsl <- read_dataset(adam_path, "adsl")
cat("ADSL rows:", nrow(adsl), " cols:", paste(names(adsl), collapse=","), "\n")

cat("Reading DV...\n")
dv <- read_dataset(sdtm_path, "dv")
cat("DV rows:", nrow(dv), " cols:", paste(names(dv), collapse=","), "\n")

# Ensure required columns exist (case-insensitive, already upper)
# ADSL: USUBJID, TRTA, SITEID, RANDFL
# DV: USUBJID, DVCAT, DVSCAT, DVTERM, DVSTDTC, DVENDTC, EPOCH, DVSEV
required_adsl <- c("USUBJID","TRTA","SITEID","RANDFL")
required_dv <- c("USUBJID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV")
for (c in required_adsl) if (!c %in% names(adsl)) stop(paste0("ADSL missing column ", c))
for (c in required_dv) if (!c %in% names(dv)) {
  # DVSEV may be missing in some synthetic data; create as NA if missing
  if (c == "DVSEV") {
    dv$DVSEV <- NA_character_
  } else {
    stop(paste0("DV missing column ", c))
  }
}

# Filter ADSL where RANDFL = 'Y' (SAS case-sensitive)
adsl_f <- adsl[adsl$RANDFL == "Y" & !is.na(adsl$RANDFL), , drop=FALSE]
# Keep only needed columns
adsl_f <- adsl_f[, c("USUBJID","TRTA","SITEID"), drop=FALSE]

# Inner join DV with ADSL on USUBJID (SAS PROC SQL inner join)
# Use base merge
dv_list <- merge(dv, adsl_f, by = "USUBJID", all = FALSE)

# Select and rename as per SAS:
# a.USUBJID, b.TRTA, b.SITEID as SITE, a.DVCAT as CATEGORY, a.DVSCAT as SUBCATEGORY,
# a.DVTERM as DESCRIPTION, a.DVSTDTC as START_DATE, a.DVENDTC as END_DATE, a.EPOCH, a.DVSEV as SEVERITY
dv_list <- dv_list[, c("USUBJID","TRTA","SITEID","DVCAT","DVSCAT","DVTERM","DVSTDTC","DVENDTC","EPOCH","DVSEV"), drop=FALSE]
names(dv_list) <- c("USUBJID","TRTA","SITE","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY")

# Convert haven types to character where needed, ensure character columns are character
char_cols <- c("USUBJID","TRTA","SITE","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY")
for (cn in char_cols) {
  if (cn %in% names(dv_list)) {
    # if Date/POSIX, format
    if (inherits(dv_list[[cn]], "Date") || inherits(dv_list[[cn]], "POSIXct")) {
      dv_list[[cn]] <- format_sas_date(dv_list[[cn]])
    } else {
      dv_list[[cn]] <- as.character(dv_list[[cn]])
    }
  }
}

# SAS ordering: order by b.TRTA, b.SITEID, a.USUBJID, a.DVSTDTC
# Missing values sort first (na.last = FALSE), character ordering uppercase before lowercase (ASCII)
# Use order with na.last = FALSE
# For character, order is locale-dependent; enforce byte ordering by using xtfrm with locale C
# We use base order; to approximate SAS, we can use order with method="radix" which is byte-wise
ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE,
             na.last = FALSE, method = "radix")
dv_list <- dv_list[ord, , drop=FALSE]

# Reorder columns to match PROC REPORT columns order
dv_list <- dv_list[, c("TRTA","SITE","USUBJID","CATEGORY","SUBCATEGORY","DESCRIPTION","START_DATE","END_DATE","EPOCH","SEVERITY"), drop=FALSE]

# Write CSV dataset (SAS dataset counterpart)
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric unrounded, na=""
# Ensure any Date columns formatted (already character)
csv_path <- file.path(out, "l_dv.csv")
write.csv(dv_list, csv_path, row.names = FALSE, na = "")
cat("Wrote CSV:", csv_path, " rows:", nrow(dv_list), "\n")

# --- Text rendering (RTF listing text) ---
# Titles and footnotes as per u_titles.sas for L_DV
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

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P078_l_dv.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

# Build text file
txt_path <- file.path(out, "l_dv.txt")
rtf_path <- file.path(out, "l_dv.rtf")

# Column headers with SAS split='~' handling: "Subject~ID" -> two lines, we flatten to "Subject ID"
# Define display labels
col_labels <- c(
  TRTA = "Treatment",
  SITE = "Site",
  USUBJID = "Subject ID",
  CATEGORY = "Deviation Category",
  SUBCATEGORY = "Sub- category",
  DESCRIPTION = "Description",
  START_DATE = "Start Date",
  END_DATE = "End Date",
  EPOCH = "Epoch",
  SEVERITY = "Severity"
)

# Column widths in characters approximated from inches (9.5in total, ~10 chars per inch at 8pt)
# SAS widths: TRTA 0.8, SITE 0.5, USUBJID 0.8, CATEGORY 1.2, SUBCATEGORY 1.0, DESCRIPTION 2.0, START_DATE 0.7, END_DATE 0.7, EPOCH 0.8, SEVERITY 0.6
# Convert to char widths proportionally: total 9.5in -> ~120 chars landscape
widths_in <- c(TRTA=0.8, SITE=0.5, USUBJID=0.8, CATEGORY=1.2, SUBCATEGORY=1.0, DESCRIPTION=2.0, START_DATE=0.7, END_DATE=0.7, EPOCH=0.8, SEVERITY=0.6)
total_in <- sum(widths_in)
total_chars <- 132  # landscape 132 chars
widths_chars <- round(widths_in / total_in * total_chars)
# Ensure sum matches
# Adjust DESCRIPTION to fill
widths_chars["DESCRIPTION"] <- widths_chars["DESCRIPTION"] + (total_chars - sum(widths_chars))

# Helper to truncate/pad string to width
pad_str <- function(s, w, align="left") {
  s <- ifelse(is.na(s), "", as.character(s))
  # truncate if longer than width
  s <- ifelse(nchar(s, type="width") > w, substr(s, 1, w), s)
  if (align == "left") {
    sprintf(paste0("%-", w, "s"), s)
  } else if (align == "center") {
    # center
    pad <- w - nchar(s, type="width")
    left <- floor(pad/2)
    right <- pad - left
    paste0(strrep(" ", left), s, strrep(" ", right))
  } else {
    sprintf(paste0("%", w, "s"), s)
  }
}

# Build header lines
header_line1 <- paste(mapply(function(nm, w) pad_str(col_labels[nm], w, "center"), names(widths_chars), widths_chars), collapse=" ")
separator_line <- paste(mapply(function(w) strrep("-", w), widths_chars), collapse=" ")

# Open txt connection
con <- file(txt_path, open="wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(header_line1, con)
writeLines(separator_line, con)

# Break after TRTA / page : SAS inserts page break after each TRTA group
# We will insert a page break marker and repeat header
current_trta <- NA_character_
page_num <- 1
for (i in seq_len(nrow(dv_list))) {
  row <- dv_list[i, , drop=FALSE]
  # Check break after TRTA
  if (!is.na(current_trta) && !identical(disp(row$TRTA), disp(current_trta))) {
    # page break
    writeLines("", con)
    writeLines(paste0("--- Page break after Treatment: ", disp(current_trta), " ---"), con)
    writeLines("", con)
    page_num <- page_num + 1
    # repeat titles with updated page number
    writeLines(paste0(sponsor, "    Page ", page_num, " of 1"), con)
    writeLines(title2, con)
    writeLines(title3, con)
    writeLines(title4, con)
    writeLines(title5, con)
    writeLines(title6, con)
    writeLines("", con)
    writeLines(header_line1, con)
    writeLines(separator_line, con)
  }
  current_trta <- disp(row$TRTA)
  # Format row values
  vals <- c(
    TRTA = disp(row$TRTA),
    SITE = disp(row$SITE),
    USUBJID = disp(row$USUBJID),
    CATEGORY = disp(row$CATEGORY),
    SUBCATEGORY = disp(row$SUBCATEGORY),
    DESCRIPTION = disp(row$DESCRIPTION),
    START_DATE = disp(row$START_DATE),
    END_DATE = disp(row$END_DATE),
    EPOCH = disp(row$EPOCH),
    SEVERITY = disp(row$SEVERITY)
  )
  line <- paste(mapply(function(nm, w) pad_str(vals[nm], w, "left"), names(widths_chars), widths_chars), collapse=" ")
  writeLines(line, con)
}

writeLines("", con)
writeLines(separator_line, con)
writeLines(paste0("Total number of protocol deviations: ", nrow(dv_list)), con)
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)

close(con)
cat("Wrote TXT:", txt_path, "\n")

# Also create a simple RTF wrapper (plain text inside RTF) for compatibility
# Write minimal RTF that contains the same text
rtf_con <- file(rtf_path, open="wt")
writeLines("{\\rtf1\\ansi\\deff0", rtf_con)
writeLines("{\\fonttbl{\\f0\\fmodern Courier;}}", rtf_con)
writeLines("\\viewkind4\\uc1\\pard\\f0\\fs16", rtf_con)
# Read txt and escape RTF special chars
txt_lines <- readLines(txt_path, warn=FALSE)
for (ln in txt_lines) {
  # escape \, {, }
  esc <- gsub("\\\\", "\\\\\\\\", ln)
  esc <- gsub("\\{", "\\\\{", esc)
  esc <- gsub("\\}", "\\\\}", esc)
  # replace multiple spaces with \\~? Keep as is, RTF will collapse spaces, so use \\par
  writeLines(paste0(esc, "\\par"), rtf_con)
}
writeLines("}", rtf_con)
close(rtf_con)
cat("Wrote RTF:", rtf_path, "\n")

# Also write a CSV of the table body (same as dataset, for verification)
table_csv_path <- file.path(out, "l_dv_table.csv")
write.csv(dv_list, table_csv_path, row.names=FALSE, na="")
cat("Wrote table CSV:", table_csv_path, "\n")

cat("P078_l_dv completed successfully. Rows:", nrow(dv_list), "\n")
```