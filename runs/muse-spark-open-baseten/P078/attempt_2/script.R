out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

read_dataset <- function(dir, dsname) {
  dsname_lc <- tolower(dsname)
  if (!dir.exists(dir)) stop(paste0("Directory not found: ", dir))
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  pattern <- paste0("^", dsname_lc, "\\.(sas7bdat|xpt)$")
  idx <- grep(pattern, tolower(basename(files)))
  if (length(idx) == 0) {
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
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      df[[nm]] <- col
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  names(df) <- toupper(names(df))
  df
}

format_sas_date <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%S"))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.character(x)) return(x)
  if (is.numeric(x)) return(as.character(x))
  as.character(x)
}

disp <- function(x) ifelse(is.na(x), "", as.character(x))

cat("Reading ADSL...\n")
adsl <- read_dataset(adam_path, "adsl")
cat("ADSL rows:", nrow(adsl), " cols:", paste(names(adsl), collapse=","), "\n")

cat("Reading DV...\n")
dv <- read_dataset(sdtm_path, "dv")
cat("DV rows:", nrow(dv), " cols:", paste(names(dv), collapse=","), "\n")

# ADSL filter RANDFL='Y'
adsl_f <- adsl[adsl$RANDFL == "Y" & !is.na(adsl$RANDFL), , drop=FALSE]
adsl_f <- adsl_f[, c("USUBJID","TRTA","SITEID"), drop=FALSE]
names(adsl_f) <- c("USUBJID","TRTA_ADSL","SITEID_ADSL")

# Inner join DV with ADSL on USUBJID - handle duplicate SITEID column
dv_list_raw <- merge(dv, adsl_f, by = "USUBJID", all = FALSE)

# Build output columns per SAS:
# a.USUBJID, b.TRTA, b.SITEID as SITE, a.DVCAT as CATEGORY, a.DVSCAT as SUBCATEGORY,
# a.DVTERM as DESCRIPTION, a.DVSTDTC as START_DATE, a.DVENDTC as END_DATE, a.EPOCH, a.DVSEV as SEVERITY
# Ensure DVSEV exists
if (!"DVSEV" %in% names(dv_list_raw)) dv_list_raw$DVSEV <- NA_character_
if (!"DVSCAT" %in% names(dv_list_raw)) dv_list_raw$DVSCAT <- NA_character_
if (!"DVCAT" %in% names(dv_list_raw)) dv_list_raw$DVCAT <- NA_character_
if (!"DVTERM" %in% names(dv_list_raw)) dv_list_raw$DVTERM <- NA_character_
if (!"DVSTDTC" %in% names(dv_list_raw)) dv_list_raw$DVSTDTC <- NA_character_
if (!"DVENDTC" %in% names(dv_list_raw)) dv_list_raw$DVENDTC <- NA_character_
if (!"EPOCH" %in% names(dv_list_raw)) dv_list_raw$EPOCH <- NA_character_

dv_list <- data.frame(
  TRTA = as.character(dv_list_raw$TRTA_ADSL),
  SITE = as.character(dv_list_raw$SITEID_ADSL),
  USUBJID = as.character(dv_list_raw$USUBJID),
  CATEGORY = as.character(dv_list_raw$DVCAT),
  SUBCATEGORY = as.character(dv_list_raw$DVSCAT),
  DESCRIPTION = as.character(dv_list_raw$DVTERM),
  START_DATE = as.character(dv_list_raw$DVSTDTC),
  END_DATE = as.character(dv_list_raw$DVENDTC),
  EPOCH = as.character(dv_list_raw$EPOCH),
  SEVERITY = as.character(dv_list_raw$DVSEV),
  stringsAsFactors = FALSE
)

# Format any Date/POSIX that slipped through (DVSTDTC is character ISO, keep as is)
for (cn in names(dv_list)) {
  if (inherits(dv_list[[cn]], "Date") || inherits(dv_list[[cn]], "POSIXct")) {
    dv_list[[cn]] <- format_sas_date(dv_list[[cn]])
  }
}

# SAS ordering: order by b.TRTA, b.SITEID, a.USUBJID, a.DVSTDTC
# Missing first, radix for SAS-like byte ordering
ord <- order(dv_list$TRTA, dv_list$SITE, dv_list$USUBJID, dv_list$START_DATE,
             na.last = FALSE, method = "radix")
dv_list <- dv_list[ord, , drop=FALSE]

# Write CSV dataset
csv_path <- file.path(out, "l_dv.csv")
write.csv(dv_list, csv_path, row.names = FALSE, na = "")
cat("Wrote CSV:", csv_path, " rows:", nrow(dv_list), "\n")

# --- Text rendering ---
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Listing 16.2.2"
title5 <- "Listing of Protocol Deviations"
title6 <- "All Subjects"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P078_l_dv.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

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

widths_in <- c(TRTA=0.8, SITE=0.5, USUBJID=0.8, CATEGORY=1.2, SUBCATEGORY=1.0, DESCRIPTION=2.0, START_DATE=0.7, END_DATE=0.7, EPOCH=0.8, SEVERITY=0.6)
total_in <- sum(widths_in)
total_chars <- 132
widths_chars <- round(widths_in / total_in * total_chars)
widths_chars["DESCRIPTION"] <- widths_chars["DESCRIPTION"] + (total_chars - sum(widths_chars))

pad_str <- function(s, w, align="left") {
  s <- ifelse(is.na(s), "", as.character(s))
  s <- ifelse(nchar(s, type="width") > w, substr(s, 1, w), s)
  if (align == "left") sprintf(paste0("%-", w, "s"), s)
  else if (align == "center") {
    pad <- w - nchar(s, type="width")
    left <- floor(pad/2)
    right <- pad - left
    paste0(strrep(" ", left), s, strrep(" ", right))
  } else sprintf(paste0("%", w, "s"), s)
}

header_line1 <- paste(mapply(function(nm, w) pad_str(col_labels[nm], w, "center"), names(widths_chars), widths_chars), collapse=" ")
separator_line <- paste(mapply(function(w) strrep("-", w), widths_chars), collapse=" ")

txt_path <- file.path(out, "l_dv.txt")
rtf_path <- file.path(out, "l_dv.rtf")

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

current_trta <- NA_character_
page_num <- 1
for (i in seq_len(nrow(dv_list))) {
  row <- dv_list[i, , drop=FALSE]
  if (!is.na(current_trta) && !identical(disp(row$TRTA), disp(current_trta))) {
    writeLines("", con)
    writeLines(paste0("--- Page break after Treatment: ", disp(current_trta), " ---"), con)
    writeLines("", con)
    page_num <- page_num + 1
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

# RTF wrapper containing same text
rtf_con <- file(rtf_path, open="wt")
writeLines("{\\rtf1\\ansi\\deff0", rtf_con)
writeLines("{\\fonttbl{\\f0\\fmodern Courier;}}", rtf_con)
writeLines("\\viewkind4\\uc1\\pard\\f0\\fs16", rtf_con)
txt_lines <- readLines(txt_path, warn=FALSE)
for (ln in txt_lines) {
  esc <- gsub("\\\\", "\\\\\\\\", ln)
  esc <- gsub("\\{", "\\\\{", esc)
  esc <- gsub("\\}", "\\\\}", esc)
  if (esc == "") esc <- " "
  writeLines(paste0(esc, "\\par"), rtf_con)
}
writeLines("}", rtf_con)
close(rtf_con)
cat("Wrote RTF:", rtf_path, "\n")

table_csv_path <- file.path(out, "l_dv_table.csv")
write.csv(dv_list, table_csv_path, row.names=FALSE, na="")
cat("Wrote table CSV:", table_csv_path, "\n")
cat("P078_l_dv completed successfully. Rows:", nrow(dv_list), "\n")
