# P077_l_death.sas -> R translation
# Listing 16.2.7.3 - Listing of Deaths

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# helper: read SAS dataset (sas7bdat or xpt) case-insensitive
read_sas_dataset <- function(dir, name) {
  # name without extension, e.g. "adsl"
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match case-insensitive
  idx <- which(tolower(basename(tools::file_path_sans_ext(files))) == tolower(name))
  if (length(idx) == 0) {
    stop(paste0("Dataset not found: ", name, " in ", dir))
  }
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    # try both readers
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <- haven::read_xpt(f))
  }
  # convert haven_labelled to plain
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  df <- haven::zap_label(df)
  # convert to data.frame for consistency
  as.data.frame(df, stringsAsFactors = FALSE)
}

# helper: SAS date conversion if numeric SAS date (days since 1960-01-01)
to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: assume days since 1960-01-01 if plausible
    # haven already converts formatted dates to Date, so numeric here means unformatted
    # Heuristic: if all non-NA values are between 0 and 30000 (~1960-2042) treat as SAS date
    # Otherwise return as is
    # But for ADSL TRTSDT etc, they are SAS dates
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character: try ISO first, then date9
  if (is.character(x)) {
    # try YYYY-MM-DD
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (all(is.na(d) | !is.na(d))) {
      # if at least some parsed, use it; otherwise try date9
      if (any(!is.na(d))) return(d)
    }
    d2 <- suppressWarnings(as.Date(x, format = "%d%b%Y"))
    # upper case month handling
    if (any(!is.na(d2))) return(d2)
    return(d)
  }
  x
}

# helper: format Date as SAS DATE9. e.g. 01JAN2020
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}
# helper: format Date as ISO YYYY-MM-DD for CSV
fmt_iso <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}
# helper: SAS BEST. for AGE - strip
fmt_best <- function(x) {
  ifelse(is.na(x), "", trimws(format(x, trim = TRUE, scientific = FALSE, nsmall = 0)))
}
# helper: SAS 8. for days
fmt_8 <- function(x) {
  ifelse(is.na(x), "", trimws(formatC(x, format = "f", digits = 0, width = 8, flag = " ")))
}

#--- Load ADaM ---
adsl <- read_sas_dataset(adam_path, "adsl")
adae <- read_sas_dataset(adam_path, "adae")

# Ensure required columns exist
# ADSL expected: USUBJID, TRTA, AGE, SEX, RACE, TRTSDT, TRTEDT, DTHDT, DTHCAUS, DTHFL
# ADAE expected: USUBJID, AEBODSYS, AEDECOD, AEREL, AEOUT
# Normalize column names to uppercase for case-insensitive matching
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

stopifnot("USUBJID" %in% names(adsl))
stopifnot("DTHFL" %in% names(adsl))
stopifnot("USUBJID" %in% names(adae))

# Convert date columns to Date
for (col in c("TRTSDT","TRTEDT","DTHDT")) {
  if (col %in% names(adsl)) adsl[[col]] <- to_Date(adsl[[col]])
}
# ADAE dates not needed

#--- Step 1: Identify deaths ---
# SAS: where a.DTHFL = 'Y' ; compute days
deaths <- adsl %>%
  filter(DTHFL == "Y") %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTA = as.character(TRTA),
    AGE = as.numeric(AGE),
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    TRT_START = TRTSDT,
    TRT_END = TRTEDT,
    DEATH_DATE = DTHDT,
    CAUSE_OF_DEATH = as.character(DTHCAUS),
    DAYS_LASTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTEDT), as.numeric(DTHDT - TRTEDT), NA_real_),
    DAYS_FIRSTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTSDT), as.numeric(DTHDT - TRTSDT + 1), NA_real_)
  ) %>%
  arrange(TRTA, USUBJID)

#--- Step 2: Add fatal AE information ---
death_ae <- adae %>%
  filter(AEOUT == "FATAL") %>%
  transmute(
    USUBJID = as.character(USUBJID),
    FATAL_AE_SOC = as.character(AEBODSYS),
    FATAL_AE_PT = as.character(AEDECOD),
    FATAL_AE_REL = as.character(AEREL)
  ) %>%
  distinct(USUBJID, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL) %>%
  arrange(USUBJID)

# Left join (drop duplicate columns except keys)
death_report <- deaths %>%
  left_join(death_ae, by = "USUBJID") %>%
  arrange(TRTA, USUBJID)

#--- Step 3: Format display ---
death_final <- death_report %>%
  mutate(
    demog = paste(
      trimws(fmt_best(AGE)),
      trimws(SEX),
      trimws(substr(RACE, 1, 1)),
      sep = "/"
    ),
    # handle missing AGE/SEX/RACE: catx skips missing? SAS catx skips empty? It concatenates non-missing with delimiter.
    # Replicate: if any component is NA/empty, catx will skip it. Our paste will produce "NA/..." if NA.
    # So clean:
    demog = {
      # rebuild with catx logic
      sapply(seq_len(n()), function(i) {
        parts <- c(
          ifelse(is.na(AGE[i]) || trimws(fmt_best(AGE[i]))=="", NA, trimws(fmt_best(AGE[i]))),
          ifelse(is.na(SEX[i]) || trimws(SEX[i])=="", NA, trimws(SEX[i])),
          ifelse(is.na(RACE[i]) || trimws(substr(RACE[i],1,1))=="", NA, trimws(substr(RACE[i],1,1)))
        )
        parts <- parts[!is.na(parts)]
        paste(parts, collapse = "/")
      })
    },
    days_display = ifelse(!is.na(DAYS_LASTDOSE_DEATH), trimws(fmt_8(DAYS_LASTDOSE_DEATH)), "N/A")
  )

# Ensure ordering
death_final <- death_final %>% arrange(TRTA, USUBJID)

#--- Prepare CSV output (ISO dates, numeric unrounded) ---
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
    FATAL_AE_REL = FATAL_AE_REL,
    # also include numeric days for traceability (optional but keep SAS variables)
    DAYS_LASTDOSE_DEATH = DAYS_LASTDOSE_DEATH,
    DAYS_FIRSTDOSE_DEATH = DAYS_FIRSTDOSE_DEATH,
    AGE = AGE,
    SEX = SEX,
    RACE = RACE
  )

# Write CSV
write.csv(csv_out, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

#--- Step 4: Generate listing text rendering ---
# Titles/footnotes from u_titles.sas for L_DEATH
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

title1 <- paste0(sponsor, "                                                  Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Listing 16.2.7.3"
title5 <- "Listing of Deaths"
title6 <- "All Subjects"
footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P077_l_death.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))

# Build display table for TXT (SAS DATE9. display)
txt_table <- death_final %>%
  transmute(
    `Treatment` = TRTA,
    `Subject ID` = USUBJID,
    `Age/Sex/Race` = demog,
    `First Dose` = fmt_date9(TRT_START),
    `Last Dose` = fmt_date9(TRT_END),
    `Death Date` = fmt_date9(DEATH_DATE),
    `Days Last Dose to Death` = days_display,
    `Cause of Death` = CAUSE_OF_DEATH,
    `Fatal AE SOC` = FATAL_AE_SOC,
    `Fatal AE PT` = FATAL_AE_PT,
    `Relationship` = FATAL_AE_REL
  )

# Create text rendering
txt_file <- file.path(out, "l_death.txt")
rtf_file <- file.path(out, "l_death.rtf")

# Helper to format table as fixed-width text
col_names <- names(txt_table)
# Determine column widths (approx SAS widths in inches -> chars)
# Use max of header and data length + 2
col_widths <- sapply(seq_along(col_names), function(i) {
  max(nchar(col_names[i], type="width"), max(nchar(as.character(txt_table[[i]]), type="width"), na.rm=TRUE), na.rm=TRUE) + 2
})
# Cap widths to reasonable
col_widths <- pmin(col_widths, 30)

# Function to pad
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), substr(ifelse(is.na(x), "", x), 1, w))
pad_center <- function(x, w) {
  x <- ifelse(is.na(x), "", x)
  x <- substr(x, 1, w)
  pad <- w - nchar(x, type="width")
  left <- floor(pad/2)
  right <- pad - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}

header_line <- paste(mapply(pad_right, col_names, col_widths), collapse = "|")
sep_line <- paste(mapply(function(w) strrep("-", w), col_widths), collapse = "+")

con <- file(txt_file, open = "wt")
writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(header_line, con)
writeLines(sep_line, con)
if (nrow(txt_table) > 0) {
  for (r in seq_len(nrow(txt_table))) {
    row_vals <- as.character(txt_table[r, ])
    # center days column (7th)
    line_parts <- mapply(function(val, w, idx) {
      if (idx == 7) pad_center(val, w) else pad_right(val, w)
    }, row_vals, col_widths, seq_along(row_vals))
    writeLines(paste(line_parts, collapse = "|"), con)
  }
} else {
  writeLines("No deaths reported.", con)
}
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
# Add summary counts for verification (46 numbers reference)
writeLines("", con)
writeLines(paste0("Number of deaths: ", nrow(death_final)), con)
if (nrow(death_final) > 0) {
  # counts by treatment
  cnt_by_trt <- death_final %>% count(TRTA)
  for (i in seq_len(nrow(cnt_by_trt))) {
    writeLines(paste0("  ", cnt_by_trt$TRTA[i], ": ", cnt_by_trt$n[i]), con)
  }
}
close(con)

# Also write RTF as plain text copy (SAS ODS RTF placeholder)
# Create minimal RTF with same content
rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Courier New;}}\n\\viewkind4\\uc1\\pard\\f0\\fs16\n"
rtf_footer <- "}"
txt_content <- readLines(txt_file, warn = FALSE)
# Escape RTF special chars
rtf_escape <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\\{", "\\\\{", s)
  s <- gsub("\\}", "\\\\}", s)
  s
}
rtf_body <- paste(sapply(txt_content, function(l) paste0(rtf_escape(l), "\\par\n")), collapse = "")
writeLines(paste0(rtf_header, rtf_body, rtf_footer), rtf_file)

# Also write CSV of table body for verification (same as l_death.csv but with display dates)
write.csv(txt_table, file.path(out, "l_death_display.csv"), row.names = FALSE, na = "")

# Clean up work datasets (R equivalent)
rm(deaths, death_ae, death_report)

cat("P077 l_death completed successfully. Rows:", nrow(death_final), "\n")
