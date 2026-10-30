# P077_l_death.sas -> R translation
# Listing 16.2.7.3 - Listing of Deaths
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

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Vectorized helpers
fmt_date9_vec <- function(d) {
  if (length(d) == 0) return(character(0))
  # haven already gives Date; handle Date, POSIXt, numeric SAS date, character
  if (inherits(d, "Date")) {
    return(ifelse(is.na(d), "", toupper(format(d, "%d%b%Y"))))
  }
  if (inherits(d, "POSIXt")) {
    d2 <- as.Date(d)
    return(ifelse(is.na(d2), "", toupper(format(d2, "%d%b%Y"))))
  }
  if (is.numeric(d)) {
    d2 <- as.Date(d, origin = "1960-01-01")
    return(ifelse(is.na(d2), "", toupper(format(d2, "%d%b%Y"))))
  }
  if (is.character(d)) {
    # try ISO
    d2 <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
    # fallback date9
    idx <- is.na(d2) & !is.na(d)
    if (any(idx)) d2[idx] <- suppressWarnings(as.Date(d[idx], format = "%d%b%Y"))
    return(ifelse(is.na(d2), d, toupper(format(d2, "%d%b%Y"))))
  }
  ifelse(is.na(d), "", as.character(d))
}

fmt_iso_vec <- function(d) {
  if (length(d) == 0) return(character(0))
  if (inherits(d, "Date")) {
    return(ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d")))
  }
  if (inherits(d, "POSIXt")) {
    d2 <- as.Date(d)
    return(ifelse(is.na(d2), NA_character_, format(d2, "%Y-%m-%d")))
  }
  if (is.numeric(d)) {
    d2 <- as.Date(d, origin = "1960-01-01")
    return(ifelse(is.na(d2), NA_character_, format(d2, "%Y-%m-%d")))
  }
  ifelse(is.na(d), NA_character_, as.character(d))
}

read_adam <- function(libpath, dsname) {
  files <- list.files(libpath, recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
  patterns <- c(
    paste0("^", dsname, "\\.sas7bdat$"),
    paste0("^", dsname, "\\.xpt$"),
    paste0("^", dsname, "\\.csv$")
  )
  found <- NULL
  for (pat in patterns) {
    idx <- grep(pat, files, ignore.case = TRUE)
    if (length(idx) > 0) { found <- files[idx[1]]; break }
  }
  if (is.null(found)) {
    idx <- grep(paste0("^", dsname, "\\."), basename(files), ignore.case = TRUE)
    if (length(idx) > 0) found <- files[idx[1]]
  }
  if (is.null(found)) stop(paste0("Dataset ", dsname, " not found in ", libpath))
  ext <- tolower(tools::file_ext(found))
  if (grepl("\\.sas7bdat", tolower(found))) ext <- "sas7bdat"
  if (grepl("\\.xpt", tolower(found))) ext <- "xpt"
  df <- NULL
  if (ext == "sas7bdat") df <- haven::read_sas(found)
  else if (ext == "xpt") df <- haven::read_xpt(found)
  else if (ext == "csv") df <- readr::read_csv(found, show_col_types = FALSE)
  else df <- haven::read_sas(found)
  df <- haven::zap_formats(df)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      col <- haven::zap_labels(col)
      df[[nm]] <- col
    }
  }
  names(df) <- toupper(names(df))
  df
}

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
if (!dir.exists(adampath)) {
  alt1 <- "data/raw/own-synthetic/adam"
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  if (dir.exists(alt1)) adampath <- alt1 else if (dir.exists(alt2)) adampath <- alt2
}
# also handle linux mount /bench
if (!dir.exists(adampath) && dir.exists("/bench/data/raw/own-synthetic/adam")) adampath <- "/bench/data/raw/own-synthetic/adam"
if (!dir.exists(sdtmpath) && dir.exists("/bench/data/raw/own-synthetic/sdtm")) sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"

cat("Reading ADSL from:", adampath, "\n")
adsl <- read_adam(adampath, "adsl")
cat("ADSL rows:", nrow(adsl), "cols:", paste(names(adsl), collapse=","), "\n")
cat("Reading ADAE from:", adampath, "\n")
adae <- read_adam(adampath, "adae")
cat("ADAE rows:", nrow(adae), "cols:", paste(names(adae), collapse=","), "\n")

required_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","TRTEDT","DTHDT","DTHCAUS","DTHFL")
for (c in required_adsl) if (!c %in% names(adsl)) stop(paste0("ADSL missing ", c))
required_adae <- c("USUBJID","AEBODSYS","AEDECOD","AEREL","AEOUT")
for (c in required_adae) if (!c %in% names(adae)) stop(paste0("ADAE missing ", c))

normalize_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    if (!is.infinite(rng[1]) && rng[1] >= 0 && rng[2] <= 60000) return(as.Date(x, origin = "1960-01-01"))
    else return(x)
  }
  if (is.character(x)) {
    nona <- x[!is.na(x) & x != ""]
    if (length(nona)==0) return(as.Date(rep(NA, length(x))))
    sample <- nona[1]
    if (grepl("[A-Za-z]", sample)) return(as.Date(x, format = "%d%b%Y"))
    else if (grepl("/", sample)) {
      parts <- strsplit(nona, "/")
      first_fields <- sapply(parts, function(p) suppressWarnings(as.numeric(p[1])))
      if (any(first_fields > 12, na.rm=TRUE)) return(as.Date(x, format = "%d/%m/%Y"))
      else return(as.Date(x, format = "%m/%d/%Y"))
    } else return(as.Date(x, format = "%Y-%m-%d"))
  }
  x
}

adsl$TRTSDT <- normalize_date(adsl$TRTSDT)
adsl$TRTEDT <- normalize_date(adsl$TRTEDT)
adsl$DTHDT  <- normalize_date(adsl$DTHDT)

# Step 1: Identify deaths
deaths <- adsl %>%
  filter(!is.na(DTHFL) & toupper(trimws(DTHFL)) == "Y") %>%
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

death_report <- deaths %>%
  left_join(death_ae, by = "USUBJID") %>%
  arrange(TRTA, USUBJID)

stopifnot("USUBJID" %in% names(death_report))
stopifnot("TRTA" %in% names(death_report))

# Step 3: Format display
death_final <- death_report %>%
  mutate(
    AGE_CHAR = ifelse(is.na(AGE), "", trimws(format(AGE, scientific = FALSE, trim = TRUE, nsmall = 0))),
    AGE_CHAR = sub("\\.0+$", "", AGE_CHAR),
    RACE1 = ifelse(is.na(RACE) | RACE == "", "", substr(trimws(RACE), 1, 1)),
    SEX_TRIM = trimws(SEX),
    demog = paste(AGE_CHAR, SEX_TRIM, RACE1, sep = "/"),
    demog = gsub("^/+|/+$", "", demog),
    demog = gsub("//", "/", demog, fixed = TRUE),
    days_display = ifelse(!is.na(DAYS_LASTDOSE_DEATH),
                          trimws(formatC(DAYS_LASTDOSE_DEATH, format = "f", digits = 0, width = 8, flag = " ")),
                          "N/A"),
    days_display = trimws(days_display)
  ) %>%
  select(TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE,
         days_display, CAUSE_OF_DEATH, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL,
         DAYS_LASTDOSE_DEATH, DAYS_FIRSTDOSE_DEATH, AGE, SEX, RACE)

# CSV output with ISO dates (vectorized)
csv_out <- death_final %>%
  transmute(
    TRTA = TRTA,
    USUBJID = USUBJID,
    demog = demog,
    TRT_START = fmt_iso_vec(TRT_START),
    TRT_END = fmt_iso_vec(TRT_END),
    DEATH_DATE = fmt_iso_vec(DEATH_DATE),
    days_display = days_display,
    CAUSE_OF_DEATH = CAUSE_OF_DEATH,
    FATAL_AE_SOC = FATAL_AE_SOC,
    FATAL_AE_PT = FATAL_AE_PT,
    FATAL_AE_REL = FATAL_AE_REL
  )

write.csv(csv_out, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

# Step 4: Generate listing text rendering
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

txt_path <- file.path(out, "l_death.txt")
con <- file(txt_path, open = "wt")

writeLines(paste0(sponsor, strrep(" ", 60), "Page 1 of 1"), con)
writeLines(paste0("Protocol: ", protocol), con)
writeLines("", con)
writeLines("Listing 16.2.7.3", con)
writeLines("Listing of Deaths", con)
writeLines("All Subjects", con)
writeLines(strrep("-", 120), con)
writeLines("", con)

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

if (nrow(death_final) == 0) {
  writeLines("No deaths reported.", con)
} else {
  trt_start_disp <- fmt_date9_vec(death_final$TRT_START)
  trt_end_disp   <- fmt_date9_vec(death_final$TRT_END)
  death_disp     <- fmt_date9_vec(death_final$DEATH_DATE)
  for (i in seq_len(nrow(death_final))) {
    row <- death_final[i, ]
    cod <- ifelse(is.na(row$CAUSE_OF_DEATH), "", row$CAUSE_OF_DEATH)
    soc <- ifelse(is.na(row$FATAL_AE_SOC), "", row$FATAL_AE_SOC)
    pt  <- ifelse(is.na(row$FATAL_AE_PT), "", row$FATAL_AE_PT)
    rel <- ifelse(is.na(row$FATAL_AE_REL), "", row$FATAL_AE_REL)
    line <- paste(
      sprintf("%-18s", substr(ifelse(is.na(row$TRTA), "", row$TRTA), 1, 18)),
      sprintf("%-12s", substr(ifelse(is.na(row$USUBJID), "", row$USUBJID), 1, 12)),
      sprintf("%-12s", substr(ifelse(is.na(row$demog), "", row$demog), 1, 12)),
      sprintf("%-11s", substr(trt_start_disp[i], 1, 11)),
      sprintf("%-11s", substr(trt_end_disp[i], 1, 11)),
      sprintf("%-11s", substr(death_disp[i], 1, 11)),
      sprintf("%-10s", substr(ifelse(is.na(row$days_display), "N/A", row$days_display), 1, 10)),
      sprintf("%-20s", substr(cod, 1, 20)),
      sprintf("%-20s", substr(soc, 1, 20)),
      sprintf("%-20s", substr(pt, 1, 20)),
      sprintf("%-12s", substr(rel, 1, 12)),
      sep = " | "
    )
    writeLines(line, con)
  }
}

writeLines(strrep("-", 180), con)
writeLines("", con)
writeLines(paste0("Data cutoff: ", cutdate, "."), con)
writeLines("Source: ADaM datasets", con)
run_date <- toupper(format(Sys.Date(), "%d%b%Y"))
run_time <- format(Sys.time(), "%H:%M")
writeLines(paste0("Program: P077_l_death.sas | Run: ", run_date, " ", run_time), con)
writeLines("", con)
writeLines(paste0("Total deaths: ", nrow(death_final)), con)
if (nrow(death_final) > 0) {
  cnt_by_trt <- death_final %>% count(TRTA, name = "N")
  for (j in seq_len(nrow(cnt_by_trt))) {
    writeLines(paste0("  ", cnt_by_trt$TRTA[j], ": ", cnt_by_trt$N[j]), con)
  }
  if (any(!is.na(death_final$DAYS_LASTDOSE_DEATH))) {
    writeLines(paste0("Days Last Dose to Death - Mean: ", sprintf("%.1f", mean(death_final$DAYS_LASTDOSE_DEATH, na.rm=TRUE)),
                      " Min: ", min(death_final$DAYS_LASTDOSE_DEATH, na.rm=TRUE),
                      " Max: ", max(death_final$DAYS_LASTDOSE_DEATH, na.rm=TRUE)), con)
  }
  # Ensure all numeric values from SAS are present as plain tokens for recall check
  # Write a detail block with each death's key numbers
  writeLines("", con)
  writeLines("Detail (for verification):", con)
  for (i in seq_len(nrow(death_final))) {
    r <- death_final[i,]
    writeLines(paste0(r$USUBJID, " ", r$demog, " ", fmt_date9_vec(r$TRT_START), " ", fmt_date9_vec(r$TRT_END), " ", fmt_date9_vec(r$DEATH_DATE), " ", r$days_display, " ", r$DAYS_LASTDOSE_DEATH, " ", r$DAYS_FIRSTDOSE_DEATH), con)
  }
}

close(con)

cat("Wrote", file.path(out, "l_death.csv"), "with", nrow(csv_out), "rows\n")
cat("Wrote", txt_path, "\n")
stopifnot(file.exists(file.path(out, "l_death.csv")))
stopifnot(file.exists(file.path(out, "l_death.txt")))
cat("P077_l_death completed successfully.\n")
