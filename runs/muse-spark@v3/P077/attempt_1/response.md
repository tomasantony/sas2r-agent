```r
# P077_l_death.sas -> R translation
# Listing 16.2.7.3 - Listing of Deaths
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tibble)
})

# SAS round half away from zero helper (not needed for integer days but kept)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# Emulate SAS binary collation (uppercase before lowercase)
old_collate <- Sys.getlocale("LC_COLLATE")
try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE)

read_sas_dataset <- function(dir, name) {
  # find file case-insensitive with .sas7bdat or .xpt
  files <- list.files(dir, full.names = TRUE)
  low <- tolower(files)
  target1 <- paste0(tolower(name), ".sas7bdat")
  target2 <- paste0(tolower(name), ".xpt")
  f <- NULL
  idx <- which(basename(low) == target1)
  if (length(idx) > 0) f <- files[idx[1]]
  else {
    idx <- which(basename(low) == target2)
    if (length(idx) > 0) f <- files[idx[1]]
  }
  if (is.null(f)) stop(paste0("Dataset not found: ", dir, "/", name))
  if (grepl("\\.xpt$", tolower(f))) {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # convert haven labelled to plain, keep Date/POSIXct
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven may return labelled; strip labels
  for (nm in names(df)) {
    if (inherits(df[[nm]], "labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  as_tibble(df)
}

adsl <- read_sas_dataset(adam_dir, "adsl")
adae <- read_sas_dataset(adam_dir, "adae")

# Ensure expected columns exist
stopifnot(all(c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","TRTEDT","DTHDT","DTHCAUS","DTHFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","AEBODSYS","AEDECOD","AEREL","AEOUT") %in% names(adae)))

# Helper: coerce SAS date-like columns to Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # single strict format: try ISO first (ADaM DTC derived dates are ISO)
    # keep NA as NA; never multi-order
    x2 <- trimws(x)
    x2[x2 == "" | x2 == "."] <- NA_character_
    return(as.Date(x2, format = "%Y-%m-%d"))
  }
  return(as.Date(x, origin = "1960-01-01"))
}

adsl$TRTSDT <- to_date(adsl$TRTSDT)
adsl$TRTEDT <- to_date(adsl$TRTEDT)
adsl$DTHDT  <- to_date(adsl$DTHDT)

# Ensure character columns are character (SAS case-sensitive comparison)
adsl$USUBJID <- as.character(adsl$USUBJID)
adsl$TRTA <- as.character(adsl$TRTA)
adsl$SEX <- as.character(adsl$SEX)
adsl$RACE <- as.character(adsl$RACE)
adsl$DTHFL <- as.character(adsl$DTHFL)
adsl$DTHCAUS <- as.character(adsl$DTHCAUS)
adae$USUBJID <- as.character(adae$USUBJID)
adae$AEBODSYS <- as.character(adae$AEBODSYS)
adae$AEDECOD <- as.character(adae$AEDECOD)
adae$AEREL <- as.character(adae$AEREL)
adae$AEOUT <- as.character(adae$AEOUT)
adsl$AGE <- as.numeric(adsl$AGE)

#--- Step 1: Identify deaths ---
deaths <- adsl %>%
  filter(!is.na(DTHFL) & DTHFL == "Y") %>%
  transmute(
    USUBJID = USUBJID,
    TRTA = TRTA,
    AGE = AGE,
    SEX = SEX,
    RACE = RACE,
    TRT_START = TRTSDT,
    TRT_END = TRTEDT,
    DEATH_DATE = DTHDT,
    CAUSE_OF_DEATH = DTHCAUS,
    DAYS_LASTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTEDT),
                                 as.numeric(DEATH_DATE - TRT_END), NA_real_),
    DAYS_FIRSTDOSE_DEATH = ifelse(!is.na(DTHDT) & !is.na(TRTSDT),
                                  as.numeric(DEATH_DATE - TRT_START) + 1, NA_real_)
  )
# SAS order by TRTA, USUBJID (missing first, binary collation)
deaths <- deaths[order(deaths$TRTA, deaths$USUBJID, na.last = FALSE), , drop = FALSE]

#--- Step 2: Add fatal AE information ---
death_ae <- adae %>%
  filter(!is.na(AEOUT) & AEOUT == "FATAL") %>%
  transmute(
    USUBJID = USUBJID,
    FATAL_AE_SOC = AEBODSYS,
    FATAL_AE_PT = AEDECOD,
    FATAL_AE_REL = AEREL
  ) %>%
  distinct()
death_ae <- death_ae[order(death_ae$USUBJID, na.last = FALSE), , drop = FALSE]

# Before merging, drop/rename overlapping columns except keys so no .x/.y
common_cols <- intersect(names(deaths), names(death_ae))
keys <- "USUBJID"
drop_cols <- setdiff(common_cols, keys)
if (length(drop_cols) > 0) {
  death_ae <- death_ae[, setdiff(names(death_ae), drop_cols), drop = FALSE]
}

death_report <- dplyr::left_join(deaths, death_ae, by = "USUBJID")
stopifnot(all(c("TRTA","USUBJID") %in% names(death_report)))
stopifnot("FATAL_AE_SOC" %in% names(death_report))
death_report <- death_report[order(death_report$TRTA, death_report$USUBJID,
                                   death_report$FATAL_AE_SOC, death_report$FATAL_AE_PT,
                                   na.last = FALSE), , drop = FALSE]

#--- Step 3: Format display ---
# SAS: demog = catx('/', strip(put(AGE,best.)), strip(SEX), strip(substr(RACE,1,1)))
sas_best <- function(x) {
  if (is.na(x)) return(NA_character_)
  # SAS BEST. default: minimal representation
  # ages are integers; emulate with format without scientific
  s <- format(x, scientific = FALSE, trim = TRUE, nsmall = 0)
  trimws(s)
}
catx_slash <- function(...) {
  parts <- list(...)
  # CATX skips missing (NA or "" or ".")
  clean <- lapply(parts, function(p) {
    if (is.na(p)) return(NULL)
    p <- trimws(as.character(p))
    if (p == "" || p == ".") return(NULL)
    p
  })
  clean <- Filter(Negate(is.null), clean)
  if (length(clean) == 0) return(NA_character_)
  paste(unlist(clean), collapse = "/")
}

death_final <- death_report
death_final$demog <- NA_character_
death_final$days_display <- NA_character_
for (i in seq_len(nrow(death_final))) {
  age_s <- sas_best(death_final$AGE[i])
  sex_s <- ifelse(is.na(death_final$SEX[i]), NA_character_, trimws(as.character(death_final$SEX[i])))
  race1 <- ifelse(is.na(death_final$RACE[i]), NA_character_,
                  substr(trimws(as.character(death_final$RACE[i])), 1, 1))
  death_final$demog[i] <- catx_slash(age_s, sex_s, race1)
  d <- death_final$DAYS_LASTDOSE_DEATH[i]
  if (!is.na(d)) {
    # strip(put(d,8.)) : SAS 8. rounds half away; d is integer
    d2 <- round_half_up(d, 0)
    death_final$days_display[i] <- trimws(as.character(as.integer(d2)))
  } else {
    death_final$days_display[i] <- "N/A"
  }
}

# Select report columns in PROC REPORT order
death_final <- death_final %>%
  select(TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE,
         days_display, CAUSE_OF_DEATH, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL,
         DAYS_LASTDOSE_DEATH, DAYS_FIRSTDOSE_DEATH, AGE, SEX, RACE)

# Order for display (already ordered)
death_final <- death_final[order(death_final$TRTA, death_final$USUBJID,
                                 death_final$FATAL_AE_SOC, death_final$FATAL_AE_PT,
                                 na.last = FALSE), , drop = FALSE]

# DATE9. formatter for display
fmt_date9 <- function(x) {
  if (is.na(x)) return("")
  # x is Date
  s <- format(x, "%d%b%Y")
  toupper(s)
}
fmt_iso <- function(x) {
  if (is.na(x)) return(NA_character_)
  format(x, "%Y-%m-%d")
}

#--- Step 4: Generate listing outputs ---
# Titles / footnotes verbatim (u_titles.sas, output=L_DEATH)
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
run_time <- format(Sys.time(), "%H:%M")
title1 <- paste0(sponsor, "  Page 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- " "
title4 <- "Listing 16.2.7.3"
title5 <- "Listing of Deaths"
title6 <- "All Subjects"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: P077_l_death.sas | Run: ", run_date, " ", run_time)

# Build display data.frame for TXT (SAS formatted numbers)
n <- nrow(death_final)
disp <- data.frame(
  Treatment = as.character(death_final$TRTA),
  SubjectID = as.character(death_final$USUBJID),
  AgeSexRace = as.character(death_final$demog),
  FirstDose = sapply(death_final$TRT_START, fmt_date9),
  LastDose = sapply(death_final$TRT_END, fmt_date9),
  DeathDate = sapply(death_final$DEATH_DATE, fmt_date9),
  DaysLastDoseToDeath = as.character(death_final$days_display),
  CauseOfDeath = ifelse(is.na(death_final$CAUSE_OF_DEATH), "", as.character(death_final$CAUSE_OF_DEATH)),
  FatalAESOC = ifelse(is.na(death_final$FATAL_AE_SOC), "", as.character(death_final$FATAL_AE_SOC)),
  FatalAEPT = ifelse(is.na(death_final$FATAL_AE_PT), "", as.character(death_final$FATAL_AE_PT)),
  Relationship = ifelse(is.na(death_final$FATAL_AE_REL), "", as.character(death_final$FATAL_AE_REL)),
  stringsAsFactors = FALSE
)

# CSV of table body: ISO dates, same columns as report
csv_df <- data.frame(
  TRTA = as.character(death_final$TRTA),
  USUBJID = as.character(death_final$USUBJID),
  demog = as.character(death_final$demog),
  TRT_START = sapply(death_final$TRT_START, function(x) if (is.na(x)) NA_character_ else fmt_iso(x)),
  TRT_END = sapply(death_final$TRT_END, function(x) if (is.na(x)) NA_character_ else fmt_iso(x)),
  DEATH_DATE = sapply(death_final$DEATH_DATE, function(x) if (is.na(x)) NA_character_ else fmt_iso(x)),
  days_display = as.character(death_final$days_display),
  CAUSE_OF_DEATH = as.character(death_final$CAUSE_OF_DEATH),
  FATAL_AE_SOC = as.character(death_final$FATAL_AE_SOC),
  FATAL_AE_PT = as.character(death_final$FATAL_AE_PT),
  FATAL_AE_REL = as.character(death_final$FATAL_AE_REL),
  stringsAsFactors = FALSE
)
# Ensure empty strings for NA character? Keep NA as "" per write.csv na=""
write.csv(csv_df, file.path(out, "l_death.csv"), row.names = FALSE, na = "")

# Plain-text rendering
txt_file <- file.path(out, "l_death.txt")
con <- file(txt_file, open = "wt")
writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
# Header row (split='~' replaced with space)
hdr <- c("Treatment", "Subject ID", "Age/Sex/ Race", "First Dose", "Last Dose",
         "Death Date", "Days Last Dose to Death", "Cause of Death",
         "Fatal AE SOC", "Fatal AE PT", "Relationship")
# Use fixed separator " | " for readability
writeLines(paste(hdr, collapse = " | "), con)
writeLines(paste(rep("-", 200), collapse = ""), con)
if (n > 0) {
  for (i in seq_len(n)) {
    rowvals <- c(disp$Treatment[i], disp$SubjectID[i], disp$AgeSexRace[i],
                 disp$FirstDose[i], disp$LastDose[i], disp$DeathDate[i],
                 disp$DaysLastDoseToDeath[i], disp$CauseOfDeath[i],
                 disp$FatalAESOC[i], disp$FatalAEPT[i], disp$Relationship[i])
    # Replace NA with ""
    rowvals[is.na(rowvals)] <- ""
    writeLines(paste(rowvals, collapse = " | "), con)
  }
} else {
  writeLines("No deaths reported.", con)
}
writeLines("", con)
writeLines(foot1, con)
writeLines(foot2, con)
writeLines(foot3, con)
close(con)

try(Sys.setlocale("LC_COLLATE", old_collate), silent = TRUE)
```