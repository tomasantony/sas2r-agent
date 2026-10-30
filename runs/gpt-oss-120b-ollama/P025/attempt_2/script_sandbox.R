# P025_ex.R - translation of SAS program to R
# -------------------------------------------------
# Load required packages
library(readxl)   # reading Excel files
library(dplyr)    # data manipulation
library(stringr)  # string handling
library(lubridate) # date handling

# -------------------------------------------------
# Helper: convert SAS DATE11. (e.g., "01JAN2020") to ISO 8601 string
sas_date11_to_iso <- function(x) {
  # Vectorised conversion; returns NA_character_ for invalid values
  if (is.na(x)) return(NA_character_)
  # Ensure character
  x <- as.character(x)
  # Trim whitespace
  x <- str_trim(x)
  # Empty strings become NA
  x[x == ""] <- NA
  # Parse using dmy with month abbreviation
  parsed <- as.Date(toupper(x), format = "%d%b%Y")
  # If parsing fails, try ISO directly
  parsed[is.na(parsed)] <- as.Date(x[is.na(parsed)], format = "%Y-%m-%d")
  # Return formatted ISO strings or NA
  ifelse(is.na(parsed), NA_character_, format(parsed, "%Y-%m-%d"))
}

# -------------------------------------------------
# Set directories (macro equivalents)
rawdir   <- "/bench/data/raw"
rawsdtm  <- "/bench/data/raw/cdisc-pilot/sdtm"
out_dir  <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -------------------------------------------------
# 1. Load SDTM DM dataset (assumed XPT file named dm.xpt)
dm_path <- file.path(rawsdtm, "dm.xpt")
if (!file.exists(dm_path)) {
  stop("DM XPT file not found at: ", dm_path)
}
sdtm_dm <- haven::read_xpt(dm_path) %>%
  # Keep only needed variables, case‑insensitive handling
  rename_with(~ toupper(.x)) %>%
  select(USUBJID, RFSTDTC) %>%
  # Ensure RFSTDTC is character ISO (if already ISO, keep as is)
  mutate(RFSTDTC = as.character(RFSTDTC)) %>%
  arrange(USUBJID)

# -------------------------------------------------
# 2. Load raw EX data from Excel (edc_ex.xlsx)
ex_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_ex.xlsx")
if (!file.exists(ex_path)) {
  stop("EX Excel file not found at: ", ex_path)
}
edc_ex <- read_excel(ex_path, guess_max = 10000) %>%
  rename_with(~ toupper(.x))

# -------------------------------------------------
# 3. Prepare Raw EX Data (equivalent to DATA step ex_prep)
ex_prep <- edc_ex %>%
  mutate(
    USUBJID = paste0("ONCO-2026-", SUBJID),
    DOSE = as.numeric(DOSE)               # ensure numeric for missing check
  ) %>%
  filter(!is.na(DOSE)) %>%                # exclude records where dose not administered
  mutate(
    EXSTDTC = sas_date11_to_iso(STDT),
    EXENDTC = sas_date11_to_iso(ENDT)
  ) %>%
  select(-STDT, -ENDT) %>%                # drop original raw date columns
  arrange(USUBJID, EXSTDTC)

# -------------------------------------------------
# 4. Merge with DM to obtain RFSTDTC and create SDTM EX1 dataset
sdtm_ex1 <- ex_prep %>%
  left_join(sdtm_dm, by = "USUBJID") %>%   # merge DM info
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "EX",
    EXTRT   = toupper(TRT),
    EXDOSE  = DOSE,
    EXDOSU  = toupper(UNIT),
    EXDOSFRM = str_replace_all(FORM, "_", " "),
    EXDOSFRQ = toupper(FREQ),
    EXVLDIST = toupper(ROUTE),
    EXLOT   = LOT
  ) %>%
  # Derive EXSTDY
  mutate(
    EXSTDY = if_else(
      !is.na(EXSTDTC) & !is.na(RFSTDTC),
      {
        stdt <- as.Date(EXSTDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(stdt - rfdt)
        ifelse(diff >= 0, diff + 1, diff)
      },
      NA_real_
    ),
    # Derive EXENDY
    EXENDY = if_else(
      !is.na(EXENDTC) & !is.na(RFSTDTC),
      {
        endt <- as.Date(EXENDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(endt - rfdt)
        ifelse(diff >= 0, diff + 1, diff)
      },
      NA_real_
    )
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU,
    EXDOSFRM, EXDOSFRQ, EXVLDIST, EXLOT,
    EXSTDTC, EXENDTC, EXSTDY, EXENDY,
    VISIT = VISIT   # keep VISIT if present; otherwise NA
  ) %>%
  arrange(USUBJID, EXSTDTC)

# -------------------------------------------------
# 5. Derive EXSEQ (sequence number within each subject)
sdtm_ex <- sdtm_ex1 %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU,
    EXDOSFRM, EXDOSFRQ, EXVLDIST, EXLOT,
    EXSTDTC, EXENDTC, EXSTDY, EXENDY, VISIT
  )

# -------------------------------------------------
# 6. Output results --------------------------------------------------------------

# CSV version of final SDTM EX dataset
csv_path <- file.path(out_dir, "P025.csv")
write.csv(sdtm_ex, csv_path, row.names = FALSE, na = "")

# Text listing mimicking PROC PRINT output (named .lst as per SAS reference)
txt_path <- file.path(out_dir, "P025.lst")
txt_lines <- capture.output(print(sdtm_ex, row.names = FALSE))
writeLines(txt_lines, txt_path)

# Also write a CSV of the listing body for completeness (optional)
listing_csv_path <- file.path(out_dir, "P025_listing.csv")
write.csv(sdtm_ex, listing_csv_path, row.names = FALSE, na = "")

# End of script
