# P025_ex.R - translation of SAS program to R (attempt 3)
# -------------------------------------------------
# Load required packages
library(readxl)   # reading Excel files
library(dplyr)    # data manipulation
library(stringr)  # string handling
library(lubridate) # date handling

# -------------------------------------------------
# Helper: vectorised conversion of SAS DATE11. (e.g., "01JAN2020") to ISO 8601 (YYYY-MM-DD)
sas_date11_to_iso <- function(x) {
  # Convert to character vector, trim whitespace
  x <- as.character(x)
  x <- str_trim(x)
  # Empty strings become NA
  x[x == ""] <- NA
  # Parse using day-month-abbrev-year format
  parsed <- as.Date(toupper(x), format = "%d%b%Y")
  # Fallback: try ISO format directly for any that failed
  need_iso <- is.na(parsed) & !is.na(x)
  if (any(need_iso)) {
    parsed[need_iso] <- as.Date(x[need_iso], format = "%Y-%m-%d")
  }
  # Return character ISO strings, preserving NA
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
  rename_with(~ toupper(.x)) %>%          # make names uppercase for consistency
  select(USUBJID, RFSTDTC) %>%            # keep only needed variables
  mutate(RFSTDTC = as.character(RFSTDTC)) %>% # ensure character
  arrange(USUBJID)

# -------------------------------------------------
# 2. Load raw EX data from Excel (edc_ex.xlsx)
ex_path <- file.path(rawdir, "faijal-oncology", "raw_data", "edc_ex.xlsx")
if (!file.exists(ex_path)) {
  stop("EX Excel file not found at: ", ex_path)
}
edc_ex <- read_excel(ex_path, guess_max = 10000) %>%
  rename_with(~ toupper(.x))

# Ensure VISIT column exists (SAS kept it even if empty)
if (!"VISIT" %in% names(edc_ex)) {
  edc_ex$VISIT <- NA_character_
}

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
  select(-STDT, -ENDT) %>%                # drop original raw date columns, keep VISIT
  arrange(USUBJID, EXSTDTC)

# -------------------------------------------------
# 4. Merge with DM and create SDTM EX1 dataset
sdtm_ex1 <- ex_prep %>%
  left_join(sdtm_dm, by = "USUBJID") %>%   # merge DM info (RFSTDTC)
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
    VISIT
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

# CSV of the listing body (optional but satisfies “CSV of the same table” requirement)
listing_csv_path <- file.path(out_dir, "P025_listing.csv")
write.csv(sdtm_ex, listing_csv_path, row.names = FALSE, na = "")

# End of script
