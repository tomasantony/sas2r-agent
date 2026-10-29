```r
# ------------------------------------------------------------
# R translation of SAS program P026_lb.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# Helper: SAS style missing handling (empty string in CSV)
na_to_empty <- function(x) {
  if (is.character(x)) {
    x[is.na(x)] <- ""
  } else if (is.numeric(x)) {
    # keep NA as NA; write.csv will turn them into empty strings via na = ""
    x
  }
  x
}

# ------------------------------------------------------------------
# 1. Set directories and output location
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# Paths used in the original SAS macro variables
raw_dir   <- "D:/meta_hackathon/sas2r-agent/data/raw"
raw_sdtm  <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

# ------------------------------------------------------------------
# 2. Load source datasets
# ------------------------------------------------------------------
# edc_lb is supplied as an Excel file in the WORK pre‑load list
edc_lb_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_lb.xlsx"
edc_lb <- read_excel(edc_lb_path, guess_max = 10000) %>% 
  mutate(across(everything(), as.character))

# sdtm_dm – locate the first XPT or SAS7BDAT file that contains "dm"
dm_file_candidates <- list.files(raw_sdtm,
                                 pattern = "(?i)dm\\.(xpt|sas7bdat)$",
                                 full.names = TRUE)
if (length(dm_file_candidates) == 0) stop("DM dataset not found in ", raw_sdtm)
dm_path <- dm_file_candidates[1]

if (grepl("\\.xpt$", dm_path, ignore.case = TRUE)) {
  sdtm_dm <- read_xpt(dm_path)
} else {
  sdtm_dm <- read_sas(dm_path)
}
sdtm_dm <- as_tibble(sdtm_dm) %>% 
  mutate(across(everything(), as.character))

# ------------------------------------------------------------------
# 3. Prepare DM reference data (keep USUBJID and RFSTDTC)
# ------------------------------------------------------------------
dm_for_lb <- sdtm_dm %>%
  select(USUBJID, RFSTDTC) %>%
  arrange(USUBJID)

# ------------------------------------------------------------------
# 4. Prepare Raw LB data with USUBJID
# ------------------------------------------------------------------
lb_prep <- edc_lb %>%
  mutate(USUBJID = str_c("ONCO-2026", "-", SUBJID)) %>%
  select(everything(), USUBJID) %>%      # keep original order plus new var
  arrange(USUBJID)

# ------------------------------------------------------------------
# 5. Merge LB with DM reference (left join, keep all LB rows)
# ------------------------------------------------------------------
sdtm_lb1 <- lb_prep %>%
  left_join(dm_for_lb, by = "USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "LB",
    LBTESTCD = TESTCD,
    LBCAT    = CAT,
    # Map Test Names
    LBTEST = case_when(
      LBTESTCD == "HGB" ~ "Hemoglobin",
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase",
      TRUE               ~ NA_character_
    ),
    LBORRES  = ORRES,
    LBORRESU = UNIT,
    LBSTRESC = ORRES,
    LBSTRESN = as.numeric(ORRES),
    LBSTRESU = UNIT,
    LBORNRLO = LOW,
    LBORNRHI = HIGH,
    VISIT = str_replace_all(VISIT, "_", " "),
    # Reference Range Indicator
    LBNRIND = case_when(
      !is.na(LBSTRESN) & LBSTRESN < as.numeric(LOW)  ~ "LOW",
      !is.na(LBSTRESN) & LBSTRESN > as.numeric(HIGH) ~ "HIGH",
      !is.na(LBSTRESN)                               ~ "NORMAL",
      TRUE                                           ~ NA_character_
    ),
    # Date conversion (LBDT assumed SAS date11. like 01JAN2020)
    LBDTC = if_else(
      LBDT != "" & !is.na(LBDT),
      {
        # Convert to Date then format as YYYY-MM-DD
        dt <- as.Date(toupper(LBDT), format = "%d%b%Y")
        ifelse(is.na(dt), "", format(dt, "%Y-%m-%d"))
      },
      ""
    ),
    # Study Day calculation
    LBSTDY = if_else(
      LBDTC != "" & RFSTDTC != "" & LBDTC != "" & RFSTDTC != "",
      {
        stdt <- as.Date(LBDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(stdt - rfdt)
        ifelse(diff >= 0, diff + 1, diff)
      },
      NA_integer_
    )
  ) %>%
  # Ensure correct column order for later steps
  select(
    STUDYID, DOMAIN, USUBJID,
    LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU,
    LBSTRESC, LBSTRESN, LBSTRESU,
    LBORNRLO, LBORNRHI,
    LBNRIND,
    LBDTC, LBSTDY,
    VISIT
  )

# ------------------------------------------------------------------
# 6. Derive LBSEQ (sequence within subject, ordered by LBDTC)
# ------------------------------------------------------------------
sdtm_lb <- sdtm_lb1 %>%
  arrange(USUBJID, LBDTC) %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  # Re‑order columns to match the SAS KEEP list
  select(
    STUDYID, DOMAIN, USUBJID, LBSEQ,
    LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU,
    LBSTRESC, LBSTRESN, LBSTRESU,
    LBORNRLO, LBORNRHI,
    LBNRIND,
    LBDTC, LBSTDY,
    VISIT
  )

# ------------------------------------------------------------------
# 7. Write final dataset (lb) as CSV
# ------------------------------------------------------------------
lb_out_path <- file.path(out_dir, "lb.csv")
write.csv(
  na_to_empty(sdtm_lb),
  lb_out_path,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 8. Produce the listing (PROC PRINT equivalent)
# ------------------------------------------------------------------
listing_txt_path <- file.path(out_dir, "P026.lst")
listing_csv_path <- file.path(out_dir, "P026_listing.csv")

# Capture a text representation similar to PROC PRINT (no row numbers)
listing_txt <- capture.output(
  print(sdtm_lb, row.names = FALSE)
)
writeLines(listing_txt, con = listing_txt_path)

# Also write a CSV of the same table body (already written as lb.csv,
# but the specification asks for a separate CSV for the listing)
write.csv(
  na_to_empty(sdtm_lb),
  listing_csv_path,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```