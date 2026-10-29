# ------------------------------------------------------------
# R translation of SAS program P026_lb.sas  (attempt 3)
# ------------------------------------------------------------
# Load required packages
library(haven)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------
# Vectorised conversion of SAS date11. strings (e.g., "01JAN2020") to YYYY-MM-DD.
convert_LBDT_vec <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || str_trim(v) == "") return("")
    # Try day-month-abbrev-year first
    dt <- as.Date(toupper(str_trim(v)), format = "%d%b%Y")
    if (!is.na(dt)) return(format(dt, "%Y-%m-%d"))
    # Try ISO ymd
    dt2 <- ymd(v, quiet = TRUE)
    if (!is.na(dt2)) return(format(dt2, "%Y-%m-%d"))
    # Try compact yyyymmdd
    dt3 <- as.Date(v, format = "%Y%m%d")
    if (!is.na(dt3)) return(format(dt3, "%Y-%m-%d"))
    ""
  }, USE.NAMES = FALSE)
}

# Write CSV with SAS‑style empty strings for missing values
write_sas_csv <- function(df, path) {
  df_out <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
  write.csv(df_out, path, row.names = FALSE, na = "")
}

# ------------------------------------------------------------------
# 1. Set directories and output location
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Paths used in the original SAS macro variables
raw_dir  <- "/bench/data/raw"
raw_sdtm <- "/bench/data/raw/cdisc-pilot/sdtm"

# ------------------------------------------------------------------
# 2. Load source datasets
# ------------------------------------------------------------------
# edc_lb (Excel)
edc_lb_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_lb.xlsx"
edc_lb <- read_excel(edc_lb_path, guess_max = 10000) %>%
  mutate(across(everything(), as.character))

# sdtm_dm (first DM file found in the SDTM folder)
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
# 4. Prepare Raw LB data with USUBJID and sort by USUBJID
# ------------------------------------------------------------------
lb_prep <- edc_lb %>%
  mutate(USUBJID = str_c("ONCO-2026", "-", SUBJID)) %>%
  arrange(USUBJID)                     # PROC SORT BY USUBJID

# ------------------------------------------------------------------
# 5. Merge LB with DM reference (left join, keep all LB rows)
# ------------------------------------------------------------------
sdtm_lb1 <- lb_prep %>%
  left_join(dm_for_lb, by = "USUBJID") %>%
  # Ensure same order as SAS (by USUBJID, stable)
  arrange(USUBJID) %>%
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
    # Date conversion (SAS date11. -> YYYY-MM-DD)
    LBDTC = convert_LBDT_vec(LBDT),
    # Study Day calculation (only when both dates present)
    LBSTDY = if_else(
      LBDTC != "" & RFSTDTC != "",
      {
        stdt <- as.Date(LBDTC)
        rfdt <- as.Date(RFSTDTC)
        diff <- as.numeric(stdt - rfdt)
        ifelse(diff >= 0, diff + 1, diff)
      },
      NA_integer_
    )
  ) %>%
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
# 6. Derive LBSEQ (sequence within subject, ordered by USUBJID then LBDTC,
#    with SAS‑style missing‑first ordering)
# ------------------------------------------------------------------
sdtm_lb <- sdtm_lb1 %>%
  arrange(USUBJID,
          desc(is.na(LBDTC)),   # missing dates first (SAS behavior)
          LBDTC) %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
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
write_sas_csv(sdtm_lb, lb_out_path)

# ------------------------------------------------------------------
# 8. Produce the listing (PROC PRINT equivalent)
# ------------------------------------------------------------------
listing_txt_path <- file.path(out_dir, "P026.lst")
listing_csv_path <- file.path(out_dir, "P026_listing.csv")

# Text rendering similar to PROC PRINT (space‑separated, no row numbers)
listing_txt <- capture.output(
  write.table(sdtm_lb, file = "", sep = " ", row.names = FALSE,
              col.names = TRUE, quote = FALSE)
)
writeLines(listing_txt, con = listing_txt_path)

# CSV of the same table body (as required)
write_sas_csv(sdtm_lb, listing_csv_path)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
