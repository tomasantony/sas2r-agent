# ------------------------------------------------------------
# R translation of SAS program P051_medication_listing.sas
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(haven)   # read_xpt
library(dplyr)   # data manipulation
library(stringr) # string handling
library(tidyr)   # replace_na (if needed)

# ------------------------------------------------------------
# 0. Macro‑like variables (as defined in the SAS environment)
# ------------------------------------------------------------
root      <- "/bench"
rawadam   <- "/bench/data/raw/cdisc-pilot/adam"
POPN      <- "SAFFL"          # population flag variable
POPNLABEL <- "Safety"        # population label used in titles

# Output directory (provided by the driver via environment variable)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------
# 1. Read the source transport file (ADCM.xpt)
# ------------------------------------------------------------
adcm_path <- file.path(rawadam, "adcm.xpt")
adcm_raw  <- read_xpt(adcm_path)

# Ensure column names are uppercase (SAS is case‑insensitive but R is not)
adcm_raw <- rename_with(adcm_raw, toupper)

# ------------------------------------------------------------
# 2. Apply the population filter (SAFFL = 'Y')
# ------------------------------------------------------------
adcm_filt <- adcm_raw %>%
  filter(.data[[POPN]] == "Y")

# ------------------------------------------------------------
# 3. Sort exactly as PROC SORT does
#    BY TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD
# ------------------------------------------------------------
adcm_sort <- adcm_filt %>%
  arrange(TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

# ------------------------------------------------------------
# 4. Create ADCM2 (adds derived columns)
# ------------------------------------------------------------
adcm2 <- adcm_sort %>%
  mutate(
    Period = "Study Period",
    # Replace missing dates with 'NK' (SAS treats blank as missing)
    CMSTDTC = if_else(is.na(CMSTDTC) | CMSTDTC == "", "NK", CMSTDTC),
    CMENDTC = if_else(is.na(CMENDTC) | CMENDTC == "", "NK", CMENDTC),
    # StartStop = compbl(CMSTDTC||'/'||CMENDTC)
    StartStop = str_squish(paste0(CMSTDTC, "/", CMENDTC)),
    # DoseUnit handling – CMDOSU holds the unit; if missing use just dose
    DoseUnit = if_else(!is.na(CMDOSU) & CMDOSU != "",
                       str_squish(paste0(CMDOSE, "(", CMDOSU, ")")),
                       str_squish(as.character(CMDOSE))),
    # Ongoing flag based on AENRF
    Ongoing = if_else(AENRF == "Ongoing", "Y", "N")
  ) %>%
  # Keep only variables that will be needed for the listing (mirrors the PROC REPORT SELECT)
  select(
    STUDYID, USUBJID, TRTA, CMTRT, CMDECOD, CMCLAS,
    DoseUnit, CMINDC, StartStop, ADURN, Ongoing,
    everything()
  )

# ------------------------------------------------------------
# 5. Write the derived dataset (ADCM2) as CSV
# ------------------------------------------------------------
write.csv(
  adcm2,
  file = file.path(out_dir, "ADCM2.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 6. Build the PROC REPORT style listing (grouped BY TRTA)
# ------------------------------------------------------------
listing_df <- adcm2 %>%
  select(
    TRTA, USUBJID, CMTRT, CMDECOD, CMCLAS,
    DoseUnit, CMINDC, StartStop, ADURN, Ongoing
  ) %>%
  arrange(TRTA, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

# ------------------------------------------------------------
# 7. Write the table body (without the grouping column) as CSV
# ------------------------------------------------------------
write.csv(
  listing_df %>% select(-TRTA),
  file = file.path(out_dir, "Target36.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 8. Plain‑text rendering of the listing (mimics the SAS RTF output)
# ------------------------------------------------------------
txt_lines <- character()

# Title lines (populated from macro variables)
txt_lines <- c(txt_lines, paste0(POPNLABEL, " Population"))
txt_lines <- c(txt_lines, "Study Period")

# Process each treatment group (BY TRTA)
for (trt in unique(listing_df$TRTA)) {
  txt_lines <- c(txt_lines, paste0("Treatment: ", trt))
  sub_df <- listing_df %>%
    filter(TRTA == trt) %>%
    select(-TRTA)

  # Simple space‑delimited rows (similar to PROC REPORT default)
  rows_txt <- apply(sub_df, 1, function(r) paste(r, collapse = " "))
  txt_lines <- c(txt_lines, rows_txt, "")  # blank line after each group
}

# Write the plain‑text file
writeLines(txt_lines, con = file.path(out_dir, "Target36.txt"))

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
