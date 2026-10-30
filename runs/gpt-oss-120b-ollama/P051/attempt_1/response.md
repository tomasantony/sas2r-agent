```r
# ------------------------------------------------------------
# R translation of SAS program P051_medication_listing.sas
# ------------------------------------------------------------
# Required packages
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(stringr)    # string handling
library(readr)      # write_csv (optional)
library(tidyr)      # for replace_na
# ------------------------------------------------------------

# ------------------------------------------------------------------
# Define macro-like variables (as in the original SAS environment)
# ------------------------------------------------------------------
root      <- "D:/meta_hackathon/sas2r-agent"
pid       <- "P051"
rawadam   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
# POPN and POPNLABEL are used in the report
POPN      <- "SAFFL"
POPNLABEL <- "Safety"

# Output directory (set by the driver)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}

# ------------------------------------------------------------------
# 1. Read source transport file (ADCM.xpt)
# ------------------------------------------------------------------
adcm_path <- file.path(rawadam, "adcm.xpt")
adcm_raw  <- read_xpt(adcm_path)

# ------------------------------------------------------------------
# 2. Apply population filter (SAFFL = 'Y')
# ------------------------------------------------------------------
adcm_filt <- adcm_raw %>%
  filter(.data[[POPN]] == "Y")

# ------------------------------------------------------------------
# 3. Sort as in PROC SORT
# ------------------------------------------------------------------
adcm_sort <- adcm_filt %>%
  arrange(TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

# ------------------------------------------------------------------
# 4. Create ADCM2 data set (adds derived columns)
# ------------------------------------------------------------------
adcm2 <- adcm_sort %>%
  mutate(
    Period = "Study Period",
    # Replace missing dates with 'NK' (SAS treats blank as missing)
    CMSTDTC = if_else(is.na(CMSTDTC) | CMSTDTC == "", "NK", CMSTDTC),
    CMENDTC = if_else(is.na(CMENDTC) | CMENDTC == "", "NK", CMENDTC),
    # StartStop = compbl(CMSTDTC||'/'||CMENDTC)
    StartStop = str_squish(paste0(CMSTDTC, "/", CMENDTC)),
    # DoseUnit handling – CMDOSU is the SAS variable that holds the unit
    DoseUnit = if_else(!is.na(CMDOSU) & CMDOSU != "",
                       str_squish(paste0(CMDOSE, "(", CMDOSU, ")")),
                       str_squish(as.character(CMDOSE))),
    # Ongoing flag
    Ongoing = if_else(AENRF == "Ongoing", "Y", "N")
  ) %>%
  # Keep only columns that will be used later (optional but mirrors SAS KEEP)
  select(
    STUDYID, USUBJID, TRTA, CMTRT, CMDECOD, CMCLAS,
    DoseUnit, CMINDC, StartStop, ADURN, Ongoing,
    everything()
  )

# ------------------------------------------------------------------
# 5. Write the derived dataset (ADCM2) as CSV
# ------------------------------------------------------------------
write.csv(
  adcm2,
  file = file.path(out_dir, "ADCM2.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 6. Build the listing that PROC REPORT would produce
#    (grouped BY TRTA, with titles)
# ------------------------------------------------------------------
# Columns that appear in the listing (TRTA is used only for grouping)
listing_df <- adcm2 %>%
  select(
    TRTA, USUBJID, CMTRT, CMDECOD, CMCLAS,
    DoseUnit, CMINDC, StartStop, ADURN, Ongoing
  ) %>%
  arrange(TRTA, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

# ------------------------------------------------------------------
# 7. Write the table body as CSV (same columns, without the grouping column)
# ------------------------------------------------------------------
write.csv(
  listing_df %>% select(-TRTA),
  file = file.path(out_dir, "Target36.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 8. Create a plain‑text rendering of the listing (mimicking the SAS RTF output)
# ------------------------------------------------------------------
txt_lines <- character()

# Title lines (populated from macro variables)
txt_lines <- c(txt_lines, paste0(POPNLABEL, " Population"))
txt_lines <- c(txt_lines, "Study Period")

# Process each treatment group
trt_levels <- unique(listing_df$TRTA)

for (trt in trt_levels) {
  txt_lines <- c(txt_lines, paste0("Treatment: ", trt))
  sub_df <- listing_df %>%
    filter(TRTA == trt) %>%
    select(-TRTA)

  # Create a simple fixed‑width representation
  # (using a single space as separator – SAS PROC REPORT default is space‑delimited)
  rows_txt <- apply(sub_df, 1, function(row) paste(row, collapse = " "))
  txt_lines <- c(txt_lines, rows_txt, "")  # blank line after each block
}

# Write the text file
writeLines(txt_lines, con = file.path(out_dir, "Target36.txt"))

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
```