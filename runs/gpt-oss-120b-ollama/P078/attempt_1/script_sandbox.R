# ------------------------------------------------------------
# R script to reproduce SAS program P078_l_dv.sas
# Listing 16.2.2 – Protocol Deviations
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(stringr)    # string handling
library(lubridate)  # date handling (if needed)

# ------------------------------------------------------------------
# 1. Define output directory (environment variable OUT_DIR)
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" ) {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 2. Read input ADaM and SDTM datasets
# ------------------------------------------------------------------
# Paths are taken from the libref definitions in the SAS program
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm/dv.sas7bdat"
adam_path <- "/bench/data/raw/own-synthetic/adam/adsl.sas7bdat"

dv  <- read_sas(sdtm_path)   # sdtm.dv
adsl <- read_sas(adam_path) # adam.adsl

# ------------------------------------------------------------------
# 3. Prepare the listing dataset (equivalent to PROC SQL step)
# ------------------------------------------------------------------
dv_list <- dv %>%
  # keep only variables needed later (helps with memory)
  select(USUBJID, DVCAT, DVSCAT, DVTERM, DVSTDTC, DVENDTC, EPOCH, DVSEV) %>%
  inner_join(
    adsl %>% select(USUBJID, TRTA, SITEID, RANDFL),
    by = "USUBJID"
  ) %>%
  filter(RANDFL == "Y") %>%
  mutate(
    SITE = SITEID,
    CATEGORY = DVCAT,
    SUBCATEGORY = DVSCAT,
    DESCRIPTION = DVTERM,
    START_DATE = DVSTDTC,
    END_DATE = DVENDTC,
    SEVERITY = DVSEV
  ) %>%
  select(
    USUBJID,
    TRTA,
    SITE,
    CATEGORY,
    SUBCATEGORY,
    DESCRIPTION,
    START_DATE,
    END_DATE,
    EPOCH,
    SEVERITY
  ) %>%
  arrange(TRTA, SITE, USUBJID, START_DATE)

# ------------------------------------------------------------------
# 4. Write the dataset to CSV (exact variable names, no row names)
# ------------------------------------------------------------------
write.csv(
  dv_list,
  file = file.path(out_dir, "l_dv.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 5. Create a plain‑text rendering that mimics PROC REPORT output
# ------------------------------------------------------------------
# Column display labels (as defined in the PROC REPORT DEFINE statements)
col_labels <- c(
  "Treatment",
  "Site",
  "Subject ID",
  "Deviation Category",
  "Sub- category",
  "Description",
  "Start Date",
  "End Date",
  "Epoch",
  "Severity"
)

# Approximate character widths derived from the SAS column width specifications
col_widths <- c(12, 8, 12, 18, 15, 25, 10, 10, 12, 9)

# Helper to left‑justify a string within a given width
pad_left <- function(x, w) {
  fmt <- sprintf("%%-%ds", w)
  sprintf(fmt, ifelse(is.na(x), "", as.character(x)))
}

# Build header line
header_line <- paste0(
  mapply(pad_left, col_labels, col_widths, SIMPLIFY = FALSE),
  collapse = " "
)

# Build data lines, inserting a blank line after each TRTA group (page break)
data_lines <- character()
prev_trta <- NA_character_

for (i in seq_len(nrow(dv_list))) {
  row <- dv_list[i, ]
  # Insert a page‑break separator when TRTA changes (except before first group)
  if (!is.na(prev_trta) && row$TRTA != prev_trta) {
    data_lines <- c(data_lines, "")  # blank line as page break
  }
  prev_trta <- row$TRTA
  
  fields <- c(
    row$TRTA,
    row$SITE,
    row$USUBJID,
    row$CATEGORY,
    row$SUBCATEGORY,
    row$DESCRIPTION,
    row$START_DATE,
    row$END_DATE,
    row$EPOCH,
    row$SEVERITY
  )
  line <- paste0(
    mapply(pad_left, fields, col_widths, SIMPLIFY = FALSE),
    collapse = " "
  )
  data_lines <- c(data_lines, line)
}

# Combine all parts
txt_content <- c(
  header_line,
  strrep("-", nchar(header_line)),  # separator line
  data_lines
)

# Write the plain‑text rendering
writeLines(txt_content, con = file.path(out_dir, "l_dv.txt"))

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
