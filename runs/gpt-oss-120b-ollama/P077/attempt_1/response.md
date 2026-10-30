```r
# ------------------------------------------------------------
# R translation of SAS program P077_l_death.sas
# Generates the "Listing 16.2.7.3 - Listing of Deaths"
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(stringr)

# ------------------------------------------------------------------
# Helper: SAS DATE9 format (e.g., 01JAN2020)
# ------------------------------------------------------------------
sas_date9 <- function(x) {
  if (is.na(x)) return("")
  fmt <- format(x, "%d%b%Y")
  toupper(fmt)
}

# ------------------------------------------------------------------
# Environment & paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# ------------------------------------------------------------------
# Step 1: Read ADaM datasets
# ------------------------------------------------------------------
adsl <- read_sas(file.path(adam_path, "adsl.sas7bdat"))
adae <- read_sas(file.path(adam_path, "adae.sas7bdat"))

# Convert SAS numeric dates to R Date objects
date_vars <- c("TRTSDT", "TRTEDT", "DTHDT")
for (v in date_vars) {
  if (v %in% names(adsl)) {
    adsl[[v]] <- as.Date(adsl[[v]], origin = "1960-01-01")
  }
}
# ------------------------------------------------------------------
# Step 2: Identify deaths
# ------------------------------------------------------------------
deaths <- adsl %>%
  filter(DTHFL == "Y") %>%
  mutate(
    TRT_START = TRTSDT,
    TRT_END   = TRTEDT,
    DEATH_DATE = DTHDT,
    DAYS_LASTDOSE_DEATH = if_else(!is.na(DTHDT) & !is.na(TRTEDT),
                                 as.numeric(DTHDT - TRTEDT), NA_real_),
    DAYS_FIRSTDOSE_DEATH = if_else(!is.na(DTHDT) & !is.na(TRTSDT),
                                   as.numeric(DTHDT - TRTSDT + 1), NA_real_)
  ) %>%
  select(
    USUBJID, TRTA, AGE, SEX, RACE,
    TRT_START, TRT_END, DEATH_DATE,
    DTHCAUS,
    DAYS_LASTDOSE_DEATH, DAYS_FIRSTDOSE_DEATH
  ) %>%
  arrange(TRTA, USUBJID)

# ------------------------------------------------------------------
# Step 3: Fatal AE information
# ------------------------------------------------------------------
death_ae <- adae %>%
  filter(AEOUT == "FATAL") %>%
  distinct(
    USUBJID,
    FATAL_AE_SOC = AEBODSYS,
    FATAL_AE_PT  = AEDECOD,
    FATAL_AE_REL = AEREL
  ) %>%
  arrange(USUBJID)

# ------------------------------------------------------------------
# Step 4: Merge death info with fatal AE data
# ------------------------------------------------------------------
death_report <- deaths %>%
  left_join(death_ae, by = "USUBJID") %>%
  arrange(TRTA, USUBJID)

# ------------------------------------------------------------------
# Step 5: Build final display variables
# ------------------------------------------------------------------
death_final <- death_report %>%
  mutate(
    demog = str_c(
      as.character(AGE), "/", SEX, "/", str_sub(RACE, 1, 1)
    ),
    days_display = if_else(!is.na(DAYS_LASTDOSE_DEATH),
                           as.character(DAYS_LASTDOSE_DEATH), "N/A"),
    # Format dates as SAS DATE9 strings
    TRT_START = sas_date9(TRT_START),
    TRT_END   = sas_date9(TRT_END),
    DEATH_DATE = sas_date9(DEATH_DATE),
    CAUSE_OF_DEATH = DTHCAUS
  ) %>%
  select(
    TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE,
    days_display, CAUSE_OF_DEATH,
    FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL
  )

# ------------------------------------------------------------------
# Step 6: Write outputs
# ------------------------------------------------------------------
# 6a) CSV of the final dataset (raw values, dates as strings)
write.csv(
  death_final,
  file = file.path(out_dir, "death_final.csv"),
  row.names = FALSE,
  na = ""
)

# 6b) CSV of the listing table (same as death_final)
write.csv(
  death_final,
  file = file.path(out_dir, "l_death.csv"),
  row.names = FALSE,
  na = ""
)

# 6c) Plain‑text rendering of the listing
txt_file <- file.path(out_dir, "l_death.txt")
col_names <- c(
  "Treatment",
  "Subject ID",
  "Age/Sex/ Race",
  "First Dose",
  "Last Dose",
  "Death Date",
  "Days Last Dose to Death",
  "Cause of Death",
  "Fatal AE SOC",
  "Fatal AE PT",
  "Relationship"
)

# Determine column widths (simple heuristic for alignment)
col_widths <- c(12, 12, 15, 12, 12, 12, 22, 20, 20, 20, 15)

# Helper to pad strings
pad <- function(x, w, side = "right") {
  if (side == "right") {
    sprintf(paste0("%-", w, "s"), x)
  } else {
    sprintf(paste0("%", w, "s"), x)
  }
}

# Build header line
header_line <- paste0(
  mapply(pad, col_names, col_widths, MoreArgs = list(side = "right")),
  collapse = " "
)

# Build data lines
data_lines <- apply(death_final, 1, function(row) {
  fields <- c(
    pad(row["TRTA"], col_widths[1]),
    pad(row["USUBJID"], col_widths[2]),
    pad(row["demog"], col_widths[3]),
    pad(row["TRT_START"], col_widths[4]),
    pad(row["TRT_END"], col_widths[5]),
    pad(row["DEATH_DATE"], col_widths[6]),
    pad(row["days_display"], col_widths[7]),
    pad(row["CAUSE_OF_DEATH"], col_widths[8]),
    pad(ifelse(is.na(row["FATAL_AE_SOC"]), "", row["FATAL_AE_SOC"]), col_widths[9]),
    pad(ifelse(is.na(row["FATAL_AE_PT"]), "", row["FATAL_AE_PT"]), col_widths[10]),
    pad(ifelse(is.na(row["FATAL_AE_REL"]), "", row["FATAL_AE_REL"]), col_widths[11])
  )
  paste0(fields, collapse = " ")
})

# Write to text file
cat("Listing 16.2.7.3 - Listing of Deaths\n", file = txt_file)
cat("\n", file = txt_file, append = TRUE)
cat(header_line, "\n", file = txt_file, append = TRUE)
cat(strrep("-", nchar(header_line)), "\n", file = txt_file, append = TRUE)
cat(paste0(data_lines, collapse = "\n"), "\n", file = txt_file, append = TRUE)

# ------------------------------------------------------------------
# Cleanup (mirrors PROC DATASETS DELETE)
# ------------------------------------------------------------------
rm(deaths, death_ae, death_report, death_final)
gc()
```