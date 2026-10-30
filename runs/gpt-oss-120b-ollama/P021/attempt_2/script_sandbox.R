# ------------------------------------------------------------
# R script to reproduce SAS PROC TABULATE output for Table 1
# Program ID: P021   (Demographics and Baseline Characteristics)
# ------------------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(tidyr)      # reshaping helpers
library(stringr)    # string utilities
library(data.table) # for fast rbindlist

# ------------------------------------------------------------------
# Helper: SAS‑style rounding (half away from zero)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign_x <- sign(x)
  abs_x  <- abs(x)
  pow    <- 10 ^ digits
  # Add 0.5 then floor => half‑up
  rounded <- floor(abs_x * pow + 0.5) / pow
  sign_x * rounded
}
fmt_num <- function(x, digits) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

# ------------------------------------------------------------------
# Paths & environment
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# Macro variables (hard‑coded as they were in the SAS run)
project_path <- "/bench/data/derived/c2c"
adsl_path    <- file.path(project_path, "data", "adam", "adsl.sas7bdat")

# ------------------------------------------------------------------
# 1. Read ADaM ADSL dataset and keep ITT subjects
# ------------------------------------------------------------------
adsl <- read_sas(adsl_path) %>%
  mutate(
    ITTFL  = as.character(ITTFL),
    TRT01P = as.character(TRT01P),
    SEX    = as.character(SEX),
    RACE   = as.character(RACE),
    AGE    = as.numeric(AGE)
  ) %>%
  filter(ITTFL == "Y")   # keep only randomized subjects

# ------------------------------------------------------------------
# 2. Determine treatment columns (sorted character order) + Total
# ------------------------------------------------------------------
trt_levels <- sort(unique(adsl$TRT01P[!is.na(adsl$TRT01P)]))
col_names  <- c(trt_levels, "Total")   # final column for overall totals

# ------------------------------------------------------------------
# 3. Helper to compute numeric statistics
# ------------------------------------------------------------------
num_stats <- function(v) {
  v_nona <- v[!is.na(v)]
  n <- length(v_nona)
  if (n == 0) {
    return(list(N = 0, Mean = NA, SD = NA, Min = NA, Max = NA))
  }
  list(
    N    = n,
    Mean = mean(v_nona),
    SD   = sd(v_nona),
    Min  = min(v_nona),
    Max  = max(v_nona)
  )
}

# ------------------------------------------------------------------
# 4. Build table rows (as named character vectors)
# ------------------------------------------------------------------
table_rows <- list()

# ---- 4.1 Total Subjects (N only) ------------------------------------
total_n_by_col <- sapply(col_names, function(col) {
  if (col == "Total") {
    nrow(adsl)
  } else {
    sum(adsl$TRT01P == col, na.rm = TRUE)
  }
})
table_rows[[length(table_rows) + 1]] <- c(
  Parameter = "Total Subjects",
  setNames(as.character(total_n_by_col), col_names)
)

# ---- 4.2 Sex -------------------------------------------------------
sex_levels <- sort(unique(adsl$SEX))
if (any(is.na(adsl$SEX))) sex_levels <- c(sex_levels, "Missing")
for (lvl in sex_levels) {
  n_vec <- sapply(col_names, function(col) {
    subset <- if (col == "Total") adsl else filter(adsl, TRT01P == col)
    if (lvl == "Missing") sum(is.na(subset$SEX)) else sum(subset$SEX == lvl, na.rm = TRUE)
  })
  pct_vec <- sapply(seq_along(col_names), function(i) {
    denom <- total_n_by_col[i]
    if (denom == 0) NA else (n_vec[i] / denom) * 100
  })
  # N row
  table_rows[[length(table_rows) + 1]] <- c(
    Parameter = paste0("Sex = ", lvl, " (n)"),
    setNames(as.character(n_vec), col_names)
  )
  # % row
  table_rows[[length(table_rows) + 1]] <- c(
    Parameter = paste0("Sex = ", lvl, " (%)"),
    setNames(sapply(pct_vec, fmt_num, digits = 1), col_names)
  )
}

# ---- 4.3 Race ------------------------------------------------------
race_levels <- sort(unique(adsl$RACE))
if (any(is.na(adsl$RACE))) race_levels <- c(race_levels, "Missing")
for (lvl in race_levels) {
  n_vec <- sapply(col_names, function(col) {
    subset <- if (col == "Total") adsl else filter(adsl, TRT01P == col)
    if (lvl == "Missing") sum(is.na(subset$RACE)) else sum(subset$RACE == lvl, na.rm = TRUE)
  })
  pct_vec <- sapply(seq_along(col_names), function(i) {
    denom <- total_n_by_col[i]
    if (denom == 0) NA else (n_vec[i] / denom) * 100
  })
  # N row
  table_rows[[length(table_rows) + 1]] <- c(
    Parameter = paste0("Race = ", lvl, " (n)"),
    setNames(as.character(n_vec), col_names)
  )
  # % row
  table_rows[[length(table_rows) + 1]] <- c(
    Parameter = paste0("Race = ", lvl, " (%)"),
    setNames(sapply(pct_vec, fmt_num, digits = 1), col_names)
  )
}

# ---- 4.4 Age (numeric statistics) ----------------------------------
age_stats_by_col <- lapply(col_names, function(col) {
  subset <- if (col == "Total") adsl else filter(adsl, TRT01P == col)
  num_stats(subset$AGE)
})
extract_stat <- function(name) sapply(age_stats_by_col, `[[`, name)

age_N    <- extract_stat("N")
age_Mean <- extract_stat("Mean")
age_SD   <- extract_stat("SD")
age_Min  <- extract_stat("Min")
age_Max  <- extract_stat("Max")

# Age N
table_rows[[length(table_rows) + 1]] <- c(
  Parameter = "Age (years) N",
  setNames(as.character(age_N), col_names)
)
# Age Mean
table_rows[[length(table_rows) + 1]] <- c(
  Parameter = "Age (years) Mean",
  setNames(sapply(age_Mean, fmt_num, digits = 1), col_names)
)
# Age SD
table_rows[[length(table_rows) + 1]] <- c(
  Parameter = "Age (years) SD",
  setNames(sapply(age_SD, fmt_num, digits = 2), col_names)
)
# Age Min
table_rows[[length(table_rows) + 1]] <- c(
  Parameter = "Age (years) Min",
  setNames(sapply(age_Min, fmt_num, digits = 0), col_names)
)
# Age Max
table_rows[[length(table_rows) + 1]] <- c(
  Parameter = "Age (years) Max",
  setNames(sapply(age_Max, fmt_num, digits = 0), col_names)
)

# ------------------------------------------------------------------
# 5. Assemble final table data frame
# ------------------------------------------------------------------
# Convert each named vector to a one‑row tibble, then row‑bind
tbl_df <- rbindlist(lapply(table_rows, function(x) as.data.table(as.list(x))), fill = TRUE)

# Ensure column order: Parameter first, then treatment columns in defined order
tbl_df <- tbl_df[, c("Parameter", col_names), with = FALSE]

# Replace any NA (e.g., percentages with denominator 0) by empty string for output
tbl_df[is.na(tbl_df)] <- ""

# ------------------------------------------------------------------
# 6. Write outputs
# ------------------------------------------------------------------
# 6.1 CSV of the table body (numeric values already formatted)
write.csv(
  tbl_df,
  file = file.path(out_dir, "P021.csv"),
  row.names = FALSE,
  na = ""
)

# 6.2 Plain‑text rendering (tab‑delimited) – .txt as required
txt_path <- file.path(out_dir, "P021.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")
# Header line
writeLines(paste(names(tbl_df), collapse = "\t"), con = con)
# Data lines
apply(tbl_df, 1, function(row) writeLines(paste(row, collapse = "\t"), con = con))
close(con)

# 6.3 Also write a .lst file (SAS convention) – identical to .txt
file.copy(txt_path, file.path(out_dir, "P021.lst"), overwrite = TRUE)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
