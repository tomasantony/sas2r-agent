# ------------------------------------------------------------
# R script generated to reproduce SAS PROC TABULATE output
# Program ID: P021   (Table 1: Demographics and Baseline Characteristics)
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default rounding)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign_x <- sign(x)
  abs_x  <- abs(x)
  pow    <- 10 ^ digits
  # Add 0.5 then floor to achieve half‑up rounding
  rounded <- floor(abs_x * pow + 0.5) / pow
  sign_x * rounded
}

fmt_num <- function(x, digits) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

# ------------------------------------------------------------------
# Environment & paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# Macro variables from the SAS environment (hard‑coded for this run)
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_path    <- file.path(project_path, "data", "adam", "adsl.sas7bdat")

# ------------------------------------------------------------------
# 1. Read ADaM ADSL dataset
# ------------------------------------------------------------------
adsl <- read_sas(adam_path) %>%
  mutate(
    ITTFL   = as.character(ITTFL),
    TRT01P  = as.character(TRT01P),
    SEX     = as.character(SEX),
    RACE    = as.character(RACE),
    AGE     = as.numeric(AGE)
  ) %>%
  filter(ITTFL == "Y")               # keep only randomized subjects

# ------------------------------------------------------------------
# 2. Define treatment columns (sorted character order) + Total
# ------------------------------------------------------------------
trt_levels <- sort(unique(adsl$TRT01P[!is.na(adsl$TRT01P)]))
col_names  <- c(trt_levels, "Total")   # final column for overall totals

# ------------------------------------------------------------------
# 3. Utility to compute N, Mean, SD, Min, Max for a numeric vector
# ------------------------------------------------------------------
num_stats <- function(v) {
  v_nonmiss <- v[!is.na(v)]
  n   <- length(v_nonmiss)
  if (n == 0) {
    return(list(N = 0, Mean = NA, SD = NA, Min = NA, Max = NA))
  }
  mean_v <- mean(v_nonmiss)
  sd_v   <- sd(v_nonmiss)
  min_v  <- min(v_nonmiss)
  max_v  <- max(v_nonmiss)
  list(N = n, Mean = mean_v, SD = sd_v, Min = min_v, Max = max_v)
}

# ------------------------------------------------------------------
# 4. Build the table row‑by‑row
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
sex_levels <- c(sex_levels[!is.na(sex_levels)], if (any(is.na(adsl$SEX))) "Missing")
for (lvl in sex_levels) {
  # N per column
  n_vec <- sapply(col_names, function(col) {
    subset <- if (col == "Total") adsl else filter(adsl, TRT01P == col)
    if (lvl == "Missing") sum(is.na(subset$SEX)) else sum(subset$SEX == lvl, na.rm = TRUE)
  })
  # % per column
  pct_vec <- sapply(seq_along(col_names), function(i) {
    col_n   <- total_n_by_col[i]
    if (col_n == 0) NA else (n_vec[i] / col_n) * 100
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
race_levels <- c(race_levels[!is.na(race_levels)], if (any(is.na(adsl$RACE))) "Missing")
for (lvl in race_levels) {
  n_vec <- sapply(col_names, function(col) {
    subset <- if (col == "Total") adsl else filter(adsl, TRT01P == col)
    if (lvl == "Missing") sum(is.na(subset$RACE)) else sum(subset$RACE == lvl, na.rm = TRUE)
  })
  pct_vec <- sapply(seq_along(col_names), function(i) {
    col_n <- total_n_by_col[i]
    if (col_n == 0) NA else (n_vec[i] / col_n) * 100
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
# Helper to extract a named vector of a given statistic
extract_stat <- function(stat_name) {
  sapply(age_stats_by_col, `[[`, stat_name)
}
age_N   <- extract_stat("N")
age_Mean<- extract_stat("Mean")
age_SD  <- extract_stat("SD")
age_Min <- extract_stat("Min")
age_Max <- extract_stat("Max")

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
# 5. Assemble final data frame
# ------------------------------------------------------------------
tbl_df <- bind_rows(lapply(table_rows, as_tibble)) %>%
  select(Parameter, all_of(col_names))

# ------------------------------------------------------------------
# 6. Write outputs
# ------------------------------------------------------------------
# 6.1 CSV of the table body
write.csv(
  tbl_df,
  file = file.path(out_dir, "P021.csv"),
  row.names = FALSE,
  na = ""
)

# 6.2 Plain‑text rendering (tab‑delimited, mimicking SAS listing)
txt_lines <- c()
header <- paste(c("Parameter", col_names), collapse = "\t")
txt_lines <- c(txt_lines, header)
for (i in seq_len(nrow(tbl_df))) {
  line <- paste(tbl_df[i, ], collapse = "\t")
  txt_lines <- c(txt_lines, line)
}
writeLines(txt_lines, con = file.path(out_dir, "P021.lst"))

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
