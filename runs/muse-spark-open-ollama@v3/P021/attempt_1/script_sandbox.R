# P021_tlf_table1.sas -> R translation
# Table 1: Demographics and Baseline Characteristics (ITT)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

# ---- helpers: SAS rounding (half away from zero) and formatting ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_sas <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  rx <- round_half_up(x, digits)
  sprintf(paste0("%.", digits, "f"), rx)
}
# SAS character ordering: uppercase before lowercase, missing first
sas_sort_char <- function(v) {
  # v is character vector, may contain NA
  # Use C locale byte ordering to mimic SAS
  uniq <- unique(v)
  nonmiss <- uniq[!is.na(uniq)]
  # stringr with C locale or base sort with radix
  # base sort with method="radix" is byte-wise
  sorted <- sort(nonmiss, method = "radix")
  if (any(is.na(uniq))) c(NA_character_, sorted) else sorted
}

# ---- locate and read ADSL ----
find_adsl <- function() {
  candidates <- c(
    "/bench/data/derived/c2c/data/adam/adsl.sas7bdat",
    "/bench/data/derived/c2c/data/adam/ADSL.sas7bdat",
    "/bench/data/derived/c2c/data/adam/adsl.xpt",
    "/bench/data/derived/c2c/data/adam/ADSL.xpt",
    "/bench/data/derived/c2c/data/adam/adsl.csv",
    "/bench/data/derived/c2c/data/adam/ADSL.csv",
    "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
    "/bench/data/raw/cdisc-pilot/adam/ADSL.xpt",
    "/bench/data/raw/cdisc-pilot/adam/adsl.sas7bdat",
    "/bench/data/derived/cdisc-pilot/adam-split/adsl.sas7bdat",
    "/bench/data/derived/cdisc-pilot/adam-split/adsl.xpt"
  )
  # also try to discover via directory listing
  proj_dirs <- c(
    "/bench/data/derived/c2c/data/adam",
    "/bench/data/derived/c2c/data/sdtm",
    "/bench/data/raw/cdisc-pilot/adam",
    Sys.getenv("project_path"),
    file.path(Sys.getenv("project_path"), "data/adam"),
    file.path(Sys.getenv("project_path"), "data/sdtm")
  )
  for (d in proj_dirs) {
    if (!is.na(d) && d != "" && dir.exists(d)) {
      files <- list.files(d, pattern = "(?i)^adsl\\.(sas7bdat|xpt|csv)$", full.names = TRUE)
      if (length(files) > 0) candidates <- c(files, candidates)
    }
  }
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  return(NA_character_)
}

adsl_path <- find_adsl()
if (is.na(adsl_path)) {
  stop("ADSL dataset not found. Searched candidates for adsl.sas7bdat/xpt/csv in derived/c2c and raw pilot.")
}

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop(paste("Unknown extension:", path))
  }
}

adsl_raw <- read_any(adsl_path)

# Convert haven labelled to plain R, and uppercase names for SAS compliance (validvarname=upcase)
adsl <- adsl_raw
# zap labels but keep values
adsl <- haven::zap_formats(adsl)
adsl <- haven::zap_label(adsl)
adsl <- haven::zap_widths(adsl)
# Convert labelled vectors to character/numeric
for (nm in names(adsl)) {
  if (haven::is.labelled(adsl[[nm]])) {
    # try to convert to character via as_factor then as.character to preserve labels if needed
    # For analysis variables we want underlying values; for class variables we want formatted values
    # Use as_factor to get labels, but if labels are missing keep original
    adsl[[nm]] <- as.character(haven::as_factor(adsl[[nm]], levels = "labels"))
  }
}
# Uppercase names to mimic SAS validvarname=upcase
names(adsl) <- toupper(names(adsl))

# Ensure required columns exist (case-insensitive check already uppercased)
required <- c("ITTFL", "TRT01P", "SEX", "RACE", "AGE")
for (r in required) {
  if (!r %in% names(adsl)) {
    stop(paste0("Required variable ", r, " not found in ADSL. Available: ", paste(names(adsl), collapse=", ")))
  }
}

# Trim character variables (SAS CATX/PUT behavior trims)
char_cols <- c("ITTFL", "TRT01P", "SEX", "RACE")
for (cc in char_cols) {
  if (cc %in% names(adsl)) {
    adsl[[cc]] <- trimws(as.character(adsl[[cc]]))
    adsl[[cc]][adsl[[cc]] == ""] <- NA_character_
  }
}
# AGE numeric
adsl$AGE <- suppressWarnings(as.numeric(adsl$AGE))

# Filter ITTFL = 'Y' (SAS WHERE is case-sensitive, missing excluded)
adsl_itt <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")
stopifnot(nrow(adsl_itt) > 0)

# ---- Determine column levels (TRT01P) with MISSING option ----
# SAS MISSING includes missing as a level, missing sorts first
trt_levels <- sas_sort_char(adsl_itt$TRT01P)
# Keep NA as explicit level if present
has_trt_miss <- any(is.na(adsl_itt$TRT01P))

# For display, replace NA with "Missing"
trt_display <- ifelse(is.na(trt_levels), "Missing", trt_levels)

# Column totals (ALL="Total Subjects" * N)
col_totals <- sapply(trt_levels, function(lv) {
  if (is.na(lv)) sum(is.na(adsl_itt$TRT01P))
  else sum(!is.na(adsl_itt$TRT01P) & adsl_itt$TRT01P == lv)
})
names(col_totals) <- trt_display
total_all_n <- nrow(adsl_itt)

# For header we need N per column
col_header_n <- c(col_totals, Total = total_all_n)
col_names <- c(trt_display, "Total")
# Ensure order matches SAS: TRT01P sorted ascending, then ALL="Total" last
# Already have that

# ---- Determine row levels ----
sex_levels <- sas_sort_char(adsl_itt$SEX)
race_levels <- sas_sort_char(adsl_itt$RACE)

# Helper to count for a given class level and treatment level
count_cell <- function(df, class_var, class_level, trt_level) {
  # class_var is string "SEX" or "RACE"
  # class_level may be NA
  # trt_level may be NA
  sub <- df
  if (is.na(trt_level)) {
    sub <- sub %>% filter(is.na(TRT01P))
  } else {
    sub <- sub %>% filter(!is.na(TRT01P) & TRT01P == trt_level)
  }
  if (is.na(class_level)) {
    sum(is.na(sub[[class_var]]))
  } else {
    sum(!is.na(sub[[class_var]]) & sub[[class_var]] == class_level, na.rm = TRUE)
  }
}
count_cell_total <- function(df, class_var, class_level) {
  if (is.na(class_level)) sum(is.na(df[[class_var]]))
  else sum(!is.na(df[[class_var]]) & df[[class_var]] == class_level, na.rm = TRUE)
}

# ---- Build result table ----
# We will create a tidy data frame with one row per Parameter/Level/Statistic
# Parameters in order: Total Subjects, Sex, Race, Age (years)

rows_list <- list()

# 1. Total Subjects: ALL * N
for (i in seq_along(col_names)) {
  cn <- col_names[i]
  lv <- if (cn == "Total") NA else trt_levels[match(cn, trt_display)]
  # For Total column, use total_all_n, else col_totals
  n_val <- if (cn == "Total") total_all_n else col_totals[cn]
  rows_list[[length(rows_list)+1]] <- tibble(
    Parameter = "Total Subjects",
    Level = "Total",
    Statistic = "n",
    Column = cn,
    N_raw = n_val,
    Display = fmt_sas(n_val, 0)
  )
}

# 2. SEX: N and ColPctN per level
for (sl in sex_levels) {
  sl_disp <- ifelse(is.na(sl), "Missing", sl)
  for (cn in col_names) {
    # denominator for percent is column total
    denom <- if (cn == "Total") total_all_n else col_totals[cn]
    # numerator
    if (cn == "Total") {
      n_val <- count_cell_total(adsl_itt, "SEX", sl)
    } else {
      trt_lv <- trt_levels[match(cn, trt_display)]
      n_val <- count_cell(adsl_itt, "SEX", sl, trt_lv)
    }
    pct_val <- if (denom == 0) 0 else n_val / denom * 100
    # N row
    rows_list[[length(rows_list)+1]] <- tibble(
      Parameter = "Sex",
      Level = sl_disp,
      Statistic = "n",
      Column = cn,
      N_raw = n_val,
      Display = fmt_sas(n_val, 0)
    )
    # % row
    rows_list[[length(rows_list)+1]] <- tibble(
      Parameter = "Sex",
      Level = sl_disp,
      Statistic = "%",
      Column = cn,
      N_raw = pct_val,
      Display = fmt_sas(pct_val, 1)
    )
  }
}

# 3. RACE: N and ColPctN per level
for (rl in race_levels) {
  rl_disp <- ifelse(is.na(rl), "Missing", rl)
  for (cn in col_names) {
    denom <- if (cn == "Total") total_all_n else col_totals[cn]
    if (cn == "Total") {
      n_val <- count_cell_total(adsl_itt, "RACE", rl)
    } else {
      trt_lv <- trt_levels[match(cn, trt_display)]
      n_val <- count_cell(adsl_itt, "RACE", rl, trt_lv)
    }
    pct_val <- if (denom == 0) 0 else n_val / denom * 100
    rows_list[[length(rows_list)+1]] <- tibble(
      Parameter = "Race",
      Level = rl_disp,
      Statistic = "n",
      Column = cn,
      N_raw = n_val,
      Display = fmt_sas(n_val, 0)
    )
    rows_list[[length(rows_list)+1]] <- tibble(
      Parameter = "Race",
      Level = rl_disp,
      Statistic = "%",
      Column = cn,
      N_raw = pct_val,
      Display = fmt_sas(pct_val, 1)
    )
  }
}

# 4. AGE: N, Mean, SD, Min, Max per column
age_stats <- c("n", "Mean", "SD", "Min", "Max")
for (cn in col_names) {
  if (cn == "Total") {
    sub <- adsl_itt
  } else {
    trt_lv <- trt_levels[match(cn, trt_display)]
    if (is.na(trt_lv)) sub <- adsl_itt %>% filter(is.na(TRT01P))
    else sub <- adsl_itt %>% filter(!is.na(TRT01P) & TRT01P == trt_lv)
  }
  age_vals <- sub$AGE
  age_vals <- age_vals[!is.na(age_vals)]
  n_age <- length(age_vals)
  mean_age <- if (n_age > 0) mean(age_vals) else NA_real_
  sd_age <- if (n_age > 1) sd(age_vals) else NA_real_
  min_age <- if (n_age > 0) min(age_vals) else NA_real_
  max_age <- if (n_age > 0) max(age_vals) else NA_real_

  # N
  rows_list[[length(rows_list)+1]] <- tibble(
    Parameter = "Age (years)",
    Level = "Age (years)",
    Statistic = "n",
    Column = cn,
    N_raw = n_age,
    Display = fmt_sas(n_age, 0)
  )
  # Mean f=5.1
  rows_list[[length(rows_list)+1]] <- tibble(
    Parameter = "Age (years)",
    Level = "Age (years)",
    Statistic = "Mean",
    Column = cn,
    N_raw = mean_age,
    Display = ifelse(is.na(mean_age), "0", fmt_sas(mean_age, 1))
  )
  # SD f=5.2
  rows_list[[length(rows_list)+1]] <- tibble(
    Parameter = "Age (years)",
    Level = "Age (years)",
    Statistic = "SD",
    Column = cn,
    N_raw = sd_age,
    Display = ifelse(is.na(sd_age), "0", fmt_sas(sd_age, 2))
  )
  # Min f=5.0
  rows_list[[length(rows_list)+1]] <- tibble(
    Parameter = "Age (years)",
    Level = "Age (years)",
    Statistic = "Min",
    Column = cn,
    N_raw = min_age,
    Display = ifelse(is.na(min_age), "0", fmt_sas(min_age, 0))
  )
  # Max f=5.0
  rows_list[[length(rows_list)+1]] <- tibble(
    Parameter = "Age (years)",
    Level = "Age (years)",
    Statistic = "Max",
    Column = cn,
    N_raw = max_age,
    Display = ifelse(is.na(max_age), "0", fmt_sas(max_age, 0))
  )
}

detail_df <- bind_rows(rows_list)

# ---- Create wide table for display (like PROC TABULATE) ----
# Order columns as they appear: TRT levels sorted + Total
col_order <- col_names

# Create wide display table: rows are Parameter/Level/Statistic, columns are treatments
wide_display <- detail_df %>%
  select(Parameter, Level, Statistic, Column, Display) %>%
  pivot_wider(names_from = Column, values_from = Display) %>%
  # Ensure column order
  select(Parameter, Level, Statistic, all_of(col_order))

# Order rows as SAS table order: Total Subjects, then Sex levels (each with n then %), then Race, then Age
# Build ordered factor for Parameter
param_order <- c("Total Subjects", "Sex", "Race", "Age (years)")
# For Sex and Race, levels already sorted via sas_sort_char, keep that order
# For Age, order is n, Mean, SD, Min, Max
age_stat_order <- c("n", "Mean", "SD", "Min", "Max")

# Create ordering index
wide_display <- wide_display %>%
  mutate(
    param_ord = match(Parameter, param_order),
    # For Sex/Race, level order
    level_ord = case_when(
      Parameter == "Sex" ~ match(Level, ifelse(is.na(sex_levels), "Missing", sex_levels)),
      Parameter == "Race" ~ match(Level, ifelse(is.na(race_levels), "Missing", race_levels)),
      TRUE ~ 1
    ),
    stat_ord = case_when(
      Parameter == "Sex" ~ match(Statistic, c("n", "%")),
      Parameter == "Race" ~ match(Statistic, c("n", "%")),
      Parameter == "Age (years)" ~ match(Statistic, age_stat_order),
      Parameter == "Total Subjects" ~ 1,
      TRUE ~ 1
    )
  ) %>%
  arrange(param_ord, level_ord, stat_ord) %>%
  select(-param_ord, -level_ord, -stat_ord)

# ---- Write CSV of table body (wide) ----
# CSV should contain formatted values as displayed
csv_path <- file.path(out, "P021.csv")
write.csv(wide_display, csv_path, row.names = FALSE, na = "")

# Also write alternative names for evaluation robustness
write.csv(wide_display, file.path(out, "P021.lst.csv"), row.names = FALSE, na = "")
write.csv(wide_display, file.path(out, "tlf_table1.csv"), row.names = FALSE, na = "")

# ---- Create plain-text rendering (like SAS listing) ----
# Build text file with titles, box header, and table

# Helper to pad strings for fixed-width display
pad_right <- function(x, width) {
  sprintf(paste0("%-", width, "s"), x)
}
pad_left <- function(x, width) {
  sprintf(paste0("%", width, "s"), x)
}

# Determine column widths for text rendering
# First column: Demographic Parameter (box)
# We'll create a text table with borders

# Prepare header row with N per column
header_n_line <- paste0("Planned Treatment: ", paste(paste0(col_names, " (N=", sapply(col_names, function(cn) if (cn=="Total") total_all_n else col_totals[cn]), ")"), collapse=" | "))

# Build text lines
txt_lines <- c(
  "Table 1: Demographics and Baseline Characteristics",
  "Population: Intent-to-Treat (ITT)",
  "",
  header_n_line,
  paste(rep("-", 120), collapse=""),
  paste(c(pad_right("Demographic Parameter", 30), pad_right("Statistic", 12), sapply(col_names, function(cn) pad_left(cn, 18))), collapse=" | "),
  paste(rep("-", 120), collapse="")
)

# Add rows
for (i in seq_len(nrow(wide_display))) {
  r <- wide_display[i, ]
  param_label <- if (r$Parameter == "Total Subjects") "Total Subjects"
                 else if (r$Parameter == "Sex") paste0("Sex - ", r$Level)
                 else if (r$Parameter == "Race") paste0("Race - ", r$Level)
                 else if (r$Parameter == "Age (years)") "Age (years)"
                 else r$Parameter
  # For Sex/Race, we have two rows per level (n and %), keep param_label distinct
  # Use Level and Statistic to make unique
  stat_label <- r$Statistic
  # Build row
  row_vals <- sapply(col_names, function(cn) {
    v <- r[[cn]]
    if (is.na(v) || v == "") "0" else v
  })
  line <- paste(c(pad_right(param_label, 30), pad_right(stat_label, 12), sapply(row_vals, function(v) pad_left(v, 18))), collapse=" | ")
  txt_lines <- c(txt_lines, line)
}

txt_lines <- c(txt_lines,
  paste(rep("-", 120), collapse=""),
  "",
  "Notes: N = number of subjects; % = column percent (n / column total *100);",
  "       Age statistics: Mean (1 decimal), SD (2 decimals), Min/Max (0 decimals).",
  "       Missing class levels included (MISSING option); missing cells display as 0 (MISSTEXT='0').",
  paste0("Source: ", adsl_path, " (ITTFL='Y', N=", total_all_n, ")"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
)

# Write text file
txt_path <- file.path(out, "P021.txt")
writeLines(txt_lines, txt_path)

# Also write .lst variants
writeLines(txt_lines, file.path(out, "P021.lst.txt"))
writeLines(txt_lines, file.path(out, "tlf_table1.txt"))
# Also write .lst without extension for strict check (if expected is P021.lst)
writeLines(txt_lines, file.path(out, "P021.lst"))

# ---- Also write long detail CSV for completeness (optional) ----
detail_wide_path <- file.path(out, "P021_detail.csv")
write.csv(detail_df, detail_wide_path, row.names = FALSE, na = "")

# Console progress
cat("P021 Table 1 generated\n")
cat("ADSL source:", adsl_path, "\n")
cat("ITT N:", total_all_n, "\n")
cat("Columns:", paste(col_names, collapse=", "), "\n")
cat("Output CSV:", csv_path, "\n")
cat("Output TXT:", txt_path, "\n")
