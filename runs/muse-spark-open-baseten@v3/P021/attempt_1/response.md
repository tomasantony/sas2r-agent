```r
# P021_tlf_table1.sas -> R translation
# Table 1: Demographics and Baseline Characteristics (ITT)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("OUTDIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- Sys.getenv("project_path")
if (project_path == "" || is.na(project_path)) project_path <- Sys.getenv("PROJECT_PATH")
if (project_path == "" || is.na(project_path)) project_path <- Sys.getenv("PROJECT_PATH")
if (project_path == "" || is.na(project_path)) project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
# fallback to root
root <- Sys.getenv("root")
if (root != "" && !is.na(root) && project_path == "") project_path <- file.path(root, "data/derived/c2c")

rawadam <- Sys.getenv("rawadam")
if (rawadam == "" || is.na(rawadam)) rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

adam_dir <- file.path(project_path, "data", "adam")
# also try alternative locations
candidate_dirs <- c(adam_dir,
                    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam",
                    rawadam,
                    file.path(root, "data/derived/c2c/data/adam"))

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_num <- function(x, digits, width = NULL) {
  if (is.na(x)) return(NA_character_)
  x_r <- round_half_up(x, digits)
  fmt <- paste0("%", if (!is.null(width)) width else "", ".", digits, "f")
  sprintf(fmt, x_r)
}
fmt_int <- function(x, width = 5) {
  if (is.na(x)) return(NA_character_)
  sprintf(paste0("%", width, ".0f"), round_half_up(x, 0))
}

# locate ADSL
find_adsl <- function(dirs) {
  for (d in dirs) {
    if (is.na(d) || d == "") next
    if (!dir.exists(d)) next
    f <- list.files(d, pattern = "^adsl\\.(sas7bdat|xpt|sas7bdat\\.gz|xpt\\.gz|csv)$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE)
    if (length(f) > 0) return(f[1])
    # also try without extension exact
    f2 <- list.files(d, pattern = "^adsl", ignore.case = TRUE, full.names = TRUE)
    if (length(f2) > 0) {
      # prefer sas7bdat/xpt
      pref <- f2[grepl("\\.(sas7bdat|xpt)$", f2, ignore.case = TRUE)]
      if (length(pref) > 0) return(pref[1])
      return(f2[1])
    }
  }
  return(NA_character_)
}
adsl_file <- find_adsl(candidate_dirs)
# also try direct file paths
if (is.na(adsl_file)) {
  try_paths <- c(
    file.path(adam_dir, "adsl.sas7bdat"),
    file.path(adam_dir, "ADSL.sas7bdat"),
    file.path(adam_dir, "adsl.xpt"),
    file.path(adam_dir, "ADSL.xpt"),
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.xpt"
  )
  for (p in try_paths) if (file.exists(p)) { adsl_file <- p; break }
}
if (is.na(adsl_file) || !file.exists(adsl_file)) {
  stop(paste0("ADSL file not found. Searched: ", paste(candidate_dirs, collapse="; ")))
}

# read ADSL
ext <- tolower(tools::file_ext(adsl_file))
if (ext == "sas7bdat") {
  adsl_raw <- haven::read_sas(adsl_file)
} else if (ext == "xpt") {
  adsl_raw <- haven::read_xpt(adsl_file)
} else if (ext == "csv") {
  adsl_raw <- readr::read_csv(adsl_file, show_col_types = FALSE)
} else {
  # try read_sas anyway
  adsl_raw <- haven::read_sas(adsl_file)
}
# normalize column names to uppercase (SAS validvarname=upcase)
names(adsl_raw) <- toupper(names(adsl_raw))

# convert haven_labelled to plain
adsl <- adsl_raw %>%
  mutate(across(everything(), ~ {
    if (haven::is_labelled(.x)) {
      # zap labels then convert to character if character-like, else numeric
      y <- haven::zap_labels(.x)
      # if original was character labelled, keep character
      if (is.character(y)) y else y
    } else .x
  }))

# ensure required columns exist
stopifnot("ITTFL" %in% names(adsl))
stopifnot("TRT01P" %in% names(adsl))
stopifnot("SEX" %in% names(adsl))
stopifnot("RACE" %in% names(adsl))
stopifnot("AGE" %in% names(adsl))

# Filter only randomized subjects (ITT)
adsl_itt <- adsl %>% filter(ITTFL == "Y")

# Helper to handle missing as category (PROC TABULATE MISSING)
# Replace NA with character "Missing" for display, but keep original for counts
# SAS MISSING option treats missing as a valid level
adsl_itt <- adsl_itt %>%
  mutate(
    TRT01P = as.character(TRT01P),
    SEX = as.character(SEX),
    RACE = as.character(RACE),
    AGE = as.numeric(AGE)
  )

# Determine column levels for TRT01P sorted SAS style (uppercase before lowercase, ASCII)
trt_levels <- sort(unique(adsl_itt$TRT01P[!is.na(adsl_itt$TRT01P)]))
# SAS character ordering: uppercase before lowercase, so base sort is fine (ASCII)
# Include missing level if any
has_trt_missing <- any(is.na(adsl_itt$TRT01P))
if (has_trt_missing) trt_levels <- c(trt_levels, NA_character_)

# Column totals (for ColPctN denominator and for ALL row)
col_totals <- sapply(trt_levels, function(lv) {
  if (is.na(lv)) sum(is.na(adsl_itt$TRT01P)) else sum(adsl_itt$TRT01P == lv, na.rm = TRUE)
})
names(col_totals) <- trt_levels
total_all <- nrow(adsl_itt)

# For display, column names: TRT levels + "Total"
col_names_display <- c(trt_levels, "Total")
# For file, keep original TRT values as column headers
# Replace NA label with "Missing"
col_names_display[is.na(col_names_display)] <- "Missing"

# SEX levels (including missing)
sex_levels <- sort(unique(adsl_itt$SEX))
# SAS missing sorts first; haven NA handling: put NA first if present
# sort with NA handling: SAS missing values sort first
# We'll ensure NA first
sex_has_na <- any(is.na(adsl_itt$SEX))
if (sex_has_na) {
  sex_levels_nonmiss <- sort(na.omit(unique(adsl_itt$SEX)))
  sex_levels <- c(NA_character_, sex_levels_nonmiss)
} else {
  sex_levels <- sort(unique(adsl_itt$SEX))
}

race_levels <- sort(unique(adsl_itt$RACE))
race_has_na <- any(is.na(adsl_itt$RACE))
if (race_has_na) {
  race_levels_nonmiss <- sort(na.omit(unique(adsl_itt$RACE)))
  race_levels <- c(NA_character_, race_levels_nonmiss)
} else {
  race_levels <- sort(unique(adsl_itt$RACE))
}

# Function to compute N and ColPctN for a class variable
compute_class_stats <- function(var_name, levels_vec) {
  res <- list()
  for (lv in levels_vec) {
    # per column
    n_per_col <- sapply(trt_levels, function(tv) {
      if (is.na(lv) && is.na(tv)) {
        sum(is.na(adsl_itt[[var_name]]) & is.na(adsl_itt$TRT01P))
      } else if (is.na(lv)) {
        if (is.na(tv)) sum(is.na(adsl_itt[[var_name]]) & is.na(adsl_itt$TRT01P))
        else sum(is.na(adsl_itt[[var_name]]) & adsl_itt$TRT01P == tv, na.rm = TRUE)
      } else if (is.na(tv)) {
        sum(adsl_itt[[var_name]] == lv & is.na(adsl_itt$TRT01P), na.rm = TRUE)
      } else {
        sum(adsl_itt[[var_name]] == lv & adsl_itt$TRT01P == tv, na.rm = TRUE)
      }
    })
    # total column
    n_total <- if (is.na(lv)) sum(is.na(adsl_itt[[var_name]])) else sum(adsl_itt[[var_name]] == lv, na.rm = TRUE)
    # percentages
    pct_per_col <- ifelse(col_totals == 0, 0, n_per_col / col_totals * 100)
    pct_total <- ifelse(total_all == 0, 0, n_total / total_all * 100)
    res[[as.character(ifelse(is.na(lv), "Missing", lv))]] <- list(n = c(n_per_col, n_total), pct = c(pct_per_col, pct_total))
  }
  res
}

sex_stats <- compute_class_stats("SEX", sex_levels)
race_stats <- compute_class_stats("RACE", race_levels)

# AGE stats per column
age_stats <- list()
for (tv in trt_levels) {
  vals <- if (is.na(tv)) adsl_itt$AGE[is.na(adsl_itt$TRT01P)] else adsl_itt$AGE[adsl_itt$TRT01P == tv & !is.na(adsl_itt$TRT01P)]
  vals <- vals[!is.na(vals)]
  age_stats[[ifelse(is.na(tv), "Missing", tv)]] <- vals
}
age_stats[["Total"]] <- adsl_itt$AGE[!is.na(adsl_itt$AGE)]

compute_age_summary <- function(vals) {
  n <- length(vals)
  if (n == 0) return(list(N = 0, Mean = NA_real_, Std = NA_real_, Min = NA_real_, Max = NA_real_))
  m <- mean(vals)
  s <- if (n > 1) sd(vals) else NA_real_  # SAS STD is sample std (N-1)
  list(N = n, Mean = m, Std = s, Min = min(vals), Max = max(vals))
}
age_summaries <- lapply(age_stats, compute_age_summary)

# Build display table for CSV and TXT
# Columns: Demographic_Parameter, Category, Statistic, <TRT levels>, Total
# For Total Subjects: one row
# For SEX/RACE: two rows per level (n and %)
# For AGE: 5 rows

# Prepare column headers for CSV
csv_col_headers <- c("Demographic_Parameter", "Category", "Statistic", col_names_display)

rows_list <- list()

# Total Subjects
total_n_per_col <- c(col_totals, total_all)
# Ensure names align
names(total_n_per_col) <- col_names_display
rows_list[[length(rows_list)+1]] <- tibble(
  Demographic_Parameter = "Total Subjects",
  Category = "Total",
  Statistic = "N",
  !!!as.list(setNames(as.integer(total_n_per_col[col_names_display]), col_names_display))
)

# SEX
for (lv in names(sex_stats)) {
  st <- sex_stats[[lv]]
  # n row
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter = "Sex",
    Category = ifelse(lv == "Missing", "Missing", lv),
    Statistic = "n",
    !!!as.list(setNames(as.integer(st$n), col_names_display))
  )
  # % row
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter = "Sex",
    Category = ifelse(lv == "Missing", "Missing", lv),
    Statistic = "%",
    !!!as.list(setNames(round_half_up(st$pct, 1), col_names_display))
  )
}

# RACE
for (lv in names(race_stats)) {
  st <- race_stats[[lv]]
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter = "Race",
    Category = ifelse(lv == "Missing", "Missing", lv),
    Statistic = "n",
    !!!as.list(setNames(as.integer(st$n), col_names_display))
  )
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter = "Race",
    Category = ifelse(lv == "Missing", "Missing", lv),
    Statistic = "%",
    !!!as.list(setNames(round_half_up(st$pct, 1), col_names_display))
  )
}

# AGE
age_row_labels <- c("n", "Mean", "SD", "Min", "Max")
for (lab in age_row_labels) {
  vals <- sapply(col_names_display, function(cn) {
    # cn corresponds to trt_levels + Total
    key <- cn
    # age_summaries keys are trt_levels (with Missing) + Total
    summ <- age_summaries[[key]]
    if (is.null(summ)) return(NA_real_)
    switch(lab,
           "n" = summ$N,
           "Mean" = summ$Mean,
           "SD" = summ$Std,
           "Min" = summ$Min,
           "Max" = summ$Max)
  })
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter = "Age (years)",
    Category = "",
    Statistic = lab,
    !!!as.list(setNames(vals, col_names_display))
  )
}

table_body <- bind_rows(rows_list)

# Create formatted version for display (apply SAS formats)
# For CSV we keep raw numeric (but percentages already rounded to 1 dec, means to 1, SD to 2)
# For text rendering we format with SAS picture
format_for_display <- function(df) {
  df_disp <- df
  for (cn in col_names_display) {
    df_disp[[cn]] <- sapply(seq_len(nrow(df_disp)), function(i) {
      stat <- df_disp$Statistic[i]
      param <- df_disp$Demographic_Parameter[i]
      val <- df[[cn]][i]
      if (is.na(val)) return("0") # misstext="0"
      if (param == "Total Subjects" && stat == "N") {
        fmt_int(val, 5)
      } else if (param %in% c("Sex", "Race") && stat == "n") {
        fmt_int(val, 5)
      } else if (param %in% c("Sex", "Race") && stat == "%") {
        fmt_num(val, 1, 5)
      } else if (param == "Age (years)") {
        if (stat == "n") fmt_int(val, 5)
        else if (stat == "Mean") fmt_num(val, 1, 5)
        else if (stat == "SD") fmt_num(val, 2, 5)
        else if (stat %in% c("Min", "Max")) fmt_int(val, 5)
        else as.character(val)
      } else {
        as.character(val)
      }
    })
  }
  df_disp
}
table_disp <- format_for_display(table_body)

# Write CSV of table body (numeric, SAS variable names preserved)
# Use raw numeric with same decimals as SAS would display (rounded)
# For percentages, keep 1 decimal; for Mean 1, SD 2, etc.
# Build CSV with formatted numbers? Requirement: numeric values unrounded unless SAS rounds them.
# We'll write the display-formatted numbers trimmed, but also keep numeric CSV
# Provide CSV with formatted display values (as SAS shows) for verification
csv_out_path <- file.path(out, "P021.csv")
# Prepare CSV with character formatted values (to preserve decimals)
# For numeric CSV, we write table_body but with rounded values as they appear
write.csv(table_body, csv_out_path, row.names = FALSE, na = "")

# Also write a second CSV with display formatting for reference (optional)
# write.csv(table_disp, file.path(out, "P021_display.csv"), row.names=FALSE, na="")

# Build plain-text rendering (P021.txt and P021.lst.txt)
title1 <- "Table 1: Demographics and Baseline Characteristics"
title2 <- "Population: Intent-to-Treat (ITT)"
box_label <- "Demographic Parameter"

# Calculate column widths for text table
# Fixed widths: Parameter 25, Category 30, Statistic 8, each treatment col 12
param_w <- 25
cat_w <- 30
stat_w <- 10
col_w <- 12

# Header lines
header_line1 <- sprintf(paste0("%-", param_w, "s %-", cat_w, "s %-", stat_w, "s"), box_label, "", "")
header_line2 <- sprintf(paste0("%-", param_w, "s %-", cat_w, "s %-", stat_w, "s"), "", "", "")
# Column headers
col_header <- sprintf(paste0("%-", param_w, "s %-", cat_w, "s %-", stat_w, "s"), "", "", "")
for (cn in col_names_display) {
  col_header <- paste0(col_header, sprintf(paste0("%", col_w, "s"), cn))
}
# Add "Planned Treatment" grouping label
group_label <- "Planned Treatment"
# Center group label over treatment columns (excluding Total)
n_trt_cols <- length(col_names_display) - 1
group_width <- n_trt_cols * col_w
# We'll create a line with group label
group_line <- sprintf(paste0("%-", param_w, "s %-", cat_w, "s %-", stat_w, "s"), "", "", "")
# pad to align over treatment cols
group_line <- paste0(group_line, sprintf(paste0("%", group_width, "s"), group_label), sprintf(paste0("%", col_w, "s"), ""))

sep_line <- paste(rep("-", param_w + 1 + cat_w + 1 + stat_w + 1 + length(col_names_display)*col_w), collapse="")

# Build rows text
row_lines <- c()
for (i in seq_len(nrow(table_disp))) {
  r <- table_disp[i, ]
  line <- sprintf(paste0("%-", param_w, "s %-", cat_w, "s %-", stat_w, "s"),
                  substr(r$Demographic_Parameter, 1, param_w),
                  substr(r$Category, 1, cat_w),
                  substr(r$Statistic, 1, stat_w))
  for (cn in col_names_display) {
    line <- paste0(line, sprintf(paste0("%", col_w, "s"), r[[cn]]))
  }
  row_lines <- c(row_lines, line)
}

# Assemble full text
txt_content <- c(
  title1,
  title2,
  "",
  group_line,
  col_header,
  sep_line,
  row_lines,
  sep_line,
  "",
  paste0("Note: Percentages are column percentages (ColPctN). N for Age is number of non-missing values."),
  paste0("Population: Intent-to-Treat (ITT) where ITTFL='Y'. Missing categories displayed as 0 per misstext option."),
  ""
)

# Write P021.txt and P021.lst.txt (both for compatibility)
txt_path <- file.path(out, "P021.txt")
lst_path <- file.path(out, "P021.lst")
writeLines(txt_content, txt_path)
writeLines(txt_content, lst_path)
# Also write .txt version of lst
writeLines(txt_content, file.path(out, "P021.lst.txt"))

# Also write dataset CSV for ADSL ITT subset as output dataset (optional, for traceability)
# Required: each output dataset as CSV - we have table body as dataset
# Additionally write the filtered ADSL counts as dataset
adsl_itt_out <- adsl_itt %>% select(any_of(c("USUBJID","TRT01P","SEX","RACE","AGE","ITTFL")))
# Write ISO dates if any date columns present (none here)
write.csv(adsl_itt_out, file.path(out, "P021_adsl_itt.csv"), row.names = FALSE, na = "")

# Console log
cat("P021 Table 1 generated\n")
cat(paste0("ADSL file: ", adsl_file, "\n"))
cat(paste0("ITT N: ", nrow(adsl_itt), "\n"))
cat(paste0("TRT levels: ", paste(col_names_display, collapse=", "), "\n"))
cat(paste0("Output written to: ", out, "\n"))
```