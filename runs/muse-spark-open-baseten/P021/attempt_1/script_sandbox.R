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
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# --- helpers: SAS semantics ---
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_num <- function(x, digits) {
  # SAS f=w.d with misstext="0" -> missing displayed as 0
  if (is.na(x) || is.nan(x) || is.infinite(x)) {
    # for counts/stats, misstext 0 with appropriate decimals
    if (digits == 0) return("0")
    if (digits == 1) return("0.0")
    if (digits == 2) return("0.00")
    return("0")
  }
  y <- round_half_up(x, digits)
  formatC(y, format = "f", digits = digits, flag = "")
}
# SAS character ordering: uppercase before lowercase (ASCII byte order)
sas_sort <- function(v) {
  # v is character vector unique
  # use radix sort which is byte-wise
  sort(v, method = "radix")
}

# --- locate ADSL ---
candidate_dirs <- c(
  "/bench/data/derived/c2c/data/adam",
  "/bench/data/derived/c2c/data/raw",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/raw",
  "/bench/data/derived/cdisc-pilot/adam-split"
)
candidate_files <- c()
for (d in candidate_dirs) {
  if (dir.exists(d)) {
    fs <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    # keep adsl files
    keep <- fs[grepl("adsl", basename(fs), ignore.case = TRUE)]
    candidate_files <- c(candidate_files, keep)
  }
}
# also explicit paths
explicit <- c(
  "/bench/data/derived/c2c/data/adam/adsl.sas7bdat",
  "/bench/data/derived/c2c/data/adam/ADSL.sas7bdat",
  "/bench/data/derived/c2c/data/adam/adsl.xpt",
  "/bench/data/derived/c2c/data/adam/ADSL.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.sas7bdat"
)
candidate_files <- unique(c(explicit, candidate_files))
# filter to existing
candidate_files <- candidate_files[file.exists(candidate_files)]
cat("Candidate ADSL files found:\n")
print(candidate_files)

adsl <- NULL
read_success <- FALSE
for (f in candidate_files) {
  cat("Trying to read:", f, "\n")
  try({
    if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
      tmp <- haven::read_xpt(f)
    } else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
      tmp <- haven::read_sas(f)
    } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
      tmp <- readr::read_csv(f, show_col_types = FALSE)
    } else {
      next
    }
    # uppercase names to mimic SAS validvarname=upcase
    names(tmp) <- toupper(names(tmp))
    if (all(c("USUBJID","TRT01P","SEX","RACE","AGE","ITTFL") %in% names(tmp))) {
      adsl <- tmp
      read_success <- TRUE
      cat("Successfully read ADSL from:", f, "nrow=", nrow(adsl), "\n")
      break
    } else {
      cat("File missing required columns, skipping. Columns:", paste(names(tmp), collapse=","), "\n")
    }
  }, silent = FALSE)
  if (read_success) break
}
if (!read_success || is.null(adsl)) {
  # fallback: search any file in project_path
  pp <- "/bench/data/derived/c2c"
  if (dir.exists(pp)) {
    allf <- list.files(pp, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    allf <- allf[grepl("adsl", basename(allf), ignore.case = TRUE)]
    for (f in allf) {
      if (!file.exists(f)) next
      cat("Fallback trying:", f, "\n")
      try({
        if (grepl("\\.xpt$", f, ignore.case = TRUE)) tmp <- haven::read_xpt(f) else tmp <- haven::read_sas(f)
        names(tmp) <- toupper(names(tmp))
        if ("ITTFL" %in% names(tmp)) { adsl <- tmp; read_success <- TRUE; break }
      }, silent = TRUE)
    }
  }
}
if (is.null(adsl)) stop("ADSL dataset not found. Searched: ", paste(candidate_files, collapse="; "))

# Convert haven labelled to plain vectors
for (nm in names(adsl)) {
  col <- adsl[[nm]]
  if (inherits(col, "haven_labelled")) {
    # keep numeric for AGE, character for others
    if (nm %in% c("AGE")) {
      adsl[[nm]] <- as.numeric(col)
    } else {
      # convert to character using labels where possible, but keep original values
      # haven::as_factor would map labels; we want formatted values if available
      # Use zap_labels then as.character for character vars
      adsl[[nm]] <- haven::zap_labels(col)
      adsl[[nm]] <- as.character(adsl[[nm]])
    }
  }
}
# Ensure character vars are character
for (nm in c("TRT01P","SEX","RACE","ITTFL","USUBJID")) {
  if (nm %in% names(adsl)) adsl[[nm]] <- as.character(adsl[[nm]])
}
adsl$AGE <- as.numeric(adsl$AGE)

# Filter ITTFL='Y' (SAS where ITTFL='Y')
adsl_itt <- adsl %>% filter(ITTFL == "Y")
cat("ITTFL=Y nrow:", nrow(adsl_itt), "\n")

# Handle missing option: include missing class levels
# For TRT01P, SEX, RACE, treat NA as a level if present
# Replace NA with character "Missing" for display, but keep logic for counts
# SAS with missing option would show missing as blank row; we will show as "Missing"

# Determine column groups: TRT01P levels + Total
trt_levels <- unique(adsl_itt$TRT01P)
trt_levels <- trt_levels[!is.na(trt_levels)]
trt_levels <- sas_sort(trt_levels)
cat("TRT01P levels:", paste(trt_levels, collapse=" | "), "\n")

# Determine SEX levels
sex_levels <- unique(adsl_itt$SEX)
# include NA as level if any
has_sex_missing <- any(is.na(adsl_itt$SEX))
sex_levels_nona <- sex_levels[!is.na(sex_levels)]
sex_levels_nona <- sas_sort(sex_levels_nona)
if (has_sex_missing) sex_levels <- c(sex_levels_nona, "Missing") else sex_levels <- sex_levels_nona
cat("SEX levels:", paste(sex_levels, collapse=" | "), "\n")

# Determine RACE levels
race_levels <- unique(adsl_itt$RACE)
has_race_missing <- any(is.na(adsl_itt$RACE))
race_levels_nona <- race_levels[!is.na(race_levels)]
race_levels_nona <- sas_sort(race_levels_nona)
if (has_race_missing) race_levels <- c(race_levels_nona, "Missing") else race_levels <- race_levels_nona
cat("RACE levels:", paste(race_levels, collapse=" | "), "\n")

# Helper to get subset for a column
get_col_subset <- function(trt) {
  if (trt == "Total") return(adsl_itt)
  adsl_itt %>% filter(TRT01P == trt)
}

# Build column list
col_names <- c(trt_levels, "Total")

# Compute statistics per column
# For each col, compute total N, sex counts/pcts, race counts/pcts, age stats
col_stats <- list()
for (cn in col_names) {
  sub <- get_col_subset(cn)
  total_n <- nrow(sub)
  # SEX
  sex_n <- sapply(sex_levels, function(lv) {
    if (lv == "Missing") sum(is.na(sub$SEX))
    else sum(sub$SEX == lv, na.rm = TRUE)
  })
  sex_pct <- if (total_n > 0) sex_n / total_n * 100 else rep(NA_real_, length(sex_n))
  # RACE
  race_n <- sapply(race_levels, function(lv) {
    if (lv == "Missing") sum(is.na(sub$RACE))
    else sum(sub$RACE == lv, na.rm = TRUE)
  })
  race_pct <- if (total_n > 0) race_n / total_n * 100 else rep(NA_real_, length(race_n))
  # AGE
  age_n <- sum(!is.na(sub$AGE))
  age_mean <- if (age_n > 0) mean(sub$AGE, na.rm = TRUE) else NA_real_
  age_sd <- if (age_n > 1) sd(sub$AGE, na.rm = TRUE) else NA_real_
  age_min <- if (age_n > 0) min(sub$AGE, na.rm = TRUE) else NA_real_
  age_max <- if (age_n > 0) max(sub$AGE, na.rm = TRUE) else NA_real_
  col_stats[[cn]] <- list(
    total_n = total_n,
    sex_n = sex_n,
    sex_pct = sex_pct,
    race_n = race_n,
    race_pct = race_pct,
    age_n = age_n,
    age_mean = age_mean,
    age_sd = age_sd,
    age_min = age_min,
    age_max = age_max
  )
}

# Build display table data frame for CSV and text
# Rows: Total Subjects, Sex levels (n, %), Race levels (n, %), Age stats
rows_list <- list()

# Total Subjects row
rows_list[[1]] <- tibble(
  `Demographic Parameter` = "Total Subjects",
  Category = "Total Subjects",
  Statistic = "n"
)
for (cn in col_names) {
  rows_list[[1]][[cn]] <- fmt_num(col_stats[[cn]]$total_n, 0)
}

# Sex rows
idx <- 2
for (lv in sex_levels) {
  # n row
  r_n <- tibble(
    `Demographic Parameter` = "Sex",
    Category = lv,
    Statistic = "n"
  )
  for (cn in col_names) {
    pos <- which(sex_levels == lv)
    r_n[[cn]] <- fmt_num(col_stats[[cn]]$sex_n[pos], 0)
  }
  rows_list[[idx]] <- r_n; idx <- idx + 1
  # % row
  r_p <- tibble(
    `Demographic Parameter` = "Sex",
    Category = lv,
    Statistic = "%"
  )
  for (cn in col_names) {
    pos <- which(sex_levels == lv)
    r_p[[cn]] <- fmt_num(col_stats[[cn]]$sex_pct[pos], 1)
  }
  rows_list[[idx]] <- r_p; idx <- idx + 1
}

# Race rows
for (lv in race_levels) {
  r_n <- tibble(
    `Demographic Parameter` = "Race",
    Category = lv,
    Statistic = "n"
  )
  for (cn in col_names) {
    pos <- which(race_levels == lv)
    r_n[[cn]] <- fmt_num(col_stats[[cn]]$race_n[pos], 0)
  }
  rows_list[[idx]] <- r_n; idx <- idx + 1
  r_p <- tibble(
    `Demographic Parameter` = "Race",
    Category = lv,
    Statistic = "%"
  )
  for (cn in col_names) {
    pos <- which(race_levels == lv)
    r_p[[cn]] <- fmt_num(col_stats[[cn]]$race_pct[pos], 1)
  }
  rows_list[[idx]] <- r_p; idx <- idx + 1
}

# Age rows
age_stats <- c("n","Mean","SD","Min","Max")
age_digits <- c(0,1,2,0,0)
for (i in seq_along(age_stats)) {
  st <- age_stats[i]
  dg <- age_digits[i]
  r <- tibble(
    `Demographic Parameter` = "Age (years)",
    Category = "Age (years)",
    Statistic = st
  )
  for (cn in col_names) {
    val <- switch(st,
      "n" = col_stats[[cn]]$age_n,
      "Mean" = col_stats[[cn]]$age_mean,
      "SD" = col_stats[[cn]]$age_sd,
      "Min" = col_stats[[cn]]$age_min,
      "Max" = col_stats[[cn]]$age_max
    )
    r[[cn]] <- fmt_num(val, dg)
  }
  rows_list[[idx]] <- r; idx <- idx + 1
}

tab_df <- bind_rows(rows_list)

# Also create a version with combined n (%) display for text rendering
# For text, we want to show "n ( %)" style? But SAS shows N and ColPctN as separate columns under each TRT?
# In our CSV we keep them separate rows; for text we will show table with columns as treatments and rows as above.

# Write CSV of table body
csv_path1 <- file.path(out, "P021.csv")
csv_path2 <- file.path(out, "P021.lst.csv")
write.csv(tab_df, csv_path1, row.names = FALSE, na = "")
write.csv(tab_df, csv_path2, row.names = FALSE, na = "")
cat("Wrote CSV to", csv_path1, "and", csv_path2, "\n")

# Also write a wide formatted CSV for plotting data (same)
# Create numeric version for verification
num_df <- tab_df
# keep as is

# --- Text rendering ---
# Build text file mimicking ODS RTF Journal style
txt_path1 <- file.path(out, "P021.txt")
txt_path2 <- file.path(out, "P021.lst.txt")
# Also P021.lst without extension? The spec says P021.lst -> write text rendering
txt_path3 <- file.path(out, "P021.lst")

# Helper to format table as fixed width
# Determine column widths
all_cols <- c("Demographic Parameter","Category","Statistic", col_names)
# Compute widths
col_widths <- sapply(all_cols, function(cn) {
  max(nchar(c(cn, as.character(tab_df[[cn]]))), na.rm = TRUE)
})
# Ensure minimum widths
col_widths <- pmax(col_widths, c(22, 30, 10, rep(12, length(col_names))))
# Actually set specific
# Build header lines
titles <- c(
  "Table 1: Demographics and Baseline Characteristics",
  "Population: Intent-to-Treat (ITT)",
  ""
)

# Build table lines
header_line <- paste0(
  sprintf(paste0("%-", col_widths[1], "s"), all_cols[1]), " | ",
  sprintf(paste0("%-", col_widths[2], "s"), all_cols[2]), " | ",
  sprintf(paste0("%-", col_widths[3], "s"), all_cols[3]), " | ",
  paste(sapply(seq_along(col_names), function(i) {
    cn <- col_names[i]
    w <- col_widths[3+i]
    sprintf(paste0("%", w, "s"), cn)
  }), collapse = " | ")
)
sep_line <- paste0(paste(sapply(col_widths, function(w) paste(rep("-", w), collapse="")), collapse="-+-"), "")

# Build row lines
row_lines <- c()
for (i in 1:nrow(tab_df)) {
  r <- tab_df[i,]
  line <- paste0(
    sprintf(paste0("%-", col_widths[1], "s"), r[[1]]), " | ",
    sprintf(paste0("%-", col_widths[2], "s"), r[[2]]), " | ",
    sprintf(paste0("%-", col_widths[3], "s"), r[[3]]), " | ",
    paste(sapply(col_names, function(cn) {
      w <- col_widths[which(all_cols==cn)]
      sprintf(paste0("%", w, "s"), r[[cn]])
    }), collapse = " | ")
  )
  row_lines <- c(row_lines, line)
}

# Add footnotes with N definitions
footer <- c(
  "",
  "Notes: N = number of subjects; % = column percent (n / Total Subjects *100).",
  "Age statistics: n = non-missing, Mean (1 dec), SD (2 dec), Min/Max (0 dec).",
  "Missing values displayed as 0 per misstext='0'.",
  paste0("ITT population N=", col_stats[["Total"]]$total_n, " (", paste(sapply(col_names, function(cn) paste0(cn, " n=", col_stats[[cn]]$total_n)), collapse=", "), ")")
)

# Count numbers in table (for verification 56 numbers)
# Count numeric cells
num_count <- sum(!is.na(as.numeric(unlist(tab_df[, col_names])))) + nrow(tab_df)*length(col_names) # approximate
# Actually count formatted numbers
all_numbers <- unlist(tab_df[, col_names])
cat("Total formatted numeric cells:", length(all_numbers), "\n")

txt_content <- c(
  titles,
  header_line,
  sep_line,
  row_lines,
  footer
)

# Write to all txt paths
for (p in c(txt_path1, txt_path2, txt_path3)) {
  writeLines(txt_content, con = p)
  cat("Wrote text rendering to", p, "\n")
}

# Also write a detailed listing with n (%) combined for readability
# Create combined display for Sex/Race where n and % are together
combined_rows <- list()
# Total Subjects
combined_rows[[1]] <- tibble(
  Parameter = "Total Subjects",
  Statistic = "N",
  vals = list(sapply(col_names, function(cn) fmt_num(col_stats[[cn]]$total_n,0)))
)
# For each sex level, combine n (%) 
for (lv in sex_levels) {
  pos <- which(sex_levels==lv)
  combined_rows[[length(combined_rows)+1]] <- tibble(
    Parameter = paste0("Sex - ", lv),
    Statistic = "n (%)",
    vals = list(sapply(col_names, function(cn) {
      n <- fmt_num(col_stats[[cn]]$sex_n[pos],0)
      p <- fmt_num(col_stats[[cn]]$sex_pct[pos],1)
      paste0(n, " (", p, "%)")
    }))
  )
}
for (lv in race_levels) {
  pos <- which(race_levels==lv)
  combined_rows[[length(combined_rows)+1]] <- tibble(
    Parameter = paste0("Race - ", lv),
    Statistic = "n (%)",
    vals = list(sapply(col_names, function(cn) {
      n <- fmt_num(col_stats[[cn]]$race_n[pos],0)
      p <- fmt_num(col_stats[[cn]]$race_pct[pos],1)
      paste0(n, " (", p, "%)")
    }))
  )
}
for (i in seq_along(age_stats)) {
  st <- age_stats[i]; dg <- age_digits[i]
  combined_rows[[length(combined_rows)+1]] <- tibble(
    Parameter = paste0("Age (years) - ", st),
    Statistic = st,
    vals = list(sapply(col_names, function(cn) {
      val <- switch(st,
        "n" = col_stats[[cn]]$age_n,
        "Mean" = col_stats[[cn]]$age_mean,
        "SD" = col_stats[[cn]]$age_sd,
        "Min" = col_stats[[cn]]$age_min,
        "Max" = col_stats[[cn]]$age_max
      )
      fmt_num(val, dg)
    }))
  )
}
# This combined view is not written separately but could be appended to txt
# Append to txt for completeness
combined_header <- paste0(sprintf("%-35s", "Parameter"), " | ", paste(sapply(col_names, function(cn) sprintf("%15s", cn)), collapse=" | "))
combined_sep <- paste(rep("-", nchar(combined_header)), collapse="")
combined_lines <- c("", "Combined n (%) view:", combined_header, combined_sep)
for (cr in combined_rows) {
  vals <- cr$vals[[1]]
  line <- paste0(sprintf("%-35s", paste0(cr$Parameter, " [", cr$Statistic, "]")), " | ",
                 paste(sapply(vals, function(v) sprintf("%15s", v)), collapse=" | "))
  combined_lines <- c(combined_lines, line)
}
# Append to txt files
for (p in c(txt_path1, txt_path2, txt_path3)) {
  cat(combined_lines, file = p, sep = "\n", append = TRUE)
}

cat("Done. Outputs in", out, "\n")
