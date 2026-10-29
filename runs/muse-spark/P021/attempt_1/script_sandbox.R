# P021 Table 1: Demographics and Baseline Characteristics (ITT)
# SAS source: P021_tlf_table1.sas  (PROC TABULATE data=adam.adsl where ITTFL='Y')
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

message("P021: starting Table 1")

# ---- helpers: SAS semantics ----
# SAS rounds half away from zero
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  res <- x
  idx <- !is.na(x)
  xx <- x[idx]
  mult <- 10 ^ digits
  # half away from zero
  res[idx] <- sign(xx) * floor(abs(xx) * mult + 0.5 + 1e-12) / mult
  # preserve -0 as 0
  res[idx & res == 0] <- 0
  res
}

# SAS byte-order sort key (uppercase 65-90 before lowercase 97-122), missing first separately
sas_sort_vals <- function(vals) {
  if (length(vals) <= 1) return(vals)
  keys <- vapply(vals, function(s) {
    s <- as.character(s)
    codes <- utf8ToInt(s)
    paste(sprintf("%04d", codes), collapse = "-")
  }, character(1))
  vals[order(keys, method = "radix")]
}

get_class_levels <- function(vec_char) {
  # vec_char: character vector with NA = missing ("" already converted to NA)
  has_missing <- any(is.na(vec_char))
  vals <- unique(vec_char[!is.na(vec_char)])
  vals <- as.character(vals)
  if (length(vals) > 0) vals <- sas_sort_vals(vals)
  if (has_missing) vals <- c(NA_character_, vals)
  vals
}

disp_label <- function(v) {
  if (is.na(v)) return("Missing")
  as.character(v)
}

# SAS formats with width 5; misstext="0" -> exactly "0" for missing
fmt_N_pad <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 0)
  sprintf("%5.0f", r)
}
fmt_PCT_pad <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 1)
  sprintf("%5.1f", r)
}
fmt_MEAN_pad <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 1)
  sprintf("%5.1f", r)
}
fmt_STD_pad <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 2)
  sprintf("%5.2f", r)
}
fmt_MINMAX_pad <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 0)
  sprintf("%5.0f", r)
}
# trimmed versions for CSV (same rounding, no padding)
fmt_N_trim <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 0)
  formatC(r, format = "f", digits = 0, flag = "")
}
fmt_PCT_trim <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 1)
  formatC(r, format = "f", digits = 1, flag = "")
}
fmt_MEAN_trim <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 1)
  formatC(r, format = "f", digits = 1, flag = "")
}
fmt_STD_trim <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 2)
  formatC(r, format = "f", digits = 2, flag = "")
}
fmt_MINMAX_trim <- function(x) {
  if (is.na(x)) return("0")
  r <- round_half_up(x, 0)
  formatC(r, format = "f", digits = 0, flag = "")
}

# ---- locate ADSL ----
find_file <- function(dirs, base) {
  for (d in dirs) {
    if (!dir.exists(d)) next
    # direct + recursive search
    allf <- list.files(d, full.names = TRUE, recursive = TRUE)
    if (length(allf) == 0) next
    pat <- paste0("^", base, "\\.")
    idx <- grepl(pat, basename(allf), ignore.case = TRUE)
    if (any(idx)) return(allf[which(idx)[1]])
  }
  return(NA_character_)
}

adam_dirs <- c(
  "/bench/data/derived/c2c/data/adam",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/derived/cdisc-pilot/adam-split",
  "/bench/data/raw"
)

adsl_file <- find_file(adam_dirs, "adsl")
message(paste0("P021: ADSL file = ", adsl_file))
if (is.na(adsl_file)) stop("ADSL file not found")

read_any <- function(f) {
  lf <- tolower(f)
  if (grepl("\\.xpt$", lf)) {
    haven::read_xpt(f)
  } else if (grepl("\\.sas7bdat$", lf)) {
    haven::read_sas(f)
  } else if (grepl("\\.csv$", lf)) {
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    # try sas then xpt
    tryCatch(haven::read_sas(f), error = function(e) haven::read_xpt(f))
  }
}

adsl_raw <- read_any(adsl_file)
adsl_raw <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
# strip haven formats/labels but keep underlying values
adsl_raw <- tryCatch(haven::zap_formats(adsl_raw), error = function(e) adsl_raw)
adsl_raw <- tryCatch(haven::zap_labels(adsl_raw), error = function(e) adsl_raw)
adsl_raw <- tryCatch(haven::zap_label(adsl_raw), error = function(e) adsl_raw)
names(adsl_raw) <- toupper(names(adsl_raw))

req <- c("ITTFL", "TRT01P", "SEX", "RACE", "AGE")
miss_req <- setdiff(req, names(adsl_raw))
if (length(miss_req) > 0) stop(paste("Missing vars:", paste(miss_req, collapse = ",")))

# normalize character vars: haven may give labelled numeric; coerce
to_char_sas <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(as.character(x))
  x <- as.character(x)
  # SAS missing = blank/empty; convert all-blank or "" to NA
  x[is.na(x)] <- NA_character_
  # trim check for blank
  blank <- !is.na(x) & trimws(x, which = "both") == ""
  x[blank] <- NA_character_
  x
}

adsl_raw$ITTFL <- to_char_sas(adsl_raw$ITTFL)
adsl_raw$TRT01P <- to_char_sas(adsl_raw$TRT01P)
adsl_raw$SEX <- to_char_sas(adsl_raw$SEX)
adsl_raw$RACE <- to_char_sas(adsl_raw$RACE)
adsl_raw$AGE <- suppressWarnings(as.numeric(adsl_raw$AGE))

# WHERE ITTFL='Y' (case-sensitive, missing not selected)
itt <- adsl_raw[!is.na(adsl_raw$ITTFL) & adsl_raw$ITTFL == "Y", , drop = FALSE]
message(paste0("P021: ITT N=", nrow(itt)))

trt_levels <- get_class_levels(itt$TRT01P)
sex_levels <- get_class_levels(itt$SEX)
race_levels <- get_class_levels(itt$RACE)

# columns: trt levels in SAS order, then Total
col_ids <- c(trt_levels, "TOTAL")
col_labels <- c(vapply(trt_levels, disp_label, character(1)), "Total")
# for CSV column names use labels (make unique, keep as is)
# sanitize duplicates slightly
col_names_csv <- col_labels
# ensure uniqueness
for (i in seq_along(col_names_csv)) {
  if (sum(col_names_csv[1:i] == col_names_csv[i]) > 1) {
    col_names_csv[i] <- paste0(col_names_csv[i], "_", i)
  }
}

# helper to subset column
col_subset <- function(trt_val) {
  if (identical(trt_val, "TOTAL")) return(itt)
  if (is.na(trt_val)) return(itt[is.na(itt$TRT01P), , drop = FALSE])
  itt[!is.na(itt$TRT01P) & itt$TRT01P == trt_val, , drop = FALSE]
}

subsets <- lapply(col_ids, col_subset)

# column N (Total Subjects)
col_N <- vapply(subsets, nrow, numeric(1))

# SEX counts and pcts
sex_n_mat <- matrix(0, nrow = length(sex_levels), ncol = length(col_ids))
sex_pct_mat <- matrix(0, nrow = length(sex_levels), ncol = length(col_ids))
for (j in seq_along(subsets)) {
  s <- subsets[[j]]
  denom <- col_N[j]
  for (i in seq_along(sex_levels)) {
    lv <- sex_levels[i]
    if (is.na(lv)) n <- sum(is.na(s$SEX))
    else n <- sum(!is.na(s$SEX) & s$SEX == lv)
    sex_n_mat[i, j] <- n
    if (!is.na(denom) && denom > 0) sex_pct_mat[i, j] <- n / denom * 100
    else sex_pct_mat[i, j] <- NA_real_
  }
}

# RACE counts and pcts
race_n_mat <- matrix(0, nrow = length(race_levels), ncol = length(col_ids))
race_pct_mat <- matrix(0, nrow = length(race_levels), ncol = length(col_ids))
for (j in seq_along(subsets)) {
  s <- subsets[[j]]
  denom <- col_N[j]
  for (i in seq_along(race_levels)) {
    lv <- race_levels[i]
    if (is.na(lv)) n <- sum(is.na(s$RACE))
    else n <- sum(!is.na(s$RACE) & s$RACE == lv)
    race_n_mat[i, j] <- n
    if (!is.na(denom) && denom > 0) race_pct_mat[i, j] <- n / denom * 100
    else race_pct_mat[i, j] <- NA_real_
  }
}

# AGE stats
age_n <- numeric(length(col_ids))
age_mean <- rep(NA_real_, length(col_ids))
age_std <- rep(NA_real_, length(col_ids))
age_min <- rep(NA_real_, length(col_ids))
age_max <- rep(NA_real_, length(col_ids))
for (j in seq_along(subsets)) {
  v <- subsets[[j]]$AGE
  v <- v[!is.na(v)]
  age_n[j] <- length(v)
  if (length(v) > 0) {
    age_mean[j] <- mean(v)
    age_min[j] <- min(v)
    age_max[j] <- max(v)
  }
  if (length(v) > 1) age_std[j] <- stats::sd(v)
  else age_std[j] <- NA_real_
}

# ---- build display tables ----
# CSV body: Parameter, Category, Statistic + treatment columns
param_vec <- c()
cat_vec <- c()
stat_vec <- c()
# matrices for padded and trimmed values per row
pad_rows <- list()
trim_rows <- list()

# Total Subjects N
param_vec <- c(param_vec, "Total Subjects")
cat_vec <- c(cat_vec, "")
stat_vec <- c(stat_vec, "N")
pad_rows[[length(pad_rows) + 1]] <- vapply(col_N, fmt_N_pad, character(1))
trim_rows[[length(trim_rows) + 1]] <- vapply(col_N, fmt_N_trim, character(1))

# Sex
for (i in seq_along(sex_levels)) {
  lab <- disp_label(sex_levels[i])
  # n row
  param_vec <- c(param_vec, "Sex")
  cat_vec <- c(cat_vec, lab)
  stat_vec <- c(stat_vec, "n")
  pad_rows[[length(pad_rows) + 1]] <- vapply(sex_n_mat[i, ], fmt_N_pad, character(1))
  trim_rows[[length(trim_rows) + 1]] <- vapply(sex_n_mat[i, ], fmt_N_trim, character(1))
  # % row
  param_vec <- c(param_vec, "Sex")
  cat_vec <- c(cat_vec, lab)
  stat_vec <- c(stat_vec, "%")
  pad_rows[[length(pad_rows) + 1]] <- vapply(sex_pct_mat[i, ], fmt_PCT_pad, character(1))
  trim_rows[[length(trim_rows) + 1]] <- vapply(sex_pct_mat[i, ], fmt_PCT_trim, character(1))
}

# Race
for (i in seq_along(race_levels)) {
  lab <- disp_label(race_levels[i])
  param_vec <- c(param_vec, "Race")
  cat_vec <- c(cat_vec, lab)
  stat_vec <- c(stat_vec, "n")
  pad_rows[[length(pad_rows) + 1]] <- vapply(race_n_mat[i, ], fmt_N_pad, character(1))
  trim_rows[[length(trim_rows) + 1]] <- vapply(race_n_mat[i, ], fmt_N_trim, character(1))
  param_vec <- c(param_vec, "Race")
  cat_vec <- c(cat_vec, lab)
  stat_vec <- c(stat_vec, "%")
  pad_rows[[length(pad_rows) + 1]] <- vapply(race_pct_mat[i, ], fmt_PCT_pad, character(1))
  trim_rows[[length(trim_rows) + 1]] <- vapply(race_pct_mat[i, ], fmt_PCT_trim, character(1))
}

# Age
param_vec <- c(param_vec, "Age (years)")
cat_vec <- c(cat_vec, "")
stat_vec <- c(stat_vec, "n")
pad_rows[[length(pad_rows) + 1]] <- vapply(age_n, fmt_N_pad, character(1))
trim_rows[[length(trim_rows) + 1]] <- vapply(age_n, fmt_N_trim, character(1))

param_vec <- c(param_vec, "Age (years)")
cat_vec <- c(cat_vec, "")
stat_vec <- c(stat_vec, "Mean")
pad_rows[[length(pad_rows) + 1]] <- vapply(age_mean, fmt_MEAN_pad, character(1))
trim_rows[[length(trim_rows) + 1]] <- vapply(age_mean, fmt_MEAN_trim, character(1))

param_vec <- c(param_vec, "Age (years)")
cat_vec <- c(cat_vec, "")
stat_vec <- c(stat_vec, "SD")
pad_rows[[length(pad_rows) + 1]] <- vapply(age_std, fmt_STD_pad, character(1))
trim_rows[[length(trim_rows) + 1]] <- vapply(age_std, fmt_STD_trim, character(1))

param_vec <- c(param_vec, "Age (years)")
cat_vec <- c(cat_vec, "")
stat_vec <- c(stat_vec, "Min")
pad_rows[[length(pad_rows) + 1]] <- vapply(age_min, fmt_MINMAX_pad, character(1))
trim_rows[[length(trim_rows) + 1]] <- vapply(age_min, fmt_MINMAX_trim, character(1))

param_vec <- c(param_vec, "Age (years)")
cat_vec <- c(cat_vec, "")
stat_vec <- c(stat_vec, "Max")
pad_rows[[length(pad_rows) + 1]] <- vapply(age_max, fmt_MINMAX_pad, character(1))
trim_rows[[length(trim_rows) + 1]] <- vapply(age_max, fmt_MINMAX_trim, character(1))

pad_mat <- do.call(rbind, pad_rows)
trim_mat <- do.call(rbind, trim_rows)
colnames(pad_mat) <- col_labels
colnames(trim_mat) <- col_names_csv

tab_csv <- data.frame(Parameter = param_vec, Category = cat_vec, Statistic = stat_vec,
                      trim_mat, stringsAsFactors = FALSE, check.names = FALSE)

# write CSV of table body
csv_path <- file.path(out, "P021.csv")
write.csv(tab_csv, csv_path, row.names = FALSE, na = "")
message(paste0("P021: wrote ", csv_path))

# ---- plain-text rendering (mimic PROC TABULATE) ----
txt_path <- file.path(out, "P021.txt")
lst_path <- file.path(out, "P021.lst")

# build fixed-width text
# column widths: row label 38, each treatment col 12 (to hold width-5 values with header)
row_label_width <- 38
col_width <- 12
# header labels may be long; truncate/wrap to col_width for separator table; keep full in header line wrap
# For text, show full treatment names in header row (may exceed width -> expand)
# Simplify: use col_width = max(12, max(nchar(col_labels))+2)
col_width <- max(12, max(nchar(col_labels), na.rm = TRUE) + 2)
if (col_width > 22) col_width <- 22

pad_center <- function(s, w) {
  s <- as.character(s)
  if (nchar(s) >= w) return(substr(s, 1, w))
  p <- w - nchar(s)
  left <- floor(p / 2)
  right <- p - left
  paste0(strrep(" ", left), s, strrep(" ", right))
}
pad_right <- function(s, w) {
  s <- as.character(s)
  if (nchar(s) >= w) return(substr(s, 1, w))
  paste0(s, strrep(" ", w - nchar(s)))
}
pad_left <- function(s, w) {
  s <- as.character(s)
  if (nchar(s) >= w) return(s)
  paste0(strrep(" ", w - nchar(s)), s)
}

con <- file(txt_path, open = "wt")
writeLines("Table 1: Demographics and Baseline Characteristics", con)
writeLines("Population: Intent-to-Treat (ITT)", con)
writeLines("", con)
writeLines(paste0("Source: ", adsl_file, "  (ITTFL='Y', N=", nrow(itt), ")"), con)
writeLines("", con)
# box header
hdr_left <- pad_right("Demographic Parameter", row_label_width)
hdr_cols <- paste0(vapply(col_labels, function(x) pad_center(x, col_width), character(1)), collapse = "|")
writeLines(paste0(hdr_left, "|", hdr_cols, "|"), con)
writeLines(paste0(strrep("-", row_label_width), "+", paste(rep(strrep("-", col_width), length(col_labels)), collapse = "+"), "+"), con)
# sub-header for Planned Treatment / Total grouping
sub_left <- pad_right("", row_label_width)
grp <- pad_center("Planned Treatment", col_width * (length(col_labels) - 1) + (length(col_labels) - 2))
# only if more than 1 col; else simple
if (length(col_labels) > 1) {
  writeLines(paste0(sub_left, "|", grp, " |", pad_center("Total", col_width), "|"), con)
  writeLines(paste0(strrep("-", row_label_width), "+", paste(rep(strrep("-", col_width), length(col_labels)), collapse = "+"), "+"), con)
}
# rows
for (r in seq_len(nrow(tab_csv))) {
  if (tab_csv$Category[r] == "" || is.na(tab_csv$Category[r])) {
    rlab <- paste0(tab_csv$Parameter[r], " - ", tab_csv$Statistic[r])
  } else {
    rlab <- paste0(tab_csv$Parameter[r], " - ", tab_csv$Category[r], " - ", tab_csv$Statistic[r])
  }
  left <- pad_right(rlab, row_label_width)
  vals <- pad_mat[r, ]
  # right-align padded values within col_width
  cells <- vapply(vals, function(v) pad_left(v, col_width), character(1))
  writeLines(paste0(left, "|", paste(cells, collapse = "|"), "|"), con)
}
writeLines(paste0(strrep("-", row_label_width), "+", paste(rep(strrep("-", col_width), length(col_labels)), collapse = "+"), "+"), con)
writeLines("", con)
writeLines("Note: N = number of subjects; % = column percent (ColPctN, 1 decimal);", con)
writeLines("Age n/Mean (1 decimal)/SD (2 decimals)/Min/Max. Missing levels included (MISSING option).", con)
writeLines("Misstext='0' displayed as 0 for missing statistics. Formats: N/Min/Max 5.0, Mean/% 5.1, SD 5.2.", con)
close(con)
# copy to .lst for SAS counterpart
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))
message(paste0("P021: wrote ", txt_path, " and ", lst_path))
message("P021: done")
