out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## ---- helpers: SAS semantics ----
sas_round <- function(x, dec) {
  # round half away from zero, vectorized, preserves NA
  y <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  xx <- x[ok]
  s <- sign(xx)
  # sign(0)=0 -> keep 0
  y[ok] <- s * floor(abs(xx) * 10^dec + 0.5 + 1e-12) / (10^dec)
  # fix -0
  y[y == 0] <- 0
  y
}
fmt_wd <- function(x, w, d) {
  # SAS w.d format with half-away rounding; missing -> misstext "0"
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return("0")
    r <- sas_round(xi, d)
    fmt <- paste0("%", w, ".", d, "f")
    sprintf(fmt, r)
  }, character(1), USE.NAMES = FALSE)
}
trimws_sas <- function(x) gsub("^ +| +$", "", x)

## ---- locate ADSL ----
adam_dir <- "/bench/data/derived/c2c/data/adam"
rawadam_dir <- "/bench/data/raw/cdisc-pilot/adam"
raw_dir <- "/bench/data/derived/c2c/data/raw"
sdtm_dir <- "/bench/data/derived/c2c/data/sdtm"
search_dirs <- c(adam_dir, rawadam_dir, raw_dir, sdtm_dir)

find_file <- function(dirs, pattern) {
  for (d in dirs) {
    if (!dir.exists(d)) next
    f <- list.files(d, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(f) > 0) return(f)
  }
  return(character(0))
}

adsl_files <- find_file(search_dirs, "^adsl\\.(xpt|sas7bdat|csv)$")
# broader fallback: any file containing adsl
if (length(adsl_files) == 0) {
  adsl_files <- find_file(search_dirs, "adsl")
}
if (length(adsl_files) == 0) {
  stop("ADSL dataset not found in searched directories")
}
adsl_path <- adsl_files[1]
cat("Using ADSL:", adsl_path, "\n")

read_ds <- function(p) {
  lp <- tolower(p)
  if (grepl("\\.xpt$", lp)) {
    df <- haven::read_xpt(p)
  } else if (grepl("\\.sas7bdat$", lp)) {
    df <- haven::read_sas(p)
  } else if (grepl("\\.csv$", lp)) {
    df <- readr::read_csv(p, show_col_types = FALSE)
  } else {
    stop(paste("Unknown extension:", p))
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}
adsl <- read_ds(adsl_path)
names(adsl) <- toupper(names(adsl))
cat("ADSL rows:", nrow(adsl), " cols:", paste(names(adsl), collapse=","), "\n")

stopifnot("ITTFL" %in% names(adsl))
stopifnot("TRT01P" %in% names(adsl))
stopifnot("SEX" %in% names(adsl))
stopifnot("RACE" %in% names(adsl))
stopifnot("AGE" %in% names(adsl))

## ---- filter ITT (SAS: where ITTFL='Y', case-sensitive) ----
# ensure character
adsl$ITTFL <- as.character(adsl$ITTFL)
adsl$TRT01P <- as.character(adsl$TRT01P)
adsl$SEX <- as.character(adsl$SEX)
adsl$RACE <- as.character(adsl$RACE)
adsl$AGE <- suppressWarnings(as.numeric(adsl$AGE))

itt <- adsl[!is.na(adsl$ITTFL) & adsl$ITTFL == "Y", , drop = FALSE]
cat("ITT rows:", nrow(itt), "\n")
stopifnot(nrow(itt) > 0)

## ---- class levels: SAS character ordering (uppercase before lowercase), missing first ----
# helper to sort character levels byte-wise (ASCII: uppercase < lowercase), missing first
sas_sort_levels <- function(v) {
  # v is character vector (may contain NA/"")
  # SAS MISSING: missing is a level, sorts first
  has_missing <- any(is.na(v) | v == "")
  uniq <- unique(v[!is.na(v) & v != ""])
  # byte-order sort: order by raw bytes; approximate with C locale ordering
  # Use xtfrm on UTF-8 bytes: sort via order of charToRaw comparison
  if (length(uniq) > 1) {
    # order using base sort with method radix which is byte-wise and puts uppercase first
    uniq <- sort(uniq, method = "radix")
  }
  if (has_missing) uniq <- c(NA, uniq)
  uniq
}

trt_levels <- sas_sort_levels(itt$TRT01P)
sex_levels <- sas_sort_levels(itt$SEX)
race_levels <- sas_sort_levels(itt$RACE)

# drop NA placeholder if no true missing? keep logic: if first is NA but no missing, remove
# sas_sort_levels only adds NA if missing present, so fine
cat("TRT levels:", paste(trt_levels, collapse=" | "), "\n")
cat("SEX levels:", paste(sex_levels, collapse=" | "), "\n")
cat("RACE levels:", paste(race_levels, collapse=" | "), "\n")

level_label <- function(x) if (is.na(x)) "Missing" else x
trt_labels <- vapply(trt_levels, level_label, character(1))
sex_labels <- vapply(sex_levels, level_label, character(1))
race_labels <- vapply(race_levels, level_label, character(1))

n_trt <- length(trt_levels)

## ---- column subsets ----
# list of row-index vectors per treatment + Total
col_subsets <- lapply(seq_along(trt_levels), function(i) {
  lv <- trt_levels[i]
  if (is.na(lv)) which(is.na(itt$TRT01P) | itt$TRT01P == "")
  else which(!is.na(itt$TRT01P) & itt$TRT01P == lv)
})
names(col_subsets) <- trt_labels
# Total
col_subsets[["Total"]] <- seq_len(nrow(itt))
col_names <- c(trt_labels, "Total")

col_N <- vapply(col_subsets, length, integer(1))

## ---- compute statistics ----
# Total Subjects N
total_N <- col_N

# SEX: N and ColPctN
sex_n_mat <- matrix(NA_real_, nrow = length(sex_levels), ncol = length(col_subsets))
sex_p_mat <- matrix(NA_real_, nrow = length(sex_levels), ncol = length(col_subsets))
for (r in seq_along(sex_levels)) {
  lv <- sex_levels[r]
  for (c in seq_along(col_subsets)) {
    idx <- col_subsets[[c]]
    if (is.na(lv)) n <- sum(is.na(itt$SEX[idx]) | itt$SEX[idx] == "")
    else n <- sum(!is.na(itt$SEX[idx]) & itt$SEX[idx] == lv)
    sex_n_mat[r, c] <- n
    denom <- length(idx)
    if (denom == 0) sex_p_mat[r, c] <- NA_real_
    else sex_p_mat[r, c] <- n / denom * 100
  }
}

# RACE
race_n_mat <- matrix(NA_real_, nrow = length(race_levels), ncol = length(col_subsets))
race_p_mat <- matrix(NA_real_, nrow = length(race_levels), ncol = length(col_subsets))
for (r in seq_along(race_levels)) {
  lv <- race_levels[r]
  for (c in seq_along(col_subsets)) {
    idx <- col_subsets[[c]]
    if (is.na(lv)) n <- sum(is.na(itt$RACE[idx]) | itt$RACE[idx] == "")
    else n <- sum(!is.na(itt$RACE[idx]) & itt$RACE[idx] == lv)
    race_n_mat[r, c] <- n
    denom <- length(idx)
    if (denom == 0) race_p_mat[r, c] <- NA_real_
    else race_p_mat[r, c] <- n / denom * 100
  }
}

# AGE: N Mean Std Min Max
age_n <- numeric(length(col_subsets))
age_mean <- rep(NA_real_, length(col_subsets))
age_std <- rep(NA_real_, length(col_subsets))
age_min <- rep(NA_real_, length(col_subsets))
age_max <- rep(NA_real_, length(col_subsets))
for (c in seq_along(col_subsets)) {
  idx <- col_subsets[[c]]
  vals <- itt$AGE[idx]
  vals <- vals[!is.na(vals)]
  age_n[c] <- length(vals)
  if (length(vals) > 0) {
    age_mean[c] <- mean(vals)
    if (length(vals) > 1) age_std[c] <- stats::sd(vals) else age_std[c] <- NA_real_
    age_min[c] <- min(vals)
    age_max[c] <- max(vals)
  }
}

## ---- format per SAS f= widths ----
f_total_N <- fmt_wd(total_N, 5, 0)
f_sex_n <- matrix(fmt_wd(as.vector(sex_n_mat), 5, 0), nrow = nrow(sex_n_mat))
f_sex_p <- matrix(fmt_wd(as.vector(sex_p_mat), 5, 1), nrow = nrow(sex_p_mat))
f_race_n <- matrix(fmt_wd(as.vector(race_n_mat), 5, 0), nrow = nrow(race_n_mat))
f_race_p <- matrix(fmt_wd(as.vector(race_p_mat), 5, 1), nrow = nrow(race_p_mat))
f_age_n <- fmt_wd(age_n, 5, 0)
f_age_mean <- fmt_wd(age_mean, 5, 1)
f_age_std <- fmt_wd(age_std, 5, 2)
f_age_min <- fmt_wd(age_min, 5, 0)
f_age_max <- fmt_wd(age_max, 5, 0)

## ---- build CSV table body ----
# Columns: PARAMETER, CATEGORY, STATISTIC, <trt...>, Total
PARAMETER <- c()
CATEGORY <- c()
STATISTIC <- c()
mat_rows <- list()
# Total Subjects
PARAMETER <- c(PARAMETER, "Total Subjects")
CATEGORY <- c(CATEGORY, "")
STATISTIC <- c(STATISTIC, "")
mat_rows[[1]] <- f_total_N
# Sex
k <- 2
for (r in seq_along(sex_labels)) {
  PARAMETER <- c(PARAMETER, "Sex", "Sex")
  CATEGORY <- c(CATEGORY, sex_labels[r], sex_labels[r])
  STATISTIC <- c(STATISTIC, "n", "%")
  mat_rows[[k]] <- f_sex_n[r, ]; k <- k + 1
  mat_rows[[k]] <- f_sex_p[r, ]; k <- k + 1
}
# Race
for (r in seq_along(race_labels)) {
  PARAMETER <- c(PARAMETER, "Race", "Race")
  CATEGORY <- c(CATEGORY, race_labels[r], race_labels[r])
  STATISTIC <- c(STATISTIC, "n", "%")
  mat_rows[[k]] <- f_race_n[r, ]; k <- k + 1
  mat_rows[[k]] <- f_race_p[r, ]; k <- k + 1
}
# Age
PARAMETER <- c(PARAMETER, "Age (years)", "Age (years)", "Age (years)", "Age (years)", "Age (years)")
CATEGORY <- c(CATEGORY, "", "", "", "", "")
STATISTIC <- c(STATISTIC, "n", "Mean", "SD", "Min", "Max")
mat_rows[[k]] <- f_age_n; k <- k + 1
mat_rows[[k]] <- f_age_mean; k <- k + 1
mat_rows[[k]] <- f_age_std; k <- k + 1
mat_rows[[k]] <- f_age_min; k <- k + 1
mat_rows[[k]] <- f_age_max; k <- k + 1

mat <- do.call(rbind, mat_rows)
colnames(mat) <- col_names
csv_df <- data.frame(PARAMETER = PARAMETER, CATEGORY = CATEGORY, STATISTIC = STATISTIC, mat,
                     stringsAsFactors = FALSE, check.names = FALSE)
# ensure column names are treatment labels + Total (may contain spaces); keep as is
names(csv_df) <- c("PARAMETER", "CATEGORY", "STATISTIC", col_names)

write.csv(csv_df, file.path(out, "P021.csv"), row.names = FALSE, na = "")

## ---- text rendering (P021.lst / P021.txt) ----
title1 <- "Table 1: Demographics and Baseline Characteristics"
title2 <- "Population: Intent-to-Treat (ITT)"
box_head <- "Demographic Parameter"

# column widths: first col 32, each treatment col 12 (to hold width-5 numbers right aligned)
first_w <- 32
cw <- 12
# header line
pad <- function(s, w, align = "left") {
  s <- as.character(s)
  n <- nchar(s, type = "chars")
  if (n > w) s <- substr(s, 1, w)
  n <- nchar(s, type = "chars")
  if (align == "left") paste0(s, strrep(" ", w - n))
  else paste0(strrep(" ", w - n), s)
}
# build lines
lines <- c()
lines <- c(lines, title1, title2, "")
# column header: box + treatment labels
hdr <- paste0(pad(box_head, first_w, "left"), "|", paste0(vapply(col_names, function(x) pad(x, cw, "right"), character(1)), collapse = "|"), "|")
sep <- strrep("-", nchar(hdr, type = "chars"))
lines <- c(lines, hdr, sep)

row_label <- function(param, cat, stat) {
  if (param == "Total Subjects") return(pad("Total Subjects", first_w, "left"))
  if (param %in% c("Sex", "Race")) {
    if (stat == "n") s <- paste0("  ", cat, " - n")
    else s <- paste0("  ", cat, " - %")
    # prefix param only on first? keep simple: "Sex: F (n)" style
    # Use "Sex, F, n"
    return(pad(s, first_w, "left"))
  }
  if (param == "Age (years)") {
    s <- paste0("Age (years) - ", stat)
    return(pad(s, first_w, "left"))
  }
  pad(param, first_w, "left")
}

# helper to format row cells right-aligned width cw (inner formatted width 5 already padded)
cell <- function(fstr) pad(trimws_sas(fstr), cw, "right")  # trim then right align in cw
# Actually keep SAS 5-width padding inside: use original fstr (width 5) right-aligned in cw
cell2 <- function(fstr) {
  # fstr already width 5; pad to cw
  n <- nchar(fstr, type = "chars")
  if (n < cw) paste0(strrep(" ", cw - n), fstr) else fstr
}

# row 1
lab <- row_label("Total Subjects", "", "")
vals <- mat_rows[[1]]
lines <- c(lines, paste0(lab, "|", paste0(vapply(vals, cell2, character(1)), collapse = "|"), "|"))
# iterate remaining rows in order
idx <- 2
for (i in seq_along(PARAMETER)[-1]) {
  lab <- row_label(PARAMETER[i], CATEGORY[i], STATISTIC[i])
  vals <- mat_rows[[idx]]; idx <- idx + 1
  lines <- c(lines, paste0(lab, "|", paste0(vapply(vals, cell2, character(1)), collapse = "|"), "|"))
}
lines <- c(lines, sep, "")
lines <- c(lines, paste0("N = ", nrow(itt), " randomized subjects (ITTFL='Y')."),
           "Statistics: N (5.0), ColPctN % (5.1), Mean (5.1), SD (5.2), Min/Max (5.0).",
           "Missing statistics displayed as 0 (MISSTEXT). Missing class levels included.")

writeLines(lines, file.path(out, "P021.txt"))
writeLines(lines, file.path(out, "P021.lst"))

cat("Wrote P021.csv with", nrow(csv_df), "rows and", ncol(csv_df), "cols\n")
cat("Text lines:", length(lines), "\n")
