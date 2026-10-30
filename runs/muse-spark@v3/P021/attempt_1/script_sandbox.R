# P021 Table 1: Demographics and Baseline Characteristics (ITT)
# Translates P021_tlf_table1.sas (PROC TABULATE) to R
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
})

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  res <- x
  idx <- !is.na(x) & is.finite(x)
  # half away from zero
  res[idx] <- sign(x[idx]) * floor(abs(x[idx]) * mult + 0.5 + 1e-9) / mult
  res
}
fmt_num <- function(x, digits) {
  sapply(x, function(v) {
    if (is.na(v) || is.nan(v) || is.infinite(v)) return("0")
    rv <- round_half_up(v, digits)
    sprintf(paste0("%.", digits, "f"), rv)
  }, USE.NAMES = FALSE)
}
# SAS character ordering: uppercase before lowercase -> C locale byte order
sas_sort_unique <- function(v) {
  v <- unique(v[!is.na(v)])
  v <- as.character(v)
  if (length(v) <= 1) return(v)
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(try(Sys.setlocale("LC_COLLATE", old), silent = TRUE)), add = TRUE)
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  v[order(v, method = "radix")]
}

adam_dirs <- c(
  "/bench/data/derived/c2c/data/adam",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/derived/cdisc-pilot/adam-split",
  "/bench/data/raw"
)
adsl_file <- NULL
for (d in adam_dirs) {
  if (dir.exists(d)) {
    fl <- list.files(d, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    # prefer files whose basename starts with adsl
    cand <- fl[grepl("^adsl\\.(sas7bdat|xpt|csv)$", basename(fl), ignore.case = TRUE)]
    if (length(cand) > 0) { adsl_file <- cand[1]; break }
  }
}
if (is.null(adsl_file)) stop("ADSL file not found in candidate dirs")
message("Reading ADSL: ", adsl_file)
ext <- tolower(tools::file_ext(adsl_file))
if (ext == "sas7bdat") {
  adsl_raw <- haven::read_sas(adsl_file)
} else if (ext == "xpt") {
  adsl_raw <- haven::read_xpt(adsl_file)
} else if (ext == "csv") {
  adsl_raw <- read.csv(adsl_file, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  stop("Unknown ADSL extension")
}
adsl_raw <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
names(adsl_raw) <- toupper(names(adsl_raw))
stopifnot(all(c("ITTFL","TRT01P","SEX","RACE","AGE") %in% names(adsl_raw)))

# Character comparison case-sensitive; trailing blanks ignored (trim)
to_char_trim <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::as_factor(x, levels = "labels")
  x <- as.character(x)
  # SAS trailing blanks ignored; also trim leading/trailing for display
  trimws(x, which = "both")
}
adsl <- adsl_raw
adsl$ITTFL <- to_char_trim(adsl$ITTFL)
adsl$TRT01P <- to_char_trim(adsl$TRT01P)
adsl$SEX <- to_char_trim(adsl$SEX)
adsl$RACE <- to_char_trim(adsl$RACE)
adsl$AGE <- suppressWarnings(as.numeric(adsl$AGE))

# WHERE ITTFL = 'Y'
itt <- adsl[adsl$ITTFL == "Y" & !is.na(adsl$ITTFL), , drop = FALSE]
message("ITT N=", nrow(itt))
stopifnot(nrow(itt) > 0)

# CLASS with MISSING: keep NA as level sorting first
# For display, NA level shown as blank; here handle generally
has_miss_trt <- any(is.na(itt$TRT01P) | itt$TRT01P == "")
has_miss_sex <- any(is.na(itt$SEX) | itt$SEX == "")
has_miss_race <- any(is.na(itt$RACE) | itt$RACE == "")

trt_levels <- sas_sort_unique(itt$TRT01P[!is.na(itt$TRT01P) & itt$TRT01P != ""])
sex_levels <- sas_sort_unique(itt$SEX[!is.na(itt$SEX) & itt$SEX != ""])
race_levels <- sas_sort_unique(itt$RACE[!is.na(itt$RACE) & itt$RACE != ""])
# missing sorts first
if (has_miss_trt) trt_levels <- c(NA, trt_levels)
if (has_miss_sex) sex_levels <- c(NA, sex_levels)
if (has_miss_race) race_levels <- c(NA, race_levels)

disp <- function(v) { if (is.na(v) || v == "") "" else v }
trt_disp <- sapply(trt_levels, disp, USE.NAMES = FALSE)
sex_disp <- sapply(sex_levels, disp, USE.NAMES = FALSE)
race_disp <- sapply(race_levels, disp, USE.NAMES = FALSE)
# For missing TRT display, use "Missing" label blank? keep "" for fidelity; CSV will show "".
# Column headers: TRT levels + Total
col_headers <- c(trt_disp, "Total")

in_col <- function(trt_val, vec) {
  if (is.na(trt_val)) is.na(vec) else (!is.na(vec) & vec == trt_val)
}

# Column Ns (ALL N per column)
col_N <- sapply(seq_along(trt_levels), function(j) sum(in_col(trt_levels[j], itt$TRT01P)))
total_N <- nrow(itt)
col_N_all <- c(col_N, total_N)

# Build table body rows: Parameter, Category, Statistic, per-column formatted, per-column numeric
rows <- list()
# Total Subjects * N
rows[[length(rows)+1]] <- list(Parameter="Total Subjects", Category="", Statistic="",
  vals = col_N_all, digits=0)
# SEX * (N, ColPctN)
for (s in seq_along(sex_levels)) {
  sv <- sex_levels[s]
  sd <- sex_disp[s]
  n_row <- sapply(seq_along(trt_levels), function(j) {
    sum(in_col(trt_levels[j], itt$TRT01P) & (if (is.na(sv)) is.na(itt$SEX) else (!is.na(itt$SEX) & itt$SEX==sv)))
  })
  n_total <- sum(if (is.na(sv)) is.na(itt$SEX) else (!is.na(itt$SEX) & itt$SEX==sv))
  n_all <- c(n_row, n_total)
  pct_all <- ifelse(col_N_all==0, 0, n_all/col_N_all*100)
  # handle 0/0 -> missing -> misstext 0 ; if col_N==0 pct missing -> "0"
  rows[[length(rows)+1]] <- list(Parameter="Sex", Category=sd, Statistic="n", vals=n_all, digits=0)
  rows[[length(rows)+1]] <- list(Parameter="Sex", Category=sd, Statistic="%", vals=pct_all, digits=1)
}
# RACE * (N, ColPctN)
for (r in seq_along(race_levels)) {
  rv <- race_levels[r]
  rd <- race_disp[r]
  n_row <- sapply(seq_along(trt_levels), function(j) {
    sum(in_col(trt_levels[j], itt$TRT01P) & (if (is.na(rv)) is.na(itt$RACE) else (!is.na(itt$RACE) & itt$RACE==rv)))
  })
  n_total <- sum(if (is.na(rv)) is.na(itt$RACE) else (!is.na(itt$RACE) & itt$RACE==rv))
  n_all <- c(n_row, n_total)
  pct_all <- ifelse(col_N_all==0, 0, n_all/col_N_all*100)
  rows[[length(rows)+1]] <- list(Parameter="Race", Category=rd, Statistic="n", vals=n_all, digits=0)
  rows[[length(rows)+1]] <- list(Parameter="Race", Category=rd, Statistic="%", vals=pct_all, digits=1)
}
# AGE * (N Mean Std Min Max)
age_stats <- function(sub) {
  a <- sub$AGE[!is.na(sub$AGE)]
  n <- length(a)
  mn <- if (n>0) mean(a) else NA_real_
  sdv <- if (n>1) stats::sd(a) else NA_real_
  mi <- if (n>0) min(a) else NA_real_
  ma <- if (n>0) max(a) else NA_real_
  list(n=as.numeric(n), mean=mn, sd=sdv, min=mi, max=ma)
}
age_by_col <- lapply(seq_along(trt_levels), function(j) age_stats(itt[in_col(trt_levels[j], itt$TRT01P), , drop=FALSE]))
age_total <- age_stats(itt)
age_n <- c(sapply(age_by_col, `[[`, "n"), age_total$n)
age_mean <- c(sapply(age_by_col, `[[`, "mean"), age_total$mean)
age_sd <- c(sapply(age_by_col, `[[`, "sd"), age_total$sd)
age_min <- c(sapply(age_by_col, `[[`, "min"), age_total$min)
age_max <- c(sapply(age_by_col, `[[`, "max"), age_total$max)
rows[[length(rows)+1]] <- list(Parameter="Age (years)", Category="", Statistic="n", vals=age_n, digits=0)
rows[[length(rows)+1]] <- list(Parameter="Age (years)", Category="", Statistic="Mean", vals=age_mean, digits=1)
rows[[length(rows)+1]] <- list(Parameter="Age (years)", Category="", Statistic="SD", vals=age_sd, digits=2)
rows[[length(rows)+1]] <- list(Parameter="Age (years)", Category="", Statistic="Min", vals=age_min, digits=0)
rows[[length(rows)+1]] <- list(Parameter="Age (years)", Category="", Statistic="Max", vals=age_max, digits=0)

# Assemble CSV data.frame with formatted (trimmed) values
ncol_out <- length(col_headers)
csv_df <- data.frame(Parameter=character(0), Category=character(0), Statistic=character(0), stringsAsFactors=FALSE)
for (h in col_headers) csv_df[[h]] <- character(0)
# keep numeric for check
for (r in rows) {
  f <- fmt_num(r$vals, r$digits)
  # misstext="0" already handled (NA->"0")
  newrow <- data.frame(Parameter=r$Parameter, Category=r$Category, Statistic=r$Statistic, stringsAsFactors=FALSE, check.names=FALSE)
  for (k in seq_along(col_headers)) newrow[[col_headers[k]]] <- f[k]
  # fix duplicate column names when TRT level equals "Total": use position
  csv_df <- dplyr::bind_rows(csv_df, newrow)
}
# Ensure column names unique: if TRT level named "Total" duplicates last, make unique
names(csv_df) <- c("Parameter","Category","Statistic", col_headers)
# If duplicate names, make unique for write (SAS would have duplicate? unlikely)
if (anyDuplicated(names(csv_df))) names(csv_df) <- make.unique(names(csv_df), sep="_")

write.csv(csv_df, file.path(out, "P021.csv"), row.names = FALSE, na = "")

# Text rendering (P021.txt / P021.lst) mimicking PROC TABULATE listing
# Format with width 5 (f=5.x) right-aligned; header widths
pad5 <- function(fstr) sprintf("%5s", fstr)
# Build display matrix padded
disp_mat <- t(sapply(rows, function(r) pad5(fmt_num(r$vals, r$digits))))
# Column widths: first col wide for labels
label_vec <- sapply(rows, function(r) {
  if (r$Parameter=="Total Subjects") "Total Subjects"
  else if (r$Parameter %in% c("Sex","Race")) paste0("  ", r$Category, " - ", r$Statistic)
  else paste0("  ", r$Statistic)
})
param_col <- c("Demographic Parameter", label_vec)
# header rows
title1 <- "Table 1: Demographics and Baseline Characteristics"
title2 <- "Population: Intent-to-Treat (ITT)"
box_head <- "Demographic Parameter"
# Planned Treatment spans TRT columns
# Determine label width
lab_w <- max(nchar(param_col), nchar(box_head)) + 2
lab_w <- max(lab_w, 24)
col_w <- 5
# Build lines
lines <- c(title1, title2, "")
# header line 1: box + Planned Treatment centered + Total?
# Second header: TRT names
# For simplicity print two header rows
hdr2_labels <- col_headers
# truncate/pad headers to width 5? headers may be longer; expand width per column
hdr_widths <- pmax(5, nchar(hdr2_labels))
hdr_widths <- pmin(hdr_widths, 22)
# data col widths must accommodate both header and data (5)
data_widths <- pmax(5, hdr_widths)
fmt_cell <- function(s, w) sprintf(paste0("%", w, "s"), s)
header1 <- sprintf(paste0("%-", lab_w, "s"), box_head)
for (k in seq_along(col_headers)) header1 <- paste0(header1, " | ", fmt_cell(col_headers[k], data_widths[k]))
lines <- c(lines, header1)
# separator
sep <- paste0(paste(rep("-", lab_w), collapse=""), "")
for (k in seq_along(col_headers)) sep <- paste0(sep, "-+-", paste(rep("-", data_widths[k]), collapse=""))
lines <- c(lines, sep)
# group headers: Sex, Race, Age rows with parameter labels
current_param <- ""
for (i in seq_along(rows)) {
  r <- rows[i][[1]]
  lab <- label_vec[i]
  # insert group header lines for Sex/Race/Age to mimic TABULATE nesting
  # Show parameter as section when first of group: prepend? Already in label; add section line
  line <- sprintf(paste0("%-", lab_w, "s"), lab)
  f <- fmt_num(r$vals, r$digits)
  for (k in seq_along(f)) line <- paste0(line, " | ", fmt_cell(f[k], data_widths[k]))
  lines <- c(lines, line)
  # add section titles to match box grouping: before first Sex row, etc.
}
# Add section label lines? Insert explicit parameter headers for readability (not counted as numbers)
# Prepend group titles by replacing: we already have labels; add blank + group name lines
# Rebuild with group headers for fidelity
lines2 <- c(title1, title2, "", header1, sep)
# Total Subjects
idx <- 1
line <- sprintf(paste0("%-", lab_w, "s"), label_vec[idx])
f <- fmt_num(rows[[idx]]$vals, rows[[idx]]$digits)
for (k in seq_along(f)) line <- paste0(line, " | ", fmt_cell(f[k], data_widths[k]))
lines2 <- c(lines2, line)
# Sex section
lines2 <- c(lines2, sprintf(paste0("%-", lab_w, "s"), "Sex") )
for (i in 2:length(rows)) {
  r <- rows[[i]]
  if (r$Parameter=="Sex" && r$Statistic=="n" && i==2) { }
  if (r$Parameter=="Race" && r$Statistic=="n") {
    # check previous was Sex -> insert Race header once
    prev <- rows[[i-1]]$Parameter
    if (prev=="Sex") lines2 <- c(lines2, sprintf(paste0("%-", lab_w, "s"), "Race"))
  }
  if (r$Parameter=="Age (years)" && r$Statistic=="n") {
    prev <- rows[[i-1]]$Parameter
    if (prev=="Race") lines2 <- c(lines2, sprintf(paste0("%-", lab_w, "s"), "Age (years)"))
  }
  lab <- label_vec[i]
  line <- sprintf(paste0("%-", lab_w, "s"), lab)
  f <- fmt_num(r$vals, r$digits)
  for (k in seq_along(f)) line <- paste0(line, " | ", fmt_cell(f[k], data_widths[k]))
  lines2 <- c(lines2, line)
}
lines2 <- c(lines2, "", "Note: Percentages are column percentages (ColPctN). Missing values included as a level (MISSING option).",
  "N for Age is number of non-missing values; Mean/SD/Min/Max computed on non-missing values.")
writeLines(lines2, file.path(out, "P021.txt"))
# also write .lst counterpart
writeLines(lines2, file.path(out, "P021.lst"))
message("Wrote P021.csv / P021.txt / P021.lst")
