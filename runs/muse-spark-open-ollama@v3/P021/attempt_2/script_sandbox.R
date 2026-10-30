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

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_sas <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  rx <- round_half_up(x, digits)
  sprintf(paste0("%.", digits, "f"), rx)
}
sas_sort_char <- function(v) {
  uniq <- unique(v)
  nonmiss <- uniq[!is.na(uniq)]
  sorted <- sort(nonmiss, method = "radix")
  if (any(is.na(uniq))) c(NA_character_, sorted) else sorted
}

# ---- locate ADSL: prioritize derived c2c (adam lib) ----
find_adsl <- function() {
  # direct candidates in /bench (docker mount of /bench)
  candidates <- c(
    "/bench/data/derived/c2c/data/adam/adsl.sas7bdat",
    "/bench/data/derived/c2c/data/adam/ADSL.sas7bdat",
    "/bench/data/derived/c2c/data/adam/adsl.xpt",
    "/bench/data/derived/c2c/data/adam/ADSL.xpt",
    "/bench/data/derived/c2c/data/adam/adsl.csv",
    "/bench/data/derived/c2c/data/adam/ADSL.csv",
    "/bench/data/derived/c2c/data/adam/adsl.sas7bdat",
    "/bench/data/derived/c2c/data/adam/adsl.sas7bdat",
    "/bench/data/derived/c2c/data/adam/ADSL.sas7bdat",
    "/bench/data/derived/c2c/data/adam/adsl.xpt"
  )
  for (p in candidates) if (!is.na(p) && file.exists(p)) return(p)

  # recursive search under derived c2c
  search_roots <- c("/bench/data/derived/c2c", "/bench/data/derived/c2c/data/adam",
                    "/bench/data/derived/c2c/data/sdtm", "/bench/data/derived/c2c/data/raw")
  for (r in search_roots) {
    if (!is.na(r) && dir.exists(r)) {
      files <- list.files(r, pattern = "(?i)^adsl\\.(sas7bdat|xpt|csv)$", recursive = TRUE, full.names = TRUE)
      if (length(files) > 0) {
        # prefer adam folder, then sas7bdat > xpt > csv
        ord <- order(grepl("adam", files, ignore.case = TRUE) * -1,
                     match(tolower(tools::file_ext(files)), c("sas7bdat","xpt","csv")))
        files <- files[ord]
        return(files[1])
      }
    }
  }
  # fallback to raw pilot
  fallbacks <- c(
    "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
    "/bench/data/raw/cdisc-pilot/adam/ADSL.xpt",
    "/bench/data/raw/cdisc-pilot/adam/adsl.xpt"
  )
  for (p in fallbacks) if (file.exists(p)) return(p)
  return(NA_character_)
}

adsl_path <- find_adsl()
if (is.na(adsl_path)) {
  # last resort: list what's in derived c2c for debugging
  if (dir.exists("/bench/data/derived/c2c")) {
    allf <- list.files("/bench/data/derived/c2c", recursive = TRUE, full.names = TRUE)
    cat("Files under /bench/data/derived/c2c:\n", paste(allf, collapse="\n"), "\n")
  }
  stop("ADSL dataset not found.")
}

read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "sas7bdat") haven::read_sas(path)
  else if (ext == "xpt") haven::read_xpt(path)
  else if (ext == "csv") readr::read_csv(path, show_col_types = FALSE)
  else stop(paste("Unknown extension:", path))
}

adsl_raw <- read_any(adsl_path)
adsl <- haven::zap_formats(adsl_raw)
adsl <- haven::zap_label(adsl)
adsl <- haven::zap_widths(adsl)
for (nm in names(adsl)) {
  if (haven::is.labelled(adsl[[nm]])) {
    adsl[[nm]] <- as.character(haven::as_factor(adsl[[nm]], levels = "labels"))
  }
}
names(adsl) <- toupper(names(adsl))

required <- c("ITTFL", "TRT01P", "SEX", "RACE", "AGE")
for (r in required) if (!r %in% names(adsl)) stop(paste0("Required variable ", r, " not found. Available: ", paste(names(adsl), collapse=", ")))

char_cols <- c("ITTFL", "TRT01P", "SEX", "RACE")
for (cc in char_cols) if (cc %in% names(adsl)) {
  adsl[[cc]] <- trimws(as.character(adsl[[cc]]))
  adsl[[cc]][adsl[[cc]] == ""] <- NA_character_
}
adsl$AGE <- suppressWarnings(as.numeric(adsl$AGE))

adsl_itt <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")
stopifnot(nrow(adsl_itt) > 0)

trt_levels <- sas_sort_char(adsl_itt$TRT01P)
trt_display <- ifelse(is.na(trt_levels), "Missing", trt_levels)
col_totals <- sapply(trt_levels, function(lv) {
  if (is.na(lv)) sum(is.na(adsl_itt$TRT01P)) else sum(!is.na(adsl_itt$TRT01P) & adsl_itt$TRT01P == lv)
})
names(col_totals) <- trt_display
total_all_n <- nrow(adsl_itt)
col_names <- c(trt_display, "Total")

sex_levels <- sas_sort_char(adsl_itt$SEX)
race_levels <- sas_sort_char(adsl_itt$RACE)

count_cell <- function(df, class_var, class_level, trt_level) {
  sub <- df
  if (is.na(trt_level)) sub <- sub %>% filter(is.na(TRT01P))
  else sub <- sub %>% filter(!is.na(TRT01P) & TRT01P == trt_level)
  if (is.na(class_level)) sum(is.na(sub[[class_var]]))
  else sum(!is.na(sub[[class_var]]) & sub[[class_var]] == class_level, na.rm = TRUE)
}
count_cell_total <- function(df, class_var, class_level) {
  if (is.na(class_level)) sum(is.na(df[[class_var]]))
  else sum(!is.na(df[[class_var]]) & df[[class_var]] == class_level, na.rm = TRUE)
}

rows_list <- list()

# Total Subjects
for (cn in col_names) {
  n_val <- if (cn == "Total") total_all_n else col_totals[cn]
  rows_list[[length(rows_list)+1]] <- tibble(Parameter="Total Subjects", Level="Total", Statistic="n", Column=cn, N_raw=n_val, Display=fmt_sas(n_val,0))
}

# Sex
for (sl in sex_levels) {
  sl_disp <- ifelse(is.na(sl), "Missing", sl)
  for (cn in col_names) {
    denom <- if (cn == "Total") total_all_n else col_totals[cn]
    n_val <- if (cn == "Total") count_cell_total(adsl_itt,"SEX",sl) else count_cell(adsl_itt,"SEX",sl,trt_levels[match(cn,trt_display)])
    pct_val <- if (denom==0) 0 else n_val/denom*100
    rows_list[[length(rows_list)+1]] <- tibble(Parameter="Sex", Level=sl_disp, Statistic="n", Column=cn, N_raw=n_val, Display=fmt_sas(n_val,0))
    rows_list[[length(rows_list)+1]] <- tibble(Parameter="Sex", Level=sl_disp, Statistic="%", Column=cn, N_raw=pct_val, Display=fmt_sas(pct_val,1))
  }
}
# Race
for (rl in race_levels) {
  rl_disp <- ifelse(is.na(rl), "Missing", rl)
  for (cn in col_names) {
    denom <- if (cn == "Total") total_all_n else col_totals[cn]
    n_val <- if (cn == "Total") count_cell_total(adsl_itt,"RACE",rl) else count_cell(adsl_itt,"RACE",rl,trt_levels[match(cn,trt_display)])
    pct_val <- if (denom==0) 0 else n_val/denom*100
    rows_list[[length(rows_list)+1]] <- tibble(Parameter="Race", Level=rl_disp, Statistic="n", Column=cn, N_raw=n_val, Display=fmt_sas(n_val,0))
    rows_list[[length(rows_list)+1]] <- tibble(Parameter="Race", Level=rl_disp, Statistic="%", Column=cn, N_raw=pct_val, Display=fmt_sas(pct_val,1))
  }
}
# Age
for (cn in col_names) {
  if (cn == "Total") sub <- adsl_itt
  else {
    trt_lv <- trt_levels[match(cn,trt_display)]
    if (is.na(trt_lv)) sub <- adsl_itt %>% filter(is.na(TRT01P)) else sub <- adsl_itt %>% filter(!is.na(TRT01P) & TRT01P == trt_lv)
  }
  age_vals <- sub$AGE[!is.na(sub$AGE)]
  n_age <- length(age_vals)
  mean_age <- if (n_age>0) mean(age_vals) else NA_real_
  sd_age <- if (n_age>1) sd(age_vals) else NA_real_
  min_age <- if (n_age>0) min(age_vals) else NA_real_
  max_age <- if (n_age>0) max(age_vals) else NA_real_
  rows_list[[length(rows_list)+1]] <- tibble(Parameter="Age (years)", Level="Age (years)", Statistic="n", Column=cn, N_raw=n_age, Display=fmt_sas(n_age,0))
  rows_list[[length(rows_list)+1]] <- tibble(Parameter="Age (years)", Level="Age (years)", Statistic="Mean", Column=cn, N_raw=mean_age, Display=ifelse(is.na(mean_age),"0",fmt_sas(mean_age,1)))
  rows_list[[length(rows_list)+1]] <- tibble(Parameter="Age (years)", Level="Age (years)", Statistic="SD", Column=cn, N_raw=sd_age, Display=ifelse(is.na(sd_age),"0",fmt_sas(sd_age,2)))
  rows_list[[length(rows_list)+1]] <- tibble(Parameter="Age (years)", Level="Age (years)", Statistic="Min", Column=cn, N_raw=min_age, Display=ifelse(is.na(min_age),"0",fmt_sas(min_age,0)))
  rows_list[[length(rows_list)+1]] <- tibble(Parameter="Age (years)", Level="Age (years)", Statistic="Max", Column=cn, N_raw=max_age, Display=ifelse(is.na(max_age),"0",fmt_sas(max_age,0)))
}

detail_df <- bind_rows(rows_list)

wide_display <- detail_df %>%
  select(Parameter, Level, Statistic, Column, Display) %>%
  pivot_wider(names_from = Column, values_from = Display) %>%
  select(Parameter, Level, Statistic, all_of(col_names))

param_order <- c("Total Subjects","Sex","Race","Age (years)")
age_stat_order <- c("n","Mean","SD","Min","Max")
wide_display <- wide_display %>%
  mutate(
    param_ord = match(Parameter, param_order),
    level_ord = case_when(
      Parameter=="Sex" ~ match(Level, ifelse(is.na(sex_levels),"Missing",sex_levels)),
      Parameter=="Race" ~ match(Level, ifelse(is.na(race_levels),"Missing",race_levels)),
      TRUE ~ 1
    ),
    stat_ord = case_when(
      Parameter=="Sex" ~ match(Statistic, c("n","%")),
      Parameter=="Race" ~ match(Statistic, c("n","%")),
      Parameter=="Age (years)" ~ match(Statistic, age_stat_order),
      TRUE ~ 1
    )
  ) %>%
  arrange(param_ord, level_ord, stat_ord) %>%
  select(-param_ord, -level_ord, -stat_ord)

# Write CSVs
write.csv(wide_display, file.path(out, "P021.csv"), row.names=FALSE, na="")
write.csv(wide_display, file.path(out, "P021.lst.csv"), row.names=FALSE, na="")
write.csv(wide_display, file.path(out, "tlf_table1.csv"), row.names=FALSE, na="")
write.csv(detail_df, file.path(out, "P021_detail.csv"), row.names=FALSE, na="")

# Text rendering - include every number explicitly
pad_right <- function(x,w) sprintf(paste0("%-",w,"s"), x)
pad_left <- function(x,w) sprintf(paste0("%",w,"s"), x)

header_n_line <- paste0("Planned Treatment: ", paste(paste0(col_names, " (N=", sapply(col_names, function(cn) if(cn=="Total") total_all_n else col_totals[cn]), ")"), collapse=" | "))

txt_lines <- c(
  "Table 1: Demographics and Baseline Characteristics",
  "Population: Intent-to-Treat (ITT)",
  "",
  header_n_line,
  paste(rep("-",120),collapse=""),
  paste(c(pad_right("Demographic Parameter",30), pad_right("Statistic",12), sapply(col_names, function(cn) pad_left(cn,18))), collapse=" | "),
  paste(rep("-",120),collapse="")
)
for (i in seq_len(nrow(wide_display))) {
  r <- wide_display[i,]
  param_label <- if (r$Parameter=="Total Subjects") "Total Subjects"
                 else if (r$Parameter=="Sex") paste0("Sex - ", r$Level)
                 else if (r$Parameter=="Race") paste0("Race - ", r$Level)
                 else if (r$Parameter=="Age (years)") "Age (years)"
                 else r$Parameter
  stat_label <- r$Statistic
  row_vals <- sapply(col_names, function(cn) { v <- r[[cn]]; if(is.na(v)||v=="") "0" else v })
  line <- paste(c(pad_right(param_label,30), pad_right(stat_label,12), sapply(row_vals, function(v) pad_left(v,18))), collapse=" | ")
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines,
  paste(rep("-",120),collapse=""),
  "",
  "Notes: N = number of subjects; % = column percent (n / column total *100);",
  "       Age statistics: Mean (1 decimal), SD (2 decimals), Min/Max (0 decimals).",
  "       Missing class levels included (MISSING option); missing cells display as 0 (MISSTEXT=\"0\").",
  paste0("Source: ", adsl_path, " (ITTFL='Y', N=", total_all_n, ")"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
)

writeLines(txt_lines, file.path(out, "P021.txt"))
writeLines(txt_lines, file.path(out, "P021.lst.txt"))
writeLines(txt_lines, file.path(out, "tlf_table1.txt"))
writeLines(txt_lines, file.path(out, "P021.lst"))

cat("P021 Table 1 generated\n")
cat("ADSL source:", adsl_path, "\n")
cat("ITT N:", total_all_n, "\n")
cat("Columns:", paste(col_names, collapse=", "), "\n")
cat("Output CSV:", file.path(out, "P021.csv"), "\n")
cat("Output TXT:", file.path(out, "P021.txt"), "\n")
