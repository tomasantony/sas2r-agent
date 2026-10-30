```r
# ------------------------------------------------------------
# R translation of SAS program P072_t_dm.sas
# Generates table t_dm (text rendering + CSV) in OUT_DIR
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ----- Helper: round half up (SAS style) -----
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}
fmt_num <- function(x, digits) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

# ----- Output directory -----
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ----- 1. Read ADaM ADSL dataset -----
adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat"
adsl <- read_sas(adsl_path)

# ----- 2. Population counts (Big N) -----
big_n <- adsl %>%
  filter(SAFFL == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

total_n <- adsl %>% filter(SAFFL == "Y") %>% summarise(bign = n_distinct(USUBJID)) %>% pull(bign)

# ----- 3. Prepare analysis dataset (dm) -----
dm <- adsl %>%
  filter(SAFFL == "Y") %>%
  mutate(
    AGEGR1 = case_when(
      AGE < 18               ~ "<18",
      AGE >= 18 & AGE < 65  ~ "18 to <65",
      AGE >= 65              ~ ">=65",
      TRUE                   ~ NA_character_
    ),
    SEXN = case_when(
      SEX == "M" ~ 1L,
      SEX == "F" ~ 2L,
      TRUE       ~ NA_integer_
    ),
    RACEN = case_when(
      RACE == "WHITE"                                 ~ 1L,
      RACE == "BLACK OR AFRICAN AMERICAN"            ~ 2L,
      RACE == "ASIAN"                                 ~ 3L,
      TRUE                                            ~ 4L
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO"          ~ 1L,
      ETHNIC == "NOT HISPANIC OR LATINO"      ~ 2L,
      TRUE                                    ~ 3L
    )
  )

# ----- 4. Continuous statistics macro -----
cont_stats <- function(var, label, ord, decimals = 1) {
  df <- dm %>%
    group_by(TRTA) %>%
    summarise(
      n   = sum(!is.na(.data[[var]])),
      mean = ifelse(n > 0, mean(.data[[var]], na.rm = TRUE), NA_real_),
      sd   = ifelse(n > 1, sd(.data[[var]], na.rm = TRUE), NA_real_),
      median = ifelse(n > 0, median(.data[[var]], na.rm = TRUE), NA_real_),
      q1   = ifelse(n > 0, quantile(.data[[var]], 0.25, na.rm = TRUE, type = 2), NA_real_),
      q3   = ifelse(n > 0, quantile(.data[[var]], 0.75, na.rm = TRUE, type = 2), NA_real_),
      min  = ifelse(n > 0, min(.data[[var]], na.rm = TRUE), NA_real_),
      max  = ifelse(n > 0, max(.data[[var]], na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      block = ord,
      category = label
    ) %>%
    # create rows for each statistic
    pivot_longer(cols = c(n, mean, sd, median, q1, q3, min, max),
                 names_to = "stat_raw", values_to = "val") %>%
    mutate(
      stat = case_when(
        stat_raw == "n"      ~ "n",
        stat_raw == "mean"   ~ "Mean",
        stat_raw == "sd"     ~ "SD",
        stat_raw == "median" ~ "Median",
        stat_raw %in% c("q1","q3") ~ "Q1, Q3",
        stat_raw %in% c("min","max") ~ "Min, Max"
      ),
      ord = case_when(
        stat_raw == "n"      ~ 1L,
        stat_raw == "mean"   ~ 2L,
        stat_raw == "sd"     ~ 3L,
        stat_raw == "median" ~ 4L,
        stat_raw %in% c("q1","q3") ~ 5L,
        stat_raw %in% c("min","max") ~ 6L
      )
    ) %>%
    group_by(TRTA, block, category, stat, ord) %>%
    summarise(
      value = if (stat == "n") {
        as.character(val[stat_raw == "n"])
      } else if (stat %in% c("Mean","SD")) {
        fmt_num(val[stat_raw == tolower(stat)], decimals + 1)
      } else if (stat == "Median") {
        fmt_num(val[stat_raw == "median"], decimals)
      } else if (stat == "Q1, Q3") {
        q1v <- fmt_num(val[stat_raw == "q1"], decimals)
        q3v <- fmt_num(val[stat_raw == "q3"], decimals)
        paste0(q1v, ", ", q3v)
      } else if (stat == "Min, Max") {
        minv <- fmt_num(val[stat_raw == "min"], decimals)
        maxv <- fmt_num(val[stat_raw == "max"], decimals)
        paste0(minv, ", ", maxv)
      } else {
        ""
      },
      .groups = "drop"
    ) %>%
    select(TRTA, block, category, stat, value, ord)
  return(df)
}

# ----- 5. Categorical statistics macro -----
cat_stats <- function(var, label, ord) {
  cnt <- dm %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(TRTA, .data[[var]]) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop")
  cnt <- cnt %>%
    left_join(big_n, by = "TRTA") %>%
    mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))
  cnt <- cnt %>%
    mutate(
      value = ifelse(cnt > 0,
                     paste0(cnt, " (", fmt_num(pct, 1), "%)"),
                     "0"),
      block = ord,
      category = label,
      stat = .data[[var]],
      ord = NA_integer_
    ) %>%
    select(TRTA, block, category, stat, value, ord)
  return(cnt)
}

# ----- 6. Generate statistics for each variable -----
c_age      <- cont_stats("AGE", "Age (years)", ord = 1, decimals = 1)
c_weightbl <- cont_stats("WEIGHTBL", "Weight (kg)", ord = 5, decimals = 1)
c_heightbl <- cont_stats("HEIGHTBL", "Height (cm)", ord = 6, decimals = 1)
c_bmibl    <- cont_stats("BMIBL", "BMI (kg/m^{super 2})", ord = 7, decimals = 1)

f_sex     <- cat_stats("SEX", "Sex", ord = 2)
f_race    <- cat_stats("RACE", "Race", ord = 3)
f_ethnic  <- cat_stats("ETHNIC", "Ethnicity", ord = 4)
f_agegr1  <- cat_stats("AGEGR1", "Age Group", ord = 8)

# ----- 7. Combine all results -----
all_stats <- bind_rows(
  c_age, f_sex, f_race, f_ethnic,
  c_weightbl, c_heightbl, c_bmibl, f_agegr1
)

# ----- 8. Sort as SAS does (by block, ord, stat) -----
all_stats <- all_stats %>%
  arrange(block, ord, stat)

# ----- 9. Transpose to wide format (columns per treatment) -----
wide <- all_stats %>%
  select(TRTA, block, category, stat, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# Determine treatment order from big_n (character sort)
trt_order <- big_n$TRTA
# Rename treatment columns to col1, col2, col3 in that order
col_names <- paste0("col", seq_along(trt_order))
for (i in seq_along(trt_order)) {
  old <- trt_order[i]
  new <- col_names[i]
  if (old %in% names(wide)) {
    names(wide)[names(wide) == old] <- new
  }
}

# ----- 10. Build final report dataset -----
report <- wide %>%
  mutate(
    display_cat = category,
    stat = paste0("  ", stat)   # indent two spaces
  ) %>%
  group_by(block, category) %>%
  mutate(display_cat = ifelse(row_number() == 1, category, " ")) %>%
  ungroup() %>%
  select(block, display_cat, stat, all_of(col_names))

# ----- 11. Column headers (treatment + N) -----
col_hdrs <- paste0(trt_order, " (N=", big_n$bign, ")")
names(col_hdrs) <- col_names

# ----- 12. Write CSV -----
write_csv(report, file.path(out_dir, "t_dm.csv"), na = "")

# ----- 13. Write plain‑text rendering -----
txt_file <- file.path(out_dir, "t_dm.txt")
con <- file(txt_file, open = "wt", encoding = "UTF-8")
# Header line
header_fmt <- "%-25s %-15s %-12s %-12s %-12s"
cat(sprintf(header_fmt,
            "Parameter", "Statistic",
            col_hdrs[1], col_hdrs[2], col_hdrs[3]), "\n", file = con)
cat(strrep("-", 80), "\n", file = con)

# Data lines
row_fmt <- "%-25s %-15s %-12s %-12s %-12s"
apply(report, 1, function(r) {
  cat(sprintf(row_fmt,
              r["display_cat"], r["stat"],
              r[col_names[1]], r[col_names[2]], r[col_names[3]]), "\n", file = con)
})
close(con)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```