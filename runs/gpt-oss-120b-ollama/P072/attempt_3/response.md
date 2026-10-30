```r
# ------------------------------------------------------------
# R translation of SAS program P072_t_dm.sas
# Generates table t_dm (CSV and plain‑text rendering) in OUT_DIR
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ----- Helper: SAS‑style round half up (vectorised) -----
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}
fmt_num <- function(x, digits) {
  # vectorised formatting, returns empty string for NA
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf(paste0("%.", digits, "f"), round_half_up(v, digits))
  }, USE.NAMES = FALSE)
}

# ----- Output directory -----
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ----- 1. Read ADaM ADSL dataset -----
adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat"
adsl <- read_sas(adsl_path)

# ----- 2. Big N (treatment group counts) -----
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
      AGE >= 18 & AGE < 65   ~ "18 to <65",
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
  stats <- dm %>%
    group_by(TRTA) %>%
    summarise(
      n      = sum(!is.na(.data[[var]])),
      mean   = ifelse(n > 0, mean(.data[[var]], na.rm = TRUE), NA_real_),
      sd     = ifelse(n > 1, sd(.data[[var]], na.rm = TRUE), NA_real_),
      median = ifelse(n > 0, median(.data[[var]], na.rm = TRUE), NA_real_),
      q1     = ifelse(n > 0, quantile(.data[[var]], 0.25, na.rm = TRUE, type = 2), NA_real_),
      q3     = ifelse(n > 0, quantile(.data[[var]], 0.75, na.rm = TRUE, type = 2), NA_real_),
      min    = ifelse(n > 0, min(.data[[var]], na.rm = TRUE), NA_real_),
      max    = ifelse(n > 0, max(.data[[var]], na.rm = TRUE), NA_real_),
      .groups = "drop"
    )

  rows <- bind_rows(
    tibble(TRTA = stats$TRTA,
           stat = "n",
           value = as.character(stats$n),
           ord = 1L),
    tibble(TRTA = stats$TRTA,
           stat = "Mean",
           value = fmt_num(stats$mean, decimals + 1),
           ord = 2L),
    tibble(TRTA = stats$TRTA,
           stat = "SD",
           value = fmt_num(stats$sd, decimals + 1),
           ord = 3L),
    tibble(TRTA = stats$TRTA,
           stat = "Median",
           value = fmt_num(stats$median, decimals),
           ord = 4L),
    tibble(TRTA = stats$TRTA,
           stat = "Q1, Q3",
           value = ifelse(stats$n > 0,
                          paste0(fmt_num(stats$q1, decimals), ", ", fmt_num(stats$q3, decimals)),
                          ""),
           ord = 5L),
    tibble(TRTA = stats$TRTA,
           stat = "Min, Max",
           value = ifelse(stats$n > 0,
                          paste0(fmt_num(stats$min, decimals), ", ", fmt_num(stats$max, decimals)),
                          ""),
           ord = 6L)
  ) %>%
    mutate(block = ord,
           category = label) %>%
    select(TRTA, block, category, stat, value, ord)

  return(rows)
}

# ----- 5. Categorical statistics macro -----
cat_stats <- function(var, label, ord) {
  cnt <- dm %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(TRTA, .data[[var]]) %>%
    summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
    rename(cat = .data[[var]])

  cnt <- cnt %>%
    left_join(big_n, by = "TRTA") %>%
    mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)) %>%
    mutate(
      value = ifelse(cnt > 0,
                     paste0(cnt, " (", fmt_num(pct, 1), "%)"),
                     "0"),
      block = ord,
      category = label,
      stat = cat,
      ord = NA_integer_
    ) %>%
    select(TRTA, block, category, stat, value, ord)

  return(cnt)
}

# ----- 6. Generate statistics for each variable -----
c_age      <- cont_stats("AGE",      "Age (years)",                ord = 1, decimals = 1)
c_weightbl <- cont_stats("WEIGHTBL", "Weight (kg)",                ord = 5, decimals = 1)
c_heightbl <- cont_stats("HEIGHTBL", "Height (cm)",                ord = 6, decimals = 1)
c_bmibl    <- cont_stats("BMIBL",    "BMI (kg/m^{super 2})",       ord = 7, decimals = 1)

f_sex     <- cat_stats("SEX",     "Sex",        ord = 2)
f_race    <- cat_stats("RACE",    "Race",       ord = 3)
f_ethnic  <- cat_stats("ETHNIC",  "Ethnicity",  ord = 4)
f_agegr1  <- cat_stats("AGEGR1",  "Age Group",  ord = 8)

# ----- 7. Combine all results -----
all_stats <- bind_rows(
  c_age, f_sex, f_race, f_ethnic,
  c_weightbl, c_heightbl, c_bmibl, f_agegr1
) %>%
  arrange(block, ord, stat)

# ----- 8. Transpose to wide format (columns per treatment) -----
wide <- all_stats %>%
  select(TRTA, block, category, stat, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# Rename treatment columns to col1‑col3 in the order expected by the SAS report
trt_order_target <- c("Placebo", "Drug X Low Dose", "Drug X High Dose")
col_names_target  <- c("col1", "col2", "col3")
for (i in seq_along(trt_order_target)) {
  old <- trt_order_target[i]
  new <- col_names_target[i]
  if (old %in% names(wide)) {
    names(wide)[names(wide) == old] <- new
  }
}

# ----- 9. Build final report dataset -----
report <- wide %>%
  mutate(
    display_cat = category,
    stat = paste0("  ", stat)   # two‑space indent
  ) %>%
  group_by(block, category) %>%
  mutate(display_cat = ifelse(row_number() == 1, category, " ")) %>%
  ungroup() %>%
  select(block, display_cat, stat, all_of(col_names_target))

# ----- 10. Column headers (treatment + N) -----
col_hdrs <- setNames(
  c(
    paste0("Placebo (N=", big_n %>% filter(TRTA == "Placebo") %>% pull(bign), ")"),
    paste0("Drug X Low Dose (N=", big_n %>% filter(TRTA == "Drug X Low Dose") %>% pull(bign), ")"),
    paste0("Drug X High Dose (N=", big_n %>% filter(TRTA == "Drug X High Dose") %>% pull(bign), ")")
  ),
  col_names_target
)

# ----- 11. Write CSV -----
write_csv(report, file.path(out_dir, "t_dm.csv"), na = "")

# ----- 12. Write plain‑text rendering -----
txt_path <- file.path(out_dir, "t_dm.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")
header_fmt <- "%-25s %-15s %-12s %-12s %-12s"
cat(sprintf(header_fmt,
            "Parameter", "Statistic",
            col_hdrs["col1"], col_hdrs["col2"], col_hdrs["col3"]),
    "\n", file = con)
cat(strrep("-", 80), "\n", file = con)

row_fmt <- "%-25s %-15s %-12s %-12s %-12s"
apply(report, 1, function(r) {
  cat(sprintf(row_fmt,
              r["display_cat"], r["stat"],
              r["col1"], r["col2"], r["col3"]),
      "\n", file = con)
})
close(con)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```