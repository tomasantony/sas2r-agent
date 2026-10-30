```r
# t_exposure.R
# Translation of SAS program P073_t_exposure.sas to R
# Generates t_exposure.csv and t_exposure.txt in OUT_DIR

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# Formatting helpers matching SAS formats
fmt_num <- function(x, digits) {
  if (is.na(x)) return("")
  formatC(round_half_up(x, digits), format = "f", digits = digits, flag = "#")
}

# ----------------------------------------------------------------------
# Setup paths
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# ----------------------------------------------------------------------
# Read ADaM datasets
adsl <- read_sas(file.path(adam_path, "adsl.sas7bdat"))
adex <- read_sas(file.path(adam_path, "adex.sas7bdat"))  # not used later but read for completeness

# ----------------------------------------------------------------------
# Step 1: Big N per treatment (SAFFL = 'Y')
bign_df <- adsl %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, .keep_all = TRUE) %>%
  group_by(TRTA) %>%
  summarise(bign = n(), .groups = "drop") %>%
  arrange(TRTA)

# Total N
bigntot <- adsl %>% filter(SAFFL == "Y") %>% distinct(USUBJID) %>% nrow()

# Build column headers (col1hdr, col2hdr, col3hdr)
trt_levels <- bign_df$TRTA
bign_vals  <- bign_df$bign
col_hdrs   <- paste0(trt_levels, " (N=", bign_vals, ")")
# Ensure exactly three treatment groups (as per &ntrt=3)
if (length(col_hdrs) < 3) col_hdrs <- c(col_hdrs, rep("", 3 - length(col_hdrs)))
col1hdr <- col_hdrs[1]
col2hdr <- col_hdrs[2]
col3hdr <- col_hdrs[3]

# ----------------------------------------------------------------------
# Step 2: Derive exposure data from ADSL (ex_adsl)
ex_adsl <- adsl %>%
  filter(SAFFL == "Y") %>%
  mutate(
    TRTDUR = if (!is.na(TRTEDT) & !is.na(TRTSDT)) as.numeric(TRTEDT - TRTSDT + 1) else NA_real_
  )

# ----------------------------------------------------------------------
# Step 3: Duration continuous statistics
cont_stats <- ex_adsl %>%
  group_by(TRTA) %>%
  summarise(
    n   = n_distinct(USUBJID[!is.na(TRTDUR)]),
    mean = if (n > 0) mean(TRTDUR, na.rm = TRUE) else NA_real_,
    sd   = if (n > 1) sd(TRTDUR, na.rm = TRUE) else NA_real_,
    median = if (n > 0) median(TRTDUR, na.rm = TRUE) else NA_real_,
    q1   = if (n > 0) quantile(TRTDUR, 0.25, na.rm = TRUE) else NA_real_,
    q3   = if (n > 0) quantile(TRTDUR, 0.75, na.rm = TRUE) else NA_real_,
    min  = if (n > 0) min(TRTDUR, na.rm = TRUE) else NA_real_,
    max  = if (n > 0) max(TRTDUR, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

# Build rows for each statistic
stat_labels <- c(
  "n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"
)
stat_order  <- 1:6
dur_stats_rows <- cont_stats %>%
  pivot_longer(cols = -TRTA, names_to = "stat_raw", values_to = "val") %>%
  mutate(
    stat = case_when(
      stat_raw == "n"      ~ "n",
      stat_raw == "mean"   ~ "Mean",
      stat_raw == "sd"     ~ "SD",
      stat_raw == "median" ~ "Median",
      stat_raw == "q1"     ~ "Q1, Q3",
      stat_raw == "q3"     ~ "Q1, Q3",
      stat_raw == "min"    ~ "Min, Max",
      stat_raw == "max"    ~ "Min, Max",
      TRUE                 ~ stat_raw
    ),
    ord = case_when(
      stat == "n"          ~ 1,
      stat == "Mean"       ~ 2,
      stat == "SD"         ~ 3,
      stat == "Median"    ~ 4,
      stat == "Q1, Q3"     ~ 5,
      stat == "Min, Max"  ~ 6
    ),
    # For combined stats (Q1, Q3) and (Min, Max) we need both values
    value = case_when(
      stat == "n"          ~ fmt_num(val, 0),
      stat == "Mean"       ~ fmt_num(val, 2),
      stat == "SD"         ~ fmt_num(val, 2),
      stat == "Median"     ~ fmt_num(val, 1),
      stat == "Q1, Q3"     ~ {
        q1v <- fmt_num(first(val[stat_raw == "q1"]), 1)
        q3v <- fmt_num(first(val[stat_raw == "q3"]), 1)
        if (q1v == "" | q3v == "") "" else paste0(q1v, ", ", q3v)
      },
      stat == "Min, Max"   ~ {
        minv <- fmt_num(first(val[stat_raw == "min"]), 1)
        maxv <- fmt_num(first(val[stat_raw == "max"]), 1)
        if (minv == "" | maxv == "") "" else paste0(minv, ", ", maxv)
      },
      TRUE                 ~ ""
    )
  ) %>%
  filter(!duplicated(paste(TRTA, stat))) %>%   # keep one row per stat per TRTA
  select(TRTA, stat, value, ord) %>%
  mutate(
    category = "Duration of Treatment (days)",
    block = 1
  ) %>%
  arrange(block, ord, TRTA)

# ----------------------------------------------------------------------
# Step 4: Duration categories
dur_cat <- ex_adsl %>%
  mutate(
    DURCAT = case_when(
      is.na(TRTDUR)                     ~ "Missing",
      TRTDUR <= 28                      ~ "<=28 days",
      TRTDUR <= 84                      ~ ">28 to <=84 days",
      TRTDUR <= 168                     ~ ">84 to <=168 days",
      TRTDUR <= 336                     ~ ">168 to <=336 days",
      TRUE                              ~ ">336 days"
    ),
    DURCATN = case_when(
      DURCAT == "<=28 days"            ~ 1L,
      DURCAT == ">28 to <=84 days"     ~ 2L,
      DURCAT == ">84 to <=168 days"    ~ 3L,
      DURCAT == ">168 to <=336 days"   ~ 4L,
      DURCAT == ">336 days"            ~ 5L,
      DURCAT == "Missing"              ~ 6L,
      TRUE                             ~ 99L
    )
  )

# Step 5: Category counts and percentages
durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

durcat_pct <- durcat_cnt %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

# Step 5b: Build formatted rows
durcat_fmt <- durcat_pct %>%
  mutate(
    value = ifelse(cnt > 0,
                   paste0(cnt, " (", fmt_num(pct, 1), ")"),
                   "0"),
    stat = paste0("  ", DURCAT),
    ord = 100 + DURCATN,
    category = "Duration Categories",
    block = 2
  ) %>%
  select(TRTA, category, ord, stat, value, block)

# ----------------------------------------------------------------------
# Step 6: Combine stats and category rows
all_exp <- bind_rows(
  dur_stats_rows %>% rename(value = value) %>% select(TRTA, category, ord, stat, value, block),
  durcat_fmt %>% select(TRTA, category, ord, stat, value, block)
) %>%
  arrange(block, ord, stat, TRTA)

# ----------------------------------------------------------------------
# Step 7: Transpose to wide format (col1, col2, col3)
# Ensure treatment order matches the header order derived earlier
all_exp_wide <- all_exp %>%
  select(TRTA, block, category, ord, stat, value) %>%
  pivot_wider(names_from = TRTA, values_from = value) %>%
  # rename treatment columns to col1/col2/col3 according to trt_levels order
  rename_with(~ paste0("col", match(., trt_levels)), .cols = all_of(trt_levels))

# If any of col1/col2/col3 are missing (e.g., fewer than 3 groups), create empty columns
for (i in 1:3) {
  colname <- paste0("col", i)
  if (!colname %in% names(all_exp_wide)) {
    all_exp_wide[[colname]] <- NA_character_
  }
}
# Reorder columns
final_df <- all_exp_wide %>%
  select(block, category, ord, stat, col1, col2, col3) %>%
  arrange(block, ord, stat)

# ----------------------------------------------------------------------
# Output files
csv_path <- file.path(out_dir, "t_exposure.csv")
txt_path <- file.path(out_dir, "t_exposure.txt")

write.csv(final_df, csv_path, row.names = FALSE, na = "")

# Plain‑text rendering (tab‑separated, no quotes)
write.table(final_df, txt_path, sep = "\t", row.names = FALSE, quote = FALSE, na = "")

# Also write a CSV of the table body (same as above, already done)
# End of script
```