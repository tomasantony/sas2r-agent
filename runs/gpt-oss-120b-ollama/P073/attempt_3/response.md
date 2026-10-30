```r
# t_exposure.R
# Translation of SAS program P073_t_exposure.sas to R
# Generates t_exposure.csv and t_exposure.txt in OUT_DIR

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------------------------------------------------
# Helper functions ------------------------------------------------------
# SAS rounds half away from zero (round half up)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}
# Format numeric values exactly as SAS would (no extra spaces)
fmt_num <- function(x, digits) {
  if (is.na(x)) return("")
  fmt <- formatC(round_half_up(x, digits), format = "f", digits = digits, flag = "#")
  str_trim(fmt, side = "both")
}
# ----------------------------------------------------------------------
# Paths ---------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# ----------------------------------------------------------------------
# Read ADaM datasets ---------------------------------------------------
adsl <- read_sas(file.path(adam_path, "adsl.sas7bdat"))
# adex is not used for the final table but read for completeness
# adex <- read_sas(file.path(adam_path, "adex.sas7bdat"))

# ----------------------------------------------------------------------
# Step 1 – Big N per treatment (SAFFL = 'Y') ---------------------------
bign_df <- adsl %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, .keep_all = TRUE) %>%
  group_by(TRTA) %>%
  summarise(bign = n(), .groups = "drop") %>%
  arrange(TRTA)                     # same ordering as PROC SQL ORDER BY TRTA

bigntot <- adsl %>% filter(SAFFL == "Y") %>% distinct(USUBJID) %>% nrow()

# Build column header strings (col1hdr, col2hdr, col3hdr) – order matches alphabetical TRTA
trt_levels <- bign_df$TRTA
col_hdrs   <- paste0(trt_levels, " (N=", bign_df$bign, ")")
while (length(col_hdrs) < 3) col_hdrs <- c(col_hdrs, "")
col1hdr <- col_hdrs[1]
col2hdr <- col_hdrs[2]
col3hdr <- col_hdrs[3]

# ----------------------------------------------------------------------
# Step 2 – Exposure data derived from ADSL ------------------------------
ex_adsl <- adsl %>%
  filter(SAFFL == "Y") %>%
  mutate(
    TRTDUR = if_else(!is.na(TRTEDT) & !is.na(TRTSDT),
                     as.numeric(TRTEDT - TRTSDT + 1),
                     NA_real_)
  )

# ----------------------------------------------------------------------
# Step 3 – Continuous statistics for TRTDUR -------------------------
cont_stats <- ex_adsl %>%
  group_by(TRTA) %>%
  summarise(
    n      = n_distinct(USUBJID[!is.na(TRTDUR)]),
    mean   = ifelse(n > 0, mean(TRTDUR, na.rm = TRUE), NA_real_),
    sd     = ifelse(n > 1, sd(TRTDUR, na.rm = TRUE), NA_real_),
    median = ifelse(n > 0, median(TRTDUR, na.rm = TRUE), NA_real_),
    q1     = ifelse(n > 0, quantile(TRTDUR, 0.25, na.rm = TRUE), NA_real_),
    q3     = ifelse(n > 0, quantile(TRTDUR, 0.75, na.rm = TRUE), NA_real_),
    min    = ifelse(n > 0, min(TRTDUR, na.rm = TRUE), NA_real_),
    max    = ifelse(n > 0, max(TRTDUR, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# Build rows for each statistic, matching SAS order and formatting
stat_rows <- cont_stats %>%
  rowwise() %>%
  mutate(
    n_fmt      = fmt_num(n, 0),
    mean_fmt   = fmt_num(mean, 2),
    sd_fmt     = fmt_num(sd, 2),
    median_fmt = fmt_num(median, 1),
    q1q3_fmt   = if (!is.na(q1) && !is.na(q3))
                   paste0(fmt_num(q1, 1), ", ", fmt_num(q3, 1)) else "",
    minmax_fmt = if (!is.na(min) && !is.na(max))
                   paste0(fmt_num(min, 1), ", ", fmt_num(max, 1)) else ""
  ) %>%
  select(TRTA,
         n_fmt, mean_fmt, sd_fmt, median_fmt, q1q3_fmt, minmax_fmt) %>%
  pivot_longer(-TRTA, names_to = "stat", values_to = "value") %>%
  mutate(
    stat = case_when(
      stat == "n_fmt"      ~ "n",
      stat == "mean_fmt"   ~ "Mean",
      stat == "sd_fmt"     ~ "SD",
      stat == "median_fmt" ~ "Median",
      stat == "q1q3_fmt"   ~ "Q1, Q3",
      stat == "minmax_fmt" ~ "Min, Max",
      TRUE                 ~ stat
    ),
    ord = case_when(
      stat == "n"          ~ 1,
      stat == "Mean"       ~ 2,
      stat == "SD"         ~ 3,
      stat == "Median"     ~ 4,
      stat == "Q1, Q3"     ~ 5,
      stat == "Min, Max"   ~ 6,
      TRUE                 ~ 99
    ),
    block = 1,
    category = "Duration of Treatment (days)"
  ) %>%
  select(TRTA, block, category, ord, stat, value)

# ----------------------------------------------------------------------
# Step 4 – Duration categories -------------------------------------------
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

# Step 5 – Category counts and percentages -------------------------------
durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

durcat_pct <- durcat_cnt %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

# Build formatted rows for categories
durcat_fmt <- durcat_pct %>%
  mutate(
    value = ifelse(cnt > 0,
                   paste0(cnt, " (", fmt_num(pct, 1), ")"),
                   "0"),
    stat = paste0("  ", DURCAT),
    ord = 100 + DURCATN,
    block = 2,
    category = "Duration Categories"
  ) %>%
  select(TRTA, block, category, ord, stat, value)

# ----------------------------------------------------------------------
# Step 6 – Combine continuous and categorical rows ----------------------
all_exp <- bind_rows(stat_rows, durcat_fmt) %>%
  arrange(block, ord, stat, TRTA)

# ----------------------------------------------------------------------
# Step 7 – Transpose to wide format (col1‑col3) -----------------------
# Preserve original treatment labels for column mapping
wide <- all_exp %>%
  select(TRTA, block, category, ord, stat, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# Map specific treatment names to col1‑col3 as SAS does via PROC TRANSPOSE rename
wide$col1 <- if ("Placebo" %in% names(wide)) wide[["Placebo"]] else ""
wide$col2 <- if ("Drug X Low Dose" %in% names(wide)) wide[["Drug X Low Dose"]] else ""
wide$col3 <- if ("Drug X High Dose" %in% names(wide)) wide[["Drug X High Dose"]] else ""

# Replace any remaining NA with empty strings
wide$col1[is.na(wide$col1)] <- ""
wide$col2[is.na(wide$col2)] <- ""
wide$col3[is.na(wide$col3)] <- ""

final_df <- wide %>%
  select(block, category, ord, stat, col1, col2, col3) %>%
  arrange(block, ord, stat)

# ----------------------------------------------------------------------
# Output ---------------------------------------------------------------
csv_path <- file.path(out_dir, "t_exposure.csv")
txt_path <- file.path(out_dir, "t_exposure.txt")

write.csv(final_df, csv_path, row.names = FALSE, na = "")
write.table(final_df, txt_path, sep = "\t", row.names = FALSE,
            quote = FALSE, na = "")

# End of script
```