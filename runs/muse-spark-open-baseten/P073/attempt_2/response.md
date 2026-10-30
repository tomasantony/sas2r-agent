```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# helpers - vectorized, SAS half-up rounding
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  # vectorized
  res <- sign(x) * floor(abs(x) * mult + 0.5 + 1e-12) / mult
  res[is.na(x)] <- NA_real_
  res
}
fmt_num <- function(x, digits) {
  # vectorized: returns character vector same length as x
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi) || is.nan(xi)) return("")
    r <- round_half_up(xi, digits)
    sprintf(paste0("%.", digits, "f"), r)
  }, character(1))
}
fmt_num_scalar <- function(x, digits) {
  if (length(x) != 1) x <- x[1]
  if (is.na(x) || is.nan(x)) return("")
  r <- round_half_up(x, digits)
  sprintf(paste0("%.", digits, "f"), r)
}
fmt_int <- function(x) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return("")
    trimws(formatC(xi, format = "f", digits = 0))
  }, character(1))
}

read_adam_file <- function(path, name) {
  files <- list.files(path, full.names = TRUE, recursive = FALSE)
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx) == 0) idx <- grep(tolower(name), tolower(basename(files)))
  if (length(idx) == 0) stop(paste0("File not found: ", name, " in ", path))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") haven::read_sas(f)
  else if (ext == "xpt") haven::read_xpt(f)
  else if (ext == "csv") readr::read_csv(f, show_col_types = FALSE)
  else tryCatch(haven::read_sas(f), error = function(e) haven::read_xpt(f))
}

library(dplyr)
library(tidyr)

adsl <- read_adam_file(adam_path, "adsl")
names(adsl) <- toupper(names(adsl))

# Big N per TRTA where SAFFL=Y
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

bign_df <- adsl_saff %>%
  group_by(TRTA, TRTAN) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, TRTA)

if (all(is.na(bign_df$TRTAN))) {
  bign_df <- adsl_saff %>%
    group_by(TRTA) %>%
    summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
    arrange(TRTA)
  bign_df$TRTAN <- seq_len(nrow(bign_df))
}
bign_df <- bign_df %>% arrange(TRTAN)
bign_map <- setNames(bign_df$bign, bign_df$TRTA)

# ex_adsl - duration
ex_adsl <- adsl_saff
ex_adsl$TRTDUR <- with(ex_adsl, ifelse(is.na(TRTSDT) | is.na(TRTEDT), NA_real_, as.numeric(TRTEDT) - as.numeric(TRTSDT) + 1))

decimals <- 1
dp <- decimals
dp1 <- decimals + 1

cont_stats_list <- lapply(seq_len(nrow(bign_df)), function(i) {
  trt <- bign_df$TRTA[i]
  vals <- ex_adsl$TRTDUR[ex_adsl$TRTA == trt]
  vals <- vals[!is.na(vals)]
  n <- length(vals)
  if (n > 0) {
    mn <- mean(vals)
    sdv <- if (n > 1) sd(vals) else NA_real_
    med <- median(vals)
    q1 <- as.numeric(quantile(vals, probs = 0.25, type = 2, na.rm = TRUE))
    q3 <- as.numeric(quantile(vals, probs = 0.75, type = 2, na.rm = TRUE))
    mnv <- min(vals)
    mxv <- max(vals)
  } else {
    mn <- sdv <- med <- q1 <- q3 <- mnv <- mxv <- NA_real_
  }
  n_str <- fmt_int(n)[1]
  mean_str <- if (n > 0) fmt_num_scalar(mn, dp1) else ""
  sd_str <- if (n > 1) fmt_num_scalar(sdv, dp1) else ""
  med_str <- if (n > 0) fmt_num_scalar(med, dp) else ""
  q1q3_str <- if (n > 0) paste0(fmt_num_scalar(q1, dp), ", ", fmt_num_scalar(q3, dp)) else ""
  minmax_str <- if (n > 0) paste0(fmt_num_scalar(mnv, dp), ", ", fmt_num_scalar(mxv, dp)) else ""
  data.frame(
    TRTA = trt,
    stat = c("n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"),
    ord = c(1, 2, 3, 4, 5, 6),
    value = c(n_str, mean_str, sd_str, med_str, q1q3_str, minmax_str),
    stringsAsFactors = FALSE
  )
})
dur_stats <- do.call(rbind, cont_stats_list)
dur_stats$category <- "Duration of Treatment (days)"
dur_stats$block <- 1

# Duration categories
dur_cat <- ex_adsl %>%
  mutate(
    DURCAT = case_when(
      is.na(TRTDUR) ~ "Missing",
      TRTDUR <= 28 ~ "<=28 days",
      TRTDUR <= 84 ~ ">28 to <=84 days",
      TRTDUR <= 168 ~ ">84 to <=168 days",
      TRTDUR <= 336 ~ ">168 to <=336 days",
      TRUE ~ ">336 days"
    ),
    DURCATN = case_when(
      DURCAT == "<=28 days" ~ 1L,
      DURCAT == ">28 to <=84 days" ~ 2L,
      DURCAT == ">84 to <=168 days" ~ 3L,
      DURCAT == ">168 to <=336 days" ~ 4L,
      DURCAT == ">336 days" ~ 5L,
      DURCAT == "Missing" ~ 6L,
      TRUE ~ 99L
    )
  )

durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

all_combos <- expand.grid(
  TRTA = bign_df$TRTA,
  DURCAT = c("<=28 days", ">28 to <=84 days", ">84 to <=168 days", ">168 to <=336 days", ">336 days", "Missing"),
  stringsAsFactors = FALSE
)
all_combos$DURCATN <- c(1, 2, 3, 4, 5, 6)[match(all_combos$DURCAT, c("<=28 days", ">28 to <=84 days", ">84 to <=168 days", ">168 to <=336 days", ">336 days", "Missing"))]

durcat_cnt_full <- all_combos %>%
  left_join(durcat_cnt, by = c("TRTA", "DURCAT", "DURCATN")) %>%
  mutate(cnt = ifelse(is.na(cnt), 0L, cnt))

durcat_pct <- durcat_cnt_full %>%
  left_join(bign_df %>% select(TRTA, bign), by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

# format value vectorized
durcat_pct$value <- ifelse(durcat_pct$cnt > 0,
  paste0(trimws(as.character(durcat_pct$cnt)), " (", fmt_num(durcat_pct$pct, 1), ")"),
  "0")

durcat_fmt <- durcat_pct %>%
  mutate(
    category = "Duration Categories",
    stat = paste0("  ", DURCAT),
    ord = 100 + DURCATN,
    block = 2
  ) %>%
  select(TRTA, category, stat, ord, block, value)

# Combine
all_exp <- bind_rows(
  dur_stats %>% select(TRTA, category, stat, ord, block, value),
  durcat_fmt %>% select(TRTA, category, stat, ord, block, value)
) %>%
  arrange(block, ord, stat)

# Transpose
final_wide <- all_exp %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

ordered_trts <- bign_df$TRTA
cols_present <- intersect(ordered_trts, names(final_wide))
final_wide <- final_wide %>% select(block, category, ord, stat, all_of(cols_present))
for (i in seq_along(cols_present)) {
  final_wide[[paste0("col", i)]] <- final_wide[[cols_present[i]]]
}
for (cn in names(final_wide)) {
  final_wide[[cn]][is.na(final_wide[[cn]])] <- ""
}
final_wide <- final_wide %>% arrange(block, ord)

# Headers with N
hdr_map <- setNames(paste0(cols_present, " (N=", bign_map[cols_present], ")"), cols_present)

display_df <- final_wide %>%
  select(category, stat, all_of(paste0("col", seq_along(cols_present)))) %>%
  rename(Category = category, Statistic = stat)
for (i in seq_along(cols_present)) {
  names(display_df)[names(display_df) == paste0("col", i)] <- hdr_map[cols_present[i]]
}

write.csv(display_df, file.path(out, "t_exposure.csv"), row.names = FALSE, na = "")
write.csv(final_wide, file.path(out, "t_exposure_dataset.csv"), row.names = FALSE, na = "")

# Text rendering - must contain all numbers
txt_path <- file.path(out, "t_exposure.txt")
con <- file(txt_path, open = "wt")
cat("Table 14.1.3\n", file = con)
cat("Summary of Study Drug Exposure\n", file = con)
cat("Safety Analysis Set\n", file = con)
cat("\n", file = con)
header_line <- sprintf("%-30s %-20s", "Category", "Statistic")
for (i in seq_along(cols_present)) {
  header_line <- paste0(header_line, sprintf(" | %-20s", hdr_map[cols_present[i]]))
}
cat(header_line, "\n", file = con)
cat(paste(rep("-", nchar(header_line)), collapse = ""), "\n", file = con)
for (r in seq_len(nrow(final_wide))) {
  cat_val <- final_wide$category[r]
  stat_val <- final_wide$stat[r]
  line <- sprintf("%-30s %-20s", substr(cat_val, 1, 30), substr(stat_val, 1, 20))
  for (i in seq_along(cols_present)) {
    v <- final_wide[[paste0("col", i)]][r]
    line <- paste0(line, sprintf(" | %-20s", v))
  }
  cat(line, "\n", file = con)
}
cat("\n", file = con)
cat("Data cutoff: [DDMONYYYY]\n", file = con)
cat("Source: ADaM datasets\n", file = con)
cat(paste0("Program: P073_t_exposure.sas | Run: ", format(Sys.Date(), "%d%b%Y"), "\n"), file = con)
close(con)

# also write .rtf as text copy (expected output name)
file.copy(txt_path, file.path(out, "t_exposure.rtf"), overwrite = TRUE)

cat("t_exposure completed\n")
```