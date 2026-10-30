# P073 t_exposure translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5 + 1e-12) / mult
}
fmt_num <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.null(x)) return("")
  r <- round_half_up(x, digits)
  # format with exactly digits decimals, strip leading spaces
  s <- formatC(r, format = "f", digits = digits, flag = "#")
  # formatC with flag "#" keeps decimal point; remove trailing zeros? No, keep
  # Use formatC without flag for integer case
  # Simpler: use sprintf
  s <- sprintf(paste0("%.", digits, "f"), r)
  # SAS strip(put(...,12.x)) removes leading blanks, keeps as is
  trimws(s)
}
fmt_int <- function(x) {
  if (is.na(x)) return("")
  trimws(formatC(x, format = "f", digits = 0))
}
fmt_pct <- function(cnt, pct) {
  if (is.na(cnt) || cnt == 0) return("0")
  # pct is numeric 0-100
  p <- fmt_num(pct, 1)
  paste0(trimws(as.character(cnt)), " (", trimws(p), ")")
}

read_adam_file <- function(path, name) {
  # name without extension, case-insensitive
  files <- list.files(path, full.names = TRUE, recursive = FALSE)
  # find matching file
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx) == 0) {
    # try lower
    idx <- grep(tolower(name), tolower(basename(files)))
  }
  if (length(idx) == 0) stop(paste0("File not found: ", name, " in ", path))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    haven::read_sas(f)
  } else if (ext == "xpt") {
    haven::read_xpt(f)
  } else if (ext == "csv") {
    readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try sas
    tryCatch(haven::read_sas(f), error = function(e) haven::read_xpt(f))
  }
}

# Load ADSL -------------------------------------------------------------
adsl <- read_adam_file(adam_path, "adsl")
# normalize names to upper for matching SAS (haven keeps original case)
names(adsl) <- toupper(names(adsl))

# Ensure required columns exist
# TRTA, TRTAN, SAFFL, USUBJID, TRTSDT, TRTEDT
# Convert SAS dates if numeric and haven labelled
# haven may have already converted to Date if format present; if numeric, convert
to_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date origin 1960-01-01
    as.Date(x, origin = "1960-01-01")
  } else {
    suppressWarnings(as.Date(x))
  }
}

# Keep original numeric for duration calc if Date conversion loses
# For duration we need days difference; if Date, diff works; if numeric, diff works
# Use numeric SAS dates if available
# Try to get numeric values: if TRTSDT is Date, convert to numeric days
# But duration = TRTEDT - TRTSDT +1 works for both

# m_counts --------------------------------------------------------------
# Big N per TRTA where SAFFL=Y
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

# distinct USUBJID per TRTA
library(dplyr)
bign_df <- adsl_saff %>%
  group_by(TRTA, TRTAN) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, TRTA)

# If TRTAN missing, arrange by TRTA
if (all(is.na(bign_df$TRTAN))) {
  bign_df <- adsl_saff %>%
    group_by(TRTA) %>%
    summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
    arrange(TRTA)
  bign_df$TRTAN <- seq_len(nrow(bign_df))
}

bign_tot <- n_distinct(adsl_saff$USUBJID)

# Build header strings (for display)
# Order by TRTAN
bign_df <- bign_df %>% arrange(TRTAN)
trt_labels <- bign_df$TRTA
trt_ns <- bign_df$bign
# Create mapping TRTA -> N
bign_map <- setNames(bign_df$bign, bign_df$TRTA)

# ex_adsl ---------------------------------------------------------------
ex_adsl <- adsl_saff
# Duration of treatment (days)
# TRTEDT and TRTSDT may be Date or numeric
# Use as.Date conversion then diff, or numeric diff
# If they are Date, subtraction gives days; if numeric SAS date, same
# Ensure numeric for calc: if Date, as.numeric; if numeric, use directly
# haven may have imported as Date; we can compute via as.Date
# Use original columns: if they are Date, diff works; if numeric, diff works
# Need to handle missing
calc_dur <- function(s, e) {
  # s,e may be Date or numeric
  # Convert to Date if numeric SAS date? But numeric diff same as Date diff
  # Just use as.numeric(as.Date(...))? Simpler: if inherits Date, as.numeric
  # For numeric, keep
  # Return e - s +1
  ifelse(is.na(s) | is.na(e), NA_real_, as.numeric(e) - as.numeric(s) + 1)
}

# If TRTSDT/TRTEDT are Date, as.numeric gives days since 1970, diff still correct
# If they are SAS numeric (days since 1960), diff also correct
# So we can just do:
ex_adsl$TRTDUR <- with(ex_adsl, ifelse(is.na(TRTSDT) | is.na(TRTEDT), NA_real_, as.numeric(TRTEDT) - as.numeric(TRTSDT) + 1))

# m_cont_stats equivalent -----------------------------------------------
# Compute stats per TRTA
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
    # SAS PCTLDEF=5 => R type 2
    q1 <- as.numeric(quantile(vals, probs = 0.25, type = 2, na.rm = TRUE))
    q3 <- as.numeric(quantile(vals, probs = 0.75, type = 2, na.rm = TRUE))
    mnv <- min(vals)
    mxv <- max(vals)
  } else {
    mn <- sdv <- med <- q1 <- q3 <- mnv <- mxv <- NA_real_
  }
  # Format
  n_str <- if (n > 0 || TRUE) fmt_int(n) else ""
  mean_str <- if (n > 0) fmt_num(mn, dp1) else ""
  sd_str <- if (n > 1) fmt_num(sdv, dp1) else ""
  med_str <- if (n > 0) fmt_num(med, dp) else ""
  q1q3_str <- if (n > 0) paste0(fmt_num(q1, dp), ", ", fmt_num(q3, dp)) else ""
  minmax_str <- if (n > 0) paste0(fmt_num(mnv, dp), ", ", fmt_num(mxv, dp)) else ""
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

# Duration categories ---------------------------------------------------
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

# Counts per TRTA, DURCAT
durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

# Ensure all combinations exist (fill 0)
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

durcat_fmt <- durcat_pct %>%
  mutate(
    category = "Duration Categories",
    stat = paste0("  ", DURCAT),
    ord = 100 + DURCATN,
    block = 2,
    value = ifelse(cnt > 0, paste0(trimws(as.character(cnt)), " (", fmt_num(pct, 1), ")"), "0")
  ) %>%
  select(TRTA, category, stat, ord, block, value)

# Need to vectorize fmt_num for pct: use rowwise
durcat_fmt$value <- mapply(function(c, p) {
  if (c > 0) paste0(trimws(as.character(c)), " (", fmt_num(p, 1), ")") else "0"
}, durcat_fmt$cnt <- durcat_pct$cnt, durcat_pct$pct)
# Actually recompute correctly
durcat_fmt <- durcat_pct %>%
  rowwise() %>%
  mutate(
    category = "Duration Categories",
    stat = paste0("  ", DURCAT),
    ord = 100 + DURCATN,
    block = 2,
    value = if (cnt > 0) paste0(trimws(as.character(cnt)), " (", fmt_num(pct, 1), ")") else "0"
  ) %>%
  ungroup() %>%
  select(TRTA, category, stat, ord, block, value)

# Combine ---------------------------------------------------------------
all_exp <- bind_rows(
  dur_stats %>% select(TRTA, category, stat, ord, block, value),
  durcat_fmt %>% select(TRTA, category, stat, ord, block, value)
) %>%
  arrange(block, ord, stat)

# Transpose -------------------------------------------------------------
# pivot_wider
final_wide <- all_exp %>%
  select(block, category, ord, stat, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value)

# Ensure column order by TRTAN
ordered_trts <- bign_df$TRTA # already ordered by TRTAN
# Reorder columns
cols_present <- intersect(ordered_trts, names(final_wide))
final_wide <- final_wide %>% select(block, category, ord, stat, all_of(cols_present))

# Rename to col1,col2,col3 for compatibility, but keep original for display
# Create col1..col3
for (i in seq_along(cols_present)) {
  final_wide[[paste0("col", i)]] <- final_wide[[cols_present[i]]]
}
# Fill NA with "" (SAS blank)
for (cn in names(final_wide)) {
  final_wide[[cn]][is.na(final_wide[[cn]])] <- ""
}

final_wide <- final_wide %>% arrange(block, ord)

# Prepare display table -------------------------------------------------
# Headers with N
hdr_map <- setNames(paste0(cols_present, " (N=", bign_map[cols_present], ")"), cols_present)
# For CSV header
display_df <- final_wide %>%
  select(category, stat, all_of(paste0("col", seq_along(cols_present)))) %>%
  rename(Category = category, Statistic = stat)
# Rename col headers
for (i in seq_along(cols_present)) {
  names(display_df)[names(display_df) == paste0("col", i)] <- hdr_map[cols_present[i]]
}

# Write CSV -------------------------------------------------------------
write.csv(display_df, file.path(out, "t_exposure.csv"), row.names = FALSE, na = "")

# Also write final dataset CSV (with block/ord)
write.csv(final_wide, file.path(out, "t_exposure_dataset.csv"), row.names = FALSE, na = "")

# Text rendering --------------------------------------------------------
# Build text file similar to RTF
txt_path <- file.path(out, "t_exposure.txt")
con <- file(txt_path, open = "wt")

cat("Table 14.1.3\n", file = con)
cat("Summary of Study Drug Exposure\n", file = con)
cat("Safety Analysis Set\n", file = con)
cat("\n", file = con)

# Header line
header_line <- sprintf("%-30s %-20s", "Category", "Statistic")
for (i in seq_along(cols_present)) {
  header_line <- paste0(header_line, sprintf(" | %-20s", hdr_map[cols_present[i]]))
}
cat(header_line, "\n", file = con)
cat(paste(rep("-", nchar(header_line)), collapse = ""), "\n", file = con)

# Rows
for (r in seq_len(nrow(final_wide))) {
  cat_val <- final_wide$category[r]
  stat_val <- final_wide$stat[r]
  # Only show category on first row of block? SAS order shows category as order variable
  # We'll show category each row but bold logic not needed
  line <- sprintf("%-30s %-20s", substr(cat_val, 1, 30), substr(stat_val, 1, 20))
  for (i in seq_along(cols_present)) {
    v <- final_wide[[paste0("col", i)]][r]
    line <- paste0(line, sprintf(" | %-20s", v))
  }
  cat(line, "\n", file = con)
  # Add blank line when block changes? Keep simple
}
cat("\n", file = con)
cat(paste0("Data cutoff: [DDMONYYYY]\n"), file = con)
cat("Source: ADaM datasets\n", file = con)
cat(paste0("Program: P073_t_exposure.sas | Run: ", format(Sys.Date(), "%d%b%Y"), "\n"), file = con)

close(con)

# Also write RTF-like txt as t_exposure.rtf.txt? But expected t_exposure.rtf
# Create a copy named t_exposure.rtf (plain text) for compatibility
file.copy(txt_path, file.path(out, "t_exposure.rtf"), overwrite = TRUE)

# Log
cat("t_exposure completed\n")
