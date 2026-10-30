```r
# P072_t_dm.sas -> R translation
# Table 14.1.1 Summary of Demographics and Baseline Characteristics
# Population: Safety Analysis Set (SAFFL='Y')

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign_x <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-9) / 10^digits
  sign_x * z
}
fmt_num <- function(x, decimals) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  rx <- round_half_up(x, decimals)
  sprintf(paste0("%.", decimals, "f"), rx)
}
fmt_int <- function(x) {
  if (is.na(x)) return("")
  as.character(as.integer(x))
}
fmt_pct <- function(pct) {
  if (is.na(pct)) return("")
  fmt_num(pct, 1)
}
# SAS PCTLDEF=5 quantile
quantile_pctldef5 <- function(x, probs) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(rep(NA_real_, length(probs)))
  sapply(probs, function(p) {
    if (is.na(p)) return(NA_real_)
    if (p <= 0) return(x[1])
    if (p >= 1) return(x[n])
    np <- n * p + 0.5
    j <- floor(np)
    g <- np - j
    if (j == 0) return(x[1])
    if (j >= n) return(x[n])
    if (abs(g) < 1e-12) {
      return(x[j])
    } else {
      return((1 - g) * x[j] + g * x[j+1])
    }
  })
}

# ---------- locate ADSL ----------
find_adsl <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/ADSL.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/ADSL.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADSL.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt"
  )
  for (f in candidates) {
    if (file.exists(f)) return(f)
  }
  # search recursively under adam paths
  search_roots <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic",
    "D:/meta_hackathon/sas2r-agent/data/raw"
  )
  for (r in search_roots) {
    if (dir.exists(r)) {
      files <- list.files(r, pattern = "(?i)adsl\\.(sas7bdat|xpt)$", recursive = TRUE, full.names = TRUE)
      if (length(files) > 0) return(files[1])
    }
  }
  return(NA)
}
adsl_path <- find_adsl()
if (is.na(adsl_path)) stop("ADSL file not found")

read_adam <- function(path) {
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    df <- haven::read_xpt(path)
  } else {
    df <- haven::read_sas(path)
  }
  # normalize names to uppercase for SAS-like handling
  names(df) <- toupper(names(df))
  df
}
adsl_raw <- read_adam(adsl_path)

# Ensure required columns exist (case-insensitive check)
required <- c("USUBJID","TRTA","SAFFL","AGE","SEX","RACE","ETHNIC","WEIGHTBL","HEIGHTBL","BMIBL")
for (col in required) {
  if (!col %in% names(adsl_raw)) {
    # try to find case-insensitive
    idx <- which(toupper(names(adsl_raw)) == col)
    if (length(idx) > 0) names(adsl_raw)[idx[1]] <- col
  }
}
# If TRTA missing try TRT01A / TRTA etc
if (!"TRTA" %in% names(adsl_raw)) {
  alt <- c("TRT01A","TRTA","TRT01P","TRTP")
  for (a in alt) if (a %in% names(adsl_raw)) { adsl_raw$TRTA <- adsl_raw[[a]]; break }
}
stopifnot("TRTA" %in% names(adsl_raw))
stopifnot("SAFFL" %in% names(adsl_raw))
stopifnot("USUBJID" %in% names(adsl_raw))

# ---------- Step 1: Get population counts (m_counts) ----------
# Big N per TRTA where SAFFL='Y' (upcase)
adsl_saffl <- adsl_raw %>% filter(toupper(SAFFL) == "Y")

# SAS sorts character with uppercase before lowercase; R default is similar for ASCII
# Use base ordering
bign_df <- adsl_saffl %>%
  group_by(TRTA) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

bign_tot <- n_distinct(adsl_saffl$USUBJID)

# Build header strings in SAS sorted order
# But for display we want Placebo, Low, High order if detectable
# Detect display order: Placebo first, then Low, then High
trt_levels_sorted <- bign_df$TRTA
# helper to classify
classify_trt <- function(x) {
  xl <- tolower(x)
  if (grepl("placebo", xl)) return(1)
  if (grepl("low", xl)) return(2)
  if (grepl("high", xl)) return(3)
  return(99)
}
ord_idx <- order(sapply(trt_levels_sorted, classify_trt), trt_levels_sorted)
# If classification yields distinct 1,2,3 then use that order, else use sorted order
if (length(unique(sapply(trt_levels_sorted, classify_trt))) == length(trt_levels_sorted) ||
    all(sapply(trt_levels_sorted, classify_trt) %in% c(1,2,3))) {
  # check if we have at least one of each
  display_order <- trt_levels_sorted[order(sapply(trt_levels_sorted, classify_trt))]
} else {
  display_order <- trt_levels_sorted
}
# Ensure display_order has length 3; if not, fallback to sorted
if (length(display_order) != nrow(bign_df)) display_order <- trt_levels_sorted

# Map TRTA -> BIGN
bign_map <- setNames(bign_df$BIGN, bign_df$TRTA)
# Headers in display order
headers <- sapply(display_order, function(trt) paste0(trt, " (N=", bign_map[[trt]], ")"))
names(headers) <- paste0("col", seq_along(headers), "hdr")
# For report we need col1hdr, col2hdr, col3hdr
col1hdr <- headers[1]
col2hdr <- if (length(headers) >= 2) headers[2] else ""
col3hdr <- if (length(headers) >= 3) headers[3] else ""
# Also keep bign for denominator lookup
# ---------- Step 2: Prepare analysis dataset dm ----------
dm <- adsl_raw %>% filter(SAFFL == "Y") %>%
  mutate(
    AGEGR1 = case_when(
      AGE < 18 ~ "<18",
      AGE < 65 ~ "18 to <65",
      AGE >= 65 ~ ">=65",
      TRUE ~ NA_character_
    ),
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ NA_real_),
    RACEN = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      TRUE ~ 4
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO" ~ 1,
      ETHNIC == "NOT HISPANIC OR LATINO" ~ 2,
      TRUE ~ 3
    )
  )

# ---------- Step 3 & 4: Continuous and Categorical blocks ----------
# Function to compute continuous stats per TRTA
cont_stats <- function(inds, anavar, trtvar = "TRTA", decimals = 1) {
  dp <- decimals
  dp1 <- decimals + 1
  var_sym <- sym(anavar)
  # compute per TRTA
  trts <- unique(inds[[trtvar]])
  trts <- trts[!is.na(trts)]
  out_list <- list()
  for (trt in trts) {
    vals <- inds %>% filter(.data[[trtvar]] == trt) %>% pull(!!var_sym)
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n > 0) {
      mean_v <- mean(vals)
      sd_v <- if (n > 1) sd(vals) else NA_real_
      median_v <- quantile_pctldef5(vals, 0.5)
      q1_v <- quantile_pctldef5(vals, 0.25)
      q3_v <- quantile_pctldef5(vals, 0.75)
      min_v <- min(vals)
      max_v <- max(vals)
    } else {
      mean_v <- sd_v <- median_v <- q1_v <- q3_v <- min_v <- max_v <- NA_real_
    }
    # build rows
    rows <- tibble(
      TRTA = trt,
      stat = c("n","Mean","SD","Median","Q1, Q3","Min, Max"),
      ord = c(1,2,3,4,5,6),
      n = n,
      mean = mean_v,
      std = sd_v,
      median = median_v,
      q1 = q1_v,
      q3 = q3_v,
      min = min_v,
      max = max_v
    )
    # format value
    rows <- rows %>% mutate(
      value = case_when(
        stat == "n" ~ fmt_int(n),
        stat == "Mean" ~ ifelse(n > 0, fmt_num(mean, dp1), ""),
        stat == "SD" ~ ifelse(n > 1, fmt_num(std, dp1), ""),
        stat == "Median" ~ ifelse(n > 0, fmt_num(median, dp), ""),
        stat == "Q1, Q3" ~ ifelse(n > 0, paste0(fmt_num(q1, dp), ", ", fmt_num(q3, dp)), ""),
        stat == "Min, Max" ~ ifelse(n > 0, paste0(fmt_num(min, dp), ", ", fmt_num(max, dp)), ""),
        TRUE ~ ""
      )
    ) %>% select(TRTA, stat, ord, value)
    out_list[[trt]] <- rows
  }
  bind_rows(out_list)
}

cat_stats <- function(inds, catvar, trtvar = "TRTA", subjvar = "USUBJID") {
  # count distinct subjects per TRTA and catvar
  # inds is already filtered to SAFFL=Y
  # Use distinct USUBJID
  # Need Big N per TRTA for pct
  cnt_df <- inds %>%
    filter(!is.na(.data[[catvar]])) %>%
    group_by(TRTA = .data[[trtvar]], CAT = .data[[catvar]]) %>%
    summarise(cnt = n_distinct(.data[[subjvar]]), .groups = "drop") %>%
    rename(!!catvar := CAT)
  # merge with bign
  cnt_df <- cnt_df %>%
    left_join(bign_df, by = "TRTA") %>%
    mutate(
      pct = ifelse(BIGN > 0, cnt / BIGN * 100, NA_real_),
      value = ifelse(cnt > 0, paste0(fmt_int(cnt), " (", fmt_pct(pct), ")"), "0")
    ) %>%
    select(TRTA, !!sym(catvar), cnt, BIGN, pct, value) %>%
    rename(stat = !!sym(catvar))
  cnt_df
}

# Build blocks
# Continuous
c_age <- cont_stats(dm, "AGE", decimals = 1) %>% mutate(category = "Age (years)", block = 1)
c_weightbl <- cont_stats(dm, "WEIGHTBL", decimals = 1) %>% mutate(category = "Weight (kg)", block = 5)
c_heightbl <- cont_stats(dm, "HEIGHTBL", decimals = 1) %>% mutate(category = "Height (cm)", block = 6)
c_bmibl <- cont_stats(dm, "BMIBL", decimals = 1) %>% mutate(category = "BMI (kg/m^{super 2})", block = 7)

# Categorical
f_sex <- cat_stats(dm, "SEX") %>% mutate(category = "Sex", block = 2)
f_race <- cat_stats(dm, "RACE") %>% mutate(category = "Race", block = 3)
f_ethnic <- cat_stats(dm, "ETHNIC") %>% mutate(category = "Ethnicity", block = 4)
f_agegr1 <- cat_stats(dm, "AGEGR1") %>% mutate(category = "Age Group", block = 8)

# Combine all_stats
all_stats <- bind_rows(
  c_age %>% select(TRTA, category, block, stat, ord, value),
  f_sex %>% select(TRTA, category, block, stat, value) %>% mutate(ord = NA_real_),
  f_race %>% select(TRTA, category, block, stat, value) %>% mutate(ord = NA_real_),
  f_ethnic %>% select(TRTA, category, block, stat, value) %>% mutate(ord = NA_real_),
  c_weightbl %>% select(TRTA, category, block, stat, ord, value),
  c_heightbl %>% select(TRTA, category, block, stat, ord, value),
  c_bmibl %>% select(TRTA, category, block, stat, ord, value),
  f_agegr1 %>% select(TRTA, category, block, stat, value) %>% mutate(ord = NA_real_)
)

# Ensure ord is numeric, missing sorts first (SAS)
# For categorical, ord is NA, will sort first

# ---------- Step 6: Transpose by treatment ----------
# Sort by block ord stat (SAS: missing first, character ascending)
# For R, arrange with block, ord (NA first), stat
all_stats_sorted <- all_stats %>%
  arrange(block, ord, stat, TRTA)

# Create wide format: one row per block, category, ord, stat
# Use pivot_wider
final_wide <- all_stats_sorted %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = NA_character_)

# Now we need to map TRTA columns to col1, col2, col3 in display_order
# final_wide has columns named after TRTA values
# Create col1, col2, col3
for (i in seq_along(display_order)) {
  trt <- display_order[i]
  colname <- paste0("col", i)
  if (trt %in% names(final_wide)) {
    final_wide[[colname]] <- final_wide[[trt]]
  } else {
    final_wide[[colname]] <- NA_character_
  }
}
# Keep only needed columns
final <- final_wide %>%
  select(block, category, ord, stat, col1, col2, col3) %>%
  arrange(block, ord, stat)

# ---------- Step 7: Create report dataset ----------
# SAS: retain block category stat col1 col2 col3; set final; by block category;
# display_cat = category if first.category else ' '; stat = '  ' || strip(stat);
# Need to determine first.category per block,category
report <- final %>%
  arrange(block, category, ord, stat) %>%
  group_by(block, category) %>%
  mutate(
    is_first = row_number() == 1,
    display_cat = ifelse(is_first, category, ""),
    stat_disp = paste0("  ", str_trim(stat))
  ) %>%
  ungroup() %>%
  select(block, category, display_cat, stat = stat_disp, col1, col2, col3, ord) %>%
  arrange(block, ord, stat)

# For display, replace NA with ""
report_disp <- report %>%
  mutate(
    col1 = ifelse(is.na(col1), "", col1),
    col2 = ifelse(is.na(col2), "", col2),
    col3 = ifelse(is.na(col3), "", col3)
  )

# ---------- Step 8: Generate output ----------
# Titles and footnotes (from u_titles.sas)
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

title1 <- paste0(sponsor, "    Page 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Table 14.1.1"
title5 <- "Summary of Demographics and Baseline Characteristics"
title6 <- "Safety Analysis Set"
footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P072_t_dm.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

# Prepare CSV output (table body)
# CSV should contain the report data with headers
csv_out <- report_disp %>%
  select(Parameter = display_cat, Statistic = stat, !!col1hdr := col1, !!col2hdr := col2, !!col3hdr := col3)
# Write CSV
write.csv(csv_out, file.path(out, "t_dm.csv"), row.names = FALSE, na = "")

# Also write a dataset CSV with SAS variable names (same as report)
# Already done

# Text rendering
txt_path <- file.path(out, "t_dm.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
# Header row
hdr_line <- sprintf("%-30s %-20s %-20s %-20s %-20s", "Parameter", "Statistic", col1hdr, col2hdr, col3hdr)
writeLines(hdr_line, con)
writeLines(paste(rep("-", nchar(hdr_line)), collapse = ""), con)

# Body rows: group by block, add blank line after each block
blocks <- unique(report_disp$block)
for (b in sort(blocks)) {
  sub <- report_disp %>% filter(block == b) %>% arrange(ord, stat)
  for (i in seq_len(nrow(sub))) {
    row <- sub[i, ]
    line <- sprintf("%-30s %-20s %-20s %-20s %-20s",
                    substr(row$display_cat, 1, 30),
                    substr(row$stat, 1, 20),
                    substr(row$col1, 1, 20),
                    substr(row$col2, 1, 20),
                    substr(row$col3, 1, 20))
    writeLines(line, con)
  }
  writeLines("", con) # blank line after block (compute after block; line ' ';)
}
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
close(con)

# Also create RTF-like text file t_dm.rtf as plain text (SAS expects RTF but we provide text rendering)
# Copy txt to rtf for compatibility
file.copy(txt_path, file.path(out, "t_dm.rtf"), overwrite = TRUE)

# Cleanup message
message("t_dm.sas completed successfully.")
message(paste0("Big N total: ", bign_tot))
for (i in seq_along(display_order)) {
  message(paste0(display_order[i], ": N=", bign_map[[display_order[i]]]))
}
```