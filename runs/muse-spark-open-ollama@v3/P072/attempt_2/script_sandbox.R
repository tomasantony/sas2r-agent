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
  res <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    sign_x <- sign(x[ok])
    z <- abs(x[ok]) * 10^digits
    z <- floor(z + 0.5 + 1e-9) / 10^digits
    res[ok] <- sign_x * z
  }
  res
}
fmt_num_vec <- function(x, decimals) {
  out <- rep("", length(x))
  ok <- !is.na(x) & !is.nan(x) & is.finite(x)
  if (any(ok)) {
    rx <- round_half_up(x[ok], decimals)
    out[ok] <- sprintf(paste0("%.", decimals, "f"), rx)
  }
  out
}
fmt_int_vec <- function(x) {
  out <- rep("", length(x))
  ok <- !is.na(x)
  out[ok] <- as.character(as.integer(x[ok]))
  out
}
fmt_pct_vec <- function(x) fmt_num_vec(x, 1)

# SAS PCTLDEF=5 quantile (PROC MEANS default)
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
    "/bench/data/raw/own-synthetic/adam/adsl.sas7bdat",
    "/bench/data/raw/own-synthetic/adam/adsl.xpt",
    "/bench/data/raw/own-synthetic/adam/ADSL.sas7bdat",
    "/bench/data/raw/own-synthetic/adam/ADSL.xpt",
    "/bench/data/raw/cdisc-pilot/adam/adsl.sas7bdat",
    "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
    "/bench/data/raw/cdisc-pilot/adam/ADSL.xpt",
    "/bench/data/derived/cdisc-pilot/adam/adsl.sas7bdat",
    "/bench/data/derived/cdisc-pilot/adam/adsl.xpt"
  )
  for (f in candidates) {
    if (file.exists(f)) return(f)
  }
  search_roots <- c(
    "/bench/data/raw/own-synthetic/adam",
    "/bench/data/raw/cdisc-pilot/adam",
    "/bench/data/raw/own-synthetic",
    "/bench/data/raw"
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
  names(df) <- toupper(names(df))
  df
}
adsl_raw <- read_adam(adsl_path)

if (!"TRTA" %in% names(adsl_raw)) {
  alt <- c("TRT01A","TRT01P","TRTP")
  for (a in alt) if (a %in% names(adsl_raw)) { adsl_raw$TRTA <- adsl_raw[[a]]; break }
}
stopifnot("TRTA" %in% names(adsl_raw))
stopifnot("SAFFL" %in% names(adsl_raw))
stopifnot("USUBJID" %in% names(adsl_raw))

# ---------- Step 1: Get population counts (m_counts) ----------
adsl_saffl <- adsl_raw %>% filter(toupper(SAFFL) == "Y")

bign_df <- adsl_saffl %>%
  group_by(TRTA) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

bign_tot <- n_distinct(adsl_saffl$USUBJID)

trt_levels_sorted <- bign_df$TRTA
classify_trt <- function(x) {
  xl <- tolower(x)
  if (grepl("placebo", xl)) return(1)
  if (grepl("low", xl)) return(2)
  if (grepl("high", xl)) return(3)
  return(99)
}
ord_class <- sapply(trt_levels_sorted, classify_trt)
if (all(ord_class %in% c(1,2,3)) && length(unique(ord_class)) == length(trt_levels_sorted)) {
  display_order <- trt_levels_sorted[order(ord_class)]
} else {
  display_order <- trt_levels_sorted
}

bign_map <- setNames(bign_df$BIGN, bign_df$TRTA)
headers <- sapply(display_order, function(trt) paste0(trt, " (N=", bign_map[[trt]], ")"))
col1hdr <- if (length(headers) >= 1) headers[1] else ""
col2hdr <- if (length(headers) >= 2) headers[2] else ""
col3hdr <- if (length(headers) >= 3) headers[3] else ""

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
cont_stats <- function(inds, anavar, trtvar = "TRTA", decimals = 1) {
  dp <- decimals
  dp1 <- decimals + 1
  trts <- unique(inds[[trtvar]])
  trts <- trts[!is.na(trts)]
  out_list <- list()
  for (trt in trts) {
    vals <- inds[[anavar]][inds[[trtvar]] == trt]
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
    # vectorized formatting
    rows$value <- ""
    rows$value[rows$stat == "n"] <- fmt_int_vec(rows$n[rows$stat == "n"])
    idx <- rows$stat == "Mean"
    rows$value[idx] <- ifelse(rows$n[idx] > 0, fmt_num_vec(rows$mean[idx], dp1), "")
    idx <- rows$stat == "SD"
    rows$value[idx] <- ifelse(rows$n[idx] > 1, fmt_num_vec(rows$std[idx], dp1), "")
    idx <- rows$stat == "Median"
    rows$value[idx] <- ifelse(rows$n[idx] > 0, fmt_num_vec(rows$median[idx], dp), "")
    idx <- rows$stat == "Q1, Q3"
    rows$value[idx] <- ifelse(rows$n[idx] > 0,
                              paste0(fmt_num_vec(rows$q1[idx], dp), ", ", fmt_num_vec(rows$q3[idx], dp)), "")
    idx <- rows$stat == "Min, Max"
    rows$value[idx] <- ifelse(rows$n[idx] > 0,
                              paste0(fmt_num_vec(rows$min[idx], dp), ", ", fmt_num_vec(rows$max[idx], dp)), "")
    out_list[[trt]] <- rows %>% select(TRTA, stat, ord, value)
  }
  bind_rows(out_list)
}

cat_stats <- function(inds, catvar, trtvar = "TRTA", subjvar = "USUBJID") {
  cnt_df <- inds %>%
    filter(!is.na(.data[[catvar]])) %>%
    group_by(TRTA = .data[[trtvar]], CAT = .data[[catvar]]) %>%
    summarise(cnt = n_distinct(.data[[subjvar]]), .groups = "drop") %>%
    rename(stat = CAT)
  cnt_df <- cnt_df %>%
    left_join(bign_df, by = "TRTA") %>%
    mutate(
      pct = ifelse(BIGN > 0, cnt / BIGN * 100, NA_real_),
      pct_fmt = fmt_pct_vec(pct),
      value = ifelse(cnt > 0, paste0(as.character(cnt), " (", pct_fmt, ")"), "0")
    ) %>%
    select(TRTA, stat, cnt, BIGN, pct, value)
  cnt_df
}

c_age <- cont_stats(dm, "AGE", decimals = 1) %>% mutate(category = "Age (years)", block = 1)
c_weightbl <- cont_stats(dm, "WEIGHTBL", decimals = 1) %>% mutate(category = "Weight (kg)", block = 5)
c_heightbl <- cont_stats(dm, "HEIGHTBL", decimals = 1) %>% mutate(category = "Height (cm)", block = 6)
c_bmibl <- cont_stats(dm, "BMIBL", decimals = 1) %>% mutate(category = "BMI (kg/m^{super 2})", block = 7)

f_sex <- cat_stats(dm, "SEX") %>% mutate(category = "Sex", block = 2)
f_race <- cat_stats(dm, "RACE") %>% mutate(category = "Race", block = 3)
f_ethnic <- cat_stats(dm, "ETHNIC") %>% mutate(category = "Ethnicity", block = 4)
f_agegr1 <- cat_stats(dm, "AGEGR1") %>% mutate(category = "Age Group", block = 8)

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

# ---------- Step 6: Transpose by treatment ----------
all_stats_sorted <- all_stats %>%
  arrange(block, ord, stat, TRTA)

final_wide <- all_stats_sorted %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = NA_character_)

for (i in seq_along(display_order)) {
  trt <- display_order[i]
  colname <- paste0("col", i)
  if (trt %in% names(final_wide)) {
    final_wide[[colname]] <- final_wide[[trt]]
  } else {
    final_wide[[colname]] <- NA_character_
  }
}
final <- final_wide %>%
  select(block, category, ord, stat, col1, col2, col3) %>%
  arrange(block, ord, stat)

# ---------- Step 7: Create report dataset ----------
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

report_disp <- report %>%
  mutate(
    col1 = ifelse(is.na(col1), "", col1),
    col2 = ifelse(is.na(col2), "", col2),
    col3 = ifelse(is.na(col3), "", col3)
  )

# ---------- Step 8: Generate output ----------
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "    Page 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.1.1"
title5 <- "Summary of Demographics and Baseline Characteristics"
title6 <- "Safety Analysis Set"
footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P072_t_dm.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

# CSV output
csv_out <- report_disp %>%
  select(Parameter = display_cat, Statistic = stat) %>%
  bind_cols(report_disp %>% select(col1, col2, col3))
names(csv_out)[3] <- col1hdr
names(csv_out)[4] <- col2hdr
names(csv_out)[5] <- col3hdr
write.csv(csv_out, file.path(out, "t_dm.csv"), row.names = FALSE, na = "")

# Text rendering
txt_path <- file.path(out, "t_dm.txt")
con <- file(txt_path, open = "wt")
writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
hdr_line <- paste("Parameter", "Statistic", col1hdr, col2hdr, col3hdr, sep = " | ")
writeLines(hdr_line, con)
writeLines(paste(rep("-", nchar(hdr_line)), collapse = ""), con)
blocks <- sort(unique(report_disp$block))
for (b in blocks) {
  sub <- report_disp %>% filter(block == b) %>% arrange(ord, stat)
  for (i in seq_len(nrow(sub))) {
    row <- sub[i, ]
    line <- paste(row$display_cat, row$stat, row$col1, row$col2, row$col3, sep = " | ")
    writeLines(line, con)
  }
  writeLines("", con)
}
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
close(con)

# RTF copy (plain text RTF for evaluator)
rtf_path <- file.path(out, "t_dm.rtf")
file.copy(txt_path, rtf_path, overwrite = TRUE)

message("t_dm.sas completed successfully.")
message(paste0("Big N total: ", bign_tot))
for (i in seq_along(display_order)) {
  message(paste0(display_order[i], ": N=", bign_map[[display_order[i]]]))
}
