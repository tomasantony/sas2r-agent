# P073 t_exposure - R translation of SAS program P073_t_exposure.sas
# Table 14.1.3 Summary of Study Drug Exposure

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}

# helper: SAS quantile PCTLDEF=5
quantile_sas_p5 <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (p <= 0) return(x[1])
  if (p >= 1) return(x[n])
  np <- n * p
  j <- floor(np + 1e-9)
  g <- np - j
  # treat g==0 within tolerance
  if (abs(g) < 1e-9) {
    # g==0 => average of x_j and x_{j+1}, j is 1-based
    # j must be >=1 and < n
    if (j < 1) return(x[1])
    if (j >= n) return(x[n])
    return((x[j] + x[j+1]) / 2)
  } else {
    k <- ceiling(np - 1e-9)
    if (k < 1) k <- 1
    if (k > n) k <- n
    return(x[k])
  }
}

# helper: read ADaM dataset by name (sas7bdat or xpt)
read_adam <- function(name) {
  # search recursively
  files <- list.files(adam_path, recursive = TRUE, full.names = TRUE)
  # match basename ignoring case
  pat <- paste0("^", name, "\\.(sas7bdat|xpt|csv)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  cand <- character(0)
  if (length(idx) > 0) cand <- files[idx]
  # also try direct path
  cand <- c(cand,
            file.path(adam_path, paste0(name, ".sas7bdat")),
            file.path(adam_path, paste0(name, ".xpt")),
            file.path(adam_path, paste0(toupper(name), ".sas7bdat")),
            file.path(adam_path, paste0(toupper(name), ".xpt")),
            file.path(adam_path, name, paste0(name, ".sas7bdat")),
            file.path(adam_path, name, paste0(name, ".xpt"))
  )
  for (f in cand) {
    if (!is.na(f) && file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        df <- haven::read_xpt(f)
      } else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
        df <- haven::read_sas(f)
      } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        df <- readr::read_csv(f, show_col_types = FALSE)
      } else next
      df <- haven::zap_labels(df)
      df <- haven::zap_formats(df)
      # convert labelled to plain
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      # make names upper for consistency then keep original
      return(df)
    }
  }
  stop(paste("Dataset", name, "not found in", adam_path))
}

# helper to get column case-insensitively
get_col <- function(df, colname) {
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
has_col <- function(df, colname) any(toupper(names(df)) == toupper(colname))

# Load ADSL
adsl <- read_adam("adsl")
# normalize names to upper for easier handling, but keep original for output
names(adsl) <- toupper(names(adsl))

# Ensure required columns exist
stopifnot(has_col(adsl, "USUBJID"))
stopifnot(has_col(adsl, "TRTA"))
stopifnot(has_col(adsl, "SAFFL"))

# Big N per TRTA where SAFFL='Y'
adsl_saff <- adsl %>% filter(toupper(SAFFL) == "Y")

# distinct TRTA / TRTAN mapping
if (has_col(adsl, "TRTAN")) {
  trt_map <- adsl_saff %>%
    select(TRTA, TRTAN) %>%
    distinct() %>%
    arrange(TRTAN)
} else {
  trt_map <- adsl_saff %>%
    select(TRTA) %>%
    distinct() %>%
    mutate(TRTAN = row_number()) %>%
    arrange(TRTA)
}
# order by TRTAN
trt_map <- trt_map %>% arrange(TRTAN)
trt_levels <- trt_map$TRTA
trt_n <- nrow(trt_map)

# Big N
bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(match(TRTA, trt_levels))

bign_tot <- n_distinct(adsl_saff$USUBJID)

# Build header strings like SAS m_counts
# col1hdr etc
hdr_vec <- character(trt_n)
for (i in seq_len(trt_n)) {
  trt <- trt_levels[i]
  bn <- bign_df$bign[bign_df$TRTA == trt]
  if (length(bn) == 0) bn <- 0
  hdr_vec[i] <- paste0(trt, " (N=", bn, ")")
}
# For report display
col_hdrs <- hdr_vec
names(col_hdrs) <- paste0("col", seq_len(trt_n))

# Derive ex_adsl: Duration of treatment
# TRTEDT and TRTSDT may be Date or numeric SAS date or character
# Helper to convert to numeric days
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01, but haven already converts to Date if format present
    # If numeric and large (e.g., > 10000) treat as SAS date
    # Convert SAS date to R Date
    # SAS date 0 = 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # try ISO
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (all(is.na(d) & !is.na(x))) {
      d <- suppressWarnings(as.Date(x, format = "%d%b%Y"))
    }
    return(d)
  }
  return(as.Date(NA))
}

# Extract TRTSDT/TRTEDT
TRTSDT_raw <- get_col(adsl, "TRTSDT")
TRTEDT_raw <- get_col(adsl, "TRTEDT")

# Convert if needed
if (is.character(TRTSDT_raw) || is.numeric(TRTSDT_raw)) {
  # haven may have already converted numeric SAS dates to Date
  # Check if numeric and not Date
  if (is.numeric(TRTSDT_raw) && !inherits(TRTSDT_raw, "Date")) {
    TRTSDT <- as.Date(TRTSDT_raw, origin = "1960-01-01")
    TRTEDT <- as.Date(TRTEDT_raw, origin = "1960-01-01")
  } else {
    TRTSDT <- to_date(TRTSDT_raw)
    TRTEDT <- to_date(TRTEDT_raw)
  }
} else {
  TRTSDT <- TRTSDT_raw
  TRTEDT <- TRTEDT_raw
}

adsl$TRTSDT_D <- TRTSDT
adsl$TRTEDT_D <- TRTEDT

ex_adsl <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  mutate(
    TRTDUR = ifelse(!is.na(TRTEDT_D) & !is.na(TRTSDT_D),
                    as.numeric(TRTEDT_D - TRTSDT_D) + 1, NA_real_)
  )

# Step 3: Duration statistics - m_cont_stats equivalent
# Compute per TRTA: n, mean, std, median, min, max, q1, q3
stats_list <- list()
for (trt in trt_levels) {
  vals <- ex_adsl$TRTDUR[ex_adsl$TRTA == trt]
  vals_nm <- vals[!is.na(vals)]
  n <- length(vals_nm)
  mn <- if (n > 0) mean(vals_nm) else NA_real_
  sdv <- if (n > 1) sd(vals_nm) else NA_real_
  med <- if (n > 0) quantile_sas_p5(vals_nm, 0.5) else NA_real_
  q1 <- if (n > 0) quantile_sas_p5(vals_nm, 0.25) else NA_real_
  q3 <- if (n > 0) quantile_sas_p5(vals_nm, 0.75) else NA_real_
  mnv <- if (n > 0) min(vals_nm) else NA_real_
  mxv <- if (n > 0) max(vals_nm) else NA_real_
  stats_list[[trt]] <- tibble(
    TRTA = trt,
    n = n,
    mean = mn,
    std = sdv,
    median = med,
    q1 = q1,
    q3 = q3,
    min = mnv,
    max = mxv
  )
}
stats_df <- bind_rows(stats_list)

# Format statistics for display (decimals=1 => dp=1, dp1=2)
format_cont_value <- function(n, mean, std, median, q1, q3, min, max) {
  # returns named list of formatted strings
  out <- list()
  out[["n"]] <- if (!is.na(n)) as.character(n) else ""
  # Mean 2 decimals
  if (!is.na(n) && n > 0 && !is.na(mean)) {
    out[["Mean"]] <- sprintf("%.2f", round_half_up(mean, 2))
  } else out[["Mean"]] <- ""
  if (!is.na(n) && n > 1 && !is.na(std)) {
    out[["SD"]] <- sprintf("%.2f", round_half_up(std, 2))
  } else out[["SD"]] <- ""
  if (!is.na(n) && n > 0 && !is.na(median)) {
    out[["Median"]] <- sprintf("%.1f", round_half_up(median, 1))
  } else out[["Median"]] <- ""
  if (!is.na(n) && n > 0 && !is.na(q1) && !is.na(q3)) {
    q1f <- sprintf("%.1f", round_half_up(q1, 1))
    q3f <- sprintf("%.1f", round_half_up(q3, 1))
    out[["Q1, Q3"]] <- paste0(q1f, ", ", q3f)
  } else out[["Q1, Q3"]] <- ""
  if (!is.na(n) && n > 0 && !is.na(min) && !is.na(max)) {
    minf <- sprintf("%.1f", round_half_up(min, 1))
    maxf <- sprintf("%.1f", round_half_up(max, 1))
    out[["Min, Max"]] <- paste0(minf, ", ", maxf)
  } else out[["Min, Max"]] <- ""
  out
}

dur_stats_rows <- list()
for (i in seq_len(nrow(stats_df))) {
  r <- stats_df[i, ]
  fmt <- format_cont_value(r$n, r$mean, r$std, r$median, r$q1, r$q3, r$min, r$max)
  # order mapping
  ord_map <- c("n" = 1, "Mean" = 2, "SD" = 3, "Median" = 4, "Q1, Q3" = 5, "Min, Max" = 6)
  for (st in names(fmt)) {
    dur_stats_rows[[length(dur_stats_rows) + 1]] <- tibble(
      TRTA = r$TRTA,
      category = "Duration of Treatment (days)",
      block = 1L,
      stat = st,
      ord = ord_map[[st]],
      value = fmt[[st]]
    )
  }
}
dur_stats <- bind_rows(dur_stats_rows)

# Step 4: Duration categories
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

# Step 5: Duration category counts
durcat_cnt <- dur_cat %>%
  group_by(TRTA, DURCAT, DURCATN) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(DURCATN, match(TRTA, trt_levels))

durcat_pct <- durcat_cnt %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

durcat_fmt <- durcat_pct %>%
  mutate(
    category = "Duration Categories",
    stat = paste0("  ", DURCAT),
    ord = 100L + DURCATN,
    block = 2L,
    value = ifelse(cnt > 0,
                   paste0(as.character(cnt), " (", sprintf("%.1f", round_half_up(pct, 1)), ")"),
                   "0")
  ) %>%
  select(TRTA, category, block, stat, ord, value, DURCAT, DURCATN, cnt, pct, bign)

# Step 6: Combine
all_exp <- bind_rows(
  dur_stats %>% select(TRTA, category, block, stat, ord, value),
  durcat_fmt %>% select(TRTA, category, block, stat, ord, value)
)

# Step 7: Transpose to wide
# Ensure ordering
all_exp <- all_exp %>% arrange(block, ord, stat, match(TRTA, trt_levels))

# pivot wider
final_wide <- all_exp %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# Rename columns to col1, col2, col3 based on TRTAN order
# final_wide currently has columns named after TRTA values
# Map to col1..col3
for (i in seq_len(trt_n)) {
  trt <- trt_levels[i]
  colname <- trt
  # pivot_wider may have sanitized names? Keep original
  if (colname %in% names(final_wide)) {
    names(final_wide)[names(final_wide) == colname] <- paste0("col", i)
  }
}
# Ensure col1..col3 exist (fill missing with NA)
for (i in seq_len(3)) {
  cn <- paste0("col", i)
  if (!cn %in% names(final_wide)) final_wide[[cn]] <- NA_character_
}
# Ensure order and sort
final <- final_wide %>%
  arrange(block, ord, stat) %>%
  select(block, category, ord, stat, col1, col2, col3)

# Replace NA with "" for display (SAS missing -> blank)
final_disp <- final %>%
  mutate(across(starts_with("col"), ~ ifelse(is.na(.), "", .)))

# Write CSV
write.csv(final_disp, file.path(out, "t_exposure.csv"), row.names = FALSE, na = "")

# Build text rendering
# Titles
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

titles <- c(
  paste0(sponsor, "  Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Table 14.1.3",
  "Summary of Study Drug Exposure",
  "Safety Analysis Set",
  ""
)

# Header row
hdr_line1 <- sprintf("%-30s | %-20s | %-20s | %-20s",
                     "Statistic",
                     hdr_vec[1],
                     ifelse(length(hdr_vec) >= 2, hdr_vec[2], ""),
                     ifelse(length(hdr_vec) >= 3, hdr_vec[3], ""))
sep_line <- paste(rep("-", nchar(hdr_line1)), collapse = "")

body_lines <- c()
# Add category headers as bold rows
current_cat <- ""
for (i in seq_len(nrow(final_disp))) {
  r <- final_disp[i, ]
  if (r$category != current_cat) {
    current_cat <- r$category
    body_lines <- c(body_lines, "", current_cat)
  }
  line <- sprintf("%-30s | %-20s | %-20s | %-20s",
                  r$stat,
                  r$col1,
                  r$col2,
                  r$col3)
  body_lines <- c(body_lines, line)
}

footnotes <- c(
  "",
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: P073_t_exposure.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

txt_content <- c(titles, hdr_line1, sep_line, body_lines, footnotes)

# Write txt
writeLines(txt_content, file.path(out, "t_exposure.txt"))

# Also write RTF-like txt for compatibility (same as txt)
# Ensure output directory has t_exposure.rtf placeholder if needed (copy txt)
if (!file.exists(file.path(out, "t_exposure.rtf"))) {
  writeLines(txt_content, file.path(out, "t_exposure.rtf"))
}

cat("P073 t_exposure completed. Rows:", nrow(final_disp), " BigN:", paste(bign_df$bign, collapse=","), "\n")
