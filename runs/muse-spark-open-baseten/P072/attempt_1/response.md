```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

## helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign_x <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-9)
  sign_x * z / 10^digits
}
fmt_num <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.null(x)) return("")
  r <- round_half_up(x, digits)
  formatC(r, format = "f", digits = digits, flag = "#")
  # SAS strip(put(...,12.d)) removes leading blanks; sprintf does same
  sprintf(paste0("%.", digits, "f"), r)
}
fmt_int <- function(x) {
  if (is.na(x)) return("")
  as.character(as.integer(x))
}

## locate ADSL -----------------------------------------------------------
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adsl_file <- NULL
if (dir.exists(adam_path)) {
  files <- list.files(adam_path, full.names = TRUE, ignore.case = TRUE)
  # prefer adsl
  cand <- files[grepl("adsl", basename(files), ignore.case = TRUE)]
  if (length(cand) > 0) adsl_file <- cand[1] else if (length(files) > 0) adsl_file <- files[1]
}
if (is.null(adsl_file) || !file.exists(adsl_file)) {
  # fallback to derived path
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
  if (dir.exists(alt)) {
    files2 <- list.files(alt, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    cand2 <- files2[grepl("adsl", basename(files2), ignore.case = TRUE)]
    if (length(cand2) > 0) adsl_file <- cand2[1]
  }
}
if (is.null(adsl_file) || !file.exists(adsl_file)) {
  stop(paste0("ADSL file not found in ", adam_path))
}

ext <- tolower(tools::file_ext(adsl_file))
adsl <- NULL
if (ext == "sas7bdat") {
  adsl <- haven::read_sas(adsl_file)
} else if (ext == "xpt") {
  adsl <- haven::read_xpt(adsl_file)
} else if (ext == "csv") {
  adsl <- readr::read_csv(adsl_file, show_col_types = FALSE)
} else {
  # try haven read_sas anyway
  adsl <- haven::read_sas(adsl_file)
}
# normalize column names to uppercase for SAS-like handling, but keep original
names(adsl) <- toupper(names(adsl))
# haven labelled -> convert to character/numeric where needed
adsl <- as.data.frame(adsl, stringsAsFactors = FALSE)
# ensure character columns are character
for (nm in names(adsl)) {
  if (haven::is.labelled(adsl[[nm]])) {
    adsl[[nm]] <- haven::as_factor(adsl[[nm]], levels = "labels")
    adsl[[nm]] <- as.character(adsl[[nm]])
  }
}

## Step 1: m_counts -------------------------------------------------------
# TRTA, SAFFL, USUBJID
# filter SAFFL='Y' case-insensitive (SAS upcase)
pop <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

# Big N per TRTA
bign_df <- aggregate(USUBJID ~ TRTA, data = pop, FUN = function(x) length(unique(x)))
names(bign_df) <- c("TRTA", "BIGN")
# SAS sorts by TRTA character ordering (uppercase before lowercase)
bign_df <- bign_df[order(bign_df$TRTA), , drop = FALSE]
bign_tot <- length(unique(pop$USUBJID))

# headers like SAS: "Placebo (N=xx)"
# ntrt from macro = 3, but derive from data
ntrt <- nrow(bign_df)
col_hdrs <- setNames(
  paste0(bign_df$TRTA, " (N=", bign_df$BIGN, ")"),
  bign_df$TRTA
)
# ordered TRTA list
trt_order <- bign_df$TRTA  # already sorted

## Step 2: dm dataset -----------------------------------------------------
dm <- pop
# derive AGEGR1
dm$AGEGR1 <- NA_character_
dm$AGEGR1[dm$AGE < 18] <- "<18"
dm$AGEGR1[dm$AGE >= 18 & dm$AGE < 65] <- "18 to <65"
dm$AGEGR1[dm$AGE >= 65] <- ">=65"

dm$SEXN <- NA_integer_
dm$SEXN[dm$SEX == "M"] <- 1L
dm$SEXN[dm$SEX == "F"] <- 2L

dm$RACEN <- NA_integer_
dm$RACEN[dm$RACE == "WHITE"] <- 1L
dm$RACEN[dm$RACE == "BLACK OR AFRICAN AMERICAN"] <- 2L
dm$RACEN[dm$RACE == "ASIAN"] <- 3L
dm$RACEN[!(dm$RACE %in% c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN"))] <- 4L

dm$ETHNICN <- NA_integer_
dm$ETHNICN[dm$ETHNIC == "HISPANIC OR LATINO"] <- 1L
dm$ETHNICN[dm$ETHNIC == "NOT HISPANIC OR LATINO"] <- 2L
dm$ETHNICN[!(dm$ETHNIC %in% c("HISPANIC OR LATINO","NOT HISPANIC OR LATINO"))] <- 3L

## helper: cont stats -----------------------------------------------------
cont_stats <- function(inds, anavar, trtvar = "TRTA", decimals = 1) {
  dp <- decimals
  dp1 <- decimals + 1
  trts <- sort(unique(inds[[trtvar]]))
  out_list <- list()
  for (tr in trts) {
    vals <- inds[inds[[trtvar]] == tr, anavar]
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    if (n > 0) {
      mn <- mean(vals)
      sdv <- if (n > 1) stats::sd(vals) else NA_real_
      med <- stats::median(vals)
      q1 <- as.numeric(stats::quantile(vals, probs = 0.25, type = 2, names = FALSE))
      q3 <- as.numeric(stats::quantile(vals, probs = 0.75, type = 2, names = FALSE))
      mnv <- min(vals)
      mxv <- max(vals)
    } else {
      mn <- sdv <- med <- q1 <- q3 <- mnv <- mxv <- NA_real_
    }
    # build 6 rows
    rows <- data.frame(
      TRTA = tr,
      stat = c("n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"),
      ord = c(1, 2, 3, 4, 5, 6),
      stringsAsFactors = FALSE
    )
    vals_fmt <- character(6)
    vals_fmt[1] <- fmt_int(n)
    vals_fmt[2] <- if (n > 0) fmt_num(mn, dp1) else ""
    vals_fmt[3] <- if (n > 1) fmt_num(sdv, dp1) else ""
    vals_fmt[4] <- if (n > 0) fmt_num(med, dp) else ""
    vals_fmt[5] <- if (n > 0) paste0(fmt_num(q1, dp), ", ", fmt_num(q3, dp)) else ""
    vals_fmt[6] <- if (n > 0) paste0(fmt_num(mnv, dp), ", ", fmt_num(mxv, dp)) else ""
    rows$value <- vals_fmt
    rows$n <- n
    out_list[[tr]] <- rows
  }
  do.call(rbind, out_list)
}

cat_stats <- function(inds, catvar, trtvar = "TRTA", bign_df) {
  # count distinct USUBJID per TRTA, catvar
  # inds already filtered
  # remove rows where catvar is NA? SAS group by includes missing? But we count distinct where catvar present; missing would be a group. Keep NA as level.
  # Use aggregate
  df <- inds[, c(trtvar, catvar, "USUBJID"), drop = FALSE]
  # create key
  # distinct subjects per cell: count distinct USUBJID
  # Use dplyr-like
  uniq <- unique(df)
  # count
  cnt_df <- aggregate(USUBJID ~ ., data = uniq, FUN = length)
  # rename
  names(cnt_df)[names(cnt_df) == "USUBJID"] <- "cnt"
  # merge bign
  cnt_df <- merge(cnt_df, bign_df, by = trtvar, all.x = TRUE, sort = FALSE)
  cnt_df$pct <- ifelse(cnt_df$BIGN > 0, cnt_df$cnt / cnt_df$BIGN * 100, NA_real_)
  # format value: cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') else '0'
  cnt_df$value <- ifelse(cnt_df$cnt > 0,
                         paste0(fmt_int(cnt_df$cnt), " (", fmt_num(cnt_df$pct, 1), ")"),
                         "0")
  # keep needed
  cnt_df$stat <- cnt_df[[catvar]]
  cnt_df$ord <- NA_integer_
  cnt_df[, c(trtvar, "stat", "ord", "value", "cnt", "pct", "BIGN")]
}

## Step 3: continuous blocks ---------------------------------------------
c_age <- cont_stats(dm, "AGE", decimals = 1)
c_age$category <- "Age (years)"
c_age$block <- 1L

c_weight <- cont_stats(dm, "WEIGHTBL", decimals = 1)
c_weight$category <- "Weight (kg)"
c_weight$block <- 5L

c_height <- cont_stats(dm, "HEIGHTBL", decimals = 1)
c_height$category <- "Height (cm)"
c_height$block <- 6L

c_bmi <- cont_stats(dm, "BMIBL", decimals = 1)
c_bmi$category <- "BMI (kg/m^{super 2})"
c_bmi$block <- 7L

## Step 4: categorical blocks ---------------------------------------------
f_sex <- cat_stats(dm, "SEX", bign_df = bign_df)
f_sex$category <- "Sex"
f_sex$block <- 2L

f_race <- cat_stats(dm, "RACE", bign_df = bign_df)
f_race$category <- "Race"
f_race$block <- 3L

f_ethnic <- cat_stats(dm, "ETHNIC", bign_df = bign_df)
f_ethnic$category <- "Ethnicity"
f_ethnic$block <- 4L

f_agegr1 <- cat_stats(dm, "AGEGR1", bign_df = bign_df)
f_agegr1$category <- "Age Group"
f_agegr1$block <- 8L

## Step 5: combine ---------------------------------------------------------
# select common columns: TRTA, category, stat, ord, value, block
all_stats <- rbind(
  c_age[, c("TRTA", "category", "stat", "ord", "value", "block")],
  f_sex[, c("TRTA", "category", "stat", "ord", "value", "block")],
  f_race[, c("TRTA", "category", "stat", "ord", "value", "block")],
  f_ethnic[, c("TRTA", "category", "stat", "ord", "value", "block")],
  c_weight[, c("TRTA", "category", "stat", "ord", "value", "block")],
  c_height[, c("TRTA", "category", "stat", "ord", "value", "block")],
  c_bmi[, c("TRTA", "category", "stat", "ord", "value", "block")],
  f_agegr1[, c("TRTA", "category", "stat", "ord", "value", "block")]
)

# sort by block ord stat (SAS missing first)
# For ord NA, treat as -Inf so they sort first within block
all_stats$ord_sort <- ifelse(is.na(all_stats$ord), -999L, all_stats$ord)
all_stats <- all_stats[order(all_stats$block, all_stats$ord_sort, all_stats$stat), , drop = FALSE]
all_stats$ord_sort <- NULL

## Step 6: transpose -------------------------------------------------------
# pivot wider by TRTA
# Use tidyr
library(tidyr)
library(dplyr)

wide <- all_stats %>%
  select(block, category, ord, stat, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = NA_character_)

# Ensure columns for each TRTA in trt_order exist
for (tr in trt_order) {
  if (!tr %in% names(wide)) wide[[tr]] <- NA_character_
}
# Order columns: block, category, ord, stat, then TRTA in trt_order
wide <- wide[, c("block", "category", "ord", "stat", trt_order), drop = FALSE]

# Rename to col1, col2, col3 as SAS patch expects (sorted TRTA order)
# Map trt_order[1] -> col1 etc.
col_map <- setNames(paste0("col", seq_along(trt_order)), trt_order)
names(wide)[match(trt_order, names(wide))] <- col_map[trt_order]

# Ensure col1-col3 exist (if ntrt=3)
for (i in 1:3) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide)) wide[[cn]] <- NA_character_
}

# Sort again by block ord stat (ord NA first)
wide$ord_sort <- ifelse(is.na(wide$ord), -999L, wide$ord)
wide <- wide[order(wide$block, wide$ord_sort, wide$stat), , drop = FALSE]
wide$ord_sort <- NULL

## Step 7: report dataset --------------------------------------------------
# display_cat only on first row of block+category
wide <- wide %>%
  group_by(block, category) %>%
  mutate(
    display_cat = ifelse(row_number() == 1, category, ""),
    stat_disp = paste0("  ", stat)
  ) %>%
  ungroup() %>%
  arrange(block, ord, stat) %>%
  # need to handle ord NA ordering again
  mutate(ord_sort = ifelse(is.na(ord), -999L, ord)) %>%
  arrange(block, ord_sort, stat) %>%
  select(block, category, display_cat, stat = stat_disp, col1, col2, col3, ord) %>%
  as.data.frame()

# Replace NA with "" for display
for (cn in c("col1","col2","col3")) {
  wide[[cn]][is.na(wide[[cn]])] <- ""
}

report <- wide

## Step 8: outputs ---------------------------------------------------------
# CSV of report (table body)
out_csv <- file.path(out, "t_dm.csv")
write.csv(report[, c("display_cat","stat","col1","col2","col3")],
          out_csv, row.names = FALSE, na = "")

# Also write full report with block/category for traceability
# (not required but helpful)

# Text rendering ----------------------------------------------------------
# Build header strings
hdr1 <- col_hdrs[trt_order[1]]
hdr2 <- if (length(trt_order) >= 2) col_hdrs[trt_order[2]] else ""
hdr3 <- if (length(trt_order) >= 3) col_hdrs[trt_order[3]] else ""

txt_file <- file.path(out, "t_dm.txt")
con <- file(txt_file, open = "wt")

writeLines("Table 14.1.1", con)
writeLines("Summary of Demographics and Baseline Characteristics", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)
# column headers
header_line <- sprintf("%-30s %-20s %-20s %-20s %-20s",
                       "Parameter", "Statistic", hdr1, hdr2, hdr3)
writeLines(header_line, con)
writeLines(paste(rep("-", nchar(header_line)), collapse = ""), con)

# iterate blocks
current_block <- NA_integer_
for (i in seq_len(nrow(report))) {
  row <- report[i, ]
  # add blank line after block (SAS compute after block line ' ')
  if (!is.na(current_block) && row$block != current_block) {
    writeLines("", con)
  }
  current_block <- row$block
  line <- sprintf("%-30s %-20s %-20s %-20s %-20s",
                  row$display_cat,
                  row$stat,
                  row$col1,
                  row$col2,
                  row$col3)
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Total N=", bign_tot), con)
close(con)

# Also ensure t_dm.rtf placeholder not needed; txt is required
# Write a simple RTF-like text copy for compatibility if expected
rtf_file <- file.path(out, "t_dm.rtf")
if (!file.exists(rtf_file)) {
  file.copy(txt_file, rtf_file, overwrite = TRUE)
}

## cleanup message
cat("t_dm completed: ", nrow(report), " rows, BIGN total ", bign_tot, "\n")
```