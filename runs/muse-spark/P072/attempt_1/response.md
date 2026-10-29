```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

adsl_files <- list.files(adam_path, pattern = "(?i)adsl", full.names = TRUE)
if (length(adsl_files) == 0) stop("ADSL file not found in adam_path")
adsl_file <- adsl_files[1]
cat("Reading ADSL:", adsl_file, "\n")
if (grepl("\\.xpt$", adsl_file, ignore.case = TRUE)) {
  adsl_raw <- haven::read_xpt(adsl_file)
} else {
  adsl_raw <- haven::read_sas(adsl_file)
}
adsl_raw <- haven::zap_labels(adsl_raw)
adsl_raw <- haven::zap_formats(adsl_raw)
adsl_raw <- haven::zap_label(adsl_raw)
adsl <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
names(adsl) <- toupper(names(adsl))

# helpers: SAS semantics
round_half_up <- function(x, digits = 0) {
  res <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is.finite(x)
  res[ok] <- sign(x[ok]) * floor(abs(x[ok]) * 10^digits + 0.5 + 1e-12) / (10^digits)
  # preserve Inf
  res[!is.na(x) & !is.finite(x)] <- x[!is.na(x) & !is.finite(x)]
  res
}
fmt_fixed <- function(x, digits) {
  vapply(seq_along(x), function(i) {
    if (length(x) == 0) return("")
    xi <- x[i]
    if (is.na(xi)) return("")
    r <- round_half_up(xi, digits)
    sprintf(paste0("%.", digits, "f"), r)
  }, character(1))
}
c_sort <- function(x) {
  # SAS byte ordering via C locale
  stringr::str_sort(x, locale = "C")
}

# Step 1: Big N (m_counts) : upcase(SAFFL)='Y', group by TRTA, order by TRTA (SAS order)
adsl$SAFFL_UP <- toupper(adsl$SAFFL)
pop <- adsl[toupper(adsl$SAFFL) == "Y" & !is.na(adsl$SAFFL), , drop = FALSE]
# distinct subjects per TRTA
bign_df <- aggregate(USUBJID ~ TRTA, data = pop,
  FUN = function(v) length(unique(v)))
names(bign_df) <- c("TRTA", "bign")
# SAS order for TRTA
lvl_trt_sorted <- c_sort(unique(bign_df$TRTA))
bign_df <- bign_df[match(bign_df$TRTA, lvl_trt_sorted), , drop = FALSE]
bign_df <- bign_df[order(match(bign_df$TRTA, lvl_trt_sorted)), , drop = FALSE]
rownames(bign_df) <- NULL
bigntot <- length(unique(pop$USUBJID))
cat("Big N:\n")
for (i in seq_len(nrow(bign_df))) cat(" ", bign_df$TRTA[i], " N=", bign_df$bign[i], "\n")
cat(" Total N=", bigntot, "\n")

# identify col mapping: col1=Placebo, col2=Low, col3=High (as SAS rename)
trt_vals <- unique(bign_df$TRTA)
find_trt <- function(pat) {
  hit <- trt_vals[grepl(pat, trt_vals, fixed = TRUE)]
  if (length(hit) == 0) return(NA_character_) else return(hit[1])
}
trt_pbo <- find_trt("Placebo")
trt_low <- find_trt("Low")
trt_high <- find_trt("High")
# fallback if pattern not found (use sorted order)
if (is.na(trt_pbo) || is.na(trt_low) || is.na(trt_high)) {
  s <- c_sort(trt_vals)
  # try to keep 3
  if (length(s) >= 3) {
    if (is.na(trt_pbo)) trt_pbo <- s[1]
    if (is.na(trt_low)) trt_low <- s[2]
    if (is.na(trt_high)) trt_high <- s[3]
  }
}
cat("Mapping col1=", trt_pbo, " col2=", trt_low, " col3=", trt_high, "\n")
get_bign <- function(tr) {
  v <- bign_df$bign[bign_df$TRTA == tr]
  if (length(v) == 0) return(0) else return(v[1])
}
bign1 <- get_bign(trt_pbo); bign2 <- get_bign(trt_low); bign3 <- get_bign(trt_high)
hdr1 <- paste0(trt_pbo, " (N=", bign1, ")")
hdr2 <- paste0(trt_low, " (N=", bign2, ")")
hdr3 <- paste0(trt_high, " (N=", bign3, ")")

# Step 2: dm dataset, where SAFFL='Y' (case-sensitive as in SAS data step)
dm <- adsl[adsl$SAFFL == "Y" & !is.na(adsl$SAFFL), , drop = FALSE]
# Derive AGEGR1 with SAS missing semantics (missing < 18 => '<18')
dm$AGEGR1 <- vapply(seq_len(nrow(dm)), function(i) {
  a <- dm$AGE[i]
  if (is.na(a)) return("<18")
  if (a < 18) return("<18")
  else if (a < 65) return("18 to <65")
  else if (a >= 65) return(">=65")
  else return(NA_character_)
}, character(1))

# Step 3: continuous stats
cont_block_fn <- function(var, label, ord_block, decimals = 1) {
  dp <- decimals; dp1 <- decimals + 1
  trts <- bign_df$TRTA
  out_list <- list()
  for (tr in trts) {
    vals <- dm[[var]][dm$TRTA == tr]
    # PROC MEANS uses non-missing
    vals_nm <- vals[!is.na(vals)]
    n <- length(vals_nm)
    if (n > 0) {
      mn <- mean(vals_nm)
      sdv <- if (n > 1) stats::sd(vals_nm) else NA_real_
      med <- stats::quantile(vals_nm, probs = 0.5, type = 2, names = FALSE)
      q1 <- stats::quantile(vals_nm, probs = 0.25, type = 2, names = FALSE)
      q3 <- stats::quantile(vals_nm, probs = 0.75, type = 2, names = FALSE)
      mi <- min(vals_nm); ma <- max(vals_nm)
    } else {
      mn <- sdv <- med <- q1 <- q3 <- mi <- ma <- NA_real_
    }
    v_n <- as.character(n)
    v_mean <- if (n > 0) fmt_fixed(mn, dp1) else ""
    v_sd <- if (n > 1) fmt_fixed(sdv, dp1) else ""
    v_med <- if (n > 0) fmt_fixed(med, dp) else ""
    v_q <- if (n > 0) paste0(fmt_fixed(q1, dp), ", ", fmt_fixed(q3, dp)) else ""
    v_mm <- if (n > 0) paste0(fmt_fixed(mi, dp), ", ", fmt_fixed(ma, dp)) else ""
    out_list[[tr]] <- data.frame(
      TRTA = tr,
      stat = c("n", "Mean", "SD", "Median", "Q1, Q3", "Min, Max"),
      ord = c(1, 2, 3, 4, 5, 6),
      value = c(v_n, v_mean, v_sd, v_med, v_q, v_mm),
      category = label, block = ord_block,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out_list)
}

c_age <- cont_block_fn("AGE", "Age (years)", 1, decimals = 1)
c_wt <- cont_block_fn("WEIGHTBL", "Weight (kg)", 5, decimals = 1)
c_ht <- cont_block_fn("HEIGHTBL", "Height (cm)", 6, decimals = 1)
c_bmi <- cont_block_fn("BMIBL", "BMI (kg/m^{super 2})", 7, decimals = 1)

# Step 4: categorical stats
cat_block_fn <- function(var, label, ord_block) {
  # distinct subjects per TRTA x var (include missing as group like SAS)
  # normalize var to character, keep NA as <NA> group then display as blank?
  vraw <- dm[[var]]
  # haven already character; convert factors to character
  if (is.factor(vraw)) vraw <- as.character(vraw)
  # for grouping, replace NA with special token
  vgrp <- ifelse(is.na(vraw), "__MISSING__", as.character(vraw))
  dtmp <- data.frame(TRTA = dm$TRTA, V = vgrp, USUBJID = dm$USUBJID, stringsAsFactors = FALSE)
  # count distinct USUBJID
  agg <- aggregate(USUBJID ~ TRTA + V, data = dtmp, FUN = function(x) length(unique(x)))
  names(agg) <- c("TRTA", "V", "cnt")
  # merge bign
  agg <- merge(agg, bign_df, by = "TRTA", all.x = TRUE)
  agg$pct <- agg$cnt / agg$bign * 100
  agg$pct_fmt <- fmt_fixed(agg$pct, 1)
  agg$value <- paste0(as.character(agg$cnt), " (", agg$pct_fmt, ")")
  # stat display: original value (blank for missing)
  agg$stat <- ifelse(agg$V == "__MISSING__", "", agg$V)
  agg$category <- label
  agg$block <- ord_block
  agg$ord <- NA_real_
  agg[, c("TRTA", "stat", "ord", "value", "category", "block")]
}
f_sex <- cat_block_fn("SEX", "Sex", 2)
f_race <- cat_block_fn("RACE", "Race", 3)
f_eth <- cat_block_fn("ETHNIC", "Ethnicity", 4)
f_agegr <- cat_block_fn("AGEGR1", "Age Group", 8)

# Step 5: combine
all_stats <- rbind(c_age, f_sex, f_race, f_eth, c_wt, c_ht, c_bmi, f_agegr)

# Step 6: sort by block ord stat (SAS order) and transpose
# ord NA first
all_stats$ord_tmp <- ifelse(is.na(all_stats$ord), -999, all_stats$ord)
# C-locale rank for stat
uniq_stat <- unique(all_stats$stat)
sorted_stat <- c_sort(uniq_stat)
stat_rank <- match(all_stats$stat, sorted_stat)
all_stats$stat_rank <- stat_rank
all_stats <- all_stats[order(all_stats$block, all_stats$ord_tmp, all_stats$stat_rank), , drop = FALSE]

# pivot wider: by block category ord stat, columns TRTA
final_w <- tidyr::pivot_wider(all_stats,
  id_cols = c(block, category, ord, stat),
  names_from = TRTA, values_from = value)
# ensure col mapping exists; create col1/2/3
# pivot column names are TRTA values; extract
for (tr in c(trt_pbo, trt_low, trt_high)) {
  if (!tr %in% names(final_w)) {
    final_w[[tr]] <- NA_character_
  }
}
final <- as.data.frame(final_w, stringsAsFactors = FALSE)
final$col1 <- as.character(final[[trt_pbo]])
final$col2 <- as.character(final[[trt_low]])
final$col3 <- as.character(final[[trt_high]])
final$col1[is.na(final$col1)] <- ""
final$col2[is.na(final$col2)] <- ""
final$col3[is.na(final$col3)] <- ""
# re-sort by block ord stat (same order)
final$ord_tmp <- ifelse(is.na(final$ord), -999, final$ord)
sorted_stat2 <- c_sort(unique(final$stat))
final$stat_rank <- match(final$stat, sorted_stat2)
final <- final[order(final$block, final$ord_tmp, final$stat_rank), , drop = FALSE]

# Step 7: report dataset with display_cat and indented stat
# first.category logic: first row per block+category
final <- final[order(final$block, final$ord_tmp, final$stat_rank), , drop = FALSE]
final$display_cat <- ""
final$stat_disp <- paste0("  ", trimws(as.character(final$stat)))
# group by block, category
key <- paste0(final$block, "|", final$category)
first_idx <- !duplicated(key)
final$display_cat[first_idx] <- final$category[first_idx]
report <- final[, c("block", "category", "display_cat", "stat", "stat_disp", "ord", "col1", "col2", "col3")]
# keep order
report <- report[order(report$block, ifelse(is.na(report$ord), -999, report$ord), match(report$stat, c_sort(unique(report$stat)))), , drop = FALSE]

# Step 8: outputs
# CSV of table body
csv_df <- data.frame(
  Parameter = report$display_cat,
  Statistic = report$stat_disp,
  check.names = FALSE, stringsAsFactors = FALSE
)
csv_df[[hdr1]] <- report$col1
csv_df[[hdr2]] <- report$col2
csv_df[[hdr3]] <- report$col3
write.csv(csv_df, file.path(out, "t_dm.csv"), row.names = FALSE, na = "")

# TXT rendering
con_file <- file.path(out, "t_dm.txt")
sink(con_file)
cat("Table 14.1.1\n")
cat("Summary of Demographics and Baseline Characteristics\n")
cat("Safety Analysis Set\n")
cat("\n")
# header row fixed width
w_param <- 30; w_stat <- 15; w_trt <- 22
fmt_row <- function(a, b, c1, c2, c3) {
  # truncate/pad
  pad <- function(s, w, align = "left") {
    s <- ifelse(is.na(s), "", as.character(s))
    if (nchar(s, type = "chars") > w) s <- substr(s, 1, w)
    if (align == "left") sprintf(paste0("%-", w, "s"), s) else sprintf(paste0("%", w, "s"), s)
  }
  paste0(pad(a, w_param), " | ", pad(b, w_stat), " | ", pad(c1, w_trt, "right"), " | ", pad(c2, w_trt, "right"), " | ", pad(c3, w_trt, "right"))
}
cat(fmt_row("Parameter", "Statistic", hdr1, hdr2, hdr3), "\n")
cat(paste0(strrep("-", w_param), "-+-", strrep("-", w_stat), "-+-", strrep("-", w_trt), "-+-", strrep("-", w_trt), "-+-", strrep("-", w_trt)), "\n")
cur_block <- NA
for (i in seq_len(nrow(report))) {
  cat(fmt_row(report$display_cat[i], report$stat_disp[i], report$col1[i], report$col2[i], report$col3[i]), "\n")
  # compute after block: blank line
  nb <- if (i < nrow(report)) report$block[i + 1] else NA
  if (!is.na(report$block[i]) && (is.na(nb) || nb != report$block[i])) {
    cat("\n")
  }
}
cat("\n")
cat(paste0("Placebo (N=", bign1, "), ", trt_low, " (N=", bign2, "), ", trt_high, " (N=", bign3, "), Total (N=", bigntot, ")\n"))
cat("Data cutoff: [DDMONYYYY]\n")
cat("Source: ADaM datasets\n")
sink()
cat("Wrote", file.path(out, "t_dm.csv"), "and", file.path(out, "t_dm.txt"), "\n")
```