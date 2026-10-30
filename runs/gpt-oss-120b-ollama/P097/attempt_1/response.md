```r
# P097_BootstrapROC.R
# Reimplementation of SAS program P097_BootstrapROC.sas in R
# ---------------------------------------------------------
# Required packages are assumed to be installed:
# haven, dplyr, tidyr, readr, stringr, lubridate, purrr,
# tibble, jsonlite, data.table, ggplot2, survival, survminer,
# broom, Hmisc, admiral, pharmaversesdtm, readxl, writexl,
# openxlsx, gt, r2rtf, arrow

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x)
  factor <- 10^digits
  rounded <- floor(absx * factor + 0.5) / factor
  return(rounded * posneg)
}

# Helper: AUC (Area under ROC) for binary outcome where event = 1
auc_calc <- function(event, pred) {
  # event: 1 = event (popind = 0 in original data), 0 = non‑event
  pos_idx <- which(event == 1)
  neg_idx <- which(event == 0)
  n_pos <- length(pos_idx)
  n_neg <- length(neg_idx)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  ranks <- rank(pred, ties.method = "average")
  U <- sum(ranks[pos_idx]) - n_pos * (n_pos + 1) / 2
  auc <- U / (n_pos * n_neg)
  return(auc)
}

# ------------------------------------------------------------------
# Output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Create the ROC data set (inline data)
roc_raw <- "
3.0 5.8 10 0   3.2 6.3  5 1   3.9 6.8  3 1   2.8 4.8  6 0
3.2 5.8  3 1   0.9 4.0  5 0   2.5 5.7  8 0   1.6 5.6  5 1
3.8 5.7  5 1   3.7 6.7  6 1   3.2 5.4  4 1   3.8 6.6  6 1
4.1 6.6  5 1   3.6 5.7  5 1   4.3 7.0  4 1   3.6 6.7  4 0
2.3 4.4  6 1   4.2 7.6  4 0   4.0 6.6  6 0   3.5 5.8  6 1
3.8 6.8  7 1   3.0 4.7  8 0   4.5 7.4  5 1   3.7 7.4  5 1
3.1 6.6  6 1   4.1 8.2  6 1   4.3 7.0  5 1   4.3 6.5  4 1
3.2 5.1  5 1   2.6 4.7  6 1   3.3 6.8  6 0   1.7 4.0  7 0
3.7 6.1  5 1   3.3 6.3  7 1   4.2 7.7  6 1   3.5 6.2  5 1
2.9 5.7  9 0   2.1 4.8  7 1   2.8 6.2  8 0   4.0 7.0  7 1
3.3 5.7  6 1   3.7 6.9  5 1   3.6 6.6  5 1
"
# Split into numeric vector
roc_vec <- as.numeric(strsplit(trimws(roc_raw), "\\s+")[[1]])
roc_df <- data.frame(
  alb = roc_vec[seq(1, length(roc_vec), by = 4)],
  tp  = roc_vec[seq(2, length(roc_vec), by = 4)],
  totscore = roc_vec[seq(3, length(roc_vec), by = 4)],
  popind   = roc_vec[seq(4, length(roc_vec), by = 4)]
)
# Apply transformation: totscore = 10 - totscore
roc_df <- roc_df %>% mutate(totscore = 10 - totscore)

# ------------------------------------------------------------------
# 2. Logistic model (popind event = 0)
# Create binary event variable: 1 when popind == 0
roc_df <- roc_df %>% mutate(event0 = ifelse(popind == 0, 1, 0))

logi_fit <- glm(event0 ~ alb + tp + totscore,
                data = roc_df,
                family = binomial(link = "logit"))

roc_df <- roc_df %>%
  mutate(LogiPred = predict(logi_fit, type = "response"))

# Save LogiOut (SAS output dataset) as CSV
LogiOut <- roc_df %>% select(alb, tp, totscore, popind, LogiPred)
write.csv(LogiOut,
          file.path(out_dir, "LogiOut.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 3. Expert predictions (inline data)
expert_raw <- "
0.95 0.2  0.05 0.3  0.1  0.6  0.8  0.5 
0.1  0.25 0.1  0.2  0.05 0.1  0.05 0.1 
0.4  0.1  0.2  0.25 0.4  0.7  0.1  0.1 
0.3  0.2  0.1  0.05 0.1  0.4  0.4  0.7
0.2  0.4  0.1  0.1  0.9  0.7  0.8  0.25
0.3  0.1  0.1 
"
expert_vec <- as.numeric(strsplit(trimws(expert_raw), "\\s+")[[1]])
ExpertPred <- data.frame(ExpertPred = expert_vec)

# ------------------------------------------------------------------
# 4. Merge LogiOut and ExpertPred (by observation order)
Survival <- cbind(LogiOut, ExpertPred)

# Save Survival dataset
write.csv(Survival,
          file.path(out_dir, "Survival.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 5. Compute single‑row table "Prob"
Prob <- data.frame(
  x = 0.73,
  xbar = 0.8162,
  s = 0.0801
) %>%
  mutate(z = abs(xbar - x) / s,
         p = 2 * pnorm(-z))

# Save Prob table
write.csv(Prob,
          file.path(out_dir, "Prob.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 6. Bootstrap ROC AUCs
set.seed(12345)  # match SAS seed
NumSamples <- 5000
N <- nrow(Survival)

# Pre‑allocate vectors
boot_rep   <- integer(NumSamples * 2)
boot_model <- character(NumSamples * 2)
boot_area  <- numeric(NumSamples * 2)

cnt <- 1
for (rep in 1:NumSamples) {
  idx <- sample.int(N, size = N, replace = TRUE)
  sample_dat <- Survival[idx, ]

  # Event definition for AUC (popind == 0)
  event_vec <- ifelse(sample_dat$popind == 0, 1, 0)

  # Logistic model AUC
  auc_logi <- auc_calc(event_vec, sample_dat$LogiPred)

  # Expert model AUC
  auc_exp  <- auc_calc(event_vec, sample_dat$ExpertPred)

  # Store
  boot_rep[cnt]   <- rep
  boot_model[cnt] <- "Logistic"
  boot_area[cnt]  <- auc_logi
  cnt <- cnt + 1

  boot_rep[cnt]   <- rep
  boot_model[cnt] <- "Expert"
  boot_area[cnt]  <- auc_exp
  cnt <- cnt + 1
}

ROCEst <- data.frame(
  Replicate = boot_rep,
  ROCModel  = boot_model,
  Area      = boot_area,
  stringsAsFactors = FALSE
)

# Save bootstrap results
write.csv(ROCEst,
          file.path(out_dir, "ROCEst.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 7. Summary statistics (PROC MEANS equivalent)
MeansSumm <- ROCEst %>%
  group_by(ROCModel) %>%
  summarise(
    Mean = round_half_up(mean(Area, na.rm = TRUE), 4),
    Std  = round_half_up(sd(Area, na.rm = TRUE), 4),
    P5   = round_half_up(quantile(Area, 0.05, na.rm = TRUE), 4),
    P95  = round_half_up(quantile(Area, 0.95, na.rm = TRUE), 4),
    P10  = round_half_up(quantile(Area, 0.10, na.rm = TRUE), 4),
    P90  = round_half_up(quantile(Area, 0.90, na.rm = TRUE), 4),
    P25  = round_half_up(quantile(Area, 0.25, na.rm = TRUE), 4),
    P75  = round_half_up(quantile(Area, 0.75, na.rm = TRUE), 4)
  ) %>%
  ungroup()

write.csv(MeansSumm,
          file.path(out_dir, "MeansSumm.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 8. Percentiles (PROC UNIVARIATE equivalent)
WidePctls <- ROCEst %>%
  group_by(ROCModel) %>%
  summarise(
    Mean = round_half_up(mean(Area, na.rm = TRUE), 4),
    Std  = round_half_up(sd(Area, na.rm = TRUE), 4),
    P_2_5 = round_half_up(quantile(Area, 0.025, na.rm = TRUE), 4),
    P_97_5 = round_half_up(quantile(Area, 0.975, na.rm = TRUE), 4)
  ) %>%
  ungroup() %>%
  rename(
    BootMean = Mean,
    BootStdErr = Std,
    `95% Lower CL` = P_2_5,
    `95% Upper CL` = P_97_5
  )

write.csv(WidePctls,
          file.path(out_dir, "WidePctls.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 9. Create listing file (P097.lst) – plain‑text rendering
listing_path <- file.path(out_dir, "P097.lst")
con <- file(listing_path, open = "wt", encoding = "UTF-8")

cat("=== Prob Table ===\n", file = con)
prob_fmt <- function(x) sprintf("%0.4f", x)
cat(sprintf("%-6s %8s %8s %8s %8s %12s\n",
            "x", "xbar", "s", "z", "p", ""), file = con)
cat(sprintf("%6s %8s %8s %8s %12s\n",
            prob_fmt(Prob$x),
            prob_fmt(Prob$xbar),
            prob_fmt(Prob$s),
            prob_fmt(Prob$z),
            prob_fmt(Prob$p)), file = con)
cat("\n", file = con)

cat("=== PROC MEANS Summary (Area) ===\n", file = con)
means_fmt <- function(x) sprintf("%0.4f", x)
header_means <- sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s %8s",
                        "ROCModel", "Mean", "Std", "P5", "P95", "P10", "P90", "P25", "P75")
cat(header_means, "\n", file = con)
apply(MeansSumm, 1, function(row) {
  cat(sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              row["ROCModel"],
              means_fmt(row["Mean"]),
              means_fmt(row["Std"]),
              means_fmt(row["P5"]),
              means_fmt(row["P95"]),
              means_fmt(row["P10"]),
              means_fmt(row["P90"]),
              means_fmt(row["P25"]),
              means_fmt(row["P75"])), file = con)
})
cat("\n", file = con)

cat("=== Bootstrap Percentiles (WidePctls) ===\n", file = con)
pct_fmt <- function(x) sprintf("%0.4f", x)
header_pct <- sprintf("%-10s %10s %10s %12s %12s",
                      "ROCModel", "BootMean", "BootStdErr", "95% Lower CL", "95% Upper CL")
cat(header_pct, "\n", file = con)
apply(WidePctls, 1, function(row) {
  cat(sprintf("%-10s %10s %10s %12s %12s\n",
              row["ROCModel"],
              pct_fmt(row["BootMean"]),
              pct_fmt(row["BootStdErr"]),
              pct_fmt(row["95% Lower CL"]),
              pct_fmt(row["95% Upper CL"])), file = con)
})
close(con)

# ------------------------------------------------------------------
# 10. Histogram of Area (overall) – Figure "Histogram.png"
hist_data_path <- file.path(out_dir, "Histogram.csv")
write.csv(ROCEst, hist_data_path, row.names = FALSE, na = "")

hist_plot <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "steelblue") +
  labs(title = "Histogram of AUC (Area) – All Models",
       x = "Area", y = "Count") +
  theme_minimal()

ggsave(filename = file.path(out_dir, "Histogram.png"),
       plot = hist_plot,
       width = 6.4, height = 4.81, units = "in", dpi = 150)

# ------------------------------------------------------------------
# 11. SGPanel – panelled histograms by ROCModel – Figure "SGPanel.png"
sgpanel_data_path <- file.path(out_dir, "SGPanel.csv")
write.csv(ROCEst, sgpanel_data_path, row.names = FALSE, na = "")

sgpanel_plot <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "steelblue") +
  facet_wrap(~ ROCModel, nrow = 2, scales = "free_y") +
  labs(title = "Bootstrap Distributions of Area Under the Curve for Two Models",
       x = "Area", y = "Count") +
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.1), limits = c(0.4, 1)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey80"))

ggsave(filename = file.path(out_dir, "SGPanel.png"),
       plot = sgpanel_plot,
       width = 6.4, height = 4.81, units = "in", dpi = 150)

# ------------------------------------------------------------------
# End of script
```