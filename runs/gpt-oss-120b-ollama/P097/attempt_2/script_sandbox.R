# P097_BootstrapROC.R
# Reimplementation of SAS program P097_BootstrapROC.sas in R
# ---------------------------------------------------------
# Required packages (assumed pre‑installed):
# haven, dplyr, tidyr, readr, stringr, lubridate, purrr,
# tibble, jsonlite, data.table, ggplot2, survival, survminer,
# broom, Hmisc, admiral, pharmaversesdtm, readxl, writexl,
# openxlsx, gt, r2rtf, arrow

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(broom)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x)
  factor <- 10^digits
  rounded <- floor(absx * factor + 0.5) / factor
  rounded * posneg
}

# Helper: AUC (Area under ROC) for binary outcome where event = 1
auc_calc <- function(event, pred) {
  pos_idx <- which(event == 1)
  neg_idx <- which(event == 0)
  n_pos <- length(pos_idx)
  n_neg <- length(neg_idx)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  ranks <- rank(pred, ties.method = "average")
  U <- sum(ranks[pos_idx]) - n_pos * (n_pos + 1) / 2
  U / (n_pos * n_neg)
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
roc_vec <- as.numeric(strsplit(trimws(roc_raw), "\\s+")[[1]])
roc_df <- data.frame(
  alb      = roc_vec[seq(1, length(roc_vec), by = 4)],
  tp       = roc_vec[seq(2, length(roc_vec), by = 4)],
  totscore = roc_vec[seq(3, length(roc_vec), by = 4)],
  popind   = roc_vec[seq(4, length(roc_vec), by = 4)]
)
roc_df <- roc_df %>% mutate(totscore = 10 - totscore)

# ------------------------------------------------------------------
# 2. Logistic model (popind event = 0)
roc_df <- roc_df %>% mutate(event0 = ifelse(popind == 0, 1, 0))

logi_fit <- glm(event0 ~ alb + tp + totscore,
                data = roc_df,
                family = binomial(link = "logit"))

roc_df <- roc_df %>%
  mutate(LogiPred = predict(logi_fit, type = "response"))

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
# 4. Merge LogiOut and ExpertPred (by row order)
Survival <- cbind(LogiOut, ExpertPred)
write.csv(Survival,
          file.path(out_dir, "Survival.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 5. Prob table (same as SAS PROC PRINT)
Prob <- data.frame(
  x    = 0.73,
  xbar = 0.8162,
  s    = 0.0801
) %>%
  mutate(z = abs(xbar - x) / s,
         p = 2 * pnorm(-z))
write.csv(Prob,
          file.path(out_dir, "Prob.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 6. Bootstrap ROC AUCs
set.seed(12345)
NumSamples <- 5000
N <- nrow(Survival)

boot_rep   <- integer(NumSamples * 2)
boot_model <- character(NumSamples * 2)
boot_area  <- numeric(NumSamples * 2)

cnt <- 1
for (rep in 1:NumSamples) {
  idx <- sample.int(N, size = N, replace = TRUE)
  samp <- Survival[idx, ]

  event_vec <- ifelse(samp$popind == 0, 1, 0)

  auc_logi <- auc_calc(event_vec, samp$LogiPred)
  auc_exp  <- auc_calc(event_vec, samp$ExpertPred)

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
write.csv(ROCEst,
          file.path(out_dir, "ROCEst.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 7. PROC MEANS equivalent
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
  ) %>% ungroup()
write.csv(MeansSumm,
          file.path(out_dir, "MeansSumm.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 8. PROC UNIVARIATE percentiles (WidePctls)
WidePctls <- ROCEst %>%
  group_by(ROCModel) %>%
  summarise(
    Mean = round_half_up(mean(Area, na.rm = TRUE), 4),
    Std  = round_half_up(sd(Area, na.rm = TRUE), 4),
    P_2_5 = round_half_up(quantile(Area, 0.025, na.rm = TRUE), 4),
    P_97_5 = round_half_up(quantile(Area, 0.975, na.rm = TRUE), 4)
  ) %>% ungroup() %>%
  rename(
    BootMean   = Mean,
    BootStdErr = Std,
    `95% Lower CL` = P_2_5,
    `95% Upper CL` = P_97_5
  )
write.csv(WidePctls,
          file.path(out_dir, "WidePctls.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 9. Logistic model detailed summary (PROC LOGISTIC like output)
logi_tidy   <- broom::tidy(logi_fit) %>%
  mutate(across(where(is.numeric), ~ round_half_up(., 4)))
logi_glance <- broom::glance(logi_fit) %>%
  mutate(across(where(is.numeric), ~ round_half_up(., 4)))

# ------------------------------------------------------------------
# 10. Original‑data AUCs (for reference)
auc_logi_orig <- auc_calc(roc_df$event0, roc_df$LogiPred)
auc_exp_orig  <- auc_calc(roc_df$event0, ExpertPred$ExpertPred[1:nrow(roc_df)])

# ------------------------------------------------------------------
# 11. Create the combined listing (P097.txt) and CSV of numeric values
txt_path  <- file.path(out_dir, "P097.txt")
csv_path  <- file.path(out_dir, "P097.csv")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

fmt_num <- function(x) formatC(as.numeric(x), format = "f", digits = 4)

cat("=== Prob Table ===\n", file = con)
cat(sprintf("%-6s %8s %8s %8s %12s\n",
            "x", "xbar", "s", "z", "p"), file = con)
cat(sprintf("%6s %8s %8s %8s %12s\n",
            fmt_num(Prob$x),
            fmt_num(Prob$xbar),
            fmt_num(Prob$s),
            fmt_num(Prob$z),
            fmt_num(Prob$p)), file = con)
cat("\n", file = con)

cat("=== Original AUCs ===\n", file = con)
cat(sprintf("Logistic AUC : %s\n", fmt_num(auc_logi_orig)), file = con)
cat(sprintf("Expert   AUC : %s\n", fmt_num(auc_exp_orig)), file = con)
cat("\n", file = con)

cat("=== PROC LOGISTIC Parameter Estimates ===\n", file = con)
cat(sprintf("%-12s %12s %12s %12s %12s\n",
            "Variable", "Estimate", "Std.Error", "z value", "Pr>|z|"), file = con)
apply(logi_tidy, 1, function(r) {
  cat(sprintf("%-12s %12s %12s %12s %12s\n",
              r["term"],
              fmt_num(r["estimate"]),
              fmt_num(r["std.error"]),
              fmt_num(r["statistic"]),
              fmt_num(r["p.value"])), file = con)
})
cat("\n", file = con)

cat("=== PROC LOGISTIC Model Fit Statistics ===\n", file = con)
cat(sprintf("%-20s %12s\n", "Null Deviance", fmt_num(logi_glance$null.deviance)), file = con)
cat(sprintf("%-20s %12s\n", "Residual Deviance", fmt_num(logi_glance$deviance)), file = con)
cat(sprintf("%-20s %12s\n", "AIC", fmt_num(logi_glance$AIC)), file = con)
cat(sprintf("%-20s %12s\n", "BIC", fmt_num(logi_glance$BIC)), file = con)
cat(sprintf("%-20s %12s\n", "LogLik", fmt_num(logi_glance$logLik)), file = con)
cat("\n", file = con)

cat("=== PROC MEANS Summary (Area) ===\n", file = con)
header_means <- sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s %8s",
                       "ROCModel", "Mean", "Std", "P5", "P95", "P10", "P90", "P25", "P75")
cat(header_means, "\n", file = con)
apply(MeansSumm, 1, function(r) {
  cat(sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              r["ROCModel"],
              fmt_num(r["Mean"]),
              fmt_num(r["Std"]),
              fmt_num(r["P5"]),
              fmt_num(r["P95"]),
              fmt_num(r["P10"]),
              fmt_num(r["P90"]),
              fmt_num(r["P25"]),
              fmt_num(r["P75"])), file = con)
})
cat("\n", file = con)

cat("=== Bootstrap Percentiles (WidePctls) ===\n", file = con)
header_pct <- sprintf("%-10s %10s %10s %12s %12s",
                      "ROCModel", "BootMean", "BootStdErr", "95% Lower CL", "95% Upper CL")
cat(header_pct, "\n", file = con)
apply(WidePctls, 1, function(r) {
  cat(sprintf("%-10s %10s %10s %12s %12s\n",
              r["ROCModel"],
              fmt_num(r["BootMean"]),
              fmt_num(r["BootStdErr"]),
              fmt_num(r["95% Lower CL"]),
              fmt_num(r["95% Upper CL"])), file = con)
})
close(con)

# CSV of all numeric values (order matches appearance in the text)
numeric_vals <- c(
  Prob$x, Prob$xbar, Prob$s, Prob$z, Prob$p,
  auc_logi_orig, auc_exp_orig,
  MeansSumm$Mean, MeansSumm$Std, MeansSumm$P5, MeansSumm$P95,
  MeansSumm$P10, MeansSumm$P90, MeansSumm$P25, MeansSumm$P75,
  WidePctls$BootMean, WidePctls$BootStdErr,
  WidePctls$`95% Lower CL`, WidePctls$`95% Upper CL`,
  logi_tidy$estimate, logi_tidy$std.error,
  logi_tidy$statistic, logi_tidy$p.value,
  logi_glance$null.deviance, logi_glance$deviance,
  logi_glance$AIC, logi_glance$BIC, logi_glance$logLik
)
write.csv(data.frame(Value = numeric_vals),
          csv_path,
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 12. Histogram of Area (all models) – Figure "Histogram.png"
hist_data_path <- file.path(out_dir, "Histogram.csv")
write.csv(ROCEst, hist_data_path, row.names = FALSE, na = "")

hist_plot <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(binwidth = 0.01, colour = "black", fill = "steelblue") +
  labs(title = "Histogram of Area Under the Curve (All Models)",
       x = "Area", y = "Count") +
  theme_minimal()

ggsave(filename = file.path(out_dir, "Histogram.png"),
       plot = hist_plot,
       width = 640/150, height = 481/150, units = "in", dpi = 150)

# ------------------------------------------------------------------
# 13. SGPanel – panelled histograms by ROCModel – Figure "SGPanel.png"
sgpanel_data_path <- file.path(out_dir, "SGPanel.csv")
write.csv(ROCEst, sgpanel_data_path, row.names = FALSE, na = "")

sgpanel_plot <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(binwidth = 0.01, colour = "black", fill = "steelblue") +
  facet_wrap(~ ROCModel, nrow = 2, scales = "free_y") +
  labs(title = "Bootstrap Distributions of Area Under the Curve for Two Models",
       x = "Area", y = "Count") +
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.1), limits = c(0.4, 1)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey80"))

ggsave(filename = file.path(out_dir, "SGPanel.png"),
       plot = sgpanel_plot,
       width = 640/150, height = 481/150, units = "in", dpi = 150)

# ------------------------------------------------------------------
# End of script
