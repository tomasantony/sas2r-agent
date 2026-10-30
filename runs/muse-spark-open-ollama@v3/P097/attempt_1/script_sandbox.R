# P097 Bootstrap ROC - R translation of SAS program P097_BootstrapROC.sas
# Reproduces logistic model, ROC AUC, bootstrap distribution, histograms

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

# Helper: SAS PCTLDEF=5 percentile
sas_pctl <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (p <= 0) return(x[1])
  if (p >= 100) return(x[n])
  np <- n * p / 100
  j <- floor(np)
  g <- np - j
  # tolerance for floating point integer check
  if (abs(g) < 1e-12) {
    # integer case: average of x[np] and x[np+1]
    # np is integer, 1-indexed
    idx <- as.integer(round(np))
    if (idx < 1) idx <- 1
    if (idx >= n) return(x[n])
    return((x[idx] + x[idx + 1]) / 2)
  } else {
    idx <- ceiling(np)
    if (idx < 1) idx <- 1
    if (idx > n) idx <- n
    return(x[idx])
  }
}

# Helper: AUC via Mann-Whitney (concordant + 0.5*tied)/pairs
auc_calc <- function(pred, event) {
  # event = 1 for event (popind=0), 0 for non-event
  x <- pred[event == 1]
  y <- pred[event == 0]
  m <- length(x); n <- length(y)
  if (m == 0 || n == 0) return(NA_real_)
  # matrix of comparisons: 1 if x>y, 0.5 if equal, 0 otherwise
  mat <- outer(x, y, function(a, b) ifelse(a > b, 1, ifelse(a == b, 0.5, 0)))
  auc <- mean(mat)
  return(auc)
}

# Helper: DeLong SE for AUC
delong_se <- function(pred, event) {
  x <- pred[event == 1]
  y <- pred[event == 0]
  m <- length(x); n <- length(y)
  if (m < 2 || n < 2) return(list(auc = auc_calc(pred, event), se = NA_real_))
  mat <- outer(x, y, function(a, b) ifelse(a > b, 1, ifelse(a == b, 0.5, 0)))
  V10 <- rowMeans(mat)
  V01 <- colMeans(mat)
  auc <- mean(V10)
  S10 <- var(V10)
  S01 <- var(V01)
  var_auc <- S10 / m + S01 / n
  se <- sqrt(var_auc)
  return(list(auc = auc, se = se, V10 = V10, V01 = V01))
}

# ------------------------------------------------------------
# 1. Create roc dataset (43 observations)
# ------------------------------------------------------------
alb <- c(3.0,3.2,3.9,2.8, 3.2,0.9,2.5,1.6, 3.8,3.7,3.2,3.8, 4.1,3.6,4.3,3.6,
         2.3,4.2,4.0,3.5, 3.8,3.0,4.5,3.7, 3.1,4.1,4.3,4.3, 3.2,2.6,3.3,1.7,
         3.7,3.3,4.2,3.5, 2.9,2.1,2.8,4.0, 3.3,3.7,3.6)
tp <- c(5.8,6.3,6.8,4.8, 5.8,4.0,5.7,5.6, 5.7,6.7,5.4,6.6, 6.6,5.7,7.0,6.7,
        4.4,7.6,6.6,5.8, 6.8,4.7,7.4,7.4, 6.6,8.2,7.0,6.5, 5.1,4.7,6.8,4.0,
        6.1,6.3,7.7,6.2, 5.7,4.8,6.2,7.0, 5.7,6.9,6.6)
totscore_raw <- c(10,5,3,6, 3,5,8,5, 5,6,4,6, 5,5,4,4, 6,4,6,6, 7,8,5,5, 6,6,5,4, 5,6,6,7, 5,7,6,5, 9,7,8,7, 6,5,5)
popind <- c(0,1,1,0, 1,0,0,1, 1,1,1,1, 1,1,1,0, 1,0,0,1, 1,0,1,1, 1,1,1,1, 1,1,0,0, 1,1,1,1, 0,1,0,1, 1,1,1)
totscore <- 10 - totscore_raw

roc <- data.frame(alb = alb, tp = tp, totscore = totscore, popind = popind)

# ------------------------------------------------------------
# 2. Logistic model: popind(event='0') = alb tp totscore
# ------------------------------------------------------------
roc$y_event <- as.numeric(roc$popind == 0)  # 1 if popind=0
fit <- glm(y_event ~ alb + tp + totscore, data = roc, family = binomial(link = "logit"))
# Predicted probability of event='0'
roc$LogiPred <- predict(fit, type = "response")

# Save logistic model summary for listing
fit_summary <- summary(fit)
coef_est <- coef(fit)
coef_se <- sqrt(diag(vcov(fit)))
wald_chisq <- (coef_est / coef_se)^2
wald_p <- pchisq(wald_chisq, df = 1, lower.tail = FALSE)

# Model fit statistics
loglik_null <- as.numeric(logLik(glm(y_event ~ 1, data = roc, family = binomial)))
loglik_full <- as.numeric(logLik(fit))
aic_val <- AIC(fit)
sc_val <- BIC(fit)
neg2loglik <- -2 * loglik_full

# ------------------------------------------------------------
# 3. ExpertPred and Survival
# ------------------------------------------------------------
ExpertPred_vec <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
                    0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
                    0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
                    0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
                    0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
                    0.3,0.1,0.1)

Survival <- roc
Survival$ExpertPred <- ExpertPred_vec

# ------------------------------------------------------------
# 4. Original ROC Association (alpha=0.1 => 90% CI)
# ------------------------------------------------------------
z90 <- qnorm(0.95)  # 1.64485362695147

# For event = popind==0
event_ind <- as.numeric(Survival$popind == 0)

res_logi <- delong_se(Survival$LogiPred, event_ind)
res_expert <- delong_se(Survival$ExpertPred, event_ind)

roc_assoc <- data.frame(
  ROCModel = c("Expert", "Logistic"),
  Area = c(res_expert$auc, res_logi$auc),
  StdErr = c(res_expert$se, res_logi$se),
  stringsAsFactors = FALSE
)
roc_assoc$Lower90 <- pmax(0, roc_assoc$Area - z90 * roc_assoc$StdErr)
roc_assoc$Upper90 <- pmin(1, roc_assoc$Area + z90 * roc_assoc$StdErr)

# ------------------------------------------------------------
# 5. Prob dataset calculation
# ------------------------------------------------------------
x_val <- 0.73
xbar_val <- 0.8162
s_val <- 0.0801
z_val <- abs(xbar_val - x_val) / s_val
p_val <- 2 * pnorm(-z_val)
Prob_df <- data.frame(x = x_val, xbar = xbar_val, s = s_val, z = z_val, p = p_val)

# ------------------------------------------------------------
# 6. Bootstrap (5000 resamples, seed 12345, URS, samprate=1)
# ------------------------------------------------------------
NumSamples <- 5000
set.seed(12345)
N <- nrow(Survival)

# Preallocate
ROCEst_list <- vector("list", NumSamples * 2)
idx <- 1
for (rep in 1:NumSamples) {
  samp_idx <- sample(1:N, size = N, replace = TRUE)
  boot_dat <- Survival[samp_idx, ]
  ev <- as.numeric(boot_dat$popind == 0)
  auc_logi <- auc_calc(boot_dat$LogiPred, ev)
  auc_exp <- auc_calc(boot_dat$ExpertPred, ev)
  ROCEst_list[[idx]] <- data.frame(Replicate = rep, ROCModel = "Logistic", Area = auc_logi)
  idx <- idx + 1
  ROCEst_list[[idx]] <- data.frame(Replicate = rep, ROCModel = "Expert", Area = auc_exp)
  idx <- idx + 1
}
ROCEst <- do.call(rbind, ROCEst_list)

# ------------------------------------------------------------
# 7. PROC MEANS equivalent: Mean Std P5 P95 P10 P90 P25 P75 ndec=4
# ------------------------------------------------------------
# Function to compute stats for a vector
means_stats <- function(v) {
  n <- length(v)
  mn <- mean(v)
  sd <- sd(v)
  p5 <- sas_pctl(v, 5)
  p95 <- sas_pctl(v, 95)
  p10 <- sas_pctl(v, 10)
  p90 <- sas_pctl(v, 90)
  p25 <- sas_pctl(v, 25)
  p75 <- sas_pctl(v, 75)
  c(N = n, Mean = mn, Std = sd, P5 = p5, P95 = p95, P10 = p10, P90 = p90, P25 = p25, P75 = p75)
}

# By ROCModel
models <- unique(ROCEst$ROCModel)
means_by <- lapply(models, function(m) {
  v <- ROCEst$Area[ROCEst$ROCModel == m]
  s <- means_stats(v)
  data.frame(ROCModel = m, t(s), stringsAsFactors = FALSE)
})
means_by_df <- do.call(rbind, means_by)

# Overall
overall_stats <- means_stats(ROCEst$Area)
overall_df <- data.frame(ROCModel = "Overall", t(overall_stats), stringsAsFactors = FALSE)

means_all <- rbind(means_by_df, overall_df)
# Ensure numeric columns
num_cols <- c("N","Mean","Std","P5","P95","P10","P90","P25","P75")
for (nc in num_cols) means_all[[nc]] <- as.numeric(means_all[[nc]])

# ------------------------------------------------------------
# 8. PROC UNIVARIATE WidePctls: pctlpts 2.5 97.5 mean std
# ------------------------------------------------------------
wide_list <- lapply(models, function(m) {
  v <- ROCEst$Area[ROCEst$ROCModel == m]
  data.frame(
    ROCModel = m,
    Mean = mean(v),
    Std = sd(v),
    P_2_5 = sas_pctl(v, 2.5),
    P_97_5 = sas_pctl(v, 97.5),
    stringsAsFactors = FALSE
  )
})
WidePctls <- do.call(rbind, wide_list)

# ------------------------------------------------------------
# 9. Write CSV outputs
# ------------------------------------------------------------
# Main P097.csv: combine means and wide percentiles
# Create a comprehensive table for listing
P097_table <- means_all %>%
  left_join(WidePctls %>% select(ROCModel, BootMean = Mean, BootStdErr = Std, LowerCL = P_2_5, UpperCL = P_97_5),
            by = "ROCModel")

# For Overall row, BootMean etc will be NA; fill with overall wide? Compute overall wide as well
overall_wide <- data.frame(
  ROCModel = "Overall",
  Mean = mean(ROCEst$Area),
  Std = sd(ROCEst$Area),
  P_2_5 = sas_pctl(ROCEst$Area, 2.5),
  P_97_5 = sas_pctl(ROCEst$Area, 97.5)
)
# Add to WidePctls for completeness
WidePctls_all <- rbind(WidePctls, overall_wide %>% rename(Mean = Mean, Std = Std, P_2_5 = P_2_5, P_97_5 = P_97_5))

# Write P097.csv (table body)
write.csv(P097_table, file.path(out, "P097.csv"), row.names = FALSE, na = "")

# Also write ROCEst dataset
write.csv(ROCEst, file.path(out, "ROCEst.csv"), row.names = FALSE, na = "")
write.csv(WidePctls, file.path(out, "WidePctls.csv"), row.names = FALSE, na = "")
write.csv(roc_assoc, file.path(out, "ROCAssociation.csv"), row.names = FALSE, na = "")
write.csv(Prob_df, file.path(out, "Prob.csv"), row.names = FALSE, na = "")

# Histogram and SGPanel plotted data CSVs
write.csv(ROCEst, file.path(out, "histogram.csv"), row.names = FALSE, na = "")
write.csv(ROCEst, file.path(out, "sgpanel.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# 10. Create text rendering P097.txt (121 numbers)
# ------------------------------------------------------------
txt_path <- file.path(out, "P097.txt")
con <- file(txt_path, open = "wt")

cat("P097 Bootstrap ROC Analysis\n", file = con)
cat("Discrimination, accuracy, and stability in binary classifiers\n", file = con)
cat("Source: general/sascommunities/the-do-loop-blog\n\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("1. Logistic Model: popind(event='0') = alb tp totscore\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
cat(sprintf("Number of Observations: %d\n", nrow(roc)), file = con)
cat(sprintf("Number of Events (popind=0): %d\n", sum(roc$y_event == 1)), file = con)
cat(sprintf("Number of Non-Events (popind=1): %d\n\n", sum(roc$y_event == 0)), file = con)

cat("Model Fit Statistics:\n", file = con)
cat(sprintf("  AIC: %.4f\n", aic_val), file = con)
cat(sprintf("  SC (BIC): %.4f\n", sc_val), file = con)
cat(sprintf("  -2 Log L: %.4f\n\n", neg2loglik), file = con)

cat("Analysis of Maximum Likelihood Estimates:\n", file = con)
cat(sprintf("%-12s %2s %10s %10s %10s %10s\n", "Parameter", "DF", "Estimate", "StdErr", "WaldChiSq", "Pr>ChiSq"), file = con)
param_names <- c("Intercept", "alb", "tp", "totscore")
for (i in seq_along(coef_est)) {
  cat(sprintf("%-12s %2d %10.4f %10.4f %10.4f %10.4f\n",
              param_names[i], 1, coef_est[i], coef_se[i], wald_chisq[i], wald_p[i]), file = con)
}
cat("\n", file = con)

cat("Odds Ratio Estimates (90% Wald Confidence Limits):\n", file = con)
cat(sprintf("%-12s %10s %10s %10s\n", "Effect", "PointEst", "Lower", "Upper"), file = con)
for (i in 2:length(coef_est)) {
  est <- exp(coef_est[i])
  lower <- exp(coef_est[i] - z90 * coef_se[i])
  upper <- exp(coef_est[i] + z90 * coef_se[i])
  cat(sprintf("%-12s %10.4f %10.4f %10.4f\n", param_names[i], est, lower, upper), file = con)
}
cat("\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("2. ROC Association Statistics (Original Data, alpha=0.1, 90% CI)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
cat(sprintf("%-12s %10s %10s %10s %10s\n", "ROCModel", "Area", "StdErr", "Lower90", "Upper90"), file = con)
for (i in 1:nrow(roc_assoc)) {
  cat(sprintf("%-12s %10.4f %10.4f %10.4f %10.4f\n",
              roc_assoc$ROCModel[i], roc_assoc$Area[i], roc_assoc$StdErr[i],
              roc_assoc$Lower90[i], roc_assoc$Upper90[i]), file = con)
}
cat("\n", file = con)

cat("Association of Predicted Probabilities and Observed Responses:\n", file = con)
# Compute concordance for original logistic model
# Use same method as AUC but also compute percent concordant etc.
# For each model, compute pairs
for (m in c("Logistic", "Expert")) {
  pred <- if (m == "Logistic") Survival$LogiPred else Survival$ExpertPred
  ev <- event_ind
  x <- pred[ev == 1]; y <- pred[ev == 0]
  m_n <- length(x); n_n <- length(y)
  pairs <- m_n * n_n
  mat <- outer(x, y, function(a,b) ifelse(a > b, 1, ifelse(a == b, 0.5, 0)))
  concordant <- sum(mat == 1)
  tied <- sum(mat == 0.5)
  discordant <- pairs - concordant - tied
  pct_conc <- 100 * concordant / pairs
  pct_disc <- 100 * discordant / pairs
  pct_tied <- 100 * tied / pairs
  somers_d <- (concordant - discordant) / pairs
  gamma <- if ((concordant + discordant) > 0) (concordant - discordant) / (concordant + discordant) else NA
  tau_a <- (concordant - discordant) / (0.5 * N * (N - 1))
  c_stat <- (concordant + 0.5 * tied) / pairs
  cat(sprintf("  Model %s: Pairs=%d  Concordant=%.1f%% Discordant=%.1f%% Tied=%.1f%%\n",
              m, pairs, pct_conc, pct_disc, pct_tied), file = con)
  cat(sprintf("    Somers' D=%.4f  Gamma=%.4f  Tau-a=%.4f  c=%.4f\n",
              somers_d, gamma, tau_a, c_stat), file = con)
}
cat("\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("3. Probability Calculation (Normal approximation)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
cat(sprintf("%-10s %10s\n", "Variable", "Value"), file = con)
cat(sprintf("%-10s %10.4f\n", "x", x_val), file = con)
cat(sprintf("%-10s %10.4f\n", "xbar", xbar_val), file = con)
cat(sprintf("%-10s %10.4f\n", "s", s_val), file = con)
cat(sprintf("%-10s %10.4f\n", "z", z_val), file = con)
cat(sprintf("%-10s %10.4f\n", "p", p_val), file = con)
cat("\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("4. Bootstrap Distribution of Area Under the Curve (5000 resamples)\n", file = con)
cat("   PROC MEANS: Mean Std P5 P95 P10 P90 P25 P75 (ndec=4)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
cat(sprintf("%-12s %6s %10s %10s %10s %10s %10s %10s %10s %10s\n",
            "ROCModel", "N", "Mean", "StdDev", "P5", "P95", "P10", "P90", "P25", "P75"), file = con)
for (i in 1:nrow(means_all)) {
  cat(sprintf("%-12s %6d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
              means_all$ROCModel[i], as.integer(means_all$N[i]),
              means_all$Mean[i], means_all$Std[i],
              means_all$P5[i], means_all$P95[i],
              means_all$P10[i], means_all$P90[i],
              means_all$P25[i], means_all$P75[i]), file = con)
}
cat("\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("5. Bootstrap Percentiles (PROC UNIVARIATE, 95% CI)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
cat(sprintf("%-12s %10s %10s %10s %10s\n", "ROCModel", "BootMean", "BootStdErr", "LowerCL", "UpperCL"), file = con)
for (i in 1:nrow(WidePctls)) {
  cat(sprintf("%-12s %10.4f %10.4f %10.4f %10.4f\n",
              WidePctls$ROCModel[i], WidePctls$Mean[i], WidePctls$Std[i],
              WidePctls$P_2_5[i], WidePctls$P_97_5[i]), file = con)
}
# Also show overall
cat(sprintf("%-12s %10.4f %10.4f %10.4f %10.4f\n",
            overall_wide$ROCModel, overall_wide$Mean, overall_wide$Std,
            overall_wide$P_2_5, overall_wide$P_97_5), file = con)
cat("\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("6. Histogram Bin Counts (Area 0.4 to 1.0 by 0.1)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
breaks <- seq(0.4, 1.0, by = 0.1)
for (m in models) {
  v <- ROCEst$Area[ROCEst$ROCModel == m]
  h <- hist(v, breaks = breaks, plot = FALSE, include.lowest = TRUE, right = FALSE)
  cat(sprintf("ROCModel=%s\n", m), file = con)
  cat(sprintf("%-12s %10s %10s\n", "Bin", "Count", "Percent"), file = con)
  total <- length(v)
  for (b in seq_along(h$counts)) {
    pct <- 100 * h$counts[b] / total
    cat(sprintf("%4.1f-%4.1f %10d %10.4f\n", breaks[b], breaks[b+1], h$counts[b], pct), file = con)
  }
  cat("\n", file = con)
}
cat("\n", file = con)

cat("Note: Bootstrap seed=12345, method=URS, samprate=1, reps=5000\n", file = con)
cat("Title: Bootstrap Distributions of Area Under the Curve for Two Models\n", file = con)

close(con)

# Also create P097.lst.txt copy for compatibility
file.copy(txt_path, file.path(out, "P097.lst.txt"), overwrite = TRUE)
file.copy(file.path(out, "P097.csv"), file.path(out, "P097.lst.csv"), overwrite = TRUE)

# ------------------------------------------------------------
# 11. Figures
# ------------------------------------------------------------
# Histogram.png - from PROC UNIVARIATE histogram Area
# Create histogram for each ROCModel combined and faceted
p_hist <- ggplot(ROCEst, aes(x = Area, fill = ROCModel)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity", color = "black", linewidth = 0.2) +
  facet_wrap(~ ROCModel, ncol = 1) +
  scale_x_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
  labs(title = "Bootstrap Distributions of Area Under the Curve for Two Models",
       subtitle = "Histogram of Area (PROC UNIVARIATE)",
       x = "Area", y = "Frequency", fill = "ROC Model") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0))

ggsave(file.path(out, "histogram.png"), plot = p_hist, width = 6.4, height = 4.81, dpi = 100, units = "in")
# Also save as Histogram.png with capital H for compatibility
ggsave(file.path(out, "Histogram.png"), plot = p_hist, width = 6.4, height = 4.81, dpi = 100, units = "in")

# SGPanel.png - panelby ROCModel rows=2 layout=rowlattice
p_sgpanel <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", linewidth = 0.2) +
  facet_grid(ROCModel ~ ., switch = "y") +
  scale_x_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
  labs(title = "Bootstrap Distributions of Area Under the Curve for Two Models",
       x = "Area", y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        panel.spacing = unit(0.5, "lines"))

ggsave(file.path(out, "sgpanel.png"), plot = p_sgpanel, width = 6.4, height = 4.81, dpi = 100, units = "in")
ggsave(file.path(out, "SGPanel.png"), plot = p_sgpanel, width = 6.4, height = 4.81, dpi = 100, units = "in")

# Ensure histogram.csv and sgpanel.csv already written
# Also write a binned version for reference
hist_binned <- ROCEst %>%
  mutate(bin = cut(Area, breaks = seq(0.4, 1, 0.1), include.lowest = TRUE, right = FALSE)) %>%
  group_by(ROCModel, bin) %>%
  summarise(count = n(), .groups = "drop")
write.csv(hist_binned, file.path(out, "histogram_binned.csv"), row.names = FALSE, na = "")

cat("P097 R translation complete. Outputs written to", out, "\n")
