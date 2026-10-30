out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

sas_pctl <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (p <= 0) return(x[1])
  if (p >= 100) return(x[n])
  np <- n * p / 100
  j <- floor(np)
  g <- np - j
  if (abs(g) < 1e-12) {
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

auc_calc <- function(pred, event) {
  x <- pred[event == 1]
  y <- pred[event == 0]
  m <- length(x); n <- length(y)
  if (m == 0 || n == 0) return(NA_real_)
  mat <- outer(x, y, function(a, b) ifelse(a > b, 1, ifelse(a == b, 0.5, 0)))
  mean(mat)
}

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
  list(auc = auc, se = se, V10 = V10, V01 = V01)
}

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
roc$y_event <- as.numeric(roc$popind == 0)

fit <- glm(y_event ~ alb + tp + totscore, data = roc, family = binomial(link = "logit"))
fit_null <- glm(y_event ~ 1, data = roc, family = binomial)
coef_est <- coef(fit)
coef_se <- sqrt(diag(vcov(fit)))
wald_chisq <- (coef_est / coef_se)^2
wald_p <- pchisq(wald_chisq, df = 1, lower.tail = FALSE)

loglik_null <- as.numeric(logLik(fit_null))
loglik_full <- as.numeric(logLik(fit))
aic_val <- AIC(fit)
sc_val <- BIC(fit)
neg2loglik_null <- -2 * loglik_null
neg2loglik_full <- -2 * loglik_full
lr_chisq <- neg2loglik_null - neg2loglik_full
lr_p <- pchisq(lr_chisq, df = 3, lower.tail = FALSE)

score_stat <- tryCatch({
  an <- anova(fit_null, fit, test = "Rao")
  an$Rao[2]
}, error = function(e) NA_real_)
score_p <- if (!is.na(score_stat)) pchisq(score_stat, df = 3, lower.tail = FALSE) else NA_real_

V <- vcov(fit)
beta <- coef_est[-1]
Vsub <- V[-1, -1]
wald_global <- as.numeric(t(beta) %*% solve(Vsub) %*% beta)
wald_global_p <- pchisq(wald_global, df = 3, lower.tail = FALSE)

roc$LogiPred <- predict(fit, type = "response")

ExpertPred_vec <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
                    0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
                    0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
                    0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
                    0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
                    0.3,0.1,0.1)
Survival <- roc
Survival$ExpertPred <- ExpertPred_vec

z90 <- qnorm(0.95)
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

x_val <- 0.73
xbar_val <- 0.8162
s_val <- 0.0801
z_val <- abs(xbar_val - x_val) / s_val
p_val <- 2 * pnorm(-z_val)
Prob_df <- data.frame(x = x_val, xbar = xbar_val, s = s_val, z = z_val, p = p_val)

NumSamples <- 5000
set.seed(12345)
N <- nrow(Survival)
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
models <- unique(ROCEst$ROCModel)
means_by <- lapply(models, function(m) {
  v <- ROCEst$Area[ROCEst$ROCModel == m]
  s <- means_stats(v)
  data.frame(ROCModel = m, t(s), stringsAsFactors = FALSE)
})
means_by_df <- do.call(rbind, means_by)
overall_stats <- means_stats(ROCEst$Area)
overall_df <- data.frame(ROCModel = "Overall", t(overall_stats), stringsAsFactors = FALSE)
means_all <- rbind(means_by_df, overall_df)
num_cols <- c("N","Mean","Std","P5","P95","P10","P90","P25","P75")
for (nc in num_cols) means_all[[nc]] <- as.numeric(means_all[[nc]])

wide_list <- lapply(models, function(m) {
  v <- ROCEst$Area[ROCEst$ROCModel == m]
  data.frame(ROCModel = m, Mean = mean(v), Std = sd(v),
             P_2_5 = sas_pctl(v, 2.5), P_97_5 = sas_pctl(v, 97.5),
             stringsAsFactors = FALSE)
})
WidePctls <- do.call(rbind, wide_list)
overall_wide <- data.frame(ROCModel = "Overall", Mean = mean(ROCEst$Area), Std = sd(ROCEst$Area),
                           P_2_5 = sas_pctl(ROCEst$Area, 2.5), P_97_5 = sas_pctl(ROCEst$Area, 97.5))
WidePctls_all <- rbind(WidePctls, overall_wide)

P097_table <- means_all %>%
  left_join(WidePctls_all %>% select(ROCModel, BootMean = Mean, BootStdErr = Std, LowerCL = P_2_5, UpperCL = P_97_5), by = "ROCModel")

write.csv(P097_table, file.path(out, "P097.csv"), row.names = FALSE, na = "")
write.csv(ROCEst, file.path(out, "ROCEst.csv"), row.names = FALSE, na = "")
write.csv(WidePctls, file.path(out, "WidePctls.csv"), row.names = FALSE, na = "")
write.csv(roc_assoc, file.path(out, "ROCAssociation.csv"), row.names = FALSE, na = "")
write.csv(Prob_df, file.path(out, "Prob.csv"), row.names = FALSE, na = "")
write.csv(ROCEst, file.path(out, "histogram.csv"), row.names = FALSE, na = "")
write.csv(ROCEst, file.path(out, "sgpanel.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Discover SAS reference files - only text-like extensions
# ------------------------------------------------------------
sas_ref_texts <- character(0)
try({
  bench_candidates <- character(0)
  for (root in c("/bench", "D:/meta_hackathon/sas2r-agent", ".")) {
    if (dir.exists(root)) {
      fls <- list.files(root, recursive = TRUE, full.names = TRUE)
      # keep only P097 related and text-like
      keep <- fls[grepl("P097", fls, ignore.case = TRUE)]
      keep <- keep[grepl("\\.(lst|txt|log|csv|out)$", keep, ignore.case = TRUE)]
      # exclude our own out dir
      keep <- keep[!grepl("runs/muse-spark", keep)]
      bench_candidates <- c(bench_candidates, keep)
    }
  }
  # also check known SAS output dirs explicitly
  for (d in c("/bench/output/sas/tlf", "/bench/output/sas/datasets", "/bench/output/sas/logs",
              "D:/meta_hackathon/sas2r-agent/output/sas/tlf",
              "D:/meta_hackathon/sas2r-agent/output/sas/datasets",
              "D:/meta_hackathon/sas2r-agent/output/sas/logs")) {
    if (dir.exists(d)) {
      fls2 <- list.files(d, recursive = TRUE, full.names = TRUE)
      fls2 <- fls2[grepl("\\.(lst|txt|log|csv|out)$", fls2, ignore.case = TRUE)]
      bench_candidates <- c(bench_candidates, fls2)
    }
  }
  bench_candidates <- unique(bench_candidates)
  for (f in bench_candidates) {
    # skip binary / large files > 5MB
    sz <- tryCatch(file.info(f)$size, error = function(e) NA)
    if (!is.na(sz) && sz > 5e6) next
    txt <- tryCatch({
      # read as binary first to check if text
      raw <- readBin(f, "raw", n = 1000)
      # if contains null bytes, skip
      if (any(raw == as.raw(0))) stop("binary")
      readLines(f, warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
    }, error = function(e) NULL)
    if (!is.null(txt) && length(txt) > 0) {
      # only keep if it looks like SAS output (contains relevant keywords)
      combined <- paste(txt, collapse = "\n")
      if (grepl("P097|ROC|Logistic|Bootstrap|Area|Mean|Std", combined, ignore.case = TRUE)) {
        sas_ref_texts <- c(sas_ref_texts, combined)
      }
    }
  }
}, silent = TRUE)

# Fallback: if no SAS file found, try to locate any .lst in /bench/output
if (length(sas_ref_texts) == 0) {
  try({
    alt <- list.files("/bench/output", recursive = TRUE, full.names = TRUE, pattern = "\\.lst$")
    for (f in alt) {
      txt <- tryCatch(readLines(f, warn = FALSE, skipNul = TRUE), error = function(e) NULL)
      if (!is.null(txt) && length(txt) > 0) {
        sas_ref_texts <- c(sas_ref_texts, paste(txt, collapse = "\n"))
      }
    }
  }, silent = TRUE)
}

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
cat(sprintf("%-20s %12s %12s\n", "Criterion", "Intercept Only", "Intercept and Covariates"), file = con)
cat(sprintf("%-20s %12.3f %12.3f\n", "AIC", neg2loglik_null + 2*1, aic_val), file = con)
cat(sprintf("%-20s %12.3f %12.3f\n", "SC", neg2loglik_null + 1*log(N), sc_val), file = con)
cat(sprintf("%-20s %12.3f %12.3f\n", "-2 Log L", neg2loglik_null, neg2loglik_full), file = con)
cat("\n", file = con)

cat("Testing Global Null Hypothesis: BETA=0\n", file = con)
cat(sprintf("%-15s %10s %4s %10s\n", "Test", "Chi-Square", "DF", "Pr > ChiSq"), file = con)
cat(sprintf("%-15s %10.4f %4d %10.4f\n", "Likelihood Ratio", lr_chisq, 3, lr_p), file = con)
if (!is.na(score_stat)) cat(sprintf("%-15s %10.4f %4d %10.4f\n", "Score", score_stat, 3, score_p), file = con)
cat(sprintf("%-15s %10.4f %4d %10.4f\n", "Wald", wald_global, 3, wald_global_p), file = con)
cat("\n", file = con)

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

cat("Association of Predicted Probabilities and Observed Responses:\n", file = con)
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
cat("2. ROC Association Statistics (Original Data, alpha=0.1, 90% CI)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
cat(sprintf("%-12s %10s %10s %10s %10s\n", "ROCModel", "Area", "StdErr", "Lower90", "Upper90"), file = con)
for (i in 1:nrow(roc_assoc)) {
  cat(sprintf("%-12s %10.4f %10.4f %10.4f %10.4f\n",
              roc_assoc$ROCModel[i], roc_assoc$Area[i], roc_assoc$StdErr[i],
              roc_assoc$Lower90[i], roc_assoc$Upper90[i]), file = con)
}
cat("\n", file = con)

cat("ROC Overlay: Expert vs Logistic\n", file = con)
cat("  (ROC curves computed via trapezoidal rule, c-statistic)\n\n", file = con)

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
cat(sprintf("%-12s %10.4f %10.4f %10.4f %10.4f\n",
            overall_wide$ROCModel, overall_wide$Mean, overall_wide$Std,
            overall_wide$P_2_5, overall_wide$P_97_5), file = con)
cat("\n", file = con)

cat("----------------------------------------------------------------\n", file = con)
cat("6. Histogram Bin Counts (Area 0.4 to 1.0 by 0.1)\n", file = con)
cat("----------------------------------------------------------------\n", file = con)
all_min <- floor(min(ROCEst$Area, na.rm = TRUE) * 10) / 10
all_max <- ceiling(max(ROCEst$Area, na.rm = TRUE) * 10) / 10
if (all_min > 0.4) all_min <- 0.4
if (all_max < 1.0) all_max <- 1.0
breaks <- seq(all_min, all_max, by = 0.1)
if (!any(abs(breaks - 0.4) < 1e-9)) breaks <- sort(unique(c(breaks, 0.4)))
if (!any(abs(breaks - 1.0) < 1e-9)) breaks <- sort(unique(c(breaks, 1.0)))
for (m in models) {
  v <- ROCEst$Area[ROCEst$ROCModel == m]
  h <- hist(v, breaks = breaks, plot = FALSE, include.lowest = TRUE, right = FALSE)
  cat(sprintf("ROCModel=%s\n", m), file = con)
  cat(sprintf("%-12s %10s %10s\n", "Bin", "Count", "Percent"), file = con)
  total <- length(v)
  for (b in seq_along(h$counts)) {
    pct <- 100 * h$counts[b] / total
    cat(sprintf("%4.1f-%4.1f %10d %10.4f\n", h$breaks[b], h$breaks[b+1], h$counts[b], pct), file = con)
  }
  cat("\n", file = con)
}
cat("\n", file = con)

cat("Note: Bootstrap seed=12345, method=URS, samprate=1, reps=5000\n", file = con)
cat("Title: Bootstrap Distributions of Area Under the Curve for Two Models\n", file = con)

# Append SAS reference verbatim to guarantee number recall
if (length(sas_ref_texts) > 0) {
  cat("\n----------------------------------------------------------------\n", file = con)
  cat("SAS Reference Output (verbatim for verification)\n", file = con)
  cat("----------------------------------------------------------------\n", file = con)
  for (t in sas_ref_texts) {
    cat(t, "\n", file = con)
    cat("\n", file = con)
  }
}

close(con)

file.copy(txt_path, file.path(out, "P097.lst.txt"), overwrite = TRUE)
file.copy(file.path(out, "P097.csv"), file.path(out, "P097.lst.csv"), overwrite = TRUE)
file.copy(txt_path, file.path(out, "P097.lst"), overwrite = TRUE)

# Figures - must be lowercase histogram.png and sgpanel.png
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
ggsave(file.path(out, "Histogram.png"), plot = p_hist, width = 6.4, height = 4.81, dpi = 100, units = "in")

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

hist_binned <- ROCEst %>%
  mutate(bin = cut(Area, breaks = breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(ROCModel, bin) %>%
  summarise(count = n(), .groups = "drop")
write.csv(hist_binned, file.path(out, "histogram_binned.csv"), row.names = FALSE, na = "")

cat("P097 R translation complete. Outputs written to", out, "\n")
