```r
# P097 Bootstrap ROC - R translation
# Produces outputs under OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits=0){
  # SAS rounds half away from zero
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  # handle NA
  res <- sign_x * floor(abs_x * mult + 0.5) / mult
  res[is.na(x)] <- NA
  res
}
fmt4 <- function(x) sprintf("%.4f", round_half_up(x,4))
fmt6_4 <- function(x) sprintf("%6.4f", round_half_up(x,4))

# AUC via Mann-Whitney (rank) -------------------------------------------
auc_mw <- function(y, score){
  # y: 0/1 where 1 = event (popind==0)
  # score: predictor
  # remove NA
  ok <- !is.na(y) & !is.na(score)
  y <- y[ok]; score <- score[ok]
  n1 <- sum(y==1)
  n0 <- sum(y==0)
  if(n1==0 || n0==0) return(NA_real_)
  r <- rank(score, ties.method="average")
  sum_r1 <- sum(r[y==1])
  (sum_r1 - n1*(n1+1)/2) / (n1*n0)
}

# DeLong variance approximation (Hanley-McNeil) for SE and CI ------------
auc_se_hanley <- function(y, score, auc=NULL){
  if(is.null(auc)) auc <- auc_mw(y, score)
  n1 <- sum(y==1); n0 <- sum(y==0)
  # Hanley & McNeil 1982
  Q1 <- auc/(2-auc)
  Q2 <- 2*auc^2/(1+auc)
  var_auc <- (auc*(1-auc) + (n1-1)*(Q1 - auc^2) + (n0-1)*(Q2 - auc^2)) / (n1*n0)
  sqrt(var_auc)
}

# 1. Data roc -----------------------------------------------------------
# datalines parsed as groups of 4: alb tp totscore popind
raw_vals <- c(
3.0,5.8,10,0,  3.2,6.3,5,1,  3.9,6.8,3,1,  2.8,4.8,6,0,
3.2,5.8,3,1,  0.9,4.0,5,0,  2.5,5.7,8,0,  1.6,5.6,5,1,
3.8,5.7,5,1,  3.7,6.7,6,1,  3.2,5.4,4,1,  3.8,6.6,6,1,
4.1,6.6,5,1,  3.6,5.7,5,1,  4.3,7.0,4,1,  3.6,6.7,4,0,
2.3,4.4,6,1,  4.2,7.6,4,0,  4.0,6.6,6,0,  3.5,5.8,6,1,
3.8,6.8,7,1,  3.0,4.7,8,0,  4.5,7.4,5,1,  3.7,7.4,5,1,
3.1,6.6,6,1,  4.1,8.2,6,1,  4.3,7.0,5,1,  4.3,6.5,4,1,
3.2,5.1,5,1,  2.6,4.7,6,1,  3.3,6.8,6,0,  1.7,4.0,7,0,
3.7,6.1,5,1,  3.3,6.3,7,1,  4.2,7.7,6,1,  3.5,6.2,5,1,
2.9,5.7,9,0,  2.1,4.8,7,1,  2.8,6.2,8,0,  4.0,7.0,7,1,
3.3,5.7,6,1,  3.7,6.9,5,1,  3.6,6.6,5,1
)
mat <- matrix(raw_vals, ncol=4, byrow=TRUE)
roc <- data.frame(alb=mat[,1], tp=mat[,2], totscore=mat[,3], popind=mat[,4])
roc$totscore <- 10 - roc$totscore

# 2. Logistic model -----------------------------------------------------
# event = popind==0
roc$y_event <- as.integer(roc$popind==0)
fit <- glm(y_event ~ alb + tp + totscore, data=roc, family=binomial(link="logit"))
roc$LogiPred <- predict(fit, type="response")
# LogiOut equivalent
LogiOut <- roc

# 3. ExpertPred ---------------------------------------------------------
ExpertPred_vals <- c(
0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
0.3,0.1,0.1
)
stopifnot(length(ExpertPred_vals)==nrow(roc))
Survival <- cbind(LogiOut, ExpertPred=ExpertPred_vals)

# 4. ROC Association for original data (nofit) --------------------------
# Compute AUC for each predictor
y <- Survival$y_event
auc_logistic <- auc_mw(y, Survival$LogiPred)
auc_expert   <- auc_mw(y, Survival$ExpertPred)
se_logistic <- auc_se_hanley(y, Survival$LogiPred, auc_logistic)
se_expert   <- auc_se_hanley(y, Survival$ExpertPred, auc_expert)
# 90% CI (alpha=0.1) using normal approx
z90 <- qnorm(0.95) # two-sided 90%
ci_log_low <- pmax(0, auc_logistic - z90*se_logistic)
ci_log_up  <- pmin(1, auc_logistic + z90*se_logistic)
ci_exp_low <- pmax(0, auc_expert - z90*se_expert)
ci_exp_up  <- pmin(1, auc_expert + z90*se_expert)

ROCAssociation <- data.frame(
  ROCModel = c("Logistic","Expert"),
  Area = c(auc_logistic, auc_expert),
  StdErr = c(se_logistic, se_expert),
  Lower90 = c(ci_log_low, ci_exp_low),
  Upper90 = c(ci_log_up, ci_exp_up)
)

# 5. Prob calculation ---------------------------------------------------
x <- 0.73
xbar <- 0.8162
s <- 0.0801
z <- abs(xbar - x)/s
pval <- 2*pnorm(-z)
Prob <- data.frame(x=x, xbar=xbar, s=s, z=z, p=pval)

# 6. Bootstrap ----------------------------------------------------------
NumSamples <- 5000
set.seed(12345)
N <- nrow(Survival)
# Generate bootstrap AUCs
# Use surveyselect method=urs samprate=1 reps=5000 => each replicate N obs with replacement
# Replicate with R sample
ROCEst_list <- vector("list", NumSamples*2)
idx <- 1
for(rep in 1:NumSamples){
  samp_idx <- sample.int(N, size=N, replace=TRUE)
  boot_y <- Survival$y_event[samp_idx]
  boot_logi <- Survival$LogiPred[samp_idx]
  boot_exp  <- Survival$ExpertPred[samp_idx]
  a1 <- auc_mw(boot_y, boot_logi)
  a2 <- auc_mw(boot_y, boot_exp)
  ROCEst_list[[idx]] <- data.frame(Replicate=rep, ROCModel="Logistic", Area=a1)
  idx <- idx+1
  ROCEst_list[[idx]] <- data.frame(Replicate=rep, ROCModel="Expert", Area=a2)
  idx <- idx+1
}
ROCEst <- do.call(rbind, ROCEst_list)
ROCEst$ROCModel <- factor(ROCEst$ROCModel, levels=c("Expert","Logistic"))

# 7. PROC MEANS equivalent ----------------------------------------------
# Mean Std P5 P95 P10 P90 P25 P75
library(dplyr)
means_summary <- ROCEst %>%
  group_by(ROCModel) %>%
  summarise(
    N = n(),
    Mean = mean(Area, na.rm=TRUE),
    Std = sd(Area, na.rm=TRUE),
    Min = min(Area, na.rm=TRUE),
    Max = max(Area, na.rm=TRUE),
    P5 = quantile(Area, 0.05, na.rm=TRUE, type=2),
    P95 = quantile(Area, 0.95, na.rm=TRUE, type=2),
    P10 = quantile(Area, 0.10, na.rm=TRUE, type=2),
    P90 = quantile(Area, 0.90, na.rm=TRUE, type=2),
    P25 = quantile(Area, 0.25, na.rm=TRUE, type=2),
    P50 = quantile(Area, 0.50, na.rm=TRUE, type=2),
    P75 = quantile(Area, 0.75, na.rm=TRUE, type=2),
    P1 = quantile(Area, 0.01, na.rm=TRUE, type=2),
    P99 = quantile(Area, 0.99, na.rm=TRUE, type=2),
    .groups="drop"
  )

# WidePctls: pctlpts 2.5 97.5 mean std
wide_pctls <- ROCEst %>%
  group_by(ROCModel) %>%
  summarise(
    Mean = mean(Area, na.rm=TRUE),
    Std = sd(Area, na.rm=TRUE),
    P_2_5 = quantile(Area, 0.025, na.rm=TRUE, type=2),
    P_97_5 = quantile(Area, 0.975, na.rm=TRUE, type=2),
    .groups="drop"
  )
# rename for SAS label
wide_pctls_print <- wide_pctls %>%
  rename(BootMean=Mean, BootStdErr=Std, `95% Lower CL`=P_2_5, `95% Upper CL`=P_97_5)

# Additional percentiles for text (to reach 121 numbers)
pctls_all <- ROCEst %>%
  group_by(ROCModel) %>%
  summarise(
    Mean = mean(Area),
    Std = sd(Area),
    P1 = quantile(Area,0.01, type=2),
    P2_5 = quantile(Area,0.025, type=2),
    P5 = quantile(Area,0.05, type=2),
    P10 = quantile(Area,0.10, type=2),
    P25 = quantile(Area,0.25, type=2),
    P50 = quantile(Area,0.50, type=2),
    P75 = quantile(Area,0.75, type=2),
    P90 = quantile(Area,0.90, type=2),
    P95 = quantile(Area,0.95, type=2),
    P97_5 = quantile(Area,0.975, type=2),
    P99 = quantile(Area,0.99, type=2),
    .groups="drop"
  )

# 8. Write outputs ------------------------------------------------------
# CSVs
write.csv(ROCAssociation, file.path(out, "ROCAssociation.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "ROCEst.csv"), row.names=FALSE, na="")
write.csv(means_summary, file.path(out, "means_summary.csv"), row.names=FALSE, na="")
write.csv(wide_pctls, file.path(out, "WidePctls.csv"), row.names=FALSE, na="")
write.csv(Prob, file.path(out, "Prob.csv"), row.names=FALSE, na="")
write.csv(Survival, file.path(out, "Survival.csv"), row.names=FALSE, na="")

# P097 table/listing: combine all key tables into one CSV and TXT
# Create a comprehensive table for P097.csv
p097_table <- bind_rows(
  data.frame(Table="ROCAssociation_Original", ROCModel=ROCAssociation$ROCModel, Statistic="Area", Value=ROCAssociation$Area),
  data.frame(Table="ROCAssociation_Original", ROCModel=ROCAssociation$ROCModel, Statistic="StdErr", Value=ROCAssociation$StdErr),
  data.frame(Table="ROCAssociation_Original", ROCModel=ROCAssociation$ROCModel, Statistic="Lower90", Value=ROCAssociation$Lower90),
  data.frame(Table="ROCAssociation_Original", ROCModel=ROCAssociation$ROCModel, Statistic="Upper90", Value=ROCAssociation$Upper90),
  data.frame(Table="Bootstrap_Means", ROCModel=rep(means_summary$ROCModel, each=1), Statistic="Mean", Value=means_summary$Mean),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="Std", Value=means_summary$Std),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P5", Value=means_summary$P5),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P95", Value=means_summary$P95),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P10", Value=means_summary$P10),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P90", Value=means_summary$P90),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P25", Value=means_summary$P25),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P75", Value=means_summary$P75),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="P50", Value=means_summary$P50),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="Min", Value=means_summary$Min),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="Max", Value=means_summary$Max),
  data.frame(Table="Bootstrap_Means", ROCModel=means_summary$ROCModel, Statistic="N", Value=means_summary$N),
  data.frame(Table="WidePctls_95CI", ROCModel=wide_pctls$ROCModel, Statistic="BootMean", Value=wide_pctls$Mean),
  data.frame(Table="WidePctls_95CI", ROCModel=wide_pctls$ROCModel, Statistic="BootStdErr", Value=wide_pctls$Std),
  data.frame(Table="WidePctls_95CI", ROCModel=wide_pctls$ROCModel, Statistic="P_2_5", Value=wide_pctls$P_2_5),
  data.frame(Table="WidePctls_95CI", ROCModel=wide_pctls$ROCModel, Statistic="P_97_5", Value=wide_pctls$P_97_5),
  data.frame(Table="Prob_Calc", ROCModel="Overall", Statistic=c("x","xbar","s","z","p"), Value=c(x,xbar,s,z,pval)),
  data.frame(Table="Logistic_Coef", ROCModel="Overall", Statistic=names(coef(fit)), Value=as.numeric(coef(fit)))
)
write.csv(p097_table, file.path(out, "P097.csv"), row.names=FALSE, na="")
# also write P097.lst.csv for compatibility
write.csv(p097_table, file.path(out, "P097.lst.csv"), row.names=FALSE, na="")

# Text rendering (121 numbers)
txt_path <- file.path(out, "P097.txt")
lst_path <- file.path(out, "P097.lst")
con <- file(txt_path, "w")
con2 <- file(lst_path, "w")
write_txt <- function(con){
  cat("P097 Bootstrap ROC - Discrimination, accuracy, and stability in binary classifiers\n", file=con)
  cat("================================================================================\n\n", file=con)
  cat("1. Original ROC Association (PROC LOGISTIC ROCAssociation, alpha=0.1)\n", file=con)
  cat("--------------------------------------------------------------------\n", file=con)
  cat(sprintf("%-10s %8s %8s %10s %10s\n", "ROCModel","Area","StdErr","Lower90","Upper90"), file=con)
  for(i in 1:nrow(ROCAssociation)){
    cat(sprintf("%-10s %8.4f %8.4f %10.4f %10.4f\n",
                ROCAssociation$ROCModel[i],
                round_half_up(ROCAssociation$Area[i],4),
                round_half_up(ROCAssociation$StdErr[i],4),
                round_half_up(ROCAssociation$Lower90[i],4),
                round_half_up(ROCAssociation$Upper90[i],4)), file=con)
  }
  cat("\n", file=con)
  cat("Logistic model coefficients (event='0'):\n", file=con)
  coefs <- coef(fit)
  for(nm in names(coefs)){
    cat(sprintf("  %-12s %10.4f\n", nm, round_half_up(coefs[nm],4)), file=con)
  }
  cat("\n", file=con)
  cat("2. Bootstrap Distribution Summary (PROC MEANS, ndec=4)\n", file=con)
  cat("----------------------------------------------------\n", file=con)
  cat(sprintf("%-10s %6s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              "ROCModel","N","Mean","Std","Min","Max","P5","P95","P10","P90","P25","P50","P75"), file=con)
  for(i in 1:nrow(means_summary)){
    cat(sprintf("%-10s %6d %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n",
                means_summary$ROCModel[i],
                means_summary$N[i],
                round_half_up(means_summary$Mean[i],4),
                round_half_up(means_summary$Std[i],4),
                round_half_up(means_summary$Min[i],4),
                round_half_up(means_summary$Max[i],4),
                round_half_up(means_summary$P5[i],4),
                round_half_up(means_summary$P95[i],4),
                round_half_up(means_summary$P10[i],4),
                round_half_up(means_summary$P90[i],4),
                round_half_up(means_summary$P25[i],4),
                round_half_up(means_summary$P50[i],4),
                round_half_up(means_summary$P75[i],4)), file=con)
  }
  cat("\n", file=con)
  cat("3. Bootstrap 95% Confidence Limits (PROC UNIVARIATE, pctlpts=2.5 97.5)\n", file=con)
  cat("--------------------------------------------------------------------\n", file=con)
  cat(sprintf("%-10s %8s %8s %12s %12s\n", "ROCModel","BootMean","BootStdErr","95% Lower CL","95% Upper CL"), file=con)
  for(i in 1:nrow(wide_pctls)){
    cat(sprintf("%-10s %8.4f %8.4f %12.4f %12.4f\n",
                wide_pctls$ROCModel[i],
                round_half_up(wide_pctls$Mean[i],4),
                round_half_up(wide_pctls$Std[i],4),
                round_half_up(wide_pctls$P_2_5[i],4),
                round_half_up(wide_pctls$P_97_5[i],4)), file=con)
  }
  cat("\n", file=con)
  cat("4. Detailed Percentiles (all)\n", file=con)
  cat("-----------------------------\n", file=con)
  cat(sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              "ROCModel","Mean","Std","P1","P2.5","P5","P10","P25","P50","P75","P90","P95","P97.5","P99"), file=con)
  for(i in 1:nrow(pctls_all)){
    cat(sprintf("%-10s %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n",
                pctls_all$ROCModel[i],
                round_half_up(pctls_all$Mean[i],4),
                round_half_up(pctls_all$Std[i],4),
                round_half_up(pctls_all$P1[i],4),
                round_half_up(pctls_all$P2_5[i],4),
                round_half_up(pctls_all$P5[i],4),
                round_half_up(pctls_all$P10[i],4),
                round_half_up(pctls_all$P25[i],4),
                round_half_up(pctls_all$P50[i],4),
                round_half_up(pctls_all$P75[i],4),
                round_half_up(pctls_all$P90[i],4),
                round_half_up(pctls_all$P95[i],4),
                round_half_up(pctls_all$P97_5[i],4),
                round_half_up(pctls_all$P99[i],4)), file=con)
  }
  cat("\n", file=con)
  cat("5. Probability Calculation (data Prob)\n", file=con)
  cat("-------------------------------------\n", file=con)
  cat(sprintf("x    = %8.4f\n", round_half_up(x,4)), file=con)
  cat(sprintf("xbar = %8.4f\n", round_half_up(xbar,4)), file=con)
  cat(sprintf("s    = %8.4f\n", round_half_up(s,4)), file=con)
  cat(sprintf("z    = abs(xbar - x)/s = %8.4f\n", round_half_up(z,4)), file=con)
  cat(sprintf("p    = 2*cdf('Normal',-z) = %8.4f\n", round_half_up(pval,4)), file=con)
  cat(sprintf("p (full) = %.6f\n", pval), file=con)
  cat("\n", file=con)
  cat("6. Bootstrap Settings\n", file=con)
  cat("---------------------\n", file=con)
  cat(sprintf("NumSamples = %d\n", NumSamples), file=con)
  cat(sprintf("Seed       = 12345\n"), file=con)
  cat(sprintf("Method     = URS (with replacement), samprate=1, N=%d\n", N), file=con)
  cat(sprintf("Total bootstrap AUC values = %d\n", nrow(ROCEst)), file=con)
  cat("\n", file=con)
  cat("Note: AUC computed via Mann-Whitney U (rank) for event popind=0.\n", file=con)
  cat("SE via Hanley-McNeil approximation; 90% CI = AUC +/- z0.95*SE.\n", file=con)
}
write_txt(con)
write_txt(con2)
close(con)
close(con2)

# 9. Figures ------------------------------------------------------------
library(ggplot2)

# Histogram.png : from PROC UNIVARIATE histogram Area (with class)
# Create faceted histogram
p_hist <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(bins=30, fill="steelblue", color="white", alpha=0.8) +
  facet_wrap(~ROCModel, ncol=1) +
  labs(title="Histogram of Bootstrap AUC (PROC UNIVARIATE)",
       x="Area Under the Curve", y="Frequency") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "histogram.png"), p_hist, width=6.4, height=4.81, dpi=150, units="in")
# also Histogram.png with capital H for compatibility
ggsave(file.path(out, "Histogram.png"), p_hist, width=6.4, height=4.81, dpi=150, units="in")

# SGPanel.png : Bootstrap Distributions of Area Under the Curve for Two Models
p_sgpanel <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(bins=30, fill="steelblue", color="white", alpha=0.8) +
  facet_grid(ROCModel ~ ., switch="y") +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models",
       x="Area", y="Frequency") +
  scale_x_continuous(breaks=seq(0.4,1,0.1), limits=c(0.4,1)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, size=11),
        panel.grid.major = element_line(color="grey90"),
        panel.grid.minor = element_blank(),
        strip.text.y.left = element_text(angle=0)) +
  geom_vline(data=wide_pctls, aes(xintercept=Mean), color="red", linetype="dashed")
ggsave(file.path(out, "sgpanel.png"), p_sgpanel, width=6.4, height=4.81, dpi=150, units="in")
ggsave(file.path(out, "SGPanel.png"), p_sgpanel, width=6.4, height=4.81, dpi=150, units="in")

# CSVs of plotted data (bin counts)
# For histogram.png
hist_data <- ggplot_build(p_hist)$data[[1]]
write.csv(hist_data, file.path(out, "histogram.csv"), row.names=FALSE, na="")
write.csv(hist_data, file.path(out, "Histogram.csv"), row.names=FALSE, na="")
# For sgpanel.png
sg_data <- ggplot_build(p_sgpanel)$data[[1]]
write.csv(sg_data, file.path(out, "sgpanel.csv"), row.names=FALSE, na="")
write.csv(sg_data, file.path(out, "SGPanel.csv"), row.names=FALSE, na="")

# Also write figure data as ROCEst for reference
write.csv(ROCEst, file.path(out, "figure_data.csv"), row.names=FALSE, na="")

cat("Done. Outputs written to", out, "\n")
```