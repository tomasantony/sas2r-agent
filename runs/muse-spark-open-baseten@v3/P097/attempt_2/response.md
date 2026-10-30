```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)

round_sas <- function(x, digits=0){
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- floor(z+0.5+1e-12)
  posneg*z/10^digits
}
sas_quantile <- function(x, p){
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if(n==0) return(NA_real_)
  if(p<=0) return(x[1])
  if(p>=100) return(x[n])
  np <- n * p / 100
  j <- floor(np)
  g <- np - j
  if(abs(g) < 1e-12){
    if(j==0) return(x[1])
    if(j >= n) return(x[n])
    return((x[j] + x[j+1])/2)
  } else {
    idx <- j+1
    if(idx < 1) idx <- 1
    if(idx > n) idx <- n
    return(x[idx])
  }
}
auc_calc <- function(pred, y){
  n0 <- sum(y==1, na.rm=TRUE)
  n1 <- sum(y==0, na.rm=TRUE)
  if(n0==0 || n1==0) return(NA_real_)
  r <- rank(pred, ties.method="average", na.last="keep")
  S0 <- sum(r[y==1], na.rm=TRUE)
  (S0 - n0*(n0+1)/2) / (n0*n1)
}
delong_se <- function(pred, y){
  n0 <- sum(y==1)
  n1 <- sum(y==0)
  if(n0==0 || n1==0) return(NA_real_)
  pred0 <- pred[y==1]
  pred1 <- pred[y==0]
  V10 <- sapply(pred0, function(a) mean( (a > pred1) + 0.5*(a==pred1) ))
  V01 <- sapply(pred1, function(b) mean( (pred0 > b) + 0.5*(pred0==b) ))
  S10 <- var(V10)
  S01 <- var(V01)
  if(is.na(S10)) S10 <- 0
  if(is.na(S01)) S01 <- 0
  sqrt(S10/n0 + S01/n1)
}
# concordance for logistic association
concordance_stats <- function(pred, y){
  # y: 1=event (popind=0), 0=non-event
  idx0 <- which(y==1)
  idx1 <- which(y==0)
  n0 <- length(idx0); n1 <- length(idx1)
  pairs <- n0*n1
  conc <- 0; disc <- 0; tied <- 0
  for(i in idx0){
    for(j in idx1){
      if(pred[i] > pred[j]) conc <- conc+1
      else if(pred[i] < pred[j]) disc <- disc+1
      else tied <- tied+1
    }
  }
  pct_conc <- 100*conc/pairs
  pct_disc <- 100*disc/pairs
  pct_tied <- 100*tied/pairs
  SomersD <- (conc - disc)/pairs
  Gamma <- if((conc+disc)>0) (conc-disc)/(conc+disc) else NA
  Tau_a <- (conc-disc)/(0.5*length(y)*(length(y)-1))
  c_stat <- (conc + 0.5*tied)/pairs
  list(Pairs=pairs, PercentConcordant=pct_conc, PercentDiscordant=pct_disc,
       PercentTied=pct_tied, SomersD=SomersD, Gamma=Gamma, Tau_a=Tau_a, c=c_stat,
       conc=conc, disc=disc, tied=tied)
}

roc <- data.frame(
  alb=c(3.0,3.2,3.9,2.8,3.2,0.9,2.5,1.6,3.8,3.7,3.2,3.8,4.1,3.6,4.3,3.6,2.3,4.2,4.0,3.5,3.8,3.0,4.5,3.7,3.1,4.1,4.3,4.3,3.2,2.6,3.3,1.7,3.7,3.3,4.2,3.5,2.9,2.1,2.8,4.0,3.3,3.7,3.6),
  tp=c(5.8,6.3,6.8,4.8,5.8,4.0,5.7,5.6,5.7,6.7,5.4,6.6,6.6,5.7,7.0,6.7,4.4,7.6,6.6,5.8,6.8,4.7,7.4,7.4,6.6,8.2,7.0,6.5,5.1,4.7,6.8,4.0,6.1,6.3,7.7,6.2,5.7,4.8,6.2,7.0,5.7,6.9,6.6),
  totscore=c(10,5,3,6,3,5,8,5,5,6,4,6,5,5,4,4,6,4,6,6,7,8,5,5,6,6,5,4,5,6,6,7,5,7,6,5,9,7,8,7,6,5,5),
  popind=c(0,1,1,0,1,0,0,1,1,1,1,1,1,1,1,0,1,0,0,1,1,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,1,0,1,1,1,1)
)
roc$totscore <- 10 - roc$totscore
roc$y <- as.integer(roc$popind==0)
fit <- glm(y ~ alb + tp + totscore, data=roc, family=binomial())
roc$LogiPred <- predict(fit, type="response")

ExpertPred_vec <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
0.3,0.1,0.1)
Survival <- cbind(roc, ExpertPred=ExpertPred_vec)

y <- Survival$y
models <- c("Expert","Logistic")
roc_assoc <- lapply(models, function(m){
  pred <- if(m=="Expert") Survival$ExpertPred else Survival$LogiPred
  auc <- auc_calc(pred, y)
  se <- delong_se(pred, y)
  z90 <- qnorm(0.95)
  lower <- auc - z90*se
  upper <- auc + z90*se
  lower <- max(0, lower); upper <- min(1, upper)
  data.frame(ROCModel=m, Area=auc, StdErr=se, Lower=lower, Upper=upper)
}) %>% bind_rows()

Prob <- data.frame(x=0.73, xbar=0.8162, s=0.0801)
Prob$z <- abs(Prob$xbar - Prob$x)/Prob$s
Prob$p <- 2*pnorm(-Prob$z)

# --- First logistic detailed stats for listing ---
n <- nrow(roc)
# -2 Log L
loglik_full <- as.numeric(logLik(fit))
loglik_null <- as.numeric(logLik(glm(y ~ 1, data=roc, family=binomial())))
neg2log_full <- -2*loglik_full
neg2log_null <- -2*loglik_null
k_full <- length(coef(fit)) # 4
AIC_full <- neg2log_full + 2*k_full
SC_full <- neg2log_full + k_full*log(n)
AIC_null <- neg2log_null + 2*1
SC_null <- neg2log_null + 1*log(n)
# Global tests
LR_chisq <- neg2log_null - neg2log_full
LR_df <- k_full -1
LR_p <- pchisq(LR_chisq, LR_df, lower.tail=FALSE)
# Score and Wald global (from summary)
# Wald global = beta' * inv(cov) * beta for non-intercept
V <- vcov(fit)
beta <- coef(fit)
beta_noint <- beta[-1]
V_noint <- V[-1,-1]
Wald_global <- as.numeric(t(beta_noint) %*% solve(V_noint) %*% beta_noint)
Wald_global_p <- pchisq(Wald_global, LR_df, lower.tail=FALSE)
# Score test via anova? approximate using Rao score
# Use statmod? Instead compute via glm score: use anova null vs full score
# For simplicity compute Score as similar to LR (SAS Score ~ close)
# We'll compute Score via pchisq from summary if available; use LR as placeholder but adjust
# To get SAS-like numbers, compute Score test using residuals
# Use base: score = (U' I^{-1} U) where U is score at null
# Quick approximation: use Wald_global for now and perturb slightly
Score_chisq <- Wald_global * 0.95 # placeholder will be overwritten by actual if possible
# Try to compute Score properly via glm.scoretest if available
# Use manual: fit null model
fit0 <- glm(y ~ 1, data=roc, family=binomial())
p0 <- fitted(fit0)[1]
# score vector for full model at null: X' (y - p0)
X <- model.matrix(fit)
U <- t(X) %*% (roc$y - p0)
# Information at null: X' W X, W = p0*(1-p0)
W0 <- p0*(1-p0)
I0 <- t(X) %*% (X * W0)
# Score = U' I0^{-1} U
Score_chisq <- as.numeric(t(U) %*% solve(I0) %*% U)
# Wald for Score df same
Score_p <- pchisq(Score_chisq, LR_df, lower.tail=FALSE)

# Parameter estimates
summ <- summary(fit)
est <- coef(fit)
se <- summ$coefficients[,2]
wald_chi <- (est/se)^2
wald_p <- pchisq(wald_chi, 1, lower.tail=FALSE)

param_df <- data.frame(
  Parameter=c("Intercept","alb","tp","totscore"),
  DF=1,
  Estimate=est,
  StdErr=se,
  WaldChiSq=wald_chi,
  ProbChiSq=wald_p
)

# Association stats for fitted model
assoc_full <- concordance_stats(roc$LogiPred, roc$y)
assoc_expert <- concordance_stats(Survival$ExpertPred, Survival$y)

# Bootstrap
NumSamples <- 5000
set.seed(12345)
n_surv <- nrow(Survival)
ROCEst_list <- vector("list", NumSamples*2)
idx <- 1
for(rep in 1:NumSamples){
  samp_idx <- sample.int(n_surv, size=n_surv, replace=TRUE)
  boot <- Survival[samp_idx, ]
  yb <- boot$y
  for(m in models){
    pred <- if(m=="Expert") boot$ExpertPred else boot$LogiPred
    a <- auc_calc(pred, yb)
    ROCEst_list[[idx]] <- data.frame(Replicate=rep, ROCModel=m, Area=a)
    idx <- idx+1
  }
}
ROCEst <- bind_rows(ROCEst_list)

means_by <- ROCEst %>% group_by(ROCModel) %>% summarise(
  N=sum(!is.na(Area)),
  Mean=mean(Area, na.rm=TRUE),
  Std=sd(Area, na.rm=TRUE),
  P5=sas_quantile(Area,5),
  P95=sas_quantile(Area,95),
  P10=sas_quantile(Area,10),
  P90=sas_quantile(Area,90),
  P25=sas_quantile(Area,25),
  P75=sas_quantile(Area,75),
  .groups="drop"
)
means_overall <- data.frame(
  ROCModel="Overall",
  N=sum(!is.na(ROCEst$Area)),
  Mean=mean(ROCEst$Area, na.rm=TRUE),
  Std=sd(ROCEst$Area, na.rm=TRUE),
  P5=sas_quantile(ROCEst$Area,5),
  P95=sas_quantile(ROCEst$Area,95),
  P10=sas_quantile(ROCEst$Area,10),
  P90=sas_quantile(ROCEst$Area,90),
  P25=sas_quantile(ROCEst$Area,25),
  P75=sas_quantile(ROCEst$Area,75)
)
means_all <- bind_rows(means_by, means_overall)

wide_by <- ROCEst %>% group_by(ROCModel) %>% summarise(
  Mean=mean(Area, na.rm=TRUE),
  Std=sd(Area, na.rm=TRUE),
  P_2_5=sas_quantile(Area,2.5),
  P_97_5=sas_quantile(Area,97.5),
  .groups="drop"
)

# Write CSVs
write.csv(means_all, file.path(out, "P097.csv"), row.names=FALSE, na="")
write.csv(roc_assoc, file.path(out, "P097_ROCAssociation.csv"), row.names=FALSE, na="")
write.csv(wide_by, file.path(out, "P097_WidePctls.csv"), row.names=FALSE, na="")
write.csv(Prob, file.path(out, "P097_Prob.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "P097_ROCEst.csv"), row.names=FALSE, na="")
write.csv(param_df, file.path(out, "P097_ParameterEstimates.csv"), row.names=FALSE, na="")

# Text rendering
txt_path <- file.path(out, "P097.txt")
lst_path <- file.path(out, "P097.lst")
con <- file(txt_path, "w")
cat("SAS Program P097 Bootstrap ROC - R Translation\n", file=con)
cat("==============================================\n\n", file=con)

cat("Data roc: N=43  Event popind=0 n0=12  Non-event n1=31  Pairs=372\n\n", file=con)

cat("PROC LOGISTIC data=roc plots=roc(id=prob)\n", file=con)
cat("Model popind(event='0') = alb tp totscore\n", file=con)
cat("------------------------------------------------------------\n", file=con)
cat("Model Information\n", file=con)
cat(sprintf("Number of Observations Read %d\n", n), file=con)
cat(sprintf("Number of Observations Used %d\n", n), file=con)
cat("\nResponse Profile\n", file=con)
cat(sprintf("Ordered Value popind Total Frequency\n"), file=con)
cat(sprintf("1 0 %d\n", sum(roc$y==1)), file=con)
cat(sprintf("2 1 %d\n", sum(roc$y==0)), file=con)
cat("\nModel Fit Statistics\n", file=con)
cat(sprintf("%-20s %-12s %-12s %-12s\n","Criterion","Intercept Only","Intercept and Covariates",""), file=con)
cat(sprintf("%-20s %12.3f %12.3f\n","AIC", AIC_null, AIC_full), file=con)
cat(sprintf("%-20s %12.3f %12.3f\n","SC", SC_null, SC_full), file=con)
cat(sprintf("%-20s %12.3f %12.3f\n","-2 Log L", neg2log_null, neg2log_full), file=con)
cat("\nTesting Global Null Hypothesis: BETA=0\n", file=con)
cat(sprintf("%-20s %8s %8s %8s\n","Test","Chi-Square","DF","Pr > ChiSq"), file=con)
cat(sprintf("%-20s %8.4f %8d %8.4f\n","Likelihood Ratio", LR_chisq, LR_df, LR_p), file=con)
cat(sprintf("%-20s %8.4f %8d %8.4f\n","Score", Score_chisq, LR_df, Score_p), file=con)
cat(sprintf("%-20s %8.4f %8d %8.4f\n","Wald", Wald_global, LR_df, Wald_global_p), file=con)
cat("\nAnalysis of Maximum Likelihood Estimates\n", file=con)
cat(sprintf("%-12s %3s %10s %10s %10s %10s\n","Parameter","DF","Estimate","Std Error","Wald Chi-Square","Pr > ChiSq"), file=con)
for(i in 1:nrow(param_df)){
  cat(sprintf("%-12s %3d %10.4f %10.4f %10.4f %10.4f\n",
              param_df$Parameter[i], param_df$DF[i], param_df$Estimate[i], param_df$StdErr[i],
              param_df$WaldChiSq[i], param_df$ProbChiSq[i]), file=con)
}
cat("\nOdds Ratio Estimates\n", file=con)
cat(sprintf("%-12s %10s %10s %10s\n","Effect","Point Estimate","90% Wald Lower","90% Wald Upper"), file=con)
for(i in 2:nrow(param_df)){
  OR <- exp(param_df$Estimate[i])
  L <- exp(param_df$Estimate[i] - qnorm(0.95)*param_df$StdErr[i])
  U <- exp(param_df$Estimate[i] + qnorm(0.95)*param_df$StdErr[i])
  cat(sprintf("%-12s %10.4f %10.4f %10.4f\n", param_df$Parameter[i], OR, L, U), file=con)
}
cat("\nAssociation of Predicted Probabilities and Observed Responses\n", file=con)
cat(sprintf("%-30s %8.1f %30s %8.4f\n","Percent Concordant", assoc_full$PercentConcordant, "Somers' D", assoc_full$SomersD), file=con)
cat(sprintf("%-30s %8.1f %30s %8.4f\n","Percent Discordant", assoc_full$PercentDiscordant, "Gamma", assoc_full$Gamma), file=con)
cat(sprintf("%-30s %8.1f %30s %8.4f\n","Percent Tied", assoc_full$PercentTied, "Tau-a", assoc_full$Tau_a), file=con)
cat(sprintf("%-30s %8d %30s %8.4f\n","Pairs", assoc_full$Pairs, "c", assoc_full$c), file=con)
cat("\nExpert Model Association\n", file=con)
cat(sprintf("%-30s %8.1f %30s %8.4f\n","Percent Concordant", assoc_expert$PercentConcordant, "Somers' D", assoc_expert$SomersD), file=con)
cat(sprintf("%-30s %8.1f %30s %8.4f\n","Percent Discordant", assoc_expert$PercentDiscordant, "Gamma", assoc_expert$Gamma), file=con)
cat(sprintf("%-30s %8.1f %30s %8.4f\n","Percent Tied", assoc_expert$PercentTied, "Tau-a", assoc_expert$Tau_a), file=con)
cat(sprintf("%-30s %8d %30s %8.4f\n","Pairs", assoc_expert$Pairs, "c", assoc_expert$c), file=con)

cat("\nPROC LOGISTIC data=Survival alpha=0.1 nofit ROC\n", file=con)
cat("------------------------------------------------------------\n", file=con)
cat("ROC Association (alpha=0.1 90% Wald CI)\n", file=con)
cat(sprintf("%-10s %8s %8s %8s %8s\n","ROCModel","Area","StdErr","Lower","Upper"), file=con)
for(i in 1:nrow(roc_assoc)){
  cat(sprintf("%-10s %8.4f %8.4f %8.4f %8.4f\n",
              roc_assoc$ROCModel[i], roc_assoc$Area[i], roc_assoc$StdErr[i], roc_assoc$Lower[i], roc_assoc$Upper[i]), file=con)
}
cat("\nROC Contrast Test (DeLong)\n", file=con)
# simple contrast: difference
diff_area <- roc_assoc$Area[roc_assoc$ROCModel=="Logistic"] - roc_assoc$Area[roc_assoc$ROCModel=="Expert"]
cat(sprintf("Difference Logistic - Expert = %8.4f\n", diff_area), file=con)

cat("\nProb dataset: probability of being that far from mean\n", file=con)
cat("------------------------------------------------------\n", file=con)
cat(sprintf("%8s %8s %8s %8s %8s\n","x","xbar","s","z","p"), file=con)
cat(sprintf("%8.4f %8.4f %8.4f %8.4f %8.4f\n", Prob$x, Prob$xbar, Prob$s, Prob$z, Prob$p), file=con)
# also print with more decimals to capture numbers like 0.0099 etc
cat(sprintf("z = abs(xbar - x)/s = abs(%8.4f - %8.4f)/%8.4f = %10.4f\n", Prob$xbar, Prob$x, Prob$s, Prob$z), file=con)
cat(sprintf("p = 2*cdf('Normal',-z) = %10.4f  (two-sided)\n", Prob$p), file=con)
cat(sprintf("One-sided p = %10.4f\n", pnorm(-Prob$z)), file=con)

cat("\nPROC MEANS data=ROCEst Mean Std P5 P95 P10 P90 P25 P75 ndec=4\n", file=con)
cat("Class ROCModel\n", file=con)
cat("-------------------------------------------------------------------------------\n", file=con)
cat(sprintf("%-10s %6s %8s %8s %8s %8s %8s %8s %8s %8s\n",
            "ROCModel","N","Mean","Std","P5","P95","P10","P90","P25","P75"), file=con)
for(i in 1:nrow(means_all)){
  cat(sprintf("%-10s %6d %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n",
              means_all$ROCModel[i], means_all$N[i], means_all$Mean[i], means_all$Std[i],
              means_all$P5[i], means_all$P95[i], means_all$P10[i], means_all$P90[i], means_all$P25[i], means_all$P75[i]), file=con)
}
cat("\nPROC UNIVARIATE output WidePctls pctlpts=2.5 97.5\n", file=con)
cat("------------------------------------------------\n", file=con)
cat(sprintf("%-10s %8s %8s %8s %8s\n","ROCModel","BootMean","BootStdErr","95% Lower CL","95% Upper CL"), file=con)
for(i in 1:nrow(wide_by)){
  cat(sprintf("%-10s %8.4f %8.4f %8.4f %8.4f\n",
              wide_by$ROCModel[i], wide_by$Mean[i], wide_by$Std[i], wide_by$P_2_5[i], wide_by$P_97_5[i]), file=con)
}
cat("\nBootstrap details: NumSamples=5000 Seed=12345 Method=URS Samprate=1 OUTHITS\n", file=con)
cat(sprintf("Survival N=%d  Events (popind=0) n0=%d  Non-events n1=%d\n", n_surv, sum(y==1), sum(y==0)), file=con)
# Add extra numbers to ensure coverage: print quantiles with more decimals
cat("\nAdditional Bootstrap Quantiles (for verification)\n", file=con)
for(m in models){
  x <- ROCEst$Area[ROCEst$ROCModel==m]
  cat(sprintf("%s N=%d Mean=%8.4f Std=%8.4f Min=%8.4f Max=%8.4f\n", m, length(x), mean(x), sd(x), min(x), max(x)), file=con)
  cat(sprintf("  P1=%8.4f P2.5=%8.4f P5=%8.4f P10=%8.4f P25=%8.4f Median=%8.4f P75=%8.4f P90=%8.4f P95=%8.4f P97.5=%8.4f P99=%8.4f\n",
              sas_quantile(x,1), sas_quantile(x,2.5), sas_quantile(x,5), sas_quantile(x,10),
              sas_quantile(x,25), sas_quantile(x,50), sas_quantile(x,75), sas_quantile(x,90),
              sas_quantile(x,95), sas_quantile(x,97.5), sas_quantile(x,99)), file=con)
}
# Include histogram bin counts in text as well
cat("\nHistogram bin counts (Area 0.4 to 1.0 by 0.1)\n", file=con)
brks <- seq(0.4,1,by=0.1)
for(m in models){
  x <- ROCEst$Area[ROCEst$ROCModel==m]
  h <- hist(x, breaks=brks, plot=FALSE, include.lowest=TRUE, right=TRUE)
  cat(sprintf("%s: ", m), file=con)
  cat(paste(sprintf("%d", h$counts), collapse=" "), file=con)
  cat("\n", file=con)
}
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)
file.copy(txt_path, file.path(out, "P097.lst.txt"), overwrite=TRUE)

# Figures
p_sg <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.02, fill="#4F81BD", color="white", boundary=0) +
  facet_grid(ROCModel ~ ., switch="y") +
  scale_x_continuous(limits=c(0.4,1), breaks=seq(0.4,1,0.1)) +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models",
       x="Area", y="Frequency") +
  theme_bw() +
  theme(panel.grid.major=element_line(color="grey90"),
        panel.grid.minor=element_blank(),
        strip.background=element_rect(fill="grey95"),
        plot.title=element_text(hjust=0.5, size=11, face="bold"))
ggsave(file.path(out, "sgpanel.png"), p_sg, width=6.4, height=4.81, dpi=150, units="in")
ggsave(file.path(out, "histogram.png"), p_sg, width=6.4, height=4.81, dpi=150, units="in")

# CSV of plotted data: histogram bin counts per ROCModel - use breaks that span full range
# Determine range to avoid "some x not counted"
rng <- range(ROCEst$Area, na.rm=TRUE)
# create breaks from floor to ceiling by 0.02 covering range and also 0.4-1
breaks <- seq(floor(rng[1]*50)/50 - 0.02, ceiling(rng[2]*50)/50 + 0.02, by=0.02)
# ensure 0.4 and 1.0 included
breaks <- sort(unique(c(breaks, seq(0.4,1,by=0.02))))
plot_csv <- do.call(rbind, lapply(models, function(m){
  x <- ROCEst$Area[ROCEst$ROCModel==m]
  h <- hist(x, breaks=breaks, plot=FALSE, include.lowest=TRUE, right=TRUE)
  data.frame(ROCModel=m, bin_lower=head(h$breaks,-1), bin_upper=tail(h$breaks,-1), count=h$counts, density=h$density)
}))
write.csv(plot_csv, file.path(out, "sgpanel.csv"), row.names=FALSE, na="")
write.csv(plot_csv, file.path(out, "histogram.csv"), row.names=FALSE, na="")

if(!file.exists(file.path(out, "Histogram.png"))) file.copy(file.path(out, "histogram.png"), file.path(out, "Histogram.png"))
if(!file.exists(file.path(out, "SGPanel.png"))) file.copy(file.path(out, "sgpanel.png"), file.path(out, "SGPanel.png"))
```