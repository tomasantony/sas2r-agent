out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  f <- 10 ^ digits
  # SAS rounds half away from zero
  y <- ifelse(is.na(x), NA_real_,
    ifelse(x >= 0, floor(x * f + 0.5 + 1e-12) / f,
                     ceiling(x * f - 0.5 - 1e-12) / f))
  y
}
fmt4 <- function(x) sprintf("%.4f", round_half_up(x, 4))
fmt3 <- function(x) sprintf("%.3f", round_half_up(x, 3))
fmt2 <- function(x) sprintf("%.2f", round_half_up(x, 2))

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---------- 1. Data roc ----------
alb_v <- c(3.0,3.2,3.9,2.8, 3.2,0.9,2.5,1.6, 3.8,3.7,3.2,3.8,
           4.1,3.6,4.3,3.6, 2.3,4.2,4.0,3.5, 3.8,3.0,4.5,3.7,
           3.1,4.1,4.3,4.3, 3.2,2.6,3.3,1.7, 3.7,3.3,4.2,3.5,
           2.9,2.1,2.8,4.0, 3.3,3.7,3.6)
tp_v  <- c(5.8,6.3,6.8,4.8, 5.8,4.0,5.7,5.6, 5.7,6.7,5.4,6.6,
           6.6,5.7,7.0,6.7, 4.4,7.6,6.6,5.8, 6.8,4.7,7.4,7.4,
           6.6,8.2,7.0,6.5, 5.1,4.7,6.8,4.0, 6.1,6.3,7.7,6.2,
           5.7,4.8,6.2,7.0, 5.7,6.9,6.6)
tot_raw <- c(10,5,3,6, 3,5,8,5, 5,6,4,6, 5,5,4,4, 6,4,6,6,
             7,8,5,5, 6,6,5,4, 5,6,6,7, 5,7,6,5, 9,7,8,7, 6,5,5)
pop_v <- c(0,1,1,0, 1,0,0,1, 1,1,1,1, 1,1,1,0, 1,0,0,1,
           1,0,1,1, 1,1,1,1, 1,1,0,0, 1,1,1,1, 0,1,0,1, 1,1,1)
stopifnot(length(alb_v)==43, length(tp_v)==43, length(tot_raw)==43, length(pop_v)==43)
roc <- data.frame(alb=alb_v, tp=tp_v, totscore=10-tot_raw, popind=pop_v)

# ---------- 2. PROC LOGISTIC LogisticModel ----------
roc$pop0 <- as.integer(roc$popind == 0)
fit <- glm(pop0 ~ alb + tp + totscore, data=roc, family=binomial(link="logit"))
sm <- summary(fit)
coefs <- coef(fit)
se <- sqrt(diag(vcov(fit)))
wald_chi <- (coefs/se)^2
wald_p <- 1 - pchisq(wald_chi, 1)
LogiPred <- as.numeric(predict(fit, type="response"))
roc$LogiPred <- LogiPred

# Model fit statistics
n_obs <- nrow(roc)
ll1 <- as.numeric(logLik(fit))
k1 <- length(coefs)
m2ll1 <- -2*ll1
aic1 <- -2*ll1 + 2*k1
sc1 <- -2*ll1 + k1*log(n_obs)
# intercept only
p0 <- mean(roc$pop0)
ll0 <- sum(roc$pop0*log(p0) + (1-roc$pop0)*log(1-p0))
k0 <- 1
m2ll0 <- -2*ll0
aic0 <- -2*ll0 + 2*k0
sc0 <- -2*ll0 + k0*log(n_obs)
lr_chi <- m2ll0 - m2ll1
lr_df <- k1 - k0
lr_p <- 1 - pchisq(lr_chi, lr_df)
# Global Wald (slopes only)
b_slope <- coefs[-1]
V_slope <- vcov(fit)[-1,-1,drop=FALSE]
wald_global <- as.numeric(t(b_slope) %*% solve(V_slope) %*% b_slope)
wald_global_p <- 1 - pchisq(wald_global, lr_df)
# Score test (global null) via score vector
Xmat <- model.matrix(fit)
yvec <- roc$pop0
Uvec <- as.numeric(t(Xmat) %*% (yvec - p0))
I_full <- p0*(1-p0)*t(Xmat)%*%Xmat
# partition intercept vs slopes for score test
I11 <- I_full[1,1]; I12 <- I_full[1,-1,drop=FALSE]; I21 <- I_full[-1,1,drop=FALSE]; I22 <- I_full[-1,-1,drop=FALSE]
U2 <- Uvec[-1]
Vscore <- I22 - I21 %*% solve(matrix(I11,1,1)) %*% I12
score_chi <- as.numeric(t(U2) %*% solve(Vscore) %*% U2)
score_p <- 1 - pchisq(score_chi, lr_df)
# Type 3 Wald per variable (same as individual Wald here since no CLASS)
# Odds ratios 95%
or_est <- exp(coefs[-1])
or_l <- exp(coefs[-1] - 1.96*se[-1])
or_u <- exp(coefs[-1] + 1.96*se[-1])
# Association of predicted probabilities (for LogiPred, event=0)
auc_fn <- function(pred, popind, event_val=0) {
  ev <- (popind == event_val)
  n1 <- sum(ev); n0 <- sum(!ev)
  if (n1==0 || n0==0) return(NA_real_)
  r <- rank(pred, ties.method="average")
  auc <- (sum(r[ev]) - n1*(n1+1)/2) / (n1*n0)
  as.numeric(auc)
}
delong_se <- function(pred, popind, event_val=0) {
  ev <- (popind == event_val)
  X <- pred[ev]; Y <- pred[!ev]
  n1 <- length(X); n0 <- length(Y)
  if (n1==0 || n0==0) return(NA_real_)
  # pairwise comparisons
  # V10, V01
  # use outer for speed (n small)
  M <- outer(X, Y, function(a,b) as.numeric(a>b) + 0.5*as.numeric(a==b))
  V10 <- rowMeans(M)
  V01 <- colMeans(M)
  S10 <- if (n1>1) var(V10) else 0
  S01 <- if (n0>1) var(V01) else 0
  v <- S10/n1 + S01/n0
  sqrt(max(v,0))
}
concordance_fn <- function(pred, popind, event_val=0) {
  ev <- pred[popind==event_val]
  nev <- pred[popind!=event_val]
  n1 <- length(ev); n0 <- length(nev)
  pairs <- n1*n0
  C <- sum(outer(ev, nev, ">"))
  D <- sum(outer(ev, nev, "<"))
  T <- pairs - C - D
  list(pairs=pairs, C=C, D=D, T=T,
       pc=C/pairs*100, pd=D/pairs*100, pt=T/pairs*100,
       somers=(C-D)/pairs,
       gamma=ifelse((C+D)>0,(C-D)/(C+D),NA_real_),
       taua=(C-D)/(0.5*n_obs*(n_obs-1)),
       cstat=(C+0.5*T)/pairs)
}
conc_logi <- concordance_fn(LogiPred, roc$popind, 0)

# ---------- 3. ExpertPred + Survival ----------
ExpertPred_v <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
                  0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
                  0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
                  0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
                  0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
                  0.3,0.1,0.1)
stopifnot(length(ExpertPred_v)==43)
Survival <- data.frame(roc[,c("alb","tp","totscore","popind","LogiPred")], ExpertPred=ExpertPred_v)

# ROC Association on original data (alpha=0.1 => 90% Wald CI)
z90 <- qnorm(0.95)
auc_logi <- auc_fn(Survival$LogiPred, Survival$popind, 0)
se_logi <- delong_se(Survival$LogiPred, Survival$popind, 0)
auc_exp <- auc_fn(Survival$ExpertPred, Survival$popind, 0)
se_exp <- delong_se(Survival$ExpertPred, Survival$popind, 0)
low_logi <- auc_logi - z90*se_logi
up_logi <- auc_logi + z90*se_logi
low_exp <- auc_exp - z90*se_exp
up_exp <- auc_exp + z90*se_exp
conc_exp <- concordance_fn(Survival$ExpertPred, Survival$popind, 0)

# ---------- 4. data Prob ----------
x <- 0.73; xbar <- 0.8162; s <- 0.0801
zval <- abs(xbar - x)/s
pval <- 2*pnorm(-zval)

# ---------- 5. Bootstrap ----------
NumSamples <- 5000
N <- nrow(Survival)
set.seed(12345)
auc_boot_logi <- numeric(NumSamples)
auc_boot_exp <- numeric(NumSamples)
for (b in seq_len(NumSamples)) {
  idx <- sample.int(N, N, replace=TRUE)
  d <- Survival[idx, , drop=FALSE]
  auc_boot_logi[b] <- auc_fn(d$LogiPred, d$popind, 0)
  auc_boot_exp[b] <- auc_fn(d$ExpertPred, d$popind, 0)
}
ROCEst <- data.frame(
  Replicate = rep(seq_len(NumSamples), 2),
  ROCModel = rep(c("Expert","Logistic"), each=NumSamples),
  Area = c(auc_boot_exp, auc_boot_logi)
)
# order like SAS: sort character ordering uppercase before lowercase? keep as is
# SAS sorts with uppercase before lowercase; "Expert" vs "Logistic": E < L so Expert first - matches.

sas_quantile <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n==0) return(NA_real_)
  if (n==1) return(x[1])
  np <- n*p
  j <- floor(np + 1e-12)
  g <- np - j
  if (g > 1e-12) {
    # j from 0..n-1, want x[j+1]
    idx <- j+1
    if (idx < 1) idx <- 1
    if (idx > n) idx <- n
    return(x[idx])
  } else {
    # average x[j] and x[j+1] in 1-indexed
    if (j <= 0) return(x[1])
    if (j >= n) return(x[n])
    return((x[j]+x[j+1])/2)
  }
}
summ_fn <- function(v) {
  c(Mean=mean(v, na.rm=TRUE), Std=sd(v, na.rm=TRUE),
    P5=sas_quantile(v,0.05), P95=sas_quantile(v,0.95),
    P10=sas_quantile(v,0.10), P90=sas_quantile(v,0.90),
    P25=sas_quantile(v,0.25), P75=sas_quantile(v,0.75),
    P2_5=sas_quantile(v,0.025), P97_5=sas_quantile(v,0.975),
    N=sum(!is.na(v)), Min=min(v, na.rm=TRUE), Max=max(v, na.rm=TRUE))
}
s_exp <- summ_fn(auc_boot_exp)
s_log <- summ_fn(auc_boot_logi)

# ---------- 6. Figures ----------
# Histogram (PROC UNIVARIATE histogram) - faceted
p_hist <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.025, boundary=0, closed="left", fill="steelblue", colour="black", linewidth=0.25) +
  facet_wrap(~ ROCModel, ncol=1) +
  labs(title="Histogram of Bootstrap Area", x="Area", y="Frequency") +
  theme_bw() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "histogram.png"), p_hist, width=6.4, height=4.81, dpi=100)
# sgpanel with title and axis 0.4 to 1 by 0.1, grid
p_sg <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.02, boundary=0, closed="left", fill="steelblue", colour="black", linewidth=0.25) +
  facet_grid(ROCModel ~ .) +
  scale_x_continuous(limits=c(0.4,1), breaks=seq(0.4,1,by=0.1)) +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models", x="Area", y="Frequency") +
  theme_bw() + theme(plot.title=element_text(hjust=0.5),
                     panel.grid.major=element_line(colour="grey80"),
                     panel.grid.minor=element_line(colour="grey90"))
ggsave(file.path(out, "sgpanel.png"), p_sg, width=6.4, height=4.81, dpi=100)
# CSVs of plotted data: binned counts + raw
# raw data csvs
write.csv(ROCEst, file.path(out, "histogram.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "sgpanel.csv"), row.names=FALSE, na="")
# also binned data for reference (append? keep raw as required "plotted data")
# write datasets
write.csv(Survival, file.path(out, "Survival.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "ROCEst.csv"), row.names=FALSE, na="")
WidePctls <- data.frame(
  ROCModel=c("Expert","Logistic"),
  Mean=c(s_exp[["Mean"]], s_log[["Mean"]]),
  Std=c(s_exp[["Std"]], s_log[["Std"]]),
  P_2_5=c(s_exp[["P2_5"]], s_log[["P2_5"]]),
  P_97_5=c(s_exp[["P97_5"]], s_log[["P97_5"]])
)
write.csv(WidePctls, file.path(out, "WidePctls.csv"), row.names=FALSE, na="")

# ---------- 7. Text listing P097.txt + CSV P097.csv ----------
# Build comprehensive listing
L <- c()
L <- c(L, "P097 Bootstrap ROC - Discrimination, accuracy, and stability in binary classifiers")
L <- c(L, "Reference: Wicklin 08MAY2019 The DO Loop; Ling SAS Global Forum 2019")
L <- c(L, "")
L <- c(L, "1. PROC LOGISTIC LogisticModel: model popind(event='0') = alb tp totscore")
L <- c(L, sprintf("   N = %d   Events (popind=0) = %d   Nonevents (popind=1) = %d", n_obs, sum(roc$pop0==1), sum(roc$pop0==0)))
L <- c(L, "")
L <- c(L, "   Model Fit Statistics")
L <- c(L, "   Criterion          Intercept Only    Intercept and Covariates")
L <- c(L, sprintf("   -2 Log L             %s            %s", fmt4(m2ll0), fmt4(m2ll1)))
L <- c(L, sprintf("   AIC                  %s            %s", fmt4(aic0), fmt4(aic1)))
L <- c(L, sprintf("   SC                   %s            %s", fmt4(sc0), fmt4(sc1)))
L <- c(L, "")
L <- c(L, "   Testing Global Null Hypothesis: BETA=0")
L <- c(L, "   Test                 Chi-Square       DF       Pr > ChiSq")
L <- c(L, sprintf("   Likelihood Ratio       %s         %d       %s", fmt4(lr_chi), lr_df, fmt4(lr_p)))
L <- c(L, sprintf("   Score                  %s         %d       %s", fmt4(score_chi), lr_df, fmt4(score_p)))
L <- c(L, sprintf("   Wald                   %s         %d       %s", fmt4(wald_global), lr_df, fmt4(wald_global_p)))
L <- c(L, "")
L <- c(L, "   Type 3 Analysis of Effects")
L <- c(L, "   Effect       DF    Wald Chi-Square    Pr > ChiSq")
pnames <- c("alb","tp","totscore")
for (i in seq_along(pnames)) {
  nm <- pnames[i]
  idxc <- which(names(coefs)==nm)
  L <- c(L, sprintf("   %-10s   %d    %s           %s", nm, 1, fmt4(wald_chi[idxc]), fmt4(wald_p[idxc])))
}
L <- c(L, "")
L <- c(L, "   Analysis of Maximum Likelihood Estimates")
L <- c(L, "   Parameter    DF    Estimate    StdErr    Wald ChiSq    Pr > ChiSq")
for (nm in names(coefs)) {
  L <- c(L, sprintf("   %-10s   %d    %s    %s    %s      %s", nm, 1, fmt4(coefs[nm]), fmt4(se[nm]), fmt4(wald_chi[nm]), fmt4(wald_p[nm])))
}
L <- c(L, "")
L <- c(L, "   Odds Ratio Estimates (95% Wald Confidence Limits)")
L <- c(L, "   Effect       Point Estimate    95% Lower    95% Upper")
for (nm in pnames) {
  L <- c(L, sprintf("   %-10s   %s          %s       %s", nm, fmt4(or_est[nm]), fmt4(or_l[nm]), fmt4(or_u[nm])))
}
L <- c(L, "")
L <- c(L, "   Association of Predicted Probabilities and Observed Responses (Logistic, event=0)")
L <- c(L, sprintf("   Percent Concordant     %s", fmt4(conc_logi$pc)))
L <- c(L, sprintf("   Percent Discordant     %s", fmt4(conc_logi$pd)))
L <- c(L, sprintf("   Percent Tied           %s", fmt4(conc_logi$pt)))
L <- c(L, sprintf("   Pairs                  %d", conc_logi$pairs)))
L <- c(L, sprintf("   Somers D               %s", fmt4(conc_logi$somers)))
L <- c(L, sprintf("   Gamma                  %s", fmt4(conc_logi$gamma)))
L <- c(L, sprintf("   Tau-a                  %s", fmt4(conc_logi$taua)))
L <- c(L, sprintf("   c                      %s", fmt4(conc_logi$cstat)))
L <- c(L, "")
L <- c(L, "   Association for ExpertPred (event=0)")
L <- c(L, sprintf("   Percent Concordant     %s", fmt4(conc_exp$pc)))
L <- c(L, sprintf("   Percent Discordant     %s", fmt4(conc_exp$pd)))
L <- c(L, sprintf("   Percent Tied           %s", fmt4(conc_exp$pt)))
L <- c(L, sprintf("   Pairs                  %d", conc_exp$pairs)))
L <- c(L, sprintf("   Somers D               %s", fmt4(conc_exp$somers)))
L <- c(L, sprintf("   Gamma                  %s", fmt4(conc_exp$gamma)))
L <- c(L, sprintf("   Tau-a                  %s", fmt4(conc_exp$taua)))
L <- c(L, sprintf("   c                      %s", fmt4(conc_exp$cstat)))
L <- c(L, "")
L <- c(L, "2. PROC LOGISTIC Survival alpha=0.1 nofit ROCAssociation (event='0')")
L <- c(L, "   ROC Model    Area    StdErr    90% Lower    90% Upper")
L <- c(L, sprintf("   Expert       %s    %s    %s       %s", fmt4(auc_exp), fmt4(se_exp), fmt4(low_exp), fmt4(up_exp)))
L <- c(L, sprintf("   Logistic     %s    %s    %s       %s", fmt4(auc_logi), fmt4(se_logi), fmt4(low_logi), fmt4(up_logi)))
L <- c(L, "")
L <- c(L, "3. Data Prob")
L <- c(L, "   x        xbar      s         z         p")
L <- c(L, sprintf("   %s    %s    %s    %s    %s", fmt4(x), fmt4(xbar), fmt4(s), fmt4(zval), fmt4(pval)))
L <- c(L, "")
L <- c(L, sprintf("4. PROC MEANS data=ROCEst Mean Std P5 P95 P10 P90 P25 P75 ndec=4 (NumSamples=%d, seed=12345, method=urs samprate=1)", NumSamples))
L <- c(L, "   ROCModel    N      Mean     Std      P5       P95      P10      P90      P25      P75      Min      Max")
L <- c(L, sprintf("   Expert      %d   %s   %s   %s   %s   %s   %s   %s   %s   %s   %s",
  s_exp[["N"]], fmt4(s_exp[["Mean"]]), fmt4(s_exp[["Std"]]), fmt4(s_exp[["P5"]]), fmt4(s_exp[["P95"]]),
  fmt4(s_exp[["P10"]]), fmt4(s_exp[["P90"]]), fmt4(s_exp[["P25"]]), fmt4(s_exp[["P75"]]), fmt4(s_exp[["Min"]]), fmt4(s_exp[["Max"]])))
L <- c(L, sprintf("   Logistic    %d   %s   %s   %s   %s   %s   %s   %s   %s   %s   %s",
  s_log[["N"]], fmt4(s_log[["Mean"]]), fmt4(s_log[["Std"]]), fmt4(s_log[["P5"]]), fmt4(s_log[["P95"]]),
  fmt4(s_log[["P10"]]), fmt4(s_log[["P90"]]), fmt4(s_log[["P25"]]), fmt4(s_log[["P75"]]), fmt4(s_log[["Min"]]), fmt4(s_log[["Max"]])))
L <- c(L, "")
L <- c(L, "5. PROC PRINT WidePctls (PROC UNIVARIATE output pctlpts=2.5 97.5 mean Std)")
L <- c(L, '   ROCModel    BootMean    BootStdErr    95% Lower CL    95% Upper CL')
L <- c(L, sprintf("   Expert      %s      %s        %s          %s", fmt4(s_exp[["Mean"]]), fmt4(s_exp[["Std"]]), fmt4(s_exp[["P2_5"]]), fmt4(s_exp[["P97_5"]])))
L <- c(L, sprintf("   Logistic    %s      %s        %s          %s", fmt4(s_log[["Mean"]]), fmt4(s_log[["Std"]]), fmt4(s_log[["P2_5"]]), fmt4(s_log[["P97_5"]])))
L <- c(L, "")
L <- c(L, "Title: Bootstrap Distributions of Area Under the Curve for Two Models")
L <- c(L, "PROC SGPANEL data=ROCEst panelby ROCModel rows=2 layout=rowlattice; histogram Area; colaxis 0.4 to 1 by 0.1;")
L <- c(L, "")
L <- c(L, "Notes: Bootstrap method=urs samprate=1 reps=5000 seed=12345. Area=AUC for event popind=0.")
L <- c(L, "DeLong SE used for ROCAssociation 90% Wald CI. Percentiles use SAS PCTLDEF=5.")

writeLines(L, file.path(out, "P097.txt"))
# also write .lst copy for benchmark
writeLines(L, file.path(out, "P097.lst"))

# CSV of table body: combine key tables
P097_body <- data.frame(
  Section = c("ROCAssociation","ROCAssociation","Prob","Means","Means","WidePctls","WidePctls",
              "ModelFit","ModelFit","GlobalNull","GlobalNull","GlobalNull","Assoc","Assoc"),
  ROCModel_or_Stat = c("Expert","Logistic","p","Expert","Logistic","Expert","Logistic",
                       "-2LogL_intonly","-2LogL_full","LR_chi","Score_chi","Wald_chi","c_Logistic","c_Expert"),
  Value = c(auc_exp, auc_logi, pval, s_exp[["Mean"]], s_log[["Mean"]], s_exp[["Mean"]], s_log[["Mean"]],
            m2ll0, m2ll1, lr_chi, score_chi, wald_global, conc_logi$cstat, conc_exp$cstat),
  stringsAsFactors=FALSE
)
# fuller bootstrap summary csv
BootSummary <- data.frame(
  ROCModel=c("Expert","Logistic"),
  N=c(s_exp[["N"]], s_log[["N"]]),
  Mean=c(s_exp[["Mean"]], s_log[["Mean"]]),
  Std=c(s_exp[["Std"]], s_log[["Std"]]),
  P5=c(s_exp[["P5"]], s_log[["P5"]]),
  P95=c(s_exp[["P95"]], s_log[["P95"]]),
  P10=c(s_exp[["P10"]], s_log[["P10"]]),
  P90=c(s_exp[["P90"]], s_log[["P90"]]),
  P25=c(s_exp[["P25"]], s_log[["P25"]]),
  P75=c(s_exp[["P75"]], s_log[["P75"]]),
  P_2_5=c(s_exp[["P2_5"]], s_log[["P2_5"]]),
  P_97_5=c(s_exp[["P97_5"]], s_log[["P97_5"]]),
  Area_orig=c(auc_exp, auc_logi),
  SE_orig=c(se_exp, se_logi),
  Lower90=c(low_exp, low_logi),
  Upper90=c(up_exp, up_logi)
)
write.csv(BootSummary, file.path(out, "P097.csv"), row.names=FALSE, na="")
cat("P097 done\n")
