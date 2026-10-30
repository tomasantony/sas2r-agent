```r
## P097 - Bootstrap ROC (translation of P097_BootstrapROC.sas)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## SAS-style rounding (half away from zero)
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
fmt <- function(x, w, d) formatC(round_half_up(x, d), width = w, format = "f", digits = d)
fmtp <- function(p, w = 8) ifelse(p < 0.0001, formatC("<.0001", width = w), fmt(p, w, 4))

## SAS percentile definition 5 (default) == R quantile type 2
pctl5 <- function(x, p) as.numeric(quantile(x, p, type = 2, names = FALSE))

txt <- character(0)
addtxt <- function(...) txt <<- c(txt, ...)

## ------------------------------------------------------------------
## data roc
## ------------------------------------------------------------------
vals <- c(
3.0,5.8,10,0, 3.2,6.3,5,1, 3.9,6.8,3,1, 2.8,4.8,6,0,
3.2,5.8,3,1, 0.9,4.0,5,0, 2.5,5.7,8,0, 1.6,5.6,5,1,
3.8,5.7,5,1, 3.7,6.7,6,1, 3.2,5.4,4,1, 3.8,6.6,6,1,
4.1,6.6,5,1, 3.6,5.7,5,1, 4.3,7.0,4,1, 3.6,6.7,4,0,
2.3,4.4,6,1, 4.2,7.6,4,0, 4.0,6.6,6,0, 3.5,5.8,6,1,
3.8,6.8,7,1, 3.0,4.7,8,0, 4.5,7.4,5,1, 3.7,7.4,5,1,
3.1,6.6,6,1, 4.1,8.2,6,1, 4.3,7.0,5,1, 4.3,6.5,4,1,
3.2,5.1,5,1, 2.6,4.7,6,1, 3.3,6.8,6,0, 1.7,4.0,7,0,
3.7,6.1,5,1, 3.3,6.3,7,1, 4.2,7.7,6,1, 3.5,6.2,5,1,
2.9,5.7,9,0, 2.1,4.8,7,1, 2.8,6.2,8,0, 4.0,7.0,7,1,
3.3,5.7,6,1, 3.7,6.9,5,1, 3.6,6.6,5,1)
m <- matrix(vals, ncol = 4, byrow = TRUE)
roc <- data.frame(alb = m[,1], tp = m[,2], totscore = 10 - m[,3], popind = m[,4])

## ------------------------------------------------------------------
## proc logistic: model popind(event='0') = alb tp totscore
## ------------------------------------------------------------------
roc$ev <- as.integer(roc$popind == 0)
fit <- glm(ev ~ alb + tp + totscore, data = roc, family = binomial(),
           control = glm.control(epsilon = 1e-12, maxit = 100))
fit0 <- glm(ev ~ 1, data = roc, family = binomial())
LogiOut <- roc[, c("alb","tp","totscore","popind")]
LogiOut$LogiPred <- as.numeric(fitted(fit))
nobs <- nrow(roc)
n_ev <- sum(roc$ev); n_nev <- nobs - n_ev

addtxt("The LOGISTIC Procedure", "Model Information",
       "Data Set WORK.ROC", "Response Variable popind",
       "Number of Response Levels 2", "Model binary logit",
       "Optimization Technique Fisher's scoring", "",
       sprintf("Number of Observations Read %d", nobs),
       sprintf("Number of Observations Used %d", nobs), "",
       "Response Profile",
       "Ordered Value  popind  Total Frequency",
       sprintf("1  0  %d", n_ev), sprintf("2  1  %d", n_nev), "",
       "Probability modeled is popind=0.", "",
       "Model Convergence Status",
       "Convergence criterion (GCONV=1E-8) satisfied.", "")

## Model fit statistics
ll0 <- as.numeric(logLik(fit0)); ll1 <- as.numeric(logLik(fit))
k0 <- 1; k1 <- 4
m2ll0 <- -2*ll0; m2ll1 <- -2*ll1
fitstat <- data.frame(Criterion = c("AIC","SC","-2 Log L"),
                      InterceptOnly = c(m2ll0 + 2*k0, m2ll0 + k0*log(nobs), m2ll0),
                      InterceptAndCovariates = c(m2ll1 + 2*k1, m2ll1 + k1*log(nobs), m2ll1))
addtxt("Model Fit Statistics",
       sprintf("%-10s %12s %14s", "Criterion", "Intercept Only", "Intercept and Covariates"))
for (i in 1:3)
  addtxt(sprintf("%-10s %12s %14s", fitstat$Criterion[i],
                 fmt(fitstat$InterceptOnly[i],12,3), fmt(fitstat$InterceptAndCovariates[i],14,3)))
addtxt("")
write.csv(fitstat, file.path(out, "fitstatistics.csv"), row.names = FALSE, na = "")

## Global null tests
X <- model.matrix(fit)
y <- roc$ev
p0 <- mean(y)
U <- t(X) %*% (y - p0)
I0 <- p0*(1-p0) * t(X) %*% X
score <- as.numeric(t(U) %*% solve(I0) %*% U)
lr <- m2ll0 - m2ll1
b <- coef(fit)[-1]; V <- vcov(fit)[-1,-1]
wald <- as.numeric(t(b) %*% solve(V) %*% b)
gn <- data.frame(Test = c("Likelihood Ratio","Score","Wald"),
                 ChiSq = c(lr, score, wald), DF = 3,
                 ProbChiSq = pchisq(c(lr, score, wald), 3, lower.tail = FALSE))
addtxt("Testing Global Null Hypothesis: BETA=0",
       sprintf("%-18s %10s %4s %10s", "Test","Chi-Square","DF","Pr > ChiSq"))
for (i in 1:3)
  addtxt(sprintf("%-18s %10s %4d %10s", gn$Test[i], fmt(gn$ChiSq[i],10,4), 3L, fmtp(gn$ProbChiSq[i],10)))
addtxt("")
write.csv(gn, file.path(out, "globaltests.csv"), row.names = FALSE, na = "")

cf <- summary(fit)$coefficients
pe <- data.frame(Parameter = c("Intercept","alb","tp","totscore"),
                 DF = 1, Estimate = cf[,1], StdErr = cf[,2],
                 WaldChiSq = (cf[,1]/cf[,2])^2,
                 ProbChiSq = pchisq((cf[,1]/cf[,2])^2, 1, lower.tail = FALSE))
addtxt("Analysis of Maximum Likelihood Estimates",
       sprintf("%-10s %3s %10s %10s %10s %8s", "Parameter","DF","Estimate","StdErr","WaldChiSq","Pr>ChiSq"))
for (i in seq_len(nrow(pe)))
  addtxt(sprintf("%-10s %3d %10s %10s %10s %8s", pe$Parameter[i], 1L,
                 fmt(pe$Estimate[i],10,4), fmt(pe$StdErr[i],10,4),
                 fmt(pe$WaldChiSq[i],10,4), fmtp(pe$ProbChiSq[i],8)))
addtxt("")
write.csv(pe, file.path(out, "parameterestimates.csv"), row.names = FALSE, na = "")

## Odds ratios
zq <- qnorm(0.975)
orr <- data.frame(Effect = pe$Parameter[-1], OddsRatio = exp(pe$Estimate[-1]),
                  LowerCL = exp(pe$Estimate[-1] - zq*pe$StdErr[-1]),
                  UpperCL = exp(pe$Estimate[-1] + zq*pe$StdErr[-1]))
addtxt("Odds Ratio Estimates",
       sprintf("%-10s %10s %10s %10s", "Effect","Point Est","95% Lower","95% Upper"))
for (i in 1:3)
  addtxt(sprintf("%-10s %10s %10s %10s", orr$Effect[i], fmt(orr$OddsRatio[i],10,3),
                 fmt(orr$LowerCL[i],10,3), fmt(orr$UpperCL[i],10,3)))
addtxt("")
write.csv(orr, file.path(out, "oddsratios.csv"), row.names = FALSE, na = "")

## Association of predicted probabilities and observed responses (binned to 0.002)
pb <- round_half_up(LogiOut$LogiPred / 0.002) * 0.002
pe1 <- pb[y == 1]; pn1 <- pb[y == 0]
cmp <- outer(pe1, pn1, "-")
nc <- sum(cmp > 1e-12); nd <- sum(cmp < -1e-12); nt <- length(cmp) - nc - nd
npairs <- length(cmp)
assoc <- data.frame(PctConcordant = 100*nc/npairs, PctDiscordant = 100*nd/npairs,
                    PctTied = 100*nt/npairs, Pairs = npairs,
                    SomersD = (nc-nd)/npairs, Gamma = (nc-nd)/(nc+nd),
                    TauA = (nc-nd)/(0.5*nobs*(nobs-1)), c = (nc + 0.5*nt)/npairs)
addtxt("Association of Predicted Probabilities and Observed Responses",
       sprintf("Percent Concordant %6s   Somers' D %6s", fmt(assoc$PctConcordant,6,1), fmt(assoc$SomersD,6,3)),
       sprintf("Percent Discordant %6s   Gamma     %6s", fmt(assoc$PctDiscordant,6,1), fmt(assoc$Gamma,6,3)),
       sprintf("Percent Tied       %6s   Tau-a     %6s", fmt(assoc$PctTied,6,1), fmt(assoc$TauA,6,3)),
       sprintf("Pairs              %6d   c         %6s", npairs, fmt(assoc$c,6,3)), "")
write.csv(assoc, file.path(out, "association.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## ExpertPred + merge
## ------------------------------------------------------------------
ExpertPred <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
                0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
                0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
                0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
                0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
                0.3,0.1,0.1)
Survival <- LogiOut
Survival$ExpertPred <- ExpertPred
write.csv(Survival, file.path(out, "survival.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## ROC association (DeLong SE, 90% Wald CI)
## ------------------------------------------------------------------
psi_mat <- function(pred, ev) {
  e <- pred[ev == 1]; n <- pred[ev == 0]
  outer(e, n, function(a, b) (a > b) + 0.5 * (a == b))
}
roc_assoc <- function(pred, ev, alpha = 0.1) {
  P <- psi_mat(pred, ev)
  n1 <- nrow(P); n0 <- ncol(P)
  auc <- mean(P)
  V10 <- rowMeans(P); V01 <- colMeans(P)
  se <- sqrt(var(V10)/n1 + var(V01)/n0)
  z <- qnorm(1 - alpha/2)
  nc <- sum(P == 1); nd <- sum(P == 0)
  N <- length(pred)
  data.frame(Area = auc, StdErr = se, LowerArea = auc - z*se, UpperArea = auc + z*se,
             SomersD = (nc - nd)/(n1*n0), Gamma = (nc - nd)/(nc + nd),
             TauA = (nc - nd)/(0.5*N*(N - 1)))
}
ev <- as.integer(Survival$popind == 0)
ROCAssoc <- rbind(cbind(ROCModel = "Expert",   roc_assoc(Survival$ExpertPred, ev)),
                  cbind(ROCModel = "Logistic", roc_assoc(Survival$LogiPred, ev)))
addtxt("ROC Association Statistics",
       sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s", "ROC Model","Area","StdErr",
               "90%Lower","90%Upper","SomersD","Gamma","Tau-a"))
for (i in 1:2)
  addtxt(sprintf("%-10s %8s %8s %8s %8s %8s %8s %8s", ROCAssoc$ROCModel[i],
                 fmt(ROCAssoc$Area[i],8,4), fmt(ROCAssoc$StdErr[i],8,4),
                 fmt(ROCAssoc$LowerArea[i],8,4), fmt(ROCAssoc$UpperArea[i],8,4),
                 fmt(ROCAssoc$SomersD[i],8,4), fmt(ROCAssoc$Gamma[i],8,4),
                 fmt(ROCAssoc$TauA[i],8,4)))
addtxt("")
write.csv(ROCAssoc, file.path(out, "rocassociation.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## data Prob
## ------------------------------------------------------------------
Prob <- data.frame(x = 0.73, xbar = 0.8162, s = 0.0801)
Prob$z <- abs(Prob$xbar - Prob$x) / Prob$s
Prob$p <- 2 * pnorm(-Prob$z)
addtxt(sprintf("%6s %8s %8s %10s %10s", "x","xbar","s","z","p"),
       sprintf("%6s %8s %8s %10s %10s", "0.73","0.8162","0.0801",
               formatC(Prob$z, format = "f", digits = 5), formatC(Prob$p, format = "f", digits = 5)),
       "")
write.csv(Prob, file.path(out, "prob.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## Bootstrap (surveyselect URS, samprate=1, reps=5000)
## (SAS RNG stream cannot be reproduced; results are statistically equivalent)
## ------------------------------------------------------------------
NumSamples <- 5000
N <- nrow(Survival)
set.seed(12345)
P_exp <- psi_mat(Survival$ExpertPred, ev)
P_log <- psi_mat(Survival$LogiPred, ev)
idx1 <- which(ev == 1); idx0 <- which(ev == 0)
area_exp <- numeric(NumSamples); area_log <- numeric(NumSamples)
for (r in seq_len(NumSamples)) {
  w <- tabulate(sample.int(N, N, replace = TRUE), nbins = N)   # NumberHits
  w1 <- w[idx1]; w0 <- w[idx0]
  den <- sum(w1) * sum(w0)
  area_exp[r] <- as.numeric(t(w1) %*% P_exp %*% w0) / den
  area_log[r] <- as.numeric(t(w1) %*% P_log %*% w0) / den
}
ROCEst <- rbind(data.frame(Replicate = 1:NumSamples, ROCModel = "Expert",   Area = area_exp),
                data.frame(Replicate = 1:NumSamples, ROCModel = "Logistic", Area = area_log)) %>%
  arrange(Replicate, ROCModel)
write.csv(ROCEst, file.path(out, "rocest.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## proc means
## ------------------------------------------------------------------
means <- ROCEst %>% group_by(ROCModel) %>%
  summarise(NObs = n(), Mean = mean(Area), StdDev = sd(Area),
            P5 = pctl5(Area, .05), P95 = pctl5(Area, .95),
            P10 = pctl5(Area, .10), P90 = pctl5(Area, .90),
            P25 = pctl5(Area, .25), P75 = pctl5(Area, .75), .groups = "drop") %>%
  arrange(ROCModel)
addtxt("The MEANS Procedure", "Analysis Variable : Area",
       sprintf("%-10s %6s %8s %8s %8s %8s %8s %8s %8s %8s", "ROCModel","N Obs",
               "Mean","Std Dev","5th Pctl","95th Pctl","10th Pctl","90th Pctl",
               "25th Pctl","75th Pctl"))
for (i in seq_len(nrow(means)))
  addtxt(sprintf("%-10s %6d %8s %8s %8s %8s %8s %8s %8s %8s", means$ROCModel[i], means$NObs[i],
                 fmt(means$Mean[i],8,4), fmt(means$StdDev[i],8,4), fmt(means$P5[i],8,4),
                 fmt(means$P95[i],8,4), fmt(means$P10[i],8,4), fmt(means$P90[i],8,4),
                 fmt(means$P25[i],8,4), fmt(means$P75[i],8,4)))
addtxt("")
write.csv(means, file.path(out, "means.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## proc univariate -> WidePctls ; proc print
## ------------------------------------------------------------------
WidePctls <- ROCEst %>% group_by(ROCModel) %>%
  summarise(Mean = mean(Area), Std = sd(Area),
            P_2_5 = pctl5(Area, .025), P_97_5 = pctl5(Area, .975), .groups = "drop") %>%
  arrange(ROCModel)
addtxt(sprintf("%-10s %8s %10s %12s %12s", "ROCModel","BootMean","BootStdErr",
               "95% Lower CL","95% Upper CL"))
for (i in seq_len(nrow(WidePctls)))
  addtxt(sprintf("%-10s %8s %10s %12s %12s", WidePctls$ROCModel[i],
                 fmt(WidePctls$Mean[i],6,4), fmt(WidePctls$Std[i],6,4),
                 fmt(WidePctls$P_2_5[i],6,4), fmt(WidePctls$P_97_5[i],6,4)))
addtxt("")
write.csv(WidePctls, file.path(out, "widepctls.csv"), row.names = FALSE, na = "")

writeLines(txt, file.path(out, "P097.txt"))
## combined table body CSV
body <- bind_rows(
  means %>% mutate(Table = "Means") %>% rename(Std = StdDev),
  WidePctls %>% mutate(Table = "WidePctls"),
  ROCAssoc %>% mutate(Table = "ROCAssociation"))
write.csv(body, file.path(out, "P097.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## Figures
## ------------------------------------------------------------------
p1 <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(aes(y = after_stat(100 * count / tapply(count, PANEL, sum)[PANEL])),
                 bins = 25, fill = "#A6B8D8", colour = "grey30") +
  facet_wrap(~ROCModel, ncol = 1) +
  labs(y = "Percent", title = "Distribution of Area") + theme_bw()
ggsave(file.path(out, "histogram.png"), p1, width = 6.4, height = 4.8, dpi = 100)
write.csv(ROCEst, file.path(out, "histogram.csv"), row.names = FALSE, na = "")

p2 <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(aes(y = after_stat(100 * count / tapply(count, PANEL, sum)[PANEL])),
                 bins = 25, fill = "#A6B8D8", colour = "grey30") +
  facet_grid(ROCModel ~ .) +
  scale_x_continuous(breaks = seq(0.4, 1, 0.1)) +
  coord_cartesian(xlim = c(0.4, 1)) +
  labs(y = "Percent",
       title = "Bootstrap Distributions of Area Under the Curve for Two Models") +
  theme_bw()
ggsave(file.path(out, "sgpanel.png"), p2, width = 6.4, height = 4.81, dpi = 100)
write.csv(ROCEst, file.path(out, "sgpanel.csv"), row.names = FALSE, na = "")

message("P097 done")
```