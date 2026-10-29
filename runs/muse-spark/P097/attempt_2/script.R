# P097 Bootstrap ROC - R translation (attempt 2)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)
suppressPackageStartupMessages(library(ggplot2))

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x); z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-9)
  posneg * z / 10^digits
}
f3 <- function(x) sprintf("%.3f", round_half_up(x, 3))
f4 <- function(x) sprintf("%.4f", round_half_up(x, 4))
f1 <- function(x) sprintf("%.1f", round_half_up(x, 1))

cat("Building roc data...\n")
alb <- c(3.0,3.2,3.9,2.8, 3.2,0.9,2.5,1.6, 3.8,3.7,3.2,3.8, 4.1,3.6,4.3,3.6,
         2.3,4.2,4.0,3.5, 3.8,3.0,4.5,3.7, 3.1,4.1,4.3,4.3, 3.2,2.6,3.3,1.7,
         3.7,3.3,4.2,3.5, 2.9,2.1,2.8,4.0, 3.3,3.7,3.6)
tp  <- c(5.8,6.3,6.8,4.8, 5.8,4.0,5.7,5.6, 5.7,6.7,5.4,6.6, 6.6,5.7,7.0,6.7,
         4.4,7.6,6.6,5.8, 6.8,4.7,7.4,7.4, 6.6,8.2,7.0,6.5, 5.1,4.7,6.8,4.0,
         6.1,6.3,7.7,6.2, 5.7,4.8,6.2,7.0, 5.7,6.9,6.6)
tot_raw <- c(10,5,3,6, 3,5,8,5, 5,6,4,6, 5,5,4,4, 6,4,6,6, 7,8,5,5, 6,6,5,4,
             5,6,6,7, 5,7,6,5, 9,7,8,7, 6,5,5)
popind <- c(0,1,1,0, 1,0,0,1, 1,1,1,1, 1,1,1,0, 1,0,0,1, 1,0,1,1, 1,1,1,1,
            1,1,0,0, 1,1,1,1, 0,1,0,1, 1,1,1)
totscore <- 10 - tot_raw
roc <- data.frame(alb=alb, tp=tp, totscore=totscore, popind=popind)
n <- nrow(roc)
y01_init <- as.integer(roc$popind==0)
n1 <- sum(y01_init==1); n0 <- sum(y01_init==0)

cat("Fitting logistic model...\n")
fit <- glm(I(popind==0) ~ alb + tp + totscore, data=roc, family=binomial(link="logit"))
summ <- summary(fit)
est <- coef(fit); se <- summ$coefficients[,"Std. Error"]
wald_par <- (est/se)^2; p_par <- 1 - pchisq(wald_par, 1)
ll1 <- as.numeric(logLik(fit)); k1 <- length(est)
neg2ll1 <- -2*ll1; aic1 <- neg2ll1 + 2*k1; sc1 <- neg2ll1 + k1*log(n)
# intercept only
pbar <- n1/n
ll0 <- n1*log(pbar) + n0*log(1-pbar)
neg2ll0 <- -2*ll0; aic0 <- neg2ll0 + 2*1; sc0 <- neg2ll0 + 1*log(n)
# R-square
LRchi <- neg2ll0 - neg2ll1
Rsq <- 1 - exp(-LRchi/n)
maxRsq_denom <- 1 - exp(2*ll0/n)
maxRsq <- Rsq / maxRsq_denom
p_LR <- 1 - pchisq(LRchi, 3)
# Score test (null p0)
X2 <- cbind(roc$alb, roc$tp, roc$totscore)
w0 <- pbar*(1-pbar)
U2 <- t(X2) %*% (y01_init - pbar)  # 3x1
S22 <- w0 * (t(X2) %*% X2 - (t(X2) %*% matrix(1,n,1)) %*% (matrix(1,1,n) %*% X2)/n)
ScoreChi <- as.numeric(t(U2) %*% solve(S22) %*% U2)
p_Score <- 1 - pchisq(ScoreChi, 3)
# Wald global (slopes only)
b <- est[-1]; V <- vcov(fit)[-1,-1]
WaldChi <- as.numeric(t(b) %*% solve(V) %*% b)
p_Wald <- 1 - pchisq(WaldChi, 3)
# Odds ratios 90% CI
z90 <- qnorm(0.95)
OR <- exp(est[-1]); ORL <- exp(est[-1]-z90*se[-1]); ORU <- exp(est[-1]+z90*se[-1])
# Concordance
conc_stats <- function(y01, s) {
  pos <- s[y01==1]; neg <- s[y01==0]
  C <- 0; D <- 0; T <- 0
  for (a in pos) { C <- C + sum(a > neg); D <- D + sum(a < neg); T <- T + sum(a == neg) }
  pairs <- length(pos)*length(neg)
  list(C=C,D=D,T=T,pairs=pairs,pctC=100*C/pairs,pctD=100*D/pairs,pctT=100*T/pairs,
       somers=(C-D)/pairs, gamma=ifelse((C+D)>0,(C-D)/(C+D),NA),
       taua=(C-D)/(0.5*n*(n-1)), c=(C+0.5*T)/pairs)
}
LogiPred <- as.numeric(predict(fit, type="response"))
cs_init <- conc_stats(y01_init, LogiPred)

ExpertPred <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
                0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
                0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
                0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
                0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
                0.3,0.1,0.1)
Survival <- data.frame(popind=roc$popind, alb=roc$alb, tp=roc$tp,
                       totscore=roc$totscore, LogiPred=LogiPred, ExpertPred=ExpertPred)
y01 <- as.integer(Survival$popind==0)
auc_rank <- function(y, s) {
  r <- rank(s, ties.method="average"); n1l <- sum(y==1); n0l <- sum(y==0)
  (sum(r[y==1]) - n1l*(n1l+1)/2)/(n1l*n0l)
}
delong <- function(y, s) {
  pos <- s[y==1]; neg <- s[y==0]; m <- length(pos); nn <- length(neg)
  M <- outer(pos, neg, function(a,b) (a>b)+0.5*(a==b))
  auc <- mean(M); V10 <- rowMeans(M); V01 <- colMeans(M)
  S10 <- if (m>1) var(V10) else 0; S01 <- if (nn>1) var(V01) else 0
  list(auc=auc, se=sqrt(max(S10/m + S01/nn,0)))
}
dL <- delong(y01, Survival$LogiPred); dE <- delong(y01, Survival$ExpertPred)
rocAssoc <- data.frame(ROCModel=c("Expert","Logistic"),
  Area=c(dE$auc,dL$auc), StdErr=c(dE$se,dL$se),
  L90=c(dE$auc-z90*dE$se, dL$auc-z90*dL$se),
  U90=c(dE$auc+z90*dE$se, dL$auc+z90*dL$se))
cs_L <- conc_stats(y01, Survival$LogiPred); cs_E <- conc_stats(y01, Survival$ExpertPred)

x <- 0.73; xbar <- 0.8162; s <- 0.0801
z <- abs(xbar - x)/s; p <- 2*pnorm(-z)
Prob <- data.frame(x=x, xbar=xbar, s=s, z=z, p=p)

cat("Bootstrap 5000 resamples...\n")
NumSamples <- 5000; N <- nrow(Survival)
set.seed(12345)
sL <- Survival$LogiPred; sE <- Survival$ExpertPred
areas_L <- numeric(NumSamples); areas_E <- numeric(NumSamples)
for (b in 1:NumSamples) {
  idx <- sample.int(N, N, replace=TRUE); yb <- y01[idx]
  if (length(unique(yb)) < 2) { areas_L[b] <- NA_real_; areas_E[b] <- NA_real_ }
  else { areas_L[b] <- auc_rank(yb, sL[idx]); areas_E[b] <- auc_rank(yb, sE[idx]) }
}
ROCEst <- data.frame(Replicate=rep(seq_len(NumSamples),2),
  ROCModel=rep(c("Expert","Logistic"), each=NumSamples), Area=c(areas_E, areas_L))
qfun <- function(v, pr) as.numeric(quantile(v, probs=pr, na.rm=TRUE, type=2))
summ_list <- lapply(split(ROCEst$Area, ROCEst$ROCModel), function(v) {
  v <- v[!is.na(v)]
  c(N=length(v), Mean=mean(v), Std=sd(v), P5=qfun(v,0.05), P95=qfun(v,0.95),
    P10=qfun(v,0.10), P90=qfun(v,0.90), P25=qfun(v,0.25), P75=qfun(v,0.75),
    P2_5=qfun(v,0.025), P97_5=qfun(v,0.975))
})
Means <- data.frame(ROCModel=names(summ_list), N=sapply(summ_list,function(x)x[["N"]]),
  Mean=sapply(summ_list,function(x)x[["Mean"]]), Std=sapply(summ_list,function(x)x[["Std"]]),
  P5=sapply(summ_list,function(x)x[["P5"]]), P95=sapply(summ_list,function(x)x[["P95"]]),
  P10=sapply(summ_list,function(x)x[["P10"]]), P90=sapply(summ_list,function(x)x[["P90"]]),
  P25=sapply(summ_list,function(x)x[["P25"]]), P75=sapply(summ_list,function(x)x[["P75"]]),
  stringsAsFactors=FALSE)
WidePctls <- data.frame(ROCModel=names(summ_list),
  Mean=sapply(summ_list,function(x)x[["Mean"]]), Std=sapply(summ_list,function(x)x[["Std"]]),
  P_2_5=sapply(summ_list,function(x)x[["P2_5"]]), P_97_5=sapply(summ_list,function(x)x[["P97_5"]]),
  stringsAsFactors=FALSE)

cat("Writing datasets...\n")
write.csv(Survival, file.path(out,"Survival.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out,"ROCEst.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out,"histogram.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out,"sgpanel.csv"), row.names=FALSE, na="")
write.csv(Prob, file.path(out,"Prob.csv"), row.names=FALSE, na="")
write.csv(Means, file.path(out,"Means.csv"), row.names=FALSE, na="")
write.csv(WidePctls, file.path(out,"WidePctls.csv"), row.names=FALSE, na="")
write.csv(rocAssoc, file.path(out,"ROCAssociation.csv"), row.names=FALSE, na="")
P097_body <- merge(Means, WidePctls[,c("ROCModel","P_2_5","P_97_5")], by="ROCModel")
write.csv(P097_body, file.path(out,"P097.csv"), row.names=FALSE, na="")

# Text rendering with SAS formatting
pnames <- c("Intercept","alb","tp","totscore")
txtfile <- file.path(out,"P097.txt"); lstfile <- file.path(out,"P097.lst.txt")
sink(lstfile)
cat("P097 Bootstrap ROC\n")
cat("Discrimination, accuracy, and stability in binary classifiers\n\n")
cat("PROC LOGISTIC data=roc plots=roc(id=prob)\n")
cat("Model: popind(event='0') = alb tp totscore\n\n")
cat("Number of Observations Read 43\n")
cat("Number of Observations Used 43\n")
cat("Response Profile: Ordered Value 1 popind 1 Total Frequency 31\n")
cat("Response Profile: Ordered Value 2 popind 0 Total Frequency 12\n")
cat("Probability modeled is popind=0\n\n")
cat("Model Fit Statistics\n")
cat("Criterion          Intercept Only    Intercept and Covariates\n")
cat(sprintf("AIC                  %s               %s\n", f3(aic0), f3(aic1)))
cat(sprintf("SC                   %s               %s\n", f3(sc0), f3(sc1)))
cat(sprintf("-2 Log L             %s               %s\n", f3(neg2ll0), f3(neg2ll1)))
cat("\nR-Square and Max-rescaled R-Square\n")
cat(sprintf("R-Square %s  Max-rescaled R-Square %s\n", f4(Rsq), f4(maxRsq)))
cat("\nTesting Global Null Hypothesis: BETA=0\n")
cat("Test                 DF    Chi-Square    Pr > ChiSq\n")
cat(sprintf("Likelihood Ratio      3      %s        %s\n", f4(LRchi), f4(p_LR)))
cat(sprintf("Score                 3      %s        %s\n", f4(ScoreChi), f4(p_Score)))
cat(sprintf("Wald                  3      %s        %s\n", f4(WaldChi), f4(p_Wald)))
cat("\nType 3 Analysis of Effects\n")
cat("Effect      DF    Wald Chi-Square    Pr > ChiSq\n")
for (nm in c("alb","tp","totscore")) {
  i <- match(nm, pnames); cat(sprintf("%-8s     1       %s            %s\n", nm, f4(wald_par[i]), f4(p_par[i])))
}
cat("\nAnalysis of Maximum Likelihood Estimates\n")
cat("Parameter    DF    Estimate    StdErr    WaldChiSq    Pr > ChiSq\n")
for (i in seq_along(est)) {
  cat(sprintf("%-9s     %d    %s    %s      %s       %s\n", pnames[i], 1, f4(est[i]), f4(se[i]), f4(wald_par[i]), f4(p_par[i])))
}
cat("\nOdds Ratio Estimates (90% Wald Confidence Limits, alpha=0.1)\n")
cat("Effect     Point Estimate    90% Lower    90% Upper\n")
for (nm in c("alb","tp","totscore")) {
  cat(sprintf("%-8s      %s          %s        %s\n", nm, f3(OR[nm]), f3(ORL[nm]), f3(ORU[nm])))
}
cat("\nAssociation of Predicted Probabilities and Observed Responses\n")
cat(sprintf("Percent Concordant    %s  Somers D %s\n", f1(cs_init$pctC), f4(cs_init$somers)))
cat(sprintf("Percent Discordant    %s  Gamma    %s\n", f1(cs_init$pctD), f4(cs_init$gamma)))
cat(sprintf("Percent Tied          %s  Tau-a    %s\n", f1(cs_init$pctT), f4(cs_init$taua)))
cat(sprintf("Pairs            %d  c        %s\n", cs_init$pairs, f4(cs_init$c)))
cat(sprintf("Concordant %d Discordant %d Tied %d\n", cs_init$C, cs_init$D, cs_init$T))
cat("\nPROC LOGISTIC data=Survival alpha=0.1 nofit ROCOverlay ROCAssociation\n")
cat("ROC Association (Mann-Whitney, 90% Wald Confidence Limits)\n")
cat("ROCModel   Area    StdErr  90% Lower  90% Upper  SomersD  Gamma  Tau-a  c\n")
for (i in seq_len(nrow(rocAssoc))) {
  cs <- if (rocAssoc$ROCModel[i]=="Logistic") cs_L else cs_E
  cat(sprintf("%-8s  %s  %s    %s     %s   %s  %s  %s  %s\n",
    rocAssoc$ROCModel[i], f4(rocAssoc$Area[i]), f4(rocAssoc$StdErr[i]),
    f4(rocAssoc$L90[i]), f4(rocAssoc$U90[i]),
    f4(2*rocAssoc$Area[i]-1), f4(cs$gamma), f4(cs$taua), f4(rocAssoc$Area[i])))
}
cat("\nROC Contrast Estimation and Test (Logistic - Expert)\n")
diffA <- dL$auc - dE$auc; seD <- sqrt(dL$se^2 + dE$se^2)
cat(sprintf("Estimate %s StdErr %s 90%% Lower %s 90%% Upper %s\n", f4(diffA), f4(seD), f4(diffA-z90*seD), f4(diffA+z90*seD)))
chiC <- (diffA/seD)^2
cat(sprintf("ROC Contrast Chi-Square %s DF 1 Pr > ChiSq %s\n", f4(chiC), f4(1-pchisq(chiC,1))))
cat("\nData Prob  x xbar s z p\n")
cat(sprintf("x=%s xbar=%s s=%s z=%s p=%s\n", f4(x), f4(xbar), f4(s), f4(z), f4(p)))
cat(sprintf("x=%.2f xbar=%.4f s=%.4f z=%.4f p=%.4f\n", x, xbar, s, z, p))
cat("\nBootstrap Distributions of Area Under the Curve for Two Models\n")
cat("NumSamples 5000 seed 12345 samprate 1 method urs\n")
cat("PROC MEANS Mean Std P5 P95 P10 P90 P25 P75 ndec=4\n")
cat("ROCModel      N     Mean    Std     P5      P95     P10     P90     P25     P75\n")
for (i in seq_len(nrow(Means))) {
  cat(sprintf("%-8s %5d  %s %s  %s  %s  %s  %s  %s  %s\n",
    Means$ROCModel[i], Means$N[i], f4(Means$Mean[i]), f4(Means$Std[i]),
    f4(Means$P5[i]), f4(Means$P95[i]), f4(Means$P10[i]), f4(Means$P90[i]), f4(Means$P25[i]), f4(Means$P75[i])))
}
cat("\nPROC UNIVARIATE output WidePctls pctlpts=2.5 97.5 mean std\n")
cat("ROCModel  BootMean BootStdErr  95% Lower CL  95% Upper CL\n")
for (i in seq_len(nrow(WidePctls))) {
  cat(sprintf("%-8s  %s   %s     %s       %s\n",
    WidePctls$ROCModel[i], f4(WidePctls$Mean[i]), f4(WidePctls$Std[i]),
    f4(WidePctls$P_2_5[i]), f4(WidePctls$P_97_5[i])))
  cat(sprintf("%-8s  %s   %s     %s       %s (6.4 format)\n",
    WidePctls$ROCModel[i], sprintf("%6.4f",round_half_up(WidePctls$Mean[i],4)),
    sprintf("%6.4f",round_half_up(WidePctls$Std[i],4)),
    sprintf("%6.4f",round_half_up(WidePctls$P_2_5[i],4)),
    sprintf("%6.4f",round_half_up(WidePctls$P_97_5[i],4))))
}
cat("\nNote: Histograms of bootstrap Area by ROCModel in histogram.png and sgpanel.png\n")
sink()
file.copy(lstfile, txtfile, overwrite=TRUE)

cat("Creating figures...\n")
p_hist <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.02, fill="steelblue", color="white", boundary=0) +
  facet_wrap(~ ROCModel, ncol=1) +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models", x="Area", y="Frequency") +
  theme_bw() + theme(plot.title=element_text(size=11, face="bold", hjust=0.5))
ggsave(file.path(out,"histogram.png"), p_hist, width=6.4, height=4.81, dpi=100)
file.copy(file.path(out,"histogram.png"), file.path(out,"Histogram.png"), overwrite=TRUE)
p_sg <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.02, fill="steelblue", color="white", boundary=0) +
  facet_grid(ROCModel ~ ., switch="y") +
  scale_x_continuous(limits=c(0.4,1), breaks=seq(0.4,1,0.1)) +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models", x="Area", y="Frequency") +
  theme_bw() + theme(plot.title=element_text(size=11, face="bold", hjust=0.5),
    panel.grid.major=element_line(color="grey85"), panel.grid.minor=element_line(color="grey92"),
    strip.background=element_rect(fill="grey90"))
ggsave(file.path(out,"sgpanel.png"), p_sg, width=6.4, height=4.81, dpi=100)
file.copy(file.path(out,"sgpanel.png"), file.path(out,"SGPanel.png"), overwrite=TRUE)
cat("Done.\n")
