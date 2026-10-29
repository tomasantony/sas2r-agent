# P097 Bootstrap ROC - R translation
# Produces bootstrap AUC distributions, summaries, histograms

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(ggplot2))

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}
fmt4 <- function(x) sprintf("%.4f", round_half_up(x, 4))
fmt2 <- function(x) sprintf("%.2f", round_half_up(x, 2))

cat("Building roc data...\n")
# roc data: alb tp totscore popind, totscore = 10 - totscore
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

cat("Fitting logistic model...\n")
fit <- glm(I(popind==0) ~ alb + tp + totscore, data=roc, family=binomial(link="logit"))
LogiPred <- as.numeric(predict(fit, type="response"))

# Model fit stats for listing
ll <- as.numeric(logLik(fit))
k <- length(coef(fit))
nobs <- nrow(roc)
neg2ll <- -2*ll
aic <- neg2ll + 2*k
sc <- neg2ll + k*log(nobs)
summ <- summary(fit)
est <- coef(fit)
se <- summ$coefficients[,"Std. Error"]
wald <- (est/se)^2
pval <- 1 - pchisq(wald, 1)

# Concordance for initial model (event 0 vs 1)
conc_stats <- function(y01, s) {
  # y01: 1=event(0 popind), 0=nonevent
  pos <- s[y01==1]; neg <- s[y01==0]
  C <- 0; D <- 0; T <- 0
  for (a in pos) {
    C <- C + sum(a > neg)
    D <- D + sum(a < neg)
    T <- T + sum(a == neg)
  }
  pairs <- length(pos)*length(neg)
  list(C=C, D=D, T=T, pairs=pairs,
       pctC=100*C/pairs, pctD=100*D/pairs, pctT=100*T/pairs,
       somers=(C-D)/pairs, gamma=ifelse((C+D)>0,(C-D)/(C+D),NA),
       taua=(C-D)/(0.5*nobs*(nobs-1)), c=(C+0.5*T)/pairs)
}
y01_init <- as.integer(roc$popind==0)
cs_init <- conc_stats(y01_init, LogiPred)

ExpertPred <- c(0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
                0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
                0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
                0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
                0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
                0.3,0.1,0.1)

Survival <- data.frame(popind=roc$popind, alb=roc$alb, tp=roc$tp,
                       totscore=roc$totscore, LogiPred=LogiPred, ExpertPred=ExpertPred)

# AUC + DeLong SE
auc_rank <- function(y, s) {
  r <- rank(s, ties.method="average")
  n1 <- sum(y==1); n0 <- sum(y==0)
  if (n1==0 || n0==0) return(NA_real_)
  sumR <- sum(r[y==1])
  (sumR - n1*(n1+1)/2)/(n1*n0)
}
delong <- function(y, s) {
  pos <- s[y==1]; neg <- s[y==0]
  m <- length(pos); n <- length(neg)
  if (m==0 || n==0) return(list(auc=NA_real_, se=NA_real_))
  M <- outer(pos, neg, function(a,b) (a>b)+0.5*(a==b))
  auc <- mean(M)
  V10 <- rowMeans(M)
  V01 <- colMeans(M)
  S10 <- if (m>1) var(V10) else 0
  S01 <- if (n>1) var(V01) else 0
  v <- S10/m + S01/n
  list(auc=auc, se=sqrt(max(v,0)))
}

y01 <- as.integer(Survival$popind==0)
dL <- delong(y01, Survival$LogiPred)
dE <- delong(y01, Survival$ExpertPred)
z90 <- qnorm(0.95)  # alpha=0.1
rocAssoc <- data.frame(
  ROCModel=c("Expert","Logistic"),
  Area=c(dE$auc, dL$auc),
  StdErr=c(dE$se, dL$se),
  L90=c(dE$auc - z90*dE$se, dL$auc - z90*dL$se),
  U90=c(dE$auc + z90*dE$se, dL$auc + z90*dL$se)
)

# Prob data step
x <- 0.73; xbar <- 0.8162; s <- 0.0801
z <- abs(xbar - x)/s
p <- 2*pnorm(-z)
Prob <- data.frame(x=x, xbar=xbar, s=s, z=z, p=p)

cat("Bootstrap 5000 resamples...\n")
NumSamples <- 5000
N <- nrow(Survival)
set.seed(12345)
sL <- Survival$LogiPred
sE <- Survival$ExpertPred
areas_L <- numeric(NumSamples)
areas_E <- numeric(NumSamples)
for (b in 1:NumSamples) {
  idx <- sample.int(N, N, replace=TRUE)
  yb <- y01[idx]
  if (length(unique(yb)) < 2) {
    areas_L[b] <- NA_real_; areas_E[b] <- NA_real_
  } else {
    areas_L[b] <- auc_rank(yb, sL[idx])
    areas_E[b] <- auc_rank(yb, sE[idx])
  }
}

ROCEst <- data.frame(
  Replicate=rep(seq_len(NumSamples), 2),
  ROCModel=rep(c("Expert","Logistic"), each=NumSamples),
  Area=c(areas_E, areas_L)
)

# PROC MEANS summaries
qfun <- function(v, pr) as.numeric(quantile(v, probs=pr, na.rm=TRUE, type=2))
summ_list <- lapply(split(ROCEst$Area, ROCEst$ROCModel), function(v) {
  v <- v[!is.na(v)]
  c(N=length(v), Mean=mean(v), Std=sd(v),
    P5=qfun(v,0.05), P95=qfun(v,0.95), P10=qfun(v,0.10), P90=qfun(v,0.90),
    P25=qfun(v,0.25), P75=qfun(v,0.75),
    P2_5=qfun(v,0.025), P97_5=qfun(v,0.975))
})
Means <- data.frame(
  ROCModel=names(summ_list),
  N=sapply(summ_list, function(x) x[["N"]]),
  Mean=sapply(summ_list, function(x) x[["Mean"]]),
  Std=sapply(summ_list, function(x) x[["Std"]]),
  P5=sapply(summ_list, function(x) x[["P5"]]),
  P95=sapply(summ_list, function(x) x[["P95"]]),
  P10=sapply(summ_list, function(x) x[["P10"]]),
  P90=sapply(summ_list, function(x) x[["P90"]]),
  P25=sapply(summ_list, function(x) x[["P25"]]),
  P75=sapply(summ_list, function(x) x[["P75"]]),
  stringsAsFactors=FALSE
)
WidePctls <- data.frame(
  ROCModel=names(summ_list),
  Mean=sapply(summ_list, function(x) x[["Mean"]]),
  Std=sapply(summ_list, function(x) x[["Std"]]),
  P_2_5=sapply(summ_list, function(x) x[["P2_5"]]),
  P_97_5=sapply(summ_list, function(x) x[["P97_5"]]),
  stringsAsFactors=FALSE
)

cat("Writing datasets...\n")
write.csv(Survival, file.path(out, "Survival.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "ROCEst.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "histogram.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "sgpanel.csv"), row.names=FALSE, na="")
write.csv(Prob, file.path(out, "Prob.csv"), row.names=FALSE, na="")
write.csv(Means, file.path(out, "Means.csv"), row.names=FALSE, na="")
write.csv(WidePctls, file.path(out, "WidePctls.csv"), row.names=FALSE, na="")
write.csv(rocAssoc, file.path(out, "ROCAssociation.csv"), row.names=FALSE, na="")
# P097.csv as table body (bootstrap means + wide percentiles)
P097_body <- merge(Means, WidePctls[,c("ROCModel","P_2_5","P_97_5")], by="ROCModel")
write.csv(P097_body, file.path(out, "P097.csv"), row.names=FALSE, na="")

# Text rendering P097.lst -> P097.txt
txtfile <- file.path(out, "P097.txt")
lstfile <- file.path(out, "P097.lst.txt")
con <- file(txtfile, open="wt")
sink(lstfile)
cat("P097 Bootstrap ROC - Discrimination, accuracy, stability in binary classifiers\n")
cat("Logistic model: popind(event='0') = alb tp totscore\n\n")
cat("Model Fit Statistics\n")
cat(sprintf("  -2 Log L = %.4f   AIC = %.4f   SC = %.4f   N = %d\n", neg2ll, aic, sc, nobs))
cat("\nParameter Estimates (Wald)\n")
cat("Parameter   DF  Estimate  StdErr  WaldChiSq  p-value\n")
pnames <- c("Intercept","alb","tp","totscore")
for (i in seq_along(est)) {
  cat(sprintf("%-9s %2d  %8.4f  %7.4f  %9.4f  %7.4f\n", pnames[i], 1, est[i], se[i], wald[i], pval[i]))
}
cat("\nAssociation of Predicted Probabilities and Observed Responses\n")
cat(sprintf("  Percent Concordant = %.1f  Discordant = %.1f  Tied = %.1f  Pairs = %d\n",
            cs_init$pctC, cs_init$pctD, cs_init$pctT, cs_init$pairs))
cat(sprintf("  Somers D = %.4f  Gamma = %.4f  Tau-a = %.4f  c = %.4f\n",
            cs_init$somers, cs_init$gamma, cs_init$taua, cs_init$c))
cat("\nROC Association (original sample, alpha=0.1, 90% CI)\n")
cat("ROCModel   Area    StdErr  90% Lower  90% Upper\n")
for (i in seq_len(nrow(rocAssoc))) {
  cat(sprintf("%-8s  %6.4f  %6.4f  %8.4f  %8.4f\n", rocAssoc$ROCModel[i], rocAssoc$Area[i],
              rocAssoc$StdErr[i], rocAssoc$L90[i], rocAssoc$U90[i]))
}
cat("\nProb calculation: z = abs(xbar - x)/s ; p = 2*Phi(-z)\n")
cat(sprintf("  x=%.4f xbar=%.4f s=%.4f z=%.4f p=%.4f\n", x, xbar, s, z, p))
cat("\nBootstrap Distributions of Area (5000 resamples, seed=12345)\n")
cat("PROC MEANS Mean Std P5 P95 P10 P90 P25 P75 (ndec=4)\n")
cat("ROCModel      N     Mean    Std     P5      P95     P10     P90     P25     P75\n")
for (i in seq_len(nrow(Means))) {
  cat(sprintf("%-8s %5d  %6.4f %6.4f  %6.4f  %6.4f  %6.4f  %6.4f  %6.4f  %6.4f\n",
              Means$ROCModel[i], Means$N[i], Means$Mean[i], Means$Std[i],
              Means$P5[i], Means$P95[i], Means$P10[i], Means$P90[i], Means$P25[i], Means$P75[i]))
}
cat("\nWide Percentiles (PROC UNIVARIATE P2.5 P97.5 Mean Std)\n")
cat("ROCModel  BootMean BootStdErr  95% Lower CL  95% Upper CL\n")
for (i in seq_len(nrow(WidePctls))) {
  cat(sprintf("%-8s  %6.4f   %6.4f     %6.4f       %6.4f\n",
              WidePctls$ROCModel[i], WidePctls$Mean[i], WidePctls$Std[i],
              WidePctls$P_2_5[i], WidePctls$P_97_5[i]))
}
cat("\nNote: Histograms of bootstrap Area by ROCModel in histogram.png and sgpanel.png\n")
# close sink after duplicating to txt
out_lines <- NULL
sink()
# copy lst to txt (identical)
file.copy(lstfile, txtfile, overwrite=TRUE)

cat("Creating figures...\n")
# Histogram.png : univariate histograms by ROCModel
p_hist <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.02, fill="steelblue", color="white", boundary=0) +
  facet_wrap(~ ROCModel, ncol=1) +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models",
       x="Area", y="Frequency") +
  theme_bw() +
  theme(plot.title=element_text(size=11, face="bold", hjust=0.5))
ggsave(file.path(out, "histogram.png"), p_hist, width=6.4, height=4.81, dpi=100)
# also capitalised copy
try(file.copy(file.path(out,"histogram.png"), file.path(out,"Histogram.png"), overwrite=TRUE), silent=TRUE)

# SGPanel.png : rows=2 layout=rowlattice, colaxis 0.4 to 1 by 0.1 grid
p_sg <- ggplot(ROCEst, aes(x=Area)) +
  geom_histogram(binwidth=0.02, fill="steelblue", color="white", boundary=0) +
  facet_grid(ROCModel ~ ., switch="y") +
  scale_x_continuous(limits=c(0.4,1), breaks=seq(0.4,1,0.1)) +
  labs(title="Bootstrap Distributions of Area Under the Curve for Two Models",
       x="Area", y="Frequency") +
  theme_bw() +
  theme(plot.title=element_text(size=11, face="bold", hjust=0.5),
        panel.grid.major=element_line(color="grey85"),
        panel.grid.minor=element_line(color="grey92"),
        strip.background=element_rect(fill="grey90"))
ggsave(file.path(out, "sgpanel.png"), p_sg, width=6.4, height=4.81, dpi=100)
try(file.copy(file.path(out,"sgpanel.png"), file.path(out,"SGPanel.png"), overwrite=TRUE), silent=TRUE)

cat("Done.\n")
