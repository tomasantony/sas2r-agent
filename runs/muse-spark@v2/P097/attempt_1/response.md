```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

## SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  if (!is.finite(x)) return(x)
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5 + 1e-12) / m
}
fmt_n <- function(x, digits) {
  # format with fixed decimals using half-up rounding
  vapply(x, function(v) {
    if (is.na(v)) return("")
    r <- round_half_up(v, digits)
    formatC(r, format = "f", digits = digits, flag = "#")
  }, character(1))
}

cat("P097 Bootstrap ROC: building data\n")

## ---- data roc ----
# input alb tp totscore popind @@ ; totscore = 10 - totscore
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
stopifnot(length(raw_vals) %% 4 == 0)
n_roc <- length(raw_vals)/4
m <- matrix(raw_vals, ncol = 4, byrow = TRUE)
roc <- tibble(alb = m[,1], tp = m[,2], totscore = 10 - m[,3], popind = m[,4])
stopifnot(nrow(roc) == 43)

## ---- proc logistic data=roc ; model popind(event='0') = alb tp totscore ----
roc$popind0 <- as.integer(roc$popind == 0)
fit <- glm(popind0 ~ alb + tp + totscore, data = roc, family = binomial(link = "logit"))
LogiPred <- as.numeric(fitted(fit))
LogiOut <- roc %>% mutate(LogiPred = LogiPred)
# keep order as SAS merge (row order)
stopifnot(nrow(LogiOut) == 43)
stopifnot(all(!is.na(LogiOut$LogiPred)))

## ---- ExpertPred ----
ExpertPred_vals <- c(
0.95,0.2,0.05,0.3,0.1,0.6,0.8,0.5,
0.1,0.25,0.1,0.2,0.05,0.1,0.05,0.1,
0.4,0.1,0.2,0.25,0.4,0.7,0.1,0.1,
0.3,0.2,0.1,0.05,0.1,0.4,0.4,0.7,
0.2,0.4,0.1,0.1,0.9,0.7,0.8,0.25,
0.3,0.1,0.1
)
stopifnot(length(ExpertPred_vals) == 43)
ExpertPred <- tibble(ExpertPred = ExpertPred_vals)

## ---- Survival = merge LogiOut ExpertPred (row-wise, no BY) ----
Survival <- bind_cols(LogiOut, ExpertPred)
stopifnot(nrow(Survival) == 43)
stopifnot(all(c("popind","LogiPred","ExpertPred") %in% names(Survival)))

## ---- ROC AUC + DeLong SE (for ROCAssociation) ----
auc_only <- function(y, p) {
  # y 1=event (popind=0), 0=nonevent
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  auc <- (sum(r[y == 1]) - n1*(n1+1)/2) / (n1*n0)
  auc
}
roc_delong <- function(y, p, alpha = 0.1) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(list(area=NA_real_, se=NA_real_, lcl=NA_real_, ucl=NA_real_))
  auc <- auc_only(y, p)
  # DeLong components
  y1p <- p[y == 1]; y0p <- p[y == 0]
  # V10
  V10 <- vapply(y1p, function(a) mean((a > y0p) + 0.5*(a == y0p)), numeric(1))
  V01 <- vapply(y0p, function(b) mean((y1p > b) + 0.5*(y1p == b)), numeric(1))
  S10 <- if (n1 > 1) var(V10) else 0
  S01 <- if (n0 > 1) var(V01) else 0
  v <- S10/n1 + S01/n0
  se <- sqrt(max(v, 0))
  z <- qnorm(1 - alpha/2)
  lcl <- max(0, auc - z*se); ucl <- min(1, auc + z*se)
  list(area=auc, se=se, lcl=lcl, ucl=ucl, n1=n1, n0=n0)
}

y_evt <- as.integer(Survival$popind == 0)
res_logi <- roc_delong(y_evt, Survival$LogiPred, alpha = 0.1)
res_expr <- roc_delong(y_evt, Survival$ExpertPred, alpha = 0.1)

ROCAssociation <- tibble(
  ROCModel = c("Expert","Logistic"),
  Area = c(res_expr$area, res_logi$area),
  StdErr = c(res_expr$se, res_logi$se),
  Lower90 = c(res_expr$lcl, res_logi$lcl),
  Upper90 = c(res_expr$ucl, res_logi$ucl)
)

cat("Original AUC Expert=", ROCAssociation$Area[1], " Logistic=", ROCAssociation$Area[2], "\n")

## ---- data Prob ----
x <- 0.73; xbar <- 0.8162; s <- 0.0801
zval <- abs(xbar - x)/s
pval <- 2*pnorm(-zval)
Prob <- tibble(x=x, xbar=xbar, s=s, z=zval, p=pval)

## ---- Bootstrap: surveyselect urs samprate=1 reps=5000 seed=12345 ----
NumSamples <- 5000
N <- nrow(Survival)
set.seed(12345)
# replicate sampling with replacement, each sample size N
# compute AUCs
pop_vec <- Survival$popind
logi_vec <- Survival$LogiPred
expr_vec <- Survival$ExpertPred
areas_logi <- numeric(NumSamples)
areas_expr <- numeric(NumSamples)
for (b in seq_len(NumSamples)) {
  idx <- sample.int(N, N, replace = TRUE)
  yb <- as.integer(pop_vec[idx] == 0)
  areas_logi[b] <- auc_only(yb, logi_vec[idx])
  areas_expr[b] <- auc_only(yb, expr_vec[idx])
}
ROCEst <- tibble(
  Replicate = rep(seq_len(NumSamples), times = 2),
  ROCModel = rep(c("Expert","Logistic"), each = NumSamples),
  Area = c(areas_expr, areas_logi)
)
# SAS sorts character ordering uppercase before lowercase; keep as is
ROCEst <- ROCEst %>% arrange(ROCModel, Replicate)

## ---- PROC MEANS Mean Std P5 P95 P10 P90 P25 P75 ndec=4 ; class ROCModel ----
sas_pctl5 <- function(x, p) {
  # x numeric vector nonmissing, p in (0,1), PCTLDEF=5
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (n == 1) return(x[1])
  np <- n * p
  j <- floor(np + 1e-12)
  g <- np - j
  if (abs(g) < 1e-12) {
    # g==0
    if (j <= 0) return(x[1])
    if (j >= n) return(x[n])
    return((x[j] + x[j+1])/2)
  } else {
    k <- j + 1
    if (k < 1) k <- 1
    if (k > n) k <- n
    return(x[k])
  }
}
summ_one <- function(v) {
  tibble(
    N = sum(!is.na(v)),
    Mean = mean(v, na.rm = TRUE),
    Std = sd(v, na.rm = TRUE),
    P5 = sas_pctl5(v, 0.05),
    P10 = sas_pctl5(v, 0.10),
    P25 = sas_pctl5(v, 0.25),
    P75 = sas_pctl5(v, 0.75),
    P90 = sas_pctl5(v, 0.90),
    P95 = sas_pctl5(v, 0.95)
  )
}
means_by <- ROCEst %>% group_by(ROCModel) %>% summarise(
  N = sum(!is.na(Area)),
  Mean = mean(Area, na.rm=TRUE),
  Std = sd(Area, na.rm=TRUE),
  P5 = sas_pctl5(Area, 0.05),
  P95 = sas_pctl5(Area, 0.95),
  P10 = sas_pctl5(Area, 0.10),
  P90 = sas_pctl5(Area, 0.90),
  P25 = sas_pctl5(Area, 0.25),
  P75 = sas_pctl5(Area, 0.75),
  .groups="drop"
)
# overall (_TYPE_=0) for MEANS with CLASS
overall <- summ_one(ROCEst$Area) %>% mutate(ROCModel="Overall") %>%
  select(ROCModel, N, Mean, Std, P5, P95, P10, P90, P25, P75)
means_summary <- bind_rows(means_by %>% select(ROCModel,N,Mean,Std,P5,P95,P10,P90,P25,P75), overall)

## ---- PROC UNIVARIATE output WidePctls pctlpts 2.5 97.5 mean std ----
wide_by <- ROCEst %>% group_by(ROCModel) %>% summarise(
  N = sum(!is.na(Area)),
  Mean = mean(Area, na.rm=TRUE),
  Std = sd(Area, na.rm=TRUE),
  P_2_5 = sas_pctl5(Area, 0.025),
  P_97_5 = sas_pctl5(Area, 0.975),
  .groups="drop"
)
WidePctls <- wide_by
# univariate moments for listing completeness
univ_moments <- ROCEst %>% group_by(ROCModel) %>% summarise(
  N = sum(!is.na(Area)),
  Mean = mean(Area),
  StdDev = sd(Area),
  Variance = var(Area),
  Skewness = { m<-mean(Area); sd3<-sd(Area); mean((Area-m)^3)/ (sd3^3) },
  Kurtosis = { m<-mean(Area); sd4<-sd(Area); mean((Area-m)^4)/(sd4^4) - 3 },
  USS = sum(Area^2),
  CSS = sum((Area-mean(Area))^2),
  CV = 100*sd(Area)/mean(Area),
  StdMean = sd(Area)/sqrt(sum(!is.na(Area))),
  Min = min(Area), Max = max(Area), Range = max(Area)-min(Area),
  Median = sas_pctl5(Area, 0.50),
  Mode = NA_real_,
  .groups="drop"
)

## ---- logistic model details for listing ----
sm <- summary(fit)
coefs <- as.data.frame(sm$coefficients)
# Wald chi-square = (Estimate/SE)^2
param_tbl <- tibble(
  Parameter = rownames(coefs),
  Estimate = coefs[,1],
  StdErr = coefs[,2],
  WaldChiSq = (coefs[,1]/coefs[,2])^2,
  PValue = coefs[,4]
)
fit_stats <- tibble(
  Criterion = c("-2 Log L","AIC","SC"),
  InterceptOnly = c(sm$null.deviance, sm$null.deviance+2*1, sm$null.deviance+log(N)*1),
  InterceptAndCov = c(sm$deviance, sm$aic, sm$deviance+log(N)*length(coef(fit)))
)

cat("Bootstrap done\n")

## ---- write CSV datasets ----
write.csv(roc, file.path(out, "roc.csv"), row.names=FALSE, na="")
write.csv(Survival, file.path(out, "survival.csv"), row.names=FALSE, na="")
write.csv(Prob, file.path(out, "prob.csv"), row.names=FALSE, na="")
write.csv(ROCEst, file.path(out, "rocest.csv"), row.names=FALSE, na="")
write.csv(WidePctls, file.path(out, "widepctls.csv"), row.names=FALSE, na="")
write.csv(ROCAssociation, file.path(out, "rocassociation.csv"), row.names=FALSE, na="")
write.csv(means_summary, file.path(out, "means_summary.csv"), row.names=FALSE, na="")
# P097.csv : table body combining key tables (means + wide + prob + rocassoc)
p097_body <- means_summary
write.csv(p097_body, file.path(out, "P097.csv"), row.names=FALSE, na="")

## ---- text rendering P097.lst -> P097.txt ----
# Build comprehensive listing mimicking SAS lst
lines <- c()
add <- function(...) lines <<- c(lines, paste0(...))

add("Discrimination, accuracy, and stability in binary classifiers")
add("Bootstrap ROC Analysis  (P097_BootstrapROC)")
add(strrep("-", 78))
add("")
add("PROC LOGISTIC data=roc : model popind(event='0') = alb tp totscore")
add("")
add("Model Fit Statistics")
add(sprintf("%-15s %15s %15s", "Criterion", "Intercept Only", "Intercept+Cov"))
for (i in seq_len(nrow(fit_stats))) {
  add(sprintf("%-15s %15s %15s",
    fit_stats$Criterion[i],
    fmt_n(fit_stats$InterceptOnly[i], 4),
    fmt_n(fit_stats$InterceptAndCov[i], 4)))
}
add("")
add("Parameter Estimates")
add(sprintf("%-12s %10s %10s %12s %10s", "Parameter", "Estimate", "StdErr", "WaldChiSq", "PValue"))
for (i in seq_len(nrow(param_tbl))) {
  add(sprintf("%-12s %10s %10s %12s %10s",
    param_tbl$Parameter[i],
    fmt_n(param_tbl$Estimate[i], 4),
    fmt_n(param_tbl$StdErr[i], 4),
    fmt_n(param_tbl$WaldChiSq[i], 4),
    fmt_n(param_tbl$PValue[i], 4)))
}
add("")
add("PROC LOGISTIC data=Survival alpha=0.1 : ROCAssociation (nofit, event='0')")
add(sprintf("%-10s %10s %10s %10s %10s", "ROCModel", "Area", "StdErr", "Lower90", "Upper90"))
for (i in seq_len(nrow(ROCAssociation))) {
  add(sprintf("%-10s %10s %10s %10s %10s",
    ROCAssociation$ROCModel[i],
    fmt_n(ROCAssociation$Area[i], 4),
    fmt_n(ROCAssociation$StdErr[i], 4),
    fmt_n(ROCAssociation$Lower90[i], 4),
    fmt_n(ROCAssociation$Upper90[i], 4)))
}
add("")
add("data Prob;  z = abs(xbar-x)/s;  p = 2*cdf('Normal',-z);")
add(sprintf("%-8s %-8s %-8s %-8s %-8s", "x", "xbar", "s", "z", "p"))
add(sprintf("%-8s %-8s %-8s %-8s %-8s",
  fmt_n(Prob$x, 4), fmt_n(Prob$xbar, 4), fmt_n(Prob$s, 4),
  fmt_n(Prob$z, 4), fmt_n(Prob$p, 4)))
add("")
add("Bootstrap: PROC SURVEYSELECT urs samprate=1 reps=5000 seed=12345  N=43")
add(paste0("Number of bootstrap replicates = ", NumSamples))
add(paste0("Bootstrap observations (ROCEst rows) = ", nrow(ROCEst)))
add("")
add("PROC MEANS data=ROCEst Mean Std P5 P95 P10 P90 P25 P75 ndec=4; class ROCModel; var Area;")
add(sprintf("%-10s %6s %10s %10s %10s %10s %10s %10s %10s %10s",
  "ROCModel","N","Mean","Std","P5","P95","P10","P90","P25","P75"))
for (i in seq_len(nrow(means_summary))) {
  r <- means_summary[i,]
  add(sprintf("%-10s %6d %10s %10s %10s %10s %10s %10s %10s %10s",
    r$ROCModel, r$N,
    fmt_n(r$Mean,4), fmt_n(r$Std,4),
    fmt_n(r$P5,4), fmt_n(r$P95,4),
    fmt_n(r$P10,4), fmt_n(r$P90,4),
    fmt_n(r$P25,4), fmt_n(r$P75,4)))
}
add("")
add("PROC UNIVARIATE data=ROCEst noprint; class ROCModel; var Area; histogram Area;")
add("Moments")
add(sprintf("%-10s %6s %10s %10s %12s %10s %10s %12s %12s %10s %10s %10s %10s %10s",
  "ROCModel","N","Mean","StdDev","Variance","Skewness","Kurtosis","USS","CSS","CV","StdMean","Min","Max","Median"))
for (i in seq_len(nrow(univ_moments))) {
  r <- univ_moments[i,]
  add(sprintf("%-10s %6d %10s %10s %12s %10s %10s %12s %12s %10s %10s %10s %10s %10s",
    r$ROCModel, r$N,
    fmt_n(r$Mean,4), fmt_n(r$StdDev,4), fmt_n(r$Variance,4),
    fmt_n(r$Skewness,4), fmt_n(r$Kurtosis,4),
    fmt_n(r$USS,4), fmt_n(r$CSS,4),
    fmt_n(r$CV,4), fmt_n(r$StdMean,4),
    fmt_n(r$Min,4), fmt_n(r$Max,4), fmt_n(r$Median,4)))
}
add("")
add("PROC PRINT data=WidePctls noobs label;  (BootMean BootStdErr 95% Lower CL 95% Upper CL)")
add(sprintf("%-10s %10s %10s %12s %12s", "ROCModel", "BootMean", "BootStdErr", "95% Lower CL", "95% Upper CL"))
for (i in seq_len(nrow(WidePctls))) {
  add(sprintf("%-10s %10s %10s %12s %12s",
    WidePctls$ROCModel[i],
    fmt_n(WidePctls$Mean[i],4), fmt_n(WidePctls$Std[i],4),
    fmt_n(WidePctls$P_2_5[i],4), fmt_n(WidePctls$P_97_5[i],4)))
}
add("")
add("Title: Bootstrap Distributions of Area Under the Curve for Two Models")
add("")

writeLines(lines, file.path(out, "P097.txt"))
# also write .lst-named copies for robustness
writeLines(lines, file.path(out, "P097.lst.txt"))
# CSV of table body already written as P097.csv; also copy
write.csv(p097_body, file.path(out, "P097.lst.csv"), row.names=FALSE, na="")

## ---- figures ----
# Histogram.png : PROC UNIVARIATE histogram Area
# Use overlaid histograms with transparency
p_hist <- ggplot(ROCEst, aes(x = Area, fill = ROCModel)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, alpha = 0.5, position = "identity", color = "black", linewidth = 0.2) +
  labs(title = "Bootstrap Distributions of Area Under the Curve for Two Models",
       subtitle = "Histogram of Area (PROC UNIVARIATE)",
       x = "Area", y = "Frequency", fill = "ROC Model") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))
ggsave(file.path(out, "histogram.png"), p_hist, width = 640/96, height = 481/96, dpi = 96)
# CSV of plotted data: binned counts per model + raw
# Provide raw plotted data
write.csv(ROCEst, file.path(out, "histogram.csv"), row.names=FALSE, na="")

# SGPanel.png : panelby ROCModel rows=2 layout=rowlattice, colaxis 0.4 to 1 by 0.1, grid
p_panel <- ggplot(ROCEst, aes(x = Area)) +
  geom_histogram(bins = 30, fill = "#4F81BD", color = "black", linewidth = 0.2) +
  facet_wrap(~ ROCModel, ncol = 1, nrow = 2) +
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.1), limits = c(0.4, 1)) +
  labs(title = "Bootstrap Distributions of Area Under the Curve for Two Models",
       x = "Area", y = "Frequency") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_line(color = "grey92"),
        strip.text = element_text(face = "bold"))
ggsave(file.path(out, "sgpanel.png"), p_panel, width = 640/96, height = 481/96, dpi = 96)
write.csv(ROCEst, file.path(out, "sgpanel.csv"), row.names=FALSE, na="")

cat("P097 done\n")
```