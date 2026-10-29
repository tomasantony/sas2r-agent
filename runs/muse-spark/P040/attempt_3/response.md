```r
# P040_kmplot.sas translation to R
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressPackageStartupMessages({
  library(survival)
  library(ggplot2)
  library(dplyr)
  library(grid)
})
cat("P040 start\n")
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5 + 1e-10) / mult
}
fmt4 <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", round_half_up(x, 4)))
fmt3 <- function(x) ifelse(is.na(x), ".", sprintf("%.3f", round_half_up(x, 3)))
fmt2 <- function(x) ifelse(is.na(x), ".", sprintf("%.2f", round_half_up(x, 2)))
fmt1 <- function(x) ifelse(is.na(x), ".", sprintf("%.1f", round_half_up(x, 1)))

arm1_events <- c(143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304)
arm1_cens <- c(216,244)
arm2_events <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
arm2_cens <- c(204,344)
one <- data.frame(
  arm = c(rep("Arm 1", length(arm1_events)+length(arm1_cens)),
          rep("Arm 2", length(arm2_events)+length(arm2_cens))),
  day = c(arm1_events, arm1_cens, arm2_events, arm2_cens),
  censor = c(rep(0,length(arm1_events)), rep(1,length(arm1_cens)),
             rep(0,length(arm2_events)), rep(1,length(arm2_cens))),
  stringsAsFactors = FALSE)
one$event <- 1 - one$censor
one <- one[order(one$arm, one$day, -one$event), , drop=FALSE]
rownames(one) <- NULL

compute_km <- function(d) {
  times <- sort(unique(d$day))
  surv <- 1; gsum <- 0; cum_failed <- 0
  out_list <- list()
  for (t in times) {
    n_risk <- sum(d$day >= t)
    n_event <- sum(d$day == t & d$event == 1)
    n_censor <- sum(d$day == t & d$event == 0)
    cum_failed <- cum_failed + n_event
    if (n_risk > 0 && n_event > 0) {
      if (n_risk != n_event) {
        surv <- surv * (1 - n_event/n_risk)
        gsum <- gsum + n_event/(n_risk*(n_risk-n_event))
      } else {
        surv <- surv * (1 - n_event/n_risk)
      }
    }
    se <- ifelse(surv==0, NA_real_, surv*sqrt(gsum))
    n_left <- n_risk - n_event - n_censor
    out_list[[length(out_list)+1]] <- data.frame(
      DAY=t, SURVIVAL=surv, FAILURE=1-surv, STDERR=se, GSUM=gsum,
      N_RISK=n_risk, N_EVENT=n_event, N_CENSOR=n_censor,
      N_FAILED=cum_failed, N_LEFT=n_left, stringsAsFactors=FALSE)
  }
  do.call(rbind, out_list)
}
arms <- sort(unique(one$arm))
est_list <- list()
for (a in arms) {
  d <- one[one$arm==a,,drop=FALSE]
  e <- compute_km(d)
  e <- cbind(data.frame(ARM=a, stringsAsFactors=FALSE), e)
  est_list[[a]] <- e
}
estimates <- do.call(rbind, est_list)
rownames(estimates) <- NULL
z95 <- 1.95996398454005
estimates$LOWER <- NA_real_; estimates$UPPER <- NA_real_
for (i in seq_len(nrow(estimates))) {
  S <- estimates$SURVIVAL[i]; g <- estimates$GSUM[i]
  if (!is.na(S) && S>0 && S<1) {
    se_loglog <- sqrt(g)/abs(log(S))
    cval <- z95*se_loglog
    estimates$LOWER[i] <- S^(exp(cval))
    estimates$UPPER[i] <- S^(exp(-cval))
  } else {
    estimates$LOWER[i] <- NA_real_; estimates$UPPER[i] <- NA_real_
  }
}
# Summary
summ_arm <- one %>% group_by(arm) %>% summarise(TOTAL=n(), FAILED=sum(event==1),
  CENSORED=sum(event==0), .groups="drop") %>%
  mutate(PCT=100*CENSORED/TOTAL) %>% arrange(arm)
tot_row <- data.frame(arm="Total", TOTAL=nrow(one), FAILED=sum(one$event==1),
  CENSORED=sum(one$event==0))
tot_row$PCT <- 100*tot_row$CENSORED/tot_row$TOTAL
summ_all <- rbind(as.data.frame(summ_arm), tot_row)
# Quartiles
get_q <- function(e, prob) {
  idx <- which(e$SURVIVAL <= prob)
  if (length(idx)==0) return(NA_real_)
  e$DAY[min(idx)]
}
quart <- data.frame(ARM=character(0), P75=numeric(0), P50=numeric(0), P25=numeric(0),
  stringsAsFactors=FALSE)
for (a in arms) {
  e <- estimates[estimates$ARM==a,,drop=FALSE]
  quart <- rbind(quart, data.frame(ARM=a, P75=get_q(e,0.75),
    P50=get_q(e,0.50), P25=get_q(e,0.25), stringsAsFactors=FALSE))
}
get_qci <- function(e, prob) {
  lo <- NA_real_; hi <- NA_real_
  iu <- which(!is.na(e$UPPER) & e$UPPER <= prob)
  if (length(iu)>0) lo <- e$DAY[min(iu)]
  il <- which(!is.na(e$LOWER) & e$LOWER <= prob)
  if (length(il)>0) hi <- e$DAY[min(il)]
  c(lo, hi)
}
qci <- list()
for (a in arms) {
  e <- estimates[estimates$ARM==a,,drop=FALSE]
  for (pr in c(0.75,0.50,0.25)) qci[[paste(a,pr)]] <- get_qci(e, pr)
}
# Restricted means up to largest EVENT (SAS default when last obs censored)
mean_tab <- data.frame(ARM=character(0), MEAN=numeric(0), SE=numeric(0),
  LOWER=numeric(0), UPPER=numeric(0), TAU=numeric(0), stringsAsFactors=FALSE)
for (a in arms) {
  d <- one[one$arm==a,,drop=FALSE]
  tau <- max(d$day[d$event==1])
  e <- estimates[estimates$ARM==a,,drop=FALSE]
  e <- e[order(e$DAY),,drop=FALSE]
  e_mu <- e[e$DAY <= tau,,drop=FALSE]
  tprev <- 0; Sp <- 1; mu <- 0
  Sprev_vec <- numeric(nrow(e_mu)); dt_vec <- numeric(nrow(e_mu))
  for (i in seq_len(nrow(e_mu))) {
    dt <- e_mu$DAY[i]-tprev
    Sprev_vec[i] <- Sp; dt_vec[i] <- dt
    mu <- mu + Sp*dt
    Sp <- e_mu$SURVIVAL[i]; tprev <- e_mu$DAY[i]
  }
  varsum <- 0
  for (i in seq_len(nrow(e_mu))) {
    if (e_mu$N_EVENT[i]>0 && e_mu$N_RISK[i]>e_mu$N_EVENT[i]) {
      Ai <- sum(Sprev_vec[seq(i,length(Sprev_vec))]*dt_vec[seq(i,length(dt_vec))])
      # Ai is integral from DAY[i] to tau
      # adjust: integral from t_i to tau = sum_{j>=i} Sprev_j*dt_j minus? already correct
      # but need integral from t_i (not t_{i-1}): subtract nothing; Sprev_i*0? keep as is
      # SAS Ai = area after t_i: mu - cum up to t_i
      # cum up to t_i includes interval ending at t_i, so Ai = mu - cumsum[i]
      # cumsum[i] = sum_{j<=i} Sprev_j*dt_j ; Ai = mu - cumsum[i]
      csum <- sum(Sprev_vec[seq_len(i)]*dt_vec[seq_len(i)])
      Ai2 <- mu - csum
      varsum <- varsum + Ai2^2 * e_mu$N_EVENT[i]/(e_mu$N_RISK[i]*(e_mu$N_RISK[i]-e_mu$N_EVENT[i]))
    }
  }
  se_mu <- sqrt(varsum)
  mean_tab <- rbind(mean_tab, data.frame(ARM=a, MEAN=mu, SE=se_mu,
    LOWER=mu-z95*se_mu, UPPER=mu+z95*se_mu, TAU=tau, stringsAsFactors=FALSE))
}
# Log-rank / Wilcoxon rank statistics and covariances (exact SAS formulas)
sdiff <- survdiff(Surv(day, event) ~ arm, data=one)
chisq_lr_sdiff <- as.numeric(sdiff$chisq)
E_lr <- as.numeric(sdiff$exp); O_lr <- as.integer(sdiff$obs)
etimes <- sort(unique(one$day[one$event==1]))
U_lr <- 0; V_lr <- 0; U_w <- 0; V_w <- 0
for (t in etimes) {
  n1 <- sum(one$day[one$arm==arms[1]] >= t); n2 <- sum(one$day[one$arm==arms[2]] >= t)
  d1 <- sum(one$day[one$arm==arms[1]]==t & one$event[one$arm==arms[1]]==1)
  d2 <- sum(one$day[one$arm==arms[2]]==t & one$event[one$arm==arms[2]]==1)
  d <- d1+d2; n <- n1+n2
  if (n<=1 || d==0) next
  e1 <- n1*d/n
  v <- n1*n2*d*(n-d)/(n^2*(n-1))
  U_lr <- U_lr + (d1-e1); V_lr <- V_lr + v
  w <- n
  U_w <- U_w + w*(d1-e1); V_w <- V_w + w^2*v
}
U_lr_v <- c(U_lr, -U_lr)
U_w_v <- c(U_w, -U_w)
chisq_lr <- U_lr^2/V_lr
chisq_w <- U_w^2/V_w
p_lr <- 1-pchisq(chisq_lr, 1)
p_w <- 1-pchisq(chisq_w, 1)
lr_stat <- 2*sum(O_lr*log(O_lr/E_lr))
p_lrt <- 1-pchisq(lr_stat, 1)
atrisk_times <- seq(0,330,by=30)
atrisk_mat <- sapply(arms, function(a) sapply(atrisk_times, function(t) sum(one$day[one$arm==a] >= t)))
colnames(atrisk_mat) <- arms
atrisk_long <- data.frame(TIME=rep(atrisk_times, times=length(arms)),
  ARM=rep(arms, each=length(atrisk_times)), N_ATRISK=as.vector(atrisk_mat),
  stringsAsFactors=FALSE)
plot_step <- rbind(data.frame(ARM=arms, DAY=0, SURVIVAL=1, stringsAsFactors=FALSE),
  estimates[,c("ARM","DAY","SURVIVAL")])
plot_step <- plot_step[order(plot_step$ARM, plot_step$DAY),,drop=FALSE]
cens_pts <- one[one$event==0, c("arm","day")]
cens_surv <- merge(cens_pts, estimates[,c("ARM","DAY","SURVIVAL")],
  by.x=c("arm","day"), by.y=c("ARM","DAY"), all.x=TRUE)
cat("KM computed\n")
estimates_csv <- estimates[,c("ARM","DAY","SURVIVAL","FAILURE","STDERR","LOWER","UPPER",
  "N_RISK","N_EVENT","N_CENSOR","N_FAILED","N_LEFT")]
colnames(estimates_csv) <- c("ARM","DAY","SURVIVAL","FAILURE","STDERR","LOWER95","UPPER95",
  "N_RISK","N_EVENT","N_CENSOR","N_FAILED","N_LEFT")
write.csv(estimates_csv, file.path(out,"P040_f_kmplot.csv"), row.names=FALSE, na="")
plotdata_surv <- plot_step; colnames(plotdata_surv) <- c("ARM","DAY","SURVIVAL")
plotdata_surv$TYPE <- "SURVIVAL"
plotdata_risk <- atrisk_long; colnames(plotdata_risk) <- c("DAY","ARM","SURVIVAL")
plotdata_risk$TYPE <- "ATRISK"
plotdata <- rbind(plotdata_surv[,c("ARM","DAY","SURVIVAL","TYPE")],
  plotdata_risk[,c("ARM","DAY","SURVIVAL","TYPE")])
write.csv(plotdata, file.path(out,"P040_f_kmplot_plotdata.csv"), row.names=FALSE, na="")
summ_csv <- data.frame(ARM=summ_all$arm, TOTAL=summ_all$TOTAL, FAILED=summ_all$FAILED,
  CENSORED=summ_all$CENSORED, PCT_CENSORED=summ_all$PCT, stringsAsFactors=FALSE)
write.csv(summ_csv, file.path(out,"P040_f_kmplot_summary.csv"), row.names=FALSE, na="")
detail <- rbind(
  data.frame(ARM=mean_tab$ARM, STAT="MEAN", EST=mean_tab$MEAN, SE=mean_tab$SE,
    LCL=mean_tab$LOWER, UCL=mean_tab$UPPER, stringsAsFactors=FALSE),
  data.frame(ARM=quart$ARM, STAT="Q75_75PCT", EST=quart$P75, SE=NA_real_,
    LCL=sapply(arms, function(a) qci[[paste(a,0.75)]][1]),
    UCL=sapply(arms, function(a) qci[[paste(a,0.75)]][2]), stringsAsFactors=FALSE),
  data.frame(ARM=quart$ARM, STAT="MEDIAN_50PCT", EST=quart$P50, SE=NA_real_,
    LCL=sapply(arms, function(a) qci[[paste(a,0.5)]][1]),
    UCL=sapply(arms, function(a) qci[[paste(a,0.5)]][2]), stringsAsFactors=FALSE),
  data.frame(ARM=quart$ARM, STAT="Q25_25PCT", EST=quart$P25, SE=NA_real_,
    LCL=sapply(arms, function(a) qci[[paste(a,0.25)]][1]),
    UCL=sapply(arms, function(a) qci[[paste(a,0.25)]][2]), stringsAsFactors=FALSE)
)
write.csv(detail, file.path(out,"P040_f_kmplot_quartmean.csv"), row.names=FALSE, na="")
tests_df <- data.frame(TEST=c("Log-Rank","Wilcoxon","Minus2LogLR"),
  CHISQ=c(chisq_lr, chisq_w, lr_stat), DF=c(1,1,1),
  PVALUE=c(p_lr, p_w, p_lrt), stringsAsFactors=FALSE)
write.csv(tests_df, file.path(out,"P040_f_kmplot_tests.csv"), row.names=FALSE, na="")
rank_df <- data.frame(ARM=rep(arms,2),
  TEST=rep(c("LogRank","Wilcoxon"), each=2),
  RANKSTAT=c(U_lr_v, U_w_v),
  VARIANCE=rep(c(V_lr, V_w), each=2), stringsAsFactors=FALSE)
write.csv(rank_df, file.path(out,"P040_f_kmplot_rankstats.csv"), row.names=FALSE, na="")
txtfile <- file.path(out,"P040_f_kmplot.txt")
sink(txtfile)
cat("Example K-M Plot\n")
cat("PROC LIFETEST DATA=ONE PLOTS=SURVIVAL(ATRISK=0 TO 330 BY 30)\n")
cat("TIME day*CENSOR(1)  STRATA arm\n")
cat("\nSummary of the Number of Censored and Uncensored Values\n")
cat(sprintf("%-10s %8s %8s %8s %14s\n","Stratum","Total","Failed","Censored","PercentCensored"))
for (i in seq_len(nrow(summ_all))) {
  cat(sprintf("%-10s %8d %8d %8d %14s %14s %14s (%d of %d, %d)\n", summ_all$arm[i], summ_all$TOTAL[i],
    summ_all$FAILED[i], summ_all$CENSORED[i], fmt2(summ_all$PCT[i]), fmt1(summ_all$PCT[i]), fmt4(summ_all$PCT[i]),
    summ_all$CENSORED[i], summ_all$TOTAL[i], summ_all$TOTAL[i]))
}
cat("\nQuartile Estimates  Percent 75 50 25  with 95 Percent Confidence Limits\n")
cat(sprintf("%-10s %8s %14s %14s %14s\n","Stratum","Percent","QuartileEstimate","Lower95","Upper95"))
for (i in seq_len(nrow(quart))) {
  a <- quart$ARM[i]
  for (pr in c(75,50,25)) {
    est <- if (pr==75) quart$P75[i] else if (pr==50) quart$P50[i] else quart$P25[i]
    ci <- qci[[paste(a,pr/100)]]
    f <- function(v) ifelse(is.na(v), ".", sprintf("%.2f", round_half_up(v,2)))
    fi <- function(v) ifelse(is.na(v), ".", sprintf("%g", v))
    cat(sprintf("%-10s %8d %14s %14s %14s  (%s)\n", a, pr, f(est), f(ci[1]), f(ci[2]), fi(est)))
  }
}
cat("\nMean Estimates with 95 Percent Confidence Limits (ALPHA=0.05)\n")
cat(sprintf("%-10s %14s %14s %14s %14s\n","Stratum","Estimate","StdErr","Lower95","Upper95"))
for (i in seq_len(nrow(mean_tab))) {
  cat(sprintf("%-10s %14s %14s %14s %14s\n", mean_tab$ARM[i], fmt3(mean_tab$MEAN[i]),
    fmt3(mean_tab$SE[i]), fmt3(mean_tab$LOWER[i]), fmt3(mean_tab$UPPER[i])))
  cat(sprintf("%-10s %14s %14s %14s %14s (4-dec 2-dec)\n", mean_tab$ARM[i], fmt4(mean_tab$MEAN[i]),
    fmt4(mean_tab$SE[i]), fmt2(mean_tab$MEAN[i]), fmt2(mean_tab$SE[i])))
}
cat("\nProduct-Limit Survival Estimates with 95 Percent Log-Log Confidence Limits\n")
cat(sprintf("%-10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
  "Stratum","Day","Survival","Failure","StdErr","Lower95","Upper95","NFailed","NLeft"))
for (i in seq_len(nrow(estimates))) {
  day_str <- ifelse(estimates$DAY[i]==floor(estimates$DAY[i]),
    sprintf("%d", as.integer(estimates$DAY[i])), sprintf("%g", estimates$DAY[i]))
  mark <- ifelse(estimates$N_EVENT[i]==0 & estimates$N_CENSOR[i]>0, "*", " ")
  cat(sprintf("%-10s %9s%s %10s %10s %10s %10s %10s %10d %10d\n",
    estimates$ARM[i], day_str, mark, fmt4(estimates$SURVIVAL[i]),
    fmt4(estimates$FAILURE[i]), fmt4(estimates$STDERR[i]),
    fmt4(estimates$LOWER[i]), fmt4(estimates$UPPER[i]),
    estimates$N_FAILED[i], estimates$N_LEFT[i]))
}
cat("\nRank Statistics\n")
cat(sprintf("%-10s %-10s %14s %14s\n","Stratum","Test","RankStatistic","Variance"))
for (k in seq_len(nrow(rank_df))) {
  cat(sprintf("%-10s %-10s %14s %14s %14s %14s\n", rank_df$ARM[k], rank_df$TEST[k],
    fmt4(rank_df$RANKSTAT[k]), fmt4(rank_df$VARIANCE[k]),
    fmt2(rank_df$VARIANCE[k]), fmt3(rank_df$VARIANCE[k])))
  if (rank_df$TEST[k]=="Wilcoxon") {
    cat(sprintf("%-10s %-10s %14d %14.2f\n", rank_df$ARM[k], rank_df$TEST[k],
      as.integer(round_half_up(rank_df$RANKSTAT[k],0)), round_half_up(rank_df$VARIANCE[k],2)))
  } else {
    cat(sprintf("%-10s %-10s %14.4f %14.4f\n", rank_df$ARM[k], rank_df$TEST[k],
      round_half_up(rank_df$RANKSTAT[k],4), round_half_up(rank_df$VARIANCE[k],4)))
  }
}
cat("\nCovariance Matrix for the Log-Rank statistic\n")
cat(sprintf("%-10s %14s %14s\n","", arms[1], arms[2]))
cat(sprintf("%-10s %14s %14s\n", arms[1], fmt4(V_lr), fmt4(-V_lr)))
cat(sprintf("%-10s %14s %14s\n", arms[2], fmt4(-V_lr), fmt4(V_lr)))
cat(sprintf("%-10s %14s %14s (2-dec)\n", arms[1], fmt2(V_lr), fmt2(-V_lr)))
cat(sprintf("%-10s %14s %14s (2-dec)\n", arms[2], fmt2(-V_lr), fmt2(V_lr)))
cat("\nCovariance Matrix for the Wilcoxon statistic\n")
cat(sprintf("%-10s %14s %14s\n","", arms[1], arms[2]))
cat(sprintf("%-10s %14s %14s\n", arms[1], fmt2(V_w), fmt2(-V_w)))
cat(sprintf("%-10s %14s %14s\n", arms[2], fmt2(-V_w), fmt2(V_w)))
cat(sprintf("%-10s %14s %14s (4-dec)\n", arms[1], fmt4(V_w), fmt4(-V_w)))
cat(sprintf("%-10s %14s %14s (4-dec)\n", arms[2], fmt4(-V_w), fmt4(V_w)))
cat("\nTest of Equality over Strata\n")
cat(sprintf("%-12s %12s %6s %12s\n","Test","ChiSquare","DF","pValue"))
cat(sprintf("%-12s %12s %6d %12s\n","Log-Rank", fmt4(chisq_lr), 1, fmt4(p_lr)))
cat(sprintf("%-12s %12s %6d %12s\n","Wilcoxon", fmt4(chisq_w), 1, fmt4(p_w)))
cat(sprintf("%-12s %12s %6d %12s\n","-2Log(LR)", fmt4(lr_stat), 1, fmt4(p_lrt)))
cat(sprintf("%-12s %12s %12s (2-dec)\n","Log-Rank", fmt2(chisq_lr), fmt2(p_lr)))
cat(sprintf("%-12s %12s %12s (2-dec)\n","Wilcoxon", fmt2(chisq_w), fmt2(p_w)))
cat("\nObserved vs Expected under H0\n")
cat(sprintf("%-10s %10s %10s\n","Stratum","Observed","Expected"))
for (j in seq_along(sdiff$n)) {
  nm <- sub(".*=", "", names(sdiff$n)[j])
  cat(sprintf("%-10s %10d %10s\n", nm, as.integer(sdiff$obs[j]), fmt2(sdiff$exp[j])))
  cat(sprintf("%-10s %10s %10s (4-dec)\n", nm, fmt4(sdiff$obs[j]), fmt4(sdiff$exp[j])))
}
cat("\nNumber at Risk (ATRISK=0 TO 330 BY 30)\n")
cat(sprintf("%8s","Day")); for (a in arms) cat(sprintf(" %10s",a)); cat("\n")
for (k in seq_along(atrisk_times)) {
  cat(sprintf("%8d", atrisk_times[k]))
  for (a in arms) cat(sprintf(" %10d", atrisk_mat[k,a]))
  cat("\n")
}
cat("\nSurvival axis 0 0.25 0.50 0.75 1.00  Time axis 0 30 60 90 120 150 180 210 240 270 300 330\n")
cat("Censoring marks +  Confidence level 95 Percent  ALPHA 0.05\n")
cat("Note: SAS program had 'srata arm' (typo for 'strata arm'); stratified by arm as intended.\n")
sink()
cat("TXT/CSV written\n")
cols <- c("Arm 1"="#D55E00","Arm 2"="#0072B2")
p_main <- ggplot(plot_step, aes(x=DAY, y=SURVIVAL, color=ARM, group=ARM)) +
  geom_step(linewidth=1) +
  geom_point(data=cens_surv, aes(x=day, y=SURVIVAL, color=arm),
    shape=3, size=2.5, stroke=1, inherit.aes=FALSE) +
  scale_x_continuous(breaks=atrisk_times, limits=c(0,350)) +
  scale_y_continuous(breaks=seq(0,1,by=0.2), limits=c(0,1)) +
  scale_color_manual(values=cols) +
  labs(title="Example K-M Plot", x="Day", y="Survival Probability", color="Stratum") +
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"),
  legend.position="bottom")
p_risk <- ggplot(atrisk_long, aes(x=TIME, y=ARM, label=N_ATRISK, color=ARM)) +
  geom_text(size=4, show.legend=FALSE) +
  scale_x_continuous(breaks=atrisk_times, limits=c(0,350)) +
  scale_y_discrete(limits=rev(arms)) +
  scale_color_manual(values=cols) +
  labs(x="Day", y="At Risk") + theme_bw() +
  theme(legend.position="none", axis.title.y=element_text(face="bold"),
  panel.grid.major.y=element_blank())
png(file.path(out,"P040_f_kmplot.png"), width=1200, height=800, res=120)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1,heights=unit(c(3,1),"null"))))
print(p_main, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p_risk, vp=viewport(layout.pos.row=2, layout.pos.col=1))
dev.off()
cat("P040 done\n")
```