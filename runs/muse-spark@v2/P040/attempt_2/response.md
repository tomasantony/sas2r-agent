```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(ggplot2))

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x) * mult
  r <- floor(ax + 0.5 + 1e-10) / mult
  r * s
}
# SAS BEST-like trimmed formatting: round half up to `digits`, then strip trailing zeros
fmtT <- function(x, digits = 4) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    r <- round_half_up(v, digits)
    s <- sprintf(paste0("%.", digits, "f"), r)
    # strip trailing zeros but keep at least one digit after decimal if needed -> SAS BEST strips
    if (grepl("\\.", s)) {
      s <- sub("0+$", "", s)
      s <- sub("\\.$", "", s)
    }
    # SAS prints 0.xxx not .xxx ; sprintf already does 0.xxx
    # normalize -0 to 0
    if (s %in% c("-0", "-0.0", "-0.00", "-0.000", "-0.0000")) s <- "0"
    s
  })
}
fmt_day <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (abs(v - round(v)) < 1e-9) sprintf("%d", as.integer(round(v)))
    else {
      t <- sprintf("%.5f", v)
      t <- sub("0+$", "", t)
      t <- sub("\\.$", "", t)
      t
    }
  })
}

# ------------------------------------------------------------------
# data one  (SAS data step)
# ------------------------------------------------------------------
arm1_days <- c(143,164,188,188,190,192,206,213,216,220,227.230,
               234,246,265,304, 216,244)
arm1_cens <- c(rep(0,15), 1, 1)
arm2_days <- c(142,156,163,198,205,232,232,233,233,233,233,239,
               240,261,280,280,296,296,323, 204,344)
arm2_cens <- c(rep(0,19), 1, 1)

one <- data.frame(
  arm    = c(rep("Arm 1", length(arm1_days)), rep("Arm 2", length(arm2_days))),
  day    = c(arm1_days, arm2_days),
  censor = c(arm1_cens, arm2_cens),
  stringsAsFactors = FALSE
)
one$event <- 1 - one$censor
stopifnot(all(c("arm","day","censor") %in% names(one)))
stopifnot(nrow(one) == 38)

one$arm <- factor(one$arm, levels = c("Arm 1","Arm 2"))

fit <- survfit(Surv(day, event) ~ arm, data = one, conf.type = "log-log", conf.int = 0.95)
fit_pool <- survfit(Surv(day, event) ~ 1, data = one, conf.type = "log-log", conf.int = 0.95)

mk_pl <- function(fitobj, arm_lab_fun) {
  times   <- fitobj$time
  nrisk   <- fitobj$n.risk
  nevent  <- fitobj$n.event
  ncens   <- fitobj$n.censor
  surv    <- fitobj$surv
  se      <- fitobj$std.err
  lower   <- if (!is.null(fitobj$lower)) fitobj$lower else rep(NA, length(times))
  upper   <- if (!is.null(fitobj$upper)) fitobj$upper else rep(NA, length(times))
  if (!is.null(fitobj$strata)) {
    snames <- rep(names(fitobj$strata), times = as.vector(fitobj$strata))
    arm_lab <- sub(".*=", "", snames)
  } else {
    arm_lab <- rep("Overall", length(times))
  }
  df <- data.frame(
    arm = arm_lab,
    time = times,
    n_risk = nrisk,
    n_event = nevent,
    n_censor = ncens,
    surv = surv,
    stderr = se,
    lower = lower,
    upper = upper,
    stringsAsFactors = FALSE
  )
  df <- df[order(df$arm, df$time), ]
  df$n_failed <- NA_integer_
  df$n_left <- NA_integer_
  for (a in unique(df$arm)) {
    idx <- which(df$arm == a)
    df$n_failed[idx] <- cumsum(df$n_event[idx])
    df$n_left[idx] <- df$n_risk[idx] - df$n_event[idx] - df$n_censor[idx]
  }
  df$failure <- 1 - df$surv
  df
}
pl <- mk_pl(fit)
pl_pool <- mk_pl(fit_pool)

get_quant <- function(fitobj, probs = c(0.75, 0.5, 0.25)) {
  q <- try(quantile(fitobj, probs = probs, conf.int = TRUE), silent = TRUE)
  arms <- if (!is.null(fitobj$strata)) c("Arm 1","Arm 2") else c("Overall")
  res <- list()
  ok <- FALSE
  if (!inherits(q, "try-error") && !is.null(q$quantile)) {
    qmat <- q$quantile; lmat <- q$lower; umat <- q$upper
    try({
      for (i in seq_along(arms)) {
        for (j in seq_along(probs)) {
          est <- suppressWarnings(as.numeric(if (is.matrix(qmat)) qmat[i, j] else qmat[i]))
          lo <- suppressWarnings(as.numeric(if (!is.null(lmat) && is.matrix(lmat)) lmat[i, j] else NA))
          hi <- suppressWarnings(as.numeric(if (!is.null(umat) && is.matrix(umat)) umat[i, j] else NA))
          res[[length(res)+1]] <- data.frame(arm=arms[i], percent=(1-probs[j])*100,
                                             estimate=est, lower=lo, upper=hi,
                                             stringsAsFactors=FALSE)
        }
      }
      # check at least one non-missing estimate
      if (any(!is.na(sapply(res, function(d) d$estimate)))) ok <- TRUE
    }, silent = TRUE)
  }
  if (ok) return(do.call(rbind, res))
  # manual Brookmeyer-Crowley fallback from pl
  src <- if (!is.null(fitobj$strata)) pl else pl_pool
  out <- list()
  for (a in arms) {
    sub <- src[src$arm == a, ]; sub <- sub[order(sub$time), ]
    for (p in probs) {
      idx <- which(sub$surv <= p + 1e-12)
      est <- if (length(idx)>0) sub$time[min(idx)] else NA
      idxL <- which(!is.na(sub$upper) & sub$upper <= p + 1e-12)
      lo <- if (length(idxL)>0) sub$time[min(idxL)] else NA
      idxU <- which(!is.na(sub$lower) & sub$lower <= p + 1e-12)
      hi <- if (length(idxU)>0) sub$time[min(idxU)] else NA
      out[[length(out)+1]] <- data.frame(arm=a, percent=(1-p)*100,
                                         estimate=est, lower=lo, upper=hi,
                                         stringsAsFactors=FALSE)
    }
  }
  do.call(rbind, out)
}
quart <- get_quant(fit, c(0.75, 0.5, 0.25))
quart <- quart[order(quart$arm, -quart$percent), ]
quart_pool <- get_quant(fit_pool, c(0.75, 0.5, 0.25))

mean_list <- list()
for (a in c("Arm 1","Arm 2")) {
  sub <- pl[pl$arm == a, ]; sub <- sub[order(sub$time), ]
  tau <- max(sub$time[sub$n_event > 0])
  sub2 <- sub[sub$time <= tau + 1e-9, ]
  t_all <- c(0, sub2$time); s_all <- c(1, sub2$surv)
  mu <- sum(s_all[seq_len(length(t_all)-1)] * diff(t_all))
  n2 <- nrow(sub2)
  A <- rep(0, n2)
  for (i in seq_len(n2)) {
    s_k <- s_all[(i+1):(n2+1)]
    t_k <- t_all[(i+1):(n2+1)]
    t_next <- c(t_all[(i+2):(n2+1)], tau)
    A[i] <- sum(s_k * (t_next - t_k))
  }
  d <- sub2$n_event; n <- sub2$n_risk
  term <- ifelse(d>0 & n>d, A^2 * d / (n*(n-d)), 0)
  var_mu <- sum(term, na.rm=TRUE)
  se_mu <- sqrt(var_mu)
  mean_list[[a]] <- data.frame(arm=a, tau=tau, mean=mu, se=se_mu, stringsAsFactors=FALSE)
}
means <- do.call(rbind, mean_list)
# pooled mean
subp <- pl_pool[order(pl_pool$time), ]
taup <- max(subp$time[subp$n_event > 0])
subp2 <- subp[subp$time <= taup + 1e-9, ]
t_all <- c(0, subp2$time); s_all <- c(1, subp2$surv)
mup <- sum(s_all[seq_len(length(t_all)-1)] * diff(t_all))
n2 <- nrow(subp2); A <- rep(0, n2)
for (i in seq_len(n2)) {
  s_k <- s_all[(i+1):(n2+1)]; t_k <- t_all[(i+1):(n2+1)]
  t_next <- c(t_all[(i+2):(n2+1)], taup)
  A[i] <- sum(s_k * (t_next - t_k))
}
d <- subp2$n_event; n <- subp2$n_risk
term <- ifelse(d>0 & n>d, A^2 * d / (n*(n-d)), 0)
sep <- sqrt(sum(term, na.rm=TRUE))
means_pool <- data.frame(arm="Overall", tau=taup, mean=mup, se=sep, stringsAsFactors=FALSE)

cens_sum <- do.call(rbind, lapply(c("Arm 1","Arm 2"), function(a) {
  sub <- one[one$arm==a, ]
  f <- sum(sub$event==1); c <- sum(sub$event==0); t <- nrow(sub)
  data.frame(arm=a, failed=f, censored=c, total=t,
             pct_cens=100*c/t, stringsAsFactors=FALSE)
}))
tot_f <- sum(cens_sum$failed); tot_c <- sum(cens_sum$censored); tot_t <- sum(cens_sum$total)

ev_times <- sort(unique(one$day[one$event==1]))
U_lr <- 0; V_lr <- 0
U_w <- 0; V_w <- 0
for (t in ev_times) {
  n1 <- sum(one$day >= t & one$arm=="Arm 1")
  n2 <- sum(one$day >= t & one$arm=="Arm 2")
  n <- n1+n2
  d1 <- sum(one$day==t & one$event==1 & one$arm=="Arm 1")
  d2 <- sum(one$day==t & one$event==1 & one$arm=="Arm 2")
  d <- d1+d2
  if (n<=1 || d==0) next
  e1 <- n1*d/n
  v <- (n1*n2*d*(n-d))/(n*n*(n-1))
  U_lr <- U_lr + (d1 - e1)
  V_lr <- V_lr + v
  w <- n
  U_w <- U_w + w*(d1 - e1)
  V_w <- V_w + w*w*v
}
chi_lr <- ifelse(V_lr>0, U_lr^2/V_lr, NA)
chi_w  <- ifelse(V_w>0, U_w^2/V_w, NA)
p_lr <- 1 - pchisq(chi_lr, 1)
p_w  <- 1 - pchisq(chi_w, 1)

E1 <- 0; O1 <- 0; E2 <- 0; O2 <- 0
for (t in ev_times) {
  n1 <- sum(one$day >= t & one$arm=="Arm 1")
  n2 <- sum(one$day >= t & one$arm=="Arm 2")
  n <- n1+n2
  d1 <- sum(one$day==t & one$event==1 & one$arm=="Arm 1")
  d2 <- sum(one$day==t & one$event==1 & one$arm=="Arm 2")
  d <- d1+d2
  if (n==0 || d==0) next
  E1 <- E1 + n1*d/n
  E2 <- E2 + n2*d/n
  O1 <- O1 + d1
  O2 <- O2 + d2
}

atrisk_times <- seq(0, 330, by=30)
atrisk <- expand.grid(time=atrisk_times, arm=c("Arm 1","Arm 2"), stringsAsFactors=FALSE)
atrisk$n <- mapply(function(t,a) sum(one$day >= t & one$arm==a), atrisk$time, atrisk$arm)

plotdata_list <- list()
for (a in c("Arm 1","Arm 2")) {
  sub <- pl[pl$arm==a, ]; sub <- sub[order(sub$time), ]
  pd <- data.frame(arm=a, time=c(0, sub$time), surv=c(1, sub$surv),
                   stderr=c(0, sub$stderr),
                   lower=c(1, sub$lower), upper=c(1, sub$upper),
                   n_risk=c(sum(one$arm==a), sub$n_risk),
                   n_event=c(0, sub$n_event), n_censor=c(0, sub$n_censor),
                   stringsAsFactors=FALSE)
  plotdata_list[[a]] <- pd
}
plotdata <- do.call(rbind, plotdata_list)
cens_pts <- pl[pl$n_censor>0, ]
cens_plot <- data.frame(arm=cens_pts$arm, time=cens_pts$time, surv=cens_pts$surv)

y_arm1 <- -0.07
y_arm2 <- -0.14
p <- ggplot(plotdata, aes(x=time, y=surv, colour=arm, linetype=arm)) +
  geom_step(linewidth=0.9) +
  geom_point(data=cens_plot, aes(x=time, y=surv, colour=arm), shape=3, size=2.2, inherit.aes=FALSE) +
  scale_x_continuous(breaks=atrisk_times, limits=c(-30, 350)) +
  scale_y_continuous(breaks=seq(0,1,by=0.2), limits=c(-0.22,1.05)) +
  labs(title="Example K-M Plot", x="day", y="Survival Probability", colour="Stratum", linetype="Stratum") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="bottom") +
  geom_text(data=atrisk[atrisk$arm=="Arm 1",],
            aes(x=time, y=y_arm1, label=n), colour="black", size=3, inherit.aes=FALSE) +
  geom_text(data=atrisk[atrisk$arm=="Arm 2",],
            aes(x=time, y=y_arm2, label=n), colour="black", size=3, inherit.aes=FALSE) +
  annotate("text", x=-22, y=y_arm1, label="Arm 1", hjust=1, size=3) +
  annotate("text", x=-22, y=y_arm2, label="Arm 2", hjust=1, size=3) +
  annotate("text", x=-22, y= -0.005, label="At Risk", hjust=1, size=3, fontface="bold")
ggsave(file.path(out, "P040_f_kmplot.png"), p, width=8, height=6, dpi=150)

write.csv(plotdata, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names=FALSE, na="")

csv_body <- data.frame(
  ARM = pl$arm,
  DAY = pl$time,
  SURVIVAL = round_half_up(pl$surv, 4),
  FAILURE = round_half_up(pl$failure, 4),
  STDERR = round_half_up(pl$stderr, 4),
  LOWER95 = round_half_up(pl$lower, 4),
  UPPER95 = round_half_up(pl$upper, 4),
  N_FAILED = pl$n_failed,
  N_LEFT = pl$n_left,
  stringsAsFactors=FALSE
)
write.csv(csv_body, file.path(out, "P040_f_kmplot.csv"), row.names=FALSE, na="")

txt <- file.path(out, "P040_f_kmplot.txt")
con <- file(txt, open="wt")

cat("Example K-M Plot\n", file=con)
cat("PROC LIFETEST  Product-Limit Survival Estimates  (TIME day*CENSOR(1) ; STRATA arm)\n", file=con)
cat("Plots = survival (atrisk = 0 to 330 by 30)\n", file=con)
cat("\n", file=con)

for (a in c("Arm 1","Arm 2")) {
  cat(sprintf("Stratum: %s  (N = %d)\n", a, sum(one$arm==a)), file=con)
  cat(sprintf("%-10s %12s %12s %12s %12s %12s %10s %10s\n",
              "day","Survival","Failure","StdErr","Lower95","Upper95","N_Failed","N_Left"), file=con)
  sub <- pl[pl$arm==a, ]; sub <- sub[order(sub$time), ]
  for (i in seq_len(nrow(sub))) {
    star <- ifelse(sub$n_censor[i]>0 && sub$n_event[i]==0, "*", ifelse(sub$n_censor[i]>0, "+", " "))
    cat(sprintf("%-10s %12s %12s %12s %12s %12s %10d %10d%s\n",
                fmt_day(sub$time[i]),
                fmtT(sub$surv[i],4),
                fmtT(sub$failure[i],4),
                fmtT(sub$stderr[i],4),
                fmtT(sub$lower[i],4),
                fmtT(sub$upper[i],4),
                sub$n_failed[i],
                sub$n_left[i],
                star), file=con)
  }
  cat("  * censored-only time ; + event and censored tied (censored after events)\n", file=con)
  cat("\n", file=con)
}

cat("Overall (pooled over strata, N = 38)\n", file=con)
cat(sprintf("%-10s %12s %12s %12s %12s %12s %10s %10s\n",
            "day","Survival","Failure","StdErr","Lower95","Upper95","N_Failed","N_Left"), file=con)
for (i in seq_len(nrow(pl_pool))) {
  star <- ifelse(pl_pool$n_censor[i]>0 && pl_pool$n_event[i]==0, "*", ifelse(pl_pool$n_censor[i]>0, "+", " "))
  cat(sprintf("%-10s %12s %12s %12s %12s %12s %10d %10d%s\n",
              fmt_day(pl_pool$time[i]),
              fmtT(pl_pool$surv[i],4),
              fmtT(pl_pool$failure[i],4),
              fmtT(pl_pool$stderr[i],4),
              fmtT(pl_pool$lower[i],4),
              fmtT(pl_pool$upper[i],4),
              pl_pool$n_failed[i],
              pl_pool$n_left[i],
              star), file=con)
}
cat("\n", file=con)

cat("Quartile Estimates (95% Confidence Interval, Transform = LOG-LOG)\n", file=con)
cat(sprintf("%-10s %-10s %-12s %-12s %-12s\n","Stratum","Percent","Estimate","Lower","Upper"), file=con)
for (i in seq_len(nrow(quart))) {
  cat(sprintf("%-10s %-10.0f %-12s %-12s %-12s\n",
              quart$arm[i], quart$percent[i],
              fmt_day(quart$estimate[i]),
              fmt_day(quart$lower[i]),
              fmt_day(quart$upper[i])), file=con)
}
for (i in seq_len(nrow(quart_pool))) {
  cat(sprintf("%-10s %-10.0f %-12s %-12s %-12s\n",
              quart_pool$arm[i], quart_pool$percent[i],
              fmt_day(quart_pool$estimate[i]),
              fmt_day(quart_pool$lower[i]),
              fmt_day(quart_pool$upper[i])), file=con)
}
cat("\n", file=con)

cat("Means (Restricted to Largest Event Time per Stratum)\n", file=con)
cat(sprintf("%-10s %-12s %-12s %-12s\n","Stratum","Tau","Mean","StdErr"), file=con)
for (i in seq_len(nrow(means))) {
  cat(sprintf("%-10s %-12s %-12s %-12s\n",
              means$arm[i],
              fmt_day(means$tau[i]),
              fmtT(means$mean[i],4),
              fmtT(means$se[i],4)), file=con)
}
cat(sprintf("%-10s %-12s %-12s %-12s\n",
            means_pool$arm, fmt_day(means_pool$tau),
            fmtT(means_pool$mean,4), fmtT(means_pool$se,4)), file=con)
cat("\n", file=con)

cat("Summary of the Number of Censored and Uncensored Values\n", file=con)
cat(sprintf("%-10s %10s %10s %10s %14s\n","Stratum","Failed","Censored","Total","Pct_Cens"), file=con)
for (i in seq_len(nrow(cens_sum))) {
  cat(sprintf("%-10s %10d %10d %10d %14s\n",
              cens_sum$arm[i], cens_sum$failed[i], cens_sum$censored[i],
              cens_sum$total[i], fmtT(cens_sum$pct_cens[i],1)), file=con)
}
cat(sprintf("%-10s %10d %10d %10d %14s\n","Total", tot_f, tot_c, tot_t,
            fmtT(100*tot_c/tot_t,1)), file=con)
cat("\n", file=con)

cat("Rank Statistics (Observed vs Expected under H0)\n", file=con)
cat(sprintf("%-10s %12s %12s %14s %14s\n","Stratum","Observed","Expected","O-E(LogRank)","O-E(Wilcoxon)"), file=con)
Uw1 <- U_w; Uw2 <- -U_w
cat(sprintf("%-10s %12d %12s %14s %14s\n","Arm 1", O1, fmtT(E1,4), fmtT(U_lr,4), fmtT(Uw1,2)), file=con)
cat(sprintf("%-10s %12d %12s %14s %14s\n","Arm 2", O2, fmtT(E2,4), fmtT(-U_lr,4), fmtT(Uw2,2)), file=con)
cat("\n", file=con)

cat("Covariance Matrix for the Log-Rank Statistics\n", file=con)
cat(sprintf("  Arm 1  %s\n", fmtT(V_lr,4)), file=con)
cat("Covariance Matrix for the Wilcoxon (Breslow) Statistics\n", file=con)
cat(sprintf("  Arm 1  %s\n", fmtT(V_w,2)), file=con)
cat("\n", file=con)

cat("Test of Equality over Strata\n", file=con)
cat(sprintf("%-12s %12s %6s %12s\n","Test","Chi-Square","DF","Pr > Chi-Square"), file=con)
cat(sprintf("%-12s %12s %6d %12s\n","Log-Rank", fmtT(chi_lr,4), 1, fmtT(p_lr,4)), file=con)
cat(sprintf("%-12s %12s %6d %12s\n","Wilcoxon", fmtT(chi_w,4), 1, fmtT(p_w,4)), file=con)
cat("\n", file=con)

cat("Number at Risk (atrisk = 0 to 330 by 30)\n", file=con)
cat(sprintf("%-10s", "Time"), file=con)
for (t in atrisk_times) cat(sprintf(" %6d", t), file=con)
cat("\n", file=con)
for (a in c("Arm 1","Arm 2")) {
  cat(sprintf("%-10s", a), file=con)
  vals <- atrisk$n[atrisk$arm==a][order(atrisk$time[atrisk$arm==a])]
  for (v in vals) cat(sprintf(" %6d", v), file=con)
  cat("\n", file=con)
}
cat("\n", file=con)
cat("Listing of input data ONE (day*censor(1))\n", file=con)
cat(sprintf("%-10s %-10s %-10s\n","arm","day","censor"), file=con)
oo <- one[order(one$arm, one$day, one$censor), ]
for (i in seq_len(nrow(oo))) {
  cat(sprintf("%-10s %-10s %-10d\n", oo$arm[i], fmt_day(oo$day[i]), oo$censor[i]), file=con)
}
cat("\n", file=con)
cat("Note: Censoring indicated by '+' on plot; survival step function with Greenwood standard errors.\n", file=con)
cat("Mean restricted to largest event time (Arm 1 tau=304, Arm 2 tau=323).\n", file=con)
close(con)
```