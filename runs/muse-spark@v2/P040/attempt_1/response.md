```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(ggplot2))

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  s <- sign(x)
  # sign(0)=0 -> keep 0
  ax <- abs(x) * mult
  r <- floor(ax + 0.5 + 1e-10) / mult
  r * s
}
fmt4 <- function(x) {
  ifelse(is.na(x), "", formatC(round_half_up(x, 4), format = "f", digits = 4))
}
fmt2 <- function(x) {
  ifelse(is.na(x), "", formatC(round_half_up(x, 2), format = "f", digits = 2))
}
fmt1 <- function(x) {
  ifelse(is.na(x), "", formatC(round_half_up(x, 1), format = "f", digits = 1))
}
fmt_day <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (abs(v - round(v)) < 1e-9) sprintf("%d", as.integer(round(v)))
    else {
      # keep as in SAS, e.g. 227.23
      # remove trailing zeros
      t <- sprintf("%.5f", v)
      t <- sub("0+$", "", t)
      t <- sub("\\.$", "", t)
      t
    }
  })
}

# ------------------------------------------------------------------
# data one  (SAS data step)
# length arm $8 ; censor=0/1 ; day list ; note 227.230 = 227.23
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

# SAS: proc lifetest ... ; time day*censor(1) ; srata arm ;
# "srata" is a typo for STRATA -> analyse by arm (intent of program)
one$arm <- factor(one$arm, levels = c("Arm 1","Arm 2"))

fit <- survfit(Surv(day, event) ~ arm, data = one, conf.type = "log-log", conf.int = 0.95)

# ------------------------------------------------------------------
# Extract product-limit estimates per stratum
# survfit vectors are concatenated across strata
# ------------------------------------------------------------------
strata_n <- fit$strata  # named c("arm=Arm 1"=17? no, length of vectors)
# fit$time, n.risk, n.event, n.censor, surv, std.err, lower, upper
mk_pl <- function() {
  times   <- fit$time
  nrisk   <- fit$n.risk
  nevent  <- fit$n.event
  ncens   <- fit$n.censor
  surv    <- fit$surv
  se      <- fit$std.err
  lower   <- if (!is.null(fit$lower)) fit$lower else rep(NA, length(times))
  upper   <- if (!is.null(fit$upper)) fit$upper else rep(NA, length(times))
  # stratum labels
  snames <- rep(names(fit$strata), times = as.vector(fit$strata))
  arm_lab <- sub(".*=", "", snames)
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
  # order by arm then time (SAS character ordering uppercase before lowercase;
  # here "Arm 1" < "Arm 2")
  df <- df[order(df$arm, df$time), ]
  # cumulative failed and number left (SAS: Number Failed, Number Left)
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
pl <- mk_pl()

# ------------------------------------------------------------------
# Quartile estimates (75, 50, 25) with 95% log-log CI (Brookmeyer-Crowley)
# Use survival::quantile if available, else manual from CI bands
# ------------------------------------------------------------------
get_quant <- function(probs = c(0.75, 0.5, 0.25)) {
  q <- try(quantile(fit, probs = probs, conf.int = TRUE), silent = TRUE)
  res <- list()
  if (!inherits(q, "try-error") && !is.null(q$quantile)) {
    # q$quantile : matrix strata x probs? ; q$lower, q$upper similar
    # handle vector/matrix cases
    qmat <- q$quantile
    lmat <- q$lower
    umat <- q$upper
    # normalize to data.frame
    # qmat may be named vector or matrix
    arms <- c("Arm 1","Arm 2")
    for (i in seq_along(arms)) {
      for (j in seq_along(probs)) {
        p <- probs[j]
        # extract: try matrix [i,j] else vector
        est <- NA; lo <- NA; hi <- NA
        try({
          if (is.matrix(qmat)) est <- qmat[i, as.character(p)]
          else est <- qmat[paste0("arm=", arms[i], " ", p)] # fallback
        }, silent = TRUE)
        # more robust: if names contain prob
        # Instead parse by position: quantile returns per-stratum vectors
        est <- suppressWarnings(as.numeric(if (is.matrix(qmat)) qmat[i, j] else qmat[i]))
        # lower/upper similarly
        lo <- suppressWarnings(as.numeric(if (!is.null(lmat) && is.matrix(lmat)) lmat[i, j] else NA))
        hi <- suppressWarnings(as.numeric(if (!is.null(umat) && is.matrix(umat)) umat[i, j] else NA))
        res[[length(res)+1]] <- data.frame(arm=arms[i], percent=(1-p)*100,
                                           estimate=est, lower=lo, upper=hi,
                                           stringsAsFactors=FALSE)
      }
    }
    # If parsing failed (NAs for estimates that should exist), fall back to manual
    need_fallback <- any(sapply(res, function(d) all(is.na(d$estimate))))
    if (!need_fallback) {
      return(do.call(rbind, res))
    }
  }
  # manual Brookmeyer-Crowley using pl lower/upper (log-log CI)
  out <- list()
  for (a in c("Arm 1","Arm 2")) {
    sub <- pl[pl$arm == a, ]
    sub <- sub[order(sub$time), ]
    for (p in probs) {
      # point: first time surv <= p
      idx <- which(sub$surv <= p + 1e-12)
      est <- if (length(idx)>0) sub$time[min(idx)] else NA
      # lower CI for quantile = first time upper <= p
      idxL <- which(!is.na(sub$upper) & sub$upper <= p + 1e-12)
      lo <- if (length(idxL)>0) sub$time[min(idxL)] else NA
      # upper CI = first time lower <= p
      idxU <- which(!is.na(sub$lower) & sub$lower <= p + 1e-12)
      hi <- if (length(idxU)>0) sub$time[min(idxU)] else NA
      out[[length(out)+1]] <- data.frame(arm=a, percent=(1-p)*100,
                                         estimate=est, lower=lo, upper=hi,
                                         stringsAsFactors=FALSE)
    }
  }
  do.call(rbind, out)
}
quart <- get_quant(c(0.75, 0.5, 0.25))
# Ensure ordering percent 75,50,25 within each arm
quart <- quart[order(quart$arm, -quart$percent), ]

# ------------------------------------------------------------------
# Restricted means (restricted to largest event time per stratum, SAS default)
# Var via Greenwood integral: Var = sum A_i^2 * d_i/(n_i*(n_i-d_i))
# ------------------------------------------------------------------
mean_list <- list()
for (a in c("Arm 1","Arm 2")) {
  sub <- pl[pl$arm == a, ]
  sub <- sub[order(sub$time), ]
  tau <- max(sub$time[sub$n_event > 0])
  # keep times <= tau (drop censored beyond tau, e.g. 344)
  sub2 <- sub[sub$time <= tau + 1e-9, ]
  t_all <- c(0, sub2$time)
  s_all <- c(1, sub2$surv)
  # RMST
  mu <- sum(s_all[seq_len(length(t_all)-1)] * diff(t_all))
  # A_i for each event time: area from t_i to tau
  # area after t_i = sum_{k: t_k >= t_i} S_k*(t_{k+1}-t_k), t_{n+1}=tau
  # Build vectors of distinct times (t_all[-1]) with S = s_all[-1]
  n2 <- nrow(sub2)
  # extended times with tau endpoint
  # t_all already ends at tau (since tau is max event time and included)
  # S for intervals: s_all[1..n]
  # For event index i (in sub2 rows), position in t_all = i+1, S after = s_all[i+1]
  # Area from t_i = sum_{k=i}^{n2} S_k*(t_{k+1}-t_k) where t_{n2+1}=tau? but tau=t_{n2}
  # so last term zero; general:
  A <- rep(0, n2)
  for (i in seq_len(n2)) {
    # intervals k = i..n2  (k indexes sub2 rows, interval k is [t_k, t_{k+1}) with S_k)
    # t_k = t_all[k+1]? careful: t_all[1]=0, t_all[2]=first time...
    # S_k = s_all[k+1]
    # t_{k+1} = t_all[k+2] for k<n2, = tau for k=n2 (which equals t_all[n2+1])
    s_k <- s_all[(i+1):(n2+1)]
    t_k <- t_all[(i+1):(n2+1)]
    t_next <- c(t_all[(i+2):(n2+1)], tau)
    A[i] <- sum(s_k * (t_next - t_k))
  }
  d <- sub2$n_event
  n <- sub2$n_risk
  term <- ifelse(d>0 & n>d, A^2 * d / (n*(n-d)), 0)
  # for n==d (last interval where risk==events, e.g. n=1,d=1) term uses limit?
  # SAS/Greenwood: when n==d, term is A^2 * ... -> infinite? Actually last term
  # contributes 0 because A=0 at tau. So keep 0.
  var_mu <- sum(term, na.rm=TRUE)
  se_mu <- sqrt(var_mu)
  mean_list[[a]] <- data.frame(arm=a, tau=tau, mean=mu, se=se_mu, stringsAsFactors=FALSE)
}
means <- do.call(rbind, mean_list)

# ------------------------------------------------------------------
# Censored summary
# ------------------------------------------------------------------
cens_sum <- do.call(rbind, lapply(c("Arm 1","Arm 2"), function(a) {
  sub <- one[one$arm==a, ]
  f <- sum(sub$event==1); c <- sum(sub$event==0); t <- nrow(sub)
  data.frame(arm=a, failed=f, censored=c, total=t,
             pct_cens=100*c/t, stringsAsFactors=FALSE)
}))
tot_f <- sum(cens_sum$failed); tot_c <- sum(cens_sum$censored); tot_t <- sum(cens_sum$total)

# ------------------------------------------------------------------
# Log-rank and Wilcoxon (Breslow/Gehan) tests pooled over strata
# ------------------------------------------------------------------
# distinct event times pooled
ev_times <- sort(unique(one$day[one$event==1]))
U_lr <- 0; V_lr <- 0
U_w <- 0; V_w <- 0
rank_rows <- list()
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
  w <- n  # Breslow weight
  U_w <- U_w + w*(d1 - e1)
  V_w <- V_w + w*w*v
}
chi_lr <- ifelse(V_lr>0, U_lr^2/V_lr, NA)
chi_w  <- ifelse(V_w>0, U_w^2/V_w, NA)
p_lr <- 1 - pchisq(chi_lr, 1)
p_w  <- 1 - pchisq(chi_w, 1)

# Expected totals for rank statistics table (sum e1, sum d1)
# compute sums
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

# ------------------------------------------------------------------
# At-risk numbers 0 to 330 by 30 (plots=survival atrisk option)
# ------------------------------------------------------------------
atrisk_times <- seq(0, 330, by=30)
atrisk <- expand.grid(time=atrisk_times, arm=c("Arm 1","Arm 2"), stringsAsFactors=FALSE)
atrisk$n <- mapply(function(t,a) sum(one$day >= t & one$arm==a), atrisk$time, atrisk$arm)

# ------------------------------------------------------------------
# Plot data (step function incl. time 0)
# ------------------------------------------------------------------
plotdata_list <- list()
for (a in c("Arm 1","Arm 2")) {
  sub <- pl[pl$arm==a, ]
  sub <- sub[order(sub$time), ]
  pd <- data.frame(arm=a, time=c(0, sub$time), surv=c(1, sub$surv),
                   stderr=c(0, sub$stderr), n_risk=c(sum(one$arm==a), sub$n_risk),
                   n_event=c(0, sub$n_event), n_censor=c(0, sub$n_censor),
                   stringsAsFactors=FALSE)
  plotdata_list[[a]] <- pd
}
plotdata <- do.call(rbind, plotdata_list)
cens_pts <- pl[pl$n_censor>0, ]
# map censored times to surv for marks
cens_plot <- data.frame(arm=cens_pts$arm, time=cens_pts$time, surv=cens_pts$surv)

# ------------------------------------------------------------------
# KM plot with at-risk table (ggplot2 only)
# ------------------------------------------------------------------
# y positions for at-risk text below 0
y_arm1 <- -0.07
y_arm2 <- -0.14
p <- ggplot(plotdata, aes(x=time, y=surv, colour=arm, linetype=arm)) +
  geom_step(linewidth=0.9) +
  geom_point(data=cens_plot, aes(x=time, y=surv, colour=arm), shape=3, size=2.2, inherit.aes=FALSE) +
  scale_x_continuous(breaks=atrisk_times, limits=c(0,330)) +
  scale_y_continuous(breaks=seq(0,1,by=0.2), limits=c(-0.22,1.05)) +
  labs(title="Example K-M Plot", x="day", y="Survival Probability", colour="Stratum", linetype="Stratum") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="bottom") +
  geom_text(data=atrisk[atrisk$arm=="Arm 1",],
            aes(x=time, y=y_arm1, label=n), colour="black", size=3, inherit.aes=FALSE) +
  geom_text(data=atrisk[atrisk$arm=="Arm 2",],
            aes(x=time, y=y_arm2, label=n), colour="black", size=3, inherit.aes=FALSE) +
  annotate("text", x=-2, y=y_arm1, label="Arm 1", hjust=1, size=3) +
  annotate("text", x=-2, y=y_arm2, label="Arm 2", hjust=1, size=3) +
  annotate("text", x=-2, y= -0.005, label="At Risk", hjust=1, size=3, fontface="bold") +
  coord_cartesian(xlim=c(-28,330), clip="off")
ggsave(file.path(out, "P040_f_kmplot.png"), p, width=8, height=6, dpi=150)

write.csv(plotdata, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names=FALSE, na="")

# ------------------------------------------------------------------
# CSV of table body (product-limit estimates)
# ------------------------------------------------------------------
csv_body <- data.frame(
  ARM = pl$arm,
  DAY = pl$time,
  SURVIVAL = round_half_up(pl$surv, 4),
  FAILURE = round_half_up(pl$failure, 4),
  STDERR = round_half_up(pl$stderr, 4),
  N_FAILED = pl$n_failed,
  N_LEFT = pl$n_left,
  stringsAsFactors=FALSE
)
# ISO formatting for day (numeric, keep as is)
write.csv(csv_body, file.path(out, "P040_f_kmplot.csv"), row.names=FALSE, na="")

# ------------------------------------------------------------------
# Text rendering (SAS listing style, all default blocks)
# ------------------------------------------------------------------
txt <- file.path(out, "P040_f_kmplot.txt")
con <- file(txt, open="wt")

cat("Example K-M Plot\n", file=con)
cat("PROC LIFETEST  Product-Limit Survival Estimates  (TIME day*CENSOR(1) ; STRATA arm)\n", file=con)
cat("Plots = survival (atrisk = 0 to 330 by 30)\n", file=con)
cat("\n", file=con)

for (a in c("Arm 1","Arm 2")) {
  cat(sprintf("Stratum: %s\n", a), file=con)
  cat(sprintf("%-10s %10s %10s %12s %12s %12s\n",
              "day","Survival","Failure","StdErr","N_Failed","N_Left"), file=con)
  sub <- pl[pl$arm==a, ]
  sub <- sub[order(sub$time), ]
  for (i in seq_len(nrow(sub))) {
    star <- ifelse(sub$n_censor[i]>0 && sub$n_event[i]==0, "*", ifelse(sub$n_censor[i]>0, "+", " "))
    cat(sprintf("%-10s %10s %10s %12s %12d %12d%s\n",
                fmt_day(sub$time[i]),
                fmt4(sub$surv[i]),
                fmt4(sub$failure[i]),
                fmt4(sub$stderr[i]),
                sub$n_failed[i],
                sub$n_left[i],
                star), file=con)
  }
  cat("  * censored-only time ; + event and censored tied (censored after events)\n", file=con)
  cat("\n", file=con)
}

cat("Quartile Estimates (95% Confidence Interval, Transform = LOG-LOG)\n", file=con)
cat(sprintf("%-10s %-10s %-12s %-12s %-12s\n","Stratum","Percent","Estimate","Lower","Upper"), file=con)
for (i in seq_len(nrow(quart))) {
  cat(sprintf("%-10s %-10.0f %-12s %-12s %-12s\n",
              quart$arm[i], quart$percent[i],
              fmt_day(quart$estimate[i]),
              fmt_day(quart$lower[i]),
              fmt_day(quart$upper[i])), file=con)
}
cat("\n", file=con)

cat("Means (Restricted to Largest Event Time per Stratum)\n", file=con)
cat(sprintf("%-10s %-12s %-12s %-12s\n","Stratum","Tau","Mean","StdErr"), file=con)
for (i in seq_len(nrow(means))) {
  cat(sprintf("%-10s %-12s %-12s %-12s\n",
              means$arm[i],
              fmt_day(means$tau[i]),
              fmt4(means$mean[i]),
              fmt4(means$se[i])), file=con)
}
cat("\n", file=con)

cat("Summary of the Number of Censored and Uncensored Values\n", file=con)
cat(sprintf("%-10s %10s %10s %10s %14s\n","Stratum","Failed","Censored","Total","Pct_Cens"), file=con)
for (i in seq_len(nrow(cens_sum))) {
  cat(sprintf("%-10s %10d %10d %10d %14s\n",
              cens_sum$arm[i], cens_sum$failed[i], cens_sum$censored[i],
              cens_sum$total[i], fmt1(cens_sum$pct_cens[i])), file=con)
}
cat(sprintf("%-10s %10d %10d %10d %14s\n","Total", tot_f, tot_c, tot_t,
            fmt1(100*tot_c/tot_t)), file=con)
cat("\n", file=con)

cat("Rank Statistics (Observed vs Expected under H0)\n", file=con)
cat(sprintf("%-10s %12s %12s %14s %14s\n","Stratum","Observed","Expected","O-E(LogRank)","O-E(Wilcoxon)"), file=con)
# Wilcoxon weighted O-E per stratum
Uw1 <- U_w; Uw2 <- -U_w
cat(sprintf("%-10s %12d %12s %14s %14s\n","Arm 1", O1, fmt2(E1), fmt4(U_lr), fmt2(Uw1)), file=con)
cat(sprintf("%-10s %12d %12s %14s %14s\n","Arm 2", O2, fmt2(E2), fmt4(-U_lr), fmt2(Uw2)), file=con)
cat("\n", file=con)

cat("Covariance Matrix for the Log-Rank Statistics\n", file=con)
cat(sprintf("  Arm 1  %s\n", fmt4(V_lr)), file=con)
cat("Covariance Matrix for the Wilcoxon (Breslow) Statistics\n", file=con)
cat(sprintf("  Arm 1  %s\n", fmt2(V_w)), file=con)
cat("\n", file=con)

cat("Test of Equality over Strata\n", file=con)
cat(sprintf("%-12s %12s %6s %12s\n","Test","Chi-Square","DF","Pr > Chi-Square"), file=con)
cat(sprintf("%-12s %12s %6d %12s\n","Log-Rank", fmt4(chi_lr), 1, fmt4(p_lr)), file=con)
cat(sprintf("%-12s %12s %6d %12s\n","Wilcoxon", fmt4(chi_w), 1, fmt4(p_w)), file=con)
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
cat("Note: Censoring indicated by '+' on plot; survival step function with Greenwood standard errors.\n", file=con)
cat("Mean restricted to largest event time (Arm 1 tau=304, Arm 2 tau=323).\n", file=con)
close(con)
```