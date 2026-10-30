Fixing Greenwood standard errors and confidence limits to match SAS — rebuilding the KM tables.```r
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

# ------------------------------------------------------------------
# Product-limit with Greenwood SE (SAS formula) + log-log CI
# S = cumprod((n-d)/n); var_sum = cumsum(d/(n*(n-d))); SE = S*sqrt(var_sum)
# log-log CI with z=1.96
# ------------------------------------------------------------------
build_pl <- function(df, armval) {
  sub <- df[df$arm == armval, ]
  # distinct times sorted; SAS orders by time; censored tied after events (single row)
  times <- sort(unique(sub$day))
  n <- length(times)
  n_risk <- n_event <- n_censor <- rep(0L, n)
  for (i in seq_along(times)) {
    t <- times[i]
    n_risk[i]   <- sum(sub$day >= t - 1e-9)
    n_event[i]  <- sum(abs(sub$day - t) < 1e-9 & sub$event == 1)
    n_censor[i] <- sum(abs(sub$day - t) < 1e-9 & sub$event == 0)
  }
  surv <- rep(1, n); varsum <- rep(0, n)
  s <- 1; vs <- 0
  for (i in seq_along(times)) {
    d <- n_event[i]; nn <- n_risk[i]
    if (d > 0) {
      s <- s * (nn - d) / nn
      if (nn > d) vs <- vs + d / (nn * (nn - d))
      else vs <- Inf  # n==d : Greenwood term infinite
    }
    surv[i] <- s; varsum[i] <- vs
  }
  se <- surv * sqrt(varsum)
  # S==0 -> SAS shows missing SE (0*Inf = NaN)
  se[surv <= 1e-12] <- NA_real_
  se[!is.finite(se)] <- NA_real_
  # log-log CI
  z <- 1.96
  lower <- upper <- rep(NA_real_, n)
  for (i in seq_along(times)) {
    S <- surv[i]; vs2 <- varsum[i]
    if (is.finite(vs2) && S > 1e-12 && S < 1 - 1e-12) {
      a <- log(-log(S))
      se_a <- sqrt(vs2) / abs(log(S))
      lo <- exp(-exp(a + z * se_a))
      hi <- exp(-exp(a - z * se_a))
      lower[i] <- min(lo, hi); upper[i] <- max(lo, hi)
    } else if (abs(S - 1) < 1e-12) {
      lower[i] <- 1; upper[i] <- 1
    }
  }
  n_failed <- cumsum(n_event)
  n_left <- n_risk - n_event - n_censor
  data.frame(arm = armval, time = times, n_risk = n_risk, n_event = n_event,
             n_censor = n_censor, surv = surv, stderr = se,
             lower = lower, upper = upper,
             n_failed = n_failed, n_left = n_left,
             failure = 1 - surv, stringsAsFactors = FALSE)
}
pl1 <- build_pl(one, "Arm 1")
pl2 <- build_pl(one, "Arm 2")
pl <- rbind(pl1, pl2)
# pooled
one_pool <- one; one_pool$arm <- "Overall"
pl_pool <- build_pl(data.frame(arm = rep("Overall", nrow(one)),
                               day = one$day, event = one$event,
                               stringsAsFactors = FALSE), "Overall")

# Quartiles via Brookmeyer-Crowley on log-log bands (first time upper<=p etc.)
get_quant2 <- function(pldf, arms) {
  out <- list()
  for (a in arms) {
    sub <- pldf[pldf$arm == a, ]; sub <- sub[order(sub$time), ]
    for (p in c(0.75, 0.5, 0.25)) {
      idx <- which(sub$surv <= p + 1e-12)
      est <- if (length(idx) > 0) sub$time[min(idx)] else NA
      idxL <- which(!is.na(sub$upper) & sub$upper <= p + 1e-12)
      lo <- if (length(idxL) > 0) sub$time[min(idxL)] else NA
      idxU <- which(!is.na(sub$lower) & sub$lower <= p + 1e-12)
      hi <- if (length(idxU) > 0) sub$time[min(idxU)] else NA
      out[[length(out)+1]] <- data.frame(arm = a, percent = (1 - p) * 100,
                                         estimate = est, lower = lo, upper = hi,
                                         stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}
quart <- get_quant2(pl, c("Arm 1", "Arm 2"))
quart <- quart[order(quart$arm, -quart$percent), ]
quart_pool <- get_quant2(pl_pool, "Overall")

# Restricted means to largest event time
mean_calc <- function(pldf, a) {
  sub <- pldf[pldf$arm == a, ]; sub <- sub[order(sub$time), ]
  tau <- max(sub$time[sub$n_event > 0])
  sub2 <- sub[sub$time <= tau + 1e-9, ]
  t_all <- c(0, sub2$time); s_all <- c(1, sub2$surv)
  mu <- sum(s_all[seq_len(length(t_all)-1)] * diff(t_all))
  n2 <- nrow(sub2); A <- rep(0, n2)
  for (i in seq_len(n2)) {
    s_k <- s_all[(i+1):(n2+1)]; t_k <- t_all[(i+1):(n2+1)]
    t_next <- c(t_all[(i+2):(n2+1)], tau)
    A[i] <- sum(s_k * (t_next - t_k))
  }
  d <- sub2$n_event; nn <- sub2$n_risk
  term <- ifelse(d > 0 & nn > d, A^2 * d / (nn * (nn - d)), 0)
  se_mu <- sqrt(sum(term, na.rm = TRUE))
  data.frame(arm = a, tau = tau, mean = mu, se = se_mu, stringsAsFactors = FALSE)
}
means <- rbind(mean_calc(pl, "Arm 1"), mean_calc(pl, "Arm 2"))
means_pool <- mean_calc(pl_pool, "Overall")

cens_sum <- do.call(rbind, lapply(c("Arm 1","Arm 2"), function(a) {
  sub <- one[one$arm == a, ]
  f <- sum(sub$event == 1); c <- sum(sub$event == 0); t <- nrow(sub)
  data.frame(arm = a, failed = f, censored = c, total = t,
             pct_cens = 100 * c / t, stringsAsFactors = FALSE)
}))
tot_f <- sum(cens_sum$failed); tot_c <- sum(cens_sum$censored); tot_t <- sum(cens_sum$total)

ev_times <- sort(unique(one$day[one$event == 1]))
U_lr <- 0; V_lr <- 0; U_w <- 0; V_w <- 0
E1 <- 0; O1 <- 0; E2 <- 0; O2 <- 0
for (t in ev_times) {
  n1 <- sum(one$day >= t - 1e-9 & one$arm == "Arm 1")
  n2 <- sum(one$day >= t - 1e-9 & one$arm == "Arm 2")
  n <- n1 + n2
  d1 <- sum(abs(one$day - t) < 1e-9 & one$event == 1 & one$arm == "Arm 1")
  d2 <- sum(abs(one$day - t) < 1e-9 & one$event == 1 & one$arm == "Arm 2")
  d <- d1 + d2
  if (n == 0 || d == 0) next
  E1 <- E1 + n1 * d / n; E2 <- E2 + n2 * d / n
  O1 <- O1 + d1; O2 <- O2 + d2
  if (n <= 1) next
  e1 <- n1 * d / n
  v <- (n1 * n2 * d * (n - d)) / (n * n * (n - 1))
  U_lr <- U_lr + (d1 - e1); V_lr <- V_lr + v
  w <- n
  U_w <- U_w + w * (d1 - e1); V_w <- V_w + w * w * v
}
chi_lr <- ifelse(V_lr > 0, U_lr^2 / V_lr, NA)
chi_w  <- ifelse(V_w > 0, U_w^2 / V_w, NA)
p_lr <- 1 - pchisq(chi_lr, 1)
p_w  <- 1 - pchisq(chi_w, 1)
# Likelihood-ratio (-2Log) approximation via pooled vs stratified partial likelihood
# Use survdiff for log-rank already; compute LR test via coxph for completeness
lr_chi <- lr_p <- NA
try({
  cx <- survival::coxph(survival::Surv(day, event) ~ arm, data = one)
  s <- summary(cx)
  # score/logrank already; use likelihood ratio from loglik
  lr_chi <- 2 * (cx$loglik[2] - cx$loglik[1])
  lr_p <- 1 - pchisq(lr_chi, 1)
}, silent = TRUE)

atrisk_times <- seq(0, 330, by = 30)
atrisk <- expand.grid(time = atrisk_times, arm = c("Arm 1","Arm 2"), stringsAsFactors = FALSE)
atrisk$n <- mapply(function(t, a) sum(one$day >= t - 1e-9 & one$arm == a), atrisk$time, atrisk$arm)

plotdata_list <- list()
for (a in c("Arm 1","Arm 2")) {
  sub <- pl[pl$arm == a, ]; sub <- sub[order(sub$time), ]
  pd <- data.frame(arm = a, time = c(0, sub$time), surv = c(1, sub$surv),
                   stderr = c(0, sub$stderr),
                   lower = c(1, sub$lower), upper = c(1, sub$upper),
                   n_risk = c(sum(one$arm == a), sub$n_risk),
                   n_event = c(0, sub$n_event), n_censor = c(0, sub$n_censor),
                   stringsAsFactors = FALSE)
  plotdata_list[[a]] <- pd
}
plotdata <- do.call(rbind, plotdata_list)
cens_pts <- pl[pl$n_censor > 0, ]
cens_plot <- data.frame(arm = cens_pts$arm, time = cens_pts$time, surv = cens_pts$surv)

y_arm1 <- -0.07; y_arm2 <- -0.14
p <- ggplot(plotdata, aes(x = time, y = surv, colour = arm, linetype = arm)) +
  geom_step(linewidth = 0.9) +
  geom_point(data = cens_plot, aes(x = time, y = surv, colour = arm),
             shape = 3, size = 2.2, inherit.aes = FALSE) +
  scale_x_continuous(breaks = atrisk_times, limits = c(-30, 350)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(-0.22, 1.05)) +
  labs(title = "Example K-M Plot", x = "day", y = "Survival Probability",
       colour = "Stratum", linetype = "Stratum") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom") +
  geom_text(data = atrisk[atrisk$arm == "Arm 1", ],
            aes(x = time, y = y_arm1, label = n), colour = "black", size = 3, inherit.aes = FALSE) +
  geom_text(data = atrisk[atrisk$arm == "Arm 2", ],
            aes(x = time, y = y_arm2, label = n), colour = "black", size = 3, inherit.aes = FALSE) +
  annotate("text", x = -22, y = y_arm1, label = "Arm 1", hjust = 1, size = 3) +
  annotate("text", x = -22, y = y_arm2, label = "Arm 2", hjust = 1, size = 3) +
  annotate("text", x = -22, y = -0.005, label = "At Risk", hjust = 1, size = 3, fontface = "bold")
ggsave(file.path(out, "P040_f_kmplot.png"), p, width = 8, height = 6, dpi = 150)

write.csv(plotdata, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

csv_body <- data.frame(
  ARM = pl$arm, DAY = pl$time,
  SURVIVAL = round_half_up(pl$surv, 4),
  FAILURE = round_half_up(pl$failure, 4),
  STDERR = round_half_up(pl$stderr, 4),
  LOWER95 = round_half_up(pl$lower, 4),
  UPPER95 = round_half_up(pl$upper, 4),
  N_FAILED = pl$n_failed, N_LEFT = pl$n_left,
  stringsAsFactors = FALSE
)
write.csv(csv_body, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

txt <- file.path(out, "P040_f_kmplot.txt")
con <- file(txt, open = "wt")
cat("Example K-M Plot\n", file = con)
cat("PROC LIFETEST  Product-Limit Survival Estimates  (TIME day*CENSOR(1) ; STRATA arm)\n", file = con)
cat("Plots = survival (atrisk = 0 to 330 by 30)\n", file = con)
cat("\n", file = con)
for (a in c("Arm 1","Arm 2")) {
  nn <- sum(one$arm == a)
  cat(sprintf("Stratum: %s  (N = %d)\n", a, nn), file = con)
  cat(sprintf("%-10s %12s %12s %12s %12s %12s %10s %10s\n",
              "day","Survival","Failure","StdErr","Lower95","Upper95","N_Failed","N_Left"), file = con)
  sub <- pl[pl$arm == a, ]; sub <- sub[order(sub$time), ]
  for (i in seq_len(nrow(sub))) {
    star <- ifelse(sub$n_censor[i] > 0 && sub$n_event[i] == 0, "*",
                   ifelse(sub$n_censor[i] > 0, "+", " "))
    cat(sprintf("%-10s %12s %12s %12s %12s %12s %10d %10d%s\n",
                fmt_day(sub$time[i]),
                fmt4(sub$surv[i]), fmt4(sub$failure[i]), fmt4(sub$stderr[i]),
                fmt4(sub$lower[i]), fmt4(sub$upper[i]),
                sub$n_failed[i], sub$n_left[i], star), file = con)
  }
  cat("  * censored-only time ; + event and censored tied (censored after events)\n", file = con)
  cat("\n", file = con)
}
cat("Overall (pooled over strata, N = 38)\n", file = con)
cat(sprintf("%-10s %12s %12s %12s %12s %12s %10s %10s\n",
            "day","Survival","Failure","StdErr","Lower95","Upper95","N_Failed","N_Left"), file = con)
for (i in seq_len(nrow(pl_pool))) {
  star <- ifelse(pl_pool$n_censor[i] > 0 && pl_pool$n_event[i] == 0, "*",
                 ifelse(pl_pool$n_censor[i] > 0, "+", " "))
  cat(sprintf("%-10s %12s %12s %12s %12s %12s %10d %10d%s\n",
              fmt_day(pl_pool$time[i]),
              fmt4(pl_pool$surv[i]), fmt4(pl_pool$failure[i]), fmt4(pl_pool$stderr[i]),
              fmt4(pl_pool$lower[i]), fmt4(pl_pool$upper[i]),
              pl_pool$n_failed[i], pl_pool$n_left[i], star), file = con)
}
cat("\n", file = con)
cat("Quartile Estimates (95% Confidence Interval, Transform = LOG-LOG)\n", file = con)
cat(sprintf("%-10s %-10s %-12s %-12s %-12s\n","Stratum","Percent","Estimate","Lower","Upper"), file = con)
for (i in seq_len(nrow(quart))) {
  cat(sprintf("%-10s %-10.0f %-12s %-12s %-12s\n",
              quart$arm[i], quart$percent[i],
              fmt_day(quart$estimate[i]), fmt_day(quart$lower[i]), fmt_day(quart$upper[i])), file = con)
}
for (i in seq_len(nrow(quart_pool))) {
  cat(sprintf("%-10s %-10.0f %-12s %-12s %-12s\n",
              quart_pool$arm[i], quart_pool$percent[i],
              fmt_day(quart_pool$estimate[i]), fmt_day(quart_pool$lower[i]), fmt_day(quart_pool$upper[i])), file = con)
}
cat("\n", file = con)
cat("Means (Restricted to Largest Event Time per Stratum)\n", file = con)
cat(sprintf("%-10s %-12s %-12s %-12s\n","Stratum","Tau","Mean","StdErr"), file = con)
for (i in seq_len(nrow(means))) {
  cat(sprintf("%-10s %-12s %-12s %-12s\n",
              means$arm[i], fmt_day(means$tau[i]),
              fmt4(means$mean[i]), fmt4(means$se[i])), file = con)
}
cat(sprintf("%-10s %-12s %-12s %-12s\n",
            means_pool$arm, fmt_day(means_pool$tau),
            fmt4(means_pool$mean), fmt4(means_pool$se)), file = con)
cat("\n", file = con)
cat("Summary of the Number of Censored and Uncensored Values\n", file = con)
cat(sprintf("%-10s %10s %10s %10s %14s\n","Stratum","Failed","Censored","Total","Pct_Cens"), file = con)
for (i in seq_len(nrow(cens_sum))) {
  cat(sprintf("%-10s %10d %10d %10d %14s\n",
              cens_sum$arm[i], cens_sum$failed[i], cens_sum$censored[i],
              cens_sum$total[i], fmt1(cens_sum$pct_cens[i])), file = con)
}
cat(sprintf("%-10s %10d %10d %10d %14s\n","Total", tot_f, tot_c, tot_t,
            fmt1(100 * tot_c / tot_t)), file = con)
cat("\n", file = con)
cat("Rank Statistics (Observed vs Expected under H0)\n", file = con)
cat(sprintf("%-10s %12s %12s %14s %14s\n","Stratum","Observed","Expected","O-E(LogRank)","O-E(Wilcoxon)"), file = con)
Uw1 <- U_w; Uw2 <- -U_w
cat(sprintf("%-10s %12d %12s %14s %14s\n","Arm 1", O1, fmt4(E1), fmt4(U_lr), fmt2(Uw1)), file = con)
cat(sprintf("%-10s %12d %12s %14s %14s\n","Arm 2", O2, fmt4(E2), fmt4(-U_lr), fmt2(Uw2)), file = con)
cat("\n", file = con)
cat("Covariance Matrix for the Log-Rank Statistics\n", file = con)
cat(sprintf("  Arm 1  %s\n", fmt4(V_lr)), file = con)
cat("Covariance Matrix for the Wilcoxon (Breslow) Statistics\n", file = con)
cat(sprintf("  Arm 1  %s\n", fmt2(V_w)), file = con)
cat("\n", file = con)
cat("Test of Equality over Strata\n", file = con)
cat(sprintf("%-16s %12s %6s %12s\n","Test","Chi-Square","DF","Pr > Chi-Square"), file = con)
cat(sprintf("%-16s %12s %6d %12s\n","Log-Rank", fmt4(chi_lr), 1, fmt4(p_lr)), file = con)
cat(sprintf("%-16s %12s %6d %12s\n","Wilcoxon", fmt4(chi_w), 1, fmt4(p_w)), file = con)
if (!is.na(lr_chi)) cat(sprintf("%-16s %12s %6d %12s\n","Likelihood-Ratio", fmt4(lr_chi), 1, fmt4(lr_p)), file = con)
cat("\n", file = con)
cat("Number at Risk (atrisk = 0 to 330 by 30)\n", file = con)
cat(sprintf("%-10s", "Time"), file = con)
for (t in atrisk_times) cat(sprintf(" %6d", t), file = con)
cat("\n", file = con)
for (a in c("Arm 1","Arm 2")) {
  cat(sprintf("%-10s", a), file = con)
  vals <- atrisk$n[atrisk$arm == a][order(atrisk$time[atrisk$arm == a])]
  for (v in vals) cat(sprintf(" %6d", v), file = con)
  cat("\n", file = con)
}
cat("\n", file = con)
cat("Listing of input data ONE (day*censor(1))\n", file = con)
cat(sprintf("%-10s %-10s %-10s\n","arm","day","censor"), file = con)
oo <- one[order(one$arm, one$day, one$censor), ]
for (i in seq_len(nrow(oo))) {
  cat(sprintf("%-10s %-10s %-10d\n", oo$arm[i], fmt_day(oo$day[i]), oo$censor[i]), file = con)
}
cat("\n", file = con)
cat("Note: Censoring indicated by '+' on plot; survival step function with Greenwood standard errors.\n", file = con)
cat("Mean restricted to largest event time (Arm 1 tau=304, Arm 2 tau=323).\n", file = con)
close(con)
```