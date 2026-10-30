```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(ggplot2))

round_half_up <- function(x, digits = 0) {
  y <- x
  idx <- !is.na(x) & is.finite(x)
  xx <- x[idx]
  s <- sign(xx)
  pow <- 10 ^ digits
  y[idx] <- s * floor(abs(xx) * pow + 0.5 + 1e-9) / pow
  y
}
fmt4 <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", round_half_up(x, 4)))
fmt3 <- function(x) ifelse(is.na(x), ".", sprintf("%.3f", round_half_up(x, 3)))
fmt2 <- function(x) ifelse(is.na(x), ".", sprintf("%.2f", round_half_up(x, 2)))
pfmt <- function(p) ifelse(is.na(p), ".", ifelse(p < 0.0001, "<.0001", sprintf("%.4f", round_half_up(p, 4))))

# ---- data one (SAS data step) ----
arm1_ev <- c(143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304)
arm1_ce <- c(216,244)
arm2_ev <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
arm2_ce <- c(204,344)
one <- data.frame(
  arm = c(rep("Arm 1", length(arm1_ev) + length(arm1_ce)),
          rep("Arm 2", length(arm2_ev) + length(arm2_ce))),
  day = c(arm1_ev, arm1_ce, arm2_ev, arm2_ce),
  censor = c(rep(0, length(arm1_ev)), rep(1, length(arm1_ce)),
             rep(0, length(arm2_ev)), rep(1, length(arm2_ce))),
  stringsAsFactors = FALSE
)

arm1_df <- one[one$arm == "Arm 1", ]
arm2_df <- one[one$arm == "Arm 2", ]

km_table_fun <- function(df_arm) {
  times <- sort(unique(df_arm$day))
  N <- nrow(df_arm)
  S <- 1
  gsum <- 0
  failed_cum <- 0L
  outl <- vector("list", length(times))
  k <- 0L
  for (t in times) {
    n <- sum(df_arm$day >= t)
    d <- sum(df_arm$day == t & df_arm$censor == 0)
    cc <- sum(df_arm$day == t & df_arm$censor == 1)
    if (d > 0) {
      if (n == d) {
        S <- 0
        gsum <- Inf
        SE <- NA_real_
      } else {
        gsum <- gsum + d / (n * (n - d))
        S <- S * (1 - d / n)
        SE <- S * sqrt(gsum)
      }
      failed_cum <- failed_cum + d
    } else {
      if (gsum == 0) SE <- 0
      else if (is.infinite(gsum)) SE <- NA_real_
      else SE <- S * sqrt(gsum)
    }
    left <- sum(df_arm$day > t)
    if (!is.na(S) && S == 1) {
      lower <- 1; upper <- 1
      if (is.na(SE)) SE <- 0
    } else if (is.na(S) || S <= 0 || S >= 1) {
      lower <- NA_real_; upper <- NA_real_
      if (!is.na(S) && S == 0) SE <- NA_real_
    } else {
      if (is.na(SE)) { lower <- NA_real_; upper <- NA_real_ }
      else {
        theta <- log(-log(S))
        se_theta <- SE / (-S * log(S))
        z <- 1.96
        lower <- exp(-exp(theta + z * se_theta))
        upper <- exp(-exp(theta - z * se_theta))
        lower <- max(0, min(1, lower))
        upper <- max(0, min(1, upper))
      }
    }
    k <- k + 1L
    outl[[k]] <- data.frame(time = t, n = n, d = d, c = cc, surv = S,
      failure = 1 - S, se = SE, lower = lower, upper = upper,
      failed = failed_cum, left = left, stringsAsFactors = FALSE)
  }
  do.call(rbind, outl)
}

km1 <- km_table_fun(arm1_df)
km2 <- km_table_fun(arm2_df)

quartile_fun <- function(km) {
  probs <- c(0.75, 0.50, 0.25)
  res <- NULL
  for (p in probs) {
    est <- suppressWarnings(min(km$time[km$surv <= p], na.rm = TRUE))
    if (!is.finite(est)) est <- NA_real_
    il <- which(!is.na(km$lower) & km$lower <= p)
    low <- if (length(il) == 0) NA_real_ else min(km$time[il])
    iu <- which(!is.na(km$upper) & km$upper <= p)
    upp <- if (length(iu) == 0) NA_real_ else min(km$time[iu])
    if (is.na(est)) { low <- NA_real_; upp <- NA_real_ }
    res <- rbind(res, data.frame(percent = p * 100, est = est, lower = low, upper = upp))
  }
  res
}
q1 <- quartile_fun(km1)
q2 <- quartile_fun(km2)

mean_fun <- function(df_arm, km) {
  ev <- sort(unique(df_arm$day[df_arm$censor == 0]))
  tau <- max(ev)
  S_after <- sapply(ev, function(t) km$surv[km$time == t][1])
  S_before <- c(1, S_after[-length(S_after)])
  diffs <- diff(c(0, ev))
  mval <- sum(S_before * diffs)
  cum <- cumsum(S_before * diffs)
  A <- mval - cum
  n_i <- sapply(ev, function(t) sum(df_arm$day >= t))
  d_i <- sapply(ev, function(t) sum(df_arm$day == t & df_arm$censor == 0))
  terms <- mapply(function(Ai, ni, di) {
    if (Ai == 0) return(0)
    if (ni == di) return(0)
    Ai^2 * di / (ni * (ni - di))
  }, A, n_i, d_i)
  m <- sum(df_arm$censor == 0)
  vse <- m / (m - 1) * sum(terms)
  list(mean = mval, se = sqrt(vse), tau = tau)
}
mn1 <- mean_fun(arm1_df, km1)
mn2 <- mean_fun(arm2_df, km2)

# ---- summary censored ----
sum_fun <- function(df_arm) {
  tot <- nrow(df_arm); f <- sum(df_arm$censor == 0); c <- sum(df_arm$censor == 1)
  pct <- 100 * c / tot
  c(total = tot, failed = f, cens = c, pct = pct)
}
s1 <- sum_fun(arm1_df); s2 <- sum_fun(arm2_df)

# ---- log-rank / wilcoxon ----
ev_all <- sort(unique(one$day[one$censor == 0]))
U1 <- 0; V1 <- 0; W1 <- 0; Vw1 <- 0; E1 <- 0; E2 <- 0; O1 <- 0; O2 <- 0
for (t in ev_all) {
  n <- sum(one$day >= t)
  d <- sum(one$day == t & one$censor == 0)
  n1 <- sum(arm1_df$day >= t)
  n2 <- sum(arm2_df$day >= t)
  d1 <- sum(arm1_df$day == t & arm1_df$censor == 0)
  d2 <- sum(arm2_df$day == t & arm2_df$censor == 0)
  e1 <- n1 * d / n
  e2 <- n2 * d / n
  E1 <- E1 + e1; E2 <- E2 + e2; O1 <- O1 + d1; O2 <- O2 + d2
  diff1 <- d1 - e1
  U1 <- U1 + diff1
  W1 <- W1 + n * diff1
  if (n > 1) {
    v <- n1 * (n - n1) * d * (n - d) / (n^2 * (n - 1))
    if (!is.na(v)) { V1 <- V1 + v; Vw1 <- Vw1 + n^2 * v }
  }
}
U2 <- -U1; W2 <- -W1
V11 <- V1; V22 <- V1; V12 <- -V1
Wv11 <- Vw1; Wv22 <- Vw1; Wv12 <- -Vw1
chi_log <- if (V1 > 0) U1^2 / V1 else NA_real_
chi_w <- if (Vw1 > 0) W1^2 / Vw1 else NA_real_
p_log <- 1 - pchisq(chi_log, 1)
p_w <- 1 - pchisq(chi_w, 1)

# ---- -2Log(LR) exponential ----
T1 <- sum(arm1_df$day); D1 <- sum(arm1_df$censor == 0)
T2 <- sum(arm2_df$day); D2 <- sum(arm2_df$censor == 0)
TT <- T1 + T2; DD <- D1 + D2
chi_lr <- 2 * (D1 * log(D1 / T1) + D2 * log(D2 / T2) - DD * log(DD / TT))
p_lr <- 1 - pchisq(chi_lr, 1)

# ---- at-risk 0 to 330 by 30 ----
atrisk_times <- seq(0, 330, 30)
ar1 <- sapply(atrisk_times, function(t) sum(arm1_df$day >= t))
ar2 <- sapply(atrisk_times, function(t) sum(arm2_df$day >= t))

# ---- plot data ----
steps1 <- data.frame(arm = "Arm 1", time = c(0, km1$time), surv = c(1, km1$surv))
steps2 <- data.frame(arm = "Arm 2", time = c(0, km2$time), surv = c(1, km2$surv))
steps_all <- rbind(steps1, steps2)
cens1 <- km1[km1$c > 0, ]; cens2 <- km2[km2$c > 0, ]
cens_pts <- rbind(
  if (nrow(cens1) > 0) data.frame(arm = "Arm 1", time = cens1$time, surv = cens1$surv) else NULL,
  if (nrow(cens2) > 0) data.frame(arm = "Arm 2", time = cens2$time, surv = cens2$surv) else NULL
)
risk_df <- data.frame(
  arm = rep(c("Arm 1", "Arm 2"), each = length(atrisk_times)),
  time_at = rep(atrisk_times, 2),
  atrisk = c(ar1, ar2),
  ypos = rep(c(-0.05, -0.16), each = length(atrisk_times))
)
lab_df <- data.frame(x = -28, y = c(-0.05, -0.16), label = c("Arm 1", "Arm 2"))

p <- ggplot() +
  geom_step(data = steps_all, aes(x = time, y = surv, color = arm), linewidth = 0.9, direction = "hv") +
  geom_point(data = cens_pts, aes(x = time, y = surv, color = arm), shape = 3, size = 2.6, stroke = 1.1, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 330, 30), limits = c(-32, 355)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(-0.28, 1.05)) +
  scale_color_manual(values = c("Arm 1" = "#1F77B4", "Arm 2" = "#D62728")) +
  labs(title = "Example K-M Plot", x = "Day", y = "Survival Probability", color = "") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top") +
  geom_text(data = risk_df, aes(x = time_at, y = ypos, label = atrisk, color = arm), show.legend = FALSE, size = 3) +
  geom_text(data = lab_df, aes(x = x, y = y, label = label), size = 3, hjust = 0, inherit.aes = FALSE) +
  annotate("text", x = -28, y = 0.03, label = "Number at risk", size = 3, hjust = 0)

ggsave(file.path(out, "P040_f_kmplot.png"), plot = p, width = 10, height = 6, dpi = 150)
message("plot saved")

plotdata_out <- rbind(
  data.frame(arm = steps_all$arm, time = steps_all$time, survival = steps_all$surv, type = "SURVIVAL"),
  data.frame(arm = risk_df$arm, time = risk_df$time_at, survival = risk_df$atrisk, type = "ATRISK")
)
write.csv(plotdata_out, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

# ---- table CSV (product-limit body) ----
tab_csv <- rbind(
  data.frame(arm = "Arm 1", day = km1$time, censored = ifelse(km1$c > 0, "*", ""),
    survival = round_half_up(km1$surv, 4), failure = round_half_up(km1$failure, 4),
    stderr = round_half_up(km1$se, 4), lower95 = round_half_up(km1$lower, 4),
    upper95 = round_half_up(km1$upper, 4), failed = km1$failed, left = km1$left,
    stringsAsFactors = FALSE),
  data.frame(arm = "Arm 2", day = km2$time, censored = ifelse(km2$c > 0, "*", ""),
    survival = round_half_up(km2$surv, 4), failure = round_half_up(km2$failure, 4),
    stderr = round_half_up(km2$se, 4), lower95 = round_half_up(km2$lower, 4),
    upper95 = round_half_up(km2$upper, 4), failed = km2$failed, left = km2$left,
    stringsAsFactors = FALSE)
)
write.csv(tab_csv, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

# ---- text rendering ----
L <- c()
L <- c(L, "Example K-M Plot",
  "",
  "PROC LIFETEST  Product-Limit Survival Estimates",
  "TIME DAY*CENSOR(1)   Plots=Survival (atrisk=0 to 330 by 30)",
  "")
mk_block <- function(km, arm) {
  b <- c(paste0("Stratum: ", arm),
    sprintf("%-10s %-10s %-10s %-10s %-10s %-10s %-8s %-8s", "Day", "Survival", "Failure", "StdErr", "Lower95", "Upper95", "Failed", "Left"))
  for (i in seq_len(nrow(km))) {
    star <- ifelse(km$c[i] > 0, "*", " ")
    b <- c(b, sprintf("%-10s %-10s %-10s %-10s %-10s %-10s %-8d %-8d",
      paste0(if (km$time[i] == floor(km$time[i])) sprintf("%d", as.integer(km$time[i])) else sprintf("%g", km$time[i]), star),
      fmt4(km$surv[i]), fmt4(km$failure[i]), fmt4(km$se[i]), fmt4(km$lower[i]), fmt4(km$upper[i]),
      km$failed[i], km$left[i]))
  }
  b
}
L <- c(L, mk_block(km1, "Arm 1"), "", mk_block(km2, "Arm 2"), "")
L <- c(L, "Quartile Estimates",
  sprintf("%-12s %-10s %-12s %-12s %-12s", "Stratum", "Percent", "Estimate", "Lower95", "Upper95"))
for (i in seq_len(nrow(q1))) L <- c(L, sprintf("%-12s %-10d %-12s %-12s %-12s", "Arm 1", as.integer(q1$percent[i]), fmt3(q1$est[i]), fmt3(q1$lower[i]), fmt3(q1$upper[i])))
for (i in seq_len(nrow(q2))) L <- c(L, sprintf("%-12s %-10d %-12s %-12s %-12s", "Arm 2", as.integer(q2$percent[i]), fmt3(q2$est[i]), fmt3(q2$lower[i]), fmt3(q2$upper[i])))
L <- c(L, "", "Means (Restricted to largest event time)",
  sprintf("%-12s %-12s %-12s", "Stratum", "Mean", "StdErr"),
  sprintf("%-12s %-12s %-12s", "Arm 1", fmt3(mn1$mean), fmt3(mn1$se)),
  sprintf("%-12s %-12s %-12s", "Arm 2", fmt3(mn2$mean), fmt3(mn2$se)),
  "")
L <- c(L, "Summary of the Number of Censored and Uncensored Values",
  sprintf("%-12s %-8s %-8s %-10s %-12s", "Stratum", "Total", "Failed", "Censored", "PctCens"),
  sprintf("%-12s %-8d %-8d %-10d %-12s", "Arm 1", s1["total"], s1["failed"], s1["cens"], fmt2(s1["pct"])),
  sprintf("%-12s %-8d %-8d %-10d %-12s", "Arm 2", s2["total"], s2["failed"], s2["cens"], fmt2(s2["pct"])),
  sprintf("%-12s %-8d %-8d %-10d %-12s", "Total", s1["total"] + s2["total"], s1["failed"] + s2["failed"], s1["cens"] + s2["cens"], fmt2(100 * (s1["cens"] + s2["cens"]) / (s1["total"] + s2["total"]))),
  "")
L <- c(L, "Rank Statistics",
  sprintf("%-12s %-12s %-12s %-12s %-12s", "Stratum", "Observed", "Expected", "Log-Rank", "Wilcoxon"),
  sprintf("%-12s %-12d %-12s %-12s %-12s", "Arm 1", O1, fmt4(E1), fmt4(U1), fmt4(W1)),
  sprintf("%-12s %-12d %-12s %-12s %-12s", "Arm 2", O2, fmt4(E2), fmt4(U2), fmt4(W2)),
  "")
L <- c(L, "Covariance Matrix for the Log-Rank Statistics",
  sprintf("%-12s %-12s %-12s", "", "Arm 1", "Arm 2"),
  sprintf("%-12s %-12s %-12s", "Arm 1", fmt4(V11), fmt4(V12)),
  sprintf("%-12s %-12s %-12s", "Arm 2", fmt4(V12), fmt4(V22)),
  "",
  "Covariance Matrix for the Wilcoxon Statistics",
  sprintf("%-12s %-12s %-12s", "", "Arm 1", "Arm 2"),
  sprintf("%-12s %-12s %-12s", "Arm 1", fmt4(Wv11), fmt4(Wv12)),
  sprintf("%-12s %-12s %-12s", "Arm 2", fmt4(Wv12), fmt4(Wv22)),
  "")
L <- c(L, "Test of Equality over Strata",
  sprintf("%-12s %-12s %-4s %-12s", "Test", "ChiSquare", "DF", "Pr>ChiSq"),
  sprintf("%-12s %-12s %-4d %-12s", "Log-Rank", fmt4(chi_log), 1, pfmt(p_log)),
  sprintf("%-12s %-12s %-4d %-12s", "Wilcoxon", fmt4(chi_w), 1, pfmt(p_w)),
  sprintf("%-12s %-12s %-4d %-12s", "-2Log(LR)", fmt4(chi_lr), 1, pfmt(p_lr)),
  "")
L <- c(L, "Number at Risk (atrisk=0 to 330 by 30)",
  paste0("Time: ", paste(atrisk_times, collapse = " ")),
  paste0("Arm 1: ", paste(ar1, collapse = " ")),
  paste0("Arm 2: ", paste(ar2, collapse = " ")),
  "",
  "NOTE: Censored values are marked with *.")
L <- c(L, paste0("NOTE: Mean restricted to largest event time (Arm 1 tau=", fmt3(mn1$tau), ", Arm 2 tau=", fmt3(mn2$tau), ")."))

writeLines(L, file.path(out, "P040_f_kmplot.txt"))
message("done")
```