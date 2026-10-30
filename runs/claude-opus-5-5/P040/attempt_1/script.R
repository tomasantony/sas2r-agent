# P040 - Kaplan-Meier plot and PROC LIFETEST tables (translation of P040_kmplot.sas)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Round half away from zero, as SAS does
rhu <- function(x, d = 0) sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
f4  <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", rhu(x, 4)))
f3  <- function(x) ifelse(is.na(x), ".", sprintf("%.3f", rhu(x, 3)))
fbest <- function(x) ifelse(is.na(x), ".", format(x, trim = TRUE, drop0trailing = TRUE))

# ---------------- DATA one ----------------
# Note: "227.230" in the SAS DO list is a single value, 227.23
a1_ev <- c(143,164,188,188,190,192,206,213,216,220,227.23,234,246,265,304)
a2_ev <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
one <- rbind(
  data.frame(arm = "Arm 1", censor = 0, day = a1_ev),
  data.frame(arm = "Arm 1", censor = 1, day = c(216, 244)),
  data.frame(arm = "Arm 2", censor = 0, day = a2_ev),
  data.frame(arm = "Arm 2", censor = 1, day = c(204, 344)),
  stringsAsFactors = FALSE
)
cat("Data built:", nrow(one), "records\n")

# ---------------- Product-limit estimates ----------------
km_fit <- function(d) {
  d <- d[order(d$day, d$censor), ]          # events before censored at ties
  ev <- d[d$censor == 0, ]
  ut <- sort(unique(ev$day))
  Y <- sapply(ut, function(t) sum(d$day >= t))
  D <- sapply(ut, function(t) sum(ev$day == t))
  S <- cumprod(1 - D / Y)
  gsum <- cumsum(ifelse(Y - D > 0, D / (Y * (Y - D)), 0))
  SE <- S * sqrt(gsum)
  SE[S == 0] <- 0
  list(d = d, ut = ut, Y = Y, D = D, S = S, SE = SE)
}

pl_table <- function(d, armname) {
  f <- km_fit(d)
  dd <- f$d
  n <- nrow(dd)
  rows <- data.frame(arm = armname, day = 0, censored = "", Survival = 1, Failure = 0,
                     SurvSE = 0, NumFailed = 0, NumLeft = n, stringsAsFactors = FALSE)
  nfail <- 0
  for (i in seq_len(n)) {
    isev <- dd$censor[i] == 0
    if (isev) nfail <- nfail + 1
    last_ev <- isev && (i == n || !(dd$day[i + 1] == dd$day[i] && dd$censor[i + 1] == 0))
    if (last_ev) {
      k <- which(f$ut == dd$day[i])
      s <- f$S[k]; se <- f$SE[k]
    } else { s <- NA; se <- NA }
    rows <- rbind(rows, data.frame(arm = armname, day = dd$day[i],
                                   censored = ifelse(isev, "", "*"),
                                   Survival = s, Failure = ifelse(is.na(s), NA, 1 - s),
                                   SurvSE = se, NumFailed = nfail, NumLeft = n - i,
                                   stringsAsFactors = FALSE))
  }
  rows
}

# ---------------- Quartiles (LOGLOG CI) ----------------
quartiles <- function(f) {
  z <- qnorm(0.975)
  res <- NULL
  for (p in c(0.75, 0.5, 0.25)) {
    tgt <- 1 - p
    est <- NA
    idx <- which(f$S < tgt - 1e-10)
    eq  <- which(abs(f$S - tgt) < 1e-10)
    if (length(eq) > 0) {
      j <- eq[1]
      est <- if (j < length(f$ut)) (f$ut[j] + f$ut[j + 1]) / 2 else NA
    } else if (length(idx) > 0) est <- f$ut[idx[1]]
    ok <- f$S > 0 & f$S < 1
    g <- function(x) log(-log(x))
    inset <- rep(FALSE, length(f$ut))
    inset[ok] <- abs(g(f$S[ok]) - g(tgt)) <= z * f$SE[ok] / abs(f$S[ok] * log(f$S[ok])) + 1e-12
    lo <- hi <- NA
    if (any(inset)) {
      lo <- min(f$ut[inset])
      jl <- max(which(inset))
      hi <- if (jl < length(f$ut)) f$ut[jl + 1] else NA
    }
    res <- rbind(res, data.frame(Percent = p * 100, Estimate = est, LowerCL = lo, UpperCL = hi))
  }
  res
}

# ---------------- Mean (restricted to largest event time) ----------------
mean_km <- function(f, d) {
  tt <- f$ut; S <- f$S
  tprev <- c(0, tt[-length(tt)]); Sprev <- c(1, S[-length(S)])
  mu <- sum(Sprev * (tt - tprev))
  # A_j = area from t_j to last event time
  areas <- S[-length(S)] * diff(tt)
  A <- rev(cumsum(rev(c(areas, 0))))
  m <- sum(f$D)
  terms <- ifelse(f$Y - f$D > 0, A^2 * f$D / (f$Y * (f$Y - f$D)), 0)
  se <- sqrt(m / (m - 1) * sum(terms))
  restricted <- max(d$day) > max(tt)
  list(mean = mu, se = se, restricted = restricted, tmax = max(tt))
}

arms <- c("Arm 1", "Arm 2")
pl_all <- NULL; q_all <- NULL; m_all <- NULL; fits <- list()
for (k in seq_along(arms)) {
  d <- one[one$arm == arms[k], ]
  f <- km_fit(d); fits[[arms[k]]] <- f
  pl_all <- rbind(pl_all, pl_table(d, arms[k]))
  q <- quartiles(f); q$arm <- arms[k]; q$Stratum <- k
  q_all <- rbind(q_all, q)
  mk <- mean_km(f, d)
  m_all <- rbind(m_all, data.frame(Stratum = k, arm = arms[k], Mean = mk$mean, StdErr = mk$se,
                                   Restricted = mk$restricted, RestrictTime = mk$tmax))
}

# ---------------- Censoring summary ----------------
cens <- one %>% group_by(arm) %>%
  summarise(Total = n(), Failed = sum(censor == 0), Censored = sum(censor == 1), .groups = "drop") %>%
  mutate(Stratum = as.character(match(arm, arms)), PctCens = 100 * Censored / Total)
cens <- rbind(cens, data.frame(arm = "Total", Total = sum(cens$Total), Failed = sum(cens$Failed),
                               Censored = sum(cens$Censored), Stratum = "",
                               PctCens = 100 * sum(cens$Censored) / sum(cens$Total)))

# ---------------- Tests of equality ----------------
ev_times <- sort(unique(one$day[one$censor == 0]))
rank_stats <- function(wfun) {
  U <- c(0, 0); V <- matrix(0, 2, 2)
  for (t in ev_times) {
    nj <- sum(one$day >= t); dj <- sum(one$day == t & one$censor == 0)
    w <- wfun(nj)
    nk <- sapply(arms, function(a) sum(one$day >= t & one$arm == a))
    dk <- sapply(arms, function(a) sum(one$day == t & one$censor == 0 & one$arm == a))
    U <- U + w * (dk - nk * dj / nj)
    if (nj > 1) {
      fac <- w^2 * dj * (nj - dj) / (nj^2 * (nj - 1))
      V <- V + fac * (diag(nk * nj) - outer(nk, nk))
    }
  }
  list(U = U, V = V, chisq = U[1]^2 / V[1, 1])
}
lr <- rank_stats(function(n) 1)
wl <- rank_stats(function(n) n)
Dk <- sapply(arms, function(a) sum(one$censor == 0 & one$arm == a))
Tk <- sapply(arms, function(a) sum(one$day[one$arm == a]))
lrt <- 2 * (sum(Dk * log(Dk / Tk)) - sum(Dk) * log(sum(Dk) / sum(Tk)))
tests <- data.frame(Test = c("Log-Rank", "Wilcoxon", "-2Log(LR)"),
                    ChiSquare = c(lr$chisq, wl$chisq, lrt), DF = 1,
                    ProbChiSq = pchisq(c(lr$chisq, wl$chisq, lrt), 1, lower.tail = FALSE))

# ---------------- Text rendering ----------------
L <- c("Example K-M Plot", "", "The LIFETEST Procedure", "")
for (k in seq_along(arms)) {
  L <- c(L, sprintf("Stratum %d: arm = %s", k, arms[k]), "", "Product-Limit Survival Estimates", "",
         sprintf("%10s %10s %10s %10s %8s %8s", "day", "Survival", "Failure", "SurvSE", "NFailed", "NLeft"))
  p <- pl_all[pl_all$arm == arms[k], ]
  for (i in seq_len(nrow(p))) {
    L <- c(L, sprintf("%9s%1s %10s %10s %10s %8d %8d", fbest(p$day[i]), p$censored[i],
                      f4(p$Survival[i]), f4(p$Failure[i]), f4(p$SurvSE[i]),
                      as.integer(p$NumFailed[i]), as.integer(p$NumLeft[i])))
  }
  L <- c(L, "", "NOTE: The marked survival times are censored observations.", "",
         "Summary Statistics for Time Variable day", "", "Quartile Estimates",
         sprintf("%8s %12s %10s %10s", "Percent", "Point Est", "Lower95", "Upper95"))
  q <- q_all[q_all$arm == arms[k], ]
  for (i in seq_len(nrow(q)))
    L <- c(L, sprintf("%8d %12s %10s %10s", as.integer(q$Percent[i]), f3(q$Estimate[i]),
                      f3(q$LowerCL[i]), f3(q$UpperCL[i])))
  mm <- m_all[m_all$arm == arms[k], ]
  L <- c(L, "", sprintf("%12s %12s", "Mean", "Std Error"), sprintf("%12s %12s", f4(mm$Mean), f4(mm$StdErr)))
  if (mm$Restricted)
    L <- c(L, sprintf("NOTE: The mean survival time and its standard error were underestimated because the largest observation was censored and the estimation was restricted to the largest event time (%s).", fbest(mm$RestrictTime)))
  L <- c(L, "")
}
L <- c(L, "Summary of the Number of Censored and Uncensored Values", "",
       sprintf("%8s %8s %6s %7s %9s %9s", "Stratum", "arm", "Total", "Failed", "Censored", "PctCens"))
for (i in seq_len(nrow(cens)))
  L <- c(L, sprintf("%8s %8s %6d %7d %9d %9s", cens$Stratum[i], cens$arm[i], cens$Total[i],
                    cens$Failed[i], cens$Censored[i], f3(cens$PctCens[i])))
L <- c(L, "", "Testing Homogeneity of Survival Curves for day over Strata", "",
       "Rank Statistics", sprintf("%8s %12s %12s", "arm", "Log-Rank", "Wilcoxon"))
for (k in 1:2) L <- c(L, sprintf("%8s %12s %12s", arms[k], f4(lr$U[k]), f4(wl$U[k])))
L <- c(L, "", "Covariance Matrix for the Log-Rank Statistics")
for (k in 1:2) L <- c(L, sprintf("%8s %12s %12s", arms[k], f4(lr$V[k, 1]), f4(lr$V[k, 2])))
L <- c(L, "", "Covariance Matrix for the Wilcoxon Statistics")
for (k in 1:2) L <- c(L, sprintf("%8s %12s %12s", arms[k], f4(wl$V[k, 1]), f4(wl$V[k, 2])))
L <- c(L, "", "Test of Equality over Strata",
       sprintf("%10s %12s %4s %12s", "Test", "Chi-Square", "DF", "Pr>ChiSq"))
for (i in 1:3) L <- c(L, sprintf("%10s %12s %4d %12s", tests$Test[i], f4(tests$ChiSquare[i]),
                                 1L, f4(tests$ProbChiSq[i])))

# At-risk table (atrisk=0 to 330 by 30)
atr_t <- seq(0, 330, by = 30)
atrisk <- expand.grid(time = atr_t, arm = arms, stringsAsFactors = FALSE)
atrisk$n_at_risk <- mapply(function(t, a) sum(one$day >= t & one$arm == a), atrisk$time, atrisk$arm)
L <- c(L, "", "Product-Limit Survival Estimates - Number at Risk",
       paste(sprintf("%8s", c("arm", atr_t)), collapse = ""))
for (a in arms) L <- c(L, paste(sprintf("%8s", c(a, atrisk$n_at_risk[atrisk$arm == a])), collapse = ""))
writeLines(L, file.path(out, "P040_f_kmplot.txt"))

# ---------------- CSVs ----------------
write.csv(pl_all, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")
write.csv(q_all, file.path(out, "P040_f_kmplot_quartiles.csv"), row.names = FALSE, na = "")
write.csv(m_all, file.path(out, "P040_f_kmplot_means.csv"), row.names = FALSE, na = "")
write.csv(cens, file.path(out, "P040_f_kmplot_censsum.csv"), row.names = FALSE, na = "")
write.csv(tests, file.path(out, "P040_f_kmplot_tests.csv"), row.names = FALSE, na = "")
write.csv(atrisk, file.path(out, "P040_f_kmplot_atrisk.csv"), row.names = FALSE, na = "")

# ---------------- Figure ----------------
step_df <- NULL; cens_df <- NULL
for (a in arms) {
  f <- fits[[a]]
  step_df <- rbind(step_df, data.frame(arm = a, time = c(0, f$ut), surv = c(1, f$S)))
  d <- one[one$arm == a & one$censor == 1, ]
  sf <- stepfun(f$ut, c(1, f$S))
  cens_df <- rbind(cens_df, data.frame(arm = a, time = d$day, surv = sf(d$day)))
}
maxt <- tapply(one$day, one$arm, max)
step_df <- rbind(step_df, do.call(rbind, lapply(arms, function(a)
  data.frame(arm = a, time = maxt[[a]], surv = tail(fits[[a]]$S, 1)))))
step_df <- step_df[order(step_df$arm, step_df$time), ]
plotdat <- rbind(cbind(step_df, type = "step"), cbind(cens_df, type = "censored"))
write.csv(plotdat, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

ymin_txt <- -0.08
p <- ggplot(step_df, aes(time, surv, colour = arm)) +
  geom_step() +
  geom_point(data = cens_df, shape = 3, size = 3) +
  geom_text(data = atrisk, aes(x = time, y = ifelse(arm == "Arm 1", ymin_txt, 2 * ymin_txt),
                               label = n_at_risk, colour = arm), size = 3, inherit.aes = FALSE) +
  scale_x_continuous(breaks = atr_t, limits = c(0, 350)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  labs(title = "Example K-M Plot", subtitle = "Product-Limit Survival Estimates",
       x = "day", y = "Survival Probability", colour = "arm") +
  theme_bw()
ggsave(file.path(out, "P040_f_kmplot.png"), p, width = 8, height = 6, dpi = 100)

cat("P040 done\n")
