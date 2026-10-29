## P020 - Kaplan-Meier analysis of ADTTE by TRT01A (PROC LIFETEST reproduction)
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/derived/c2c/data/adam"
f_sas <- file.path(adam_dir, "adtte.sas7bdat")
f_xpt <- file.path(adam_dir, "adtte.xpt")
adtte <- if (file.exists(f_sas)) read_sas(f_sas) else read_xpt(f_xpt)
names(adtte) <- toupper(names(adtte))
cat("Read ADTTE:", nrow(adtte), "rows\n")

rhu <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
fmt <- function(x, d) ifelse(is.na(x), ".", formatC(rhu(x, d), format = "f", digits = d))

dat <- adtte %>%
  filter(!is.na(AVAL), !is.na(CNSR)) %>%
  mutate(TRT01A = as.character(TRT01A), EVENT = as.integer(CNSR != 1))
strata <- sort(unique(dat$TRT01A), method = "radix")
z <- qnorm(0.975)

## ---- KM per stratum ----
km_stratum <- function(d) {
  d <- d[order(d$AVAL, -d$EVENT), ]
  n <- nrow(d)
  et <- sort(unique(d$AVAL[d$EVENT == 1]))
  S <- 1; gw <- 0
  tab <- data.frame(t = et, n = NA, d = NA, S = NA, se = NA)
  for (j in seq_along(et)) {
    nj <- sum(d$AVAL >= et[j]); dj <- sum(d$AVAL == et[j] & d$EVENT == 1)
    S <- S * (1 - dj / nj)
    if (nj > dj) gw <- gw + dj / (nj * (nj - dj))
    tab$n[j] <- nj; tab$d[j] <- dj; tab$S[j] <- S
    tab$se[j] <- if (S > 0) S * sqrt(gw) else 0
  }
  # row-level listing
  rows <- data.frame(AVAL = 0, CENS = "", Survival = 1, Failure = 0, SE = 0,
                     Failed = 0, Left = n)
  cumf <- 0
  for (i in seq_len(n)) {
    ev <- d$EVENT[i] == 1
    if (ev) cumf <- cumf + 1
    last_tie <- ev && (i == n || !(d$AVAL[i + 1] == d$AVAL[i] && d$EVENT[i + 1] == 1))
    if (last_tie) {
      k <- which(tab$t == d$AVAL[i])
      rows <- rbind(rows, data.frame(AVAL = d$AVAL[i], CENS = "", Survival = tab$S[k],
                                     Failure = 1 - tab$S[k], SE = tab$se[k],
                                     Failed = cumf, Left = n - i))
    } else {
      rows <- rbind(rows, data.frame(AVAL = d$AVAL[i], CENS = ifelse(ev, "", "*"),
                                     Survival = NA, Failure = NA, SE = NA,
                                     Failed = cumf, Left = n - i))
    }
  }
  # quartiles
  qs <- do.call(rbind, lapply(c(75, 50, 25), function(p) {
    tgt <- 1 - p / 100
    pe <- NA
    eq <- which(abs(tab$S - tgt) < 1e-12)
    if (length(eq)) {
      j <- eq[1]; pe <- if (j < nrow(tab)) (tab$t[j] + tab$t[j + 1]) / 2 else NA
    } else {
      w <- which(tab$S < tgt); if (length(w)) pe <- tab$t[w[1]]
    }
    ok <- with(tab, S > 0 & S < 1 & se > 0)
    cond <- rep(FALSE, nrow(tab))
    cond[ok] <- abs(log(-log(tab$S[ok])) - log(-log(tgt))) <=
      z * tab$se[ok] / (tab$S[ok] * abs(log(tab$S[ok])))
    lo <- up <- NA
    if (any(cond)) {
      lo <- tab$t[min(which(cond))]
      jl <- max(which(cond)); up <- if (jl < nrow(tab)) tab$t[jl + 1] else NA
    }
    data.frame(Percent = p, Estimate = pe, Lower = lo, Upper = up)
  }))
  # restricted mean to largest event time
  mean_t <- NA; mean_se <- NA
  if (nrow(tab) > 0) {
    tt <- c(0, tab$t); SS <- c(1, tab$S)
    mean_t <- sum(SS[-length(SS)] * diff(tt))
    m <- sum(tab$d); v <- 0
    for (j in seq_len(nrow(tab))) {
      Aj <- if (j < nrow(tab)) sum(tab$S[j:(nrow(tab) - 1)] * diff(tab$t[j:nrow(tab)])) else 0
      if (tab$n[j] > tab$d[j]) v <- v + Aj^2 * tab$d[j] / (tab$n[j] * (tab$n[j] - tab$d[j]))
    }
    mean_se <- if (m > 1) sqrt(m / (m - 1) * v) else NA
  }
  list(tab = tab, rows = rows, qs = qs, mean = mean_t, mean_se = mean_se,
       n = n, nev = sum(d$EVENT), ncen = sum(1 - d$EVENT),
       lastcens = d$EVENT[n] == 0)
}

res <- lapply(strata, function(s) km_stratum(dat[dat$TRT01A == s, ]))
names(res) <- strata

## ---- Tests of equality ----
K <- length(strata)
allt <- sort(unique(dat$AVAL[dat$EVENT == 1]))
rank_test <- function(wfun) {
  U <- rep(0, K); V <- matrix(0, K, K)
  for (t in allt) {
    nk <- sapply(strata, function(s) sum(dat$AVAL[dat$TRT01A == s] >= t))
    dk <- sapply(strata, function(s) sum(dat$AVAL[dat$TRT01A == s] == t & dat$EVENT[dat$TRT01A == s] == 1))
    N <- sum(nk); D <- sum(dk); w <- wfun(N)
    U <- U + w * (dk - nk * D / N)
    if (N > 1) {
      fac <- w^2 * D * (N - D) / (N^2 * (N - 1))
      V <- V + fac * (diag(N * nk, K) - outer(nk, nk))
    }
  }
  list(U = U, V = V)
}
lr <- rank_test(function(N) 1); wl <- rank_test(function(N) N)
chisq_q <- function(rt) {
  idx <- 1:(K - 1)
  as.numeric(t(rt$U[idx]) %*% solve(rt$V[idx, idx, drop = FALSE]) %*% rt$U[idx])
}
Dk <- sapply(strata, function(s) sum(dat$EVENT[dat$TRT01A == s]))
Tk <- sapply(strata, function(s) sum(dat$AVAL[dat$TRT01A == s]))
term <- function(D, T) ifelse(D > 0, D * log(T / D), 0)
lr2 <- 2 * (term(sum(Dk), sum(Tk)) - sum(term(Dk, Tk)))
tests <- data.frame(Test = c("Log-Rank", "Wilcoxon", "-2Log(LR)"),
                    ChiSquare = c(chisq_q(lr), chisq_q(wl), lr2), DF = K - 1)
tests$ProbChiSq <- pchisq(tests$ChiSquare, tests$DF, lower.tail = FALSE)

## ---- Text output ----
L <- c("Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
       "Population: Safety Population", "", "The LIFETEST Procedure", "")
est_csv <- list(); q_csv <- list(); m_csv <- list()
for (i in seq_along(strata)) {
  r <- res[[i]]; s <- strata[i]
  L <- c(L, sprintf("Stratum %d: TRT01A = %s", i, s), "",
         "Product-Limit Survival Estimates", "",
         sprintf("%10s %10s %10s %10s %8s %8s", "AVAL", "Survival", "Failure",
                 "SE", "Failed", "Left"))
  rr <- r$rows
  for (k in seq_len(nrow(rr)))
    L <- c(L, sprintf("%9s%1s %10s %10s %10s %8d %8d", format(rr$AVAL[k]), rr$CENS[k],
                      ifelse(is.na(rr$Survival[k]), "", fmt(rr$Survival[k], 4)),
                      ifelse(is.na(rr$Failure[k]), "", fmt(rr$Failure[k], 4)),
                      ifelse(is.na(rr$SE[k]), "", fmt(rr$SE[k], 4)),
                      as.integer(rr$Failed[k]), as.integer(rr$Left[k])))
  L <- c(L, "", "NOTE: The marked survival times are censored observations.", "",
         "Summary Statistics for Time Variable AVAL", "", "Quartile Estimates",
         sprintf("%8s %10s %22s", "Percent", "Point Est", "95% CI [Lower Upper)"))
  for (k in 1:3) L <- c(L, sprintf("%8d %10s %10s %10s", r$qs$Percent[k],
                                   fmt(r$qs$Estimate[k], 3), fmt(r$qs$Lower[k], 3),
                                   fmt(r$qs$Upper[k], 3)))
  L <- c(L, "", sprintf("%10s %15s", "Mean", "Standard Error"),
         sprintf("%10s %15s", fmt(r$mean, 3), fmt(r$mean_se, 3)), "")
  if (r$lastcens) L <- c(L, "NOTE: The mean survival time and its standard error were underestimated because the largest observation was censored and the estimation was restricted to the largest event time.", "")
  est_csv[[i]] <- cbind(TRT01A = s, rr)
  q_csv[[i]] <- cbind(TRT01A = s, r$qs)
  m_csv[[i]] <- data.frame(TRT01A = s, Mean = r$mean, StdErr = r$mean_se)
}
cs <- data.frame(Stratum = seq_along(strata), TRT01A = strata,
                 Total = sapply(res, `[[`, "n"), Failed = sapply(res, `[[`, "nev"),
                 Censored = sapply(res, `[[`, "ncen"))
cs$PctCens <- 100 * cs$Censored / cs$Total
L <- c(L, "Summary of the Number of Censored and Uncensored Values", "",
       sprintf("%7s %-30s %6s %6s %8s %9s", "Stratum", "TRT01A", "Total", "Failed", "Censored", "Percent"))
for (k in seq_len(nrow(cs))) L <- c(L, sprintf("%7d %-30s %6d %6d %8d %9s", cs$Stratum[k],
                                           cs$TRT01A[k], cs$Total[k], cs$Failed[k],
                                           cs$Censored[k], fmt(cs$PctCens[k], 2)))
L <- c(L, sprintf("%7s %-30s %6d %6d %8d %9s", "", "Total", sum(cs$Total), sum(cs$Failed),
                  sum(cs$Censored), fmt(100 * sum(cs$Censored) / sum(cs$Total), 2)), "",
       "Testing Homogeneity of Survival Curves for AVAL over Strata", "",
       "Rank Statistics", sprintf("%-30s %12s %12s", "TRT01A", "Log-Rank", "Wilcoxon"))
for (k in seq_len(K)) L <- c(L, sprintf("%-30s %12s %12s", strata[k], fmt(lr$U[k], 4), fmt(wl$U[k], 3)))
for (nm in c("Log-Rank", "Wilcoxon")) {
  V <- if (nm == "Log-Rank") lr$V else wl$V
  L <- c(L, "", paste("Covariance Matrix for the", nm, "Statistics"))
  for (k in seq_len(K)) L <- c(L, sprintf("%-30s %s", strata[k],
                                          paste(sprintf("%12s", fmt(V[k, ], ifelse(nm == "Log-Rank", 4, 1))), collapse = " ")))
}
L <- c(L, "", "Test of Equality over Strata",
       sprintf("%-12s %12s %4s %12s", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
for (k in 1:3) L <- c(L, sprintf("%-12s %12s %4d %12s", tests$Test[k], fmt(tests$ChiSquare[k], 4),
                                 tests$DF[k], ifelse(tests$ProbChiSq[k] < 0.0001, "<.0001",
                                                     fmt(tests$ProbChiSq[k], 4))))
writeLines(L, file.path(out, "P020.txt"))

est_df <- do.call(rbind, est_csv)
write.csv(est_df, file.path(out, "P020.csv"), row.names = FALSE, na = "")
write.csv(do.call(rbind, q_csv), file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = "")
write.csv(do.call(rbind, m_csv), file.path(out, "P020_means.csv"), row.names = FALSE, na = "")
write.csv(cs, file.path(out, "P020_censsum.csv"), row.names = FALSE, na = "")
write.csv(tests, file.path(out, "P020_tests.csv"), row.names = FALSE, na = "")

## ---- Figure ----
plot_df <- do.call(rbind, lapply(strata, function(s) {
  tb <- res[[s]]$tab
  data.frame(TRT01A = s, AVAL = c(0, tb$t), SURVIVAL = c(1, tb$S))
}))
cens_df <- do.call(rbind, lapply(strata, function(s) {
  d <- dat[dat$TRT01A == s & dat$EVENT == 0, ]
  tb <- res[[s]]$tab
  if (nrow(d) == 0) return(NULL)
  sv <- sapply(d$AVAL, function(t) { w <- which(tb$t <= t); if (length(w)) tb$S[max(w)] else 1 })
  data.frame(TRT01A = s, AVAL = d$AVAL, SURVIVAL = sv)
}))
write.csv(plot_df, file.path(out, "P020_plot.csv"), row.names = FALSE, na = "")
p <- ggplot(plot_df, aes(AVAL, SURVIVAL, colour = TRT01A)) + geom_step() +
  { if (!is.null(cens_df)) geom_point(data = cens_df, shape = 3) } +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
       subtitle = "Population: Safety Population", x = "AVAL", y = "Survival Probability",
       colour = "TRT01A") + theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(out, "P020.png"), p, width = 7, height = 5.5, dpi = 150)
cat("P020 done\n")
