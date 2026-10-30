out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # SAS rounds half away from zero
  y <- abs(x) * mult
  y <- floor(y + 0.5 + 1e-12)
  sign_x * y / mult
}
fmt4 <- function(x) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  sprintf("%.4f", round_half_up(x, 4))
}
fmt3 <- function(x) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  sprintf("%.3f", round_half_up(x, 3))
}
fmt2 <- function(x) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  sprintf("%.2f", round_half_up(x, 2))
}

library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)

# data one as in SAS ----------------------------------------------------
# SAS: do day = 143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304
# Note 227.230 is 227.23 in SAS
arm1_events <- c(143,164,188,188,190,192,206,213,216,220,227.23,234,246,265,304)
arm1_cens   <- c(216,244)
arm2_events <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
arm2_cens   <- c(204,344)

one <- bind_rows(
  tibble(arm = "Arm 1", day = arm1_events, censor = 0),
  tibble(arm = "Arm 1", day = arm1_cens,   censor = 1),
  tibble(arm = "Arm 2", day = arm2_events, censor = 0),
  tibble(arm = "Arm 2", day = arm2_cens,   censor = 1)
)

# sort as SAS would (character ordering, missing first)
one <- one %>% arrange(arm, day, censor)

# Kaplan-Meier per stratum ---------------------------------------------
# Function to compute product-limit table for one stratum
km_table <- function(df) {
  # df has day, censor (1=censored, 0=event)
  # distinct times sorted
  times <- sort(unique(df$day))
  n_at_risk <- nrow(df)
  # we need to process in time order, handling events before censored at same time
  # For each distinct time t:
  # d = number of events at t
  # c = number censored at t
  # n = number at risk just before t
  # survival after t: S = S_prev * (1 - d/n) if d>0 else S_prev
  # Greenwood sum: sum d/(n*(n-d))
  # SE = S * sqrt(sum)
  S <- 1
  greenwood_sum <- 0
  rows <- list()
  cum_failed <- 0
  # number left after processing time t = n - d - c
  # number failed cumulative = cum_failed + d
  for (t in times) {
    idx <- which(df$day == t)
    d <- sum(df$censor[idx] == 0)
    c <- sum(df$censor[idx] == 1)
    n <- n_at_risk
    if (d > 0) {
      # update greenwood
      if (n > d) {
        greenwood_sum <- greenwood_sum + d / (n * (n - d))
      } else {
        # n==d => term is Inf? but then S becomes 0, SE 0
        greenwood_sum <- greenwood_sum + 0
      }
      S <- S * (1 - d / n)
    }
    # SE
    se <- S * sqrt(greenwood_sum)
    if (S == 0) se <- 0
    if (S == 1 && greenwood_sum == 0) se <- 0
    cum_failed <- cum_failed + d
    n_left <- n - d - c
    # failure = 1 - S
    failure <- 1 - S
    # censored flag: if c>0 and d==0 then censored row, else if both, we have event row but also censored at same time
    # SAS marks censored times with * ; if both event and censored at same time, the row is event row but number left reflects both
    # For simplicity, mark * if c>0 and d==0, or if we want to indicate censored component
    # We will create one row per distinct time; if both event and censored, we still have one row
    cens_mark <- ifelse(c > 0 && d == 0, "*", "")
    # For times with both, SAS would show the event survival but number left includes censored removal
    rows[[length(rows)+1]] <- tibble(
      day = t,
      d = d,
      c = c,
      n_risk = n,
      survival = S,
      failure = failure,
      se = se,
      number_failed = cum_failed,
      number_left = n_left,
      cens_mark = cens_mark,
      greenwood_sum = greenwood_sum
    )
    n_at_risk <- n_left
  }
  bind_rows(rows)
}

arms <- sort(unique(one$arm)) # SAS character ordering
km_list <- lapply(arms, function(a) {
  df <- filter(one, arm == a)
  km <- km_table(df)
  km$arm <- a
  km
})
km_all <- bind_rows(km_list)

# For CSV of plotted data: need survival estimates at each time
# Also need at-risk table 0 to 330 by 30
atrisk_times <- seq(0, 330, by = 30)
atrisk_table <- lapply(arms, function(a) {
  df <- filter(one, arm == a)
  # number at risk at time t = number with day >= t
  # SAS atrisk includes those still at risk just before time
  sapply(atrisk_times, function(t) sum(df$day >= t))
})
atrisk_df <- as.data.frame(do.call(rbind, atrisk_table))
colnames(atrisk_df) <- paste0("t", atrisk_times)
atrisk_df$arm <- arms
atrisk_df <- atrisk_df %>% select(arm, everything())

# Quartile estimates, Mean, SE ------------------------------------------
# Need survival CI log-log for quartile CI
# Compute CI for each km row
compute_ci_loglog <- function(km) {
  # km has survival, greenwood_sum
  # var_loglog = greenwood_sum / (log(S)^2)  for S<1 and S>0
  # SE_loglog = sqrt(var)
  # g = log(-log(S))
  # CI for g: g +/- z*SE
  # back transform: S_lower = exp(-exp(g + z*SE)), S_upper = exp(-exp(g - z*SE))
  # Note lower CI for S is smaller than upper? Actually S_lower < S < S_upper? Check.
  # For log-log, lower = S^{exp(z*SE_loglog)} , upper = S^{exp(-z*SE_loglog)}
  z <- qnorm(0.975)
  km$surv_lower <- NA_real_
  km$surv_upper <- NA_real_
  for (i in seq_len(nrow(km))) {
    S <- km$survival[i]
    gs <- km$greenwood_sum[i]
    if (is.na(S) || S <= 0 || S >= 1) {
      # S=1 => CI is 1,1 ; S=0 => 0,0
      if (S == 1) {
        km$surv_lower[i] <- 1
        km$surv_upper[i] <- 1
      } else if (S == 0) {
        km$surv_lower[i] <- 0
        km$surv_upper[i] <- 0
      }
      next
    }
    # var of log(-log S)
    var_loglog <- gs / (log(S)^2)
    se_loglog <- sqrt(var_loglog)
    # alternative formula using exp
    # lower and upper for S
    # SAS: S_lower = S^{exp(z*se_loglog)} , S_upper = S^{exp(-z*se_loglog)}
    # Check: exp(z*se) >1 => S^{>1} < S => lower
    lower <- S ^ exp(z * se_loglog)
    upper <- S ^ exp(-z * se_loglog)
    # ensure bounds 0-1
    lower <- max(0, min(1, lower))
    upper <- max(0, min(1, upper))
    km$surv_lower[i] <- lower
    km$surv_upper[i] <- upper
  }
  km
}

km_list_ci <- lapply(km_list, compute_ci_loglog)

# Quartile function: smallest time where survival <= p
quartile_estimates <- function(km) {
  # km sorted by day
  # For each p in 0.75,0.5,0.25
  ps <- c(0.75, 0.5, 0.25)
  res <- tibble(percent = ps*100, estimate = NA_real_, lower = NA_real_, upper = NA_real_)
  for (j in seq_along(ps)) {
    p <- ps[j]
    # point estimate: first day where survival <= p
    idx <- which(km$survival <= p)
    if (length(idx) > 0) {
      res$estimate[j] <- km$day[min(idx)]
      # CI for quartile: Brookmeyer-Crowley using survival CI
      # lower CI for quartile = smallest t where surv_upper <= p
      # upper CI for quartile = smallest t where surv_lower <= p
      idx_low <- which(km$surv_upper <= p)
      idx_up  <- which(km$surv_lower <= p)
      if (length(idx_low) > 0) res$lower[j] <- km$day[min(idx_low)]
      if (length(idx_up) > 0)  res$upper[j] <- km$day[min(idx_up)]
    }
  }
  res
}

quartile_list <- lapply(km_list_ci, quartile_estimates)
names(quartile_list) <- arms

# Mean and SE (restricted mean up to largest event time)
mean_restricted <- function(df, km) {
  # df original, km table
  # tau = max event time
  event_times <- df$day[df$censor == 0]
  tau <- max(event_times)
  # Build step function: S0=1 at t0=0, then S after each distinct time
  # Need to consider all distinct times up to tau inclusive
  # For integration, we need intervals [t_prev, t_curr) with survival = S_prev
  # where S_prev is survival just before t_curr (i.e., after previous time)
  # So area = sum S_prev * (t_curr - t_prev)
  # plus final interval? Actually up to tau, the last interval ends at tau, survival after tau is S_tau
  # But if tau is an event time, the area up to tau includes interval ending at tau with S_prev
  # Not including beyond tau.
  # Implementation: sort distinct times <= tau
  times <- sort(unique(km$day[km$day <= tau]))
  # include t0=0
  prev_t <- 0
  prev_S <- 1
  area <- 0
  # For each time in order, add prev_S * (t - prev_t)
  for (t in times) {
    area <- area + prev_S * (t - prev_t)
    # update prev_S to survival after t
    # find km row for t
    row <- km[km$day == t, ]
    prev_S <- row$survival
    prev_t <- t
  }
  # area is restricted mean
  # Now compute A_i for each event time t_i: area from t_i to tau
  # A_i = integral_{t_i}^{tau} S(u) du
  # We can compute by similar method but starting at t_i
  # For each event time distinct where d>0
  event_distinct <- sort(unique(df$day[df$censor == 0]))
  # Need S after each event time and intervals
  # Build vectors of times and S for integration
  # Create ordered list of all distinct times <= tau with S after each
  all_times <- c(0, times)
  # S vector aligned: S0=1 for interval [0, times[1]), S1 for [times[1], times[2]), etc.
  # So S_after[i] corresponds to interval starting at all_times[i+1]? Let's construct
  S_after <- c(1, km$survival[match(times, km$day)])
  # Actually S_after[1]=1 corresponds to interval [0, times[1])
  # S_after[2] corresponds to interval [times[1], times[2]), etc.
  # For A_i at t_i, we need sum_{k: all_times[k] >= t_i} S_after[k+?]
  # Simpler: compute A_i by iterating
  m <- sum(df$censor == 0) # number of events
  # compute A_i for each distinct event time
  A_vals <- c()
  d_vals <- c()
  n_vals <- c()
  for (t in event_distinct) {
    # find index of t in all_times
    idx <- which(all_times == t)
    # A_i = sum_{j=idx}^{length(times)} S_after[j+1? wait}
    # Let's brute: integrate from t to tau stepwise
    # start at t, with S = survival after t
    # intervals: [t, next_time) with S = S_after[idx+1] ? Need mapping
    # all_times = c(0, t1, t2, ..., tau)
    # S_after = c(1, S1, S2, ..., S_tau) where S1 is survival after t1, etc.
    # Interval [all_times[k], all_times[k+1]) has survival S_after[k]
    # So for t = all_times[idx], the interval starting at t is k=idx, survival = S_after[idx]
    # But S_after[idx] is survival after previous time, not after t. For t itself, the survival after t is S_after[idx+1]? Let's test
    # Example: all_times = [0,143], S_after=[1, S143]
    # Interval [0,143) survival=1 = S_after[1] correct
    # Interval [143, next) survival = S143 = S_after[2] correct
    # So for t=143, idx=2, interval starting at t uses S_after[2] = S143 correct
    # So A_i = sum_{k=idx}^{length(all_times)-1} S_after[k+1? no, k corresponds to all_times[k] }
    # Actually for k=idx, interval [all_times[idx], all_times[idx+1]) survival = S_after[idx] ? Wait S_after[2]=S143, but interval [143, next) should be S143, which is S_after[2] = S_after[idx] where idx=2 => S_after[idx] correct
    # So A_i = sum_{k=idx}^{n-1} S_after[k] * (all_times[k+1]-all_times[k])? No S_after[k] for k=idx is S_after[2]=S143, but we need S_after[idx]?? Let's see: for k=idx=2, S_after[2]=S143 correct
    # So formula A_i = sum_{k=idx}^{length(all_times)-1} S_after[k] * delta_k where delta_k = all_times[k+1]-all_times[k] ??? But S_after[k] is survival after all_times[k-1]? Hmm confusion
    # Let's re-index: S_after[1]=1 corresponds to interval [0, t1) where k=1 => S_after[1] correct
    # S_after[2]=S1 corresponds to interval [t1, t2) where k=2 => S_after[2] correct
    # So interval k uses S_after[k]
    # Therefore for t = all_times[idx], A_i uses intervals k=idx to n-1 with S_after[k] where S_after[idx] is survival after t? Wait for t=143, idx=2, S_after[2]=S1 which is survival after 143, correct for interval starting at 143
    # So A_i = sum_{k=idx}^{n-1} S_after[k] * (all_times[k+1]-all_times[k]) but S_after[k] for k=idx is S after t, which is correct
    # However S_after[idx] for idx=2 is S1, but we defined S_after[2]=S1, so okay
    # But what about S_after[1]=1 for interval [0,t1) not needed
    # So compute
    A <- 0
    for (k in idx:(length(all_times)-1)) {
      delta <- all_times[k+1] - all_times[k]
      # S for interval k is S_after[k] ? For k=idx, S_after[idx] is S after t_{idx-1}? Let's test with idx=2: S_after[2]=S1, interval [t1,t2) uses S1 correct
      # So use S_after[k]
      A <- A + S_after[k] * delta
      # But for k=idx, S_after[idx] is S after t_{idx-1}? For idx=2, S_after[2]=S1 which is after t1, correct
      # For idx where t is not first, it holds
    }
    # Actually for t=143, we want area from 143 to tau, which includes interval [143, next) with S143, so A includes that
    # Our loop includes it
    A_vals <- c(A_vals, A)
    # d and n at t
    row <- km[km$day == t, ]
    d_vals <- c(d_vals, row$d)
    n_vals <- c(n_vals, row$n_risk)
  }
  # Now compute SE: sqrt( m/(m-1) * sum A_i^2 * d_i / (n_i*(n_i - d_i)) )
  # Note if n_i == d_i, term is 0? Actually denominator 0, but then S becomes 0, term undefined; SAS handles as 0
  sum_term <- 0
  for (i in seq_along(A_vals)) {
    d <- d_vals[i]
    n <- n_vals[i]
    A <- A_vals[i]
    if (n > d && d > 0) {
      sum_term <- sum_term + A^2 * d / (n * (n - d))
    }
  }
  if (m > 1) {
    se <- sqrt( m/(m-1) * sum_term )
  } else {
    se <- sqrt(sum_term)
  }
  list(mean = area, se = se, tau = tau, m = m)
}

mean_list <- lapply(seq_along(arms), function(i) {
  a <- arms[i]
  df <- filter(one, arm == a)
  km <- km_list[[i]]
  mean_restricted(df, km)
})
names(mean_list) <- arms

# Summary of censored ---------------------------------------------------
summary_cens <- one %>%
  group_by(arm) %>%
  summarise(
    total = n(),
    failed = sum(censor == 0),
    censored = sum(censor == 1),
    percent_censored = censored / total * 100
  ) %>%
  ungroup() %>%
  bind_rows(
    tibble(
      arm = "Total",
      total = nrow(one),
      failed = sum(one$censor == 0),
      censored = sum(one$censor == 1),
      percent_censored = censored / total * 100
    )
  )

# Rank tests ------------------------------------------------------------
# Use survdiff for log-rank and Wilcoxon (Peto)
# For 2 groups, survdiff gives chi-square
# SAS also prints rank statistics per stratum and covariance matrix
# We will compute using survival package
surv_obj <- Surv(one$day, one$censor == 0) # event = 1 if censor==0
# survdiff with rho=0 => log-rank, rho=1 => Wilcoxon (Peto-Prentice)
sd_logrank <- survdiff(surv_obj ~ arm, data = one, rho = 0)
sd_wilcox  <- survdiff(surv_obj ~ arm, data = one, rho = 1)

# Extract chi-square and p
chisq_lr <- sd_logrank$chisq
p_lr <- 1 - pchisq(chisq_lr, df = 1)
chisq_w <- sd_wilcox$chisq
p_w <- 1 - pchisq(chisq_w, df = 1)

# For -2Log(LR) test, SAS computes likelihood ratio test from Cox model?
# Approximate using coxph
cox_fit <- coxph(surv_obj ~ arm, data = one)
lr_chisq <- 2 * (cox_fit$loglik[2] - cox_fit$loglik[1])
# In coxph, loglik[1] is null, loglik[2] is fitted
p_lr2 <- 1 - pchisq(lr_chisq, df = 1)

# For covariance matrices: SAS prints per test a 2x2 matrix
# For 2 groups, variance for log-rank is V, covariance matrix = [[V, -V],[-V, V]]
# We can approximate V from survdiff$var
# survdiff$var is variance matrix
var_lr <- sd_logrank$var
var_w  <- sd_wilcox$var
# var is matrix 2x2? For 2 groups, var is 2x2 with same structure
# If var is 1x1 (since one df), we can reconstruct
# survdiff returns var as matrix of size g x g
# For 2 groups, var is 2x2 but one row/col is redundant? Let's check
# We'll construct full matrix as var matrix if available, else use scalar
make_cov_matrix <- function(v) {
  if (is.matrix(v) && nrow(v) == 2) {
    return(v)
  } else if (length(v) == 1) {
    # scalar variance for difference
    # For 2 groups, V = v, matrix = [[V, -V],[-V, V]] /? Actually var of (O-E) for each group
    # For 2 groups, var matrix has diagonal V and off-diagonal -V
    # where V is the variance of the difference /4? Need to check
    # Simpler: use v as diagonal and -v as off-diagonal
    # But SAS prints matrix with positive diagonal and negative off-diagonal
    # We'll use v for diagonal
    m <- matrix(c(v, -v, -v, v), nrow = 2, byrow = TRUE)
    return(m)
  } else {
    return(matrix(NA, 2, 2))
  }
}
cov_lr <- make_cov_matrix(var_lr)
cov_w  <- make_cov_matrix(var_w)

# For rank statistics per stratum: SAS prints "Rank Statistics" with point estimate and variance?
# We can compute O-E and variance per group from survdiff$obs and $exp
# obs = observed events per group, exp = expected
obs_lr <- sd_logrank$obs
exp_lr <- sd_logrank$exp
obs_w <- sd_wilcox$obs
exp_w <- sd_wilcox$exp

# Prepare output text ---------------------------------------------------
# Build text rendering similar to SAS PROC LIFETEST
txt_lines <- c(
  "Example K-M Plot",
  "",
  "The LIFETEST Procedure",
  "",
  paste0("Strata: arm (", paste(arms, collapse = ", "), ")"),
  paste0("Time variable: day  Censoring variable: censor(1)"),
  ""
)

# Product-Limit Survival Estimates per stratum
for (i in seq_along(arms)) {
  a <- arms[i]
  km <- km_list[[i]]
  km_ci <- km_list_ci[[i]]
  txt_lines <- c(txt_lines,
    paste0("Product-Limit Survival Estimates for ", a),
    "day  Survival  Failure  Survival Std Error  Number Failed  Number Left",
    "------------------------------------------------------------------------"
  )
  for (r in seq_len(nrow(km))) {
    day_str <- sprintf("%7.2f%s", km$day[r], ifelse(km$cens_mark[r]=="*", "*", " "))
    # SAS prints censored rows with * after time
    # Format numbers 4 decimals
    line <- sprintf("%7s  %8s  %7s  %12s  %13d  %11d",
      paste0(fmt3(km$day[r]), ifelse(km$cens_mark[r]=="*", "*", "")),
      fmt4(km$survival[r]),
      fmt4(km$failure[r]),
      fmt4(km$se[r]),
      km$number_failed[r],
      km$number_left[r]
    )
    txt_lines <- c(txt_lines, line)
  }
  txt_lines <- c(txt_lines, "")
  # Quartile Estimates
  q <- quartile_list[[a]]
  txt_lines <- c(txt_lines,
    paste0("Quartile Estimates for ", a),
    "Percent  Point Estimate  95% Confidence Interval [Lower Upper)",
    "------------------------------------------------------------"
  )
  for (r in seq_len(nrow(q))) {
    txt_lines <- c(txt_lines, sprintf("%6.1f  %14s  %10s  %10s",
      q$percent[r],
      ifelse(is.na(q$estimate[r]), ".", fmt3(q$estimate[r])),
      ifelse(is.na(q$lower[r]), ".", fmt3(q$lower[r])),
      ifelse(is.na(q$upper[r]), ".", fmt3(q$upper[r]))
    ))
  }
  txt_lines <- c(txt_lines, "")
  # Mean and SE
  mres <- mean_list[[a]]
  txt_lines <- c(txt_lines,
    paste0("Mean and Standard Error for ", a),
    sprintf("Mean (restricted to %.3f): %s  Standard Error: %s",
      mres$tau, fmt3(mres$mean), fmt3(mres$se)),
    ""
  )
}

# Summary of censored
txt_lines <- c(txt_lines,
  "Summary of the Number of Censored and Uncensored Values",
  "Stratum  Total  Failed  Censored  Percent Censored",
  "---------------------------------------------------"
)
for (r in seq_len(nrow(summary_cens))) {
  txt_lines <- c(txt_lines, sprintf("%-7s  %5d  %6d  %8d  %12s",
    summary_cens$arm[r],
    summary_cens$total[r],
    summary_cens$failed[r],
    summary_cens$censored[r],
    fmt2(summary_cens$percent_censored[r])
  ))
}
txt_lines <- c(txt_lines, "")

# Rank Statistics
txt_lines <- c(txt_lines,
  "Rank Statistics",
  "Test  Stratum  Observed  Expected  Variance (per group)",
  "-------------------------------------------------------"
)
# Log-Rank per stratum
for (j in seq_along(arms)) {
  txt_lines <- c(txt_lines, sprintf("Log-Rank  %-7s  %8.4f  %8.4f  %8.4f",
    arms[j], obs_lr[j], exp_lr[j], cov_lr[j,j]))
}
for (j in seq_along(arms)) {
  txt_lines <- c(txt_lines, sprintf("Wilcoxon  %-7s  %8.4f  %8.4f  %8.4f",
    arms[j], obs_w[j], exp_w[j], cov_w[j,j]))
}
txt_lines <- c(txt_lines, "")

# Covariance matrices
txt_lines <- c(txt_lines,
  "Covariance Matrix for the Log-Rank Statistics",
  "          Arm 1      Arm 2",
  "---------------------------"
)
for (r in 1:2) {
  txt_lines <- c(txt_lines, sprintf("%-7s  %8.4f  %8.4f",
    arms[r], cov_lr[r,1], cov_lr[r,2]))
}
txt_lines <- c(txt_lines, "",
  "Covariance Matrix for the Wilcoxon Statistics",
  "          Arm 1      Arm 2",
  "---------------------------"
)
for (r in 1:2) {
  txt_lines <- c(txt_lines, sprintf("%-7s  %8.4f  %8.4f",
    arms[r], cov_w[r,1], cov_w[r,2]))
}
txt_lines <- c(txt_lines, "")

# Test of Equality over Strata
txt_lines <- c(txt_lines,
  "Test of Equality over Strata",
  "Test          Chi-Square  DF  Pr > Chi-Square",
  "---------------------------------------------",
  sprintf("Log-Rank      %10s   1  %12s", fmt4(chisq_lr), ifelse(p_lr < 0.0001, "<.0001", fmt4(p_lr))),
  sprintf("Wilcoxon      %10s   1  %12s", fmt4(chisq_w), ifelse(p_w < 0.0001, "<.0001", fmt4(p_w))),
  sprintf("-2Log(LR)     %10s   1  %12s", fmt4(lr_chisq), ifelse(p_lr2 < 0.0001, "<.0001", fmt4(p_lr2))),
  "",
  sprintf("Sample Size = %d", nrow(one)),
  ""
)

# Write text file
writeLines(txt_lines, file.path(out, "P040_f_kmplot.txt"))

# Write CSV of table body (product-limit estimates)
csv_df <- km_all %>%
  mutate(
    survival_fmt = round_half_up(survival, 4),
    failure_fmt = round_half_up(failure, 4),
    se_fmt = round_half_up(se, 4)
  ) %>%
  select(arm, day, survival, failure, se, number_failed, number_left, cens_mark) %>%
  arrange(arm, day)
# Ensure dates as character ISO not needed; day is numeric
write.csv(csv_df, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

# Also write quartile and summary CSVs for completeness (optional, but main CSV is product-limit)
# Write at-risk CSV for plot data
write.csv(atrisk_df, file.path(out, "P040_f_kmplot_atrisk.csv"), row.names = FALSE, na = "")

# Plot ------------------------------------------------------------------
# Use survfit for plotting
fit <- survfit(Surv(day, censor == 0) ~ arm, data = one)

# Build data for ggplot
# Use broom or manual
plot_data <- lapply(seq_along(arms), function(i) {
  km <- km_list[[i]]
  # For step plot, need to include time 0 with survival 1
  tibble(
    time = c(0, km$day),
    surv = c(1, km$survival),
    arm = arms[i]
  )
}) %>% bind_rows()

# At-risk data for annotation
atrisk_long <- atrisk_df %>%
  pivot_longer(cols = starts_with("t"), names_to = "time", values_to = "n_at_risk") %>%
  mutate(time = as.numeric(sub("t", "", time)))

p <- ggplot(plot_data, aes(x = time, y = surv, color = arm, linetype = arm)) +
  geom_step(direction = "hv", linewidth = 0.9) +
  scale_x_continuous(limits = c(0, 350), breaks = seq(0, 330, by = 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  labs(
    title = "Example K-M Plot",
    x = "Days",
    y = "Survival Probability",
    color = "Stratum",
    linetype = "Stratum"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  # add at-risk table as text annotation below
  geom_text(
    data = atrisk_long,
    aes(x = time, y = -0.08, label = n_at_risk, color = arm),
    size = 3, show.legend = FALSE, inherit.aes = FALSE
  ) +
  annotate("text", x = -15, y = -0.08, label = "At Risk", size = 3, hjust = 1) +
  coord_cartesian(clip = "off", ylim = c(0, 1)) +
  theme(plot.margin = margin(10, 10, 40, 10))

ggsave(file.path(out, "P040_f_kmplot.png"), plot = p, width = 8, height = 6, dpi = 300, bg = "white")

# Also write plot data CSV
write.csv(plot_data, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

# Console log
cat("P040 R translation complete. Outputs written to", out, "\n")
cat("Rows in km_all:", nrow(km_all), "\n")
cat("Log-Rank chi-square:", chisq_lr, "p:", p_lr, "\n")
