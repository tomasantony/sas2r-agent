```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)
library(survival)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

fmt_n <- function(x, digits) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

# ------------------------------------------------------------
# Create data exactly as SAS data step
# ------------------------------------------------------------
one <- tibble::tibble(
  arm = c(rep("Arm 1", 17), rep("Arm 2", 21)),
  day = c(
    143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304, 216,244,
    142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323, 204,344
  ),
  censor = c(rep(0,15), rep(1,2), rep(0,19), rep(1,2))
)
# event indicator for survival (1=event, 0=censored)
one$event <- 1 - one$censor

# ------------------------------------------------------------
# Helper to compute KM per arm (SAS semantics)
# ------------------------------------------------------------
compute_km_arm <- function(df_arm) {
  # df_arm has day, censor, event
  # distinct times sorted
  times <- sort(unique(df_arm$day))
  n_total <- nrow(df_arm)
  res <- tibble()
  S_prev <- 1
  sum_term <- 0  # for Greenwood sum d/(n*(n-d))
  # for mean calculation we need vectors
  for (t in times) {
    n_risk <- sum(df_arm$day >= t)
    d <- sum(df_arm$day == t & df_arm$censor == 0)
    cns <- sum(df_arm$day == t & df_arm$censor == 1)
    # Greenwood update only if d>0
    if (d > 0) {
      sum_term <- sum_term + d / (n_risk * (n_risk - d))
      S <- S_prev * (1 - d / n_risk)
    } else {
      S <- S_prev
    }
    var_green <- S^2 * sum_term
    se <- sqrt(var_green)
    # number failed / left (SAS definitions)
    # need cumulative failed up to this time
    # will compute after loop
    res <- bind_rows(res, tibble(
      time = t,
      n_risk = n_risk,
      n_event = d,
      n_censor = cns,
      survival = S,
      se = se,
      sum_term = sum_term
    ))
    S_prev <- S
  }
  # cumulative failed and number left
  res <- res %>%
    mutate(
      n_failed = cumsum(n_event),
      n_left = n_risk - n_event - n_censor,
      failure = 1 - survival,
      censored_flag = ifelse(n_event == 0 & n_censor > 0, "*", "")
    )
  res
}

arms <- unique(one$arm)
km_list <- lapply(arms, function(a) {
  df <- filter(one, arm == a)
  km <- compute_km_arm(df)
  km$arm <- a
  km
})
km_all <- bind_rows(km_list)

# ------------------------------------------------------------
# Quartile estimates and CI (log-log transform)
# ------------------------------------------------------------
z <- qnorm(0.975)

compute_quartiles <- function(km) {
  # km has time, survival, se
  # compute log-log CI for survival at each time
  km <- km %>% arrange(time)
  km <- km %>%
    mutate(
      # avoid log(0) issues
      loglog = ifelse(survival > 0 & survival < 1, log(-log(survival)), NA_real_),
      se_loglog = ifelse(survival > 0 & survival < 1,
                         se / (survival * log(survival)), NA_real_),
      surv_lower = ifelse(!is.na(loglog),
                          exp(-exp(loglog + z * abs(se_loglog))), NA_real_),
      surv_upper = ifelse(!is.na(loglog),
                          exp(-exp(loglog - z * abs(se_loglog))), NA_real_)
    )
  # for S=1, CI is (.,.)? set to 1
  km$surv_lower[km$survival == 1] <- 1
  km$surv_upper[km$survival == 1] <- 1
  # for S=0, CI 0
  km$surv_lower[km$survival == 0] <- 0
  km$surv_upper[km$survival == 0] <- 0

  qs <- c(0.75, 0.50, 0.25)
  out_q <- tibble()
  for (q in qs) {
    # point estimate: smallest time where survival <= q
    idx_pt <- which(km$survival <= q)
    pt <- if (length(idx_pt) > 0) km$time[min(idx_pt)] else NA_real_
    # CI via inversion: lower limit = min time where surv_upper <= q
    # upper limit = min time where surv_lower <= q
    idx_up <- which(!is.na(km$surv_upper) & km$surv_upper <= q)
    idx_low <- which(!is.na(km$surv_lower) & km$surv_lower <= q)
    lower <- if (length(idx_up) > 0) km$time[min(idx_up)] else NA_real_
    upper <- if (length(idx_low) > 0) km$time[min(idx_low)] else NA_real_
    # SAS orders lower < upper, so swap if needed
    if (!is.na(lower) && !is.na(upper) && lower > upper) {
      tmp <- lower; lower <- upper; upper <- tmp
    }
    out_q <- bind_rows(out_q, tibble(
      percent = q*100,
      point = pt,
      lower = lower,
      upper = upper
    ))
  }
  out_q
}

quartile_list <- lapply(arms, function(a) {
  km <- filter(km_all, arm == a)
  q <- compute_quartiles(km)
  q$arm <- a
  q
})
quartile_all <- bind_rows(quartile_list)

# ------------------------------------------------------------
# Mean and SE (restricted mean, SAS formula with m/(m-1))
# ------------------------------------------------------------
compute_mean_se <- function(df_arm, km) {
  # km sorted by time, includes censored times
  # tau = largest event time if last obs censored else largest time
  max_event_time <- max(df_arm$day[df_arm$censor == 0])
  max_time <- max(df_arm$day)
  last_is_censored <- max_time != max_event_time
  tau <- if (last_is_censored) max_event_time else max_time

  # Build intervals from 0 to tau using km times <= tau
  km2 <- km %>% filter(time <= tau) %>% arrange(time)
  # add t0=0
  times <- c(0, km2$time)
  survs <- c(1, km2$survival) # survival after each time, but for interval [t_i, t_{i+1}) survival is surv after t_i
  # Actually survival for interval starting at t_i is survival after t_i (except first interval uses 1)
  # So area = sum survs[i] * (times[i+1]-times[i]) for i=1..n
  # where survs[i] corresponds to survival after times[i]
  # times[1]=0 survs[1]=1
  # times[2]=first event time survs[2]=S after first time, interval 0->t1 uses survs[1]
  # interval t1->t2 uses survs[2], etc.
  # So we need survs shifted
  n_int <- length(times) - 1
  area <- 0
  for (i in 1:n_int) {
    delta <- times[i+1] - times[i]
    area <- area + survs[i] * delta
  }
  # For mean, if tau is event time, last interval already included up to tau
  # If tau < max_time (censored tail beyond tau not included) we already stopped at tau
  mean_val <- area

  # Compute A_i for each event time t_i (where n_event>0)
  # A_i = area from t_i to tau
  # Need to compute area from each event time to tau
  event_rows <- km2 %>% filter(n_event > 0)
  m <- sum(event_rows$n_event) # total number of events
  # Precompute cumulative area from each time to tau
  # For each event time t_i, A_i = sum_{j: times[j] >= t_i} survs_after_j * delta_j
  # where survs_after_j is survival after times[j]
  # Simpler: compute total area from t_i to tau as mean - area from 0 to t_i
  # area from 0 to t_i = sum_{k: times[k] < t_i} survs[k] * delta_k
  # So A_i = mean - area_0_to_ti
  # But need area_0_to_ti includes intervals up to t_i
  # Let's compute prefix areas
  prefix_area <- c(0)
  cum <- 0
  for (i in 1:n_int) {
    delta <- times[i+1] - times[i]
    cum <- cum + survs[i] * delta
    prefix_area <- c(prefix_area, cum)
  }
  # prefix_area[j] = area from 0 to times[j]
  # For event time t_i = times[idx], area from t_i to tau = mean - prefix_area[idx]
  # where idx is position of t_i in times vector
  sum_term_mean <- 0
  for (k in 1:nrow(event_rows)) {
    t_i <- event_rows$time[k]
    d_i <- event_rows$n_event[k]
    n_i <- event_rows$n_risk[k]
    idx <- which(times == t_i)
    # if duplicate times (should not), take first
    idx <- idx[1]
    A_i <- mean_val - prefix_area[idx]
    # Note: SAS defines A_i as area from t_i to largest event time, using survival after t_i?
    # Our A_i uses area from t_i (including interval starting at t_i with survival after t_i)
    # which matches definition: integral from t_i to tau S(t) dt, where S(t) for t>=t_i is S after t_i
    # So A_i should be mean - prefix_area[idx] but prefix_area[idx] is area up to t_i, which uses survival before t_i for last interval
    # Actually prefix_area[idx] is area up to t_i, correct.
    # So A_i as above is correct.
    sum_term_mean <- sum_term_mean + (A_i^2 * d_i) / (n_i * (n_i - d_i))
  }
  if (m > 1) {
    var_mean <- (m / (m - 1)) * sum_term_mean
    se_mean <- sqrt(var_mean)
  } else {
    se_mean <- NA_real_
  }
  tibble(mean = mean_val, se_mean = se_mean, m = m, tau = tau)
}

mean_list <- lapply(arms, function(a) {
  df <- filter(one, arm == a)
  km <- filter(km_all, arm == a)
  ms <- compute_mean_se(df, km)
  ms$arm <- a
  ms
})
mean_all <- bind_rows(mean_list)

# ------------------------------------------------------------
# Summary of censored / uncensored
# ------------------------------------------------------------
summary_tbl <- one %>%
  group_by(arm) %>%
  summarise(
    total = n(),
    failed = sum(censor == 0),
    censored = sum(censor == 1),
    percent_censored = censored / total * 100,
    .groups = "drop"
  )

# ------------------------------------------------------------
# Rank tests: Log-Rank and Wilcoxon (Gehan-Breslow)
# ------------------------------------------------------------
# pooled distinct event times
event_times <- sort(unique(one$day[one$censor == 0]))
rank_data <- tibble(time = event_times)
# compute at-risk and events per arm at each time
for (a in arms) {
  df_a <- filter(one, arm == a)
  rank_data[[paste0("n_", a)]] <- sapply(event_times, function(t) sum(df_a$day >= t))
  rank_data[[paste0("d_", a)]] <- sapply(event_times, function(t) sum(df_a$day == t & df_a$censor == 0))
}
rank_data$n_total <- rank_data[[paste0("n_", arms[1])]] + rank_data[[paste0("n_", arms[2])]]
rank_data$d_total <- rank_data[[paste0("d_", arms[1])]] + rank_data[[paste0("d_", arms[2])]]
# expected
rank_data$e1 <- rank_data[[paste0("n_", arms[1])]] * rank_data$d_total / rank_data$n_total
rank_data$e2 <- rank_data[[paste0("n_", arms[2])]] * rank_data$d_total / rank_data$n_total
# variance for log-rank
rank_data$v <- with(rank_data,
                    ifelse(n_total > 1,
                           `n_Arm 1` * `n_Arm 2` * d_total * (n_total - d_total) / (n_total^2 * (n_total - 1)),
                           0))
# Wilcoxon weight = n_total
rank_data$w <- rank_data$n_total
rank_data$U1 <- rank_data[[paste0("d_", arms[1])]] - rank_data$e1
rank_data$U2 <- rank_data[[paste0("d_", arms[2])]] - rank_data$e2
rank_data$U1_w <- rank_data$w * rank_data$U1
rank_data$U2_w <- rank_data$w * rank_data$U2
rank_data$v_w <- rank_data$w^2 * rank_data$v

U_logrank_1 <- sum(rank_data$U1)
U_logrank_2 <- sum(rank_data$U2)
V_logrank <- sum(rank_data$v)
chi_logrank <- ifelse(V_logrank > 0, (U_logrank_1^2) / V_logrank, 0)
p_logrank <- 1 - pchisq(chi_logrank, df = 1)

U_wilcoxon_1 <- sum(rank_data$U1_w)
U_wilcoxon_2 <- sum(rank_data$U2_w)
V_wilcoxon <- sum(rank_data$v_w)
chi_wilcoxon <- ifelse(V_wilcoxon > 0, (U_wilcoxon_1^2) / V_wilcoxon, 0)
p_wilcoxon <- 1 - pchisq(chi_wilcoxon, df = 1)

# Covariance matrices (full square)
cov_logrank <- matrix(c(V_logrank, -V_logrank, -V_logrank, V_logrank), nrow = 2, byrow = TRUE,
                      dimnames = list(arms, arms))
cov_wilcoxon <- matrix(c(V_wilcoxon, -V_wilcoxon, -V_wilcoxon, V_wilcoxon), nrow = 2, byrow = TRUE,
                       dimnames = list(arms, arms))

# -2Log(LR) via Cox model
cox_fit <- coxph(Surv(day, event) ~ arm, data = one)
lr_chi <- 2 * (cox_fit$loglik[2] - cox_fit$loglik[1])
p_lr <- 1 - pchisq(lr_chi, df = 1)

# ------------------------------------------------------------
# At-risk numbers for plot (0 to 330 by 30)
# ------------------------------------------------------------
atrisk_times <- seq(0, 330, by = 30)
atrisk_tbl <- expand.grid(arm = arms, atrisk_time = atrisk_times) %>%
  as_tibble() %>%
  mutate(
    n_at_risk = map2_int(arm, atrisk_time, function(a, t) sum(one$arm == a & one$day >= t))
  ) %>%
  arrange(arm, atrisk_time)

# ------------------------------------------------------------
# Plot: KM curves
# ------------------------------------------------------------
# Build step data for ggplot
plot_data <- km_all %>%
  arrange(arm, time) %>%
  group_by(arm) %>%
  # add starting point 0,1
  group_modify(~ {
    add_row(.x, time = 0, survival = 1, .before = 1)
  }) %>%
  ungroup()

# For step plot, need to create stairs
p <- ggplot(plot_data, aes(x = time, y = survival, color = arm, linetype = arm)) +
  geom_step(linewidth = 0.9) +
  geom_point(data = filter(km_all, n_censor > 0 & n_event == 0), aes(x = time, y = survival), shape = 3, size = 2, show.legend = FALSE) +
  scale_x_continuous(breaks = atrisk_times, limits = c(0, 330)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Example K-M Plot",
       x = "Day",
       y = "Survival Probability",
       color = "Stratum",
       linetype = "Stratum") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

# Add at-risk table as annotation (text)
# Create a separate grob for at-risk table using ggplot
atrisk_wide <- atrisk_tbl %>%
  pivot_wider(names_from = atrisk_time, values_from = n_at_risk)

# Save main plot
png_file <- file.path(out, "P040_f_kmplot.png")
ggsave(png_file, plot = p, width = 8, height = 6, dpi = 300, units = "in")

# Also save plot data CSV
plot_data_csv <- km_all %>%
  select(arm, time, survival, failure, se, n_failed, n_left, n_risk, n_event, n_censor)
write.csv(plot_data_csv, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# CSV of product-limit estimates (main table)
# ------------------------------------------------------------
csv_main <- km_all %>%
  select(arm, time, survival, failure, se, n_failed, n_left, n_event, n_censor) %>%
  arrange(arm, time)
write.csv(csv_main, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Text rendering (SAS-like listing)
# ------------------------------------------------------------
txt_file <- file.path(out, "P040_f_kmplot.txt")
sink(txt_file)

cat("Example K-M Plot\n")
cat("PROC LIFETEST\n")
cat("\n")
cat("Product-Limit Survival Estimates\n")
cat("\n")
for (a in arms) {
  cat(paste0("Stratum: ", a, "\n"))
  cat(sprintf("%-10s %10s %10s %12s %10s %10s\n",
              "Time", "Survival", "Failure", "StdErr", "Failed", "Left"))
  km <- filter(km_all, arm == a) %>% arrange(time)
  for (i in 1:nrow(km)) {
    r <- km[i,]
    t_str <- fmt_n(r$time, 3)
    if (r$n_event == 0 && r$n_censor > 0) t_str <- paste0(t_str, "*")
    cat(sprintf("%-10s %10s %10s %12s %10d %10d\n",
                t_str,
                fmt_n(r$survival, 4),
                fmt_n(r$failure, 4),
                fmt_n(r$se, 4),
                r$n_failed,
                r$n_left))
  }
  cat("\n")
}

cat("Quartile Estimates\n")
cat(sprintf("%-10s %10s %12s %12s %12s\n", "Stratum", "Percent", "Point", "Lower 95%", "Upper 95%"))
for (a in arms) {
  q <- filter(quartile_all, arm == a) %>% arrange(percent)
  for (i in 1:nrow(q)) {
    r <- q[i,]
    cat(sprintf("%-10s %10.0f %12s %12s %12s\n",
                a,
                r$percent,
                ifelse(is.na(r$point), ".", fmt_n(r$point, 3)),
                ifelse(is.na(r$lower), ".", fmt_n(r$lower, 3)),
                ifelse(is.na(r$upper), ".", fmt_n(r$upper, 3))))
  }
}
cat("\n")

cat("Mean and Standard Error (Restricted Mean)\n")
cat(sprintf("%-10s %12s %12s %10s\n", "Stratum", "Mean", "StdErr", "Tau"))
for (i in 1:nrow(mean_all)) {
  r <- mean_all[i,]
  cat(sprintf("%-10s %12s %12s %10s\n",
              r$arm,
              fmt_n(r$mean, 3),
              fmt_n(r$se_mean, 3),
              fmt_n(r$tau, 3)))
}
cat("\n")

cat("Summary of the Number of Censored and Uncensored Values\n")
cat(sprintf("%-10s %8s %8s %8s %12s\n", "Stratum", "Total", "Failed", "Censored", "Pct Cens"))
for (i in 1:nrow(summary_tbl)) {
  r <- summary_tbl[i,]
  cat(sprintf("%-10s %8d %8d %8d %12s\n",
              r$arm, r$total, r$failed, r$censored, fmt_n(r$percent_censored, 2)))
}
cat("\n")

cat("Rank Statistics\n")
cat(sprintf("%-10s %12s %12s\n", "Stratum", "Log-Rank", "Wilcoxon"))
cat(sprintf("%-10s %12s %12s\n", arms[1], fmt_n(U_logrank_1, 4), fmt_n(U_wilcoxon_1, 4)))
cat(sprintf("%-10s %12s %12s\n", arms[2], fmt_n(U_logrank_2, 4), fmt_n(U_wilcoxon_2, 4)))
cat("\n")

cat("Covariance Matrix for the Log-Rank Statistics\n")
cat(sprintf("%10s %12s %12s\n", "", arms[1], arms[2]))
for (i in 1:2) {
  cat(sprintf("%10s %12s %12s\n", arms[i],
              fmt_n(cov_logrank[i,1], 4),
              fmt_n(cov_logrank[i,2], 4)))
}
cat("\n")
cat("Covariance Matrix for the Wilcoxon Statistics\n")
cat(sprintf("%10s %12s %12s\n", "", arms[1], arms[2]))
for (i in 1:2) {
  cat(sprintf("%10s %12s %12s\n", arms[i],
              fmt_n(cov_wilcoxon[i,1], 4),
              fmt_n(cov_wilcoxon[i,2], 4)))
}
cat("\n")

cat("Test of Equality over Strata\n")
cat(sprintf("%-12s %12s %4s %12s\n", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
p_fmt <- function(p) ifelse(p < 0.0001, "<.0001", fmt_n(p, 4))
cat(sprintf("%-12s %12s %4d %12s\n", "Log-Rank", fmt_n(chi_logrank, 4), 1, p_fmt(p_logrank)))
cat(sprintf("%-12s %12s %4d %12s\n", "Wilcoxon", fmt_n(chi_wilcoxon, 4), 1, p_fmt(p_wilcoxon)))
cat(sprintf("%-12s %12s %4d %12s\n", "-2Log(LR)", fmt_n(lr_chi, 4), 1, p_fmt(p_lr)))
cat("\n")

cat("Number at Risk (atrisk=0 to 330 by 30)\n")
cat(sprintf("%-10s", "Stratum"))
for (t in atrisk_times) cat(sprintf("%6d", t))
cat("\n")
for (a in arms) {
  cat(sprintf("%-10s", a))
  for (t in atrisk_times) {
    n <- sum(one$arm == a & one$day >= t)
    cat(sprintf("%6d", n))
  }
  cat("\n")
}
cat("\n")
cat("Note: * indicates censored observation\n")
cat("Restricted mean is area under survival curve up to largest event time\n")
cat(sprintf("SE of restricted mean uses SAS formula: sqrt(m/(m-1)*sum A_i^2*d_i/(n_i*(n_i-d_i))) with m events\n"))

sink()

# Ensure files exist
stopifnot(file.exists(file.path(out, "P040_f_kmplot.csv")))
stopifnot(file.exists(file.path(out, "P040_f_kmplot.txt")))
stopifnot(file.exists(file.path(out, "P040_f_kmplot.png")))
stopifnot(file.exists(file.path(out, "P040_f_kmplot_plotdata.csv")))
```