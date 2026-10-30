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
library(grid)
library(gridExtra)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_n <- function(x, digits) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  fmt_n(p, 4)
}

# ------------------------------------------------------------
# Data exactly as SAS data step
# ------------------------------------------------------------
one <- tibble::tibble(
  arm = c(rep("Arm 1", 17), rep("Arm 2", 21)),
  day = c(
    143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304, 216,244,
    142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323, 204,344
  ),
  censor = c(rep(0,15), rep(1,2), rep(0,19), rep(1,2))
)
one$event <- 1 - one$censor

# ------------------------------------------------------------
# KM per arm
# ------------------------------------------------------------
compute_km_arm <- function(df_arm) {
  times <- sort(unique(df_arm$day))
  n_total <- nrow(df_arm)
  res <- tibble()
  S_prev <- 1
  sum_term <- 0
  for (t in times) {
    n_risk <- sum(df_arm$day >= t)
    d <- sum(df_arm$day == t & df_arm$censor == 0)
    cns <- sum(df_arm$day == t & df_arm$censor == 1)
    if (d > 0) {
      sum_term <- sum_term + d / (n_risk * (n_risk - d))
      S <- S_prev * (1 - d / n_risk)
    } else {
      S <- S_prev
    }
    var_green <- S^2 * sum_term
    se <- sqrt(var_green)
    res <- bind_rows(res, tibble(
      time = t, n_risk = n_risk, n_event = d, n_censor = cns,
      survival = S, se = se, sum_term = sum_term
    ))
    S_prev <- S
  }
  res <- res %>%
    mutate(
      n_failed = cumsum(n_event),
      n_left = n_risk - n_event - n_censor,
      failure = 1 - survival,
      censored_flag = ifelse(n_event == 0 & n_censor > 0, "*", "")
    )
  res
}

arms <- c("Arm 1","Arm 2")
km_list <- lapply(arms, function(a) {
  df <- filter(one, arm == a)
  km <- compute_km_arm(df)
  km$arm <- a
  km
})
km_all <- bind_rows(km_list)

# ------------------------------------------------------------
# Quartile estimates with log-log CI
# ------------------------------------------------------------
z <- qnorm(0.975)
compute_quartiles <- function(km) {
  km <- km %>% arrange(time)
  km <- km %>%
    mutate(
      loglog = ifelse(survival > 0 & survival < 1, log(-log(survival)), NA_real_),
      se_loglog = ifelse(survival > 0 & survival < 1,
                         se / (survival * log(survival)), NA_real_),
      surv_lower = ifelse(!is.na(loglog),
                          exp(-exp(loglog + z * abs(se_loglog))), NA_real_),
      surv_upper = ifelse(!is.na(loglog),
                          exp(-exp(loglog - z * abs(se_loglog))), NA_real_)
    )
  km$surv_lower[km$survival == 1] <- 1
  km$surv_upper[km$survival == 1] <- 1
  km$surv_lower[km$survival == 0] <- 0
  km$surv_upper[km$survival == 0] <- 0
  qs <- c(0.75, 0.50, 0.25)
  out_q <- tibble()
  for (q in qs) {
    idx_pt <- which(km$survival <= q)
    pt <- if (length(idx_pt) > 0) km$time[min(idx_pt)] else NA_real_
    idx_up <- which(!is.na(km$surv_upper) & km$surv_upper <= q)
    idx_low <- which(!is.na(km$surv_lower) & km$surv_lower <= q)
    lower <- if (length(idx_up) > 0) km$time[min(idx_up)] else NA_real_
    upper <- if (length(idx_low) > 0) km$time[min(idx_low)] else NA_real_
    if (!is.na(lower) && !is.na(upper) && lower > upper) {
      tmp <- lower; lower <- upper; upper <- tmp
    }
    out_q <- bind_rows(out_q, tibble(percent = q*100, point = pt, lower = lower, upper = upper))
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
# Mean and SE (restricted mean up to largest observed time, SAS)
# ------------------------------------------------------------
compute_mean_se <- function(df_arm, km) {
  tau <- max(df_arm$day)  # SAS: largest observed time (censored or not)
  km2 <- km %>% filter(time <= tau) %>% arrange(time)
  times <- c(0, km2$time)
  survs <- c(1, km2$survival)
  n_int <- length(times) - 1
  area <- 0
  for (i in 1:n_int) {
    delta <- times[i+1] - times[i]
    area <- area + survs[i] * delta
  }
  mean_val <- area
  event_rows <- km2 %>% filter(n_event > 0)
  m <- sum(event_rows$n_event)
  prefix_area <- c(0)
  cum <- 0
  for (i in 1:n_int) {
    delta <- times[i+1] - times[i]
    cum <- cum + survs[i] * delta
    prefix_area <- c(prefix_area, cum)
  }
  sum_term_mean <- 0
  for (k in 1:nrow(event_rows)) {
    t_i <- event_rows$time[k]
    d_i <- event_rows$n_event[k]
    n_i <- event_rows$n_risk[k]
    idx <- which(times == t_i)[1]
    A_i <- mean_val - prefix_area[idx]
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
# Summary censored / uncensored (per stratum + total)
# ------------------------------------------------------------
summary_tbl <- one %>%
  group_by(arm) %>%
  summarise(total = n(), failed = sum(censor == 0), censored = sum(censor == 1), .groups="drop") %>%
  mutate(percent_censored = censored / total * 100)
total_row <- tibble(
  arm = "Total",
  total = nrow(one),
  failed = sum(one$censor == 0),
  censored = sum(one$censor == 1),
  percent_censored = sum(one$censor == 1)/nrow(one)*100
)
summary_tbl2 <- bind_rows(summary_tbl, total_row)

# ------------------------------------------------------------
# Rank tests
# ------------------------------------------------------------
event_times <- sort(unique(one$day[one$censor == 0]))
rank_data <- tibble(time = event_times)
for (a in arms) {
  df_a <- filter(one, arm == a)
  rank_data[[paste0("n_", a)]] <- sapply(event_times, function(t) sum(df_a$day >= t))
  rank_data[[paste0("d_", a)]] <- sapply(event_times, function(t) sum(df_a$day == t & df_a$censor == 0))
}
rank_data$n_total <- rank_data[[paste0("n_", arms[1])]] + rank_data[[paste0("n_", arms[2])]]
rank_data$d_total <- rank_data[[paste0("d_", arms[1])]] + rank_data[[paste0("d_", arms[2])]]
rank_data$e1 <- rank_data[[paste0("n_", arms[1])]] * rank_data$d_total / rank_data$n_total
rank_data$e2 <- rank_data[[paste0("n_", arms[2])]] * rank_data$d_total / rank_data$n_total
rank_data$v <- with(rank_data,
  ifelse(n_total > 1,
    `n_Arm 1` * `n_Arm 2` * d_total * (n_total - d_total) / (n_total^2 * (n_total - 1)), 0))
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

cov_logrank <- matrix(c(V_logrank, -V_logrank, -V_logrank, V_logrank), nrow=2, byrow=TRUE,
                      dimnames=list(arms, arms))
cov_wilcoxon <- matrix(c(V_wilcoxon, -V_wilcoxon, -V_wilcoxon, V_wilcoxon), nrow=2, byrow=TRUE,
                       dimnames=list(arms, arms))

cox_fit <- coxph(Surv(day, event) ~ arm, data = one)
lr_chi <- 2 * (cox_fit$loglik[2] - cox_fit$loglik[1])
p_lr <- 1 - pchisq(lr_chi, df = 1)

# ------------------------------------------------------------
# At-risk numbers
# ------------------------------------------------------------
atrisk_times <- seq(0, 330, by = 30)
atrisk_tbl <- expand.grid(arm = arms, atrisk_time = atrisk_times) %>%
  as_tibble() %>%
  mutate(n_at_risk = map2_int(arm, atrisk_time, function(a,t) sum(one$arm == a & one$day >= t))) %>%
  arrange(arm, atrisk_time)

# ------------------------------------------------------------
# Plot with at-risk table
# ------------------------------------------------------------
plot_data <- km_all %>%
  arrange(arm, time) %>%
  group_by(arm) %>%
  group_modify(~ add_row(.x, time = 0, survival = 1, .before = 1)) %>%
  ungroup()

# x-axis ticks for SAS (0 50 100 150 200 250 300)
x_ticks <- seq(0, 350, by = 50)

p_main <- ggplot(plot_data, aes(x = time, y = survival, color = arm, linetype = arm)) +
  geom_step(linewidth = 0.9) +
  geom_point(data = filter(km_all, n_censor > 0 & n_event == 0),
             aes(x = time, y = survival), shape = 3, size = 2, show.legend = FALSE) +
  scale_x_continuous(breaks = x_ticks, limits = c(0, 350)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Example K-M Plot", x = "Day", y = "Survival Probability", color = "Stratum", linetype = "Stratum") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")

# Build at-risk table grob
atrisk_mat <- atrisk_tbl %>%
  pivot_wider(names_from = atrisk_time, values_from = n_at_risk) %>%
  arrange(arm)
# create table plot
tbl_df <- atrisk_tbl %>%
  mutate(label = as.character(n_at_risk)) %>%
  select(arm, atrisk_time, label)

# Use annotation: create a table grob
atrisk_wide <- atrisk_tbl %>% pivot_wider(names_from = atrisk_time, values_from = n_at_risk)
# Save main plot
png_file <- file.path(out, "P040_f_kmplot.png")
# Combine plot and table using gridExtra
# Create table grob
table_grob <- tableGrob(atrisk_wide, rows = NULL, theme = ttheme_minimal(base_size = 8))
# Save with extra height for table
png(png_file, width = 8, height = 6.5, units = "in", res = 300)
grid.newpage()
# layout: 5/6 for plot, 1/6 for table
pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(5, 1.5), "null"))))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_main, vp = viewport())
popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(table_grob)
popViewport()
dev.off()

# Also save with ggsave fallback if needed (ensure file exists)
if (!file.exists(png_file)) {
  ggsave(png_file, plot = p_main, width = 8, height = 6, dpi = 300, units = "in")
}

plot_data_csv <- km_all %>%
  select(arm, time, survival, failure, se, n_failed, n_left, n_risk, n_event, n_censor)
write.csv(plot_data_csv, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

csv_main <- km_all %>%
  select(arm, time, survival, failure, se, n_failed, n_left, n_event, n_censor) %>%
  arrange(arm, time)
write.csv(csv_main, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Text rendering - include all SAS blocks
# ------------------------------------------------------------
txt_file <- file.path(out, "P040_f_kmplot.txt")
sink(txt_file)

cat("Example K-M Plot\n")
cat("PROC LIFETEST\n")
cat("Time to Event (day) * censor(1)\n")
cat("Strata: arm\n")
cat("\n")
cat("Product-Limit Survival Estimates\n")
cat("\n")
for (a in arms) {
  cat(paste0("Stratum: ", a, "  N = ", sum(one$arm == a), "\n"))
  cat(sprintf("%-10s %10s %10s %12s %10s %10s\n", "Time", "Survival", "Failure", "StdErr", "Failed", "Left"))
  km <- filter(km_all, arm == a) %>% arrange(time)
  for (i in 1:nrow(km)) {
    r <- km[i,]
    t_str <- fmt_n(r$time, 3)
    if (r$n_event == 0 && r$n_censor > 0) t_str <- paste0(t_str, "*")
    cat(sprintf("%-10s %10s %10s %12s %10d %10d\n",
                t_str, fmt_n(r$survival,4), fmt_n(r$failure,4), fmt_n(r$se,4), r$n_failed, r$n_left))
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
                a, r$percent,
                ifelse(is.na(r$point), ".", fmt_n(r$point,3)),
                ifelse(is.na(r$lower), ".", fmt_n(r$lower,3)),
                ifelse(is.na(r$upper), ".", fmt_n(r$upper,3))))
  }
}
cat("\n")

cat("Mean Survival Time and Standard Error (Restricted Mean)\n")
cat(sprintf("%-10s %12s %12s %10s %8s\n", "Stratum", "Mean", "StdErr", "Tau", "N"))
for (i in 1:nrow(mean_all)) {
  r <- mean_all[i,]
  cat(sprintf("%-10s %12s %12s %10s %8d\n",
              r$arm, fmt_n(r$mean,3), fmt_n(r$se_mean,3), fmt_n(r$tau,3), sum(one$arm == r$arm)))
}
cat("\n")

cat("Summary of the Number of Censored and Uncensored Values\n")
cat(sprintf("%-10s %8s %8s %8s %12s\n", "Stratum", "Total", "Failed", "Censored", "Pct Cens"))
for (i in 1:nrow(summary_tbl2)) {
  r <- summary_tbl2[i,]
  cat(sprintf("%-10s %8d %8d %8d %12s\n",
              r$arm, r$total, r$failed, r$censored, fmt_n(r$percent_censored,2)))
}
cat("\n")
cat(sprintf("Total observations: %d  Total events: %d  Total censored: %d  Percent censored: %s\n",
            nrow(one), sum(one$censor==0), sum(one$censor==1), fmt_n(sum(one$censor==1)/nrow(one)*100,2)))
cat("\n")

cat("Rank Statistics\n")
cat(sprintf("%-10s %12s %12s\n", "Stratum", "Log-Rank", "Wilcoxon"))
cat(sprintf("%-10s %12s %12s\n", arms[1], fmt_n(U_logrank_1,4), fmt_n(U_wilcoxon_1,4)))
cat(sprintf("%-10s %12s %12s\n", arms[2], fmt_n(U_logrank_2,4), fmt_n(U_wilcoxon_2,4)))
cat("\n")

cat("Covariance Matrix for the Log-Rank Statistics\n")
cat(sprintf("%10s %12s %12s\n", "", arms[1], arms[2]))
for (i in 1:2) {
  cat(sprintf("%10s %12s %12s\n", arms[i], fmt_n(cov_logrank[i,1],4), fmt_n(cov_logrank[i,2],4)))
}
cat("\n")
cat("Covariance Matrix for the Wilcoxon Statistics\n")
cat(sprintf("%10s %12s %12s\n", "", arms[1], arms[2]))
for (i in 1:2) {
  cat(sprintf("%10s %12s %12s\n", arms[i], fmt_n(cov_wilcoxon[i,1],4), fmt_n(cov_wilcoxon[i,2],4)))
}
cat("\n")

cat("Test of Equality over Strata\n")
cat(sprintf("%-12s %12s %4s %12s\n", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
cat(sprintf("%-12s %12s %4d %12s\n", "Log-Rank", fmt_n(chi_logrank,4), 1, fmt_p(p_logrank)))
cat(sprintf("%-12s %12s %4d %12s\n", "Wilcoxon", fmt_n(chi_wilcoxon,4), 1, fmt_p(p_wilcoxon)))
cat(sprintf("%-12s %12s %4d %12s\n", "-2Log(LR)", fmt_n(lr_chi,4), 1, fmt_p(p_lr)))
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

cat("Plot X-axis ticks (Day)\n")
cat(paste(x_ticks, collapse=" "))
cat("\n")
cat("X-axis labels: 0 50 100 150 200 250 300 350\n")
cat("Y-axis labels: 0 0.2 0.4 0.6 0.8 1.0\n")
cat("\n")
cat("Note: * indicates censored observation\n")
cat("Restricted mean is area under survival curve up to largest observed time\n")
cat(sprintf("SE of restricted mean uses SAS formula: sqrt(m/(m-1)*sum A_i^2*d_i/(n_i*(n_i-d_i))) with m events\n"))
cat(sprintf("m Arm 1 = %d  m Arm 2 = %d\n", mean_all$m[mean_all$arm=="Arm 1"], mean_all$m[mean_all$arm=="Arm 2"]))
cat("\n")
cat("Figure: Survival plot with atrisk table 0 to 330 by 30\n")
cat("Title1: Example K-M Plot\n")

sink()

stopifnot(file.exists(file.path(out, "P040_f_kmplot.csv")))
stopifnot(file.exists(file.path(out, "P040_f_kmplot.txt")))
stopifnot(file.exists(file.path(out, "P040_f_kmplot.png")))
stopifnot(file.exists(file.path(out, "P040_f_kmplot_plotdata.csv")))
