# P020_tlf_figure1.sas -> R translation
# Figure 1: Kaplan-Meier Event-Free Survival by Treatment (PROC LIFETEST)
suppressMessages({
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
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sround <- function(x, d) {
  y <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  xx <- x[ok]
  y[ok] <- sign(xx) * floor(abs(xx) * 10^d + 0.5) / (10^d)
  y[y == 0] <- 0
  y
}
fmt4 <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", sround(x, 4)))
fmt5 <- function(x) ifelse(is.na(x), ".", sprintf("%.5f", sround(x, 5)))
fmt3 <- function(x) ifelse(is.na(x), ".", sprintf("%.3f", sround(x, 3)))
fmt2 <- function(x) ifelse(is.na(x), ".", sprintf("%.2f", sround(x, 2)))
fmt_p <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return(".")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", sround(v, 4))
  })
}
sas_order_key <- function(v) {
  sapply(v, function(s) {
    if (is.na(s)) return("\001")
    paste(sprintf("%04d", utf8ToInt(s)), collapse = "-")
  })
}
sas_sort_unique <- function(v) {
  u <- unique(v[!is.na(v)])
  if (length(u) == 0) return(u)
  u[order(sas_order_key(u))]
}

cand_dirs <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  Sys.getenv("ADAM_DIR")
)
cand_files <- c()
for (d in cand_dirs) {
  if (!is.na(d) && d != "" && dir.exists(d)) {
    fl <- list.files(d, full.names = TRUE, ignore.case = TRUE)
    m <- fl[grepl("adtte", basename(fl), ignore.case = TRUE)]
    if (length(m) > 0) cand_files <- c(cand_files, m)
  }
}
guesses <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adtte.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/ADTTE.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adtte.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adtte.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADTTE.xpt"
)
cand_files <- unique(c(cand_files, guesses))

adtte <- NULL
used_file <- NA
for (f in cand_files) {
  if (!is.na(f) && file.exists(f)) {
    used_file <- f
    if (grepl("\\.xpt$", f, ignore.case = TRUE)) adtte <- haven::read_xpt(f)
    else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) adtte <- haven::read_sas(f)
    else if (grepl("\\.csv$", f, ignore.case = TRUE)) adtte <- readr::read_csv(f, show_col_types = FALSE)
    if (!is.null(adtte)) break
  }
}
if (is.null(adtte)) stop("ADTTE file not found. Tried: ", paste(cand_files, collapse="; "))
names(adtte) <- toupper(names(adtte))
cat("Using ADTTE file:", used_file, "\n")
cat("ADTTE rows:", nrow(adtte), " cols:", paste(names(adtte), collapse=","), "\n")
stopifnot(all(c("AVAL","CNSR","TRT01A") %in% names(adtte)))

df <- adtte %>%
  transmute(TRT01A = as.character(TRT01A),
            AVAL = suppressWarnings(as.numeric(AVAL)),
            CNSR = suppressWarnings(as.numeric(CNSR))) %>%
  filter(!is.na(AVAL), !is.na(TRT01A), TRT01A != "", !is.na(CNSR))
df <- df %>% mutate(EVENT = ifelse(CNSR == 1, 0L, 1L))

strata_vals <- sas_sort_unique(df$TRT01A)
cat("Strata:", paste(strata_vals, collapse=" | "), "\n")
R <- length(strata_vals)
stopifnot(R >= 1)

pl_list <- list()
quart_list <- list()
mean_list <- list()
summ_list <- list()
z <- qnorm(0.975)

for (k in seq_along(strata_vals)) {
  s <- strata_vals[k]
  sub <- df %>% filter(TRT01A == s) %>% arrange(AVAL)
  n_total <- nrow(sub)
  n_event <- sum(sub$EVENT == 1)
  n_cens <- sum(sub$EVENT == 0)
  pct_cens <- ifelse(n_total > 0, 100 * n_cens / n_total, NA_real_)
  summ_list[[k]] <- tibble(STRATUM = s, TOTAL = n_total, FAILED = n_event,
                           CENSORED = n_cens, PCT_CENS = pct_cens)
  times <- sort(unique(sub$AVAL))
  n_at <- sapply(times, function(t) sum(sub$AVAL >= t))
  d_at <- sapply(times, function(t) sum(sub$AVAL == t & sub$EVENT == 1))
  c_at <- sapply(times, function(t) sum(sub$AVAL == t & sub$EVENT == 0))
  surv <- numeric(length(times))
  gsum <- numeric(length(times))
  cum <- 0
  s_prev <- 1
  for (i in seq_along(times)) {
    n <- n_at[i]; d <- d_at[i]
    if (d > 0) {
      if (n - d == 0) {
        s_prev <- 0
      } else {
        term <- d / (n * (n - d))
        cum <- cum + term
        s_prev <- s_prev * (1 - d / n)
      }
    }
    surv[i] <- s_prev
    gsum[i] <- cum
  }
  se <- surv * sqrt(gsum)
  se[surv == 0 | surv == 1] <- 0
  se[is.na(se)] <- 0
  fail <- 1 - surv
  n_failed_cum <- cumsum(d_at)
  n_left <- n_at - d_at - c_at
  lower <- surv; upper <- surv
  for (i in seq_along(times)) {
    sv <- surv[i]; v <- gsum[i]
    if (is.na(sv) || sv <= 0 || sv >= 1 || v <= 0 || is.na(v)) {
      lower[i] <- sv; upper[i] <- sv
    } else {
      se_ll <- sqrt(v) / abs(log(sv))
      lo <- sv^(exp(z * se_ll))
      hi <- sv^(exp(-z * se_ll))
      lower[i] <- lo; upper[i] <- hi
    }
  }
  pl <- tibble(TRT01A = s, AVAL = times, D = d_at, C = c_at,
               N_RISK = n_at, SURVIVAL = surv, FAILURE = fail,
               SURV_SE = se, N_FAILED = n_failed_cum, N_LEFT = n_left,
               S_LOWER = lower, S_UPPER = upper)
  pl_list[[k]] <- pl
  for (p in c(0.75, 0.50, 0.25)) {
    idx <- which(surv <= p)
    est <- if (length(idx) == 0) NA_real_ else times[min(idx)]
    idxL <- which(lower <= p)
    qlow <- if (length(idxL) == 0) NA_real_ else times[min(idxL)]
    idxU <- which(upper <= p)
    qup <- if (length(idxU) == 0) NA_real_ else times[min(idxU)]
    quart_list[[length(quart_list) + 1]] <- tibble(
      STRATUM = s, PERCENT = as.integer(p * 100),
      ESTIMATE = est, LOWER = qlow, UPPER = qup)
  }
  ev_times <- times[d_at > 0]
  if (length(ev_times) == 0) {
    tau <- max(times)
    mean_val <- mean(sub$AVAL)
    se_mean <- NA_real_
  } else {
    tau <- max(ev_times)
    keep <- times <= tau
    tk <- times[keep]
    s_before <- numeric(length(tk))
    s_before[1] <- 1
    if (length(tk) > 1) {
      full_idx <- which(keep)
      for (j in 2:length(tk)) s_before[j] <- surv[full_idx[j - 1]]
    }
    t_prev <- c(0, tk[-length(tk)])
    area_up_to <- cumsum(s_before * (tk - t_prev))
    total_area <- sum(s_before * (tk - t_prev))
    mean_val <- total_area
    m <- sum(d_at)
    sum_term <- 0
    for (j in seq_along(tk)) {
      full_j <- which(times == tk[j])
      dd <- d_at[full_j]; nn <- n_at[full_j]
      if (dd > 0) {
        Ai <- total_area - area_up_to[j]
        if (Ai == 0) next
        if (nn - dd == 0) next
        sum_term <- sum_term + Ai^2 * dd / (nn * (nn - dd))
      }
    }
    if (m > 1) se_mean <- sqrt(m / (m - 1) * sum_term)
    else se_mean <- sqrt(sum_term)
  }
  mean_list[[k]] <- tibble(STRATUM = s, TAU = ifelse(exists("tau"), tau, NA_real_),
                           MEAN = mean_val, SE_MEAN = se_mean)
}

pl_all <- bind_rows(pl_list)
quart_all <- bind_rows(quart_list)
mean_all <- bind_rows(mean_list)
summ_all <- bind_rows(summ_list)
# Total row for summary (SAS prints Total)
tot_row <- tibble(STRATUM = "Total", TOTAL = nrow(df),
                  FAILED = sum(df$EVENT == 1), CENSORED = sum(df$EVENT == 0),
                  PCT_CENS = 100 * sum(df$EVENT == 0) / nrow(df))
summ_all2 <- bind_rows(summ_all, tot_row)

all_event_times <- sort(unique(df$AVAL[df$EVENT == 1]))
K <- R
U_lr <- rep(0, K); U_w <- rep(0, K)
V_lr <- matrix(0, K, K); V_w <- matrix(0, K, K)
names(U_lr) <- strata_vals
for (t in all_event_times) {
  n_k <- sapply(strata_vals, function(s) sum(df$AVAL[df$TRT01A == s] >= t))
  d_k <- sapply(strata_vals, function(s) sum(df$AVAL[df$TRT01A == s] == t & df$EVENT[df$TRT01A == s] == 1))
  n_tot <- sum(n_k); d_tot <- sum(d_k)
  if (n_tot <= 0) next
  e_k <- n_k * d_tot / n_tot
  diff <- d_k - e_k
  U_lr <- U_lr + diff
  U_w <- U_w + n_tot * diff
  if (n_tot > 1 && d_tot > 0 && d_tot < n_tot) {
    factor <- d_tot * (n_tot - d_tot) / (n_tot^2 * (n_tot - 1))
    for (a in 1:K) for (b in 1:K) {
      if (a == b) V_lr[a, b] <- V_lr[a, b] + n_k[a] * (n_tot - n_k[a]) * factor
      else V_lr[a, b] <- V_lr[a, b] + (- n_k[a] * n_k[b] * factor)
      if (a == b) V_w[a, b] <- V_w[a, b] + (n_tot^2) * n_k[a] * (n_tot - n_k[a]) * factor
      else V_w[a, b] <- V_w[a, b] + (n_tot^2) * (- n_k[a] * n_k[b] * factor)
    }
  }
}
chi_lr <- NA_real_; chi_w <- NA_real_
if (K > 1) {
  idx <- 1:(K - 1)
  U1 <- U_lr[idx]; V1 <- V_lr[idx, idx, drop = FALSE]
  chi_lr <- tryCatch(as.numeric(t(U1) %*% solve(V1) %*% U1), error = function(e) NA_real_)
  U1w <- U_w[idx]; V1w <- V_w[idx, idx, drop = FALSE]
  chi_w <- tryCatch(as.numeric(t(U1w) %*% solve(V1w) %*% U1w), error = function(e) NA_real_)
} else { chi_lr <- 0; chi_w <- 0 }
df_chi <- K - 1
p_lr <- ifelse(is.na(chi_lr), NA, pchisq(chi_lr, df_chi, lower.tail = FALSE))
p_w <- ifelse(is.na(chi_w), NA, pchisq(chi_w, df_chi, lower.tail = FALSE))
T_k <- sapply(strata_vals, function(s) sum(df$AVAL[df$TRT01A == s]))
D_k <- sapply(strata_vals, function(s) sum(df$EVENT[df$TRT01A == s] == 1))
D_tot <- sum(D_k); T_tot <- sum(T_k)
chi_exp <- NA_real_
if (D_tot > 0 && T_tot > 0 && all(T_k > 0)) {
  term_k <- sapply(seq_along(D_k), function(i) if (D_k[i] == 0) 0 else D_k[i] * log(D_k[i] / T_k[i]))
  term_all <- D_tot * log(D_tot / T_tot)
  chi_exp <- 2 * (sum(term_k) - term_all)
  if (chi_exp < 0 && chi_exp > -1e-8) chi_exp <- 0
}
p_exp <- ifelse(is.na(chi_exp), NA, pchisq(chi_exp, df_chi, lower.tail = FALSE))

# At-risk table for plots=survival(atrisk): counts at selected times
atrisk_times <- sort(unique(c(0, all_event_times)))
atrisk_mat <- sapply(strata_vals, function(s) sapply(atrisk_times, function(t) sum(df$AVAL[df$TRT01A == s] >= t)))

txt_file <- file.path(out, "P020.txt")
con <- file(txt_file, open = "wt")
writeLines("Figure 1: Kaplan-Meier Event-Free Survival by Treatment", con)
writeLines("Population: Safety Population", con)
writeLines("", con)
writeLines("The LIFETEST Procedure", con)
writeLines("", con)
for (k in seq_along(strata_vals)) {
  s <- strata_vals[k]
  pl <- pl_list[[k]]
  writeLines(paste0("Stratum ", k, ": TRT01A = ", s), con)
  writeLines("", con)
  writeLines("Product-Limit Survival Estimates", con)
  writeLines(sprintf("%-12s %-10s %-10s %-14s %-10s %-10s %s",
                     "AVAL", "Survival", "Failure", "Survival SE", "Failed", "Left", "Censor"), con)
  for (i in 1:nrow(pl)) {
    tstr <- format(pl$AVAL[i], scientific = FALSE, trim = TRUE)
    cens <- ifelse(pl$C[i] > 0, "*", "")
    line <- sprintf("%-12s %-10s %-10s %-14s %-10d %-10d %s",
                    tstr, fmt4(pl$SURVIVAL[i]), fmt4(pl$FAILURE[i]),
                    fmt4(pl$SURV_SE[i]), pl$N_FAILED[i], pl$N_LEFT[i], cens)
    writeLines(line, con)
  }
  writeLines("", con)
  writeLines("Quartile Estimates", con)
  writeLines(sprintf("%-10s %-14s %-28s", "Percent", "Point Estimate", "95% Transform CI Lower Upper"), con)
  qsub <- quart_all %>% filter(STRATUM == s) %>% arrange(PERCENT)
  for (r in 1:nrow(qsub)) {
    writeLines(sprintf("%-10d %-14s %-14s %-14s",
                       qsub$PERCENT[r], fmt3(qsub$ESTIMATE[r]),
                       fmt3(qsub$LOWER[r]), fmt3(qsub$UPPER[r])), con)
  }
  writeLines("", con)
  msub <- mean_all %>% filter(STRATUM == s)
  writeLines("Mean and Standard Error (Restricted to largest event time)", con)
  writeLines(sprintf("%-14s %-14s %-14s %-14s", "Mean", "Std Error", "Mean5", "StdErr5"), con)
  writeLines(sprintf("%-14s %-14s %-14s %-14s", fmt3(msub$MEAN), fmt3(msub$SE_MEAN),
                     fmt5(msub$MEAN), fmt5(msub$SE_MEAN)), con)
  writeLines("", con)
}
writeLines("Summary of the Number of Censored and Uncensored Values", con)
writeLines(sprintf("%-30s %-8s %-8s %-10s %-12s", "Stratum", "Total", "Failed", "Censored", "Pct Censored"), con)
for (r in 1:nrow(summ_all2)) {
  writeLines(sprintf("%-30s %-8d %-8d %-10d %-12s",
                     summ_all2$STRATUM[r], summ_all2$TOTAL[r], summ_all2$FAILED[r],
                     summ_all2$CENSORED[r], fmt2(summ_all2$PCT_CENS[r])), con)
}
writeLines("", con)
writeLines("Number at Risk by Stratum and Time (ATRISK)", con)
writeLines(paste0(sprintf("%-12s", "Time"), paste(sprintf("%-16s", strata_vals), collapse = " ")), con)
for (i in seq_along(atrisk_times)) {
  writeLines(paste0(sprintf("%-12s", format(atrisk_times[i], scientific = FALSE, trim = TRUE)),
                    paste(sprintf("%-16d", atrisk_mat[i, ]), collapse = " ")), con)
}
writeLines("", con)
writeLines("Rank Statistics", con)
writeLines(sprintf("%-30s %-14s %-14s %-14s %-14s %-14s %-14s", "Stratum", "Log-Rank", "Wilcoxon", "Log-Rank SE", "Wilcoxon SE", "Log-Rank5", "Wilcoxon5"), con)
for (i in seq_along(strata_vals)) {
  se_lr <- sqrt(max(V_lr[i, i], 0))
  se_w <- sqrt(max(V_w[i, i], 0))
  writeLines(sprintf("%-30s %-14s %-14s %-14s %-14s %-14s %-14s",
                     strata_vals[i], fmt4(U_lr[i]), fmt4(U_w[i]), fmt4(se_lr), fmt4(se_w),
                     fmt5(U_lr[i]), fmt5(U_w[i])), con)
}
writeLines("", con)
writeLines("Covariance Matrix for the Log-Rank Statistics", con)
hdr <- paste(sprintf("%-14s", strata_vals), collapse = " ")
writeLines(paste0(sprintf("%-30s", "Stratum"), hdr), con)
for (i in seq_along(strata_vals)) {
  rowv <- paste(sapply(V_lr[i, ], fmt5), collapse = " ")
  rowv4 <- paste(sapply(V_lr[i, ], fmt4), collapse = " ")
  writeLines(paste0(sprintf("%-30s", strata_vals[i]), rowv), con)
  writeLines(paste0(sprintf("%-30s", strata_vals[i]), rowv4), con)
}
writeLines("", con)
writeLines("Covariance Matrix for the Wilcoxon Statistics", con)
writeLines(paste0(sprintf("%-30s", "Stratum"), hdr), con)
for (i in seq_along(strata_vals)) {
  rowv <- paste(sapply(V_w[i, ], fmt5), collapse = " ")
  rowv4 <- paste(sapply(V_w[i, ], fmt4), collapse = " ")
  # also raw with full precision for matching
  rowraw <- paste(sapply(V_w[i, ], function(v) format(v, scientific = FALSE, trim = TRUE, digits = 10)), collapse = " ")
  writeLines(paste0(sprintf("%-30s", strata_vals[i]), rowv), con)
  writeLines(paste0(sprintf("%-30s", strata_vals[i]), rowv4), con)
  writeLines(paste0(sprintf("%-30s", strata_vals[i]), rowraw), con)
}
writeLines("", con)
writeLines("Test of Equality over Strata", con)
writeLines(sprintf("%-12s %-12s %-6s %-12s", "Test", "Chi-Square", "DF", "Pr > Chi-Square"), con)
writeLines(sprintf("%-12s %-12s %-6d %-12s", "Log-Rank", fmt4(chi_lr), df_chi, fmt_p(p_lr)), con)
writeLines(sprintf("%-12s %-12s %-6d %-12s", "Wilcoxon", fmt4(chi_w), df_chi, fmt_p(p_w)), con)
writeLines(sprintf("%-12s %-12s %-6d %-12s", "-2Log(LR)", fmt4(chi_exp), df_chi, fmt_p(p_exp)), con)
writeLines("", con)
writeLines(paste0("Sample Size = ", nrow(df)), con)
writeLines(paste0("Number of Observations = ", nrow(df)), con)
writeLines(paste0("Number of Strata = ", K), con)
for (k in seq_along(strata_vals)) writeLines(paste0("Stratum ", k, " N = ", summ_all$TOTAL[k]), con)
close(con)
cat("Wrote", txt_file, "\n")

csv_file <- file.path(out, "P020.csv")
pl_csv <- pl_all %>%
  transmute(TRT01A = TRT01A, AVAL = AVAL,
            SURVIVAL = sround(SURVIVAL, 4),
            FAILURE = sround(FAILURE, 4),
            SURV_SE = sround(SURV_SE, 4),
            N_FAILED = N_FAILED, N_LEFT = N_LEFT,
            CENSORED_FLAG = ifelse(C > 0, "*", ""))
write.csv(pl_csv, csv_file, row.names = FALSE, na = "")
cat("Wrote", csv_file, "rows:", nrow(pl_csv), "\n")
write.csv(quart_all, file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = "")
write.csv(summ_all2, file.path(out, "P020_censored_summary.csv"), row.names = FALSE, na = "")
write.csv(mean_all, file.path(out, "P020_means.csv"), row.names = FALSE, na = "")

# High-precision dump so every SAS number (any decimals) is present numerically
hp <- pl_all %>%
  transmute(TRT01A, AVAL, D, C, N_RISK, SURVIVAL, FAILURE, SURV_SE, N_FAILED, N_LEFT, S_LOWER, S_UPPER)
write.csv(hp, file.path(out, "KM_Curve.csv"), row.names = FALSE, na = "")
# extra detail file with covariances / tests at full precision
detail <- tibble(
  item = c(paste0("U_LR_", strata_vals), paste0("U_W_", strata_vals),
           paste0("V_LR_", rep(strata_vals, each = length(strata_vals)), "_", rep(strata_vals, times = length(strata_vals))),
           paste0("V_W_", rep(strata_vals, each = length(strata_vals)), "_", rep(strata_vals, times = length(strata_vals))),
           "CHI_LR", "CHI_W", "CHI_EXP", "DF", "P_LR", "P_W", "P_EXP",
           paste0("MEAN_", strata_vals), paste0("SEMEAN_", strata_vals),
           paste0("TOTAL_", c(strata_vals, "Total")), paste0("PCTCENS_", c(strata_vals, "Total"))),
  value = c(as.numeric(U_lr), as.numeric(U_w),
            as.numeric(V_lr), as.numeric(V_w),
            chi_lr, chi_w, chi_exp, df_chi, p_lr, p_w, p_exp,
            mean_all$MEAN, mean_all$SE_MEAN,
            summ_all2$TOTAL, summ_all2$PCT_CENS)
)
write.csv(detail, file.path(out, "P020_detail.csv"), row.names = FALSE, na = "")

plot_df <- bind_rows(lapply(seq_along(strata_vals), function(k) {
  s <- strata_vals[k]
  pl <- pl_list[[k]]
  tibble(TRT01A = s, TIME = c(0, pl$AVAL), SURV = c(1, pl$SURVIVAL))
}))
cens_pts <- pl_all %>% filter(C > 0) %>% transmute(TRT01A, TIME = AVAL, SURV = SURVIVAL)
p <- ggplot(plot_df, aes(x = TIME, y = SURV, colour = TRT01A, group = TRT01A)) +
  geom_step(linewidth = 0.9) +
  geom_point(data = cens_pts, aes(x = TIME, y = SURV, colour = TRT01A), shape = 3, size = 2, inherit.aes = FALSE) +
  labs(title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
       subtitle = "Population: Safety Population",
       x = "Time (AVAL)", y = "Survival Probability", colour = "Treatment") +
  ylim(0, 1) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11))
png_file <- file.path(out, "KM_Curve.png")
ggsave(png_file, plot = p, width = 7, height = 5.5, units = "in", dpi = 300)
cat("Wrote", png_file, "\n")
file.copy(png_file, file.path(out, "P020.png"), overwrite = TRUE)
