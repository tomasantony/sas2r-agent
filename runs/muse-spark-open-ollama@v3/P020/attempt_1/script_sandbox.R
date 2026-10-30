# P020_tlf_figure1.sas -> R translation
# Figure 1: Kaplan-Meier Event-Free Survival by Treatment
# PROC LIFETEST with STRATA TRT01A, TIME AVAL*CNSR(1)

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
  library(ggplot2)
  library(survival)
  library(survminer)
})

# ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt4 <- function(x) ifelse(is.na(x), "", sprintf("%.4f", round_half_up(x, 4)))
fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", round_half_up(x, 3)))
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", round_half_up(x, 2)))
fmt1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", round_half_up(x, 1)))

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- Sys.getenv("project_path")
if (project_path == "" || is.na(project_path)) project_path <- "/bench/data/derived/c2c"
rawadam_env <- Sys.getenv("rawadam")
if (rawadam_env == "" || is.na(rawadam_env)) rawadam_env <- "/bench/data/raw/cdisc-pilot/adam"

# ---------- locate ADTTE ----------
find_adtte <- function() {
  candidates <- c(
    file.path(project_path, "data/adam/adtte.sas7bdat"),
    file.path(project_path, "data/adam/ADTTe.sas7bdat"),
    file.path(project_path, "data/adam/adtte.xpt"),
    "/bench/data/derived/c2c/data/adam/adtte.sas7bdat",
    "/bench/data/derived/c2c/data/adam/adtte.xpt",
    "/bench/data/derived/c2c/data/adam/ADTTe.sas7bdat",
    file.path(rawadam_env, "adtte.xpt"),
    file.path(rawadam_env, "adtte.sas7bdat"),
    "/bench/data/raw/cdisc-pilot/adam/adtte.xpt"
  )
  for (f in candidates) if (file.exists(f)) return(f)
  # search recursively
  search_roots <- unique(c(project_path, "/bench/data/derived/c2c",
                           "/bench/data/raw/cdisc-pilot/adam",
                           rawadam_env, "/bench"))
  for (r in search_roots) {
    if (!dir.exists(r)) next
    ff <- list.files(r, pattern = "adtte", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
    if (length(ff) > 0) {
      # prefer sas7bdat/xpt
      ff2 <- ff[grepl("\\.(sas7bdat|xpt)$", ff, ignore.case = TRUE)]
      if (length(ff2) > 0) return(ff2[1])
      return(ff[1])
    }
  }
  return(NA)
}

adtte_path <- find_adtte()
cat("ADTTE path:", adtte_path, "\n")
stopifnot(!is.na(adtte_path), file.exists(adtte_path))

read_adtte <- function(p) {
  ext <- tolower(tools::file_ext(p))
  if (ext == "sas7bdat") haven::read_sas(p)
  else if (ext == "xpt") haven::read_xpt(p)
  else if (ext == "csv") readr::read_csv(p, show_col_types = FALSE)
  else haven::read_sas(p)
}
adtte_raw <- read_adtte(adtte_path)
names(adtte_raw) <- toupper(names(adtte_raw))
cat("Columns:", paste(names(adtte_raw), collapse = ","), "\n")

# ensure required columns exist
stopifnot("AVAL" %in% names(adtte_raw))
stopifnot("CNSR" %in% names(adtte_raw))
# TRT01A may be TRT01A or TRT01AN with format
if (!"TRT01A" %in% names(adtte_raw)) {
  if ("TRT01AN" %in% names(adtte_raw)) {
    adtte_raw$TRT01A <- as.character(adtte_raw$TRT01AN)
  } else if ("TRTA" %in% names(adtte_raw)) {
    adtte_raw$TRT01A <- as.character(adtte_raw$TRTA)
  } else stop("TRT01A not found")
}
# PARAM handling: if multiple PARAMs, use all (SAS does) but log
if ("PARAM" %in% names(adtte_raw)) {
  cat("PARAM distinct:", paste(unique(adtte_raw$PARAM), collapse = " | "), "\n")
}
# clean
adtte <- adtte_raw %>%
  mutate(
    AVAL = suppressWarnings(as.numeric(AVAL)),
    CNSR = suppressWarnings(as.numeric(CNSR)),
    TRT01A = as.character(TRT01A)
  ) %>%
  filter(!is.na(AVAL), !is.na(CNSR), !is.na(TRT01A), TRT01A != "")

# SAS sorts character ordering uppercase before lowercase; use base sort
strata_levels <- sort(unique(adtte$TRT01A))
cat("Strata:", paste(strata_levels, collapse = ", "), "\n")
cat("N total:", nrow(adtte), "\n")
stopifnot(length(strata_levels) >= 1)

# ---------- KM per stratum ----------
z_alpha <- qnorm(0.975)

compute_km_stratum <- function(df_stratum) {
  # df_stratum has AVAL, CNSR
  df_stratum <- df_stratum %>% arrange(AVAL)
  distinct_times <- sort(unique(df_stratum$AVAL))
  n_times <- length(distinct_times)
  res <- tibble(
    TIME = distinct_times,
    N_AT_RISK = NA_real_,
    D = NA_real_,
    C = NA_real_,
    SURVIVAL = NA_real_,
    FAILURE = NA_real_,
    STDERR = NA_real_,
    SIGMA2 = NA_real_,
    LOWER_CI = NA_real_,
    UPPER_CI = NA_real_
  )
  S_prev <- 1
  sigma2_prev <- 0
  for (i in seq_along(distinct_times)) {
    t <- distinct_times[i]
    n <- sum(df_stratum$AVAL >= t)
    d <- sum(df_stratum$AVAL == t & df_stratum$CNSR == 0)
    c_ <- sum(df_stratum$AVAL == t & df_stratum$CNSR == 1)
    res$N_AT_RISK[i] <- n
    res$D[i] <- d
    res$C[i] <- c_
    if (d > 0) {
      if (n - d == 0) {
        S_cur <- 0
        sigma2_cur <- sigma2_prev + 1e12 # large, but S=0 => SE 0
      } else {
        S_cur <- S_prev * (1 - d / n)
        sigma2_cur <- sigma2_prev + d / (n * (n - d))
      }
    } else {
      S_cur <- S_prev
      sigma2_cur <- sigma2_prev
    }
    res$SURVIVAL[i] <- S_cur
    res$FAILURE[i] <- 1 - S_cur
    res$SIGMA2[i] <- sigma2_cur
    if (S_cur == 1 || S_cur == 0 || sigma2_cur == 0) {
      res$STDERR[i] <- ifelse(S_cur == 0, 0, S_cur * sqrt(sigma2_cur))
      res$LOWER_CI[i] <- S_cur
      res$UPPER_CI[i] <- S_cur
    } else {
      se <- S_cur * sqrt(sigma2_cur)
      res$STDERR[i] <- se
      logS <- log(S_cur)
      # var log-log
      var_loglog <- sigma2_cur / (logS * logS)
      se_loglog <- sqrt(var_loglog)
      loglog <- log(-log(S_cur))
      lower_loglog <- loglog - z_alpha * se_loglog
      upper_loglog <- loglog + z_alpha * se_loglog
      # back transform: lower_S = exp(-exp(upper)), upper_S = exp(-exp(lower))
      lower_S <- exp(-exp(upper_loglog))
      upper_S <- exp(-exp(lower_loglog))
      # clamp 0-1
      lower_S <- max(0, min(1, lower_S))
      upper_S <- max(0, min(1, upper_S))
      res$LOWER_CI[i] <- lower_S
      res$UPPER_CI[i] <- upper_S
    }
    S_prev <- S_cur
    sigma2_prev <- sigma2_cur
  }
  res
}

km_list <- list()
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  km_list[[lv]] <- compute_km_stratum(df_s)
}

# ---------- Quartile estimates ----------
quartile_p <- c(0.75, 0.50, 0.25)
quartile_tbl <- tibble()
for (lv in strata_levels) {
  km <- km_list[[lv]]
  for (p in quartile_p) {
    # point estimate: smallest TIME where SURVIVAL <= p
    idx <- which(km$SURVIVAL <= p)
    pt <- if (length(idx) > 0) km$TIME[min(idx)] else NA_real_
    # lower CI for quartile: smallest TIME where UPPER_CI <= p
    idx_l <- which(km$UPPER_CI <= p)
    lower_q <- if (length(idx_l) > 0) km$TIME[min(idx_l)] else NA_real_
    # upper CI: smallest TIME where LOWER_CI <= p
    idx_u <- which(km$LOWER_CI <= p)
    upper_q <- if (length(idx_u) > 0) km$TIME[min(idx_u)] else NA_real_
    quartile_tbl <- bind_rows(quartile_tbl, tibble(
      STRATUM = lv,
      PERCENT = p * 100,
      POINT_ESTIMATE = pt,
      LOWER = lower_q,
      UPPER = upper_q
    ))
  }
}

# ---------- Mean and SE ----------
mean_tbl <- tibble()
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  km <- km_list[[lv]]
  event_times <- sort(unique(df_s$AVAL[df_s$CNSR == 0]))
  m <- sum(df_s$CNSR == 0)
  if (length(event_times) == 0) {
    mean_val <- NA_real_; se_val <- NA_real_
  } else {
    tau <- max(event_times)
    # S at event times
    # map event_times to km rows
    S_event <- sapply(event_times, function(t) {
      idx <- which(km$TIME == t)
      if (length(idx) > 0) km$SURVIVAL[idx[1]] else NA
    })
    # mean
    e0 <- 0
    S0 <- 1
    mean_val <- 0
    prev_t <- 0
    prev_S <- 1
    # iterate over event_times in order
    for (i in seq_along(event_times)) {
      cur_t <- event_times[i]
      # area from prev_t to cur_t with prev_S
      mean_val <- mean_val + prev_S * (cur_t - prev_t)
      prev_t <- cur_t
      prev_S <- S_event[i]
    }
    # Note: mean is up to tau, which is last event time, so we have included all intervals
    # compute A_i for each event time
    k <- length(event_times)
    A <- numeric(k)
    for (i in seq_along(event_times)) {
      # A_i = sum_{j=i}^{k-1} S_event[j] * (event_times[j+1] - event_times[j])
      if (i < k) {
        s <- 0
        for (j in i:(k - 1)) {
          s <- s + S_event[j] * (event_times[j + 1] - event_times[j])
        }
        A[i] <- s
      } else {
        A[i] <- 0
      }
    }
    # need n_i and d_i at each event time
    sum_term <- 0
    for (i in seq_along(event_times)) {
      t <- event_times[i]
      n_i <- sum(df_s$AVAL >= t)
      d_i <- sum(df_s$AVAL == t & df_s$CNSR == 0)
      if (n_i > 0 && (n_i - d_i) > 0) {
        sum_term <- sum_term + (A[i]^2) * d_i / (n_i * (n_i - d_i))
      }
    }
    if (m > 1) {
      se_val <- sqrt(m / (m - 1) * sum_term)
    } else {
      se_val <- NA_real_
    }
  }
  mean_tbl <- bind_rows(mean_tbl, tibble(
    STRATUM = lv,
    MEAN = mean_val,
    STDERR = se_val,
    N_EVENTS = m
  ))
}

# ---------- Summary censored ----------
summary_tbl <- tibble()
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  total <- nrow(df_s)
  failed <- sum(df_s$CNSR == 0)
  censored <- sum(df_s$CNSR == 1)
  pct <- if (total > 0) 100 * censored / total else NA
  summary_tbl <- bind_rows(summary_tbl, tibble(
    STRATUM = lv,
    TOTAL = total,
    FAILED = failed,
    CENSORED = censored,
    PERCENT_CENSORED = pct
  ))
}
# overall
total_all <- nrow(adtte)
failed_all <- sum(adtte$CNSR == 0)
censored_all <- sum(adtte$CNSR == 1)
summary_tbl <- bind_rows(summary_tbl, tibble(
  STRATUM = "Total",
  TOTAL = total_all,
  FAILED = failed_all,
  CENSORED = censored_all,
  PERCENT_CENSORED = 100 * censored_all / total_all
))

# ---------- Rank statistics (Log-Rank, Wilcoxon) ----------
# pooled event times
pooled_event_times <- sort(unique(adtte$AVAL[adtte$CNSR == 0]))
k_strata <- length(strata_levels)
# map stratum index
strata_idx <- setNames(seq_along(strata_levels), strata_levels)

# prepare per time per stratum n_ji, d_ji
rank_data <- tibble(TIME = pooled_event_times)
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  n_ji <- sapply(pooled_event_times, function(t) sum(df_s$AVAL >= t))
  d_ji <- sapply(pooled_event_times, function(t) sum(df_s$AVAL == t & df_s$CNSR == 0))
  rank_data[[paste0("N_", lv)]] <- n_ji
  rank_data[[paste0("D_", lv)]] <- d_ji
}
rank_data$N_TOTAL <- rowSums(rank_data[, paste0("N_", strata_levels), drop = FALSE])
rank_data$D_TOTAL <- rowSums(rank_data[, paste0("D_", strata_levels), drop = FALSE])

# compute U and V for Log-Rank (w=1) and Wilcoxon (w=n)
compute_rank_stats <- function(w_type = "logrank") {
  k <- k_strata
  U <- numeric(k)
  names(U) <- strata_levels
  V <- matrix(0, nrow = k, ncol = k, dimnames = list(strata_levels, strata_levels))
  for (i in seq_len(nrow(rank_data))) {
    n_i <- rank_data$N_TOTAL[i]
    d_i <- rank_data$D_TOTAL[i]
    if (n_i <= 1 || d_i == 0) next
    w <- if (w_type == "logrank") 1 else n_i
    # for each stratum
    n_ji_vec <- sapply(strata_levels, function(lv) rank_data[[paste0("N_", lv)]][i])
    d_ji_vec <- sapply(strata_levels, function(lv) rank_data[[paste0("D_", lv)]][i])
    e_ji_vec <- n_ji_vec * d_i / n_i
    # U
    for (j in seq_along(strata_levels)) {
      U[j] <- U[j] + w * (d_ji_vec[j] - e_ji_vec[j])
    }
    # V
    # factor = (n_i - d_i)/(n_i -1) * d_i * w^2 / n_i^2 ??? need correct
    # For logrank: V_jj = sum n_ji*(n_i - n_ji)/n_i^2 * (n_i - d_i)/(n_i -1) * d_i
    # For wilcoxon: multiply by w^2 = n_i^2 => V_jj = sum n_ji*(n_i - n_ji) * (n_i - d_i)/(n_i -1) * d_i
    # Off-diagonal: V_jk = - sum n_ji*n_ki / n_i^2 * (n_i - d_i)/(n_i -1) * d_i * w^2
    factor_base <- (n_i - d_i) / (n_i - 1) * d_i
    for (j in seq_along(strata_levels)) {
      for (kk in seq_along(strata_levels)) {
        if (j == kk) {
          contrib <- n_ji_vec[j] * (n_i - n_ji_vec[j]) / (n_i * n_i) * factor_base * w * w
          V[j, kk] <- V[j, kk] + contrib
        } else {
          contrib <- - n_ji_vec[j] * n_ji_vec[kk] / (n_i * n_i) * factor_base * w * w
          V[j, kk] <- V[j, kk] + contrib
        }
      }
    }
  }
  list(U = U, V = V)
}

logrank_res <- compute_rank_stats("logrank")
wilcoxon_res <- compute_rank_stats("wilcoxon")

# Test of Equality
compute_chi <- function(U, V) {
  k <- length(U)
  if (k <= 1) return(list(chi = NA, df = 0, p = NA))
  # use first k-1
  idx <- 1:(k - 1)
  U_sub <- U[idx]
  V_sub <- V[idx, idx, drop = FALSE]
  # check invertible
  if (det(V_sub) == 0) {
    # use generalized inverse via solve with tolerance
    inv <- try(solve(V_sub), silent = TRUE)
    if (inherits(inv, "try-error")) return(list(chi = NA, df = k - 1, p = NA))
  } else {
    inv <- solve(V_sub)
  }
  chi <- as.numeric(t(U_sub) %*% inv %*% U_sub)
  df <- k - 1
  p <- 1 - pchisq(chi, df)
  list(chi = chi, df = df, p = p)
}
logrank_test <- compute_chi(logrank_res$U, logrank_res$V)
wilcoxon_test <- compute_chi(wilcoxon_res$U, wilcoxon_res$V)

# -2Log(LR) approximation: 2*sum O_j log(O_j/E_j)
compute_lr_test <- function() {
  O_j <- sapply(strata_levels, function(lv) sum(adtte$TRT01A == lv & adtte$CNSR == 0))
  E_j <- numeric(length(strata_levels))
  for (j in seq_along(strata_levels)) {
    lv <- strata_levels[j]
    e_sum <- 0
    for (i in seq_len(nrow(rank_data))) {
      n_i <- rank_data$N_TOTAL[i]
      d_i <- rank_data$D_TOTAL[i]
      n_ji <- rank_data[[paste0("N_", lv)]][i]
      if (n_i > 0) e_sum <- e_sum + n_ji * d_i / n_i
    }
    E_j[j] <- e_sum
  }
  # avoid log(0)
  valid <- O_j > 0 & E_j > 0
  chi <- 2 * sum(O_j[valid] * log(O_j[valid] / E_j[valid]))
  df <- length(strata_levels) - 1
  p <- 1 - pchisq(chi, df)
  list(chi = chi, df = df, p = p, O = O_j, E = E_j)
}
lr_test <- compute_lr_test()

# ---------- Build text rendering ----------
txt_path <- file.path(out, "P020.txt")
con <- file(txt_path, "w")

write_txt <- function(...) cat(..., "\n", file = con, sep = "")

write_txt("Figure 1: Kaplan-Meier Event-Free Survival by Treatment")
write_txt("Population: Safety Population")
write_txt("")
write_txt("The LIFETEST Procedure")
write_txt("")

for (lv in strata_levels) {
  km <- km_list[[lv]]
  write_txt(paste0("Stratum: TRT01A = ", lv))
  write_txt("Product-Limit Survival Estimates")
  write_txt(sprintf("%-10s %-10s %-10s %-12s %-13s %-12s",
                    "Time", "Survival", "Failure", "StdErr", "NumberFailed", "NumberLeft"))
  for (i in seq_len(nrow(km))) {
    star <- ifelse(km$D[i] == 0, "*", "")
    write_txt(sprintf("%-10s %-10s %-10s %-12s %-13s %-12s",
                      paste0(fmt3(km$TIME[i]), star),
                      fmt4(km$SURVIVAL[i]),
                      fmt4(km$FAILURE[i]),
                      fmt4(km$STDERR[i]),
                      as.character(km$D[i]),
                      as.character(km$N_AT_RISK[i])))
  }
  write_txt("")
}

write_txt("Quartile Estimates")
write_txt(sprintf("%-20s %-10s %-15s %-15s %-15s", "Stratum", "Percent", "PointEstimate", "Lower 95% CI", "Upper 95% CI"))
for (i in seq_len(nrow(quartile_tbl))) {
  write_txt(sprintf("%-20s %-10s %-15s %-15s %-15s",
                    quartile_tbl$STRATUM[i],
                    fmt1(quartile_tbl$PERCENT[i]),
                    fmt3(quartile_tbl$POINT_ESTIMATE[i]),
                    fmt3(quartile_tbl$LOWER[i]),
                    fmt3(quartile_tbl$UPPER[i])))
}
write_txt("")

write_txt("Mean and Standard Error (Restricted Mean up to largest event time)")
write_txt(sprintf("%-20s %-12s %-12s", "Stratum", "Mean", "StdErr"))
for (i in seq_len(nrow(mean_tbl))) {
  write_txt(sprintf("%-20s %-12s %-12s",
                    mean_tbl$STRATUM[i],
                    fmt3(mean_tbl$MEAN[i]),
                    fmt3(mean_tbl$STDERR[i])))
}
write_txt("")

write_txt("Summary of the Number of Censored and Uncensored Values")
write_txt(sprintf("%-20s %-8s %-8s %-10s %-12s", "Stratum", "Total", "Failed", "Censored", "PctCensored"))
for (i in seq_len(nrow(summary_tbl))) {
  write_txt(sprintf("%-20s %-8s %-8s %-10s %-12s",
                    summary_tbl$STRATUM[i],
                    as.character(summary_tbl$TOTAL[i]),
                    as.character(summary_tbl$FAILED[i]),
                    as.character(summary_tbl$CENSORED[i]),
                    fmt2(summary_tbl$PERCENT_CENSORED[i])))
}
write_txt("")

write_txt("Rank Statistics")
write_txt(sprintf("%-20s %-15s %-15s", "Stratum", "Log-Rank", "Wilcoxon"))
for (j in seq_along(strata_levels)) {
  lv <- strata_levels[j]
  write_txt(sprintf("%-20s %-15s %-15s",
                    lv,
                    fmt4(logrank_res$U[lv]),
                    fmt4(wilcoxon_res$U[lv])))
}
write_txt("")

write_txt("Covariance Matrix for the Log-Rank Statistics")
# header
hdr <- paste(c("Stratum", strata_levels), collapse = " | ")
write_txt(hdr)
for (r in strata_levels) {
  row_vals <- paste(fmt4(logrank_res$V[r, ]), collapse = " | ")
  write_txt(paste(r, "|", row_vals))
}
write_txt("")

write_txt("Covariance Matrix for the Wilcoxon Statistics")
write_txt(hdr)
for (r in strata_levels) {
  row_vals <- paste(fmt4(wilcoxon_res$V[r, ]), collapse = " | ")
  write_txt(paste(r, "|", row_vals))
}
write_txt("")

write_txt("Test of Equality over Strata")
write_txt(sprintf("%-15s %-12s %-4s %-12s", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
write_txt(sprintf("%-15s %-12s %-4s %-12s",
                  "Log-Rank", fmt4(logrank_test$chi), as.character(logrank_test$df),
                  ifelse(is.na(logrank_test$p), "", ifelse(logrank_test$p < 0.0001, "<.0001", fmt4(logrank_test$p)))))
write_txt(sprintf("%-15s %-12s %-4s %-12s",
                  "Wilcoxon", fmt4(wilcoxon_test$chi), as.character(wilcoxon_test$df),
                  ifelse(is.na(wilcoxon_test$p), "", ifelse(wilcoxon_test$p < 0.0001, "<.0001", fmt4(wilcoxon_test$p)))))
write_txt(sprintf("%-15s %-12s %-4s %-12s",
                  "-2Log(LR)", fmt4(lr_test$chi), as.character(lr_test$df),
                  ifelse(is.na(lr_test$p), "", ifelse(lr_test$p < 0.0001, "<.0001", fmt4(lr_test$p)))))
write_txt("")

write_txt("Note: Time in days; * indicates censored observation; Survival SE via Greenwood's formula;")
write_txt("      Quartile CI via log-log transform (Brookmeyer-Crowley); Mean is restricted mean;")
write_txt("      SE of mean uses m/(m-1) factor as in SAS PROC LIFETEST.")

close(con)
cat("Text written to", txt_path, "\n")

# ---------- CSV outputs ----------
# Product-limit combined
pl_combined <- bind_rows(lapply(strata_levels, function(lv) {
  km <- km_list[[lv]]
  km %>% mutate(STRATUM = lv, CENSORED_FLAG = ifelse(D == 0, "*", "")) %>%
    select(STRATUM, TIME, SURVIVAL, FAILURE, STDERR, N_AT_RISK, D, C, CENSORED_FLAG, LOWER_CI, UPPER_CI)
}), .id = NULL)
write.csv(pl_combined, file.path(out, "P020.csv"), row.names = FALSE, na = "")

# Quartile CSV
write.csv(quartile_tbl, file.path(out, "P020_quartile.csv"), row.names = FALSE, na = "")
# Mean CSV
write.csv(mean_tbl, file.path(out, "P020_mean.csv"), row.names = FALSE, na = "")
# Summary CSV
write.csv(summary_tbl, file.path(out, "P020_summary.csv"), row.names = FALSE, na = "")
# Rank stats CSV
rank_stats_tbl <- tibble(
  STRATUM = strata_levels,
  LOG_RANK = as.numeric(logrank_res$U[strata_levels]),
  WILCOXON = as.numeric(wilcoxon_res$U[strata_levels])
)
write.csv(rank_stats_tbl, file.path(out, "P020_rank.csv"), row.names = FALSE, na = "")
# Test equality CSV
test_tbl <- tibble(
  TEST = c("Log-Rank", "Wilcoxon", "-2Log(LR)"),
  CHISQ = c(logrank_test$chi, wilcoxon_test$chi, lr_test$chi),
  DF = c(logrank_test$df, wilcoxon_test$df, lr_test$df),
  PVALUE = c(logrank_test$p, wilcoxon_test$p, lr_test$p)
)
write.csv(test_tbl, file.path(out, "P020_test.csv"), row.names = FALSE, na = "")

# ---------- Figure ----------
# Prepare plot data: step function
plot_data <- pl_combined %>% arrange(STRATUM, TIME)
# For ggplot step, need to add time 0, survival 1
plot_data0 <- tibble()
for (lv in strata_levels) {
  plot_data0 <- bind_rows(plot_data0, tibble(STRATUM = lv, TIME = 0, SURVIVAL = 1, LOWER_CI = 1, UPPER_CI = 1))
}
plot_data_all <- bind_rows(plot_data0, plot_data %>% select(STRATUM, TIME, SURVIVAL, LOWER_CI, UPPER_CI)) %>%
  arrange(STRATUM, TIME)

# At-risk table time points
max_time <- max(adtte$AVAL, na.rm = TRUE)
breaks <- pretty(c(0, max_time), n = 6)
breaks <- breaks[breaks >= 0 & breaks <= max_time]
if (length(breaks) < 3) breaks <- c(0, max_time/2, max_time)

atrisk_tbl <- tibble()
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  for (b in breaks) {
    n_at <- sum(df_s$AVAL >= b)
    atrisk_tbl <- bind_rows(atrisk_tbl, tibble(STRATUM = lv, TIME = b, N_AT_RISK = n_at))
  }
}

p <- ggplot(plot_data_all, aes(x = TIME, y = SURVIVAL, color = STRATUM, group = STRATUM)) +
  geom_step(linewidth = 0.9) +
  geom_step(aes(y = LOWER_CI), linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
  geom_step(aes(y = UPPER_CI), linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
  labs(
    title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
    subtitle = "Population: Safety Population",
    x = "Time (days)",
    y = "Survival Probability",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 1) +
  scale_x_continuous(breaks = breaks)

# Add at-risk table as annotation via secondary plot? Use grid
# Create at-risk plot
p_atrisk <- ggplot(atrisk_tbl, aes(x = TIME, y = STRATUM, label = N_AT_RISK)) +
  geom_text(size = 3.5) +
  labs(x = "Time", y = "At Risk") +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 8)) +
  scale_x_continuous(breaks = breaks, limits = range(breaks))

# Combine using gridExtra if available, else just save main plot
# Save main KM curve
png_path <- file.path(out, "P020.png")
ggsave(png_path, plot = p, width = 7, height = 5.5, units = "in", dpi = 300)
cat("Figure written to", png_path, "\n")

# Also save KM_Curve.png for SAS imagename compatibility
ggsave(file.path(out, "KM_Curve.png"), plot = p, width = 7, height = 5.5, units = "in", dpi = 300)

# Plot data CSV
write.csv(plot_data_all, file.path(out, "P020_plot_data.csv"), row.names = FALSE, na = "")
write.csv(atrisk_tbl, file.path(out, "P020_atrisk.csv"), row.names = FALSE, na = "")

cat("All outputs written to", out, "\n")
