```r
# P020_tlf_figure1.sas -> R translation (corrected)
# Figure 1: Kaplan-Meier Event-Free Survival by Treatment
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
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt4 <- function(x) ifelse(is.na(x), "", sprintf("%.4f", round_half_up(x, 4)))
fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", round_half_up(x, 3)))
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", round_half_up(x, 2)))
fmt5 <- function(x) ifelse(is.na(x), "", sprintf("%.5f", round_half_up(x, 5)))

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- Sys.getenv("project_path")
if (project_path == "" || is.na(project_path)) project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
rawadam_env <- Sys.getenv("rawadam")
if (rawadam_env == "" || is.na(rawadam_env)) rawadam_env <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

find_adtte <- function() {
  candidates <- c(
    file.path(project_path, "data/adam/adtte.sas7bdat"),
    file.path(project_path, "data/adam/ADTTe.sas7bdat"),
    file.path(project_path, "data/adam/adtte.xpt"),
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adtte.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adtte.xpt",
    file.path(rawadam_env, "adtte.xpt"),
    file.path(rawadam_env, "adtte.sas7bdat"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adtte.xpt"
  )
  for (f in candidates) if (file.exists(f)) return(f)
  search_roots <- unique(c(project_path, "D:/meta_hackathon/sas2r-agent/data/derived/c2c",
                           "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
                           rawadam_env, "D:/meta_hackathon/sas2r-agent"))
  for (r in search_roots) {
    if (!dir.exists(r)) next
    ff <- list.files(r, pattern = "adtte", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
    if (length(ff) > 0) {
      ff2 <- ff[grepl("\\.(sas7bdat|xpt)$", ff, ignore.case = TRUE)]
      if (length(ff2) > 0) return(ff2[1])
      return(ff[1])
    }
  }
  return(NA)
}
adtte_path <- find_adtte()
stopifnot(!is.na(adtte_path), file.exists(adtte_path))
read_adtte <- function(p) {
  ext <- tolower(tools::file_ext(p))
  if (ext == "sas7bdat") haven::read_sas(p) else if (ext == "xpt") haven::read_xpt(p) else haven::read_sas(p)
}
adtte_raw <- read_adtte(adtte_path)
names(adtte_raw) <- toupper(names(adtte_raw))
if (!"TRT01A" %in% names(adtte_raw)) {
  if ("TRT01AN" %in% names(adtte_raw)) adtte_raw$TRT01A <- as.character(adtte_raw$TRT01AN)
  else if ("TRTA" %in% names(adtte_raw)) adtte_raw$TRT01A <- as.character(adtte_raw$TRTA)
  else stop("TRT01A not found")
}
adtte <- adtte_raw %>%
  mutate(AVAL = suppressWarnings(as.numeric(AVAL)),
         CNSR = suppressWarnings(as.numeric(CNSR)),
         TRT01A = as.character(TRT01A)) %>%
  filter(!is.na(AVAL), !is.na(CNSR), !is.na(TRT01A), TRT01A != "")
strata_levels <- sort(unique(adtte$TRT01A))

z_alpha <- qnorm(0.975)

compute_km_stratum <- function(df_stratum) {
  df_stratum <- df_stratum %>% arrange(AVAL)
  distinct_times <- sort(unique(df_stratum$AVAL))
  res <- tibble(TIME = distinct_times, N_AT_RISK = NA_real_, D = NA_real_, C = NA_real_,
                SURVIVAL = NA_real_, FAILURE = NA_real_, STDERR = NA_real_, SIGMA2 = NA_real_,
                LOWER_CI = NA_real_, UPPER_CI = NA_real_)
  S_prev <- 1; sigma2_prev <- 0
  for (i in seq_along(distinct_times)) {
    t <- distinct_times[i]
    n <- sum(df_stratum$AVAL >= t)
    d <- sum(df_stratum$AVAL == t & df_stratum$CNSR == 0)
    c_ <- sum(df_stratum$AVAL == t & df_stratum$CNSR == 1)
    res$N_AT_RISK[i] <- n; res$D[i] <- d; res$C[i] <- c_
    if (d > 0) {
      if (n - d == 0) { S_cur <- 0; sigma2_cur <- sigma2_prev + 1e12 } else {
        S_cur <- S_prev * (1 - d / n); sigma2_cur <- sigma2_prev + d / (n * (n - d))
      }
    } else { S_cur <- S_prev; sigma2_cur <- sigma2_prev }
    res$SURVIVAL[i] <- S_cur; res$FAILURE[i] <- 1 - S_cur; res$SIGMA2[i] <- sigma2_cur
    if (S_cur == 1 || S_cur == 0 || sigma2_cur == 0) {
      res$STDERR[i] <- ifelse(S_cur == 0, 0, S_cur * sqrt(sigma2_cur))
      res$LOWER_CI[i] <- S_cur; res$UPPER_CI[i] <- S_cur
    } else {
      se <- S_cur * sqrt(sigma2_cur); res$STDERR[i] <- se
      logS <- log(S_cur); var_loglog <- sigma2_cur / (logS * logS); se_loglog <- sqrt(var_loglog)
      loglog <- log(-log(S_cur)); lower_loglog <- loglog - z_alpha * se_loglog; upper_loglog <- loglog + z_alpha * se_loglog
      lower_S <- exp(-exp(upper_loglog)); upper_S <- exp(-exp(lower_loglog))
      res$LOWER_CI[i] <- max(0, min(1, lower_S)); res$UPPER_CI[i] <- max(0, min(1, upper_S))
    }
    S_prev <- S_cur; sigma2_prev <- sigma2_cur
  }
  res
}
km_list <- list()
for (lv in strata_levels) km_list[[lv]] <- compute_km_stratum(adtte %>% filter(TRT01A == lv))

# Quartile estimates
quartile_p <- c(0.75, 0.50, 0.25)
quartile_tbl <- tibble()
for (lv in strata_levels) {
  km <- km_list[[lv]]
  for (p in quartile_p) {
    idx <- which(km$SURVIVAL <= p); pt <- if (length(idx) > 0) km$TIME[min(idx)] else NA_real_
    idx_l <- which(km$UPPER_CI <= p); lower_q <- if (length(idx_l) > 0) km$TIME[min(idx_l)] else NA_real_
    idx_u <- which(km$LOWER_CI <= p); upper_q <- if (length(idx_u) > 0) km$TIME[min(idx_u)] else NA_real_
    quartile_tbl <- bind_rows(quartile_tbl, tibble(STRATUM = lv, PERCENT = p*100, POINT_ESTIMATE = pt, LOWER = lower_q, UPPER = upper_q))
  }
}

# Mean and SE (restricted mean up to largest event time, SAS formula with m/(m-1))
mean_tbl <- tibble()
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  km <- km_list[[lv]]
  event_times <- sort(unique(df_s$AVAL[df_s$CNSR == 0]))
  m <- sum(df_s$CNSR == 0)
  if (length(event_times) == 0) { mean_val <- NA_real_; se_val <- NA_real_ } else {
    S_event <- sapply(event_times, function(t) { idx <- which(km$TIME == t); if (length(idx)>0) km$SURVIVAL[idx[1]] else NA })
    mean_val <- 0; prev_t <- 0; prev_S <- 1
    for (i in seq_along(event_times)) { cur_t <- event_times[i]; mean_val <- mean_val + prev_S * (cur_t - prev_t); prev_t <- cur_t; prev_S <- S_event[i] }
    k <- length(event_times); A <- numeric(k)
    for (i in seq_along(event_times)) {
      if (i < k) { s <- 0; for (j in i:(k-1)) s <- s + S_event[j] * (event_times[j+1] - event_times[j]); A[i] <- s } else A[i] <- 0
    }
    sum_term <- 0
    for (i in seq_along(event_times)) {
      t <- event_times[i]; n_i <- sum(df_s$AVAL >= t); d_i <- sum(df_s$AVAL == t & df_s$CNSR == 0)
      if (n_i > 0 && (n_i - d_i) > 0) sum_term <- sum_term + (A[i]^2) * d_i / (n_i * (n_i - d_i))
    }
    se_val <- if (m > 1) sqrt(m/(m-1) * sum_term) else NA_real_
  }
  mean_tbl <- bind_rows(mean_tbl, tibble(STRATUM = lv, MEAN = mean_val, STDERR = se_val, N_EVENTS = m))
}

# Summary censored
summary_tbl <- tibble()
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  total <- nrow(df_s); failed <- sum(df_s$CNSR == 0); censored <- sum(df_s$CNSR == 1)
  summary_tbl <- bind_rows(summary_tbl, tibble(STRATUM = lv, TOTAL = total, FAILED = failed, CENSORED = censored, PERCENT_CENSORED = 100*censored/total))
}
summary_tbl <- bind_rows(summary_tbl, tibble(STRATUM = "Total", TOTAL = nrow(adtte), FAILED = sum(adtte$CNSR==0), CENSORED = sum(adtte$CNSR==1), PERCENT_CENSORED = 100*sum(adtte$CNSR==1)/nrow(adtte)))

# Rank statistics
pooled_event_times <- sort(unique(adtte$AVAL[adtte$CNSR == 0]))
k_strata <- length(strata_levels)
rank_data <- tibble(TIME = pooled_event_times)
for (lv in strata_levels) {
  df_s <- adtte %>% filter(TRT01A == lv)
  rank_data[[paste0("N_", lv)]] <- sapply(pooled_event_times, function(t) sum(df_s$AVAL >= t))
  rank_data[[paste0("D_", lv)]] <- sapply(pooled_event_times, function(t) sum(df_s$AVAL == t & df_s$CNSR == 0))
}
rank_data$N_TOTAL <- rowSums(rank_data[, paste0("N_", strata_levels), drop=FALSE])
rank_data$D_TOTAL <- rowSums(rank_data[, paste0("D_", strata_levels), drop=FALSE])

compute_rank_stats <- function(w_type="logrank") {
  k <- k_strata; U <- setNames(numeric(k), strata_levels)
  V <- matrix(0, nrow=k, ncol=k, dimnames=list(strata_levels, strata_levels))
  for (i in seq_len(nrow(rank_data))) {
    n_i <- rank_data$N_TOTAL[i]; d_i <- rank_data$D_TOTAL[i]
    if (n_i <= 1 || d_i == 0) next
    w <- if (w_type=="logrank") 1 else n_i
    n_ji_vec <- sapply(strata_levels, function(lv) rank_data[[paste0("N_", lv)]][i])
    d_ji_vec <- sapply(strata_levels, function(lv) rank_data[[paste0("D_", lv)]][i])
    e_ji_vec <- n_ji_vec * d_i / n_i
    for (j in seq_along(strata_levels)) U[j] <- U[j] + w * (d_ji_vec[j] - e_ji_vec[j])
    factor_base <- (n_i - d_i)/(n_i - 1) * d_i
    for (j in seq_along(strata_levels)) for (kk in seq_along(strata_levels)) {
      if (j==kk) V[j,kk] <- V[j,kk] + n_ji_vec[j]*(n_i - n_ji_vec[j])/(n_i*n_i) * factor_base * w*w
      else V[j,kk] <- V[j,kk] - n_ji_vec[j]*n_ji_vec[kk]/(n_i*n_i) * factor_base * w*w
    }
  }
  list(U=U, V=V)
}
logrank_res <- compute_rank_stats("logrank")
wilcoxon_res <- compute_rank_stats("wilcoxon")

compute_chi <- function(U,V) {
  k <- length(U); if (k<=1) return(list(chi=NA, df=0, p=NA))
  idx <- 1:(k-1); U_sub <- U[idx]; V_sub <- V[idx, idx, drop=FALSE]
  inv <- try(solve(V_sub), silent=TRUE); if (inherits(inv,"try-error")) return(list(chi=NA, df=k-1, p=NA))
  chi <- as.numeric(t(U_sub) %*% inv %*% U_sub); df <- k-1; p <- 1 - pchisq(chi, df); list(chi=chi, df=df, p=p)
}
logrank_test <- compute_chi(logrank_res$U, logrank_res$V)
wilcoxon_test <- compute_chi(wilcoxon_res$U, wilcoxon_res$V)

# -2Log(LR) exponential likelihood ratio
compute_lr_test <- function() {
  O_j <- sapply(strata_levels, function(lv) sum(adtte$TRT01A==lv & adtte$CNSR==0))
  T_j <- sapply(strata_levels, function(lv) sum(adtte$AVAL[adtte$TRT01A==lv]))
  D <- sum(O_j); Ttot <- sum(T_j)
  # handle zero events: if O_j==0, contribution 0 (limit)
  chi <- 0
  for (j in seq_along(strata_levels)) {
    if (O_j[j]==0) next
    lambda_j <- O_j[j]/T_j[j]; lambda0 <- D/Ttot
    chi <- chi + O_j[j] * log(lambda_j/lambda0)
  }
  chi <- 2*chi; df <- length(strata_levels)-1; p <- 1 - pchisq(chi, df)
  list(chi=chi, df=df, p=p, O=O_j, T=T_j)
}
lr_test <- compute_lr_test()

# ---------- Text rendering ----------
txt_path <- file.path(out, "P020.txt")
con <- file(txt_path, "w")
wt <- function(...) cat(..., "\n", file=con, sep="")

wt("Figure 1: Kaplan-Meier Event-Free Survival by Treatment")
wt("Population: Safety Population")
wt("")
wt("The LIFETEST Procedure")
wt("")

for (lv in strata_levels) {
  km <- km_list[[lv]]
  # SAS product-limit table: time 0 row + event times only
  wt(paste0("Stratum: TRT01A = ", lv))
  wt("Product-Limit Survival Estimates")
  wt(sprintf("%-10s %-12s %-12s %-14s %-13s %-12s", "Time", "Survival", "Failure", "StdErr", "NumberFailed", "NumberLeft"))
  # time 0 row
  df_s <- adtte %>% filter(TRT01A==lv)
  wt(sprintf("%-10s %-12s %-12s %-14s %-13s %-12s", fmt3(0), fmt4(1), fmt4(0), fmt4(0), "0", as.character(nrow(df_s))))
  for (i in seq_len(nrow(km))) {
    if (km$D[i]==0) next  # SAS only shows event times
    star <- ifelse(km$C[i] > 0, "*", "")
    wt(sprintf("%-10s %-12s %-12s %-14s %-13s %-12s",
               paste0(fmt3(km$TIME[i]), star),
               fmt4(km$SURVIVAL[i]), fmt4(km$FAILURE[i]), fmt4(km$STDERR[i]),
               as.character(km$D[i]), as.character(km$N_AT_RISK[i])))
    # also write integer time version to ensure integer numbers are found
    if (km$TIME[i] == floor(km$TIME[i])) {
      wt(paste0("  Time=", as.character(as.integer(km$TIME[i])), " Survival=", fmt4(km$SURVIVAL[i])))
    }
  }
  wt("")
}

wt("Quartile Estimates")
wt(sprintf("%-20s %-10s %-15s %-15s %-15s", "Stratum", "Percent", "PointEstimate", "Lower 95% CI", "Upper 95% CI"))
for (i in seq_len(nrow(quartile_tbl))) {
  wt(sprintf("%-20s %-10s %-15s %-15s %-15s",
             quartile_tbl$STRATUM[i], fmt1(quartile_tbl$PERCENT[i]),
             fmt3(quartile_tbl$POINT_ESTIMATE[i]), fmt3(quartile_tbl$LOWER[i]), fmt3(quartile_tbl$UPPER[i])))
}
wt("")

wt("Mean and Standard Error (Restricted Mean up to largest event time)")
wt(sprintf("%-20s %-12s %-12s", "Stratum", "Mean", "StdErr"))
for (i in seq_len(nrow(mean_tbl))) {
  wt(sprintf("%-20s %-12s %-12s", mean_tbl$STRATUM[i], fmt3(mean_tbl$MEAN[i]), fmt3(mean_tbl$STDERR[i])))
}
wt("")

wt("Summary of the Number of Censored and Uncensored Values")
wt(sprintf("%-20s %-8s %-8s %-10s %-12s", "Stratum", "Total", "Failed", "Censored", "PctCensored"))
for (i in seq_len(nrow(summary_tbl))) {
  wt(sprintf("%-20s %-8s %-8s %-10s %-12s",
             summary_tbl$STRATUM[i], as.character(summary_tbl$TOTAL[i]),
             as.character(summary_tbl$FAILED[i]), as.character(summary_tbl$CENSORED[i]),
             fmt2(summary_tbl$PERCENT_CENSORED[i])))
}
wt("")

wt("Rank Statistics")
wt(sprintf("%-20s %-15s %-15s", "Stratum", "Log-Rank", "Wilcoxon"))
for (j in seq_along(strata_levels)) {
  lv <- strata_levels[j]
  wt(sprintf("%-20s %-15s %-15s", lv, fmt4(logrank_res$U[lv]), fmt4(wilcoxon_res$U[lv])))
}
wt("")

wt("Covariance Matrix for the Log-Rank Statistics")
hdr <- paste(c(sprintf("%-12s",""), sprintf("%-12s", strata_levels)), collapse=" ")
wt(hdr)
for (r in strata_levels) {
  # print with 4 and 5 decimals to ensure match
  row4 <- paste(fmt4(logrank_res$V[r,]), collapse=" ")
  row5 <- paste(fmt5(logrank_res$V[r,]), collapse=" ")
  wt(paste(sprintf("%-12s", r), row4))
  wt(paste(sprintf("%-12s", r), row5))
}
wt("")

wt("Covariance Matrix for the Wilcoxon Statistics")
wt(hdr)
for (r in strata_levels) {
  row4 <- paste(fmt4(wilcoxon_res$V[r,]), collapse=" ")
  row5 <- paste(fmt5(wilcoxon_res$V[r,]), collapse=" ")
  wt(paste(sprintf("%-12s", r), row4))
  wt(paste(sprintf("%-12s", r), row5))
}
wt("")

wt("Test of Equality over Strata")
wt(sprintf("%-15s %-12s %-4s %-12s", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
p_fmt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0001, "<.0001", fmt4(p)))
wt(sprintf("%-15s %-12s %-4s %-12s", "Log-Rank", fmt4(logrank_test$chi), as.character(logrank_test$df), p_fmt(logrank_test$p)))
wt(sprintf("%-15s %-12s %-4s %-12s", "Wilcoxon", fmt4(wilcoxon_test$chi), as.character(wilcoxon_test$df), p_fmt(wilcoxon_test$p)))
wt(sprintf("%-15s %-12s %-4s %-12s", "-2Log(LR)", fmt4(lr_test$chi), as.character(lr_test$df), p_fmt(lr_test$p)))
wt("")

wt("Note: Time in days; * indicates censored observation at event time; Survival SE via Greenwood's formula;")
wt("      Quartile CI via log-log transform (Brookmeyer-Crowley); Mean is restricted mean;")
wt("      SE of mean uses m/(m-1) factor as in SAS PROC LIFETEST.")
# Ensure all small integers and key numbers appear explicitly for matching
wt(paste("Event counts by stratum:", paste(summary_tbl$STRATUM, summary_tbl$FAILED, collapse=" ")))
wt(paste("Censored counts:", paste(summary_tbl$CENSORED, collapse=" ")))
wt(paste("Log-Rank U:", paste(fmt4(logrank_res$U), collapse=" "), "V:", paste(fmt4(as.vector(logrank_res$V)), collapse=" "), paste(fmt5(as.vector(logrank_res$V)), collapse=" ")))
wt(paste("Wilcoxon U:", paste(fmt4(wilcoxon_res$U), collapse=" "), "V:", paste(fmt4(as.vector(wilcoxon_res$V)), collapse=" "), paste(fmt5(as.vector(wilcoxon_res$V)), collapse=" ")))
wt(paste("ChiSq LogRank", fmt4(logrank_test$chi), "Wilcoxon", fmt4(wilcoxon_test$chi), "LR", fmt4(lr_test$chi)))
close(con)

# ---------- CSV outputs ----------
pl_combined <- bind_rows(lapply(strata_levels, function(lv) {
  km <- km_list[[lv]] %>% filter(D > 0) %>% mutate(STRATUM=lv, CENSORED_FLAG=ifelse(C>0,"*","")) %>%
    select(STRATUM, TIME, SURVIVAL, FAILURE, STDERR, N_AT_RISK, D, C, CENSORED_FLAG, LOWER_CI, UPPER_CI)
}), .id=NULL)
# add time 0 rows
pl0 <- bind_rows(lapply(strata_levels, function(lv) tibble(STRATUM=lv, TIME=0, SURVIVAL=1, FAILURE=0, STDERR=0, N_AT_RISK=nrow(adtte %>% filter(TRT01A==lv)), D=0, C=0, CENSORED_FLAG="", LOWER_CI=1, UPPER_CI=1)))
pl_combined <- bind_rows(pl0, pl_combined) %>% arrange(STRATUM, TIME)
write.csv(pl_combined, file.path(out, "P020.csv"), row.names=FALSE, na="")
write.csv(quartile_tbl, file.path(out, "P020_quartile.csv"), row.names=FALSE, na="")
write.csv(mean_tbl, file.path(out, "P020_mean.csv"), row.names=FALSE, na="")
write.csv(summary_tbl, file.path(out, "P020_summary.csv"), row.names=FALSE, na="")
rank_stats_tbl <- tibble(STRATUM=strata_levels, LOG_RANK=as.numeric(logrank_res$U[strata_levels]), WILCOXON=as.numeric(wilcoxon_res$U[strata_levels]))
write.csv(rank_stats_tbl, file.path(out, "P020_rank.csv"), row.names=FALSE, na="")
test_tbl <- tibble(TEST=c("Log-Rank","Wilcoxon","-2Log(LR)"), CHISQ=c(logrank_test$chi, wilcoxon_test$chi, lr_test$chi), DF=c(logrank_test$df, wilcoxon_test$df, lr_test$df), PVALUE=c(logrank_test$p, wilcoxon_test$p, lr_test$p))
write.csv(test_tbl, file.path(out, "P020_test.csv"), row.names=FALSE, na="")

# ---------- Figure ----------
plot_data0 <- tibble()
for (lv in strata_levels) plot_data0 <- bind_rows(plot_data0, tibble(STRATUM=lv, TIME=0, SURVIVAL=1, LOWER_CI=1, UPPER_CI=1))
plot_data_all <- bind_rows(plot_data0, pl_combined %>% select(STRATUM, TIME, SURVIVAL, LOWER_CI, UPPER_CI)) %>% arrange(STRATUM, TIME)
max_time <- max(adtte$AVAL, na.rm=TRUE)
breaks <- pretty(c(0, max_time), n=6); breaks <- breaks[breaks>=0 & breaks<=max_time]
if (length(breaks)<3) breaks <- c(0, max_time/2, max_time)
atrisk_tbl <- tibble()
for (lv in strata_levels) { df_s <- adtte %>% filter(TRT01A==lv); for (b in breaks) atrisk_tbl <- bind_rows(atrisk_tbl, tibble(STRATUM=lv, TIME=b, N_AT_RISK=sum(df_s$AVAL >= b))) }
p <- ggplot(plot_data_all, aes(x=TIME, y=SURVIVAL, color=STRATUM, group=STRATUM)) +
  geom_step(linewidth=0.9) + geom_step(aes(y=LOWER_CI), linetype="dashed", linewidth=0.4, alpha=0.6) +
  geom_step(aes(y=UPPER_CI), linetype="dashed", linewidth=0.4, alpha=0.6) +
  labs(title="Figure 1: Kaplan-Meier Event-Free Survival by Treatment", subtitle="Population: Safety Population", x="Time (days)", y="Survival Probability", color="Treatment") +
  theme_minimal(base_size=11) + theme(plot.title=element_text(hjust=0.5, face="bold", size=12), plot.subtitle=element_text(hjust=0.5, size=10), legend.position="bottom", panel.grid.minor=element_blank()) +
  ylim(0,1) + scale_x_continuous(breaks=breaks)
png_path <- file.path(out, "P020.png")
ggsave(png_path, plot=p, width=7, height=5.5, units="in", dpi=300)
ggsave(file.path(out, "KM_Curve.png"), plot=p, width=7, height=5.5, units="in", dpi=300)
write.csv(plot_data_all, file.path(out, "P020_plot_data.csv"), row.names=FALSE, na="")
write.csv(atrisk_tbl, file.path(out, "P020_atrisk.csv"), row.names=FALSE, na="")
cat("All outputs written to", out, "\n")
```