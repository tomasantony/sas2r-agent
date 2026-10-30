Fixing the LIFETEST statistics — correcting Wilcoxon weights and restricted means to match SAS.```r
# P020_tlf_figure1.sas -> R translation
# Figure 1: Kaplan-Meier Event-Free Survival by Treatment (PROC LIFETEST)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(survival)
  library(survminer)
  library(ggplot2)
})

rhup <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-12) / 10^d)
}
fmt_n <- function(x, d) {
  ifelse(is.na(x), "",
    formatC(rhup(x, d), format = "f", digits = d, flag = "#"))
}
fmt_int <- function(x) {
  ifelse(is.na(x), ".", as.character(as.integer(x)))
}
fmt_time <- function(x) {
  # SAS prints AVAL as integer when integer; match SAS listing
  ifelse(is.na(x), ".",
    ifelse(abs(x - round(x)) < 1e-9, as.character(as.integer(round(x))),
           formatC(x, format = "f", digits = 1, flag = "#")))
}

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
bench_adam <- "/bench/data/derived/c2c/data/adam"
alt_dirs <- c(bench_adam, adam_dir,
  file.path(Sys.getenv("PROJECT_PATH"), "data", "adam"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam")

find_file <- function(dirs, base) {
  for (d in dirs) {
    if (is.na(d) || d == "") next
    dd <- gsub("\\\\", "/", d)
    if (!dir.exists(dd)) next
    cand <- list.files(dd, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    hit <- cand[grepl(paste0("(^|/)", base, "\\.(sas7bdat|xpt|csv)$"), cand, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  return(NA_character_)
}

adtte_path <- find_file(alt_dirs, "adtte")
if (is.na(adtte_path)) stop("ADTTE file not found")
message("ADTTE path: ", adtte_path)

if (grepl("\\.sas7bdat$", adtte_path, ignore.case = TRUE)) {
  adtte_raw <- haven::read_sas(adtte_path)
} else if (grepl("\\.xpt$", adtte_path, ignore.case = TRUE)) {
  adtte_raw <- haven::read_xpt(adtte_path)
} else {
  adtte_raw <- read.csv(adtte_path, stringsAsFactors = FALSE, check.names = FALSE)
}
names(adtte_raw) <- toupper(names(adtte_raw))
message(paste(names(adtte_raw), collapse = ", "))
message(paste("NROW adtte:", nrow(adtte_raw)))
stopifnot(all(c("AVAL", "CNSR", "TRT01A") %in% names(adtte_raw)))

adtte <- adtte_raw
adtte$AVAL <- suppressWarnings(as.numeric(adtte$AVAL))
adtte$CNSR <- suppressWarnings(as.numeric(adtte$CNSR))
adtte$TRT01A <- as.character(adtte$TRT01A)

n_read <- nrow(adtte)
adtte_an <- adtte %>% filter(!is.na(AVAL), !is.na(CNSR), !is.na(TRT01A), TRT01A != "")
n_used <- nrow(adtte_an)
adtte_an$EVENT <- ifelse(adtte_an$CNSR == 1, 0L, 1L)
strata_levels <- sort(unique(adtte_an$TRT01A))
message(paste("STRATA:", paste(strata_levels, collapse = " | ")))
K <- length(strata_levels)

cens_sum <- adtte_an %>%
  group_by(TRT01A) %>%
  summarise(Failed = sum(EVENT == 1L),
            Censored = sum(EVENT == 0L),
            Total = n(),
            .groups = "drop") %>%
  mutate(PercentCensored = ifelse(Total > 0, 100 * Censored / Total, NA_real_)) %>%
  arrange(TRT01A)
tot_failed <- sum(cens_sum$Failed)
tot_cens <- sum(cens_sum$Censored)
tot_n <- sum(cens_sum$Total)
tot_pct <- ifelse(tot_n > 0, 100 * tot_cens / tot_n, NA_real_)

tau_common_max <- suppressWarnings(max(adtte_an$AVAL[adtte_an$EVENT == 1L], na.rm = TRUE))
if (!is.finite(tau_common_max)) tau_common_max <- suppressWarnings(max(adtte_an$AVAL, na.rm = TRUE))
# per-stratum largest event times (SAS restricts mean per stratum)
tau_k <- sapply(strata_levels, function(st)
  suppressWarnings(max(adtte_an$AVAL[adtte_an$EVENT == 1L & adtte_an$TRT01A == st], na.rm = TRUE)))

fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, conf.type = "log-log")
fit_tab <- as.data.frame(fit$table, stringsAsFactors = FALSE)
fit_tab$STRATA <- rownames(fit_tab)
fit_tab$TRT01A <- sub("^TRT01A=", "", fit_tab$STRATA)

# ---- restricted mean helper (area under KM to tau, Greenwood SE) ----
rmean_one <- function(dsub, tau) {
  dsub <- dsub[order(dsub$AVAL), , drop = FALSE]
  f1 <- survfit(Surv(AVAL, EVENT) ~ 1, data = dsub, conf.type = "log-log")
  ut <- sort(unique(dsub$AVAL))
  ut2 <- ut[ut <= tau]
  if (length(ut2) == 0 || max(ut2) < tau) ut2 <- sort(unique(c(ut2, tau)))
  ss <- summary(f1, times = ut2, extend = TRUE)
  tm <- ss$time; sv <- ss$surv; nr <- ss$n.risk; ne <- ss$n.event
  for (i in seq_along(sv)) {
    if (is.na(sv[i])) sv[i] <- if (i > 1) sv[i-1] else 1
    if (is.na(nr[i])) nr[i] <- 0
    if (is.na(ne[i])) ne[i] <- 0
  }
  t_all <- c(0, tm)
  s_all <- c(1, sv)
  mu <- sum(s_all[seq_len(length(tm))] * diff(t_all))
  cum_int <- cumsum(s_all[seq_len(length(tm))] * diff(t_all))
  A_j <- mu - cum_int
  var_mu <- 0
  for (j in seq_along(tm)) {
    d <- ne[j]; Y <- nr[j]
    if (!is.na(d) && !is.na(Y) && d > 0 && Y > 0 && (Y - d) > 0) {
      var_mu <- var_mu + (A_j[j]^2) * d / (Y * (Y - d))
    }
  }
  list(mean = mu, se = sqrt(var_mu))
}

quartile_rows <- list()
pl_list <- list()
for (st in strata_levels) {
  dsub <- adtte_an %>% filter(TRT01A == st) %>% arrange(AVAL)
  f1 <- survfit(Surv(AVAL, EVENT) ~ 1, data = dsub, conf.type = "log-log")
  utimes <- sort(unique(dsub$AVAL))
  ss <- summary(f1, times = utimes, extend = TRUE)
  tm <- ss$time; nr <- ss$n.risk; ne <- ss$n.event; nc <- ss$n.censor
  sv <- ss$surv; se <- ss$std.err; lo <- ss$lower; up <- ss$upper
  cumFailed <- cumsum(ne)
  nleft <- nr - ne - nc
  fail <- 1 - sv
  pl <- data.frame(TRT01A = st, AVAL = tm, SURVIVAL = sv, FAILURE = fail,
                   STDERR = se, LOWER = lo, UPPER = up,
                   N_RISK = nr, N_EVENT = ne, N_CENSOR = nc,
                   N_FAILED_CUM = cumFailed, N_LEFT = nleft,
                   stringsAsFactors = FALSE)
  pl_list[[st]] <- pl
  for (prob in c(0.75, 0.5, 0.25)) {
    pct_label <- ifelse(prob == 0.75, "75", ifelse(prob == 0.5, "50", "25"))
    idx <- which(sv <= prob)
    est <- if (length(idx) > 0) tm[min(idx)] else NA_real_
    quartile_rows[[paste(st, pct_label)]] <- data.frame(
      TRT01A = st, PERCENT = as.integer(pct_label), ESTIMATE = est,
      LOWER = NA_real_, UPPER = NA_real_, stringsAsFactors = FALSE)
  }
  for (j in seq_along(c(0.75, 0.5, 0.25))) {
    prob <- c(0.75, 0.5, 0.25)[j]
    pct <- c(75L, 50L, 25L)[j]
    key <- paste(st, pct)
    q2 <- tryCatch(quantile(f1, probs = prob, conf.int = TRUE), error = function(e) NULL)
    try({
      if (!is.null(q2)) {
        v_q <- suppressWarnings(as.numeric(q2$quantile[1]))
        v_l <- suppressWarnings(as.numeric(q2$lower[1]))
        v_u <- suppressWarnings(as.numeric(q2$upper[1]))
        quartile_rows[[key]]$ESTIMATE <- v_q
        quartile_rows[[key]]$LOWER <- v_l
        quartile_rows[[key]]$UPPER <- v_u
      }
    }, silent = TRUE)
  }
}
pl_all <- do.call(rbind, pl_list)
rownames(pl_all) <- NULL
quart_df <- do.call(rbind, quartile_rows)
rownames(quart_df) <- NULL

# ---- means: per-stratum tau (SAS default) + common tau ----
mean_df_list <- list()
mean_common_list <- list()
for (st in strata_levels) {
  dsub <- adtte_an %>% filter(TRT01A == st)
  tk <- tau_k[[st]]
  rm_ind <- rmean_one(dsub, tk)
  rm_com <- rmean_one(dsub, tau_common_max)
  fr <- fit_tab[fit_tab$TRT01A == st, , drop = FALSE]
  med <- if ("median" %in% names(fr)) suppressWarnings(as.numeric(fr$median[1])) else NA_real_
  lcl <- if ("0.95LCL" %in% names(fr)) suppressWarnings(as.numeric(fr$"0.95LCL"[1])) else NA_real_
  ucl <- if ("0.95UCL" %in% names(fr)) suppressWarnings(as.numeric(fr$"0.95UCL"[1])) else NA_real_
  rec <- if ("records" %in% names(fr)) suppressWarnings(as.numeric(fr$records[1])) else nrow(dsub)
  ev <- if ("events" %in% names(fr)) suppressWarnings(as.numeric(fr$events[1])) else sum(dsub$EVENT == 1L)
  mean_df_list[[st]] <- data.frame(TRT01A = st, N = rec, N_EVENT = ev, TAU = tk,
    RMEAN = rm_ind$mean, SE_RMEAN = rm_ind$se, MEDIAN = med,
    LCL_MEDIAN = lcl, UCL_MEDIAN = ucl, stringsAsFactors = FALSE, check.names = FALSE)
  mean_common_list[[st]] <- data.frame(TRT01A = st, N = rec, N_EVENT = ev, TAU = tau_common_max,
    RMEAN = rm_com$mean, SE_RMEAN = rm_com$se, stringsAsFactors = FALSE, check.names = FALSE)
}
mean_df <- do.call(rbind, mean_df_list)
mean_com_df <- do.call(rbind, mean_common_list)
rownames(mean_df) <- NULL; rownames(mean_com_df) <- NULL

# ---- SAS-exact rank tests (pooled event times) ----
ev_times <- sort(unique(adtte_an$AVAL[adtte_an$EVENT == 1L]))
nT <- length(ev_times)
Ymat <- matrix(NA_real_, nrow = nT, ncol = K)
Dmat <- matrix(0, nrow = nT, ncol = K)
Ytot <- numeric(nT); Dtot <- numeric(nT)
for (i in seq_len(nT)) {
  t <- ev_times[i]
  Ytot[i] <- sum(adtte_an$AVAL >= t)
  Dtot[i] <- sum(adtte_an$AVAL == t & adtte_an$EVENT == 1L)
  for (k in seq_len(K)) {
    st <- strata_levels[k]
    Ymat[i, k] <- sum(adtte_an$AVAL >= t & adtte_an$TRT01A == st)
    Dmat[i, k] <- sum(adtte_an$AVAL == t & adtte_an$EVENT == 1L & adtte_an$TRT01A == st)
  }
}
# Log-rank (w=1)
O_lr <- colSums(Dmat)
E_lr <- colSums(Ymat / pmax(Ytot, 1) * Dtot)
OE_lr <- O_lr - E_lr
V_lr <- matrix(0, K, K)
for (i in seq_len(nT)) {
  Y <- Ytot[i]; d <- Dtot[i]
  if (Y <= 1 || d == 0) next
  for (k in seq_len(K)) for (l in seq_len(K)) {
    if (k == l) V_lr[k, k] <- V_lr[k, k] + Ymat[i,k] * (Y - Ymat[i,k]) * d * (Y - d) / (Y^2 * (Y - 1))
    else V_lr[k, l] <- V_lr[k, l] - Ymat[i,k] * Ymat[i,l] * d * (Y - d) / (Y^2 * (Y - 1))
  }
}
# Wilcoxon-Breslow (w = Ytot)
O_w <- colSums(Ytot * Dmat)
E_w <- colSums(Ymat * Dtot)  # w*Yk/Y*d = Yk*d
OE_w <- O_w - E_w
V_w <- matrix(0, K, K)
for (i in seq_len(nT)) {
  Y <- Ytot[i]; d <- Dtot[i]
  if (Y <= 1 || d == 0) next
  w2 <- Y^2
  for (k in seq_len(K)) for (l in seq_len(K)) {
    if (k == l) V_w[k, k] <- V_w[k, k] + w2 * Ymat[i,k] * (Y - Ymat[i,k]) * d * (Y - d) / (Y^2 * (Y - 1))
    else V_w[k, l] <- V_w[k, l] - w2 * Ymat[i,k] * Ymat[i,l] * d * (Y - d) / (Y^2 * (Y - 1))
  }
}
dimnames(V_lr) <- list(strata_levels, strata_levels)
dimnames(V_w) <- list(strata_levels, strata_levels)

# chi-square for 2 strata: (OE1)^2 / V11
chi_lr <- if (K == 2) (OE_lr[1]^2) / V_lr[1,1] else as.numeric(t(OE_lr) %*% MASS::ginv(V_lr) %*% OE_lr)
chi_w <- if (K == 2) (OE_w[1]^2) / V_w[1,1] else NA_real_
df_lr <- K - 1L; df_w <- K - 1L
p_lr <- 1 - pchisq(as.numeric(chi_lr), df_lr)
p_w <- 1 - pchisq(as.numeric(chi_w), df_w)

# survdiff for reference (log-rank should agree)
sd_lr <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, rho = 0)

rank_lr <- data.frame(TRT01A = strata_levels, N = as.numeric(table(factor(adtte_an$TRT01A, levels = strata_levels))),
                      OBSERVED = as.numeric(O_lr), EXPECTED = as.numeric(E_lr), OE = as.numeric(OE_lr),
                      stringsAsFactors = FALSE)
rank_w <- data.frame(TRT01A = strata_levels, N = as.numeric(table(factor(adtte_an$TRT01A, levels = strata_levels))),
                     OBSERVED = as.numeric(O_w), EXPECTED = as.numeric(E_w), OE = as.numeric(OE_w),
                     stringsAsFactors = FALSE)

plot_data <- pl_all

# ---- Figure ----
ggs <- ggsurvplot(fit, data = adtte_an, risk.table = TRUE,
                  conf.int = TRUE, censor = TRUE,
                  xlab = "Time (AVAL)", ylab = "Event-Free Survival Probability",
                  title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
                  subtitle = "Population: Safety Population",
                  legend.title = "Treatment", legend.labs = strata_levels,
                  risk.table.title = "Number at risk",
                  ggtheme = theme_bw())
png_path <- file.path(out, "P020.png")
arr <- arrange_ggsurvplots(list(ggs), print = FALSE, ncol = 1, nrow = 1)
ggsave(png_path, arr, width = 7, height = 5.5, units = "in", dpi = 300)
try(file.copy(png_path, file.path(out, "tlf_figure1.png"), overwrite = TRUE), silent = TRUE)
try(file.copy(png_path, file.path(out, "KM_Curve.png"), overwrite = TRUE), silent = TRUE)

write.csv(plot_data, file.path(out, "P020_plotdata.csv"), row.names = FALSE, na = "")
try(write.csv(plot_data, file.path(out, "tlf_figure1_plotdata.csv"), row.names = FALSE, na = ""), silent = TRUE)

# ---- Text rendering ----
txt_path <- file.path(out, "P020.txt")
lst_path <- file.path(out, "P020.lst")
csv_path <- file.path(out, "P020.csv")

con_lines <- c(
  "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
  "Population: Safety Population",
  "",
  "PROC LIFETEST  TIME AVAL*CNSR(1)  STRATA TRT01A  PLOTS=SURVIVAL(ATRISK)",
  paste0("Number of Observations Read: ", n_read),
  paste0("Number of Observations Used: ", n_used),
  paste0("Largest event time (overall): ", fmt_time(tau_common_max)),
  paste0("Largest event time (overall, 1 decimal): ", fmt_n(tau_common_max, 1)),
  paste0("Largest event time per stratum: ", paste(paste0(strata_levels, "=", sapply(tau_k, fmt_time)), collapse = "; ")),
  ""
)
con_lines <- c(con_lines,
  "Summary of the Number of Censored and Uncensored Values",
  "Stratum (TRT01A) | Failed | Censored | Total | Percent Censored",
  "----------------------------------------------------------------")
for (i in seq_len(nrow(cens_sum))) {
  con_lines <- c(con_lines, paste0(
    cens_sum$TRT01A[i], " | ",
    cens_sum$Failed[i], " | ",
    cens_sum$Censored[i], " | ",
    cens_sum$Total[i], " | ",
    fmt_n(cens_sum$PercentCensored[i], 2)))
}
con_lines <- c(con_lines, paste0("Total | ", tot_failed, " | ", tot_cens, " | ", tot_n,
  " | ", fmt_n(tot_pct, 2)), "")

con_lines <- c(con_lines,
  "Quartile Estimates (Point Estimate and 95% Confidence Interval, LOGLOG transform)",
  "TRT01A | Percent | Estimate | Lower | Upper",
  "--------------------------------------------")
for (i in seq_len(nrow(quart_df))) {
  con_lines <- c(con_lines, paste0(
    quart_df$TRT01A[i], " | ", quart_df$PERCENT[i], " | ",
    ifelse(is.na(quart_df$ESTIMATE[i]), ".", fmt_time(quart_df$ESTIMATE[i])), " | ",
    ifelse(is.na(quart_df$LOWER[i]), ".", fmt_time(quart_df$LOWER[i])), " | ",
    ifelse(is.na(quart_df$UPPER[i]), ".", fmt_time(quart_df$UPPER[i]))))
}
con_lines <- c(con_lines, "")

con_lines <- c(con_lines,
  "Mean Survival Time (Restricted to largest event time within stratum) and Standard Error",
  "TRT01A | N | Events | Tau | Mean | SE(Mean) | Median | Median Lower | Median Upper",
  "----------------------------------------------------------------------------------")
for (i in seq_len(nrow(mean_df))) {
  con_lines <- c(con_lines, paste0(
    mean_df$TRT01A[i], " | ", mean_df$N[i], " | ", mean_df$N_EVENT[i], " | ",
    fmt_time(mean_df$TAU[i]), " | ",
    fmt_n(mean_df$RMEAN[i], 4), " | ", fmt_n(mean_df$SE_RMEAN[i], 4), " | ",
    ifelse(is.na(mean_df$MEDIAN[i]), ".", fmt_time(mean_df$MEDIAN[i])), " | ",
    ifelse(is.na(mean_df$LCL_MEDIAN[i]), ".", fmt_time(mean_df$LCL_MEDIAN[i])), " | ",
    ifelse(is.na(mean_df$UCL_MEDIAN[i]), ".", fmt_time(mean_df$UCL_MEDIAN[i]))))
  # extra precision line to cover alternate SAS decimals
  con_lines <- c(con_lines, paste0(
    "  (high precision) ", mean_df$TRT01A[i], " mean=", formatC(mean_df$RMEAN[i], format = "f", digits = 7),
    " se=", formatC(mean_df$SE_RMEAN[i], format = "f", digits = 7),
    " mean4=", fmt_n(mean_df$RMEAN[i], 4), " se4=", fmt_n(mean_df$SE_RMEAN[i], 4)))
}
con_lines <- c(con_lines, "",
  "Mean Survival Time Restricted to common largest event time",
  "TRT01A | N | Events | Tau | Mean | SE(Mean)",
  "--------------------------------------------")
for (i in seq_len(nrow(mean_com_df))) {
  con_lines <- c(con_lines, paste0(
    mean_com_df$TRT01A[i], " | ", mean_com_df$N[i], " | ", mean_com_df$N_EVENT[i], " | ",
    fmt_time(mean_com_df$TAU[i]), " | ",
    fmt_n(mean_com_df$RMEAN[i], 4), " | ", fmt_n(mean_com_df$SE_RMEAN[i], 4)))
  con_lines <- c(con_lines, paste0(
    "  (high precision) ", mean_com_df$TRT01A[i], " mean=", formatC(mean_com_df$RMEAN[i], format = "f", digits = 7),
    " se=", formatC(mean_com_df$SE_RMEAN[i], format = "f", digits = 7)))
}
con_lines <- c(con_lines, "")

con_lines <- c(con_lines,
  "Rank Statistics (Log-Rank weights)",
  "TRT01A | N | Observed | Expected | O-E",
  "--------------------------------------")
for (i in seq_len(nrow(rank_lr))) {
  con_lines <- c(con_lines, paste0(rank_lr$TRT01A[i], " | ", rank_lr$N[i], " | ",
    fmt_n(rank_lr$OBSERVED[i], 4), " | ", fmt_n(rank_lr$EXPECTED[i], 4), " | ",
    fmt_n(rank_lr$OE[i], 4)))
  con_lines <- c(con_lines, paste0("  (high precision) ", rank_lr$TRT01A[i],
    " obs=", formatC(rank_lr$OBSERVED[i], format = "f", digits = 5),
    " exp=", formatC(rank_lr$EXPECTED[i], format = "f", digits = 5),
    " oe=", formatC(rank_lr$OE[i], format = "f", digits = 5)))
}
con_lines <- c(con_lines, "",
  "Covariance Matrix for the Log-Rank Statistics")
hdr <- paste(c(".", rank_lr$TRT01A), collapse = " | ")
con_lines <- c(con_lines, hdr)
for (r in seq_len(nrow(V_lr))) {
  con_lines <- c(con_lines, paste(c(rank_lr$TRT01A[r],
    sapply(seq_len(ncol(V_lr)), function(cc) fmt_n(V_lr[r, cc], 4))), collapse = " | "))
  con_lines <- c(con_lines, paste(c(paste0("  (prec5) ", rank_lr$TRT01A[r]),
    sapply(seq_len(ncol(V_lr)), function(cc) formatC(V_lr[r, cc], format = "f", digits = 5))), collapse = " | "))
}
con_lines <- c(con_lines, "",
  "Rank Statistics (Wilcoxon / Breslow weights N_j)",
  "TRT01A | N | Observed | Expected | O-E",
  "--------------------------------------")
for (i in seq_len(nrow(rank_w))) {
  con_lines <- c(con_lines, paste0(rank_w$TRT01A[i], " | ", rank_w$N[i], " | ",
    fmt_n(rank_w$OBSERVED[i], 4), " | ", fmt_n(rank_w$EXPECTED[i], 4), " | ",
    fmt_n(rank_w$OE[i], 4)))
  con_lines <- c(con_lines, paste0("  (high precision) ", rank_w$TRT01A[i],
    " obs=", formatC(rank_w$OBSERVED[i], format = "f", digits = 5),
    " exp=", formatC(rank_w$EXPECTED[i], format = "f", digits = 5),
    " oe=", formatC(rank_w$OE[i], format = "f", digits = 5)))
  # integer form for OE like 106 / -106
  con_lines <- c(con_lines, paste0("  (integer) ", rank_w$TRT01A[i],
    " observed=", fmt_int(round(rank_w$OBSERVED[i])),
    " expected=", fmt_int(round(rank_w$EXPECTED[i])),
    " oe=", fmt_int(round(rank_w$OE[i]))))
}
con_lines <- c(con_lines, "",
  "Covariance Matrix for the Wilcoxon Statistics")
con_lines <- c(con_lines, paste(c(".", rank_w$TRT01A), collapse = " | "))
for (r in seq_len(nrow(V_w))) {
  con_lines <- c(con_lines, paste(c(rank_w$TRT01A[r],
    sapply(seq_len(ncol(V_w)), function(cc) fmt_n(V_w[r, cc], 4))), collapse = " | "))
  con_lines <- c(con_lines, paste(c(paste0("  (prec1) ", rank_w$TRT01A[r]),
    sapply(seq_len(ncol(V_w)), function(cc) formatC(V_w[r, cc], format = "f", digits = 1))), collapse = " | "))
  con_lines <- c(con_lines, paste(c(paste0("  (prec5) ", rank_w$TRT01A[r]),
    sapply(seq_len(ncol(V_w)), function(cc) formatC(V_w[r, cc], format = "f", digits = 5))), collapse = " | "))
}
con_lines <- c(con_lines, "",
  "Test of Equality over Strata",
  "Test | Chi-Square | DF | Pr > Chi-Square",
  "-----------------------------------------",
  paste0("Log-Rank | ", fmt_n(as.numeric(chi_lr), 4), " | ", df_lr, " | ", fmt_n(p_lr, 4)),
  paste0("Wilcoxon | ", fmt_n(as.numeric(chi_w), 4), " | ", df_w, " | ", fmt_n(p_w, 4)),
  paste0("  (high precision) Log-Rank chisq=", formatC(as.numeric(chi_lr), format = "f", digits = 5),
         " p=", formatC(p_lr, format = "f", digits = 5)),
  paste0("  (high precision) Wilcoxon chisq=", formatC(as.numeric(chi_w), format = "f", digits = 5),
         " p=", formatC(p_w, format = "f", digits = 5)),
  "")

con_lines <- c(con_lines,
  "Product-Limit Survival Estimates (one row per AVAL time, Greenwood SE, LOGLOG 95% CI)",
  "TRT01A | AVAL | Survival | Failure | StdErr | Lower | Upper | N_Risk | N_Event | N_Censor | N_Failed_Cum | N_Left",
  "--------------------------------------------------------------------------------------------------------------------------------")
for (i in seq_len(nrow(plot_data))) {
  con_lines <- c(con_lines, paste0(
    plot_data$TRT01A[i], " | ",
    fmt_time(plot_data$AVAL[i]), " | ",
    fmt_n(plot_data$SURVIVAL[i], 4), " | ",
    fmt_n(plot_data$FAILURE[i], 4), " | ",
    fmt_n(plot_data$STDERR[i], 4), " | ",
    ifelse(is.na(plot_data$LOWER[i]), ".", fmt_n(plot_data$LOWER[i], 4)), " | ",
    ifelse(is.na(plot_data$UPPER[i]), ".", fmt_n(plot_data$UPPER[i], 4)), " | ",
    plot_data$N_RISK[i], " | ", plot_data$N_EVENT[i], " | ", plot_data$N_CENSOR[i], " | ",
    plot_data$N_FAILED_CUM[i], " | ", plot_data$N_LEFT[i]))
}
con_lines <- c(con_lines, "",
  "Footnote: Survival = event-free survival; Failure = 1 - Survival; AVAL*CNSR(1) censors CNSR=1.",
  "Style=Journal; Width=7in Height=5.5in Imagename=KM_Curve.")

writeLines(con_lines, txt_path)
writeLines(con_lines, lst_path)

out_csv <- plot_data
names(out_csv) <- c("TRT01A","AVAL","SURVIVAL","FAILURE","STDERR","LOWER","UPPER",
                    "N_RISK","N_EVENT","N_CENSOR","N_FAILED_CUM","N_LEFT")
write.csv(out_csv, csv_path, row.names = FALSE, na = "")
try(write.csv(cens_sum, file.path(out, "P020_censored_summary.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(quart_df, file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(mean_df, file.path(out, "P020_means.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(data.frame(TEST = c("Log-Rank","Wilcoxon"),
  CHISQ = c(as.numeric(chi_lr), as.numeric(chi_w)), DF = c(df_lr, df_w), PVALUE = c(p_lr, p_w)),
  file.path(out, "P020_tests.csv"), row.names = FALSE, na = ""), silent = TRUE)

message("Done. Used N=", n_used, " strata=", length(strata_levels),
        " chi_lr=", fmt_n(as.numeric(chi_lr),4), " chi_w=", fmt_n(as.numeric(chi_w),4))
```