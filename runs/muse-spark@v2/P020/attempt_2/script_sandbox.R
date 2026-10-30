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

# ---- SAS round half away from zero ----
rhup <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-12) / 10^d)
}
fmt_n <- function(x, d) {
  ifelse(is.na(x), "",
    formatC(rhup(x, d), format = "f", digits = d, flag = "#"))
}

adam_dir <- "/bench/data/derived/c2c/data/adam"
bench_adam <- "/bench/data/derived/c2c/data/adam"
alt_dirs <- c(bench_adam, adam_dir,
  file.path(Sys.getenv("PROJECT_PATH"), "data", "adam"),
  "/bench/data/raw/cdisc-pilot/adam")

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
if (is.na(adtte_path)) {
  if (dir.exists(adam_dir)) {
    allf <- list.files(adam_dir, full.names = TRUE, recursive = TRUE)
    print(allf)
  }
  if (dir.exists(bench_adam)) {
    print(list.files(bench_adam, full.names = TRUE, recursive = TRUE))
  }
  stop("ADTTE file not found")
}
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

tau_common <- suppressWarnings(max(adtte_an$AVAL[adtte_an$EVENT == 1L], na.rm = TRUE))
if (!is.finite(tau_common)) tau_common <- suppressWarnings(max(adtte_an$AVAL, na.rm = TRUE))

fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, conf.type = "log-log")

# base table for N/events/median (no rmean arg for compatibility)
fit_tab <- as.data.frame(fit$table, stringsAsFactors = FALSE)
fit_tab$STRATA <- rownames(fit_tab)
fit_tab$TRT01A <- sub("^TRT01A=", "", fit_tab$STRATA)

# ---- restricted mean up to tau_common (area under KM) + Greenwood-type SE ----
rmean_one <- function(dsub, tau) {
  dsub <- dsub[order(dsub$AVAL), , drop = FALSE]
  f1 <- survfit(Surv(AVAL, EVENT) ~ 1, data = dsub, conf.type = "log-log")
  ut <- sort(unique(dsub$AVAL))
  # keep only times <= tau; ensure tau included
  ut2 <- ut[ut <= tau]
  if (length(ut2) == 0 || max(ut2) < tau) ut2 <- sort(unique(c(ut2, tau)))
  ss <- summary(f1, times = ut2, extend = TRUE)
  tm <- ss$time; sv <- ss$surv; nr <- ss$n.risk; ne <- ss$n.event
  # summary with extend may return NA surv beyond range; locf
  for (i in seq_along(sv)) {
    if (is.na(sv[i])) sv[i] <- if (i > 1) sv[i-1] else 1
    if (is.na(nr[i])) nr[i] <- 0
    if (is.na(ne[i])) ne[i] <- 0
  }
  t_all <- c(0, tm)
  s_all <- c(1, sv)
  # mu = sum S_{i-1} * (t_i - t_{i-1})
  mu <- sum(s_all[seq_len(length(tm))] * diff(t_all))
  # partial integrals up to each t_j
  cum_int <- cumsum(s_all[seq_len(length(tm))] * diff(t_all))
  A_j <- mu - cum_int  # integral from t_j to tau
  var_mu <- 0
  for (j in seq_along(tm)) {
    d <- ne[j]; Y <- nr[j]
    if (!is.na(d) && !is.na(Y) && d > 0 && Y > 0 && (Y - d) > 0) {
      var_mu <- var_mu + (A_j[j]^2) * d / (Y * (Y - d))
    }
  }
  list(mean = mu, se = sqrt(var_mu))
}

q_obj <- tryCatch(quantile(fit, probs = c(0.75, 0.5, 0.25), conf.int = TRUE), error = function(e) NULL)

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
  # per-stratum quantile CIs
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

# Means data.frame combining fit$table medians + manual rmean
mean_df_list <- list()
for (st in strata_levels) {
  dsub <- adtte_an %>% filter(TRT01A == st)
  rm <- rmean_one(dsub, tau_common)
  # locate row in fit_tab
  fr <- fit_tab[fit_tab$TRT01A == st, , drop = FALSE]
  med <- if ("median" %in% names(fr)) suppressWarnings(as.numeric(fr$median[1])) else NA_real_
  lcl <- if ("0.95LCL" %in% names(fr)) suppressWarnings(as.numeric(fr$"0.95LCL"[1])) else NA_real_
  ucl <- if ("0.95UCL" %in% names(fr)) suppressWarnings(as.numeric(fr$"0.95UCL"[1])) else NA_real_
  rec <- if ("records" %in% names(fr)) suppressWarnings(as.numeric(fr$records[1])) else nrow(dsub)
  ev <- if ("events" %in% names(fr)) suppressWarnings(as.numeric(fr$events[1])) else sum(dsub$EVENT == 1L)
  mean_df_list[[st]] <- data.frame(TRT01A = st, N = rec, N_EVENT = ev,
    RMEAN = rm$mean, SE_RMEAN = rm$se, MEDIAN = med,
    LCL_MEDIAN = lcl, UCL_MEDIAN = ucl, stringsAsFactors = FALSE, check.names = FALSE)
}
mean_df <- do.call(rbind, mean_df_list)
rownames(mean_df) <- NULL

sd_lr <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, rho = 0)
sd_w <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, rho = 1)
chisq_lr <- as.numeric(sd_lr$chisq); df_lr <- length(sd_lr$n) - 1L
p_lr <- 1 - pchisq(chisq_lr, df_lr)
chisq_w <- as.numeric(sd_w$chisq); df_w <- length(sd_w$n) - 1L
p_w <- 1 - pchisq(chisq_w, df_w)

rank_lr <- data.frame(TRT01A = sub("^TRT01A=", "", names(sd_lr$n)),
                      N = as.numeric(sd_lr$n),
                      OBSERVED = as.numeric(sd_lr$obs),
                      EXPECTED = as.numeric(sd_lr$exp),
                      stringsAsFactors = FALSE)
rank_lr$OE <- rank_lr$OBSERVED - rank_lr$EXPECTED
rank_w <- data.frame(TRT01A = sub("^TRT01A=", "", names(sd_w$n)),
                     N = as.numeric(sd_w$n),
                     OBSERVED = as.numeric(sd_w$obs),
                     EXPECTED = as.numeric(sd_w$exp),
                     stringsAsFactors = FALSE)
rank_w$OE <- rank_w$OBSERVED - rank_w$EXPECTED

cov_lr <- sd_lr$var
cov_w <- sd_w$var
dimnames(cov_lr) <- list(rank_lr$TRT01A, rank_lr$TRT01A)
dimnames(cov_w) <- list(rank_w$TRT01A, rank_w$TRT01A)

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
  paste0("Restricted mean common tau (largest event time): ", fmt_n(tau_common, 1)),
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
    ifelse(is.na(quart_df$ESTIMATE[i]), ".", fmt_n(quart_df$ESTIMATE[i], 1)), " | ",
    ifelse(is.na(quart_df$LOWER[i]), ".", fmt_n(quart_df$LOWER[i], 1)), " | ",
    ifelse(is.na(quart_df$UPPER[i]), ".", fmt_n(quart_df$UPPER[i], 1))))
}
con_lines <- c(con_lines, "")

con_lines <- c(con_lines,
  paste0("Mean Survival Time (Restricted to ", fmt_n(tau_common, 1), ") and Standard Error"),
  "TRT01A | N | Events | Mean | SE(Mean) | Median | Median Lower | Median Upper",
  "----------------------------------------------------------------------------")
for (i in seq_len(nrow(mean_df))) {
  con_lines <- c(con_lines, paste0(
    mean_df$TRT01A[i], " | ", mean_df$N[i], " | ", mean_df$N_EVENT[i], " | ",
    fmt_n(mean_df$RMEAN[i], 4), " | ", fmt_n(mean_df$SE_RMEAN[i], 4), " | ",
    ifelse(is.na(mean_df$MEDIAN[i]), ".", fmt_n(mean_df$MEDIAN[i], 1)), " | ",
    ifelse(is.na(mean_df$LCL_MEDIAN[i]), ".", fmt_n(mean_df$LCL_MEDIAN[i], 1)), " | ",
    ifelse(is.na(mean_df$UCL_MEDIAN[i]), ".", fmt_n(mean_df$UCL_MEDIAN[i], 1))))
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
}
con_lines <- c(con_lines, "",
  "Covariance Matrix for the Log-Rank Statistics")
hdr <- paste(c(".", rank_lr$TRT01A), collapse = " | ")
con_lines <- c(con_lines, hdr)
for (r in seq_len(nrow(cov_lr))) {
  con_lines <- c(con_lines, paste(c(rank_lr$TRT01A[r],
    sapply(seq_len(ncol(cov_lr)), function(cc) fmt_n(cov_lr[r, cc], 4))), collapse = " | "))
}
con_lines <- c(con_lines, "",
  "Rank Statistics (Wilcoxon / Breslow weights, rho=1)",
  "TRT01A | N | Observed | Expected | O-E",
  "--------------------------------------")
for (i in seq_len(nrow(rank_w))) {
  con_lines <- c(con_lines, paste0(rank_w$TRT01A[i], " | ", rank_w$N[i], " | ",
    fmt_n(rank_w$OBSERVED[i], 4), " | ", fmt_n(rank_w$EXPECTED[i], 4), " | ",
    fmt_n(rank_w$OE[i], 4)))
}
con_lines <- c(con_lines, "",
  "Covariance Matrix for the Wilcoxon Statistics")
con_lines <- c(con_lines, paste(c(".", rank_w$TRT01A), collapse = " | "))
for (r in seq_len(nrow(cov_w))) {
  con_lines <- c(con_lines, paste(c(rank_w$TRT01A[r],
    sapply(seq_len(ncol(cov_w)), function(cc) fmt_n(cov_w[r, cc], 4))), collapse = " | "))
}
con_lines <- c(con_lines, "",
  "Test of Equality over Strata",
  "Test | Chi-Square | DF | Pr > Chi-Square",
  "-----------------------------------------",
  paste0("Log-Rank | ", fmt_n(chisq_lr, 4), " | ", df_lr, " | ", fmt_n(p_lr, 4)),
  paste0("Wilcoxon | ", fmt_n(chisq_w, 4), " | ", df_w, " | ", fmt_n(p_w, 4)),
  "")

con_lines <- c(con_lines,
  "Product-Limit Survival Estimates (one row per AVAL time, Greenwood SE, LOGLOG 95% CI)",
  "TRT01A | AVAL | Survival | Failure | StdErr | Lower | Upper | N_Risk | N_Event | N_Censor | N_Failed_Cum | N_Left",
  "--------------------------------------------------------------------------------------------------------------------------------")
for (i in seq_len(nrow(plot_data))) {
  con_lines <- c(con_lines, paste0(
    plot_data$TRT01A[i], " | ",
    fmt_n(plot_data$AVAL[i], 1), " | ",
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
  CHISQ = c(chisq_lr, chisq_w), DF = c(df_lr, df_w), PVALUE = c(p_lr, p_w)),
  file.path(out, "P020_tests.csv"), row.names = FALSE, na = ""), silent = TRUE)

message("Done. Used N=", n_used, " strata=", length(strata_levels))
