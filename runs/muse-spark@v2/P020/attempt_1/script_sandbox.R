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
fmt_p <- function(x, d = 1) fmt_n(x, d)

adam_dir <- "/bench/data/derived/c2c/data/adam"
alt_dirs <- c(adam_dir,
  file.path(Sys.getenv("PROJECT_PATH"), "data", "adam"),
  "/bench/data/raw/cdisc-pilot/adam")

find_file <- function(dirs, base) {
  for (d in dirs) {
    if (is.na(d) || d == "") next
    dd <- gsub("\\\\", "/", d)
    if (!dir.exists(dd)) next
    # exact names
    cand <- list.files(dd, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    hit <- cand[grepl(paste0("(^|/)", base, "\\.(sas7bdat|xpt|csv)$"), cand, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  return(NA_character_)
}

adtte_path <- find_file(alt_dirs, "adtte")
# fallback: list adam_dir directly
if (is.na(adtte_path)) {
  if (dir.exists(adam_dir)) {
    allf <- list.files(adam_dir, full.names = TRUE, recursive = TRUE)
    print(allf)
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
# drop .x/.y style duplicates if any (none expected)
# keep needed cols; ensure types
adtte$AVAL <- suppressWarnings(as.numeric(adtte$AVAL))
adtte$CNSR <- suppressWarnings(as.numeric(adtte$CNSR))
adtte$TRT01A <- as.character(adtte$TRT01A)
if ("USUBJID" %in% names(adtte)) adtte$USUBJID <- as.character(adtte$USUBJID)
if ("PARAMCD" %in% names(adtte)) adtte$PARAMCD <- as.character(adtte$PARAMCD)
if ("PARAM" %in% names(adtte)) adtte$PARAM <- as.character(adtte$PARAM)

# SAS: TIME AVAL*CNSR(1) -> event = (CNSR=0), censored=(CNSR=1); missing excluded
n_read <- nrow(adtte)
adtte_an <- adtte %>% filter(!is.na(AVAL), !is.na(CNSR), !is.na(TRT01A), TRT01A != "")
n_used <- nrow(adtte_an)
adtte_an$EVENT <- ifelse(adtte_an$CNSR == 1, 0L, 1L)
# SAS missing sorts first; character ordering uppercase before lowercase -> base sort
strata_levels <- sort(unique(adtte_an$TRT01A))
message(paste("STRATA:", paste(strata_levels, collapse = " | ")))

# ---- Censored summary (Summary of the Number of Censored and Uncensored Values) ----
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

# ---- Survival fits ----
# common tau = largest event time (SAS restricts mean to largest event time)
tau_common <- suppressWarnings(max(adtte_an$AVAL[adtte_an$EVENT == 1L], na.rm = TRUE))
if (!is.finite(tau_common)) tau_common <- suppressWarnings(max(adtte_an$AVAL, na.rm = TRUE))

fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, conf.type = "log-log")
fit_rmean <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an,
                     conf.type = "log-log", rmean = "common")

# rmean table
rmean_tab <- as.data.frame(fit_rmean$table, stringsAsFactors = FALSE)
# rownames like TRT01A=Placebo
rmean_tab$STRATA <- rownames(rmean_tab)
rmean_tab$TRT01A <- sub("^TRT01A=", "", rmean_tab$STRATA)

# Quartiles with CI (Brookmeyer-Crowley via quantile)
q_obj <- tryCatch(quantile(fit, probs = c(0.75, 0.5, 0.25), conf.int = TRUE), error = function(e) NULL)
# Build quartile data.frame manually from surv tables for robustness
quartile_rows <- list()
mean_rows <- list()
pl_list <- list()

for (st in strata_levels) {
  dsub <- adtte_an %>% filter(TRT01A == st) %>% arrange(AVAL)
  f1 <- survfit(Surv(AVAL, EVENT) ~ 1, data = dsub, conf.type = "log-log")
  # product-limit at every distinct AVAL (mimic SAS showing each time incl. censored)
  utimes <- sort(unique(dsub$AVAL))
  ss <- summary(f1, times = utimes, extend = TRUE)
  # ss vectors
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

  # quartiles: first time where surv <= p
  for (prob in c(0.75, 0.5, 0.25)) {
    pct_label <- ifelse(prob == 0.75, "75", ifelse(prob == 0.5, "50", "25"))
    idx <- which(sv <= prob)
    est <- if (length(idx) > 0) tm[min(idx)] else NA_real_
    # CI from quantile object if available
    ci_lo <- NA_real_; ci_up <- NA_real_
    if (!is.null(q_obj)) {
      # q_obj is list/matrix; try to extract
      try({
        qq <- q_obj$quantile
        ql <- q_obj$lower
        qu <- q_obj$upper
        # rownames correspond to probs? colnames strata?
        # quantile.survfit returns matrix with rows = probs? Handle generically below
      }, silent = TRUE)
    }
    quartile_rows[[paste(st, pct_label)]] <- data.frame(
      TRT01A = st, PERCENT = as.integer(pct_label), ESTIMATE = est,
      LOWER = ci_lo, UPPER = ci_up, stringsAsFactors = FALSE)
  }
}
pl_all <- do.call(rbind, pl_list)
rownames(pl_all) <- NULL

# Use quantile() CIs where possible: parse q_obj
# q_obj$quantile etc. dimensions vary; attempt to fill quartile CIs
try({
  qq <- q_obj$quantile; ql <- q_obj$lower; qu <- q_obj$upper
  # qq may be matrix [strata x probs] or vector
  # Normalize: ensure matrix
  if (!is.null(qq)) {
    # Determine strata order in fit
    fit_strata_names <- sub("^TRT01A=", "", names(fit$strata))
    probs <- c(0.75, 0.5, 0.25)
    # qq dimensions: if matrix, rows=strata or probs?
    # survival::quantile.survfit returns matrix with rows = quantiles?
    # We'll match by dimnames if present
    dn1 <- dimnames(qq)[[1]]; dn2 <- dimnames(qq)[[2]]
    for (st in strata_levels) {
      for (j in seq_along(probs)) {
        pct <- c(75L, 50L, 25L)[j]
        key <- paste(st, pct)
        # try to locate value by dimnames
        v_q <- NA; v_l <- NA; v_u <- NA
        # brute: search all cells? Instead use quantile per-stratum fit
        dsub2 <- adtte_an %>% filter(TRT01A == st)
        f2 <- survfit(Surv(AVAL, EVENT) ~ 1, data = dsub2, conf.type = "log-log")
        q2 <- tryCatch(quantile(f2, probs = probs[j], conf.int = TRUE), error = function(e) NULL)
        if (!is.null(q2)) {
          v_q <- suppressWarnings(as.numeric(q2$quantile[1]))
          v_l <- suppressWarnings(as.numeric(q2$lower[1]))
          v_u <- suppressWarnings(as.numeric(q2$upper[1]))
          quartile_rows[[key]]$ESTIMATE <- v_q
          quartile_rows[[key]]$LOWER <- v_l
          quartile_rows[[key]]$UPPER <- v_u
        }
      }
    }
  }
}, silent = TRUE)
quart_df <- do.call(rbind, quartile_rows)
rownames(quart_df) <- NULL

# Means (restricted to tau_common): from fit_rmean$table
# columns: *rmean, *se(rmean), *median etc.
mean_df <- data.frame(TRT01A = rmean_tab$TRT01A,
                      N = rmean_tab$records,
                      N_EVENT = rmean_tab$events,
                      RMEAN = rmean_tab$`rmean`,
                      SE_RMEAN = rmean_tab$`se(rmean)`,
                      MEDIAN = rmean_tab$median,
                      LCL_MEDIAN = rmean_tab$`0.95LCL`,
                      UCL_MEDIAN = rmean_tab$`0.95UCL`,
                      stringsAsFactors = FALSE, check.names = FALSE)

# ---- Rank tests: Log-Rank and Wilcoxon (Breslow, rho=1 approx) ----
sd_lr <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, rho = 0)
sd_w <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte_an, rho = 1)
chisq_lr <- as.numeric(sd_lr$chisq); df_lr <- length(sd_lr$n) - 1L
p_lr <- 1 - pchisq(chisq_lr, df_lr)
chisq_w <- as.numeric(sd_w$chisq); df_w <- length(sd_w$n) - 1L
p_w <- 1 - pchisq(chisq_w, df_w)

# Observed vs Expected per stratum (rank statistics O-E)
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

# Covariance matrices (O-E covariance): sd$var
cov_lr <- sd_lr$var
cov_w <- sd_w$var
dimnames(cov_lr) <- list(rank_lr$TRT01A, rank_lr$TRT01A)
dimnames(cov_w) <- list(rank_w$TRT01A, rank_w$TRT01A)

# ---- Figure: KM curve with at-risk ----
# Prepare plot data CSV (plotted survival points at event/censor times)
plot_data <- pl_all
# ggsurvplot
ggs <- ggsurvplot(fit, data = adtte_an, risk.table = TRUE,
                  conf.int = TRUE, censor = TRUE,
                  xlab = "Time (AVAL)", ylab = "Event-Free Survival Probability",
                  title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
                  subtitle = "Population: Safety Population",
                  legend.title = "Treatment", legend.labs = strata_levels,
                  risk.table.title = "Number at risk",
                  ggtheme = theme_bw())
# ggsurvplot returns list; arrange and save
png_path <- file.path(out, "P020.png")
png_path2 <- file.path(out, "tlf_figure1.png")
# Use arrange_ggsurvplots for combined
arr <- arrange_ggsurvplots(list(ggs), print = FALSE, ncol = 1, nrow = 1)
ggsave(png_path, arr, width = 7, height = 5.5, units = "in", dpi = 300)
# copy to alternate name
try(file.copy(png_path, png_path2, overwrite = TRUE), silent = TRUE)
# also KM_Curve name from SAS imagename
try(file.copy(png_path, file.path(out, "KM_Curve.png"), overwrite = TRUE), silent = TRUE)

# CSV of plotted data
write.csv(plot_data, file.path(out, "P020_plotdata.csv"), row.names = FALSE, na = "")
try(write.csv(plot_data, file.path(out, "tlf_figure1_plotdata.csv"), row.names = FALSE, na = ""), silent = TRUE)

# ---- Text rendering (P020.lst equivalent) ----
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
# Censored summary
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
    fmt_n(rhup(cens_sum$PercentCensored[i], 2), 2)))
}
con_lines <- c(con_lines, paste0("Total | ", tot_failed, " | ", tot_cens, " | ", tot_n,
  " | ", fmt_n(rhup(tot_pct, 2), 2)), "")

# Quartiles
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

# Means
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

# Rank statistics
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

# Product-limit estimates per stratum
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

# CSV of table body: product-limit estimates (main table) + summary sheets?
# Primary CSV = product-limit estimates
out_csv <- plot_data
names(out_csv) <- c("TRT01A","AVAL","SURVIVAL","FAILURE","STDERR","LOWER","UPPER",
                    "N_RISK","N_EVENT","N_CENSOR","N_FAILED_CUM","N_LEFT")
write.csv(out_csv, csv_path, row.names = FALSE, na = "")
# Additional CSVs for other blocks (do not harm)
try(write.csv(cens_sum, file.path(out, "P020_censored_summary.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(quart_df, file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(mean_df, file.path(out, "P020_means.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(data.frame(TEST = c("Log-Rank","Wilcoxon"),
  CHISQ = c(chisq_lr, chisq_w), DF = c(df_lr, df_w), PVALUE = c(p_lr, p_w)),
  file.path(out, "P020_tests.csv"), row.names = FALSE, na = ""), silent = TRUE)

message("Done. Used N=", n_used, " strata=", length(strata_levels))
