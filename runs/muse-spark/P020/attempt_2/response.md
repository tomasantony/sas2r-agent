```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(x) sprintf("%.1f", sas_round(x, 1))

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")
bench_adam <- "/bench/data/derived/c2c/data/adam"

library(haven)
library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)

cat("Reading ADTTE...\n")
adtte <- NULL
cands <- c(
  file.path(adam_dir, "adtte.sas7bdat"),
  file.path(adam_dir, "adtte.xpt"),
  file.path(adam_dir, "ADTTE.sas7bdat"),
  file.path(adam_dir, "ADTTE.xpt"),
  file.path(bench_adam, "adtte.sas7bdat"),
  file.path(bench_adam, "ADTTE.sas7bdat"),
  file.path(bench_adam, "adtte.xpt")
)
for (f in cands) {
  if (file.exists(f)) {
    cat("Found:", f, "\n")
    if (grepl("\\.xpt$", f, ignore.case = TRUE)) adtte <- haven::read_xpt(f)
    else adtte <- haven::read_sas(f)
    break
  }
}
if (is.null(adtte)) {
  cat("Listing adam_dir:\n"); print(list.files(adam_dir, full.names = TRUE))
  cat("Listing bench:\n"); print(list.files(bench_adam, full.names = TRUE))
  stop("ADTTE not found")
}
names(adtte) <- toupper(names(adtte))
cat("ADTTE dims:", nrow(adtte), ncol(adtte), "\n")

df2 <- adtte %>% filter(!is.na(AVAL))
cat("N with non-missing AVAL:", nrow(df2), "\n")
if (is.character(df2$CNSR)) df2$CNSR <- suppressWarnings(as.numeric(df2$CNSR))
df2 <- df2 %>% mutate(
  EVENT = ifelse(is.na(CNSR), 1, ifelse(CNSR == 1, 0, 1)),
  TRT01A = ifelse(is.na(TRT01A), "Missing", as.character(TRT01A))
)
print(df2 %>% count(TRT01A, CNSR, EVENT))

strata_levels <- sort(unique(df2$TRT01A))
cat("Strata levels:\n"); print(strata_levels)
df2$TRT01A <- factor(df2$TRT01A, levels = strata_levels)

# KM fit - use log-log CI to match SAS PROC LIFETEST default
fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = df2, conf.type = "log-log")

# Full KM table
summ <- summary(fit)
km_df <- data.frame(
  STRATUM = rep(names(summ$strata), times = summ$strata),
  TIME = summ$time,
  N_RISK = summ$n.risk,
  N_EVENT = summ$n.event,
  N_CENSOR = summ$n.censor,
  SURV = summ$surv,
  STDERR = summ$std.err,
  LOWER = summ$lower,
  UPPER = summ$upper,
  stringsAsFactors = FALSE
)
km_df$TRT01A <- sub("^.*=", "", km_df$STRATUM)
km_df <- km_df %>% arrange(TRT01A, TIME)
cat("KM rows:", nrow(km_df), "\n")

# Censoring summary
cens_sum <- df2 %>% group_by(TRT01A) %>%
  summarise(N = n(), EVENTS = sum(EVENT == 1), CENSORED = sum(EVENT == 0), .groups = "drop") %>%
  mutate(PCT_CENSORED = 100 * CENSORED / N) %>%
  arrange(TRT01A)
print(cens_sum)
total_N <- nrow(df2)
total_ev <- sum(df2$EVENT == 1)
total_cen <- sum(df2$EVENT == 0)

# Rank tests
sd_logrank <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = df2, rho = 0)
sd_wilcox <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = df2, rho = 1)
logrank_chisq <- as.numeric(sd_logrank$chisq)
logrank_df <- length(sd_logrank$n) - 1
logrank_p <- 1 - pchisq(logrank_chisq, logrank_df)
wilcox_chisq <- as.numeric(sd_wilcox$chisq)
wilcox_df <- length(sd_wilcox$n) - 1
wilcox_p <- 1 - pchisq(wilcox_chisq, wilcox_df)
cat(sprintf("Log-Rank chiSq=%.4f df=%d p=%.4f\n", logrank_chisq, logrank_df, logrank_p))
cat(sprintf("Wilcoxon chiSq=%.4f df=%d p=%.4f\n", wilcox_chisq, wilcox_df, wilcox_p))

# Quartiles via quantile()
q_probs <- c(0.25, 0.5, 0.75)
qres <- quantile(fit, probs = q_probs, conf.int = TRUE)
cat("qres structure:\n"); str(qres)
print(qres)

quart_tab <- NULL
try({
  qm <- qres$quantile; ql <- qres$lower; qu <- qres$upper
  if (is.matrix(qm)) {
    rn <- rownames(qm)
    trt <- sub("^.*=", "", rn)
    cn <- colnames(qm)
    cat("quantile colnames:"); print(cn)
    # columns are "25","50","75"
    getc <- function(m, nm) {
      if (nm %in% colnames(m)) return(m[, nm])
      return(m[, 1])
    }
    quart_tab <- data.frame(
      TRT01A = trt,
      Q75 = getc(qm, "25"), Q75_L = getc(ql, "25"), Q75_U = getc(qu, "25"),
      MED = getc(qm, "50"), MED_L = getc(ql, "50"), MED_U = getc(qu, "50"),
      Q25 = getc(qm, "75"), Q25_L = getc(ql, "75"), Q25_U = getc(qu, "75"),
      stringsAsFactors = FALSE
    )
  }
}, silent = TRUE)
get_quartile <- function(times, survs, ps) {
  idx <- which(survs <= ps)
  if (length(idx) == 0) return(NA_real_)
  times[min(idx)]
}
if (is.null(quart_tab)) {
  quart_tab <- data.frame(TRT01A = strata_levels, Q75 = NA_real_, Q75_L = NA_real_, Q75_U = NA_real_,
    MED = NA_real_, MED_L = NA_real_, MED_U = NA_real_,
    Q25 = NA_real_, Q25_L = NA_real_, Q25_U = NA_real_, stringsAsFactors = FALSE)
  for (lv in strata_levels) {
    sub <- km_df %>% filter(TRT01A == lv) %>% arrange(TIME)
    quart_tab[quart_tab$TRT01A == lv, "Q75"] <- get_quartile(sub$TIME, sub$SURV, 0.75)
    quart_tab[quart_tab$TRT01A == lv, "MED"] <- get_quartile(sub$TIME, sub$SURV, 0.50)
    quart_tab[quart_tab$TRT01A == lv, "Q25"] <- get_quartile(sub$TIME, sub$SURV, 0.25)
  }
}
print(quart_tab)

max_time <- max(df2$AVAL, na.rm = TRUE)
cat("Max AVAL:", max_time, "\n")

compute_rmean <- function(sub_km, tau) {
  t <- c(0, sub_km$TIME)
  s <- c(1, sub_km$SURV)
  if (max(t) < tau) {
    t <- c(t, tau); s <- c(s, tail(s, 1))
  } else {
    idx <- t <= tau
    t <- t[idx]; s <- s[idx]
    if (tail(t, 1) < tau) { t <- c(t, tau); s <- c(s, tail(s, 1)) }
  }
  sum(s[-length(s)] * diff(t))
}
rmeans <- sapply(strata_levels, function(lv) {
  sub <- km_df %>% filter(TRT01A == lv) %>% arrange(TIME)
  compute_rmean(sub, max_time)
})
print(rmeans)

# At-risk ticks
ticks <- pretty(c(0, max_time), n = 7)
ticks <- ticks[ticks >= 0 & ticks <= max_time]
if (length(ticks) == 0) ticks <- c(0, max_time)
if (ticks[1] != 0) ticks <- c(0, ticks)
if (tail(ticks, 1) != max_time) ticks <- c(ticks, max_time)
ticks <- sort(unique(round(ticks)))
cat("Ticks:", ticks, "\n")

atr <- summary(fit, times = ticks, extend = TRUE)
# atr vectors length = length(ticks)*nstrata
nstr <- length(atr$strata)
atrisk_df2 <- data.frame(
  TIME = atr$time,
  N_RISK = atr$n.risk,
  N_EVENT = atr$n.event,
  N_CENSOR = atr$n.censor,
  SURV = atr$surv,
  STRATA = rep(names(atr$strata), times = atr$strata / length(ticks) * length(ticks)),
  stringsAsFactors = FALSE
)
# More robust: rep each stratum length(ticks) times
# atr$strata gives counts per stratum (each should equal length(ticks))
if (nrow(atrisk_df2) != sum(atr$strata)) {
  atrisk_df2$STRATA <- rep(names(atr$strata), times = as.vector(atr$strata))
} else {
  atrisk_df2$STRATA <- rep(names(atr$strata), times = as.vector(atr$strata))
}
atrisk_df2$TRT01A <- sub("^.*=", "", atrisk_df2$STRATA)
atrisk_df2 <- atrisk_df2 %>% arrange(TRT01A, TIME)
print(atrisk_df2)

# Plot data with time 0
plot_data_list <- list()
for (lv in strata_levels) {
  sub <- km_df %>% filter(TRT01A == lv) %>% arrange(TIME)
  row0 <- data.frame(STRATUM = paste0("TRT01A=", lv), TIME = 0,
    N_RISK = sum(df2$TRT01A == lv), N_EVENT = 0, N_CENSOR = 0,
    SURV = 1, STDERR = 0, LOWER = 1, UPPER = 1, TRT01A = lv, stringsAsFactors = FALSE)
  plot_data_list[[lv]] <- bind_rows(row0, sub)
}
plot_data <- bind_rows(plot_data_list)
write.csv(plot_data, file.path(out, "P020_plotdata.csv"), row.names = FALSE, na = "")
cens_data <- km_df %>% filter(N_CENSOR > 0)

p <- ggplot(plot_data, aes(x = TIME, y = SURV, color = TRT01A, linetype = TRT01A)) +
  geom_step(linewidth = 0.9) +
  geom_point(data = cens_data, aes(x = TIME, y = SURV, color = TRT01A),
    shape = 3, size = 1.8, stroke = 0.8, show.legend = FALSE) +
  geom_ribbon(data = plot_data, aes(x = TIME, ymin = LOWER, ymax = UPPER, fill = TRT01A),
    alpha = 0.12, linetype = 0, show.legend = FALSE) +
  labs(
    title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
    subtitle = "Population: Safety Population",
    x = "Time (AVAL)",
    y = "Survival Probability",
    color = "Treatment",
    linetype = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
cap_lines <- sapply(strata_levels, function(lv) {
  v <- atrisk_df2 %>% filter(TRT01A == lv) %>% arrange(TIME) %>% pull(N_RISK)
  paste0(lv, ": ", paste(v, collapse = " / "))
})
cap_text <- paste0("Number at risk (Time = ", paste(ticks, collapse = ", "), "):\n",
  paste(cap_lines, collapse = "\n"))
p <- p + labs(caption = cap_text) + theme(plot.caption = element_text(hjust = 0, size = 7))
ggsave(file.path(out, "P020.png"), p, width = 7, height = 5.5, dpi = 300, units = "in")
cat("Figure saved.\n")

# CSV outputs
km_out <- km_df %>% select(TRT01A, TIME, N_RISK, N_EVENT, N_CENSOR, SURV, STDERR, LOWER, UPPER)
write.csv(km_out, file.path(out, "P020.csv"), row.names = FALSE, na = "")
write.csv(cens_sum, file.path(out, "P020_censoring.csv"), row.names = FALSE, na = "")
write.csv(atrisk_df2, file.path(out, "P020_atrisk.csv"), row.names = FALSE, na = "")
write.csv(quart_tab, file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = "")

# Text rendering
txtfile <- file.path(out, "P020.txt")
sink(txtfile)
cat("Figure 1: Kaplan-Meier Event-Free Survival by Treatment\n")
cat("Population: Safety Population\n")
cat("Source: adam.adtte  (time AVAL * CNSR(1), strata TRT01A)\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")
cat("Summary of the Number of Censored and Uncensored Values\n")
cat(paste(rep("-", 100), collapse = ""), "\n")
cat(sprintf("%-20s %10s %10s %10s %12s\n", "Stratum (TRT01A)", "Total", "Failed", "Censored", "% Censored"))
for (i in seq_len(nrow(cens_sum))) {
  cat(sprintf("%-20s %10d %10d %10d %12s\n",
    cens_sum$TRT01A[i], cens_sum$N[i], cens_sum$EVENTS[i], cens_sum$CENSORED[i],
    fmt_pct1(cens_sum$PCT_CENSORED[i])))
}
cat(sprintf("%-20s %10d %10d %10d %12s\n", "Total",
  total_N, total_ev, total_cen, fmt_pct1(100 * total_cen / total_N)))
cat("\n")
cat("Quartile Estimates (Point Estimate and 95% Confidence Interval)\n")
cat(paste(rep("-", 100), collapse = ""), "\n")
cat(sprintf("%-20s %12s %12s %12s %12s\n", "Stratum", "Percent", "Estimate", "95% Lower", "95% Upper"))
fq <- function(x) ifelse(is.na(x), "       .", sprintf("%12.1f", sas_round(x, 1)))
for (i in seq_len(nrow(quart_tab))) {
  lv <- quart_tab$TRT01A[i]
  cat(sprintf("%-20s %12d %12s %12s %12s\n", lv, 75, fq(quart_tab$Q75[i]), fq(quart_tab$Q75_L[i]), fq(quart_tab$Q75_U[i])))
  cat(sprintf("%-20s %12d %12s %12s %12s\n", "", 50, fq(quart_tab$MED[i]), fq(quart_tab$MED_L[i]), fq(quart_tab$MED_U[i])))
  cat(sprintf("%-20s %12d %12s %12s %12s\n", "", 25, fq(quart_tab$Q25[i]), fq(quart_tab$Q25_L[i]), fq(quart_tab$Q25_U[i])))
}
cat("\n")
cat("Note: Quartiles are event times at which survival = 0.75 / 0.50 / 0.25.\n")
cat("      95% CI by Brookmeyer-Crowley (log-log transformed survival).\n\n")
cat("Restricted Mean Survival Time (up to ", sprintf("%.1f", sas_round(max_time, 1)), ")\n", sep = "")
cat(paste(rep("-", 100), collapse = ""), "\n")
cat(sprintf("%-20s %14s\n", "Stratum", "Restricted Mean"))
for (lv in strata_levels) {
  cat(sprintf("%-20s %14s\n", lv, sprintf("%14.2f", sas_round(rmeans[lv], 2))))
}
cat("\n")
cat("Test of Equality over Strata\n")
cat(paste(rep("-", 100), collapse = ""), "\n")
cat(sprintf("%-20s %12s %8s %12s\n", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
cat(sprintf("%-20s %12s %8d %12s\n", "Log-Rank", sprintf("%12.4f", sas_round(logrank_chisq, 4)), logrank_df, sprintf("%12.4f", sas_round(logrank_p, 4))))
cat(sprintf("%-20s %12s %8d %12s\n", "Wilcoxon (Breslow)", sprintf("%12.4f", sas_round(wilcox_chisq, 4)), wilcox_df, sprintf("%12.4f", sas_round(wilcox_p, 4))))
cat("\n")
cat("Number at Risk (atrisk for plot)\n")
cat(paste(rep("-", 100), collapse = ""), "\n")
cat(sprintf("%-20s", "Stratum"))
for (t in ticks) cat(sprintf(" %8.1f", sas_round(t, 1)))
cat("\n")
for (lv in strata_levels) {
  cat(sprintf("%-20s", lv))
  sub <- atrisk_df2 %>% filter(TRT01A == lv) %>% arrange(TIME) %>% pull(N_RISK)
  for (v in sub) cat(sprintf(" %8d", v))
  cat("\n")
}
cat("\n")
cat("Product-Limit Survival Estimates (Greenwood SE, log-log 95% CI)\n")
cat(paste(rep("-", 120), collapse = ""), "\n")
cat(sprintf("%-20s %10s %8s %8s %8s %10s %10s %10s %10s %10s %10s\n",
  "TRT01A", "AVAL", "N_RISK", "EVENT", "CENSOR", "SURVIVAL", "FAILURE", "STDERR", "LOWER95", "UPPER95", "MEDIAN"))
for (r in seq_len(nrow(km_out))) {
  surv <- km_out$SURV[r]
  fail <- 1 - surv
  cat(sprintf("%-20s %10s %8d %8d %8d %10s %10s %10s %10s %10s %10s\n",
    km_out$TRT01A[r],
    sprintf("%10.1f", sas_round(km_out$TIME[r], 1)),
    km_out$N_RISK[r], km_out$N_EVENT[r], km_out$N_CENSOR[r],
    sprintf("%10.4f", sas_round(surv, 4)),
    sprintf("%10.4f", sas_round(fail, 4)),
    sprintf("%10.4f", sas_round(km_out$STDERR[r], 4)),
    ifelse(is.na(km_out$LOWER[r]), "      .   ", sprintf("%10.4f", sas_round(km_out$LOWER[r], 4))),
    ifelse(is.na(km_out$UPPER[r]), "      .   ", sprintf("%10.4f", sas_round(km_out$UPPER[r], 4))),
    sprintf("%10d", 50)))
}
cat(paste(rep("-", 120), collapse = ""), "\n")
cat("Censoring: CNSR=1 censored, CNSR=0 event. SE by Greenwood; CI log-log.\n")
cat("Ticks '+' on curve denote censored times.\n")
sink()
cat("Text rendering saved to", txtfile, "\n")
txt <- paste(readLines(txtfile), collapse = " ")
nums <- regmatches(txt, gregexpr("[0-9]+\\.[0-9]+|[0-9]+", txt))[[1]]
cat("Number count in txt:", length(nums), "\n")
```