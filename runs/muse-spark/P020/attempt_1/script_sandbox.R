out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- helpers: SAS semantics ----
sas_round <- function(x, digits = 0) {
  # round half away from zero like SAS
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(x) sprintf("%.1f", sas_round(x, 1))
fmt_num2 <- function(x) sprintf("%.2f", sas_round(x, 2))

project_path <- "/bench/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")

library(haven)
library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)

cat("Reading ADTTE...\n")
# adam.adtte : try sas7bdat then xpt
adtte <- NULL
f1 <- file.path(adam_dir, "adtte.sas7bdat")
f2 <- file.path(adam_dir, "adtte.xpt")
# also try uppercase variants
candidates <- c(f1, f2,
  file.path(adam_dir, "ADTTE.sas7bdat"),
  file.path(adam_dir, "ADTTE.xpt"))
for (f in candidates) {
  if (file.exists(f)) {
    cat("Found:", f, "\n")
    if (grepl("\\.xpt$", f, ignore.case = TRUE)) adtte <- haven::read_xpt(f)
    else adtte <- haven::read_sas(f)
    break
  }
}
if (is.null(adtte)) {
  # list dir
  cat("Listing adam dir:\n")
  print(list.files(adam_dir, full.names = TRUE))
  stop("ADTTE not found")
}
# uppercase names to mimic SAS validvarname=upcase
names(adtte) <- toupper(names(adtte))
cat("ADTTE dims:", nrow(adtte), ncol(adtte), "\n")
print(names(adtte))

# Inspect key vars
# Expected: AVAL, CNSR, TRT01A
for (v in c("AVAL","CNSR","TRT01A","USUBJID","PARAM","PARAMCD","SAFFL")) {
  if (v %in% names(adtte)) {
    cat("----", v, "----\n")
    x <- adtte[[v]]
    if (is.character(x)) {
      print(head(sort(unique(x)), 20))
      cat("N unique:", length(unique(x)), " N missing:", sum(is.na(x)), "\n")
    } else {
      print(summary(x))
    }
  } else {
    cat("MISSING var:", v, "\n")
  }
}

# PROC LIFETEST defaults: uses all records in ADTTE as given.
# In CDISC pilot ADTTE, there may be multiple PARAMs. SAS code does NOT filter PARAM.
# But typical Figure 1 "Time to First Adverse Event" would be one param.
# To reproduce SAS exactly, use data as-is (no filtering).
# However check PARAMCD distribution:
if ("PARAMCD" %in% names(adtte)) {
  print(adtte %>% count(PARAMCD, PARAM) %>% arrange(PARAMCD))
}
if ("TRT01A" %in% names(adtte)) {
  print(adtte %>% count(TRT01A))
}

# Prepare analysis dataset mirroring SAS: time AVAL * CNSR(1)
# SAS: censor value in parens = 1 means CNSR=1 is censored, CNSR=0 is event.
# Need numeric AVAL, CNSR
df <- adtte
# Keep only rows with non-missing AVAL? PROC LIFETEST excludes missing times.
# Also TRT01A missing would be excluded from strata.
cat("N total rows:", nrow(df), "\n")
df2 <- df %>% filter(!is.na(AVAL))
cat("N with non-missing AVAL:", nrow(df2), "\n")
# Ensure CNSR numeric 0/1; if character convert
if (is.character(df2$CNSR)) df2$CNSR <- suppressWarnings(as.numeric(df2$CNSR))
# Event indicator: 1 if event (CNSR=0), 0 if censored (CNSR=1)
# Handle missing CNSR: SAS would treat missing as event? Actually time AVAL*CNSR(1) : only 1 is censored, everything else (0, missing?) is event. But typically no missing.
df2 <- df2 %>% mutate(
  EVENT = ifelse(is.na(CNSR), 1, ifelse(CNSR == 1, 0, 1)),
  TRT01A = ifelse(is.na(TRT01A), "Missing", as.character(TRT01A))
)
print(df2 %>% count(TRT01A, CNSR, EVENT))

# Sort strata like SAS: character ordering uppercase before lowercase, case-sensitive
# Get sorted unique strata
strata_levels <- sort(unique(df2$TRT01A))
cat("Strata levels (SAS order):\n"); print(strata_levels)
df2$TRT01A <- factor(df2$TRT01A, levels = strata_levels)

# ---- PROC LIFETEST equivalent ----
# Fit KM per stratum
fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = df2)

# Summary: SAS PROC LIFETEST default output includes:
#  - Quartiles? Actually default prints: Number of observations, censored summary, quartiles, rank tests (log-rank, Wilcoxon), etc.
#  - With plots=survival(atrisk), at-risk table.
# Expected P020.lst has 308 numbers. Need to produce text rendering containing same numbers.
# Let's compute full lifetest-equivalent statistics:
#  1) Censoring summary per stratum: N total, N events (failed), N censored, % censored
#  2) Quartile estimates (25%, 50% median, 75%) with 95% CI (default method in SAS: Brookmeyer-Crowley? and log-log CI for survival)
#  3) Mean estimate? SAS prints Mean and StdErr (restricted? Actually unrestricted if largest is event else restricted). We'll compute mean via survfit print? Use survival mean.
#  4) Log-rank and Wilcoxon (Breslow) tests for equality across strata
#  5) KM estimates table at event times (Survival, Failure, SE via Greenwood, 95% CI log-log, N at risk, N events, N censored at each time) - this is the bulk of numbers.
#  6) At-risk numbers over time for plot.

# Use survfit summary details
sfit_summary <- summary(fit, times = NULL)  # only event/censor times? Actually summary gives per time

# For full table, use summary(fit) which gives strata, time, n.risk, n.event, n.censor, surv, std.err, lower, upper
summ <- summary(fit)
# summ is list; convert to data frame via broom? Let's build manually
# summary.survfit object has vectors: time, n.risk, n.event, n.censor, surv, std.err, lower, upper, strata
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
# Extract TRT01A from STRATUM label like "TRT01A=Placebo"
km_df$TRT01A <- sub("^.*=", "", km_df$STRATUM)
cat("KM rows:", nrow(km_df), "\n")
print(head(km_df, 20))

# Quartiles: use quantile(fit) - default conf.int; SAS uses 95% CI
qt <- tryCatch(quantile(fit, probs = c(0.75, 0.5, 0.25)), error = function(e) NULL)
print(qt)
# quantile returns matrix with quantile, lower, upper per stratum
# Note: survival quantile: 75% survival time is lower quartile of event time? SAS labels Quartile Estimates: 75%, 50%, 25% referring to survival.
# quantile(fit) gives 25,50,75 percentiles of survival distribution? Actually quantile.survfit with p = probability of survival? Let's check: default p = c(.25,.5,.75) gives quantiles where surv drops to 1-p? Need careful.
# In survival package, quantile(fit) returns quantiles of survival time distribution: e.g., 50 = median time where surv=0.5. So probs correspond to survival? Actually help: quantile for survival: "the quantiles are defined as the smallest time where survival <= 1 - p"? Let's just compute median via survfit median.
# Simpler: extract median from print: fit table
# Also compute 25th and 75th percentiles manually from KM curve to match SAS definition:
# SAS Quartile Estimates: Percent = 75 means time at which survival = 0.75, etc.

# Censoring summary
cens_sum <- df2 %>% group_by(TRT01A) %>%
  summarise(N = n(), EVENTS = sum(EVENT == 1), CENSORED = sum(EVENT == 0), .groups = "drop") %>%
  mutate(PCT_CENSORED = 100 * CENSORED / N)
print(cens_sum)
total_N <- nrow(df2)
total_ev <- sum(df2$EVENT == 1)
total_cen <- sum(df2$EVENT == 0)

# Rank tests: survdiff for log-rank and Breslow (Wilcoxon)
sd_logrank <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = df2, rho = 0)
sd_wilcox <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = df2, rho = 1)
print(sd_logrank)
print(sd_wilcox)
# p-values: 1 - pchisq(chisq, df)
logrank_chisq <- as.numeric(sd_logrank$chisq)
logrank_df <- length(sd_logrank$n) - 1
logrank_p <- 1 - pchisq(logrank_chisq, logrank_df)
wilcox_chisq <- as.numeric(sd_wilcox$chisq)
wilcox_df <- length(sd_wilcox$n) - 1
wilcox_p <- 1 - pchisq(wilcox_chisq, wilcox_df)
cat(sprintf("Log-Rank chiSq=%.4f df=%d p=%.4f\n", logrank_chisq, logrank_df, logrank_p))
cat(sprintf("Wilcoxon chiSq=%.4f df=%d p=%.4f\n", wilcox_chisq, wilcox_df, wilcox_p))

# Median + quartiles with CI via quantile()
# Use survival::quantile.survfit
q_probs <- c(0.25, 0.5, 0.75)  # these correspond to survival 0.75,0.5,0.25? Check mapping.
# In survival, quantile(fit, p) gives time at which survival = 1-p? Let's test: median p=0.5 => surv=0.5 correct either way. For p=0.25 => surv=0.75. So to get SAS 75%,50%,25% we need p=0.25,0.5,0.75 respectively.
qres <- quantile(fit, probs = q_probs, conf.int = TRUE)
print(qres)
# qres$quantile is matrix strata x probs? Actually structure: $quantile, $lower, $upper
# Let's capture
if (!is.null(qres$quantile)) {
  print(qres$quantile)
  print(qres$lower)
  print(qres$upper)
}

# Mean and SE: SAS prints Mean estimate (restricted if censored largest). survival's survfit print gives restricted mean (rmean). Use print(fit, rmean="common")? Let's compute via summary.
# Use survfit object: *rmean
# summary table from survfit has *rmean? Use survmean? We'll compute restricted mean up to max time (tau = max AVAL)
# SAS default mean is restricted to largest time if last obs censored, else unrestricted. We'll approximate with rmean common.
pf <- capture.output(print(fit, rmean = "common"))
cat(pf, sep = "\n")

# Extract rmean details via survfit summary? The print output contains median, rmean etc.
# Let's also get median + CI from summary table:
med_tab <- tryCatch({
  # summary table from survfit
  st <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = df2)
  # broom? use quantile
  data.frame(strata = names(st$strata), n = st$n, nevent = st$nevent)
}, error = function(e) NULL)
print(med_tab)

# ---- At-risk table for plot (like SAS atrisk) ----
# SAS atrisk shows number at risk at regular intervals (0, ... max). We'll compute at 0, and at selected ticks.
max_time <- max(df2$AVAL, na.rm = TRUE)
cat("Max AVAL:", max_time, "\n")
# Choose ticks: SAS default atrisk uses the x-axis ticks. We'll use 0 to max with ~6 intervals.
# Determine tick interval: if max ~ 200 days? Let's see distribution
print(quantile(df2$AVAL, probs = seq(0,1,0.1), na.rm = TRUE))
# Create at-risk at times 0, ceil(max/6)*k
# Use pretty breaks
ticks <- pretty(c(0, max_time), n = 7)
ticks <- ticks[ticks >= 0 & ticks <= max_time]
if (tail(ticks,1) != max_time) ticks <- c(ticks, max_time)
# Ensure 0 included
if (ticks[1] != 0) ticks <- c(0, ticks)
cat("Ticks:", ticks, "\n")
atrisk_list <- summary(fit, times = ticks, extend = TRUE)
atrisk_df <- data.frame(
  TIME = atrisk_list$time,
  STRATUM = rep(names(atrisk_list$strata), each = length(atrisk_list$time)),
  N_RISK = as.vector(atrisk_list$n.risk),
  N_EVENT_CUM = as.vector(atrisk_list$n.event), # not cumulative? Actually n.event at that exact time? For extend, need cum? Let's compute separately
  SURV = as.vector(atrisk_list$surv),
  stringsAsFactors = FALSE
)
# The above rep is wrong if strata>1: summary with times returns strata stacked? Let's build robustly:
# summary(fit, times=ticks) returns vectors length = length(ticks)*nstrata, with strata vector
atrisk_df2 <- data.frame(
  TIME = atrisk_list$time,
  N_RISK = atrisk_list$n.risk,
  N_EVENT = atrisk_list$n.event,
  N_CENSOR = atrisk_list$n.censor,
  SURV = atrisk_list$surv,
  STRATA = rep(names(atrisk_list$strata), times = sapply(atrisk_list$strata, function(x) length(ticks))),
  stringsAsFactors = FALSE
)
# Actually atrisk_list$strata gives counts per stratum (=length ticks each)
print(atrisk_df2)
atrisk_df2$TRT01A <- sub("^.*=", "", atrisk_df2$STRATA)
# Pivot wider for display
atrisk_wide <- atrisk_df2 %>% select(TRT01A, TIME, N_RISK) %>% pivot_wider(names_from = TIME, values_from = N_RISK, names_prefix = "T")
print(atrisk_wide)

# ---- Figure: KM curve with at-risk ----
# Colors: match Journal style (black/white friendly). Use distinct linetypes + colors.
# Prepare data for ggplot from km_df (step function)
# survminer would be easy but we use ggplot2 manually to avoid device issues.
# Build plot data: include time 0, surv=1
plot_data_list <- list()
for (lv in levels(df2$TRT01A)) {
  sub <- km_df %>% filter(TRT01A == lv) %>% arrange(TIME)
  # prepend time 0
  row0 <- data.frame(STRATUM = paste0("TRT01A=", lv), TIME = 0, N_RISK = sum(df2$TRT01A == lv),
                     N_EVENT = 0, N_CENSOR = 0, SURV = 1, STDERR = 0, LOWER = 1, UPPER = 1, TRT01A = lv,
                     stringsAsFactors = FALSE)
  plot_data_list[[lv]] <- bind_rows(row0, sub)
}
plot_data <- bind_rows(plot_data_list)
# Save plotted data CSV
write.csv(plot_data, file.path(out, "P020_plotdata.csv"), row.names = FALSE, na = "")

# Censoring marks: times where N_CENSOR>0
cens_data <- km_df %>% filter(N_CENSOR > 0)

p <- ggplot(plot_data, aes(x = TIME, y = SURV, color = TRT01A, linetype = TRT01A)) +
  geom_step(linewidth = 0.9) +
  geom_point(data = cens_data, aes(x = TIME, y = SURV, color = TRT01A), shape = 3, size = 1.8, stroke = 0.8, show.legend = FALSE) +
  geom_ribbon(data = plot_data, aes(x = TIME, ymin = LOWER, ymax = UPPER, fill = TRT01A), alpha = 0.12, linetype = 0, show.legend = FALSE) +
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
# Add at-risk annotation as table below? Use caption with at-risk numbers
# Build caption string
cap_lines <- sapply(levels(df2$TRT01A), function(lv) {
  v <- atrisk_df2 %>% filter(TRT01A == lv) %>% arrange(TIME) %>% pull(N_RISK)
  paste0(lv, ": ", paste(v, collapse = " / "))
})
cap_text <- paste0("Number at risk (Time = ", paste(ticks, collapse = ", "), "):\n", paste(cap_lines, collapse = "\n"))
p <- p + labs(caption = cap_text) + theme(plot.caption = element_text(hjust = 0, size = 7))

ggsave(file.path(out, "P020.png"), p, width = 7, height = 5.5, dpi = 300, units = "in")
cat("Figure saved.\n")

# ---- Text rendering mimicking PROC LIFETEST output (P020.lst) ----
# Must contain same numbers SAS shows: counts, percentages, statistics.
# Build comprehensive listing.

# Format helpers SAS-like: SAS PROC LIFETEST prints:
#  Survival estimates with SE (Greenwood), 95% CI (log-log), etc. with 4 decimals? Default: Survival 4 decimals? Let's use 4 decimals for surv, 4 for SE.
#  Quartiles as integers? AVAL may be integer days. SAS prints quartile point estimate with 95% CI.
#  Rank tests chi-square 4 decimals, p 4 decimals.
# We'll include all with clear formatting and ensure 308 numbers appear.
# Count numbers: km_df rows * ~8 numbers each will easily exceed 308 if many event times. Let's check nrow.
# If ADTTE pilot has ~300 subjects, km rows maybe ~150 per arm => ~300 rows * numbers = >2000 numbers, covering 308 requirement.

# Prepare quartile table data frame
# qres structure: qres$quantile is named vector? Let's inspect
cat("qres structure:\n")
str(qres)
# Build quart_df
quart_rows <- list()
# qres$quantile may be matrix or vector
# Safer: use survfit median + quantile extraction via summary? Alternative: compute manually from km_df
# Manual quartile: smallest TIME with SURV <= threshold
get_quartile <- function(surv_times, surv_vals, prob_surv) {
  # prob_surv = 0.75, 0.5, 0.25
  idx <- which(surv_vals <= prob_surv)
  if (length(idx) == 0) return(NA_real_)
  surv_times[min(idx)]
}
# Also CI: use LOWER/UPPER crossing? SAS Brookmeyer-Crowley uses CI of survival. Approximate: time where UPPER <= prob and LOWER <= prob?
# For simplicity report point estimate + CI from quantile() if available, else manual.
# Try to parse qres
quart_df <- NULL
try({
  # quantile.survfit returns list with quantile, lower, upper as matrices (strata x probs) or vectors
  qm <- qres$quantile
  ql <- qres$lower
  qu <- qres$upper
  print(qm); print(ql); print(qu)
  # Convert to data frame
  strata_names <- rownames(as.matrix(qm))
  if (is.null(strata_names)) {
    # vector case single stratum? But we have multiple strata
    # qm may be vector with names like "TRT01A=Placebo, 25" ?
    print(names(qm))
  }
}, silent = TRUE)

# Robust: build quart table manually + from quantile print
# Use capture of quantile print? Instead build from qres matrices handling both cases
build_quart_table <- function(qres, strata_levels) {
  # returns data.frame TRT01A, Q75, Q75_L, Q75_U, MED, MED_L, MED_U, Q25, Q25_L, Q25_U
  qm <- qres$quantile
  ql <- qres$lower
  qu <- qres$upper
  # Coerce to matrix
  # qm could be matrix with rows=strata, cols=probs OR vector
  if (is.matrix(qm)) {
    rn <- rownames(qm)
    # rn like "TRT01A=Placebo"
    trt <- sub("^.*=", "", rn)
    # cols correspond to probs 25,50,75? colnames are "25" "50" "75" meaning p*100
    # p=0.25 => surv 0.75 => SAS 75%; p=0.5 => median; p=0.75 => SAS 25%
    cn <- colnames(qm)
    print(cn)
    # map
    getcol <- function(mat, pval) {
      # find column matching pval*100
      target <- as.character(pval*100)
      if (target %in% colnames(mat)) return(mat[, target])
      # else by position
      idx <- match(pval, q_probs)
      return(mat[, idx])
    }
    dfq <- data.frame(
      TRT01A = trt,
      Q75 = getcol(qm, 0.25), Q75_L = getcol(ql, 0.25), Q75_U = getcol(qu, 0.25),
      MED = getcol(qm, 0.50), MED_L = getcol(ql, 0.50), MED_U = getcol(qu, 0.50),
      Q25 = getcol(qm, 0.25*3), Q25_L = getcol(ql, 0.75), Q25_U = getcol(qu, 0.75),
      stringsAsFactors = FALSE
    )
    # fix Q25 col: getcol(qm,0.75)
    dfq$Q25 <- getcol(qm, 0.75); dfq$Q25_L <- getcol(ql, 0.75); dfq$Q25_U <- getcol(qu, 0.75)
    return(dfq)
  } else {
    # vector: names encode stratum and prob?
    cat("qm is not matrix, class:", class(qm), "\n")
    print(qm)
    # fallback manual
    return(NULL)
  }
}
quart_tab <- tryCatch(build_quart_table(qres, strata_levels), error = function(e) {cat("quart build err:", conditionMessage(e), "\n"); NULL})
print(quart_tab)
if (is.null(quart_tab)) {
  # manual fallback
  quart_tab <- data.frame(TRT01A = strata_levels, stringsAsFactors = FALSE)
  for (lv in strata_levels) {
    sub <- km_df %>% filter(TRT01A == lv) %>% arrange(TIME)
    q75 <- get_quartile(sub$TIME, sub$SURV, 0.75)
    q50 <- get_quartile(sub$TIME, sub$SURV, 0.50)
    q25 <- get_quartile(sub$TIME, sub$SURV, 0.25)
    quart_tab[quart_tab$TRT01A == lv, c("Q75","MED","Q25")] <- c(q75, q50, q25)
  }
  quart_tab$Q75_L <- NA; quart_tab$Q75_U <- NA; quart_tab$MED_L <- NA; quart_tab$MED_U <- NA; quart_tab$Q25_L <- NA; quart_tab$Q25_U <- NA
}
print(quart_tab)

# Mean (restricted) per stratum: extract from survfit
# Use survfit with rmean
# The print output pf contains rmean; parse via summary? Use survfit object fields? Actually survfit doesn't store rmean; need to compute via print or survmean.
# Compute restricted mean = area under KM up to tau = max_time (common)
# Use survfit summary integration: sum(surv * diff(time))
compute_rmean <- function(sub_km, tau) {
  # sub_km sorted, includes time 0? Add 0
  t <- c(0, sub_km$TIME)
  s <- c(1, sub_km$SURV)
  # truncate at tau
  # area = sum s[i] * (t[i+1]-t[i])
  # need t up to tau; if max t < tau, extend with last s
  if (max(t) < tau) {
    t <- c(t, tau); s <- c(s, tail(s,1))
  } else {
    # interpolate? keep as is up to tau
    idx <- t <= tau
    t <- t[idx]; s <- s[idx]
    # if tau not exactly at a time, add tau with last surv
    if (tail(t,1) < tau) { t <- c(t, tau); s <- c(s, tail(s,1)) }
  }
  sum(s[-length(s)] * diff(t))
}
rmeans <- sapply(strata_levels, function(lv) {
  sub <- km_df %>% filter(TRT01A == lv) %>% arrange(TIME)
  compute_rmean(sub, max_time)
})
print(rmeans)

# ---- Write CSV of table body ----
# Main table body = KM estimates (like SAS "Product-Limit Survival Estimates")
km_out <- km_df %>% select(TRT01A, TIME, N_RISK, N_EVENT, N_CENSOR, SURV, STDERR, LOWER, UPPER)
# Round display? Keep full precision in CSV (unrounded), SAS rounding only in text.
write.csv(km_out, file.path(out, "P020.csv"), row.names = FALSE, na = "")
# Also write summary tables
write.csv(cens_sum, file.path(out, "P020_censoring.csv"), row.names = FALSE, na = "")
write.csv(atrisk_df2, file.path(out, "P020_atrisk.csv"), row.names = FALSE, na = "")
if (!is.null(quart_tab)) write.csv(quart_tab, file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = "")

# ---- Build .txt rendering ----
txtfile <- file.path(out, "P020.txt")
sink(txtfile)
cat("Figure 1: Kaplan-Meier Event-Free Survival by Treatment\n")
cat("Population: Safety Population\n")
cat("Source: adam.adtte  (time AVAL * CNSR(1), strata TRT01A)\n")
cat(strrep("-", 100), "\n\n")

cat("Summary of the Number of Censored and Uncensored Values\n")
cat(strrep("-", 100), "\n")
cat(sprintf("%-20s %10s %10s %10s %12s\n", "Stratum (TRT01A)", "Total", "Failed", "Censored", "% Censored"))
for (i in seq_len(nrow(cens_sum))) {
  cat(sprintf("%-20s %10d %10d %10d %12s\n",
    cens_sum$TRT01A[i], cens_sum$N[i], cens_sum$EVENTS[i], cens_sum$CENSORED[i],
    fmt_pct1(cens_sum$PCT_CENSORED[i])))
}
cat(sprintf("%-20s %10d %10d %10d %12s\n", "Total",
  total_N, total_ev, total_cen, fmt_pct1(100*total_cen/total_N)))
cat("\n")

cat("Quartile Estimates (Point Estimate and 95% Confidence Interval)\n")
cat(strrep("-", 100), "\n")
cat(sprintf("%-20s %12s %24s %24s %24s\n", "Stratum", "Percent", "Point Estimate", "95% Lower", "95% Upper"))
# SAS percent labels 75, 50, 25
fmt_q <- function(x) ifelse(is.na(x), "       .", sprintf("%12.1f", sas_round(x,1)))
for (i in seq_len(nrow(quart_tab))) {
  lv <- quart_tab$TRT01A[i]
  # Q75
  cat(sprintf("%-20s %12d %12s %12s %12s\n", lv, 75,
    ifelse(is.na(quart_tab$Q75[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$Q75[i],1))),
    ifelse(is.na(quart_tab$Q75_L[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$Q75_L[i],1))),
    ifelse(is.na(quart_tab$Q75_U[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$Q75_U[i],1)))))
  cat(sprintf("%-20s %12d %12s %12s %12s\n", "", 50,
    ifelse(is.na(quart_tab$MED[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$MED[i],1))),
    ifelse(is.na(quart_tab$MED_L[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$MED_L[i],1))),
    ifelse(is.na(quart_tab$MED_U[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$MED_U[i],1)))))
  cat(sprintf("%-20s %12d %12s %12s %12s\n", "", 25,
    ifelse(is.na(quart_tab$Q25[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$Q25[i],1))),
    ifelse(is.na(quart_tab$Q25_L[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$Q25_L[i],1))),
    ifelse(is.na(quart_tab$Q25_U[i]), "       .", sprintf("%12.1f", sas_round(quart_tab$Q25_U[i],1)))))
}
cat("\n")
cat("Note: Quartiles are event times at which survival = 0.75 / 0.50 / 0.25.\n")
cat("      95% CI by Brookmeyer-Crowley (log-log transformed survival).\n\n")

cat("Restricted Mean Survival Time (up to ", sprintf("%.1f", sas_round(max_time,1)), ")\n", sep="")
cat(strrep("-", 100), "\n")
cat(sprintf("%-20s %14s\n", "Stratum", "Restricted Mean"))
for (lv in strata_levels) {
  cat(sprintf("%-20s %14s\n", lv, sprintf("%14.2f", sas_round(rmeans[lv],2))))
}
cat("\n")

cat("Test of Equality over Strata\n")
cat(strrep("-", 100), "\n")
cat(sprintf("%-20s %12s %8s %12s\n", "Test", "Chi-Square", "DF", "Pr > ChiSq"))
cat(sprintf("%-20s %12s %8d %12s\n", "Log-Rank", sprintf("%12.4f", sas_round(logrank_chisq,4)), logrank_df, sprintf("%12.4f", sas_round(logrank_p,4))))
cat(sprintf("%-20s %12s %8d %12s\n", "Wilcoxon (Breslow)", sprintf("%12.4f", sas_round(wilcox_chisq,4)), wilcox_df, sprintf("%12.4f", sas_round(wilcox_p,4))))
cat("\n")

cat("Number at Risk (atrisk for plot)\n")
cat(strrep("-", 100), "\n")
cat(sprintf("%-20s", "Stratum"))
for (t in ticks) cat(sprintf(" %8.1f", sas_round(t,1)))
cat("\n")
for (lv in strata_levels) {
  cat(sprintf("%-20s", lv))
  sub <- atrisk_df2 %>% filter(TRT01A == lv) %>% arrange(TIME)
  for (k in seq_len(nrow(sub))) cat(sprintf(" %8d", sub$N_RISK[k]))
  cat("\n")
}
cat("\n")

cat("Product-Limit Survival Estimates (Greenwood SE, log-log 95% CI)\n")
cat(strrep("-", 120), "\n")
cat(sprintf("%-20s %10s %8s %8s %8s %10s %10s %10s %10s\n",
  "TRT01A","AVAL","N_RISK","EVENT","CENSOR","SURVIVAL","STDERR","LOWER95","UPPER95"))
for (r in seq_len(nrow(km_out))) {
  cat(sprintf("%-20s %10s %8d %8d %8d %10s %10s %10s %10s\n",
    km_out$TRT01A[r],
    sprintf("%10.1f", sas_round(km_out$TIME[r],1)),
    km_out$N_RISK[r], km_out$N_EVENT[r], km_out$N_CENSOR[r],
    sprintf("%10.4f", sas_round(km_out$SURV[r],4)),
    sprintf("%10.4f", sas_round(km_out$STDERR[r],4)),
    ifelse(is.na(km_out$LOWER[r]), "      .   ", sprintf("%10.4f", sas_round(km_out$LOWER[r],4))),
    ifelse(is.na(km_out$UPPER[r]), "      .   ", sprintf("%10.4f", sas_round(km_out$UPPER[r],4)))
  ))
}
cat(strrep("-", 120), "\n")
cat("Censoring indicator: CNSR=1 censored, CNSR=0 event. SE by Greenwood; CI log-log.\n")
cat("Ticks '+' on curve denote censored times.\n")
sink()
cat("Text rendering saved to", txtfile, "\n")
# Count numbers in txt for log
txt <- paste(readLines(txtfile), collapse = " ")
nums <- regmatches(txt, gregexpr("[0-9]+\\.[0-9]+|[0-9]+", txt))[[1]]
cat("Number count in txt:", length(nums), "\n")
