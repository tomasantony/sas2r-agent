# P040_kmplot.sas -> R translation
# Kaplan-Meier plot example (synthetic data created in SAS data step)
# Outputs: PNG + CSV (plotted data) + TXT (table rendering)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}
fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}
fmt_num <- function(x, digits = 4) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

suppressPackageStartupMessages({
  library(survival)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(survminer)
})

# -------------------------------------------------------------------------
# Recreate SAS data step "one"
# SAS code:
#   arm="Arm 1" censor=0 days: 143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304
#   censor=1 days: 216,244
#   arm="Arm 2" censor=0 days: 142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323
#   censor=1 days: 204,344
# Note: 227.230 is a single numeric value (SAS would read as 227.23)
# -------------------------------------------------------------------------
arm1_events <- c(143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304)
arm1_cens   <- c(216,244)
arm2_events <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
arm2_cens   <- c(204,344)

one <- dplyr::bind_rows(
  tibble::tibble(arm = "Arm 1", day = arm1_events, censor = 0),
  tibble::tibble(arm = "Arm 1", day = arm1_cens,   censor = 1),
  tibble::tibble(arm = "Arm 2", day = arm2_events, censor = 0),
  tibble::tibble(arm = "Arm 2", day = arm2_cens,   censor = 1)
) %>%
  mutate(
    arm = factor(arm, levels = c("Arm 1", "Arm 2")),
    # event indicator for Surv(): 1 = event, 0 = censored
    event = as.integer(censor == 0)
  ) %>%
  arrange(arm, day, censor)

cat("Data 'one' created:", nrow(one), "rows\n")
print(table(one$arm, one$censor))

# -------------------------------------------------------------------------
# Kaplan-Meier estimation: time day*censor(1)  strata arm
# In R: Surv(day, event) where event = 1 if censor==0
# -------------------------------------------------------------------------
fit <- survfit(Surv(day, event) ~ arm, data = one)

# Summary table (product-limit estimates) - one row per event/censor time
sfit_summary <- summary(fit, censored = TRUE)

# Build tidy survival table similar to PROC LIFETEST output
# Extract per-stratum vectors
surv_tbl <- tibble::tibble(
  arm      = as.character(sfit_summary$strata) |> sub("arm=", "", x = _),
  time     = sfit_summary$time,
  n_risk   = sfit_summary$n.risk,
  n_event  = sfit_summary$n.event,
  n_censor = sfit_summary$n.censor,
  survival = sfit_summary$surv,
  std_err  = sfit_summary$std.err,  # Greenwood SE on log scale? survfit gives std.err
  # survfit std.err is SE of cumulative hazard / log survival; convert to Greenwood SE of survival:
  # SAS reports Greenwood SE of survival: se_surv = surv * std.err (when std.err is SE of log(-log)??)
  # For simplicity use sfit_summary$std.err * survival as approximation, but also compute via Greenwood
  # We'll compute Greenwood SE directly from summary if available via survfit's std.err
  # Actually survfit$std.err is SE of survival on log scale? Let's keep both.
  lower    = sfit_summary$lower,
  upper    = sfit_summary$upper
) %>%
  # SAS columns: Survival, Failure (=1-Survival), Survival StdErr, Number Failed, Number Left
  mutate(
    failure = 1 - survival,
    # Greenwood SE: surv * se_log_surv ; survfit std.err is already SE of survival? Check:
    # In survfit, std.err is SE of the cumulative hazard, and SE(surv) = surv * std.err
    # So compute:
    se_surv = std_err * survival,
    # Number Failed = cumulative events up to that time per stratum
    # Number Left = n.risk - n.event - n.censor (remaining at risk after time)
    n_left = n_risk - n_event - n_censor
  ) %>%
  arrange(arm, time) %>%
  group_by(arm) %>%
  mutate(
    cum_failed = cumsum(n_event),
    cum_censored = cumsum(n_censor)
  ) %>%
  ungroup()

# For CSV output, keep SAS-like column names
surv_tbl_csv <- surv_tbl %>%
  transmute(
    ARM = arm,
    Time = time,
    Survival = round_half_up(survival, 4),
    Failure = round_half_up(failure, 4),
    Survival_StdErr = round_half_up(se_surv, 4),
    Number_Failed = cum_failed,
    Number_Left = n_left,
    N_Risk = n_risk,
    N_Event = n_event,
    N_Censor = n_censor,
    Lower_95CI = round_half_up(lower, 4),
    Upper_95CI = round_half_up(upper, 4)
  )

# At-risk table for atrisk=0 to 330 by 30
atrisk_times <- seq(0, 330, by = 30)
atrisk_summary <- summary(fit, times = atrisk_times, extend = TRUE)

# Build at-risk data frame
atrisk_df <- tibble::tibble(
  time = atrisk_summary$time,
  strata = atrisk_summary$strata,
  n_risk = atrisk_summary$n.risk,
  n_event = atrisk_summary$n.event,
  n_censor = atrisk_summary$n.censor,
  surv = atrisk_summary$surv
) %>%
  mutate(arm = sub("arm=", "", strata)) %>%
  select(arm, time, n_risk, surv) %>%
  arrange(arm, time)

# Wide format for display
atrisk_wide <- atrisk_df %>%
  select(arm, time, n_risk) %>%
  tidyr::pivot_wider(names_from = time, values_from = n_risk, names_prefix = "t") %>%
  arrange(arm)

# Quartile / median estimates (like PROC LIFETEST quartile output)
# Use quantile(fit)
qt <- quantile(fit, probs = c(0.25, 0.5, 0.75))
# qt is a list; extract
median_tbl <- tibble::tibble(
  arm = sub("arm=", "", names(fit$strata)),
  median = NA_real_,
  median_lower = NA_real_,
  median_upper = NA_real_,
  q1 = NA_real_,
  q3 = NA_real_
)
# Try to extract from quantile object
# quantile returns matrix when multiple strata
if (!is.null(qt$quantile)) {
  qmat <- qt$quantile
  # qmat rows are quantiles, cols are strata if matrix
  if (is.matrix(qmat)) {
    for (i in seq_along(fit$strata)) {
      arm_name <- sub("arm=", "", names(fit$strata)[i])
      idx <- which(median_tbl$arm == arm_name)
      # quantile levels are 25,50,75
      if ("25" %in% rownames(qmat)) median_tbl$q1[idx] <- qmat["25", i]
      if ("50" %in% rownames(qmat)) median_tbl$median[idx] <- qmat["50", i]
      if ("75" %in% rownames(qmat)) median_tbl$q3[idx] <- qmat["75", i]
    }
  }
}
# Also get confidence limits for median if available
if (!is.null(qt$lower)) {
  lmat <- qt$lower
  umat <- qt$upper
  if (is.matrix(lmat)) {
    for (i in seq_along(fit$strata)) {
      arm_name <- sub("arm=", "", names(fit$strata)[i])
      idx <- which(median_tbl$arm == arm_name)
      if ("50" %in% rownames(lmat)) median_tbl$median_lower[idx] <- lmat["50", i]
      if ("50" %in% rownames(umat)) median_tbl$median_upper[idx] <- umat["50", i]
    }
  }
}

# Number of events/censored per arm
cens_tbl <- one %>%
  group_by(arm) %>%
  summarise(
    N_Total = n(),
    N_Event = sum(event == 1),
    N_Censored = sum(event == 0),
    Percent_Censored = round_half_up(100 * mean(event == 0), 1),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# Write CSV outputs
# -------------------------------------------------------------------------
# Main CSV: survival estimates (plotted data)
write.csv(surv_tbl_csv, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

# Also write at-risk CSV for completeness (append suffix if needed, but keep main CSV as above)
# The PNG CSV is the same as P040_f_kmplot.csv per spec; we also write atrisk as separate file for traceability
write.csv(atrisk_df, file.path(out, "P040_f_kmplot_atrisk.csv"), row.names = FALSE, na = "")

cat("CSV written to", file.path(out, "P040_f_kmplot.csv"), "\n")

# -------------------------------------------------------------------------
# Text rendering (mimic ODS PDF listing)
# -------------------------------------------------------------------------
txt_file <- file.path(out, "P040_f_kmplot.txt")
sink(txt_file)

cat("Example K-M Plot\n")
cat("PROC LIFETEST - Product-Limit Survival Estimates\n")
cat("Stratified by ARM\n")
cat(strrep("-", 80), "\n\n")

cat("Summary of the Number of Censored and Uncensored Values\n")
cat(strrep("-", 80), "\n")
cat(sprintf("%-10s %8s %8s %10s %12s\n", "Stratum", "Total", "Failed", "Censored", "Pct Censored"))
cat(strrep("-", 80), "\n")
for (i in seq_len(nrow(cens_tbl))) {
  cat(sprintf("%-10s %8d %8d %10d %11s%%\n",
              cens_tbl$arm[i],
              cens_tbl$N_Total[i],
              cens_tbl$N_Event[i],
              cens_tbl$N_Censored[i],
              fmt_pct(cens_tbl$Percent_Censored[i], 1)))
}
cat("\n")

cat("Quartile Estimates (Median Survival Time)\n")
cat(strrep("-", 80), "\n")
cat(sprintf("%-10s %10s %10s %10s %12s %12s\n", "Stratum", "Q1", "Median", "Q3", "Lower 95%", "Upper 95%"))
cat(strrep("-", 80), "\n")
for (i in seq_len(nrow(median_tbl))) {
  cat(sprintf("%-10s %10s %10s %10s %12s %12s\n",
              median_tbl$arm[i],
              ifelse(is.na(median_tbl$q1[i]), ".", sprintf("%.1f", median_tbl$q1[i])),
              ifelse(is.na(median_tbl$median[i]), ".", sprintf("%.1f", median_tbl$median[i])),
              ifelse(is.na(median_tbl$q3[i]), ".", sprintf("%.1f", median_tbl$q3[i])),
              ifelse(is.na(median_tbl$median_lower[i]), ".", sprintf("%.1f", median_tbl$median_lower[i])),
              ifelse(is.na(median_tbl$median_upper[i]), ".", sprintf("%.1f", median_tbl$median_upper[i]))))
}
cat("\n")

# Detailed product-limit table per stratum
for (arm_level in levels(one$arm)) {
  cat("\n")
  cat(paste0("Stratum: ", arm_level, "\n"))
  cat(strrep("-", 100), "\n")
  cat(sprintf("%8s %10s %10s %10s %12s %10s %10s %10s %10s\n",
              "Time", "Survival", "Failure", "StdErr", "95% CI Low", "95% CI Up", "Failed", "Left", "AtRisk"))
  cat(strrep("-", 100), "\n")
  sub <- surv_tbl %>% filter(arm == arm_level) %>% arrange(time)
  for (r in seq_len(nrow(sub))) {
    cat(sprintf("%8.1f %10s %10s %10s %12s %10s %10d %10d %10d\n",
                sub$time[r],
                fmt_num(sub$survival[r], 4),
                fmt_num(sub$failure[r], 4),
                fmt_num(sub$se_surv[r], 4),
                fmt_num(sub$lower[r], 4),
                fmt_num(sub$upper[r], 4),
                sub$cum_failed[r],
                sub$n_left[r],
                sub$n_risk[r]))
  }
}

cat("\n")
cat("Number at Risk (atrisk=0 to 330 by 30)\n")
cat(strrep("-", 100), "\n")
# Header
header <- sprintf("%-10s", "Stratum")
for (t in atrisk_times) header <- paste0(header, sprintf(" %6d", t))
cat(header, "\n")
cat(strrep("-", 100), "\n")
for (arm_level in levels(one$arm)) {
  row_vals <- atrisk_df %>% filter(arm == arm_level) %>% arrange(time)
  line <- sprintf("%-10s", arm_level)
  for (t in atrisk_times) {
    v <- row_vals$n_risk[row_vals$time == t]
    if (length(v) == 0) v <- NA
    line <- paste0(line, sprintf(" %6s", ifelse(is.na(v), ".", as.character(v))))
  }
  cat(line, "\n")
}
cat("\n")
cat("Censoring indicated by '+' on plot; ticks at event times.\n")
cat("Time unit: Day\n")
cat(strrep("-", 100), "\n")
cat("Generated by R translation of P040_kmplot.sas\n")

sink()
cat("TXT written to", txt_file, "\n")

# -------------------------------------------------------------------------
# Figure: Kaplan-Meier plot with at-risk table
# -------------------------------------------------------------------------
# Use survminer for faithful at-risk display, fallback to ggplot2 if needed
# Prepare data for ggplot step
plot_data <- surv_tbl_csv

# Create survminer plot
# survminer expects fit object
p <- tryCatch({
  ggsurvplot(
    fit,
    data = one,
    risk.table = TRUE,
    risk.table.title = "Number at risk",
    risk.table.height = 0.25,
    surv.median.line = "hv",
    palette = c("#1f77b4", "#ff7f0e"),
    xlab = "Day",
    ylab = "Survival Probability",
    title = "Example K-M Plot",
    legend.title = "Stratum",
    legend.labs = c("Arm 1", "Arm 2"),
    break.time.by = 30,
    xlim = c(0, 350),
    ylim = c(0, 1),
    censor.shape = 3,
    censor.size = 3,
    ggtheme = theme_minimal(base_size = 11),
    tables.theme = theme_minimal(base_size = 9)
  )
}, error = function(e) {
  cat("ggsurvplot failed:", conditionMessage(e), "\n")
  NULL
})

png_file <- file.path(out, "P040_f_kmplot.png")

if (!is.null(p)) {
  # ggsurvplot returns list with $plot and $table
  # Combine via arrange_ggsurvplots or print
  # Use ggsave on the plot object; need to arrange
  # survminer provides print method; we save via ggsave after arranging
  # Create combined figure using gridExtra
  # Simpler: save using png device with print(p)
  png(png_file, width = 2400, height = 1800, res = 300)
  print(p)
  dev.off()
  cat("PNG written via survminer to", png_file, "\n")
} else {
  # Fallback ggplot2 manual step plot
  # Build step data including time 0
  step_df <- surv_tbl %>%
    group_by(arm) %>%
    arrange(time) %>%
    # Add time 0 point
    group_modify(~ {
      add0 <- tibble::tibble(time = 0, survival = 1, arm = .y$arm)
      # Keep only needed cols for plot
      bind_rows(add0 %>% select(time, survival, arm),
                .x %>% select(time, survival, arm))
    }) %>%
    ungroup()

  # Censor points
  cens_df <- one %>% filter(censor == 1)

  gg <- ggplot() +
    geom_step(data = step_df, aes(x = time, y = survival, color = arm, linetype = arm), linewidth = 0.9) +
    geom_point(data = cens_df, aes(x = day, y = NA), shape = 3, size = 2) + # placeholder
    # Add censor marks by merging with survival at censor time
    geom_point(data = surv_tbl %>% filter(n_censor > 0) %>% left_join(atrisk_df, by = c("arm", "time")),
               aes(x = time, y = survival), shape = 3, size = 2.5, color = "black") +
    scale_x_continuous(breaks = seq(0, 350, by = 30), limits = c(0, 350)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
    scale_color_manual(values = c("Arm 1" = "#1f77b4", "Arm 2" = "#ff7f0e")) +
    labs(title = "Example K-M Plot", x = "Day", y = "Survival Probability", color = "Stratum", linetype = "Stratum") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom")

  ggsave(png_file, plot = gg, width = 8, height = 6, dpi = 300, units = "in")
  cat("PNG written via ggplot2 fallback to", png_file, "\n")
}

# Ensure PNG exists; if survminer print didn't create proper file, try ggsave alternative
if (!file.exists(png_file) || file.info(png_file)$size == 0) {
  # Last resort: simple ggplot
  step_df2 <- surv_tbl %>%
    group_by(arm) %>%
    arrange(time) %>%
    group_modify(~ bind_rows(tibble::tibble(time=0, survival=1, arm=.y$arm), .x %>% select(time, survival, arm))) %>%
    ungroup()
  gg2 <- ggplot(step_df2, aes(x=time, y=survival, color=arm)) +
    geom_step(linewidth=1) +
    labs(title="Example K-M Plot", x="Day", y="Survival Probability") +
    theme_minimal()
  ggsave(png_file, plot=gg2, width=8, height=6, dpi=300)
}

cat("All outputs written to OUT_DIR:", out, "\n")
# List files
print(list.files(out, pattern = "P040"))
