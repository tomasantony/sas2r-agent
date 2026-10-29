# P040_kmplot.sas translation to R
# Example K-M Plot - PROC LIFETEST plots=survival (atrisk=0 to 330 by 30)
# time day*censor(1); strata arm (SAS code has typo "srata arm" - intent is STRATA arm)
# Outputs: P040_f_kmplot.png + plotdata csv, P040_f_kmplot.txt + P040_f_kmplot.csv

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(survival)
  library(ggplot2)
  library(dplyr)
  library(grid)
})

cat("P040 start\n")

# SAS round half away from zero helper (for display formatting)
round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5 + 1e-10) / mult
}
fmt4 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.4f", round_half_up(x, 4)))
}
fmt2 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f", round_half_up(x, 2)))
}
fmt1 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.1f", round_half_up(x, 1)))
}

# ---- data one ; length arm $8 ; ----
arm1_events <- c(143,164,188,188,190,192,206,213,216,220,227.230,234,246,265,304)
arm1_cens   <- c(216,244)
arm2_events <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
arm2_cens   <- c(204,344)

one <- data.frame(
  arm    = c(rep("Arm 1", length(arm1_events) + length(arm1_cens)),
             rep("Arm 2", length(arm2_events) + length(arm2_cens))),
  day    = c(arm1_events, arm1_cens, arm2_events, arm2_cens),
  censor = c(rep(0, length(arm1_events)), rep(1, length(arm1_cens)),
             rep(0, length(arm2_events)), rep(1, length(arm2_cens))),
  stringsAsFactors = FALSE
)
one$event <- 1 - one$censor  # TIME day*censor(1): censor=1 is censored, event = 1-censor

# sort like SAS BY arm day (events before censored at same day for KM risk logic)
one <- one[order(one$arm, one$day, -one$event), , drop = FALSE]
rownames(one) <- NULL

# ---- PROC LIFETEST product-limit estimates (manual to match SAS) ----
compute_km <- function(d) {
  # d: data.frame for one arm with day, event (1=failed,0=censored)
  times <- sort(unique(d$day))
  n <- nrow(d)
  surv <- 1
  gsum <- 0
  out_list <- list()
  cum_failed <- 0
  for (t in times) {
    n_risk   <- sum(d$day >= t)
    n_event  <- sum(d$day == t & d$event == 1)
    n_censor <- sum(d$day == t & d$event == 0)
    cum_failed <- cum_failed + n_event
    if (n_risk > 0 && n_event > 0) {
      if (n_risk == n_event) {
        # if all at risk fail, survival goes to 0; Greenwood term undefined -> keep prior gsum
        surv <- surv * (1 - n_event / n_risk)
        # gsum not updated when n_risk - n_event == 0 (avoid div0), se = . when surv=0 handled below
      } else {
        surv <- surv * (1 - n_event / n_risk)
        gsum <- gsum + n_event / (n_risk * (n_risk - n_event))
      }
    } else {
      # censored-only time: survival unchanged
    }
    if (surv == 0) {
      se <- NA_real_
    } else {
      se <- surv * sqrt(gsum)
    }
    n_left <- n_risk - n_event - n_censor
    out_list[[length(out_list) + 1]] <- data.frame(
      DAY = t, SURVIVAL = surv, FAILURE = 1 - surv, STDERR = se,
      N_RISK = n_risk, N_EVENT = n_event, N_CENSOR = n_censor,
      N_FAILED = cum_failed, N_LEFT = n_left,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out_list)
}

arms <- sort(unique(one$arm))  # SAS character ordering: uppercase before lowercase; "Arm 1","Arm 2"
est_list <- list()
for (a in arms) {
  d <- one[one$arm == a, , drop = FALSE]
  e <- compute_km(d)
  e <- cbind(data.frame(ARM = a, stringsAsFactors = FALSE), e)
  est_list[[a]] <- e
}
estimates <- do.call(rbind, est_list)
rownames(estimates) <- NULL

# ---- Summary of censored / uncensored (SAS "Summary of the Number of Censored and Uncensored Values") ----
summ <- one %>%
  group_by(arm) %>%
  summarise(TOTAL = n(),
            FAILED = sum(event == 1),
            CENSORED = sum(event == 0),
            .groups = "drop") %>%
  mutate(PCT_CENSORED = ifelse(TOTAL > 0, 100 * CENSORED / TOTAL, NA_real_)) %>%
  arrange(arm)
colnames(summ) <- c("ARM","TOTAL","FAILED","CENSORED","PCT_CENSORED")

# ---- Quartile estimates (time at which survival <= 0.75 / 0.50 / 0.25) ----
get_quartile <- function(e, prob) {
  # prob = survival level: 0.75, 0.50, 0.25
  idx <- which(e$SURVIVAL <= prob)
  if (length(idx) == 0) return(NA_real_)
  e$DAY[min(idx)]
}
quart <- data.frame(ARM = character(0), Q25 = numeric(0), MEDIAN = numeric(0), Q75 = numeric(0),
                    stringsAsFactors = FALSE)
for (a in arms) {
  e <- estimates[estimates$ARM == a, , drop = FALSE]
  q75_time <- get_quartile(e, 0.75)  # 25% failure
  med_time <- get_quartile(e, 0.50)
  q25_time <- get_quartile(e, 0.25)  # 75% failure
  quart <- rbind(quart, data.frame(ARM = a, Q25 = q75_time, MEDIAN = med_time, Q75 = q25_time,
                                   stringsAsFactors = FALSE))
}

# ---- Test of equality over strata (log-rank) ----
sdiff <- survdiff(Surv(day, event) ~ arm, data = one)
# sdiff$n, sdiff$obs, sdiff$exp
chisq_lr <- as.numeric(sdiff$chisq)
df_lr <- length(sdiff$n) - 1
p_lr <- 1 - pchisq(chisq_lr, df_lr)

# ---- Number at risk 0 to 330 by 30 (plots=survival atrisk) ----
atrisk_times <- seq(0, 330, by = 30)
atrisk_mat <- sapply(arms, function(a) sapply(atrisk_times, function(t) sum(one$day[one$arm == a] >= t)))
colnames(atrisk_mat) <- arms
atrisk_df <- data.frame(TIME = atrisk_times, atrisk_mat, check.names = FALSE, stringsAsFactors = FALSE)
# long form for plotting
atrisk_long <- data.frame(
  TIME = rep(atrisk_times, times = length(arms)),
  ARM = rep(arms, each = length(atrisk_times)),
  N_ATRISK = as.vector(atrisk_mat),
  stringsAsFactors = FALSE
)

# ---- Survival step data for plot (include time 0, survival 1) ----
plot_step <- rbind(
  data.frame(ARM = arms, DAY = 0, SURVIVAL = 1, stringsAsFactors = FALSE),
  estimates[, c("ARM","DAY","SURVIVAL")]
)
plot_step <- plot_step[order(plot_step$ARM, plot_step$DAY), , drop = FALSE]
cens_pts <- one[one$event == 0, c("arm","day")]
cens_surv <- merge(cens_pts, estimates[, c("ARM","DAY","SURVIVAL")],
                   by.x = c("arm","day"), by.y = c("ARM","DAY"), all.x = TRUE)
# for censored times that coincide with event times, survival is post-event survival (correct for plot)
# for censored-only times, survival carried forward (already in estimates)

cat("KM computed\n")

# ---- Write CSV of table body (estimates) ----
estimates_csv <- estimates
colnames(estimates_csv) <- c("ARM","DAY","SURVIVAL","FAILURE","STDERR","N_RISK","N_EVENT","N_CENSOR","N_FAILED","N_LEFT")
write.csv(estimates_csv, file.path(out, "P040_f_kmplot.csv"), row.names = FALSE, na = "")

# ---- Write CSV of plotted data ----
# survival curve points + at-risk table combined in one file with record type
plotdata_surv <- plot_step
colnames(plotdata_surv) <- c("ARM","DAY","SURVIVAL")
plotdata_surv$TYPE <- "SURVIVAL"
plotdata_risk <- atrisk_long
colnames(plotdata_risk) <- c("DAY","ARM","SURVIVAL")
plotdata_risk$TYPE <- "ATRISK"
# align columns: ARM, DAY, SURVIVAL, TYPE
plotdata <- rbind(plotdata_surv[, c("ARM","DAY","SURVIVAL","TYPE")],
                  plotdata_risk[, c("ARM","DAY","SURVIVAL","TYPE")])
write.csv(plotdata, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names = FALSE, na = "")

# ---- Write TXT rendering ----
txtfile <- file.path(out, "P040_f_kmplot.txt")
sink(txtfile)
cat("Example K-M Plot\n")
cat("PROC LIFETEST Product-Limit Survival Estimates  TIME day*CENSOR(1)  STRATA arm\n")
cat("\n")
cat("Summary of the Number of Censored and Uncensored Values\n")
cat(sprintf("%-10s %8s %8s %8s %12s\n", "Stratum", "Total", "Failed", "Censored", "PctCensored"))
for (i in seq_len(nrow(summ))) {
  cat(sprintf("%-10s %8d %8d %8d %12s\n",
              summ$ARM[i], summ$TOTAL[i], summ$FAILED[i], summ$CENSORED[i],
              fmt1(summ$PCT_CENSORED[i])))
}
cat("\n")
cat("Quartile Estimates (time at which survival <= level)\n")
cat(sprintf("%-10s %12s %12s %12s\n", "Stratum", "Q25(0.75)", "Median(0.50)", "Q75(0.25)"))
for (i in seq_len(nrow(quart))) {
  f <- function(v) ifelse(is.na(v), ".", sprintf("%g", v))
  cat(sprintf("%-10s %12s %12s %12s\n", quart$ARM[i], f(quart$Q25[i]), f(quart$MEDIAN[i]), f(quart$Q75[i])))
}
cat("\n")
cat("Product-Limit Survival Estimates\n")
cat(sprintf("%-10s %10s %10s %10s %10s %10s %10s\n",
            "Stratum", "Day", "Survival", "Failure", "StdErr", "NFailed", "NLeft"))
for (i in seq_len(nrow(estimates))) {
  day_str <- ifelse(estimates$DAY[i] == floor(estimates$DAY[i]),
                    sprintf("%d", as.integer(estimates$DAY[i])),
                    sprintf("%g", estimates$DAY[i]))
  # mark censored-only times with * like SAS (when N_EVENT==0 & N_CENSOR>0)
  mark <- ifelse(estimates$N_EVENT[i] == 0 & estimates$N_CENSOR[i] > 0, "*", " ")
  cat(sprintf("%-10s %9s%s %10s %10s %10s %10d %10d\n",
              estimates$ARM[i], day_str, mark,
              fmt4(estimates$SURVIVAL[i]), fmt4(estimates$FAILURE[i]), fmt4(estimates$STDERR[i]),
              estimates$N_FAILED[i], estimates$N_LEFT[i]))
}
cat("\n")
cat("Test of Equality over Strata (Log-Rank)\n")
cat(sprintf("%-12s %12s %6s %12s\n", "Test", "ChiSquare", "DF", "pValue"))
cat(sprintf("%-12s %12s %6d %12s\n", "Log-Rank", fmt4(chisq_lr), df_lr, fmt4(p_lr)))
cat("\n")
cat("Observed vs Expected (Log-Rank)\n")
cat(sprintf("%-10s %10s %10s\n", "Stratum", "Observed", "Expected"))
for (j in seq_along(sdiff$n)) {
  nm <- names(sdiff$n)[j]
  # names like "arm=Arm 1"
  nm <- sub(".*=", "", nm)
  cat(sprintf("%-10s %10d %10s\n", nm, as.integer(sdiff$obs[j]), fmt2(sdiff$exp[j])))
}
cat("\n")
cat("Number at Risk (0 to 330 by 30)\n")
cat(sprintf("%8s", "Day"))
for (a in arms) cat(sprintf(" %10s", a))
cat("\n")
for (k in seq_along(atrisk_times)) {
  cat(sprintf("%8d", atrisk_times[k]))
  for (a in arms) cat(sprintf(" %10d", atrisk_mat[k, a]))
  cat("\n")
}
cat("\n")
cat("Note: SAS program had 'srata arm' (typo for 'strata arm'); stratified by arm as intended.\n")
sink()

cat("TXT/CSV written\n")

# ---- Figure: KM plot with at-risk table ----
cols <- c("Arm 1" = "#D55E00", "Arm 2" = "#0072B2")

p_main <- ggplot(plot_step, aes(x = DAY, y = SURVIVAL, color = ARM, group = ARM)) +
  geom_step(linewidth = 1) +
  geom_point(data = cens_surv, aes(x = day, y = SURVIVAL, color = arm),
             shape = 3, size = 2.5, stroke = 1, inherit.aes = FALSE) +
  scale_x_continuous(breaks = atrisk_times, limits = c(0, 330)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(values = cols) +
  labs(title = "Example K-M Plot", x = "Day", y = "Survival Probability", color = "Stratum") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

p_risk <- ggplot(atrisk_long, aes(x = TIME, y = ARM, label = N_ATRISK, color = ARM)) +
  geom_text(size = 4, show.legend = FALSE) +
  scale_x_continuous(breaks = atrisk_times, limits = c(0, 330)) +
  scale_y_discrete(limits = rev(arms)) +
  scale_color_manual(values = cols) +
  labs(x = "Day", y = "At Risk") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

png(file.path(out, "P040_f_kmplot.png"), width = 1200, height = 800, res = 120)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(3, 1), "null"))))
print(p_main, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_risk, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

cat("P040 done\n")
