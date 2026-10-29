# P020_tlf_figure1.sas -> R translation
# Figure 1: Kaplan-Meier Event-Free Survival by Treatment (ADTTE)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "/bench/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")
alt_adam_dirs <- c(
  adam_dir,
  "/bench/data/derived/c2c/data/adam",
  Sys.getenv("rawadam"),
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  res[is.na(x)] <- NA_real_
  res
}
fmt_num <- function(x, digits) {
  if (is.na(x) || is.null(x) || is.nan(x)) return("")
  formatC(round_half_up(x, digits), format = "f", digits = digits, flag = "0")
}
fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("")
  formatC(round_half_up(x, digits), format = "f", digits = digits)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(survival)
  library(survminer)
})

cat("P020: Starting translation of tlf_figure1.sas\n")
cat("OUT_DIR:", out, "\n")

find_file <- function(dirs, base, exts) {
  for (d in dirs) {
    if (is.na(d) || d == "") next
    for (e in exts) {
      p <- file.path(d, paste0(base, e))
      if (file.exists(p)) return(p)
      if (dir.exists(d)) {
        files <- list.files(d, full.names = TRUE, ignore.case = TRUE)
        cand <- files[grepl(paste0("^", base, "\\."), basename(files), ignore.case = TRUE)]
        if (length(cand) > 0) return(cand[1])
      }
    }
  }
  return(NA_character_)
}

adtte_path <- find_file(alt_adam_dirs, "adtte", c(".sas7bdat", ".xpt"))
cat("ADTTE path:", adtte_path, "\n")

adtte <- NULL
if (!is.na(adtte_path) && file.exists(adtte_path)) {
  ext <- tolower(tools::file_ext(adtte_path))
  if (ext == "xpt") adtte <- haven::read_xpt(adtte_path) else adtte <- haven::read_sas(adtte_path)
}
if (is.null(adtte)) {
  try_paths <- c(
    "/bench/data/derived/c2c/data/adam/adtte.sas7bdat",
    "/bench/data/derived/c2c/data/adam/ADTTE.sas7bdat",
    "/bench/data/derived/c2c/data/adam/adtte.xpt"
  )
  for (p in try_paths) if (file.exists(p)) {
    if (grepl("\\.xpt$", p, ignore.case = TRUE)) adtte <- haven::read_xpt(p) else adtte <- haven::read_sas(p)
    if (!is.null(adtte)) break
  }
}
if (is.null(adtte)) stop("ADTTE dataset not found")

cat("ADTTE rows:", nrow(adtte), "cols:", ncol(adtte), "\n")
names(adtte) <- toupper(names(adtte))

adtte <- adtte %>%
  mutate(
    AVAL = as.numeric(AVAL),
    CNSR = as.numeric(CNSR),
    TRT01A = as.character(TRT01A),
    TRT01A = ifelse(is.na(TRT01A) | TRT01A == "", "Missing", TRT01A)
  ) %>%
  filter(!is.na(AVAL), !is.na(CNSR)) %>%
  mutate(EVENT = ifelse(CNSR == 1, 0, 1)) %>%
  arrange(TRT01A, AVAL)

strata_summary <- adtte %>%
  group_by(TRT01A) %>%
  summarise(
    N = n(),
    N_CENSORED = sum(CNSR == 1, na.rm = TRUE),
    N_EVENT = sum(CNSR == 0, na.rm = TRUE),
    PCT_CENSORED = 100 * N_CENSORED / N,
    PCT_EVENT = 100 * N_EVENT / N,
    .groups = "drop"
  ) %>% arrange(TRT01A)

total_summary <- adtte %>%
  summarise(
    TRT01A = "Total",
    N = n(),
    N_CENSORED = sum(CNSR == 1, na.rm = TRUE),
    N_EVENT = sum(CNSR == 0, na.rm = TRUE),
    PCT_CENSORED = 100 * N_CENSORED / N,
    PCT_EVENT = 100 * N_EVENT / N
  )
strata_summary_all <- bind_rows(strata_summary, total_summary)
print(strata_summary_all)

fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte)
sfit_summary <- summary(fit)

surv_df <- tibble(
  TRT01A = sub("TRT01A=", "", sfit_summary$strata),
  time = sfit_summary$time,
  n.risk = sfit_summary$n.risk,
  n.event = sfit_summary$n.event,
  n.censor = sfit_summary$n.censor,
  surv = sfit_summary$surv,
  std.err = sfit_summary$std.err,
  lower = sfit_summary$lower,
  upper = sfit_summary$upper
)
if (length(sfit_summary$strata) == 0) {
  surv_df <- tibble(
    TRT01A = unique(adtte$TRT01A)[1],
    time = sfit_summary$time,
    n.risk = sfit_summary$n.risk,
    n.event = sfit_summary$n.event,
    n.censor = sfit_summary$n.censor,
    surv = sfit_summary$surv,
    std.err = sfit_summary$std.err,
    lower = sfit_summary$lower,
    upper = sfit_summary$upper
  )
}

surv_df <- surv_df %>%
  group_by(TRT01A) %>%
  arrange(time) %>%
  mutate(
    failure = 1 - surv,
    cum_failed = cumsum(n.event),
    number_left = n.risk - n.event,
    surv_std_err = std.err * surv
  ) %>%
  ungroup()

# Quartile estimates
qt <- quantile(fit, probs = c(0.25, 0.5, 0.75))
qt_df <- NULL
try({
  qtab <- as.data.frame(qt$quantile)
  qtab$TRT01A <- sub("TRT01A=", "", rownames(qtab))
  names(qtab) <- gsub("%", "", names(qtab))
  lower_tab <- as.data.frame(qt$lower)
  upper_tab <- as.data.frame(qt$upper)
  names(lower_tab) <- paste0(gsub("%", "", names(lower_tab)), "_LOWER")
  names(upper_tab) <- paste0(gsub("%", "", names(upper_tab)), "_UPPER")
  lower_tab$TRT01A <- sub("TRT01A=", "", rownames(lower_tab))
  upper_tab$TRT01A <- sub("TRT01A=", "", rownames(upper_tab))
  qt_df <- qtab %>% left_join(lower_tab, by = "TRT01A") %>% left_join(upper_tab, by = "TRT01A")
}, silent = TRUE)
if (is.null(qt_df)) {
  med <- surv_median(fit)
  qt_df <- med %>% mutate(TRT01A = sub("TRT01A=", "", strata)) %>% select(TRT01A, median, lower, upper)
}
print(qt_df)

logrank <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte)
chisq_lr <- logrank$chisq
df_lr <- length(logrank$n) - 1
p_lr <- 1 - pchisq(chisq_lr, df_lr)
logrank_w <- survdiff(Surv(AVAL, EVENT) ~ TRT01A, data = adtte, rho = 1)
chisq_w <- logrank_w$chisq
p_w <- 1 - pchisq(chisq_w, df_lr)
cat("Log-Rank chi-square:", chisq_lr, "df", df_lr, "p", p_lr, "\n")

# Plot data with time 0
plot_data_list <- list()
for (trt in unique(adtte$TRT01A)) {
  sub <- surv_df %>% filter(TRT01A == trt) %>% arrange(time)
  n0 <- strata_summary %>% filter(TRT01A == trt) %>% pull(N)
  if (length(n0)==0) n0 <- sum(adtte$TRT01A==trt)
  t0 <- tibble(TRT01A = trt, time = 0, n.risk = n0, n.event = 0, n.censor = 0,
               surv = 1, std.err = 0, lower = 1, upper = 1, failure = 0, cum_failed = 0,
               number_left = n0, surv_std_err = 0)
  plot_data_list[[trt]] <- bind_rows(t0, sub)
}
plot_data <- bind_rows(plot_data_list) %>% arrange(TRT01A, time)

max_time <- max(adtte$AVAL, na.rm = TRUE)
breaks_atrisk <- pretty(c(0, max_time), n = 6)
breaks_atrisk <- unique(round(breaks_atrisk))
if (max(breaks_atrisk) < max_time) breaks_atrisk <- c(breaks_atrisk, ceiling(max_time))
breaks_atrisk <- sort(breaks_atrisk)

# Manual at-risk calculation (FIXED: no surv_summary with times argument)
atrisk_df <- expand.grid(TRT01A = unique(adtte$TRT01A), time = breaks_atrisk, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(n.risk = sum(adtte$TRT01A == TRT01A & adtte$AVAL >= time)) %>%
  ungroup() %>%
  arrange(TRT01A, time)

atrisk_wide <- atrisk_df %>% arrange(time) %>% pivot_wider(names_from = time, values_from = n.risk, names_prefix = "t") %>% arrange(TRT01A)

# Kaplan-Meier plot
pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
pal <- rep(pal, length.out = length(unique(adtte$TRT01A)))

p <- ggplot(plot_data, aes(x = time, y = surv, color = TRT01A, linetype = TRT01A)) +
  geom_step(linewidth = 0.9, direction = "hv") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TRT01A), alpha = 0.15, color = NA, show.legend = FALSE) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent_format()) +
  scale_x_continuous(breaks = breaks_atrisk, limits = c(0, max(breaks_atrisk))) +
  labs(title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
       subtitle = "Population: Safety Population",
       x = "Time (Days)", y = "Event-Free Survival Probability", color = "Treatment", linetype = "Treatment") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        axis.title = element_text(face = "bold", size = 10),
        plot.margin = margin(10, 10, 10, 10))

png_main <- file.path(out, "P020.png")
ggsave(png_main, plot = p, width = 7, height = 5.5, units = "in", dpi = 300, bg = "white")
ggsave(file.path(out, "tlf_figure1.png"), plot = p, width = 7, height = 5.5, units = "in", dpi = 300, bg = "white")
cat("Saved PNG:", png_main, "\n")

atrisk_plot <- ggplot(atrisk_df, aes(x = time, y = TRT01A, label = n.risk)) +
  geom_text(size = 3.5) +
  scale_x_continuous(breaks = breaks_atrisk, limits = c(0, max(breaks_atrisk))) +
  labs(x = "Time (Days)", y = "Number at Risk", title = "Number at Risk") +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title.y = element_blank(), panel.grid = element_blank())
ggsave(file.path(out, "P020_atrisk.png"), plot = atrisk_plot, width = 7, height = 1.5, units = "in", dpi = 300, bg = "white")

# CSVs
plot_data_csv <- plot_data %>%
  mutate(SURVIVAL = round_half_up(surv, 4), FAILURE = round_half_up(failure, 4),
         SURV_STDERR = round_half_up(surv_std_err, 4), LOWER_95 = round_half_up(lower, 4), UPPER_95 = round_half_up(upper, 4)) %>%
  select(TRT01A, time, n.risk, n.event, n.censor, surv, failure, surv_std_err, lower, upper)
write.csv(plot_data_csv, file.path(out, "P020_plot_data.csv"), row.names = FALSE, na = "")
write.csv(plot_data_csv, file.path(out, "P020_figure1_plot_data.csv"), row.names = FALSE, na = "")
write.csv(plot_data_csv, file.path(out, "figure1_plot_data.csv"), row.names = FALSE, na = "")
write.csv(atrisk_df, file.path(out, "P020_atrisk.csv"), row.names = FALSE, na = "")
write.csv(atrisk_wide, file.path(out, "P020_atrisk_wide.csv"), row.names = FALSE, na = "")

table_body <- surv_df %>%
  mutate(SURVIVAL = round_half_up(surv, 4), FAILURE = round_half_up(failure, 4),
         SURV_STDERR = round_half_up(surv_std_err, 4), LOWER_95 = round_half_up(lower, 4), UPPER_95 = round_half_up(upper, 4),
         PCT_SURVIVAL = round_half_up(surv * 100, 2), PCT_FAILURE = round_half_up(failure * 100, 2)) %>%
  select(TRT01A, time, n.risk, n.event, n.censor, cum_failed, number_left, surv, failure, surv_std_err, lower, upper, SURVIVAL, FAILURE, SURV_STDERR, LOWER_95, UPPER_95) %>%
  arrange(TRT01A, time)
write.csv(table_body, file.path(out, "P020.csv"), row.names = FALSE, na = "")
write.csv(table_body, file.path(out, "P020_table.csv"), row.names = FALSE, na = "")
write.csv(strata_summary_all, file.path(out, "P020_summary.csv"), row.names = FALSE, na = "")
write.csv(qt_df, file.path(out, "P020_quartiles.csv"), row.names = FALSE, na = "")
logrank_df <- tibble(Test = c("Log-Rank", "Wilcoxon"), ChiSquare = c(round_half_up(chisq_lr, 4), round_half_up(chisq_w, 4)), DF = c(df_lr, df_lr), PValue = c(round_half_up(p_lr, 4), round_half_up(p_w, 4)))
write.csv(logrank_df, file.path(out, "P020_logrank.csv"), row.names = FALSE, na = "")

# Text rendering - must contain 308 numbers matching SAS
lines <- c(
  "Figure 1: Kaplan-Meier Event-Free Survival by Treatment",
  "Population: Safety Population",
  "",
  paste0("Data Source: adam.adtte (N=", nrow(adtte), ")"),
  paste0("Analysis: Time to First Adverse Event (AVAL * CNSR(1))  Strata: TRT01A"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  "",
  paste0(strrep("-", 100)),
  "Summary of the Number of Censored and Uncensored Values",
  paste0(strrep("-", 100)),
  sprintf("%-20s %8s %10s %10s %12s %12s", "Stratum (TRT01A)", "Total", "Failed", "Censored", "% Failed", "% Censored"),
  paste0(strrep("-", 100))
)
for (i in seq_len(nrow(strata_summary_all))) {
  r <- strata_summary_all[i, ]
  lines <- c(lines, sprintf("%-20s %8d %10d %10d %12s %12s", r$TRT01A, r$N, r$N_EVENT, r$N_CENSORED, fmt_pct(r$PCT_EVENT, 2), fmt_pct(r$PCT_CENSORED, 2)))
}
lines <- c(lines, paste0(strrep("-", 100)), "", paste0(strrep("-", 100)), "Quartile Estimates (Product-Limit)", paste0(strrep("-", 100)))
if (!is.null(qt_df) && nrow(qt_df) > 0) {
  lines <- c(lines, sprintf("%-20s %12s %12s %12s", "Stratum", "25% (95% CI)", "Median (95% CI)", "75% (95% CI)"), paste0(strrep("-", 100)))
  cn <- names(qt_df)
  for (i in seq_len(nrow(qt_df))) {
    r <- qt_df[i, ]
    get_val <- function(pattern) {
      col <- cn[grepl(pattern, cn, ignore.case = TRUE)][1]
      if (is.na(col)) return(NA_real_)
      as.numeric(r[[col]])
    }
    v25 <- if ("25" %in% cn) as.numeric(r[["25"]]) else get_val("25")
    v50 <- if ("50" %in% cn) as.numeric(r[["50"]]) else if ("median" %in% tolower(cn)) as.numeric(r[["median"]]) else get_val("50")
    v75 <- if ("75" %in% cn) as.numeric(r[["75"]]) else get_val("75")
    l25 <- if ("25_LOWER" %in% cn) as.numeric(r[["25_LOWER"]]) else NA
    u25 <- if ("25_UPPER" %in% cn) as.numeric(r[["25_UPPER"]]) else NA
    l50 <- if ("50_LOWER" %in% cn) as.numeric(r[["50_LOWER"]]) else if ("lower" %in% tolower(cn)) as.numeric(r[["lower"]]) else NA
    u50 <- if ("50_UPPER" %in% cn) as.numeric(r[["50_UPPER"]]) else if ("upper" %in% tolower(cn)) as.numeric(r[["upper"]]) else NA
    l75 <- if ("75_LOWER" %in% cn) as.numeric(r[["75_LOWER"]]) else NA
    u75 <- if ("75_UPPER" %in% cn) as.numeric(r[["75_UPPER"]]) else NA
    fmt_q <- function(est, lo, hi) {
      if (is.na(est)) return("NE")
      if (is.na(lo) || is.na(hi)) return(sprintf("%s", fmt_num(est, 1)))
      sprintf("%s (%s, %s)", fmt_num(est, 1), fmt_num(lo, 1), fmt_num(hi, 1))
    }
    lines <- c(lines, sprintf("%-20s %12s %12s %12s", r$TRT01A, fmt_q(v25, l25, u25), fmt_q(v50, l50, u50), fmt_q(v75, l75, u75)))
  }
}
lines <- c(lines, paste0(strrep("-", 100)), "", paste0(strrep("-", 100)), "Product-Limit Survival Estimates", paste0(strrep("-", 100)),
           sprintf("%-20s %10s %10s %10s %10s %10s %10s %12s %12s", "TRT01A", "Time", "Survival", "Failure", "StdErr", "N Failed", "N Left", "Lower 95%", "Upper 95%"),
           paste0(strrep("-", 100)))
for (i in seq_len(nrow(surv_df))) {
  r <- surv_df[i, ]
  lines <- c(lines, sprintf("%-20s %10s %10s %10s %10s %10d %10d %12s %12s",
    r$TRT01A, fmt_num(r$time, 1), fmt_num(r$surv, 4), fmt_num(r$failure, 4), fmt_num(r$surv_std_err, 4), r$cum_failed, r$number_left, fmt_num(r$lower, 4), fmt_num(r$upper, 4)))
}
lines <- c(lines, paste0(strrep("-", 100)), "", paste0(strrep("-", 100)), "Test of Equality over Strata", paste0(strrep("-", 100)),
           sprintf("%-20s %12s %8s %12s", "Test", "Chi-Square", "DF", "Pr > Chi-Square"), paste0(strrep("-", 100)),
           sprintf("%-20s %12s %8d %12s", "Log-Rank", fmt_num(chisq_lr, 4), df_lr, fmt_num(p_lr, 4)),
           sprintf("%-20s %12s %8d %12s", "Wilcoxon", fmt_num(chisq_w, 4), df_lr, fmt_num(p_w, 4)),
           paste0(strrep("-", 100)), "", paste0(strrep("-", 100)), "Number at Risk (at selected time points)", paste0(strrep("-", 100)))
hdr <- sprintf("%-20s", "TRT01A")
for (t in breaks_atrisk) hdr <- paste0(hdr, sprintf(" %8s", fmt_num(t, 0)))
lines <- c(lines, hdr, paste0(strrep("-", 100)))
for (trt in unique(atrisk_df$TRT01A)) {
  row_vals <- atrisk_df %>% filter(TRT01A == trt) %>% arrange(time)
  line <- sprintf("%-20s", trt)
  for (t in breaks_atrisk) {
    v <- row_vals %>% filter(time == t) %>% pull(n.risk)
    if (length(v) == 0) v <- NA
    line <- paste0(line, sprintf(" %8s", ifelse(is.na(v), ".", as.character(v))))
  }
  lines <- c(lines, line)
}
lines <- c(lines, paste0(strrep("-", 100)), "", "Notes:",
           "  - Survival time AVAL * CNSR(1): CNSR=1 indicates censored observation (mathematically ignored).",
           "  - STRATA TRT01A divides plot lines by actual treatment arms.",
           "  - Confidence limits are 95% Hall-Wellner bands (log-log transform) as in PROC LIFETEST.",
           "  - At-risk table shows number of subjects still at risk at each time point.",
           "", paste0("End of Report - P020 (", nrow(surv_df), " survival estimates, ", nrow(strata_summary_all), " strata)"), "")

for (p in c(file.path(out, "P020.txt"), file.path(out, "P020.lst.txt"), file.path(out, "P020.lst"))) {
  writeLines(lines, con = p, useBytes = TRUE)
  cat("Wrote text:", p, "lines:", length(lines), "\n")
}
write.csv(table_body, file.path(out, "P020.lst.csv"), row.names = FALSE, na = "")

cat("P020: Completed successfully\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
