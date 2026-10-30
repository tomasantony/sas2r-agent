# P081_f_mean_cfb.sas -> R translation
# Figure 14.2.4 Mean Change from Baseline Over Time
# Input: adam.adeff, adam.adsl
# Output: f_mean_cfb.png + CSV of plotted data

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(data.table)
})

# helper: SAS round half away from zero (ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: read SAS dataset from a libref directory (tries .sas7bdat, .xpt, .sas7bdat lowercase)
read_sas_dataset <- function(libpath, dsname) {
  dsname_lc <- tolower(dsname)
  candidates <- c(
    file.path(libpath, paste0(dsname, ".sas7bdat")),
    file.path(libpath, paste0(dsname_lc, ".sas7bdat")),
    file.path(libpath, paste0(toupper(dsname), ".sas7bdat")),
    file.path(libpath, paste0(dsname, ".xpt")),
    file.path(libpath, paste0(dsname_lc, ".xpt")),
    file.path(libpath, paste0(dsname, ".sas7bdat.gz"))
  )
  # also list directory to find case-insensitive match
  if (dir.exists(libpath)) {
    files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
    # find by lower case basename without extension
    idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == dsname_lc)
    if (length(idx) > 0) candidates <- c(files[idx], candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) {
      ext <- tolower(tools::file_ext(f))
      df <- NULL
      if (ext == "xpt") {
        df <- haven::read_xpt(f)
      } else if (ext %in% c("sas7bdat", "sas7bdat.gz")) {
        df <- haven::read_sas(f)
      } else {
        # try both
        try({ df <- haven::read_sas(f) }, silent = TRUE)
        if (is.null(df)) try({ df <- haven::read_xpt(f) }, silent = TRUE)
      }
      if (!is.null(df)) {
        message(sprintf("Read %s from %s (%d rows, %d cols)", dsname, f, nrow(df), ncol(df)))
        return(df)
      }
    }
  }
  stop(sprintf("Dataset %s not found in %s (tried: %s)", dsname, libpath, paste(candidates, collapse="; ")))
}

# helper: zap haven labels and trim character
zap_and_trim <- function(df) {
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  df <- haven::zap_widths(df)
  # convert haven_labelled to appropriate type
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      # keep numeric if numeric, else character
      df[[nm]] <- haven::zap_labels(col)
    }
    if (is.character(df[[nm]])) {
      df[[nm]] <- stringr::str_trim(df[[nm]])
      # empty string to NA? SAS missing char is "" -> keep as "" then treat as NA where needed
      # keep as is; filtering will handle
    }
  }
  df
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# paths from u_setup.sas / macro variables
adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
tlfpath  <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"

# allow override via env if needed
if (Sys.getenv("ADAM_PATH") != "") adampath <- Sys.getenv("ADAM_PATH")
if (Sys.getenv("SDTM_PATH") != "") sdtmpath <- Sys.getenv("SDTM_PATH")

# Parameters (from SAS program)
eff_param <- "ACTOT"
y_label   <- "Change from Baseline in ADAS-Cog Total Score"

# Titles / footnotes (from u_titles.sas and program)
sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"
studyid  <- "[STUDYID]"

# Treatment labels (from u_setup.sas)
trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
trt3lbl <- "[Drug] High Dose"

# --- Step 1: Prepare analysis data ---
# Read adam.adeff and adam.adsl
adeff_raw <- read_sas_dataset(adampath, "adeff")
adsl_raw  <- read_sas_dataset(adampath, "adsl")

adeff <- zap_and_trim(adeff_raw)
adsl  <- zap_and_trim(adsl_raw)

# Ensure required columns exist (case-insensitive check)
# haven preserves case as in SAS (usually uppercase)
# Normalize names to uppercase for processing, but keep original for output
names(adeff) <- toupper(names(adeff))
names(adsl)  <- toupper(names(adsl))

# Check columns
required_adeff <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","AVAL","BASE","CHG","ANL01FL")
required_adsl  <- c("USUBJID","EFFFL")
missing_adeff <- setdiff(required_adeff, names(adeff))
missing_adsl  <- setdiff(required_adsl, names(adsl))
if (length(missing_adeff) > 0) stop(sprintf("adeff missing columns: %s", paste(missing_adeff, collapse=", ")))
if (length(missing_adsl) > 0) stop(sprintf("adsl missing columns: %s", paste(missing_adsl, collapse=", ")))

# Drop/rename overlapping columns except keys before join (avoid .x/.y)
# Keep only needed from adsl
adsl_keep <- adsl %>% select(USUBJID, EFFFL)

# Inner join and filter (SAS: where b.EFFFL='Y' and a.PARAMCD="&eff_param." and a.ANL01FL='Y' and a.CHG ne .)
eff <- adeff %>%
  inner_join(adsl_keep, by = "USUBJID") %>%
  filter(EFFFL == "Y",
         PARAMCD == eff_param,
         ANL01FL == "Y",
         !is.na(CHG)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, AVAL, BASE, CHG, ANL01FL) %>%
  arrange(TRTAN, AVISITN)

# Ensure types
eff <- eff %>%
  mutate(
    TRTAN = as.numeric(TRTAN),
    AVISITN = as.numeric(AVISITN),
    CHG = as.numeric(CHG),
    TRTA = as.character(TRTA),
    AVISIT = as.character(AVISIT)
  )

# Stop if zero rows (join bug)
stopifnot(nrow(eff) > 0)

# --- Step 2: Compute summary statistics by visit and treatment ---
# PROC MEANS nway: class TRTA TRTAN AVISIT AVISITN; var CHG; output n mean stderr lclm uclm
# lclm/uclm = mean +/- t_{0.975, n-1} * stderr, stderr = sd / sqrt(n)
stats <- eff %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(
    n = sum(!is.na(CHG)),
    mean = mean(CHG, na.rm = TRUE),
    sd = sd(CHG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se = ifelse(n > 1, sd / sqrt(n), ifelse(n == 1, NA_real_, NA_real_)),
    # t critical value
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lcl = mean - tcrit * se,
    ucl = mean + tcrit * se
  ) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl) %>%
  arrange(TRTAN, AVISITN)

# --- Step 3: Small offset to prevent overlapping points ---
plot_data <- stats %>%
  mutate(
    xpos = case_when(
      TRTAN == 1 ~ AVISITN - 0.15,
      TRTAN == 2 ~ AVISITN,
      TRTAN == 3 ~ AVISITN + 0.15,
      TRUE ~ AVISITN
    )
  ) %>%
  arrange(TRTAN, AVISITN)

# Ensure TRTA is factor ordered by TRTAN for legend
trt_order <- plot_data %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN) %>% pull(TRTA)
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_order)

# --- Step 4: Generate figure ---
# Titles
title4 <- "Figure 14.2.4"
title5 <- "Mean Change from Baseline Over Time"
title6 <- "Efficacy Analysis Set"

# Footnotes
fn1 <- paste0("Data cutoff: ", cutdate, ".")
fn2 <- "Error bars represent 95% confidence intervals."
fn3 <- "n = number of subjects with non-missing change from baseline."
# SAS program uses &sysin. and date; replicate generically
fn4 <- paste0("Program: P081_f_mean_cfb.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

# Build ggplot
# Determine x breaks: unique AVISITN values
x_breaks <- sort(unique(plot_data$AVISITN))
x_labels <- plot_data %>% distinct(AVISITN, AVISIT) %>% arrange(AVISITN) %>% pull(AVISIT)
# If AVISIT labels are like "Week 4" etc, use them; otherwise use numeric
if (length(x_breaks) != length(x_labels)) x_labels <- as.character(x_breaks)

# Color palette - journal style (distinct)
# Use default ggplot2 hues but ensure 3 groups
p <- ggplot(plot_data, aes(x = xpos, y = mean, group = TRTA, color = TRTA)) +
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  # Error bars (95% CI) - highlow type=bar barwidth=0.05
  geom_errorbar(aes(ymin = lcl, ymax = ucl, color = TRTA),
                width = 0.08, linewidth = 0.8, alpha = 0.9) +
  # Mean values connected by lines
  geom_line(linewidth = 1.0) +
  geom_point(aes(color = TRTA), size = 3, shape = 16) +
  scale_x_continuous(
    name = "Visit (Week)",
    breaks = x_breaks,
    labels = x_labels,
    limits = c(min(x_breaks) - 0.5, max(x_breaks) + 0.5)
  ) +
  scale_y_continuous(
    name = y_label,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    title = paste0(title4, "\n", title5, "\n", title6),
    color = NULL,
    caption = paste(fn1, fn2, fn3, fn4, sep = "\n")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, lineheight = 1.1),
    plot.caption = element_text(hjust = 0, size = 7, lineheight = 0.9, margin = margin(t = 10)),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9),
    panel.grid.major.y = element_line(linetype = "dotted", color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# Save PNG (width 10in height 6.5in as in SAS)
png_file <- file.path(out, "f_mean_cfb.png")
ggsave(filename = png_file, plot = p, width = 10, height = 6.5, units = "in", dpi = 300, bg = "white")
message(sprintf("Saved figure to %s", png_file))

# --- Write CSV of plotted data ---
# SAS variable names same case (uppercase) - write with original names
# Include all stats columns plus xpos
plot_data_out <- plot_data %>%
  mutate(TRTA = as.character(TRTA)) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl, xpos) %>%
  arrange(TRTAN, AVISITN)

# Write primary CSV (expected)
csv_file <- file.path(out, "f_mean_cfb.csv")
write.csv(plot_data_out, csv_file, row.names = FALSE, na = "")
message(sprintf("Saved CSV to %s", csv_file))

# Also write plot_data specific name for robustness
csv_file2 <- file.path(out, "f_mean_cfb_plot_data.csv")
write.csv(plot_data_out, csv_file2, row.names = FALSE, na = "")

# --- Supporting data table (n per visit per treatment) - also write as CSV for completeness ---
# PROC TABULATE equivalent: n per visit per treatment
ntable <- plot_data_out %>%
  select(TRTA, AVISIT, AVISITN, n) %>%
  arrange(TRTAN, AVISITN)

ntable_file <- file.path(out, "f_mean_cfb_ntable.csv")
write.csv(ntable, ntable_file, row.names = FALSE, na = "")

# --- Plain-text rendering of the table body (for compliance) ---
txt_file <- file.path(out, "f_mean_cfb.txt")
sink(txt_file)
cat(paste0(sponsor, strrep(" ", 60), "Page 1 of 1\n"))
cat(paste0("Protocol: ", protocol, "\n"))
cat("\n")
cat(paste0(title4, "\n"))
cat(paste0(title5, "\n"))
cat(paste0(title6, "\n"))
cat("\n")
cat(paste0("Parameter: ", eff_param, " (", y_label, ")\n"))
cat(paste0("Population: Efficacy Analysis Set\n"))
cat("\n")
# Print stats table
# Format numbers to 4 decimals for stats as SAS does, but keep n integer
fmt_num <- function(x, digits=4) ifelse(is.na(x), "", formatC(x, format="f", digits=digits, flag=""))
# Build display table
disp <- plot_data_out %>%
  mutate(
    mean_f = fmt_num(mean, 4),
    se_f   = fmt_num(se, 4),
    lcl_f  = fmt_num(lcl, 4),
    ucl_f  = fmt_num(ucl, 4)
  ) %>%
  select(TRTA, AVISIT, AVISITN, n, mean_f, se_f, lcl_f, ucl_f)
cat(sprintf("%-20s %-15s %6s %10s %10s %10s %10s %10s\n",
            "Treatment", "Visit", "AVISITN", "n", "Mean", "SE", "LCLM", "UCLM"))
cat(paste(rep("-", 95), collapse=""), "\n")
for (i in seq_len(nrow(disp))) {
  cat(sprintf("%-20s %-15s %6.0f %10s %10s %10s %10s %10s\n",
              disp$TRTA[i], disp$AVISIT[i], disp$AVISITN[i], disp$n[i],
              disp$mean_f[i], disp$se_f[i], disp$lcl_f[i], disp$ucl_f[i]))
}
cat("\n")
cat(paste0(fn1, "\n"))
cat(paste0(fn2, "\n"))
cat(paste0(fn3, "\n"))
cat(paste0(fn4, "\n"))
sink()
message(sprintf("Saved text rendering to %s", txt_file))

# Clean up (mimic PROC DATASETS delete)
rm(adeff_raw, adsl_raw, adeff, adsl, adsl_keep, eff, stats, ntable, disp, p)
message("NOTE: f_mean_cfb.sas completed successfully (R translation).")
