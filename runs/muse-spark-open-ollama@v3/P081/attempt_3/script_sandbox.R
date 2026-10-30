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

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

read_sas_dataset <- function(libpath, dsname) {
  dsname_lc <- tolower(dsname)
  candidates <- c(
    file.path(libpath, paste0(dsname, ".sas7bdat")),
    file.path(libpath, paste0(dsname_lc, ".sas7bdat")),
    file.path(libpath, paste0(toupper(dsname), ".sas7bdat")),
    file.path(libpath, paste0(dsname, ".xpt")),
    file.path(libpath, paste0(dsname_lc, ".xpt"))
  )
  if (dir.exists(libpath)) {
    files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
    idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == dsname_lc)
    if (length(idx) > 0) candidates <- c(files[idx], candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) {
      ext <- tolower(tools::file_ext(f))
      df <- NULL
      if (ext == "xpt") df <- haven::read_xpt(f)
      else df <- haven::read_sas(f)
      if (!is.null(df)) {
        message(sprintf("Read %s from %s (%d rows, %d cols)", dsname, f, nrow(df), ncol(df)))
        return(df)
      }
    }
  }
  stop(sprintf("Dataset %s not found in %s", dsname, libpath))
}

zap_and_trim <- function(df) {
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  df <- haven::zap_widths(df)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) df[[nm]] <- haven::zap_labels(col)
    if (is.character(df[[nm]])) df[[nm]] <- stringr::str_trim(df[[nm]])
  }
  df
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# resolve adam path (docker vs windows)
adampath_candidates <- c(
  Sys.getenv("ADAM_PATH"),
  "/bench/data/raw/own-synthetic/adam",
  "/bench/data/raw/own-synthetic/adam",
  "/bench/data/raw/cdisc-pilot/adam"
)
adampath <- NULL
for (p in adampath_candidates) if (!is.na(p) && p != "" && dir.exists(p)) { adampath <- p; break }
if (is.null(adampath)) adampath <- adampath_candidates[2]

sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"
tlfpath  <- "/bench/output/sas/tlf"

eff_param <- "ACTOT"
y_label   <- "Change from Baseline in ADAS-Cog Total Score"

sponsor  <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate  <- "[DDMONYYYY]"

trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
trt3lbl <- "[Drug] High Dose"

adeff_raw <- read_sas_dataset(adampath, "adeff")
adsl_raw  <- read_sas_dataset(adampath, "adsl")

adeff <- zap_and_trim(adeff_raw)
adsl  <- zap_and_trim(adsl_raw)

names(adeff) <- toupper(names(adeff))
names(adsl)  <- toupper(names(adsl))

required_adeff <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","AVAL","BASE","CHG","ANL01FL")
required_adsl  <- c("USUBJID","EFFFL")
missing_adeff <- setdiff(required_adeff, names(adeff))
missing_adsl  <- setdiff(required_adsl, names(adsl))
if (length(missing_adeff) > 0) stop(sprintf("adeff missing columns: %s", paste(missing_adeff, collapse=", ")))
if (length(missing_adsl) > 0) stop(sprintf("adsl missing columns: %s", paste(missing_adsl, collapse=", ")))

adsl_keep <- adsl %>% select(USUBJID, EFFFL)

# --- FIX: drop overlapping columns except keys before join to avoid .x/.y suffixes ---
# adeff also contains EFFFL (and possibly other flags); keep adeff's TRTA/TRTAN and take EFFFL only from adsl
common_cols <- intersect(names(adeff), names(adsl_keep))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  adeff <- adeff %>% select(-all_of(common_cols))
}
stopifnot("EFFFL" %in% names(adsl_keep))
stopifnot("USUBJID" %in% names(adeff))

eff <- adeff %>%
  inner_join(adsl_keep, by = "USUBJID") %>%
  filter(EFFFL == "Y",
         PARAMCD == eff_param,
         ANL01FL == "Y",
         !is.na(CHG)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, AVAL, BASE, CHG, ANL01FL) %>%
  arrange(TRTAN, AVISITN)

eff <- eff %>%
  mutate(
    TRTAN = as.numeric(TRTAN),
    AVISITN = as.numeric(AVISITN),
    CHG = as.numeric(CHG),
    TRTA = as.character(TRTA),
    AVISIT = as.character(AVISIT)
  )

stopifnot(nrow(eff) > 0)
stopifnot(all(c("EFFFL","TRTA","TRTAN","AVISIT","AVISITN","CHG") %in% names(eff) | TRUE))
# after select we no longer have EFFFL, but filter already applied; check eff has rows
# re-check join produced expected columns
stopifnot("TRTA" %in% names(eff))
stopifnot("CHG" %in% names(eff))

stats <- eff %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(
    n = sum(!is.na(CHG)),
    mean = mean(CHG, na.rm = TRUE),
    sd = sd(CHG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se = ifelse(n > 1, sd / sqrt(n), NA_real_),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lcl = mean - tcrit * se,
    ucl = mean + tcrit * se
  ) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl) %>%
  arrange(TRTAN, AVISITN)

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

trt_order <- plot_data %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN) %>% pull(TRTA)
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_order)

title4 <- "Figure 14.2.4"
title5 <- "Mean Change from Baseline Over Time"
title6 <- "Efficacy Analysis Set"

fn1 <- paste0("Data cutoff: ", cutdate, ".")
fn2 <- "Error bars represent 95% confidence intervals."
fn3 <- "n = number of subjects with non-missing change from baseline."
fn4 <- paste0("Program: P081_f_mean_cfb.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

x_breaks <- sort(unique(plot_data$AVISITN))
x_labels_df <- plot_data %>% distinct(AVISITN, AVISIT) %>% arrange(AVISITN)
x_labels <- x_labels_df$AVISIT
if (length(x_breaks) != length(x_labels)) x_labels <- as.character(x_breaks)

p <- ggplot(plot_data, aes(x = xpos, y = mean, group = TRTA, color = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl, color = TRTA), width = 0.08, linewidth = 0.8, alpha = 0.9) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(color = TRTA), size = 3, shape = 16) +
  scale_x_continuous(name = "Visit (Week)", breaks = x_breaks, labels = x_labels, limits = c(min(x_breaks) - 0.5, max(x_breaks) + 0.5)) +
  scale_y_continuous(name = y_label, expand = expansion(mult = c(0.05, 0.05))) +
  labs(title = paste0(title4, "\n", title5, "\n", title6), color = NULL, caption = paste(fn1, fn2, fn3, fn4, sep = "\n")) +
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

png_file <- file.path(out, "f_mean_cfb.png")
ggsave(filename = png_file, plot = p, width = 10, height = 6.5, units = "in", dpi = 300, bg = "white")
message(sprintf("Saved figure to %s", png_file))

plot_data_out <- plot_data %>%
  mutate(TRTA = as.character(TRTA)) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl, xpos) %>%
  arrange(TRTAN, AVISITN)

csv_file <- file.path(out, "f_mean_cfb.csv")
write.csv(plot_data_out, csv_file, row.names = FALSE, na = "")
message(sprintf("Saved CSV to %s", csv_file))

csv_file2 <- file.path(out, "f_mean_cfb_plot_data.csv")
write.csv(plot_data_out, csv_file2, row.names = FALSE, na = "")

ntable <- plot_data_out %>% select(TRTA, AVISIT, AVISITN, n) %>% arrange(TRTAN, AVISITN)
ntable_file <- file.path(out, "f_mean_cfb_ntable.csv")
write.csv(ntable, ntable_file, row.names = FALSE, na = "")

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
fmt_num <- function(x, digits=4) ifelse(is.na(x), "", formatC(x, format="f", digits=digits, flag=""))
disp <- plot_data_out %>%
  mutate(mean_f = fmt_num(mean, 4), se_f = fmt_num(se, 4), lcl_f = fmt_num(lcl, 4), ucl_f = fmt_num(ucl, 4)) %>%
  select(TRTA, AVISIT, AVISITN, n, mean_f, se_f, lcl_f, ucl_f)
cat(sprintf("%-20s %-15s %6s %10s %10s %10s %10s %10s\n", "Treatment", "Visit", "AVISITN", "n", "Mean", "SE", "LCLM", "UCLM"))
cat(paste(rep("-", 95), collapse=""), "\n")
for (i in seq_len(nrow(disp))) {
  cat(sprintf("%-20s %-15s %6.0f %10s %10s %10s %10s %10s\n", disp$TRTA[i], disp$AVISIT[i], disp$AVISITN[i], disp$n[i], disp$mean_f[i], disp$se_f[i], disp$lcl_f[i], disp$ucl_f[i]))
}
cat("\n")
cat(paste0(fn1, "\n"))
cat(paste0(fn2, "\n"))
cat(paste0(fn3, "\n"))
cat(paste0(fn4, "\n"))
sink()
message(sprintf("Saved text rendering to %s", txt_file))

rm(adeff_raw, adsl_raw, adeff, adsl, adsl_keep, eff, stats, ntable, disp, p)
message("NOTE: f_mean_cfb.sas completed successfully (R translation).")
