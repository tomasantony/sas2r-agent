# P081_f_mean_cfb.sas -> R translation
# Figure 14.2.4 - Mean Change from Baseline Over Time

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Paths from SAS program / environment
root     <- "/bench"
adampath <- "/bench/data/raw/own-synthetic/adam"
sdtmpath <- "/bench/data/raw/own-synthetic/sdtm"
tlfpath  <- "/bench/output/sas/tlf"

eff_param <- "ACTOT"
y_label   <- "Change from Baseline in ADAS-Cog Total Score"

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
})

cat("NOTE: Starting P081_f_mean_cfb translation\n")

# ---- Helper to read SAS library (directory of .sas7bdat / .xpt) ----
read_adam_member <- function(libpath, memname) {
  memlow <- tolower(memname)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  cand <- files[tolower(tools::file_path_sans_ext(basename(files))) == memlow]
  if (length(cand) == 0) {
    stop(paste0("Member ", memname, " not found in ", libpath))
  }
  f <- cand[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  df <- dplyr::mutate(df, dplyr::across(where(haven::is.labelled), haven::zap_labels))
  df
}

# ---- Step 1: Prepare analysis data ----
cat("NOTE: Reading adam.adeff and adam.adsl\n")
adeff <- read_adam_member(adampath, "adeff")
adsl  <- read_adam_member(adampath, "adsl")

names(adeff) <- toupper(names(adeff))
names(adsl)  <- toupper(names(adsl))

eff <- adeff %>%
  inner_join(adsl %>% select(USUBJID, EFFFL), by = "USUBJID") %>%
  filter(EFFFL == "Y",
         PARAMCD == eff_param,
         ANL01FL == "Y",
         !is.na(CHG)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, AVAL, BASE, CHG, ANL01FL) %>%
  arrange(TRTAN, AVISITN)

cat(paste0("NOTE: eff rows after filter: ", nrow(eff), "\n"))

# ---- Step 2: Compute summary statistics by visit and treatment ----
if (nrow(eff) > 0) {
  stats <- eff %>%
    group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
    summarise(
      n = sum(!is.na(CHG)),
      mean = ifelse(n > 0, mean(CHG, na.rm = TRUE), NA_real_),
      sd = ifelse(n > 1, sd(CHG, na.rm = TRUE), NA_real_),
      se = ifelse(n > 0, sd(CHG, na.rm = TRUE) / sqrt(n), NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
      lcl = mean - tcrit * se,
      ucl = mean + tcrit * se
    ) %>%
    select(-sd, -tcrit) %>%
    arrange(TRTAN, AVISITN)
} else {
  stats <- tibble(TRTA=character(), TRTAN=numeric(), AVISIT=character(), AVISITN=numeric(),
                  n=numeric(), mean=numeric(), se=numeric(), lcl=numeric(), ucl=numeric())
}

cat(paste0("NOTE: stats rows: ", nrow(stats), "\n"))

# ---- Step 3: Small offset to prevent overlapping points ----
plot_data <- stats %>%
  mutate(
    xpos = case_when(
      TRTAN == 1 ~ AVISITN - 0.15,
      TRTAN == 2 ~ AVISITN,
      TRTAN == 3 ~ AVISITN + 0.15,
      TRUE ~ as.numeric(AVISITN)
    )
  ) %>%
  arrange(TRTAN, AVISITN)

plot_data <- plot_data %>%
  mutate(TRTA = as.character(TRTA))

out_csv <- file.path(out, "f_mean_cfb.csv")
write.csv(plot_data, out_csv, row.names = FALSE, na = "")

cat(paste0("NOTE: Wrote ", out_csv, "\n"))

if (nrow(plot_data) > 0) {
  ntable <- plot_data %>% select(TRTA, AVISIT, AVISITN, n) %>% arrange(TRTAN, AVISITN)
  write.csv(ntable, file.path(out, "f_mean_cfb_ntable.csv"), row.names = FALSE, na = "")
}

# ---- Step 4: Generate figure ----
title4 <- "Figure 14.2.4"
title5 <- "Mean Change from Baseline Over Time"
title6 <- "Efficacy Analysis Set"
foot1 <- "Data cutoff: [DDMONYYYY]."
foot2 <- "Error bars represent 95% confidence intervals."
foot3 <- "n = number of subjects with non-missing change from baseline."
foot4 <- "Program: f_mean_cfb.sas"

x_breaks <- sort(unique(plot_data$AVISITN))
x_labels <- plot_data %>%
  distinct(AVISITN, AVISIT) %>%
  arrange(AVISITN) %>%
  pull(AVISIT)

if (length(x_breaks) == 0) {
  x_breaks <- pretty(c(0, 1), n = 5)
  x_labels <- as.character(x_breaks)
}

trt_levels <- plot_data %>%
  arrange(TRTAN) %>%
  distinct(TRTA) %>%
  pull(TRTA)

plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_levels)

pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
pal <- pal[seq_along(trt_levels)]

p <- ggplot(plot_data, aes(x = xpos, y = mean, group = TRTA, color = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl, color = TRTA),
                width = 0.08, linewidth = 0.8, alpha = 0.9) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(color = TRTA), size = 3, shape = 19) +
  scale_color_manual(values = pal, name = NULL) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    name = "Visit (Week)"
  ) +
  scale_y_continuous(name = y_label) +
  labs(
    title = paste0(title4, "\n", title5, "\n", title6),
    caption = paste(foot1, foot2, foot3, foot4, sep = "\n")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, lineheight = 1.1),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(linetype = "dotted", color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 7, lineheight = 0.9),
    plot.caption.position = "plot"
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

out_png <- file.path(out, "f_mean_cfb.png")
ggsave(filename = out_png, plot = p, width = 10, height = 6.5, units = "in", dpi = 300, bg = "white")
cat(paste0("NOTE: Wrote ", out_png, "\n"))

# ---- Plain-text rendering for figure ----
out_txt <- file.path(out, "f_mean_cfb.txt")
txt_con <- file(out_txt, open = "wt")
writeLines(c(
  "Protocol: [Protocol Number]",
  paste0(title4),
  paste0(title5),
  paste0(title6),
  "",
  paste0("Y-axis: ", y_label),
  paste0("X-axis: Visit (Week)"),
  "",
  "Summary statistics by visit and treatment (plotted data):",
  ""
), txt_con)
close(txt_con)
if (nrow(plot_data) > 0) {
  fmt_num <- function(x, digits = 2) {
    ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, flag = "#"))
  }
  txt_df <- plot_data %>%
    mutate(
      n_fmt = as.character(n),
      mean_fmt = fmt_num(mean, 2),
      se_fmt = fmt_num(se, 3),
      lcl_fmt = fmt_num(lcl, 2),
      ucl_fmt = fmt_num(ucl, 2)
    ) %>%
    select(TRTA, AVISIT, AVISITN, n_fmt, mean_fmt, se_fmt, lcl_fmt, ucl_fmt, xpos)
  header <- paste(c("TRTA","AVISIT","AVISITN","n","mean","SE","LCL","UCL","xpos"), collapse = " | ")
  write(header, file = out_txt, append = TRUE)
  write(paste(rep("-", nchar(header)), collapse = ""), file = out_txt, append = TRUE)
  for (i in seq_len(nrow(txt_df))) {
    line <- paste(txt_df[i, ], collapse = " | ")
    write(line, file = out_txt, append = TRUE)
  }
}
writeLines(c(
  "",
  foot1,
  foot2,
  foot3,
  foot4
), con = file(out_txt, open = "at"))

cat("NOTE: f_mean_cfb.sas completed successfully.\n")
