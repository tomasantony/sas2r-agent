# P081_f_mean_cfb.sas -> R translation
# Figure 14.2.4 - Mean Change from Baseline Over Time
# Input: adam.adeff, adam.adsl
# Output: f_mean_cfb.png + f_mean_cfb.csv (plotted data)

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

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
tlfpath   <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"

# Parameters from SAS program
eff_param <- "ACTOT"
y_label   <- "Change from Baseline in ADAS-Cog Total Score"
cutdate   <- "[DDMONYYYY]"
sysin     <- "f_mean_cfb.sas"

# Helper: find dataset file (sas7bdat or xpt, case-insensitive)
find_dataset_file <- function(libpath, dsname) {
  if (!dir.exists(libpath)) return(NA_character_)
  files <- list.files(libpath, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  # normalize for comparison
  lower_files <- tolower(files)
  lower_ds <- tolower(dsname)
  # candidates: exact name with extensions
  cand_idx <- which(
    lower_files == tolower(file.path(libpath, paste0(dsname, ".sas7bdat"))) |
    lower_files == tolower(file.path(libpath, paste0(dsname, ".xpt"))) |
    lower_files == tolower(file.path(libpath, paste0(dsname, ".sas7bdat"))) |
    grepl(paste0("/", lower_ds, "\\.(sas7bdat|xpt)$"), lower_files)
  )
  if (length(cand_idx) > 0) return(files[cand_idx[1]])
  # fallback: any file whose basename without ext equals dsname (case-insensitive)
  basenoext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx2 <- which(basenoext == lower_ds)
  if (length(idx2) > 0) return(files[idx2[1]])
  return(NA_character_)
}

read_adam <- function(dsname) {
  f <- find_dataset_file(adam_path, dsname)
  if (is.na(f) || !file.exists(f)) {
    # try alternative derived path
    alt_path <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
    f2 <- find_dataset_file(alt_path, dsname)
    if (!is.na(f2) && file.exists(f2)) f <- f2 else stop(paste0("Dataset not found: ", dsname, " in ", adam_path))
  }
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    stop(paste0("Unknown extension for ", f))
  }
  # convert haven_labelled to plain vectors, keep names as is
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven may keep labelled class; convert
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      df[[nm]] <- haven::as_factor(df[[nm]], levels = "default")
      # if factor, try to keep original numeric if needed; but for TRTAN etc keep numeric
      # haven::as_factor converts to factor; we want numeric for TRTAN/AVISITN if possible
      # So re-read as numeric if original was numeric labelled
      # Instead use zap_labels
      df[[nm]] <- haven::zap_labels(haven::read_sas(f)[[nm]])
      if (is.null(df[[nm]])) df[[nm]] <- haven::zap_labels(haven::read_xpt(f)[[nm]])
    }
  }
  # zap labels for all
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  return(df)
}

cat("Reading ADSL and ADEFF...\n")
adsl <- read_adam("adsl")
adeff <- read_adam("adeff")

# Ensure expected columns exist
stopifnot(all(c("USUBJID", "EFFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID", "PARAMCD", "ANL01FL", "CHG", "TRTA", "TRTAN", "AVISIT", "AVISITN") %in% names(adeff)))

# Step 1: Prepare analysis data (PROC SQL)
# select a.USUBJID, a.TRTA, a.TRTAN, a.PARAMCD, a.PARAM, a.AVISIT, a.AVISITN, a.AVAL, a.BASE, a.CHG, a.ANL01FL
# from adam.adeff a inner join adam.adsl b on a.USUBJID = b.USUBJID
# where b.EFFFL='Y' and a.PARAMCD="ACTOT" and a.ANL01FL='Y' and a.CHG ne .

# Keep only EFFFL='Y' subjects
adsl_eff <- adsl %>%
  filter(!is.na(EFFFL) & EFFFL == "Y") %>%
  select(USUBJID, EFFFL)

# If PARAMCD ACTOT not present, fallback to first available PARAMCD (defensive)
if (!any(adeff$PARAMCD == eff_param, na.rm = TRUE)) {
  cat(paste0("WARNING: PARAMCD ", eff_param, " not found. Available: ", paste(unique(adeff$PARAMCD), collapse=", "), "\n"))
  # keep as is -> will result in 0 rows, but try to use first PARAMCD
  # Do not change eff_param to keep fidelity; just warn
}

eff <- adeff %>%
  inner_join(adsl_eff, by = "USUBJID") %>%
  filter(PARAMCD == eff_param,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(CHG)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, AVAL, BASE, CHG, ANL01FL) %>%
  arrange(TRTAN, AVISITN)

cat(paste0("EFF rows: ", nrow(eff), "\n"))

# Step 2: Compute summary statistics by visit and treatment (PROC MEANS nway)
# class TRTA TRTAN AVISIT AVISITN; var CHG; output n=n mean=mean stderr=se lclm=lcl uclm=ucl
# SAS lclm/uclm = mean +/- t_{0.975, n-1} * SE

stats <- eff %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(
    n = sum(!is.na(CHG)),
    mean = mean(CHG, na.rm = TRUE),
    sd = sd(CHG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se = ifelse(n > 0, sd / sqrt(n), NA_real_),
    # t critical value
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lcl = mean - tcrit * se,
    ucl = mean + tcrit * se
  ) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl) %>%
  arrange(TRTAN, AVISITN)

# Ensure numeric types
stats <- stats %>%
  mutate(
    TRTAN = as.numeric(TRTAN),
    AVISITN = as.numeric(AVISITN),
    n = as.integer(n)
  )

# Step 3: Small offset to prevent overlapping points
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

# Write CSV of plotted data (for QC)
# Keep SAS variable names: TRTA TRTAN AVISIT AVISITN n mean se lcl ucl xpos
# Also include stderr naming consistency: se is stderr
plot_data_out <- plot_data %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl, xpos)

write.csv(plot_data_out, file.path(out, "f_mean_cfb.csv"), row.names = FALSE, na = "")

# Also write a txt rendering of the table body
txt_file <- file.path(out, "f_mean_cfb.txt")
sink(txt_file)
cat("Figure 14.2.4\n")
cat("Mean Change from Baseline Over Time\n")
cat("Efficacy Analysis Set\n")
cat("\n")
cat(paste0("Analysis Parameter: ", eff_param, " (", y_label, ")\n"))
cat(paste0("Population: Efficacy Analysis Set (EFFFL='Y', ANL01FL='Y', PARAMCD='", eff_param, "')\n"))
cat("\n")
cat("Summary Statistics by Visit and Treatment (Mean Change from Baseline, 95% CI)\n")
cat("--------------------------------------------------------------------------------\n")
# Print table sorted by AVISITN then TRTAN
print_df <- plot_data_out %>% arrange(AVISITN, TRTAN)
# Format numbers to 4 decimals like SAS default, but keep readable
fmt4 <- function(x) ifelse(is.na(x), "", sprintf("%.4f", x))
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", x))
# Create display table
disp <- print_df %>%
  mutate(
    mean_f = sprintf("%.4f", mean),
    se_f = ifelse(is.na(se), "", sprintf("%.4f", se)),
    lcl_f = ifelse(is.na(lcl), "", sprintf("%.4f", lcl)),
    ucl_f = ifelse(is.na(ucl), "", sprintf("%.4f", ucl))
  ) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean_f, se_f, lcl_f, ucl_f, xpos)
print(as.data.frame(disp), row.names = FALSE)
cat("\n")
cat("Error bars represent 95% confidence intervals (mean +/- t(0.975, n-1) * SE).\n")
cat("n = number of subjects with non-missing change from baseline.\n")
cat(paste0("Data cutoff: ", cutdate, "\n"))
cat(paste0("Source: ADaM datasets (adam.adeff, adam.adsl)\n"))
cat(paste0("Program: ", sysin, " | Run: ", format(Sys.Date(), "%d%b%Y"), "\n"))
sink()

# Step 4: Generate figure (PROC SGPLOT)
# Need to handle TRTA ordering by TRTAN
trt_levels <- plot_data %>% arrange(TRTAN) %>% distinct(TRTA) %>% pull(TRTA)
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_levels)

# Determine x-axis breaks and labels
x_breaks <- sort(unique(plot_data$AVISITN))
# Map AVISITN -> AVISIT label (first occurrence)
x_labels_df <- plot_data %>% arrange(AVISITN) %>% distinct(AVISITN, AVISIT)
x_labels <- x_labels_df$AVISIT
names(x_labels) <- x_labels_df$AVISITN
# If AVISIT contains visit names, use them; otherwise use numeric
x_label_vec <- x_labels[as.character(x_breaks)]
# Fallback to numeric if labels are NA
x_label_vec[is.na(x_label_vec)] <- as.character(x_breaks[is.na(x_label_vec)])

# Color palette - journal style, distinct
# Use default ggplot2 hues but ensure 3 groups
n_trt <- length(trt_levels)
palette_vals <- scales::hue_pal()(n_trt)
# Alternatively use manual: c("#1f77b4", "#ff7f0e", "#2ca02c")
# Keep hue_pal for consistency

p <- ggplot(plot_data, aes(x = xpos, y = mean, group = TRTA, color = TRTA, fill = TRTA)) +
  # Error bars (95% CI) - highlow type=bar barwidth=0.05
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.05, linewidth = 0.6, na.rm = TRUE) +
  # Mean values connected by lines
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(shape = 21, size = 3, stroke = 0.8, na.rm = TRUE) +
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_label_vec,
    name = "Visit (Week)"
  ) +
  scale_y_continuous(
    name = y_label
  ) +
  scale_color_manual(values = palette_vals) +
  scale_fill_manual(values = palette_vals) +
  labs(
    title = "Figure 14.2.4\nMean Change from Baseline Over Time\nEfficacy Analysis Set",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, lineheight = 1.1),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(linetype = "dotted", color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 7, lineheight = 1.1),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE))

# Add caption with footnotes (SAS footnote1-4)
caption_text <- paste0(
  "Data cutoff: ", cutdate, "\n",
  "Error bars represent 95% confidence intervals.\n",
  "n = number of subjects with non-missing change from baseline.\n",
  "Program: ", sysin, " | Run: ", format(Sys.Date(), "%d%b%Y")
)
p <- p + labs(caption = caption_text)

# Save figure as PNG (SAS was EMF 10in x 6.5in)
ggsave(
  filename = file.path(out, "f_mean_cfb.png"),
  plot = p,
  width = 10, height = 6.5, units = "in", dpi = 300, bg = "white"
)

# Also save plotted data for figure reproducibility (already saved as f_mean_cfb.csv)
# Ensure supporting n per visit table is also available as CSV (optional)
ntable <- plot_data_out %>% select(TRTA, TRTAN, AVISIT, AVISITN, n) %>% arrange(TRTAN, AVISITN)
write.csv(ntable, file.path(out, "f_mean_cfb_ntable.csv"), row.names = FALSE, na = "")

cat("f_mean_cfb completed successfully.\n")
cat(paste0("Outputs written to: ", out, "\n"))
