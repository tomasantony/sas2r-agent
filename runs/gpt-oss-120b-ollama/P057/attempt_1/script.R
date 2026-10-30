# ------------------------------------------------------------
# R translation of SAS program P057_pk_mean_conc.sas
# Generates target13.png and MeanData.csv in OUT_DIR
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS style) – not needed for
# calculations but kept for possible future use
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10^digits
  ((floor(abs(x) * factor + 0.5)) / factor) * ifelse(pos, 1, -1)
}

# ------------------------------------------------------------------
# Paths (hard‑coded as in the original SAS macro variables)
adpc_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"

# Output directory (environment variable set by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# Read source ADaM dataset
adpc_raw <- read_xpt(adpc_path)

# ------------------------------------------------------------------
# Macro parameters (as called in the SAS program)
pctestcd   <- "ANAL1"   # PCTESTCD to plot
meantype   <- "A"       # "A" = arithmetic, "G" = geometric
sd_option  <- "BOTH"   # "BOTH", "UPPER", or "N"

# ------------------------------------------------------------------
# Prepare working ADPC data (keep only needed variables)
adpc <- adpc_raw %>%
  select(ANLPRNT, TRTAN, PCSPEC, PCTESTL, TRTA, DOSREFID,
         PCTESTCD, PCORRESU, PCORRES, USUBJID, PCORRESN, EPLTM) %>%
  filter(toupper(PCTESTCD) == toupper(pctestcd)) %>%
  mutate(
    PCTPT = as.numeric(as.character(EPLTM)),               # time variable
    ORDERBY = paste0(ANLPRNT, PCSPEC, PCTESTL, TRTAN),
    BYVAL6 = paste0(tolower(PCTESTCD), " Conc (", PCORRESU, ")")
  ) %>%
  arrange(TRTAN, TRTA, PCTPT)

# ------------------------------------------------------------------
# Compute means / SDs per treatment and time
if (toupper(meantype) == "A") {
  mean_data <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      PCMEAN = ifelse(all(is.na(PCORRESN)), NA_real_, mean(PCORRESN, na.rm = TRUE)),
      PCSD   = ifelse(all(is.na(PCORRESN)), NA_real_, sd(PCORRESN,  na.rm = TRUE)),
      .groups = "drop"
    )
} else if (toupper(meantype) == "G") {
  mean_data <- adpc %>%
    filter(!is.na(PCORRESN)) %>%
    mutate(LOGORRES = log(PCORRESN)) %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      LogMean = mean(LOGORRES, na.rm = TRUE),
      LogSD   = sd(LOGORRES,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      PCMEAN = exp(LogMean),
      PCSD   = exp(LogSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD)
} else {
  stop("MEANTYPE must be 'A' or 'G'")
}

# Apply SAS rule: if mean is exactly 0, set mean and SD to missing
mean_data <- mean_data %>%
  mutate(
    PCMEAN = ifelse(PCMEAN == 0, NA_real_, PCMEAN),
    PCSD   = ifelse(is.na(PCMEAN), NA_real_, PCSD)
  )

# ------------------------------------------------------------------
# Save the MeanData dataset (CSV)
write.csv(
  mean_data,
  file = file.path(out_dir, "MeanData.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# Plotting parameters (mirroring the SAS PROC TEMPLATE call)
x_label   <- "Time (h)"
y_label   <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
x_max     <- 100
y_max     <- max(mean_data$PCMEAN + ifelse(sd_option == "BOTH", mean_data$PCSD, 0), na.rm = TRUE) * 1.05
ticks_str <- "0 4 8 12 24 36 48 72 96"
ticks_vec <- as.numeric(strsplit(ticks_str, "\\s+")[[1]])

# ------------------------------------------------------------------
# Base ggplot object (common aesthetics)
base_gg <- ggplot(mean_data, aes(x = PCTPT, y = PCMEAN, colour = factor(TRTAN), group = factor(TRTAN))) +
  scale_colour_discrete(name = "Treatment") +
  labs(x = x_label, y = y_label) +
  theme_minimal(base_size = 10) +
  theme(
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 11),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = ticks_vec, limits = c(0, x_max))

# ------------------------------------------------------------------
# Linear scale panel
p_linear <- base_gg +
  geom_line(size = 0.6) +
  {
    if (sd_option == "BOTH") {
      geom_errorbar(aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD), width = 0.2, size = 0.4)
    } else if (sd_option == "UPPER") {
      geom_errorbar(aes(ymin = PCMEAN, ymax = PCMEAN + PCSD), width = 0.2, size = 0.4)
    } else {
      NULL
    }
  } +
  scale_y_continuous(limits = c(0, y_max)) +
  ggtitle("Linear view")

# ------------------------------------------------------------------
# Semi‑logarithmic (log‑y) panel
p_semi <- base_gg +
  geom_line(size = 0.6) +
  geom_point(size = 1.2) +   # points to mimic scatterplot
  scale_y_log10(limits = c(0.1, y_max), labels = scales::comma) +
  ggtitle("Semilogarithmic view")

# ------------------------------------------------------------------
# Extract shared legend
g_legend <- function(a.gplot){
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}
shared_legend <- g_legend(p_linear)

# ------------------------------------------------------------------
# Arrange the two panels and legend (2 columns, 2 rows; rowweights .9 .1)
panel_grid <- arrangeGrob(
  p_linear, p_semi,
  ncol = 2,
  widths = unit(c(0.5, 0.5), "npc")
)

final_plot <- arrangeGrob(
  panel_grid,
  shared_legend,
  ncol = 1,
  heights = unit(c(0.9, 0.1), "npc")
)

# ------------------------------------------------------------------
# Save the figure as PNG (size matches ODS GRAPHICS dimensions)
png_file <- file.path(out_dir, "target13.png")
ggsave(
  filename = png_file,
  plot = final_plot,
  width = 22.87, height = 12, units = "cm", dpi = 300, device = "png"
)

# ------------------------------------------------------------------
# Also write the plotted data (MeanData) as CSV (already done above)
# End of script
