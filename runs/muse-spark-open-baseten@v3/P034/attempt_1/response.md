```r
# P034_target13.sas -> R translation
# Figure: Arithmetic Mean concentration-time plot per analyte (ANAL1) with SD bars
# Outputs: target13.png + target13.csv (plotted data)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS behaviour)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file adpc.xpt
find_adpc <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADPC.xpt",
    file.path(Sys.getenv("rawadam"), "adpc.xpt"),
    file.path(Sys.getenv("rawadam"), "ADPC.xpt"),
    "./data/raw/cdisc-pilot/adam/adpc.xpt",
    "./data/raw/cdisc-pilot/adam/ADPC.xpt",
    "/tmp/data/raw/cdisc-pilot/adam/adpc.xpt",
    "data/raw/cdisc-pilot/adam/adpc.xpt"
  )
  # also search recursively from root if exists
  roots <- c(Sys.getenv("root"), "D:/meta_hackathon/sas2r-agent", ".", "/tmp")
  for (r in roots) {
    if (!is.na(r) && nzchar(r) && dir.exists(r)) {
      f <- list.files(r, pattern = "^adpc\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
      if (length(f) > 0) candidates <- c(candidates, f)
    }
  }
  candidates <- unique(candidates)
  for (p in candidates) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(p)
  }
  return(NA_character_)
}

adpc_path <- find_adpc()

# fallback: also check inlib derived path
if (is.na(adpc_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt"
  if (file.exists(alt)) adpc_path <- alt
}
if (is.na(adpc_path) || !file.exists(adpc_path)) {
  # try haven search via pharmaversesdtm? last resort: create empty to avoid crash
  stop(paste0("adpc.xpt not found. Searched candidates. OUT_DIR=", out))
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)

# Read adpc.xpt
adpc_raw <- haven::read_xpt(adpc_path)

# Ensure column names upper case as in SAS (haven preserves)
# Work with whatever case, convert to upper for logic then keep original
names(adpc_raw) <- toupper(names(adpc_raw))

# Keep required columns if exist
keep_cols <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
               "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keep_cols <- intersect(keep_cols, names(adpc_raw))
adpc_raw <- adpc_raw[, keep_cols, drop = FALSE]

# Filter PCTESTCD == ANAL1 (macro call %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH))
PCTESTCD_filter <- "ANAL1"
adpc <- adpc_raw %>%
  filter(toupper(PCTESTCD) == toupper(PCTESTCD_filter))

# PCTPT = input(EPLTM, best10.)  -> numeric conversion
# EPLTM may be character like "0", "4", "8" etc. Use as.numeric
adpc <- adpc %>%
  mutate(
    PCTPT = suppressWarnings(as.numeric(as.character(EPLTM))),
    ORDERBY = paste0(ANLPRNT, PCSPEC, PCTESTL, TRTAN),
    BYVAL6 = paste0(stringr::str_to_title(PCTESTCD), " Conc ", " (", PCORRESU, ")")
  )

# Sort by TRTAN TRTA PCTPT (SAS sort)
adpc <- adpc %>%
  arrange(TRTAN, TRTA, PCTPT)

# PROC UNIVARIATE by TRTAN TRTA PCTPT var PCORRESN output mean and std
# SAS STD is sample std (n-1). R sd() matches.
# Need to handle missing PCORRESN
MeanData <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    PCMEAN = ifelse(N > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
    PCSD = ifelse(N > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(TRTAN, PCTPT)

# if PCMEAN == 0 then set to missing (SAS)
MeanData <- MeanData %>%
  mutate(
    PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
    PCSD = ifelse(is.na(PCMEAN), NA_real_, PCSD)
  )

# For plotting, need TRTA label; ensure factor ordered by TRTAN
MeanData <- MeanData %>%
  mutate(
    TRTA_f = factor(TRTA, levels = unique(TRTA[order(TRTAN)])),
    TRTAN_f = factor(TRTAN)
  )

# Determine axis limits
# SAS dynamic _XMAX=100, TICKS="0 4 8 12 24 36 48 72 96"
x_ticks <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
x_max <- 100
# _YMAX not supplied in sgrender; compute from data
y_max_linear <- suppressWarnings(max(MeanData$PCMEAN + MeanData$PCSD, na.rm = TRUE))
if (!is.finite(y_max_linear)) y_max_linear <- suppressWarnings(max(MeanData$PCMEAN, na.rm = TRUE))
if (!is.finite(y_max_linear) || y_max_linear == 0) y_max_linear <- 10
# round up to nice value
y_max_linear <- ceiling(y_max_linear * 1.1)
# For log view, viewmax = _YMAX (same). Use same y_max_linear but log scale needs >0
y_max_log <- y_max_linear

# Y label from SAS: "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
y_label <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
x_label <- "Time (h)"

# Write CSV of plotted data (MeanData)
out_csv <- file.path(out, "target13.csv")
# Ensure dates not needed; write numeric
write.csv(MeanData, out_csv, row.names = FALSE, na = "")

# Also write dataset for verification (same as CSV)
# Create plots
# Common theme
base_theme <- theme_bw() +
  theme(
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key = element_blank()
  )

# Linear view
p_linear <- ggplot(MeanData, aes(x = PCTPT, y = PCMEAN, group = TRTA_f, colour = TRTA_f, shape = TRTA_f, linetype = TRTA_f)) +
  geom_line(linewidth = 0.6, colour = "black") +
  geom_point(size = 2, colour = "black", fill = "white") +
  geom_errorbar(aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD), width = 1.5, colour = "black", linewidth = 0.4, na.rm = TRUE) +
  scale_x_continuous(limits = c(0, x_max), breaks = x_ticks, labels = x_ticks) +
  scale_y_continuous(limits = c(0, y_max_linear), breaks = scales::pretty_breaks(6)) +
  labs(title = "Linear view", x = x_label, y = y_label) +
  base_theme +
  theme(plot.title = element_text(size = 9, face = "plain", hjust = 0.5)) +
  scale_colour_grey(start = 0, end = 0) +
  guides(colour = "none", shape = "none", linetype = "none")

# Semilog view - y log base 10, viewmax = _YMAX, viewmin default >0
# Need to handle zero/NA on log scale: filter >0
MeanData_log <- MeanData %>% filter(!is.na(PCMEAN) & PCMEAN > 0)

# Determine log breaks: use pretty log breaks
p_semilog <- ggplot(MeanData_log, aes(x = PCTPT, y = PCMEAN, group = TRTA_f, colour = TRTA_f, shape = TRTA_f, linetype = TRTA_f)) +
  geom_line(linewidth = 0.6, colour = "black") +
  geom_point(size = 2, colour = "black", fill = "white") +
  scale_x_continuous(limits = c(0, x_max), breaks = x_ticks, labels = x_ticks) +
  scale_y_log10(limits = c(NA, y_max_log)) +
  annotation_logticks(sides = "l", colour = "grey50", linewidth = 0.3) +
  labs(title = "Semilogarithmic view", x = x_label, y = y_label) +
  base_theme +
  theme(plot.title = element_text(size = 9, face = "plain", hjust = 0.5)) +
  scale_colour_grey(start = 0, end = 0) +
  guides(colour = "none", shape = "none", linetype = "none")

# Build combined figure with titles/footnotes similar to SAS
# Titles:
# title J=L "SPONSOR/PROTOCOL/PRODUCT INFO" j=R "page x of y";
# title3 "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately";
# title4 "Analysis set: PK analysis set";
# footnote j=l "PATH DATA/PROGRAM/OUTPUT";

# Create legend grob from one plot with legend
# Build a plot for legend extraction
p_legend_source <- ggplot(MeanData, aes(x = PCTPT, y = PCMEAN, group = TRTA_f, colour = TRTA_f, shape = TRTA_f, linetype = TRTA_f)) +
  geom_line(aes(linetype = TRTA_f), colour = "black") +
  geom_point(aes(shape = TRTA_f), colour = "black", fill = "white", size = 2) +
  scale_linetype_discrete(name = NULL) +
  scale_shape_discrete(name = NULL) +
  scale_colour_grey(start = 0, end = 0, guide = "none") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 11))

# Extract legend
g_legend <- function(a.gplot) {
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) tmp$grobs[[leg]] else nullGrob()
}
legend_grob <- g_legend(p_legend_source)

# Prepare grobs for linear and semilog
g_linear <- ggplotGrob(p_linear)
g_semilog <- ggplotGrob(p_semilog)

# Output PNG
out_png <- file.path(out, "target13.png")
png_width_cm <- 22.87
png_height_cm <- 12
# Use ggsave with grid arrangement via grid
# Create combined layout using grid
# Design: top row 2 columns, bottom row legend
# We'll use grid.arrange manually

# Open png device via ggsave alternative: use png() then grid.draw
# Use ggsave for simplicity with custom grid object
# Build final gtable with grid

# Create a function to draw combined figure
draw_combined <- function() {
  grid.newpage()
  # Layout: 4 rows: title1, title2, plots, footnote/legend
  # Use viewports
  pushViewport(viewport(layout = grid.layout(
    nrow = 6, ncol = 2,
    heights = unit(c(0.6, 0.6, 0.6, 8, 1.2, 0.5), "cm"),
    widths = unit(c(0.5, 0.5), "npc")
  )))
  # Title row 1: left and right
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.text("SPONSOR/PROTOCOL/PRODUCT INFO", x = unit(0.01, "npc"), y = unit(0.5, "npc"),
            just = c("left", "center"), gp = gpar(fontsize = 8))
  grid.text("page x of y", x = unit(0.99, "npc"), y = unit(0.5, "npc"),
            just = c("right", "center"), gp = gpar(fontsize = 8))
  popViewport()
  # Title3
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
  grid.text("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",
            x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "center", gp = gpar(fontsize = 8))
  popViewport()
  # Title4
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
  grid.text("Analysis set: PK analysis set",
            x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "center", gp = gpar(fontsize = 8))
  popViewport()
  # Plots
  pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
  grid.draw(g_linear)
  popViewport()
  pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2))
  grid.draw(g_semilog)
  popViewport()
  # Legend
  pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1:2))
  grid.draw(legend_grob)
  popViewport()
  # Footnote
  pushViewport(viewport(layout.pos.row = 6, layout.pos.col = 1:2))
  grid.text("PATH DATA/PROGRAM/OUTPUT", x = unit(0.01, "npc"), y = unit(0.5, "npc"),
            just = c("left", "center"), gp = gpar(fontsize = 7))
  popViewport()
  popViewport()
}

# Save via png device
png(filename = out_png, width = png_width_cm, height = png_height_cm, units = "cm", res = 300, bg = "white")
draw_combined()
dev.off()

# Also create a ggsave version for compatibility (ensure file exists)
# If png failed, fallback to ggsave of linear plot
if (!file.exists(out_png) || file.info(out_png)$size == 0) {
  ggsave(filename = out_png, plot = p_linear, width = png_width_cm, height = png_height_cm, units = "cm", dpi = 300)
}

# Write TXT rendering of table body (counts)
out_txt <- file.path(out, "target13.txt")
txt_con <- file(out_txt, "w")
writeLines(c(
  "SPONSOR/PROTOCOL/PRODUCT INFO                                                          page x of y",
  "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",
  "Analysis set: PK analysis set",
  "",
  paste0("PCTESTCD filter: ", PCTESTCD_filter, "  Meantype: A  SD: BOTH"),
  paste0("XMAX=", x_max, "  YMAX=", y_max_linear, "  TICKS=", paste(x_ticks, collapse = " ")),
  "",
  "MeanData (Arithmetic mean +/- SD) by TRTAN/TRTA/PCTPT:",
  ""
), txt_con)
close(txt_con)
# Append MeanData as formatted table
write.table(MeanData, file = out_txt, append = TRUE, sep = "\t", row.names = FALSE, na = "")

# Ensure CSV of plotted data also written as target13_data.csv for compatibility
write.csv(MeanData, file.path(out, "target13_data.csv"), row.names = FALSE, na = "")

```