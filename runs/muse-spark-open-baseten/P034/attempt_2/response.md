```r
# P034_target13.sas -> R translation
# Figure: Arithmetic Mean concentration-time plot (linear + semilog)

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
  library(grid)
})

# helper: SAS round half away from zero
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Input paths (from macro variables)
# &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
# &rawsplit. etc.
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

adpc_path_xpt <- file.path(rawadam, "adpc.xpt")
adpc_path2 <- file.path(inlib_dir, "adpc.xpt")

adpc <- NULL
if (file.exists(adpc_path_xpt)) {
  adpc <- haven::read_xpt(adpc_path_xpt)
} else if (file.exists(adpc_path2)) {
  adpc <- haven::read_xpt(adpc_path2)
} else {
  # fallback: try to find adpc.xpt under OUT_DIR parent
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "adpc\\.xpt$", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) adpc <- haven::read_xpt(cand[1])
}
if (is.null(adpc)) stop("adpc.xpt not found")

# Keep original for reference (work.orig_adpc)
orig_adpc <- adpc
# Write orig dataset as CSV (dataset output)
write.csv(orig_adpc, file.path(out, "orig_adpc.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Macro parameters for this call:
PCTESTCD_param <- "ANAL1"
Meantype <- "A"  # A = arithmetic, G = geometric
SD_param <- "BOTH" # BOTH, UPPER, N

# data work.adpc ;
# keep ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
# where PCTESTCD="&PCTESTCD";
# PCTPT=input(EPLTM,best10.);
# ORDERBY=compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN);
# BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')');

# Ensure character columns are character (haven may import as labelled)
adpc_work <- adpc %>%
  filter(PCTESTCD == PCTESTCD_param) %>%
  mutate(
    # SAS INPUT(EPLTM,best10.) : convert character to numeric, missing if not numeric
    PCTPT = suppressWarnings(as.numeric(as.character(EPLTM))),
    # ORDERBY not used further but reproduce
    ORDERBY = gsub(" +", "", paste0(as.character(ANLPRNT), as.character(PCSPEC), as.character(PCTESTL), as.character(TRTAN))),
    # BYVAL6 = propcase(pctestcd) + ' Conc ' + ' (' + PCORRESU + ')'
    # propcase: first letter upper, rest lower
    BYVAL6 = trimws(gsub("\\s+", " ",
      paste0(stringr::str_to_title(tolower(as.character(PCTESTCD))), " Conc ", " (", as.character(PCORRESU), ")")
    ))
  )

# proc sort by TRTAN TRTA PCTPT
adpc_work <- adpc_work %>%
  arrange(TRTAN, TRTA, PCTPT)

# Write intermediate ADPC for traceability
write.csv(adpc_work, file.path(out, "adpc_filtered.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# proc univariate by TRTAN TRTA PCTPT var PCORRESN output mean STD
# SAS STD is sample std (n-1). R sd() matches.
# Need to handle groups where all missing -> mean = NA, sd = NA

if (toupper(Meantype) == "A") {
  pcmeans <- adpc_work %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      PCMEAN = if (all(is.na(PCORRESN))) NA_real_ else mean(PCORRESN, na.rm = TRUE),
      PCSD   = if (all(is.na(PCORRESN))) NA_real_ else sd(PCORRESN, na.rm = TRUE),
      N      = sum(!is.na(PCORRESN)),
      .groups = "drop"
    ) %>%
    arrange(TRTAN, TRTA, PCTPT)

  MeanData <- pcmeans %>%
    mutate(
      PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
      PCSD   = ifelse(is.na(PCMEAN), NA_real_, PCSD)
    )

} else if (toupper(Meantype) == "G") {
  logpc <- adpc_work %>%
    mutate(LOGORRES = ifelse(!is.na(PCORRESN), log(PCORRESN), NA_real_))

  logmeans <- logpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      Logmean = if (all(is.na(PCORRESN))) NA_real_ else mean(PCORRESN, na.rm = TRUE),
      logSD   = if (all(is.na(PCORRESN))) NA_real_ else sd(PCORRESN, na.rm = TRUE),
      .groups = "drop"
    )
  MeanData <- logmeans %>%
    mutate(
      PCMEAN = exp(Logmean),
      PCSD   = exp(logSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD)
} else {
  stop("Meantype must be A or G")
}

# Ensure sorting
MeanData <- MeanData %>% arrange(TRTAN, TRTA, PCTPT)

# Write dataset outputs
write.csv(MeanData, file.path(out, "MeanData.csv"), row.names = FALSE, na = "")
write.csv(MeanData, file.path(out, "target13.csv"), row.names = FALSE, na = "")
# Also write PCMEANS equivalent
write.csv(pcmeans, file.path(out, "PCMEANS.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Plotting parameters from sgrender dynamic
XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100
TICKS <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
SD_opt <- SD_param # BOTH
LEGENDSIZE <- 11

# Determine YMAX if not supplied: max of mean+sd
ymax_calc <- suppressWarnings(max(MeanData$PCMEAN + MeanData$PCSD, na.rm = TRUE))
if (!is.finite(ymax_calc) || is.na(ymax_calc)) ymax_calc <- max(MeanData$PCMEAN, na.rm = TRUE)
YMAX <- ceiling(ymax_calc * 1.1)
if (!is.finite(YMAX) || YMAX == 0) YMAX <- 10

# Prepare data for plotting: ensure TRTA is factor ordered by TRTAN
MeanData <- MeanData %>%
  mutate(
    TRTA_f = factor(TRTA, levels = unique(TRTA[order(TRTAN)])),
    TRTAN_f = factor(TRTAN)
  )

# Common theme
theme_pksum <- function() {
  theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(size = 9, hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = LEGENDSIZE * 0.75),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
    )
}

# Linear view
p_linear <- ggplot(MeanData, aes(x = PCTPT, y = PCMEAN, group = TRTA_f, color = TRTA_f, shape = TRTA_f, linetype = TRTA_f)) +
  geom_line(linewidth = 0.7, color = "black") +
  geom_point(size = 2.5, color = "black", fill = "white") +
  { if (SD_opt == "BOTH") geom_errorbar(data = MeanData, aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD), width = 1.5, color = "black", linewidth = 0.5, inherit.aes = FALSE, show.legend = FALSE) } +
  { if (SD_opt == "UPPER") geom_errorbar(data = MeanData, aes(ymin = PCMEAN, ymax = PCMEAN + PCSD), width = 1.5, color = "black", linewidth = 0.5, inherit.aes = FALSE, show.legend = FALSE) } +
  scale_x_continuous(name = XLABEL, limits = c(0, XMAX), breaks = TICKS, expand = c(0.01, 0)) +
  scale_y_continuous(name = YLABEL, limits = c(0, YMAX), expand = c(0, 0)) +
  scale_color_grey(start = 0, end = 0) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4)[seq_len(nlevels(MeanData$TRTA_f))]) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[seq_len(nlevels(MeanData$TRTA_f))]) +
  ggtitle("Linear view") +
  theme_pksum() +
  theme(legend.position = "none")

# Semilog view (no error bars per template)
# For log scale, need positive values; filter 0/NA
MeanData_log <- MeanData %>% filter(!is.na(PCMEAN) & PCMEAN > 0)

p_semilog <- ggplot(MeanData_log, aes(x = PCTPT, y = PCMEAN, group = TRTA_f, color = TRTA_f, shape = TRTA_f, linetype = TRTA_f)) +
  geom_line(linewidth = 0.7, color = "black") +
  geom_point(size = 2.5, color = "black", fill = "white") +
  scale_x_continuous(name = XLABEL, limits = c(0, XMAX), breaks = TICKS, expand = c(0.01, 0)) +
  scale_y_log10(name = YLABEL, limits = c(NA, YMAX)) +
  annotation_logticks(sides = "l", colour = "grey50", size = 0.3) +
  scale_color_grey(start = 0, end = 0) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4)[seq_len(nlevels(MeanData$TRTA_f))]) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[seq_len(nlevels(MeanData$TRTA_f))]) +
  ggtitle("Semilogarithmic view") +
  theme_pksum() +
  theme(legend.position = "none")

# Legend plot (extract legend)
# Create a dummy plot for legend
p_legend_source <- ggplot(MeanData, aes(x = PCTPT, y = PCMEAN, group = TRTA_f, color = TRTA_f, shape = TRTA_f, linetype = TRTA_f)) +
  geom_line(aes(linetype = TRTA_f), color = "black") +
  geom_point(aes(shape = TRTA_f), color = "black") +
  scale_color_grey(start = 0, end = 0) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4)[seq_len(nlevels(MeanData$TRTA_f))]) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[seq_len(nlevels(MeanData$TRTA_f))]) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 9))

# Function to extract legend grob
g_legend <- function(a.gplot) {
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) tmp$grobs[[leg]] else nullGrob()
}
legend_grob <- g_legend(p_legend_source)

# -------------------------------------------------------------------------
# Combine into one PNG: designwidth 2500px designheight 1400px ~ 22.87cm x 12cm
# Use png device with high resolution
png_file <- file.path(out, "target13.png")
# 22.87cm = 9.0 inch, 12cm = 4.72 inch; at 300 dpi => 2700x1417 px close to 2500x1400
png(png_file, width = 2500, height = 1400, res = 300, units = "px", bg = "white")
grid.newpage()
# Layout: 2 rows, 2 cols, row heights 0.9, 0.1 ; add titles
# Outer viewport for titles
pushViewport(viewport(layout = grid.layout(4, 2,
  heights = unit(c(0.06, 0.06, 0.78, 0.10), "npc"),
  widths = unit(c(0.5, 0.5), "npc")
)))

# Title 1: SPONSOR/PROTOCOL/PRODUCT INFO (left) and page x of y (right)
grid.text("SPONSOR/PROTOCOL/PRODUCT INFO", x = unit(0.01, "npc"), y = unit(1, "npc"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2), just = c("left", "top"),
          gp = gpar(fontsize = 8))
grid.text("page x of y", x = unit(0.99, "npc"), y = unit(1, "npc"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2), just = c("right", "top"),
          gp = gpar(fontsize = 8))

# Title 3 and 4
grid.text("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",
          vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2), gp = gpar(fontsize = 9, fontface = "bold"))
grid.text("Analysis set: PK analysis set",
          vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2, just = "bottom"), y = unit(0.2, "npc"),
          gp = gpar(fontsize = 8))

# Plots
print(p_linear, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(p_semilog, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

# Legend sidebar bottom
if (!is.null(legend_grob) && !inherits(legend_grob, "nullGrob")) {
  pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1:2))
  grid.draw(legend_grob)
  popViewport()
}

# Footnote
grid.text("PATH DATA/PROGRAM/OUTPUT", x = unit(0.01, "npc"), y = unit(0.01, "npc"),
          vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2), just = c("left", "bottom"),
          gp = gpar(fontsize = 7))

popViewport()
dev.off()

# Also save via ggsave alternative for verification (single panel fallback if grid fails)
# Ensure file exists
if (!file.exists(png_file)) {
  ggsave(png_file, plot = p_linear, width = 22.87, height = 12, units = "cm", dpi = 300)
}

# -------------------------------------------------------------------------
# Create TXT rendering of the table body (counts/statistics)
txt_file <- file.path(out, "target13.txt")
sink(txt_file)
cat("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately\n")
cat("Analysis set: PK analysis set\n")
cat("PCTESTCD = ANAL1, Meantype = Arithmetic, SD = BOTH\n")
cat("----------------------------------------------------------------\n")
cat(sprintf("X-axis: %s  (0 to %d, ticks: %s)\n", XLABEL, XMAX, paste(TICKS, collapse = ", ")))
cat(sprintf("Y-axis: %s  (0 to %.2f)\n", YLABEL, YMAX))
cat("\nMeanData (plotted data):\n")
print(as.data.frame(MeanData %>% select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD) %>% arrange(TRTAN, PCTPT)))
cat("\nNote: Linear view shows mean +/- SD error bars; Semilog view shows mean only on log10 scale.\n")
sink()

# Ensure CSV of plotted data already written as target13.csv
# Also write a summary table CSV for display
table_body <- MeanData %>%
  mutate(
    MEAN_SD = ifelse(is.na(PCMEAN), "",
      sprintf("%.2f (%.2f)", round_sas(PCMEAN, 2), round_sas(PCSD, 2))
    )
  ) %>%
  select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD, MEAN_SD) %>%
  arrange(TRTAN, PCTPT)

write.csv(table_body, file.path(out, "target13_table.csv"), row.names = FALSE, na = "")

cat("Done: outputs written to", out, "\n")
```