# P057_pk_mean_conc.sas -> R translation
# Figure: Arithmetic Mean concentration-time plot (linear + semilog)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)

# -------------------------------------------------------------------------
# 1. Read ADPC (XPORT)
# -------------------------------------------------------------------------
adpc_path <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"
if (!file.exists(adpc_path)) {
  # fallback with forward slashes / lower case
  adpc_path <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"
}
adpc_raw <- haven::read_xpt(adpc_path)

# Convert haven labelled to plain vectors for needed columns
# Keep only columns used in SAS program
# SAS keep: ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
# Note ANLPRNT duplicated in keep list
adpc_raw2 <- adpc_raw %>%
  mutate(
    TRTAN = as.numeric(TRTAN),
    TRTA = as.character(TRTA),
    PCTESTCD = as.character(PCTESTCD),
    PCTESTL = as.character(PCTESTL),
    PCSPEC = as.character(PCSPEC),
    ANLPRNT = as.character(ANLPRNT),
    PCORRESU = as.character(PCORRESU),
    PCORRES = as.character(PCORRES),
    USUBJID = as.character(USUBJID),
    PCORRESN = as.numeric(PCORRESN),
    EPLTM = as.character(EPLTM),
    DOSREFID = as.character(DOSREFID)
  )

# -------------------------------------------------------------------------
# 2. Macro parameters (as called)
# -------------------------------------------------------------------------
PCTESTCD_param <- "ANAL1"
Meantype <- "A"   # A = Arithmetic
SD_param <- "BOTH" # BOTH, UPPER, N

# -------------------------------------------------------------------------
# 3. Filter and derive PCTPT (SAS: PCTPT=input(EPLTM,best10.))
# -------------------------------------------------------------------------
adpc <- adpc_raw2 %>%
  filter(PCTESTCD == PCTESTCD_param) %>%
  mutate(
    PCTPT = suppressWarnings(as.numeric(EPLTM)),
    ORDERBY = paste0(
      ifelse(is.na(ANLPRNT), "", ANLPRNT),
      ifelse(is.na(PCSPEC), "", PCSPEC),
      ifelse(is.na(PCTESTL), "", PCTESTL),
      ifelse(is.na(TRTAN), "", as.character(TRTAN))
    ),
    BYVAL6 = paste0(
      stringr::str_to_title(ifelse(is.na(PCTESTCD), "", PCTESTCD)),
      " Conc ",
      " (", ifelse(is.na(PCORRESU), "", PCORRESU), ")"
    )
  )

# SAS sort by TRTAN TRTA PCTPT
adpc <- adpc %>% arrange(TRTAN, TRTA, PCTPT)

# -------------------------------------------------------------------------
# 4. Compute means (PROC UNIVARIATE mean/std by TRTAN TRTA PCTPT)
#    SAS STD is sample std (denominator N-1)
# -------------------------------------------------------------------------
if (toupper(Meantype) == "A") {
  pcmeans <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(PCORRESN)),
      PCmean = ifelse(N > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      PCSD = ifelse(N > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )

  MeanData <- pcmeans %>%
    mutate(
      PCMEAN = PCmean,
      PCSD = PCSD
    ) %>%
    mutate(
      PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
      PCSD = ifelse(is.na(PCMEAN), NA_real_, PCSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD, N) %>%
    arrange(TRTAN, PCTPT)

} else {
  # Geometric (not used for this call, but implemented for completeness)
  logpc <- adpc %>%
    mutate(LOGORRES = ifelse(!is.na(PCORRESN), log(PCORRESN), NA_real_))
  logmeans <- logpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(PCORRESN)),
      Logmean = ifelse(N > 0, mean(LOGORRES, na.rm = TRUE), NA_real_),
      logSD = ifelse(N > 1, sd(LOGORRES, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  MeanData <- logmeans %>%
    mutate(
      PCMEAN = exp(Logmean),
      PCSD = exp(logSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD, N) %>%
    arrange(TRTAN, PCTPT)
}

# Ensure TRTA is factor ordered by TRTAN for legend
MeanData <- MeanData %>%
  mutate(
    TRTA = factor(TRTA, levels = unique(TRTA[order(TRTAN)])),
    TRTAN_f = factor(TRTAN)
  )

# -------------------------------------------------------------------------
# 5. Write dataset CSV (MeanData)
# -------------------------------------------------------------------------
# Dates not present; write ISO if any
MeanData_out <- MeanData %>%
  select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD, N) %>%
  arrange(TRTAN, PCTPT)

write.csv(MeanData_out, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# Also write a generic dataset name if expected
write.csv(MeanData_out, file.path(out, "MeanData.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# 6. Prepare plot parameters (from SAS template call)
# -------------------------------------------------------------------------
XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100
TICKS <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
LEGENDSIZE <- 11 # pt

# YMAX not supplied in SAS call -> compute from data
ymax_linear <- max(MeanData_out$PCMEAN + MeanData_out$PCSD, na.rm = TRUE)
if (!is.finite(ymax_linear)) ymax_linear <- max(MeanData_out$PCMEAN, na.rm = TRUE)
ymax_linear <- ymax_linear * 1.15
# round up to nice value
ymax_linear <- ceiling(ymax_linear / 10) * 10
if (ymax_linear == 0 || !is.finite(ymax_linear)) ymax_linear <- 100

# For log view, need positive values
ymax_log <- max(MeanData_out$PCMEAN, na.rm = TRUE) * 1.5
if (!is.finite(ymax_log) || ymax_log <= 0) ymax_log <- 100

# Error bar limits
MeanData_plot <- MeanData_out %>%
  mutate(
    lower = PCMEAN - PCSD,
    upper = PCMEAN + PCSD,
    lower_upper = PCMEAN, # for UPPER case
    lower_both = PCMEAN - PCSD
  ) %>%
  mutate(
    lower = ifelse(lower < 0, 0, lower)
  )

# -------------------------------------------------------------------------
# 7. Build ggplot objects
# -------------------------------------------------------------------------
# Common theme
base_theme <- theme_bw(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1.2, "cm")
  )

# Linear view
p_linear <- ggplot(MeanData_plot, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(color = "black", fill = "white", size = 2.5, stroke = 0.8) +
  { if (SD_param == "BOTH") geom_errorbar(aes(ymin = lower, ymax = upper), width = 1.5, color = "black", linewidth = 0.5) } +
  { if (SD_param == "UPPER") geom_errorbar(aes(ymin = PCMEAN, ymax = upper), width = 1.5, color = "black", linewidth = 0.5) } +
  scale_x_continuous(name = XLABEL, limits = c(0, XMAX), breaks = TICKS) +
  scale_y_continuous(name = YLABEL, limits = c(0, ymax_linear)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[seq_along(levels(MeanData_plot$TRTA))]) +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4)[seq_along(levels(MeanData_plot$TRTA))]) +
  ggtitle("Linear view") +
  base_theme

# Semilog view
# For log scale, need to avoid 0
p_semilog <- ggplot(MeanData_plot, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(color = "black", fill = "white", size = 2.5, stroke = 0.8) +
  scale_x_continuous(name = XLABEL, limits = c(0, XMAX), breaks = TICKS) +
  scale_y_log10(name = YLABEL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[seq_along(levels(MeanData_plot$TRTA))]) +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4)[seq_along(levels(MeanData_plot$TRTA))]) +
  ggtitle("Semilogarithmic view") +
  base_theme +
  theme(legend.position = "none")

# Extract legend from linear plot
get_legend <- function(plot) {
  tmp <- ggplotGrob(plot)
  leg_idx <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg_idx) > 0) tmp$grobs[[leg_idx]] else nullGrob()
}
legend_grob <- get_legend(p_linear)

# Remove legend from linear for grid arrangement (we will place at bottom)
p_linear_noleg <- p_linear + theme(legend.position = "none")

# -------------------------------------------------------------------------
# 8. Render PNG with titles/footnotes (SAS titles)
# -------------------------------------------------------------------------
title1_left <- "SPONSOR/PROTOCOL/PRODUCT INFO"
title1_right <- "page x of y"
title3 <- "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
title4 <- "Analysis set: PK analysis set"
footnote_text <- "PATH DATA/PROGRAM/OUTPUT"

# Use png device to allow grid layout with titles
png_file <- file.path(out, "target13.png")
png(png_file, width = 2500, height = 1400, res = 200)
grid.newpage()

# Layout: rows = title area (0.08), subtitle area (0.06), plots (0.78), legend (0.08)
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 2,
                                           heights = unit(c(0.04, 0.04, 0.04, 0.80, 0.08), "npc"),
                                           widths = unit(c(0.5, 0.5), "npc"))))

# Title row 1: left and right
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text(title1_left, x = unit(0.01, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(fontsize = 9))
popViewport()
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.text(title1_right, x = unit(0.99, "npc"), y = unit(0.5, "npc"), just = "right", gp = gpar(fontsize = 9))
popViewport()

# Title3
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
grid.text(title3, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "center", gp = gpar(fontsize = 9, fontface = "bold"))
popViewport()

# Title4
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
grid.text(title4, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "center", gp = gpar(fontsize = 9))
popViewport()

# Plots
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
print(p_linear_noleg, vp = viewport(x = 0.5, y = 0.5, width = unit(0.98, "npc"), height = unit(0.98, "npc")))
popViewport()

pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2))
print(p_semilog, vp = viewport(x = 0.5, y = 0.5, width = unit(0.98, "npc"), height = unit(0.98, "npc")))
popViewport()

# Legend at bottom spanning both columns
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1:2))
grid.draw(legend_grob)
popViewport()

# Footnote
# Draw footnote at very bottom left (outside layout, use viewport overlay)
# Use grid.text with npc 0.01, 0.01
grid.text(footnote_text, x = unit(0.01, "npc"), y = unit(0.005, "npc"), just = c("left", "bottom"), gp = gpar(fontsize = 7))

dev.off()

# Also create a ggsave version for compatibility (single combined grob not needed)
# Ensure file exists
if (!file.exists(png_file)) {
  ggsave(png_file, plot = p_linear, width = 11, height = 6, dpi = 300)
}

# -------------------------------------------------------------------------
# 9. Write TXT rendering of table (mean data)
# -------------------------------------------------------------------------
txt_file <- file.path(out, "target13.txt")
sink(txt_file)
cat(paste0(title1_left, strrep(" ", 60), title1_right, "\n"))
cat(paste0(title3, "\n"))
cat(paste0(title4, "\n"))
cat("\n")
cat(paste0("Analysis: PCTESTCD=", PCTESTCD_param, "  Meantype=", Meantype, "  SD=", SD_param, "\n"))
cat(paste0(YLABEL, " vs ", XLABEL, "\n"))
cat("\n")
# Print table header
cat(sprintf("%-8s %-25s %8s %12s %12s %6s\n", "TRTAN", "TRTA", "PCTPT", "PCMEAN", "PCSD", "N"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (i in seq_len(nrow(MeanData_out))) {
  cat(sprintf("%-8s %-25s %8.2f %12.4f %12.4f %6d\n",
              as.character(MeanData_out$TRTAN[i]),
              as.character(MeanData_out$TRTA[i]),
              MeanData_out$PCTPT[i],
              MeanData_out$PCMEAN[i],
              MeanData_out$PCSD[i],
              MeanData_out$N[i]))
}
cat("\n")
cat(paste0(footnote_text, "\n"))
sink()

# -------------------------------------------------------------------------
# 10. Verify outputs
# -------------------------------------------------------------------------
stopifnot(file.exists(file.path(out, "target13.png")))
stopifnot(file.exists(file.path(out, "target13.csv")))
