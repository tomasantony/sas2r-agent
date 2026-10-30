# P057_pk_mean_conc.sas -> R translation
# Figure 14.2-x.x Arithmetic Mean concentration-time plot
# Input: adpc.xpt  Output: target13.png + CSV

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file in multiple candidate locations
find_file <- function(fname) {
  candidates <- c(
    file.path(Sys.getenv("RAWADAM"), fname),
    file.path(Sys.getenv("rawadam"), fname),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam", fname),
    file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam", fname),
    file.path(Sys.getenv("RAWADAM"), "adpc.xpt"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
    file.path(getwd(), fname),
    file.path(getwd(), "data/raw/cdisc-pilot/adam", fname),
    file.path(getwd(), "data/derived/cdisc-pilot/adam", fname),
    "/tmp/adpc.xpt"
  )
  # also search common linux roots if not found
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  # recursive search under / (limited depth)
  search_roots <- c("/data", "/tmp", ".", getwd(), "/opt", "/home")
  for (r in search_roots) {
    if (dir.exists(r)) {
      found <- list.files(r, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
      if (length(found) > 0) return(found[1])
      # also search for adpc.xpt generically
      found2 <- list.files(r, pattern = "^adpc\\.xpt$", recursive = TRUE, full.names = TRUE)
      if (length(found2) > 0) return(found2[1])
    }
  }
  return(NA_character_)
}

adpc_path <- find_file("adpc.xpt")
if (is.na(adpc_path) || !file.exists(adpc_path)) {
  # last resort: try haven example paths
  alt <- c(
    "data/raw/cdisc-pilot/adam/adpc.xpt",
    "data/derived/cdisc-pilot/adam/adpc.xpt",
    "../data/raw/cdisc-pilot/adam/adpc.xpt"
  )
  for (p in alt) if (file.exists(p)) adpc_path <- p
}
if (is.na(adpc_path) || !file.exists(adpc_path)) {
  stop(paste0("adpc.xpt not found. Searched candidates; OUT_DIR=", out))
}
message("Using adpc.xpt: ", adpc_path)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(grid)
})

# Read ADPC
adpc_raw <- haven::read_xpt(adpc_path)
# zap labels/formats to plain vectors for easier handling
adpc_raw <- haven::zap_labels(adpc_raw)
adpc_raw <- haven::zap_formats(adpc_raw)

# Save orig dataset as CSV (dataset output)
write.csv(adpc_raw, file.path(out, "orig_adpc.csv"), row.names = FALSE, na = "")

# Parameters from macro call: %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH)
PCTESTCD_param <- "ANAL1"
Meantype <- "A"  # A=Arithmetic, G=Geometric
SD_param <- "BOTH" # UPPER, BOTH, N

# Data step work.adpc
# keep=ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
# where PCTESTCD="&PCTESTCD"
# PCTPT=input(EPLTM,best10.)
# ORDERBY=compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN)
# BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')

# Ensure columns exist
required_cols <- c("PCTESTCD","EPLTM","ANLPRNT","PCSPEC","PCTESTL","TRTAN","TRTA","PCORRESU","PCORRESN","USUBJID")
for (col in required_cols) {
  if (!col %in% names(adpc_raw)) {
    warning(paste("Column", col, "not found in adpc_raw; creating NA"))
    adpc_raw[[col]] <- NA
  }
}

adpc <- adpc_raw %>%
  filter(PCTESTCD == PCTESTCD_param) %>%
  mutate(
    # SAS input(EPLTM,best10.) : numeric conversion, best10. reads leading blanks, scientific etc.
    PCTPT = suppressWarnings(as.numeric(as.character(EPLTM))),
    # ORDERBY = compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN)
    ORDERBY = str_replace_all(paste0(as.character(ANLPRNT), as.character(PCSPEC), as.character(PCTESTL), as.character(TRTAN)), "\\s+", ""),
    # BYVAL6 = compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
    # propcase: first letter upper rest lower
    BYVAL6 = {
      pc <- ifelse(is.na(PCTESTCD), "", paste0(toupper(substr(PCTESTCD,1,1)), tolower(substr(PCTESTCD,2,nchar(PCTESTCD)))))
      # compbl = compress multiple blanks to one and trim
      raw <- paste0(pc, " Conc ", " (", as.character(PCORRESU), ")")
      str_squish(raw)
    }
  )

# proc sort by TRTAN TRTA PCTPT
adpc <- adpc %>% arrange(TRTAN, TRTA, PCTPT)

write.csv(adpc, file.path(out, "adpc.csv"), row.names = FALSE, na = "")

# proc univariate by TRTAN TRTA PCTPT var PCORRESN output mean and std
# SAS PROC UNIVARIATE STD is sample std (n-1 denominator)
if (toupper(Meantype) == "A") {
  pcmeans <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      n = sum(!is.na(PCORRESN)),
      PCmean = ifelse(n > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      PCSD = ifelse(n > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      PCmean = ifelse(is.nan(PCmean), NA_real_, PCmean),
      PCSD = ifelse(is.nan(PCSD), NA_real_, PCSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCmean, PCSD)

  # data MeanData; if PCMEAN=0 then PCMEAN=.; PCSD=.;
  MeanData <- pcmeans %>%
    mutate(
      PCMEAN = PCmean,
      PCSD = PCSD,
      PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
      PCSD = ifelse(!is.na(PCMEAN) & is.na(PCMEAN), NA_real_, PCSD) # actually SAS sets PCSD=. when PCMEAN=0
    ) %>%
    # SAS second assignment: if PCMEAN=0 then PCSD=. ; need to handle original PCmean==0
    mutate(
      PCSD = ifelse(!is.na(PCmean) & PCmean == 0, NA_real_, PCSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD) %>%
    arrange(TRTAN, TRTA, PCTPT)

} else {
  # Geometric mean branch (not used for this call, but implemented for completeness)
  logpc <- adpc %>%
    mutate(LOGORRES = ifelse(!is.na(PCORRESN), log(PCORRESN), NA_real_))
  logmeans <- logpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      n = sum(!is.na(PCORRESN)),
      Logmean = ifelse(n > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      logSD = ifelse(n > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  MeanData <- logmeans %>%
    mutate(
      PCMEAN = exp(Logmean),
      PCSD = exp(logSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD) %>%
    arrange(TRTAN, TRTA, PCTPT)
}

# Ensure MeanData has no zero means (SAS sets to missing)
# Already handled

# Save MeanData dataset
write.csv(MeanData, file.path(out, "MeanData.csv"), row.names = FALSE, na = "")
write.csv(MeanData, file.path(out, "target13.csv"), row.names = FALSE, na = "")
write.csv(MeanData, file.path(out, "pcmeans.csv"), row.names = FALSE, na = "")

# Prepare plot data
plot_data <- MeanData %>%
  filter(!is.na(PCMEAN), !is.na(PCTPT)) %>%
  arrange(TRTAN, PCTPT)

# Determine treatment levels for legend
trt_levels <- plot_data %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN)
n_trt <- nrow(trt_levels)

# Define aesthetics: SAS uses black for all, differentiate by shape/linetype
shapes <- c(16, 17, 15, 18, 8, 3, 4, 7, 9)
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
shapes_use <- shapes[seq_len(n_trt)]
linetypes_use <- linetypes[seq_len(n_trt)]

# X ticks and limits
x_ticks <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
x_max <- 100
y_label <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
x_label <- "Time (h)"

# Create linear view plot
p_linear <- ggplot(plot_data, aes(x = PCTPT, y = PCMEAN, group = TRTA)) +
  geom_line(aes(linetype = TRTA), color = "black", linewidth = 0.7) +
  geom_point(aes(shape = TRTA), color = "black", size = 2.5, fill = "black") +
  scale_x_continuous(limits = c(0, x_max), breaks = x_ticks, expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.05, 0.1))) +
  scale_shape_manual(values = setNames(shapes_use, trt_levels$TRTA)) +
  scale_linetype_manual(values = setNames(linetypes_use, trt_levels$TRTA)) +
  labs(x = x_label, y = y_label, title = "Linear view", shape = "Treatment", linetype = "Treatment") +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )

# Add error bars for SD=BOTH or UPPER
if (toupper(SD_param) == "BOTH") {
  p_linear <- p_linear + geom_errorbar(
    aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD),
    width = 1.5, color = "black", linewidth = 0.4, na.rm = TRUE
  )
} else if (toupper(SD_param) == "UPPER") {
  p_linear <- p_linear + geom_errorbar(
    aes(ymin = PCMEAN, ymax = PCMEAN + PCSD),
    width = 1.5, color = "black", linewidth = 0.4, na.rm = TRUE
  )
}

# Create semilog view plot (log y, no error bars per SAS template)
# For log scale, need to handle zero/NA; filter positive
plot_data_log <- plot_data %>% filter(PCMEAN > 0)

p_semilog <- ggplot(plot_data_log, aes(x = PCTPT, y = PCMEAN, group = TRTA)) +
  geom_line(aes(linetype = TRTA), color = "black", linewidth = 0.7) +
  geom_point(aes(shape = TRTA), color = "black", size = 2.5) +
  scale_x_continuous(limits = c(0, x_max), breaks = x_ticks, expand = c(0.02, 0)) +
  scale_y_log10(expand = expansion(mult = c(0.05, 0.1))) +
  scale_shape_manual(values = setNames(shapes_use, trt_levels$TRTA)) +
  scale_linetype_manual(values = setNames(linetypes_use, trt_levels$TRTA)) +
  labs(x = x_label, y = y_label, title = "Semilogarithmic view", shape = "Treatment", linetype = "Treatment") +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )

# Combined legend at bottom (merged legend)
# Create a dummy plot for legend extraction
p_legend <- ggplot(plot_data, aes(x = PCTPT, y = PCMEAN, group = TRTA, shape = TRTA, linetype = TRTA)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  scale_shape_manual(values = setNames(shapes_use, trt_levels$TRTA)) +
  scale_linetype_manual(values = setNames(linetypes_use, trt_levels$TRTA)) +
  labs(shape = "", linetype = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 11))

# Extract legend grob
get_legend_grob <- function(p) {
  g <- ggplotGrob(p)
  idx <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  if (length(idx) > 0) g$grobs[[idx]] else nullGrob()
}
legend_grob <- get_legend_grob(p_legend)

# Create PNG with titles, two panels, legend, footnote
png_file <- file.path(out, "target13.png")
# designwidth=2500px designheight=1400px ; use 2500x1400 at 150 dpi ~ 16.6x9.3 inches
png(png_file, width = 2500, height = 1400, res = 150, bg = "white")
grid.newpage()

# Layout: 4 rows: title1, title3/4, plots, footnote+legend
# Use grid layout 5 rows x 2 cols
lyt <- grid.layout(
  nrow = 5, ncol = 2,
  heights = unit(c(0.04, 0.04, 0.04, 0.80, 0.08), "npc"),
  widths = unit(c(0.5, 0.5), "npc")
)
pushViewport(viewport(layout = lyt))

# Title J=L "SPONSOR/PROTOCOL/PRODUCT INFO" j=R "page x of y"
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
grid.text("SPONSOR/PROTOCOL/PRODUCT INFO", x = unit(0.01, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(fontsize = 9))
grid.text("page x of y", x = unit(0.99, "npc"), y = unit(0.5, "npc"), just = "right", gp = gpar(fontsize = 9))
popViewport()

# Title3
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
grid.text("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",
          x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "centre", gp = gpar(fontsize = 10, fontface = "bold"))
popViewport()

# Title4
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
grid.text("Analysis set: PK analysis set",
          x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "centre", gp = gpar(fontsize = 9))
popViewport()

# Linear view
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
print(p_linear, vp = viewport(x = 0.5, y = 0.5, width = unit(0.98, "npc"), height = unit(0.98, "npc")))
popViewport()

# Semilog view
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2))
print(p_semilog, vp = viewport(x = 0.5, y = 0.5, width = unit(0.98, "npc"), height = unit(0.98, "npc")))
popViewport()

# Footer + legend row
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1:2))
# footnote
grid.text("PATH DATA/PROGRAM/OUTPUT", x = unit(0.01, "npc"), y = unit(0.2, "npc"), just = "left", gp = gpar(fontsize = 7))
# legend centered
pushViewport(viewport(x = 0.5, y = 0.6, width = unit(0.8, "npc"), height = unit(0.7, "npc")))
grid.draw(legend_grob)
popViewport()
popViewport()

popViewport()
dev.off()

# Also create a ggsave version for verification (optional)
# ggsave handles differently, but png already created

# Create plain-text rendering of the figure data (target13.txt)
txt_file <- file.path(out, "target13.txt")
sink(txt_file)
cat("SPONSOR/PROTOCOL/PRODUCT INFO                                                  page x of y\n")
cat("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately\n")
cat("Analysis set: PK analysis set\n")
cat("\n")
cat("Arithmetic mean (SD) Anal2 Conc (ng/mL) by Time (h) and Treatment\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf("%-20s %8s %12s %12s %8s\n", "Treatment", "Time(h)", "Mean", "SD", "N"))
cat(paste(rep("-", 80), collapse = ""), "\n")
# Need N per group for txt; recompute n
txt_data <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    n = sum(!is.na(PCORRESN)),
    mean = mean(PCORRESN, na.rm = TRUE),
    sd = ifelse(n > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(TRTAN, PCTPT)
for (i in seq_len(nrow(txt_data))) {
  cat(sprintf("%-20s %8.1f %12.4f %12.4f %8d\n",
              txt_data$TRTA[i],
              txt_data$PCTPT[i],
              txt_data$mean[i],
              ifelse(is.na(txt_data$sd[i]), 0, txt_data$sd[i]),
              txt_data$n[i]))
}
cat(paste(rep("-", 80), collapse = ""), "\n")
cat("\nLinear view: X-axis 0 to 100 h, ticks 0 4 8 12 24 36 48 72 96; Y-axis linear 0 to auto\n")
cat("Semilogarithmic view: Y-axis log base 10\n")
cat("Error bars: ", SD_param, " (Mean +/- SD)\n", sep = "")
cat("\nPATH DATA/PROGRAM/OUTPUT\n")
sink()

# Verify outputs
message("Outputs written to ", out)
message(" - ", png_file)
message(" - ", file.path(out, "target13.csv"))
message(" - ", txt_file)
stopifnot(file.exists(png_file))
stopifnot(file.exists(file.path(out, "target13.csv")))
