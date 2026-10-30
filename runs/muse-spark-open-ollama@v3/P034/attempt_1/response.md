```r
# P034_target13.sas -> R translation
# Figure: Arithmetic Mean concentration-time plot (linear + semilog)
# Input: adpc.xpt (CDISC pilot ADaM)
# Output: target13.png + target13.csv (plotted data)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: round half away from zero (SAS behavior) - not strictly needed here but included for fidelity
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file across possible mount points
find_file <- function(candidates) {
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  return(NA_character_)
}

# Locate adpc.xpt - try SAS absolute path and common Linux mirrors
candidates_adpc <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADPC.xpt",
  "/mnt/d/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "data/raw/cdisc-pilot/adam/adpc.xpt",
  "../data/raw/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("RAWADAM"), "adpc.xpt"),
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  "/tmp/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt"
)
# also try to locate via recursive search if not found
adpc_path <- find_file(candidates_adpc)
if (is.na(adpc_path)) {
  # brute force search under current working directory and /tmp
  search_roots <- c(getwd(), "/tmp", ".", "..", Sys.getenv("HOME"))
  for (r in search_roots) {
    if (!dir.exists(r)) next
    found <- list.files(r, pattern = "^adpc\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(found) > 0) { adpc_path <- found[1]; break }
  }
}
if (is.na(adpc_path) || !file.exists(adpc_path)) {
  stop("adpc.xpt not found. Tried: ", paste(candidates_adpc, collapse="; "))
}
message("Reading ADPC from: ", adpc_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)

# Read XPT - haven::read_xpt handles XPORT engine
adpc_raw <- haven::read_xpt(adpc_path)

# Keep original for reference (work.orig_adpc)
orig_adpc <- adpc_raw

# Macro parameters for this call
PCTESTCD_param <- "ANAL1"
Meantype_param <- "A"  # A = Arithmetic, G = Geometric
SD_param <- "BOTH"     # BOTH, UPPER, N

# ------------------------------------------------------------------
# data work.adpc ;
# set source.adpc(keep=ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD
#                      PCORRESU PCORRES USUBJID  PCORRESN EPLTM);
#   where PCTESTCD="&PCTESTCD";
#   PCTPT=input(EPLTM,best10.);
#   ORDERBY=compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN);
#   BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')');
# ------------------------------------------------------------------
# Ensure expected columns exist (case-insensitive check)
# haven preserves original case; normalize to upper for filtering
names(adpc_raw) <- toupper(names(adpc_raw))
# Required columns
keep_cols <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD","PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
# Some pilot datasets may have slightly different names; intersect
keep_cols_present <- intersect(keep_cols, names(adpc_raw))
adpc_filt <- adpc_raw %>%
  filter(toupper(PCTESTCD) == toupper(PCTESTCD_param))

# PCTPT = input(EPLTM, best10.)  -> numeric conversion, SAS best10. reads as numeric
# EPLTM is character like "0", "4", etc. Use as.numeric after trimming
adpc_filt <- adpc_filt %>%
  mutate(
    PCTPT = suppressWarnings(as.numeric(str_trim(as.character(EPLTM)))),
    # ORDERBY = compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN)  # not used downstream
    ORDERBY = str_replace_all(paste0(as.character(ANLPRNT), as.character(PCSPEC), as.character(PCTESTL), as.character(TRTAN)), "\\s+", ""),
    # BYVAL6 = compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
    BYVAL6 = str_squish(paste0(str_to_title(tolower(PCTESTCD)), " Conc ", " (", PCORRESU, ")"))
  )

# proc sort data=ADPC; by TRTAN TRTA PCTPT; run;
adpc_sorted <- adpc_filt %>%
  arrange(TRTAN, TRTA, PCTPT)

# ------------------------------------------------------------------
# proc univariate data=ADPC noprint; by TRTAN TRTA PCTPT; var PCORRESN; output out=PCMEANS mean=PCmean STD=PCSD ;
# ------------------------------------------------------------------
# SAS PROC UNIVARIATE STD is sample SD (denominator N-1), missing if N<=1
# Replicate exactly
if (toupper(Meantype_param) == "A") {
  pcmeans <- adpc_sorted %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(PCORRESN)),
      PCMEAN = ifelse(N > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      PCSD = ifelse(N > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD)

  # data MeanData; set PCMEANS; if PCMEAN=0 then do; PCMEAN=.; PCSD=.; end; run;
  MeanData <- pcmeans %>%
    mutate(
      PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
      PCSD   = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCSD) # note: after PCMEAN set to NA, need original check; use pcmeans$PCMEAN
    )
  # Correct the above: need to check original PCMEAN ==0 before overwrite
  MeanData <- pcmeans %>%
    mutate(
      PCSD = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCSD),
      PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN)
    )

} else if (toupper(Meantype_param) == "G") {
  # Geometric path (not used for this call, but implemented for fidelity)
  # data LOGPC; set ADPC; if PCORRESN ne . then LOGORRES=log(PCORRESN); run;
  # proc univariate data=LOGPC noprint; by TRTAN TRTA PCTPT; var PCORRESN; output out=LogMEANS mean=Logmean STD=logSD ;
  # Note: SAS code has bug var PCORRESN instead of LOGORRES, we replicate SAS exactly
  logpc <- adpc_sorted %>%
    mutate(LOGORRES = ifelse(!is.na(PCORRESN), log(PCORRESN), NA_real_))
  logmeans <- logpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(PCORRESN)),
      Logmean = ifelse(N > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      logSD = ifelse(N > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  MeanData <- logmeans %>%
    mutate(
      PCMEAN = exp(Logmean),
      PCSD = exp(logSD)
    ) %>%
    select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD)
} else {
  stop("Meantype must be A or G")
}

# Sort for plotting (SAS by TRTAN TRTA PCTPT)
MeanData <- MeanData %>% arrange(TRTAN, TRTA, PCTPT)

# Remove rows where PCTPT is NA (SAS would have missing sort first, but plot would ignore)
MeanData_plot <- MeanData %>% filter(!is.na(PCTPT))

# Ensure TRTAN/TRTA are factors ordered by TRTAN (SAS character ordering: uppercase before lowercase, but TRTAN numeric)
# Create legend labels as TRTA
MeanData_plot <- MeanData_plot %>%
  mutate(
    TRTA = as.character(TRTA),
    TRTAN = as.numeric(TRTAN)
  )

# Determine YMAX if not supplied (SAS _YMAX missing -> auto)
# For linear view, viewmin=0 viewmax=_YMAX (auto). We compute a nice max.
# For semilog, TYPE=LOG BASE=10 viewmax=_YMAX (auto)
# Compute max of PCMEAN+PCSD for linear to set limit
y_linear_max <- max(MeanData_plot$PCMEAN + ifelse(is.na(MeanData_plot$PCSD), 0, MeanData_plot$PCSD), na.rm = TRUE)
# Add 10% padding, round up to nice number
y_linear_max_padded <- ceiling(y_linear_max * 1.1)
if (!is.finite(y_linear_max_padded) || y_linear_max_padded == 0) y_linear_max_padded <- NA

# XMAX and TICKS from SAS dynamic
XMAX_val <- 100
TICKS_vals <- c(0,4,8,12,24,36,48,72,96)

# Y label from SAS dynamic
YLABEL_val <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XLABEL_val <- "Time (h)"

# Prepare data for error bars based on SD param
# SD=BOTH -> yerrorlower=PCMEAN-PCSD, yerrorupper=PCMEAN+PCSD
# SD=UPPER -> yerrorlower=PCMEAN, yerrorupper=PCMEAN+PCSD
# SD=N -> no error bars
MeanData_plot <- MeanData_plot %>%
  mutate(
    y_lower = case_when(
      toupper(SD_param) == "BOTH" ~ PCMEAN - PCSD,
      toupper(SD_param) == "UPPER" ~ PCMEAN,
      TRUE ~ NA_real_
    ),
    y_upper = case_when(
      toupper(SD_param) %in% c("BOTH","UPPER") ~ PCMEAN + PCSD,
      TRUE ~ NA_real_
    )
  )

# For linear view, ensure lower not below 0 (viewmin=0 will clip, but we keep value for drawing)
# For log view, no error bars

# Create treatment palette: SAS uses black for all, differentiate by line type and marker
# Determine unique treatments
trt_levels <- MeanData_plot %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN)
n_trt <- nrow(trt_levels)
# Define shapes and linetypes (SAS default cycle)
shapes <- c(16, 17, 15, 18, 3, 8, 7, 9)  # circle, triangle, square, diamond, etc.
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
# Assign
trt_levels <- trt_levels %>%
  mutate(
    shape_id = shapes[(row_number()-1) %% length(shapes) + 1],
    lty_id = linetypes[(row_number()-1) %% length(linetypes) + 1]
  )

# Merge back for plotting
MeanData_plot <- MeanData_plot %>% left_join(trt_levels, by = c("TRTAN","TRTA"))

# Create lookup for scales
shape_vals <- setNames(trt_levels$shape_id, trt_levels$TRTA)
lty_vals <- setNames(trt_levels$lty_id, trt_levels$TRTA)

# ------------------------------------------------------------------
# Build two ggplot panels to mimic SAS template
# ------------------------------------------------------------------
# Common theme
theme_sas <- theme_bw() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5,5,5,5)
  )

# Linear view
p_linear <- ggplot(MeanData_plot, aes(x = PCTPT, y = PCMEAN, group = TRTA, shape = TRTA, linetype = TRTA)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 2.5, fill = "black") +
  { if (toupper(SD_param) %in% c("BOTH","UPPER")) geom_errorbar(aes(ymin = y_lower, ymax = y_upper), width = 1.5, color = "black", linewidth = 0.5) } +
  scale_x_continuous(name = XLABEL_val, limits = c(0, XMAX_val), breaks = TICKS_vals) +
  scale_y_continuous(name = YLABEL_val, limits = c(0, y_linear_max_padded), expand = expansion(mult = c(0, 0.05))) +
  scale_shape_manual(values = shape_vals) +
  scale_linetype_manual(values = lty_vals) +
  ggtitle("Linear view") +
  theme_sas +
  theme(plot.title = element_text(size = 9, hjust = 0.5))

# Semilog view - Y log base 10, viewmin auto, viewmax auto
# SAS: YAXISOPTS TYPE=LOG LOGOPTS BASE=10 viewmax=_YMAX (auto)
# Need to handle zero/negative PCMEAN (already set to NA if 0)
# Set log breaks as 10^n
# Determine log range
y_log_min <- min(MeanData_plot$PCMEAN, na.rm = TRUE)
y_log_max <- max(MeanData_plot$PCMEAN, na.rm = TRUE)
# Create log breaks: 0.1,1,10,100,1000 etc covering range
log_breaks <- 10^seq(floor(log10(max(y_log_min, 0.01))), ceiling(log10(y_log_max*1.2)))
# Filter to reasonable
log_breaks <- log_breaks[log_breaks >= 0.1 & log_breaks <= 10000]

p_semilog <- ggplot(MeanData_plot, aes(x = PCTPT, y = PCMEAN, group = TRTA, shape = TRTA, linetype = TRTA)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 2.5, fill = "black") +
  scale_x_continuous(name = XLABEL_val, limits = c(0, XMAX_val), breaks = TICKS_vals) +
  scale_y_log10(name = YLABEL_val, breaks = log_breaks, labels = function(x) format(x, scientific = FALSE, trim = TRUE)) +
  scale_shape_manual(values = shape_vals) +
  scale_linetype_manual(values = lty_vals) +
  ggtitle("Semilogarithmic view") +
  theme_sas +
  theme(plot.title = element_text(size = 9, hjust = 0.5)) +
  annotation_logticks(sides = "l", outside = FALSE)

# Combine plots side by side with shared legend at bottom (SAS sidebar align=bottom)
# Use grid to arrange with titles/footnotes
# Extract legend from one plot
get_legend <- function(p) {
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) tmp$grobs[[leg]] else nullGrob()
}
# Remove legends from individual plots for combined layout
p_linear_noleg <- p_linear + theme(legend.position = "none")
p_semilog_noleg <- p_semilog + theme(legend.position = "none")

# Create combined grobs
g1 <- ggplotGrob(p_linear_noleg)
g2 <- ggplotGrob(p_semilog_noleg)
legend_grob <- get_legend(p_linear + theme(legend.position = "bottom", legend.box = "horizontal"))

# Titles and footnotes as in SAS
title1_left <- "SPONSOR/PROTOCOL/PRODUCT INFO"
title1_right <- "page x of y"
title3 <- "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
title4 <- "Analysis set: PK analysis set"
footnote_text <- "PATH DATA/PROGRAM/OUTPUT"

# Build final PNG via grid
png_file <- file.path(out, "target13.png")
# SAS ODS graphics width=22.87cm height=12cm ; designwidth=2500px designheight=1400px
# Use ggsave equivalent via png device
width_cm <- 22.87
height_cm <- 12
width_in <- width_cm / 2.54
height_in <- height_cm / 2.54
dpi <- 300

png(filename = png_file, width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
grid.newpage()
# Layout: 4 rows: title1, title3/4, plots (2 cols), footnote + legend
# Use grid.layout
pushViewport(viewport(layout = grid.layout(nrow = 6, ncol = 2,
                                           heights = unit(c(0.6, 0.6, 0.6, 1, 8, 0.8), c("cm","cm","cm","cm","cm","cm")),
                                           widths = unit(c(0.5, 0.5), "npc"))))

# Row1: title J=L and J=R
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
grid.text(title1_left, x = unit(0.01, "npc"), y = unit(0.5, "npc"), just = c("left","center"),
          gp = gpar(fontsize = 8, fontfamily = "sans"))
grid.text(title1_right, x = unit(0.99, "npc"), y = unit(0.5, "npc"), just = c("right","center"),
          gp = gpar(fontsize = 8, fontfamily = "sans"))
popViewport()

# Row2: title3
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
grid.text(title3, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "center",
          gp = gpar(fontsize = 9, fontface = "bold", fontfamily = "sans"))
popViewport()

# Row3: title4
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
grid.text(title4, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "center",
          gp = gpar(fontsize = 9, fontfamily = "sans"))
popViewport()

# Row4: empty spacer (SAS lattice rowweights .9 .1 second row is legend area, but we handle separately)
# Row5: plots side by side
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1))
grid.draw(g1)
popViewport()
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 2))
grid.draw(g2)
popViewport()

# Row6: legend + footnote
pushViewport(viewport(layout.pos.row = 6, layout.pos.col = 1:2))
# footnote left
grid.text(footnote_text, x = unit(0.01, "npc"), y = unit(0.5, "npc"), just = c("left","center"),
          gp = gpar(fontsize = 7, fontfamily = "sans"))
# legend centered
if (!inherits(legend_grob, "null")) {
  pushViewport(viewport(x = 0.5, y = 0.5, width = 0.6, height = 0.9))
  grid.draw(legend_grob)
  popViewport()
}
popViewport()

dev.off()
message("Wrote PNG: ", png_file)

# ------------------------------------------------------------------
# Write CSV of plotted data (MeanData) - SAS dataset equivalent
# ------------------------------------------------------------------
# SAS variable names: TRTAN TRTA PCTPT PCMEAN PCSD (plus N if desired)
# Write with same case, dates as ISO (no dates here), numeric unrounded
csv_file <- file.path(out, "target13.csv")
# Prepare output dataset matching SAS MeanData
out_dataset <- MeanData %>%
  arrange(TRTAN, TRTA, PCTPT) %>%
  select(TRTAN, TRTA, PCTPT, PCMEAN, PCSD)

# Also include N for traceability (optional, but keep core columns)
# Write CSV with na=""
write.csv(out_dataset, csv_file, row.names = FALSE, na = "")
message("Wrote CSV: ", csv_file)

# Also write a TXT rendering of the figure data for verification (counts/stats)
txt_file <- file.path(out, "target13.txt")
sink(txt_file)
cat("SPONSOR/PROTOCOL/PRODUCT INFO                                                          page x of y\n")
cat("\n")
cat("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately\n")
cat("Analysis set: PK analysis set\n")
cat("\n")
cat("Parameters: PCTESTCD=", PCTESTCD_param, "  Meantype=", Meantype_param, "  SD=", SD_param, "\n", sep="")
cat("XMAX=", XMAX_val, "  TICKS=", paste(TICKS_vals, collapse=" "), "\n", sep="")
cat("YLABEL=", YLABEL_val, "\n", sep="")
cat("\n")
cat("MeanData (Arithmetic Mean, SD) by Treatment and Time:\n")
cat("--------------------------------------------------------------------------------\n")
# Print table with SAS-like formatting
# Format PCMEAN and PCSD to 4 decimals as SAS PROC MEANS default, but display as is
fmt <- function(x) ifelse(is.na(x), "", sprintf("%.4f", x))
out_print <- out_dataset %>%
  mutate(
    PCMEAN_fmt = fmt(PCMEAN),
    PCSD_fmt = fmt(PCSD),
    PCTPT_fmt = as.character(PCTPT)
  ) %>%
  select(TRTAN, TRTA, PCTPT_fmt, PCMEAN_fmt, PCSD_fmt)
# Use base print
print(as.data.frame(out_print), row.names = FALSE, right = TRUE)
cat("\n")
cat("Linear view: Y linear scale 0 to ", ifelse(is.na(y_linear_max_padded), "auto", y_linear_max_padded), "  X 0 to ", XMAX_val, "\n", sep="")
cat("Semilog view: Y log base 10\n")
cat("\n")
cat("PATH DATA/PROGRAM/OUTPUT\n")
sink()
message("Wrote TXT: ", txt_file)

# Verify outputs exist
stopifnot(file.exists(png_file))
stopifnot(file.exists(csv_file))
message("All outputs written to OUT_DIR=", out)
```