```r
# P057_pk_mean_conc.sas -> R translation
# Figure: Arithmetic Mean concentration-time plot per analyte
# Input: adpc.xpt (PCTESTCD=ANAL1, MEANTYPE=A, SD=BOTH)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # add 0.5 and floor, with small epsilon for floating point
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# helper: zap haven labels to plain vectors
zap_all <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled") || inherits(v, "haven_labelled_spss")) {
      # preserve numeric if numeric, else character
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      # haven labelled may still be labelled; convert to numeric/character
      if (is.numeric(v)) {
        df[[nm]] <- as.numeric(v)
      } else {
        df[[nm]] <- as.character(v)
      }
    } else if (is.factor(v)) {
      df[[nm]] <- as.character(v)
    }
  }
  df
}

# locate adpc.xpt
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  file.path(Sys.getenv("ROOT"), "data/derived/cdisc-pilot/adam/adpc.xpt")
)
# also try lower case
candidates <- unique(c(candidates,
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADPC.xpt"
))
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { adpc_path <- p; break }
}
# fallback: search inlib env
if (is.null(adpc_path)) {
  # try haven read from libref directory (sas7bdat)
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
  if (dir.exists(alt)) {
    f <- list.files(alt, pattern = "^adpc\\.", ignore.case = TRUE, full.names = TRUE)
    if (length(f) > 0) adpc_path <- f[1]
  }
}
if (is.null(adpc_path)) {
  stop("adpc.xpt not found. Searched: ", paste(candidates, collapse="; "))
}
cat("Reading ADPC from:", adpc_path, "\n")

# read
if (grepl("\\.xpt$", adpc_path, ignore.case = TRUE)) {
  adpc_raw <- haven::read_xpt(adpc_path)
} else if (grepl("\\.sas7bdat$", adpc_path, ignore.case = TRUE)) {
  adpc_raw <- haven::read_sas(adpc_path)
} else {
  adpc_raw <- haven::read_xpt(adpc_path)
}
adpc_raw <- zap_all(adpc_raw)

# Ensure expected columns exist (case-insensitive handling)
# SAS keeps: ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
# Normalize names to upper for checking, but keep original
names(adpc_raw) <- toupper(names(adpc_raw))
# Check required
req <- c("PCTESTCD","TRTAN","TRTA","PCTPT","EPLTM","PCORRESN","PCORRESU","PCSPEC","PCTESTL","ANLPRNT")
# EPLTM exists, PCTPT may not exist yet
# Filter PCTESTCD = ANAL1
PCTESTCD_val <- "ANAL1"
Meantype <- "A"
SD_opt <- "BOTH"  # BOTH, UPPER, N

adpc_filt <- adpc_raw[adpc_raw$PCTESTCD == PCTESTCD_val, , drop=FALSE]
cat("Rows after PCTESTCD filter:", nrow(adpc_filt), "\n")

# Derive PCTPT = input(EPLTM, best10.)  -> numeric
# EPLTM may be character or numeric
if (!"EPLTM" %in% names(adpc_filt)) {
  stop("EPLTM not found in ADPC")
}
# Convert EPLTM to character then numeric (SAS best10. ignores leading/trailing spaces)
adpc_filt$EPLTM_chr <- as.character(adpc_filt$EPLTM)
adpc_filt$PCTPT <- suppressWarnings(as.numeric(trimws(adpc_filt$EPLTM_chr)))
# ORDERBY and BYVAL6 (for reference, not used in plot but kept)
# ORDERBY=compress( ANLPRNT||PCSPEC||PCTESTL||TRTAN)
# BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
# Implement helpers
compress_sas <- function(x) gsub(" ", "", x, fixed = TRUE)
propcase <- function(x) {
  # SAS propcase: first letter upper, rest lower per word
  sapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    w <- strsplit(tolower(s), " ", fixed=TRUE)[[1]]
    w <- paste0(toupper(substr(w,1,1)), substr(w,2,nchar(w)))
    paste(w, collapse=" ")
  }, USE.NAMES=FALSE)
}
compbl <- function(x) gsub("\\s+", " ", trimws(x))

adpc_filt$ORDERBY <- compress_sas(paste0(
  ifelse(is.na(adpc_filt$ANLPRNT), "", as.character(adpc_filt$ANLPRNT)),
  ifelse(is.na(adpc_filt$PCSPEC), "", as.character(adpc_filt$PCSPEC)),
  ifelse(is.na(adpc_filt$PCTESTL), "", as.character(adpc_filt$PCTESTL)),
  ifelse(is.na(adpc_filt$TRTAN), "", as.character(adpc_filt$TRTAN))
))
adpc_filt$BYVAL6 <- compbl(paste0(propcase(adpc_filt$PCTESTCD), " Conc ", " (", adpc_filt$PCORRESU, ")"))

# Keep only needed and sort by TRTAN TRTA PCTPT (SAS sort: character ordering, missing first)
# Ensure TRTAN numeric, TRTA character
adpc_filt$TRTAN <- suppressWarnings(as.numeric(adpc_filt$TRTAN))
adpc_filt$TRTA <- as.character(adpc_filt$TRTA)
adpc_filt$PCORRESN <- suppressWarnings(as.numeric(adpc_filt$PCORRESN))

# Sort
ord <- order(adpc_filt$TRTAN, adpc_filt$TRTA, adpc_filt$PCTPT, na.last = FALSE)
adpc <- adpc_filt[ord, , drop=FALSE]

# PROC UNIVARIATE by TRTAN TRTA PCTPT var PCORRESN mean STD
# Use dplyr
library(dplyr)

# Remove rows where PCTPT is NA? SAS would include them as a BY group with missing PCTPT first
# Keep them, but for plot we will filter NA PCTPT later
summary_df <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    PCMEAN = ifelse(sum(!is.na(PCORRESN)) > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
    PCSD = ifelse(sum(!is.na(PCORRESN)) > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(TRTAN, TRTA, PCTPT)

# SAS: if PCMEAN=0 then PCMEAN=. PCSD=.
summary_df$PCMEAN[summary_df$PCMEAN == 0 & !is.na(summary_df$PCMEAN)] <- NA_real_
summary_df$PCSD[is.na(summary_df$PCMEAN)] <- NA_real_
# Actually SAS sets both to . when PCMEAN=0, so need to handle after
# Re-apply: where original PCMEAN was 0, set PCSD NA (already)
# The above already sets PCSD NA where PCMEAN NA, but need to ensure PCSD also NA for those rows
# Already done

# For geometric mean case (not used here) would be log transform, but include for completeness
if (toupper(Meantype) == "G") {
  # Not executed for this call, but keep logic
  adpc$LOGORRES <- ifelse(!is.na(adpc$PCORRESN), log(adpc$PCORRESN), NA_real_)
  summary_df <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(LOGORRES)),
      Logmean = mean(LOGORRES, na.rm=TRUE),
      logSD = sd(LOGORRES, na.rm=TRUE),
      .groups="drop"
    )
  summary_df$PCMEAN <- exp(summary_df$Logmean)
  summary_df$PCSD <- exp(summary_df$logSD)
}

# MeanData is summary_df
MeanData <- summary_df

# Determine axis settings from SAS dynamic
XMAX <- 100
TICKS <- c(0,4,8,12,24,36,48,72,96)
XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
# YMAX not provided -> compute
# For linear view, viewmax = _YMAX (missing) -> auto; we compute max PCMEAN+PCSD
ymax_linear <- suppressWarnings(max(MeanData$PCMEAN + MeanData$PCSD, na.rm=TRUE))
if (!is.finite(ymax_linear)) ymax_linear <- suppressWarnings(max(MeanData$PCMEAN, na.rm=TRUE))
# add 10% padding
ymax_linear <- ymax_linear * 1.1
# For log view, viewmax = _YMAX (same) -> use max PCMEAN
ymax_log <- suppressWarnings(max(MeanData$PCMEAN, na.rm=TRUE)) * 1.2

# Prepare data for plotting: remove NA PCTPT or NA PCMEAN? SAS would still plot missing? But we filter NA PCMEAN for visibility
plot_data <- MeanData %>% filter(!is.na(PCTPT), !is.na(PCMEAN))

# Ensure TRTA is factor ordered by TRTAN
trt_order <- plot_data %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN)
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_order$TRTA)
# Also keep TRTAN for grouping

# Write CSV of plotted data (MeanData)
out_csv <- file.path(out, "target13.csv")
write.csv(MeanData, out_csv, row.names = FALSE, na = "")
# Also write plot_data csv for convenience (same as required CSV of plotted data)
out_csv2 <- file.path(out, "target13_plot_data.csv")
write.csv(plot_data, out_csv2, row.names = FALSE, na = "")
cat("Wrote CSV:", out_csv, "rows", nrow(MeanData), "\n")

# Create plots with ggplot2
library(ggplot2)

# Common theme
theme_pk <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color="grey90", linewidth=0.3),
    axis.title = element_text(size=10),
    axis.text = element_text(size=9),
    plot.title = element_text(size=9, hjust=0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    legend.key.width = unit(1.2, "cm")
  )

# Define shapes and linetypes per treatment (up to 3)
n_trt <- length(levels(plot_data$TRTA))
shapes <- c(16,17,15,18,8,3)[seq_len(n_trt)]
linetypes <- c("solid","dashed","dotted","dotdash","longdash","twodash")[seq_len(n_trt)]

# Linear view
p1 <- ggplot(plot_data, aes(x=PCTPT, y=PCMEAN, group=TRTA, color=TRTA, shape=TRTA, linetype=TRTA)) +
  geom_line(linewidth=0.7, color="black") +
  geom_point(size=2.5, color="black", fill="black") +
  # error bars BOTH
  geom_errorbar(aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD),
                width=1.5, color="black", linewidth=0.5, na.rm=TRUE) +
  scale_x_continuous(name=XLABEL, limits=c(0, XMAX), breaks=TICKS) +
  scale_y_continuous(name=YLABEL, limits=c(0, ymax_linear)) +
  scale_shape_manual(values=shapes) +
  scale_linetype_manual(values=linetypes) +
  scale_color_manual(values=rep("black", n_trt)) +
  ggtitle("Linear view") +
  theme_pk +
  theme(legend.position="none")

# Semilog view
# For log scale, need to handle zero lower limit; set lower limit to min positive
ymin_log <- suppressWarnings(min(plot_data$PCMEAN[plot_data$PCMEAN>0], na.rm=TRUE)) * 0.8
if (!is.finite(ymin_log) || ymin_log <=0) ymin_log <- 0.1

p2 <- ggplot(plot_data, aes(x=PCTPT, y=PCMEAN, group=TRTA, color=TRTA, shape=TRTA, linetype=TRTA)) +
  geom_line(linewidth=0.7, color="black") +
  geom_point(size=2.5, color="black", fill="black") +
  scale_x_continuous(name=XLABEL, limits=c(0, XMAX), breaks=TICKS) +
  scale_y_log10(name=YLABEL, limits=c(ymin_log, ymax_log)) +
  scale_shape_manual(values=shapes) +
  scale_linetype_manual(values=linetypes) +
  scale_color_manual(values=rep("black", n_trt)) +
  ggtitle("Semilogarithmic view") +
  theme_pk +
  theme(legend.position="none")

# Combine using grid
# Create legend separately
# Build a dummy plot for legend
p_legend <- ggplot(plot_data, aes(x=PCTPT, y=PCMEAN, group=TRTA, color=TRTA, shape=TRTA, linetype=TRTA)) +
  geom_line(aes(color=TRTA, linetype=TRTA), linewidth=0.7) +
  geom_point(aes(shape=TRTA), size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_linetype_manual(values=linetypes) +
  scale_color_manual(values=rep("black", n_trt)) +
  theme_bw() +
  theme(legend.position="bottom", legend.title=element_blank(), legend.text=element_text(size=9))

# Extract legend via cowplot-like method using ggplotGrob
get_legend <- function(a.gplot){
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) tmp$grobs[[leg]] else nullGrob()
}
library(grid)
legend_grob <- get_legend(p_legend)

# Arrange with grid
png_file <- file.path(out, "target13.png")
# Use png device with high res
# Width 22.87cm ~ 9in, height 12cm ~ 4.7in; use 2500x1400 px equivalent at 300dpi
# We'll use ggsave with grid arrangement via gridExtra alternative: manual grid

# Create combined grob
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

# Open png
png(png_file, width=2500, height=1400, res=300, units="px", bg="white")
grid.newpage()
# Layout: 2 columns, 2 rows (row heights 0.9, 0.1)
pushViewport(viewport(layout=grid.layout(2,2, heights=unit(c(0.9,0.1), "npc"), widths=unit(c(0.5,0.5), "npc"))))
# Top left
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
grid.draw(g1)
popViewport()
# Top right
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.draw(g2)
popViewport()
# Bottom legend spanning both columns
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:2))
grid.draw(legend_grob)
popViewport()
# Add titles as grid text
# Main titles (simulate SAS titles)
grid.text("SPONSOR/PROTOCOL/PRODUCT INFO", x=unit(0.02,"npc"), y=unit(0.99,"npc"), just=c("left","top"), gp=gpar(fontsize=8))
grid.text("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", x=unit(0.5,"npc"), y=unit(0.97,"npc"), just=c("center","top"), gp=gpar(fontsize=9, fontface="bold"))
grid.text("Analysis set: PK analysis set", x=unit(0.5,"npc"), y=unit(0.95,"npc"), just=c("center","top"), gp=gpar(fontsize=8))
grid.text("PATH DATA/PROGRAM/OUTPUT", x=unit(0.02,"npc"), y=unit(0.02,"npc"), just=c("left","bottom"), gp=gpar(fontsize=6))
popViewport()
dev.off()
cat("Wrote PNG:", png_file, "\n")

# Also create a ggsave version for verification (single combined via grid)
# Write TXT rendering (table body)
txt_file <- file.path(out, "target13.txt")
sink(txt_file)
cat("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately\n")
cat("Analysis set: PK analysis set\n")
cat("PCTESTCD=ANAL1  Meantype=A  SD=BOTH\n\n")
cat("Mean concentration-time data used for plot:\n\n")
# Print table with formatting similar to SAS
# Format: TRTAN TRTA PCTPT N PCMEAN PCSD
fmt_num <- function(x, dig=2) {
  ifelse(is.na(x), "", format(round_half_up(x, dig), nsmall=dig, trim=TRUE))
}
print_df <- MeanData %>%
  mutate(
    PCMEAN_f = fmt_num(PCMEAN, 2),
    PCSD_f = fmt_num(PCSD, 2),
    PCTPT_f = fmt_num(PCTPT, 1)
  ) %>%
  select(TRTAN, TRTA, PCTPT=PCTPT_f, N, PCMEAN=PCMEAN_f, PCSD=PCSD_f)
print(print_df, row.names=FALSE)
cat("\nX-axis ticks:", paste(TICKS, collapse=" "), "  XMAX=", XMAX, "\n")
cat("Y-axis (linear) max ~", round_half_up(ymax_linear,2), "  Y-axis (log) max ~", round_half_up(ymax_log,2), "\n")
cat("\nNote: Linear view shows mean +/- SD (both directions). Semilog view shows mean only on log10 scale.\n")
sink()
cat("Wrote TXT:", txt_file, "\n")

# Also ensure dataset output as per expected outds (if any)
# The SAS program produced no dataset output besides figure, but we have CSVs

cat("Done. Outputs in", out, "\n")
```