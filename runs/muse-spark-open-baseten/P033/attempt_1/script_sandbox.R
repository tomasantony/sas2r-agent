# P033_target12.sas -> R translation
# Figure 16.2.5-x.x Individual concentration-time profiles
# Outputs: OUT_DIR/sasgraph.png + OUT_DIR/sasgraph.csv (plotted data)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # for NA keep NA
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  res[is.na(x)] <- NA_real_
  res
}

# helper: format dates for CSV export
format_for_csv <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "Date")) {
      df[[nm]] <- format(col, "%Y-%m-%d")
    } else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
      df[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(col, "POSIXlt")) {
      df[[nm]] <- format(as.POSIXct(col), "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

# helper: convert haven labelled vectors to R native
zap_haven <- function(df) {
  # haven::read_xpt returns haven_labelled; convert to plain
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      # zap labels but keep underlying values
      col <- haven::zap_labels(col)
      col <- haven::zap_formats(col)
      # if still labelled, zap again
      if (haven::is.labelled(col)) col <- haven::zap_labels(col)
      df[[nm]] <- col
    }
    # convert factors (from zap) to character to preserve SAS character semantics
    if (is.factor(df[[nm]])) {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  df
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(readr)
})

# -------------------------------------------------------------------------
# Paths from macro variables
root     <- "/bench"
rawadam  <- "/bench/data/raw/cdisc-pilot/adam"
rawsdtm  <- "/bench/data/raw/cdisc-pilot/sdtm"
rawsplit <- "/bench/data/derived/cdisc-pilot/adam-split"
inlib_dir <- "/bench/data/derived/cdisc-pilot/adam"

# Try to read ADSL and ADPC from rawadam (as in SAS program)
adsl_path <- file.path(rawadam, "adsl.xpt")
adpc_path <- file.path(rawadam, "adpc.xpt")

# fallback locations if primary not found
if (!file.exists(adpc_path)) {
  alt1 <- file.path(inlib_dir, "adpc.xpt")
  alt2 <- file.path(root, "data/derived/cdisc-pilot/adam/adpc.xpt")
  alt3 <- file.path(root, "data/raw/cdisc-pilot/adam/adpc.xpt")
  for (p in c(alt1, alt2, alt3)) if (file.exists(p)) { adpc_path <- p; break }
}
if (!file.exists(adsl_path)) {
  alt1 <- file.path(inlib_dir, "adsl.xpt")
  alt2 <- file.path(root, "data/derived/cdisc-pilot/adam/adsl.xpt")
  for (p in c(alt1, alt2)) if (file.exists(p)) { adsl_path <- p; break }
}

# Also handle .sas7bdat fallback
if (!file.exists(adpc_path)) {
  cand <- file.path(inlib_dir, "adpc.sas7bdat")
  if (file.exists(cand)) adpc_path <- cand
}
if (!file.exists(adsl_path)) {
  cand <- file.path(inlib_dir, "adsl.sas7bdat")
  if (file.exists(cand)) adsl_path <- cand
}

# Read datasets
read_sas_dataset <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Input file not found: ", path))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else {
    stop(paste0("Unsupported file type: ", path))
  }
  df <- zap_haven(df)
  # ensure names are as in SAS (haven preserves case)
  df
}

adsl <- read_sas_dataset(adsl_path)
adpc <- read_sas_dataset(adpc_path)

# -------------------------------------------------------------------------
# Replicate SAS DATA steps: work.adsl and work.adpc are direct copies
work_adsl <- adsl
work_adpc <- adpc

# PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc; BY USUBJID parcat1 eactm;
# SAS missing values sort first; character ordering uppercase before lowercase (ASCII)
# Use base order with na.last=FALSE to mimic SAS

# Ensure expected columns exist; if case differs, find case-insensitive match
find_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}
col_usubjid <- find_col(work_adpc, "USUBJID")
col_parcat1 <- find_col(work_adpc, "PARCAT1")
col_eactm   <- find_col(work_adpc, "EACTM")
col_aval    <- find_col(work_adpc, "AVAL")

if (is.na(col_usubjid) || is.na(col_parcat1) || is.na(col_eactm)) {
  stop("Required BY variables USUBJID, PARCAT1, EACTM not found in ADPC")
}

# Standardize column names to expected case for downstream (keep original case for CSV)
# Create sorted copy
# Use order with na.last=FALSE for EACTM; for character columns, order handles ASCII
# Build order vectors
ord_usubjid <- work_adpc[[col_usubjid]]
ord_parcat1 <- work_adpc[[col_parcat1]]
ord_eactm   <- work_adpc[[col_eactm]]

# Convert haven labelled already zapped; ensure numeric for eactm
# order() with multiple vectors and na.last=FALSE
ord_idx <- order(ord_usubjid, ord_parcat1, ord_eactm, na.last = FALSE, method = "radix")
work_sortadpc <- work_adpc[ord_idx, , drop = FALSE]

# -------------------------------------------------------------------------
# Prepare plotted data CSV
# The SAS GPLOT uses AVAL * eactm with BY USUBJID parcat1
# Keep all columns for traceability, but ensure sorted order
plotted_data <- work_sortadpc

# Format dates for CSV export (SAS dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS)
plotted_data_csv <- format_for_csv(plotted_data)

# Write CSV of plotted data (required: sasgraph.csv)
write.csv(plotted_data_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# Also write the sorted dataset as dataset output (for completeness)
write.csv(plotted_data_csv, file.path(out, "SORTadpc.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Build figure: PROC GPLOT PLOT AVAL * eactm / VAXIS=AXIS1 HAXIS=AXIS2 FRAME; BY USUBJID parcat1;
# SYMBOL1 INTERPOL=JOIN VALUE=NONE LINE=1 WIDTH=2
# Axis1 LABEL="Conc (ug/mL)" ; Axis2 LABEL="Time (hour)"

# Determine axis labels
x_var <- col_eactm
y_var <- col_aval

# Ensure numeric
plotted_data[[x_var]] <- suppressWarnings(as.numeric(plotted_data[[x_var]]))
plotted_data[[y_var]] <- suppressWarnings(as.numeric(plotted_data[[y_var]]))

# Create BY group identifier for faceting
# SAS BY USUBJID parcat1 produces separate plots per combination
# In R we facet to reproduce all in one PNG (single file expected: sasgraph.png)
# If many groups, facet_wrap will paginate; we limit to reasonable layout

# Build facet label: USUBJID + PARCAT1
# Preserve original character values
plotted_data$BY_USUBJID <- plotted_data[[col_usubjid]]
plotted_data$BY_PARCATA <- plotted_data[[col_parcat1]]
# Combined facet strip
plotted_data$facet_label <- paste0(plotted_data$BY_USUBJID, " / ", plotted_data$BY_PARCATA)

# Count distinct BY groups
n_groups <- nrow(unique(plotted_data[, c("BY_USUBJID", "BY_PARCATA"), drop = FALSE]))
# Determine facet layout: aim for ~3 columns
ncol_facet <- min(4, max(1, ceiling(sqrt(n_groups))))
nrow_facet <- ceiling(n_groups / ncol_facet)

# Titles from SAS
title1 <- "(page x of x)"
title2 <- "Figure 16.2.5-x.x Individual concentration-time profiles"
title3 <- "by compound, matrix, analyte and [actual/randomised][treatments/group]"
title4 <- "Analysis Set : PK analysis set"
footnote_text <- paste0("Generated by R on ", format(Sys.Date(), "%Y-%m-%d"), " at ", format(Sys.time(), "%H:%M:%S"))

# Build ggplot
p <- ggplot(plotted_data, aes(x = .data[[x_var]], y = .data[[y_var]], group = interaction(.data[["BY_USUBJID"]], .data[["BY_PARCATA"]]))) +
  geom_line(linewidth = 0.8, na.rm = TRUE, color = "black") +
  # VALUE=NONE means no symbol; we omit points to match SAS SYMBOL VALUE=NONE
  labs(
    title = paste0(title1, "\n", title2),
    subtitle = paste0(title3, "\n", title4),
    x = "Time (hour)",
    y = "Conc (ug/mL)",
    caption = footnote_text
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    plot.caption = element_text(hjust = 0, size = 7, face = "italic"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 7),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)))

# Add faceting if more than one BY group
if (n_groups > 1) {
  p <- p + facet_wrap(~ facet_label, ncol = ncol_facet, scales = "free_y")
} else {
  # single group: add subtitle with BY info
  p <- p + labs(subtitle = paste0(title3, "\n", title4, "\n", unique(plotted_data$facet_label)[1]))
}

# Determine PNG size: width/height adapt to number of facets
# Base size 10x7 for few groups, larger for many
png_width  <- max(10, ncol_facet * 4)
png_height <- max(7, nrow_facet * 3 + 2)  # +2 for titles
# Cap to avoid huge image
png_width  <- min(png_width, 16)
png_height <- min(png_height, 20)

# Save figure
ggsave(filename = file.path(out, "sasgraph.png"), plot = p, width = png_width, height = png_height, dpi = 300, units = "in", bg = "white")

# Also ensure a plain-text rendering of the figure metadata (for completeness)
txt_path <- file.path(out, "sasgraph.txt")
sink(txt_path)
cat(title1, "\n")
cat(title2, "\n")
cat(title3, "\n")
cat(title4, "\n")
cat("\n")
cat("Axis1: Conc (ug/mL)  Axis2: Time (hour)\n")
cat("SYMBOL: INTERPOL=JOIN VALUE=NONE LINE=1 WIDTH=2\n")
cat("BY variables: USUBJID PARCAT1\n")
cat(paste0("Number of BY groups: ", n_groups, "\n"))
cat(paste0("Number of observations plotted: ", nrow(plotted_data), "\n"))
cat(paste0("Footnote: ", footnote_text, "\n"))
cat("\nBY groups:\n")
by_summary <- plotted_data %>%
  group_by(BY_USUBJID, BY_PARCATA) %>%
  summarise(N = n(), .groups = "drop") %>%
  arrange(BY_USUBJID, BY_PARCATA)
for (i in seq_len(nrow(by_summary))) {
  cat(paste0(by_summary$BY_USUBJID[i], " / ", by_summary$BY_PARCATA[i], " : N=", by_summary$N[i], "\n"))
}
sink()

# -------------------------------------------------------------------------
# Write additional dataset outputs if needed (ADSL/ADPC copies)
# These are not required by expected outputs but satisfy "dataset" output type
# Write as CSV with formatted dates
work_adsl_csv <- format_for_csv(work_adsl)
work_adpc_csv <- format_for_csv(work_adpc)
# Only write if not already written to avoid clutter; write to OUT_DIR with distinct names
# Uncomment if needed for audit:
# write.csv(work_adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
# write.csv(work_adpc_csv, file.path(out, "adpc.csv"), row.names = FALSE, na = "")
