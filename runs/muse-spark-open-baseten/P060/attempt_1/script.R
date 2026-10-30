# P060_pk_subj_conc.sas -> R translation
# Figure 16.2.5-x.x Individual concentration-time profiles by compound, matrix, analyte
# Analysis Set: PK analysis set
# Input: adsl.xpt, adpc.xpt  Output: sasgraph.png + CSV of plotted data

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS ROUND)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(abs+0.5) then restore sign
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# helper: find file in multiple candidate locations
find_candidate <- function(fname, subdir = "adam") {
  candidates <- c(
    Sys.getenv("rawadam"),
    Sys.getenv("RAWADAM"),
    Sys.getenv("rawADAM"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
    "/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "./data/raw/cdisc-pilot/adam",
    "./data/derived/cdisc-pilot/adam",
    file.path(out, "..", "..", "data", "raw", "cdisc-pilot", "adam"),
    file.path(getwd(), "data", "raw", "cdisc-pilot", "adam")
  )
  # also try inlib path
  candidates <- c(candidates,
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
    Sys.getenv("inlib"),
    Sys.getenv("INLIB")
  )
  candidates <- unique(candidates[candidates != "" & !is.na(candidates)])
  # normalize slashes
  candidates <- gsub("\\\\", "/", candidates)
  for (d in candidates) {
    p1 <- file.path(d, fname)
    if (file.exists(p1)) return(p1)
    # try case-insensitive search in directory
    if (dir.exists(d)) {
      fl <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      idx <- which(tolower(basename(fl)) == tolower(fname))
      if (length(idx) > 0) return(fl[idx[1]])
    }
  }
  # fallback: search from root
  roots <- c("D:/meta_hackathon/sas2r-agent", ".", getwd(), dirname(out))
  for (r in roots) {
    if (!dir.exists(r)) next
    fl <- list.files(r, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(fl) > 0) return(fl[1])
  }
  return(file.path(candidates[1], fname))
}

# helper: case-insensitive column lookup
get_col <- function(df, name) {
  idx <- which(tolower(names(df)) == tolower(name))
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}
get_vec <- function(df, name) {
  cn <- get_col(df, name)
  if (is.null(cn)) return(NULL)
  df[[cn]]
}

# helper: zap haven labels but keep values
zap_all <- function(df) {
  # convert labelled to plain
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  # convert POSIXct etc to character for CSV? keep as is, handle later
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}

# --- locate inputs ---
adsl_path <- find_candidate("adsl.xpt", "adam")
adpc_path <- find_candidate("adpc.xpt", "adam")

cat("ADSL path:", adsl_path, "\n")
cat("ADPC path:", adpc_path, "\n")

# --- read ADSL (not directly plotted but kept for completeness / filtering) ---
adsl <- NULL
if (file.exists(adsl_path)) {
  adsl_raw <- haven::read_xpt(adsl_path)
  adsl <- zap_all(adsl_raw)
} else {
  cat("WARNING: adsl.xpt not found, continuing without it\n")
}

# --- read ADPC ---
adpc <- NULL
if (file.exists(adpc_path)) {
  adpc_raw <- haven::read_xpt(adpc_path)
  adpc <- zap_all(adpc_raw)
} else {
  stop(paste0("ADPC file not found: ", adpc_path))
}

cat("ADPC rows:", nrow(adpc), " cols:", ncol(adpc), "\n")
cat("ADPC names:", paste(names(adpc), collapse=", "), "\n")

# --- reproduce WORK.ADSL / WORK.ADPC (no transformation) and SORTadpc ---
# SAS: PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc BY USUBJID parcat1 eactm;

# Identify key columns case-insensitively
cn_usubjid <- get_col(adpc, "USUBJID")
cn_parcat1 <- get_col(adpc, "PARCAT1")
cn_eactm   <- get_col(adpc, "EACTM")
cn_aval    <- get_col(adpc, "AVAL")

if (is.null(cn_usubjid)) stop("USUBJID not found in ADPC")
if (is.null(cn_parcat1)) {
  # try alternative: PARCAT1 may be missing, create dummy
  cat("WARNING: PARCAT1 not found, creating dummy\n")
  adpc$PARCAT1 <- "All"
  cn_parcat1 <- "PARCAT1"
}
if (is.null(cn_eactm)) stop("EACTM not found in ADPC")
if (is.null(cn_aval)) stop("AVAL not found in ADPC")

# Ensure character ordering like SAS (uppercase before lowercase, case-sensitive)
# R default order is similar for ASCII; use xtfrm for character
# Missing values sort first (SAS default) -> na.last = NA then re-add? Actually SAS missing first.
# We'll sort with order() and na.last = TRUE? SAS missing first, so na.last=FALSE for ascending?
# For character, "" or NA should be first. Use na.last=FALSE.

# Convert to data.frame for sorting
sort_adpc <- adpc

# Create sort keys (handle NA)
# Use base order with na.last = FALSE to put NA first
ord <- order(
  get_vec(sort_adpc, cn_usubjid),
  get_vec(sort_adpc, cn_parcat1),
  get_vec(sort_adpc, cn_eactm),
  na.last = FALSE,
  method = "radix"
)
sort_adpc <- sort_adpc[ord, , drop = FALSE]

# Write dataset CSV (WORK.SORTadpc equivalent)
# Format dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# Identify date/datetime columns via haven attributes or class
format_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    col <- df2[[nm]]
    if (inherits(col, "Date")) {
      df2[[nm]] <- format(col, "%Y-%m-%d")
    } else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
      df2[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
    } else if (is.list(col)) {
      df2[[nm]] <- sapply(col, function(x) ifelse(is.null(x) || is.na(x), "", as.character(x)))
    }
  }
  df2
}

sort_adpc_csv <- format_for_csv(sort_adpc)
write.csv(sort_adpc_csv, file.path(out, "SORTadpc.csv"), row.names = FALSE, na = "")
# Also write as sasgraph plotted data (same content, filtered to non-missing plot vars)
plotted_data <- sort_adpc[!is.na(get_vec(sort_adpc, cn_aval)) & !is.na(get_vec(sort_adpc, cn_eactm)), , drop = FALSE]
plotted_data_csv <- format_for_csv(plotted_data)
write.csv(plotted_data_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

cat("SORTadpc written:", nrow(sort_adpc), "rows\n")
cat("Plotted data written:", nrow(plotted_data), "rows\n")

# --- Prepare plot data ---
# SAS: PROC GPLOT DATA=WORK.SORTadpc NOCACHE; PLOT AVAL * eactm / VAXIS=AXIS1 HAXIS=AXIS2 FRAME; BY USUBJID parcat1;
# SYMBOL1 INTERPOL=JOIN VALUE=NONE LINE=1 WIDTH=2 CV=_STYLE_
# Axis1 LABEL="Conc (ug/mL)" Axis2 LABEL="Time (hour)"

library(ggplot2)
library(dplyr)

# Build a clean plotting dataframe with standardized names
plot_df <- data.frame(
  USUBJID = get_vec(plotted_data, cn_usubjid),
  PARCAT1 = get_vec(plotted_data, cn_parcat1),
  EACTM   = suppressWarnings(as.numeric(get_vec(plotted_data, cn_eactm))),
  AVAL    = suppressWarnings(as.numeric(get_vec(plotted_data, cn_aval))),
  stringsAsFactors = FALSE
)

# Remove rows where both are NA (already filtered) but keep for safety
plot_df <- plot_df[!is.na(plot_df$EACTM) & !is.na(plot_df$AVAL), ]

# Create BY group label for faceting
plot_df$BY_LABEL <- paste0(plot_df$USUBJID, " / ", plot_df$PARCAT1)
# Keep original ordering for facets: order by USUBJID, PARCAT1
by_order <- unique(plot_df[order(plot_df$USUBJID, plot_df$PARCAT1), c("USUBJID","PARCAT1","BY_LABEL")])
plot_df$BY_LABEL <- factor(plot_df$BY_LABEL, levels = unique(by_order$BY_LABEL))

n_facets <- nlevels(plot_df$BY_LABEL)
cat("Number of BY groups (USUBJID*PARCAT1):", n_facets, "\n")

# Determine layout: aim for readable grid
# Use facet_wrap with ncol based on n_facets
if (n_facets <= 4) {
  ncol_facet <- n_facets
  nrow_facet <- 1
} else if (n_facets <= 12) {
  ncol_facet <- 3
  nrow_facet <- ceiling(n_facets / ncol_facet)
} else if (n_facets <= 24) {
  ncol_facet <- 4
  nrow_facet <- ceiling(n_facets / ncol_facet)
} else {
  ncol_facet <- 5
  nrow_facet <- ceiling(n_facets / ncol_facet)
}

# Dynamic figure size: width ~ 3.5 inch per column, height ~ 2.8 inch per row, plus titles
fig_width  <- max(8, min(20, ncol_facet * 3.5 + 1))
fig_height <- max(6, min(30, nrow_facet * 2.8 + 2))

# Titles / footnotes matching SAS
title1 <- "(page x of x)"
title2 <- "Figure 16.2.5-x.x Individual concentration-time profiles"
title3 <- "by compound, matrix, analyte and [actual/randomised][treatments/group]"
title4 <- "Analysis Set : PK analysis set"
footnote_txt <- paste0("Generated by the R System on ", format(Sys.Date(), "%d %B %Y"), " at ", format(Sys.time(), "%H:%M"))

# Build ggplot
p <- ggplot(plot_df, aes(x = EACTM, y = AVAL, group = BY_LABEL)) +
  geom_line(linewidth = 0.8, color = "black", na.rm = TRUE) +
  # VALUE=NONE means no symbol, so no points
  facet_wrap(~ BY_LABEL, ncol = ncol_facet, scales = "free_y") +
  labs(
    title = paste(title1, title2, title3, title4, sep = "\n"),
    x = "Time (hour)",
    y = "Conc (ug/mL)",
    caption = footnote_txt
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), # FRAME
    strip.background = element_rect(fill = "grey95", color = "black"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold", lineheight = 1.1),
    plot.caption = element_text(hjust = 0, size = 7, color = "grey30"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# Save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = fig_width, height = fig_height, dpi = 300, units = "in", bg = "white")
cat("Figure saved to:", png_path, " size:", fig_width, "x", fig_height, "in\n")

# Also ensure a text rendering of the table body (counts) for completeness
# Create a summary txt that mirrors SAS listing expectations
txt_path <- file.path(out, "sasgraph.txt")
sink(txt_path)
cat(paste(title1, title2, title3, title4, sep="\n"), "\n\n")
cat("Individual concentration-time profiles (AVAL vs EACTM) by USUBJID and PARCAT1\n")
cat("Analysis Set: PK analysis set\n")
cat("----------------------------------------------------------------\n")
cat("Number of subjects:", length(unique(plot_df$USUBJID)), "\n")
cat("Number of BY groups (USUBJID x PARCAT1):", n_facets, "\n")
cat("Total observations plotted:", nrow(plot_df), "\n\n")
# Show first few BY groups summary
summ <- plot_df %>%
  group_by(USUBJID, PARCAT1) %>%
  summarise(
    N = n(),
    Mean_AVAL = mean(AVAL, na.rm = TRUE),
    Min_EACTM = min(EACTM, na.rm = TRUE),
    Max_EACTM = max(EACTM, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(summ), row.names = FALSE)
cat("\n")
cat(footnote_txt, "\n")
sink()

cat("Text rendering saved to:", txt_path, "\n")
cat("Done.\n")
