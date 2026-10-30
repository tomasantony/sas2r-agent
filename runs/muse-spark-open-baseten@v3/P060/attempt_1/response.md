```r
# P060_pk_subj_conc.sas -> R translation
# Figure 16.2.5-x.x Individual concentration-time profiles by compound, matrix, analyte
# BY USUBJID parcat1 : PLOT AVAL * eactm

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper to find file trying Windows absolute path and fallback locations
find_file <- function(win_path) {
  candidates <- c(
    win_path,
    sub("^D:/meta_hackathon/sas2r-agent", Sys.getenv("ROOT", unset = ""), win_path),
    sub("^D:", "", win_path),
    file.path("/tmp", basename(win_path)),
    file.path(getwd(), basename(win_path))
  )
  # also try common linux mount
  candidates <- unique(c(candidates,
    file.path("/data/raw/cdisc-pilot/adam", basename(win_path)),
    file.path("data/raw/cdisc-pilot/adam", basename(win_path)),
    file.path(Sys.getenv("RAWADAM", unset = ""), basename(win_path))
  ))
  for (p in candidates) {
    if (nzchar(p) && file.exists(p)) return(p)
  }
  return(win_path)
}

# absolute paths as declared in SAS program (forward slashes)
adsl_path_win <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
adpc_path_win <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"

adsl_path <- find_file(adsl_path_win)
adpc_path <- find_file(adpc_path_win)

# also try env vars if still not found
if (!file.exists(adsl_path)) {
  alt <- Sys.getenv("RAWADAM")
  if (nzchar(alt)) {
    p2 <- file.path(alt, "adsl.xpt")
    if (file.exists(p2)) adsl_path <- p2
  }
}
if (!file.exists(adpc_path)) {
  alt <- Sys.getenv("RAWADAM")
  if (nzchar(alt)) {
    p2 <- file.path(alt, "adpc.xpt")
    if (file.exists(p2)) adpc_path <- p2
  }
}
# final fallback: search under root
if (!file.exists(adpc_path) || !file.exists(adsl_path)) {
  root <- Sys.getenv("root")
  if (!nzchar(root)) root <- Sys.getenv("ROOT")
  if (nzchar(root)) {
    p1 <- file.path(root, "data", "raw", "cdisc-pilot", "adam", "adsl.xpt")
    p2 <- file.path(root, "data", "raw", "cdisc-pilot", "adam", "adpc.xpt")
    if (file.exists(p1)) adsl_path <- p1
    if (file.exists(p2)) adpc_path <- p2
  }
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

# Read ADSL (not used in plot but required by SAS program)
adsl <- tryCatch(haven::read_xpt(adsl_path), error = function(e) {
  message("Failed to read ADSL at ", adsl_path, ": ", e$message)
  tibble::tibble()
})

# Read ADPC
adpc <- haven::read_xpt(adpc_path)

# Convert haven labelled to plain R vectors for plotting but keep names
# haven::zap_labels and zap_formats to get numeric/character
adpc <- haven::zap_formats(adpc)
adpc <- haven::zap_label(adpc)
# zap_label keeps labelled class; convert labelled to character/numeric
adpc <- dplyr::mutate(adpc, dplyr::across(where(haven::is.labelled), haven::as_factor))
# For character labelled, as_factor may be okay; but ensure numeric stays numeric
# Re-read numeric columns that were converted incorrectly: AVAL, eactm should be numeric
# If they became factor, coerce back
coerce_numeric <- function(x) {
  if (is.factor(x)) suppressWarnings(as.numeric(as.character(x))) else x
}
if ("AVAL" %in% names(adpc)) adpc$AVAL <- coerce_numeric(adpc$AVAL)
if ("eactm" %in% names(adpc)) {
  # eactm in ADPC is often character or numeric; ensure numeric
  adpc$eactm <- coerce_numeric(adpc$eactm)
  # if still character with numeric strings, parse
  if (is.character(adpc$eactm)) adpc$eactm <- suppressWarnings(as.numeric(adpc$eactm))
}
if ("EACTM" %in% names(adpc) && !"eactm" %in% names(adpc)) {
  adpc$eactm <- coerce_numeric(adpc$EACTM)
}

# Ensure required columns exist (case-insensitive check)
# SAS is case-sensitive but we handle both
nms <- names(adpc)
# standardize to original case but ensure we have USUBJID, parcat1, eactm, AVAL
# haven preserves case as in xpt (usually uppercase)
# Map to expected names
get_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}
col_usubjid <- get_col(adpc, "USUBJID")
col_parcat1 <- get_col(adpc, "PARCAT1")
col_eactm   <- get_col(adpc, "EACTM")
col_aval    <- get_col(adpc, "AVAL")

stopifnot(!is.null(col_usubjid), !is.null(col_parcat1), !is.null(col_eactm), !is.null(col_aval))

# Rename to canonical for internal use but keep original for output
adpc_work <- adpc

# Create sortable columns with canonical names
adpc_work$USUBJID_c <- adpc_work[[col_usubjid]]
adpc_work$PARCAT1_c <- adpc_work[[col_parcat1]]
adpc_work$EACTM_c   <- suppressWarnings(as.numeric(as.character(adpc_work[[col_eactm]])))
# if EACTM_c is all NA but original is numeric, use original
if (all(is.na(adpc_work$EACTM_c)) && is.numeric(adpc_work[[col_eactm]])) {
  adpc_work$EACTM_c <- adpc_work[[col_eactm]]
}
adpc_work$AVAL_c <- suppressWarnings(as.numeric(as.character(adpc_work[[col_aval]])))
if (all(is.na(adpc_work$AVAL_c)) && is.numeric(adpc_work[[col_aval]])) {
  adpc_work$AVAL_c <- adpc_work[[col_aval]]
}

# PROC SORT BY USUBJID parcat1 eactm (SAS sorts missing first, character uppercase before lowercase)
# Use base order which matches SAS for ASCII
ord <- order(adpc_work$USUBJID_c, adpc_work$PARCAT1_c, adpc_work$EACTM_c, na.last = FALSE)
SORTadpc <- adpc_work[ord, , drop = FALSE]

# For output CSV, keep original column names and order, but sorted
# Remove helper columns before writing
out_csv_df <- SORTadpc[, !(names(SORTadpc) %in% c("USUBJID_c","PARCAT1_c","EACTM_c","AVAL_c")), drop = FALSE]

# Format dates as YYYY-MM-DD and datetimes as YYYY-MM-DDTHH:MM:SS if any Date/POSIX columns exist
for (cn in names(out_csv_df)) {
  if (inherits(out_csv_df[[cn]], "Date")) {
    out_csv_df[[cn]] <- format(out_csv_df[[cn]], "%Y-%m-%d")
  } else if (inherits(out_csv_df[[cn]], "POSIXct") || inherits(out_csv_df[[cn]], "POSIXt")) {
    out_csv_df[[cn]] <- format(out_csv_df[[cn]], "%Y-%m-%dT%H:%M:%S")
  }
}

# Write dataset CSV (SAS dataset counterpart)
write.csv(out_csv_df, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# Also write a dataset named after program if needed (P060 dataset)
write.csv(out_csv_df, file.path(out, "P060_pk_subj_conc.csv"), row.names = FALSE, na = "")

# Prepare plot data (use canonical columns)
plot_df <- SORTadpc %>%
  dplyr::filter(!is.na(EACTM_c), !is.na(AVAL_c)) %>%
  dplyr::mutate(
    USUBJID = as.character(USUBJID_c),
    PARCAT1 = as.character(PARCAT1_c),
    EACTM = as.numeric(EACTM_c),
    AVAL = as.numeric(AVAL_c)
  )

# Titles and footnote verbatim from SAS
title1 <- "(page x of x)"
title2 <- "Figure 16.2.5-x.x Individual concentration-time profiles"
title3 <- "by compound, matrix, analyte and [actual/randomised][treatments/group]"
title4 <- "Analysis Set : PK analysis set"
# Footnote: Generated by the SAS System (...) on <date> at <time>
# Reproduce with current date/time in similar format
foot_date <- format(Sys.Date(), "%B %d, %Y")
foot_time <- format(Sys.time(), "%I:%M:%S %p")
footnote_text <- paste0("Generated by the SAS System on ", foot_date, " at ", foot_time)

# Determine faceting: BY USUBJID parcat1
# Create combined BY label
plot_df$BYGRP <- paste0(plot_df$USUBJID, " / ", plot_df$PARCAT1)
# Order BYGRP by USUBJID, PARCAT1
by_levels <- unique(paste0(SORTadpc$USUBJID_c, " / ", SORTadpc$PARCAT1_c))
by_levels <- by_levels[!is.na(by_levels)]
plot_df$BYGRP <- factor(plot_df$BYGRP, levels = by_levels)

n_panels <- nlevels(plot_df$BYGRP)
if (is.na(n_panels) || n_panels == 0) n_panels <- 1

# Decide layout: aim for readable panels
# Use facet_wrap with ncol based on n_panels
ncol_wrap <- if (n_panels <= 4) n_panels else if (n_panels <= 12) 4 else 6
nrow_wrap <- ceiling(n_panels / ncol_wrap)

# Dynamic figure size: each panel ~ 3 inches wide, 2.5 inches tall
panel_w <- 3
panel_h <- 2.5
fig_w <- min(20, max(8, ncol_wrap * panel_w + 2))
fig_h <- min(20, max(6, nrow_wrap * panel_h + 3))

p <- ggplot(plot_df, aes(x = EACTM, y = AVAL, group = 1)) +
  geom_line(linewidth = 0.8, color = "black") +
  facet_wrap(~ BYGRP, ncol = ncol_wrap, scales = "free_y") +
  labs(
    title = paste0(title1, "\n", title2, "\n", title3, "\n", title4),
    x = "Time (hour)",
    y = "Conc (ug/mL)",
    caption = footnote_text
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "plain", lineheight = 1.1),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 7),
    strip.background = element_rect(fill = "grey90", colour = "grey50"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 1, size = 7, face = "italic"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# Save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = fig_w, height = fig_h, units = "in", dpi = 300, bg = "white")

# Also save a plain-text rendering of titles and summary
txt_path <- file.path(out, "sasgraph.txt")
sink(txt_path)
cat(title1, "\n")
cat(title2, "\n")
cat(title3, "\n")
cat(title4, "\n")
cat("\n")
cat("Analysis Set : PK analysis set\n")
cat("Plot: AVAL * EACTM  BY USUBJID PARCAT1\n")
cat("X-axis: Time (hour)  Y-axis: Conc (ug/mL)\n")
cat("Interpolation: JOIN  Symbol: NONE  Line: 1 Width: 2\n")
cat("\n")
cat("Number of BY groups (USUBJID * PARCAT1):", n_panels, "\n")
cat("Number of observations plotted:", nrow(plot_df), "\n")
cat("\n")
cat("BY groups:\n")
print(as.data.frame(table(plot_df$BYGRP)))
cat("\n")
cat(footnote_text, "\n")
sink()

# Ensure CSV of plotted data already written; also write a clean plotted data CSV with key columns
plotted_data <- plot_df[, c("USUBJID", "PARCAT1", "EACTM", "AVAL", "BYGRP")]
# Keep original column names for traceability: also include USUBJID, PARCAT1, EACTM, AVAL
write.csv(plotted_data, file.path(out, "sasgraph_plotted_data.csv"), row.names = FALSE, na = "")

# Final check: ensure at least sasgraph.png and sasgraph.csv exist
stopifnot(file.exists(file.path(out, "sasgraph.png")))
stopifnot(file.exists(file.path(out, "sasgraph.csv")))
```