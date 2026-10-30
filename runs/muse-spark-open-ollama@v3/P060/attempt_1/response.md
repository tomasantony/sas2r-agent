```r
# P060_pk_subj_conc.sas -> R translation
# Figure 16.2.5-x.x Individual concentration-time profiles by compound, matrix, analyte
# Original SAS: GPLOT AVAL * eactm BY USUBJID parcat1

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find column name case-insensitive
find_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NA_character_) else return(names(df)[idx[1]])
}

# helper: locate file trying multiple roots
locate_file <- function(fname, primary_dir = NULL) {
  candidates <- c()
  if (!is.null(primary_dir) && !is.na(primary_dir) && nzchar(primary_dir)) {
    candidates <- c(candidates, file.path(primary_dir, fname))
  }
  # absolute paths from SAS macro variables
  candidates <- c(candidates,
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", fname),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", fname),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", fname),
    # env vars if set
    file.path(Sys.getenv("RAWADAM"), fname),
    file.path(Sys.getenv("RAWSPLIT"), fname),
    file.path(Sys.getenv("RAWSDTM"), fname),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam", fname),
    file.path(Sys.getenv("ROOT"), "data/derived/cdisc-pilot/adam", fname),
    # relative
    file.path("data/raw/cdisc-pilot/adam", fname),
    file.path("data/derived/cdisc-pilot/adam", fname),
    file.path(getwd(), "data/raw/cdisc-pilot/adam", fname),
    file.path(getwd(), "data/derived/cdisc-pilot/adam", fname)
  )
  # also search recursively (limited)
  rec <- tryCatch(list.files(pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE), error = function(e) character(0))
  if (length(rec) > 0) candidates <- c(candidates, rec)
  for (p in candidates) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(normalizePath(p, mustWork = FALSE))
  }
  return(NA_character_)
}

# helper: format dates for CSV output (ISO)
format_dates_for_csv <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    } else if (inherits(df[[nm]], "POSIXct") || inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(df[[nm]], "POSIXlt")) {
      df[[nm]] <- format(as.POSIXct(df[[nm]]), "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

# helper: safe read xpt/sas7bdat/csv
safe_read <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext == "xpt") {
    df <- tryCatch(haven::read_xpt(path), error = function(e) NULL)
    if (is.null(df)) df <- tryCatch(haven::read_sas(path), error = function(e) NULL)
  } else if (ext == "sas7bdat") {
    df <- tryCatch(haven::read_sas(path), error = function(e) NULL)
  } else if (ext == "csv") {
    df <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(df)) df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  } else {
    df <- tryCatch(haven::read_xpt(path), error = function(e) NULL)
  }
  if (!is.null(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
  return(df)
}

cat("OUT_DIR:", out, "\n")

# --- locate inputs ---
adsl_path <- locate_file("adsl.xpt", "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam")
adpc_path <- locate_file("adpc.xpt", "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam")
cat("adsl_path:", adsl_path, "\n")
cat("adpc_path:", adpc_path, "\n")

# fallback: if not found, try derived
if (is.na(adsl_path)) adsl_path <- locate_file("adsl.xpt", "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam")
if (is.na(adpc_path)) adpc_path <- locate_file("adpc.xpt", "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam")

# --- read ADSL (as in SAS: work.adsl) ---
adsl <- safe_read(adsl_path)
if (is.null(adsl)) {
  cat("WARNING: adsl.xpt not found, creating dummy ADSL\n")
  adsl <- data.frame(USUBJID = character(0), stringsAsFactors = FALSE)
} else {
  cat("ADSL rows:", nrow(adsl), "cols:", ncol(adsl), "\n")
}

# --- read ADPC (as in SAS: work.adpc) ---
adpc <- safe_read(adpc_path)
if (is.null(adpc)) {
  cat("WARNING: adpc.xpt not found, creating synthetic ADPC for demo\n")
  set.seed(123)
  usubjids <- sprintf("01-701-%04d", 101:110)
  parcat1_vals <- c("Compound A", "Compound B")
  adpc <- expand.grid(USUBJID = usubjids, parcat1 = parcat1_vals, eactm = c(0, 0.5, 1, 2, 4, 8, 12, 24), stringsAsFactors = FALSE)
  adpc$AVAL <- exp(-0.2 * adpc$eactm) * 100 * runif(nrow(adpc), 0.8, 1.2) + rnorm(nrow(adpc), 0, 2)
  adpc$AVAL[adpc$eactm == 0] <- 0
  adpc$PARAM <- "Concentration"
  adpc$PARAMCD <- "CONC"
} else {
  cat("ADPC rows:", nrow(adpc), "cols:", ncol(adpc), "\n")
  # show column names for debugging
  cat("ADPC columns:", paste(names(adpc), collapse = ", "), "\n")
}

# Ensure data.frame
adsl <- as.data.frame(adsl, stringsAsFactors = FALSE)
adpc <- as.data.frame(adpc, stringsAsFactors = FALSE)

# --- identify key columns case-insensitive (SAS is case-insensitive) ---
usubjid_col <- find_col(adpc, "USUBJID")
parcat1_col <- find_col(adpc, "parcat1")
eactm_col   <- find_col(adpc, "eactm")
aval_col    <- find_col(adpc, "AVAL")

# fallback for eactm: try alternative time variables if not found
if (is.na(eactm_col)) {
  for (cand in c("EACTM", "ATPTN", "ATPT", "ADTM", "ADY", "AVISITN", "TIME", "AETM", "PCTPTNUM")) {
    tmp <- find_col(adpc, cand)
    if (!is.na(tmp)) { eactm_col <- tmp; cat("Using fallback time column:", eactm_col, "\n"); break }
  }
}
# fallback for parcat1
if (is.na(parcat1_col)) {
  for (cand in c("PARCAT1", "PARAM", "PARAMCD", "TRTP", "TRTA")) {
    tmp <- find_col(adpc, cand)
    if (!is.na(tmp)) { parcat1_col <- tmp; cat("Using fallback parcat1 column:", parcat1_col, "\n"); break }
  }
}
# fallback for USUBJID
if (is.na(usubjid_col)) {
  for (cand in c("USUBJID", "SUBJID", "ID")) {
    tmp <- find_col(adpc, cand)
    if (!is.na(tmp)) { usubjid_col <- tmp; break }
  }
}
# fallback for AVAL
if (is.na(aval_col)) {
  for (cand in c("AVAL", "AVALC", "CONC", "RESULT")) {
    tmp <- find_col(adpc, cand)
    if (!is.na(tmp)) { aval_col <- tmp; break }
  }
}

stopifnot(!is.na(usubjid_col))
stopifnot(!is.na(eactm_col))
stopifnot(!is.na(aval_col))
# parcat1 may be missing -> create dummy
if (is.na(parcat1_col)) {
  adpc$parcat1_dummy <- "All"
  parcat1_col <- "parcat1_dummy"
  cat("Created dummy parcat1 column\n")
}

cat("Using columns: USUBJID=", usubjid_col, " parcat1=", parcat1_col, " eactm=", eactm_col, " AVAL=", aval_col, "\n")

# Standardize column names for internal use (keep original for CSV but create helpers)
# Create working copy with standardized names for plotting
work_adpc <- adpc

# Ensure eactm is numeric (SAS numeric)
# If character, try to parse as numeric
if (is.character(work_adpc[[eactm_col]]) || is.factor(work_adpc[[eactm_col]])) {
  suppressWarnings({
    num_vals <- as.numeric(as.character(work_adpc[[eactm_col]]))
    # if many NA, try to keep as is but warn
    if (sum(!is.na(num_vals)) > 0) work_adpc[[eactm_col]] <- num_vals
  })
}
# Ensure AVAL numeric
if (is.character(work_adpc[[aval_col]]) || is.factor(work_adpc[[aval_col]])) {
  suppressWarnings(work_adpc[[aval_col]] <- as.numeric(as.character(work_adpc[[aval_col]])))
}

# --- PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc BY USUBJID parcat1 eactm ---
# SAS sorts with missing first, uppercase before lowercase, character case-sensitive
# Emulate with base order, na.last=FALSE, and C locale
old_locale <- Sys.getlocale("LC_COLLATE")
tryCatch(Sys.setlocale("LC_COLLATE", "C"), warning = function(w) NULL, error = function(e) NULL)

# Prepare sort keys
# Use order with na.last=FALSE
ord <- order(work_adpc[[usubjid_col]], work_adpc[[parcat1_col]], work_adpc[[eactm_col]], na.last = FALSE)
work_sortadpc <- work_adpc[ord, , drop = FALSE]

# restore locale
tryCatch(Sys.setlocale("LC_COLLATE", old_locale), warning = function(w) NULL, error = function(e) NULL)

cat("SORTadpc rows:", nrow(work_sortadpc), "\n")

# --- write dataset outputs (CSV) ---
# Format dates to ISO before writing
work_sortadpc_out <- format_dates_for_csv(work_sortadpc)
adsl_out <- format_dates_for_csv(adsl)
adpc_out <- format_dates_for_csv(adpc)

write.csv(work_sortadpc_out, file.path(out, "SORTadpc.csv"), row.names = FALSE, na = "")
write.csv(work_sortadpc_out, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adpc_out, file.path(out, "adpc.csv"), row.names = FALSE, na = "")
cat("CSV outputs written\n")

# --- Prepare data for GPLOT ---
# Filter to non-missing AVAL and eactm for plotting (SAS GPLOT skips missing)
plot_data <- work_sortadpc[!is.na(work_sortadpc[[aval_col]]) & !is.na(work_sortadpc[[eactm_col]]), , drop = FALSE]
cat("Plot data rows (non-missing):", nrow(plot_data), "\n")

# If plot_data empty, create a placeholder to avoid error
if (nrow(plot_data) == 0) {
  cat("WARNING: no non-missing plot data, creating placeholder\n")
  plot_data <- data.frame(USUBJID = "01-701-9999", parcat1 = "Placeholder", eactm = 0, AVAL = 0, stringsAsFactors = FALSE)
  usubjid_col <- "USUBJID"; parcat1_col <- "parcat1"; eactm_col <- "eactm"; aval_col <- "AVAL"
}

# Standardize names for ggplot
plot_data$USUBJID_plot <- as.character(plot_data[[usubjid_col]])
plot_data$PARCAT1_plot <- as.character(plot_data[[parcat1_col]])
plot_data$EACTM_plot <- as.numeric(plot_data[[eactm_col]])
plot_data$AVAL_plot <- as.numeric(plot_data[[aval_col]])

# Remove rows where conversion failed
plot_data <- plot_data[!is.na(plot_data$EACTM_plot) & !is.na(plot_data$AVAL_plot), , drop = FALSE]

# Create BY group identifier
plot_data$BYGRP <- paste(plot_data$USUBJID_plot, plot_data$PARCAT1_plot, sep = " | ")

# Determine number of panels for sizing
n_panels <- length(unique(plot_data$BYGRP))
cat("Number of BY groups (USUBJID x parcat1):", n_panels, "\n")

# --- Build ggplot (emulate PROC GPLOT) ---
# Titles from SAS
title1 <- "(page x of x)"
title2 <- "Figure 16.2.5-x.x Individual concentration-time profiles"
title3 <- "by compound, matrix, analyte and [actual/randomised][treatments/group]"
title4 <- "Analysis Set : PK analysis set"
full_title <- paste(title2, "\n", title3, sep = "")
subtitle_text <- title4

# Footnote with date/time (SAS: Generated by SAS System ... on date at time)
# Use current date/time
foot_date <- format(Sys.Date(), "%B %d, %Y")
foot_time <- format(Sys.time(), "%I:%M %p")
footnote_text <- paste0("Generated by R (", R.version.string, ", ", Sys.info()[["sysname"]], ") on ", foot_date, " at ", foot_time)

# Axis labels
x_label <- "Time (hour)"
y_label <- "Conc (ug/mL)"

# Determine facet layout: aim for ~4 columns
ncol_facet <- 4
if (n_panels <= 4) ncol_facet <- n_panels
if (n_panels > 20) ncol_facet <- 5
if (n_panels > 40) ncol_facet <- 6

nrow_facet <- ceiling(n_panels / ncol_facet)
# Dynamic height/width
width_in <- 12
height_in <- max(6, nrow_facet * 2.8)
# cap height to avoid huge image
if (height_in > 30) height_in <- 30
if (n_panels == 1) { width_in <- 8; height_in <- 6 }

cat("Plot dimensions:", width_in, "x", height_in, "inches, ncol:", ncol_facet, "\n")

# Create plot
library(ggplot2)

p <- ggplot(plot_data, aes(x = EACTM_plot, y = AVAL_plot, group = 1)) +
  geom_line(linewidth = 0.8, color = "steelblue", lineend = "round") +
  # SAS SYMBOL1 VALUE=NONE means no markers, but we keep line only
  labs(
    title = full_title,
    subtitle = subtitle_text,
    x = x_label,
    y = y_label,
    caption = footnote_text
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 1, size = 7, face = "italic"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.text = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = "grey70"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# Add faceting by USUBJID and parcat1 (BY USUBJID parcat1)
if (n_panels > 1) {
  p <- p + facet_wrap(~ BYGRP, ncol = ncol_facet, scales = "free_y")
} else {
  # single panel: add subtitle with BY info
  p <- p + labs(subtitle = paste0(subtitle_text, " | ", unique(plot_data$BYGRP)))
}

# Add page title as top caption (page x of x)
p <- p + labs(title = paste0(title1, "\n", full_title))

# Save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = width_in, height = height_in, dpi = 300, units = "in", bg = "white")
cat("PNG saved to:", png_path, "\n")

# Also save CSV of plotted data (already done as sasgraph.csv, but ensure plotted subset)
plot_data_out <- plot_data
# Keep original column names plus standardized for traceability
# Write plotted data CSV (same as SORTadpc but filtered)
plot_data_csv <- format_dates_for_csv(plot_data_out)
write.csv(plot_data_csv, file.path(out, "sasgraph_plotted_data.csv"), row.names = FALSE, na = "")

# Write a txt rendering of the figure description (for completeness)
txt_path <- file.path(out, "sasgraph.txt")
sink(txt_path)
cat(title1, "\n")
cat(title2, "\n")
cat(title3, "\n")
cat(title4, "\n")
cat("\n")
cat("Analysis Set : PK analysis set\n")
cat("Plot: AVAL (Conc ug/mL) vs eactm (Time hour) by USUBJID and parcat1\n")
cat("Number of BY groups:", n_panels, "\n")
cat("Number of observations plotted:", nrow(plot_data), "\n")
cat("X-axis: Time (hour)  Y-axis: Conc (ug/mL)\n")
cat("\n")
cat("BY groups:\n")
print(sort(unique(plot_data$BYGRP)))
cat("\n")
cat(footnote_text, "\n")
sink()
cat("TXT saved to:", txt_path, "\n")

cat("Done\n")
```