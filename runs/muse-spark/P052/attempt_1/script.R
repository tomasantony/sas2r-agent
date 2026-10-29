# P052_outliers_Scatter_MeanTime.sas -> R translation
# Shift (scatter) plot: Maximum Baseline vs Maximum Post-baseline DIABP, faceted by STUDYID

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  s <- sign(x)
  # SAS rounds half away from zero
  s * floor(abs(x) * mult + 0.5 + 1e-12) / mult
}

library(haven)
library(dplyr)
library(ggplot2)

cat("Reading ADVSMAX XPT...\n")
advsmax_path <- file.path(rawadam, "advsmax.xpt")
advsmax <- haven::read_xpt(advsmax_path)

# Zap haven labels/formats to plain vectors for SAS-like comparisons
advsmax <- haven::zap_labels(advsmax)
advsmax <- haven::zap_formats(advsmax)
# Normalise names to uppercase (SAS case-insensitive)
names(advsmax) <- toupper(names(advsmax))

# Also read ADSL as in SAS (not used downstream, for fidelity)
adsl_path <- file.path(rawadam, "adsl.xpt")
if (file.exists(adsl_path)) {
  adsl <- haven::read_xpt(adsl_path)
  adsl <- haven::zap_labels(adsl)
  adsl <- haven::zap_formats(adsl)
  names(adsl) <- toupper(names(adsl))
}

# ---- proc sort data=advsmax; by studyid; ----
# SAS sorts missing first, uppercase before lowercase (byte order).
# STUDYID is character; use order with na.last=FALSE
advsmax <- advsmax[order(advsmax$STUDYID, na.last = FALSE), , drop = FALSE]

# ---- data advsmax_final; WHERE ...; if first.studyid ... ----
# WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y" and ADY>1 and SAFFL="Y"
# Handle ATPTN possibly character/numeric; ADY numeric
atptn_num <- suppressWarnings(as.numeric(advsmax$ATPTN))

keep <- rep(TRUE, nrow(advsmax))
keep <- keep & !is.na(advsmax$PARAMCD) & advsmax$PARAMCD == "DIABP"
keep <- keep & !is.na(atptn_num) & atptn_num == 815
keep <- keep & !is.na(advsmax$ANL01FL) & advsmax$ANL01FL == "Y"
keep <- keep & !is.na(advsmax$ADY) & advsmax$ADY > 1
keep <- keep & !is.na(advsmax$SAFFL) & advsmax$SAFFL == "Y"
keep[is.na(keep)] <- FALSE

advsmax_final <- advsmax[keep, , drop = FALSE]
# re-sort by STUDYID to ensure BY processing order
advsmax_final <- advsmax_final[order(advsmax_final$STUDYID, na.last = FALSE), , drop = FALSE]

# if first.studyid then LLN_label="LLN"; ULN_label="ULN";
n <- nrow(advsmax_final)
advsmax_final$LLN_LABEL <- NA_character_
advsmax_final$ULN_LABEL <- NA_character_
if (n > 0) {
  # FIRST.STUDYID: first obs overall or STUDYID differs from previous (missing sorts first)
  is_first <- c(TRUE, sapply(2:n, function(i) {
    cur <- advsmax_final$STUDYID[i]
    prv <- advsmax_final$STUDYID[i-1]
    if (is.na(cur) && is.na(prv)) FALSE
    else if (is.na(cur) || is.na(prv)) TRUE
    else cur != prv
  }))
  if (n == 1) is_first <- TRUE
  advsmax_final$LLN_LABEL[is_first] <- "LLN"
  advsmax_final$ULN_LABEL[is_first] <- "ULN"
}

# ---- proc sql; create table range as select min(base), max(base), min(aval), max(aval) ----
min_base <- if (n > 0) suppressWarnings(min(advsmax_final$BASE, na.rm = TRUE)) else NA_real_
max_base <- if (n > 0) suppressWarnings(max(advsmax_final$BASE, na.rm = TRUE)) else NA_real_
min_aval <- if (n > 0) suppressWarnings(min(advsmax_final$AVAL, na.rm = TRUE)) else NA_real_
max_aval <- if (n > 0) suppressWarnings(max(advsmax_final$AVAL, na.rm = TRUE)) else NA_real_
# haven min/max with all-NA gives Inf with warning; coerce to NA like SAS (missing)
if (!is.finite(min_base)) min_base <- NA_real_
if (!is.finite(max_base)) max_base <- NA_real_
if (!is.finite(min_aval)) min_aval <- NA_real_
if (!is.finite(max_aval)) max_aval <- NA_real_

range_df <- data.frame(MIN_BASE = min_base, MAX_BASE = max_base,
                       MIN_AVAL = min_aval, MAX_AVAL = max_aval)

# ---- data range2; min=min(...); max=max(...); ----
omin <- suppressWarnings(min(c(min_base, min_aval), na.rm = TRUE))
omax <- suppressWarnings(max(c(max_base, max_aval), na.rm = TRUE))
if (!is.finite(omin)) omin <- NA_real_
if (!is.finite(omax)) omax <- NA_real_
range2 <- data.frame(MIN_BASE = min_base, MAX_BASE = max_base,
                     MIN_AVAL = min_aval, MAX_AVAL = max_aval,
                     MIN = omin, MAX = omax)

minv <- omin
maxv <- omax
cat(sprintf("Range: min=%s max=%s\n", minv, maxv))

# ---- Finding out amount of studyids ----
count_study <- if (n > 0) dplyr::n_distinct(advsmax_final$STUDYID) else 0L

advsmax_final2 <- advsmax_final
advsmax_final2$COUNT_STUDY <- count_study

# ---- data advsmax_final3; column/row numbers ----
advsmax_final3 <- advsmax_final2
if (count_study <= 2) {
  advsmax_final3$COLUMN_NUMBERS <- 2
  advsmax_final3$ROW_NUMBERS <- 1
} else {
  advsmax_final3$COLUMN_NUMBERS <- 2
  advsmax_final3$ROW_NUMBERS <- 2
}
column_numbers <- 2
row_numbers <- if (count_study <= 2) 1 else 2
if (n == 0) { column_numbers <- 2; row_numbers <- 1 }

# ---- Write P052 table (range: 4 numbers) as CSV + TXT ----
# Expected P052.lst (4 numbers) corresponds to range table
write.csv(range_df, file.path(out, "P052.csv"), row.names = FALSE, na = "")
write.csv(range_df, file.path(out, "P052.lst.csv"), row.names = FALSE, na = "")

txt_lines <- c(
  "P052: Range for Shift Plot Axes (DIABP, ATPTN=815, ANL01FL=Y, ADY>1, SAFFL=Y)",
  "--------------------------------------------------------------------------------",
  sprintf("MIN_BASE = %s   MAX_BASE = %s", format(min_base, scientific = FALSE), format(max_base, scientific = FALSE)),
  sprintf("MIN_AVAL = %s   MAX_AVAL = %s", format(min_aval, scientific = FALSE), format(max_aval, scientific = FALSE)),
  sprintf("Overall axis MIN = %s   MAX = %s", format(minv, scientific = FALSE), format(maxv, scientific = FALSE)),
  sprintf("N obs (advsmax_final) = %d   N STUDYID (COUNT_STUDY) = %d", n, count_study),
  sprintf("ROWS = %d   COLUMNS = %d", row_numbers, column_numbers),
  "",
  "Table body (range):",
  paste0(paste(names(range_df), collapse = " | ")),
  paste0(paste(as.character(unlist(range_df[1, ])), collapse = " | "))
)
writeLines(txt_lines, file.path(out, "P052.txt"))
writeLines(txt_lines, file.path(out, "P052.lst.txt"))

# ---- Build SGRender plot: datapanel by STUDYID ----
# Template: scatterplot x=base y=aval / group=trta; lineparm slope 1; ref lines ANRLO/ANRHI
plot_data <- advsmax_final3

# CSV of plotted data
# Keep SAS-like variable names; write full advsmax_final3
write.csv(plot_data, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "SGRender.csv"), row.names = FALSE, na = "")

# Determine reference values (ANRLO/ANRHI) - may be constant per param
anrlo_vals <- sort(unique(stats::na.omit(plot_data$ANRLO)))
anrhi_vals <- sort(unique(stats::na.omit(plot_data$ANRHI)))

# Axis limits: viewmin=min viewmax=max, integer=true
xlims <- c(minv, maxv)
ylims <- c(minv, maxv)
# Guard against missing/degenerate limits
if (any(is.na(xlims)) || any(!is.finite(xlims)) || minv == maxv) {
  xlims <- NULL
  ylims <- NULL
}

# Integer breaks helper
int_breaks <- function(lims) {
  if (is.null(lims)) return(ggplot2::waiver())
  lo <- ceiling(lims[1]); hi <- floor(lims[2])
  if (hi < lo) return(ggplot2::waiver())
  # cap number of breaks to avoid overcrowding
  br <- seq(lo, hi, by = 1)
  if (length(br) > 20) br <- seq(lo, hi, length.out = 20)
  br <- round(br)
  unique(br)
}

p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = BASE, y = AVAL, colour = TRTA, group = TRTA)) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black", linewidth = 0.6, na.rm = TRUE)

if (length(anrlo_vals) > 0 && all(is.finite(anrlo_vals))) {
  for (v in anrlo_vals) {
    p <- p + ggplot2::geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6, na.rm = TRUE)
    p <- p + ggplot2::geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6, na.rm = TRUE)
  }
}
if (length(anrhi_vals) > 0 && all(is.finite(anrhi_vals))) {
  for (v in anrhi_vals) {
    p <- p + ggplot2::geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6, na.rm = TRUE)
    p <- p + ggplot2::geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6, na.rm = TRUE)
  }
}

# Annotate LLN/ULN labels near min position (curvelabelposition=min) if available
# Place text at axis min edge
if (!is.null(xlims) && length(anrlo_vals) > 0) {
  p <- p + ggplot2::annotate("text", x = anrlo_vals[1], y = xlims[1], label = "LLN", hjust = 0, vjust = -0.5, size = 3.5, colour = "grey30")
}
if (!is.null(xlims) && length(anrhi_vals) > 0) {
  p <- p + ggplot2::annotate("text", x = anrhi_vals[1], y = xlims[1], label = "ULN", hjust = 0, vjust = -0.5, size = 3.5, colour = "grey30")
}

p <- p +
  ggplot2::labs(x = "Maximum Baseline Measurment", y = "Maximum Post-baseline Measurment", colour = "Treatment") +
  ggplot2::coord_fixed(ratio = 1, xlim = xlims, ylim = ylims) +
  ggplot2::scale_x_continuous(breaks = if (is.null(xlims)) ggplot2::waiver() else int_breaks(xlims), limits = xlims) +
  ggplot2::scale_y_continuous(breaks = if (is.null(ylims)) ggplot2::waiver() else int_breaks(ylims), limits = ylims) +
  ggplot2::facet_wrap(~ STUDYID, nrow = row_numbers, ncol = column_numbers, dir = "h") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom",
                panel.grid.minor = ggplot2::element_blank())

# ggsave PNG (never open graphics window)
png1 <- file.path(out, "sgrender.png")
png2 <- file.path(out, "SGRender.png")
ggplot2::ggsave(png1, plot = p, width = 10, height = 8, dpi = 300, units = "in")
# duplicate for case-sensitive checkers
try(file.copy(png1, png2, overwrite = TRUE), silent = TRUE)

cat("Done. Outputs written to OUT_DIR.\n")
