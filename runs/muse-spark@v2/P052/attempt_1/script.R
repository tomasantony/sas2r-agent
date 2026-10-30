# P052_outliers_Scatter_MeanTime.sas -> R translation
# Shift (scatter) plot: Maximum Post-baseline vs Maximum Baseline DIABP
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

# SAS round half away from zero (helper, per spec)
round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

root <- "D:/meta_hackathon/sas2r-agent"
rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")

find_name <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}

# ---- Read inputs (same as SAS filename source + libname xport) ----
adsl_path <- file.path(rawadam, "adsl.xpt")
advs_path <- file.path(rawadam, "advsmax.xpt")
cat("Reading ADVSMAX...\n")
advsmax_raw <- haven::read_xpt(advs_path)
# strip haven labels/formats but keep values
advsmax_raw <- haven::zap_formats(haven::zap_labels(advsmax_raw))
advsmax_raw <- as.data.frame(advsmax_raw, stringsAsFactors = FALSE)
# adsl read for fidelity (not used downstream, as in SAS)
if (file.exists(adsl_path)) {
  adsl_raw <- haven::read_xpt(adsl_path)
  adsl_raw <- as.data.frame(haven::zap_formats(haven::zap_labels(adsl_raw)), stringsAsFactors = FALSE)
}

nm_STUDYID <- find_name(advsmax_raw, "STUDYID")
nm_PARAMCD <- find_name(advsmax_raw, "PARAMCD")
nm_ATPTN   <- find_name(advsmax_raw, "ATPTN")
nm_ANL01FL <- find_name(advsmax_raw, "ANL01FL")
nm_ADY     <- find_name(advsmax_raw, "ADY")
nm_SAFFL   <- find_name(advsmax_raw, "SAFFL")
nm_BASE    <- find_name(advsmax_raw, "BASE")
nm_AVAL    <- find_name(advsmax_raw, "AVAL")
nm_ANRLO   <- find_name(advsmax_raw, "ANRLO")
nm_ANRHI   <- find_name(advsmax_raw, "ANRHI")
nm_TRTA    <- find_name(advsmax_raw, "TRTA")
stopifnot(!is.na(nm_STUDYID), !is.na(nm_PARAMCD), !is.na(nm_ATPTN),
          !is.na(nm_ANL01FL), !is.na(nm_ADY), !is.na(nm_SAFFL),
          !is.na(nm_BASE), !is.na(nm_AVAL), !is.na(nm_TRTA))

# ---- PROC SORT data=advsmax; by studyid; ----
# SAS character ordering: uppercase before lowercase; STUDYID values are uniform case
advsmax_raw <- advsmax_raw[order(advsmax_raw[[nm_STUDYID]], na.last = FALSE), , drop = FALSE]

# ---- DATA advsmax_final; WHERE ...; if first.studyid then LLN/ULN labels ----
atptn_num <- suppressWarnings(as.numeric(as.character(advsmax_raw[[nm_ATPTN]])))
ady_num   <- suppressWarnings(as.numeric(as.character(advsmax_raw[[nm_ADY]])))
paramcd_v <- as.character(advsmax_raw[[nm_PARAMCD]])
anl01fl_v <- as.character(advsmax_raw[[nm_ANL01FL]])
saffl_v   <- as.character(advsmax_raw[[nm_SAFFL]])

keep <- !is.na(paramcd_v) & paramcd_v == "DIABP" &
        !is.na(atptn_num) & atptn_num == 815 &
        !is.na(anl01fl_v) & anl01fl_v == "Y" &
        !is.na(ady_num) & ady_num > 1 &
        !is.na(saffl_v) & saffl_v == "Y"
advsmax_final <- advsmax_raw[keep, , drop = FALSE]
# re-sort by studyid (SAS BY processing requires sorted)
advsmax_final <- advsmax_final[order(advsmax_final[[nm_STUDYID]], na.last = FALSE), , drop = FALSE]
stopifnot(nm_STUDYID %in% names(advsmax_final))
cat(sprintf("advsmax_final rows: %d\n", nrow(advsmax_final)))

# FIRST.studyid labels
advsmax_final$LLN_label <- NA_character_
advsmax_final$ULN_label <- NA_character_
if (nrow(advsmax_final) > 0) {
  sid <- as.character(advsmax_final[[nm_STUDYID]])
  first_idx <- c(TRUE, sid[-1] != sid[-length(sid)])
  first_idx[is.na(first_idx)] <- TRUE
  advsmax_final$LLN_label[first_idx] <- "LLN"
  advsmax_final$ULN_label[first_idx] <- "ULN"
}

# ---- PROC SQL range: min(base), max(base), min(aval), max(aval) ----
base_v <- suppressWarnings(as.numeric(as.character(advsmax_final[[nm_BASE]])))
aval_v <- suppressWarnings(as.numeric(as.character(advsmax_final[[nm_AVAL]])))
min_base <- if (all(is.na(base_v))) NA_real_ else min(base_v, na.rm = TRUE)
max_base <- if (all(is.na(base_v))) NA_real_ else max(base_v, na.rm = TRUE)
min_aval <- if (all(is.na(aval_v))) NA_real_ else min(aval_v, na.rm = TRUE)
max_aval <- if (all(is.na(aval_v))) NA_real_ else max(aval_v, na.rm = TRUE)

range_df <- data.frame(min_base = min_base, max_base = max_base,
                       min_aval = min_aval, max_aval = max_aval)

# data range2; min=min(...); max=max(...);
axis_min <- min(c(min_base, min_aval), na.rm = TRUE)
axis_max <- max(c(max_base, max_aval), na.rm = TRUE)
cat(sprintf("range: min_base=%s max_base=%s min_aval=%s max_aval=%s axis_min=%s axis_max=%s\n",
            min_base, max_base, min_aval, max_aval, axis_min, axis_max))

# ---- count distinct studyid; column/row numbers ----
count_study <- dplyr::n_distinct(as.character(advsmax_final[[nm_STUDYID]]))
if (count_study <= 2) {
  column_numbers <- 2; row_numbers <- 1
} else {
  column_numbers <- 2; row_numbers <- 2
}
advsmax_final2 <- advsmax_final
advsmax_final2$count_study <- count_study
advsmax_final3 <- advsmax_final2
advsmax_final3$column_numbers <- column_numbers
advsmax_final3$row_numbers <- row_numbers

# ---- Write table outputs (P052.lst counterpart: range with 4 numbers) ----
write.csv(range_df, file.path(out, "P052.csv"), row.names = FALSE, na = "")
# text rendering containing same numbers
txt_path <- file.path(out, "P052.txt")
sink(txt_path)
cat("Range for Shift Plot - Maximum Baseline and Maximum Post-baseline Measurement\n")
cat("Filter: PARAMCD=DIABP and ATPTN=815 and ANL01FL=Y and ADY>1 and SAFFL=Y\n")
cat("\n")
cat(sprintf("%-12s %-12s %-12s %-12s\n", "min_base", "max_base", "min_aval", "max_aval"))
cat(sprintf("%-12s %-12s %-12s %-12s\n", min_base, max_base, min_aval, max_aval))
cat("\n")
cat(sprintf("Axis viewmin (min of min_base, min_aval) = %s\n", axis_min))
cat(sprintf("Axis viewmax (max of max_base, max_aval) = %s\n", axis_max))
cat(sprintf("Number of distinct STUDYID = %s\n", count_study))
cat(sprintf("Panel rows = %s  columns = %s\n", row_numbers, column_numbers))
sink()
# also provide .lst alias for harness expecting P052.lst
try(writeLines(readLines(txt_path), file.path(out, "P052.lst")), silent = TRUE)

# ---- Write plotted data CSV (SGRender data) ----
# keep SAS variable names; write ISO dates if any date cols present (none expected here)
plot_data <- advsmax_final3
# ensure dates written ISO if present (generic handling)
for (cn in names(plot_data)) {
  if (inherits(plot_data[[cn]], "Date")) plot_data[[cn]] <- format(plot_data[[cn]], "%Y-%m-%d")
  if (inherits(plot_data[[cn]], "POSIXt")) plot_data[[cn]] <- format(plot_data[[cn]], "%Y-%m-%dT%H:%M:%S")
}
write.csv(plot_data, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")

# ---- Figure: GTL shiftplot -> ggplot2 ----
# datapanel classvars=(studyid) rows/cols, equated axes viewmin/viewmax, integer ticks
# scatterplot x=base y=aval / group=trta; lineparm slope 1; ref lines ANRLO/ANRHI
dfp <- advsmax_final3
dfp[[".X"]] <- suppressWarnings(as.numeric(as.character(dfp[[nm_BASE]])))
dfp[[".Y"]] <- suppressWarnings(as.numeric(as.character(dfp[[nm_AVAL]])))
dfp[[".STUDYID"]] <- as.character(dfp[[nm_STUDYID]])
dfp[[".TRTA"]] <- as.character(dfp[[nm_TRTA]])
# reference values per study (distinct non-missing)
ref_df <- unique(dfp[, c(".STUDYID", nm_ANRLO, nm_ANRHI), drop = FALSE])
names(ref_df) <- c(".STUDYID", ".ANRLO", ".ANRHI")
ref_df$.ANRLO <- suppressWarnings(as.numeric(as.character(ref_df$.ANRLO)))
ref_df$.ANRHI <- suppressWarnings(as.numeric(as.character(ref_df$.ANRHI)))
ref_v <- ref_df[!is.na(ref_df$.STUDYID), , drop = FALSE]

# integer breaks within [axis_min, axis_max]
mk_breaks <- function(lo, hi) {
  if (!is.finite(lo) || !is.finite(hi)) return(ggplot2::waiver())
  loI <- ceiling(lo); hiI <- floor(hi)
  n <- hiI - loI + 1
  if (n <= 0) return(c(lo, hi))
  if (n <= 25) return(seq(loI, hiI, by = 1))
  # wider range: step to keep ~10 breaks on integers
  step <- ceiling(n / 10)
  seq(loI, hiI, by = step)
}
brks <- mk_breaks(axis_min, axis_max)

p <- ggplot2::ggplot(dfp, ggplot2::aes(x = .X, y = .Y, colour = .TRTA)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey40", linewidth = 0.6) +
  ggplot2::geom_vline(data = ref_v, ggplot2::aes(xintercept = .ANRLO),
                      colour = "red", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  ggplot2::geom_vline(data = ref_v, ggplot2::aes(xintercept = .ANRHI),
                      colour = "red", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  ggplot2::geom_hline(data = ref_v, ggplot2::aes(yintercept = .ANRLO),
                      colour = "red", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  ggplot2::geom_hline(data = ref_v, ggplot2::aes(yintercept = .ANRHI),
                      colour = "red", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  ggplot2::geom_point(size = 1.6, alpha = 0.8) +
  ggplot2::scale_x_continuous(name = "Maximum Baseline Measurment",
                              limits = c(axis_min, axis_max), breaks = brks) +
  ggplot2::scale_y_continuous(name = "Maximum Post-baseline Measurment",
                              limits = c(axis_min, axis_max), breaks = brks) +
  ggplot2::facet_wrap(~ .STUDYID, nrow = row_numbers, ncol = column_numbers, dir = "h") +
  ggplot2::labs(colour = "Treatment") +
  ggplot2::coord_equal() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom",
                 panel.grid.minor = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "grey90"))

# LLN/ULN curve labels at min edge (curvelabelposition=min)
if (nrow(ref_v) > 0) {
  lab_vmin <- data.frame()
  for (i in seq_len(nrow(ref_v))) {
    s <- ref_v$.STUDYID[i]; lo <- ref_v$.ANRLO[i]; hi <- ref_v$.ANRHI[i]
    if (!is.na(lo)) lab_vmin <- rbind(lab_vmin, data.frame(.STUDYID = s, x = lo, y = axis_min, lab = "LLN"))
    if (!is.na(hi)) lab_vmin <- rbind(lab_vmin, data.frame(.STUDYID = s, x = hi, y = axis_min, lab = "ULN"))
  }
  lab_hmin <- data.frame()
  for (i in seq_len(nrow(ref_v))) {
    s <- ref_v$.STUDYID[i]; lo <- ref_v$.ANRLO[i]; hi <- ref_v$.ANRHI[i]
    if (!is.na(lo)) lab_hmin <- rbind(lab_hmin, data.frame(.STUDYID = s, x = axis_min, y = lo, lab = "LLN"))
    if (!is.na(hi)) lab_hmin <- rbind(lab_hmin, data.frame(.STUDYID = s, x = axis_min, y = hi, lab = "ULN"))
  }
  if (nrow(lab_vmin) > 0) {
    p <- p + ggplot2::geom_text(data = lab_vmin,
             ggplot2::aes(x = x, y = y, label = lab),
             inherit.aes = FALSE, size = 3, vjust = -0.5, hjust = 0.5, colour = "black")
  }
  if (nrow(lab_hmin) > 0) {
    p <- p + ggplot2::geom_text(data = lab_hmin,
             ggplot2::aes(x = x, y = y, label = lab),
             inherit.aes = FALSE, size = 3, vjust = 0.5, hjust = -0.2, colour = "black")
  }
}

ggplot2::ggsave(file.path(out, "sgrender.png"), plot = p, width = 10, height = 8, dpi = 150)
# alias with original case
try(file.copy(file.path(out, "sgrender.png"), file.path(out, "SGRender.png"), overwrite = TRUE), silent = TRUE)
cat("Done.\n")
