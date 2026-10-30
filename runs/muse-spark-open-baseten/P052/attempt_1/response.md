```r
# P052_outliers_Scatter_MeanTime.sas -> R translation
# Outputs: P052.lst (range table) + SGRender figure

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawadam2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(abs+0.5+1e-12) to avoid floating error
  rounded <- floor(abs_x + 0.5 + 1e-9) / mult
  sign_x * rounded
}

# helper to convert haven labelled vectors to R native
zap_haven <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      # if labelled character or numeric with labels, zap labels
      # keep underlying values
      if (is.character(col) || is.factor(col)) {
        df[[nm]] <- as.character(haven::zap_labels(col))
      } else {
        # numeric labelled
        # if all values are integer-like and labels exist, keep numeric
        df[[nm]] <- as.numeric(haven::zap_labels(col))
      }
    }
    # also handle haven_labelled character stored as numeric? already done
    # convert Date/POSIX if needed - keep as is
  }
  df
}

# helper to read xpt with fallback
read_xpt_fallback <- function(fname) {
  p1 <- file.path(rawadam, fname)
  p2 <- file.path(rawadam2, fname)
  p3 <- file.path(inlib_dir, fname)
  p <- NULL
  for (cand in c(p1, p2, p3)) {
    if (file.exists(cand)) { p <- cand; break }
  }
  if (is.null(p)) {
    # try case-insensitive search in rawadam
    if (dir.exists(rawadam)) {
      files <- list.files(rawadam, pattern = "\\.xpt$", ignore.case = TRUE, full.names = TRUE)
      idx <- grep(gsub("\\.xpt$", "", fname, ignore.case = TRUE), files, ignore.case = TRUE)
      if (length(idx) > 0) p <- files[idx[1]]
    }
  }
  if (is.null(p) || !file.exists(p)) stop(paste("File not found:", fname, "searched", rawadam))
  haven::read_xpt(p)
}

# Read ADSL (not directly used but read for completeness as in SAS)
adsl_raw <- tryCatch(read_xpt_fallback("adsl.xpt"), error = function(e) NULL)
if (!is.null(adsl_raw)) {
  adsl <- zap_haven(adsl_raw)
} else {
  adsl <- data.frame()
}

# Read ADVSMAX
advsmax_raw <- read_xpt_fallback("advsmax.xpt")
advsmax <- zap_haven(advsmax_raw)

# Ensure character columns are character (haven may produce factors)
# Convert all character-like to character
advsmax <- advsmax %>%
  dplyr::mutate(across(where(is.factor), as.character))

# proc sort data=advsmax; by studyid;
advsmax <- advsmax[order(advsmax$STUDYID, na.last = FALSE), , drop = FALSE]
# normalize column names to lower for easier handling but keep original case for output
# SAS is case-insensitive; we will work with uppercase names as in XPT
# Ensure expected columns exist (uppercase)
# haven preserves original case (often uppercase)
# Map to uppercase for logic
names(advsmax) <- toupper(names(advsmax))

# Selecting appropriate data
# WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL ="Y" and ADY > 1 and SAFFL="Y";
# ATPTN may be numeric or character; handle both
# Ensure types
if ("ATPTN" %in% names(advsmax)) {
  # try numeric
  advsmax$ATPTN_num <- suppressWarnings(as.numeric(advsmax$ATPTN))
} else {
  advsmax$ATPTN_num <- NA_real_
}
if ("ADY" %in% names(advsmax)) advsmax$ADY_num <- suppressWarnings(as.numeric(advsmax$ADY))
if ("BASE" %in% names(advsmax)) advsmax$BASE_num <- suppressWarnings(as.numeric(advsmax$BASE))
if ("AVAL" %in% names(advsmax)) advsmax$AVAL_num <- suppressWarnings(as.numeric(advsmax$AVAL))

# Filter
advsmax_final <- advsmax %>%
  dplyr::filter(PARAMCD == "DIABP",
                ATPTN_num == 815,
                ANL01FL == "Y",
                !is.na(ADY_num) & ADY_num > 1,
                SAFFL == "Y")

# Creating labels for reference lines: if first.studyid then LLN_label="LLN", ULN_label="ULN"
# Need to sort by STUDYID (already) and then mark first per STUDYID
advsmax_final <- advsmax_final %>%
  dplyr::arrange(STUDYID) %>%
  dplyr::group_by(STUDYID) %>%
  dplyr::mutate(
    LLN_label = ifelse(dplyr::row_number() == 1, "LLN", NA_character_),
    ULN_label = ifelse(dplyr::row_number() == 1, "ULN", NA_character_)
  ) %>%
  dplyr::ungroup()

# Creating ranges so that axis can be equated
# proc sql; select min(base) as min_base, max(base) as max_base, min(aval) as min_aval, max(aval) as max_aval
min_base <- suppressWarnings(min(advsmax_final$BASE, na.rm = TRUE))
max_base <- suppressWarnings(max(advsmax_final$BASE, na.rm = TRUE))
min_aval <- suppressWarnings(min(advsmax_final$AVAL, na.rm = TRUE))
max_aval <- suppressWarnings(max(advsmax_final$AVAL, na.rm = TRUE))

# Handle case where all NA -> Inf/-Inf
if (is.infinite(min_base)) min_base <- NA_real_
if (is.infinite(max_base)) max_base <- NA_real_
if (is.infinite(min_aval)) min_aval <- NA_real_
if (is.infinite(max_aval)) max_aval <- NA_real_

range_df <- data.frame(min_base = min_base, max_base = max_base,
                       min_aval = min_aval, max_aval = max_aval)

range2 <- range_df %>%
  dplyr::mutate(
    min = pmin(min_base, min_aval, na.rm = TRUE),
    max = pmax(max_base, max_aval, na.rm = TRUE)
  )

min_val <- range2$min[1]
max_val <- range2$max[1]

# Finding out the amount of studyids
count_study <- dplyr::n_distinct(advsmax_final$STUDYID)

advsmax_final2 <- advsmax_final %>%
  dplyr::mutate(count_study = count_study)

advsmax_final3 <- advsmax_final2 %>%
  dplyr::mutate(
    column_numbers = 2,
    row_numbers = ifelse(count_study <= 2, 1, 2)
  )

column_numbers <- 2
row_numbers <- ifelse(count_study <= 2, 1, 2)

# Prepare dataset for output (keep original SAS variable names, uppercase)
# Write advsmax_final3 as dataset CSV
# Convert dates to YYYY-MM-DD if any Date columns exist
format_dates_for_csv <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "Date")) {
      df[[nm]] <- format(col, "%Y-%m-%d")
    } else if (inherits(col, "POSIXt")) {
      df[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
    }
  }
  df
}

advsmax_final3_out <- format_dates_for_csv(advsmax_final3)
# Remove helper numeric columns if they were added
advsmax_final3_out <- advsmax_final3_out[, !names(advsmax_final3_out) %in% c("ATPTN_num","ADY_num","BASE_num","AVAL_num"), drop = FALSE]

write.csv(advsmax_final3_out, file.path(out, "advsmax_final3.csv"), row.names = FALSE, na = "")
write.csv(advsmax_final3_out, file.path(out, "P052_dataset.csv"), row.names = FALSE, na = "")

# Also write range dataset
range_out <- format_dates_for_csv(range2)
write.csv(range_out, file.path(out, "range.csv"), row.names = FALSE, na = "")
write.csv(range_out, file.path(out, "range2.csv"), row.names = FALSE, na = "")

# Create P052.lst text rendering (4 numbers)
# SAS P052.lst likely shows min_base, max_base, min_aval, max_aval
# Produce a plain text table with those 4 numbers
lst_lines <- c(
  "P052 - Range for Shift Plot (DIABP, ATPTN=815, ANL01FL=Y, ADY>1, SAFFL=Y)",
  "==========================================================================",
  "",
  sprintf("Number of distinct STUDYID: %d", count_study),
  sprintf("Axis limits: MIN=%s  MAX=%s  (equated)", format(min_val, trim=TRUE), format(max_val, trim=TRUE)),
  "",
  sprintf("%-12s %-12s %-12s %-12s", "MIN_BASE", "MAX_BASE", "MIN_AVAL", "MAX_AVAL"),
  sprintf("%-12s %-12s %-12s %-12s", "--------", "--------", "--------", "--------"),
  sprintf("%-12s %-12s %-12s %-12s",
          ifelse(is.na(min_base), "", format(min_base, trim=TRUE, scientific=FALSE)),
          ifelse(is.na(max_base), "", format(max_base, trim=TRUE, scientific=FALSE)),
          ifelse(is.na(min_aval), "", format(min_aval, trim=TRUE, scientific=FALSE)),
          ifelse(is.na(max_aval), "", format(max_aval, trim=TRUE, scientific=FALSE))),
  "",
  sprintf("Derived equated range: MIN=%s  MAX=%s", format(min_val, trim=TRUE), format(max_val, trim=TRUE)),
  sprintf("Panel layout: ROWS=%d  COLUMNS=%d", row_numbers, column_numbers),
  ""
)
# Write .txt versions
writeLines(lst_lines, file.path(out, "P052.txt"))
writeLines(lst_lines, file.path(out, "P052.lst.txt"))
# Also write CSV of table body (the 4 numbers)
p052_table <- data.frame(
  MIN_BASE = min_base,
  MAX_BASE = max_base,
  MIN_AVAL = min_aval,
  MAX_AVAL = max_aval,
  MIN = min_val,
  MAX = max_val,
  COUNT_STUDY = count_study,
  ROW_NUMBERS = row_numbers,
  COLUMN_NUMBERS = column_numbers
)
write.csv(p052_table, file.path(out, "P052.csv"), row.names = FALSE, na = "")
write.csv(p052_table, file.path(out, "P052.lst.csv"), row.names = FALSE, na = "")

# Creating template / figure
# Extract reference lines values (ANRLO, ANRHI) - take distinct non-missing
anrlo_vals <- unique(stats::na.omit(advsmax_final3$ANRLO))
anrhi_vals <- unique(stats::na.omit(advsmax_final3$ANRHI))
# If ANRLO/ANRHI are character, coerce to numeric
anrlo_vals <- suppressWarnings(as.numeric(anrlo_vals))
anrhi_vals <- suppressWarnings(as.numeric(anrhi_vals))
anrlo_vals <- anrlo_vals[!is.na(anrlo_vals)]
anrhi_vals <- anrhi_vals[!is.na(anrhi_vals)]

# Prepare plot data: ensure BASE, AVAL numeric, TRTA character
plot_data <- advsmax_final3 %>%
  dplyr::mutate(
    BASE = suppressWarnings(as.numeric(BASE)),
    AVAL = suppressWarnings(as.numeric(AVAL)),
    TRTA = as.character(TRTA),
    STUDYID = as.character(STUDYID)
  )

# If no data, create empty plot placeholder
if (nrow(plot_data) == 0) {
  p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data for DIABP shift plot")
} else {
  # Determine axis limits with a little padding if min==max
  if (!is.na(min_val) && !is.na(max_val) && min_val == max_val) {
    min_val <- min_val - 1
    max_val <- max_val + 1
  }
  # Fallback if NA
  if (is.na(min_val) || is.na(max_val) || is.infinite(min_val) || is.infinite(max_val)) {
    min_val <- min(c(plot_data$BASE, plot_data$AVAL), na.rm = TRUE)
    max_val <- max(c(plot_data$BASE, plot_data$AVAL), na.rm = TRUE)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = BASE, y = AVAL, colour = TRTA)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black", linewidth = 0.6) +
    ggplot2::facet_wrap(~ STUDYID, nrow = row_numbers, ncol = column_numbers) +
    ggplot2::labs(
      x = "Maximum Baseline Measurment",
      y = "Maximum Post-baseline Measurment",
      colour = "Treatment"
    ) +
    ggplot2::coord_equal(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90")
    )

  # Add reference lines if available
  if (length(anrlo_vals) > 0) {
    for (v in anrlo_vals) {
      p <- p + ggplot2::geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5)
    }
    # Add LLN labels at min position (approximate SAS curvelabelposition=min)
    # Use annotate for each facet - simple annotate at x=min, y=min
    # We add text near the reference lines
    # To avoid overplotting, add once per plot
    # Use Inf positioning not ideal; use min_val
    # Add text grobs
    # Only add if min_val is finite
    if (is.finite(min_val)) {
      for (v in anrlo_vals) {
        p <- p + ggplot2::annotate("text", x = v, y = min_val, label = "LLN", hjust = 0.5, vjust = -0.5, size = 3, colour = "grey30")
        p <- p + ggplot2::annotate("text", x = min_val, y = v, label = "LLN", hjust = -0.2, vjust = 0.5, size = 3, colour = "grey30", angle = 90)
      }
    }
  }
  if (length(anrhi_vals) > 0) {
    for (v in anrhi_vals) {
      p <- p + ggplot2::geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5)
    }
    if (is.finite(min_val)) {
      for (v in anrhi_vals) {
        p <- p + ggplot2::annotate("text", x = v, y = min_val, label = "ULN", hjust = 0.5, vjust = -0.5, size = 3, colour = "grey30")
        p <- p + ggplot2::annotate("text", x = min_val, y = v, label = "ULN", hjust = -0.2, vjust = 0.5, size = 3, colour = "grey30", angle = 90)
      }
    }
  }
}

# Save figure as PNG
# SAS template size roughly 800x600; use 10x8 inches at 150 dpi
ggplot2::ggsave(filename = file.path(out, "sgrender.png"), plot = p, width = 10, height = 8, dpi = 150, units = "in")
# Also save as SGRender.png case variant
try(ggplot2::ggsave(filename = file.path(out, "SGRender.png"), plot = p, width = 10, height = 8, dpi = 150, units = "in"), silent = TRUE)

# CSV of plotted data
plot_csv <- plot_data %>%
  dplyr::select(STUDYID, USUBJID, TRTA, PARAMCD, BASE, AVAL, ANRLO, ANRHI, LLN_label, ULN_label, dplyr::everything()) %>%
  format_dates_for_csv()
write.csv(plot_csv, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
write.csv(plot_csv, file.path(out, "SGRender.csv"), row.names = FALSE, na = "")

# Also ensure figure data CSV for generic name
write.csv(plot_csv, file.path(out, "figure_data.csv"), row.names = FALSE, na = "")

```