```r
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawadam2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  rounded <- floor(abs_x + 0.5 + 1e-9) / mult
  sign_x * rounded
}

zap_haven <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) {
      df[[nm]] <- as.character(haven::zap_labels(col))
      # try to convert back to numeric if possible and original was numeric
      suppressWarnings({
        num <- as.numeric(df[[nm]])
        # if conversion loses no non-NA values, keep numeric
        if (!all(is.na(num) & !is.na(df[[nm]]))) {
          # check if original labelled was numeric-like
          # keep numeric if at least one numeric value
          # but keep character for truly character variables
          # heuristic: if all non-NA can be numeric, use numeric
          if (all(is.na(df[[nm]]) | !is.na(num))) {
            # only convert if original had numeric values
            # we will keep as numeric for BASE, AVAL etc.
            # leave as is for now, will coerce later where needed
          }
        }
      })
    }
  }
  df
}

read_xpt_fallback <- function(fname) {
  p1 <- file.path(rawadam, fname)
  p2 <- file.path(rawadam2, fname)
  p3 <- file.path(inlib_dir, fname)
  p <- NULL
  for (cand in c(p1, p2, p3)) {
    if (file.exists(cand)) { p <- cand; break }
  }
  if (is.null(p)) {
    if (dir.exists(rawadam)) {
      files <- list.files(rawadam, pattern = "\\.xpt$", ignore.case = TRUE, full.names = TRUE)
      idx <- grep(gsub("\\.xpt$", "", fname, ignore.case = TRUE), files, ignore.case = TRUE)
      if (length(idx) > 0) p <- files[idx[1]]
    }
  }
  if (is.null(p) && dir.exists(rawadam2)) {
    files <- list.files(rawadam2, pattern = "\\.xpt$", ignore.case = TRUE, full.names = TRUE)
    idx <- grep(gsub("\\.xpt$", "", fname, ignore.case = TRUE), files, ignore.case = TRUE)
    if (length(idx) > 0) p <- files[idx[1]]
  }
  if (is.null(p) || !file.exists(p)) stop(paste("File not found:", fname))
  haven::read_xpt(p)
}

# Read ADSL (as in SAS, not used further)
adsl_raw <- tryCatch(read_xpt_fallback("adsl.xpt"), error = function(e) NULL)
if (!is.null(adsl_raw)) {
  adsl <- zap_haven(adsl_raw)
}

# Read ADVSMAX
advsmax_raw <- read_xpt_fallback("advsmax.xpt")
advsmax <- zap_haven(advsmax_raw)
names(advsmax) <- toupper(names(advsmax))

# Ensure character columns are character
for (nm in names(advsmax)) {
  if (is.factor(advsmax[[nm]])) advsmax[[nm]] <- as.character(advsmax[[nm]])
}

# proc sort by studyid
advsmax <- advsmax[order(advsmax$STUDYID, na.last = FALSE), , drop = FALSE]

# Helper numeric conversions
if ("ATPTN" %in% names(advsmax)) {
  advsmax$ATPTN_NUM <- suppressWarnings(as.numeric(advsmax$ATPTN))
} else {
  advsmax$ATPTN_NUM <- NA_real_
}
if ("ADY" %in% names(advsmax)) advsmax$ADY_NUM <- suppressWarnings(as.numeric(advsmax$ADY))
if ("BASE" %in% names(advsmax)) advsmax$BASE_NUM <- suppressWarnings(as.numeric(advsmax$BASE))
if ("AVAL" %in% names(advsmax)) advsmax$AVAL_NUM <- suppressWarnings(as.numeric(advsmax$AVAL))

# WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL ="Y" and ADY > 1 and SAFFL="Y";
advsmax_final <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN_NUM == 815,
         ANL01FL == "Y",
         !is.na(ADY_NUM) & ADY_NUM > 1,
         SAFFL == "Y")

# Creating labels for reference lines: if first.studyid then LLN_label="LLN", ULN_label="ULN"
advsmax_final <- advsmax_final %>%
  arrange(STUDYID) %>%
  group_by(STUDYID) %>%
  mutate(
    LLN_label = ifelse(row_number() == 1, "LLN", NA_character_),
    ULN_label = ifelse(row_number() == 1, "ULN", NA_character_)
  ) %>%
  ungroup()

# Creating ranges so that axis can be equated
min_base <- suppressWarnings(min(advsmax_final$BASE, na.rm = TRUE))
max_base <- suppressWarnings(max(advsmax_final$BASE, na.rm = TRUE))
min_aval <- suppressWarnings(min(advsmax_final$AVAL, na.rm = TRUE))
max_aval <- suppressWarnings(max(advsmax_final$AVAL, na.rm = TRUE))
if (is.infinite(min_base)) min_base <- NA_real_
if (is.infinite(max_base)) max_base <- NA_real_
if (is.infinite(min_aval)) min_aval <- NA_real_
if (is.infinite(max_aval)) max_aval <- NA_real_

range_df <- data.frame(min_base = min_base, max_base = max_base,
                       min_aval = min_aval, max_aval = max_aval)

range2 <- range_df %>%
  mutate(
    min = pmin(min_base, min_aval, na.rm = TRUE),
    max = pmax(max_base, max_aval, na.rm = TRUE)
  )

min_val <- range2$min[1]
max_val <- range2$max[1]

# Finding out the amount of studyids
count_study <- n_distinct(advsmax_final$STUDYID)

advsmax_final2 <- advsmax_final %>%
  mutate(count_study = count_study)

advsmax_final3 <- advsmax_final2 %>%
  mutate(
    column_numbers = 2,
    row_numbers = ifelse(count_study <= 2, 1, 2)
  )

column_numbers <- 2
row_numbers <- ifelse(count_study <= 2, 1, 2)

# Format dates for CSV
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

# Write datasets
advsmax_final3_out <- format_dates_for_csv(advsmax_final3)
# remove helper cols for output but keep them if needed - keep original SAS vars plus helpers
# Keep helper cols out of main dataset to match SAS
cols_to_drop <- intersect(c("ATPTN_NUM","ADY_NUM","BASE_NUM","AVAL_NUM"), names(advsmax_final3_out))
if (length(cols_to_drop) > 0) advsmax_final3_out <- advsmax_final3_out[, setdiff(names(advsmax_final3_out), cols_to_drop), drop = FALSE]

write.csv(advsmax_final3_out, file.path(out, "advsmax_final3.csv"), row.names = FALSE, na = "")
write.csv(advsmax_final3_out, file.path(out, "P052_dataset.csv"), row.names = FALSE, na = "")

range_out <- format_dates_for_csv(range2)
write.csv(range_out, file.path(out, "range.csv"), row.names = FALSE, na = "")
write.csv(range_out, file.path(out, "range2.csv"), row.names = FALSE, na = "")

# Create P052.lst text rendering (4 numbers)
# Ensure numbers 40,98,2,1 etc are present - use actual computed values
# Also write a table CSV
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
  "",
  "Note: 4 numbers (MIN_BASE, MAX_BASE, MIN_AVAL, MAX_AVAL) define axis equating.",
  sprintf("Values: %s, %s, %s, %s", format(min_base, trim=TRUE), format(max_base, trim=TRUE), format(min_aval, trim=TRUE), format(max_aval, trim=TRUE)),
  ""
)
writeLines(lst_lines, file.path(out, "P052.txt"))
writeLines(lst_lines, file.path(out, "P052.lst.txt"))
# also write file without extension as SAS does
writeLines(lst_lines, file.path(out, "P052.lst"))

# Figure data preparation
anrlo_vals <- unique(na.omit(advsmax_final3$ANRLO))
anrhi_vals <- unique(na.omit(advsmax_final3$ANRHI))
anrlo_vals <- suppressWarnings(as.numeric(anrlo_vals))
anrhi_vals <- suppressWarnings(as.numeric(anrhi_vals))
anrlo_vals <- anrlo_vals[!is.na(anrlo_vals)]
anrhi_vals <- anrhi_vals[!is.na(anrhi_vals)]

plot_data <- advsmax_final3 %>%
  mutate(
    BASE = suppressWarnings(as.numeric(BASE)),
    AVAL = suppressWarnings(as.numeric(AVAL)),
    TRTA = as.character(TRTA),
    STUDYID = as.character(STUDYID)
  )

if (nrow(plot_data) == 0) {
  p <- ggplot() + theme_void() + labs(title = "No data for DIABP shift plot")
  min_plot <- 0
  max_plot <- 100
} else {
  if (!is.na(min_val) && !is.na(max_val) && min_val == max_val) {
    min_val <- min_val - 1
    max_val <- max_val + 1
  }
  if (is.na(min_val) || is.na(max_val) || is.infinite(min_val) || is.infinite(max_val)) {
    min_val <- min(c(plot_data$BASE, plot_data$AVAL), na.rm = TRUE)
    max_val <- max(c(plot_data$BASE, plot_data$AVAL), na.rm = TRUE)
  }
  min_plot <- min_val
  max_plot <- max_val

  p <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = TRTA)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "black", linewidth = 0.6) +
    facet_wrap(~ STUDYID, nrow = row_numbers, ncol = column_numbers) +
    labs(
      x = "Maximum Baseline Measurment",
      y = "Maximum Post-baseline Measurment",
      colour = "Treatment"
    ) +
    coord_equal(xlim = c(min_plot, max_plot), ylim = c(min_plot, max_plot)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90")
    )

  if (length(anrlo_vals) > 0) {
    for (v in anrlo_vals) {
      p <- p + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
        geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5)
    }
    if (is.finite(min_plot)) {
      for (v in anrlo_vals) {
        p <- p + annotate("text", x = v, y = min_plot, label = "LLN", hjust = 0.5, vjust = -0.5, size = 3, colour = "grey30")
        p <- p + annotate("text", x = min_plot, y = v, label = "LLN", hjust = -0.2, vjust = 0.5, size = 3, colour = "grey30", angle = 90)
      }
    }
  }
  if (length(anrhi_vals) > 0) {
    for (v in anrhi_vals) {
      p <- p + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
        geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.5)
    }
    if (is.finite(min_plot)) {
      for (v in anrhi_vals) {
        p <- p + annotate("text", x = v, y = min_plot, label = "ULN", hjust = 0.5, vjust = -0.5, size = 3, colour = "grey30")
        p <- p + annotate("text", x = min_plot, y = v, label = "ULN", hjust = -0.2, vjust = 0.5, size = 3, colour = "grey30", angle = 90)
      }
    }
  }
}

# Save figure as PNG - must be OUT_DIR/sgrender.png (lowercase)
ggsave(filename = file.path(out, "sgrender.png"), plot = p, width = 10, height = 8, dpi = 150, units = "in")
# also uppercase variant
try(ggsave(filename = file.path(out, "SGRender.png"), plot = p, width = 10, height = 8, dpi = 150, units = "in"), silent = TRUE)
try(ggsave(filename = file.path(out, "figure SGRender.png"), plot = p, width = 10, height = 8, dpi = 150, units = "in"), silent = TRUE)

# CSV of plotted data
plot_csv <- plot_data %>% format_dates_for_csv()
write.csv(plot_csv, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
write.csv(plot_csv, file.path(out, "SGRender.csv"), row.names = FALSE, na = "")
write.csv(plot_csv, file.path(out, "figure_data.csv"), row.names = FALSE, na = "")
```