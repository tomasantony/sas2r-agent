```r
# P052_outliers_Scatter_MeanTime.sas -> R translation
# Shift (scatter) plot: Maximum Baseline vs Maximum Post-baseline DIABP
# Outputs: P052.txt/P052.csv (range table, 4 numbers) + sgrender.png/sgrender.csv

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(readr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- Inputs (absolute paths from context) ----
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_dir   <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
advsmax_xpt_raw <- file.path(rawadam_dir, "advsmax.xpt")
adsl_xpt_raw    <- file.path(rawadam_dir, "adsl.xpt")
advsmax_xpt_der <- file.path(inlib_dir, "advsmax.xpt")

advsmax_path <- advsmax_xpt_raw
if (!file.exists(advsmax_path) && file.exists(advsmax_xpt_der)) advsmax_path <- advsmax_xpt_der
# also try .sas7bdat fallback in derived lib
if (!file.exists(advsmax_path)) {
  cand <- file.path(inlib_dir, "advsmax.sas7bdat")
  if (file.exists(cand)) advsmax_path <- cand
}

adsl_path <- adsl_xpt_raw
if (!file.exists(adsl_path)) {
  cand2 <- file.path(inlib_dir, "adsl.sas7bdat")
  cand3 <- file.path(inlib_dir, "adsl.xpt")
  if (file.exists(cand2)) adsl_path <- cand2 else if (file.exists(cand3)) adsl_path <- cand3
}

# Read ADSL (as in SAS; not used downstream but reproduce step)
if (file.exists(adsl_path)) {
  if (grepl("\\.xpt$", adsl_path, ignore.case = TRUE)) {
    adsl <- haven::read_xpt(adsl_path)
  } else {
    adsl <- haven::read_sas(adsl_path)
  }
} else {
  adsl <- tibble::tibble()
}

# Read ADVSMAX
if (grepl("\\.xpt$", advsmax_path, ignore.case = TRUE)) {
  advsmax <- haven::read_xpt(advsmax_path)
} else {
  advsmax <- haven::read_sas(advsmax_path)
}

# Normalize names to uppercase for SAS-like handling, keep original case mapping
names(advsmax) <- toupper(names(advsmax))
stopifnot(all(c("STUDYID","PARAMCD","ATPTN","ANL01FL","ADY","SAFFL","BASE","AVAL") %in% names(advsmax)))

# ---- PROC SORT by studyid ----
advsmax <- advsmax %>% arrange(STUDYID)

# ---- DATA advsmax_final; WHERE ... ; first.studyid labels ----
# WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y" and ADY>1 and SAFFL="Y"
# ATPTN may be numeric or character; emulate SAS numeric comparison
atptn_num <- suppressWarnings(as.numeric(as.character(advsmax$ATPTN)))

advsmax_final <- advsmax %>%
  filter(PARAMCD == "DIABP",
         !is.na(atptn_num) & atptn_num == 815,
         ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y") %>%
  arrange(STUDYID)

# if first.studyid then LLN_label="LLN"; ULN_label="ULN";
advsmax_final <- advsmax_final %>%
  group_by(STUDYID) %>%
  mutate(LLN_label = ifelse(row_number() == 1, "LLN", NA_character_),
         ULN_label = ifelse(row_number() == 1, "ULN", NA_character_)) %>%
  ungroup()

stopifnot(nrow(advsmax_final) > 0)

# ---- PROC SQL range: min(base), max(base), min(aval), max(aval) ----
min_base <- suppressWarnings(min(advsmax_final$BASE, na.rm = TRUE))
max_base <- suppressWarnings(max(advsmax_final$BASE, na.rm = TRUE))
min_aval <- suppressWarnings(min(advsmax_final$AVAL, na.rm = TRUE))
max_aval <- suppressWarnings(max(advsmax_final$AVAL, na.rm = TRUE))
if (is.infinite(min_base)) min_base <- NA_real_
if (is.infinite(max_base)) max_base <- NA_real_
if (is.infinite(min_aval)) min_aval <- NA_real_
if (is.infinite(max_aval)) max_aval <- NA_real_

range_tbl <- data.frame(min_base = min_base, max_base = max_base,
                        min_aval = min_aval, max_aval = max_aval)

# ---- DATA range2; min=min(...); max=max(...) ----
rng_min <- suppressWarnings(min(c(min_base, min_aval), na.rm = TRUE))
rng_max <- suppressWarnings(max(c(max_base, max_aval), na.rm = TRUE))
range2 <- data.frame(min_base = min_base, max_base = max_base,
                     min_aval = min_aval, max_aval = max_aval,
                     min = rng_min, max = rng_max)

mn <- rng_min
mx <- rng_max

# ---- count(distinct studyid) ----
count_study <- dplyr::n_distinct(advsmax_final$STUDYID)

advsmax_final2 <- advsmax_final %>% mutate(count_study = count_study)

# ---- DATA advsmax_final3; column/row numbers ----
advsmax_final3 <- advsmax_final2 %>%
  mutate(column_numbers = ifelse(count_study <= 2, 2, 2),
         row_numbers = ifelse(count_study <= 2, 1, 2))
# distinct values -> macro vars
column_numbers <- unique(advsmax_final3$column_numbers)[1]
row_numbers <- unique(advsmax_final3$row_numbers)[1]

# ---- Write P052 dataset + listing (range = 4 numbers) ----
# CSV of table body
write.csv(range_tbl, file.path(out, "P052.csv"), row.names = FALSE, na = "")

# Text rendering mimicking listing of range
txt_con <- file.path(out, "P052.txt")
sink(txt_con)
cat("P052_outliers_Scatter_MeanTime\n")
cat("Range for equated axes (DIABP, ATPTN=815, ANL01FL=Y, ADY>1, SAFFL=Y)\n")
cat("\n")
cat(sprintf("%-12s %12s %12s %12s %12s\n", "", "min_base", "max_base", "min_aval", "max_aval"))
cat(sprintf("%-12s %12g %12g %12g %12g\n", "Value", min_base, max_base, min_aval, max_aval))
cat("\n")
cat(sprintf("Equated axis viewmin (min) = %g\n", mn))
cat(sprintf("Equated axis viewmax (max) = %g\n", mx))
cat(sprintf("Distinct STUDYID (count_study) = %d\n", count_study))
cat(sprintf("Panel columns = %d  rows = %d\n", column_numbers, row_numbers))
cat(sprintf("N observations plotted (advsmax_final3) = %d\n", nrow(advsmax_final3)))
sink()

# Also write full plotted dataset + range2 for traceability (dataset outputs)
# Primary plotted-data CSV for the figure:
# keep SAS variable names (uppercase), no .x/.y suffixes
plot_data <- as.data.frame(advsmax_final3)
write.csv(plot_data, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")

# ---- Build SGRender shiftplot equivalent with ggplot2 ----
# Reference values (ANRLO/ANRHI) - distinct non-missing in filtered data
anrlo_vals <- sort(unique(plot_data$ANRLO[!is.na(plot_data$ANRLO)]))
anrhi_vals <- sort(unique(plot_data$ANRHI[!is.na(plot_data$ANRHI)]))

# Integer breaks within [mn, mx]
span <- mx - mn
step <- 1
if (!is.na(span) && span > 10) step <- ceiling(span / 10)
brks <- seq(floor(mn), ceiling(mx), by = step)
brks <- brks[brks >= mn & brks <= mx]
if (length(brks) == 0) brks <- c(mn, mx)

# Ensure TRTA is character/factor for grouping (SAS group=trta)
plot_data$TRTA <- as.character(plot_data$TRTA)

p <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = TRTA)) +
  geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
  geom_abline(intercept = 0, slope = 1, colour = "black", linewidth = 0.6, na.rm = TRUE)

# referenceline x=ANRLO / x=ANRHI (dashed, pattern=2)
for (v in anrlo_vals) {
  p <- p + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6)
}
for (v in anrhi_vals) {
  p <- p + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6)
}
# referenceline y=ANRLO / y=ANRHI
for (v in anrlo_vals) {
  p <- p + geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6)
}
for (v in anrhi_vals) {
  p <- p + geom_hline(yintercept = v, linetype = "dashed", colour = "grey40", linewidth = 0.6)
}

# curvelabel LLN/ULN at min position
# vertical lines: label at y=mn ; horizontal lines: label at x=mn
if (length(anrlo_vals) > 0) {
  for (v in anrlo_vals) {
    p <- p + annotate("text", x = v, y = mn, label = "LLN", size = 3.5, vjust = -0.5, hjust = 0.5, colour = "grey30")
    p <- p + annotate("text", x = mn, y = v, label = "LLN", size = 3.5, vjust = 0.5, hjust = -0.2, colour = "grey30")
  }
}
if (length(anrhi_vals) > 0) {
  for (v in anrhi_vals) {
    p <- p + annotate("text", x = v, y = mn, label = "ULN", size = 3.5, vjust = -0.5, hjust = 0.5, colour = "grey30")
    p <- p + annotate("text", x = mn, y = v, label = "ULN", size = 3.5, vjust = 0.5, hjust = -0.2, colour = "grey30")
  }
}

p <- p +
  scale_x_continuous(limits = c(mn, mx), breaks = brks) +
  scale_y_continuous(limits = c(mn, mx), breaks = brks) +
  coord_equal(xlim = c(mn, mx), ylim = c(mn, mx), expand = TRUE) +
  labs(x = "Maximum Baseline Measurment",
       y = "Maximum Post-baseline Measurment",
       colour = NULL) +
  facet_wrap(~ STUDYID, nrow = row_numbers, ncol = column_numbers, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        aspect.ratio = 1)

ggsave(file.path(out, "sgrender.png"), plot = p, width = 10, height = 8, dpi = 300)
```