```r
# P052_outliers_Scatter_MeanTime.R
# Translation of SAS program P052_outliers_Scatter_MeanTime.sas to R
# --------------------------------------------------------------
# Required libraries (already installed in the execution environment)
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(tidyr)      # data tidying
library(ggplot2)    # graphics
library(stringr)   # string handling
library(purrr)      # functional programming
library(readr)      # write_csv
library(lubridate) # date handling (not used here but loaded for completeness)

# ------------------------------------------------------------------
# 1. Define paths from macro variables (hard‑coded as in the SAS program)
root      <- "D:/meta_hackathon/sas2r-agent"
rawadam   <- file.path(root, "data", "raw", "cdisc-pilot", "adam")
out_dir   <- Sys.getenv("OUT_DIR")               # directory for all outputs
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# 2. Read source XPT files
adsl_path   <- file.path(rawadam, "adsl.xpt")
advsmax_path<- file.path(rawadam, "advsmax.xpt")

adsl   <- read_xpt(adsl_path)   # not used later but read to mirror SAS steps
advsmax<- read_xpt(advsmax_path)

# ------------------------------------------------------------------
# 3. Sort advsmax by STUDYID (SAS default sort is ascending, case‑sensitive)
advsmax <- advsmax %>% arrange(STUDYID)

# ------------------------------------------------------------------
# 4. Create advsmax_final with the same WHERE clause and first‑by‑STUDYID logic
advsmax_final <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN   == 815,
         ANL01FL == "Y",
         ADY     > 1,
         SAFFL   == "Y") %>%
  group_by(STUDYID) %>%
  mutate(
    LLN_label = if_else(row_number() == 1, "LLN", NA_character_),
    ULN_label = if_else(row_number() == 1, "ULN", NA_character_)
  ) %>%
  ungroup()

# ------------------------------------------------------------------
# 5. Compute overall ranges (min/max of BASE and AVAL)
range_vals <- advsmax_final %>%
  summarise(
    min_base = min(BASE, na.rm = TRUE),
    max_base = max(BASE, na.rm = TRUE),
    min_aval = min(AVAL, na.rm = TRUE),
    max_aval = max(AVAL, na.rm = TRUE)
  ) %>% as.list()

# 6. Derive combined min / max (range2)
combined_min <- min(range_vals$min_base, range_vals$min_aval, na.rm = TRUE)
combined_max <- max(range_vals$max_base, range_vals$max_aval, na.rm = TRUE)

# Store macro‑like values for later use
min_axis <- combined_min
max_axis <- combined_max

# ------------------------------------------------------------------
# 7. Count distinct STUDYID across the whole dataset
count_study <- advsmax_final %>% distinct(STUDYID) %>% nrow()

# 8. Add layout variables (column_numbers, row_numbers) based on count_study
advsmax_final2 <- advsmax_final %>%
  mutate(count_study = count_study)

advsmax_final3 <- advsmax_final2 %>%
  mutate(
    column_numbers = 2L,
    row_numbers    = if_else(count_study <= 2L, 1L, 2L)
  )

# Capture the (single) layout values for later use
column_numbers <- advsmax_final3$column_numbers[1]
row_numbers    <- advsmax_final3$row_numbers[1]

# ------------------------------------------------------------------
# 9. Write the listing (P052.lst) – contains the four numbers:
#    MIN, MAX, COLUMN_NUMBERS, ROW_NUMBERS
listing_df <- tibble(
  Variable = c("MIN", "MAX", "COLUMN_NUMBERS", "ROW_NUMBERS"),
  Value    = c(min_axis, max_axis, column_numbers, row_numbers)
)

# Text rendering (fixed‑width simple table)
listing_txt <- paste0(
  sprintf("%-20s %s", "Variable", "Value"), "\n",
  paste0(
    apply(listing_df, 1, function(r) sprintf("%-20s %s", r[1], r[2])),
    collapse = "\n"
  ), "\n"
)

writeLines(listing_txt, file.path(out_dir, "P052.lst"))
write_csv(listing_df, file.path(out_dir, "P052.lst.csv"))

# ------------------------------------------------------------------
# 10. Prepare data for the scatter plot
#    (same dataset that SAS fed to PROC SGPLOT)
plot_data <- advsmax_final3

# For reference lines we need the unique ANRLO / ANRHI per STUDYID.
# SAS draws a line for each observation; drawing once per panel is sufficient.
ref_lines <- plot_data %>%
  select(STUDYID, ANRLO, ANRHI, LLN_label, ULN_label) %>%
  distinct()

# ------------------------------------------------------------------
# 11. Build the ggplot
p <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = TRTAN)) +
  geom_point() +
  # identity line (slope = 1, intercept = 0)
  geom_abline(intercept = 0, slope = 1, colour = "black", linetype = "dashed") +
  # vertical reference lines (ANRLO, ANRHI) with pattern = 2 (dashed)
  geom_vline(data = ref_lines, aes(xintercept = ANRLO), linetype = "dashed", colour = "grey50") +
  geom_vline(data = ref_lines, aes(xintercept = ANRHI), linetype = "dashed", colour = "grey50") +
  # horizontal reference lines
  geom_hline(data = ref_lines, aes(yintercept = ANRLO), linetype = "dashed", colour = "grey50") +
  geom_hline(data = ref_lines, aes(yintercept = ANRHI), linetype = "dashed", colour = "grey50") +
  # labels for LLN / ULN placed at the minimum axis value
  geom_text(
    data = ref_lines,
    aes(x = ANRLO, y = min_axis, label = LLN_label),
    vjust = 1.5, hjust = -0.1, size = 3, colour = "grey30"
  ) +
  geom_text(
    data = ref_lines,
    aes(x = ANRHI, y = min_axis, label = ULN_label),
    vjust = 1.5, hjust = 1.1, size = 3, colour = "grey30"
  ) +
  geom_text(
    data = ref_lines,
    aes(x = min_axis, y = ANRLO, label = LLN_label),
    vjust = -0.5, hjust = -0.1, size = 3, colour = "grey30"
  ) +
  geom_text(
    data = ref_lines,
    aes(x = min_axis, y = ANRHI, label = ULN_label),
    vjust = -0.5, hjust = 1.1, size = 3, colour = "grey30"
  ) +
  scale_x_continuous(
    name = "Maximum Baseline Measurement",
    limits = c(min_axis, max_axis),
    breaks = pretty(c(min_axis, max_axis), n = max_axis - min_axis + 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Maximum Post‑baseline Measurement",
    limits = c(min_axis, max_axis),
    breaks = pretty(c(min_axis, max_axis), n = max_axis - min_axis + 1),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(colour = "grey90")
  ) +
  # Facet layout: rows = row_numbers, columns = column_numbers
  facet_wrap(~ STUDYID, ncol = column_numbers)

# ------------------------------------------------------------------
# 12. Save the plot as PNG (SGRender.png) and also write the plotted data
png_path <- file.path(out_dir, "SGRender.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 300, units = "in")
write_csv(plot_data, file.path(out_dir, "SGRender_data.csv"))

# ------------------------------------------------------------------
# End of script
```