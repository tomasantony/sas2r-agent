# P055_outliers_TEAbnorm_Qual.R
# Reimplementation of SAS program P055_outliers_TEAbnorm_Qual.sas in R
# --------------------------------------------------------------
# Required libraries (pre‑installed in the execution environment)
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(tidyr)      # data reshaping
library(ggplot2)    # graphics
library(readr)      # write_csv
library(stringr)    # string handling
library(purrr)      # functional helpers
library(lubridate)  # date handling (not used here but loaded for completeness)

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  rounded <- floor(absx * 10^digits + 0.5) / 10^digits
  return(posneg * rounded)
}

# ------------------------------------------------------------------
# Set up directories
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Load source data (advs.xpt)
rawadam   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
advs_path <- file.path(rawadam, "advs.xpt")
advs_raw  <- read_xpt(advs_path)

# ------------------------------------------------------------------
# 2. Apply the same filters as the SAS DATA step
advs <- advs_raw %>%
  filter(
    SAFFL   == "Y",
    PARAMCD == "DIABP",
    ATPT    == "AFTER LYING DOWN FOR 5 MINUTES",
    AVISITN >= 0, AVISITN < 99,
    ANL01FL == "Y"
  ) %>%
  select(USUBJID, TRTA, ADY, AVAL) %>%
  mutate(
    ADY = as.numeric(ADY),   # ensure numeric
    AVAL = as.numeric(AVAL)
  ) %>%
  arrange(TRTA, ADY)

# ------------------------------------------------------------------
# 3. PROC MEANS equivalent: mean, n, LCLM, UCLM per TRTA ADY
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    mean = mean(AVAL, na.rm = TRUE),
    num  = n(),
    sd   = sd(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se   = sd / sqrt(num),
    # SAS uses t‑distribution for 95% CI (default alpha=0.05)
    tval = ifelse(num > 1, qt(0.975, df = num - 1), NA_real_),
    lclm = mean - tval * se,
    uclm = mean + tval * se
  ) %>%
  select(TRTA, ADY, mean, num, lclm, uclm)

# ------------------------------------------------------------------
# 4. Create the three intermediate tables (mean, num, ci) with ADY adjustments
#    (the same logic as the SAS DATA step that modifies ADY for plotting)
ci <- meanvs %>%
  select(TRTA, ADY, lclm, uclm) %>%
  mutate(
    ADY = case_when(
      TRTA == "Xanomeline Low Dose"  ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo"               ~ ADY + 0.6,
      TRUE                           ~ ADY
    )
  ) %>%
  arrange(TRTA, ADY)

mean_tbl <- meanvs %>%
  select(TRTA, ADY, mean)

num_tbl <- meanvs %>%
  select(TRTA, ADY, num)

# ------------------------------------------------------------------
# 5. Produce the listing that SAS would have printed (PROC PRINT of CI)
#    Write both a CSV and a plain‑text rendering.
ci_csv_path  <- file.path(out_dir, "ci.csv")
ci_txt_path  <- file.path(out_dir, "ci.txt")
write_csv(ci, ci_csv_path, na = "")

# Plain‑text rendering – mimic SAS PROC PRINT layout (simple tabular view)
ci_txt <- capture.output({
  cat("CI (Adjusted ADY) – PROC PRINT output\n")
  cat(strrep("-", 60), "\n")
  print(ci, row.names = FALSE)
})
writeLines(ci_txt, ci_txt_path)

# ------------------------------------------------------------------
# 6. Build the final dataset used for the plot (mean + num + adjusted ADY)
final <- mean_tbl %>%
  left_join(num_tbl, by = c("TRTA", "ADY")) %>%
  mutate(
    ADY_adj = case_when(
      TRTA == "Xanomeline Low Dose"  ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo"               ~ ADY + 0.6,
      TRUE                           ~ ADY
    )
  ) %>%
  select(TRTA, ADY = ADY_adj, mean, num)

final_csv_path <- file.path(out_dir, "final.csv")
write_csv(final, final_csv_path, na = "")

# ------------------------------------------------------------------
# 7. Create annotation data for the sample‑size labels (placed below the x‑axis)
anno2 <- final %>%
  mutate(
    y = case_when(
      TRTA == "Placebo"               ~ -170,
      TRTA == "Xanomeline High Dose"  ~ -180,
      TRTA == "Xanomeline Low Dose"  ~ -190,
      TRUE                            ~ -170
    ),
    text = as.character(num),
    color = case_when(
      TRTA == "Placebo"               ~ "blue",
      TRTA == "Xanomeline High Dose"  ~ "red",
      TRTA == "Xanomeline Low Dose"  ~ "green",
      TRUE                            ~ "black"
    )
  ) %>%
  select(TRTA, x = ADY, y, text, color)

anno2_csv_path <- file.path(out_dir, "anno2.csv")
write_csv(anno2, anno2_csv_path, na = "")

# ------------------------------------------------------------------
# 8. Plot – replicate the SAS GPLOT appearance using ggplot2
#    Symbol mapping: Placebo = blue dot, High Dose = red triangle, Low Dose = green star
shape_map <- c(
  "Placebo"               = 16,  # solid circle (dot)
  "Xanomeline High Dose"  = 17,  # triangle
  "Xanomeline Low Dose"   = 8    # star
)
color_map <- c(
  "Placebo"               = "blue",
  "Xanomeline High Dose"  = "red",
  "Xanomeline Low Dose"   = "green"
)

p <- ggplot(final, aes(x = ADY, y = mean, colour = TRTA, shape = TRTA)) +
  # error bars (CI)
  geom_errorbar(aes(ymin = lclm, ymax = uclm), width = 0.2, size = 0.8) +
  # mean points
  geom_point(size = 3) +
  # sample‑size annotations
  geom_text(data = anno2, aes(label = text, x = x, y = y, colour = color),
            inherit.aes = FALSE, size = 3, vjust = 0) +
  scale_shape_manual(values = shape_map) +
  scale_colour_manual(values = color_map) +
  labs(
    title = "Systolic Blood Pressure Figure by Treatment",
    subtitle = "",
    x = "Visit number (visit day)",
    y = "Systolic blood pressure",
    colour = "Treatment",
    shape = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank()
  ) +
  # replicate SAS axis ranges
  scale_y_continuous(limits = c(-200, 350), breaks = seq(-200, 350, 50)) +
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 50))

# Save the figure
png_path <- file.path(out_dir, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------
# 9. Write the listing requested by the benchmark (P055.lst)
#    The SAS program printed the CI table; we will use that as the listing.
listing_txt_path <- file.path(out_dir, "P055.lst.txt")
listing_csv_path <- file.path(out_dir, "P055.lst.csv")
writeLines(ci_txt, listing_txt_path)
write_csv(ci, listing_csv_path, na = "")

# ------------------------------------------------------------------
# End of script – all required artefacts are now in OUT_DIR
