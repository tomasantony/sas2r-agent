# P055_outliers_TEAbnorm_Qual.R
# --------------------------------------------------------------
# Reimplementation of SAS program P055_outliers_TEAbnorm_Qual.sas in R
# --------------------------------------------------------------
# Packages (pre‑installed)
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(tidyr)      # reshaping
library(ggplot2)    # graphics
library(readr)      # write_csv
library(stringr)    # string handling
library(purrr)      # functional helpers

# ------------------------------------------------------------------
# Helper: round half away from zero (SAS default) – not needed for means/CI
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  rounded <- floor(absx * 10^digits + 0.5) / 10^digits
  posneg * rounded
}

# ------------------------------------------------------------------
# Directories
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Load source data (advs.xpt)
rawadam   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
advs_path <- file.path(rawadam, "advs.xpt")
advs_raw  <- read_xpt(advs_path)

# ------------------------------------------------------------------
# 2. Apply SAS filters and keep needed variables
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
    ADY  = as.numeric(ADY),
    AVAL = as.numeric(AVAL)
  ) %>%
  arrange(TRTA, ADY)

# ------------------------------------------------------------------
# 3. PROC MEANS equivalent – mean, n, LCLM, UCLM per TRTA ADY
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
    tval = ifelse(num > 1, qt(0.975, df = num - 1), NA_real_),
    lclm = mean - tval * se,
    uclm = mean + tval * se
  ) %>%
  select(TRTA, ADY, mean, num, lclm, uclm)

# ------------------------------------------------------------------
# 4. Create CI table with ADY adjustments (same logic as SAS DATA step)
ci <- meanvs %>%
  select(TRTA, ADY, lclm, uclm) %>%
  mutate(
    ADY = case_when(
      TRTA == "Xanomeline Low Dose"  ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo"              ~ ADY + 0.6,
      TRUE                           ~ ADY
    )
  ) %>%
  arrange(TRTA, ADY)

mean_tbl <- meanvs %>% select(TRTA, ADY, mean)
num_tbl  <- meanvs %>% select(TRTA, ADY, num)

# ------------------------------------------------------------------
# 5. Build final dataset for the plot (mean + num + CI + adjusted ADY)
final <- mean_tbl %>%
  left_join(num_tbl, by = c("TRTA", "ADY")) %>%
  left_join(ci %>% select(TRTA, ADY, lclm, uclm), by = c("TRTA", "ADY")) %>%
  mutate(
    ADY_adj = case_when(
      TRTA == "Xanomeline Low Dose"  ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo"              ~ ADY + 0.6,
      TRUE                           ~ ADY
    )
  ) %>%
  select(TRTA, ADY = ADY_adj, mean, num, lclm, uclm)

write_csv(final, file.path(out_dir, "final.csv"), na = "")

# ------------------------------------------------------------------
# 6. Annotation for sample‑size labels (placed below the x‑axis)
anno2 <- num_tbl %>%
  mutate(
    y = case_when(
      TRTA == "Placebo"              ~ -170,
      TRTA == "Xanomeline High Dose" ~ -180,
      TRTA == "Xanomeline Low Dose"  ~ -190,
      TRUE                           ~ -170
    ),
    text  = as.character(num),
    color = case_when(
      TRTA == "Placebo"              ~ "blue",
      TRTA == "Xanomeline High Dose" ~ "red",
      TRTA == "Xanomeline Low Dose"  ~ "green",
      TRUE                           ~ "black"
    )
  ) %>%
  select(TRTA, x = ADY, y, text, color)

write_csv(anno2, file.path(out_dir, "anno2.csv"), na = "")

# ------------------------------------------------------------------
# 7. Produce the SAS‑style listing (PROC PRINT of CI)
ci_csv_path <- file.path(out_dir, "ci.csv")
write_csv(ci, ci_csv_path, na = "")

# Helper to mimic SAS BEST12. format (no unnecessary trailing zeros)
fmt_best12 <- function(x) {
  if (is.na(x)) return("")
  # Use format with scientific = FALSE, trim = TRUE, digits = 12, format = "g"
  format(x, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
}

# Build text lines
header_line <- sprintf("%-20s %12s %12s %12s", "TRTA", "ADY", "LCLM", "UCLM")
row_lines <- apply(ci, 1, function(r) {
  trta  <- sprintf("%-20s", r["TRTA"])
  ady   <- sprintf("%12s", fmt_best12(as.numeric(r["ADY"])))
  lclm  <- sprintf("%12s", fmt_best12(as.numeric(r["lclm"])))
  uclm  <- sprintf("%12s", fmt_best12(as.numeric(r["uclm"])))
  paste0(trta, " ", ady, " ", lclm, " ", uclm)
})

listing_txt_path <- file.path(out_dir, "P055.lst")   # no extension, as SAS expects
listing_txt <- c(
  "CI (Adjusted ADY) – PROC PRINT output",
  strrep("-", 60),
  header_line,
  row_lines
)
writeLines(listing_txt, listing_txt_path)

# Also write a CSV version of the listing (required by benchmark)
write_csv(ci, file.path(out_dir, "P055.lst.csv"), na = "")

# ------------------------------------------------------------------
# 8. Plot – replicate SAS GPLOT using ggplot2
shape_map <- c(
  "Placebo"               = 16,  # solid circle
  "Xanomeline High Dose"  = 17,  # triangle
  "Xanomeline Low Dose"   = 8    # star
)
color_map <- c(
  "Placebo"               = "blue",
  "Xanomeline High Dose"  = "red",
  "Xanomeline Low Dose"   = "green"
)

p <- ggplot(final, aes(x = ADY, y = mean, colour = TRTA, shape = TRTA)) +
  geom_errorbar(aes(ymin = lclm, ymax = uclm), width = 0.2, size = 0.8) +
  geom_point(size = 3) +
  geom_text(
    data = anno2,
    aes(x = x, y = y, label = text, colour = color),
    inherit.aes = FALSE,
    size = 3,
    vjust = 0
  ) +
  scale_shape_manual(values = shape_map) +
  scale_colour_manual(values = color_map) +
  labs(
    title    = "Systolic Blood Pressure Figure by Treatment",
    x        = "Visit number (visit day)",
    y        = "Systolic blood pressure",
    colour   = "Treatment",
    shape    = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank()
  ) +
  scale_y_continuous(limits = c(-200, 350), breaks = seq(-200, 350, 50)) +
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 50))

png_path <- file.path(out_dir, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------
# End of script – all required artefacts are now in OUT_DIR
