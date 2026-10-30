# P047_mean_time.R
# Reimplementation of SAS program P047_mean_time.sas in R
# ----------------------------------------------------
# Required packages are assumed to be installed:
# haven, dplyr, tidyr, readr, stringr, lubridate, ggplot2, tibble

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

# ------------------------------------------------------------------
# Helper: round half up (SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x) * 10^digits
  rounded <- floor(absx + 0.5) / 10^digits
  return(rounded * posneg)
}

# ------------------------------------------------------------------
# Output directory (environment variable OUT_DIR must be set)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Read source data (advs.xpt)
source_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- read_xpt(source_path)

# 2. Filter and keep required variables
advs <- advs_raw %>%
  filter(
    PARAMCD == "DIABP",
    ATPTN == 815,
    ANL01FL == "Y",
    !is.na(avisitn) & avisitn > 0 & avisitn < 99,
    SAFFL == "Y"
  ) %>%
  select(
    usubjid, trtpn, ANL01FL, PARAM, PARAMCD,
    avisit, avisitn, ATPTN, SAFFL, aval
  )

# ------------------------------------------------------------------
# 3. Summary statistics (PROC MEANS equivalent)
summary_stats <- advs %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(
    n = n(),
    mean = mean(aval, na.rm = TRUE),
    sd = sd(aval, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stderr = sd / sqrt(n),
    t_val = qt(0.95, df = n - 1),               # SAS TINV(0.95, n-1) equivalent
    lo = mean - t_val * stderr,
    hi = mean + t_val * stderr
  ) %>%
  select(trtpn, avisitn, avisit, n, mean, lo, hi)

# 4. Prepare data for plotting (split by trtpn)
a  <- summary_stats %>% filter(trtpn == 0)
b  <- summary_stats %>% filter(trtpn == 54)
c  <- summary_stats %>% filter(trtpn == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi)
c1 <- c %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi)

# 5. Merge the three groups by avisitn and avisit
all_data <- a1 %>%
  full_join(b1, by = c("avisitn", "avisit")) %>%
  full_join(c1, by = c("avisitn", "avisit")) %>%
  rename(
    xc = avisitn,
    x  = avisit
  ) %>%
  select(
    xc, x,
    a_n, a_mean, a_lcl, a_ucl,
    b_n, b_mean, b_lcl, b_ucl,
    c_n, c_mean, c_lcl, c_ucl
  ) %>%
  arrange(xc)

# ------------------------------------------------------------------
# 6. Write the merged dataset as CSV (output dataset)
write.csv(
  all_data,
  file = file.path(out_dir, "all.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 7. Produce the listing (P047.lst) – a formatted table of the data
#    SAS typically prints numbers with 2 decimal places for means/CI
fmt_num <- function(x) sprintf("%8.2f", round_half_up(x, 2))
fmt_int <- function(x) sprintf("%4d", x)

listing_lines <- c(
  "Mean DBP Profile by Treatment Group",
  "------------------------------------------------------------",
  sprintf("%-4s %-4s %4s %8s %8s %8s %4s %8s %8s %8s %4s %8s %8s %8s",
          "xc","x","a_n","a_mean","a_lcl","a_ucl",
          "b_n","b_mean","b_lcl","b_ucl",
          "c_n","c_mean","c_lcl","c_ucl")
)

for (i in seq_len(nrow(all_data))) {
  row <- all_data[i, ]
  listing_lines <- c(
    listing_lines,
    sprintf("%-4s %-4s %s %s %s %s %s %s %s %s %s %s %s %s",
            row$xc, row$x,
            fmt_int(row$a_n), fmt_num(row$a_mean), fmt_num(row$a_lcl), fmt_num(row$a_ucl),
            fmt_int(row$b_n), fmt_num(row$b_mean), fmt_num(row$b_lcl), fmt_num(row$b_ucl),
            fmt_int(row$c_n), fmt_num(row$c_mean), fmt_num(row$c_lcl), fmt_num(row$c_ucl))
  )
}

# Write text rendering
writeLines(listing_lines, con = file.path(out_dir, "P047.lst"))

# Also write CSV version of the table body (without header formatting)
write.csv(
  all_data,
  file = file.path(out_dir, "P047.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 8. Create the graph (ClinicalHandout_DbpProfile.png)
#    Offsets mimic the SAS scatterplot x adjustments
plot_df <- all_data %>%
  mutate(
    x_a = xc - 0.05,
    x_b = xc + 0.05,
    x_c = xc + 0.15
  )

p <- ggplot() +
  # Scatter with error bars
  geom_errorbar(data = plot_df, aes(x = x_a, ymin = a_lcl, ymax = a_ucl), width = 0.02, colour = "#1b9e77") +
  geom_point(data = plot_df, aes(x = x_a, y = a_mean), colour = "#1b9e77", size = 3) +

  geom_errorbar(data = plot_df, aes(x = x_b, ymin = b_lcl, ymax = b_ucl), width = 0.02, colour = "#d95f02") +
  geom_point(data = plot_df, aes(x = x_b, y = b_mean), colour = "#d95f02", size = 3) +

  geom_errorbar(data = plot_df, aes(x = x_c, ymin = c_lcl, ymax = c_ucl), width = 0.02, colour = "#7570b3") +
  geom_point(data = plot_df, aes(x = x_c, y = c_mean), colour = "#7570b3", size = 3) +

  # Series lines
  geom_line(data = plot_df, aes(x = x_a, y = a_mean, group = 1), linetype = "shortdash", colour = "#1b9e77") +
  geom_line(data = plot_df, aes(x = x_b, y = b_mean, group = 1), linetype = "mediumdash", colour = "#d95f02") +
  geom_line(data = plot_df, aes(x = x_c, y = c_mean, group = 1), linetype = "dashed", colour = "#7570b3") +

  # Blockplot – counts as columns
  geom_col(data = plot_df, aes(x = xc, y = a_n), width = 0.08, fill = "#1b9e77", position = position_nudge(x = -0.05)) +
  geom_col(data = plot_df, aes(x = xc, y = b_n), width = 0.08, fill = "#d95f02", position = position_nudge(x = 0.05)) +
  geom_col(data = plot_df, aes(x = xc, y = c_n), width = 0.08, fill = "#7570b3", position = position_nudge(x = 0.15)) +

  # Count labels on top of blocks
  geom_text(data = plot_df, aes(x = xc - 0.05, y = a_n, label = a_n), vjust = -0.5, size = 3, colour = "#1b9e77") +
  geom_text(data = plot_df, aes(x = xc + 0.05, y = b_n, label = b_n), vjust = -0.5, size = 3, colour = "#d95f02") +
  geom_text(data = plot_df, aes(x = xc + 0.15, y = c_n, label = c_n), vjust = -0.5, size = 3, colour = "#7570b3") +

  labs(
    title = "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
    x = "Weeks Since Randomized",
    y = "Mean (unit) with 95% CI"
  ) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,16,20,24,26), limits = c(0, 27)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = "Treatment Group:", override.aes = list(alpha = 1))) 

png_file <- file.path(out_dir, "clinicalhandout_dbpprofile.png")
ggsave(filename = png_file, plot = p, width = 8, height = 6, dpi = 100)

# Save plotted data (same as all_data) for reference
write.csv(
  all_data,
  file = file.path(out_dir, "clinicalhandout_dbpprofile.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 9. Create HTML wrapper (DBP_Profile.html) that embeds the PNG
html_content <- paste0(
  "<!DOCTYPE html>\n<html>\n<head><title>DBP Profile</title></head>\n<body>\n",
  "<h2>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h2>\n",
  "<img src='clinicalhandout_dbpprofile.png' alt='DBP Profile'>\n",
  "</body>\n</html>"
)

writeLines(html_content, con = file.path(out_dir, "DBP_Profile.html"))

# ------------------------------------------------------------------
# End of script
