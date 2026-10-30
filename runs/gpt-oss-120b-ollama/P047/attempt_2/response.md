```r
# P047_mean_time.R
# Reimplementation of SAS program P047_mean_time.sas in R
# ----------------------------------------------------
# Required packages (assumed pre‑installed):
# haven, dplyr, tidyr, readr, stringr, lubridate, ggplot2

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

# ------------------------------------------------------------------
# Helper: round half up (SAS default rounding)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx <- abs(x) * 10^digits
  rounded <- floor(absx + 0.5) / 10^digits
  rounded * posneg
}

# ------------------------------------------------------------------
# Output directory (environment variable OUT_DIR must be set)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Read source data (advs.xpt)
source_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- read_xpt(source_path)

# Convert all variable names to lower case for easier handling (SAS is case‑insensitive)
advs_raw <- advs_raw %>% rename_all(tolower)

# ------------------------------------------------------------------
# 2. Filter and keep required variables (mirrors SAS WHERE clause)
advs <- advs_raw %>%
  filter(
    paramcd == "DIABP",
    atptn == 815,
    anl01fl == "Y",
    !is.na(avisitn) & avisitn > 0 & avisitn < 99,
    saffl == "Y"
  ) %>%
  select(
    usubjid, trtpn, anl01fl, param, paramcd,
    avisit, avisitn, atptn, saffl, aval
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
    t_val = qt(0.95, df = n - 1),          # SAS TINV(0.95, n-1) equivalent
    lo = mean - t_val * stderr,
    hi = mean + t_val * stderr
  ) %>%
  select(trtpn, avisitn, avisit, n, mean, lo, hi)

# ------------------------------------------------------------------
# 4. Split by treatment group (trtpn values 0, 54, 81)
a  <- summary_stats %>% filter(trtpn == 0)
b  <- summary_stats %>% filter(trtpn == 54)
c  <- summary_stats %>% filter(trtpn == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi)
c1 <- c %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi)

# ------------------------------------------------------------------
# 5. Merge the three groups by avisitn and avisit (PROC MERGE style)
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
# 6. Write the merged dataset (output dataset)
write.csv(
  all_data,
  file = file.path(out_dir, "all.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 7. Produce the SAS‑style listing (P047.lst) and a CSV version
# Helper formatters matching typical SAS default (2 decimals for means/CI, integer for counts)
fmt_num <- function(x) sprintf("%8.2f", round_half_up(x, 2))
fmt_int <- function(x) ifelse(is.na(x), "    .", sprintf("%4d", x))

# Header lines (mirroring SAS PROC PRINT output style)
listing_header <- c(
  "Mean DBP Profile by Treatment Group",
  "------------------------------------------------------------",
  sprintf("%-4s %-4s %4s %8s %8s %8s %4s %8s %8s %8s %4s %8s %8s %8s",
          "xc","x","a_n","a_mean","a_lcl","a_ucl",
          "b_n","b_mean","b_lcl","b_ucl",
          "c_n","c_mean","c_lcl","c_ucl")
)

listing_body <- apply(all_data, 1, function(row) {
  sprintf("%-4s %-4s %s %s %s %s %s %s %s %s %s %s %s %s",
          row["xc"], row["x"],
          fmt_int(row["a_n"]), fmt_num(row["a_mean"]), fmt_num(row["a_lcl"]), fmt_num(row["a_ucl"]),
          fmt_int(row["b_n"]), fmt_num(row["b_mean"]), fmt_num(row["b_lcl"]), fmt_num(row["b_ucl"]),
          fmt_int(row["c_n"]), fmt_num(row["c_mean"]), fmt_num(row["c_lcl"]), fmt_num(row["c_ucl"]))
})

listing_all <- c(listing_header, listing_body)

# Write text rendering (both .lst and .txt for safety)
writeLines(listing_all, con = file.path(out_dir, "P047.lst"))
writeLines(listing_all, con = file.path(out_dir, "P047.txt"))

# Write CSV version of the table body (no formatting)
write.csv(
  all_data,
  file = file.path(out_dir, "P047.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 8. Create the graph (ClinicalHandout_DbpProfile.png)
# Offsets replicate SAS scatterplot x adjustments
plot_df <- all_data %>%
  mutate(
    x_a = xc - 0.05,
    x_b = xc + 0.05,
    x_c = xc + 0.15
  )

p <- ggplot() +
  # Scatter points with error bars
  geom_errorbar(data = plot_df, aes(x = x_a, ymin = a_lcl, ymax = a_ucl),
                width = 0.02, colour = "#1b9e77") +
  geom_point(data = plot_df, aes(x = x_a, y = a_mean),
             colour = "#1b9e77", size = 3) +

  geom_errorbar(data = plot_df, aes(x = x_b, ymin = b_lcl, ymax = b_ucl),
                width = 0.02, colour = "#d95f02") +
  geom_point(data = plot_df, aes(x = x_b, y = b_mean),
             colour = "#d95f02", size = 3) +

  geom_errorbar(data = plot_df, aes(x = x_c, ymin = c_lcl, ymax = c_ucl),
                width = 0.02, colour = "#7570b3") +
  geom_point(data = plot_df, aes(x = x_c, y = c_mean),
             colour = "#7570b3", size = 3) +

  # Series lines (dash patterns)
  geom_line(data = plot_df, aes(x = x_a, y = a_mean, group = 1),
            linetype = "shortdash", colour = "#1b9e77") +
  geom_line(data = plot_df, aes(x = x_b, y = b_mean, group = 1),
            linetype = "mediumdash", colour = "#d95f02") +
  geom_line(data = plot_df, aes(x = x_c, y = c_mean, group = 1),
            linetype = "dashed", colour = "#7570b3") +

  # Block plots for at‑risk subject counts
  geom_col(data = plot_df, aes(x = xc, y = a_n),
           width = 0.08, fill = "#1b9e77", position = position_nudge(x = -0.05)) +
  geom_col(data = plot_df, aes(x = xc, y = b_n),
           width = 0.08, fill = "#d95f02", position = position_nudge(x = 0.05)) +
  geom_col(data = plot_df, aes(x = xc, y = c_n),
           width = 0.08, fill = "#7570b3", position = position_nudge(x = 0.15)) +

  # Count labels on top of blocks
  geom_text(data = plot_df, aes(x = xc - 0.05, y = a_n, label = a_n),
            vjust = -0.5, size = 3, colour = "#1b9e77") +
  geom_text(data = plot_df, aes(x = xc + 0.05, y = b_n, label = b_n),
            vjust = -0.5, size = 3, colour = "#d95f02") +
  geom_text(data = plot_df, aes(x = xc + 0.15, y = c_n, label = c_n),
            vjust = -0.5, size = 3, colour = "#7570b3") +

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

png_path <- file.path(out_dir, "clinicalhandout_dbpprofile.png")
ggsave(filename = png_path, plot = p, width = 8, height = 6, dpi = 100)

# Save the data used for the plot (same as all_data) for reference
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
```