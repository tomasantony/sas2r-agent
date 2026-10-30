# P096_RoundToEven.R
# Reimplementation of SAS program P096_RoundToEven.sas in R
# Generates the same numeric results and graphics as the original SAS code.

library(dplyr)
library(tidyr)      # for pivot_longer (not used in final version but kept for completeness)
library(ggplot2)

# ----------------------------------------------------------------------
# Helper functions ------------------------------------------------------
# ----------------------------------------------------------------------
# Round half away from zero (SAS ROUND default)
round_half_away <- function(x, unit = 1) {
  sign_x <- sign(x)
  abs_x <- abs(x)
  rounded <- floor(abs_x / unit + 0.5) * unit
  sign_x * rounded
}

# Round half to even (SAS ROUNDE) – R's round() already does this
round_to_even <- function(x, unit = 1) {
  rounded <- round(x / unit) * unit
  rounded
}

# ----------------------------------------------------------------------
# Set up output directory -----------------------------------------------
# ----------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Create Series, Scatter1, Scatter2, combine into RoundToEven ----------
# ----------------------------------------------------------------------
# Series
series <- tibble(
  x = seq(0, 6, by = 0.05)
) %>%
  mutate(
    y = round_to_even(x),
    group = y
  )

# Scatter1 (closed endpoints)
scatter1 <- tibble(
  x1 = seq(0.5, 5.5, by = 1)
) %>%
  mutate(
    y1 = round_to_even(x1)
  )

# Scatter2 (open endpoints)
scatter2 <- tibble(
  x2 = seq(0.5, 5.5, by = 1)
) %>%
  mutate(
    y2 = round_to_even(x2 + 1) - 1
  )

# Combine into one data set (order matches SAS SET statement)
round_to_even_df <- bind_rows(
  series %>% rename(x = x, y = y, group = group) %>% mutate(x1 = NA, y1 = NA, x2 = NA, y2 = NA),
  scatter1 %>% rename(x = x1, y = y1) %>% mutate(group = NA, x2 = NA, y2 = NA),
  scatter2 %>% rename(x = x2, y = y2) %>% mutate(group = NA, x1 = NA, y1 = NA)
) %>% select(x, y, group, x1, y1, x2, y2)

write.csv(round_to_even_df,
          file.path(out_dir, "RoundToEven.csv"),
          row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# 2. Plot (SGPLOT equivalent) -------------------------------------------
# ----------------------------------------------------------------------
p <- ggplot() +
  geom_line(data = series,
            aes(x = x, y = y, group = group),
            colour = "black") +
  geom_point(data = scatter1,
             aes(x = x1, y = y1),
             shape = 21, size = 3,
             colour = "black",
             fill = "#445694",
             stroke = 1) +
  geom_point(data = scatter2,
             aes(x = x2, y = y2),
             shape = 21, size = 3,
             colour = "black",
             fill = "white",
             stroke = 1) +
  labs(x = "x", y = "RoundE(x)", title = "The Round-To-Even Function") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank())

ggsave(filename = file.path(out_dir, "SGPlot.png"),
       plot = p, width = 8, height = 6, dpi = 150)

write.csv(round_to_even_df,
          file.path(out_dir, "SGPlot_data.csv"),
          row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# 3. Seq table (compare ROUND vs ROUNDE) --------------------------------
# ----------------------------------------------------------------------
seq_df <- tibble(
  x = seq(0, 3.5, by = 0.25)
) %>%
  mutate(
    Round = round_half_away(x),      # SAS ROUND (away from zero)
    RoundEven = round_to_even(x)    # SAS ROUNDE (bankers rounding)
  )

write.csv(seq_df,
          file.path(out_dir, "Seq.csv"),
          row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# 4. Channel1 data (hard‑coded) -----------------------------------------
# ----------------------------------------------------------------------
channel1_vals <- c(
  0.91,1.01,0.95,1.13,1.12,0.86,0.96,1.17,1.36,1.10,
  0.98,1.27,1.13,0.92,1.15,1.26,1.14,0.88,1.03,1.00,
  0.98,0.94,1.09,0.92,1.10,0.95,1.05,1.05,1.11,1.15,
  1.11,0.98,0.78,1.09,0.94,1.05,0.89,1.16,0.88,1.19,
  1.01,1.08,1.19,0.94,0.92,1.27,0.90,0.88,1.38,1.02,
  1.21,0.99,1.19,1.05,0.95,0.92,1.06,1.03,0.76,1.04,
  0.84,0.93,0.96,1.23,1.06,1.20,0.94,0.74,0.80,1.00,
  1.00,0.97,0.94,1.05,1.09,1.07,0.79,1.06,0.96,1.00,
  1.07,0.98,1.26,0.99,1.12,0.99,1.05,1.12,0.85,0.96,
  1.01,1.21,0.95,0.86,0.86,1.19,1.24,0.75,1.03,0.84,
  0.93,0.65,0.84,0.92,0.86,1.07,1.05,0.95,1.05,1.10,
  0.93,1.03,0.94,0.97,0.95,0.99,0.97,0.81,1.10,0.95,
  1.08,0.75,0.97,0.94,0.95,0.85,1.02,1.07,1.11,1.02,
  1.16,1.05,0.88,1.29,0.99,0.83,1.06,1.05,1.03,1.15,
  1.14,0.91,1.17,1.17,0.70,0.84,1.33,0.95,1.34,1.14,
  1.08,1.28,1.14,0.84,1.09,1.12,0.99,1.08,0.95,1.12,
  0.70,1.07,1.06,1.24,1.17,0.96,1.00,0.85,1.25,0.84,
  1.00,1.01,1.08,1.08,0.75,1.06,1.09,0.86,1.08,1.11,
  1.19,1.25,0.94,0.97,0.81,0.91,0.99,0.94,0.94,1.12,
  1.10,0.82,1.17,0.89,0.75,0.84,1.10,0.93,1.11,1.07,
  1.20,1.22,0.90,1.01,1.17,0.97,1.02,0.77,1.07,0.87,
  0.96,1.21,1.04,0.89,0.95,0.86,0.84,1.03,1.26,0.71,
  0.75,1.34,1.14,1.09,0.85,0.68,0.79,1.06,1.12,1.10,
  0.98,1.11,0.98,0.86,1.15,0.89,1.21,0.79,1.31,0.84,
  1.05,0.89,1.01,0.81,1.10,1.04,1.07,1.02,0.91,1.00,
  0.98,0.95,0.82,1.11,1.07,1.07,0.89,1.15,1.04,1.19,
  1.07,1.03,1.04,0.81,0.93,1.06,0.92,1.29,1.04,0.99,
  0.86,0.84,1.22,1.09,1.14,0.76,0.86,0.96,1.07,1.21,
  0.93,1.07,1.13,0.95,0.92,1.01,1.01,1.16,1.24,0.82,
  1.03,1.31,0.79,1.14,1.15,0.97,0.82,1.10,1.06,1.00,
  0.58,0.79,1.08,1.06,1.06,1.14,1.09,1.01,0.87,0.97,
  1.08,1.03,1.08,0.92,1.26,0.74,0.98,0.94,0.84,1.07,
  1.04,0.93,0.90,0.83,0.91,1.15,1.00,0.86,0.87,0.75,
  1.19,1.13,1.14,0.78,1.03,0.95,0.77,0.89,0.90,0.96,
  0.95,1.08,1.03,0.98,1.00,0.86,1.16,1.05,0.97,0.91,
  1.06,1.12,0.97,1.03,0.88,1.02,0.98,0.95,0.86,1.14,
  1.15,1.26,0.84,0.82,0.91,1.02,0.96,0.95,1.17,1.03,
  0.89,1.07,1.06,1.09,0.95,0.99,1.38,1.10,0.88,1.05,
  1.18,0.93,1.14,0.86,1.24,0.97,1.04,1.12,1.10,1.14,
  1.22,0.89,0.93,1.30,0.93,1.00,1.06,0.92,1.26,0.75,
  1.24,1.17,1.09,1.06,0.78,0.95,0.97,0.90,0.98,0.88,
  0.87,1.00,0.83,1.11,1.15,1.20,1.21,1.00,0.95,0.82,
  1.16,0.80,0.99,0.89,1.17
)

channel1_df <- tibble(Length = channel1_vals)

# ----------------------------------------------------------------------
# 5. Apply rounding to Channel1 (traditional and round‑to‑even) ----------
# ----------------------------------------------------------------------
round_df <- channel1_df %>%
  mutate(
    Round = round_half_away(Length, 0.1),
    RoundEven = round_to_even(Length, 0.1),
    RoundUp = as.integer(Round > Length),
    RoundEvenUp = as.integer(RoundEven > Length)
  )

write.csv(round_df,
          file.path(out_dir, "Round.csv"),
          row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# 6. PROC MEANS equivalents (with SAS rounding rules) --------------------
# ----------------------------------------------------------------------
N_obs <- nrow(round_df)

# Helper to round to 3 decimal places using SAS's round‑half‑away rule
sas_round3 <- function(x) round_half_away(x, 0.001)

means_full <- tibble(
  Variable = c("Length", "Round", "RoundEven"),
  N = N_obs,
  Mean = c(sas_round3(mean(round_df$Length)),
           sas_round3(mean(round_df$Round)),
           sas_round3(mean(round_df$RoundEven))),
  Sum  = c(sas_round3(sum(round_df$Length)),
           sas_round3(sum(round_df$Round)),
           sas_round3(sum(round_df$RoundEven)))
)

means_up <- tibble(
  Variable = c("RoundUp", "RoundEvenUp"),
  Mean = c(sas_round3(mean(round_df$RoundUp)),
           sas_round3(mean(round_df$RoundEvenUp)))
)

# ----------------------------------------------------------------------
# 7. Create the combined listing (P096.lst) ----------------------------
# ----------------------------------------------------------------------
listing_lines <- character()

# Seq table
listing_lines <- c(listing_lines, "Seq - Comparison of ROUND vs ROUNDE")
listing_lines <- c(listing_lines, sprintf("%-6s %8s %10s", "x", "Round", "RoundEven"))
for (i in seq_len(nrow(seq_df))) {
  listing_lines <- c(listing_lines,
                     sprintf("%-6.2f %8.0f %10.0f",
                             seq_df$x[i],
                             seq_df$Round[i],
                             seq_df$RoundEven[i]))
}
listing_lines <- c(listing_lines, "")

# PROC MEANS – Length, Round, RoundEven
listing_lines <- c(listing_lines, "PROC MEANS - Length, Round, RoundEven")
listing_lines <- c(listing_lines,
                   sprintf("%-12s %5s %12s %12s", "Variable", "N", "Mean", "Sum"))
for (i in seq_len(nrow(means_full))) {
  listing_lines <- c(listing_lines,
                     sprintf("%-12s %5d %12.3f %12.3f",
                             means_full$Variable[i],
                             means_full$N[i],
                             means_full$Mean[i],
                             means_full$Sum[i]))
}
listing_lines <- c(listing_lines, "")

# PROC MEANS – Proportion rounded up
listing_lines <- c(listing_lines, "PROC MEANS - Proportion rounded up")
listing_lines <- c(listing_lines,
                   sprintf("%-12s %12s", "Variable", "Mean"))
for (i in seq_len(nrow(means_up))) {
  listing_lines <- c(listing_lines,
                     sprintf("%-12s %12.3f",
                             means_up$Variable[i],
                             means_up$Mean[i]))
}
listing_lines <- c(listing_lines, "")

# Write the listing text file
writeLines(listing_lines,
           con = file.path(out_dir, "P096.lst"))

# CSV versions of the tables (for completeness)
write.csv(seq_df, file.path(out_dir, "Seq_table.csv"),
          row.names = FALSE, na = "")
write.csv(means_full, file.path(out_dir, "Means_full_table.csv"),
          row.names = FALSE, na = "")
write.csv(means_up, file.path(out_dir, "Means_up_table.csv"),
          row.names = FALSE, na = "")

# ----------------------------------------------------------------------
# End of script
# ----------------------------------------------------------------------
