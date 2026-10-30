out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers: SAS semantics
round_sas <- function(x, unit = 1) {
  # traditional round half away from zero, SAS ROUND
  # vectorized
  # add small epsilon to handle binary floating representation
  # SAS: round(x, unit) = round(x/unit)*unit with half away from zero
  z <- x / unit
  # for positive and negative, away from zero
  # use floor(abs(z)+0.5+1e-9)
  res <- sign(z) * floor(abs(z) + 0.5 + 1e-9) * unit
  # clean floating noise: round to 10 decimal places then to decimals implied by unit
  # determine decimals from unit if unit is power of 10
  # generic clean
  res <- round(res, 10)
  res
}

rounde_sas <- function(x, unit = 1) {
  # round half to even, SAS ROUNDE, uses R's round (bankers)
  z <- x / unit
  res <- round(z) * unit
  res <- round(res, 10)
  res
}

# ------------------------------------------------------------
# Create step plot data of round-half-to-even function
# data Series;
x_series <- seq(0, 6, by = 0.05)
# ensure exactly as SAS: 0 to 6 by 0.05 inclusive
y_series <- rounde_sas(x_series, 1)
group_series <- y_series
Series <- data.frame(x = x_series, y = y_series, group = group_series)

# data Scatter1;
x1_vals <- seq(0.5, 5.5, by = 1)
y1_vals <- rounde_sas(x1_vals, 1)
Scatter1 <- data.frame(x1 = x1_vals, y1 = y1_vals)

# data Scatter2;
x2_vals <- seq(0.5, 5.5, by = 1)
y2_vals <- rounde_sas(x2_vals + 1, 1) - 1
Scatter2 <- data.frame(x2 = x2_vals, y2 = y2_vals)

# data RoundToEven; set Series Scatter1 Scatter2;
# In SAS this creates a dataset with all variables, missing where not present
# For plotting we keep separate data frames, but also create combined for CSV
max_n <- max(nrow(Series), nrow(Scatter1), nrow(Scatter2))
RoundToEven <- data.frame(
  x = c(Series$x, rep(NA, max_n - nrow(Series))),
  y = c(Series$y, rep(NA, max_n - nrow(Series))),
  group = c(Series$group, rep(NA, max_n - nrow(Series))),
  x1 = c(Scatter1$x1, rep(NA, max_n - nrow(Scatter1))),
  y1 = c(Scatter1$y1, rep(NA, max_n - nrow(Scatter1))),
  x2 = c(Scatter2$x2, rep(NA, max_n - nrow(Scatter2))),
  y2 = c(Scatter2$y2, rep(NA, max_n - nrow(Scatter2)))
)
# For CSV of plotted data, also create long version that matches SAS RoundToEven (stacked)
# SAS set stacks observations: first Series rows, then Scatter1 rows, then Scatter2 rows
RoundToEven_stacked <- data.frame(
  x = c(Series$x, rep(NA, nrow(Scatter1) + nrow(Scatter2))),
  y = c(Series$y, rep(NA, nrow(Scatter1) + nrow(Scatter2))),
  group = c(Series$group, rep(NA, nrow(Scatter1) + nrow(Scatter2))),
  x1 = c(rep(NA, nrow(Series)), Scatter1$x1, rep(NA, nrow(Scatter2))),
  y1 = c(rep(NA, nrow(Series)), Scatter1$y1, rep(NA, nrow(Scatter2))),
  x2 = c(rep(NA, nrow(Series) + nrow(Scatter1)), Scatter2$x2),
  y2 = c(rep(NA, nrow(Series) + nrow(Scatter1)), Scatter2$y2)
)

# ------------------------------------------------------------
# Plot: proc sgplot
library(ggplot2)

# Prepare data for ggplot
# series grouped by group: need to ensure breaks at half integers are not connected
# In SAS, group= y creates separate lines per y level, so gaps at jumps
# Replicate by using group aesthetic
p <- ggplot() +
  geom_line(data = Series, aes(x = x, y = y, group = factor(group)), color = "#445694", linewidth = 0.9) +
  geom_point(data = Scatter1, aes(x = x1, y = y1), shape = 21, size = 2.5, fill = "#445694", colour = "#445694", stroke = 0.8) +
  geom_point(data = Scatter2, aes(x = x2, y = y2), shape = 21, size = 2.5, fill = "white", colour = "#445694", stroke = 0.8) +
  labs(title = "The Round-To-Even Function", x = "x", y = "RoundE(x)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_line(colour = "grey92")
  ) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6)

# Save figure
ggsave(filename = file.path(out, "sgplot.png"), plot = p, width = 7, height = 5, dpi = 300, units = "in")
# also save as expected name SGPlot.png variant
ggsave(filename = file.path(out, "SGPlot.png"), plot = p, width = 7, height = 5, dpi = 300, units = "in")
# CSV of plotted data
write.csv(RoundToEven_stacked, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(RoundToEven_stacked, file.path(out, "SGPlot.csv"), row.names = FALSE, na = "")
# also write RoundToEven dataset as CSV for completeness
write.csv(RoundToEven_stacked, file.path(out, "RoundToEven.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Create table that compares ROUND and ROUNDE
x_seq <- seq(0, 3.5, by = 0.25)
Round_seq <- round_sas(x_seq, 1)
RoundEven_seq <- rounde_sas(x_seq, 1)
Seq <- data.frame(
  x = x_seq,
  Round = Round_seq,
  RoundEven = RoundEven_seq
)
# labels: Round = "Round(x)" RoundEven="RoundE(x)"
# Save dataset
write.csv(Seq, file.path(out, "Seq.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Channel1 data
Channel1_vals <- c(
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
Channel1 <- data.frame(Length = Channel1_vals)
write.csv(Channel1, file.path(out, "Channel1.csv"), row.names = FALSE, na = "")

# round real data to nearest 0.1
Round_df <- Channel1
Round_df$Round <- round_sas(Round_df$Length, 0.1)
Round_df$RoundEven <- rounde_sas(Round_df$Length, 0.1)
Round_df$RoundUp <- as.integer(Round_df$Round > Round_df$Length)
Round_df$RoundEvenUp <- as.integer(Round_df$RoundEven > Round_df$Length)
write.csv(Round_df, file.path(out, "Round.csv"), row.names = FALSE, na = "")

# compare estimates that use full data and rounded data
# proc means sum mean ndec=3 var Length Round RoundEven
vars1 <- c("Length", "Round", "RoundEven")
means1_sum <- sapply(Round_df[vars1], sum, na.rm = TRUE)
means1_mean <- sapply(Round_df[vars1], mean, na.rm = TRUE)
means1_n <- sapply(Round_df[vars1], function(v) sum(!is.na(v)))
means1_df <- data.frame(
  Variable = vars1,
  N = means1_n,
  Sum = means1_sum,
  Mean = means1_mean,
  stringsAsFactors = FALSE
)
# format to 3 decimals for display
means1_df$Sum_fmt <- sprintf("%.3f", means1_df$Sum)
means1_df$Mean_fmt <- sprintf("%.3f", means1_df$Mean)

# how many observations were rounded up or down?
# proc means mean ndec=3 var RoundUp RoundEvenUp
vars2 <- c("RoundUp", "RoundEvenUp")
means2_mean <- sapply(Round_df[vars2], mean, na.rm = TRUE)
means2_n <- sapply(Round_df[vars2], function(v) sum(!is.na(v)))
means2_df <- data.frame(
  Variable = vars2,
  N = means2_n,
  Mean = means2_mean,
  stringsAsFactors = FALSE
)
means2_df$Mean_fmt <- sprintf("%.3f", means2_df$Mean)
means2_df$Label <- c("Proportion rounded up for ROUND", "Proportion rounded up for ROUNDE")

write.csv(means1_df, file.path(out, "means1.csv"), row.names = FALSE, na = "")
write.csv(means2_df, file.path(out, "means2.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Build P096.lst text rendering (53 numbers)
# This includes Seq print + means tables
# Create text file with SAS-like proc print and proc means output

txt_lines <- c(
  "The Round-To-Even Function",
  "",
  "Comparison of ROUND and ROUNDE (Round-To-Even) Functions",
  "-------------------------------------------------------",
  ""
)

# Seq table header
txt_lines <- c(txt_lines,
  sprintf("%-10s %-12s %-12s", "x", "Round(x)", "RoundE(x)"),
  sprintf("%-10s %-12s %-12s", "----------", "----------", "----------")
)
for (i in seq_len(nrow(Seq))) {
  txt_lines <- c(txt_lines,
    sprintf("%-10g %-12g %-12g", Seq$x[i], Seq$Round[i], Seq$RoundEven[i])
  )
}
txt_lines <- c(txt_lines, "", "")

# Means 1
txt_lines <- c(txt_lines,
  "Comparison of estimates that use full data and rounded data",
  "PROC MEANS data=Round sum mean ndec=3;",
  "Label Length=\"True values\" Round=\"Rounded values\" RoundEven=\"Round-to-even values\";",
  ""
)
txt_lines <- c(txt_lines,
  sprintf("%-20s %6s %12s %12s", "Variable", "N", "Sum", "Mean"),
  sprintf("%-20s %6s %12s %12s", "--------------------", "------", "------------", "------------")
)
for (i in seq_len(nrow(means1_df))) {
  lab <- c("True values", "Rounded values", "Round-to-even values")[i]
  txt_lines <- c(txt_lines,
    sprintf("%-20s %6d %12s %12s", paste0(means1_df$Variable[i], " (", lab, ")"), means1_df$N[i], means1_df$Sum_fmt[i], means1_df$Mean_fmt[i])
  )
}
txt_lines <- c(txt_lines, "", "")

# Means 2
txt_lines <- c(txt_lines,
  "Proportion rounded up",
  "PROC MEANS data=Round mean ndec=3;",
  ""
)
txt_lines <- c(txt_lines,
  sprintf("%-40s %6s %12s", "Variable", "N", "Mean"),
  sprintf("%-40s %6s %12s", "----------------------------------------", "------", "------------")
)
for (i in seq_len(nrow(means2_df))) {
  txt_lines <- c(txt_lines,
    sprintf("%-40s %6d %12s", means2_df$Label[i], means2_df$N[i], means2_df$Mean_fmt[i])
  )
}
txt_lines <- c(txt_lines, "")

# Write text files
writeLines(txt_lines, file.path(out, "P096.txt"))
writeLines(txt_lines, file.path(out, "P096.lst.txt"))
writeLines(txt_lines, file.path(out, "P096.lst"))

# CSV of table body: combine Seq and means for 53 numbers requirement
# Create P096.csv that contains Seq plus means summaries
# For validation, ensure CSV contains all numbers
P096_csv <- Seq
# add label columns for means as extra rows? Instead create separate CSV that mirrors text
# Write Seq as main P096.csv
write.csv(Seq, file.path(out, "P096.csv"), row.names = FALSE, na = "")
# also write P096.lst.csv
write.csv(Seq, file.path(out, "P096.lst.csv"), row.names = FALSE, na = "")

# Additional combined table CSV for completeness (contains all 53 numbers in long form)
combined_numbers <- data.frame(
  Source = c(rep("Seq_x", nrow(Seq)), rep("Seq_Round", nrow(Seq)), rep("Seq_RoundEven", nrow(Seq)),
             paste0("Means1_Sum_", means1_df$Variable), paste0("Means1_Mean_", means1_df$Variable),
             paste0("Means2_Mean_", means2_df$Variable)),
  Value = c(Seq$x, Seq$Round, Seq$RoundEven, means1_df$Sum, means1_df$Mean, means2_df$Mean)
)
write.csv(combined_numbers, file.path(out, "P096_all_numbers.csv"), row.names = FALSE, na = "")

# Also ensure figure CSV is present under expected name
# Already created sgplot.csv
