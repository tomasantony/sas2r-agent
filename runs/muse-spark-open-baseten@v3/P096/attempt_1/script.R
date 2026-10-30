out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helpers: SAS rounding semantics
# SAS ROUND: half away from zero
sas_round <- function(x, unit = 1) {
  if (unit == 0) return(x)
  y <- x / unit
  # away from zero: sign(y) * floor(abs(y) + 0.5 + 1e-9)
  # add small epsilon to handle floating representation like 2.4999999999
  eps <- 1e-9
  res <- sign(y) * floor(abs(y) + 0.5 + eps)
  # preserve sign of zero
  res[y == 0] <- 0
  res * unit
}

# SAS ROUNDE: round half to even (banker's)
sas_rounde <- function(x, unit = 1) {
  if (unit == 0) return(x)
  y <- x / unit
  eps <- 1e-9
  # vectorized
  res <- numeric(length(y))
  for (i in seq_along(y)) {
    yi <- y[i]
    if (is.na(yi)) { res[i] <- NA; next }
    if (yi == 0) { res[i] <- 0; next }
    s <- sign(yi)
    a <- abs(yi)
    fl <- floor(a)
    frac <- a - fl
    if (abs(frac - 0.5) < eps) {
      # tie: choose even
      if (fl %% 2 == 0) {
        r <- fl
      } else {
        r <- fl + 1
      }
    } else if (frac < 0.5) {
      r <- fl
    } else {
      r <- fl + 1
    }
    res[i] <- s * r
  }
  res * unit
}

# ------------------------------------------------------------
# Create step plot data for round-half-to-even function
# Series: x 0 to 6 by 0.05
x_seq <- seq(0, 6, by = 0.05)
# to avoid floating drift, round to 2 decimals
x_seq <- round(x_seq, 10)
Series <- data.frame(
  x = x_seq,
  y = sas_rounde(x_seq),
  group = sas_rounde(x_seq)
)

Scatter1 <- data.frame(
  x1 = seq(0.5, 5.5, by = 1),
  y1 = sas_rounde(seq(0.5, 5.5, by = 1))
)

Scatter2 <- data.frame(
  x2 = seq(0.5, 5.5, by = 1),
  y2 = sas_rounde(seq(0.5, 5.5, by = 1) + 1) - 1
)

RoundToEven <- dplyr::bind_rows(
  Series,
  data.frame(x = Scatter1$x1, y = Scatter1$y1, group = Scatter1$y1, x1 = Scatter1$x1, y1 = Scatter1$y1),
  data.frame(x = Scatter2$x2, y = Scatter2$y2, group = Scatter2$y2, x2 = Scatter2$x2, y2 = Scatter2$y2)
)

# For plotting we keep original Series + scatters separately
# Save plotted data CSV
write.csv(RoundToEven, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(RoundToEven, file.path(out, "RoundToEven.csv"), row.names = FALSE, na = "")

# SGPlot reproduction with ggplot2
library(ggplot2)
p <- ggplot() +
  geom_line(data = Series, aes(x = x, y = y, group = group), color = "#445694", linewidth = 0.9) +
  geom_point(data = Scatter1, aes(x = x1, y = y1), shape = 21, size = 2.8,
             fill = "#445694", colour = "#445694", stroke = 0.8) +
  geom_point(data = Scatter2, aes(x = x2, y = y2), shape = 21, size = 2.8,
             fill = "white", colour = "#445694", stroke = 0.8) +
  labs(title = "The Round-To-Even Function", x = "x", y = "RoundE(x)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_line(colour = "grey92"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6)

ggsave(file.path(out, "sgplot.png"), plot = p, width = 7, height = 5, dpi = 150)
# also save as SGPlot.png for case-sensitive check
ggsave(file.path(out, "SGPlot.png"), plot = p, width = 7, height = 5, dpi = 150)

# ------------------------------------------------------------
# Create table that compares ROUND and ROUNDE
x2 <- seq(0, 3.5, by = 0.25)
x2 <- round(x2, 10)
Seq <- data.frame(
  x = x2,
  Round = sas_round(x2),
  RoundEven = sas_rounde(x2)
)
# labels: Round(x) and RoundE(x)
# Save dataset
write.csv(Seq, file.path(out, "Seq.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Channel1 data from PROC UNIVARIATE example
chan_vals <- c(
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
Channel1 <- data.frame(Length = chan_vals)
write.csv(Channel1, file.path(out, "Channel1.csv"), row.names = FALSE, na = "")

# round real data to nearest 0.1
Round <- Channel1
Round$Round <- sas_round(Round$Length, 0.1)
Round$RoundEven <- sas_rounde(Round$Length, 0.1)
Round$RoundUp <- as.integer(Round$Round > Round$Length)
Round$RoundEvenUp <- as.integer(Round$RoundEven > Round$Length)
write.csv(Round, file.path(out, "Round.csv"), row.names = FALSE, na = "")

# PROC MEANS sum mean ndec=3 for Length Round RoundEven
vars1 <- c("Length", "Round", "RoundEven")
means1 <- data.frame(
  Variable = vars1,
  Label = c("True values", "Rounded values", "Round-to-even values"),
  N = sapply(Round[vars1], function(v) sum(!is.na(v))),
  Sum = sapply(Round[vars1], function(v) sum(v, na.rm = TRUE)),
  Mean = sapply(Round[vars1], function(v) mean(v, na.rm = TRUE))
)
# format to 3 decimals for display
means1$Sum_fmt <- sprintf("%.3f", means1$Sum)
means1$Mean_fmt <- sprintf("%.3f", means1$Mean)

# PROC MEANS mean ndec=3 for RoundUp RoundEvenUp
vars2 <- c("RoundUp", "RoundEvenUp")
means2 <- data.frame(
  Variable = vars2,
  Label = c("Proportion rounded up for ROUND", "Proportion rounded up for ROUNDE"),
  N = sapply(Round[vars2], function(v) sum(!is.na(v))),
  Mean = sapply(Round[vars2], function(v) mean(v, na.rm = TRUE))
)
means2$Mean_fmt <- sprintf("%.3f", means2$Mean)

write.csv(means1, file.path(out, "means1.csv"), row.names = FALSE, na = "")
write.csv(means2, file.path(out, "means2.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Build P096.lst text rendering (53 numbers)
# Count: Seq 15*3=45 + means1 3*2=6 + means2 2*1=2 =53

txt_path <- file.path(out, "P096.txt")
lst_path <- file.path(out, "P096.lst.txt")
csv_path <- file.path(out, "P096.csv")
# also create P096.lst.csv
lst_csv_path <- file.path(out, "P096.lst.csv")

# Create CSV of the same table: combine Seq and means
# For simplicity, create a CSV that contains Seq rows and means rows
# First, Seq part
seq_csv <- Seq
names(seq_csv) <- c("x", "Round(x)", "RoundE(x)")
# Means part as separate rows appended with NA padding
# Build a unified CSV with columns: Table, x, Round, RoundEven, Variable, Sum, Mean
# Instead, write a simple CSV that contains all numbers in long form for verification
p096_long <- data.frame(
  Table = c(rep("Seq", nrow(Seq)), rep("Means_SumMean", nrow(means1)), rep("Means_Prop", nrow(means2))),
  Variable = c(rep(NA, nrow(Seq)), means1$Variable, means2$Variable),
  x = c(Seq$x, rep(NA, nrow(means1)+nrow(means2))),
  Round = c(Seq$Round, rep(NA, nrow(means1)+nrow(means2))),
  RoundEven = c(Seq$RoundEven, rep(NA, nrow(means1)+nrow(means2))),
  Sum = c(rep(NA, nrow(Seq)), means1$Sum, rep(NA, nrow(means2))),
  Mean = c(rep(NA, nrow(Seq)), means1$Mean, means2$Mean)
)
write.csv(p096_long, csv_path, row.names = FALSE, na = "")
write.csv(p096_long, lst_csv_path, row.names = FALSE, na = "")
write.csv(seq_csv, file.path(out, "P096_Seq.csv"), row.names = FALSE, na = "")

# Text rendering
con <- file(txt_path, open = "wt")
cat("The Round-To-Even Function\n", file = con)
cat("\n", file = con)
cat("Comparison of ROUND and ROUNDE (round-to-even) functions\n", file = con)
cat("--------------------------------------------------------\n", file = con)
cat(sprintf("%-10s %-12s %-12s\n", "x", "Round(x)", "RoundE(x)"), file = con)
cat(paste(rep("-", 34), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(Seq))) {
  # SAS prints x with default BEST: 0, 0.25 etc.
  # Format x: remove trailing zeros
  x_fmt <- format(Seq$x[i], nsmall = 2, trim = TRUE, scientific = FALSE)
  # Actually format to show 0, 0.25, 0.5 etc. Use gsub to trim
  x_fmt <- sub("\\.?0+$", "", format(Seq$x[i], nsmall = 2, trim = TRUE))
  if (x_fmt == "") x_fmt <- "0"
  # But for 0.25 keep 0.25, for 0.5 keep 0.5
  # Use sprintf with %g
  x_fmt <- format(Seq$x[i], trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  cat(sprintf("%-10s %-12.0f %-12.0f\n", x_fmt, Seq$Round[i], Seq$RoundEven[i]), file = con)
}
cat("\n", file = con)
cat("Lengths of circuit boards for Lot 1 - Rounded to nearest 0.1\n", file = con)
cat("------------------------------------------------------------\n", file = con)
cat("PROC MEANS: Sum and Mean (ndec=3)\n", file = con)
cat(sprintf("%-25s %10s %10s %10s\n", "Variable", "N", "Sum", "Mean"), file = con)
cat(paste(rep("-", 60), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(means1))) {
  cat(sprintf("%-25s %10d %10s %10s\n",
              paste0(means1$Variable[i], " (", means1$Label[i], ")"),
              means1$N[i], means1$Sum_fmt[i], means1$Mean_fmt[i]), file = con)
}
cat("\n", file = con)
cat("PROC MEANS: Proportion rounded up (ndec=3)\n", file = con)
cat(sprintf("%-40s %10s %10s\n", "Variable", "N", "Mean"), file = con)
cat(paste(rep("-", 65), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(means2))) {
  cat(sprintf("%-40s %10d %10s\n",
              means2$Variable[i],
              means2$N[i], means2$Mean_fmt[i]), file = con)
  cat(sprintf("  Label: %s\n", means2$Label[i]), file = con)
}
cat("\n", file = con)
cat("Note: ROUND = traditional half away from zero; ROUNDE = round half to even.\n", file = con)
close(con)
# duplicate for .lst.txt
file.copy(txt_path, lst_path, overwrite = TRUE)
# also create P096.lst
file.copy(txt_path, file.path(out, "P096.lst"), overwrite = TRUE)

# Ensure at least one figure CSV exists
# sgplot.csv already written
