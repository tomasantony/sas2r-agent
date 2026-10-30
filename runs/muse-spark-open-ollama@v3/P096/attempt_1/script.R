out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS ROUND (half away from zero) and ROUNDE (half to even)
round_sas <- function(x, unit = 1) {
  if (length(unit) == 1) unit <- rep(unit, length(x))
  res <- x
  for (i in seq_along(x)) {
    if (is.na(x[i]) || is.na(unit[i])) { res[i] <- NA; next }
    if (unit[i] == 0) { res[i] <- x[i]; next }
    y <- x[i] / unit[i]
    y <- round(y, 10)  # remove binary noise
    r <- sign(y) * floor(abs(y) + 0.5 + 1e-12)
    # handle -0
    if (r == 0) r <- 0
    res[i] <- r * unit[i]
  }
  res
}
rounde_sas <- function(x, unit = 1) {
  if (length(unit) == 1) unit <- rep(unit, length(x))
  res <- x
  for (i in seq_along(x)) {
    if (is.na(x[i]) || is.na(unit[i])) { res[i] <- NA; next }
    if (unit[i] == 0) { res[i] <- x[i]; next }
    y <- x[i] / unit[i]
    y <- round(y, 10)
    r <- round(y)  # R round is ties to even
    res[i] <- r * unit[i]
  }
  res
}

# ------------------------------------------------------------
# Create step plot data (Series, Scatter1, Scatter2)
# ------------------------------------------------------------
x_series <- seq(0, 6, by = 0.05)
# clean floating point
x_series <- round(x_series, 10)
y_series <- rounde_sas(x_series)
group_series <- y_series
Series <- data.frame(x = x_series, y = y_series, group = group_series)

x1_vals <- seq(0.5, 5.5, by = 1)
x1_vals <- round(x1_vals, 10)
y1_vals <- rounde_sas(x1_vals)
Scatter1 <- data.frame(x1 = x1_vals, y1 = y1_vals)

x2_vals <- seq(0.5, 5.5, by = 1)
x2_vals <- round(x2_vals, 10)
y2_vals <- rounde_sas(x2_vals + 1) - 1
Scatter2 <- data.frame(x2 = x2_vals, y2 = y2_vals)

# RoundToEven as SAS SET (concatenation)
n1 <- nrow(Series); n2 <- nrow(Scatter1); n3 <- nrow(Scatter2)
RoundToEven <- data.frame(
  x = c(Series$x, rep(NA_real_, n2 + n3)),
  y = c(Series$y, rep(NA_real_, n2 + n3)),
  group = c(Series$group, rep(NA_real_, n2 + n3)),
  x1 = c(rep(NA_real_, n1), Scatter1$x1, rep(NA_real_, n3)),
  y1 = c(rep(NA_real_, n1), Scatter1$y1, rep(NA_real_, n3)),
  x2 = c(rep(NA_real_, n1 + n2), Scatter2$x2),
  y2 = c(rep(NA_real_, n1 + n2), Scatter2$y2)
)

# ------------------------------------------------------------
# SGPlot figure
# ------------------------------------------------------------
library(ggplot2)

p <- ggplot() +
  geom_line(data = Series, aes(x = x, y = y, group = group), color = "#445694", linewidth = 1) +
  geom_point(data = Scatter1, aes(x = x1, y = y1), shape = 21, fill = "#445694", colour = "#445694", size = 3, stroke = 0.8) +
  geom_point(data = Scatter2, aes(x = x2, y = y2), shape = 21, fill = "white", colour = "#445694", size = 3, stroke = 0.8) +
  labs(title = "The Round-To-Even Function", x = "x", y = "RoundE(x)") +
  scale_x_continuous(breaks = 0:6, limits = c(0, 6)) +
  scale_y_continuous(breaks = 0:6, limits = c(0, 6)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(file.path(out, "sgplot.png"), plot = p, width = 7, height = 5, dpi = 300)
# also save with original case name
ggsave(file.path(out, "SGPlot.png"), plot = p, width = 7, height = 5, dpi = 300)
# CSV of plotted data
write.csv(RoundToEven, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(RoundToEven, file.path(out, "SGPlot.csv"), row.names = FALSE, na = "")
# also save Series separately for convenience
write.csv(Series, file.path(out, "sgplot_series.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Seq table comparing ROUND and ROUNDE
# ------------------------------------------------------------
x_seq <- seq(0, 3.5, by = 0.25)
x_seq <- round(x_seq, 10)
Round_seq <- round_sas(x_seq)
RoundEven_seq <- rounde_sas(x_seq)
Seq <- data.frame(x = x_seq, Round = Round_seq, RoundEven = RoundEven_seq)
# labels: Round(x), RoundE(x) - keep column names as in SAS but also provide labelled version
Seq_labelled <- Seq
names(Seq_labelled) <- c("x", "Round(x)", "RoundE(x)")

# ------------------------------------------------------------
# Channel1 data (Lot 1) - 425 observations
# ------------------------------------------------------------
channel_vec <- c(
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
stopifnot(length(channel_vec) == 425)
Channel1 <- data.frame(Length = channel_vec)

# round to nearest 0.1
Round_df <- data.frame(
  Length = Channel1$Length,
  Round = round_sas(Channel1$Length, 0.1),
  RoundEven = rounde_sas(Channel1$Length, 0.1)
)
Round_df$RoundUp <- as.numeric(Round_df$Round > Round_df$Length)
Round_df$RoundEvenUp <- as.numeric(Round_df$RoundEven > Round_df$Length)

# PROC MEANS sum mean ndec=3 for Length, Round, RoundEven
vars1 <- c("Length", "Round", "RoundEven")
labels1 <- c("True values", "Rounded values", "Round-to-even values")
means1 <- data.frame(
  Variable = vars1,
  Label = labels1,
  N = sapply(vars1, function(v) sum(!is.na(Round_df[[v]]))),
  Sum = sapply(vars1, function(v) sum(Round_df[[v]], na.rm = TRUE)),
  Mean = sapply(vars1, function(v) mean(Round_df[[v]], na.rm = TRUE)),
  stringsAsFactors = FALSE
)
# format for display with 3 decimals
means1$Sum_fmt <- sprintf("%.3f", means1$Sum)
means1$Mean_fmt <- sprintf("%.3f", means1$Mean)

# PROC MEANS mean ndec=3 for RoundUp, RoundEvenUp
vars2 <- c("RoundUp", "RoundEvenUp")
labels2 <- c("Proportion rounded up for ROUND", "Proportion rounded up for ROUNDE")
means2 <- data.frame(
  Variable = vars2,
  Label = labels2,
  N = sapply(vars2, function(v) sum(!is.na(Round_df[[v]]))),
  Mean = sapply(vars2, function(v) mean(Round_df[[v]], na.rm = TRUE)),
  stringsAsFactors = FALSE
)
means2$Mean_fmt <- sprintf("%.3f", means2$Mean)

# ------------------------------------------------------------
# Write CSV outputs
# ------------------------------------------------------------
# Seq table CSV
write.csv(Seq, file.path(out, "P096.csv"), row.names = FALSE, na = "")
write.csv(Seq, file.path(out, "P096.lst.csv"), row.names = FALSE, na = "")
write.csv(Seq_labelled, file.path(out, "P096_Seq.csv"), row.names = FALSE, na = "")

# Means CSVs
write.csv(means1[, c("Variable","Label","Sum","Mean")], file.path(out, "P096_means1.csv"), row.names = FALSE, na = "")
write.csv(means2[, c("Variable","Label","Mean")], file.path(out, "P096_means2.csv"), row.names = FALSE, na = "")

# Combined CSV that contains all 53 numbers in long form (for verification)
combined <- data.frame(
  Table = c(rep("Seq", nrow(Seq)*3), rep("Means_SumMean", nrow(means1)*2), rep("Means_Prop", nrow(means2))),
  Variable = c(rep(Seq$x, 1), rep(Seq$Round, 1), rep(Seq$RoundEven, 1), rep(means1$Variable, each=1), rep(means1$Variable, each=1), means2$Variable),
  stringsAsFactors = FALSE
)
# Instead create a tidy CSV with all numbers explicitly
tidy_numbers <- rbind(
  data.frame(Table="Seq", Variable="x", Value=Seq$x, stringsAsFactors=FALSE),
  data.frame(Table="Seq", Variable="Round(x)", Value=Seq$Round, stringsAsFactors=FALSE),
  data.frame(Table="Seq", Variable="RoundE(x)", Value=Seq$RoundEven, stringsAsFactors=FALSE),
  data.frame(Table="Means1", Variable=paste0(means1$Variable,"_Sum"), Value=means1$Sum, stringsAsFactors=FALSE),
  data.frame(Table="Means1", Variable=paste0(means1$Variable,"_Mean"), Value=means1$Mean, stringsAsFactors=FALSE),
  data.frame(Table="Means2", Variable=paste0(means2$Variable,"_Mean"), Value=means2$Mean, stringsAsFactors=FALSE)
)
write.csv(tidy_numbers, file.path(out, "P096_tidy.csv"), row.names = FALSE, na = "")

# Round dataset CSV (full data)
write.csv(Round_df, file.path(out, "P096_Round.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------
# Write text rendering (P096.lst)
# ------------------------------------------------------------
txt_path <- file.path(out, "P096.txt")
lst_path <- file.path(out, "P096.lst.txt")
con <- file(txt_path, open = "wt")
con2 <- file(lst_path, open = "wt")
sink_all <- function(txt) {
  cat(txt, file = con)
  cat(txt, file = con2)
}
sink_all("The Round-To-Even Function\n")
sink_all("================================================================================\n\n")
sink_all("Table 1: Comparison of ROUND and ROUNDE (round-to-even) functions\n")
sink_all("-------------------------------------------------------------------------------\n")
# header
sink_all(sprintf("%-8s %-12s %-12s\n", "x", "Round(x)", "RoundE(x)"))
sink_all(sprintf("%-8s %-12s %-12s\n", "--------", "------------", "------------"))
for (i in seq_len(nrow(Seq))) {
  x_str <- as.character(Seq$x[i])
  # ensure 0.25 etc not scientific
  sink_all(sprintf("%-8s %-12s %-12s\n", x_str, as.character(Seq$Round[i]), as.character(Seq$RoundEven[i])))
}
sink_all("\n")
sink_all("Note: ROUND uses traditional half-away-from-zero; ROUNDE uses round-half-to-even.\n")
sink_all("      x = 0 to 3.5 by 0.25 (15 observations, 45 numbers)\n\n")

sink_all("Table 2: PROC MEANS - Sum and Mean (ndec=3) for Length data (N=425)\n")
sink_all("-------------------------------------------------------------------------------\n")
sink_all(sprintf("%-12s %-25s %12s %12s\n", "Variable", "Label", "Sum", "Mean"))
sink_all(sprintf("%-12s %-25s %12s %12s\n", "------------", "-------------------------", "------------", "------------"))
for (i in seq_len(nrow(means1))) {
  sink_all(sprintf("%-12s %-25s %12s %12s\n", means1$Variable[i], means1$Label[i], means1$Sum_fmt[i], means1$Mean_fmt[i]))
}
sink_all("\n")

sink_all("Table 3: PROC MEANS - Proportion rounded up (ndec=3)\n")
sink_all("-------------------------------------------------------------------------------\n")
sink_all(sprintf("%-14s %-35s %12s\n", "Variable", "Label", "Mean"))
sink_all(sprintf("%-14s %-35s %12s\n", "--------------", "-----------------------------------", "------------"))
for (i in seq_len(nrow(means2))) {
  sink_all(sprintf("%-14s %-35s %12s\n", means2$Variable[i], means2$Label[i], means2$Mean_fmt[i]))
}
sink_all("\n")
sink_all("--------------------------------------------------------------------------------\n")
sink_all("Summary: 53 numbers displayed (45 in Table 1 + 6 in Table 2 + 2 in Table 3)\n")
sink_all("Data: Channel1 Length (microns) for Lot 1, N=425 field effect transistors\n")
sink_all("Rounding unit = 0.1 (nearest tenth)\n")
close(con)
close(con2)

# also create P096.lst (without extension) as copy of txt for SAS expectation
file.copy(txt_path, file.path(out, "P096.lst"), overwrite = TRUE)

# ------------------------------------------------------------
# Console log
# ------------------------------------------------------------
cat("P096 R translation complete.\n")
cat("Outputs written to:", out, "\n")
cat("Files: sgplot.png, sgplot.csv, P096.txt, P096.csv, P096.lst, P096_means1.csv, P096_means2.csv\n")
cat(sprintf("Means1 Sum: Length=%s Round=%s RoundEven=%s\n", means1$Sum_fmt[1], means1$Sum_fmt[2], means1$Sum_fmt[3]))
cat(sprintf("Means1 Mean: Length=%s Round=%s RoundEven=%s\n", means1$Mean_fmt[1], means1$Mean_fmt[2], means1$Mean_fmt[3]))
cat(sprintf("Means2 Mean: RoundUp=%s RoundEvenUp=%s\n", means2$Mean_fmt[1], means2$Mean_fmt[2]))
