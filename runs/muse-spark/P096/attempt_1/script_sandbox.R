out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---- SAS ROUND / ROUNDE helpers (half away vs half to even) ----
sas_round <- function(x, unit = 1) {
  # SAS ROUND: round to nearest multiple of unit, ties away from zero
  res <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    xi <- x[i]; ui <- if (length(unit) == 1) unit else unit[i]
    if (is.na(xi) || is.na(ui)) { res[i] <- NA_real_; next }
    if (ui == 0) { res[i] <- NA_real_; next }
    y <- xi / ui
    if (y == 0) { res[i] <- 0; next }
    s <- sign(y)
    ay <- abs(y)
    f <- floor(ay + 1e-12)
    diff <- ay - f
    eps <- 1e-7
    if (abs(diff - 0.5) < eps) {
      r <- f + 1
    } else if (diff < 0.5) {
      r <- f
    } else {
      r <- f + 1
    }
    res[i] <- s * r * ui
  }
  res
}

sas_rounde <- function(x, unit = 1) {
  # SAS ROUNDE: round to nearest multiple of unit, ties to even multiple
  res <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    xi <- x[i]; ui <- if (length(unit) == 1) unit else unit[i]
    if (is.na(xi) || is.na(ui)) { res[i] <- NA_real_; next }
    if (ui == 0) { res[i] <- NA_real_; next }
    y <- xi / ui
    fl <- floor(y + 1e-12)
    diff <- y - fl
    eps <- 1e-7
    if (abs(diff - 0.5) < eps) {
      # tie: choose even
      # fl %% 2 works for negatives in R (even => 0)
      if (fl %% 2 == 0) {
        r <- fl
      } else {
        r <- fl + 1
      }
    } else if (diff < 0.5) {
      r <- fl
    } else {
      r <- fl + 1
    }
    res[i] <- r * ui
  }
  res
}

fmt3 <- function(x) {
  # SAS NDEC=3 formatting: round half away then show 3 decimals
  ri <- sas_round(x * 1000, 1)
  sprintf("%.3f", ri / 1000)
}

# ---- Series / Scatter data (step plot of rounde) ----
x_seq <- seq(0, 6, by = 0.05)  # 121 obs like SAS DO 0 TO 6 BY 0.05
y_seq <- sas_rounde(x_seq)
group_seq <- y_seq
Series <- data.frame(x = x_seq, y = y_seq, group = group_seq)

x1 <- seq(0.5, 5.5, by = 1)
y1 <- sas_rounde(x1)
Scatter1 <- data.frame(x1 = x1, y1 = y1)

x2 <- seq(0.5, 5.5, by = 1)
y2 <- sas_rounde(x2 + 1) - 1
Scatter2 <- data.frame(x2 = x2, y2 = y2)

# SAS: data RoundToEven; set Series Scatter1 Scatter2; run;
# Combined plotted data: keep all columns, missing where not applicable
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

# ---- Figure: SGPlot equivalent ----
p <- ggplot() +
  geom_line(data = Series, aes(x = x, y = y, group = group), color = "#445694", linewidth = 0.9) +
  geom_point(data = Scatter1, aes(x = x1, y = y1), shape = 21, size = 2.6, colour = "#445694", fill = "#445694", stroke = 1) +
  geom_point(data = Scatter2, aes(x = x2, y = y2), shape = 21, size = 2.6, colour = "#445694", fill = "white", stroke = 1) +
  labs(title = "The Round-To-Even Function", x = "x", y = "RoundE(x)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "grey85"),
        panel.grid.minor = element_line(colour = "grey92"),
        legend.position = "none")

ggsave(file.path(out, "sgplot.png"), plot = p, width = 8, height = 6, dpi = 150)
# CSV of plotted data
write.csv(RoundToEven, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# ---- Seq table: compare ROUND and ROUNDE ----
x_s <- seq(0, 3.5, by = 0.25)
Seq <- data.frame(
  x = x_s,
  Round = sas_round(x_s),
  RoundEven = sas_rounde(x_s)
)

# ---- Channel1 data ----
Length_vals <- c(
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
Channel1 <- data.frame(Length = Length_vals)

Round_df <- data.frame(Length = Channel1$Length)
Round_df$Round <- sas_round(Round_df$Length, 0.1)
Round_df$RoundEven <- sas_rounde(Round_df$Length, 0.1)
Round_df$RoundUp <- as.numeric(Round_df$Round > Round_df$Length)
Round_df$RoundEvenUp <- as.numeric(Round_df$RoundEven > Round_df$Length)

# ---- PROC MEANS equivalents ----
# Means1: sum mean for Length, Round, RoundEven
vars1 <- c("Length", "Round", "RoundEven")
labels1 <- c("True values", "Rounded values", "Round-to-even values")
sums1 <- sapply(vars1, function(v) sum(Round_df[[v]], na.rm = TRUE))
means1 <- sapply(vars1, function(v) mean(Round_df[[v]], na.rm = TRUE))
n1obs <- sapply(vars1, function(v) sum(!is.na(Round_df[[v]])))
Means1 <- data.frame(
  Variable = vars1,
  Label = labels1,
  N = as.integer(n1obs),
  Sum = as.numeric(sums1),
  Mean = as.numeric(means1),
  stringsAsFactors = FALSE
)
Means1_disp <- data.frame(
  Variable = vars1,
  Label = labels1,
  Sum = fmt3(sums1),
  Mean = fmt3(means1),
  stringsAsFactors = FALSE
)

# Means2: mean for RoundUp, RoundEvenUp
vars2 <- c("RoundUp", "RoundEvenUp")
labels2 <- c("Proportion rounded up for ROUND", "Proportion rounded up for ROUNDE")
means2 <- sapply(vars2, function(v) mean(Round_df[[v]], na.rm = TRUE))
n2obs <- sapply(vars2, function(v) sum(!is.na(Round_df[[v]])))
Means2 <- data.frame(
  Variable = vars2,
  Label = labels2,
  N = as.integer(n2obs),
  Mean = as.numeric(means2),
  stringsAsFactors = FALSE
)
Means2_disp <- data.frame(
  Variable = vars2,
  Label = labels2,
  Mean = fmt3(means2),
  stringsAsFactors = FALSE
)

# ---- Text rendering (P096.lst equivalent) ----
txt_path <- file.path(out, "P096.txt")
lst_txt_path <- file.path(out, "P096.lst.txt")
con <- file(txt_path, open = "wt")

cat("The Round-To-Even Function\n", file = con)
cat("\n", file = con)
cat("Seq: Comparison of ROUND and ROUNDE functions\n", file = con)
cat(sprintf("%-10s %-10s %-10s\n", "x", "Round(x)", "RoundE(x)"), file = con)
cat(paste(rep("-", 32), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(Seq))) {
  # SAS BEST display: x as is, Round/RoundEven as integers
  xs <- ifelse(Seq$x[i] == round(Seq$x[i]), sprintf("%d", as.integer(Seq$x[i])),
               gsub("0+$", "", gsub("\\.$", "", sprintf("%.2f", Seq$x[i]))))
  # simpler: format x removing trailing zeros
  xs2 <- format(Seq$x[i], nsmall = 2, trim = TRUE)
  # keep SAS-like: 0, 0.25, 0.50 -> 0.5 etc. Use default R display stripped?
  # Use plain: as.character(Seq$x[i])
  xs3 <- as.character(Seq$x[i])
  cat(sprintf("%-10s %-10d %-10d\n", xs3, as.integer(Seq$Round[i]), as.integer(Seq$RoundEven[i])), file = con)
}
cat("\n", file = con)
cat("PROC MEANS DATA=Round SUM MEAN NDEC=3\n", file = con)
cat(" True values / Rounded values / Round-to-even values\n", file = con)
cat(sprintf("%-12s %-32s %10s %10s\n", "Variable", "Label", "Sum", "Mean"), file = con)
cat(paste(rep("-", 70), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(Means1_disp))) {
  cat(sprintf("%-12s %-32s %10s %10s\n", Means1_disp$Variable[i], Means1_disp$Label[i],
              Means1_disp$Sum[i], Means1_disp$Mean[i]), file = con)
}
cat("\n", file = con)
cat("PROC MEANS DATA=Round MEAN NDEC=3\n", file = con)
cat(" Proportion rounded up\n", file = con)
cat(sprintf("%-12s %-32s %10s\n", "Variable", "Label", "Mean"), file = con)
cat(paste(rep("-", 60), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(Means2_disp))) {
  cat(sprintf("%-12s %-32s %10s\n", Means2_disp$Variable[i], Means2_disp$Label[i],
              Means2_disp$Mean[i]), file = con)
}
close(con)
# duplicate for .lst.txt name
file.copy(txt_path, lst_txt_path, overwrite = TRUE)

# ---- CSV outputs ----
# Main table body: Seq (15 rows, 45 numbers)
write.csv(Seq, file.path(out, "P096.csv"), row.names = FALSE, na = "")
# Additional means CSVs (contain remaining 8 numbers; together 53)
write.csv(Means1, file.path(out, "P096_means_sum_mean.csv"), row.names = FALSE, na = "")
write.csv(Means2, file.path(out, "P096_means_prop.csv"), row.names = FALSE, na = "")
# Copies for lst naming
file.copy(file.path(out, "P096.csv"), file.path(out, "P096.lst.csv"), overwrite = TRUE)
# Display-formatted versions for reference
write.csv(Means1_disp, file.path(out, "P096_means_sum_mean_disp.csv"), row.names = FALSE, na = "")
write.csv(Means2_disp, file.path(out, "P096_means_prop_disp.csv"), row.names = FALSE, na = "")
# Round detail dataset (optional, documents rounding)
write.csv(Round_df, file.path(out, "P096_Round.csv"), row.names = FALSE, na = "")
