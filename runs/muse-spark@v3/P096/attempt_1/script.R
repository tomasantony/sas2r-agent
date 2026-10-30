out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## helpers: SAS semantics
rhu <- function(x, d = 0) {
  p <- 10^d
  sign(x) * floor(abs(x) * p + 0.5 + 1e-9) / p
}
fmt3 <- function(x) sprintf("%.3f", rhu(x, 3))

sas_round <- function(x, unit = 1) {
  q <- x / unit
  rq <- sign(q) * floor(abs(q) + 0.5 + 1e-9)
  rq[is.na(q)] <- NA_real_
  # sign(0) is 0 -> gives 0, ok
  rq * unit
}
sas_rounde <- function(x, unit = 1) {
  q <- x / unit
  res <- rep(NA_real_, length(q))
  for (i in seq_along(q)) {
    qi <- q[i]
    if (is.na(qi)) { res[i] <- NA_real_; next }
    if (qi == 0) { res[i] <- 0; next }
    s <- sign(qi)
    aq <- abs(qi)
    fl <- floor(aq + 1e-9)
    # adjust fl when aq is integer with floating error e.g. 10.999999999
    # if aq very close to integer, snap
    # fl already snapped by +1e-9; recompute frac
    frac <- aq - fl
    # snap frac near 0 or 1 due to fuzz
    if (abs(frac) < 1e-9) frac <- 0
    if (abs(frac - 1) < 1e-9) { fl <- fl + 1; frac <- 0 }
    if (abs(frac - 0.5) < 1e-9) {
      # tie to even
      if (fl %% 2 == 1) fl <- fl + 1
      res[i] <- s * fl
    } else {
      res[i] <- s * floor(aq + 0.5 + 1e-9)
    }
  }
  res * unit
}

## Series for step plot
x_seq <- seq(0, 6, by = 0.05)
y_seq <- sas_rounde(x_seq)
group_seq <- y_seq
Series <- data.frame(x = x_seq, y = y_seq, group = group_seq)

x1_seq <- seq(0.5, 5.5, by = 1)
y1_seq <- sas_rounde(x1_seq)
Scatter1 <- data.frame(x1 = x1_seq, y1 = y1_seq)

x2_seq <- seq(0.5, 5.5, by = 1)
y2_seq <- sas_rounde(x2_seq + 1) - 1
Scatter2 <- data.frame(x2 = x2_seq, y2 = y2_seq)

# RoundToEven as SAS SET concatenation
n1 <- nrow(Series); n2 <- nrow(Scatter1); n3 <- nrow(Scatter2)
N <- n1 + n2 + n3
RoundToEven <- data.frame(
  x = c(Series$x, rep(NA_real_, n2 + n3)),
  y = c(Series$y, rep(NA_real_, n2 + n3)),
  group = c(Series$group, rep(NA_real_, n2 + n3)),
  x1 = c(rep(NA_real_, n1), Scatter1$x1, rep(NA_real_, n3)),
  y1 = c(rep(NA_real_, n1), Scatter1$y1, rep(NA_real_, n3)),
  x2 = c(rep(NA_real_, n1 + n2), Scatter2$x2),
  y2 = c(rep(NA_real_, n1 + n2), Scatter2$y2)
)

## Plot
library(ggplot2)
df_series <- Series
df_s1 <- Scatter1
df_s2 <- Scatter2
p <- ggplot() +
  geom_line(data = df_series, aes(x = x, y = y, group = group), color = "#445694", linewidth = 0.9) +
  geom_point(data = df_s1, aes(x = x1, y = y1), shape = 21, size = 3, fill = "#445694", color = "#445694", stroke = 1) +
  geom_point(data = df_s2, aes(x = x2, y = y2), shape = 21, size = 3, fill = "white", color = "#445694", stroke = 1) +
  labs(title = "The Round-To-Even Function", x = "x", y = "RoundE(x)") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_line(color = "grey92"),
        plot.title = element_text(hjust = 0.5))
ggsave(file.path(out, "sgplot.png"), plot = p, width = 6, height = 4, dpi = 150)
# capital variant for safety
try(ggsave(file.path(out, "SGPlot.png"), plot = p, width = 6, height = 4, dpi = 150), silent = TRUE)

write.csv(RoundToEven, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

## Seq table comparing ROUND and ROUNDE
x_s <- seq(0, 3.5, by = 0.25)
Round_s <- sas_round(x_s)
RoundEven_s <- sas_rounde(x_s)
Seq <- data.frame(x = x_s, Round = Round_s, RoundEven = RoundEven_s)

## Channel1 data
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
Round_df <- data.frame(
  Length = Length_vals,
  Round = sas_round(Length_vals, 0.1),
  RoundEven = sas_rounde(Length_vals, 0.1)
)
Round_df$RoundUp <- as.integer(Round_df$Round > Round_df$Length)
Round_df$RoundEvenUp <- as.integer(Round_df$RoundEven > Round_df$Length)

## PROC MEANS sum mean and mean
sum_Length <- sum(Round_df$Length)
mean_Length <- mean(Round_df$Length)
sum_Round <- sum(Round_df$Round)
mean_Round <- mean(Round_df$Round)
sum_RE <- sum(Round_df$RoundEven)
mean_RE <- mean(Round_df$RoundEven)
mean_RU <- mean(Round_df$RoundUp)
mean_REU <- mean(Round_df$RoundEvenUp)

## write datasets
write.csv(Seq, file.path(out, "Seq.csv"), row.names = FALSE, na = "")
write.csv(Round_df, file.path(out, "Round.csv"), row.names = FALSE, na = "")
write.csv(Channel1, file.path(out, "Channel1.csv"), row.names = FALSE, na = "")

## Build text rendering P096.txt / P096.lst (53 numbers: 45 + 6 + 2)
# format Seq x without trailing zeros like SAS BEST
fmt_x <- function(v) {
  # SAS BEST would show 0, 0.25, 0.5 etc.
  as.character(v)
}
lines <- c(
  "The Round-To-Even Function",
  "",
  "Comparison of ROUND and ROUNDE Functions",
  "",
  sprintf("%-8s %-10s %-10s", "x", "Round(x)", "RoundE(x)")
)
for (i in seq_len(nrow(Seq))) {
  lines <- c(lines, sprintf("%-8s %-10s %-10s", fmt_x(Seq$x[i]), fmt_x(Seq$Round[i]), fmt_x(Seq$RoundEven[i])))
}
lines <- c(lines, "",
  "Summary Statistics for True, Rounded and Round-to-Even Values",
  "",
  sprintf("%-20s %-12s %-12s", "Variable", "Sum", "Mean"),
  sprintf("%-20s %-12s %-12s", "True values", fmt3(sum_Length), fmt3(mean_Length)),
  sprintf("%-20s %-12s %-12s", "Rounded values", fmt3(sum_Round), fmt3(mean_Round)),
  sprintf("%-20s %-12s %-12s", "Round-to-even values", fmt3(sum_RE), fmt3(mean_RE)),
  "",
  "Proportion Rounded Up",
  "",
  sprintf("%-35s %-12s", "Variable", "Mean"),
  sprintf("%-35s %-12s", "Proportion rounded up for ROUND", fmt3(mean_RU)),
  sprintf("%-35s %-12s", "Proportion rounded up for ROUNDE", fmt3(mean_REU))
)
writeLines(lines, file.path(out, "P096.txt"))
writeLines(lines, file.path(out, "P096.lst"))

## CSV of table body containing same numbers
# Long form with all numbers
csv_seq <- data.frame(Table = "Seq", Variable = "", x = Seq$x, Round = Seq$Round, RoundEven = Seq$RoundEven, Sum = NA_real_, Mean = NA_real_, stringsAsFactors = FALSE)
csv_m1 <- data.frame(
  Table = "Means_SumMean",
  Variable = c("True values", "Rounded values", "Round-to-even values"),
  x = NA_real_, Round = NA_real_, RoundEven = NA_real_,
  Sum = c(sum_Length, sum_Round, sum_RE),
  Mean = c(mean_Length, mean_Round, mean_RE),
  stringsAsFactors = FALSE
)
csv_m2 <- data.frame(
  Table = "Means_Proportion",
  Variable = c("Proportion rounded up for ROUND", "Proportion rounded up for ROUNDE"),
  x = NA_real_, Round = NA_real_, RoundEven = NA_real_,
  Sum = NA_real_,
  Mean = c(mean_RU, mean_REU),
  stringsAsFactors = FALSE
)
P096_body <- rbind(csv_seq, csv_m1, csv_m2)
write.csv(P096_body, file.path(out, "P096.csv"), row.names = FALSE, na = "")
