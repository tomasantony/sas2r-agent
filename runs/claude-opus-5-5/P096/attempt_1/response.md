```r
# P096 - Round to even (translation of P096_RoundToEven.sas)
suppressPackageStartupMessages({
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------- helpers emulating SAS ROUND / ROUNDE ----------------
fuzz <- 1e-9
sas_round <- function(x, unit = 1) {
  q <- x / unit
  n <- sign(q) * floor(abs(q) + 0.5 + fuzz)
  round(n * unit, 10)
}
sas_rounde <- function(x, unit = 1) {
  q <- x / unit
  s <- sign(q); a <- abs(q)
  f <- floor(a + fuzz)
  d <- a - f
  tie <- abs(d - 0.5) < fuzz
  n <- ifelse(tie, ifelse(f %% 2 == 0, f, f + 1), floor(a + 0.5))
  round(s * n * unit, 10)
}
# SAS DO loop with accumulating increment
sas_do <- function(from, to, by) {
  v <- c(); x <- from
  while (x <= to) { v <- c(v, x); x <- x + by }
  v
}
fmt3 <- function(v) formatC(sas_round(v, 0.001), format = "f", digits = 3)
best <- function(v) sapply(v, function(z) format(signif(z, 12), trim = TRUE))

# ---------------- Step plot ----------------
xs <- sas_do(0, 6, 0.05)
Series <- data.frame(x = xs, y = sas_rounde(xs))
Series$group <- Series$y
x1 <- sas_do(0.5, 5.5, 1)
Scatter1 <- data.frame(x1 = x1, y1 = sas_rounde(x1))
Scatter2 <- data.frame(x2 = x1, y2 = sas_rounde(x1 + 1) - 1)

RoundToEven <- rbind(
  data.frame(x = Series$x, y = Series$y, group = Series$group,
             x1 = NA, y1 = NA, x2 = NA, y2 = NA),
  data.frame(x = NA, y = NA, group = NA, x1 = Scatter1$x1, y1 = Scatter1$y1, x2 = NA, y2 = NA),
  data.frame(x = NA, y = NA, group = NA, x1 = NA, y1 = NA, x2 = Scatter2$x2, y2 = Scatter2$y2)
)
write.csv(RoundToEven, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

p <- ggplot() +
  geom_line(data = Series, aes(x = x, y = y, group = group), colour = "#A23A2E", linewidth = 1) +
  geom_point(data = Scatter1, aes(x = x1, y = y1), shape = 21, fill = "#445694",
             colour = "black", size = 2.5) +
  geom_point(data = Scatter2, aes(x = x2, y = y2), shape = 21, fill = "white",
             colour = "black", size = 2.5) +
  labs(title = "The Round-To-Even Function", x = "x", y = "RoundE(x)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(out, "sgplot.png"), p, width = 6.4, height = 4.8, dpi = 100)

# ---------------- Seq table ----------------
sx <- sas_do(0, 3.5, 0.25)
Seq <- data.frame(x = sx, Round = sas_round(sx), RoundEven = sas_rounde(sx))
write.csv(Seq, file.path(out, "seq.csv"), row.names = FALSE, na = "")

# ---------------- Channel1 data ----------------
vals <- scan(text = "
0.91 1.01 0.95 1.13 1.12 0.86 0.96 1.17 1.36 1.10 
0.98 1.27 1.13 0.92 1.15 1.26 1.14 0.88 1.03 1.00 
0.98 0.94 1.09 0.92 1.10 0.95 1.05 1.05 1.11 1.15 
1.11 0.98 0.78 1.09 0.94 1.05 0.89 1.16 0.88 1.19 
1.01 1.08 1.19 0.94 0.92 1.27 0.90 0.88 1.38 1.02 
1.21 0.99 1.19 1.05 0.95 0.92 1.06 1.03 0.76 1.04 
0.84 0.93 0.96 1.23 1.06 1.20 0.94 0.74 0.80 1.00 
1.00 0.97 0.94 1.05 1.09 1.07 0.79 1.06 0.96 1.00 
1.07 0.98 1.26 0.99 1.12 0.99 1.05 1.12 0.85 0.96 
1.01 1.21 0.95 0.86 0.86 1.19 1.24 0.75 1.03 0.84 
0.93 0.65 0.84 0.92 0.86 1.07 1.05 0.95 1.05 1.10 
0.93 1.03 0.94 0.97 0.95 0.99 0.97 0.81 1.10 0.95 
1.08 0.75 0.97 0.94 0.95 0.85 1.02 1.07 1.11 1.02 
1.16 1.05 0.88 1.29 0.99 0.83 1.06 1.05 1.03 1.15 
1.14 0.91 1.17 1.17 0.70 0.84 1.33 0.95 1.34 1.14 
1.08 1.28 1.14 0.84 1.09 1.12 0.99 1.08 0.95 1.12 
0.70 1.07 1.06 1.24 1.17 0.96 1.00 0.85 1.25 0.84 
1.00 1.01 1.08 1.08 0.75 1.06 1.09 0.86 1.08 1.11 
1.19 1.25 0.94 0.97 0.81 0.91 0.99 0.94 0.94 1.12 
1.10 0.82 1.17 0.89 0.75 0.84 1.10 0.93 1.11 1.07 
1.20 1.22 0.90 1.01 1.17 0.97 1.02 0.77 1.07 0.87 
0.96 1.21 1.04 0.89 0.95 0.86 0.84 1.03 1.26 0.71 
0.75 1.34 1.14 1.09 0.85 0.68 0.79 1.06 1.12 1.10 
0.98 1.11 0.98 0.86 1.15 0.89 1.21 0.79 1.31 0.84 
1.05 0.89 1.01 0.81 1.10 1.04 1.07 1.02 0.91 1.00 
0.98 0.95 0.82 1.11 1.07 1.07 0.89 1.15 1.04 1.19 
1.07 1.03 1.04 0.81 0.93 1.06 0.92 1.29 1.04 0.99 
0.86 0.84 1.22 1.09 1.14 0.76 0.86 0.96 1.07 1.21 
0.93 1.07 1.13 0.95 0.92 1.01 1.01 1.16 1.24 0.82 
1.03 1.31 0.79 1.14 1.15 0.97 0.82 1.10 1.06 1.00 
0.58 0.79 1.08 1.06 1.06 1.14 1.09 1.01 0.87 0.97 
1.08 1.03 1.08 0.92 1.26 0.74 0.98 0.94 0.84 1.07 
1.04 0.93 0.90 0.83 0.91 1.15 1.00 0.86 0.87 0.75 
1.19 1.13 1.14 0.78 1.03 0.95 0.77 0.89 0.90 0.96 
0.95 1.08 1.03 0.98 1.00 0.86 1.16 1.05 0.97 0.91 
1.06 1.12 0.97 1.03 0.88 1.02 0.98 0.95 0.86 1.14 
1.15 1.26 0.84 0.82 0.91 1.02 0.96 0.95 1.17 1.03 
0.89 1.07 1.06 1.09 0.95 0.99 1.38 1.10 0.88 1.05 
1.18 0.93 1.14 0.86 1.24 0.97 1.04 1.12 1.10 1.14 
1.22 0.89 0.93 1.30 0.93 1.00 1.06 0.92 1.26 0.75 
1.24 1.17 1.09 1.06 0.78 0.95 0.97 0.90 0.98 0.88 
0.87 1.00 0.83 1.11 1.15 1.20 1.21 1.00 0.95 0.82 
1.16 0.80 0.99 0.89 1.17 ", quiet = TRUE)

Round <- data.frame(Length = vals)
Round$Round       <- sas_round(Round$Length, 0.1)
Round$RoundEven   <- sas_rounde(Round$Length, 0.1)
Round$RoundUp     <- as.numeric(Round$Round > Round$Length)
Round$RoundEvenUp <- as.numeric(Round$RoundEven > Round$Length)
write.csv(Round, file.path(out, "round.csv"), row.names = FALSE, na = "")

# ---------------- PROC MEANS ----------------
means1 <- data.frame(
  Variable = c("Length", "Round", "RoundEven"),
  Label = c("True values", "Rounded values", "Round-to-even values"),
  Sum = c(sum(Round$Length), sum(Round$Round), sum(Round$RoundEven)),
  Mean = c(mean(Round$Length), mean(Round$Round), mean(Round$RoundEven))
)
means2 <- data.frame(
  Variable = c("RoundUp", "RoundEvenUp"),
  Label = c("Proportion rounded up for ROUND", "Proportion rounded up for ROUNDE"),
  Mean = c(mean(Round$RoundUp), mean(Round$RoundEvenUp))
)
write.csv(means1, file.path(out, "means_sum_mean.csv"), row.names = FALSE, na = "")
write.csv(means2, file.path(out, "means_roundup.csv"), row.names = FALSE, na = "")

# ---------------- Combined table CSV ----------------
tab <- rbind(
  data.frame(section = "Seq", c1 = best(Seq$x), c2 = best(Seq$Round), c3 = best(Seq$RoundEven),
             c4 = ""),
  data.frame(section = "Means1", c1 = means1$Variable, c2 = means1$Label,
             c3 = fmt3(means1$Sum), c4 = fmt3(means1$Mean)),
  data.frame(section = "Means2", c1 = means2$Variable, c2 = means2$Label,
             c3 = fmt3(means2$Mean), c4 = "")
)
write.csv(tab, file.path(out, "P096.csv"), row.names = FALSE, na = "")

# ---------------- Text rendering ----------------
lines <- c("The Round-To-Even Function", "",
           sprintf("%8s %9s %10s", "x", "Round(x)", "RoundE(x)"))
for (i in seq_len(nrow(Seq))) {
  lines <- c(lines, sprintf("%8s %9s %10s", best(Seq$x[i]), best(Seq$Round[i]),
                            best(Seq$RoundEven[i])))
}
lines <- c(lines, "", "", "The MEANS Procedure", "",
           sprintf("%-10s %-22s %12s %12s", "Variable", "Label", "Sum", "Mean"),
           strrep("-", 60))
for (i in seq_len(nrow(means1))) {
  lines <- c(lines, sprintf("%-10s %-22s %12s %12s", means1$Variable[i], means1$Label[i],
                            fmt3(means1$Sum[i]), fmt3(means1$Mean[i])))
}
lines <- c(lines, strrep("-", 60), "", "", "The MEANS Procedure", "",
           sprintf("%-12s %-34s %10s", "Variable", "Label", "Mean"),
           strrep("-", 58))
for (i in seq_len(nrow(means2))) {
  lines <- c(lines, sprintf("%-12s %-34s %10s", means2$Variable[i], means2$Label[i],
                            fmt3(means2$Mean[i])))
}
lines <- c(lines, strrep("-", 58))
writeLines(lines, file.path(out, "P096.txt"))

cat("P096 done\n")
```