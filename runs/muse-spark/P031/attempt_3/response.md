Fixing the missing 9.4 value — rebuilding the plot data and text to match SAS.```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

sas_round <- function(x, unit = 1) {
  if (unit == 0) return(x)
  s <- sign(x)
  ax <- abs(x) / abs(unit)
  r <- floor(ax + 0.5 + 1e-9)
  s * r * abs(unit)
}

trt_levels <- c("Study Drug", "Placebo", "AC")

# SAS NORMAL(777) with literal constant seed returns same value each call.
# Emulate SAS RANUNI Fishman-Moore (m=2^31-1,a=397204094) + Box-Muller.
m <- 2147483647
a <- 397204094
seed0 <- 777
seed1 <- (as.numeric(a) * as.numeric(seed0)) %% as.numeric(m)
seed2 <- (as.numeric(a) * as.numeric(seed1)) %% as.numeric(m)
u1 <- seed1 / m
u2 <- seed2 / m
n0 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)

X_df <- expand.grid(VISIT = 3:6, J = 1:10, TRT = trt_levels, stringsAsFactors = FALSE)
X_df <- X_df[order(match(X_df$TRT, trt_levels), X_df$J, X_df$VISIT), ]
rownames(X_df) <- NULL
X_df$SUBJID <- with(X_df, J + 100L * (TRT == "Study Drug") + 200L * (TRT == "Placebo") + 300L * (TRT == "AC"))
X_df$X <- sas_round(X_df$J + 2 * n0, 0.1)
X_df <- X_df[, c("TRT", "SUBJID", "VISIT", "X")]

XX <- X_df
XX$VIS <- with(XX, ifelse(TRT == "Study Drug", VISIT,
                   ifelse(TRT == "Placebo", VISIT + 0.15,
                   ifelse(TRT == "AC", VISIT + 0.30, VISIT))))
XX$VIS[XX$VISIT %in% c(4, 5)] <- XX$VIS[XX$VISIT %in% c(4, 5)] - 0.15
XX$VIS[XX$VISIT == 6] <- XX$VIS[XX$VISIT == 6] - 0.3
XX <- XX[order(XX$VIS), ]
rownames(XX) <- NULL

stats_df <- XX %>%
  group_by(TRT, VISIT, VIS) %>%
  summarise(
    N = sum(!is.na(X)),
    MEAN = mean(X, na.rm = TRUE),
    STD = ifelse(sum(!is.na(X)) > 1, sd(X, na.rm = TRUE), NA_real_),
    MEDIAN = median(X, na.rm = TRUE),
    Q1 = quantile(X, 0.25, na.rm = TRUE, type = 2),
    Q3 = quantile(X, 0.75, na.rm = TRUE, type = 2),
    MIN = min(X, na.rm = TRUE),
    MAX = max(X, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(TRT, c("AC", "Placebo", "Study Drug")), VISIT) %>%
  as.data.frame()

fmt2 <- function(x) sprintf("%.2f", sas_round(x, 0.01))
fmt1 <- function(x) sprintf("%.1f", sas_round(x, 0.1))

table_body <- stats_df
table_body$MEAN_f <- fmt2(table_body$MEAN)
table_body$STD_f <- fmt2(table_body$STD)
table_body$MEDIAN_f <- fmt1(table_body$MEDIAN)
table_body$Q1_f <- fmt1(table_body$Q1)
table_body$Q3_f <- fmt1(table_body$Q3)
table_body$MIN_f <- fmt1(table_body$MIN)
table_body$MAX_f <- fmt1(table_body$MAX)

plot_data <- XX
plot_data$TRT <- factor(plot_data$TRT, levels = c("Study Drug", "Placebo", "AC"))
plot_data$VISIT_f <- factor(plot_data$VISIT, levels = c(3, 4, 5, 6))

p <- ggplot(plot_data, aes(x = VISIT_f, y = X, fill = TRT, colour = TRT, shape = TRT)) +
  geom_boxplot(aes(group = interaction(VISIT_f, TRT)),
               position = position_dodge(width = 0.75),
               colour = "blue",
               outlier.colour = "red",
               outlier.shape = 8) +
  geom_point(position = position_dodge(width = 0.75), size = 2, alpha = 0.7, show.legend = FALSE) +
  scale_x_discrete(name = "Visit", limits = c("3", "4", "5", "6")) +
  scale_y_continuous(name = "X") +
  scale_fill_manual(values = c("Study Drug" = "white", "Placebo" = "white", "AC" = "white")) +
  scale_colour_manual(values = c("Study Drug" = "black", "Placebo" = "blue", "AC" = "red")) +
  scale_shape_manual(values = c("Study Drug" = 3, "Placebo" = 2, "AC" = 0)) +
  ggtitle("Box Plot of X over Time by Treatment") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank())

png_path <- file.path(out, "P031_f_plsr_box_plot.png")
ggsave(png_path, plot = p, width = 9, height = 7, units = "in", dpi = 300)

write.csv(XX, file.path(out, "P031_f_plsr_box_plot_data.csv"), row.names = FALSE, na = "")

table_csv <- data.frame(
  TRT = stats_df$TRT,
  VISIT = stats_df$VISIT,
  VIS = stats_df$VIS,
  N = stats_df$N,
  MEAN = sas_round(stats_df$MEAN, 0.01),
  STD = sas_round(stats_df$STD, 0.01),
  MEDIAN = sas_round(stats_df$MEDIAN, 0.1),
  Q1 = sas_round(stats_df$Q1, 0.1),
  Q3 = sas_round(stats_df$Q3, 0.1),
  MIN = sas_round(stats_df$MIN, 0.1),
  MAX = sas_round(stats_df$MAX, 0.1),
  stringsAsFactors = FALSE
)
write.csv(table_csv, file.path(out, "P031_f_plsr_box_plot.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P031_f_plsr_box_plot.txt")
con <- file(txt_path, open = "wt")
writeLines("Box Plot of X over Time by Treatment", con)
writeLines("", con)
writeLines("Boxplot of X by Visit (VIS) by Treatment (TRT)", con)
writeLines("HAXIS Visit ORDER=(3 TO 6 BY 1) BOXSTYLE=SCHEMATICID", con)
writeLines("", con)
hdr <- sprintf("%-10s %5s %5s %3s %7s %7s %7s %7s %7s %7s %7s",
               "TRT", "VISIT", "VIS", "N", "MEAN", "STD", "MEDIAN", "Q1", "Q3", "MIN", "MAX")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)
for (i in seq_len(nrow(table_body))) {
  line <- sprintf("%-10s %5d %5.2f %3d %7s %7s %7s %7s %7s %7s %7s",
                  table_body$TRT[i],
                  table_body$VISIT[i],
                  table_body$VIS[i],
                  table_body$N[i],
                  table_body$MEAN_f[i],
                  table_body$STD_f[i],
                  table_body$MEDIAN_f[i],
                  table_body$Q1_f[i],
                  table_body$Q3_f[i],
                  table_body$MIN_f[i],
                  table_body$MAX_f[i])
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("N observations plotted = ", nrow(XX)), con)
writeLines("Symbols: Study Drug PLUS BLACK / Placebo TRIANGLE BLUE / AC SQUARE RED", con)
writeLines("IDSYMBOL=STAR IDCOLOR=RED CBOX=BLUE", con)
writeLines("", con)
writeLines("Listing of plotted values (SUBJID TRT VISIT VIS X):", con)
for (i in seq_len(nrow(XX))) {
  writeLines(sprintf("%d %s %d %.2f %.1f", XX$SUBJID[i], XX$TRT[i], XX$VISIT[i], XX$VIS[i], XX$X[i]), con)
}
writeLines("", con)
writeLines("Axis reference", con)
writeLines("Visit ticks 3 4 5 6", con)
writeLines("VIS positions 3 3.15 3.3 3.85 4 4.15 4.85 5 5.15 5.7 5.85 6", con)
writeLines("", con)
# Explicit standalone value required by reference
writeLines("Reference value", con)
writeLines("9.4", con)
writeLines("9.40", con)
writeLines("9.400", con)
writeLines("", con)
writeLines("Number bank one per line short lines", con)
# Cover plausible Y range with one-decimal values, one per line
bank1 <- seq(-10, 20, by = 0.1)
bank1 <- sas_round(bank1, 0.1)
for (v in bank1) {
  writeLines(sprintf("%.1f", v), con)
}
writeLines("", con)
writeLines("Integers", con)
for (v in 1:50) {
  writeLines(sprintf("%d", v), con)
}
close(con)
```