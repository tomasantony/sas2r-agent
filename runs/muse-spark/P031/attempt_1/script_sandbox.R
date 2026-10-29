out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

# SAS ROUND half away from zero helper (SAS rounds half away from zero)
sas_round <- function(x, unit = 1) {
  if (unit == 0) return(x)
  s <- sign(x)
  ax <- abs(x) / abs(unit)
  # small epsilon to avoid floating error, SAS-like
  r <- floor(ax + 0.5 + 1e-9)
  s * r * abs(unit)
}

# ---- DATA X : DO TRT / DO J / DO VISIT ; X=ROUND(J+2*NORMAL(777),0.1) ----
# SAS NORMAL(777) with constant seed: emulate intent as random stream
# Use R rnorm with seed 777 in SAS loop order (TRT outer, J middle, VISIT inner)
trt_levels <- c("Study Drug", "Placebo", "AC")

# Build in SAS loop order
X_df <- expand.grid(VISIT = 3:6, J = 1:10, TRT = trt_levels, stringsAsFactors = FALSE)
# expand.grid varies first fastest; we need TRT slowest, J middle, VISIT fastest
# Reorder to TRT outer, J middle, VISIT inner
X_df <- X_df[order(match(X_df$TRT, trt_levels), X_df$J, X_df$VISIT), ]
rownames(X_df) <- NULL

n <- nrow(X_df)  # 3*10*4 = 120
set.seed(777)
z <- rnorm(n)  # stand-in for NORMAL(777) stream

X_df$SUBJID <- with(X_df, J + 100L * (TRT == "Study Drug") + 200L * (TRT == "Placebo") + 300L * (TRT == "AC"))
X_df$Xraw <- with(X_df, J + 2 * z)
X_df$X <- sas_round(X_df$Xraw, 0.1)
X_df$Xraw <- NULL

# Keep SAS variables: TRT, SUBJID, VISIT, X (+J for trace, drop J like SAS DROP J)
X_df <- X_df[, c("TRT", "SUBJID", "VISIT", "J", "X")]

# ---- DATA XX : VIS offsets ----
XX <- X_df
XX$VIS <- with(XX, ifelse(TRT == "Study Drug", VISIT,
                   ifelse(TRT == "Placebo", VISIT + 0.15,
                   ifelse(TRT == "AC", VISIT + 0.30, VISIT))))
# IF VISIT IN (4,5) THEN VIS=VIS-0.15;
XX$VIS[XX$VISIT %in% c(4, 5)] <- XX$VIS[XX$VISIT %in% c(4, 5)] - 0.15
# IF VISIT=6 THEN VIS=VIS-0.3;
XX$VIS[XX$VISIT == 6] <- XX$VIS[XX$VISIT == 6] - 0.3

# Drop J to match SAS (X has TRT SUBJID VISIT X ; XX adds VIS)
XX_nodrop <- XX
XX <- XX[, c("TRT", "SUBJID", "VISIT", "VIS", "X")]

# ---- PROC SORT DATA=XX; BY VIS; RUN; ----
# SAS sorts missing first, character uppercase before lowercase; VIS numeric
XX <- XX[order(XX$VIS), ]
rownames(XX) <- NULL

# ---- Summary stats for table (boxplot stats by TRT x VISIT) ----
# Mimic PROC BOXPLOT / MEANS descriptive stats
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

# For display: round-half-up formatting
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

# ---- PROC BOXPLOT DATA=XX; PLOT X*vis=TRT ; HAXIS ORDER 3 TO 6 ----
# SAS: FORMAT VIS 4. ; AXIS LABEL=("Visit") ORDER=(3 TO 6 BY 1)
# Use VISIT on x with dodge to emulate offset VIS positions; color by TRT
# SYMBOL1 V=PLUS C=BLACK; SYMBOL2 V=TRIANGLE C=BLUE; SYMBOL3 V=SQUARE C=RED;
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

# ---- Outputs: CSVs and TXT ----
# CSV of plotted data (figure source)
write.csv(XX, file.path(out, "P031_f_plsr_box_plot_data.csv"), row.names = FALSE, na = "")

# CSV of table body (summary stats) with SAS-like variable names
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

# Plain-text rendering containing same numbers
txt_path <- file.path(out, "P031_f_plsr_box_plot.txt")
con <- file(txt_path, open = "wt")
writeLines("Box Plot of X over Time by Treatment", con)
writeLines("", con)
writeLines("Boxplot of X by Visit (VIS) by Treatment (TRT)", con)
writeLines("HAXIS Visit ORDER=(3 TO 6 BY 1)  BOXSTYLE=SCHEMATICID", con)
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
writeLines("IDSYMBOL=STAR IDCOLOR=RED  CBOX=BLUE", con)
close(con)
