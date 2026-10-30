# P031 Boxplot group - R translation of P031_Boxplot_group.sas
# Generates same data as SAS DATA steps and reproduces PROC BOXPLOT

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS ROUND half away from zero
round_half_away <- function(x, unit) {
  # SAS ROUND(x, unit) : round to nearest multiple of unit, half away from zero
  # unit > 0
  res <- rep(NA_real_, length(x))
  idx <- !is.na(x)
  xv <- x[idx]
  sign_x <- sign(xv)
  abs_x <- abs(xv)
  # add small epsilon to avoid floating point 0.4999999 issues
  rounded_abs <- floor(abs_x / unit + 0.5 + 1e-12) * unit
  res[idx] <- sign_x * rounded_abs
  # SAS keeps 0 as 0
  res[idx & xv == 0] <- 0
  res
}

# SAS RANUNI / NORMAL replication
# SAS uses Fishman-Moore LCG: modulus 2^31-1, multiplier 397204094
mod_mult <- function(a, b, m) {
  # (a*b) %% m without overflow using binary method
  result <- 0
  a <- a %% m
  b <- floor(b + 0.5) # b is integer-valued
  while (b > 0) {
    if (b %% 2 == 1) {
      result <- (result + a) %% m
    }
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  result
}

sas_normal_seq <- function(n, seed_init = 777) {
  a <- 397204094
  m <- 2147483647
  seed <- seed_init
  res <- numeric(n)
  for (i in seq_len(n)) {
    seed <- mod_mult(a, seed, m)
    u1 <- seed / m
    if (u1 <= 0) u1 <- 1e-12
    if (u1 >= 1) u1 <- 1 - 1e-12
    seed <- mod_mult(a, seed, m)
    u2 <- seed / m
    # Box-Muller: SAS uses sqrt(-2*log(U1))*cos(2*pi*U2)
    z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
    res[i] <- z
  }
  res
}

# ------------------------------------------------------------
# DATA X
# DO TRT="Study Drug","Placebo","AC";
#   DO J=1 TO 10;
#     SUBJID=J+100*(TRT="Study Drug")+200*(TRT="Placebo")+300*(TRT="AC");
#     DO VISIT=3 TO 6;
#       X=ROUND(J+2*NORMAL(777),0.1);
# ------------------------------------------------------------
trt_levels <- c("Study Drug", "Placebo", "AC")
n_per_trt <- 10
visits <- 3:6
total_n <- length(trt_levels) * n_per_trt * length(visits) # 120

normals <- sas_normal_seq(total_n, seed_init = 777)

# Build in SAS loop order: TRT outer, J inner, VISIT innermost
rows <- vector("list", total_n)
k <- 1
for (trt in trt_levels) {
  for (j in 1:n_per_trt) {
    subjid <- j + 100 * (trt == "Study Drug") + 200 * (trt == "Placebo") + 300 * (trt == "AC")
    for (visit in visits) {
      z <- normals[k]
      x_raw <- j + 2 * z
      x_val <- round_half_away(x_raw, 0.1)
      rows[[k]] <- data.frame(
        TRT = trt,
        SUBJID = subjid,
        VISIT = visit,
        X = x_val,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
}
X <- do.call(rbind, rows)

# DATA XX
# IF TRT="Study Drug" THEN VIS=VISIT;
# ELSE IF TRT="Placebo" THEN VIS=VISIT+0.15;
# ELSE IF TRT="AC" THEN VIS=VISIT+0.30;
# IF VISIT IN (4,5) THEN VIS=VIS-0.15;
# IF VISIT=6 THEN VIS=VIS-0.3;
XX <- X
XX$VIS <- NA_real_
XX$VIS[XX$TRT == "Study Drug"] <- XX$VISIT[XX$TRT == "Study Drug"]
XX$VIS[XX$TRT == "Placebo"]    <- XX$VISIT[XX$TRT == "Placebo"] + 0.15
XX$VIS[XX$TRT == "AC"]         <- XX$VISIT[XX$TRT == "AC"] + 0.30
idx45 <- XX$VISIT %in% c(4, 5)
XX$VIS[idx45] <- XX$VIS[idx45] - 0.15
idx6 <- XX$VISIT == 6
XX$VIS[idx6] <- XX$VIS[idx6] - 0.3

# PROC SORT DATA=XX; BY VIS;
XX <- XX[order(XX$VIS, XX$TRT, XX$SUBJID), ]

# Verify columns exist
stopifnot(all(c("TRT","SUBJID","VISIT","VIS","X") %in% names(XX)))

# ------------------------------------------------------------
# Write CSV of plotted data (SAS dataset XX)
# ------------------------------------------------------------
csv_path <- file.path(out, "p031_f_plsr_box_plot.csv")
write.csv(XX[, c("TRT","SUBJID","VISIT","VIS","X")],
          file = csv_path, row.names = FALSE, na = "")

# Also write TXT rendering with summary
txt_path <- file.path(out, "p031_f_plsr_box_plot.txt")
# Build summary similar to PROC BOXPLOT detail: per VIS/TRT stats
library(dplyr)
stats_tbl <- XX %>%
  group_by(TRT, VIS, VISIT) %>%
  summarise(
    N = n(),
    Mean = mean(X, na.rm = TRUE),
    Median = median(X, na.rm = TRUE),
    StdDev = sd(X, na.rm = TRUE),
    Min = min(X, na.rm = TRUE),
    Max = max(X, na.rm = TRUE),
    Q1 = quantile(X, 0.25, na.rm = TRUE, type = 2),
    Q3 = quantile(X, 0.75, na.rm = TRUE, type = 2),
    .groups = "drop"
  ) %>%
  arrange(VIS)

# Format numbers to SAS-like decimals (4 decimals for stats, 1 for X)
fmt4 <- function(x) sprintf("%.4f", x)
fmt1 <- function(x) sprintf("%.1f", x)

sink(txt_path)
cat("Box Plot of X over Time by Treatment\n")
cat("==================================================\n\n")
cat("Data: XX  (sorted by VIS)\n")
cat(sprintf("Observations: %d\n\n", nrow(XX)))
cat("VIS mapping (offset to avoid overplotting):\n")
cat(" Study Drug: VIS = VISIT, then -0.15 for 4,5 and -0.3 for 6 => 3, 3.85, 4.85, 5.7\n")
cat(" Placebo   : VIS = VISIT+0.15, then -0.15 for 4,5 and -0.3 for 6 => 3.15, 4.0, 5.0, 5.85\n")
cat(" AC        : VIS = VISIT+0.30, then -0.15 for 4,5 and -0.3 for 6 => 3.3, 4.15, 5.15, 6.0\n\n")
cat("Summary statistics by Treatment and Visit (VIS):\n")
cat("--------------------------------------------------------------------------------\n")
# print header
cat(sprintf("%-12s %4s %4s %3s %8s %8s %8s %8s %8s %8s %8s\n",
            "TRT","VISIT","VIS","N","Mean","Median","StdDev","Min","Max","Q1","Q3"))
for (i in seq_len(nrow(stats_tbl))) {
  r <- stats_tbl[i,]
  cat(sprintf("%-12s %4d %4.2f %3d %8s %8s %8s %8s %8s %8s %8s\n",
              r$TRT, r$VISIT, r$VIS, r$N,
              fmt4(r$Mean), fmt4(r$Median), fmt4(r$StdDev),
              fmt1(r$Min), fmt1(r$Max), fmt4(r$Q1), fmt4(r$Q3)))
}
cat("\n")
cat("Plot: X*VIS=TRT  / HAXIS=(3 TO 6 BY 1) BOXSTYLE=SCHEMATICID CBOX=BLUE IDSYMBOL=STAR IDCOLOR=RED\n")
cat("SYMBOL1 V=PLUS C=BLACK (Study Drug)  SYMBOL2 V=TRIANGLE C=BLUE (Placebo)  SYMBOL3 V=SQUARE C=RED (AC)\n")
sink()

# ------------------------------------------------------------
# PROC BOXPLOT equivalent with ggplot2
# ------------------------------------------------------------
library(ggplot2)

# Ensure TRT factor order matches SAS encounter order
XX$TRT <- factor(XX$TRT, levels = c("Study Drug", "Placebo", "AC"))

# Create grouping for boxplot: each VIS value is distinct, but we want boxes at those VIS positions
# Use interaction(VIS, TRT) as group
p <- ggplot(XX, aes(x = VIS, y = X, group = interaction(VIS, TRT), fill = TRT)) +
  geom_boxplot(
    colour = "blue",
    outlier.colour = "red",
    outlier.shape = 8,  # star
    outlier.size = 2.5,
    width = 0.12,
    lwd = 0.6
  ) +
  scale_x_continuous(
    name = "Visit",
    breaks = 3:6,
    limits = c(2.8, 6.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(name = "X") +
  scale_fill_manual(
    values = c("Study Drug" = "white", "Placebo" = "#D0E7FF", "AC" = "#FFD0D0"),
    name = "Treatment"
  ) +
  ggtitle("Box Plot of X over Time by Treatment") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    legend.position = "bottom",
    plot.margin = margin(10, 15, 10, 10)
  )

# Add manual symbols for legend to mimic SYMBOL statements (PLUS, TRIANGLE, SQUARE)
# We keep fill legend; shape is shown via boxplot fill, but we add a caption
p <- p + labs(caption = "Symbols: Study Drug PLUS (black), Placebo TRIANGLE (blue), AC SQUARE (red) | Boxes: CBOX=BLUE, Outliers: STAR RED")

png_path <- file.path(out, "p031_f_plsr_box_plot.png")
ggsave(filename = png_path, plot = p, width = 9, height = 7, units = "in", dpi = 300)

# Also ensure CSV of plotted data is written (already done)
cat(sprintf("Outputs written to %s\n", out))
cat(sprintf(" PNG: %s\n", png_path))
cat(sprintf(" CSV: %s\n", csv_path))
cat(sprintf(" TXT: %s\n", txt_path))
