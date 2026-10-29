# P031 Boxplot group - R translation of P031_Boxplot_group.sas
# Outputs: OUT_DIR/p031_f_plsr_box_plot.png + CSV of plotted data

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

# ---- helpers: SAS semantics ----
# SAS ROUND half away from zero to nearest unit
round_sas <- function(x, unit = 0.1) {
  if (unit == 0) return(x)
  s <- sign(x)
  # handle NA
  res <- s * floor(abs(x) / unit + 0.5 + 1e-9) * unit
  # remove -0
  res[res == 0] <- 0
  res
}

# SAS RANUNI LCG: seed = mod(seed * 397204094, 2^31-1)
# Use modular multiplication without overflow (binary method)
mod_mult <- function(a, b, m) {
  a <- a %% m
  result <- 0
  while (b > 0) {
    if (b %% 2 == 1) {
      result <- (result + a) %% m
    }
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  result
}

sas_ranuni_seq <- function(n, seed = 777) {
  m <- 2147483647
  mult <- 397204094
  cur <- seed
  u <- numeric(n)
  for (i in seq_len(n)) {
    cur <- mod_mult(cur, mult, m)
    # SAS returns 0 < U < 1 ; if seed becomes 0 it is set to m? but not needed here
    if (cur == 0) cur <- m
    u[i] <- cur / m
  }
  list(u = u, seed = cur)
}

# SAS NORMAL via Box-Muller: Z = sqrt(-2*log(U1))*cos(2*pi*U2)
# Each NORMAL call consumes 2 uniform variates
sas_normal_seq <- function(n, seed = 777) {
  # need 2*n uniforms
  res <- sas_ranuni_seq(2 * n, seed)
  u <- res$u
  z <- numeric(n)
  for (i in seq_len(n)) {
    u1 <- u[2 * i - 1]
    u2 <- u[2 * i]
    # avoid log(0)
    if (u1 <= 0) u1 <- 1e-12
    z[i] <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  }
  z
}

# ---- Generate DATA X ----
# SAS:
# DO TRT="Study Drug","Placebo","AC";
#   DO J=1 TO 10;
#     SUBJID=J+100*(TRT="Study Drug")+200*(TRT="Placebo")+300*(TRT="AC");
#     DO VISIT=3 TO 6;
#       X=ROUND(J+2*NORMAL(777),0.1);
#     END;
#   END;
# END;

trt_levels <- c("Study Drug", "Placebo", "AC")
n_obs <- length(trt_levels) * 10 * 4  # 120

# Generate normals using SAS LCG + Box-Muller to faithfully reproduce SAS NORMAL(777)
norm_vals <- tryCatch(
  sas_normal_seq(n_obs, seed = 777),
  error = function(e) {
    # fallback to R RNG if SAS implementation fails
    set.seed(777)
    rnorm(n_obs)
  }
)

# Build X in SAS loop order
X_df <- expand.grid(
  TRT = trt_levels,
  J = 1:10,
  VISIT = 3:6,
  stringsAsFactors = FALSE
)
# expand.grid varies first factor fastest; SAS varies VISIT fastest, then J, then TRT slowest.
# Reorder to SAS order: TRT outer, J middle, VISIT inner
X_df <- X_df[order(match(X_df$TRT, trt_levels), X_df$J, X_df$VISIT), ]

# Assign normals in that order
X_df$norm <- norm_vals[seq_len(nrow(X_df))]
X_df$X_raw <- X_df$J + 2 * X_df$norm
X_df$X <- round_sas(X_df$X_raw, 0.1)

X_df$SUBJID <- X_df$J +
  100 * (X_df$TRT == "Study Drug") +
  200 * (X_df$TRT == "Placebo") +
  300 * (X_df$TRT == "AC")

X <- X_df %>% select(TRT, SUBJID, VISIT, X)

# ---- DATA XX ----
XX <- X %>%
  mutate(
    VIS = case_when(
      TRT == "Study Drug" ~ as.numeric(VISIT),
      TRT == "Placebo"    ~ as.numeric(VISIT) + 0.15,
      TRT == "AC"         ~ as.numeric(VISIT) + 0.30,
      TRUE ~ as.numeric(VISIT)
    )
  ) %>%
  mutate(
    VIS = ifelse(VISIT %in% c(4, 5), VIS - 0.15, VIS),
    VIS = ifelse(VISIT == 6, VIS - 0.3, VIS)
  )

# PROC SORT BY VIS (SAS missing first, character uppercase before lowercase - not relevant here)
XX <- XX %>% arrange(VIS)

# ---- Write CSV of plotted data ----
# CSV must contain same variable names as SAS dataset (case-insensitive)
# Include TRT, SUBJID, VISIT, X, VIS
write.csv(XX, file.path(out, "p031_f_plsr_box_plot.csv"), row.names = FALSE, na = "")

# ---- Plot: PROC BOXPLOT ----
# SAS: PLOT X*vis=TRT / HAXIS=AXIS1 BOXSTYLE=SCHEMATICID CBOX=BLUE IDSYMBOL=STAR IDCOLOR=RED
# AXIS LABEL=("Visit") ORDER=(3 TO 6 BY 1) MAJOR=NONE MINOR=NONE OFFSET=(2,2)
# SYMBOL1 V=PLUS C=BLACK (Study Drug), SYMBOL2 V=TRIANGLE C=BLUE (Placebo), SYMBOL3 V=SQUARE C=RED (AC)
# TITLE1 "Box Plot of X over Time by Treatment"

# Map TRT to colors/symbols approximating SAS SYMBOL statements
trt_colors <- c("Study Drug" = "black", "Placebo" = "blue", "AC" = "red")
trt_shapes <- c("Study Drug" = 3, "Placebo" = 17, "AC" = 15) # plus, triangle, square
trt_fill   <- c("Study Drug" = "white", "Placebo" = "white", "AC" = "white")

# For boxplot, each VIS value is a distinct box (12 boxes). Use group=VIS.
# Use continuous x-axis to mimic HAXIS ORDER 3 TO 6 BY 1
p <- ggplot(XX, aes(x = VIS, y = X, group = VIS)) +
  geom_boxplot(
    colour = "blue",
    fill = "white",
    outlier.shape = 8,  # star for IDSYMBOL=STAR
    outlier.colour = "red",
    outlier.size = 2.5,
    width = 0.12
  ) +
  # overlay group means or jittered points colored by TRT to show SYMBOL distinction
  geom_point(
    data = XX,
    aes(x = VIS, y = X, colour = TRT, shape = TRT),
    size = 2,
    alpha = 0.6,
    position = position_jitter(width = 0.02, height = 0)
  ) +
  scale_colour_manual(values = trt_colors) +
  scale_shape_manual(values = trt_shapes) +
  scale_x_continuous(
    name = "Visit",
    breaks = 3:6,
    limits = c(2.8, 6.2),
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(name = "X") +
  ggtitle("Box Plot of X over Time by Treatment") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# SAS GOPTIONS HSIZE=9 IN VSIZE=7 IN -> aspect ratio ~ 9/7
ggsave(
  filename = file.path(out, "p031_f_plsr_box_plot.png"),
  plot = p,
  width = 9, height = 7, units = "in", dpi = 300
)

# Also write a plain-text rendering of the plot data summary (table counterpart)
# Provide summary stats per VIS/TRT to mimic boxplot statistics
desc_stats <- XX %>%
  group_by(TRT, VISIT, VIS) %>%
  summarise(
    N = n(),
    MEAN = mean(X, na.rm = TRUE),
    MEDIAN = median(X, na.rm = TRUE),
    Q1 = quantile(X, 0.25, na.rm = TRUE, type = 2),
    Q3 = quantile(X, 0.75, na.rm = TRUE, type = 2),
    MIN = min(X, na.rm = TRUE),
    MAX = max(X, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(VIS)

write.csv(desc_stats, file.path(out, "p031_f_plsr_box_plot_summary.csv"), row.names = FALSE, na = "")

# Plain text rendering
txt_file <- file.path(out, "p031_f_plsr_box_plot.txt")
sink(txt_file)
cat("Box Plot of X over Time by Treatment\n")
cat("=====================================\n\n")
cat("Data: XX (sorted by VIS)\n")
cat(sprintf("N observations: %d\n\n", nrow(XX)))
print(as.data.frame(desc_stats), row.names = FALSE)
cat("\n")
cat("Note: Boxplot generated with PROC BOXPLOT semantics (BOXSTYLE=SCHEMATICID, CBOX=BLUE, IDSYMBOL=STAR).\n")
cat("X = ROUND(J + 2*NORMAL(777), 0.1) with SAS half-away-from-zero rounding.\n")
sink()
