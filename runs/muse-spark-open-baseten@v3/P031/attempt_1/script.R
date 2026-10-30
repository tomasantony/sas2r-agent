# P031 Boxplot group - R translation of P031_Boxplot_group.sas
# Generates synthetic data using SAS RANUNI/RANNOR logic and produces boxplot

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# Helper: SAS ROUND half away from zero to nearest unit
sas_round <- function(x, unit) {
  # SAS ROUND(x, unit): round x to nearest multiple of unit, half away from zero
  # unit >0
  ifelse(x >= 0,
         floor(x / unit + 0.5 + 1e-12) * unit,
         ceiling(x / unit - 0.5 - 1e-12) * unit)
}

# Helper: modular multiplication (a*b) mod m without overflow (binary method)
mod_mult <- function(a, b, m) {
  # a,b,m < 2^31, product may overflow double exact integer
  # use binary doubling
  result <- 0
  a_mod <- a %% m
  b_tmp <- b
  while (b_tmp > 0) {
    if (b_tmp %% 2 == 1) {
      result <- (result + a_mod) %% m
    }
    a_mod <- (a_mod * 2) %% m
    b_tmp <- floor(b_tmp / 2)
  }
  result
}

# SAS RANUNI / RANNOR emulation
# RANUNI: seed_{n+1} = (397204094 * seed_n) mod 2147483647, uniform = seed / 2147483647
# RANNOR: Box-Muller, returns one of pair per call, saves second
sas_normals <- function(n, seed_init = 777) {
  m <- 2147483647
  a <- 397204094
  seed <- seed_init
  has_saved <- FALSE
  saved_z <- 0
  out <- numeric(n)
  idx <- 1
  while (idx <= n) {
    if (has_saved) {
      out[idx] <- saved_z
      has_saved <- FALSE
      idx <- idx + 1
    } else {
      # generate two uniforms
      seed <- mod_mult(a, seed, m)
      u1 <- seed / m
      seed <- mod_mult(a, seed, m)
      u2 <- seed / m
      # avoid log(0) - u1 never 0 because seed in 1..m-1
      r <- sqrt(-2 * log(u1))
      theta <- 2 * pi * u2
      z0 <- r * cos(theta)
      z1 <- r * sin(theta)
      out[idx] <- z0
      idx <- idx + 1
      if (idx <= n) {
        # save z1 for next call, but if we need only one more we could still save
        # Actually SAS saves and returns on next call, so we save
        saved_z <- z1
        has_saved <- TRUE
      } else {
        # last element was z0, z1 is discarded (but would be saved for next call beyond n)
        # keep state if needed
        saved_z <- z1
        has_saved <- TRUE
      }
    }
  }
  out
}

# Generate data exactly as SAS DATA X
# DO TRT="Study Drug","Placebo","AC";
# DO J=1 TO 10;
# SUBJID=J+100*(TRT="Study Drug")+200*(TRT="Placebo")+300*(TRT="AC");
# DO VISIT=3 TO 6;
# X=ROUND(J+2*NORMAL(777),0.1);

trt_levels <- c("Study Drug", "Placebo", "AC")
n_per_trt <- 10
visits <- 3:6
total_n <- length(trt_levels) * n_per_trt * length(visits) # 120

norms <- sas_normals(total_n, seed_init = 777)

# Build data frame in SAS loop order
rows <- vector("list", total_n)
k <- 1
for (trt in trt_levels) {
  for (j in 1:10) {
    subjid <- j + 100 * (trt == "Study Drug") + 200 * (trt == "Placebo") + 300 * (trt == "AC")
    for (visit in visits) {
      z <- norms[k]
      x_raw <- j + 2 * z
      x <- sas_round(x_raw, 0.1)
      rows[[k]] <- data.frame(TRT = trt, J = j, SUBJID = subjid, VISIT = visit, X = x, stringsAsFactors = FALSE)
      k <- k + 1
    }
  }
}
X <- do.call(rbind, rows)

# DATA XX
XX <- X
XX$VIS <- NA_real_
for (i in seq_len(nrow(XX))) {
  trt <- XX$TRT[i]
  visit <- XX$VISIT[i]
  if (trt == "Study Drug") vis <- visit
  else if (trt == "Placebo") vis <- visit + 0.15
  else if (trt == "AC") vis <- visit + 0.30
  else vis <- visit
  if (visit %in% c(4,5)) vis <- vis - 0.15
  if (visit == 6) vis <- vis - 0.3
  XX$VIS[i] <- vis
}

# PROC SORT BY VIS
XX <- XX[order(XX$VIS), ]

# Write CSV of plotted data (as SAS dataset)
# Keep variable names similar to SAS: TRT, SUBJID, VISIT, X, VIS
# Also include J for traceability
write.csv(XX[, c("TRT","SUBJID","VISIT","VIS","X")],
          file.path(out, "p031_f_plsr_box_plot.csv"),
          row.names = FALSE, na = "")

# Also write full XX for reference
write.csv(XX, file.path(out, "XX.csv"), row.names = FALSE, na = "")

# Compute SAS PCTLDEF=5 quantiles for boxplot verification
sas_quantile_pctldef5 <- function(x, p) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (p <= 0) return(x[1])
  if (p >= 1) return(x[n])
  np <- n * p
  j <- floor(np)
  g <- np - j
  # tolerance for integer check
  if (abs(g) < 1e-12) {
    # g == 0
    if (j == 0) return(x[1])
    if (j >= n) return(x[n])
    return((x[j] + x[j+1]) / 2)
  } else {
    k <- ceiling(np)
    if (k < 1) k <- 1
    if (k > n) k <- n
    return(x[k])
  }
}

# Compute boxplot stats per VIS (which corresponds to TRT*VISIT)
# For display we need per group
library(dplyr)

stats_list <- XX %>%
  group_by(TRT, VISIT, VIS) %>%
  summarise(
    n = n(),
    min = min(X, na.rm=TRUE),
    q1 = sas_quantile_pctldef5(X, 0.25),
    median = sas_quantile_pctldef5(X, 0.5),
    q3 = sas_quantile_pctldef5(X, 0.75),
    max = max(X, na.rm=TRUE),
    mean = mean(X, na.rm=TRUE),
    sd = sd(X, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iqr = q3 - q1,
    lower_fence = q1 - 1.5 * iqr,
    upper_fence = q3 + 1.5 * iqr
  )

# Determine whiskers and outliers per SAS SCHEMATIC
# lower whisker = smallest observation >= lower_fence
# upper whisker = largest observation <= upper_fence
whiskers <- XX %>%
  group_by(TRT, VISIT, VIS) %>%
  group_modify(function(df, key) {
    q1 <- sas_quantile_pctldef5(df$X, 0.25)
    q3 <- sas_quantile_pctldef5(df$X, 0.75)
    iqr <- q3 - q1
    lf <- q1 - 1.5 * iqr
    uf <- q3 + 1.5 * iqr
    lower_whisker <- min(df$X[df$X >= lf], na.rm=TRUE)
    upper_whisker <- max(df$X[df$X <= uf], na.rm=TRUE)
    # handle case where no observation inside fence (should not happen)
    if (is.infinite(lower_whisker)) lower_whisker <- q1
    if (is.infinite(upper_whisker)) upper_whisker <- q3
    outliers <- df$X[df$X < lf | df$X > uf]
    tibble(lower_whisker = lower_whisker,
           upper_whisker = upper_whisker,
           n_outliers = length(outliers))
  }) %>%
  ungroup()

stats_full <- left_join(stats_list, whiskers, by = c("TRT","VISIT","VIS"))

# Write stats CSV for QC
write.csv(stats_full, file.path(out, "p031_f_plsr_box_plot_stats.csv"), row.names = FALSE, na = "")

# Identify outliers for plotting
outliers_df <- XX %>%
  left_join(stats_full %>% select(TRT, VISIT, VIS, lower_fence, upper_fence), by = c("TRT","VISIT","VIS")) %>%
  filter(X < lower_fence | X > upper_fence)

# Prepare data for ggplot: need factor for VIS with numeric positions
# Use VIS as numeric x, TRT as fill/group
# Create boxplot data frame for geom_boxplot with precomputed stats
box_data <- stats_full %>%
  transmute(
    VIS = VIS,
    TRT = TRT,
    VISIT = VISIT,
    ymin = lower_whisker,
    lower = q1,
    middle = median,
    upper = q3,
    ymax = upper_whisker
  )

# Plot
library(ggplot2)

# Define colors to match SAS SYMBOL statements: Study Drug=PLUS BLACK, Placebo=TRIANGLE BLUE, AC=SQUARE RED
# For boxplot, CBOX=BLUE (box color blue), IDSYMBOL=STAR RED for outliers
# We will use fill white, colour blue for boxes, and outlier colour red

# Map TRT to shape for potential overlay (not strictly needed for boxplot)
shape_map <- c("Study Drug" = 2, "Placebo" = 6, "AC" = 0) # plus=3, triangle=2, square=0 in R pch
color_map <- c("Study Drug" = "black", "Placebo" = "blue", "AC" = "red")

p <- ggplot() +
  # boxplots
  geom_boxplot(
    data = box_data,
    aes(x = VIS, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax, group = VIS),
    stat = "identity",
    fill = "white",
    colour = "blue",
    linewidth = 0.6,
    outlier.shape = NA
  ) +
  # median line is already drawn; ensure whiskers
  # outliers as red stars
  geom_point(
    data = outliers_df,
    aes(x = VIS, y = X),
    shape = 8, # star
    colour = "red",
    size = 2.5
  ) +
  # optional: show means as plus? SAS BOXCONNECT=MEAN commented out, so not needed
  scale_x_continuous(
    name = "Visit",
    breaks = 3:6,
    limits = c(2.8, 6.2),
    labels = as.character(3:6)
  ) +
  scale_y_continuous(name = "X") +
  ggtitle("Box Plot of X over Time by Treatment") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  # Add legend for TRT via manual points (to mimic SYMBOL1-3)
  geom_point(
    data = data.frame(VIS = rep(NA_real_, 3), X = rep(NA_real_, 3), TRT = trt_levels),
    aes(x = VIS, y = X, shape = TRT, colour = TRT),
    size = 3
  ) +
  scale_shape_manual(values = c("Study Drug" = 3, "Placebo" = 2, "AC" = 0)) +
  scale_colour_manual(values = c("Study Drug" = "black", "Placebo" = "blue", "AC" = "red"))

# Save PNG 9in x 7in as per GOPTIONS HSIZE=9 VSIZE=7, DEV=SASEMF (vector)
# Use 300 dpi
ggsave(file.path(out, "p031_f_plsr_box_plot.png"), plot = p, width = 9, height = 7, units = "in", dpi = 300)

# Also save CSV of plotted data for figure (already done)
# Create TXT rendering of table/figure summary
txt_path <- file.path(out, "p031_f_plsr_box_plot.txt")
sink(txt_path)
cat("Box Plot of X over Time by Treatment\n")
cat("==================================================\n\n")
cat("Figure: Box Plot of X over Time by Treatment\n")
cat("X-axis: Visit (3 to 6)  Y-axis: X\n")
cat("Groups: Study Drug (PLUS black), Placebo (TRIANGLE blue), AC (SQUARE red)\n")
cat("BoxStyle: SCHEMATICID  CBOX=BLUE  IDSYMBOL=STAR RED\n\n")
cat("Data: Synthetic data generated with SAS NORMAL(777) seed\n")
cat(sprintf("Total observations: %d\n", nrow(XX)))
cat(sprintf("Groups: %d boxes (3 treatments x 4 visits, n=10 per box)\n\n", nrow(stats_full)))
cat("Boxplot Statistics (PCTLDEF=5, 1.5*IQR whiskers):\n")
cat("--------------------------------------------------------------------------------------------\n")
cat(sprintf("%-12s %5s %6s %6s %6s %6s %6s %6s %6s %6s %6s\n",
            "TRT","VISIT","VIS","N","Q1","Median","Q3","Mean","SD","LowWhis","UppWhis"))
cat("--------------------------------------------------------------------------------------------\n")
for (i in seq_len(nrow(stats_full))) {
  r <- stats_full[i,]
  cat(sprintf("%-12s %5d %6.2f %6d %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
              r$TRT, r$VISIT, r$VIS, r$n, r$q1, r$median, r$q3, r$mean, r$sd, r$lower_whisker, r$upper_whisker))
}
cat("--------------------------------------------------------------------------------------------\n\n")
cat("Outliers (beyond 1.5*IQR):\n")
if (nrow(outliers_df) == 0) {
  cat("None\n")
} else {
  cat(sprintf("%-12s %5s %6s %6s\n", "TRT","VISIT","VIS","X"))
  for (i in seq_len(nrow(outliers_df))) {
    r <- outliers_df[i,]
    cat(sprintf("%-12s %5d %6.2f %6.1f\n", r$TRT, r$VISIT, r$VIS, r$X))
  }
}
cat("\n")
cat("Note: VIS offsets: Study Drug=VISIT, Placebo=VISIT+0.15, AC=VISIT+0.30\n")
cat("      Adjusted: VISIT 4,5 minus 0.15; VISIT 6 minus 0.30\n")
cat("      Resulting VIS values: 3,3.15,3.30,3.85,4.00,4.15,4.85,5.00,5.15,5.70,5.85,6.00\n")
sink()

# Also ensure a CSV of the plotted data is available under expected name
# Already written as p031_f_plsr_box_plot.csv
# For compatibility, also write with uppercase if needed
# Done
