# ------------------------------------------------------------
# R translation of SAS program P095_WaterFall_94.sas
# Generates the same dummy data, sorts it, and creates the
# waterfall plot (TumorSize_Band_SG_V94.png) with accompanying CSV.
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------
# Helper: SAS-like uniform random number generator (ranuni)
# Uses the same LCG algorithm as SAS with seed 2.
# ------------------------------------------------------------------
sas_ranuni <- function(n, seed) {
  mod <- 2^31
  vals <- numeric(n)
  s <- seed
  for (i in seq_len(n)) {
    s <- (s * 69069) %% mod
    vals[i] <- s / mod
  }
  list(values = vals, seed = s)
}

# ------------------------------------------------------------------
# Set up output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Create dummy data (equivalent to SAS DATA step)
# ------------------------------------------------------------------
set.seed(NULL)  # ensure no R RNG interference

# Generate 50 uniform numbers (2 per observation) using SAS algorithm
ran <- sas_ranuni(50, seed = 2)
u <- ran$values

group_vec <- c("Treatment 2", "Treatment 1")
label_vec <- c("E", "C", "R", "S", "P")

tumor_df <- tibble(
  i = 1:25,
  Cid = formatC(1:25, width = 2, flag = " "),               # same as PUT(i,2.)
  Change = 30 - 120 * u[seq(1, 49, by = 2)],                # first uniform per obs
  Group = {
    grp_idx <- floor(u[seq(2, 50, by = 2)] + 0.5) + 1       # second uniform per obs
    group_vec[grp_idx]
  },
  Label = label_vec[( (1:25) %% 5 ) + 1]                    # MOD(i,5)+1 logic
) %>%
  select(-i)  # drop loop index (like DROP i)

# ------------------------------------------------------------------
# 2. Sort descending by Change (SAS PROC SORT DESCENDING)
# ------------------------------------------------------------------
tumor_desc <- tumor_df %>%
  arrange(desc(Change))

# Write the sorted dataset as CSV (SAS would have created TumorSizeDesc)
write.csv(
  tumor_desc,
  file = file.path(out_dir, "TumorSizeDesc.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 3. Build the waterfall plot (SAS PROC SGPLOT)
# ------------------------------------------------------------------
# Preserve the order of Cid as it appears after sorting
tumor_desc <- tumor_desc %>%
  mutate(Cid = factor(Cid, levels = Cid))

# Data for the background band (one rectangle per category)
band_df <- tumor_desc %>%
  mutate(
    x_num = as.numeric(Cid),
    xmin = x_num - 0.5,
    xmax = x_num + 0.5,
    ymin = -30,
    ymax = 20
  )

# Plot
p <- ggplot() +
  # Band (transparent fill, no outline)
  geom_rect(
    data = band_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray", alpha = 0.5, colour = NA
  ) +
  # Bars (clustered by Group)
  geom_col(
    data = tumor_desc,
    aes(x = Cid, y = Change, fill = Group),
    position = position_dodge(width = 0.9),
    width = 0.8,
    colour = NA
  ) +
  # Data labels on bars
  geom_text(
    data = tumor_desc,
    aes(x = Cid, y = Change, label = Label, group = Group),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3,
    fontface = "bold"
  ) +
  # Manual colors to match SAS (cxbf0000 = "#BF0000")
  scale_fill_manual(values = c("Treatment 2" = "#BF0000", "Treatment 1" = "gold")) +
  # Titles
  ggtitle("Change in Tumor Size") +
  labs(subtitle = "ITT Population") +
  # X axis removed
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  # Y axis settings (descending values, custom grid)
  scale_y_continuous(
    breaks = seq(60, -100, by = -20),
    limits = c(60, -100),
    expand = c(0, 0)
  ) +
  theme(
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank()
  ) +
  # Legend placement (inside top‑right, no title)
  theme(
    legend.position = c(0.95, 0.95),
    legend.justify = c("right", "top"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", colour = "black")
  ) +
  # Inset text (BCR legend)
  annotate(
    "text",
    x = -Inf, y = -Inf,
    hjust = -0.02, vjust = -0.02,
    label = paste0(
      "C= Complete Response\n",
      "R= Partial Response\n",
      "S= Stable Disease\n",
      "P= Progressive Disease\n",
      "E= Early Death"
    ),
    size = 3,
    fontface = "bold",
    family = "sans",
    colour = "black",
    lineheight = 1.1,
    parse = FALSE,
    # Title for inset
    label2 = "BCR"
  ) +
  # Add inset title (SAS puts title='BCR' above the lines)
  annotate(
    "text",
    x = -Inf, y = -Inf,
    hjust = -0.02, vjust = 0.12,
    label = "BCR",
    size = 3.5,
    fontface = "bold"
  )

# Save plot as PNG (5in x 3in, 300 DPI)
png_file <- file.path(out_dir, "tumorsize_band_sg_v94.png")
ggsave(
  filename = png_file,
  plot = p,
  width = 5, height = 3, units = "in", dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------
# 4. Export the data used for the plot (CSV)
# ------------------------------------------------------------------
write.csv(
  tumor_desc,
  file = file.path(out_dir, "tumorsize_band_sg_v94.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
