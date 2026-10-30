# ------------------------------------------------------------
# R translation of SAS program P060_pk_subj_conc.sas
# ------------------------------------------------------------
# Load required packages
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(ggplot2)   # plotting
library(stringr)    # string handling (if needed)

# ------------------------------------------------------------------
# Define paths and environment
# ------------------------------------------------------------------
# Macro variables from SAS environment (hard‑coded as per description)
rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# Output directory (set by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# Helper: SAS‑style round‑half‑up (not required here but provided)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10^digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# ------------------------------------------------------------------
# 1. Read source XPT files
# ------------------------------------------------------------------
adsl_path <- file.path(rawadam, "adsl.xpt")
adpc_path <- file.path(rawadam, "adpc.xpt")

adsl <- read_xpt(adsl_path)
adpc <- read_xpt(adpc_path)

# ------------------------------------------------------------------
# 2. Sort ADPC as in PROC SORT
#    BY USUBJID parcat1 eactm
# ------------------------------------------------------------------
sortadpc <- adpc %>%
  arrange(USUBJID, parcat1, eactm)

# ------------------------------------------------------------------
# 3. Write the sorted dataset (WORK.SORTadpc) to CSV
# ------------------------------------------------------------------
write.csv(
  sortadpc,
  file = file.path(out_dir, "SORTadpc.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 4. Create the individual concentration‑time profile plot
#    (equivalent to PROC GPLOT with BY USUBJID parcat1)
# ------------------------------------------------------------------
# Axis labels as defined in the SAS AXIS statements
y_label <- "Conc (ug/mL)"
x_label <- "Time (hour)"

# Build ggplot
p <- ggplot(sortadpc, aes(x = eactm, y = AVAL, group = 1)) +
  geom_line(size = 1) +                     # LINE=1 WIDTH=2 in SAS → size 1 (relative)
  labs(
    title = "Figure 16.2.5-x.x Individual concentration-time profiles",
    subtitle = "by compound, matrix, analyte and [actual/randomised][treatments/group]",
    x = x_label,
    y = y_label
  ) +
  theme_minimal(base_size = 10) +          # Approximate HEIGHT=10pt
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  facet_grid(rows = vars(USUBJID), cols = vars(parcat1), scales = "free_y")

# Save the figure as PNG
png_file <- file.path(out_dir, "sasgraph.png")
ggsave(
  filename = png_file,
  plot = p,
  width = 10, height = 8, units = "in", dpi = 300
)

# ------------------------------------------------------------------
# 5. Write the data used for the plot to CSV
# ------------------------------------------------------------------
write.csv(
  sortadpc,
  file = file.path(out_dir, "sasgraph.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
