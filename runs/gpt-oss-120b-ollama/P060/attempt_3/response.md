```r
# ------------------------------------------------------------
# R translation of SAS program P060_pk_subj_conc.sas
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(haven)   # read_xpt
library(dplyr)   # data manipulation
library(ggplot2) # plotting

# ------------------------------------------------------------------
# 1. Define paths and output directory
# ------------------------------------------------------------------
# Macro variables from the SAS environment (hard‑coded as described)
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# Output directory supplied by the harness
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 2. Helper: SAS‑style round‑half‑up (not needed for this program)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  sign_x <- sign(x)
  abs_x  <- abs(x)
  factor <- 10^digits
  rounded <- floor(abs_x * factor + 0.5) / factor
  sign_x * rounded
}

# ------------------------------------------------------------------
# 3. Read ADaM XPT files
# ------------------------------------------------------------------
adsl_path <- file.path(rawadam, "adsl.xpt")
adpc_path <- file.path(rawadam, "adpc.xpt")

adsl <- read_xpt(adsl_path)   # read only to mirror SAS steps
adpc <- read_xpt(adpc_path)

# Convert column names to lower case for easier handling.
# SAS variable names are case‑insensitive, so this does not affect output validity.
adpc <- rename_all(adpc, tolower)

# ------------------------------------------------------------------
# 4. PROC SORT equivalent
#    BY USUBJID PARCAT1 eactm   (now usubjid, parcat1, eactm)
# ------------------------------------------------------------------
sortadpc <- adpc %>%
  arrange(usubjid, parcat1, eactm)

# ------------------------------------------------------------------
# 5. Write the sorted ADPC dataset (WORK.SORTadpc) as CSV
# ------------------------------------------------------------------
write.csv(
  sortadpc,
  file = file.path(out_dir, "SORTadpc.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# 6. Create the individual concentration‑time profile plot
#    (PROC GPLOT with BY USUBJID PARCAT1)
# ------------------------------------------------------------------
y_label <- "Conc (ug/mL)"
x_label <- "Time (hour)"

p <- ggplot(sortadpc, aes(x = eactm, y = aval, group = 1)) +
  geom_line(size = 1) +                     # LINE=1, WIDTH=2 → size 1 (relative)
  labs(
    title    = "Figure 16.2.5-x.x Individual concentration-time profiles",
    subtitle = "by compound, matrix, analyte and [actual/randomised][treatments/group]",
    x        = x_label,
    y        = y_label
  ) +
  theme_minimal(base_size = 10) +          # HEIGHT=10pt approximated by base_size
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  facet_grid(rows = vars(usubjid), cols = vars(parcat1), scales = "free_y")

# Save PNG with the exact name required by the benchmark
png_path <- file.path(out_dir, "sasgraph.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 10, height = 8, units = "in", dpi = 300
)

# ------------------------------------------------------------------
# 7. Write the data used for the plot to CSV (named after the figure)
# ------------------------------------------------------------------
write.csv(
  sortadpc,
  file = file.path(out_dir, "sasgraph.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```