# ------------------------------------------------------------
# R translation of SAS program P033_target12.sas
# ------------------------------------------------------------
# Load required libraries (already installed in the environment)
library(haven)      # for reading XPT files
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(readr)      # writing CSVs

# ------------------------------------------------------------
# Setup output directory
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Define helper: read XPT from absolute path
read_xpt_file <- function(path) {
  # haven::read_xpt returns a tibble with appropriate types
  read_xpt(path)
}

# ------------------------------------------------------------
# Paths to source XPT files (as defined by macro variables)
rawadam   <- "/bench/data/raw/cdisc-pilot/adam"
adsl_path <- file.path(rawadam, "adsl.xpt")
adpc_path <- file.path(rawadam, "adpc.xpt")

# ------------------------------------------------------------
# Read source datasets
adsl <- read_xpt_file(adsl_path)
adpc <- read_xpt_file(adpc_path)

# ------------------------------------------------------------
# PROC SORT equivalent: sort ADPC by USUBJID, parcat1, eactm
# SAS sorts missing values first, case‑sensitive for character vars.
# dplyr::arrange follows the same rules for typical data.
SORTadpc <- adpc %>%
  arrange(USUBJID, parcat1, eactm)

# ------------------------------------------------------------
# Write the sorted dataset as CSV (SAS dataset output)
write.csv(
  SORTadpc,
  file = file.path(out_dir, "SORTadpc.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# Prepare data for the plot
# SAS GPLOT with BY USUBJID parcat1 creates a separate panel for each combination.
# We'll mimic this using ggplot2 facets (rows = USUBJID, cols = parcat1).

# Ensure the variables exist; if not, stop with informative message.
required_vars <- c("USUBJID", "parcat1", "eactm", "AVAL")
missing_vars <- setdiff(required_vars, names(SORTadpc))
if (length(missing_vars) > 0) {
  stop("Missing required variables in ADPC dataset: ", paste(missing_vars, collapse = ", "))
}

# Convert to data frame for ggplot (tibble works as well)
plot_data <- SORTadpc %>%
  select(USUBJID, parcat1, eactm, AVAL) %>%
  mutate(
    USUBJID = as.factor(USUBJID),
    parcat1 = as.factor(parcat1)
  )

# ------------------------------------------------------------
# Create the plot
p <- ggplot(plot_data, aes(x = eactm, y = AVAL, group = 1)) +
  geom_line(size = 0.7) +                     # LINE=1 WIDTH=2 in SAS (approx)
  labs(
    x = "Time (hour)",
    y = "Conc (ug/mL)",
    title = "Figure 16.2.5-x.x Individual concentration-time profiles"
  ) +
  theme_minimal(base_size = 10) +            # Approximate HEIGHT=10pt
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 10)
  ) +
  facet_grid(rows = vars(USUBJID), cols = vars(parcat1), scales = "free_y")

# ------------------------------------------------------------
# Save the figure as PNG
png_path <- file.path(out_dir, "sasgraph.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 10, height = 8, units = "in", dpi = 150
)

# ------------------------------------------------------------
# Save the plotted data (the data used for the graph) as CSV
write.csv(
  plot_data,
  file = file.path(out_dir, "sasgraph_data.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# End of script
