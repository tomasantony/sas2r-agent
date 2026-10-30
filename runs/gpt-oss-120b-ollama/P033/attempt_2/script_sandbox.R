# ------------------------------------------------------------
# R translation of SAS program P033_target12.sas (attempt 2)
# ------------------------------------------------------------
# Load required libraries (pre‑installed in the execution environment)
library(haven)      # read XPORT files
library(dplyr)      # data manipulation
library(ggplot2)    # graphics
library(readr)      # CSV output

# ------------------------------------------------------------
# Set up output directory (environment variable OUT_DIR must be defined)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" || out_dir == "NULL") {
  stop("OUT_DIR environment variable is not set.")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helper to locate a variable in a data frame ignoring case
find_var <- function(df, target) {
  nm <- names(df)
  idx <- which(tolower(nm) == tolower(target))
  if (length(idx) == 0) {
    stop(paste0("Variable '", target, "' not found in dataset."))
  }
  nm[idx][1]   # return the exact name as stored in the data
}

# ------------------------------------------------------------
# Paths to the source XPT files (as defined by the original macro vars)
rawadam   <- "/bench/data/raw/cdisc-pilot/adam"
adsl_path <- file.path(rawadam, "adsl.xpt")
adpc_path <- file.path(rawadam, "adpc.xpt")

# ------------------------------------------------------------
# Read the ADaM datasets
adsl <- read_xpt(adsl_path)   # not used later but read for completeness
adpc <- read_xpt(adpc_path)

# ------------------------------------------------------------
# PROC SORT equivalent: sort ADPC by USUBJID, parcat1, eactm
# SAS sorts missing values first, case‑sensitive for character vars.
# We'll use dplyr::arrange with the exact column names.
var_usubjid <- find_var(adpc, "USUBJID")
var_parcat1 <- find_var(adpc, "parcat1")   # may be PARCAT1 in the file
var_eactm  <- find_var(adpc, "eactm")

SORTadpc <- adpc %>%
  arrange(.data[[var_usubjid]],
          .data[[var_parcat1]],
          .data[[var_eactm]])

# ------------------------------------------------------------
# Write the sorted dataset as CSV (matching SAS dataset name)
write.csv(
  SORTadpc,
  file = file.path(out_dir, "SORTadpc.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# Prepare data for the GPLOT equivalent
# Required variables: USUBJID, parcat1, eactm, AVAL
var_aval   <- find_var(SORTadpc, "AVAL")

plot_data <- SORTadpc %>%
  select(
    USUBJID = .data[[var_usubjid]],
    parcat1 = .data[[var_parcat1]],
    eactm   = .data[[var_eactm]],
    AVAL    = .data[[var_aval]]
  ) %>%
  mutate(
    USUBJID = as.factor(USUBJID),
    parcat1 = as.factor(parcat1)
  )

# ------------------------------------------------------------
# Create the figure (GPLOT with BY USUBJID parcat1)
# SAS GPLOT draws a line joining points (INTERPOL=JOIN) for each BY group.
# In ggplot we facet by USUBJID (rows) and parcat1 (columns) and draw a single line per panel.
p <- ggplot(plot_data, aes(x = eactm, y = AVAL, group = 1)) +
  geom_line(size = 0.7) +                     # LINE=1, WIDTH=2 approximated
  labs(
    x = "Time (hour)",
    y = "Conc (ug/mL)",
    title = "Figure 16.2.5-x.x Individual concentration-time profiles"
  ) +
  theme_minimal(base_size = 10) +            # HEIGHT=10pt approximated
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 10)
  ) +
  facet_grid(rows = vars(USUBJID), cols = vars(parcat1), scales = "free_y")

# ------------------------------------------------------------
# Save the figure as PNG (exact filename required by the benchmark)
png_path <- file.path(out_dir, "sasgraph.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 10, height = 8, units = "in", dpi = 150
)

# ------------------------------------------------------------
# Save the data that underlies the figure (CSV)
write.csv(
  plot_data,
  file = file.path(out_dir, "sasgraph_data.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
