# P043_Figure_11_1 translation to R
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(stringr)    # string utilities
library(readr)      # for write_csv with specific NA handling

# ------------------------------------------------------------
# Helper: round half up (SAS default) - not needed for this script
# ------------------------------------------------------------

# ------------------------------------------------------------
# Set up paths from environment variables (as defined by the benchmark)
root   <- "/bench"
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("OUT_DIR environment variable is not set.")
}
# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------
# 1. Read ADaM dataset ADLBC (SAS7BDAT) from the ADaM split library
# ------------------------------------------------------------
adlbc_path <- file.path(root, "data", "derived", "cdisc-pilot", "adam-split", "ADLBC.sas7bdat")
if (!file.exists(adlbc_path)) {
  stop("ADLBC dataset not found at: ", adlbc_path)
}
adlbc <- read_sas(adlbc_path)

# ------------------------------------------------------------
# 2. Subset to PARAMCD = "SODIUM"
# ------------------------------------------------------------
adlb_sodium <- adlbc %>%
  filter(PARAMCD == "SODIUM")

# ------------------------------------------------------------
# 3. Create START dataset:
#    - Keep only records where AENTMTFL = "Y" and AVISITN != 99
#    - Keep selected variables
# ------------------------------------------------------------
START <- adlb_sodium %>%
  filter(AENTMTFL == "Y", AVISITN != 99) %>%
  select(USUBJID, TRTAN, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI)

# Write START to CSV (as a dataset output)
write.csv(
  START,
  file = file.path(out_dir, "START.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 4. Determine LLN (most frequent A1LO) and ULN (most frequent A1HI)
# ------------------------------------------------------------
# Frequency for A1LO
low_freq <- START %>%
  group_by(PARAMCD, A1LO) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(PARAMCD, desc(COUNT), A1LO)

# Get first (most frequent) per PARAMCD
LLN_val <- low_freq %>%
  group_by(PARAMCD) %>%
  slice_head(n = 1) %>%
  pull(A1LO) %>%
  unique()

# Frequency for A1HI
high_freq <- START %>%
  group_by(PARAMCD, A1HI) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(PARAMCD, desc(COUNT), A1HI)

# Get first (most frequent) per PARAMCD
ULN_val <- high_freq %>%
  group_by(PARAMCD) %>%
  slice_head(n = 1) %>%
  pull(A1HI) %>%
  unique()

# In case there are multiple PARAMCD values, keep the first (SAS would set macro per last iteration)
LLN <- LLN_val[1]
ULN <- ULN_val[1]

# ------------------------------------------------------------
# 5. Create scatter plot (Figure 11.1) using ggplot2
# ------------------------------------------------------------
# Prepare data for plotting (same as START)
plot_data <- START %>%
  mutate(
    # Ensure numeric types (in case they were read as character)
    BASE = as.numeric(BASE),
    AVAL = as.numeric(AVAL),
    TRTA = as.factor(TRTA)
  ) %>%
  # Remove rows with missing x or y (SAS would drop them automatically)
  filter(!is.na(BASE) & !is.na(AVAL))

# Save plotted data as CSV
write.csv(
  plot_data,
  file = file.path(out_dir, "sgrender_plot_data.csv"),
  row.names = FALSE,
  na = ""
)

# Build the plot
p <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = TRTA, shape = TRTA)) +
  geom_point(size = 3) +
  # 45-degree reference line from (0,0) to (100,100)
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100),
               linetype = "solid", colour = "gray40") +
  # Horizontal reference lines (LLN and ULN)
  geom_hline(yintercept = LLN, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = ULN, linetype = "dashed", colour = "blue") +
  # Vertical reference lines (LLN and ULN)
  geom_vline(xintercept = LLN, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = ULN, linetype = "dashed", colour = "blue") +
  labs(
    title = "Figure 11.1 Scatterplot",
    subtitle = NULL,
    x = "Min. Baseline",
    y = "Min. Post Baseline",
    colour = "Treatment",
    shape = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# ------------------------------------------------------------
# 6. Save the figure as PNG
# ------------------------------------------------------------
png_path <- file.path(out_dir, "sgrender.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 9.51,   # designwidth in inches (SAS uses points; 1 point = 1/72 inch)
  height = 6.89,  # designheight in inches
  units = "in",
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 7. (Optional) Write a plain‑text rendering of the plot data
# ------------------------------------------------------------
txt_path <- file.path(out_dir, "sgrender.txt")
write_lines(
  capture.output(print(p)),
  txt_path
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
