# f_waterfall.R - translation of P082_f_waterfall.sas
# -------------------------------------------------
# Reads ADaM datasets, creates a waterfall plot, and writes outputs.
# Assumes required packages are already installed.

library(haven)      # read SAS datasets
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(stringr)    # string handling
library(readr)      # write CSV with proper NA handling

# ----------------------------------------------------------------------
# Helper: round-half-up (not needed here but defined for completeness)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}

# ----------------------------------------------------------------------
# Directories (environment variables set by the benchmark)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

adam_path <- "/bench/data/raw/own-synthetic/adam"

# ----------------------------------------------------------------------
# Read ADaM datasets ----------------------------------------------------
adtr_file <- file.path(adam_path, "adtr.sas7bdat")
adsl_file <- file.path(adam_path, "adsl.sas7bdat")

adtr <- read_sas(adtr_file)
adsl <- read_sas(adsl_file)

# ----------------------------------------------------------------------
# Step 1: Get best percentage change per subject -----------------------
tumor <- adtr %>%
  filter(PARAMCD == "BPCHG") %>%                     # best percent change
  inner_join(adsl %>% filter(EFFFL == "Y"), by = "USUBJID") %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC, EFFFL) %>%
  arrange(PCHG)                                      # order by percent change

# ----------------------------------------------------------------------
# Step 2: Assign display order and colors --------------------------------
waterfall <- tumor %>%
  mutate(
    bar_order = row_number(),
    bar_color = case_when(
      str_to_upper(AVALC) == "CR" ~ "darkgreen",
      str_to_upper(AVALC) == "PR" ~ "green",
      str_to_upper(AVALC) == "SD" ~ "gold",
      str_to_upper(AVALC) == "PD" ~ "red",
      TRUE                       ~ "gray"
    ),
    PCHG_display = case_when(
      PCHG > 100  ~ 100,
      PCHG < -100 ~ -100,
      TRUE        ~ PCHG
    )
  )

# ----------------------------------------------------------------------
# Write the data used for the plot ---------------------------------------
csv_path <- file.path(out_dir, "waterfall.csv")
write_csv(waterfall, csv_path, na = "")

# ----------------------------------------------------------------------
# Step 3: Generate waterfall plot -----------------------------------------
# Define colors for known response categories; others get gray
response_levels <- unique(waterfall$AVALC)
color_map <- setNames(
  rep("gray", length(response_levels)),
  response_levels
)
# Override known categories
color_map["CR"] <- "darkgreen"
color_map["PR"] <- "green"
color_map["SD"] <- "gold"
color_map["PD"] <- "red"

p <- ggplot(waterfall, aes(x = bar_order, y = PCHG_display, fill = AVALC)) +
  geom_col(width = 1, colour = "black", size = 0.5) +
  scale_fill_manual(values = color_map, name = "Best Overall Response") +
  geom_hline(yintercept = -30, linetype = "dashed", colour = "blue", size = 1) +
  geom_hline(yintercept = 20,  linetype = "dashed", colour = "red",  size = 1) +
  geom_hline(yintercept = 0,  colour = "black", size = 1) +
  labs(
    title    = "Figure 14.2.3",
    subtitle = "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
    y        = "Best Percentage Change from Baseline (%)"
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(
    limits = c(-100, 100),
    breaks = seq(-100, 100, 20)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.x   = element_blank(),
    axis.text.x    = element_blank(),
    axis.ticks.x   = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", colour = "lightgray"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(title = "Best Overall Response", nrow = 1, byrow = TRUE))

# ----------------------------------------------------------------------
# Save the figure ---------------------------------------------------------
png_path <- file.path(out_dir, "f_waterfall.png")
ggsave(
  filename = png_path,
  plot = p,
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 300,
  device = "png"
)

# ----------------------------------------------------------------------
# Completion message
cat("Waterfall figure and data have been written to", out_dir, "\n")
