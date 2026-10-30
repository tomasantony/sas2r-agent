# ------------------------------------------------------------
# R translation of SAS program P058_pk_overlay_conc.sas
# Generates six PNG figures and corresponding CSV data files
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading XPT files
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(grid)       # arranging multiple plots
library(tidyr)      # for data handling if needed

# Helper: round-half-up (SAS default) – not needed for this script but defined for completeness
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  return(posneg * z / 10^digits)
}

# ------------------------------------------------------------------
# 1. Define paths
# ------------------------------------------------------------------
# Absolute path to the source XPT file (hard‑coded from the SAS macro vars)
source_xpt <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"

# Output directory (environment variable set by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("OUT_DIR environment variable is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 2. Read and prepare the data (mirrors the SAS DATA step)
# ------------------------------------------------------------------
conc_raw <- read_xpt(source_xpt) %>%
  # Keep only the variables used in the SAS program
  select(usubjid, trta, aperiodc, atpt, parcat3l, aval, adtm,
         saffn, pkfn, paexcfln, pclloq, parcat1)

# Helper to extract numeric part from a character string by removing
# a set of characters (the SAS COMPRESS behaviour)
extract_numeric <- function(str, chars_to_remove) {
  # Remove the specified characters (case‑sensitive) and any spaces
  cleaned <- gsub(paste0("[", chars_to_remove, "]"), "", str, perl = TRUE)
  # Convert to numeric; non‑numeric results become NA
  as.numeric(cleaned)
}

conc <- conc_raw %>%
  mutate(
    # t = input(compress(atpt,' HOURSPOSTDOSE'),best.);
    t = extract_numeric(atpt, " HOURSPOSTDOSE"),
    # concen = aval;
    concen = aval,
    # order = input(compress(parcat1,' DRUGANALPlasma'),best.);
    order = extract_numeric(parcat1, " DRUGANALPlasma")
  ) %>%
  # Apply LLOQ: if pclloq is not missing, replace concen with pclloq
  mutate(concen = ifelse(!is.na(pclloq), pclloq, concen)) %>%
  # Keep only needed columns for plotting
  select(usubjid, trta, t, concen, order) %>%
  arrange(usubjid, t)   # SAS PROC SORT BY usubjid t (missing values first automatically)

# ------------------------------------------------------------------
# 3. Plotting function (creates side‑by‑side linear & semilog panels)
# ------------------------------------------------------------------
plot_pk_overlay <- function(df, out_png, out_csv) {
  # Linear view plot
  p_linear <- ggplot(df, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black") +
    labs(title = "Linear view", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  # Semilogarithmic view plot
  p_semi <- ggplot(df, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black") +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format10x())
    ) +
    labs(title = "Semilogarithmic view", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  # Combine the two panels side‑by‑side using grid layout
  png(filename = out_png, width = 24, height = 12, units = "cm", res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2,
                                             widths = unit(c(0.5, 0.5), "null"))))
  print(ggplotGrob(p_linear), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(ggplotGrob(p_semi),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
  
  # Write the data used for the plot to CSV (same columns as SAS dataset)
  write.csv(df, out_csv, row.names = FALSE, na = "")
}

# ------------------------------------------------------------------
# 4. Generate the six figures (order = 1:3, trta = "A" and "C")
# ------------------------------------------------------------------
counter <- 0  # to build the SGRender*.png naming scheme
for (trt in c("A", "C")) {
  for (ord in 1:3) {
    counter <- counter + 1
    # Build output file names
    png_name <- if (counter == 1) "SGRender.png" else paste0("SGRender", counter - 1, ".png")
    csv_name <- sub("\\.png$", ".csv", png_name)
    out_png_path <- file.path(out_dir, png_name)
    out_csv_path <- file.path(out_dir, csv_name)
    
    # Subset data as per the SAS WHERE clause
    df_plot <- conc %>%
      filter(order == ord, trta == trt) %>%
      select(usubjid, trta, t, concen, order)
    
    # If there is no data for this combination, still create an empty CSV
    # and a blank plot (to mimic SAS behaviour of producing an empty graph)
    if (nrow(df_plot) == 0) {
      # Create an empty plot with a message
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data", size = 6) +
        theme_void()
      png(filename = out_png_path, width = 24, height = 12, units = "cm", res = 300)
      print(empty_plot)
      dev.off()
      write.csv(df_plot, out_csv_path, row.names = FALSE, na = "")
    } else {
      plot_pk_overlay(df_plot, out_png_path, out_csv_path)
    }
  }
}
