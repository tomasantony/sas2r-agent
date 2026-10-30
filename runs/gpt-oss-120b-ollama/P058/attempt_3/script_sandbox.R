# ------------------------------------------------------------
# R translation of SAS program P058_pk_overlay_conc.sas
# Generates six PNG figures (sgrender.png … sgrender5.png) 
# and corresponding CSV data files in OUT_DIR.
# ------------------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(haven)      # read XPORT files
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(grid)       # arranging multiple panels

# ------------------------------------------------------------------
# 1. Paths and environment
# ------------------------------------------------------------------
# Source XPT file (hard‑coded from the SAS macro variables)
source_xpt <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"

# Output directory (provided by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" || !dir.exists(out_dir)) {
  stop("OUT_DIR environment variable is not set or points to a non‑existent directory.")
}

# ------------------------------------------------------------------
# 2. Helper: SAS‑style COMPRESS + INPUT (numeric extraction)
# ------------------------------------------------------------------
extract_numeric <- function(str, chars_to_remove) {
  # Remove the specified characters *and* spaces (SAS COMPRESS removes spaces by default)
  pattern <- paste0("[", chars_to_remove, " ]")
  cleaned <- gsub(pattern, "", str, perl = TRUE)
  # Convert to numeric; non‑numeric results become NA
  as.numeric(cleaned)
}

# ------------------------------------------------------------------
# 3. Read the ADaM dataset and mimic the SAS DATA step
# ------------------------------------------------------------------
conc_raw <- read_xpt(source_xpt)

# Convert variable names to lower case for case‑insensitive handling
conc_raw <- rename_with(conc_raw, tolower)

# Keep only the variables referenced in the SAS program
conc_raw <- conc_raw %>%
  select(
    usubjid, trta, aperiodc, atpt, parcat3l, aval, adtm,
    saffn, pkfn, paexcfln, pclloq, parcat1
  )

# Create the working dataset (mirroring the SAS DATA step)
conc <- conc_raw %>%
  mutate(
    # t = input(compress(atpt,' HOURSPOSTDOSE'),best.);
    t = extract_numeric(atpt, "HOURSPOSTDOSE"),
    # concen = aval;
    concen = aval,
    # order = input(compress(parcat1,' DRUGANALPlasma'),best.);
    order = extract_numeric(parcat1, "DRUGANALPlasma")
  ) %>%
  # Apply LLOQ: if pclloq is not missing, replace concen with pclloq
  mutate(concen = ifelse(!is.na(pclloq), pclloq, concen)) %>%
  # Keep only columns needed for the plots
  select(usubjid, trta, t, concen, order) %>%
  arrange(usubjid, t)   # SAS PROC SORT BY usubjid t (missing values first automatically)

# ------------------------------------------------------------------
# 4. Plotting routine (side‑by‑side linear & semilog panels)
# ------------------------------------------------------------------
plot_pk_overlay <- function(df, png_path, csv_path) {
  # Linear view
  p_linear <- ggplot(df, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black") +
    labs(title = "Linear view", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  # Semilogarithmic view (log y‑axis)
  p_semi <- ggplot(df, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black") +
    scale_y_log10() +                     # default log10 scaling (SAS uses log base 10)
    labs(title = "Semilogarithmic view", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  # Combine panels side‑by‑side (2 columns, equal width)
  png(filename = png_path, width = 24, height = 12, units = "cm", res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2,
                                             widths = unit(c(0.5, 0.5), "null"))))
  print(ggplotGrob(p_linear), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(ggplotGrob(p_semi),   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
  
  # Write the plotted data to CSV (same column order as SAS dataset)
  write.csv(df, csv_path, row.names = FALSE, na = "")
}

# ------------------------------------------------------------------
# 5. Generate the six figures (order = 1,2,3 for each treatment A and C)
# ------------------------------------------------------------------
# SAS macro loop creates 3 plots for each treatment, resulting in 6 PNGs.
# Naming follows the SAS reference: sgrender.png, sgrender1.png … sgrender5.png
png_names <- c("sgrender.png",
               "sgrender1.png",
               "sgrender2.png",
               "sgrender3.png",
               "sgrender4.png",
               "sgrender5.png")

counter <- 0
for (trt in c("A", "C")) {
  for (ord in 1:3) {
    counter <- counter + 1
    png_file <- file.path(out_dir, png_names[counter])
    csv_file <- sub("\\.png$", ".csv", png_file)
    
    # Subset data exactly as the SAS WHERE clause: order=&I and trta="&TRTA"
    df_plot <- conc %>%
      filter(order == ord, trta == trt) %>%
      select(usubjid, trta, t, concen, order)
    
    if (nrow(df_plot) == 0) {
      # No data – create an empty plot with a placeholder message
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data", size = 6) +
        theme_void()
      png(filename = png_file, width = 24, height = 12, units = "cm", res = 300)
      print(empty_plot)
      dev.off()
      write.csv(df_plot, csv_file, row.names = FALSE, na = "")
    } else {
      plot_pk_overlay(df_plot, png_file, csv_file)
    }
  }
}
