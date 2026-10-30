```r
# ------------------------------------------------------------
# R translation of SAS program P034_target13.sas
# Generates MeanData dataset, PNG plot (target13.png) and CSV of plotted data
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading XPT files
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(stringr)    # string utilities
library(tidyr)      # data tidying
library(lubridate)  # date handling
library(readr)      # CSV I/O
library(purrr)      # functional tools
library(tibble)     # tibble helpers

# ------------------------------------------------------------------
# Define helper functions
# ------------------------------------------------------------------
# SAS round-half-up (not needed for this program but kept for completeness)
sas_round <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  return(posneg * rounded)
}

# ------------------------------------------------------------------
# Set up paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR not set")

# Input file (macro &rawadam. points to the ADaM raw folder)
adpc_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"

# ------------------------------------------------------------------
# Read source ADPC dataset
# ------------------------------------------------------------------
adpc_raw <- read_xpt(adpc_path)

# ------------------------------------------------------------------
# Macro parameters (as called in the SAS program)
# ------------------------------------------------------------------
pctestcd_param <- "ANAL1"   # %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH);
meantype_param <- "A"      # Arithmetic
sd_param       <- "BOTH"   # Show both lower and upper error bars

# ------------------------------------------------------------------
# Data step equivalent to SAS DATA work.adpc;
# keep only needed variables and create derived columns
# ------------------------------------------------------------------
adpc <- adpc_raw %>%
  select(
    ANLPRNT, TRTAN, PCSPEC, PCTESTL, TRTA,
    DOSREFID, PCTESTCD, PCORRESU, PCORRES,
    USUBJID, PCORRESN, EPLTM
  ) %>%
  # Filter for the requested analyte
  filter(toupper(PCTESTCD) == toupper(pctestcd_param)) %>%
  # Create numeric time variable (PCTPT) from EPLTM (character numeric)
  mutate(
    PCTPT = as.numeric(EPLTM),
    ORDERBY = str_c(ANLPRNT, PCSPEC, PCTESTL, TRTAN, sep = ""),
    BYVAL6 = str_squish(str_c(str_to_title(tolower(PCTESTCD)), " Conc (", PCORRESU, ")"))
  ) %>%
  # Ensure ordering matches SAS (missing values first, case‑sensitive)
  arrange(TRTAN, TRTA, PCTPT)

# ------------------------------------------------------------------
# Compute means and standard deviations (PROC UNIVARIATE equivalent)
# ------------------------------------------------------------------
if (toupper(meantype_param) == "A") {
  mean_data <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      PCmean = if (all(is.na(PCORRESN))) NA_real_ else mean(PCORRESN, na.rm = TRUE),
      PCSD   = if (all(is.na(PCORRESN))) NA_real_ else sd(PCORRESN, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Replicate SAS logic: set mean & SD to missing when mean == 0
    mutate(
      PCmean = ifelse(!is.na(PCmean) & PCmean == 0, NA_real_, PCmean),
      PCSD   = ifelse(is.na(PCmean), NA_real_, PCSD)
    )
} else {
  stop("Only Arithmetic mean (MEANTYPE=A) is implemented in this translation.")
}

# ------------------------------------------------------------------
# Write MeanData dataset to CSV (SAS dataset output)
# ------------------------------------------------------------------
write.csv(
  mean_data,
  file = file.path(out_dir, "MeanData.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# Prepare data for plotting (same as MeanData)
# ------------------------------------------------------------------
plot_data <- mean_data %>%
  mutate(
    TRTAN = as.factor(TRTAN)  # for colour/legend handling
  )

# ------------------------------------------------------------------
# Create PNG figure (target13.png) replicating the SAS graph
# ------------------------------------------------------------------
png(
  filename = file.path(out_dir, "target13.png"),
  width = 2500, height = 1400, units = "px", res = 150
)

# Layout: two rows (linear view on top, semilog view below)
par(mfrow = c(2, 1), mar = c(5, 5, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

# ---- Linear view ------------------------------------------------
with(plot_data, {
  # Determine y‑limits (include error bars)
  y_max <- max(PCmean + PCSD, na.rm = TRUE)
  y_min <- min(PCmean - PCSD, na.rm = TRUE)
  y_lim <- c(0, y_max * 1.1)

  plot(
    NA, NA,
    xlim = c(0, 100), ylim = y_lim,
    xlab = "Time (h)",
    ylab = "Arithmetic mean (SD) Anal2 Conc (ng/mL )",
    main = "Linear view"
  )

  # Plot each treatment group
  trt_levels <- levels(plot_data$TRTAN)
  cols <- rainbow(length(trt_levels))
  for (i in seq_along(trt_levels)) {
    subd <- subset(plot_data, TRTAN == trt_levels[i])
    lines(subd$PCTPT, subd$PCmean, type = "b", col = cols[i], pch = 19)
    if (sd_param == "BOTH") {
      arrows(
        x0 = subd$PCTPT, y0 = subd$PCmean - subd$PCSD,
        x1 = subd$PCTPT, y1 = subd$PCmean + subd$PCSD,
        angle = 90, code = 3, length = 0.05, col = cols[i]
      )
    } else if (sd_param == "UPPER") {
      arrows(
        x0 = subd$PCTPT, y0 = subd$PCmean,
        x1 = subd$PCTPT, y1 = subd$PCmean + subd$PCSD,
        angle = 90, code = 3, length = 0.05, col = cols[i]
      )
    }
  }

  legend(
    "topright",
    legend = trt_levels,
    col = cols,
    lty = 1,
    pch = 19,
    pt.cex = 1,
    cex = 0.9,
    bty = "n",
    title = "TRTAN"
  )
})

# ---- Semilogarithmic view ----------------------------------------
with(plot_data, {
  # y‑axis on log10 scale; avoid non‑positive values
  pos_idx <- which(PCmean > 0)
  y_max <- max(PCmean[pos_idx] + PCSD[pos_idx], na.rm = TRUE)
  y_min <- min(PCmean[pos_idx] - PCSD[pos_idx], na.rm = TRUE)
  y_lim <- c(y_min * 0.9, y_max * 1.1)

  plot(
    NA, NA,
    xlim = c(0, 100), ylim = y_lim,
    xlab = "Time (h)",
    ylab = "Arithmetic mean (SD) Anal2 Conc (ng/mL )",
    main = "Semilogarithmic view",
    log = "y"
  )

  trt_levels <- levels(plot_data$TRTAN)
  cols <- rainbow(length(trt_levels))
  for (i in seq_along(trt_levels)) {
    subd <- subset(plot_data, TRTAN == trt_levels[i] & PCmean > 0)
    lines(subd$PCTPT, subd$PCmean, type = "b", col = cols[i], pch = 19)
    # Error bars on log scale (same as linear; SAS uses raw values)
    if (sd_param == "BOTH") {
      arrows(
        x0 = subd$PCTPT, y0 = subd$PCmean - subd$PCSD,
        x1 = subd$PCTPT, y1 = subd$PCmean + subd$PCSD,
        angle = 90, code = 3, length = 0.05, col = cols[i]
      )
    } else if (sd_param == "UPPER") {
      arrows(
        x0 = subd$PCTPT, y0 = subd$PCmean,
        x1 = subd$PCTPT, y1 = subd$PCmean + subd$PCSD,
        angle = 90, code = 3, length = 0.05, col = cols[i]
      )
    }
  }

  legend(
    "topright",
    legend = trt_levels,
    col = cols,
    lty = 1,
    pch = 19,
    pt.cex = 1,
    cex = 0.9,
    bty = "n",
    title = "TRTAN"
  )
})

dev.off()

# ------------------------------------------------------------------
# Write plotted data (same as MeanData) to CSV for the figure
# ------------------------------------------------------------------
write.csv(
  plot_data,
  file = file.path(out_dir, "target13_data.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
```