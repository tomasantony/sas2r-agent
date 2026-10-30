# ------------------------------------------------------------
# R translation of SAS program P034_target13.sas (attempt 2)
# Generates MeanData dataset, PNG plot (target13.png) and CSV of plotted data
# ------------------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(haven)      # read XPORT files
library(dplyr)      # data manipulation
library(ggplot2)    # plotting (used for consistency, but base graphics for layout)
library(stringr)    # string utilities
library(tidyr)      # data tidying
library(lubridate)  # date handling
library(readr)      # CSV I/O
library(purrr)      # functional helpers
library(tibble)     # tibble helpers

# ------------------------------------------------------------------
# Helper: SAS‑style round‑half‑up (not needed here but kept)
# ------------------------------------------------------------------
sas_round <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR not set")

# Input ADPC XPT (macro &rawadam. points to this folder)
adpc_path <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"

# ------------------------------------------------------------------
# Read source ADPC dataset
# ------------------------------------------------------------------
adpc_raw <- read_xpt(adpc_path)

# ------------------------------------------------------------------
# Macro parameters (as invoked in the SAS program)
# ------------------------------------------------------------------
pctestcd_param <- "ANAL1"   # %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH);
meantype_param <- "A"       # Arithmetic mean
sd_param       <- "BOTH"    # Show both lower and upper error bars

# ------------------------------------------------------------------
# DATA step equivalent: keep variables, filter, create derived columns
# ------------------------------------------------------------------
adpc <- adpc_raw %>%
  select(
    ANLPRNT, TRTAN, PCSPEC, PCTESTL, TRTA,
    DOSREFID, PCTESTCD, PCORRESU, PCORRES,
    USUBJID, PCORRESN, EPLTM
  ) %>%
  # Keep only the requested analyte (case‑insensitive match as SAS does)
  filter(toupper(PCTESTCD) == toupper(pctestcd_param)) %>%
  mutate(
    PCTPT   = as.numeric(EPLTM),                                   # numeric time
    ORDERBY = str_c(ANLPRNT, PCSPEC, PCTESTL, TRTAN, sep = ""),   # not used later
    BYVAL6  = str_squish(str_c(str_to_title(tolower(PCTESTCD)),
                               " Conc (", PCORRESU, ")"))
  ) %>%
  # SAS sorts missing first, then by character (case‑sensitive)
  arrange(TRTAN, TRTA, PCTPT)

# ------------------------------------------------------------------
# Compute means / SDs (PROC UNIVARIATE analogue)
# ------------------------------------------------------------------
if (toupper(meantype_param) == "A") {
  mean_data <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      PCmean = if (all(is.na(PCORRESN))) NA_real_ else mean(PCORRESN, na.rm = TRUE),
      PCSD   = if (all(is.na(PCORRESN))) NA_real_ else sd(PCORRESN, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # SAS: if mean = 0 then set mean & SD to missing
    mutate(
      PCmean = ifelse(!is.na(PCmean) & PCmean == 0, NA_real_, PCmean),
      PCSD   = ifelse(is.na(PCmean), NA_real_, PCSD)
    )
} else {
  stop("Only Arithmetic mean (MEANTYPE=A) is implemented.")
}

# ------------------------------------------------------------------
# Write MeanData dataset (CSV) – matches SAS output dataset
# ------------------------------------------------------------------
write.csv(
  mean_data,
  file = file.path(out_dir, "MeanData.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# Prepare data for the figure (same as MeanData)
# ------------------------------------------------------------------
plot_data <- mean_data %>%
  mutate(TRTAN = as.factor(TRTAN))   # factor for colour/legend handling

# ------------------------------------------------------------------
# PNG figure: two panels (linear view on top, semilog view below)
# ------------------------------------------------------------------
png(
  filename = file.path(out_dir, "target13.png"),
  width = 2500, height = 1400, units = "px", res = 150
)

# Layout: 2 rows, 1 column
par(mfrow = c(2, 1), mar = c(5, 5, 4, 2) + 0.1)

# -------------------------------------------------
# 1) Linear view (replicates SAS “Linear view” panel)
# -------------------------------------------------
with(plot_data, {
  # Determine y‑limits including error bars (ignore NA)
  y_max <- max(PCmean + PCSD, na.rm = TRUE)
  y_lim <- c(0, y_max * 1.1)

  plot(
    NA, NA,
    xlim = c(0, 100), ylim = y_lim,
    xlab = "Time (h)",
    ylab = "Arithmetic mean (SD) Anal2 Conc (ng/mL )",
    main = "Linear view"
  )

  trt_levels <- levels(TRTAN)
  cols <- rainbow(length(trt_levels))

  for (i in seq_along(trt_levels)) {
    subd <- subset(plot_data, TRTAN == trt_levels[i] & !is.na(PCmean))
    if (nrow(subd) == 0) next

    lines(subd$PCTPT, subd$PCmean, type = "b", col = cols[i], pch = 19)

    # Error bars according to SD parameter
    if (sd_param == "BOTH") {
      ok <- !is.na(subd$PCSD)
      if (any(ok)) {
        arrows(
          x0 = subd$PCTPT[ok], y0 = subd$PCmean[ok] - subd$PCSD[ok],
          x1 = subd$PCTPT[ok], y1 = subd$PCmean[ok] + subd$PCSD[ok],
          angle = 90, code = 3, length = 0.05, col = cols[i]
        )
      }
    } else if (sd_param == "UPPER") {
      ok <- !is.na(subd$PCSD)
      if (any(ok)) {
        arrows(
          x0 = subd$PCTPT[ok], y0 = subd$PCmean[ok],
          x1 = subd$PCTPT[ok], y1 = subd$PCmean[ok] + subd$PCSD[ok],
          angle = 90, code = 3, length = 0.05, col = cols[i]
        )
      }
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

# -------------------------------------------------
# 2) Semilogarithmic view (replicates SAS “Semilogarithmic view” panel)
# -------------------------------------------------
with(plot_data, {
  # Keep only positive means for log scale (SAS would drop non‑positive)
  pos_data <- subset(plot_data, PCmean > 0 & !is.na(PCmean))

  if (nrow(pos_data) == 0) {
    # No positive data – create a dummy plot with a safe positive range
    y_lim <- c(0.1, 1)
    plot(
      NA, NA,
      xlim = c(0, 100), ylim = y_lim,
      xlab = "Time (h)",
      ylab = "Arithmetic mean (SD) Anal2 Conc (ng/mL )",
      main = "Semilogarithmic view",
      log = "y"
    )
  } else {
    # Determine y‑limits (positive only)
    y_max <- max(pos_data$PCmean + pos_data$PCSD, na.rm = TRUE)
    y_min_raw <- min(pos_data$PCmean - pos_data$PCSD, na.rm = TRUE)
    y_min <- if (y_min_raw <= 0) {
      # SAS would start a little above zero; use half the smallest positive mean
      min(pos_data$PCmean[pos_data$PCmean > 0]) * 0.5
    } else {
      y_min_raw
    }
    y_lim <- c(y_min * 0.9, y_max * 1.1)

    plot(
      NA, NA,
      xlim = c(0, 100), ylim = y_lim,
      xlab = "Time (h)",
      ylab = "Arithmetic mean (SD) Anal2 Conc (ng/mL )",
      main = "Semilogarithmic view",
      log = "y"
    )

    trt_levels <- levels(TRTAN)
    cols <- rainbow(length(trt_levels))

    for (i in seq_along(trt_levels)) {
      subd <- subset(pos_data, TRTAN == trt_levels[i])
      if (nrow(subd) == 0) next

      lines(subd$PCTPT, subd$PCmean, type = "b", col = cols[i], pch = 19)

      # Error bars (same raw values as linear view)
      if (sd_param == "BOTH") {
        ok <- !is.na(subd$PCSD)
        if (any(ok)) {
          arrows(
            x0 = subd$PCTPT[ok], y0 = subd$PCmean[ok] - subd$PCSD[ok],
            x1 = subd$PCTPT[ok], y1 = subd$PCmean[ok] + subd$PCSD[ok],
            angle = 90, code = 3, length = 0.05, col = cols[i]
          )
        }
      } else if (sd_param == "UPPER") {
        ok <- !is.na(subd$PCSD)
        if (any(ok)) {
          arrows(
            x0 = subd$PCTPT[ok], y0 = subd$PCmean[ok],
            x1 = subd$PCTPT[ok], y1 = subd$PCmean[ok] + subd$PCSD[ok],
            angle = 90, code = 3, length = 0.05, col = cols[i]
          )
        }
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
  }
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
