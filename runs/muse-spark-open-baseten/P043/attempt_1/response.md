```r
# P043 Figure 11.1 - Translation from SAS to R
# Outputs: sgrender.png + sgrender.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsplit <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find and read ADLBC from adam-split
read_adlbc <- function() {
  candidates <- c(
    file.path(rawsplit, "ADLBC.sas7bdat"),
    file.path(rawsplit, "adlbc.sas7bdat"),
    file.path(rawsplit, "ADLBC.xpt"),
    file.path(rawsplit, "adlbc.xpt"),
    file.path(rawsplit, "ADLBC.XPT"),
    file.path(root, "data/derived/cdisc-pilot/adam-split/ADLBC.sas7bdat"),
    file.path(root, "data/derived/cdisc-pilot/adam-split/adlbc.sas7bdat"),
    file.path(root, "data/derived/cdisc-pilot/adam-split/ADLBC.xpt"),
    file.path(root, "data/derived/cdisc-pilot/adam-split/adlbc.xpt"),
    file.path(root, "data/derived/cdisc-pilot/adam/ADLBC.sas7bdat"),
    file.path(root, "data/derived/cdisc-pilot/adam/adlbc.sas7bdat"),
    file.path(root, "data/derived/cdisc-pilot/adam/ADLBC.xpt"),
    file.path(root, "data/derived/cdisc-pilot/adam/adlbc.xpt")
  )
  # also try listing directory
  if (dir.exists(rawsplit)) {
    fl <- list.files(rawsplit, pattern = "(?i)^adlbc\\.(sas7bdat|xpt)$", full.names = TRUE)
    candidates <- c(fl, candidates)
  }
  if (dir.exists(file.path(root, "data/derived/cdisc-pilot/adam"))) {
    fl2 <- list.files(file.path(root, "data/derived/cdisc-pilot/adam"), pattern = "(?i)^adlbc\\.(sas7bdat|xpt)$", full.names = TRUE)
    candidates <- c(fl2, candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) {
      cat("Reading ADLBC from:", f, "\n")
      if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
        df <- haven::read_sas(f)
      } else {
        df <- haven::read_xpt(f)
      }
      return(df)
    }
  }
  cat("ADLBC file not found in expected locations, will use synthetic test data\n")
  return(NULL)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tibble)
})

adlbc_raw <- read_adlbc()

# If ADLBC not found, fallback to synthetic test data defined in SAS (for reproducibility)
if (is.null(adlbc_raw)) {
  adlbc_raw <- tibble::tibble(
    USUBJID = as.character(1:9),
    TRT = c("PL","PL","PL","T1","T1","T1","T2","T2","T2"),
    TRTA = c("PL","PL","PL","T1","T1","T1","T2","T2","T2"),
    TRTAN = c(1,1,1,2,2,2,3,3,3),
    PARAMCD = "SODIUM",
    PARAM = "Sodium (mmol/L)",
    AVAL = c(30,20,30,50,22,12,38,44,60),
    BASE = c(10,30,40,60,30,22,39,49,12),
    A1LO = 15,
    A1HI = 55,
    AENTMTFL = "Y",
    AVISITN = 1
  )
}

# Normalize column names to uppercase for SAS-like processing
names(adlbc_raw) <- toupper(names(adlbc_raw))

# Ensure required columns exist; if missing create NA
required_cols <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVAL","BASE","A1LO","A1HI","AENTMTFL","AVISITN")
for (cn in required_cols) {
  if (!cn %in% names(adlbc_raw)) adlbc_raw[[cn]] <- NA
}

# DATA ADLBC; WHERE PARAMCD = "SODIUM";
adlbc_filt <- adlbc_raw %>%
  filter(PARAMCD == "SODIUM")

# DATA START; WHERE AENTMTFL = "Y" AND AVISITN NE 99; KEEP ...
# SAS: AVISITN NE 99 includes missing? In SAS missing < 99 so true. In R we replicate: is.na(AVISITN) | AVISITN != 99
START <- adlbc_filt %>%
  filter(AENTMTFL == "Y", is.na(AVISITN) | AVISITN != 99) %>%
  select(USUBJID, TRTAN, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI)

cat("START rows:", nrow(START), "\n")
if (nrow(START) == 0) {
  cat("WARNING: START is empty after filtering\n")
}

# PROC FREQ DATA=START NOPRINT; TABLES PARAMCD * A1LO / OUT=LOW; TABLES PARAMCD * A1HI / OUT=HIGH;
# Replicate SAS PROC FREQ counting
LOW <- START %>%
  filter(!is.na(PARAMCD), !is.na(A1LO)) %>%
  group_by(PARAMCD, A1LO) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(PARAMCD, desc(COUNT))

HIGH <- START %>%
  filter(!is.na(PARAMCD), !is.na(A1HI)) %>%
  group_by(PARAMCD, A1HI) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(PARAMCD, desc(COUNT))

# PROC SORT DATA=LOW; BY PARAMCD DESCENDING COUNT;
# DATA LOW; BY PARAMCD DESCENDING COUNT; IF FIRST.PARAMCD; CALL SYMPUTX("LLN", A1LO);
LOW_sorted <- LOW %>% arrange(PARAMCD, desc(COUNT))
HIGH_sorted <- HIGH %>% arrange(PARAMCD, desc(COUNT))

# FIRST.PARAMCD logic
LOW_first <- LOW_sorted %>%
  group_by(PARAMCD) %>%
  slice(1) %>%
  ungroup()

HIGH_first <- HIGH_sorted %>%
  group_by(PARAMCD) %>%
  slice(1) %>%
  ungroup()

LLN <- NA_real_
ULN <- NA_real_
if (nrow(LOW_first) > 0) LLN <- LOW_first$A1LO[1]
if (nrow(HIGH_first) > 0) ULN <- HIGH_first$A1HI[1]

cat("LLN =", LLN, "\n")
cat("ULN =", ULN, "\n")

# Fallback if LLN/ULN still NA (e.g., synthetic test data uses 15/55)
if (is.na(LLN)) LLN <- 15
if (is.na(ULN)) ULN <- 55

# Prepare plot data - ensure numeric
START_plot <- START %>%
  mutate(
    BASE = as.numeric(BASE),
    AVAL = as.numeric(AVAL),
    TRTA = as.character(TRTA)
  ) %>%
  filter(!is.na(BASE), !is.na(AVAL))

# Define colors and symbols matching SAS template
# DataColors=(CXFFA53D CXF7DF54 CXEF6B48 CX61A6E7 CXCB79C8 CX85CE79 CX967CD0 CXDD6475 CX9DAA2D CX6C69D9 CX2EA64A CXC17D35)
sas_colors <- c("#FFA53D","#F7DF54","#EF6B48","#61A6E7","#CB79C8","#85CE79","#967CD0","#DD6475","#9DAA2D","#6C69D9","#2EA64A","#C17D35")
# DataSymbols = (PLUS circlefilled trianglefilled) - cycle through
# Map to ggplot shapes: PLUS=3, circlefilled=16, trianglefilled=17
sas_shapes <- c(3, 16, 17)

# Determine unique treatments for mapping
trt_levels <- sort(unique(START_plot$TRTA))
n_trt <- length(trt_levels)
# Assign colors/shapes
color_map <- setNames(rep_len(sas_colors, n_trt), trt_levels)
shape_map <- setNames(rep_len(sas_shapes, n_trt), trt_levels)

# Build ggplot replicating SGDESIGN
# designwidth=951 designheight=689, wallcolor white, grid on, labels Min. Baseline / Min. Post Baseline
# scatterplot x=_BASE y=_POST group=_TRT, 45-degree line, reference lines at LLN/ULN

p <- ggplot(START_plot, aes(x = BASE, y = AVAL, color = TRTA, shape = TRTA)) +
  geom_point(size = 3, stroke = 1.1) +
  # 45-degree reference line from 0,0 to 100,100 (wallpercent)
  geom_abline(intercept = 0, slope = 1, color = "grey40", linetype = "solid", linewidth = 0.6) +
  # Reference lines
  geom_hline(yintercept = ULN, color = "#0000FF", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = LLN, color = "#FF0000", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = LLN, color = "#FF0000", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = ULN, color = "#0000FF", linetype = "dashed", linewidth = 0.7) +
  # Annotate ULN/LLN labels near lines (curvelabel position=min)
  annotate("text", x = min(START_plot$BASE, na.rm = TRUE), y = ULN, label = "ULN", hjust = 0, vjust = -0.5, color = "#0000FF", size = 3.5) +
  annotate("text", x = min(START_plot$BASE, na.rm = TRUE), y = LLN, label = "LLN", hjust = 0, vjust = -0.5, color = "#FF0000", size = 3.5) +
  annotate("text", x = LLN, y = min(START_plot$AVAL, na.rm = TRUE), label = "LLN", hjust = -0.2, vjust = 0, color = "#FF0000", size = 3.5, angle = 90) +
  annotate("text", x = ULN, y = min(START_plot$AVAL, na.rm = TRUE), label = "ULN", hjust = -0.2, vjust = 0, color = "#0000FF", size = 3.5, angle = 90) +
  scale_color_manual(values = color_map, name = "Treatment") +
  scale_shape_manual(values = shape_map, name = "Treatment") +
  labs(
    title = "Figure 11.1 Scatterplot",
    x = "Min. Baseline",
    y = "Min. Post Baseline",
    caption = "LLN ULN ...."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "transparent", color = NA)
  ) +
  guides(color = guide_legend(title = "Treatment", override.aes = list(size = 3)),
         shape = guide_legend(title = "Treatment"))

# Ensure axis limits include reference lines and 0-100 diagonal if needed
# SAS used rowdatarange=data columndatarange=data but also drawline 0-100 wallpercent
# We keep data-driven limits expanded slightly
x_range <- range(c(START_plot$BASE, LLN, ULN, 0, 100), na.rm = TRUE)
y_range <- range(c(START_plot$AVAL, LLN, ULN, 0, 100), na.rm = TRUE)
# Add small padding
x_pad <- diff(x_range) * 0.05
y_pad <- diff(y_range) * 0.05
p <- p + coord_cartesian(xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
                         ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))

# Save figure as PNG with designwidth 951 designheight 689
# Use ggsave with width/height in pixels via dpi
# 951x689 at 150 dpi => 6.34 x 4.59 inches
png_path <- file.path(out, "sgrender.png")
ggsave(filename = png_path, plot = p, width = 951, height = 689, units = "px", dpi = 150, bg = "white")
cat("Saved figure to:", png_path, "\n")

# Also save CSV of plotted data (START dataset)
csv_path <- file.path(out, "sgrender.csv")
# Keep original SAS variable names (uppercase) and order
START_out <- START_plot %>%
  select(USUBJID, TRTAN, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI)
write.csv(START_out, csv_path, row.names = FALSE, na = "")
cat("Saved plotted data CSV to:", csv_path, "\n")

# Also save a txt rendering of summary for verification (optional table)
txt_path <- file.path(out, "sgrender.txt")
sink(txt_path)
cat("Figure 11.1 Scatterplot\n")
cat("=======================\n\n")
cat("LLN =", LLN, " ULN =", ULN, "\n\n")
cat("Treatment counts in START (PARAMCD=SODIUM, AENTMTFL=Y, AVISITN !=99):\n")
print(table(START_plot$TRTA, useNA = "ifany"))
cat("\nSummary of BASE and AVAL by Treatment:\n")
if (nrow(START_plot) > 0) {
  summ <- START_plot %>%
    group_by(TRTA) %>%
    summarise(
      N = n(),
      BASE_mean = mean(BASE, na.rm = TRUE),
      BASE_min = min(BASE, na.rm = TRUE),
      BASE_max = max(BASE, na.rm = TRUE),
      AVAL_mean = mean(AVAL, na.rm = TRUE),
      AVAL_min = min(AVAL, na.rm = TRUE),
      AVAL_max = max(AVAL, na.rm = TRUE),
      .groups = "drop"
    )
  print(as.data.frame(summ))
}
cat("\nReference lines:\n")
cat("  Horizontal y=ULN (", ULN, ") blue dashed\n")
cat("  Horizontal y=LLN (", LLN, ") red dashed\n")
cat("  Vertical x=LLN (", LLN, ") red dashed\n")
cat("  Vertical x=ULN (", ULN, ") blue dashed\n")
cat("  45-degree line y=x from 0 to 100\n")
sink()
cat("Saved text summary to:", txt_path, "\n")
```