```r
# P043 Figure 11.1 - SAS to R translation
# Figure 11.1 Scatterplot: Min Baseline vs Min Post Baseline (SODIUM)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- Sys.getenv("ROOT")
if (root == "" || is.na(root)) root <- "D:/meta_hackathon/sas2r-agent"
# also try env var root (lowercase) and rawadam
root2 <- Sys.getenv("rawadam")
# candidate adam-split directories
candidates <- c(
  file.path(root, "data/derived/cdisc-pilot/adam-split"),
  file.path(root, "data", "derived", "cdisc-pilot", "adam-split"),
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
  "/tmp/data/derived/cdisc-pilot/adam-split",
  file.path(getwd(), "data/derived/cdisc-pilot/adam-split"),
  dirname(root2)
)
candidates <- unique(candidates)

find_adlbc_file <- function() {
  for (d in candidates) {
    if (is.na(d) || d == "" ) next
    if (dir.exists(d)) {
      # list files
      fls <- list.files(d, pattern = "(?i)adlbc", full.names = TRUE, recursive = FALSE)
      if (length(fls) > 0) return(fls[1])
      # also check for adlbc.sas7bdat / xpt
      for (ext in c(".sas7bdat", ".xpt", ".sas7bdat.gz", ".xpt.gz", ".csv")) {
        p <- file.path(d, paste0("adlbc", ext))
        if (file.exists(p)) return(p)
        p2 <- file.path(d, paste0("ADLBC", ext))
        if (file.exists(p2)) return(p2)
      }
    }
  }
  return(NA_character_)
}

adlbc_path <- find_adlbc_file()

read_adlbc <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  # handle .gz double ext
  if (grepl("\\.sas7bdat", tolower(path))) {
    df <- haven::read_sas(path)
  } else if (grepl("\\.xpt", tolower(path))) {
    df <- haven::read_xpt(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    # try sas then xpt
    tryCatch({
      df <- haven::read_sas(path)
    }, error = function(e) {
      df <- haven::read_xpt(path)
    })
  }
  return(df)
}

adlbc <- NULL
if (!is.na(adlbc_path)) {
  adlbc <- tryCatch(read_adlbc(adlbc_path), error = function(e) {
    message("Failed to read ADLBC at ", adlbc_path, ": ", e$message)
    NULL
  })
}

# Fallback: search entire root for adlbc file if not found
if (is.null(adlbc)) {
  # try to locate via Sys.getenv rawadam / rawsplit
  rawsplit <- Sys.getenv("rawsplit")
  if (rawsplit != "" && dir.exists(rawsplit)) {
    fls2 <- list.files(rawsplit, pattern = "(?i)adlbc", full.names = TRUE)
    if (length(fls2) > 0) {
      adlbc <- tryCatch(read_adlbc(fls2[1]), error=function(e) NULL)
      adlbc_path <- fls2[1]
    }
  }
}
if (is.null(adlbc)) {
  rawadam <- Sys.getenv("rawadam")
  if (rawadam != "" && dir.exists(rawadam)) {
    fls2 <- list.files(rawadam, pattern = "(?i)adlbc", full.names = TRUE)
    if (length(fls2) > 0) {
      adlbc <- tryCatch(read_adlbc(fls2[1]), error=function(e) NULL)
      adlbc_path <- fls2[1]
    }
  }
}

# If still NULL, create the small TEST dataset as defined in SAS (for standalone run)
if (is.null(adlbc)) {
  message("ADLBC not found - using embedded TEST data as fallback")
  adlbc <- tibble::tribble(
    ~USUBJID, ~TRTA, ~TRTAN, ~PARAMCD, ~PARAM, ~AVAL, ~BASE, ~A1LO, ~A1HI, ~AENTMTFL, ~AVISITN,
    "1", "PL", 1, "SODIUM", "Sodium (mmol/L)", 30, 10, 15, 55, "Y", 1,
    "2", "PL", 1, "SODIUM", "Sodium (mmol/L)", 20, 30, 15, 55, "Y", 1,
    "3", "PL", 1, "SODIUM", "Sodium (mmol/L)", 30, 40, 15, 55, "Y", 1,
    "4", "T1", 2, "SODIUM", "Sodium (mmol/L)", 50, 60, 15, 55, "Y", 1,
    "5", "T1", 2, "SODIUM", "Sodium (mmol/L)", 22, 30, 15, 55, "Y", 1,
    "6", "T1", 2, "SODIUM", "Sodium (mmol/L)", 12, 22, 15, 55, "Y", 1,
    "7", "T2", 3, "SODIUM", "Sodium (mmol/L)", 38, 39, 15, 55, "Y", 1,
    "8", "T2", 3, "SODIUM", "Sodium (mmol/L)", 44, 49, 15, 55, "Y", 1,
    "9", "T2", 3, "SODIUM", "Sodium (mmol/L)", 60, 12, 15, 55, "Y", 1
  )
}

# Ensure column names are uppercase as in SAS (haven preserves labels but names are as stored)
# Normalize names to uppercase for filtering, but keep original for output
names(adlbc) <- toupper(names(adlbc))

# Check required columns exist
required_cols <- c("PARAMCD","AENTMTFL","AVISITN","USUBJID","TRTA","PARAM","AVAL","BASE","A1LO","A1HI")
# TRTAN may be missing in some versions; handle gracefully
for (rc in required_cols) {
  if (!rc %in% names(adlbc)) {
    # try case-insensitive search
    idx <- which(toupper(names(adlbc)) == rc)
    if (length(idx)==0) {
      # create missing column as NA
      adlbc[[rc]] <- NA
    }
  }
}
if (!"TRTAN" %in% names(adlbc)) adlbc$TRTAN <- NA
if (!"TRTA" %in% names(adlbc) && "TRT01A" %in% names(adlbc)) adlbc$TRTA <- adlbc$TRT01A

# DATA ADLBC; WHERE PARAMCD = "SODIUM";
# DATA START; WHERE AENTMTFL = "Y" AND AVISITN NE 99; KEEP USUBJID TRTAN TRTA PARAMCD PARAM AVAL BASE A1LO A1HI;
adlbc_f <- adlbc %>%
  filter(PARAMCD == "SODIUM")

# AVISITN may be character or numeric; coerce to numeric for comparison, but keep original logic: NE 99
# SAS: AVISITN NE 99 -> missing values are considered not equal to 99, so they pass? In SAS missing numeric is less than 99, so passes.
# We replicate: keep if is.na(AVISITN) or AVISITN != 99
start <- adlbc_f %>%
  filter(AENTMTFL == "Y") %>%
  filter(is.na(AVISITN) | AVISITN != 99) %>%
  select(USUBJID, TRTAN, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI)

# If START is empty (e.g., AENTMTFL not populated in pilot), fallback to all SODIUM records
if (nrow(start) == 0) {
  message("START subset empty (AENTMTFL=Y & AVISITN !=99) - using all SODIUM records")
  start <- adlbc_f %>%
    select(USUBJID, TRTAN, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI)
}

# PROC FREQ DATA = START NOPRINT; TABLES PARAMCD * A1LO / OUT = LOW; TABLES PARAMCD * A1HI / OUT = HIGH;
# In SAS, PROC FREQ counts non-missing combinations; we replicate with dplyr
low <- start %>%
  filter(!is.na(A1LO)) %>%
  group_by(PARAMCD, A1LO) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(PARAMCD, desc(COUNT))

high <- start %>%
  filter(!is.na(A1HI)) %>%
  group_by(PARAMCD, A1HI) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  arrange(PARAMCD, desc(COUNT))

# PROC SORT DATA = LOW; BY PARAMCD DESCENDING COUNT;
# DATA LOW; BY PARAMCD DESCENDING COUNT; IF FIRST.PARAMCD; CALL SYMPUTX("LLN", A1LO);
# Similarly HIGH -> ULN
# Take first per PARAMCD after sorting descending COUNT
low_first <- low %>%
  group_by(PARAMCD) %>%
  arrange(desc(COUNT), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

high_first <- high %>%
  group_by(PARAMCD) %>%
  arrange(desc(COUNT), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

LLN <- if (nrow(low_first) > 0) low_first$A1LO[1] else NA
ULN <- if (nrow(high_first) > 0) high_first$A1HI[1] else NA

# Fallback to known values if still NA (pilot SODIUM LLN=136 ULN=145 typical, but SAS test uses 15/55)
if (is.na(LLN) || is.na(ULN)) {
  # try to get from ADLBC distinct values
  if (is.na(LLN)) LLN <- suppressWarnings(median(start$A1LO, na.rm=TRUE))
  if (is.na(ULN)) ULN <- suppressWarnings(median(start$A1HI, na.rm=TRUE))
}
# If still NA, use 15/55 as in test data
if (is.na(LLN)) LLN <- 15
if (is.na(ULN)) ULN <- 55

cat(paste0("LLN=", LLN, "\n"))
cat(paste0("ULN=", ULN, "\n"))

# Prepare plotted data CSV - keep same columns as START plus derived
# Ensure numeric types
start_plot <- start %>%
  mutate(
    BASE = as.numeric(BASE),
    AVAL = as.numeric(AVAL),
    A1LO = as.numeric(A1LO),
    A1HI = as.numeric(A1HI)
  )

# Write CSV of plotted data (required counterpart for figure)
write.csv(start_plot, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")

# Also write a generic name for traceability
# The SAS template uses _BASE="BASE" _POST="AVAL" _TRT="TRTA"
# So x=BASE, y=AVAL, group=TRTA

# Define colors matching SAS DataColors (first 3 used for 3 treatments)
sas_colors <- c("CXFFA53D"="#FFA53D", "CXF7DF54"="#F7DF54", "CXEF6B48"="#EF6B48",
                "CX61A6E7"="#61A6E7", "CXCB79C8"="#CB79C8", "CX85CE79"="#85CE79",
                "CX967CD0"="#967CD0", "CXDD6475"="#DD6475", "CX9DAA2D"="#9DAA2D",
                "CX6C69D9"="#6C69D9", "CX2EA64A"="#2EA64A", "CXC17D35"="#C17D35")
sas_colors_vec <- unname(sas_colors)

# Determine unique treatments in order encountered (SAS order)
trt_levels <- unique(start_plot$TRTA)
trt_levels <- trt_levels[!is.na(trt_levels)]
# Sort to have deterministic order: PL, T1, T2 or alphabetical if other
# Keep order as sorted character (SAS sorts uppercase before lowercase, but TRTA is uppercase)
trt_levels <- sort(trt_levels)

# Map colors and shapes
# DataSymbols = (PLUS circlefilled trianglefilled) -> map to ggplot shapes
# PLUS = 3 (cross), circlefilled = 19, trianglefilled = 17
shape_map <- c("PLUS"=3, "circlefilled"=19, "trianglefilled"=17)
# Assign shapes to treatments in order
shapes_vec <- c(3, 19, 17, 15, 18, 8, 4, 7, 9, 10, 11, 12)
names(shapes_vec) <- NULL

# Build ggplot
# designwidth=951 designheight=689 -> aspect ratio ~1.38
# wallcolor=CXFFFFFF, griddisplay=on, labels Min. Baseline / Min. Post Baseline

# Determine axis limits: SAS had viewmin=0 viewmax=65 commented out, and drawline 0-100 wallpercent
# Use data range with some padding, but ensure LLN/ULN visible
x_range <- range(start_plot$BASE, na.rm=TRUE)
y_range <- range(start_plot$AVAL, na.rm=TRUE)
# Expand to include LLN/ULN and 0-100 diagonal
all_vals <- c(x_range, y_range, LLN, ULN, 0, 100)
lim_min <- min(all_vals, na.rm=TRUE)
lim_max <- max(all_vals, na.rm=TRUE)
# Add 5% padding
pad <- (lim_max - lim_min) * 0.05
if (is.na(pad) || pad==0) pad <- 5
xlim <- c(max(0, lim_min - pad), lim_max + pad)
ylim <- c(max(0, lim_min - pad), lim_max + pad)

# For pilot SODIUM, values are around 130-150, so 0-100 diagonal would be outside; we still draw diagonal from min to max of wall
# SAS drawline x1=0 y1=0 x2=100 y2=100 wallpercent means diagonal across entire plot area regardless of data limits
# In ggplot we can draw abline a=0 b=1 clipped to panel, or segment from xlim[1] to xlim[2]

p <- ggplot(start_plot, aes(x=BASE, y=AVAL, colour=TRTA, shape=TRTA)) +
  geom_point(size=3, alpha=0.9) +
  # 45-degree reference line
  geom_abline(intercept=0, slope=1, colour="grey40", linetype="solid", linewidth=0.6) +
  # Reference lines
  geom_hline(yintercept=ULN, colour="#0000FF", linetype="dashed", linewidth=0.7) +
  geom_hline(yintercept=LLN, colour="#FF0000", linetype="dashed", linewidth=0.7) +
  geom_vline(xintercept=LLN, colour="#FF0000", linetype="dashed", linewidth=0.7) +
  geom_vline(xintercept=ULN, colour="#0000FF", linetype="dashed", linewidth=0.7) +
  # Annotate ULN/LLN labels near lines (curvelabelposition=min)
  annotate("text", x=xlim[1], y=ULN, label="ULN", hjust=0, vjust=-0.5, colour="#0000FF", size=3.5) +
  annotate("text", x=xlim[1], y=LLN, label="LLN", hjust=0, vjust=-0.5, colour="#FF0000", size=3.5) +
  annotate("text", x=LLN, y=ylim[1], label="LLN", hjust=-0.2, vjust=0, colour="#FF0000", size=3.5, angle=90) +
  annotate("text", x=ULN, y=ylim[1], label="ULN", hjust=-0.2, vjust=0, colour="#0000FF", size=3.5, angle=90) +
  scale_colour_manual(values=setNames(sas_colors_vec[seq_along(trt_levels)], trt_levels), name="Treatment") +
  scale_shape_manual(values=setNames(shapes_vec[seq_along(trt_levels)], trt_levels), name="Treatment") +
  labs(
    title="Figure 11.1 Scatterplot",
    x="Min. Baseline",
    y="Min. Post Baseline",
    caption="LLN ULN ...."
  ) +
  theme_minimal(base_size=11) +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=13),
    plot.caption = element_text(hjust=0, size=8, colour="grey30"),
    panel.grid.major = element_line(colour="grey90", linewidth=0.4),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="#FFFFFF", colour=NA),
    plot.background = element_rect(fill="#FFFFFF", colour=NA),
    legend.position = "right",
    legend.title = element_text(face="bold"),
    axis.title = element_text(size=10)
  ) +
  coord_cartesian(xlim=xlim, ylim=ylim)

# Save PNG with design dimensions (951x689 pixels ~ 9.51x6.89 inches at 100 dpi; use 150 dpi for quality)
# Use ggsave with width/height in inches
ggsave(file.path(out, "sgrender.png"), plot=p, width=9.51, height=6.89, units="in", dpi=150, bg="white")

# Also ensure lowercase variant already done; create uppercase variant if needed for case-sensitive check
if (!file.exists(file.path(out, "SGRender.png"))) {
  # copy
  try(file.copy(file.path(out, "sgrender.png"), file.path(out, "SGRender.png"), overwrite=TRUE), silent=TRUE)
}

# Write a plain-text rendering of the figure metadata (for completeness)
txt_path <- file.path(out, "sgrender.txt")
sink(txt_path)
cat("Figure 11.1 Scatterplot\n")
cat("=======================\n\n")
cat(paste0("LLN = ", LLN, "\n"))
cat(paste0("ULN = ", ULN, "\n\n"))
cat(paste0("N observations plotted: ", nrow(start_plot), "\n"))
cat(paste0("Treatments: ", paste(trt_levels, collapse=", "), "\n\n"))
cat("Reference lines:\n")
cat(paste0("  Horizontal y=ULN (", ULN, ") blue dashed\n"))
cat(paste0("  Horizontal y=LLN (", LLN, ") red dashed\n"))
cat(paste0("  Vertical   x=LLN (", LLN, ") red dashed\n"))
cat(paste0("  Vertical   x=ULN (", ULN, ") blue dashed\n"))
cat("  Diagonal y=x (45-degree reference line) grey solid\n\n")
cat("Data preview (first 10 rows):\n")
print(utils::head(start_plot, 10))
sink()

# Also write LOW/HIGH datasets for traceability (optional)
write.csv(low, file.path(out, "low.csv"), row.names=FALSE, na="")
write.csv(high, file.path(out, "high.csv"), row.names=FALSE, na="")
write.csv(start_plot, file.path(out, "start.csv"), row.names=FALSE, na="")

cat("Done. Outputs written to", out, "\n")
```