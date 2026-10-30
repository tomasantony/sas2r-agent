out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Resolve input paths: SAS macro &rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
# Try absolute path as given, then fallback to common locations in container
candidate_roots <- c(
  "D:/meta_hackathon/sas2r-agent",
  "/meta_hackathon/sas2r-agent",
  Sys.getenv("ROOT"),
  Sys.getenv("root"),
  "."
)
# Also try to locate adpc.xpt via search
find_file <- function(fname) {
  # try known candidate paths
  candidates <- c(
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", fname),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam-split", fname),
    file.path("/tmp", fname),
    file.path(getwd(), fname)
  )
  # search recursively under / if needed (limited)
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  # brute search under common base
  search_bases <- c("/meta_hackathon", "/opt", "/data", ".", "/github", "/workspace")
  for (b in search_bases) {
    if (dir.exists(b)) {
      found <- list.files(b, pattern = fname, recursive = TRUE, full.names = TRUE)
      if (length(found) > 0) return(found[1])
    }
  }
  return(NA)
}

adpc_path <- find_file("adpc.xpt")
adsl_path <- find_file("adsl.xpt")

# If not found via search, try direct SAS libref path translation
if (is.na(adpc_path)) {
  # translate D:/ to /
  adpc_path <- "/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"
}
if (is.na(adsl_path)) {
  adsl_path <- "/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
}

# Also check inlib derived path
inlib_adpc <- find_file("adpc.xpt")
# Prefer rawadam path if exists
if (file.exists(adpc_path)) {
  adpc_file <- adpc_path
} else if (!is.na(inlib_adpc) && file.exists(inlib_adpc)) {
  adpc_file <- inlib_adpc
} else {
  # last resort: list all xpt
  all_xpt <- list.files("/", pattern = "adpc\\.xpt", recursive = TRUE, full.names = TRUE)
  if (length(all_xpt) > 0) adpc_file <- all_xpt[1] else adpc_file <- adpc_path
}

if (file.exists(adsl_path)) {
  adsl_file <- adsl_path
} else {
  adsl_file <- find_file("adsl.xpt")
}

message("Using ADPC: ", adpc_file)
message("Using ADSL: ", adsl_file)

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)

# Read ADPC and ADSL
adpc <- haven::read_xpt(adpc_file)
adsl <- haven::read_xpt(adsl_file)

# Replicate SAS: work.adsl and work.adpc directly from source
# Then PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc BY USUBJID parcat1 eactm
# SAS sort: character ordering uppercase before lowercase, missing first
# In R, arrange will approximate; ensure same order

# Ensure expected columns exist
# ADPC typical vars: USUBJID, PARCAT1, EACTM, AVAL, etc.
# Check column names case-insensitive
names(adpc) <- toupper(names(adpc))
names(adsl) <- toupper(names(adsl))

# Verify required columns
stopifnot("USUBJID" %in% names(adpc))
stopifnot("PARCAT1" %in% names(adpc))
# EACTM may be named EACTM or ACTM or ELTM etc. Check SAS uses eactm
if (!"EACTM" %in% names(adpc)) {
  # try to find similar
  cand <- grep("EACTM|ACTM|ATPT|TIME", names(adpc), value = TRUE)
  message("EACTM not found, candidates: ", paste(cand, collapse=", "))
  # fallback: try to use nominal time
  if ("NFRLT" %in% names(adpc)) {
    adpc$EACTM <- adpc$NFRLT
  } else if ("AFRLT" %in% names(adpc)) {
    adpc$EACTM <- adpc$AFRLT
  } else {
    stop("EACTM column missing and no fallback")
  }
}
if (!"AVAL" %in% names(adpc)) {
  stop("AVAL missing in ADPC")
}

# Convert haven labelled to plain vectors for plotting
adpc <- adpc %>%
  mutate(across(everything(), ~ {
    if (inherits(.x, "haven_labelled")) as.numeric(.x) else .x
  }))

# For character labelled, haven may keep as character; ensure USUBJID, PARCAT1 are character
adpc$USUBJID <- as.character(adpc$USUBJID)
adpc$PARCAT1 <- as.character(adpc$PARCAT1)

# SAS PROC SORT BY USUBJID parcat1 eactm
# Do sort
sort_adpc <- adpc %>%
  arrange(USUBJID, PARCAT1, EACTM)

# Write dataset CSV as per spec: each output dataset as CSV
# The expected output is figure sasgraph.png -> write OUT_DIR/sasgraph.png and CSV of plotted data
# Also need to write dataset counterpart if any. The program declares output types graph;dataset
# But SAS program only produces graph. We will write the sorted data as dataset for completeness
# and the plotted data CSV.

# Prepare plotted data: AVAL * EACTM BY USUBJID PARCAT1
# Keep relevant columns for traceability
plot_data <- sort_adpc %>%
  select(USUBJID, PARCAT1, EACTM, AVAL, everything()) %>%
  # Keep original order
  arrange(USUBJID, PARCAT1, EACTM)

# Write CSV of plotted data (required for figure)
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# Also write as dataset if needed (same as plot data)
# The spec says each output dataset as CSV; we provide one
write.csv(plot_data, file.path(out, "SORTadpc.csv"), row.names = FALSE, na = "")

# Titles from SAS
title1 <- "(page x of x)"
title2 <- "Figure 16.2.5-x.x Individual concentration-time profiles"
title3 <- "by compound, matrix, analyte and [actual/randomised][treatments/group]"
title4 <- "Analysis Set : PK analysis set"

# Footnote: Generated by SAS System (...) on date at time
# Replicate with R info
footnote_text <- paste0("Generated by the R System on ",
                        format(Sys.Date(), "%d%b%Y"),
                        " at ", format(Sys.time(), "%I:%M %p"))

# Axis labels
x_label <- "Time (hour)"
y_label <- "Conc (ug/mL)"

# PROC GPLOT with BY USUBJID parcat1
# This creates one plot per BY group, each with AVAL vs EACTM joined line
# In R, we need to reproduce as faceted plot or multi-panel PNG
# SAS GPLOT with BY would produce multiple pages; we will create a faceted ggplot
# that shows all subjects, faceted by USUBJID and PARCAT1, or if too many, sample

# Determine number of BY groups
by_groups <- plot_data %>%
  distinct(USUBJID, PARCAT1) %>%
  arrange(USUBJID, PARCAT1)

n_groups <- nrow(by_groups)
message("Number of BY groups (USUBJID x PARCAT1): ", n_groups)

# For plotting, we need to handle potentially many subjects (ADPC has ~ 300 subjects)
# SAS would produce many pages; in R we produce a single PNG with facets
# To keep readable, we will facet_wrap by USUBJID and PARCAT1, but limit to reasonable layout
# If n_groups > 60, we will create a paginated approach: show all in a large image with free scales
# Use ggplot with geom_line + geom_point

# Prepare plot: use SYMBOL1 INTERPOL=JOIN VALUE=NONE LINE=1 WIDTH=2
# So line only, no markers, joined in order of EACTM

# Create ggplot
# If too many groups, we need to adjust dimensions
# Estimate needed height: ~ 2 inches per row of facets

# Use facet_wrap with labeller
# Create a combined BY label for faceting
plot_data$BY_LABEL <- paste0(plot_data$USUBJID, " / ", plot_data$PARCAT1)

# Order BY_LABEL by USUBJID, PARCAT1
by_order <- by_groups %>%
  mutate(BY_LABEL = paste0(USUBJID, " / ", PARCAT1)) %>%
  pull(BY_LABEL)

plot_data$BY_LABEL <- factor(plot_data$BY_LABEL, levels = by_order)

# Determine facet layout: try to make roughly square
n_col <- min(4, ceiling(sqrt(n_groups)))
n_row <- ceiling(n_groups / n_col)

# Width and height for PNG: SAS default ~ 8x6 inches, but for many facets need larger
# Use dynamic sizing: width 12 inches, height 2.5 * n_row inches, capped
width_in <- 12
height_in <- min(40, max(6, 2.2 * n_row + 2))
if (n_groups > 100) {
  # For very large, increase width and use more columns
  n_col <- 6
  n_row <- ceiling(n_groups / n_col)
  width_in <- 16
  height_in <- min(60, max(8, 1.8 * n_row + 2))
}

p <- ggplot(plot_data, aes(x = EACTM, y = AVAL, group = BY_LABEL)) +
  geom_line(linewidth = 0.8, color = "black") +
  facet_wrap(~ BY_LABEL, ncol = n_col, scales = "free_y") +
  labs(
    title = paste(title1, "\n", title2, "\n", title3, "\n", title4),
    x = x_label,
    y = y_label,
    caption = footnote_text
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "plain"),
    plot.caption = element_text(hjust = 0, size = 7),
    axis.title = element_text(size = 9),
    strip.text = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.border = element_rect(color = "black", linewidth = 0.8)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# Save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = width_in, height = height_in, units = "in", dpi = 300, limitsize = FALSE)
message("Saved figure to ", png_path, " (", width_in, "x", height_in, " inches)")

# Also create a plain-text rendering of the figure data summary (as SAS would list)
# Write a txt file with titles and summary
txt_path <- file.path(out, "sasgraph.txt")
sink(txt_path)
cat(title1, "\n")
cat(title2, "\n")
cat(title3, "\n")
cat(title4, "\n")
cat("\n")
cat("Individual concentration-time profiles by USUBJID and PARCAT1\n")
cat("Analysis Set: PK analysis set\n")
cat("------------------------------------------------------------\n")
cat("Number of subjects (USUBJID): ", length(unique(plot_data$USUBJID)), "\n")
cat("Number of BY groups (USUBJID * PARCAT1): ", n_groups, "\n")
cat("Total observations plotted: ", nrow(plot_data), "\n")
cat("\n")
cat("Axis1: Conc (ug/mL)  Axis2: Time (hour)\n")
cat("Symbol: INTERPOL=JOIN VALUE=NONE LINE=1 WIDTH=2\n")
cat("\n")
cat("BY groups:\n")
print(as.data.frame(by_groups), row.names = FALSE)
cat("\n")
cat(footnote_text, "\n")
sink()

# Ensure OUT_DIR contains required files
# Also write a minimal dataset CSV for ADSL if needed (not required but for completeness)
# The spec says each output dataset as CSV with SAS variable names
# We have already written SORTadpc.csv and sasgraph.csv

message("Done. Outputs in ", out, ": ", paste(list.files(out), collapse=", "))
