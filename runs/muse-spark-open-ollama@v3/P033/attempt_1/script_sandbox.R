# P033_target12.sas -> R translation
# Figure 16.2.5-x.x Individual concentration-time profiles by compound, matrix, analyte and treatments/group
# Analysis Set: PK analysis set
# GPLOT AVAL * eactm BY USUBJID parcat1

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not used heavily here but included for compliance)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# Helper: find file trying multiple candidate roots
find_file <- function(rel_candidates) {
  for (p in rel_candidates) {
    if (file.exists(p)) return(p)
  }
  return(NA_character_)
}

# Candidate roots for raw ADaM
candidate_roots <- c(
  "/bench",
  "/meta_hackathon/sas2r-agent",
  "/bench/data/raw/cdisc-pilot/adam",
  Sys.getenv("RAWADAM"),
  Sys.getenv("rawadam"),
  file.path(getwd(), "data/raw/cdisc-pilot/adam"),
  file.path(getwd(), "data/derived/cdisc-pilot/adam"),
  "/tmp",
  "."
)

# Explicit paths from SAS program
sas_rawadam <- "/bench/data/raw/cdisc-pilot/adam"
sas_rawadam_alt <- gsub("D:", "", sas_rawadam)

# Try to locate adpc.xpt and adsl.xpt
search_paths_adpc <- c(
  file.path(sas_rawadam, "adpc.xpt"),
  file.path(sas_rawadam_alt, "adpc.xpt"),
  file.path("/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", "adpc.xpt"),
  file.path("/bench/data/raw/cdisc-pilot/adam", "adpc.xpt"),
  list.files(path = ".", pattern = "adpc\\.xpt$", recursive = TRUE, full.names = TRUE),
  list.files(path = "/tmp", pattern = "adpc\\.xpt$", recursive = TRUE, full.names = TRUE),
  list.files(path = "/data", pattern = "adpc\\.xpt$", recursive = TRUE, full.names = TRUE),
  list.files(path = getwd(), pattern = "adpc\\.xpt$", recursive = TRUE, full.names = TRUE)
)
search_paths_adpc <- unique(unlist(search_paths_adpc))
search_paths_adpc <- search_paths_adpc[!is.na(search_paths_adpc) & file.exists(search_paths_adpc)]

search_paths_adsl <- c(
  file.path(sas_rawadam, "adsl.xpt"),
  file.path(sas_rawadam_alt, "adpc.xpt"), # placeholder to keep structure
  file.path("/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", "adsl.xpt"),
  list.files(path = ".", pattern = "adsl\\.xpt$", recursive = TRUE, full.names = TRUE),
  list.files(path = "/tmp", pattern = "adsl\\.xpt$", recursive = TRUE, full.names = TRUE),
  list.files(path = "/data", pattern = "adsl\\.xpt$", recursive = TRUE, full.names = TRUE),
  list.files(path = getwd(), pattern = "adsl\\.xpt$", recursive = TRUE, full.names = TRUE)
)
search_paths_adsl <- unique(unlist(search_paths_adsl))
search_paths_adsl <- search_paths_adsl[!is.na(search_paths_adsl) & file.exists(search_paths_adsl)]

# Fallback: brute force search if not found
if (length(search_paths_adpc) == 0) {
  all_xpt <- list.files(path = "/", pattern = "adpc\\.xpt$", recursive = TRUE, full.names = TRUE)
  # limit to first 20 to avoid huge search
  if (length(all_xpt) > 0) search_paths_adpc <- all_xpt[1:min(5, length(all_xpt))]
}
if (length(search_paths_adsl) == 0) {
  all_xpt2 <- list.files(path = "/", pattern = "adsl\\.xpt$", recursive = TRUE, full.names = TRUE)
  if (length(all_xpt2) > 0) search_paths_adsl <- all_xpt2[1:min(5, length(all_xpt2))]
}

# If still not found, try known derived location
if (length(search_paths_adpc) == 0) {
  cand <- c(
    "data/derived/cdisc-pilot/adam/adpc.xpt",
    "data/raw/cdisc-pilot/adam/adpc.xpt",
    "../data/raw/cdisc-pilot/adam/adpc.xpt",
    "/github/workspace/data/raw/cdisc-pilot/adam/adpc.xpt"
  )
  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) search_paths_adpc <- cand
}
if (length(search_paths_adsl) == 0) {
  cand2 <- c(
    "data/derived/cdisc-pilot/adam/adsl.xpt",
    "data/raw/cdisc-pilot/adam/adsl.xpt",
    "../data/raw/cdisc-pilot/adam/adsl.xpt"
  )
  cand2 <- cand2[file.exists(cand2)]
  if (length(cand2) > 0) search_paths_adsl <- cand2
}

adpc_path <- if (length(search_paths_adpc) > 0) search_paths_adpc[1] else NA
adsl_path <- if (length(search_paths_adsl) > 0) search_paths_adsl[1] else NA

# Also try direct SAS libref path
inlib_path_adpc <- find_file(c(
  "/bench/data/derived/cdisc-pilot/adam/adpc.xpt",
  "/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
  "data/derived/cdisc-pilot/adam/adpc.xpt"
))
if (!is.na(inlib_path_adpc) && is.na(adpc_path)) adpc_path <- inlib_path_adpc

message("Using ADPC path: ", adpc_path)
message("Using ADSL path: ", adsl_path)

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

# Read datasets
adpc <- NULL
adsl <- NULL

if (!is.na(adpc_path) && file.exists(adpc_path)) {
  adpc <- haven::read_xpt(adpc_path)
} else {
  # Try haven read_xpt with original SAS path (may work on Windows runner)
  try({
    adpc <- haven::read_xpt("/bench/data/raw/cdisc-pilot/adam/adpc.xpt")
  }, silent = TRUE)
}
if (!is.na(adsl_path) && file.exists(adsl_path)) {
  adsl <- haven::read_xpt(adsl_path)
} else {
  try({
    adsl <- haven::read_xpt("/bench/data/raw/cdisc-pilot/adam/adsl.xpt")
  }, silent = TRUE)
}

# If still NULL, try alternative extensions (sas7bdat, csv)
if (is.null(adpc)) {
  alt <- list.files(path = getwd(), pattern = "adpc\\.(sas7bdat|csv|xpt)$", recursive = TRUE, full.names = TRUE)
  if (length(alt) > 0) {
    p <- alt[1]
    if (grepl("\\.xpt$", p, ignore.case = TRUE)) adpc <- haven::read_xpt(p)
    else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) adpc <- haven::read_sas(p)
    else adpc <- readr::read_csv(p, show_col_types = FALSE)
  }
}
if (is.null(adsl)) {
  alt2 <- list.files(path = getwd(), pattern = "adsl\\.(sas7bdat|csv|xpt)$", recursive = TRUE, full.names = TRUE)
  if (length(alt2) > 0) {
    p <- alt2[1]
    if (grepl("\\.xpt$", p, ignore.case = TRUE)) adsl <- haven::read_xpt(p)
    else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) adsl <- haven::read_sas(p)
    else adsl <- readr::read_csv(p, show_col_types = FALSE)
  }
}

stopifnot(!is.null(adpc))
# adsl may be optional for this figure, but we keep it if available
if (is.null(adsl)) {
  message("ADSL not found, continuing with ADPC only")
  adsl <- tibble(USUBJID = character())
}

# Convert haven labelled to plain, preserve names
adpc <- adpc %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
if (nrow(adsl) > 0) {
  adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
    mutate(across(where(is.factor), as.character))
}

# Inspect columns
message("ADPC columns: ", paste(names(adpc), collapse = ", "))
if (nrow(adsl) > 0) message("ADSL columns: ", paste(names(adsl), collapse = ", "))

# SAS program: PROC SORT BY USUBJID parcat1 eactm
# Ensure required columns exist
required_cols <- c("USUBJID", "PARCAT1", "EACTM", "AVAL")
# Handle case-insensitive column names
col_map <- setNames(names(adpc), toupper(names(adpc)))
# Rename to expected case if needed
for (rc in required_cols) {
  if (!rc %in% names(adpc) && rc %in% names(col_map)) {
    names(adpc)[names(adpc) == col_map[[rc]]] <- rc
  }
  # also try lower case variants
  if (!rc %in% names(adpc)) {
    idx <- which(toupper(names(adpc)) == rc)
    if (length(idx) > 0) names(adpc)[idx[1]] <- rc
  }
}
# Check again, try alternative names: eactm might be EACTM, AVAL etc.
# Some ADPC versions use PCTPT, ATPT, etc. But program uses eactm
# If EACTM not present, try to derive from other time variables
if (!"EACTM" %in% names(adpc)) {
  # Try common alternatives
  candidates <- c("EACTM", "ACTM", "ATPTN", "PCTPTNUM", "AVISITN", "ATPT", "TPT", "NTPT", "TIME")
  found <- intersect(toupper(names(adpc)), candidates)
  if (length(found) > 0) {
    # pick first that is numeric
    for (f in found) {
      orig <- names(adpc)[toupper(names(adpc)) == f][1]
      if (!is.na(orig)) {
        adpc$EACTM <- adpc[[orig]]
        message("Mapped EACTM from ", orig)
        break
      }
    }
  }
}
if (!"PARCAT1" %in% names(adpc)) {
  # Try PARAM, PARCAT1, etc.
  if ("PARAM" %in% names(adpc)) {
    adpc$PARCAT1 <- adpc$PARAM
  } else if ("PARCAT" %in% names(adpc)) {
    adpc$PARCAT1 <- adpc$PARCAT
  } else {
    # fallback to single group
    adpc$PARCAT1 <- "All"
  }
}
if (!"AVAL" %in% names(adpc)) {
  if ("AVALC" %in% names(adpc)) adpc$AVAL <- suppressWarnings(as.numeric(adpc$AVALC))
}

stopifnot(all(c("USUBJID", "PARCAT1", "EACTM", "AVAL") %in% names(adpc)))

# Ensure types
adpc$USUBJID <- as.character(adpc$USUBJID)
adpc$PARCAT1 <- as.character(adpc$PARCAT1)
# EACTM should be numeric (time in hours)
adpc$EACTM <- suppressWarnings(as.numeric(as.character(adpc$EACTM)))
adpc$AVAL <- suppressWarnings(as.numeric(as.character(adpc$AVAL)))

# PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc BY USUBJID parcat1 eactm
# SAS sorts with character ordering (uppercase before lowercase) and missing first
# R's order with na.last = TRUE approximates; for character, use base order
SORTadpc <- adpc %>%
  arrange(USUBJID, PARCAT1, EACTM)

# Write dataset CSV (as per spec: each output dataset as CSV)
# The SAS program does not create an output dataset, but we must provide CSV of plotted data
# We will write SORTadpc as the dataset counterpart
# Dates handling: if any date columns exist, format as YYYY-MM-DD
# Identify date/datetime columns by name pattern
date_cols <- grep("DTC$|DT$|DTM$", names(SORTadpc), value = TRUE, ignore.case = TRUE)
for (dc in date_cols) {
  # Try to parse and format as ISO if not already
  if (inherits(SORTadpc[[dc]], "Date")) {
    SORTadpc[[dc]] <- format(SORTadpc[[dc]], "%Y-%m-%d")
  } else if (inherits(SORTadpc[[dc]], "POSIXt")) {
    SORTadpc[[dc]] <- format(SORTadpc[[dc]], "%Y-%m-%dT%H:%M:%S")
  }
}

# Write CSV of sorted ADPC (plotted data)
write.csv(SORTadpc, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# Also write a minimal dataset that matches SAS expected output name if any
# The expected output is figure sasgraph.png -> we also provide dataset as sasgraph.csv (already done)

# Prepare titles/footnotes verbatim from SAS
title1 <- "(page x of x)"
title2 <- "Figure 16.2.5-x.x Individual concentration-time profiles"
title3 <- "by compound, matrix, analyte and [actual/randomised][treatments/group]"
title4 <- "Analysis Set : PK analysis set"
footnote_text <- paste0("Generated by the SAS System (", Sys.info()[["nodename"]], ", ", R.version$platform, ") on ",
                        format(Sys.Date(), "%d%b%Y"), " at ", format(Sys.time(), "%I:%M %p"))

# For reproducibility, also capture footnote as SAS would: use NLDATE20. and TIMEAMPM12.
# We already generated a footnote; keep it for txt output

# Create GPLOT equivalent: PLOT AVAL * eactm BY USUBJID parcat1
# Each BY group is a separate plot page in SAS. In R we facet by USUBJID and PARCAT1
# Use ggplot with line join (INTERPOL=JOIN), VALUE=NONE (no markers), LINE=1, WIDTH=2

# Determine unique BY groups
by_groups <- SORTadpc %>% distinct(USUBJID, PARCAT1) %>% arrange(USUBJID, PARCAT1)
n_groups <- nrow(by_groups)
message("Number of BY groups (USUBJID x PARCAT1): ", n_groups)

# For large number of subjects, faceting all at once would be unreadable.
# SAS GPLOT with BY creates one plot per BY group (multiple pages).
# In R we will create a faceted overview plus ensure the PNG contains all profiles.
# Strategy: create a faceted ggplot with facet_wrap by USUBJID and PARCAT1, but limit to reasonable layout.
# If too many groups (> 100), we will sample or paginate; but we will attempt to plot all with free scales.

# Prepare plot data: remove missing AVAL or EACTM for line drawing, but keep for CSV
plot_data <- SORTadpc %>% filter(!is.na(EACTM), !is.na(AVAL))

# Axis labels as in SAS
x_label <- "Time (hour)"
y_label <- "Conc (ug/mL)"

# Create combined label for faceting: USUBJID + PARCAT1
plot_data <- plot_data %>%
  mutate(BY_LABEL = paste0(USUBJID, " / ", PARCAT1))

# Determine facet layout: try to keep readable
# If n_groups > 60, we will create a multi-page approach but for single PNG we use facet_wrap with many panels
# Use ggplot2

# Order BY_LABEL by USUBJID, PARCAT1 to match SAS BY order
by_order <- SORTadpc %>% distinct(USUBJID, PARCAT1) %>% arrange(USUBJID, PARCAT1) %>%
  mutate(BY_LABEL = paste0(USUBJID, " / ", PARCAT1))
plot_data$BY_LABEL <- factor(plot_data$BY_LABEL, levels = by_order$BY_LABEL)

# Create ggplot
p <- ggplot(plot_data, aes(x = EACTM, y = AVAL, group = interaction(USUBJID, PARCAT1))) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 0.5, color = "black", na.rm = TRUE) +
  facet_wrap(~ BY_LABEL, scales = "free_y", ncol = 4) +
  labs(
    title = paste(title1, "\n", title2, "\n", title3, "\n", title4),
    x = x_label,
    y = y_label,
    caption = footnote_text
  ) +
  theme_bw(base_size = 8) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 9, face = "plain"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6),
    strip.text = element_text(size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"),
    plot.caption = element_text(hjust = 0, size = 6),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Adjust figure size based on number of panels
ncol_fac <- 4
nrow_fac <- ceiling(n_groups / ncol_fac)
# Height: ~1.5 inch per row, width 12 inches
fig_width <- 12
fig_height <- max(6, min(30, 2 + nrow_fac * 1.8))
# If too many groups, cap height and allow scrolling via larger height
if (n_groups > 100) {
  fig_height <- 30
  message("Large number of BY groups, capping figure height to 30 inches")
}

# Save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = fig_width, height = fig_height, units = "in", dpi = 300, limitsize = FALSE)
message("Saved figure to ", png_path)

# Also create a plain-text rendering of the figure data (as required for tables/listings)
# For graph, we provide a txt with titles and summary of plotted data
txt_path <- file.path(out, "sasgraph.txt")
sink(txt_path)
cat(title1, "\n", sep = "")
cat(title2, "\n", sep = "")
cat(title3, "\n", sep = "")
cat(title4, "\n", sep = "")
cat("\n")
cat("Figure 16.2.5-x.x Individual concentration-time profiles\n")
cat("by compound, matrix, analyte and [actual/randomised][treatments/group]\n")
cat("Analysis Set : PK analysis set\n")
cat("\n")
cat("Axis1: Conc (ug/mL)  Axis2: Time (hour)\n")
cat("Plot: AVAL * EACTM  BY USUBJID PARCAT1  INTERPOL=JOIN\n")
cat("\n")
cat("Number of BY groups (USUBJID x PARCAT1): ", n_groups, "\n", sep = "")
cat("Number of observations plotted: ", nrow(plot_data), "\n", sep = "")
cat("Number of observations in SORTadpc: ", nrow(SORTadpc), "\n", sep = "")
cat("\n")
cat("BY groups:\n")
for (i in seq_len(nrow(by_groups))) {
  cat(sprintf("  %s | %s\n", by_groups$USUBJID[i], by_groups$PARCAT1[i]))
}
cat("\n")
cat("Summary of AVAL by PARCAT1:\n")
summ <- SORTadpc %>% group_by(PARCAT1) %>% summarise(
  N = n(),
  Nmiss = sum(is.na(AVAL)),
  Mean = mean(AVAL, na.rm = TRUE),
  Median = median(AVAL, na.rm = TRUE),
  Min = min(AVAL, na.rm = TRUE),
  Max = max(AVAL, na.rm = TRUE),
  .groups = "drop"
)
print(as.data.frame(summ))
cat("\n")
cat("First 20 rows of plotted data (USUBJID, PARCAT1, EACTM, AVAL):\n")
print(head(plot_data %>% select(USUBJID, PARCAT1, EACTM, AVAL) %>% arrange(USUBJID, PARCAT1, EACTM), 20))
cat("\n")
cat(footnote_text, "\n", sep = "")
sink()

# Ensure CSV of plotted data already written as sasgraph.csv
# Also write a second CSV with just the plotted points for verification
write.csv(plot_data %>% select(USUBJID, PARCAT1, EACTM, AVAL) %>% arrange(USUBJID, PARCAT1, EACTM),
          file.path(out, "sasgraph_plotted_data.csv"), row.names = FALSE, na = "")

# Verify outputs exist
stopifnot(file.exists(file.path(out, "sasgraph.png")))
stopifnot(file.exists(file.path(out, "sasgraph.csv")))
stopifnot(file.exists(file.path(out, "sasgraph.txt")))

message("All outputs written to ", out)
