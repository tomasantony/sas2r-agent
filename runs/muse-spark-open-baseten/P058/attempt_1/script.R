# P058_pk_overlay_conc.sas -> R translation
# Outputs: 6 overlay PK concentration plots (linear + semilog) for Analyte 1-3 x Treatment A/C

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
  library(grid)
})

# helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS COMPRESS(x, chars) - remove any character in chars (case-sensitive, fixed)
sas_compress <- function(x, chars) {
  if (is.na(chars) || chars == "") return(x)
  # remove each character literally
  chars_vec <- strsplit(chars, "", fixed = TRUE)[[1]]
  out <- x
  for (ch in unique(chars_vec)) {
    out <- gsub(ch, "", out, fixed = TRUE)
  }
  out
}

# helper: SAS INPUT(compress(...), best.) -> numeric, blank => NA
sas_best <- function(x) {
  # trim spaces
  x2 <- trimws(x)
  x2[x2 == ""] <- NA_character_
  suppressWarnings(as.numeric(x2))
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# -------------------------------------------------------------------------
# Locate adpc.xpt
# SAS: filename source "&rawadam./adpc.xpt";  &rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  file.path("data/raw/cdisc-pilot/adam", "adpc.xpt")
)
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) {
  # fallback: search under root
  root <- Sys.getenv("root")
  if (root != "") {
    pp <- file.path(root, "data/raw/cdisc-pilot/adam/adpc.xpt")
    if (file.exists(pp)) adpc_path <- pp
  }
}
if (is.null(adpc_path)) {
  # last resort: try haven read from inlib path
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt"
  if (file.exists(alt)) adpc_path <- alt
}
if (is.null(adpc_path)) stop("adpc.xpt not found. Checked: ", paste(candidates, collapse="; "))

cat("Reading:", adpc_path, "\n")
adpc_raw <- haven::read_xpt(adpc_path)

# Convert haven_labelled to plain vectors (keep values)
adpc_raw <- adpc_raw %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels)) %>%
  mutate(across(where(haven::is.labelled), haven::zap_formats))

# Keep only required variables if they exist (SAS keep= list)
keep_vars <- c("usubjid","trta","aperiodc","atpt","parcat3l","aval","adtm","saffn","pkfn","paexcfln","pclloq","parcat1")
keep_vars_present <- intersect(keep_vars, names(adpc_raw))
adpc <- adpc_raw %>% select(all_of(keep_vars_present))

# -------------------------------------------------------------------------
# DATA conc;
#  t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
#  concen=aval;
#  order=input(compress(parcat1,' DRUGANALPlasma'),best.);
#  if pclloq ne . then concen=pclloq;
# -------------------------------------------------------------------------
conc <- adpc %>%
  mutate(
    atpt_chr = as.character(atpt),
    parcat1_chr = as.character(parcat1),
    t_compress = sas_compress(atpt_chr, " HOURSPOSTDOSE"),
    t = sas_best(t_compress),
    concen = suppressWarnings(as.numeric(aval)),
    order_compress = sas_compress(parcat1_chr, " DRUGANALPlasma"),
    order = sas_best(order_compress)
  ) %>%
  mutate(
    concen = ifelse(!is.na(pclloq), as.numeric(pclloq), concen)
  )

# SAS labels: t='Time', concen='Concentration (ug/mL)' - keep as attributes for reference
attr(conc$t, "label") <- "Time"
attr(conc$concen, "label") <- "Concentration (ug/mL)"

# proc sort by usubjid t;
conc <- conc %>% arrange(usubjid, t)

# Write conc dataset as CSV (dataset output)
# Format dates as YYYY-MM-DD and datetimes as YYYY-MM-DDTHH:MM:SS
conc_out <- conc
# handle adtm datetime formatting if present
if ("adtm" %in% names(conc_out)) {
  # haven reads datetimes as POSIXct if formatted, else numeric
  # Try to format POSIXct
  if (inherits(conc_out$adtm, "POSIXct") || inherits(conc_out$adtm, "POSIXt")) {
    conc_out$adtm <- format(conc_out$adtm, "%Y-%m-%dT%H:%M:%S")
  } else if (is.numeric(conc_out$adtm)) {
    # SAS datetime is seconds since 1960-01-01; haven may have converted already
    # If numeric and large, attempt conversion only if plausible
    # Leave as is if not POSIX
  }
}
# also handle any Date columns
date_cols <- sapply(conc_out, function(x) inherits(x, "Date"))
for (nm in names(conc_out)[date_cols]) {
  conc_out[[nm]] <- format(conc_out[[nm]], "%Y-%m-%d")
}
write.csv(conc_out, file.path(out, "conc.csv"), row.names = FALSE, na = "")

# Also write a generic dataset output expected by harness (if any)
# The SAS libref outds would contain conc; we already wrote conc.csv

# -------------------------------------------------------------------------
# Plotting: 6 figures for trta=A,C and order=1:3
# Template pkplot: lattice columns=2 (linear + log)
# -------------------------------------------------------------------------
combos <- list(
  list(trta="A", order=1),
  list(trta="A", order=2),
  list(trta="A", order=3),
  list(trta="C", order=1),
  list(trta="C", order=2),
  list(trta="C", order=3)
)

# PNG names matching SAS ODS: SGRender.png, SGRender1.png ... SGRender5.png
png_names <- c("sgrender.png","sgrender1.png","sgrender2.png","sgrender3.png","sgrender4.png","sgrender5.png")
csv_names <- c("sgrender.csv","sgrender1.csv","sgrender2.csv","sgrender3.csv","sgrender4.csv","sgrender5.csv")

# width=24cm height=12cm, dpi 300
width_cm <- 24
height_cm <- 12
width_in <- width_cm / 2.54
height_in <- height_cm / 2.54

for (idx in seq_along(combos)) {
  trta_val <- combos[[idx]]$trta
  order_val <- combos[[idx]]$order

  filtered <- conc %>% filter(trta == trta_val, order == order_val)

  # Save plotted data CSV (even if 0 rows, write header)
  filtered_out <- filtered
  # format datetime columns for CSV
  if ("adtm" %in% names(filtered_out) && inherits(filtered_out$adtm, "POSIXct")) {
    filtered_out$adtm <- format(filtered_out$adtm, "%Y-%m-%dT%H:%M:%S")
  }
  # format Date columns
  dc2 <- sapply(filtered_out, function(x) inherits(x, "Date"))
  for (nm in names(filtered_out)[dc2]) filtered_out[[nm]] <- format(filtered_out[[nm]], "%Y-%m-%d")
  write.csv(filtered_out, file.path(out, csv_names[idx]), row.names = FALSE, na = "")

  # Build plots (even if empty, produce empty axes)
  # Linear view
  p_linear <- ggplot(filtered, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    labs(x = "Time", y = "Concentration (ug/mL)", title = "Linear view") +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "plain"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 10, 5, 5)
    )

  # Log-linear view
  p_log <- ggplot(filtered, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    labs(x = "Time", y = "", title = "Semilogarithmic view") +
    scale_y_log10(
      breaks = scales::log_breaks(base = 10),
      labels = scales::label_number()
    ) +
    annotation_logticks(sides = "l", outside = FALSE, short = unit(0.05, "cm"), mid = unit(0.08, "cm"), long = unit(0.12, "cm")) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "plain"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 10, 5, 5)
    )

  # Handle case where all concen <=0 for log scale: ggplot will warn and drop; keep plot

  png_path <- file.path(out, png_names[idx])

  # Use png device with grid layout to mimic ODS titles
  png(filename = png_path, width = width_cm, height = height_cm, units = "cm", res = 300, bg = "white")
  grid.newpage()
  # layout: 3 rows (title1, title2, plots) x 2 cols
  pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 2,
                                             heights = unit(c(0.8, 0.6, 10.6), "cm"),
                                             widths = unit(c(12, 12), "cm"))))
  # Title1
  grid.text("Overlaid PK Concentration Plots",
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
            gp = gpar(fontsize = 11, fontface = "bold", col = "black"))
  # Title2
  grid.text(paste0("Analyte: ", order_val, "; Treatment=", trta_val),
            vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
            gp = gpar(fontsize = 10, col = "black"))
  # Linear plot
  print(p_linear, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  # Log plot
  print(p_log, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  dev.off()

  cat(sprintf("Wrote %s (%d rows) and %s\n", png_names[idx], nrow(filtered), csv_names[idx]))
}

cat("Done. Outputs in:", out, "\n")
