# P058_pk_overlay_conc.sas -> R translation
# Outputs: 6 overlay PK concentration plots (linear + semilog) for order 1..3 x trta A/C

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
rawsplit <- "/bench/data/derived/cdisc-pilot/adam-split"
rawsdtm <- "/bench/data/raw/cdisc-pilot/sdtm"
inlib_dir <- "/bench/data/derived/cdisc-pilot/adam"

# helper: SAS round half away from zero (ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: SAS compress(string, chars) removes any char in chars
sas_compress <- function(x, chars) {
  if (is.null(x)) return(x)
  # build regex character class; escape ] \ ^ -
  esc <- gsub("([\\]\\\\^\\-])", "\\\\\\1", chars, perl = TRUE)
  pat <- paste0("[", esc, "]")
  gsub(pat, "", x, perl = TRUE)
}

# helper: write CSV with ISO dates
write_csv_iso <- function(df, path) {
  df2 <- df
  for (nm in names(df2)) {
    col <- df2[[nm]]
    if (inherits(col, "Date")) {
      df2[[nm]] <- format(col, "%Y-%m-%d")
    } else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
      df2[[nm]] <- format(col, "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(col, "haven_labelled")) {
      # fallback: zap
      df2[[nm]] <- haven::zap_labels(col)
    }
  }
  # ensure character NA becomes "" via na=""
  write.csv(df2, file.path(out, path), row.names = FALSE, na = "")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

# ---- read ADPC ----
adpc_path <- file.path(rawadam, "adpc.xpt")
if (!file.exists(adpc_path)) {
  # fallback to inlib derived
  adpc_path <- file.path(inlib_dir, "adpc.xpt")
  if (!file.exists(adpc_path)) {
    # try sas7bdat
    adpc_path <- file.path(inlib_dir, "adpc.sas7bdat")
  }
}
# haven can read .xpt and .sas7bdat
if (grepl("\\.xpt$", adpc_path, ignore.case = TRUE)) {
  adpc_raw <- haven::read_xpt(adpc_path)
} else {
  adpc_raw <- haven::read_sas(adpc_path)
}
# zap labels/formats to get plain vectors
adpc_raw <- haven::zap_labels(adpc_raw)
adpc_raw <- haven::zap_formats(adpc_raw)

# keep only required columns if present (SAS keep=)
keep_vars <- c("usubjid","trta","aperiodc","atpt","parcat3l","aval","adtm","saffn","pkfn","paexcfln","pclloq","parcat1")
keep_vars_lc <- tolower(keep_vars)
# normalize names to lower case for matching (SAS case-insensitive)
names(adpc_raw) <- tolower(names(adpc_raw))
# if some keep vars missing, keep what exists
keep_exist <- intersect(keep_vars_lc, names(adpc_raw))
adpc <- adpc_raw[, keep_exist, drop = FALSE]

# Ensure character columns are character (haven may give labelled numeric)
# Convert factors to character
for (nm in names(adpc)) {
  if (is.factor(adpc[[nm]])) adpc[[nm]] <- as.character(adpc[[nm]])
}

# ---- DATA step: conc ----
# t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
# concen=aval;
# order=input(compress(parcat1,' DRUGANALPlasma'),best.);
# if pclloq ne . then concen=pclloq;

# Ensure columns exist
if (!"atpt" %in% names(adpc)) adpc$atpt <- NA_character_
if (!"parcat1" %in% names(adpc)) adpc$parcat1 <- NA_character_
if (!"aval" %in% names(adpc)) adpc$aval <- NA_real_
if (!"pclloq" %in% names(adpc)) adpc$pclloq <- NA_real_

conc <- adpc %>%
  mutate(
    # SAS compress removes any character in the second argument
    t_char = sas_compress(as.character(atpt), " HOURSPOSTDOSE"),
    t = suppressWarnings(as.numeric(t_char)),
    concen = suppressWarnings(as.numeric(aval)),
    order_char = sas_compress(as.character(parcat1), " DRUGANALPlasma"),
    order = suppressWarnings(as.numeric(order_char))
  ) %>%
  mutate(
    concen = ifelse(!is.na(pclloq), pclloq, concen)
  )

# labels (for plot)
# t='Time' concen='Concentration (ug/mL)'

# proc sort by usubjid t (SAS missing first, character uppercase before lowercase)
# R arrange with na last would differ; put NA first to match SAS
conc <- conc %>%
  arrange(usubjid, t)

# Ensure trta is character and trimmed
if ("trta" %in% names(conc)) {
  conc$trta <- trimws(as.character(conc$trta))
}

# ---- plotting ----
# Template pkplot: lattice 2 columns, linear and log
# For each order 1..3 and trta A, C

# Define file mapping in SAS order: A1,A2,A3,C1,C2,C3 -> SGRender, SGRender1..5
plot_specs <- expand.grid(order = 1:3, trta = c("A","C"), stringsAsFactors = FALSE)
# expand.grid gives order varying fastest; we need A1,A2,A3,C1,C2,C3
# So sort by trta (A before C) then order
plot_specs <- plot_specs %>% arrange(match(trta, c("A","C")), order)
# file names
png_names <- c("sgrender.png","sgrender1.png","sgrender2.png","sgrender3.png","sgrender4.png","sgrender5.png")
csv_names <- c("sgrender.csv","sgrender1.csv","sgrender2.csv","sgrender3.csv","sgrender4.csv","sgrender5.csv")
txt_names <- c("sgrender.txt","sgrender1.txt","sgrender2.txt","sgrender3.txt","sgrender4.txt","sgrender5.txt")

# dimensions: 24cm x 12cm, 300 dpi
width_in <- 24 / 2.54  # 9.4488
height_in <- 12 / 2.54 # 4.7244
dpi <- 300

for (idx in seq_len(nrow(plot_specs))) {
  ord <- plot_specs$order[idx]
  trt <- plot_specs$trta[idx]
  png_file <- file.path(out, png_names[idx])
  csv_file <- csv_names[idx]
  txt_file <- file.path(out, txt_names[idx])

  sub <- conc %>% filter(order == ord, trta == trt)

  # stopifnot check that filter columns exist (SAS semantics)
  stopifnot("order" %in% names(conc))
  stopifnot("trta" %in% names(conc))

  # Write CSV of plotted data (even if 0 rows, write header)
  # Keep original SAS variable names (lowercase is fine, matching case-insensitive)
  # Include t, concen, order plus key vars
  write_csv_iso(sub, csv_file)

  # Also write txt rendering (counts)
  txt_con <- file(txt_file, open = "wt")
  writeLines(paste0("Overlaid PK Concentration Plots"), txt_con)
  writeLines(paste0("Analyte: ", ord, "; Treatment=", trt), txt_con)
  writeLines(paste0("N observations: ", nrow(sub)), txt_con)
  writeLines(paste0("N subjects: ", length(unique(sub$usubjid))), txt_con)
  if (nrow(sub) > 0) {
    writeLines("", txt_con)
    # summary by time
    summ <- sub %>% group_by(t) %>% summarise(n=n(), mean_concen=mean(concen, na.rm=TRUE), .groups="drop")
    writeLines("Summary by Time:", txt_con)
    for (r in seq_len(nrow(summ))) {
      writeLines(sprintf(" t=%s n=%d mean=%.4f", as.character(summ$t[r]), summ$n[r], summ$mean_concen[r]), txt_con)
    }
  } else {
    writeLines("No data for this stratum.", txt_con)
  }
  close(txt_con)

  # Build plots (even if empty, produce empty panel with titles)
  title_main <- "Overlaid PK Concentration Plots"
  title_sub <- paste0("Analyte: ", ord, "; Treatment=", trt)

  # Linear view
  p_linear <- ggplot(sub, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    labs(x = "Time", y = "Concentration (ug/mL)", title = "Linear view") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
    )

  # Log-linear view - handle non-positive values: SAS would omit or clip; we filter <=0 for log scale
  # Keep data as is but set y scale log10; if all <=0, plot will be empty
  p_log <- ggplot(sub, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    labs(x = "Time", y = NULL, title = "Semilogarithmic view") +
    scale_y_log10() +
    annotation_logticks(sides = "l", outside = FALSE) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
    )

  # Combine using grid viewports (no extra package)
  png(filename = png_file, width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
  grid.newpage()
  # layout: 3 rows (title, subtitle, plots) x 2 cols
  lay <- grid.layout(nrow = 3, ncol = 2,
                     heights = unit(c(0.12, 0.08, 0.80), "npc"),
                     widths = unit(c(0.5, 0.5), "npc"))
  pushViewport(viewport(layout = lay))
  # titles spanning both columns
  grid.text(title_main, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
            gp = gpar(fontsize = 12, fontface = "bold"))
  grid.text(title_sub, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
            gp = gpar(fontsize = 10))
  # plots
  print(p_linear, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(p_log, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  dev.off()
}

# Also ensure overall dataset CSV if needed (not required but helpful)
# Write conc dataset as reference
write_csv_iso(conc, "conc.csv")
