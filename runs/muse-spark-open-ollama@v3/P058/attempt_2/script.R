# P058_pk_overlay_conc.sas -> R translation
# Outputs: 6 overlay PK concentration plots (linear + semilog) for Analyte 1-3 x Treatment A,C

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS COMPRESS - remove any character in chars (case-sensitive, fixed)
compress_sas <- function(x, chars) {
  if (is.na(x)) return(NA_character_)
  chars_vec <- strsplit(chars, "", fixed = TRUE)[[1]]
  for (ch in chars_vec) {
    x <- gsub(ch, "", x, fixed = TRUE)
  }
  x
}
compress_vec <- function(v, chars) {
  v <- as.character(v)
  sapply(v, function(x) if (is.na(x)) NA_character_ else compress_sas(x, chars), USE.NAMES = FALSE)
}
# helper: SAS INPUT(...,best.) -> numeric
sas_best <- function(v) suppressWarnings(as.numeric(v))

# helper: find file among candidates
find_file <- function(cands) {
  for (f in cands) {
    if (!is.na(f) && file.exists(f)) return(f)
  }
  return(NA_character_)
}

# locate adpc.xpt
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  "data/raw/cdisc-pilot/adam/adpc.xpt",
  "../data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
  "/tmp/data/raw/cdisc-pilot/adam/adpc.xpt",
  file.path(getwd(), "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt")
)
# also try to search relative to OUT_DIR
if (out != "") {
  candidates <- c(candidates,
    file.path(dirname(out), "data/raw/cdisc-pilot/adam/adpc.xpt"),
    file.path(dirname(dirname(out)), "data/raw/cdisc-pilot/adam/adpc.xpt")
  )
}
# brute search for adpc.xpt under current working directory (limited depth)
try({
  found <- list.files(getwd(), pattern = "^adpc\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(found) > 0) candidates <- c(candidates, found[1:5])
}, silent = TRUE)

adpc_path <- find_file(candidates)
if (is.na(adpc_path)) {
  # last resort: search filesystem for adpc.xpt in common locations
  alt <- c("/github", "/workspace", "/opt", ".")
  for (base in alt) {
    if (dir.exists(base)) {
      ff <- list.files(base, pattern = "adpc\\.xpt", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(ff) > 0) { adpc_path <- ff[1]; break }
    }
  }
}
if (is.na(adpc_path) || !file.exists(adpc_path)) {
  stop(paste0("adpc.xpt not found. Tried: ", paste(candidates, collapse="; ")))
}
cat("Reading:", adpc_path, "\n")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(grid)
})

# read XPT
adpc_raw <- haven::read_xpt(adpc_path, .name_repair = "minimal")

# zap labels/formats to plain vectors, keep names
adpc_raw <- haven::zap_formats(adpc_raw)
adpc_raw <- haven::zap_label(adpc_raw)
adpc_raw <- haven::zap_labels(adpc_raw)

# standardize names to lower for processing, but remember original case for output
names(adpc_raw) <- tolower(names(adpc_raw))

# ensure expected columns exist (case-insensitive)
# keep=usubjid trta aperiodc atpt parcat3l aval adtm saffn pkfn paexcfln pclloq parcat1
needed <- c("usubjid","trta","aperiodc","atpt","parcat3l","aval","adtm","saffn","pkfn","paexcfln","pclloq","parcat1")
missing_needed <- setdiff(needed, names(adpc_raw))
if (length(missing_needed) > 0) {
  cat("Warning: missing columns in adpc:", paste(missing_needed, collapse=","), "\n")
  for (col in missing_needed) adpc_raw[[col]] <- NA
}

# Bring in specified data; SAS:
# t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
# concen=aval;
# order=input(compress(parcat1,' DRUGANALPlasma'),best.);
# if pclloq ne . then concen=pclloq;
conc <- adpc_raw %>%
  mutate(
    # keep original character columns as character
    usubjid = as.character(usubjid),
    trta = as.character(trta),
    aperiodc = as.character(aperiodc),
    atpt = as.character(atpt),
    parcat3l = as.character(parcat3l),
    parcat1 = as.character(parcat1),
    aval = suppressWarnings(as.numeric(aval)),
    pclloq = suppressWarnings(as.numeric(pclloq)),
    # SAS compress is case-sensitive; chars include space + uppercase HOURSPOSTDOSE
    t = sas_best(compress_vec(atpt, " HOURSPOSTDOSE")),
    concen = aval,
    order = sas_best(compress_vec(parcat1, " DRUGANALPlasma"))
  )

# Apply LLOQ: if pclloq ne . then concen=pclloq;
conc <- conc %>%
  mutate(concen = ifelse(!is.na(pclloq), pclloq, concen))

# labels (for plot)
# t='Time' concen='Concentration (ug/mL)'

# sort by usubjid t (SAS: by usubjid t; missing first)
conc <- conc %>%
  arrange(usubjid, t)

# Prepare output dataset: keep original keep vars + derived t, concen, order
# Ensure dates/datetimes formatted as ISO when writing CSV
# adtm may be numeric SAS datetime or character; handle both
format_iso_datetime <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  # if numeric SAS datetime (seconds since 1960-01-01) - haven may have converted?
  # haven usually converts SAS dates to Date/POSIXct automatically; if still numeric, leave as is
  return(x)
}

# Create a copy for CSV output with ISO formatting for date vars
conc_out <- conc
# Identify date/datetime columns by name pattern
for (col in names(conc_out)) {
  if (grepl("dtm$", col, ignore.case = TRUE) || grepl("dt$", col, ignore.case = TRUE)) {
    # try to format if Date/POSIX
    if (inherits(conc_out[[col]], "POSIXct") || inherits(conc_out[[col]], "POSIXt")) {
      conc_out[[col]] <- format(conc_out[[col]], "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(conc_out[[col]], "Date")) {
      conc_out[[col]] <- format(conc_out[[col]], "%Y-%m-%d")
    }
  }
}

# Write conc dataset as CSV (SAS dataset counterpart)
write.csv(conc_out, file.path(out, "conc.csv"), row.names = FALSE, na = "")

# Also write a txt rendering of conc head for verification (not required but helpful)
# Keep intermediate steps close to SAS structure

# Plotting function: reproduces PROC TEMPLATE PKPlot
# lattice 2 columns: linear view and semilog view, seriesplot x=t y=concen/group=usubjid black solid

make_plots <- function(df_sub, title1, title2) {
  # df_sub: filtered conc for one order/trta
  # Ensure sorted by usubjid t
  df_sub <- df_sub %>% arrange(usubjid, t)

  # If no data, create empty placeholder
  if (nrow(df_sub) == 0) {
    df_sub <- data.frame(usubjid=character(0), t=numeric(0), concen=numeric(0))
  }

  # Common theme mimicking SAS listing style, walldisplay=none border=false
  base_theme <- theme_bw(base_size = 10) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.4),
      plot.title = element_text(hjust = 0.5, size = 10, face = "plain"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    )

  # Linear plot
  p_linear <- ggplot(df_sub, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.5, na.rm = TRUE) +
    labs(x = "Time", y = "Concentration (ug/mL)", title = "Linear view") +
    base_theme +
    theme(plot.title = element_text(hjust = 0.5, size = 10))

  # Log-linear plot: yaxisopts=(type=log logopts=(base=10 minorticks=TRUE) label=(' '))
  # Use log10 scale, minor ticks, blank y label
  # Handle non-positive values: SAS would not plot them on log scale; we filter to >0 for log panel
  df_log <- df_sub %>% filter(!is.na(concen) & concen > 0)

  p_log <- ggplot(df_log, aes(x = t, y = concen, group = usubjid)) +
    geom_line(color = "black", linewidth = 0.5, na.rm = TRUE) +
    scale_y_log10(
      breaks = scales::log_breaks(base = 10),
      minor_breaks = NULL
    ) +
    annotation_logticks(sides = "l", base = 10, outside = FALSE, linewidth = 0.3) +
    labs(x = "Time", y = "", title = "Semilogarithmic view") +
    base_theme +
    theme(plot.title = element_text(hjust = 0.5, size = 10))

  # If no positive data, still create empty log plot with log scale
  if (nrow(df_log) == 0) {
    p_log <- ggplot(df_sub, aes(x = t, y = concen, group = usubjid)) +
      geom_line(color = "black", linewidth = 0.5, na.rm = TRUE) +
      scale_y_log10() +
      labs(x = "Time", y = "", title = "Semilogarithmic view") +
      base_theme
  }

  list(linear = p_linear, log = p_log, data_linear = df_sub, data_log = df_log)
}

# Macro %plot(trta=) %do i=1 %to 3; title1 ... title2 ... proc sgrender where order=&I and trta="&TRTA"
# For trta=A then trta=C, each i=1..3 => 6 figures
# Mapping to SAS ODS listing names: SGRender.png, SGRender1.png ... SGRender5.png
trta_list <- c("A", "C")
order_list <- 1:3

idx <- 0
for (trta_val in trta_list) {
  for (ord in order_list) {
    file_base <- if (idx == 0) "sgrender" else paste0("sgrender", idx)
    title1 <- "Overlaid PK Concentration Plots"
    title2 <- paste0("Analyte: ", ord, "; Treatment=", trta_val)

    df_filt <- conc %>% filter(order == ord & trta == trta_val)

    # stopifnot that filter columns exist (SAS semantics check)
    stopifnot("order" %in% names(conc))
    stopifnot("trta" %in% names(conc))

    plots <- make_plots(df_filt, title1, title2)

    # Prepare combined grobs
    g1 <- ggplotGrob(plots$linear)
    g2 <- ggplotGrob(plots$log)

    # Create PNG with ODS graphics dimensions: width=24cm height=12cm, dpi=300, noborder
    png_path <- file.path(out, paste0(file_base, ".png"))
    # width 24cm = 9.4488in, height 12cm = 4.7244in at 300dpi
    png(filename = png_path, width = 24, height = 12, units = "cm", res = 300, bg = "white")
    grid.newpage()
    # Layout: 1 row, 2 cols equal weights .50 .50, plus top titles
    # Use grid layout with 2 rows: titles row and plots row
    pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 2,
      heights = unit(c(0.08, 0.07, 0.85), "npc"),
      widths = unit(c(0.5, 0.5), "npc"))))

    # Title1
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
    grid.text(title1, gp = gpar(fontsize = 12, fontface = "bold"), y = unit(0.5, "npc"))
    popViewport()
    # Title2
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
    grid.text(title2, gp = gpar(fontsize = 10), y = unit(0.5, "npc"))
    popViewport()

    # Left panel (linear)
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
    grid.draw(g1)
    popViewport()
    # Right panel (log)
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
    grid.draw(g2)
    popViewport()

    popViewport() # outer
    dev.off()
    cat("Wrote", png_path, "with", nrow(df_filt), "rows\n")

    # CSV of plotted data (SAS semantics: same data used for rendering)
    # Write filtered conc sorted by usubjid t, with key columns
    csv_path <- file.path(out, paste0(file_base, ".csv"))
    # Select relevant columns for plotted data, keep SAS variable names
    # Include usubjid, trta, order, t, concen, atpt, aval, pclloq, parcat1
    out_df <- df_filt %>%
      select(any_of(c("usubjid","trta","order","t","concen","atpt","aval","pclloq","parcat1","aperiodc","parcat3l","adtm","saffn","pkfn","paexcfln"))) %>%
      arrange(usubjid, t)
    # Format dates for CSV
    for (col in names(out_df)) {
      if (inherits(out_df[[col]], "POSIXct") || inherits(out_df[[col]], "POSIXt")) {
        out_df[[col]] <- format(out_df[[col]], "%Y-%m-%dT%H:%M:%S")
      } else if (inherits(out_df[[col]], "Date")) {
        out_df[[col]] <- format(out_df[[col]], "%Y-%m-%d")
      }
    }
    write.csv(out_df, csv_path, row.names = FALSE, na = "")

    idx <- idx + 1
  }
}

cat("Done. Outputs in", out, "\n")
