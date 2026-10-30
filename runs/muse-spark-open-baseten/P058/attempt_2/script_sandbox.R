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

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helper SAS COMPRESS: remove any character in chars (case-sensitive)
sas_compress <- function(x, chars) {
  if (is.na(x)) return(NA_character_)
  if (is.na(chars) || chars == "") return(x)
  # split chars into individual characters
  cv <- strsplit(chars, "", fixed = TRUE)[[1]]
  # unique to avoid repeated gsub
  for (ch in unique(cv)) {
    x <- gsub(ch, "", x, fixed = TRUE)
  }
  x
}
sas_compress_vec <- function(vec, chars) {
  vapply(vec, function(v) if (is.na(v)) NA_character_ else sas_compress(v, chars),
         character(1), USE.NAMES = FALSE)
}
sas_best <- function(x) {
  x2 <- trimws(as.character(x))
  x2[x2 == ""] <- NA_character_
  suppressWarnings(as.numeric(x2))
}

# locate adpc.xpt
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adpc.xpt")
)
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) stop("adpc.xpt not found. Checked: ", paste(candidates, collapse="; "))
cat("Reading:", adpc_path, "\n")
adpc_raw <- haven::read_xpt(adpc_path)
cat("Columns in adpc:", paste(names(adpc_raw), collapse=", "), "\n")

# normalize names to lower for processing (SAS is case-insensitive)
names(adpc_raw) <- tolower(names(adpc_raw))

# zap labels/formats
adpc_raw <- adpc_raw %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels)) %>%
  mutate(across(where(haven::is.labelled), haven::zap_formats))

# keep only available of the SAS keep list
keep_vars <- c("usubjid","trta","aperiodc","atpt","parcat3l","aval","adtm","saffn","pkfn","paexcfln","pclloq","parcat1")
keep_present <- intersect(keep_vars, names(adpc_raw))
adpc <- adpc_raw %>% select(all_of(keep_present))

# ensure expected columns exist (create NA if missing to avoid error)
for (v in c("usubjid","trta","atpt","aval","pclloq","parcat1")) {
  if (!v %in% names(adpc)) adpc[[v]] <- NA_character_
}
# show unique values for debugging
cat("Unique TRTA:", paste(unique(as.character(adpc$trta))[1:20], collapse=" | "), "\n")
if ("parcat1" %in% names(adpc)) cat("Unique PARCAT1:", paste(unique(as.character(adpc$parcat1)), collapse=" | "), "\n")
if ("atpt" %in% names(adpc)) cat("Unique ATPT:", paste(unique(as.character(adpc$atpt))[1:20], collapse=" | "), "\n")

# DATA conc;
#  t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
#  concen=aval;
#  order=input(compress(parcat1,' DRUGANALPlasma'),best.);
#  if pclloq ne . then concen=pclloq;
conc <- adpc %>%
  mutate(
    atpt_chr = as.character(atpt),
    parcat1_chr = as.character(parcat1),
    t_compress = sas_compress_vec(atpt_chr, " HOURSPOSTDOSE"),
    t = sas_best(t_compress),
    concen = suppressWarnings(as.numeric(aval)),
    order_compress = sas_compress_vec(parcat1_chr, " DRUGANALPlasma"),
    order = sas_best(order_compress),
    pclloq_num = suppressWarnings(as.numeric(pclloq)),
    concen = ifelse(!is.na(pclloq_num), pclloq_num, concen)
  )

# proc sort by usubjid t;
conc <- conc %>% arrange(usubjid, t)

# write conc dataset CSV (for reference)
conc_out <- conc
# format datetime/date columns if any
for (nm in names(conc_out)) {
  if (inherits(conc_out[[nm]], "POSIXct") || inherits(conc_out[[nm]], "POSIXt")) {
    conc_out[[nm]] <- format(conc_out[[nm]], "%Y-%m-%dT%H:%M:%S")
  } else if (inherits(conc_out[[nm]], "Date")) {
    conc_out[[nm]] <- format(conc_out[[nm]], "%Y-%m-%d")
  }
}
write.csv(conc_out, file.path(out, "conc.csv"), row.names = FALSE, na = "")

# combos: trta A/C x order 1:3  -> 6 figures
combos <- list(
  list(trta="A", order=1),
  list(trta="A", order=2),
  list(trta="A", order=3),
  list(trta="C", order=1),
  list(trta="C", order=2),
  list(trta="C", order=3)
)
png_names <- c("sgrender.png","sgrender1.png","sgrender2.png","sgrender3.png","sgrender4.png","sgrender5.png")
csv_names <- c("sgrender.csv","sgrender1.csv","sgrender2.csv","sgrender3.csv","sgrender4.csv","sgrender5.csv")

width_cm <- 24
height_cm <- 12

for (idx in seq_along(combos)) {
  trta_val <- combos[[idx]]$trta
  order_val <- combos[[idx]]$order

  filtered <- conc %>% filter(trta == trta_val, order == order_val)

  # write plotted data CSV
  filtered_out <- filtered
  for (nm in names(filtered_out)) {
    if (inherits(filtered_out[[nm]], "POSIXct") || inherits(filtered_out[[nm]], "POSIXt")) {
      filtered_out[[nm]] <- format(filtered_out[[nm]], "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(filtered_out[[nm]], "Date")) {
      filtered_out[[nm]] <- format(filtered_out[[nm]], "%Y-%m-%d")
    }
  }
  write.csv(filtered_out, file.path(out, csv_names[idx]), row.names = FALSE, na = "")
  cat(sprintf("Filtered trta=%s order=%s rows=%d -> %s\n", trta_val, order_val, nrow(filtered), csv_names[idx]))

  # Build plots
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

  # Log-linear view - handle non-positive values: ggplot log scale will drop them
  # Use same data; if all <=0, plot will be empty but still render axes
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

  png_path <- file.path(out, png_names[idx])

  # Render using png device + grid layout to mimic SAS template lattice 2 columns
  png(filename = png_path, width = width_cm, height = height_cm, units = "cm", res = 300, bg = "white")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 2,
                                             heights = unit(c(0.8, 0.6, 10.6), "cm"),
                                             widths = unit(c(12, 12), "cm"))))
  grid.text("Overlaid PK Concentration Plots",
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
            gp = gpar(fontsize = 11, fontface = "bold", col = "black"))
  grid.text(paste0("Analyte: ", order_val, "; Treatment=", trta_val),
            vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
            gp = gpar(fontsize = 10, col = "black"))
  # print plots into viewports; use tryCatch to avoid log-scale errors aborting
  tryCatch({
    print(p_linear, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  }, error = function(e) {
    cat("p_linear error:", conditionMessage(e), "\n")
    grid.text("No data", vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  })
  tryCatch({
    print(p_log, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  }, error = function(e) {
    cat("p_log error:", conditionMessage(e), "\n")
    grid.text("No data", vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  })
  dev.off()
  cat(sprintf("Wrote %s\n", png_path))
}

cat("Done. Outputs in:", out, "\n")
