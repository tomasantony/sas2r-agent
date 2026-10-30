```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

sas_compress_remove <- function(x, chars_to_remove) {
  rm_chars <- strsplit(chars_to_remove, "", fixed = TRUE)[[1]]
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    for (ch in rm_chars) {
      s <- gsub(ch, "", s, fixed = TRUE)
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adpc_path <- file.path(rawadam, "adpc.xpt")
alt_path <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt"

src_path <- adpc_path
if (!file.exists(src_path) && file.exists(alt_path)) src_path <- alt_path
message("Reading ADPC from: ", src_path)
stopifnot(file.exists(src_path))

adpc_raw <- haven::read_xpt(src_path)
# normalize names to upper for processing
orig_names <- names(adpc_raw)
names(adpc_raw) <- toupper(names(adpc_raw))

message(paste0("ADPC rows=", nrow(adpc_raw), " cols=", paste(names(adpc_raw), collapse=",")))
# show unique values for key vars (truncated, not full data)
if ("TRTA" %in% names(adpc_raw)) message("TRTA unique: ", paste(utils::head(unique(adpc_raw$TRTA), 20), collapse=" | "))
if ("PARCAT1" %in% names(adpc_raw)) message("PARCAT1 unique: ", paste(utils::head(unique(adpc_raw$PARCAT1), 20), collapse=" | "))
if ("ATPT" %in% names(adpc_raw)) message("ATPT unique: ", paste(utils::head(unique(adpc_raw$ATPT), 20), collapse=" | "))

keep_vars <- c("USUBJID","TRTA","APERIODC","ATPT","PARCAT3L","AVAL","ADTM","SAFFN","PKFN","PAEXCFLN","PCLLOQ","PARCAT1")
keep_vars <- keep_vars[keep_vars %in% names(adpc_raw)]
stopifnot(all(c("USUBJID","TRTA","ATPT","AVAL","PARCAT1") %in% names(adpc_raw)))
conc <- adpc_raw[, keep_vars, drop = FALSE]

# t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
# order=input(compress(parcat1,' DRUGANALPlasma'),best.);
atpt_char <- as.character(conc$ATPT)
parcat1_char <- as.character(conc$PARCAT1)
t_comp <- sas_compress_remove(atpt_char, " HOURSPOSTDOSE")
o_comp <- sas_compress_remove(parcat1_char, " DRUGANALPlasma")
conc$T <- suppressWarnings(as.numeric(t_comp))
conc$CONCEN <- suppressWarnings(as.numeric(conc$AVAL))
# Apply LLOQ: if pclloq ne . then concen=pclloq;
if ("PCLLOQ" %in% names(conc)) {
  pcl <- suppressWarnings(as.numeric(conc$PCLLOQ))
  idx <- !is.na(pcl)
  conc$CONCEN[idx] <- pcl[idx]
}
conc$ORDER <- suppressWarnings(as.numeric(o_comp))

message("T unique (head): ", paste(utils::head(sort(unique(conc$T)), 20), collapse=","))
message("ORDER unique: ", paste(sort(unique(conc$ORDER)), collapse=","))
message("TRTA unique in conc: ", paste(unique(conc$TRTA), collapse=" | "))

# proc sort data=conc; by usubjid t; run;  (missing sorts first)
ord_idx <- order(conc$USUBJID, conc$T, na.last = FALSE)
conc <- conc[ord_idx, , drop = FALSE]

# helper to format ADTM for CSV (ISO)
fmt_csv_df <- function(df) {
  df2 <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df2)) {
    v <- df2[[nm]]
    if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
      df2[[nm]] <- format(v, "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(v, "Date")) {
      df2[[nm]] <- format(v, "%Y-%m-%d")
    }
  }
  df2
}

# define 6 combinations in SAS ODS order
combos <- list(
  list(analyte = 1, trta = "A", base = "sgrender"),
  list(analyte = 2, trta = "A", base = "sgrender1"),
  list(analyte = 3, trta = "A", base = "sgrender2"),
  list(analyte = 1, trta = "C", base = "sgrender3"),
  list(analyte = 2, trta = "C", base = "sgrender4"),
  list(analyte = 3, trta = "C", base = "sgrender5")
)

stopifnot(all(c("ORDER","TRTA") %in% names(conc)))

build_plots <- function(dsub) {
  # linear data: non-missing t and concen
  dlin <- dsub[!is.na(dsub$T) & !is.na(dsub$CONCEN), , drop = FALSE]
  dlog <- dlin[dlin$CONCEN > 0, , drop = FALSE]
  p1 <- ggplot2::ggplot()
  if (nrow(dlin) > 0) {
    p1 <- ggplot2::ggplot(dlin, ggplot2::aes(x = T, y = CONCEN, group = USUBJID)) +
      ggplot2::geom_line(color = "black", linewidth = 0.4, na.rm = TRUE)
  } else {
    p1 <- ggplot2::ggplot(data.frame(T = numeric(0), CONCEN = numeric(0)), ggplot2::aes(x = T, y = CONCEN)) +
      ggplot2::geom_blank()
  }
  p1 <- p1 +
    ggplot2::labs(title = "Linear view", x = "Time", y = "Concentration (ug/mL)") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                   axis.title = ggplot2::element_text(size = 8),
                   axis.text = ggplot2::element_text(size = 7))
  p2 <- ggplot2::ggplot()
  if (nrow(dlog) > 0) {
    p2 <- ggplot2::ggplot(dlog, ggplot2::aes(x = T, y = CONCEN, group = USUBJID)) +
      ggplot2::geom_line(color = "black", linewidth = 0.4, na.rm = TRUE)
  } else {
    p2 <- ggplot2::ggplot(data.frame(T = numeric(0), CONCEN = numeric(0)), ggplot2::aes(x = T, y = CONCEN)) +
      ggplot2::geom_blank()
  }
  p2 <- p2 +
    ggplot2::scale_y_log10(breaks = scales_log_breaks_fallback()) +
    ggplot2::annotation_logticks(sides = "l", outside = FALSE) +
    ggplot2::labs(title = "Semilogarithmic view", x = "Time", y = " ") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                   axis.title = ggplot2::element_text(size = 8),
                   axis.text = ggplot2::element_text(size = 7))
  list(p1 = p1, p2 = p2)
}

scales_log_breaks_fallback <- function() {
  # mimic SAS log base 10 ticks: use standard log breaks
  function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] <= 0) return(c(0.1, 1, 10, 100, 1000))
    lo <- floor(log10(max(rng[1], 1e-12)))
    hi <- ceiling(log10(max(rng[2], 1e-12)))
    10^(seq(lo, hi))
  }
}

for (cb in combos) {
  analyte <- cb$analyte
  trtav <- cb$trta
  base <- cb$base
  title1 <- "Overlaid PK Concentration Plots"
  title2 <- paste0("Analyte: ", analyte, "; Treatment=", trtav)
  dsub <- conc[conc$ORDER == analyte & !is.na(conc$ORDER) & conc$TRTA == trtav, , drop = FALSE]
  message(base, ": analyte=", analyte, " trta=", trtav, " n=", nrow(dsub))
  # CSV of plotted data
  csv_path <- file.path(out, paste0(base, ".csv"))
  utils::write.csv(fmt_csv_df(dsub), csv_path, row.names = FALSE, na = "")
  # build ggplots
  pp <- build_plots(dsub)
  p1 <- pp$p1
  p2 <- pp$p2
  png_path <- file.path(out, paste0(base, ".png"))
  grDevices::png(filename = png_path, width = 24, height = 12, units = "cm", res = 300)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 3, ncol = 2,
    heights = grid::unit(c(0.12, 0.08, 0.80), "npc"),
    widths = grid::unit(c(0.5, 0.5), "npc"))))
  grid::grid.text(title1,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2),
    gp = grid::gpar(fontsize = 12, fontface = "bold"))
  grid::grid.text(title2,
    vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1:2),
    gp = grid::gpar(fontsize = 10))
  print(p1, vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(p2, vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid::popViewport()
  grDevices::dev.off()
  message("wrote ", png_path, " and ", csv_path)
}
```