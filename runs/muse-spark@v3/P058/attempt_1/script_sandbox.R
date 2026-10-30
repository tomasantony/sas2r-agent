# P058 pk_overlay_conc - R translation
# Overlaid PK Concentration Plots (linear + semilog) by Analyte and Treatment

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

adpc_path <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"

cat("Reading ADPC:", adpc_path, "\n")
adpc <- haven::read_xpt(adpc_path)

# Normalize names to uppercase for processing
names(adpc) <- toupper(names(adpc))
cat("ADPC rows:", nrow(adpc), " cols:", paste(names(adpc), collapse=","), "\n")

# SAS helpers ---------------------------------------------------------------
sas_compress <- function(x, chars) {
  # Remove every character in `chars` (case-sensitive), vectorized, like SAS COMPRESS(x, chars)
  if (is.null(x)) return(x)
  xc <- as.character(x)
  ch_vec <- strsplit(chars, "", fixed = TRUE)[[1]]
  res <- xc
  for (ch in ch_vec) {
    # preserve NA
    idx <- !is.na(res)
    res[idx] <- gsub(ch, "", res[idx], fixed = TRUE)
  }
  res
}

sas_best <- function(x) {
  # SAS INPUT(..., BEST.) : parse leading numeric, missing if none
  sapply(x, function(s) {
    if (is.na(s)) return(NA_real_)
    s2 <- sub("^[ ]+", "", s, perl = TRUE)
    if (s2 == "") return(NA_real_)
    m <- regexpr("^[+-]?(\\.[0-9]+|[0-9]+\\.?[0-9]*)([Ee][+-]?[0-9]+)?", s2, perl = TRUE)
    if (m == 1) {
      len <- attr(m, "match.length")
      numstr <- substr(s2, 1, len)
      suppressWarnings(as.numeric(numstr))
    } else {
      NA_real_
    }
  }, USE.NAMES = FALSE)
}

# Keep specified vars (case-insensitive); only those present
keep_want <- c("USUBJID","TRTA","APERIODC","ATPT","PARCAT3L","AVAL","ADTM","SAFFN","PKFN","PAEXCFLN","PCLLOQ","PARCAT1")
keep_have <- intersect(keep_want, names(adpc))
# also keep any extra needed? keep all if missing check
conc <- adpc[, keep_have, drop = FALSE]

# Ensure character vars are character (haven labelled -> character)
for (cc in c("USUBJID","TRTA","APERIODC","ATPT","PARCAT3L","PARCAT1")) {
  if (cc %in% names(conc)) {
    # zap labels then as.character
    v <- conc[[cc]]
    if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
    conc[[cc]] <- as.character(v)
  }
}
# Ensure numeric vars numeric
for (cc in c("AVAL","PCLLOQ","SAFFN","PKFN","PAEXCFLN")) {
  if (cc %in% names(conc)) {
    v <- conc[[cc]]
    if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
    conc[[cc]] <- suppressWarnings(as.numeric(v))
  }
}
# ADTM may be datetime
if ("ADTM" %in% names(conc)) {
  v <- conc[["ADTM"]]
  if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
  conc[["ADTM"]] <- v
}

# Bring in specified data ---------------------------------------------------
# t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
# concen=aval;
# order=input(compress(parcat1,' DRUGANALPlasma'),best.);
# if pclloq ne . then concen=pclloq;
conc$t <- sas_best(sas_compress(conc$ATPT, " HOURSPOSTDOSE"))
conc$concen <- conc$AVAL
if ("PCLLOQ" %in% names(conc)) {
  idx <- !is.na(conc$PCLLOQ)
  conc$concen[idx] <- conc$PCLLOQ[idx]
}
conc$order <- sas_best(sas_compress(conc$PARCAT1, " DRUGANALPlasma"))

# label t='Time' concen='Concentration (ug/mL)' (used in plots)

# proc sort data=conc; by usubjid t; run;  (SAS missing first, ASCII order)
oldloc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
conc <- conc[base::order(conc$USUBJID, conc$t, na.last = FALSE), , drop = FALSE]
suppressWarnings(try(Sys.setlocale("LC_COLLATE", oldloc), silent = TRUE))

cat("CONC rows:", nrow(conc), "\n")
print(table(conc$order, useNA = "ifany"))
if ("TRTA" %in% names(conc)) print(table(conc$TRTA, useNA = "ifany"))

# Helper to format dates for CSV -------------------------------------------
fmt_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    v <- df2[[nm]]
    if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
      df2[[nm]] <- format(v, format = "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(v, "Date")) {
      df2[[nm]] <- format(v, format = "%Y-%m-%d")
    } else if (inherits(v, "haven_labelled")) {
      df2[[nm]] <- haven::zap_labels(v)
    }
  }
  # haven labelled columns that are POSIX? already handled
  df2
}

# Write full conc dataset as well (dataset output)
conc_out <- fmt_for_csv(conc)
write.csv(conc_out, file.path(out, "conc.csv"), row.names = FALSE, na = "")

# Plot definitions ----------------------------------------------------------
# Template PKPlot: lattice 2 cols, linear + log, seriesplot black solid grouped by usubjid

combos <- list(
  list(trta = "A", analy = 1, suffix = "sgrender"),
  list(trta = "A", analy = 2, suffix = "sgrender1"),
  list(trta = "A", analy = 3, suffix = "sgrender2"),
  list(trta = "C", analy = 1, suffix = "sgrender3"),
  list(trta = "C", analy = 2, suffix = "sgrender4"),
  list(trta = "C", analy = 3, suffix = "sgrender5")
)

make_two_panel <- function(sub, trta, analy, pngfile) {
  title1 <- "Overlaid PK Concentration Plots"
  title2 <- paste0("Analyte: ", analy, "; Treatment=", trta)

  # Linear plot
  p1 <- ggplot(sub, aes(x = t, y = concen, group = USUBJID)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    labs(x = "Time", y = "Concentration (ug/mL)", title = "Linear view") +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          panel.grid = element_blank())

  # Log-linear plot (base 10, minor ticks, blank y label like SAS label=(' '))
  p2 <- ggplot(sub, aes(x = t, y = concen, group = USUBJID)) +
    geom_line(color = "black", linewidth = 0.4, na.rm = TRUE) +
    scale_y_log10() +
    annotation_logticks(sides = "l") +
    labs(x = "Time", y = " ", title = "Semilogarithmic view") +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          panel.grid = element_blank())

  # 24cm x 12cm at 300dpi
  png(pngfile, width = 24, height = 12, units = "cm", res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 2,
    heights = unit(c(0.10, 0.08, 0.82), "npc"),
    widths = unit(c(0.5, 0.5), "npc"))))
  grid.text(title1,
    vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
    gp = gpar(fontsize = 12, fontface = "bold"))
  grid.text(title2,
    vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
    gp = gpar(fontsize = 10))
  print(p1, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(p2, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  popViewport()
  dev.off()
  cat("Wrote", pngfile, " n=", nrow(sub), "\n")
}

for (cb in combos) {
  trta <- cb$trta
  analy <- cb$analy
  suffix <- cb$suffix
  sub <- conc[conc$order == analy & !is.na(conc$order) & conc$TRTA == trta & !is.na(conc$TRTA), , drop = FALSE]
  # SAS sort within subset remains by usubjid t
  sub <- sub[base::order(sub$USUBJID, sub$t, na.last = FALSE), , drop = FALSE]
  cat("Figure", suffix, " Treatment=", trta, " Analyte=", analy, " n=", nrow(sub),
      " subjects=", length(unique(sub$USUBJID)), "\n", sep = "")
  # CSV of plotted data
  sub_csv <- fmt_for_csv(sub)
  write.csv(sub_csv, file.path(out, paste0(suffix, ".csv")), row.names = FALSE, na = "")
  # PNG
  pngfile <- file.path(out, paste0(suffix, ".png"))
  make_two_panel(sub, trta, analy, pngfile)
}

cat("Done. Files in OUT_DIR:", paste(list.files(out), collapse = ", "), "\n")
