```r
# P058 - Overlaid PK Concentration Plots (translation of P058_pk_overlay_conc.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ---- SAS helpers ---------------------------------------------------------
# COMPRESS(x, chars): remove every character listed in chars
sas_compress <- function(x, chars) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  set <- unique(strsplit(chars, "")[[1]])
  for (ch in set) x <- gsub(ch, "", x, fixed = TRUE)
  x
}

# INPUT(x, best.): numeric or missing
sas_input_best <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

fmt_dtm <- function(x) {
  if (inherits(x, "POSIXt")) {
    r <- format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    r[is.na(x)] <- NA
    r
  } else x
}

# ---- Data step: conc -----------------------------------------------------
cat("Reading adpc.xpt\n")
adpc <- read_xpt(file.path(rawadam, "adpc.xpt"))
names(adpc) <- tolower(names(adpc))

keepvars <- c("usubjid", "trta", "aperiodc", "atpt", "parcat3l", "aval", "adtm",
              "saffn", "pkfn", "paexcfln", "pclloq", "parcat1")
keepvars <- intersect(keepvars, names(adpc))

conc <- adpc[, keepvars, drop = FALSE]
conc <- as.data.frame(lapply(conc, function(v) { attr(v, "label") <- NULL; attr(v, "format.sas") <- NULL; v }),
                      stringsAsFactors = FALSE)

conc$t      <- sas_input_best(sas_compress(conc$atpt, " HOURSPOSTDOSE"))
conc$concen <- conc$aval
conc$order  <- sas_input_best(sas_compress(conc$parcat1, " DRUGANALPlasma"))
if ("pclloq" %in% names(conc)) {
  idx <- !is.na(conc$pclloq)
  conc$concen[idx] <- conc$pclloq[idx]
}

# ---- PROC SORT by usubjid t (missing first, stable) ----------------------
conc <- conc[order(conc$usubjid, !is.na(conc$t), conc$t, method = "radix"), ]
rownames(conc) <- NULL

conc_out <- conc
if ("adtm" %in% names(conc_out)) conc_out$adtm <- fmt_dtm(conc_out$adtm)
write.csv(conc_out, file.path(out, "conc.csv"), row.names = FALSE, na = "")

# ---- Plotting ------------------------------------------------------------
make_panel <- function(d, log = FALSE, title) {
  p <- ggplot(d, aes(x = t, y = concen, group = usubjid)) +
    geom_line(colour = "black", linetype = 1, na.rm = TRUE) +
    labs(title = title, x = "Time", y = if (log) " " else "Concentration (ug/mL)") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  if (log) p <- p + scale_y_log10()
  p
}

plot_names <- c("sgrender", paste0("sgrender", 1:5))
k <- 0
for (trt in c("A", "C")) {
  for (i in 1:3) {
    d <- conc %>% filter(!is.na(order) & order == i & !is.na(trta) & trta == trt)
    if (nrow(d) == 0) {
      cat(sprintf("No observations for order=%d trta=%s - no graph produced\n", i, trt))
      next
    }
    k <- k + 1
    nm <- plot_names[k]
    cat(sprintf("Writing %s (Analyte %d, Treatment=%s)\n", nm, i, trt))

    pd <- d[, intersect(c("usubjid", "trta", "order", "parcat1", "atpt", "t", "aval", "concen"), names(d))]
    write.csv(pd, file.path(out, paste0(nm, ".csv")), row.names = FALSE, na = "")

    p1 <- make_panel(d, FALSE, "Linear view")
    dlog <- d %>% filter(!is.na(concen) & concen > 0)
    p2 <- make_panel(dlog, TRUE, "Semilogarithmic view")

    png(file.path(out, paste0(nm, ".png")), width = 24, height = 12, units = "cm", res = 300)
    grid.newpage()
    lay <- grid.layout(nrow = 3, ncol = 2,
                       heights = unit(c(0.6, 0.6, 1), c("cm", "cm", "null")))
    pushViewport(viewport(layout = lay))
    grid.text("Overlaid PK Concentration Plots",
              vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
              gp = gpar(fontsize = 12, fontface = "bold"))
    grid.text(sprintf("Analyte: %d; Treatment=%s", i, trt),
              vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
              gp = gpar(fontsize = 11, fontface = "bold"))
    print(p1, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
    print(p2, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
    popViewport()
    dev.off()
  }
}
cat("Done\n")
```