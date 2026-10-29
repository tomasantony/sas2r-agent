# P034_target13.sas -> R translation
# Target13: Arithmetic mean concentration-time plot (ANAL1, MEANTYPE=A, SD=BOTH)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(grid)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_n <- function(x, digits = 2) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, digits), format = "f", digits = digits, flag = "#"))
}

cat("Locating adpc.xpt ...\n")
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("RAWADAM"), "adpc.xpt"),
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  "/bench/data/derived/cdisc-pilot/adam/adpc.xpt"
)
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) {
  # fallback search
  alt1 <- "/bench/data/raw/cdisc-pilot/adam/adpc.xpt"
  if (file.exists(alt1)) adpc_path <- alt1 else stop("adpc.xpt not found")
}
cat("Reading:", adpc_path, "\n")
adpc_all <- haven::read_xpt(adpc_path)
# normalize names to uppercase like SAS
names(adpc_all) <- toupper(names(adpc_all))

work_orig_adpc <- adpc_all

cat("Preparing ADPC for PCTESTCD=ANAL1, MEANTYPE=A, SD=BOTH ...\n")
PCTESTCD_sel <- "ANAL1"
MEANTYPE <- "A"
SDopt <- "BOTH"

# keep list (only if present)
keeplist <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
              "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keeplist <- keeplist[keeplist %in% names(adpc_all)]

adpc <- adpc_all %>%
  filter(PCTESTCD == PCTESTCD_sel) %>%
  select(any_of(keeplist))

# SAS: PCTPT=input(EPLTM,best10.) -- numeric leading part
parse_best <- function(v) {
  if (is.numeric(v)) return(as.numeric(v))
  v <- as.character(v)
  # SAS BEST. reads leading numeric; extract it
  m <- stringr::str_extract(v, "^\\s*[+-]?[0-9]*\\.?[0-9]+([Ee][+-]?[0-9]+)?")
  suppressWarnings(as.numeric(m))
}
adpc$PCTPT <- parse_best(adpc$EPLTM)

# ORDERBY=compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN)
# BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
propcase <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- as.character(v)
    if (nchar(v) == 0) return(v)
    paste0(toupper(substr(v,1,1)), tolower(substr(v,2,nchar(v))))
  }, USE.NAMES = FALSE)
}
compbl <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- gsub("\\s+", " ", v, perl = TRUE)
    trimws(v)
  }, USE.NAMES = FALSE)
}
# build safely even if some cols missing
getcol <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep("", nrow(df))
if (nrow(adpc) > 0) {
  adpc$ORDERBY <- gsub(" ", "", paste0(getcol(adpc,"ANLPRNT"), getcol(adpc,"PCSPEC"),
                                       getcol(adpc,"PCTESTL"), getcol(adpc,"TRTAN")),
                       fixed = TRUE)
  pc_u <- if ("PCORRESU" %in% names(adpc)) as.character(adpc$PCORRESU) else rep("", nrow(adpc))
  pc_u[is.na(pc_u)] <- ""
  adpc$BYVAL6 <- compbl(paste0(propcase(adpc$PCTESTCD), " Conc ", " (", pc_u, ")"))
} else {
  adpc$ORDERBY <- character(0); adpc$BYVAL6 <- character(0)
}

# proc sort by TRTAN TRTA PCTPT (missing first, char uppercase-first = C locale order)
# R arrange: ensure missing first
adpc <- adpc %>% arrange(TRTAN, TRTA, PCTPT)

# proc univariate by TRTAN TRTA PCTPT var PCORRESN -> mean, std (sample, n-1)
cat("Computing means ...\n")
if (nrow(adpc) > 0) {
  pcmeans <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(PCORRESN)),
      NALL = n(),
      PCMEAN = ifelse(sum(!is.na(PCORRESN)) > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      PCSD = suppressWarnings(sd(PCORRESN, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(TRTAN, TRTA, PCTPT)
} else {
  pcmeans <- data.frame(TRTAN=numeric(0), TRTA=character(0), PCTPT=numeric(0),
                        N=numeric(0), NALL=numeric(0), PCMEAN=numeric(0), PCSD=numeric(0))
}

# data MeanData; if PCMEAN=0 then PCMEAN=.; PCSD=.;
meandata <- pcmeans
idx0 <- !is.na(meandata$PCMEAN) & meandata$PCMEAN == 0
if (any(idx0)) {
  meandata$PCMEAN[idx0] <- NA_real_
  meandata$PCSD[idx0] <- NA_real_
}
meandata <- meandata %>% arrange(TRTAN, TRTA, PCTPT)

cat(paste0("MeanData rows: ", nrow(meandata), "\n"))

# Dynamic values from sgrender call
XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100
TICKS <- c(0,4,8,12,24,36,48,72,96)
LEGENDSIZE <- 11

# YMAX not passed -> derive like SAS auto; use max upper
upper_vals <- meandata$PCMEAN + meandata$PCSD
upper_vals <- upper_vals[is.finite(upper_vals) & !is.na(upper_vals)]
if (length(upper_vals) == 0) {
  upper_vals <- meandata$PCMEAN[is.finite(meandata$PCMEAN) & !is.na(meandata$PCMEAN)]
}
if (length(upper_vals) == 0) YMAX <- 100 else YMAX <- max(upper_vals, na.rm = TRUE) * 1.1
if (!is.finite(YMAX) || YMAX <= 0) YMAX <- 100

# ---- Write dataset CSV (plotted data) ----
out_csv <- file.path(out, "target13.csv")
# SAS dataset variable names: TRTAN TRTA PCTPT PCMEAN PCSD (plus N for traceability)
csv_df <- meandata %>% select(any_of(c("TRTAN","TRTA","PCTPT","PCMEAN","PCSD","N","NALL")))
write.csv(csv_df, out_csv, row.names = FALSE, na = "")

# ---- Figure: two panels Linear + Semilog ----
cat("Building figure ...\n")
# order treatments by TRTAN
trt_order <- meandata %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN)
meandata$TRTA_f <- factor(meandata$TRTA, levels = trt_order$TRTA)
meandata$TRTAN_f <- factor(meandata$TRTAN, levels = trt_order$TRTAN)

# error bounds
meandata$LOWER <- meandata$PCMEAN - meandata$PCSD
meandata$UPPER <- meandata$PCMEAN + meandata$PCSD

xbreaks <- TICKS

p1 <- ggplot(meandata, aes(x = PCTPT, y = PCMEAN, group = TRTAN_f,
                           linetype = TRTA_f, shape = TRTA_f)) +
  geom_errorbar(aes(ymin = LOWER, ymax = UPPER),
                color = "black", width = XMAX * 0.015, linewidth = 0.5, na.rm = TRUE) +
  geom_line(color = "black", linewidth = 0.7, na.rm = TRUE) +
  geom_point(color = "black", fill = "white", size = 2.5, stroke = 0.8, na.rm = TRUE) +
  scale_x_continuous(breaks = xbreaks, limits = c(0, XMAX), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, YMAX), expand = c(0.01, 0.02)) +
  labs(x = XLABEL, y = YLABEL, linetype = NULL, shape = NULL,
       title = "Linear view") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(size = 9, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9, color = "black"),
        legend.position = "none",
        panel.grid.minor = element_blank())

# semilog: filter positive means
meandata_log <- meandata %>% filter(!is.na(PCMEAN) & PCMEAN > 0)
p2 <- ggplot(meandata_log, aes(x = PCTPT, y = PCMEAN, group = TRTAN_f,
                               linetype = TRTA_f, shape = TRTA_f)) +
  geom_line(color = "black", linewidth = 0.7, na.rm = TRUE) +
  geom_point(color = "black", fill = "white", size = 2.5, stroke = 0.8, na.rm = TRUE) +
  scale_x_continuous(breaks = xbreaks, limits = c(0, XMAX), expand = c(0.01, 0.01)) +
  scale_y_log10() +
  labs(x = XLABEL, y = YLABEL, linetype = "Treatment", shape = "Treatment",
       title = "Semilogarithmic view") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(size = 9, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9, color = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = LEGENDSIZE),
        legend.title = element_text(size = LEGENDSIZE),
        panel.grid.minor = element_blank())

out_png <- file.path(out, "target13.png")
# 2500x1400 px like template design size
png(out_png, width = 2500, height = 1400, res = 150)
grid.newpage()
# layout: titles (row1), header sponsor (row2), fig titles (row3), plots (row4), footnote (row5)
lay <- grid.layout(nrow = 5, ncol = 2,
                   heights = unit(c(0.06, 0.05, 0.10, 0.73, 0.06), "npc"),
                   widths = unit(c(0.5, 0.5), "npc"))
pushViewport(viewport(layout = lay))
# Title sponsor line
grid.text("SPONSOR/PROTOCOL/PRODUCT INFO",
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1),
          x = 0.01, y = 0.5, just = c("left","center"),
          gp = gpar(fontsize = 11))
grid.text("page x of y",
          vp = viewport(layout.pos.row = 1, layout.pos.col = 2),
          x = 0.99, y = 0.5, just = c("right","center"),
          gp = gpar(fontsize = 11))
grid.text("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",
          vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
          y = 0.7, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("Analysis set: PK analysis set  |  PCTESTCD=ANAL1  Arithmetic mean (SD=BOTH)",
          vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
          y = 0.15, gp = gpar(fontsize = 10))
print(p1, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
grid.text("PATH DATA/PROGRAM/OUTPUT",
          vp = viewport(layout.pos.row = 5, layout.pos.col = 1),
          x = 0.01, y = 0.5, just = c("left","center"),
          gp = gpar(fontsize = 9))
grid.text(paste0("XMAX=", XMAX, "  TICKS=", paste(TICKS, collapse=" "),
                 "  YMAX=", formatC(round_half_up(YMAX,2), format="f", digits=2)),
          vp = viewport(layout.pos.row = 5, layout.pos.col = 2),
          x = 0.99, y = 0.5, just = c("right","center"),
          gp = gpar(fontsize = 8))
popViewport()
dev.off()

# ---- Text rendering (target13.txt) ----
out_txt <- file.path(out, "target13.txt")
con <- file(out_txt, open = "wt")
writeLines("SPONSOR/PROTOCOL/PRODUCT INFO                                              page x of y", con)
writeLines("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", con)
writeLines("Analysis set: PK analysis set", con)
writeLines("", con)
writeLines("PCTESTCD=ANAL1  MEANTYPE=Arithmetic (A)  SD=BOTH", con)
writeLines(paste0("XLABEL=", XLABEL, "  YLABEL=", YLABEL), con)
writeLines(paste0("XMAX=", XMAX, "  YMAX=", formatC(round_half_up(YMAX,2), format="f", digits=2),
                  "  TICKS=", paste(TICKS, collapse=" ")), con)
writeLines("", con)
writeLines("Arithmetic mean (SD) by Treatment and Time (h):", con)
writeLines("--------------------------------------------------------------------------------", con)
writeLines(sprintf("%-6s %-30s %10s %15s %15s %6s", "TRTAN", "TRTA", "PCTPT", "PCMEAN", "PCSD", "N"), con)
writeLines("--------------------------------------------------------------------------------", con)
if (nrow(meandata) > 0) {
  for (i in seq_len(nrow(meandata))) {
    r <- meandata[i, ]
    line <- sprintf("%-6s %-30s %10s %15s %15s %6s",
      ifelse(is.na(r$TRTAN), "", as.character(r$TRTAN)),
      ifelse(is.na(r$TRTA), "", substr(as.character(r$TRTA), 1, 30)),
      ifelse(is.na(r$PCTPT), "", formatC(r$PCTPT, format = "g", digits = 10)),
      ifelse(is.na(r$PCMEAN), ".", formatC(round_half_up(r$PCMEAN, 2), format = "f", digits = 2)),
      ifelse(is.na(r$PCSD), ".", formatC(round_half_up(r$PCSD, 2), format = "f", digits = 2)),
      ifelse(is.na(r$N), "", as.character(r$N)))
    writeLines(line, con)
  }
} else {
  writeLines("No observations for PCTESTCD=ANAL1.", con)
}
writeLines("--------------------------------------------------------------------------------", con)
writeLines("", con)
writeLines("Note: Error bars in linear view are PCMEAN +/- PCSD (BOTH).", con)
writeLines("      Semilogarithmic view shows PCMEAN only (log base 10 Y-axis).", con)
writeLines("      PCMEAN=0 set to missing per program.", con)
writeLines("PATH DATA/PROGRAM/OUTPUT", con)
close(con)

cat("Done. Outputs in:", out, "\n")
