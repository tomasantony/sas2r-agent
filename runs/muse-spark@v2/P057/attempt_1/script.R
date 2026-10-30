# P057_pk_mean_conc.sas -> R translation (Target13)
# Figure: Arithmetic Mean concentration-time plot (linear + semilog) for PCTESTCD=ANAL1, SD=BOTH
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

adpc_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"
cat("Reading:", adpc_path, "\n")
adpc_raw <- haven::read_xpt(adpc_path)
# normalise names to upper for case-insensitive matching (SAS is case-insensitive)
names(adpc_raw) <- toupper(names(adpc_raw))
cat("ADPC rows:", nrow(adpc_raw), " cols:", paste(names(adpc_raw), collapse=","), "\n")

# SAS helper: INPUT(EPLTM, best10.) - parse leading SAS numeric (best format)
sas_best <- function(x) {
  # x may be numeric or character/haven_labelled
  if (inherits(x, "haven_labelled")) x <- haven::as_factor(x, levels = "labels")
  x <- as.character(x)
  outv <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi)) { outv[i] <- NA_real_; next }
    xi <- trimws(xi)
    if (xi == "" || xi == ".") { outv[i] <- NA_real_; next }
    # SAS best.: optional sign, digits with optional decimal/exponent at start
    m <- regexpr("^[+-]?((\\d+\\.?\\d*)|(\\.\\d+))([eE][+-]?\\d+)?", xi, perl = TRUE)
    mm <- regmatches(xi, m)
    if (length(mm) == 0 || mm == "") {
      outv[i] <- NA_real_
    } else {
      outv[i] <- suppressWarnings(as.numeric(mm))
    }
  }
  outv
}

# SAS propcase: first letter of each word upper, rest lower
sas_propcase <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    # SAS PROPCASE delimits on blanks and most special chars; approximate with word boundaries
    # Use lower then upper-case first letter of each alphanumeric word
    low <- tolower(s)
    gsub("\\b([a-z])", "\\U\\1", low, perl = TRUE)
  }, USE.NAMES = FALSE)
}
sas_compbl <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    gsub("\\s+", " ", trimws(s))
  }, USE.NAMES = FALSE)
}

PCTESTCD_sel <- "ANAL1"
MEANTYPE <- "A"
SDOPT <- "BOTH"

# data work.adpc ; keep + where PCTESTCD="ANAL1"
keep <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
          "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keep <- intersect(keep, names(adpc_raw))
adpc <- adpc_raw[, keep, drop = FALSE]
stopifnot("PCTESTCD" %in% names(adpc))
stopifnot("PCORRESN" %in% names(adpc))
stopifnot("EPLTM" %in% names(adpc))

adpc <- adpc %>% filter(PCTESTCD == PCTESTCD_sel)
cat("After PCTESTCD=ANAL1 filter:", nrow(adpc), "rows\n")
stopifnot(nrow(adpc) > 0)

# PCTPT=input(EPLTM,best10.); ORDERBY; BYVAL6
adpc$PCTPT <- sas_best(adpc$EPLTM)
# ORDERBY=compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN)
# SAS compress removes blanks; || concatenates (missing -> blank handling approximated)
paste0_sas <- function(...) {
  args <- list(...)
  args <- lapply(args, function(a) { a <- as.character(a); a[is.na(a)] <- ""; a })
  do.call(paste0, args)
}
if (all(c("ANLPRNT","PCSPEC","PCTESTL","TRTAN") %in% names(adpc))) {
  adpc$ORDERBY <- gsub(" ", "", paste0_sas(adpc$ANLPRNT, adpc$PCSPEC, adpc$PCTESTL, adpc$TRTAN), fixed = TRUE)
}
if (all(c("PCTESTCD","PCORRESU") %in% names(adpc))) {
  by6 <- paste0(sas_propcase(adpc$PCTESTCD), " Conc ", " (", as.character(adpc$PCORRESU), ")")
  adpc$BYVAL6 <- sas_compbl(by6)
}

# proc sort by TRTAN TRTA PCTPT (SAS: missing first, uppercase before lowercase)
# Use order with na.last=FALSE to mimic SAS missing-first
ord <- order(ifelse(is.na(adpc$TRTAN), -Inf, adpc$TRTAN),
             xtfrm(ifelse(is.na(adpc$TRTA), "", adpc$TRTA)),
             ifelse(is.na(adpc$PCTPT), -Inf, adpc$PCTPT),
             na.last = FALSE)
adpc <- adpc[ord, , drop = FALSE]

# proc univariate by TRTAN TRTA PCTPT var PCORRESN output mean STD
# SAS uses sample SD (N-1); R sd() matches. Missing ignored.
pcmeans <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    NALL = n(),
    PCMEAN = ifelse(sum(!is.na(PCORRESN)) > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
    PCSD = ifelse(sum(!is.na(PCORRESN)) > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  as.data.frame()
# mean(..., na.rm=TRUE) on all-NA gives NaN -> convert to NA (SAS missing)
pcmeans$PCMEAN[is.nan(pcmeans$PCMEAN)] <- NA_real_

# data MeanData; if PCMEAN=0 then PCMEAN=.; PCSD=.;
meandata <- pcmeans
idx0 <- !is.na(meandata$PCMEAN) & meandata$PCMEAN == 0
meandata$PCMEAN[idx0] <- NA_real_
meandata$PCSD[idx0] <- NA_real_

# sort MeanData by TRTAN TRTA PCTPT
meandata <- meandata[order(ifelse(is.na(meandata$TRTAN), -Inf, meandata$TRTAN),
                           xtfrm(ifelse(is.na(meandata$TRTA), "", meandata$TRTA)),
                           ifelse(is.na(meandata$PCTPT), -Inf, meandata$PCTPT)), , drop = FALSE]

cat("MeanData rows:", nrow(meandata), "\n")
print(head(meandata, 20))

# Write plotted-data CSV (counterpart of target13.rtf data)
write.csv(meandata, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# ---- Figure: two panels (Linear view + Semilogarithmic view), SD=BOTH ----
# Titles/footnotes verbatim from SAS
t1_left  <- "SPONSOR/PROTOCOL/PRODUCT INFO"
t1_right <- "page x of y"
t3 <- "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
t4 <- "Analysis set: PK analysis set"
ft <- "PATH DATA/PROGRAM/OUTPUT"

# Treatment ordering by TRTAN
trt_ord <- unique(meandata[, c("TRTAN","TRTA"), drop = FALSE])
trt_ord <- trt_ord[order(trt_ord$TRTAN, xtfrm(trt_ord$TRTA)), , drop = FALSE]
trt_levels <- trt_ord$TRTA
# Ensure factor with TRTAN order (handle duplicate TRTA labels by making unique label)
meandata$TRT_LAB <- factor(meandata$TRTA, levels = trt_levels)
# For linetype/shape mapping use TRT_LAB
xbreaks <- c(0,4,8,12,24,36,48,72,96)
xmax <- 100
ylabel <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
xlabel <- "Time (h)"

# y range for linear: 0 to max(PCMEAN+PCSD) with margin
ymax_lin <- suppressWarnings(max(meandata$PCMEAN + meandata$PCSD, na.rm = TRUE))
if (!is.finite(ymax_lin)) ymax_lin <- suppressWarnings(max(meandata$PCMEAN, na.rm = TRUE))
ymax_lin <- ymax_lin * 1.1

plotdat_lin <- meandata[!is.na(meandata$PCMEAN), , drop = FALSE]

p1 <- ggplot(plotdat_lin, aes(x = PCTPT, y = PCMEAN, group = TRT_LAB, linetype = TRT_LAB, shape = TRT_LAB)) +
  geom_errorbar(aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD), width = 2.5, colour = "black", na.rm = TRUE) +
  geom_line(colour = "black", na.rm = TRUE) +
  geom_point(colour = "black", size = 3, fill = "black", na.rm = TRUE) +
  scale_x_continuous(name = xlabel, breaks = xbreaks) +
  scale_y_continuous(name = ylabel) +
  coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax_lin)) +
  ggtitle("Linear view") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "none")

# Semilog view: no error bars, log10 y
plotdat_log <- meandata[!is.na(meandata$PCMEAN) & meandata$PCMEAN > 0, , drop = FALSE]
p2 <- ggplot(plotdat_log, aes(x = PCTPT, y = PCMEAN, group = TRT_LAB, linetype = TRT_LAB, shape = TRT_LAB)) +
  geom_line(colour = "black", na.rm = TRUE) +
  geom_point(colour = "black", size = 3, fill = "black", na.rm = TRUE) +
  scale_x_continuous(name = xlabel, breaks = xbreaks) +
  scale_y_log10(name = ylabel, breaks = scales::log_breaks(base = 10)) +
  coord_cartesian(xlim = c(0, xmax)) +
  ggtitle("Semilogarithmic view") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "none")

# Legend grob at bottom (merged legend s1+s2): line+point per treatment, black
leg_dat <- trt_ord
leg_dat$TRT_LAB <- factor(leg_dat$TRTA, levels = trt_levels)
p_leg <- ggplot(leg_dat, aes(x = 1, y = 1, group = TRT_LAB, linetype = TRT_LAB, shape = TRT_LAB)) +
  geom_line(colour = "black") +
  geom_point(colour = "black", size = 3) +
  facet_wrap(~ TRT_LAB, nrow = 1) +
  theme_void() +
  theme(legend.position = "none", strip.text = element_text(size = 11))
# Build custom legend text instead: use grid legend manually
legend_labels <- paste0(trt_ord$TRTA, " (TRTAN=", trt_ord$TRTAN, ")")

png(file.path(out, "target13.png"), width = 2500, height = 1400)
grid.newpage()
# layout: titles(3 rows) + plots(1 row with 2 cols) + legend + footnote
lay <- grid.layout(nrow = 6, ncol = 2,
                   heights = unit(c(0.05, 0.045, 0.045, 0.72, 0.09, 0.05), "npc"),
                   widths = unit(c(0.5, 0.5), "npc"))
pushViewport(viewport(layout = lay))
# Title row 1: left + right
print(grid.text(t1_left, x = unit(0.01, "npc"), y = unit(0.5, "npc"),
                just = c("left","center"), gp = gpar(fontsize = 14)),
      vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(grid.text(t1_right, x = unit(0.99, "npc"), y = unit(0.5, "npc"),
                just = c("right","center"), gp = gpar(fontsize = 14)),
      vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(grid.text(t3, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                just = "center", gp = gpar(fontsize = 13)),
      vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
print(grid.text(t4, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                just = "center", gp = gpar(fontsize = 13)),
      vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
# Plots
print(p1, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
# Bottom legend (sidebar align=bottom, merged legend s1 s2)
# Draw legend manually centered
leg_vp <- viewport(layout.pos.row = 5, layout.pos.col = 1:2)
pushViewport(leg_vp)
nleg <- nrow(trt_ord)
# line types and shapes cycled as ggplot default
lty_vals <- c("solid","dashed","dotted","dotdash","longdash","twodash")[seq_len(nleg)]
pch_vals <- c(16,17,15,18,8,3)[seq_len(nleg)]
# compute positions
for (i in seq_len(nleg)) {
  xc <- (i - 0.5) / nleg
  # line segment
  grid.lines(x = unit(c(xc - 0.06, xc - 0.01), "npc"), y = unit(0.6, "npc"),
             gp = gpar(lty = lty_vals[i], lwd = 1.5, col = "black"))
  grid.points(x = unit(xc - 0.035, "npc"), y = unit(0.6, "npc"), pch = pch_vals[i],
              gp = gpar(col = "black", cex = 1.4))
  grid.text(legend_labels[i], x = unit(xc + 0.045, "npc"), y = unit(0.6, "npc"),
            just = c("left","center"), gp = gpar(fontsize = 13))
}
popViewport()
# Footnote
print(grid.text(ft, x = unit(0.01, "npc"), y = unit(0.5, "npc"),
                just = c("left","center"), gp = gpar(fontsize = 12)),
      vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))
popViewport()
dev.off()

cat("Wrote", file.path(out, "target13.png"), "and", file.path(out, "target13.csv"), "\n")
