```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## round half away from zero (SAS) helper
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(grid)
})

cat("P034 target13: locating adpc.xpt\n")
candidates <- c(
  file.path(Sys.getenv("RAWADAM"), "adpc.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(getwd(), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  file.path(getwd(), "data/derived/cdisc-pilot/adam/adpc.xpt"),
  "/data/raw/cdisc-pilot/adam/adpc.xpt"
)
candidates <- candidates[candidates != "" & candidates != "adpc.xpt"]
adpc_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) {
  ## last resort: recursive search
  roots <- c("D:/meta_hackathon/sas2r-agent", getwd(), "/")
  for (r in roots) {
    if (!dir.exists(r)) next
    f <- try(list.files(r, pattern = "^adpc\\.xpt$", recursive = TRUE, full.names = TRUE), silent = TRUE)
    if (!inherits(f, "try-error") && length(f) > 0) { adpc_path <- f[1]; break }
  }
}
if (is.null(adpc_path)) stop("adpc.xpt not found")
cat(paste0("Reading: ", adpc_path, "\n"))

adpc_all <- haven::read_xpt(adpc_path)
## normalize names to upper for matching, keep original case for output
names(adpc_all) <- toupper(names(adpc_all))

stopifnot(all(c("PCTESTCD","PCORRESN","EPLTM") %in% names(adpc_all)))
stopifnot(all(c("TRTAN","TRTA","PCTPT") %in% c(names(adpc_all), "PCTPT")))

## macro call: %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH)
PCTESTCD_param <- "ANAL1"
MEANTYPE <- "A"
SDOPT <- "BOTH"

## data work.adpc ; keep ANLPRNT TRTAN PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
## where PCTESTCD="ANAL1"; PCTPT=input(EPLTM,best10.);
keepvars <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
              "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keepvars <- keepvars[keepvars %in% names(adpc_all)]

adpc <- adpc_all %>%
  filter(PCTESTCD == PCTESTCD_param) %>%
  select(all_of(keepvars))

## PCTPT = input(EPLTM, best10.) : single informat for whole column
if ("EPLTM" %in% names(adpc)) {
  epl <- adpc$EPLTM
  if (is.numeric(epl)) {
    adpc$PCTPT <- as.numeric(epl)
  } else {
    ## SAS BEST10. reads leading numeric, blank -> missing
    adpc$PCTPT <- suppressWarnings(as.numeric(as.character(epl)))
  }
} else {
  adpc$PCTPT <- NA_real_
}

## ORDERBY=compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN); BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')');
propcase_sas <- function(x) {
  ## SAS PROPCASE: first letter of each word upper, rest lower
  sapply(as.character(x), function(v) {
    if (is.na(v)) return(NA_character_)
    w <- strsplit(tolower(v), " ", fixed = TRUE)[[1]]
    w <- paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    paste(w, collapse = " ")
  }, USE.NAMES = FALSE)
}
if (all(c("ANLPRNT","PCSPEC","PCTESTL","TRTAN") %in% names(adpc))) {
  adpc$ORDERBY <- gsub(" ", "", paste0(as.character(adpc$ANLPRNT),
    as.character(adpc$PCSPEC), as.character(adpc$PCTESTL), as.character(adpc$TRTAN)), fixed = TRUE)
  ## compress removes all blanks
  adpc$ORDERBY <- gsub("[[:space:]]+", "", adpc$ORDERBY)
}
if (all(c("PCTESTCD","PCORRESU") %in% names(adpc))) {
  bv <- paste0(propcase_sas(adpc$PCTESTCD), " Conc", " (", as.character(adpc$PCORRESU), ")")
  ## COMPBL: multiple blanks to single, trim leading/trailing? SAS COMPBL removes? keep single
  bv <- gsub("[[:space:]]+", " ", bv)
  bv <- trimws(bv)
  adpc$BYVAL6 <- bv
}

## proc sort data=ADPC; by TRTAN TRTA PCTPT;  (SAS: missing first, uppercase before lowercase)
## emulate SAS sort: numeric missing first, character case-sensitive (byte order)
adpc <- adpc %>%
  mutate(
    .sTRTAN = ifelse(is.na(TRTAN), -Inf, TRTAN),
    .sPCTPT = ifelse(is.na(PCTPT), -Inf, PCTPT),
    .sTRTA = ifelse(is.na(TRTA), "", as.character(TRTA))
  ) %>%
  arrange(.sTRTAN, .sTRTA, .sPCTPT) %>%
  select(-.sTRTAN, -.sPCTPT, -.sTRTA)

## proc univariate noprint; by TRTAN TRTA PCTPT; var PCORRESN; output mean STD;
## SAS mean/std ignore missing; STD sample (N-1); N=1 -> missing; all missing -> missing
meandata_raw <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT, .drop = FALSE) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    PCMEAN = ifelse(sum(!is.na(PCORRESN)) == 0, NA_real_, mean(PCORRESN, na.rm = TRUE)),
    PCSD = ifelse(sum(!is.na(PCORRESN)) <= 1, NA_real_, sd(PCORRESN, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(TRTAN, TRTA, PCTPT)

## data MeanData; if PCMEAN=0 then do; PCMEAN=.; PCSD=.; end;
MeanData <- meandata_raw %>%
  mutate(
    PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
    PCSD = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCSD)
  )
## Note: SAS checks PCMEAN=0 before missing assignment; above uses original? replicate exactly:
MeanData <- meandata_raw
MeanData$PCMEAN[ !is.na(MeanData$PCMEAN) & MeanData$PCMEAN == 0 ] <- NA_real_
MeanData$PCSD[ is.na(MeanData$PCMEAN) & meandata_raw$PCMEAN == 0 ] <- NA_real_

## order for plot: TRTAN then PCTPT (SAS BY order)
MeanData <- MeanData %>% arrange(TRTAN, TRTA, PCTPT)

cat(paste0("MeanData rows: ", nrow(MeanData), "\n"))

## write datasets as CSV (SAS variable names, ISO dates, unrounded)
write.csv(MeanData, file.path(out, "MeanData.csv"), row.names = FALSE, na = "")
write.csv(MeanData, file.path(out, "target13.csv"), row.names = FALSE, na = "")

## ---- figure parameters from SGRENDER ----
XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100
TICKS <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
LEGENDSIZE <- 11

## _YMAX not passed in DYNAMIC -> SAS auto; emulate as max upper bar with margin
upper <- MeanData$PCMEAN + MeanData$PCSD
upper <- upper[!is.na(upper)]
m1 <- suppressWarnings(max(c(MeanData$PCMEAN, upper), na.rm = TRUE))
if (!is.finite(m1) || is.na(m1)) m1 <- 1
YMAX <- ceiling(m1 * 1.1)
if (YMAX <= 0 || !is.finite(YMAX)) YMAX <- 1

plotdat <- MeanData %>% filter(!is.na(PCMEAN), !is.na(PCTPT))
## error bar limits clipped at 0 to respect viewmin=0
plotdat <- plotdat %>%
  mutate(
    YLOW = PCMEAN - PCSD,
    YHIGH = PCMEAN + PCSD,
    YLOWC = pmax(YLOW, 0)
  )

## ensure TRTA factor ordered by TRTAN (SAS table order)
trt_ord <- MeanData %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN) %>% pull(TRTA)
plotdat$TRTA <- factor(plotdat$TRTA, levels = trt_ord)
MeanData$TRTA_f <- factor(MeanData$TRTA, levels = trt_ord)

n_trt <- length(trt_ord)
ltys <- c("solid","dashed","dotted","dotdash","longdash","twodash")[seq_len(max(1,n_trt))]
shps <- c(16,17,15,18,8,3)[seq_len(max(1,n_trt))]

base_th <- theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )

p1 <- ggplot(plotdat, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_errorbar(aes(ymin = YLOWC, ymax = YHIGH), width = 2.2, colour = "black", linewidth = 0.5, na.rm = TRUE) +
  geom_line(colour = "black", linewidth = 0.6) +
  geom_point(colour = "black", size = 2.2, fill = "black") +
  scale_x_continuous(name = XLABEL, limits = c(0, XMAX), breaks = TICKS) +
  scale_y_continuous(name = YLABEL, limits = c(0, YMAX)) +
  scale_linetype_manual(values = setNames(ltys[seq_along(trt_ord)], trt_ord)) +
  scale_shape_manual(values = setNames(shps[seq_along(trt_ord)], trt_ord)) +
  ggtitle("Linear view") +
  base_th

## semilog view: no error bars, Y log base 10, viewmax=YMAX
ymin_log <- suppressWarnings(min(plotdat$PCMEAN[plotdat$PCMEAN > 0], na.rm = TRUE))
if (!is.finite(ymin_log) || is.na(ymin_log)) ymin_log <- 0.1
## choose lower limit as decade below min
ymin_log_lim <- 10^(floor(log10(ymin_log)))
if (!is.finite(ymin_log_lim) || ymin_log_lim <= 0) ymin_log_lim <- 0.1
if (ymin_log_lim >= YMAX) ymin_log_lim <- YMAX/100

p2 <- ggplot(plotdat, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_line(colour = "black", linewidth = 0.6) +
  geom_point(colour = "black", size = 2.2, fill = "black") +
  scale_x_continuous(name = XLABEL, limits = c(0, XMAX), breaks = TICKS) +
  scale_y_log10(name = YLABEL, limits = c(ymin_log_lim, YMAX)) +
  annotation_logticks(sides = "l", colour = "black") +
  scale_linetype_manual(values = setNames(ltys[seq_along(trt_ord)], trt_ord)) +
  scale_shape_manual(values = setNames(shps[seq_along(trt_ord)], trt_ord)) +
  ggtitle("Semilogarithmic view") +
  base_th

## combined figure with titles/footnotes verbatim
title1_left <- "SPONSOR/PROTOCOL/PRODUCT INFO"
title1_right <- "page x of y"
title3 <- "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
title4 <- "Analysis set: PK analysis set"
foot <- "PATH DATA/PROGRAM/OUTPUT"

## legend grob (bottom sidebar merged legend s1+s2)
leg_df <- data.frame(TRTA = factor(trt_ord, levels = trt_ord), x = 1, y = 1)
p_leg_source <- ggplot(leg_df, aes(x = x, y = y, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_line(colour = "black") + geom_point(colour = "black", size = 2.5) +
  scale_linetype_manual(values = setNames(ltys[seq_along(trt_ord)], trt_ord)) +
  scale_shape_manual(values = setNames(shps[seq_along(trt_ord)], trt_ord)) +
  theme_void() + theme(legend.position = "bottom", legend.title = element_blank(),
    legend.text = element_text(size = LEGENDSIZE))
leg_grob <- {
  ## extract legend via ggplotGrob
  g <- ggplotGrob(p_leg_source + guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)))
  idx <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  if (length(idx) > 0) g$grobs[[idx]] else nullGrob()
}

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

png(file.path(out, "target13.png"), width = 2500, height = 1400, res = 150)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 2, heights = unit(c(0.4, 0.5, 0.5, 7.5, 1.2), "null"))))
## title row 1: left/right
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
grid.text(title1_left, x = 0.01, y = 0.5, just = c("left","center"), gp = gpar(fontsize = 11))
grid.text(title1_right, x = 0.99, y = 0.5, just = c("right","center"), gp = gpar(fontsize = 11))
popViewport()
## title3
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
grid.text(title3, x = 0.5, y = 0.5, just = "center", gp = gpar(fontsize = 12))
popViewport()
## title4
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
grid.text(title4, x = 0.5, y = 0.5, just = "center", gp = gpar(fontsize = 12))
popViewport()
## panels
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
grid.draw(g1)
popViewport()
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2))
grid.draw(g2)
popViewport()
## sidebar legend + footnote
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1:2))
grid.draw(leg_grob)
grid.text(foot, x = 0.01, y = 0.05, just = c("left","bottom"), gp = gpar(fontsize = 9))
popViewport()
popViewport()
dev.off()

cat("Wrote target13.png and target13.csv\n")
```