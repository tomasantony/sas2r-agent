# P057_pk_mean_conc.sas -> R translation
# Target13: Arithmetic mean concentration-time plot (ANAL1, MEANTYPE=A, SD=BOTH)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_first_existing <- function(paths) {
  for (p in paths) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(p)
  }
  return(NA_character_)
}

rawadam_env <- Sys.getenv("RAWADAM")
if (rawadam_env == "") rawadam_env <- Sys.getenv("rawadam")
root_env <- Sys.getenv("ROOT")
if (root_env == "") root_env <- "/bench"

candidates_adpc <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  file.path(root_env, "data/raw/cdisc-pilot/adam/adpc.xpt"),
  if (nzchar(rawadam_env)) file.path(rawadam_env, "adpc.xpt") else NA_character_,
  "data/raw/cdisc-pilot/adam/adpc.xpt",
  "../data/raw/cdisc-pilot/adam/adpc.xpt"
)
adpc_path <- find_first_existing(candidates_adpc)

# also try derived inlib location if xpt not found (sas7bdat)
if (is.na(adpc_path)) {
  candidates2 <- c(
    "/bench/data/derived/cdisc-pilot/adam/adpc.sas7bdat",
    file.path(root_env, "data/derived/cdisc-pilot/adam/adpc.sas7bdat")
  )
  adpc_path <- find_first_existing(candidates2)
}

if (is.na(adpc_path)) {
  stop("adpc input not found. Tried: ", paste(candidates_adpc, collapse="; "))
}
cat("Reading ADPC from:", adpc_path, "\n")

if (grepl("\\.xpt$", adpc_path, ignore.case = TRUE)) {
  adpc_raw <- haven::read_xpt(adpc_path)
} else {
  adpc_raw <- haven::read_sas(adpc_path)
}
# remove haven labels for easier handling but keep names
adpc_raw <- as.data.frame(adpc_raw, stringsAsFactors = FALSE)
cat("ADPC rows:", nrow(adpc_raw), " cols:", paste(names(adpc_raw), collapse=","), "\n")

PCTESTCD_val <- "ANAL1"
MEANTYPE <- "A"
SDMODE <- "BOTH"  # BOTH / UPPER / N

# ---- data work.adpc ----
# keep= ANLPRNT TRTAN PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
# where PCTESTCD="ANAL1"; PCTPT=input(EPLTM,best10.);
need_cols <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
               "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
have_cols <- intersect(need_cols, names(adpc_raw))
missing_cols <- setdiff(need_cols, names(adpc_raw))
if (length(missing_cols) > 0) cat("Warning: missing cols in ADPC:", paste(missing_cols, collapse=","), "\n")
stopifnot("PCTESTCD" %in% names(adpc_raw))
stopifnot("PCORRESN" %in% names(adpc_raw))
stopifnot("EPLTM" %in% names(adpc_raw))

adpc <- adpc_raw
# case-sensitive filter like SAS WHERE
adpc <- adpc[adpc$PCTESTCD == PCTESTCD_val, , drop = FALSE]
cat("After PCTESTCD=ANAL1 filter rows:", nrow(adpc), "\n")
stopifnot(nrow(adpc) > 0)

# PCTPT = input(EPLTM, best10.)  -- single informat for whole column
# EPLTM may be numeric or character; emulate SAS INPUT by coercing via character
epltm_chr <- as.character(adpc$EPLTM)
# SAS BEST strips blanks; as.numeric does same; non-numeric -> NA (missing)
PCTPT <- suppressWarnings(as.numeric(trimws(epltm_chr)))
# if EPLTM was already numeric and character conversion lost precision, prefer direct numeric
# (as.numeric of character is equivalent for BEST)
adpc$PCTPT <- PCTPT

# ORDERBY = compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN) -- not used for output, create for fidelity
compress_sas <- function(x) gsub("[[:space:]]+", "", x)
get_chr <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else ""
ANLPRNT_c <- get_chr(adpc, "ANLPRNT")
PCSPEC_c  <- get_chr(adpc, "PCSPEC")
PCTESTL_c <- get_chr(adpc, "PCTESTL")
TRTAN_c   <- if ("TRTAN" %in% names(adpc)) as.character(adpc$TRTAN) else ""
adpc$ORDERBY <- compress_sas(paste0(ifelse(is.na(ANLPRNT_c),"",ANLPRNT_c),
                                    ifelse(is.na(PCSPEC_c),"",PCSPEC_c),
                                    ifelse(is.na(PCTESTL_c),"",PCTESTL_c),
                                    ifelse(is.na(TRTAN_c),"",TRTAN_c)))
# BYVAL6 = compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
propcase_sas <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    w <- strsplit(tolower(v), " ", fixed=TRUE)[[1]]
    w <- paste0(toupper(substr(w,1,1)), substr(w,2,nchar(w)))
    paste(w, collapse=" ")
  }, USE.NAMES=FALSE)
}
compbl_sas <- function(x) gsub("[[:space:]]+", " ", trimws(x))
PCTESTCD_c <- as.character(adpc$PCTESTCD)
PCORRESU_c <- get_chr(adpc, "PCORRESU")
adpc$BYVAL6 <- compbl_sas(paste0(ifelse(is.na(propcase_sas(PCTESTCD_c)),"",propcase_sas(PCTESTCD_c)),
                                 " Conc ", " (", ifelse(is.na(PCORRESU_c),"",PCORRESU_c), ")"))

# proc sort by TRTAN TRTA PCTPT (SAS: missing first, uppercase before lowercase)
# Use C collation ordering to mimic SAS binary order
old_collate <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE","C"), silent=TRUE))
# order with na.last=FALSE? SAS missing first -> na first
# For character TRTA, use xtfrm with C locale via order()
ord <- order(adpc$TRTAN, adpc$TRTA, adpc$PCTPT, na.last = FALSE, method = "radix")
adpc <- adpc[ord, , drop = FALSE]
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_collate), silent=TRUE))
rownames(adpc) <- NULL

# ---- proc univariate by TRTAN TRTA PCTPT var PCORRESN output mean STD ----
# SAS MEAN = sum/n ignoring missing; STD = sample std (n-1); missing if n<=1 or all missing
df_u <- adpc[, c("TRTAN","TRTA","PCTPT","PCORRESN"), drop=FALSE]
# ensure numeric
df_u$PCORRESN <- suppressWarnings(as.numeric(as.character(df_u$PCORRESN)))

PCMEANS <- df_u %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    PCMEAN = ifelse(sum(!is.na(PCORRESN))==0, NA_real_, mean(PCORRESN, na.rm=TRUE)),
    PCSD = suppressWarnings(sd(PCORRESN, na.rm=TRUE)),
    .groups = "drop"
  ) %>%
  as.data.frame()
# mean(..., na.rm=TRUE) on all-missing gives NaN -> convert to NA (SAS missing)
PCMEANS$PCMEAN[is.nan(PCMEANS$PCMEAN)] <- NA_real_
# sd on all-missing gives NA already; sd on n=1 gives NA (matches SAS)
# sort like SAS BY
ord2 <- order(PCMEANS$TRTAN, PCMEANS$TRTA, PCMEANS$PCTPT, na.last=FALSE, method="radix")
PCMEANS <- PCMEANS[ord2, , drop=FALSE]

# data MeanData; if PCMEAN=0 then PCMEAN=.; PCSD=.;
MeanData <- PCMEANS
zero_idx <- !is.na(MeanData$PCMEAN) & MeanData$PCMEAN == 0
MeanData$PCMEAN[zero_idx] <- NA_real_
MeanData$PCSD[zero_idx] <- NA_real_

# keep only template vars + N for traceability; plotted data needs TRTAN TRTA PCTPT PCMEAN PCSD
MeanData_plot <- MeanData[, c("TRTAN","TRTA","PCTPT","PCMEAN","PCSD"), drop=FALSE]
# add error bar bounds for BOTH mode
MeanData_plot$LOWER <- MeanData_plot$PCMEAN - MeanData_plot$PCSD
MeanData_plot$UPPER <- MeanData_plot$PCMEAN + MeanData_plot$PCSD

cat("MeanData rows:", nrow(MeanData_plot), "\n")
print(head(MeanData_plot, 10))

# ---- write dataset CSV (plotted data) ----
write.csv(MeanData_plot, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# ---- figure params from SGRNDER ----
XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100
TICKS <- c(0,4,8,12,24,36,48,72,96)
TITLE1 <- "SPONSOR/PROTOCOL/PRODUCT INFO"
TITLE3 <- "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
TITLE4 <- "Analysis set: PK analysis set"
FOOT <- "PATH DATA/PROGRAM/OUTPUT"

# y max for linear view (SAS _YMAX not passed -> auto). Use max UPPER with padding.
ymax_lin <- suppressWarnings(max(MeanData_plot$UPPER, na.rm=TRUE))
if (!is.finite(ymax_lin)) ymax_lin <- suppressWarnings(max(MeanData_plot$PCMEAN, na.rm=TRUE))
if (!is.finite(ymax_lin) || ymax_lin <= 0) ymax_lin <- 1
ymax_lin <- ymax_lin * 1.05

# order treatments by TRTAN for legend consistency
trt_order <- unique(MeanData_plot[order(MeanData_plot$TRTAN, na.last=FALSE), c("TRTAN","TRTA")])
trt_labels <- paste0(trt_order$TRTAN, ": ", trt_order$TRTA)
# map TRTA -> label
MeanData_plot$TRTGRP <- factor(MeanData_plot$TRTA, levels = trt_order$TRTA,
                               labels = trt_labels)

# linetypes and shapes for black-and-white SAS style
n_trt <- nrow(trt_order)
lt_vals <- c("solid","dashed","dotted","dotdash","longdash","twodash")[seq_len(max(1,n_trt))]
sh_vals <- c(16,17,15,18,8,3)[seq_len(max(1,n_trt))]

# Linear view with BOTH error bars
p1 <- ggplot(MeanData_plot, aes(x=PCTPT, y=PCMEAN, group=TRTGRP, linetype=TRTGRP, shape=TRTGRP)) +
  geom_errorbar(aes(ymin=LOWER, ymax=UPPER), width=1.5, colour="black", na.rm=TRUE) +
  geom_line(colour="black", na.rm=TRUE) +
  geom_point(colour="black", size=2.2, na.rm=TRUE) +
  scale_x_continuous(limits=c(0, XMAX), breaks=TICKS) +
  scale_y_continuous(limits=c(0, ymax_lin)) +
  scale_linetype_manual(values=lt_vals) +
  scale_shape_manual(values=sh_vals) +
  labs(x=XLABEL, y=YLABEL, linetype="Treatment", shape="Treatment",
       title="Linear view") +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(size=9), axis.text=element_text(size=9),
        axis.title=element_text(size=10), legend.position="none",
        panel.grid.minor=element_blank())

# Semilog view: no error bars, log10 y
plot_log <- MeanData_plot[!is.na(MeanData_plot$PCMEAN) & MeanData_plot$PCMEAN > 0, , drop=FALSE]
p2 <- ggplot(plot_log, aes(x=PCTPT, y=PCMEAN, group=TRTGRP, linetype=TRTGRP, shape=TRTGRP)) +
  geom_line(colour="black", na.rm=TRUE) +
  geom_point(colour="black", size=2.2, na.rm=TRUE) +
  scale_x_continuous(limits=c(0, XMAX), breaks=TICKS) +
  scale_y_log10() +
  scale_linetype_manual(values=lt_vals) +
  scale_shape_manual(values=sh_vals) +
  labs(x=XLABEL, y=YLABEL, linetype="Treatment", shape="Treatment",
       title="Semilogarithmic view") +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(size=9), axis.text=element_text(size=9),
        axis.title=element_text(size=10), legend.position="none",
        panel.grid.minor=element_blank())

# Combined figure with titles/footnote and shared legend using grid
png_path <- file.path(out, "target13.png")
grDevices::png(png_path, width=2500, height=1400, res=150)
grid.newpage()
# layout: row1 titles, row2 plots, row3 legend, row4 footnote
lay <- grid.layout(nrow=5, ncol=2,
                   heights=unit(c(0.06, 0.05, 0.72, 0.12, 0.05), "npc"),
                   widths=unit(c(0.5, 0.5), "npc"))
pushViewport(viewport(layout=lay))
# Title row (span both cols)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1:2))
grid.text(TITLE1, x=0.5, y=0.5, gp=gpar(fontsize=11)) 
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1:2))
grid.text(TITLE3, x=0.5, y=0.65, gp=gpar(fontsize=11, fontface="bold"))
grid.text(TITLE4, x=0.5, y=0.20, gp=gpar(fontsize=10))
popViewport()
# plots
pushViewport(viewport(layout.pos.row=3, layout.pos.col=1))
print(p1, vp=viewport(x=0.5, y=0.5, width=0.98, height=0.98))
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
print(p2, vp=viewport(x=0.5, y=0.5, width=0.98, height=0.98))
popViewport()
# shared legend (bottom sidebar)
pushViewport(viewport(layout.pos.row=4, layout.pos.col=1:2))
# build legend grob from p1 with legend on
p_leg <- p1 + theme(legend.position="bottom", legend.box="horizontal",
                    legend.text=element_text(size=11), legend.title=element_text(size=11)) +
  guides(linetype=guide_legend(nrow=3, byrow=TRUE), shape=guide_legend(nrow=3, byrow=TRUE))
leg_grob <- ggplotGrob(p_leg)
leg_idx <- which(leg_grob$layout$name=="guide-box")
if (length(leg_idx)>0) {
  grid.draw(leg_grob$grobs[[leg_idx]])
} else {
  grid.text(paste(trt_labels, collapse="   "), gp=gpar(fontsize=11))
}
popViewport()
pushViewport(viewport(layout.pos.row=5, layout.pos.col=1:2))
grid.text(FOOT, x=0.02, y=0.5, just="left", gp=gpar(fontsize=8))
popViewport()
popViewport()
dev.off()
cat("Wrote:", png_path, "\n")
cat("Wrote:", file.path(out, "target13.csv"), "\n")
