# P034_target13.sas -> R translation
# Target 13: Arithmetic Mean concentration-time plot (ANAL1, MEANTYPE=A, SD=BOTH)
# Input: adpc.xpt ; Output: target13.png + target13.csv (plotted MeanData)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(grid)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# SAS round half away from zero helper (for display formatting if needed)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

cat("Resolving input adpc.xpt path...\n")
cand <- character(0)
rawadam_env <- Sys.getenv("RAWADAM")
root_env <- Sys.getenv("ROOT")
if (rawadam_env != "") cand <- c(cand, file.path(rawadam_env, "adpc.xpt"))
if (root_env != "") cand <- c(cand, file.path(root_env, "data/raw/cdisc-pilot/adam/adpc.xpt"),
                                    file.path(root_env, "data/derived/cdisc-pilot/adam/adpc.xpt"))
cand <- c(cand,
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(getwd(), "adpc.xpt"))
# also try OUT_DIR parent search is not needed; find first existing
adpc_path <- NULL
for (p in cand) {
  pp <- gsub("\\\\", "/", p)
  if (file.exists(pp)) { adpc_path <- pp; break }
}
if (is.null(adpc_path)) {
  # last resort: search for adpc.xpt under current drive
  stop("adpc.xpt not found. Tried: ", paste(cand, collapse=" | "))
}
cat("Reading: ", adpc_path, "\n")
orig_adpc <- haven::read_xpt(adpc_path)

# work.orig_adpc = set source.adpc
work_orig_adpc <- orig_adpc

# Macro call: %TARGET13(pctestcd=ANAL1, MEANTYPE=A, SD=BOTH)
PCTESTCD_m <- "ANAL1"
MEANTYPE_m <- "A"
SD_m <- "BOTH"

# data work.adpc; set source.adpc(keep=...); where PCTESTCD="&PCTESTCD";
# PCTPT=input(EPLTM,best10.); ORDERBY=...; BYVAL6=...;
keep_vars <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
               "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keep_vars <- keep_vars[keep_vars %in% names(work_orig_adpc)]
adpc <- work_orig_adpc[, keep_vars, drop = FALSE]

# case-sensitive filter like SAS WHERE
stopifnot("PCTESTCD" %in% names(adpc))
adpc <- adpc %>% filter(PCTESTCD == PCTESTCD_m)
stopifnot(nrow(adpc) > 0)

# PCTPT = input(EPLTM, best10.) : SAS numeric informat, leading numeric
# EPLTM may be character or numeric
if ("EPLTM" %in% names(adpc)) {
  epltm_chr <- trimws(as.character(adpc$EPLTM))
  # SAS BEST informat: parse optional sign, digits, decimal, exponent; else missing
  # emulate with suppressWarnings(as.numeric()) which matches for plain numbers
  adpc$PCTPT <- suppressWarnings(as.numeric(epltm_chr))
  # For strings with trailing text SAS would give missing (without ??) -> as.numeric also NA, consistent
} else {
  adpc$PCTPT <- NA_real_
}

# ORDERBY=compress(ANLPRNT||PCSPEC||PCTESTL||TRTAN); BYVAL6=compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')');
# SAS COMPRESS removes blanks; || concatenates (missing -> blank handling approximated)
sas_cat <- function(...) {
  args <- lapply(list(...), function(x) ifelse(is.na(x), "", as.character(x)))
  do.call(paste0, args)
}
if (all(c("ANLPRNT","PCSPEC","PCTESTL","TRTAN") %in% names(adpc))) {
  adpc$ORDERBY <- gsub(" ", "", sas_cat(adpc$ANLPRNT, adpc$PCSPEC, adpc$PCTESTL, adpc$TRTAN), fixed = TRUE)
}
if (all(c("PCTESTCD","PCORRESU") %in% names(adpc))) {
  # propcase: first letter upper, rest lower
  prop <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x), "", paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x)))))
  }
  raw6 <- paste0(prop(adpc$PCTESTCD), " Conc", " (", ifelse(is.na(adpc$PCORRESU),"",adpc$PCORRESU), ")")
  adpc$BYVAL6 <- gsub("\\s+", " ", trimws(raw6))
}

# proc sort data=ADPC; by TRTAN TRTA PCTPT; run;
# SAS: missing first, character binary (uppercase before lowercase), numeric ascending
# Use base order with na.last=FALSE, method="radix" for binary order
ord <- base::order(adpc$TRTAN, adpc$TRTA, adpc$PCTPT, na.last = FALSE, method = "radix")
adpc <- adpc[ord, , drop = FALSE]

# %if &meantype=A : proc univariate noprint; by TRTAN TRTA PCTPT; var PCORRESN; output mean STD;
# SAS UNIVARIATE STD = sample std (n-1), missing if n<=1 or all missing
if (toupper(MEANTYPE_m) == "A") {
  stopifnot(all(c("TRTAN","TRTA","PCTPT","PCORRESN") %in% names(adpc)))
  pcmeans <- adpc %>%
    group_by(TRTAN, TRTA, PCTPT) %>%
    summarise(
      N = sum(!is.na(PCORRESN)),
      PCMEAN = ifelse(sum(!is.na(PCORRESN)) > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
      PCSD = ifelse(sum(!is.na(PCORRESN)) > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    as.data.frame()
  # convert NaN (all-missing mean) to NA like SAS missing
  pcmeans$PCMEAN[is.nan(pcmeans$PCMEAN)] <- NA_real_
  # data MeanData; set PCMEANS; if PCMEAN=0 then do; PCMEAN=.; PCSD=.; end; run;
  meandata <- pcmeans
  idx0 <- !is.na(meandata$PCMEAN) & meandata$PCMEAN == 0
  meandata$PCMEAN[idx0] <- NA_real_
  meandata$PCSD[idx0] <- NA_real_
} else {
  stop("Only MEANTYPE=A path needed for this call (ANAL1/A/BOTH)")
}

# sort MeanData for series plot (by TRTAN TRTA PCTPT)
ord2 <- base::order(meandata$TRTAN, meandata$TRTA, meandata$PCTPT, na.last = FALSE, method = "radix")
meandata <- meandata[ord2, , drop = FALSE]

cat("MeanData rows:", nrow(meandata), "\n")

# Write plotted dataset as CSV (SAS dataset counterpart + plotted-data CSV)
# Use SAS variable names, ISO dates not applicable here, numerics unrounded
write.csv(meandata, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# ---- Figure: replicate PROC TEMPLATE temp_pksum + SGRENDER ----
# Dynamics from sgrender:
# _sd="BOTH" _XLABEL="Time (h)" SCALE="LINEAR"
# _YLABEL="Arithmetic mean (SD) Anal2 Conc (ng/mL )" _XMAX=100 _LEGENDSIZE="11pt"
# TICKS="0 4 8 12 24 36 48 72 96"
x_label <- "Time (h)"
y_label <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
x_max <- 100
ticks <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)

title1L <- "SPONSOR/PROTOCOL/PRODUCT INFO"
title1R <- "page x of y"
title3 <- "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately"
title4 <- "Analysis set: PK analysis set"
foot1 <- "PATH DATA/PROGRAM/OUTPUT"

# Ensure TRTA is factor ordered by TRTAN for stable legend (SAS group=TRTAN order)
trt_order <- unique(meandata[order(meandata$TRTAN, method = "radix"), c("TRTAN","TRTA")])
trt_levels <- trt_order$TRTA
meandata$TRTA <- factor(meandata$TRTA, levels = trt_levels)

# For log panel, SAS TYPE=LOG BASE=10 ; non-positive means are dropped
meandata_log <- meandata %>% filter(!is.na(PCMEAN) & PCMEAN > 0)

common_theme <- theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Linear view with BOTH error bars: yerrorlower=PCMEAN-PCSD, yerrorupper=PCMEAN+PCSD
p_linear <- ggplot(meandata, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_errorbar(aes(ymin = PCMEAN - PCSD, ymax = PCMEAN + PCSD),
                color = "black", width = 1.5, na.rm = TRUE) +
  geom_line(color = "black", na.rm = TRUE) +
  geom_point(color = "black", size = 2.5, na.rm = TRUE) +
  scale_x_continuous(limits = c(0, x_max), breaks = ticks) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = x_label, y = y_label, title = "Linear view") +
  common_theme

p_semilog <- ggplot(meandata_log, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_line(color = "black", na.rm = TRUE) +
  geom_point(color = "black", size = 2.5, na.rm = TRUE) +
  scale_x_continuous(limits = c(0, x_max), breaks = ticks) +
  scale_y_log10() +
  labs(x = x_label, y = y_label, title = "Semilogarithmic view") +
  common_theme

# Legend grob data: merged legend s1+s2 (line + marker) per treatment
# Build a small legend plot to extract legend, then draw manually via grid
p_legend_source <- ggplot(meandata, aes(x = PCTPT, y = PCMEAN, group = TRTA, linetype = TRTA, shape = TRTA)) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2.5) +
  labs(linetype = "", shape = "") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.width = unit(2.2, "cm"))
leg_grob <- {
  g <- ggplotGrob(p_legend_source + guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)))
  idx <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  if (length(idx) > 0) g$grobs[[idx]] else nullGrob()
}

png(file.path(out, "target13.png"), width = 2500, height = 1400)
grid.newpage()
lay <- grid.layout(nrow = 6, ncol = 2,
                   heights = unit(c(0.055, 0.05, 0.045, 0.74, 0.07, 0.04), "npc"),
                   widths = unit(c(0.5, 0.5), "npc"))
pushViewport(viewport(layout = lay))
# Row1: title J=L ... j=R ...
grid.text(title1L, x = unit(0.01, "npc"), y = unit(1, "npc"),
          just = c("left", "center"), gp = gpar(fontsize = 12),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
grid.text(title1R, x = unit(0.99, "npc"), y = unit(1, "npc"),
          just = c("right", "center"), gp = gpar(fontsize = 12),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
grid.text(title3, gp = gpar(fontsize = 12), vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
grid.text(title4, gp = gpar(fontsize = 12), vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
print(p_linear, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(p_semilog, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
# Sidebar bottom legend (merged)
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1:2))
grid.draw(leg_grob)
popViewport()
grid.text(foot1, x = unit(0.01, "npc"), just = c("left", "center"),
          gp = gpar(fontsize = 10), vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))
popViewport()
dev.off()

cat("Wrote ", file.path(out, "target13.png"), " and ", file.path(out, "target13.csv"), "\n")
