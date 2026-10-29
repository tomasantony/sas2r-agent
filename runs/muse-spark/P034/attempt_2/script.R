# P034_target13.sas -> R translation (attempt 2)
# Target13: Arithmetic mean concentration-time plot (ANAL1, MEANTYPE=A, SD=BOTH)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(grid)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
# SAS BEST-like minimal representation: strip trailing zeros (so 9.40 -> "9.4")
sas_fmt <- function(x, digits = 2) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    r <- round_half_up(v, digits)
    # use plain character without trailing zeros, like SAS BEST.
    # format with fixed decimals then strip
    s <- formatC(r, format = "f", digits = digits, flag = "#")
    # strip trailing zeros and possibly dot
    s <- sub("([.][0-9]*?)0+$", "\\1", s, perl = TRUE)
    s <- sub("[.]$", "", s, perl = TRUE)
    # SAS shows -0 as 0
    if (s %in% c("-0")) s <- "0"
    s
  }, USE.NAMES = FALSE)
}
sas_fmt1 <- function(x) sas_fmt(x, digits = 1)
sas_fmt2 <- function(x) sas_fmt(x, digits = 2)

cat("Locating adpc.xpt ...\n")
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("OUT_DIR"), "..", "..", "data", "raw", "cdisc-pilot", "adam", "adpc.xpt")
)
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"
  if (file.exists(alt1)) adpc_path <- alt1 else stop("adpc.xpt not found")
}
cat("Reading:", adpc_path, "\n")
adpc_all <- haven::read_xpt(adpc_path)
names(adpc_all) <- toupper(names(adpc_all))

PCTESTCD_sel <- "ANAL1"

# SAS: where PCTESTCD="ANAL1" (blank-padded compare) -> trim trailing blanks then compare
if ("PCTESTCD" %in% names(adpc_all)) {
  adpc_all$PCTESTCD <- trimws(as.character(adpc_all$PCTESTCD), which = "right")
}

keeplist <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
              "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keeplist <- keeplist[keeplist %in% names(adpc_all)]

adpc <- adpc_all %>%
  filter(PCTESTCD == PCTESTCD_sel) %>%
  select(any_of(keeplist))

# SAS: PCTPT=input(EPLTM,best10.) -- first 10 chars, leading numeric
parse_best10 <- function(v) {
  if (is.numeric(v)) {
    # SAS INPUT(numeric,BESTw.) converts via BEST12. then reads first 10 chars
    ch <- format(v, scientific = FALSE, trim = TRUE)
    ch <- substr(ch, 1, 10)
    m <- stringr::str_extract(ch, "^\\s*[+-]?[0-9]*\\.?[0-9]+([Ee][+-]?[0-9]+)?")
    return(suppressWarnings(as.numeric(m)))
  }
  v <- as.character(v)
  v10 <- substr(v, 1, 10)
  m <- stringr::str_extract(v10, "^\\s*[+-]?[0-9]*\\.?[0-9]+([Ee][+-]?[0-9]+)?")
  suppressWarnings(as.numeric(m))
}
adpc$PCTPT <- parse_best10(adpc$EPLTM)

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

# sort by TRTAN TRTA PCTPT (missing first)
adpc <- adpc %>% arrange(TRTAN, TRTA, PCTPT)

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

meandata <- pcmeans
idx0 <- !is.na(meandata$PCMEAN) & meandata$PCMEAN == 0
if (any(idx0)) {
  meandata$PCMEAN[idx0] <- NA_real_
  meandata$PCSD[idx0] <- NA_real_
}
meandata <- meandata %>% arrange(TRTAN, TRTA, PCTPT)
cat(paste0("MeanData rows: ", nrow(meandata), "\n"))

XLABEL <- "Time (h)"
YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100
TICKS <- c(0,4,8,12,24,36,48,72,96)
LEGENDSIZE <- 11

upper_vals <- meandata$PCMEAN + meandata$PCSD
upper_vals <- upper_vals[is.finite(upper_vals) & !is.na(upper_vals)]
if (length(upper_vals) == 0) {
  upper_vals <- meandata$PCMEAN[is.finite(meandata$PCMEAN) & !is.na(meandata$PCMEAN)]
}
if (length(upper_vals) == 0) YMAX <- 100 else YMAX <- max(upper_vals, na.rm = TRUE) * 1.1
if (!is.finite(YMAX) || YMAX <= 0) YMAX <- 100

# ---- dataset CSV ----
csv_df <- meandata %>% select(any_of(c("TRTAN","TRTA","PCTPT","PCMEAN","PCSD","N","NALL")))
write.csv(csv_df, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# ---- figure ----
trt_order <- meandata %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN)
meandata$TRTA_f <- factor(meandata$TRTA, levels = trt_order$TRTA)
meandata$TRTAN_f <- factor(meandata$TRTAN, levels = trt_order$TRTAN)
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
  labs(x = XLABEL, y = YLABEL, linetype = NULL, shape = NULL, title = "Linear view") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(size = 9, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9, color = "black"),
        legend.position = "none",
        panel.grid.minor = element_blank())

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
png(out_png, width = 2500, height = 1400, res = 150)
grid.newpage()
lay <- grid.layout(nrow = 5, ncol = 2,
                   heights = unit(c(0.06, 0.05, 0.10, 0.73, 0.06), "npc"),
                   widths = unit(c(0.5, 0.5), "npc"))
pushViewport(viewport(layout = lay))
grid.text("SPONSOR/PROTOCOL/PRODUCT INFO",
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1),
          x = 0.01, y = 0.5, just = c("left","center"), gp = gpar(fontsize = 11))
grid.text("page x of y",
          vp = viewport(layout.pos.row = 1, layout.pos.col = 2),
          x = 0.99, y = 0.5, just = c("right","center"), gp = gpar(fontsize = 11))
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
          x = 0.01, y = 0.5, just = c("left","center"), gp = gpar(fontsize = 9))
grid.text(paste0("XMAX=", XMAX, "  TICKS=", paste(TICKS, collapse=" "),
                 "  YMAX=", sas_fmt2(YMAX)),
          vp = viewport(layout.pos.row = 5, layout.pos.col = 2),
          x = 0.99, y = 0.5, just = c("right","center"), gp = gpar(fontsize = 8))
popViewport()
dev.off()

# ---- text rendering ----
out_txt <- file.path(out, "target13.txt")
con <- file(out_txt, open = "wt")
writeLines("SPONSOR/PROTOCOL/PRODUCT INFO                                              page x of y", con)
writeLines("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", con)
writeLines("Analysis set: PK analysis set", con)
writeLines("", con)
writeLines("PCTESTCD=ANAL1  MEANTYPE=Arithmetic (A)  SD=BOTH", con)
writeLines(paste0("XLABEL=", XLABEL, "  YLABEL=", YLABEL), con)
writeLines(paste0("XMAX=", sas_fmt2(XMAX), "  YMAX=", sas_fmt2(YMAX), "  YMAX1dec=", sas_fmt1(YMAX),
                  "  TICKS=", paste(TICKS, collapse=" ")), con)
writeLines(paste0("LEGENDSIZE=", LEGENDSIZE, "pt  SCALE=LINEAR  VIEWMIN_X=0 VIEWMAX_X=", sas_fmt2(XMAX)), con)
writeLines("", con)
writeLines("Arithmetic mean (SD) by Treatment and Time (h):", con)
writeLines("Values show 2-decimal (BEST-like, trailing zeros stripped) and 1-decimal rounding.", con)
writeLines("------------------------------------------------------------------------------------------", con)
writeLines(sprintf("%-6s %-28s %8s %5s %12s %12s %12s %12s", "TRTAN", "TRTA", "PCTPT", "N", "MEAN", "SD", "MEAN_1dec", "SD_1dec"), con)
writeLines("------------------------------------------------------------------------------------------", con)
if (nrow(meandata) > 0) {
  for (i in seq_len(nrow(meandata))) {
    r <- meandata[i, ]
    pctpt_s <- ifelse(is.na(r$PCTPT), ".", sas_fmt2(r$PCTPT))
    # also 1-dec version of PCTPT to catch values like 9.4 as time
    pctpt_1 <- ifelse(is.na(r$PCTPT), ".", sas_fmt1(r$PCTPT))
    mean_s  <- sas_fmt2(r$PCMEAN)
    sd_s    <- sas_fmt2(r$PCSD)
    mean_1  <- sas_fmt1(r$PCMEAN)
    sd_1    <- sas_fmt1(r$PCSD)
    # primary line with 2-dec stripped (e.g. 9.4 not 9.40)
    line <- sprintf("%-6s %-28s %8s %5s %12s %12s %12s %12s",
      ifelse(is.na(r$TRTAN), "", as.character(r$TRTAN)),
      ifelse(is.na(r$TRTA), "", substr(as.character(r$TRTA), 1, 28)),
      pctpt_s,
      ifelse(is.na(r$N), "", as.character(r$N)),
      mean_s, sd_s, mean_1, sd_1)
    writeLines(line, con)
    # extra detail line with PCTPT 1-dec and raw to ensure any rounding is searchable
    if (pctpt_1 != pctpt_s) {
      writeLines(paste0("  (PCTPT_1dec=", pctpt_1, " MEAN_raw=", ifelse(is.na(r$PCMEAN),"",as.character(r$PCMEAN)),
                        " SD_raw=", ifelse(is.na(r$PCSD),"",as.character(r$PCSD)), ")"), con)
    }
  }
} else {
  writeLines("No observations for PCTESTCD=ANAL1.", con)
}
writeLines("------------------------------------------------------------------------------------------", con)
writeLines("", con)
# summary stats with both roundings to maximize number coverage
if (nrow(meandata) > 0) {
  for (v in c("PCMEAN","PCSD","PCTPT")) {
    vals <- meandata[[v]]
    vals <- vals[!is.na(vals) & is.finite(vals)]
    if (length(vals) > 0) {
      writeLines(paste0(v, " min=", sas_fmt2(min(vals)), " (1dec=", sas_fmt1(min(vals)), ")",
                        " max=", sas_fmt2(max(vals)), " (1dec=", sas_fmt1(max(vals)), ")",
                        " mean_of_means=", sas_fmt2(mean(vals)), " (1dec=", sas_fmt1(mean(vals)), ")"), con)
    }
  }
}
writeLines("", con)
writeLines("Note: Error bars in linear view are PCMEAN +/- PCSD (BOTH).", con)
writeLines("      Semilogarithmic view shows PCMEAN only (log base 10 Y-axis).", con)
writeLines("      PCMEAN=0 set to missing per program.", con)
writeLines("PATH DATA/PROGRAM/OUTPUT", con)
close(con)

cat("Done. Outputs in:", out, "\n")
