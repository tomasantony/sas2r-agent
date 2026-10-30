```r
# P034 - PhUSE Target 13: Mean concentration-time plot (linear and semi-log)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

orig_adpc <- read_xpt(file.path(rawadam, "adpc.xpt"))
names(orig_adpc) <- toupper(names(orig_adpc))
cat("Read ADPC:", nrow(orig_adpc), "rows\n")

# SAS INPUT(x, BEST10.): reads only the first 10 characters of the value
sas_input_best10 <- function(x) {
  if (is.numeric(x)) {
    x <- format(x, trim = TRUE)
  }
  x <- substr(as.character(x), 1, 10)
  suppressWarnings(as.numeric(trimws(x)))
}

# SAS PROPCASE: first letter of each word upper, the rest lower
propcase <- function(x) {
  x <- tolower(x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

target13 <- function(PCTESTCD = "ANAL1", Meantype = "A", SD = "BOTH") {

  keepv <- intersect(c("ANLPRNT", "TRTAN", "PCSPEC", "PCTESTL", "TRTA", "DOSREFID",
                       "PCTESTCD", "PCORRESU", "PCORRES", "USUBJID", "PCORRESN",
                       "EPLTM"),
                     names(orig_adpc))

  adpc <- orig_adpc[, keepv] %>%
    filter(PCTESTCD == !!PCTESTCD) %>%
    mutate(PCTPT = sas_input_best10(EPLTM))

  # PROC SORT: missing values sort first
  adpc <- adpc %>%
    arrange(!is.na(TRTAN), TRTAN, !is.na(TRTA), TRTA, !is.na(PCTPT), PCTPT)

  if (toupper(Meantype) == "A") {
    # PROC UNIVARIATE: mean and SD by group (SD missing when n < 2)
    pcmeans <- adpc %>%
      group_by(TRTAN, TRTA, PCTPT) %>%
      summarise(
        PCmean = if (sum(!is.na(PCORRESN)) > 0) mean(PCORRESN, na.rm = TRUE) else NA_real_,
        PCSD   = if (sum(!is.na(PCORRESN)) > 1) sd(PCORRESN, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )
    meandata <- pcmeans %>%
      mutate(
        zero   = !is.na(PCmean) & PCmean == 0,
        PCmean = ifelse(zero, NA_real_, PCmean),
        PCSD   = ifelse(zero, NA_real_, PCSD)
      ) %>%
      select(-zero)
  } else {
    stop("Only the arithmetic mean option is executed in this program")
  }
  meandata
}

meandata <- target13(PCTESTCD = "ANAL1", Meantype = "A", SD = "BOTH")
cat("MeanData rows:", nrow(meandata), "\n")

write.csv(meandata, file.path(out, "meandata.csv"), row.names = FALSE, na = "")

# Plotted data: SD = BOTH, so error bars run from mean - SD to mean + SD
plotdat <- meandata %>%
  mutate(
    YERRLOWER = PCmean - PCSD,
    YERRUPPER = PCmean + PCSD,
    TRTAN_F   = factor(TRTAN)
  )
write.csv(plotdat %>% select(-TRTAN_F), file.path(out, "target13.csv"),
          row.names = FALSE, na = "")

ticks <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
ylab  <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
xlab  <- "Time (h)"

pd <- plotdat %>% filter(!is.na(PCTPT), !is.na(PCmean))

# Linear view
p_lin <- ggplot(pd, aes(PCTPT, PCmean, group = TRTAN_F, linetype = TRTAN_F, shape = TRTAN_F)) +
  geom_errorbar(aes(ymin = YERRLOWER, ymax = YERRUPPER), colour = "black", width = 1,
                na.rm = TRUE) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  scale_x_continuous(breaks = ticks, limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = xlab, y = ylab, title = "Linear view",
       linetype = "TRTAN", shape = "TRTAN") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(size = 9, hjust = 0.5))

# Semilogarithmic view (log scale requires positive values)
p_log <- ggplot(pd %>% filter(PCmean > 0),
                aes(PCTPT, PCmean, group = TRTAN_F, linetype = TRTAN_F, shape = TRTAN_F)) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  scale_x_continuous(breaks = ticks, limits = c(0, 100)) +
  scale_y_log10() +
  labs(x = xlab, y = ylab, title = "Semilogarithmic view",
       linetype = "TRTAN", shape = "TRTAN") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(size = 9, hjust = 0.5))

png(file.path(out, "target13.png"), width = 2500, height = 1400, res = 200)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
  3, 2, heights = unit(c(0.12, 0.8, 0.08), "npc"))))
grid.text(paste0("SPONSOR/PROTOCOL/PRODUCT INFO\n",
                 "Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per ",
                 "[analyte] (overlaying) and analyte separately\n",
                 "Analysis set: PK analysis set"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
          gp = gpar(fontsize = 9))
print(p_lin, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_log, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
grid.text("PATH DATA/PROGRAM/OUTPUT", x = 0.02, hjust = 0,
          vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2),
          gp = gpar(fontsize = 8))
invisible(dev.off())

cat("Done. Outputs written to", out, "\n")
```