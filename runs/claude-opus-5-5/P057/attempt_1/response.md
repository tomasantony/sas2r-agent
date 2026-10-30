```r
# P057 - PhUSE Target 13: Mean concentration-time plot (linear + semilog views)
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

# ---- data work.orig_adpc ----
orig_adpc <- haven::read_xpt(file.path(rawadam, "adpc.xpt"))
names(orig_adpc) <- toupper(names(orig_adpc))
cat("Read ADPC:", nrow(orig_adpc), "rows\n")

PCTESTCD_ARG <- "ANAL1"
MEANTYPE <- "A"
SD_ARG <- "BOTH"

# SAS input(x, best10.) on a character value: reads the first 10 characters
sas_input_best10 <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  s <- trimws(substr(as.character(x), 1, 10))
  s[s %in% c("", ".")] <- NA
  suppressWarnings(as.numeric(s))
}

propcase <- function(x) {
  x <- tolower(x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# ---- data work.adpc ----
keepv <- intersect(c("ANLPRNT", "TRTAN", "PCSPEC", "PCTESTL", "TRTA", "DOSREFID",
                     "PCTESTCD", "PCORRESU", "PCORRES", "USUBJID", "PCORRESN", "EPLTM"),
                   names(orig_adpc))
adpc <- orig_adpc[, keepv] %>%
  filter(PCTESTCD == PCTESTCD_ARG) %>%
  mutate(
    PCTPT = sas_input_best10(EPLTM),
    BYVAL6 = gsub("\\s+", " ", paste0(propcase(PCTESTCD), " Conc ", " (", PCORRESU, ")"))
  )
cat("ADPC after WHERE:", nrow(adpc), "rows\n")

# ---- proc sort by TRTAN TRTA PCTPT (missing first) ----
adpc <- adpc %>% arrange(!is.na(TRTAN), TRTAN, !is.na(TRTA), TRTA, !is.na(PCTPT), PCTPT)

# ---- proc univariate: mean / std ----
pcmeans <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    n = sum(!is.na(PCORRESN)),
    PCmean = if (n > 0) mean(PCORRESN, na.rm = TRUE) else NA_real_,
    PCSD = if (n > 1) sd(PCORRESN, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(!is.na(TRTAN), TRTAN, !is.na(TRTA), TRTA, !is.na(PCTPT), PCTPT) %>%
  select(-n)

# ---- data MeanData ----
meandata <- pcmeans %>%
  mutate(
    zero = !is.na(PCmean) & PCmean == 0,
    PCSD = ifelse(zero, NA_real_, PCSD),
    PCmean = ifelse(zero, NA_real_, PCmean)
  ) %>%
  select(-zero) %>%
  as.data.frame()

write.csv(meandata, file.path(out, "meandata.csv"), row.names = FALSE, na = "")
write.csv(meandata, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# ---- Figure ----
pd <- meandata %>% mutate(TRTAN_f = factor(TRTAN))
ticks <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)
xlab <- "Time (h)"
ylab <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"

p_lin <- ggplot(pd %>% filter(!is.na(PCmean)),
                aes(x = PCTPT, y = PCmean, group = TRTAN_f, shape = TRTAN_f, linetype = TRTAN_f)) +
  geom_errorbar(aes(ymin = PCmean - PCSD, ymax = PCmean + PCSD), width = 1, colour = "black") +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  scale_x_continuous(breaks = ticks, limits = c(0, 100)) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = xlab, y = ylab, title = "Linear view", shape = "TRTAN", linetype = "TRTAN") +
  theme_bw() +
  theme(plot.title = element_text(size = 9, hjust = 0.5), legend.position = "bottom")

p_log <- ggplot(pd %>% filter(!is.na(PCmean) & PCmean > 0),
                aes(x = PCTPT, y = PCmean, group = TRTAN_f, shape = TRTAN_f, linetype = TRTAN_f)) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  scale_x_continuous(breaks = ticks, limits = c(0, 100)) +
  scale_y_log10() +
  labs(x = xlab, y = ylab, title = "Semilogarithmic view", shape = "TRTAN", linetype = "TRTAN") +
  theme_bw() +
  theme(plot.title = element_text(size = 9, hjust = 0.5), legend.position = "bottom")

png(file.path(out, "target13.png"), width = 2500, height = 1400, res = 200)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(0.08, 0.92), "npc"))))
grid.text(paste("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte]",
                "(overlaying) and analyte separately\nAnalysis set: PK analysis set"),
          gp = gpar(fontsize = 9),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(p_lin, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_log, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
invisible(dev.off())

cat("Done. MeanData rows:", nrow(meandata), "\n")
```