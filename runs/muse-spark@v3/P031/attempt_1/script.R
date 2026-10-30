out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# SAS ROUND half away from zero helper
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  y <- abs(x) * mult
  y <- floor(y + 0.5 + 1e-10) / mult
  y * sign(x + 0 * y) * ifelse(x == 0, 1, 1)
}
sas_round <- function(x, unit) {
  if (unit == 0) return(x)
  round_half_up(x / unit, 0) * unit
}
# SAS ROUND(x,0.1) -> 1 decimal half away
sas_round_01 <- function(x) round_half_up(x, 1)

# ------------------------------------------------------------------
# DATA X;
# LENGTH TRT $10.;
# DO TRT="Study Drug","Placebo","AC";
# DO J= 1 TO 10;
# SUBJID=J+100*(TRT="Study Drug")+200*(TRT="Placebo")+300*(TRT="AC");
# DO VISIT=3 TO 6;
# X=ROUND(J+2*NORMAL(777),0.1);
# OUTPUT;
# ------------------------------------------------------------------
# SAS NORMAL(777): SAS old RNG stream seeded at 777, one call per
# TRT/J/VISIT in loop order TRT outer, J middle, VISIT inner.
# R cannot call SAS RNG; emulate deterministically with R RNG seeded
# at 777 in the identical call order so structure/counts match.
set.seed(777)
trt_levels <- c("Study Drug", "Placebo", "AC")
n_total <- length(trt_levels) * 10 * 4  # 120
z <- rnorm(n_total)  # one N(0,1) per OUTPUT in SAS loop order

X <- data.frame(TRT = character(0), SUBJID = numeric(0),
                VISIT = numeric(0), X = numeric(0),
                stringsAsFactors = FALSE)
k <- 0L
for (trt in trt_levels) {
  for (j in 1:10) {
    subjid <- j + 100 * (trt == "Study Drug") + 200 * (trt == "Placebo") + 300 * (trt == "AC")
    for (visit in 3:6) {
      k <- k + 1L
      xv <- sas_round_01(j + 2 * z[k])
      X <- rbind(X, data.frame(TRT = trt, SUBJID = subjid,
                               VISIT = visit, X = xv,
                               stringsAsFactors = FALSE))
    }
  }
}
# LENGTH TRT $10 already satisfied (max nchar 10)

# ------------------------------------------------------------------
# DATA XX;
# SET X;
# IF TRT="Study Drug" THEN VIS=VISIT;
# ELSE IF TRT="Placebo" THEN VIS=VISIT+0.15;
# ELSE IF TRT="AC" THEN VIS=VISIT+0.30;
# IF VISIT IN (4,5) THEN VIS=VIS-0.15;
# IF VISIT=6 THEN VIS=VIS-0.3;
# RUN;
# PROC SORT DATA=XX; BY VIS; RUN;
# ------------------------------------------------------------------
XX <- X %>%
  mutate(
    VIS = ifelse(TRT == "Study Drug", VISIT,
          ifelse(TRT == "Placebo", VISIT + 0.15,
          ifelse(TRT == "AC", VISIT + 0.30, NA_real_))),
    VIS = ifelse(VISIT %in% c(4, 5), VIS - 0.15, VIS),
    VIS = ifelse(VISIT == 6, VIS - 0.3, VIS)
  )
# SAS sort: missing first, then numeric ascending; no missings here
XX <- XX %>% arrange(VIS)

# CSV of plotted data (PROC BOXPLOT DATA=XX, PLOT X*vis=TRT)
plot_data <- XX[, c("TRT", "SUBJID", "VISIT", "X", "VIS")]
write.csv(plot_data, file.path(out, "p031_f_plsr_box_plot.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------
# PROC BOXPLOT DATA=XX;
# FORMAT VIS 4.;
# PLOT X*vis=TRT / HAXIS=AXIS1 BOXSTYLE=SCHEMATICID CBOX=BLUE
#                  IDSYMBOL=STAR IDCOLOR=RED;
# AXIS LABEL=("Visit") ORDER=(3 TO 6 BY 1);
# TITLE1 "Box Plot of X over Time by Treatment";
# SYMBOL1 V=PLUS C=BLACK; SYMBOL2 V=TRIANGLE C=BLUE; SYMBOL3 V=SQUARE C=RED;
# ------------------------------------------------------------------
# VIS positions (12 groups):
#  Study Drug: 3, 3.85, 4.85, 5.7
#  Placebo   : 3.15, 4, 5, 5.85
#  AC        : 3.3, 4.15, 5.15, 6.0
plot_data$TRT <- factor(plot_data$TRT, levels = c("Study Drug", "Placebo", "AC"))
plot_data$VISGRP <- factor(plot_data$VIS, levels = sort(unique(plot_data$VIS)))

# Schematic boxplot: boxes blue outline, outliers as red stars
p <- ggplot(plot_data, aes(x = VIS, y = X, group = VISGRP, fill = TRT)) +
  geom_boxplot(width = 0.12, colour = "blue", outlier.shape = 8,
               outlier.colour = "red", outlier.size = 3) +
  scale_x_continuous(name = "Visit", breaks = 3:6, labels = c("3", "4", "5", "6"),
                     limits = c(2.8, 6.2)) +
  scale_fill_manual(values = c("Study Drug" = "grey80", "Placebo" = "lightblue", "AC" = "mistyrose"),
                    name = "TRT") +
  ggtitle("Box Plot of X over Time by Treatment") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

ggsave(file.path(out, "p031_f_plsr_box_plot.png"), plot = p,
       width = 9, height = 7, units = "in", dpi = 150)
