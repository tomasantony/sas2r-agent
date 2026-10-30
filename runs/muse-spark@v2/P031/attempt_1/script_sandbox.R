out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

cat("P031 Boxplot group: generating data\n")

# SAS ROUND(x, unit): round half away from zero
sas_round <- function(x, unit) {
  if (unit == 0) return(x)
  s <- sign(x)
  # sign(0)=0 -> keep 0
  a <- abs(x) / unit
  r <- floor(a + 0.5 + 1e-12)
  s * r * unit
}

# Reproduce DATA X:
# DO TRT="Study Drug","Placebo","AC"; DO J=1 TO 10; DO VISIT=3 TO 6;
# X=ROUND(J+2*NORMAL(777),0.1);
# R translation: set.seed(777) once then rnorm(0,1) in same loop order
set.seed(777)
trts <- c("Study Drug", "Placebo", "AC")

rows <- vector("list", 3*10*4)
k <- 1L
for (trt in trts) {
  for (j in 1:10) {
    subjid <- j + 100*(trt == "Study Drug") + 200*(trt == "Placebo") + 300*(trt == "AC")
    for (visit in 3:6) {
      z <- rnorm(1, mean = 0, sd = 1)
      xval <- sas_round(j + 2*z, 0.1)
      # avoid floating artefacts, keep 1 decimal like SAS 0.1 rounding
      xval <- sas_round(xval, 0.1)
      rows[[k]] <- data.frame(TRT = trt, SUBJID = subjid, VISIT = visit, X = xval,
                              stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
}
X <- bind_rows(rows)

# DATA XX: VIS jitter for side-by-side boxes
XX <- X %>%
  mutate(
    VIS = dplyr::case_when(
      TRT == "Study Drug" ~ as.numeric(VISIT),
      TRT == "Placebo"    ~ as.numeric(VISIT) + 0.15,
      TRT == "AC"         ~ as.numeric(VISIT) + 0.30,
      TRUE ~ as.numeric(NA)
    )
  )
# IF VISIT IN (4,5) THEN VIS=VIS-0.15;
XX$VIS[XX$VISIT %in% c(4,5)] <- XX$VIS[XX$VISIT %in% c(4,5)] - 0.15
# IF VISIT=6 THEN VIS=VIS-0.3;
XX$VIS[XX$VISIT == 6] <- XX$VIS[XX$VISIT == 6] - 0.3

stopifnot(all(c("TRT","SUBJID","VISIT","VIS","X") %in% names(XX)))
stopifnot(nrow(XX) == 120)

# PROC SORT DATA=XX; BY VIS;
XX <- XX %>% arrange(VIS, TRT, SUBJID)

# CSV of plotted data (same basename as figure)
plot_csv <- file.path(out, "p031_f_plsr_box_plot.csv")
write.csv(XX[, c("TRT","SUBJID","VISIT","VIS","X")], plot_csv, row.names = FALSE, na = "")
cat("Wrote", plot_csv, "rows:", nrow(XX), "\n")

# PROC BOXPLOT DATA=XX; PLOT X*vis=TRT / HAXIS ORDER 3 TO 6, BOXSTYLE=SCHEMATICID
# CBOX=BLUE IDSYMBOL=STAR IDCOLOR=RED; SYMBOL colors black/blue/red; TITLE Box Plot...
# Reproduce with ggplot: continuous VIS on x (offset per TRT), one box per VIS value
XX$TRT <- factor(XX$TRT, levels = c("Study Drug","Placebo","AC"))
XX$VISF <- factor(XX$VIS)

p <- ggplot(XX, aes(x = VIS, y = X, group = VISF, fill = TRT)) +
  geom_boxplot(width = 0.12, colour = "blue",
               outlier.shape = 8, outlier.colour = "red", outlier.size = 3,
               position = position_identity()) +
  scale_x_continuous(name = "Visit", breaks = 3:6, labels = c("3","4","5","6"),
                     limits = c(2.85, 6.15)) +
  labs(title = "Box Plot of X over Time by Treatment", y = "X", fill = "Treatment") +
  scale_fill_manual(values = c("Study Drug" = "grey85", "Placebo" = "lightblue", "AC" = "mistyrose"),
                    breaks = c("Study Drug","Placebo","AC")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

png_file <- file.path(out, "p031_f_plsr_box_plot.png")
ggsave(png_file, plot = p, width = 9, height = 7, units = "in", dpi = 150)
cat("Wrote", png_file, "\n")
