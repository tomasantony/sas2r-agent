# P055 - outliers_TEAbnorm_Qual : mean DIABP by treatment and study day with 95% CI
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# ---------------- data work.advs ----------------
advs0 <- read_xpt(file.path(rawadam, "advs.xpt"))

# format of AVAL (PROC MEANS output stats inherit the analysis variable's format)
aval_fmt <- attr(advs0$AVAL, "format.sas")
aval_dec <- 3
if (!is.null(aval_fmt) && !is.na(aval_fmt)) {
  m <- regmatches(aval_fmt, regexpr("\\.[0-9]+$", aval_fmt))
  if (length(m) == 1) aval_dec <- as.integer(sub("\\.", "", m))
}

advs <- advs0 %>%
  filter(SAFFL == "Y", PARAMCD == "DIABP",
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN), AVISITN >= 0, AVISITN < 99,
         ANL01FL == "Y") %>%
  select(USUBJID, TRTA, ADY, AVAL)
advs <- as.data.frame(lapply(advs, function(x) { attributes(x) <- NULL; x }),
                      stringsAsFactors = FALSE)

# proc sort by TRTA ADY (missing first, SAS char ordering)
advs <- advs[order(advs$TRTA, ifelse(is.na(advs$ADY), -Inf, advs$ADY),
                   method = "radix"), ]

# ---------------- proc means ----------------
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    `_TYPE_` = 0L,
    `_FREQ_` = n(),
    num  = sum(!is.na(AVAL)),
    mean = if (sum(!is.na(AVAL)) > 0) mean(AVAL, na.rm = TRUE) else NA_real_,
    sdv  = if (sum(!is.na(AVAL)) > 1) sd(AVAL, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  as.data.frame()
tq <- rep(NA_real_, nrow(meanvs))
ok <- meanvs$num > 1
tq[ok] <- qt(0.975, meanvs$num[ok] - 1)
meanvs$lclm <- ifelse(ok, meanvs$mean - tq * meanvs$sdv / sqrt(meanvs$num), NA_real_)
meanvs$uclm <- ifelse(ok, meanvs$mean + tq * meanvs$sdv / sqrt(meanvs$num), NA_real_)
meanvs <- meanvs[, c("TRTA", "ADY", "_TYPE_", "_FREQ_", "mean", "num", "lclm", "uclm")]
meanvs <- meanvs[order(meanvs$TRTA, ifelse(is.na(meanvs$ADY), -Inf, meanvs$ADY),
                       method = "radix"), ]
write.csv(meanvs, file.path(out, "meanvs.csv"), row.names = FALSE, na = "")

shift <- function(trta, ady) {
  ifelse(trta == "Xanomeline Low Dose", ady,
  ifelse(trta == "Xanomeline High Dose", ady + 0.3,
  ifelse(trta == "Placebo", ady + 0.6, ady)))
}

mean_ds <- meanvs[, c("TRTA", "ADY", "mean")]
num_ds  <- meanvs[, c("TRTA", "ADY", "num")]
ci <- meanvs[, c("TRTA", "ADY", "lclm", "uclm")]
ci$ADY <- shift(ci$TRTA, ci$ADY)
ci <- ci[order(ci$TRTA, ifelse(is.na(ci$ADY), -Inf, ci$ADY), method = "radix"), ]
rownames(ci) <- NULL
write.csv(ci, file.path(out, "ci.csv"), row.names = FALSE, na = "")

# ---------------- proc print data=ci ----------------
round_half_up <- function(x, d) sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (v == round(v) && abs(v) < 1e11) return(sprintf("%.0f", v))
    for (d in 10:0) {
      s <- sprintf(paste0("%.", d, "f"), v)
      if (nchar(s) <= 12) { s <- sub("0+$", "", s); s <- sub("\\.$", "", s); return(s) }
    }
    formatC(v, format = "E", digits = 5)
  })
}
fmtd <- function(x, d) {
  sapply(x, function(v) if (is.na(v)) "." else sprintf(paste0("%.", d, "f"), round_half_up(v, d)))
}
pr <- data.frame(Obs = seq_len(nrow(ci)), TRTA = ci$TRTA,
                 ADY = best12(ci$ADY),
                 lclm = fmtd(ci$lclm, aval_dec), uclm = fmtd(ci$uclm, aval_dec),
                 stringsAsFactors = FALSE)
write.csv(pr, file.path(out, "P055.csv"), row.names = FALSE, na = "")

w <- sapply(names(pr), function(n) max(nchar(n), nchar(as.character(pr[[n]]))))
fmtcol <- function(v, n, left) formatC(as.character(v), width = if (left) -w[n] else w[n])
hdr <- paste(mapply(function(n) fmtcol(n, n, n == "TRTA"), names(pr)), collapse = "    ")
lines <- c(" Target 17", " ", "Systolic Blood Pressure Figure by Treatment", " ",
           "created by Xiaopeng Li", "", hdr, "")
for (i in seq_len(nrow(pr))) {
  lines <- c(lines, paste(mapply(function(n) fmtcol(pr[[n]][i], n, n == "TRTA"),
                                 names(pr)), collapse = "    "))
}
writeLines(lines, file.path(out, "P055.txt"))

# ---------------- annotate data (CI bars + N labels) ----------------
anno_ci <- ci[!is.na(ci$lclm) & !is.na(ci$uclm), ]
anno2 <- num_ds[!is.na(num_ds$num), ]
anno2$x <- anno2$ADY
anno2$y <- ifelse(anno2$TRTA == "Placebo", -170,
           ifelse(anno2$TRTA == "Xanomeline High Dose", -180,
           ifelse(anno2$TRTA == "Xanomeline Low Dose", -190, NA)))
anno2$text <- as.character(anno2$num)

# ---------------- data final ----------------
final <- bind_rows(mean_ds, num_ds)
final$ADY <- shift(final$TRTA, final$ADY)
write.csv(final, file.path(out, "final.csv"), row.names = FALSE, na = "")

plotd <- final[!is.na(final$mean) & !is.na(final$ADY), ]
plotd <- plotd[order(plotd$TRTA, plotd$ADY, method = "radix"), ]
plot_csv <- merge(plotd[, c("TRTA", "ADY", "mean")], anno_ci, by = c("TRTA", "ADY"), all.x = TRUE)
write.csv(plot_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

cols <- c("Placebo" = "blue", "Xanomeline High Dose" = "red", "Xanomeline Low Dose" = "green")
shps <- c("Placebo" = 16, "Xanomeline High Dose" = 17, "Xanomeline Low Dose" = 8)

p <- ggplot(plotd, aes(x = ADY, y = mean, colour = TRTA, shape = TRTA)) +
  geom_line() + geom_point() +
  geom_errorbar(data = anno_ci, aes(x = ADY, ymin = lclm, ymax = uclm, colour = TRTA),
                inherit.aes = FALSE, width = 1.5, linewidth = 0.6) +
  geom_text(data = anno2[!is.na(anno2$x), ], aes(x = x, y = y, label = text, colour = TRTA),
            inherit.aes = FALSE, size = 2, show.legend = FALSE) +
  scale_colour_manual(values = cols) + scale_shape_manual(values = shps) +
  scale_y_continuous(limits = c(-200, 350), breaks = seq(-200, 350, 50)) +
  scale_x_continuous(breaks = seq(0, 250, 50)) +
  coord_cartesian(xlim = c(0, 250)) +
  labs(x = "Visit number (visit day)", y = "Systolic blood pressure",
       title = "Target 17",
       subtitle = "Systolic Blood Pressure Figure by Treatment\ncreated by Xiaopeng Li",
       colour = "TRTA", shape = "TRTA") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(out, "sasgraph.png"), p, width = 8, height = 6, dpi = 100)

cat("P055 done\n")
