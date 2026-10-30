```r
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- SAS-like helpers -------------------------------------------------------
round_half_up <- function(x, unit = 1) {
  sign(x) * floor(abs(x) / unit + 0.5 + 1e-9) * unit
}

# Emulation of the legacy SAS RANUNI/NORMAL stream
# (multiplicative congruential generator, modulus 2^31-1, multiplier 397204094).
# This approximates SAS; exact equality with the SAS stream cannot be guaranteed.
sas_rng_env <- new.env()
sas_rng_env$seed <- NULL
sas_rng_env$cache <- NULL
mulmod <- function(a, s, m) {
  a1 <- a %/% 65536
  a0 <- a %% 65536
  t1 <- ((a1 * s) %% m) * 65536 %% m
  t0 <- (a0 * s) %% m
  (t1 + t0) %% m
}
sas_ranuni <- function() {
  m <- 2147483647
  sas_rng_env$seed <- mulmod(397204094, sas_rng_env$seed, m)
  sas_rng_env$seed / m
}
sas_normal <- function(seed) {
  if (is.null(sas_rng_env$seed)) sas_rng_env$seed <- seed
  if (!is.null(sas_rng_env$cache)) {
    v <- sas_rng_env$cache
    sas_rng_env$cache <- NULL
    return(v)
  }
  u1 <- sas_ranuni()
  u2 <- sas_ranuni()
  r <- sqrt(-2 * log(u1))
  sas_rng_env$cache <- r * sin(2 * pi * u2)
  r * cos(2 * pi * u2)
}

# ---- DATA X ----------------------------------------------------------------
rows <- list()
k <- 0
for (TRT in c("Study Drug", "Placebo", "AC")) {
  for (J in 1:10) {
    SUBJID <- J + 100 * (TRT == "Study Drug") + 200 * (TRT == "Placebo") + 300 * (TRT == "AC")
    for (VISIT in 3:6) {
      X <- round_half_up(J + 2 * sas_normal(777), 0.1)
      k <- k + 1
      rows[[k]] <- data.frame(TRT = TRT, SUBJID = SUBJID, VISIT = VISIT, X = X,
                              stringsAsFactors = FALSE)
    }
  }
}
x <- bind_rows(rows)

# ---- DATA XX ----------------------------------------------------------------
xx <- x %>%
  mutate(
    VIS = case_when(
      TRT == "Study Drug" ~ VISIT,
      TRT == "Placebo" ~ VISIT + 0.15,
      TRT == "AC" ~ VISIT + 0.30
    ),
    VIS = ifelse(VISIT %in% c(4, 5), VIS - 0.15, VIS),
    VIS = ifelse(VISIT == 6, VIS - 0.3, VIS),
    VIS = round(VIS, 10)
  )
xx <- xx[order(xx$VIS), ]   # stable sort, like PROC SORT

write.csv(xx, file.path(out, "xx.csv"), row.names = FALSE, na = "")

# ---- Box statistics (SAS PCTLDEF=5 == R type 2), schematic whiskers ---------
box_stats <- xx %>%
  group_by(VIS, TRT) %>%
  summarise(
    N = n(),
    MEAN = mean(X),
    MIN = min(X), MAX = max(X),
    Q1 = unname(quantile(X, 0.25, type = 2)),
    MEDIAN = unname(quantile(X, 0.50, type = 2)),
    Q3 = unname(quantile(X, 0.75, type = 2)),
    .groups = "drop"
  ) %>%
  mutate(IQR = Q3 - Q1, LFENCE = Q1 - 1.5 * IQR, UFENCE = Q3 + 1.5 * IQR)

wh <- xx %>%
  inner_join(box_stats %>% select(VIS, TRT, LFENCE, UFENCE), by = c("VIS", "TRT")) %>%
  group_by(VIS, TRT) %>%
  summarise(LOWWHISK = min(X[X >= LFENCE]), HIGHWHISK = max(X[X <= UFENCE]), .groups = "drop")
box_stats <- box_stats %>% left_join(wh, by = c("VIS", "TRT"))

outliers <- xx %>%
  inner_join(box_stats %>% select(VIS, TRT, LFENCE, UFENCE), by = c("VIS", "TRT")) %>%
  filter(X < LFENCE | X > UFENCE)

box_stats$TRT <- factor(box_stats$TRT, levels = c("Study Drug", "Placebo", "AC"))
outliers$TRT <- factor(outliers$TRT, levels = c("Study Drug", "Placebo", "AC"))

write.csv(box_stats, file.path(out, "p031_f_plsr_box_plot.csv"), row.names = FALSE, na = "")
write.csv(box_stats, file.path(out, "p031_box_stats.csv"), row.names = FALSE, na = "")

# ---- Plot -------------------------------------------------------------------
w <- 0.12
p <- ggplot(box_stats) +
  geom_segment(aes(x = VIS, xend = VIS, y = LOWWHISK, yend = Q1), colour = "black") +
  geom_segment(aes(x = VIS, xend = VIS, y = Q3, yend = HIGHWHISK), colour = "black") +
  geom_segment(aes(x = VIS - w / 2, xend = VIS + w / 2, y = LOWWHISK, yend = LOWWHISK)) +
  geom_segment(aes(x = VIS - w / 2, xend = VIS + w / 2, y = HIGHWHISK, yend = HIGHWHISK)) +
  geom_rect(aes(xmin = VIS - w / 2, xmax = VIS + w / 2, ymin = Q1, ymax = Q3),
            fill = NA, colour = "blue") +
  geom_segment(aes(x = VIS - w / 2, xend = VIS + w / 2, y = MEDIAN, yend = MEDIAN), colour = "blue") +
  geom_point(aes(x = VIS, y = MEAN, shape = TRT, colour = TRT), size = 2) +
  scale_shape_manual(values = c("Study Drug" = 3, "Placebo" = 2, "AC" = 0)) +
  scale_colour_manual(values = c("Study Drug" = "black", "Placebo" = "blue", "AC" = "red")) +
  scale_x_continuous(breaks = 3:6, labels = sprintf("%4d", 3:6), limits = c(2.7, 6.3)) +
  labs(title = "Box Plot of X over Time by Treatment", x = "Visit", y = "X",
       shape = "TRT", colour = "TRT") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
if (nrow(outliers) > 0) {
  p <- p + geom_point(data = outliers, aes(x = VIS, y = X), shape = 8, colour = "red",
                      inherit.aes = FALSE)
}

ggsave(file.path(out, "p031_f_plsr_box_plot.png"), p, width = 9, height = 7, dpi = 100)

# Plain-text summary of the plotted statistics
txt <- c("Box Plot of X over Time by Treatment", "",
         sprintf("%-6s %-10s %3s %7s %7s %7s %7s %7s %7s",
                 "VIS", "TRT", "N", "MEAN", "MIN", "Q1", "MEDIAN", "Q3", "MAX"),
         sprintf("%-6.2f %-10s %3d %7.2f %7.1f %7.2f %7.2f %7.2f %7.1f",
                 box_stats$VIS, as.character(box_stats$TRT), box_stats$N, box_stats$MEAN,
                 box_stats$MIN, box_stats$Q1, box_stats$MEDIAN, box_stats$Q3, box_stats$MAX))
writeLines(txt, file.path(out, "p031_f_plsr_box_plot.txt"))

cat("P031 done\n")
```