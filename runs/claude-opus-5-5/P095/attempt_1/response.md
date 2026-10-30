```r
# P095 - Waterfall chart of tumor size change (SAS SGPLOT translation)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- SAS RANUNI emulation ----------------------------------------------------
# Multiplicative congruential generator: seed = seed * 397204094 mod (2^31 - 1)
# The value returned is seed / (2^31 - 1).
# Every RANUNI call in the step shares one stream, initialised by the first seed.
.m <- 2147483647
.a <- 397204094

# Computes (a * s) mod m exactly. The full product can exceed 2^53, so the
# multiplier is split into 16-bit pieces to keep every product exact in doubles.
mulmod <- function(a, s, m) {
  a_hi <- a %/% 65536
  a_lo <- a %% 65536
  t1 <- (a_hi * s) %% m
  t1 <- (t1 * 65536) %% m
  t2 <- (a_lo * s) %% m
  (t1 + t2) %% m
}

.rstate <- new.env()
.rstate$seed <- NA_real_
ranuni <- function(seed) {
  if (is.na(.rstate$seed)) .rstate$seed <- seed
  .rstate$seed <- mulmod(.a, .rstate$seed, .m)
  .rstate$seed / .m
}

# ---- Build dummy data (DATA step) -------------------------------------------
groupArr <- c("Treatment 2", "Treatment 1")
labelArr <- c("E", "C", "R", "S", "P")
rows <- vector("list", 25)
for (i in 1:25) {
  Cid    <- formatC(i, width = 2)            # put(i, 2.0): right aligned
  Change <- 30 - 120 * ranuni(2)
  Group  <- groupArr[floor(ranuni(2) + 0.5) + 1]
  Label  <- labelArr[(i %% 5) + 1]
  rows[[i]] <- data.frame(Cid = Cid, Change = Change, Group = Group,
                          Label = Label, stringsAsFactors = FALSE)
}
TumorSize <- bind_rows(rows)

# ---- PROC SORT by descending change (stable sort) ---------------------------
TumorSizeDesc <- TumorSize[order(-TumorSize$Change, seq_len(nrow(TumorSize))), ]
rownames(TumorSizeDesc) <- NULL

write.csv(TumorSizeDesc, file.path(out, "tumorsize_band_sg_v94.csv"),
          row.names = FALSE, na = "")

# ---- Plot -------------------------------------------------------------------
pd <- TumorSizeDesc
pd$Cid <- factor(pd$Cid, levels = unique(pd$Cid))   # category axis in data order
grp_levels <- unique(pd$Group)                      # group order of first appearance
pd$Group <- factor(pd$Group, levels = grp_levels)
fill_cols <- setNames(c("#bf0000", "gold")[seq_along(grp_levels)], grp_levels)

# Data labels sit just outside the end of each bar
pd$lab_y <- ifelse(pd$Change >= 0, pd$Change + 3, pd$Change - 3)

inset_txt <- paste("BCR",
                   "C= Complete Response", "R= Partial Response",
                   "S= Stable Disease", "P= Progressive Disease",
                   "E= Early Death", sep = "\n")

p <- ggplot(pd) +
  annotate("rect", xmin = 0.5, xmax = nlevels(pd$Cid) + 0.5,
           ymin = -30, ymax = 20, fill = "grey60", alpha = 0.5) +
  geom_col(aes(x = Cid, y = Change, fill = Group),
           colour = "black", width = 0.8) +
  geom_text(aes(x = Cid, y = lab_y, label = Label),
            size = 5 / .pt * 1.2, fontface = "bold") +
  scale_fill_manual(values = fill_cols, name = NULL) +
  scale_y_continuous(breaks = seq(60, -100, by = -20),
                     limits = c(-100, 60),
                     name = "Change from Baseline (%)") +
  annotate("label", x = 0.6, y = -98, label = inset_txt,
           hjust = 0, vjust = 0, size = 6 / .pt, fontface = "bold",
           label.size = 0.3) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population", x = NULL) +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "#f0f0f0"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.direction = "vertical",
        plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p,
       width = 5, height = 3, dpi = 300, units = "in")

cat("P095 done\n")
```