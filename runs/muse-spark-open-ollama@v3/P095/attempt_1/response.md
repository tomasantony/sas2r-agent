```r
# P095 WaterFall translation - SAS ranuni(2) emulation + SGPlot recreation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS round half away from zero (not needed here but required by spec)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(x+0.5) for positive, -floor(|x|+0.5) for negative
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# SAS RANUNI emulation: prime modulus LCG with m=2^31-1, a=397204094
# SAS doc: seed_{n+1} = mod(a*seed_n, m), u = seed_{n+1}/m
sas_ranuni_next <- function(seed) {
  m <- 2147483647
  a <- 397204094
  # modular multiplication without overflow (a< m, seed < m, product up to ~8e17 > 2^53)
  mod_mult <- function(a, b, m) {
    res <- 0
    a <- a %% m
    while (b > 0) {
      if (b %% 2 == 1) res <- (res + a) %% m
      a <- (a * 2) %% m
      b <- b %/% 2
    }
    res
  }
  new_seed <- mod_mult(a, seed, m)
  u <- new_seed / m
  list(seed = new_seed, u = u)
}

library(ggplot2)

# Creating dummy data for graphing - replicate SAS data step
n <- 25
groupArr <- c("Treatment 2", "Treatment 1")  # grp1, grp2
labelArr <- c("E", "C", "R", "S", "P")        # l1-l5

Cid <- character(n)
Change <- numeric(n)
Group <- character(n)
Label <- character(n)

seed <- 2
for (i in 1:n) {
  # Cid = put(i, 2.0)  -> width 2, right aligned
  Cid[i] <- sprintf("%2d", i)

  # Change = 30-120*ranuni(2)
  r1 <- sas_ranuni_next(seed)
  seed <- r1$seed
  Change[i] <- 30 - 120 * r1$u

  # Group = groupArr{int(ranuni(2)+0.5)+1}
  r2 <- sas_ranuni_next(seed)
  seed <- r2$seed
  idx <- as.integer(floor(r2$u + 0.5)) + 1  # 1 or 2
  # SAS int() truncates toward 0 for positive = floor
  Group[i] <- groupArr[idx]

  # Label = labelArr{mod(i,5)+1}
  Label[i] <- labelArr[(i %% 5) + 1]
}

TumorSize <- data.frame(Cid = Cid, Change = Change, Group = Group, Label = Label,
                        stringsAsFactors = FALSE)

# proc sort descending change
ord <- order(-TumorSize$Change)
TumorSizeDesc <- TumorSize[ord, , drop = FALSE]
rownames(TumorSizeDesc) <- NULL

# verify columns exist
stopifnot(all(c("Cid","Change","Group","Label") %in% names(TumorSizeDesc)))

# write CSV of plotted data (plotted data = sorted data)
write.csv(TumorSizeDesc, file.path(out, "tumorsize_band_sg_v94.csv"),
          row.names = FALSE, na = "")

# Prepare factor for x in sorted order (SAS by descending change)
TumorSizeDesc$Cid_f <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)

# Colors from SAS styleattrs
col_t1 <- "#bf0000"  # cxbf0000
col_t2 <- "gold"     # gold = #FFD700
col_band <- "grey80"

# Build plot to match PROC SGPLOT
# Band: x=cid upper=20 lower=-30 transparency 0.5 fill noOutline legendLabel='Confidence'
# vBarParm category=cid response=change / group=group dataLabel=label groupDisplay=cluster
# xaxis display=none; yaxis values=(60 to -100 by -20) grid gridAttrs=(color=cxf0f0f0)
# inset BCR bottomLeft border; keyLegend inside topRight across=1 border opaque

# For legend we need Confidence to appear: use geom_rect with fill aesthetic
band_df <- data.frame(xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20, fill = "Confidence")

p <- ggplot() +
  geom_rect(data = band_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            alpha = 0.5, colour = NA, inherit.aes = FALSE) +
  geom_col(data = TumorSizeDesc,
           aes(x = Cid_f, y = Change, fill = Group),
           width = 0.75, colour = "black", linewidth = 0.25) +
  geom_text(data = TumorSizeDesc,
            aes(x = Cid_f, y = Change, label = Label),
            vjust = ifelse(TumorSizeDesc$Change >= 0, -0.6, 1.4),
            size = 1.8, fontface = "bold", colour = "black") +
  scale_fill_manual(
    values = c("Confidence" = col_band, "Treatment 1" = col_t1, "Treatment 2" = col_t2),
    breaks = c("Confidence", "Treatment 1", "Treatment 2"),
    name = ""
  ) +
  scale_y_continuous(breaks = seq(60, -100, by = -20), limits = c(-100, 60), expand = c(0, 0)) +
  labs(title = "Change in Tumor Size",
       subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#f0f0f0", linewidth = 0.4),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    legend.position = c(0.97, 0.97),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = "black", linewidth = 0.3),
    legend.key.size = unit(0.35, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  ) +
  annotate("label",
           x = -Inf, y = -Inf, hjust = 0, vjust = 0,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           size = 1.9, fontface = "bold", label.size = 0.3,
           fill = "white", colour = "black",
           label.padding = unit(0.35, "lines"))

# Save figure: width=5in height=3in dpi=300 imagename='TumorSize_Band_SG_V94'
ggsave(filename = file.path(out, "tumorsize_band_sg_v94.png"),
       plot = p, width = 5, height = 3, units = "in", dpi = 300, bg = "white")

# Also write a txt rendering for completeness
txt_path <- file.path(out, "tumorsize_band_sg_v94.txt")
sink(txt_path)
cat("Change in Tumor Size\n")
cat("ITT Population\n\n")
cat("Band: upper=20 lower=-30 transparency 0.5 legendLabel=Confidence\n")
cat("Y axis: 60 to -100 by -20 grid color #f0f0f0\n")
cat("X axis: display=none (Cid sorted descending Change)\n\n")
print(TumorSizeDesc[, c("Cid","Change","Group","Label")], row.names = FALSE)
sink()

cat("Done. Outputs written to", out, "\n")
```