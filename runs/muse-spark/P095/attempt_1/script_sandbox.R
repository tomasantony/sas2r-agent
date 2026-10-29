out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
})

# SAS round half away from zero (helper, not strictly needed here)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# SAS RANUNI(seed) replication: prime modulus multiplicative generator
# modulus m = 2^31-1, multiplier a = 397204094 (Fishman & Moore 1982)
sas_ranuni_seq <- function(seed, n) {
  m <- 2147483647
  a_high <- 6060
  a_low <- 55934
  s <- as.numeric(seed)
  u <- numeric(n)
  for (k in seq_len(n)) {
    sh <- floor(s / 65536)
    sl <- s - sh * 65536
    t1 <- a_high * sh * 2
    t_mid <- a_high * sl + a_low * sh
    t_mid_mod <- t_mid %% m
    t2 <- (t_mid_mod * 65536) %% m
    t3 <- (a_low * sl) %% m
    s <- (t1 + t2 + t3) %% m
    u[k] <- s / m
  }
  u
}

# Creating dummy data for graphing (SAS data step TumorSize)
n <- 25L
uvec <- sas_ranuni_seq(2, 2L * n)
u_change <- uvec[seq(1, 2L * n, by = 2L)]
u_group  <- uvec[seq(2, 2L * n, by = 2L)]

Change <- 30 - 120 * u_change
# Group = groupArr{int(ranuni+0.5)+1}; groupArr[1]='Treatment 2', [2]='Treatment 1'
gidx <- floor(u_group + 0.5) + 1L
Group <- ifelse(gidx == 1L, "Treatment 2", "Treatment 1")

# labelArr l1-l5 = ('E','C','R','S','P'); Label = labelArr{mod(i,5)+1}
labelArr <- c("E", "C", "R", "S", "P")
Label <- character(n)
Cid <- character(n)
for (i in seq_len(n)) {
  # SAS put(i,2.0) : width 2 right-aligned
  Cid[i] <- sprintf("%2d", i)
  Label[i] <- labelArr[(i %% 5L) + 1L]
}

TumorSize <- data.frame(Cid = Cid, Change = Change, Group = Group, Label = Label,
                        stringsAsFactors = FALSE)
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

# proc sort by descending change (SAS missing first; no missings here)
TumorSizeDesc <- TumorSize[order(-TumorSize$Change), , drop = FALSE]
row.names(TumorSizeDesc) <- NULL

# CSV of plotted data
write.csv(TumorSizeDesc, file.path(out, "tumorsize_band_sg_v94.csv"),
          row.names = FALSE, na = "")

# Plot: SGPLOT waterfall reproduction
# x order = sorted Cid order
TumorSizeDesc$CidF <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)
# label position just outside bar end
TumorSizeDesc$LabY <- TumorSizeDesc$Change + ifelse(TumorSizeDesc$Change >= 0, 2.5, -2.5)

p <- ggplot(TumorSizeDesc, aes(x = CidF, y = Change, fill = Group)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20),
            fill = "grey80", alpha = 0.5, color = NA, inherit.aes = FALSE) +
  geom_col(color = "black", width = 0.7, position = position_dodge(width = 0.8)) +
  geom_text(aes(y = LabY, label = Label), size = 2.2, fontface = "bold",
            position = position_dodge(width = 0.8), show.legend = FALSE) +
  scale_fill_manual(values = c("Treatment 1" = "#BF0000", "Treatment 2" = "gold"),
                    breaks = c("Treatment 1", "Treatment 2")) +
  scale_y_continuous(breaks = seq(60, -100, by = -20), limits = c(-100, 62),
                     expand = c(0, 0)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = NULL) +
  theme_bw(base_size = 9) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F0F0F0"),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = c(0.88, 0.82),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.key = element_rect(color = NA),
    legend.text = element_text(size = 7),
    legend.margin = margin(2, 4, 2, 4)
  ) +
  annotate("rect", xmin = 0.5, xmax = 6.5, ymin = -100, ymax = -68,
           fill = "white", color = "black", linewidth = 0.4) +
  annotate("text", x = 0.7, y = -71,
           label = "BCR", size = 2.8, fontface = "bold", hjust = 0, vjust = 1) +
  annotate("text", x = 0.7, y = -75.5,
           label = "C= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           size = 2.2, fontface = "bold", hjust = 0, vjust = 1, lineheight = 1.1)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), plot = p,
       width = 5, height = 3, dpi = 300, units = "in")
