out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
})

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# ---- Emulate SAS RANUNI(seed) with Fishman-Moore generator ----
# modulus 2^31-1, multiplier 397204094
mod_mult <- function(a, b, m) {
  a <- a %% m
  b <- floor(b)
  res <- 0
  while (b > 0) {
    if ((b %% 2) == 1) res <- (res + a) %% m
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  res
}

m_mod <- 2147483647
mult  <- 397204094
seed  <- 2
n_calls <- 50L
U <- numeric(n_calls)
for (k in seq_len(n_calls)) {
  seed <- mod_mult(mult, seed, m_mod)
  U[k] <- seed / m_mod
}

# ---- Creating dummy data for graphing (data TumorSize) ----
# array groupArr grp1-grp2 ('Treatment 2','Treatment 1')
# array labelArr l1-l5 ('E','C','R','S','P')
# do i=1 to 25: Cid=put(i,2.0); Change=30-120*ranuni(2);
# Group=groupArr{int(ranuni(2)+0.5)+1}; Label=labelArr{mod(i,5)+1}
labelArr <- c("E", "C", "R", "S", "P")
n <- 25L
Cid <- character(n)
Change <- numeric(n)
Group <- character(n)
Label <- character(n)
for (i in seq_len(n)) {
  u_change <- U[(2L * i - 1L)]
  u_group  <- U[(2L * i)]
  Cid[i] <- sprintf("%2d", i)  # put(i, 2.0)
  Change[i] <- 30 - 120 * u_change
  idx_g <- as.integer(floor(u_group + 0.5)) + 1L  # int() truncates for positive
  if (idx_g == 1L) {
    Group[i] <- "Treatment 2"
  } else {
    Group[i] <- "Treatment 1"
  }
  idx_l <- (i %% 5L) + 1L  # mod(i,5)+1 ; SAS mod same as R for positive
  Label[i] <- labelArr[idx_l]
}
TumorSize <- data.frame(Cid = Cid, Change = Change, Group = Group, Label = Label,
                        stringsAsFactors = FALSE)
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

# ---- proc sort by descending change ----
ord <- order(-TumorSize$Change)
TumorSizeDesc <- TumorSize[ord, , drop = FALSE]
rownames(TumorSizeDesc) <- NULL

# CSV of plotted data
write.csv(TumorSizeDesc, file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")

# ---- sgPlot waterfall ----
# order x by descending change (sorted order)
TumorSizeDesc$Cid_f <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)
# label position just outside bar end
TumorSizeDesc$label_y <- ifelse(TumorSizeDesc$Change >= 0,
                                TumorSizeDesc$Change + 2.5,
                                TumorSizeDesc$Change - 3.5)

bcr_text <- "C= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death"

p <- ggplot(TumorSizeDesc, aes(x = Cid_f, y = Change, fill = Group)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20),
            fill = "grey75", alpha = 0.5, inherit.aes = FALSE, color = NA) +
  geom_col(color = "black", linewidth = 0.4, width = 0.75,
           position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = Label), size = 1.8, fontface = "bold",
            position = position_dodge(width = 0.9), show.legend = FALSE) +
  scale_y_continuous(limits = c(-100, 62), breaks = seq(-100, 60, by = 20)) +
  scale_fill_manual(values = c("Treatment 1" = "#BF0000", "Treatment 2" = "gold"),
                    breaks = c("Treatment 1", "Treatment 2")) +
  labs(title = "Change in Tumor Size",
       subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = NULL) +
  theme_bw(base_size = 9) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F0F0F0", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = c(0.88, 0.82),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 7),
    legend.margin = margin(3, 5, 3, 5)
  ) +
  annotate("label", x = 4.5, y = -68,
           label = bcr_text,
           hjust = 0, vjust = 0.5,
           size = 2.2, fontface = "bold",
           fill = "white", color = "black",
           label.size = 0.5, lineheight = 1.1) +
  annotate("text", x = 4.5, y = -52.5, label = "BCR",
           hjust = 0, size = 2.8, fontface = "bold")

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), plot = p,
       width = 5, height = 3, dpi = 300, units = "in")
