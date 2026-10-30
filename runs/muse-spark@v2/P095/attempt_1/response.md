```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(ggplot2))

# ---- SAS RANUNI(2) replication ----
# Fishman-Moore prime modulus multiplicative generator:
# modulus m = 2^31-1, multiplier a = 397204094
# seed_n = (a * seed_{n-1}) mod m ; u_n = seed_n / m
mod_mult <- function(a, b, m) {
  res <- 0
  aa <- a %% m
  bb <- b
  while (bb > 0) {
    if (bb %% 2 == 1) res <- (res + aa) %% m
    aa <- (aa * 2) %% m
    bb <- bb %/% 2
  }
  res
}

m <- 2147483647
a <- 397204094
seed <- 2
u <- numeric(50)
for (k in 1:50) {
  seed <- mod_mult(a, seed, m)
  u[k] <- seed / m
}

# ---- Creating dummy data for graphing (DATA step TumorSize) ----
# Cid = put(i, 2.0); Change = 30-120*ranuni(2);
# Group = groupArr{int(ranuni(2)+0.5)+1}; groupArr = c('Treatment 2','Treatment 1')
# Label = labelArr{mod(i,5)+1}; labelArr = c('E','C','R','S','P')
labelArr <- c("E", "C", "R", "S", "P")
n <- 25
Cid <- character(n)
Change <- numeric(n)
Group <- character(n)
Label <- character(n)
for (i in 1:n) {
  Cid[i] <- sprintf("%2d", i)
  u_change <- u[2 * i - 1]
  u_group  <- u[2 * i]
  Change[i] <- 30 - 120 * u_change
  idx <- as.integer(u_group + 0.5) + 1L
  if (idx == 1L) Group[i] <- "Treatment 2" else Group[i] <- "Treatment 1"
  Label[i] <- labelArr[(i %% 5) + 1L]
}
TumorSize <- data.frame(Cid = Cid, Change = Change, Group = Group, Label = Label,
                        stringsAsFactors = FALSE)

# ---- PROC SORT by descending change ----
ord <- order(-TumorSize$Change)
TumorSizeDesc <- TumorSize[ord, , drop = FALSE]
rownames(TumorSizeDesc) <- NULL

# factor ordered by sorted order for waterfall
TumorSizeDesc$Cid_factor <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)
# label position just outside bar end
TumorSizeDesc$label_y <- TumorSizeDesc$Change + ifelse(TumorSizeDesc$Change >= 0, 2.5, -2.5)

# ---- CSV of plotted data ----
plot_data_csv <- data.frame(Cid = TumorSizeDesc$Cid,
                            Change = TumorSizeDesc$Change,
                            Group = TumorSizeDesc$Group,
                            Label = TumorSizeDesc$Label,
                            stringsAsFactors = FALSE)
write.csv(plot_data_csv, file.path(out, "tumorsize_band_sg_v94.csv"),
          row.names = FALSE, na = "")

# ---- SGPLOT waterfall ----
band_df <- data.frame(xmin = 0.5, xmax = 25.5, ymin = -30, ymax = 20,
                      fill = "Confidence", stringsAsFactors = FALSE)

p <- ggplot() +
  geom_rect(data = band_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            alpha = 0.5, colour = NA, show.legend = TRUE) +
  geom_col(data = TumorSizeDesc,
           aes(x = Cid_factor, y = Change, fill = Group),
           colour = "black", width = 0.7) +
  geom_text(data = TumorSizeDesc,
            aes(x = Cid_factor, y = label_y, label = Label),
            size = 2.5, fontface = "bold") +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(name = "",
                    values = c("Confidence" = "#BFBFBF",
                               "Treatment 1" = "gold",
                               "Treatment 2" = "#BF0000"),
                    breaks = c("Confidence", "Treatment 2", "Treatment 1")) +
  labs(title = "Change in Tumor Size",
       subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(colour = "#F0F0F0"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = c(0.96, 0.96),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white", colour = NA)
  ) +
  annotate("rect", xmin = 0.6, xmax = 6.4, ymin = -100, ymax = -57,
           fill = "white", colour = "black", linewidth = 0.5) +
  annotate("text", x = 3.5, y = -61, label = "BCR", size = 2.8, fontface = "bold") +
  annotate("text", x = 0.8, y = -66.5, label = "C= Complete Response", size = 2, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.8, y = -73.5, label = "R= Partial Response", size = 2, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.8, y = -80.5, label = "S= Stable Disease", size = 2, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.8, y = -87.5, label = "P= Progressive Disease", size = 2, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.8, y = -94.5, label = "E= Early Death", size = 2, fontface = "bold", hjust = 0)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), plot = p,
       width = 5, height = 3, dpi = 300, units = "in")
```