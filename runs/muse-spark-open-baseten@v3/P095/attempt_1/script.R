# P095 WaterFall translation - SAS ranuni replication and SGPlot recreation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# Helper: SAS RANUNI (Fishman-Moore) with modulus 2^31-1 and multiplier 397204094
# Avoid overflow using modular multiplication via addition
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

sas_ranuni_seq <- function(n, seed_init = 2) {
  m <- 2147483647
  a <- 397204094
  seed <- seed_init
  out <- numeric(n)
  for (i in seq_len(n)) {
    seed <- mod_mult(seed, a, m)
    out[i] <- seed / m
  }
  out
}

# Generate dummy data exactly as SAS data step
# SAS: do i=1 to 25; Cid=put(i,2.); Change=30-120*ranuni(2);
#      Group=groupArr{int(ranuni(2)+0.5)+1}; Label=labelArr{mod(i,5)+1}
n <- 25
# 2 calls per iteration => 50 uniforms in order: Change1, Group1, Change2, Group2, ...
u <- sas_ranuni_seq(n * 2, seed_init = 2)
# u[1]=Change1, u[2]=Group1, u[3]=Change2, u[4]=Group2, ...

groupArr <- c("Treatment 2", "Treatment 1")
labelArr <- c("E", "C", "R", "S", "P")  # l1-l5

TumorSize <- data.frame(
  Cid = character(n),
  Change = numeric(n),
  Group = character(n),
  Label = character(n),
  stringsAsFactors = FALSE
)

for (i in 1:n) {
  # SAS put(i,2.) => width 2, right aligned, space padded
  Cid <- sprintf("%2d", i)
  # Change uses odd-indexed uniform: 1,3,5,...
  ran_change <- u[(i - 1) * 2 + 1]
  Change <- 30 - 120 * ran_change
  # Group uses even-indexed uniform: 2,4,6,...
  ran_group <- u[(i - 1) * 2 + 2]
  # SAS int(ran+0.5) = floor(ran+0.5) for positive
  idx <- floor(ran_group + 0.5) + 1  # 1 or 2
  Group <- groupArr[idx]
  # SAS mod(i,5) +1 ; SAS mod matches R %% for positive
  lab_idx <- (i %% 5) + 1
  Label <- labelArr[lab_idx]
  TumorSize$Cid[i] <- Cid
  TumorSize$Change[i] <- Change
  TumorSize$Group[i] <- Group
  TumorSize$Label[i] <- Label
}

# Sort descending Change (SAS: proc sort by descending change)
TumorSizeDesc <- TumorSize[order(-TumorSize$Change), , drop = FALSE]
# Preserve order for x-axis: factor levels in sorted order
TumorSizeDesc$Cid <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)
# Also keep original Cid character for CSV (trim? keep as SAS put value)
# For plotting we use factor, for CSV we output trimmed? SAS Cid has leading space for 1-9.
# Write datasets as CSV (ISO dates not needed here)
write.csv(TumorSize, file.path(out, "tumorsize.csv"), row.names = FALSE, na = "")
write.csv(TumorSizeDesc, file.path(out, "tumorsize_desc.csv"), row.names = FALSE, na = "")

# CSV of plotted data (required counterpart for figure)
# Use character Cid as in SAS (with leading space) and numeric Change
plot_data_csv <- data.frame(
  Cid = as.character(TumorSizeDesc$Cid),
  Change = TumorSizeDesc$Change,
  Group = TumorSizeDesc$Group,
  Label = TumorSizeDesc$Label,
  stringsAsFactors = FALSE
)
write.csv(plot_data_csv, file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")

# Plot recreation with ggplot2
library(ggplot2)

# Colors: dataColors=(cxbf0000 gold) => Treatment 2 = #bf0000, Treatment 1 = gold
fill_vals <- c("Treatment 1" = "gold", "Treatment 2" = "#bf0000")

# Y breaks as SAS: values=(60 to -100 by -20)
y_breaks <- seq(60, -100, by = -20)

p <- ggplot(TumorSizeDesc, aes(x = Cid, y = Change, fill = Group)) +
  # Band x=cid upper=20 lower=-30 transparency 0.5 fill noOutline
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20,
           fill = "grey80", alpha = 0.5, colour = NA) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8, colour = "black", linewidth = 0.2) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.9),
            vjust = ifelse(TumorSizeDesc$Change >= 0, -0.5, 1.5),
            size = 1.8, fontface = "bold") +
  scale_fill_manual(values = fill_vals, name = "") +
  scale_y_continuous(breaks = y_breaks, limits = c(-100, 60), expand = c(0, 0)) +
  labs(title = "Change in Tumor Size",
       subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(colour = "#f0f0f0"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    legend.position = c(0.92, 0.92),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 6),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  # Inset BCR bottomLeft
  annotate("label",
           x = 1, y = -95,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           hjust = 0, vjust = 0,
           size = 1.8, fontface = "bold",
           label.size = 0.3, fill = "white", colour = "black",
           lineheight = 0.9, alpha = 1)

# Save figure 5in x 3in at 300 dpi (1500x900)
ggsave(filename = file.path(out, "tumorsize_band_sg_v94.png"),
       plot = p, width = 5, height = 3, units = "in", dpi = 300)

# Also save with original case name for compatibility
ggsave(filename = file.path(out, "TumorSize_Band_SG_V94.png"),
       plot = p, width = 5, height = 3, units = "in", dpi = 300)

# Plain-text rendering of table body for verification
txt_path <- file.path(out, "tumorsize_band_sg_v94.txt")
sink(txt_path)
cat("Change in Tumor Size\n")
cat("ITT Population\n\n")
cat("TumorSizeDesc sorted by descending Change\n")
print(plot_data_csv, row.names = FALSE)
cat("\nBand: upper=20 lower=-30 transparency 0.5\n")
cat("Y axis values=(60 to -100 by -20) grid color f0f0f0\n")
sink()
