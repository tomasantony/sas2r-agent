# P095 WaterFall translation - R script
# Reproduces SAS program P095_WaterFall_94.sas

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not needed here but kept for fidelity)
round_sas <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(x+0.5) for positive
  rounded <- floor(abs_x + 0.5 + 1e-12)
  sign_x * rounded / mult
}

# Helper: modular multiplication without overflow (a*b) %% m
mod_mult <- function(a, b, m) {
  result <- 0
  a <- a %% m
  b <- floor(b)
  while (b > 0) {
    if (b %% 2 == 1) {
      result <- (result + a) %% m
    }
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  result
}

# SAS RANUNI implementation: modulus 2^31-1, multiplier 397204094
sas_ranuni_seq <- function(init_seed, n) {
  m <- 2147483647
  mult <- 397204094
  cur <- init_seed
  out <- numeric(n)
  for (i in seq_len(n)) {
    cur <- mod_mult(mult, cur, m)
    out[i] <- cur / m
  }
  out
}

# Creating dummy data for graphing - replicate SAS DATA step
# SAS: do i=1 to 25; Cid=put(i,2.0); Change=30-120*ranuni(2); Group=groupArr{int(ranuni(2)+0.5)+1}; Label=labelArr{mod(i,5)+1}
set.seed(1) # not used, SAS RNG is used instead

n <- 25
# 2 ranuni calls per iteration => 50 uniforms
u <- sas_ranuni_seq(init_seed = 2, n = 2 * n)

groupArr <- c("Treatment 2", "Treatment 1")
labelArr <- c("E", "C", "R", "S", "P") # l1-l5

TumorSize <- data.frame(
  Cid = character(n),
  Change = numeric(n),
  Group = character(n),
  Label = character(n),
  stringsAsFactors = FALSE
)

for (i in 1:n) {
  cid_val <- sprintf("%2d", i) # SAS put(i,2.0) right-aligned width 2
  change_val <- 30 - 120 * u[2 * i - 1]
  grp_idx <- as.integer(floor(u[2 * i] + 0.5)) + 1 # int(ranuni+0.5)+1 ; SAS INT = floor for positive
  # grp_idx is 1 or 2
  group_val <- groupArr[grp_idx]
  # SAS: labelArr{mod(i,5)+1} ; SAS MOD
  mod_val <- i %% 5
  label_val <- labelArr[mod_val + 1]
  TumorSize$Cid[i] <- cid_val
  TumorSize$Change[i] <- change_val
  TumorSize$Group[i] <- group_val
  TumorSize$Label[i] <- label_val
}

# proc sort descending change
TumorSizeDesc <- TumorSize[order(-TumorSize$Change), , drop = FALSE]
# reset rownames
rownames(TumorSizeDesc) <- NULL

# Write CSV of plotted data (SAS dataset TumorSizeDesc)
# Keep variable names as in SAS: Cid, Change, Group, Label
# Dates not present
write.csv(TumorSizeDesc, file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")

# Also write a txt rendering of the table body for completeness
txt_path <- file.path(out, "tumorsize_band_sg_v94.txt")
con <- file(txt_path, open = "wt")
writeLines("Change in Tumor Size", con)
writeLines("ITT Population", con)
writeLines("", con)
# header
writeLines(sprintf("%-4s %12s %-12s %5s", "Cid", "Change", "Group", "Label"), con)
writeLines(paste(rep("-", 40), collapse = ""), con)
for (r in seq_len(nrow(TumorSizeDesc))) {
  writeLines(sprintf("%-4s %12.4f %-12s %5s",
                     TumorSizeDesc$Cid[r],
                     TumorSizeDesc$Change[r],
                     TumorSizeDesc$Group[r],
                     TumorSizeDesc$Label[r]), con)
}
close(con)

# Plot - reproduce SGPlot
# Libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# Factor for x ordering (already sorted descending)
TumorSizeDesc$Cid_f <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)

# For data label positioning: if Change >=0 label above bar, else below
TumorSizeDesc$vjust <- ifelse(TumorSizeDesc$Change >= 0, -0.5, 1.3)
TumorSizeDesc$label_y <- TumorSizeDesc$Change # will use vjust to offset

# Band data for legend
# Use scale_fill_manual to include Confidence

p <- ggplot() +
  # Band x=cid upper=20 lower=-30 transparency 0.5 fill noOutline legendLabel='Confidence'
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20, fill = "Confidence"),
            alpha = 0.5, colour = NA, show.legend = TRUE) +
  geom_col(data = TumorSizeDesc,
           aes(x = Cid_f, y = Change, fill = Group),
           position = position_dodge(width = 0.9),
           width = 0.8,
           colour = "black") +
  geom_text(data = TumorSizeDesc,
            aes(x = Cid_f, y = Change, label = Label),
            size = 2.5, fontface = "bold",
            vjust = TumorSizeDesc$vjust) +
  scale_fill_manual(
    name = "",
    values = c("Treatment 2" = "#bf0000", "Treatment 1" = "gold", "Confidence" = "grey70"),
    breaks = c("Treatment 2", "Treatment 1", "Confidence")
  ) +
  scale_y_continuous(
    limits = c(-100, 60),
    breaks = seq(60, -100, by = -20),
    labels = seq(60, -100, by = -20)
  ) +
  labs(
    title = "Change in Tumor Size",
    subtitle = "ITT Population",
    x = NULL,
    y = "Change from Baseline (%)"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    panel.grid.major.y = element_line(color = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.88, 0.88),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_blank()
  ) +
  # Inset BCR box bottomLeft
  annotate("label",
           x = 1, y = -85,
           label = "C= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           hjust = 0, vjust = 0,
           size = 2, fontface = "bold",
           label.size = 0.3,
           fill = "white", colour = "black") +
  annotate("text",
           x = 1, y = -62,
           label = "BCR",
           hjust = 0, vjust = 0,
           size = 2.5, fontface = "bold")

# Save PNG 5in x 3in 300dpi
ggsave(filename = file.path(out, "tumorsize_band_sg_v94.png"),
       plot = p,
       width = 5, height = 3, units = "in", dpi = 300, bg = "white")

# Also ensure CSV of plotted data already written
