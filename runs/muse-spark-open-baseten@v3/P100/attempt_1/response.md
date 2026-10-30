```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed for this program but defined)
sas_round <- function(x, d = 0) {
  p <- 10^d
  sign(x) * floor(abs(x) * p + 0.5) / p
}

# SAS RANUNI with seed 2 : Fishman & Moore multiplier 397204094, modulus 2^31-1
sas_ranuni_seq <- function(seed, n) {
  m <- 2147483647
  a <- 397204094
  s <- as.numeric(seed)
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (a * s) %% m
    u[i] <- s / m
  }
  list(u = u, seed = s)
}

# Generate TumorSize exactly as SAS data step
n_id <- 25
# 2 calls per iteration: change, group
seq50 <- sas_ranuni_seq(2, n_id * 2)
u <- seq50$u

Id <- 1:n_id
Cid <- sprintf("%2d", Id)  # put(id,2.0)
Change <- numeric(n_id)
Group <- character(n_id)
Label <- character(n_id)

for (i in seq_len(n_id)) {
  u_change <- u[(i - 1) * 2 + 1]
  u_group  <- u[(i - 1) * 2 + 2]
  Change[i] <- 30 - 120 * u_change
  # int(ranuni+0.5) -> 1 if >=0.5 else 0 ; ifc(..., 'Treatment 1','Treatment 2')
  Group[i] <- ifelse(floor(u_group + 0.5) == 1, "Treatment 1", "Treatment 2")
  r <- i %% 5
  Label[i] <- ifelse(r == 1, "C",
              ifelse(r == 2, "R",
              ifelse(r == 3, "S",
              ifelse(r == 4, "P", "E"))))
}

TumorSize <- data.frame(
  Id = Id,
  Cid = Cid,
  Change = Change,
  Group = Group,
  Label = Label,
  stringsAsFactors = FALSE
)
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

# Save dataset as CSV (ADaM-like)
write.csv(TumorSize, file.path(out, "TumorSize.csv"), row.names = FALSE, na = "")

# Sorted descending for second figure
TumorSizeDesc <- TumorSize[order(-TumorSize$Change), ]
write.csv(TumorSizeDesc, file.path(out, "TumorSizeDesc.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT obs=5  -> P100.lst and sashtml.htm ----
# SAS prints Obs, Id, Cid, Change, Group, Label
print5 <- TumorSize[1:5, ]

# CSV of table body (both names)
write.csv(print5, file.path(out, "P100.csv"), row.names = FALSE, na = "")
write.csv(print5, file.path(out, "sashtml.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT / ODS HTML listing
# Build a fixed-width text table
hdr <- sprintf("%-4s %-3s %-4s %-12s %-13s %-5s", "Obs", "Id", "Cid", "Change", "Group", "Label")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
body_lines <- c(hdr, sep)
for (i in seq_len(nrow(print5))) {
  # SAS default BEST12. for numeric: we reproduce with enough decimals
  # Use 4 decimals as SAS default for display? Keep full value but format similar to SAS listing
  # SAS prints Change with default BEST12. -> we format with 4 decimals for readability
  line <- sprintf("%-4d %-3d %-4s %-12.4f %-13s %-5s",
                  i, print5$Id[i], print5$Cid[i], print5$Change[i], print5$Group[i], print5$Label[i])
  body_lines <- c(body_lines, line)
}
txt_content <- c(
  "Change in Tumor Size (PROC PRINT obs=5)",
  "Data Set: TumorSize",
  "",
  body_lines,
  "",
  sprintf("N rows displayed: %d  (total rows in TumorSize: %d)", nrow(print5), nrow(TumorSize))
)

# Write both .txt renderings and also .lst / .htm names for exact expected output match
writeLines(txt_content, file.path(out, "P100.txt"))
writeLines(txt_content, file.path(out, "P100.lst"))
writeLines(txt_content, file.path(out, "sashtml.txt"))
# minimal html wrapper for sashtml.htm
html_lines <- c(
  "<html><head><title>P100 - TumorSize (obs=5)</title></head><body>",
  "<h2>Change in Tumor Size (PROC PRINT obs=5)</h2>",
  "<pre>",
  txt_content,
  "</pre>",
  "</body></html>"
)
writeLines(html_lines, file.path(out, "sashtml.htm"))

# ---- Figure 1: TumorSize_SG_V94 ----
# categoryorder=respdesc => order Cid by descending Change
TumorSize$Cid_ord <- factor(TumorSize$Cid, levels = TumorSize$Cid[order(-TumorSize$Change)])
# colors: cxbf0000 (red) and cxafafaf (grey)
cols1 <- c("Treatment 1" = "#bf0000", "Treatment 2" = "#afafaf")

library(ggplot2)

p1 <- ggplot(TumorSize, aes(x = Cid_ord, y = Change, fill = Group)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.9),
            vjust = ifelse(TumorSize$Change >= 0, -0.5, 1.2),
            size = 2.5, fontface = "bold") +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", colour = "black", linewidth = 0.5) +
  scale_fill_manual(values = cols1, name = "") +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.88, 0.88),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.key.size = unit(0.4, "cm")
  ) +
  annotate("label",
           x = 1, y = -95,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           hjust = 0, vjust = 0, size = 2.2, fontface = "bold",
           fill = "white", colour = "black", label.size = 0.3, lineheight = 0.9)

ggsave(file.path(out, "tumorsize_sg_v94.png"), p1, width = 5, height = 3, dpi = 300, units = "in")
# also exact case name
ggsave(file.path(out, "TumorSize_SG_V94.png"), p1, width = 5, height = 3, dpi = 300, units = "in")
write.csv(TumorSize, file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")

# ---- Figure 2: TumorSize_Band_SG_V94 ----
TumorSizeDesc$Cid_ord2 <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid) # already descending
cols2 <- c("Treatment 1" = "#bf0000", "Treatment 2" = "gold")

p2 <- ggplot(TumorSizeDesc, aes(x = Cid_ord2, y = Change, fill = Group)) +
  geom_rect(aes(xmin = as.numeric(Cid_ord2) - 0.5, xmax = as.numeric(Cid_ord2) + 0.5,
                ymin = -30, ymax = 20),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.9),
            vjust = ifelse(TumorSizeDesc$Change >= 0, -0.5, 1.2),
            size = 2.5, fontface = "bold") +
  scale_fill_manual(values = cols2, name = "") +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.88, 0.88),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.key.size = unit(0.4, "cm")
  ) +
  annotate("label",
           x = 1, y = -95,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           hjust = 0, vjust = 0, size = 2.2, fontface = "bold",
           fill = "white", colour = "black", label.size = 0.3, lineheight = 0.9)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p2, width = 5, height = 3, dpi = 300, units = "in")
ggsave(file.path(out, "TumorSize_Band_SG_V94.png"), p2, width = 5, height = 3, dpi = 300, units = "in")
write.csv(TumorSizeDesc, file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")
```