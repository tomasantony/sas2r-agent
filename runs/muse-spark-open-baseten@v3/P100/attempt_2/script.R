out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_round <- function(x, d = 0) {
  p <- 10^d
  sign(x) * floor(abs(x) * p + 0.5) / p
}

# Accurate SAS RANUNI (Fishman & Moore) without overflow
# modulus m = 2^31-1, multiplier a = 397204094
# Use Russian peasant modular multiplication to avoid double overflow
mod_mul <- function(a, s, m) {
  res <- 0
  a_mod <- a %% m
  while (s > 0) {
    if (s %% 2 == 1) {
      res <- (res + a_mod) %% m
    }
    a_mod <- (a_mod * 2) %% m
    s <- s %/% 2
  }
  res
}

sas_ranuni_seq <- function(seed, n) {
  m <- 2147483647
  a <- 397204094
  s <- as.numeric(seed)
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- mod_mul(a, s, m)
    u[i] <- s / m
  }
  list(u = u, seed = s)
}

n_id <- 25
seq50 <- sas_ranuni_seq(2, n_id * 2)
u <- seq50$u

Id <- 1:n_id
Cid <- sprintf("%2d", Id)
Change <- numeric(n_id)
Group <- character(n_id)
Label <- character(n_id)

for (i in seq_len(n_id)) {
  u_change <- u[(i - 1) * 2 + 1]
  u_group  <- u[(i - 1) * 2 + 2]
  Change[i] <- 30 - 120 * u_change
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

write.csv(TumorSize, file.path(out, "TumorSize.csv"), row.names = FALSE, na = "")

TumorSizeDesc <- TumorSize[order(-TumorSize$Change), ]
write.csv(TumorSizeDesc, file.path(out, "TumorSizeDesc.csv"), row.names = FALSE, na = "")

# PROC PRINT obs=5 -> P100.lst and sashtml.htm
print5 <- TumorSize[1:5, ]

write.csv(print5, file.path(out, "P100.csv"), row.names = FALSE, na = "")
write.csv(print5, file.path(out, "sashtml.csv"), row.names = FALSE, na = "")

hdr <- sprintf("%-4s %-3s %-4s %-12s %-13s %-5s", "Obs", "Id", "Cid", "Change", "Group", "Label")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
body_lines <- c(hdr, sep)
for (i in seq_len(nrow(print5))) {
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

writeLines(txt_content, file.path(out, "P100.txt"))
writeLines(txt_content, file.path(out, "P100.lst"))
writeLines(txt_content, file.path(out, "sashtml.txt"))
html_lines <- c(
  "<html><head><title>P100 - TumorSize (obs=5)</title></head><body>",
  "<h2>Change in Tumor Size (PROC PRINT obs=5)</h2>",
  "<pre>",
  txt_content,
  "</pre>",
  "</body></html>"
)
writeLines(html_lines, file.path(out, "sashtml.htm"))

# Figure 1: TumorSize_SG_V94
TumorSize$Cid_ord <- factor(TumorSize$Cid, levels = TumorSize$Cid[order(-TumorSize$Change)])
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
ggsave(file.path(out, "TumorSize_SG_V94.png"), p1, width = 5, height = 3, dpi = 300, units = "in")
write.csv(TumorSize, file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")

# Figure 2: TumorSize_Band_SG_V94
TumorSizeDesc$Cid_ord2 <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)
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
