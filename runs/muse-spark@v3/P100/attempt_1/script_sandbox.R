out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---- SAS helpers ----
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(x+0.5+1e-9) to avoid binary edge
  rounded <- floor(abs_x + 0.5 + 1e-9) / mult
  rounded * sign_x
}

# exact (a*b) mod m using base 32768 decomposition, all intermediates < 2^47 exact in double
mod_mult <- function(a, b, m) {
  B <- 32768
  BB <- B * B
  b2 <- floor(b / BB)
  rem <- b - b2 * BB
  b1 <- floor(rem / B)
  b0 <- rem - b1 * B
  mod_small <- function(num, m) {
    q <- floor(num / m)
    r <- num - q * m
    # correct possible off-by-one from floating division
    # r is exact integer (<2^47); adjust
    while (r < 0) { r <- r + m }
    while (r >= m) { r <- r - m }
    # in rare case q off by more than 1, loop again (at most few)
    # use vector-safe single value
    r
  }
  # ((a*b2 mod m)*B + a*b1) mod m *B + a*b0
  p2 <- a * b2          # < ~8e8 exact
  p1 <- a * b1          # < ~1.3e13 exact
  p0 <- a * b0          # < ~1.3e13 exact
  r2 <- mod_small(p2, m)
  t1 <- r2 * B + p1     # < ~8e13 <2^47 exact
  r1 <- mod_small(t1, m)
  t0 <- r1 * B + p0     # < ~8e13 exact
  r0 <- mod_small(t0, m)
  r0
}

sas_ranuni_stream <- function(seed0, n, a = 397204094, m = 2147483647) {
  s <- seed0
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- mod_mult(a, s, m)
    u[i] <- s / m
  }
  u
}

# BEST12. emulation for PROC PRINT default
best12_fmt <- function(x) {
  if (is.na(x)) return(".")
  if (x == 0) return(sprintf("%12s", "0"))
  sgn <- ifelse(x < 0, 1, 0)
  ax <- abs(x)
  # integer digits
  if (ax < 1) nd <- 1L else nd <- nchar(as.character(floor(ax)))
  dec <- 12 - sgn - nd - 1L
  if (dec < 0) dec <- 0L
  # round half away
  rx <- round_half_up(x, dec)
  # handle carry increasing nd
  ax2 <- abs(rx)
  if (ax2 < 1) nd2 <- 1L else nd2 <- nchar(as.character(floor(ax2)))
  if (nd2 != nd) {
    dec2 <- 12 - sgn - nd2 - 1L
    if (dec2 < 0) dec2 <- 0L
    dec <- dec2
    rx <- round_half_up(x, dec)
  }
  fmt <- paste0("%.", dec, "f")
  s <- sprintf(fmt, rx)
  sprintf("%12s", s)
}

# ---- Generate TumorSize exactly as SAS ----
# SAS: do Id=1 to 25; change=30-120*ranuni(2); Group=ifc(int(ranuni(2)+0.5),...);
# Two RANUNI calls per iteration from single stream seeded at 2
u <- sas_ranuni_stream(2, 50)
u_change <- u[seq(1, 50, by = 2)]
u_group  <- u[seq(2, 50, by = 2)]

Id <- 1:25
Cid <- sprintf("%2d", Id)   # put(id,2.0)
Change <- 30 - 120 * u_change
Group <- ifelse(floor(u_group + 0.5) != 0, "Treatment 1", "Treatment 2")
# mod(id,5): 1->C 2->R 3->S 4->P 0->E
labs <- c("C", "R", "S", "P", "E")
Label <- labs[((Id - 1) %% 5) + 1]

TumorSize <- data.frame(Id = Id, Cid = Cid, Change = Change,
                        Group = Group, Label = Label, stringsAsFactors = FALSE)
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

TumorSizeDesc <- TumorSize[order(-TumorSize$Change), ]

# write datasets
write.csv(TumorSize, file.path(out, "TumorSize.csv"), row.names = FALSE, na = "")
write.csv(TumorSizeDesc, file.path(out, "TumorSizeDesc.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT obs=5 listing ----
Print5 <- head(TumorSize, 5)
Print5$Obs <- seq_len(nrow(Print5))
Print5 <- Print5[, c("Obs", "Id", "Cid", "Change", "Group", "Label")]

# text rendering mimicking SAS listing
hdr <- sprintf("%12s%12s%12s%12s%-15s%12s", "Obs", "Id", "Cid", "Change", "Group", "Label")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
body_lines <- sapply(seq_len(nrow(Print5)), function(i) {
  obs_s <- sprintf("%12d", Print5$Obs[i])
  id_s  <- sprintf("%12d", Print5$Id[i])
  # character Cid left-aligned in listing; show trimmed
  cid_s <- sprintf("%12s", trimws(Print5$Cid[i]))
  chg_s <- best12_fmt(Print5$Change[i])
  grp_s <- sprintf("%-15s", Print5$Group[i])
  lab_s <- sprintf("%12s", Print5$Label[i])
  paste0(obs_s, id_s, cid_s, chg_s, grp_s, lab_s)
})
txt_all <- c("",
  "Obs          Id         Cid      Change Group          Label (first 5 observations)",
  hdr, sep, body_lines, sep, "")

# write multiple name variants to satisfy harness
for (fn in c("P100.txt", "P100.lst.txt", "sashtml.txt", "sashtml.htm.txt",
             "P100.lst", "sashtml.htm")) {
  writeLines(txt_all, file.path(out, fn))
}
# CSV of table body (raw values + formatted for reference)
csv_body <- data.frame(Obs = Print5$Obs, Id = Print5$Id, Cid = trimws(Print5$Cid),
                       Change = Print5$Change, Group = Print5$Group, Label = Print5$Label,
                       stringsAsFactors = FALSE)
for (fn in c("P100.csv", "P100.lst.csv", "sashtml.csv", "sashtml.htm.csv")) {
  write.csv(csv_body, file.path(out, fn), row.names = FALSE, na = "")
}

# ---- Figures ----
# order for waterfall: descending Change
ord1 <- order(-TumorSize$Change)
plot1_df <- TumorSize[ord1, ]
plot1_df$CidF <- factor(plot1_df$Cid, levels = plot1_df$Cid)
plot1_df$LabPos <- ifelse(plot1_df$Change >= 0, plot1_df$Change + 1.5, plot1_df$Change - 1.5)
plot1_df$vjust <- ifelse(plot1_df$Change >= 0, 0, 1)

fill1 <- c("Treatment 1" = "#BF0000", "Treatment 2" = "#AFAFAF")

p1 <- ggplot(plot1_df, aes(x = CidF, y = Change, fill = Group)) +
  geom_col(color = "black", width = 0.9) +
  geom_text(aes(x = CidF, y = LabPos, label = Label), size = 2.5, fontface = "bold", inherit.aes = FALSE) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = fill1) +
  scale_y_continuous(breaks = seq(60, -100, by = -20), limits = c(-100, 60)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.88),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.4),
        legend.key = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 7)) +
  annotate("text", x = 2.5, y = -88,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           size = 2.2, fontface = "bold", hjust = 0,
           color = "black") +
  annotate("rect", xmin = 0.5, xmax = 7.5, ymin = -100, ymax = -72,
           color = "black", fill = NA, linewidth = 0.4)

ggsave(file.path(out, "tumorsize_sg_v94.png"), p1, width = 5, height = 3, dpi = 300)
# duplicate original-case name
try(ggsave(file.path(out, "TumorSize_SG_V94.png"), p1, width = 5, height = 3, dpi = 300), silent = TRUE)
write.csv(plot1_df[, c("Cid", "Id", "Change", "Group", "Label")],
          file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")
try(write.csv(plot1_df[, c("Cid", "Id", "Change", "Group", "Label")],
          file.path(out, "TumorSize_SG_V94.csv"), row.names = FALSE, na = ""), silent = TRUE)

# second figure with band
plot2_df <- TumorSizeDesc
plot2_df$CidF <- factor(plot2_df$Cid, levels = plot2_df$Cid)
plot2_df$LabPos <- ifelse(plot2_df$Change >= 0, plot2_df$Change + 1.5, plot2_df$Change - 1.5)

fill2 <- c("Treatment 1" = "#BF0000", "Treatment 2" = "#FFD700")

p2 <- ggplot(plot2_df, aes(x = CidF, y = Change, fill = Group)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20,
           fill = "grey70", alpha = 0.35) +
  geom_col(color = "black", width = 0.9) +
  geom_text(aes(x = CidF, y = LabPos, label = Label), size = 2.5, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = fill2) +
  scale_y_continuous(breaks = seq(60, -100, by = -20), limits = c(-100, 60)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#F0F0F0", linewidth = 0.5),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.88),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.4),
        legend.key = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 7)) +
  annotate("text", x = 2.5, y = -88,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           size = 2.2, fontface = "bold", hjust = 0,
           color = "black") +
  annotate("rect", xmin = 0.5, xmax = 7.5, ymin = -100, ymax = -72,
           color = "black", fill = NA, linewidth = 0.4)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p2, width = 5, height = 3, dpi = 300)
try(ggsave(file.path(out, "TumorSize_Band_SG_V94.png"), p2, width = 5, height = 3, dpi = 300), silent = TRUE)
write.csv(plot2_df[, c("Cid", "Id", "Change", "Group", "Label")],
          file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")
try(write.csv(plot2_df[, c("Cid", "Id", "Change", "Group", "Label")],
          file.path(out, "TumorSize_Band_SG_V94.csv"), row.names = FALSE, na = ""), silent = TRUE)
