```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## ---- helpers ----
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5) / m
}

## SAS RANUNI(2) : prime modulus 2**31-1 multiplier 397204094
sas_ranuni_seq <- function(init_seed, n) {
  m <- 2147483647
  a <- 397204094
  s <- as.numeric(init_seed)
  u <- numeric(n)
  for (i in seq_len(n)) {
    res <- 0
    base <- s
    y <- a
    while (y > 0) {
      if ((y %% 2) == 1) {
        res <- res + base
        if (res >= m) res <- res - m
      }
      base <- base * 2
      if (base >= m) base <- base - m
      y <- floor(y / 2)
    }
    s <- res
    u[i] <- s / m
  }
  u
}

suppressPackageStartupMessages({
  library(ggplot2)
})

## ---- data TumorSize : do Id=1 to 25 ----
## two RANUNI calls per iteration: change then group
u <- sas_ranuni_seq(2, 50)
u_change <- u[seq(1, 50, by = 2)]
u_group  <- u[seq(2, 50, by = 2)]

Id <- 1:25
Cid <- sprintf("%2d", Id)
Change <- 30 - 120 * u_change
gnum <- as.integer(floor(u_group + 0.5))
Group <- ifelse(gnum == 1, "Treatment 1", "Treatment 2")
Label <- character(25)
for (i in Id) {
  r <- i %% 5
  if (r == 1) Label[i] <- "C"
  if (r == 2) Label[i] <- "R"
  if (r == 3) Label[i] <- "S"
  if (r == 4) Label[i] <- "P"
  if (r == 0) Label[i] <- "E"
}

TumorSize <- data.frame(Id = Id, Cid = Cid, Change = Change,
                        Group = Group, Label = Label, stringsAsFactors = FALSE)
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

cat("TumorSize generated:", nrow(TumorSize), "rows\n")

## ---- PROC PRINT obs=5 ----
Print5 <- head(TumorSize, 5)
Print5$Obs <- seq_len(nrow(Print5))
Print5 <- Print5[, c("Obs", "Id", "Cid", "Change", "Group", "Label")]

## CSV counterparts (SAS variable names, same case)
csv_print <- data.frame(Obs = Print5$Obs, Id = Print5$Id, Cid = Print5$Cid,
                        Change = Print5$Change, Group = Print5$Group,
                        Label = Print5$Label, stringsAsFactors = FALSE)
write.csv(csv_print, file.path(out, "P100.csv"), row.names = FALSE, na = "")
write.csv(csv_print, file.path(out, "P100_lst.csv"), row.names = FALSE, na = "")
write.csv(csv_print, file.path(out, "sashtml.csv"), row.names = FALSE, na = "")
## aliases to cover naming variants
try(write.csv(csv_print, file.path(out, "P100.lst.csv"), row.names = FALSE, na = ""), silent = TRUE)
try(write.csv(csv_print, file.path(out, "sashtml_htm.csv"), row.names = FALSE, na = ""), silent = TRUE)

## TXT rendering
fmt_best <- function(x) {
  ## emulate BEST12. : up to 12 significant digits
  s <- format(x, digits = 12, scientific = FALSE, trim = TRUE, nsmall = 0)
  trimws(s)
}
hdr <- c("Obs", "Id", "Cid", "Change", "Group", "Label")
lines_p <- c("",
  "Obs    Id    Cid         Change    Group          Label",
  "------------------------------------------------------")
for (k in seq_len(nrow(Print5))) {
  lines_p <- c(lines_p, sprintf("%3d %5d   %3s %14s    %-12s   %s",
    Print5$Obs[k], Print5$Id[k], Print5$Cid[k],
    fmt_best(Print5$Change[k]), Print5$Group[k], Print5$Label[k]))
}
txt_all <- c("PROC PRINT data=TumorSize(obs=5);", "", lines_p, "")
writeLines(txt_all, file.path(out, "P100.txt"))
writeLines(txt_all, file.path(out, "P100_lst.txt"))
writeLines(txt_all, file.path(out, "sashtml.txt"))
try(writeLines(txt_all, file.path(out, "P100.lst.txt")), silent = TRUE)
try(writeLines(txt_all, file.path(out, "sashtml.htm.txt")), silent = TRUE)

cat("Print table written\n")

## ---- sort descending change ----
ord <- order(-TumorSize$Change)
TumorSizeDesc <- TumorSize[ord, , drop = FALSE]

## ---- Figure 1 : TumorSize_SG_V94 ----
## categoryorder=respdesc => descending change left to right
plot1_data <- TumorSize[order(-TumorSize$Change), , drop = FALSE]
plot1_data$CidF <- factor(plot1_data$Cid, levels = plot1_data$Cid)

fill1 <- c("Treatment 1" = "#BF0000", "Treatment 2" = "#AFAFAF")

p1 <- ggplot(plot1_data, aes(x = CidF, y = Change, fill = Group)) +
  geom_col(colour = "black", width = 0.9) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_text(aes(label = Label, y = Change),
            vjust = ifelse(plot1_data$Change >= 0, -0.4, 1.4),
            size = 1.8, fontface = "bold", show.legend = FALSE) +
  scale_y_continuous(breaks = seq(60, -100, by = -20), limits = c(-100, 60)) +
  scale_fill_manual(values = fill1) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = NULL) +
  theme_bw(base_size = 8) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.88, 0.88),
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.key.size = unit(0.25, "in"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = 2.5, y = -82,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           size = 1.8, fontface = "bold", hjust = 0, vjust = 0,
           lineheight = 0.9) +
  annotate("rect", xmin = 0.6, xmax = 6.2, ymin = -98, ymax = -68,
           colour = "black", fill = NA, linewidth = 0.4)

ggsave(file.path(out, "tumorsize_sg_v94.png"), p1, width = 5, height = 3, dpi = 300)
cat("Figure 1 saved\n")

plot1_csv <- data.frame(Cid = plot1_data$Cid, Id = plot1_data$Id,
                        Change = plot1_data$Change, Group = plot1_data$Group,
                        Label = plot1_data$Label, stringsAsFactors = FALSE)
write.csv(plot1_csv, file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")

## ---- Figure 2 : TumorSize_Band_SG_V94 ----
plot2_data <- TumorSizeDesc
plot2_data$CidF <- factor(plot2_data$Cid, levels = plot2_data$Cid)

fill2 <- c("Treatment 1" = "#BF0000", "Treatment 2" = "#FFD700")

p2 <- ggplot(plot2_data, aes(x = CidF, y = Change, fill = Group)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -30, ymax = 20,
           fill = "grey70", alpha = 0.5) +
  geom_col(colour = "black", width = 0.9) +
  geom_text(aes(label = Label, y = Change),
            vjust = ifelse(plot2_data$Change >= 0, -0.4, 1.4),
            size = 1.8, fontface = "bold", show.legend = FALSE) +
  scale_y_continuous(breaks = seq(60, -100, by = -20), limits = c(-100, 60)) +
  scale_fill_manual(values = fill2, labels = c("Treatment 1", "Treatment 2", "Confidence")) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = NULL) +
  theme_bw(base_size = 8) +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.88, 0.88),
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.key.size = unit(0.25, "in"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = 2.5, y = -82,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           size = 1.8, fontface = "bold", hjust = 0, vjust = 0,
           lineheight = 0.9) +
  annotate("rect", xmin = 0.6, xmax = 6.2, ymin = -98, ymax = -68,
           colour = "black", fill = NA, linewidth = 0.4)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p2, width = 5, height = 3, dpi = 300)
cat("Figure 2 saved\n")

plot2_csv <- data.frame(Cid = plot2_data$Cid, Id = plot2_data$Id,
                        Change = plot2_data$Change, Group = plot2_data$Group,
                        Label = plot2_data$Label, stringsAsFactors = FALSE)
write.csv(plot2_csv, file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")

cat("Done\n")
```