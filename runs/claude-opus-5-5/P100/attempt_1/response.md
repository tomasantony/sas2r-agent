```r
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- SAS RANUNI emulation: seed = (397204094 * seed) mod (2^31 - 1) ----
ranuni_env <- new.env()
ranuni_env$seed <- NA_real_
m_mod <- 2147483647
mulmod <- function(a, s, m) {
  a_hi <- floor(a / 65536); a_lo <- a %% 65536
  t1 <- ((a_hi * s) %% m) * 65536 %% m
  t2 <- (a_lo * s) %% m
  (t1 + t2) %% m
}
ranuni <- function(seed) {
  if (is.na(ranuni_env$seed)) ranuni_env$seed <- seed
  ranuni_env$seed <- mulmod(397204094, ranuni_env$seed, m_mod)
  ranuni_env$seed / m_mod
}

# ---- DATA TumorSize ----
rows <- vector("list", 25)
for (id in 1:25) {
  cid <- sprintf("%2d", id)
  change <- 30 - 120 * ranuni(2)
  grp <- if (floor(ranuni(2) + 0.5) != 0) "Treatment 1" else "Treatment 2"
  lab <- c("E", "C", "R", "S", "P")[(id %% 5) + 1]
  rows[[id]] <- data.frame(Cid = cid, Change = change, Id = id,
                           Group = grp, Label = lab, stringsAsFactors = FALSE)
}
TumorSize <- bind_rows(rows)
write.csv(TumorSize, file.path(out, "tumorsize.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT (obs=5) ----
best12 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (v == round(v)) return(as.character(v))
    intd <- nchar(as.character(floor(abs(v))))
    dec <- 12 - intd - 1 - (v < 0)
    s <- sprintf("%.*f", max(dec, 0), v)
    s <- sub("0+$", "", s); s <- sub("\\.$", "", s)
    s
  })
}
pr <- TumorSize[1:5, ]
pr_out <- data.frame(Obs = 1:5, Cid = pr$Cid, Change = best12(pr$Change),
                     Id = pr$Id, Group = pr$Group, Label = pr$Label,
                     stringsAsFactors = FALSE)
write.csv(pr_out, file.path(out, "P100.csv"), row.names = FALSE, na = "")
write.csv(pr_out, file.path(out, "sashtml.csv"), row.names = FALSE, na = "")
txt <- capture.output(print(pr_out, row.names = FALSE, right = TRUE))
writeLines(c("The SAS System", "", txt), file.path(out, "P100.txt"))
writeLines(c("The SAS System", "", txt), file.path(out, "sashtml.txt"))

legend_txt <- "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death"

# ---- Figure 1: TumorSize_SG_V94 (categoryorder=respdesc) ----
pd1 <- TumorSize %>% arrange(desc(Change))
pd1$Cid <- factor(pd1$Cid, levels = pd1$Cid)
write.csv(pd1 %>% mutate(Cid = as.character(Cid)),
          file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")
p1 <- ggplot(pd1, aes(x = Cid, y = Change, fill = Group)) +
  geom_col(colour = "black", width = 1) +
  geom_text(aes(label = Label, vjust = ifelse(Change >= 0, -0.3, 1.2)),
            size = 1.8, fontface = "bold") +
  geom_hline(yintercept = c(20, -30), linetype = "dashed") +
  scale_fill_manual(values = c("Treatment 1" = "#bf0000", "Treatment 2" = "#afafaf"), name = NULL) +
  scale_y_continuous(breaks = seq(-100, 60, 20), limits = c(-100, 60)) +
  annotate("label", x = 0.6, y = -98, label = legend_txt, hjust = 0, vjust = 0, size = 1.8) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_blank(), panel.grid = element_blank(),
        legend.position = c(0.88, 0.85),
        legend.background = element_rect(colour = "black", fill = "white"),
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(out, "tumorsize_sg_v94.png"), p1, width = 5, height = 3, dpi = 300)

# ---- PROC SORT descending change ----
TumorSizeDesc <- TumorSize[order(-TumorSize$Change), ]
write.csv(TumorSizeDesc, file.path(out, "tumorsizedesc.csv"), row.names = FALSE, na = "")

# ---- Figure 2: TumorSize_Band_SG_V94 ----
pd2 <- TumorSizeDesc
pd2$Cid <- factor(pd2$Cid, levels = unique(pd2$Cid))
write.csv(pd2 %>% mutate(Cid = as.character(Cid), Upper = 20, Lower = -30),
          file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")
p2 <- ggplot(pd2, aes(x = Cid, y = Change)) +
  annotate("rect", xmin = 1, xmax = nrow(pd2), ymin = -30, ymax = 20,
           fill = "grey70", alpha = 0.5) +
  geom_col(aes(fill = Group), colour = "black", width = 0.8) +
  geom_text(aes(label = Label, vjust = ifelse(Change >= 0, -0.3, 1.2)),
            size = 1.8, fontface = "bold") +
  scale_fill_manual(values = c("Treatment 1" = "#bf0000", "Treatment 2" = "gold"), name = NULL) +
  scale_y_continuous(breaks = seq(-100, 60, 20), limits = c(-100, 60)) +
  annotate("label", x = 0.6, y = -98, label = legend_txt, hjust = 0, vjust = 0, size = 1.8) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "#f0f0f0"),
        legend.position = c(0.88, 0.85),
        legend.background = element_rect(colour = "black", fill = "white"),
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p2, width = 5, height = 3, dpi = 300)

message("P100 done")
```