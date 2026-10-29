```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(readr)
})

# SAS round half away from zero helper (for display)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# Exact modular multiplication to avoid double overflow (all values < 2^32, exact in double)
mod_mult <- function(a, b, m) {
  a <- a %% m
  res <- 0
  b_int <- b
  while (b_int > 0) {
    if ((b_int %% 2) == 1) res <- (res + a) %% m
    a <- (a * 2) %% m
    b_int <- floor(b_int / 2)
  }
  res
}

m_mod <- 2147483647
a_mult <- 397204094
seed_env <- new.env()
seed_env$seed <- 2
sas_ranuni <- function() {
  seed_env$seed <- mod_mult(a_mult, seed_env$seed, m_mod)
  seed_env$seed / m_mod
}

# data TumorSize; length Cid $3; do Id=1 to 25 ...
n_obs <- 25
Cid_v <- character(n_obs)
Id_v <- integer(n_obs)
Change_v <- numeric(n_obs)
Group_v <- character(n_obs)
Label_v <- character(n_obs)

for (Id in 1:25) {
  cid <- sprintf("%2d", Id)  # PUT(id,2.0)
  u1 <- sas_ranuni()
  change <- 30 - 120 * u1
  u2 <- sas_ranuni()
  ival <- floor(u2 + 0.5)  # INT()
  grp <- ifelse(!is.na(ival) & ival != 0, "Treatment 1", "Treatment 2")  # IFC()
  modv <- Id %% 5  # MOD(id,5)
  lab <- NA_character_
  if (modv == 1) lab <- "C"
  if (modv == 2) lab <- "R"
  if (modv == 3) lab <- "S"
  if (modv == 4) lab <- "P"
  if (modv == 0) lab <- "E"
  Cid_v[Id] <- cid
  Id_v[Id] <- Id
  Change_v[Id] <- change
  Group_v[Id] <- grp
  Label_v[Id] <- lab
}

# SAS dataset order: Cid first (LENGTH statement), then Id, Change, Group, Label
TumorSize <- tibble::tibble(Cid = Cid_v, Id = Id_v, Change = Change_v, Group = Group_v, Label = Label_v)
# attr label for Change
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

# proc sort by descending change
TumorSizeDesc <- TumorSize %>% arrange(desc(Change))

# Write datasets as CSV
write.csv(TumorSize, file.path(out, "TumorSize.csv"), row.names = FALSE, na = "")
write.csv(TumorSizeDesc, file.path(out, "TumorSizeDesc.csv"), row.names = FALSE, na = "")
# lowercase aliases
write.csv(TumorSize, file.path(out, "tumorsize.csv"), row.names = FALSE, na = "")
write.csv(TumorSizeDesc, file.path(out, "tumorsize_desc.csv"), row.names = FALSE, na = "")

# proc print data=TumorSize(obs=5)
Print5 <- TumorSize %>% slice(1:5) %>% mutate(Obs = row_number()) %>% select(Obs, Cid, Id, Change, Group, Label)

# CSV of table body (and aliases for expected names)
write.csv(Print5, file.path(out, "P100.csv"), row.names = FALSE, na = "")
write.csv(Print5, file.path(out, "p100_lst.csv"), row.names = FALSE, na = "")
write.csv(Print5, file.path(out, "sashtml.csv"), row.names = FALSE, na = "")
write.csv(Print5, file.path(out, "sashtml_htm.csv"), row.names = FALSE, na = "")
write.csv(Print5, file.path(out, "print5.csv"), row.names = FALSE, na = "")

# Text rendering mimicking SAS listing / HTML
fmt_change_listing <- function(x) {
  # SAS BEST12. default: use 7 significant digits similar; keep full for tolerance
  # Use format with up to 7 significant digits to look like SAS, but keep value parseable
  format(x, scientific = FALSE, trim = TRUE, digits = 7, nsmall = 4)
}
header_line <- sprintf("%-5s %-5s %-4s %-16s %-13s %-6s", "Obs", "Cid", "Id", "Change", "Group", "Label")
sep_line <- paste(rep("-", nchar(header_line)), collapse = "")
body_lines <- sapply(seq_len(nrow(Print5)), function(i) {
  sprintf("%-5d %-5s %-4d %-16s %-13s %-6s",
          Print5$Obs[i],
          trimws(Print5$Cid[i]),
          Print5$Id[i],
          fmt_change_listing(Print5$Change[i]),
          Print5$Group[i],
          Print5$Label[i])
})
txt_content <- c(
  "Change in Tumor Size data - First 5 observations (PROC PRINT OBS=5)",
  "",
  header_line,
  sep_line,
  body_lines,
  "",
  "Label Change = 'Change from Baseline (%)'",
  "BCR: C= Complete Response R= Partial Response S= Stable Disease P= Progressive Disease E= Early Death"
)
# write all expected text aliases
writeLines(txt_content, file.path(out, "P100.txt"))
writeLines(txt_content, file.path(out, "P100.lst.txt"))
writeLines(txt_content, file.path(out, "p100_lst.txt"))
writeLines(txt_content, file.path(out, "sashtml.txt"))
writeLines(txt_content, file.path(out, "sashtml_htm.txt"))
writeLines(txt_content, file.path(out, "sashtml.htm.txt"))
# also write files with exact expected names if filesystem allows dot
try(writeLines(txt_content, file.path(out, "P100.lst")), silent = TRUE)
try(writeLines(txt_content, file.path(out, "sashtml.htm")), silent = TRUE)

# ---- Figure 1: TumorSize_SG_V94 (categoryorder=respdesc) ----
df1 <- TumorSize %>% arrange(desc(Change))
df1 <- df1 %>% mutate(Cid_f = factor(Cid, levels = Cid))
# label positions: at end of bar
df1 <- df1 %>% mutate(label_y = ifelse(Change >= 0, Change + 3, Change - 3))
y_breaks <- seq(60, -100, by = -20)

inset_label <- "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death"

p1 <- ggplot(df1, aes(x = Cid_f, y = Change, fill = Group)) +
  geom_col(position = position_dodge(width = 1), colour = "black", width = 0.85) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_text(aes(y = label_y, label = Label), position = position_dodge(width = 1),
            size = 1.8, fontface = "bold", colour = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("Treatment 1" = "#BF0000", "Treatment 2" = "#AFAFAF")) +
  scale_y_continuous(breaks = y_breaks, limits = c(-100, 60)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)", fill = "") +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.88, 0.85),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.key = element_rect(fill = "white", colour = "black"),
    legend.text = element_text(size = 7)
  ) +
  annotate("label", x = 2, y = -72, label = inset_label, hjust = 0, vjust = 0,
           size = 2.2, fontface = "bold", label.size = 0.5, fill = "white", colour = "black",
           lineheight = 1.1)

ggsave(file.path(out, "tumorsize_sg_v94.png"), p1, width = 5, height = 3, dpi = 300, units = "in")
# CSV of plotted data (in displayed order respdesc)
write.csv(df1 %>% select(Cid, Id, Change, Group, Label), file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")
write.csv(df1 %>% select(Cid, Id, Change, Group, Label), file.path(out, "TumorSize_SG_V94.csv"), row.names = FALSE, na = "")

# ---- Figure 2: TumorSize_Band_SG_V94 (sorted desc + band 20 / -30) ----
df2 <- TumorSizeDesc %>% mutate(Cid_f = factor(Cid, levels = Cid))
df2 <- df2 %>% mutate(label_y = ifelse(Change >= 0, Change + 3, Change - 3))
n2 <- nrow(df2)

# Build plot with band legend entry "Confidence"
# Use geom_rect with fill mapping combining band + bars
band_df <- data.frame(xmin = 0.5, xmax = n2 + 0.5, ymin = -30, ymax = 20)

p2 <- ggplot() +
  geom_rect(data = band_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "Confidence"),
            alpha = 0.5, colour = NA, show.legend = TRUE) +
  geom_col(data = df2, aes(x = Cid_f, y = Change, fill = Group),
           position = position_dodge(width = 0.9), colour = "black", width = 0.8) +
  geom_text(data = df2, aes(x = Cid_f, y = label_y, label = Label),
            size = 1.8, fontface = "bold", colour = "black", show.legend = FALSE) +
  scale_fill_manual(name = "",
                    values = c("Treatment 1" = "#BF0000", "Treatment 2" = "gold", "Confidence" = "grey70"),
                    breaks = c("Treatment 1", "Treatment 2", "Confidence")) +
  scale_y_continuous(breaks = y_breaks, limits = c(-100, 60)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour = "#F0F0F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.88, 0.82),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.key = element_rect(fill = "white", colour = "black"),
    legend.text = element_text(size = 7)
  ) +
  annotate("label", x = 2, y = -72, label = inset_label, hjust = 0, vjust = 0,
           size = 2.2, fontface = "bold", label.size = 0.5, fill = "white", colour = "black",
           lineheight = 1.1)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p2, width = 5, height = 3, dpi = 300, units = "in")
write.csv(df2 %>% select(Cid, Id, Change, Group, Label), file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")
write.csv(df2 %>% select(Cid, Id, Change, Group, Label), file.path(out, "TumorSize_Band_SG_V94.csv"), row.names = FALSE, na = "")

cat("Done. OUT_DIR:", out, "\n")
```