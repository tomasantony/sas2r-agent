out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

## ------------------------------------------------------------
## SAS RANUNI replication (Fishman & Moore, SAS 9.x)
##   seed_{n+1} = (397204094 * seed_n) mod 2147483647
##   u = seed / 2147483647
## ------------------------------------------------------------
m <- 2147483647
a <- 397204094

mod_mult <- function(a, b, m) {
  # (a*b) %% m without overflow (binary method)
  res <- 0
  a <- a %% m
  b <- as.numeric(b)
  while (b > 0) {
    if (b %% 2 == 1) {
      res <- (res + a) %% m
    }
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  res
}

seed <- 2
sas_ranuni <- function() {
  seed <<- mod_mult(a, seed, m)
  seed / m
}

## ------------------------------------------------------------
## Data step: TumorSize
## ------------------------------------------------------------
n <- 25
Id <- 1:n
Cid <- sprintf("%2d", Id)          # put(id,2.0)  right-aligned width 2
Change <- numeric(n)
Group <- character(n)
Label <- character(n)

for (i in seq_len(n)) {
  u1 <- sas_ranuni()
  Change[i] <- 30 - 120 * u1
  u2 <- sas_ranuni()
  # int(ranuni+0.5) -> floor(u+0.5) ; 1 => Treatment 1 else Treatment 2
  iv <- floor(u2 + 0.5)
  Group[i] <- if (iv == 1) "Treatment 1" else "Treatment 2"
  # SAS: if mod(id,5)=1 then Label='C' etc.  Note SAS is case-insensitive
  # but variable is Label (BCR)
  r <- i %% 5
  if (r == 1) Label[i] <- "C"
  else if (r == 2) Label[i] <- "R"
  else if (r == 3) Label[i] <- "S"
  else if (r == 4) Label[i] <- "P"
  else if (r == 0) Label[i] <- "E"
}

TumorSize <- data.frame(
  Cid = Cid,
  Id = Id,
  Change = Change,
  Group = Group,
  Label = Label,
  stringsAsFactors = FALSE
)
# label attribute (SAS label)
attr(TumorSize$Change, "label") <- "Change from Baseline (%)"

# CSV of full dataset (for reference, not required but useful)
write.csv(TumorSize, file.path(out, "TumorSize.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------
## PROC PRINT obs=5  -> P100.lst / sashtml.htm
## ------------------------------------------------------------
print5 <- TumorSize[1:5, c("Cid","Id","Change","Group","Label")]
# SAS proc print shows Obs number
print5_with_obs <- cbind(Obs = 1:5, print5)

# write CSVs
write.csv(print5_with_obs, file.path(out, "P100.csv"), row.names = FALSE, na = "")
write.csv(print5_with_obs, file.path(out, "sashtml.csv"), row.names = FALSE, na = "")

# text rendering helper
render_print <- function(df, title1 = NULL, title2 = NULL) {
  # simple fixed-width text similar to SAS listing
  hdr <- c("Obs","Cid","Id","Change","Group","Label")
  # format Change with SAS default BEST12. ? Use 6 decimals as generated
  # Keep full precision for listing; format with 4 decimals for readability
  # We will show Change with 4 decimals (SAS default would show full)
  fmt_change <- format(round(df$Change, 4), nsmall = 4, trim = TRUE)
  lines <- c()
  if (!is.null(title1)) lines <- c(lines, title1)
  if (!is.null(title2)) lines <- c(lines, title2)
  lines <- c(lines, paste(sprintf("%-4s %-4s %-4s %-12s %-13s %-5s",
                                  "Obs","Cid","Id","Change","Group","Label"),
                          collapse = ""))
  lines <- c(lines, paste(rep("-", 60), collapse = ""))
  for (i in seq_len(nrow(df))) {
    lines <- c(lines, sprintf("%-4d %-4s %-4d %-12s %-13s %-5s",
                              df$Obs[i],
                              df$Cid[i],
                              df$Id[i],
                              fmt_change[i],
                              df$Group[i],
                              df$Label[i]))
  }
  lines
}

txt_lines <- render_print(print5_with_obs)

# write .txt, .lst, .htm renderings (all same content)
writeLines(txt_lines, file.path(out, "P100.txt"))
writeLines(txt_lines, file.path(out, "P100.lst"))
writeLines(c("<html><body><pre>", txt_lines, "</pre></body></html>"),
           file.path(out, "sashtml.htm"))
writeLines(txt_lines, file.path(out, "sashtml.txt"))

## ------------------------------------------------------------
## Figure 1: TumorSize_SG_V94  (categoryorder=respdesc)
## ------------------------------------------------------------
library(ggplot2)
library(dplyr)

# order by Change descending (respdesc)
ord1 <- TumorSize %>% arrange(desc(Change))
ord1$Cid <- factor(ord1$Cid, levels = ord1$Cid)

# colors: cxbf0000 (red) , cxafafaf (light grey)
col1 <- c("Treatment 1" = "#bf0000", "Treatment 2" = "#afafaf")

p1 <- ggplot(ord1, aes(x = Cid, y = Change, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           colour = "black", width = 0.8) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", colour = "black") +
  geom_text(aes(label = Label), position = position_dodge(width = 0.9),
            vjust = ifelse(ord1$Change >= 0, -0.5, 1.2),
            size = 2.5, fontface = "bold") +
  scale_fill_manual(values = col1, name = "") +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 9) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.92, 0.92),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.justification = c(1, 1)
  ) +
  annotate("label",
           x = 1, y = -85,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           hjust = 0, vjust = 0, size = 2.2, fontface = "bold",
           fill = "white", colour = "black", label.size = 0.3,
           lineheight = 0.9)

# 5in x 3in at 300 dpi => 1500 x 900 px
ggsave(file.path(out, "tumorsize_sg_v94.png"), p1, width = 5, height = 3, dpi = 300, units = "in")
ggsave(file.path(out, "TumorSize_SG_V94.png"), p1, width = 5, height = 3, dpi = 300, units = "in")

# CSV of plotted data (ordered as displayed)
write.csv(ord1, file.path(out, "tumorsize_sg_v94.csv"), row.names = FALSE, na = "")
write.csv(ord1, file.path(out, "TumorSize_SG_V94.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------
## Figure 2: TumorSize_Band_SG_V94  (sorted desc, with band)
## ------------------------------------------------------------
TumorSizeDesc <- TumorSize %>% arrange(desc(Change))
TumorSizeDesc$Cid <- factor(TumorSizeDesc$Cid, levels = TumorSizeDesc$Cid)

col2 <- c("Treatment 1" = "#bf0000", "Treatment 2" = "gold")

# band data: rectangle from -30 to 20 across all x
band_df <- data.frame(
  Cid = levels(TumorSizeDesc$Cid),
  upper = 20,
  lower = -30
)
# need numeric x for geom_rect
TumorSizeDesc$xnum <- as.numeric(TumorSizeDesc$Cid)

p2 <- ggplot() +
  geom_rect(aes(xmin = 0.5, xmax = nrow(TumorSizeDesc) + 0.5, ymin = -30, ymax = 20),
            fill = "grey80", alpha = 0.5, colour = NA) +
  geom_bar(data = TumorSizeDesc,
           aes(x = Cid, y = Change, fill = Group),
           stat = "identity", position = position_dodge(width = 0.9),
           colour = "black", width = 0.8) +
  geom_text(data = TumorSizeDesc,
            aes(x = Cid, y = Change, label = Label, group = Group),
            position = position_dodge(width = 0.9),
            vjust = ifelse(TumorSizeDesc$Change >= 0, -0.5, 1.2),
            size = 2.5, fontface = "bold") +
  scale_fill_manual(values = col2, name = "") +
  scale_y_continuous(limits = c(-100, 60), breaks = seq(60, -100, by = -20)) +
  labs(title = "Change in Tumor Size", subtitle = "ITT Population",
       x = NULL, y = "Change from Baseline (%)") +
  theme_minimal(base_size = 9) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.92, 0.92),
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.justification = c(1, 1)
  ) +
  annotate("label",
           x = 1, y = -85,
           label = "BCR\nC= Complete Response\nR= Partial Response\nS= Stable Disease\nP= Progressive Disease\nE= Early Death",
           hjust = 0, vjust = 0, size = 2.2, fontface = "bold",
           fill = "white", colour = "black", label.size = 0.3,
           lineheight = 0.9)

ggsave(file.path(out, "tumorsize_band_sg_v94.png"), p2, width = 5, height = 3, dpi = 300, units = "in")
ggsave(file.path(out, "TumorSize_Band_SG_V94.png"), p2, width = 5, height = 3, dpi = 300, units = "in")

write.csv(TumorSizeDesc %>% select(Cid, Id, Change, Group, Label),
          file.path(out, "tumorsize_band_sg_v94.csv"), row.names = FALSE, na = "")
write.csv(TumorSizeDesc %>% select(Cid, Id, Change, Group, Label),
          file.path(out, "TumorSize_Band_SG_V94.csv"), row.names = FALSE, na = "")

# also ensure lower-case names expected by harness
# (already written)
