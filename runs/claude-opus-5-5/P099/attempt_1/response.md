```r
# P099 - Cumulative frequency curves (translation of P099_cumulativecurve.sas)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Round half away from zero (SAS ROUND semantics)
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5) / m
}

# ------------------------------------------------------------------
# Demo data (SAS: call streaminit(123); rand("Normal"))
# NOTE: SAS's random number stream cannot be reproduced in R; an R seed is used.
# The linear block (days 35-67) is deterministic and matches SAS exactly.
# ------------------------------------------------------------------
set.seed(123)
Day <- 1:100
Block <- character(100)
NewCases <- numeric(100)
for (i in seq_along(Day)) {
  d <- Day[i]
  if (d < 35) {
    Block[i] <- "Concave Up:/New Cases/Increasing"
    NewCases[i] <- d + rnorm(1)
  } else if (d < 68) {
    Block[i] <- "Linear:/New Cases/Constant"
    NewCases[i] <- 30 + (d %% 2)
  } else {
    Block[i] <- "Concave Down:/New Cases/Decreasing"
    NewCases[i] <- 100 - d + rnorm(1)
  }
  NewCases[i] <- floor(0.49 * NewCases[i])
  if (NewCases[i] < 0) NewCases[i] <- 0
}
Demo <- data.frame(Day = Day, Block = Block, NewCases = NewCases,
                   Cumul = cumsum(NewCases), stringsAsFactors = FALSE)

# ------------------------------------------------------------------
# Scenario datasets (Cumul = running total of NewCases, as in datalines)
# ------------------------------------------------------------------
mk <- function(type, nc) {
  if (length(nc) != 100) warning(sprintf("%s: length %d", type, length(nc)))
  data.frame(Type = type, Day = 1:100, NewCases = nc, Cumul = cumsum(nc),
             stringsAsFactors = FALSE)
}

Uni <- mk("Constant", c(
  9,5,8,8,5,13,11,12,9,3,13,5,12,14,11,11,11,10,12,14,
  11,11,13,12,14,13,15,11,12,11,5,12,5,9,10,12,11,11,11,8,
  15,9,9,16,13,8,13,9,5,12,11,16,9,7,9,7,12,7,7,12,
  11,9,5,9,12,6,14,15,9,7,10,7,9,8,12,10,7,10,11,12,
  4,12,12,16,13,12,7,6,14,12,7,7,5,11,4,12,6,9,11,8))

Peak <- mk("Early Peak", c(
  8,13,18,24,24,28,31,31,35,37,37,37,33,37,32,32,33,33,29,27,
  26,27,23,24,20,20,21,13,16,17,16,14,12,13,15,10,11,11,9,7,
  7,7,7,8,9,5,2,3,3,4,3,5,3,3,4,0,3,0,2,0,
  0,4,2,0,0,0,0,0,0,0,1,0,0,1,0,0,1,2,0,0,
  0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,1,0,0,0))

Flatten <- mk("Flattened", c(
  3,5,7,11,10,13,16,16,20,23,24,25,21,27,22,23,26,26,24,23,
  23,24,21,23,20,21,22,15,19,20,19,17,16,17,20,14,16,16,14,12,
  12,12,12,13,14,10,7,8,8,9,10,13,11,10,8,4,10,3,6,0,
  3,7,7,3,4,0,2,2,3,3,7,2,0,3,0,3,4,5,2,0,
  0,0,0,0,3,3,4,0,0,1,0,4,3,2,0,0,2,1,2,1))

Outbreak2 <- mk("Second Outbreak", c(
  4,5,5,8,6,9,12,13,17,20,22,25,23,29,27,29,33,34,32,32,
  32,33,30,32,28,27,27,19,21,21,19,16,14,13,14,9,9,8,6,4,
  3,3,3,3,5,1,0,0,0,1,2,5,3,3,1,0,4,0,2,0,
  1,6,8,7,9,7,11,13,15,15,19,13,8,13,6,8,10,10,3,1,
  0,0,0,0,2,1,2,0,0,0,0,2,1,1,0,0,1,0,1,0))

Exp <- mk("Exponential", c(
  0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,
  0,1,0,1,0,0,1,0,0,1,1,0,0,1,2,0,1,1,1,1,
  1,1,2,2,3,2,1,1,2,2,3,4,4,4,3,2,5,3,4,2,
  3,5,5,4,5,4,6,6,7,8,11,10,9,12,11,13,16,17,16,17,
  17,19,19,22,25,26,28,29,31,34,34,40,42,45,46,50,55,58,63,66))

for (d in list(Uni, Peak, Flatten, Outbreak2, Exp)) {
  if (tail(d$Cumul, 1) != 1000) warning(paste(d$Type[1], "does not sum to 1000"))
}

# ------------------------------------------------------------------
# SGPlot: Cumulative cases with blocks
# ------------------------------------------------------------------
blk <- Demo %>% mutate(grp = cumsum(Block != lag(Block, default = ""))) %>%
  group_by(grp, Block) %>% summarise(xmin = min(Day) - 0.5, xmax = max(Day) + 0.5,
                                     .groups = "drop") %>%
  mutate(label = gsub("/", "\n", Block))
ymax <- max(Demo$Cumul) * 1.22
p <- ggplot() +
  geom_rect(data = blk, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Block),
            alpha = 0.8, show.legend = FALSE) +
  geom_text(data = blk, aes(x = (xmin + xmax) / 2, y = ymax * 0.93, label = label), size = 4) +
  geom_line(data = Demo, aes(Day, Cumul), linewidth = 1.5, colour = "#445694") +
  scale_fill_manual(values = c("Concave Up:/New Cases/Increasing" = "#FF9999",
                               "Linear:/New Cases/Constant" = "#FFFF99",
                               "Concave Down:/New Cases/Decreasing" = "#99FF99")) +
  coord_cartesian(ylim = c(0, ymax)) +
  labs(title = "Cumulative Cases versus Time", x = "Day", y = "Cumulative Cases") +
  theme_bw()
ggsave(file.path(out, "sgplot.png"), p, width = 8, height = 6, dpi = 100)
write.csv(Demo, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# SGPlot1 / SGPlot2: Outbreak2 new cases (needle) and total cases (series)
p1 <- ggplot(Outbreak2, aes(Day, NewCases)) +
  geom_segment(aes(xend = Day, yend = 0), colour = "#445694") +
  labs(title = "New Cases", x = "Day", y = NULL) + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file.path(out, "sgplot1.png"), p1, width = 3, height = 2, dpi = 100)
write.csv(Outbreak2[, c("Day", "NewCases")], file.path(out, "sgplot1.csv"), row.names = FALSE)

p2 <- ggplot(Outbreak2, aes(Day, Cumul)) + geom_line(colour = "#445694") +
  labs(title = "Total Cases", x = "Day", y = NULL) + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file.path(out, "sgplot2.png"), p2, width = 3, height = 2, dpi = 100)
write.csv(Outbreak2[, c("Day", "Cumul")], file.path(out, "sgplot2.csv"), row.names = FALSE)

# ------------------------------------------------------------------
# Infection + PROC MEANS
# ------------------------------------------------------------------
Infection <- bind_rows(Uni, Peak, Flatten, Outbreak2, Exp)
type_order <- unique(Infection$Type)
Infection$TypeF <- factor(Infection$Type, levels = type_order)

means <- Infection %>% group_by(TypeF) %>%
  summarise(N_Obs = n(), Mean = mean(NewCases), StdDev = sd(NewCases),
            Min = min(NewCases), Max = max(NewCases), Sum = sum(NewCases),
            .groups = "drop") %>%
  rename(Type = TypeF) %>% mutate(Type = as.character(Type))
write.csv(means, file.path(out, "p099.csv"), row.names = FALSE, na = "")
write.csv(means, file.path(out, "P099_means.csv"), row.names = FALSE, na = "")

fmt <- function(x) {
  ip <- nchar(as.character(floor(abs(x))))
  dec <- pmax(0, pmin(7, 11 - ip - 1))
  formatC(round_half_up(x, dec), format = "f", digits = dec, width = 12)
}
lines <- c("The MEANS Procedure", "",
           "Analysis Variable : NewCases New Cases", "",
           sprintf("%-16s %6s %12s %12s %12s %12s %12s",
                   "Type", "N Obs", "Mean", "Std Dev", "Minimum", "Maximum", "Sum"),
           strrep("-", 90))
for (i in seq_len(nrow(means))) {
  lines <- c(lines, sprintf("%-16s %6d %s %s %s %s %s", means$Type[i], means$N_Obs[i],
                            fmt(means$Mean[i]), fmt(means$StdDev[i]), fmt(means$Min[i]),
                            fmt(means$Max[i]), fmt(means$Sum[i])))
}
lines <- c(lines, strrep("-", 90))
writeLines(lines, file.path(out, "p099.txt"))

# ------------------------------------------------------------------
# SGPanel figures
# ------------------------------------------------------------------
pp <- ggplot(Infection, aes(Day, NewCases)) +
  geom_segment(aes(xend = Day, yend = 0), colour = "#445694") +
  facet_grid(TypeF ~ .) + scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "New Cases versus Time", y = "New Cases") + theme_bw()
ggsave(file.path(out, "sgpanel.png"), pp, width = 5, height = 6, dpi = 100)
write.csv(Infection[, c("Type", "Day", "NewCases")], file.path(out, "sgpanel.csv"), row.names = FALSE)

pp1 <- ggplot(Infection, aes(Day, Cumul)) + geom_line(colour = "#445694") +
  facet_grid(TypeF ~ .) + expand_limits(y = 0) +
  labs(title = "Cumulative Cases versus Time", x = NULL, y = "Cumulative Cases") + theme_bw()
ggsave(file.path(out, "sgpanel1.png"), pp1, width = 5, height = 6, dpi = 100)
write.csv(Infection[, c("Type", "Day", "Cumul")], file.path(out, "sgpanel1.csv"), row.names = FALSE)

InfPos <- Infection %>% filter(Cumul > 0)
pp2 <- ggplot(InfPos, aes(Day, Cumul)) + geom_line(colour = "#445694") +
  facet_grid(TypeF ~ .) + scale_y_log10() +
  labs(title = "log(Cumulative Cases) versus Time", x = NULL, y = "Cumulative Cases") + theme_bw()
ggsave(file.path(out, "sgpanel2.png"), pp2, width = 5, height = 6, dpi = 100)
write.csv(InfPos[, c("Type", "Day", "Cumul")], file.path(out, "sgpanel2.csv"), row.names = FALSE)

# ------------------------------------------------------------------
# SGRender (Epidemic template): cumulative (top) + new-case needles (bottom)
# ------------------------------------------------------------------
render <- function(d, title, name) {
  long <- bind_rows(
    data.frame(Panel = "Cumulative Cases", Day = d$Day, Value = d$Cumul),
    data.frame(Panel = "New Cases", Day = d$Day, Value = d$NewCases))
  long$Panel <- factor(long$Panel, levels = c("Cumulative Cases", "New Cases"))
  g <- ggplot(long, aes(Day, Value)) +
    geom_line(data = subset(long, Panel == "Cumulative Cases"), colour = "#445694") +
    geom_segment(data = subset(long, Panel == "New Cases"),
                 aes(xend = Day, yend = 0), colour = "#445694") +
    geom_blank(data = data.frame(Panel = factor("New Cases", levels = levels(long$Panel)),
                                 Day = 0, Value = 66)) +
    facet_grid(Panel ~ ., scales = "free_y", switch = "y") +
    scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
    labs(title = title, x = "Day", y = NULL) + theme_bw() +
    theme(strip.placement = "outside", strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  ggsave(file.path(out, paste0(name, ".png")), g, width = 5, height = 3.6, dpi = 100)
  write.csv(d[, c("Day", "NewCases", "Cumul")], file.path(out, paste0(name, ".csv")),
            row.names = FALSE)
}
render(Uni, "New Cases at a Constant Rate", "sgrender")
render(Peak, "Rapid Growth; Gradual Decline", "sgrender1")
render(Flatten, "Gradual Growth; Gradual Decline", "sgrender2")
render(Outbreak2, "Secondary Outbreak", "sgrender3")
render(Exp, "Exponential Growth in New Cases", "sgrender4")

cat("P099 done\n")
```