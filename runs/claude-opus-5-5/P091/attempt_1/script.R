# P091 - SGPLOT horizontal grouped bar (bar1)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

name <- "bar1"

# ---- DATA my_data (datalines) ----
my_data <- data.frame(
  CATEGORY = c(1, 2, 1, 2),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9),
  stringsAsFactors = FALSE
)
write.csv(my_data, file.path(out, "my_data.csv"), row.names = FALSE, na = "")

# ---- Summarised data plotted (stat=sum) ----
plotdat <- my_data %>%
  group_by(CATEGORY, SERIES) %>%
  summarise(AMOUNT = sum(AMOUNT), .groups = "drop") %>%
  arrange(CATEGORY, desc(SERIES))

# grouporder=descending -> Series B is first group, gets first datacolor
grp_levels <- sort(unique(plotdat$SERIES), decreasing = TRUE)
cols <- setNames(c("#9999ff", "#993366"), grp_levels)

pd <- plotdat %>%
  mutate(CATEGORY_F = factor(CATEGORY, levels = rev(sort(unique(CATEGORY)))),  # 1 at top
         SERIES_F   = factor(SERIES, levels = rev(grp_levels)))               # first group at top within cluster

write.csv(plotdat, file.path(out, paste0(name, "_plotdata.csv")), row.names = FALSE, na = "")

gray33 <- "#545454"
p <- ggplot(pd, aes(x = CATEGORY_F, y = AMOUNT, fill = SERIES_F)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.85,
           colour = "black") +
  scale_fill_manual(values = cols, guide = "none") +
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10),
                     minor_breaks = seq(1, 9, by = 2),
                     expand = expansion(mult = c(0.02, 0))) +
  coord_flip() +
  labs(title = "Horizontal Grouped Bar",
       subtitle = "Compares values across categories",
       x = "CATEGORY", y = "AMOUNT") +
  theme_bw() +
  theme(
    plot.title = element_text(colour = gray33, size = 23, hjust = 0.5),
    plot.subtitle = element_text(colour = gray33, size = 17, hjust = 0.5),
    axis.title = element_text(colour = gray33, size = 16, face = "bold"),
    axis.text = element_text(colour = gray33, size = 16, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.minor.x = element_line(colour = "grey93")
  )

ggsave(file.path(out, paste0(name, ".png")), p, width = 8, height = 6, dpi = 100)

# ---- Text rendering / CSV of the HTML output (graph-only page, titles) ----
txt <- c("SGplot Horizontal Grouped Bar",
         "",
         "Horizontal Grouped Bar",
         "Compares values across categories",
         "",
         "[Graph: bar1.png]",
         "",
         sprintf("%-10s %-10s %8s", "CATEGORY", "SERIES", "AMOUNT"),
         sprintf("%-10s %-10s %8s", plotdat$CATEGORY, plotdat$SERIES,
                 format(plotdat$AMOUNT)))
writeLines(txt, file.path(out, paste0(name, ".txt")))
write.csv(plotdat, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")

cat("P091 done\n")
