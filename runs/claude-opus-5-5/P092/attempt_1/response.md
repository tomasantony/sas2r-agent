```r
# P092 - sct1: Series plot lines with markers
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

name <- "sct1"

# data my_data
my_data <- data.frame(
  x  = c(0.5, 1.5, 2.5, 3.5),
  y1 = c(1.0, 2.8, 0.5, 2.0),
  y2 = c(3.1, 1.3, 1.1, 3.1)
)

# long format for plotting
plot_data <- my_data %>%
  pivot_longer(cols = c(y1, y2), names_to = "series", values_to = "y") %>%
  arrange(series, x)

# Minor ticks: minorcount=3 between majors at step 1 -> every 0.25
minor_br <- setdiff(seq(0, 4, by = 0.25), 0:4)

p <- ggplot() +
  geom_line(data = my_data, aes(x = x, y = y1), colour = "navy", linewidth = 1.1) +
  geom_point(data = my_data, aes(x = x, y = y1), colour = "navy", shape = 18, size = 6) +
  geom_line(data = my_data, aes(x = x, y = y2), colour = "magenta", linewidth = 1.1) +
  geom_point(data = my_data, aes(x = x, y = y2), colour = "magenta", shape = 15, size = 4.5) +
  scale_x_continuous(name = "X Axis", limits = c(0, 4), breaks = 0:4,
                     minor_breaks = minor_br, expand = c(0, 0)) +
  scale_y_continuous(name = "Y Axis", limits = c(0, 4), breaks = 0:4,
                     minor_breaks = minor_br, expand = c(0, 0)) +
  coord_fixed(ratio = 1) +
  labs(title = "Series Plot Lines", subtitle = "With Markers") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 23, colour = "gray20"),
    plot.subtitle = element_text(hjust = 0.5, size = 17, colour = "gray20"),
    axis.title = element_text(size = 16, face = "bold", colour = "gray20"),
    axis.text = element_text(size = 16, face = "bold", colour = "gray20"),
    panel.grid.major = element_line(colour = "gray85"),
    panel.grid.minor = element_line(colour = "gray93"),
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(file.path(out, paste0(name, ".png")), p,
       width = 650 / 96, height = 650 / 96, dpi = 96, units = "in")

# CSV of plotted data
write.csv(plot_data, file.path(out, paste0(name, "_plotdata.csv")), row.names = FALSE, na = "")
write.csv(my_data, file.path(out, "my_data.csv"), row.names = FALSE, na = "")

# The HTML output contains only titles and the graph (no numeric table);
# write a text rendering and CSV of the "table" body (titles + image reference)
tbl <- data.frame(
  element = c("title1", "title2", "graph"),
  text = c("Series Plot Lines", "With Markers", paste0(name, ".png")),
  stringsAsFactors = FALSE
)
write.csv(tbl, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")

txt <- c(
  "SGplot Series Plot Lines With Markers",
  "",
  "Series Plot Lines",
  "With Markers",
  "",
  paste0("[Graph: ", name, ".png]")
)
writeLines(txt, file.path(out, paste0(name, ".txt")))

cat("P092 done\n")
```