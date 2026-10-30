```r
# P091_bar1.sas -> R translation
# Horizontal Grouped Bar (SGPLOT hbar)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (ROUND) - not strictly needed here but kept for fidelity
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# -------------------------------------------------------------------------
# Data: inline datalines from SAS
# data my_data;
# input CATEGORY SERIES $ 3-11 AMOUNT;
# datalines;
# 1 Series A  5
# 2 Series A  7.8
# 1 Series B  9.5
# 2 Series B  5.9
# ;
my_data <- tibble::tibble(
  CATEGORY = c(1L, 2L, 1L, 2L),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9)
)

# For plotting: SAS options
# hbar category / response=amount stat=sum group=series groupdisplay=cluster grouporder=descending
# datacolors=(cx9999ff cx993366) -> #9999FF, #993366
# grouporder=descending => descending formatted values: Series B before Series A
my_data_plot <- my_data
my_data_plot$SERIES <- factor(my_data_plot$SERIES, levels = c("Series B", "Series A"))
# CATEGORY as factor for discrete y axis; keep numeric order 1,2 (1 bottom, 2 top in ggplot)
my_data_plot$CATEGORY_f <- factor(my_data_plot$CATEGORY, levels = c(1, 2))

# Colors in SAS order: first color for first group (Series B), second for Series A
fill_colors <- c("Series B" = "#9999FF", "Series A" = "#993366")

# -------------------------------------------------------------------------
# Write CSV of plotted data (and table body)
# SAS variable names: CATEGORY, SERIES, AMOUNT
utils::write.csv(my_data, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Text rendering for bar1.htm -> bar1.txt (and bar1.htm)
# SAS titles:
# title1 color=gray33 ls=0.5 h=23pt "Horizontal Grouped Bar";
# title2 color=gray33 ls=0.5 h=17pt "Compares values across categories";
txt_path <- file.path(out, "bar1.txt")
htm_path <- file.path(out, "bar1.htm")

# Build plain-text table rendering
txt_lines <- c(
  "Horizontal Grouped Bar",
  "Compares values across categories",
  "",
  "Data for Horizontal Grouped Bar (SGplot Horizontal Grouped Bar)",
  "---------------------------------------------------------------",
  sprintf("%-10s %-12s %8s", "CATEGORY", "SERIES", "AMOUNT"),
  sprintf("%-10s %-12s %8s", "--------", "--------", "------"),
  sprintf("%-10d %-12s %8.1f", 1L, "Series A", 5.0),
  sprintf("%-10d %-12s %8.1f", 2L, "Series A", 7.8),
  sprintf("%-10d %-12s %8.1f", 1L, "Series B", 9.5),
  sprintf("%-10d %-12s %8.1f", 2L, "Series B", 5.9),
  "",
  "XAXIS: 0 to 10 by 2 (minorcount=1), offsetmax=0, grid minor",
  "YAXIS: CATEGORY (1, 2), display=(noticks)",
  "HBAR: response=AMOUNT stat=sum group=SERIES groupdisplay=cluster grouporder=descending",
  "Style: datacolors=(cx9999ff cx993366) outlineattrs=(color=black) nostatlabel",
  "Style: htmlblue, imagefmt=png imagename=bar1 width=800px height=600px noborder"
)
writeLines(txt_lines, con = txt_path)

# Also write an HTML file (SAS ODS HTML body="bar1.htm" title="SGplot Horizontal Grouped Bar")
html_lines <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  '<meta charset="UTF-8">',
  "<title>SGplot Horizontal Grouped Bar</title>",
  "</head>",
  "<body>",
  '<h1 style="color:#333333; text-align:center; font-size:23pt; font-weight:normal;">Horizontal Grouped Bar</h1>',
  '<h2 style="color:#333333; text-align:center; font-size:17pt; font-weight:normal;">Compares values across categories</h2>',
  '<div style="text-align:center;">',
  '<img src="bar1.png" alt="Horizontal Grouped Bar" width="800" height="600" style="border:none;">',
  "</div>",
  "<br>",
  '<table border="1" cellpadding="4" cellspacing="0" style="margin:auto; border-collapse:collapse;">',
  "<caption>Data for Horizontal Grouped Bar</caption>",
  "<tr><th>CATEGORY</th><th>SERIES</th><th>AMOUNT</th></tr>",
  "<tr><td>1</td><td>Series A</td><td>5</td></tr>",
  "<tr><td>2</td><td>Series A</td><td>7.8</td></tr>",
  "<tr><td>1</td><td>Series B</td><td>9.5</td></tr>",
  "<tr><td>2</td><td>Series B</td><td>5.9</td></tr>",
  "</table>",
  "</body>",
  "</html>"
)
writeLines(html_lines, con = htm_path)

# -------------------------------------------------------------------------
# Figure: proc sgplot hbar
# xaxis values=(0 to 10 by 2) labelattrs=(size=16pt weight=bold color=gray33)
#       valueattrs=(size=16pt weight=bold color=gray33) offsetmax=0 grid minor minorcount=1
# yaxis labelattrs=(size=16pt weight=bold color=gray33) valueattrs=(size=16pt weight=bold color=gray33) display=(noticks)

library(ggplot2)

# SAS gray33 approx #333333 (SAS gray33) ; R gray33 is #545454, use SAS hex
col_gray33 <- "#333333"

p <- ggplot(my_data_plot, aes(x = AMOUNT, y = CATEGORY_f, fill = SERIES)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6, colour = "black", linewidth = 0.4) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_manual(values = fill_colors, breaks = c("Series B", "Series A")) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # htmlblue style approximation: white background, light grid
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold", colour = col_gray33),
    axis.text.y = element_text(size = 16, face = "bold", colour = col_gray33),
    axis.title.x = element_text(size = 16, face = "bold", colour = col_gray33),
    axis.title.y = element_text(size = 16, face = "bold", colour = col_gray33),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = col_gray33),
    plot.title = element_text(colour = col_gray33, size = 23, hjust = 0.5, face = "plain"),
    plot.subtitle = element_text(colour = col_gray33, size = 17, hjust = 0.5, face = "plain"),
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 10)
  )

# Save as 800px x 600px -> 8in x 6in at 100 dpi (noborder)
png_path <- file.path(out, "bar1.png")
ggplot2::ggsave(filename = png_path, plot = p, width = 8, height = 6, units = "in", dpi = 100, bg = "white")

# CSV of plotted data already written as bar1.csv; ensure it exists
stopifnot(file.exists(file.path(out, "bar1.csv")))
stopifnot(file.exists(png_path))
stopifnot(file.exists(txt_path))
```