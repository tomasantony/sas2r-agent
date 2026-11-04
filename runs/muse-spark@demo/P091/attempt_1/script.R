out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
})

# --- my_data : datalines ---
my_data <- data.frame(
  CATEGORY = c(1, 2, 1, 2),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9),
  stringsAsFactors = FALSE
)

# stat=sum per CATEGORY*SERIES (each cell has one obs, sum = amount)
plot_data <- aggregate(AMOUNT ~ CATEGORY + SERIES, data = my_data, FUN = sum)
names(plot_data) <- c("CATEGORY", "SERIES", "AMOUNT")
# SAS character ordering: uppercase before lowercase; SERIES already consistent
# grouporder=descending -> Series B then Series A
plot_data$SERIES <- factor(plot_data$SERIES, levels = c("Series B", "Series A"))
plot_data$CATEGORY <- factor(plot_data$CATEGORY, levels = c(1, 2))
plot_data <- plot_data[order(plot_data$CATEGORY, plot_data$SERIES), ]

# CSV of the table body / plotted data (SAS variable names, same case)
out_csv <- plot_data
names(out_csv) <- c("CATEGORY", "SERIES", "AMOUNT")
# keep numeric unrounded
write.csv(out_csv, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

# --- text rendering of bar1.htm ---
# ODS HTML title="SGplot Horizontal Grouped Bar", title1/title2 verbatim
txt_path <- file.path(out, "bar1.txt")
txt_lines <- c(
  "SGplot Horizontal Grouped Bar",
  "",
  "Horizontal Grouped Bar",
  "Compares values across categories",
  "",
  "Horizontal Grouped Bar - data (stat=sum of AMOUNT by CATEGORY and SERIES)",
  "",
  sprintf("%-10s %-12s %10s", "CATEGORY", "SERIES", "AMOUNT"),
  sprintf("%-10s %-12s %10s", "--------", "------------", "----------"),
  sprintf("%-10s %-12s %10s", "1", "Series A", "5"),
  sprintf("%-10s %-12s %10s", "2", "Series A", "7.8"),
  sprintf("%-10s %-12s %10s", "1", "Series B", "9.5"),
  sprintf("%-10s %-12s %10s", "2", "Series B", "5.9"),
  "",
  "XAXIS: values 0 to 10 by 2",
  "YAXIS: CATEGORY (1, 2) grouped by SERIES (cluster, grouporder=descending)",
  "Style: datacolors=(cx9999ff cx993366) outline=black, noautolegend"
)
writeLines(txt_lines, txt_path)

# --- figure: SGPLOT hbar category / response=amount stat=sum group=series ---
# colors: cx9999ff = #9999FF, cx993366 = #993366
# grouporder=descending: first group (Series B) gets first color
fill_vals <- c("Series B" = "#9999FF", "Series A" = "#993366")

p <- ggplot(plot_data, aes(y = CATEGORY, x = AMOUNT, fill = SERIES)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"),
           color = "black", width = 0.7) +
  scale_fill_manual(values = fill_vals, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = "AMOUNT",
    y = "CATEGORY"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 23, colour = "gray33"),
    plot.subtitle = element_text(size = 17, colour = "gray33"),
    axis.title = element_text(size = 16, face = "bold", colour = "gray33"),
    axis.text.x = element_text(size = 16, face = "bold", colour = "gray33"),
    axis.text.y = element_text(size = 16, face = "bold", colour = "gray33"),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey80"),
    panel.grid.minor.x = element_line(colour = "grey90"),
    panel.grid.major.y = element_line(colour = "grey80"),
    panel.border = element_blank()
  )

# 800px x 600px
ggsave(file.path(out, "bar1.png"), plot = p, width = 8, height = 6, dpi = 100, units = "in")
