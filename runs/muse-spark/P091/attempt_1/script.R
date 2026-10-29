out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(ggplot2))

# SAS round half away from zero (for displayed values if needed)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# --- data my_data : datalines ---
# input CATEGORY SERIES $ 3-11 AMOUNT;
my_data <- data.frame(
  CATEGORY = c(1, 2, 1, 2),
  SERIES   = c("Series A", "Series A", "Series B", "Series B"),
  AMOUNT   = c(5, 7.8, 9.5, 5.9),
  stringsAsFactors = FALSE
)

# --- PROC SGPLOT stat=sum : aggregate response by category*group ---
plot_data <- aggregate(AMOUNT ~ CATEGORY + SERIES, data = my_data, FUN = sum)
# grouporder=descending -> descending order of SERIES formatted values
# SERIES descending: "Series B" then "Series A"
plot_data$SERIES <- factor(plot_data$SERIES, levels = c("Series B", "Series A"))
plot_data$CATEGORY <- factor(plot_data$CATEGORY, levels = c("1", "2"))
# order rows by CATEGORY, SERIES descending to mirror SAS processing
plot_data <- plot_data[order(plot_data$CATEGORY, as.character(plot_data$SERIES), decreasing = FALSE), ]
# ensure Series B first within each category (descending)
plot_data <- plot_data[order(as.integer(plot_data$CATEGORY), xtfrm(plot_data$SERIES)), ]

message("my_data rows: ", nrow(my_data))

# --- SGPLOT hbar horizontal grouped bar ---
# styleattrs datacolors=(cx9999ff cx993366)
fill_colors <- c("Series B" = "#9999FF", "Series A" = "#993366")

p <- ggplot(plot_data, aes(x = AMOUNT, y = CATEGORY, fill = SERIES)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6,
           colour = "black", linewidth = 0.5) +
  scale_fill_manual(values = fill_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 2),
    minor_breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Horizontal Grouped Bar",
    subtitle = "Compares values across categories",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 23, colour = "gray33", hjust = 0.5, face = "plain"),
    plot.subtitle = element_text(size = 17, colour = "gray33", hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 16, face = "bold", colour = "gray33"),
    axis.text.x = element_text(size = 16, face = "bold", colour = "gray33"),
    axis.text.y = element_text(size = 16, face = "bold", colour = "gray33"),
    panel.grid.major.x = element_line(colour = "grey75", linewidth = 0.5),
    panel.grid.minor.x = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# width=800px height=600px noborder -> 8x6 inches at 100 dpi
ggsave(file.path(out, "bar1.png"), plot = p, width = 8, height = 6, dpi = 100, units = "in", bg = "white")

# --- CSV of plotted data (stat=sum values actually drawn) ---
plot_out <- data.frame(
  CATEGORY = as.integer(as.character(plot_data$CATEGORY)),
  SERIES = as.character(plot_data$SERIES),
  AMOUNT = plot_data$AMOUNT,
  stringsAsFactors = FALSE
)
write.csv(plot_out, file.path(out, "bar1_plotdata.csv"), row.names = FALSE, na = "")
write.csv(plot_out, file.path(out, "bar1_data.csv"), row.names = FALSE, na = "")
# table body CSV for bar1.htm counterpart (same underlying table)
write.csv(plot_out, file.path(out, "bar1.csv"), row.names = FALSE, na = "")

# --- text rendering for bar1.htm (ODS HTML SGplot page) ---
txt_file <- file.path(out, "bar1.txt")
con <- file(txt_file, open = "wt")
writeLines("SGplot Horizontal Grouped Bar", con)
writeLines("Horizontal Grouped Bar", con)
writeLines("Compares values across categories", con)
writeLines("", con)
writeLines("Horizontal Grouped Bar - data values (stat=sum, group=series, cluster)", con)
writeLines("CATEGORY  SERIES     AMOUNT", con)
writeLines("--------  --------  ------", con)
for (i in seq_len(nrow(plot_out))) {
  writeLines(sprintf("%-8d  %-8s  %6s",
                     plot_out$CATEGORY[i],
                     plot_out$SERIES[i],
                     format(plot_out$AMOUNT[i], nsmall = 1)), con)
}
writeLines("", con)
writeLines("XAXIS values 0 to 10 by 2 (minorcount=1), offsetmax=0, grid minor", con)
writeLines("Figure: see bar1.png (800x600, datacolors #9999FF #993366, outline black, noautolegend)", con)
close(con)

message("done")
