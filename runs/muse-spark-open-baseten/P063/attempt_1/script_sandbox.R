# R translation of P063_event_gant_excel.sas
# Produces Gantt dataset and Excel file equivalent to SAS ODS EXCEL output

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(openxlsx)
})

# helper: SAS round half away from zero (not needed here but kept for fidelity)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# -------------------------------------------------------------------------
# test_dummy_data (as in SAS macro)
dummy <- tibble::tibble(
  SUBJID = c("AAA", "AAA", "BBB", "BBB", "BBB", "CCC"),
  AETERM = c("Adverse Event1", "Adverse Event2", "Adverse Event1",
             "Adverse Event2", "Adverse Event3", "Adverse Event1"),
  AESTDY = c(1, 3, 2, -2, 4, 15),
  AEENDY = c(5, 8, 30, 3, NA_real_, 20)
)

target_dataest <- dummy
target_id <- "SUBJID"
target_event_name <- "AETERM"
target_st <- "AESTDY"
target_en <- "AEENDY"

# -------------------------------------------------------------------------
# proc sql: min(min(&target_st,1)) and max(max(&target_st,&target_en))
# SAS min/max ignore missing; pmin/pmax with na.rm handling
# min = min( pmin(AESTDY,1) )
min_val <- min(pmin(target_dataest[[target_st]], 1), na.rm = TRUE)

# max = max( max(AESTDY, AEENDY) ) per row, ignoring missing
row_max <- ifelse(is.na(target_dataest[[target_en]]),
                  target_dataest[[target_st]],
                  pmax(target_dataest[[target_st]], target_dataest[[target_en]], na.rm = TRUE))
max_val <- max(row_max, na.rm = TRUE)

range_val <- (max_val - min_val) + as.integer(0 < min_val)

# -------------------------------------------------------------------------
# data __wk1
# st = &target_st - &min + (&target_st<1)  if &min <1 else &target_st
# en = &target_en - &min + (&target_en<1) if not missing
wk1 <- target_dataest %>%
  mutate(
    st = ifelse(min_val < 1,
                .data[[target_st]] - min_val + as.integer(.data[[target_st]] < 1),
                .data[[target_st]]),
    en = ifelse(min_val < 1,
                ifelse(is.na(.data[[target_en]]), NA_real_,
                       .data[[target_en]] - min_val + as.integer(.data[[target_en]] < 1)),
                .data[[target_en]])
  )

# Build ar{range} array
n <- nrow(wk1)
ar_mat <- matrix("", nrow = n, ncol = range_val)
colnames(ar_mat) <- paste0("ar", seq_len(range_val))

for (r in seq_len(n)) {
  st_r <- wk1$st[r]
  en_r <- wk1$en[r]
  for (j in seq_len(range_val)) {
    if (!is.na(en_r) && st_r <= j && j <= en_r) {
      ar_mat[r, j] <- "Y"
    } else if (is.na(en_r) && st_r == j) {
      ar_mat[r, j] <- "Y"
    } else if (is.na(en_r) && st_r <= j) {
      ar_mat[r, j] <- "O"
    } else {
      ar_mat[r, j] <- ""
    }
  }
}

ar_df <- as_tibble(ar_mat)

gant <- bind_cols(wk1, ar_df)

# Keep columns as in SAS: st en &target_id &target_event_name &target_st &target_en ar:
# SAS keep order: st en SUBJID AETERM AESTDY AEENDY ar:
# Reorder to match SAS keep list
gant <- gant %>%
  select(st, en, all_of(c(target_id, target_event_name, target_st, target_en)), starts_with("ar"))

# Write dataset gant.csv (as expected output)
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Build Excel file equivalent to ODS EXCEL with Report Writing Interface
# Header days: from min to max excluding 0
day_seq <- seq(min_val, max_val)
day_seq <- day_seq[day_seq != 0]
day_labels <- as.character(day_seq)  # length should equal range_val

# Prepare sheet data: header + rows
header <- c("ID", "Event", "STDY", "ENDY", day_labels)

# Data rows for Excel: ID, Event, STDY, ENDY, ar values
excel_data <- tibble(
  ID = dummy$SUBJID,
  Event = dummy$AETERM,
  STDY = dummy$AESTDY,
  ENDY = dummy$AEENDY
) %>%
  bind_cols(as_tibble(ar_mat, .name_repair = "minimal"))
colnames(excel_data) <- header

wb <- createWorkbook()
addWorksheet(wb, "GANT")

# Styles
header_darkblue <- createStyle(fgFill = "#00008B", fontColour = "#FFFFFF",
                               textDecoration = "bold", halign = "center", valign = "center",
                               border = "TopBottomLeftRight", borderStyle = "thin")
header_blue <- createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF",
                           textDecoration = "bold", halign = "center", valign = "center",
                           border = "TopBottomLeftRight", borderStyle = "thin")
style_red <- createStyle(fgFill = "#FF0000", fontColour = "#FF0000",
                         halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
style_pink <- createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB",
                          halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
style_white <- createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF",
                           halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
style_normal <- createStyle(halign = "left", valign = "center",
                            border = "TopBottomLeftRight", borderStyle = "thin")
style_normal_center <- createStyle(halign = "center", valign = "center",
                                    border = "TopBottomLeftRight", borderStyle = "thin")

# Write header
writeData(wb, "GANT", t(header), startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)

# Apply header styles: first 4 columns darkblue, rest blue
for (c in seq_along(header)) {
  st <- if (c <= 4) header_darkblue else header_blue
  addStyle(wb, "GANT", style = st, rows = 1, cols = c, gridExpand = FALSE, stack = TRUE)
}

# Write data rows
writeData(wb, "GANT", excel_data, startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)

# Apply cell styles for data area
n_rows <- nrow(excel_data)
n_cols <- ncol(excel_data)
for (r in seq_len(n_rows)) {
  excel_row <- r + 1  # offset for header
  # ID, Event, STDY, ENDY columns
  addStyle(wb, "GANT", style = style_normal, rows = excel_row, cols = 1, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_normal, rows = excel_row, cols = 2, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_normal_center, rows = excel_row, cols = 3, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_normal_center, rows = excel_row, cols = 4, gridExpand = FALSE, stack = TRUE)
  # ar columns
  for (c in 5:n_cols) {
    j <- c - 4  # ar index 1..range
    val <- ar_mat[r, j]
    st_cell <- if (val == "Y") style_red else if (val == "O") style_pink else style_white
    addStyle(wb, "GANT", style = st_cell, rows = excel_row, cols = c, gridExpand = FALSE, stack = TRUE)
  }
}

# Column widths
setColWidths(wb, "GANT", cols = 1, widths = 10)
setColWidths(wb, "GANT", cols = 2, widths = 20)
setColWidths(wb, "GANT", cols = 3:4, widths = 8)
if (n_cols >= 5) setColWidths(wb, "GANT", cols = 5:n_cols, widths = 3)

# Freeze header
freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)

# Save Excel to OUT_DIR (as gant.xlsx) - matches SAS outpath/outfile
saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# -------------------------------------------------------------------------
# Also write plain-text rendering of the Gantt table (for verification)
txt_path <- file.path(out, "gant.txt")
sink(txt_path)
cat("Gantt Chart Data (derived from event_gant_excel macro)\n")
cat("=====================================================\n\n")
cat(sprintf("Time range: min=%d max=%d range=%d (0 excluded)\n\n", min_val, max_val, range_val))
cat("Header: ID | Event | STDY | ENDY | Days (", paste(day_labels, collapse = " "), ")\n")
cat(paste(rep("-", 120), collapse = ""), "\n")
# Print gant dataset preview
print(as.data.frame(gant), row.names = FALSE)
cat("\n\nLegend: Y = event day (red), O = ongoing (pink), blank = no event\n")
sink()

# CSV of plotted data already written as gant.csv; also write excel_data for completeness
write.csv(excel_data, file.path(out, "gant_plot_data.csv"), row.names = FALSE, na = "")

# Create a PNG visualization of the Gantt chart using ggplot2
suppressPackageStartupMessages(library(ggplot2))
# Prepare long format for plotting
plot_df <- dummy %>%
  mutate(
    row_id = row_number(),
    label = paste(SUBJID, AETERM, sep = " - "),
    end_plot = ifelse(is.na(AEENDY), max_val, AEENDY)
  )

# For ongoing, use arrow
p <- ggplot(plot_df) +
  geom_segment(aes(x = AESTDY, xend = end_plot, y = reorder(label, -row_id), yend = reorder(label, -row_id),
                   colour = is.na(AEENDY)), linewidth = 6) +
  geom_point(aes(x = AESTDY, y = reorder(label, -row_id)), size = 2) +
  scale_colour_manual(values = c("TRUE" = "pink", "FALSE" = "red"), guide = "none") +
  labs(title = "Gantt Chart of Adverse Events",
       x = "Study Day", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out, "gant.png"), plot = p, width = 10, height = 4, dpi = 150, bg = "white")
