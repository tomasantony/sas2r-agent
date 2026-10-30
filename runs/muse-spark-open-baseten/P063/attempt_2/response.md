```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(openxlsx)
})

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
target_st <- "AESTDY"
target_en <- "AEENDY"

# -------------------------------------------------------------------------
# proc sql: min(min(&target_st,1)) and max(max(&target_st,&target_en))
min_val <- min(pmin(target_dataest[[target_st]], 1), na.rm = TRUE)
row_max <- ifelse(is.na(target_dataest[[target_en]]),
                  target_dataest[[target_st]],
                  pmax(target_dataest[[target_st]], target_dataest[[target_en]], na.rm = TRUE))
max_val <- max(row_max, na.rm = TRUE)
range_val <- (max_val - min_val) + as.integer(0 < min_val)

# -------------------------------------------------------------------------
# data __wk1 logic for st/en and ar array
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

n <- nrow(wk1)
ar_mat <- matrix("", nrow = n, ncol = range_val)
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

# Day labels: from min to max excluding 0, length == range_val
day_seq <- seq(min_val, max_val)
day_seq <- day_seq[day_seq != 0]
day_labels <- as.character(day_seq)  # "-2","-1","1",...,"30"

# Build gant dataset as SAS reference expects:
# columns: ID, EVENT, STDY, ENDY, -2, -1, 1, 2, ..., 30
# values for day columns are ar values
gant_core <- tibble::tibble(
  ID = dummy$SUBJID,
  EVENT = dummy$AETERM,
  STDY = dummy$AESTDY,
  ENDY = dummy$AEENDY
)

ar_df_named <- as_tibble(ar_mat, .name_repair = "minimal")
colnames(ar_df_named) <- day_labels

gant <- bind_cols(gant_core, ar_df_named)

# Write dataset gant.csv with exact SAS column names
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Build Excel file equivalent to ODS EXCEL (optional, for completeness)
header <- c("ID", "Event", "STDY", "ENDY", day_labels)
excel_data <- gant_core
# need same header case as SAS ODS: "ID","Event","STDY","ENDY" + days
excel_data_full <- bind_cols(excel_data, ar_df_named)
colnames(excel_data_full) <- header

wb <- createWorkbook()
addWorksheet(wb, "GANT")

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

writeData(wb, "GANT", t(header), startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)
for (c in seq_along(header)) {
  st <- if (c <= 4) header_darkblue else header_blue
  addStyle(wb, "GANT", style = st, rows = 1, cols = c, gridExpand = FALSE, stack = TRUE)
}
writeData(wb, "GANT", excel_data_full, startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)

n_rows <- nrow(excel_data_full)
n_cols <- ncol(excel_data_full)
for (r in seq_len(n_rows)) {
  excel_row <- r + 1
  addStyle(wb, "GANT", style = style_normal, rows = excel_row, cols = 1, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_normal, rows = excel_row, cols = 2, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_normal_center, rows = excel_row, cols = 3, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_normal_center, rows = excel_row, cols = 4, gridExpand = FALSE, stack = TRUE)
  for (c in 5:n_cols) {
    j <- c - 4
    val <- ar_mat[r, j]
    st_cell <- if (val == "Y") style_red else if (val == "O") style_pink else style_white
    addStyle(wb, "GANT", style = st_cell, rows = excel_row, cols = c, gridExpand = FALSE, stack = TRUE)
  }
}
setColWidths(wb, "GANT", cols = 1, widths = 10)
setColWidths(wb, "GANT", cols = 2, widths = 20)
setColWidths(wb, "GANT", cols = 3:4, widths = 8)
if (n_cols >= 5) setColWidths(wb, "GANT", cols = 5:n_cols, widths = 3)
freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)
saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# Plain-text rendering
txt_path <- file.path(out, "gant.txt")
sink(txt_path)
cat("Gantt Chart Data\n")
print(as.data.frame(gant), row.names = FALSE)
sink()

# PNG visualization
suppressPackageStartupMessages(library(ggplot2))
plot_df <- dummy %>%
  mutate(
    row_id = row_number(),
    label = paste(SUBJID, AETERM, sep = " - "),
    end_plot = ifelse(is.na(AEENDY), max_val, AEENDY)
  )
p <- ggplot(plot_df) +
  geom_segment(aes(x = AESTDY, xend = end_plot, y = reorder(label, -row_id), yend = reorder(label, -row_id),
                   colour = is.na(AEENDY)), linewidth = 6) +
  geom_point(aes(x = AESTDY, y = reorder(label, -row_id)), size = 2) +
  scale_colour_manual(values = c("TRUE" = "pink", "FALSE" = "red"), guide = "none") +
  labs(title = "Gantt Chart of Adverse Events", x = "Study Day", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.y = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out, "gant.png"), plot = p, width = 10, height = 4, dpi = 150, bg = "white")
```