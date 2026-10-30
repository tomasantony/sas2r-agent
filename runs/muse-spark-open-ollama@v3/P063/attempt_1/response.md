```r
# P063_event_gant_excel.sas -> R translation
# Generates Gantt chart data and Excel file from dummy AE data

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tibble)
library(openxlsx)

# ------------------------------------------------------------------
# 1. test_dummy_data (as in SAS macro)
# ------------------------------------------------------------------
dummy <- tibble(
  SUBJID = c("AAA", "AAA", "BBB", "BBB", "BBB", "CCC"),
  AETERM = c("Adverse Event1", "Adverse Event2", "Adverse Event1", "Adverse Event2", "Adverse Event3", "Adverse Event1"),
  AESTDY = c(1, 3, 2, -2, 4, 15),
  AEENDY = c(5, 8, 30, 3, NA_real_, 20)
)

target_dataest <- dummy
target_id <- "SUBJID"
target_event_name <- "AETERM"
target_st <- "AESTDY"
target_en <- "AEENDY"

# ------------------------------------------------------------------
# 2. proc sql: min(min(target_st,1)) and max(max(target_st,target_en))
# SAS missing is treated as smaller than any number for MAX
# ------------------------------------------------------------------
# min = min( pmin(AESTDY,1) )
min_val <- min(pmin(target_dataest[[target_st]], 1, na.rm = FALSE), na.rm = TRUE)

# max = max( pmax(AESTDY, AEENDY) ) where missing AEENDY is ignored (SAS MAX returns non-missing)
# pmax with na.rm=TRUE mimics SAS MAX
max_val <- max(pmax(target_dataest[[target_st]], target_dataest[[target_en]], na.rm = TRUE), na.rm = TRUE)

# range = range(min,max) + (0 < min)  ; SAS RANGE = max-min
range_val <- (max_val - min_val) + as.integer(0 < min_val)

# ------------------------------------------------------------------
# 3. data __wk1  (array ar{range} $1.)
# ------------------------------------------------------------------
wk1_base <- target_dataest %>%
  mutate(
    st = ifelse(min_val < 1,
                .data[[target_st]] - min_val + as.integer(.data[[target_st]] < 1),
                .data[[target_st]]),
    en = ifelse(min_val < 1,
                ifelse(is.na(.data[[target_en]]), NA_real_, .data[[target_en]] - min_val + as.integer(.data[[target_en]] < 1)),
                .data[[target_en]])
  )

# Build ar1..ar_range matrix
n <- nrow(wk1_base)
ar_mat <- matrix("", nrow = n, ncol = range_val)
colnames(ar_mat) <- paste0("ar", seq_len(range_val))

for (i in seq_len(range_val)) {
  for (r in seq_len(n)) {
    st_r <- wk1_base$st[r]
    en_r <- wk1_base$en[r]
    if (!is.na(en_r) && !is.na(st_r) && st_r <= i && i <= en_r) {
      ar_mat[r, i] <- "Y"
    } else if (!is.na(st_r) && st_r == i && is.na(en_r)) {
      ar_mat[r, i] <- "Y"
    } else if (!is.na(st_r) && st_r <= i && is.na(en_r)) {
      ar_mat[r, i] <- "O"
    } else {
      ar_mat[r, i] <- ""
    }
  }
}

wk1 <- bind_cols(
  wk1_base %>% select(st, en, all_of(c(target_id, target_event_name, target_st, target_en))),
  as_tibble(ar_mat)
)

# ------------------------------------------------------------------
# 4. Dataset gant -> OUT_DIR/gant.csv
# Keep st en target_id target_event_name target_st target_en ar:
# This is the SAS __wk1 dataset; also the source for the Excel Gantt
# ------------------------------------------------------------------
gant <- wk1

# Ensure column order matches SAS keep: st en SUBJID AETERM AESTDY AEENDY ar1..arN
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 5. Build Excel file gant.xlsx (ODS EXCEL equivalent)
# Header: ID, Event, STDY, ENDY, then day numbers from min to max excluding 0
# ------------------------------------------------------------------
day_seq <- seq(min_val, max_val)
day_labels <- as.character(day_seq[day_seq != 0])  # 32 labels: -2,-1,1..30

header_row <- c("ID", "Event", "STDY", "ENDY", day_labels)

# Data rows for Excel: SUBJID, AETERM, AESTDY, AEENDY, ar1..ar_range
excel_data <- tibble(
  ID = gant$SUBJID,
  Event = gant$AETERM,
  STDY = gant$AESTDY,
  ENDY = gant$AEENDY
) %>%
  bind_cols(as_tibble(ar_mat))

# Column names for Excel sheet: use header_row
colnames(excel_data) <- header_row

wb <- createWorkbook()
addWorksheet(wb, "GANT")

# Styles
header_darkblue <- createStyle(fgFill = "#00008B", fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000")
header_blue <- createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000")
style_red <- createStyle(fgFill = "#FF0000", fontColour = "#FF0000", halign = "center")
style_pink <- createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB", halign = "center")
style_white <- createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF", halign = "center")
style_default <- createStyle(halign = "left", valign = "center")

# Write header and data
writeData(wb, "GANT", t(header_row), startCol = 1, startRow = 1, colNames = FALSE, rowNames = FALSE)

# Write data rows starting at row 2
writeData(wb, "GANT", excel_data, startCol = 1, startRow = 2, colNames = FALSE, rowNames = FALSE)

# Apply header styles: first 4 columns darkblue, day columns blue
addStyle(wb, "GANT", header_darkblue, rows = 1, cols = 1:4, gridExpand = TRUE)
if (length(day_labels) > 0) {
  addStyle(wb, "GANT", header_blue, rows = 1, cols = 5:(4 + length(day_labels)), gridExpand = TRUE)
}

# Apply cell styles for Gantt area (rows 2..n+1, cols 5..)
# We need to color each ar cell based on Y/O/blank
for (r in seq_len(n)) {
  excel_row <- r + 1  # +1 for header
  for (c in seq_len(range_val)) {
    excel_col <- 4 + c
    val <- ar_mat[r, c]
    if (val == "Y") {
      addStyle(wb, "GANT", style_red, rows = excel_row, cols = excel_col, gridExpand = FALSE)
    } else if (val == "O") {
      addStyle(wb, "GANT", style_pink, rows = excel_row, cols = excel_col, gridExpand = FALSE)
    } else {
      addStyle(wb, "GANT", style_white, rows = excel_row, cols = excel_col, gridExpand = FALSE)
    }
  }
}

# Column widths
setColWidths(wb, "GANT", cols = 1, widths = 10)
setColWidths(wb, "GANT", cols = 2, widths = 20)
setColWidths(wb, "GANT", cols = 3:4, widths = 8)
if (length(day_labels) > 0) {
  setColWidths(wb, "GANT", cols = 5:(4 + length(day_labels)), widths = 4)
}
setRowHeights(wb, "GANT", rows = 1, heights = 15)

# Freeze panes at header
freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)

saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# ------------------------------------------------------------------
# 6. Plain-text rendering of the Gantt table (for OUT_DIR/gant.txt)
# ------------------------------------------------------------------
# Build a text table showing ID, Event, STDY, ENDY and day columns with Y/O
txt_file <- file.path(out, "gant.txt")
sink(txt_file)

cat("Gantt Chart of Adverse Events (Study Days)\n")
cat("===========================================\n\n")
cat(sprintf("Time range: %d to %d (excluding 0), %d days\n", min_val, max_val, range_val))
cat(sprintf("Subjects: %d, Events: %d\n\n", length(unique(gant$SUBJID)), nrow(gant)))

# Print header
hdr_fmt <- paste0("%-6s %-18s %4s %4s ", paste(rep("%3s", length(day_labels)), collapse = " "), "\n")
cat(sprintf(hdr_fmt, "ID", "Event", "STDY", "ENDY", day_labels))

# Separator
cat(paste(rep("-", 6 + 1 + 18 + 1 + 4 + 1 + 4 + 1 + length(day_labels)*4), collapse = ""), "\n")

for (r in seq_len(n)) {
  # Format ENDY as blank if NA
  endy_str <- ifelse(is.na(gant$AEENDY[r]), "", as.character(gant$AEENDY[r]))
  # Build row values: ID, Event, STDY, ENDY, then ar values (blank shown as .)
  ar_vals <- ar_mat[r, ]
  ar_disp <- ifelse(ar_vals == "", ".", ar_vals)
  # Use cat with formatting
  cat(sprintf("%-6s %-18s %4d %4s ", gant$SUBJID[r], substr(gant$AETERM[r], 1, 18), gant$AESTDY[r], endy_str))
  cat(paste(sprintf("%3s", ar_disp), collapse = " "))
  cat("\n")
}
cat("\nLegend: Y = event duration (red), O = ongoing after start (pink), . = no event\n")
cat("Note: Day 0 excluded; ongoing events show Y at start day then O thereafter.\n")

sink()

# Also write a CSV of the table body (same as gant.csv already, but ensure txt counterpart exists)
# gant.csv already written; no additional action needed
```