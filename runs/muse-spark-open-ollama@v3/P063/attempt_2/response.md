```r
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
# ------------------------------------------------------------------
min_val <- min(pmin(target_dataest[[target_st]], 1, na.rm = FALSE), na.rm = TRUE)
max_val <- max(pmax(target_dataest[[target_st]], target_dataest[[target_en]], na.rm = TRUE), na.rm = TRUE)
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

# ------------------------------------------------------------------
# 4. Build gant dataset as SAS does for the Excel sheet
#    Columns: ID, EVENT, STDY, ENDY, then day columns -2,-1,1..30
#    This matches SAS output dataset gant (36 columns)
# ------------------------------------------------------------------
day_seq <- seq(min_val, max_val)
day_labels <- as.character(day_seq[day_seq != 0])  # "-2","-1","1",...,"30"  (32 columns)

# Create data frame with correct SAS column names
gant_core <- data.frame(
  ID    = wk1_base$SUBJID,
  EVENT = wk1_base$AETERM,
  STDY  = wk1_base$AESTDY,
  ENDY  = wk1_base$AEENDY,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Add day columns in order, values from ar_mat
day_df <- as.data.frame(ar_mat, stringsAsFactors = FALSE, check.names = FALSE)
colnames(day_df) <- day_labels

gant <- cbind(gant_core, day_df)

# Write dataset gant -> OUT_DIR/gant.csv  (SAS dataset variable names, dates ISO, na="")
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 5. Build Excel file gant.xlsx (ODS EXCEL equivalent)
# ------------------------------------------------------------------
header_row <- c("ID", "Event", "STDY", "ENDY", day_labels)

excel_data <- data.frame(
  ID    = gant$ID,
  Event = gant$EVENT,
  STDY  = gant$STDY,
  ENDY  = gant$ENDY,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
excel_data <- cbind(excel_data, day_df)
colnames(excel_data) <- header_row

wb <- createWorkbook()
addWorksheet(wb, "GANT")

header_darkblue <- createStyle(fgFill = "#00008B", fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000")
header_blue     <- createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000")
style_red   <- createStyle(fgFill = "#FF0000", fontColour = "#FF0000", halign = "center")
style_pink  <- createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB", halign = "center")
style_white <- createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF", halign = "center")

writeData(wb, "GANT", t(header_row), startCol = 1, startRow = 1, colNames = FALSE, rowNames = FALSE)
writeData(wb, "GANT", excel_data, startCol = 1, startRow = 2, colNames = FALSE, rowNames = FALSE)

addStyle(wb, "GANT", header_darkblue, rows = 1, cols = 1:4, gridExpand = TRUE)
if (length(day_labels) > 0) {
  addStyle(wb, "GANT", header_blue, rows = 1, cols = 5:(4 + length(day_labels)), gridExpand = TRUE)
}

for (r in seq_len(n)) {
  excel_row <- r + 1
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

setColWidths(wb, "GANT", cols = 1, widths = 10)
setColWidths(wb, "GANT", cols = 2, widths = 20)
setColWidths(wb, "GANT", cols = 3:4, widths = 8)
if (length(day_labels) > 0) {
  setColWidths(wb, "GANT", cols = 5:(4 + length(day_labels)), widths = 4)
}
setRowHeights(wb, "GANT", rows = 1, heights = 15)
freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)

saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# ------------------------------------------------------------------
# 6. Plain-text rendering
# ------------------------------------------------------------------
txt_file <- file.path(out, "gant.txt")
sink(txt_file)
cat("Gantt Chart of Adverse Events (Study Days)\n")
cat("===========================================\n\n")
cat(sprintf("Time range: %d to %d (excluding 0), %d days\n", min_val, max_val, range_val))
cat(sprintf("Subjects: %d, Events: %d\n\n", length(unique(gant$ID)), nrow(gant)))
# header line
cat(paste(c("ID", "Event", "STDY", "ENDY", day_labels), collapse = " | "))
cat("\n")
cat(paste(rep("-", 120), collapse = ""))
cat("\n")
for (r in seq_len(n)) {
  endy_str <- ifelse(is.na(gant$ENDY[r]), "", as.character(gant$ENDY[r]))
  ar_vals <- as.character(day_df[r, ])
  ar_disp <- ifelse(ar_vals == "", ".", ar_vals)
  cat(paste(c(gant$ID[r], gant$EVENT[r], as.character(gant$STDY[r]), endy_str, ar_disp), collapse = " | "))
  cat("\n")
}
cat("\nLegend: Y = event duration (red), O = ongoing after start (pink), . = no event\n")
cat("Note: Day 0 excluded; ongoing events show Y at start day then O thereafter.\n")
sink()
```