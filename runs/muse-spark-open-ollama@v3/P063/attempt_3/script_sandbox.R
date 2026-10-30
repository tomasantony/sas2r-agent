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

# ------------------------------------------------------------------
# 2. proc sql: min(min(target_st,1)) and max(max(target_st,target_en))
# ------------------------------------------------------------------
min_val <- min(pmin(target_dataest$AESTDY, 1, na.rm = FALSE), na.rm = TRUE)
max_val <- max(pmax(target_dataest$AESTDY, target_dataest$AEENDY, na.rm = TRUE), na.rm = TRUE)
range_val <- (max_val - min_val) + as.integer(0 < min_val)

# day labels as in SAS header: from min to max excluding 0
day_seq <- seq(min_val, max_val)
day_labels <- as.character(day_seq[day_seq != 0])  # "-2","-1","1",...,"30"
day_nums  <- as.integer(day_labels)

n <- nrow(dummy)

# ------------------------------------------------------------------
# 3. Build gant dataset directly from day numbers (avoids st/en mapping bugs)
#    SAS blank is " " (single space), not "".
# ------------------------------------------------------------------
gant_core <- data.frame(
  ID    = dummy$SUBJID,
  EVENT = dummy$AETERM,
  STDY  = dummy$AESTDY,
  ENDY  = dummy$AEENDY,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# matrix of day values, initialized to " " (SAS missing character)
day_mat <- matrix(" ", nrow = n, ncol = length(day_labels))
colnames(day_mat) <- day_labels

for (r in seq_len(n)) {
  aestdy <- dummy$AESTDY[r]
  aeendy <- dummy$AEENDY[r]
  for (c in seq_along(day_nums)) {
    d <- day_nums[c]
    if (!is.na(aeendy)) {
      if (aestdy <= d && d <= aeendy) day_mat[r, c] <- "Y"
    } else {
      # missing end date = ongoing
      if (aestdy == d) day_mat[r, c] <- "Y"
      else if (aestdy < d) day_mat[r, c] <- "O"
    }
  }
}

day_df <- as.data.frame(day_mat, stringsAsFactors = FALSE, check.names = FALSE)

gant <- cbind(gant_core, day_df)

# Write dataset gant -> OUT_DIR/gant.csv  (SAS dataset variable names, na="")
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 4. Build Excel file gant.xlsx (ODS EXCEL equivalent)
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
  for (c in seq_along(day_labels)) {
    excel_col <- 4 + c
    val <- day_mat[r, c]
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
# 5. Plain-text rendering
# ------------------------------------------------------------------
txt_file <- file.path(out, "gant.txt")
sink(txt_file)
cat("Gantt Chart of Adverse Events (Study Days)\n")
cat("===========================================\n\n")
cat(sprintf("Time range: %d to %d (excluding 0), %d days\n", min_val, max_val, length(day_labels)))
cat(sprintf("Subjects: %d, Events: %d\n\n", length(unique(gant$ID)), nrow(gant)))
cat(paste(c("ID", "Event", "STDY", "ENDY", day_labels), collapse = " | "))
cat("\n")
cat(paste(rep("-", 120), collapse = ""))
cat("\n")
for (r in seq_len(n)) {
  endy_str <- ifelse(is.na(gant$ENDY[r]), "", as.character(gant$ENDY[r]))
  ar_vals <- as.character(day_df[r, ])
  ar_disp <- ifelse(ar_vals == " ", ".", ar_vals)
  cat(paste(c(gant$ID[r], gant$EVENT[r], as.character(gant$STDY[r]), endy_str, ar_disp), collapse = " | "))
  cat("\n")
}
cat("\nLegend: Y = event duration (red), O = ongoing after start (pink), . = no event\n")
cat("Note: Day 0 excluded; ongoing events show Y at start day then O thereafter.\n")
sink()
