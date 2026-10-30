```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

dummy <- data.frame(
  SUBJID = c("AAA", "AAA", "BBB", "BBB", "BBB", "CCC"),
  AETERM = c("Adverse Event1", "Adverse Event2", "Adverse Event1", "Adverse Event2", "Adverse Event3", "Adverse Event1"),
  AESTDY = c(1, 3, 2, -2, 4, 15),
  AEENDY = c(5, 8, 30, 3, NA, 20),
  stringsAsFactors = FALSE
)

target_dataest <- dummy
target_id <- "SUBJID"
target_event_name <- "AETERM"
target_st <- "AESTDY"
target_en <- "AEENDY"

st_vals <- target_dataest[[target_st]]
en_vals <- target_dataest[[target_en]]

row_min <- pmin(st_vals, 1, na.rm = FALSE)
min_val <- min(row_min, na.rm = TRUE)

row_max <- suppressWarnings(pmax(st_vals, en_vals, na.rm = TRUE))
max_val <- max(row_max, na.rm = TRUE)

range_val <- abs(max_val - min_val) + as.integer(0 < min_val)

wk1 <- target_dataest

if (min_val < 1) {
  wk1$st <- wk1[[target_st]] - min_val + as.integer(wk1[[target_st]] < 1)
  wk1$en <- NA_integer_
  idx_notmiss <- !is.na(wk1[[target_en]])
  wk1$en[idx_notmiss] <- wk1[[target_en]][idx_notmiss] - min_val + as.integer(wk1[[target_en]][idx_notmiss] < 1)
} else {
  wk1$st <- wk1[[target_st]]
  wk1$en <- wk1[[target_en]]
}

n <- nrow(wk1)
ar_mat <- matrix(NA_character_, nrow = n, ncol = range_val)
colnames(ar_mat) <- paste0("ar", seq_len(range_val))

for (r in seq_len(n)) {
  st_r <- wk1$st[r]
  en_r <- wk1$en[r]
  for (i in seq_len(range_val)) {
    if (!is.na(en_r) && st_r <= i && i <= en_r) {
      ar_mat[r, i] <- "Y"
    } else if (is.na(en_r) && st_r == i) {
      ar_mat[r, i] <- "Y"
    } else if (is.na(en_r) && st_r <= i) {
      ar_mat[r, i] <- "O"
    } else {
      ar_mat[r, i] <- NA_character_
    }
  }
}

# Build SAS-equivalent gant dataset:
# Columns: ID, EVENT, STDY, ENDY, then one column per day (named "-2","-1","1",...,"30")
days_seq <- seq(min_val, max_val)
days_seq <- days_seq[days_seq != 0]
stopifnot(length(days_seq) == range_val)

# Map ar columns to day-named columns
# ar1 corresponds to days_seq[1], etc.
gant <- data.frame(
  ID = wk1[[target_id]],
  EVENT = wk1[[target_event_name]],
  STDY = wk1[[target_st]],
  ENDY = wk1[[target_en]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Add day columns in order
for (j in seq_along(days_seq)) {
  dname <- as.character(days_seq[j])
  gant[[dname]] <- ar_mat[, j]
}

# Ensure column order: ID, EVENT, STDY, ENDY, then days_seq as character
# gant already in that order

# Write dataset gant -> OUT_DIR/gant.csv
# Use check.names=FALSE already; write.csv will preserve names
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# Also create Excel file gant.xlsx mimicking ODS EXCEL with styling
header <- c("ID", "Event", "STDY", "ENDY", as.character(days_seq))

excel_body <- gant
# For Excel body, need same values but header uses "Event" capital E only for display
# excel_body column names for writing: use header
colnames(excel_body) <- header

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "GANT")

  hdr_darkblue <- openxlsx::createStyle(fgFill = "#00008B", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center", valign = "center", border = "TopBottomLeftRight")
  hdr_blue <- openxlsx::createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF", halign = "center", valign = "center", border = "TopBottomLeftRight")
  style_red <- openxlsx::createStyle(fgFill = "#FF0000", fontColour = "#FF0000")
  style_pink <- openxlsx::createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB")
  style_white <- openxlsx::createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF")
  style_default <- openxlsx::createStyle(border = "TopBottomLeftRight", halign = "center", valign = "center")

  openxlsx::writeData(wb, "GANT", t(header), startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)

  for (c in seq_along(header)) {
    st <- if (c <= 4) hdr_darkblue else hdr_blue
    openxlsx::addStyle(wb, "GANT", style = st, rows = 1, cols = c, gridExpand = FALSE, stack = TRUE)
  }

  if (nrow(excel_body) > 0) {
    openxlsx::writeData(wb, "GANT", excel_body, startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)
    openxlsx::addStyle(wb, "GANT", style = style_default, rows = 2:(nrow(excel_body)+1), cols = 1:ncol(excel_body), gridExpand = TRUE, stack = TRUE)
    if (ncol(excel_body) >= 5) {
      for (r in seq_len(nrow(excel_body))) {
        excel_row <- r + 1
        for (c in 5:ncol(excel_body)) {
          val <- excel_body[r, c]
          if (!is.na(val) && val == "Y") {
            openxlsx::addStyle(wb, "GANT", style = style_red, rows = excel_row, cols = c, gridExpand = FALSE, stack = TRUE)
          } else if (!is.na(val) && val == "O") {
            openxlsx::addStyle(wb, "GANT", style = style_pink, rows = excel_row, cols = c, gridExpand = FALSE, stack = TRUE)
          } else {
            openxlsx::addStyle(wb, "GANT", style = style_white, rows = excel_row, cols = c, gridExpand = FALSE, stack = TRUE)
          }
        }
      }
    }
  }

  openxlsx::setColWidths(wb, "GANT", cols = 1, widths = 10)
  openxlsx::setColWidths(wb, "GANT", cols = 2, widths = 20)
  openxlsx::setColWidths(wb, "GANT", cols = 3:4, widths = 8)
  if (ncol(excel_body) >= 5) {
    openxlsx::setColWidths(wb, "GANT", cols = 5:ncol(excel_body), widths = 3)
  }
  openxlsx::freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)

  openxlsx::saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)
} else {
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(list(GANT = rbind(header, excel_body)), path = file.path(out, "gant.xlsx"))
  }
}

# Plain-text rendering
txt_path <- file.path(out, "gant.txt")
con <- file(txt_path, open = "wt")
writeLines("Gantt Chart Data - Event Durations by Subject", con)
writeLines(paste0("Time range: ", min_val, " to ", max_val, " (excluding 0), range=", range_val), con)
writeLines("", con)
writeLines(paste(colnames(gant), collapse = " | "), con)
writeLines(paste(rep("-", 120), collapse = ""), con)
for (r in seq_len(nrow(gant))) {
  vals <- sapply(gant[r, ], function(x) ifelse(is.na(x), "", as.character(x)))
  writeLines(paste(vals, collapse = " | "), con)
}
close(con)
```