```r
# P063_event_gant_excel.sas -> R translation
# Produces Gantt chart data (dummy test data) as CSV + Excel

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but included for fidelity)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# -------------------------------------------------------------------------
# test_dummy_data (as in SAS macro)
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

# -------------------------------------------------------------------------
# proc sql: min(min(&target_st,1)) and max(max(&target_st,&target_en))
# SAS min() with missing: missing is ignored for max, but min(AESTDY,1) never missing
# Compute row-wise
st_vals <- target_dataest[[target_st]]
en_vals <- target_dataest[[target_en]]

# min = min over rows of min(AESTDY,1)
row_min <- pmin(st_vals, 1, na.rm = FALSE)
min_val <- min(row_min, na.rm = TRUE)

# max = max over rows of max(AESTDY, AEENDY)
# SAS max(AESTDY, AEENDY) returns non-missing if one is missing
row_max <- suppressWarnings(pmax(st_vals, en_vals, na.rm = TRUE))
max_val <- max(row_max, na.rm = TRUE)

# range = abs(max-min) + (0 < min)
range_val <- abs(max_val - min_val) + as.integer(0 < min_val)

# -------------------------------------------------------------------------
# data __wk1
# array ar{&range.} $1.
# if &min < 1 then st = &target_st - &min + (&target_st<1) etc.
wk1 <- target_dataest

if (min_val < 1) {
  wk1$st <- wk1[[target_st]] - min_val + as.integer(wk1[[target_st]] < 1)
  # en: only when not missing
  wk1$en <- NA_integer_
  idx_notmiss <- !is.na(wk1[[target_en]])
  wk1$en[idx_notmiss] <- wk1[[target_en]][idx_notmiss] - min_val + as.integer(wk1[[target_en]][idx_notmiss] < 1)
} else {
  wk1$st <- wk1[[target_st]]
  wk1$en <- wk1[[target_en]]
}

# Build ar columns: ar1 .. ar{range}
# SAS logic:
# do i=1 to &range;
#   if st <=i <=en then ar{i}="Y";
#   else if st=i and missing(en) then ar{i}="Y";
#   else if st<=i and missing(en) then ar{i}="O";
# end;
# Note: missing(en) rows have en=NA
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

ar_df <- as.data.frame(ar_mat, stringsAsFactors = FALSE)

# keep st en &target_id &target_event_name &target_st &target_en ar:
gant <- cbind(
  wk1[, c("st", "en", target_id, target_event_name, target_st, target_en)],
  ar_df
)

# Ensure column order exactly as SAS keep
# st en SUBJID AETERM AESTDY AEENDY ar1..arN
# Already in that order

# -------------------------------------------------------------------------
# Write dataset gant -> OUT_DIR/gant.csv
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Also create Excel file gant.xlsx mimicking ODS EXCEL with styling
# Header days: from min to max excluding 0
days_seq <- seq(min_val, max_val)
days_seq <- days_seq[days_seq != 0]
stopifnot(length(days_seq) == range_val)

# Build header row for Excel: ID, Event, STDY, ENDY, then days
header <- c("ID", "Event", "STDY", "ENDY", as.character(days_seq))

# Prepare data for Excel body: SUBJID, AETERM, AESTDY, AEENDY, ar values
excel_body <- cbind(
  wk1[, c(target_id, target_event_name, target_st, target_en)],
  ar_df
)
colnames(excel_body) <- header

# Create workbook with openxlsx styling
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "GANT")

  # Styles
  hdr_darkblue <- openxlsx::createStyle(fgFill = "#00008B", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center", valign = "center", border = "TopBottomLeftRight")
  hdr_blue <- openxlsx::createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF", halign = "center", valign = "center", border = "TopBottomLeftRight")
  style_red <- openxlsx::createStyle(fgFill = "#FF0000", fontColour = "#FF0000")
  style_pink <- openxlsx::createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB")
  style_white <- openxlsx::createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF")
  style_default <- openxlsx::createStyle(border = "TopBottomLeftRight", halign = "center", valign = "center")

  # Write header
  openxlsx::writeData(wb, "GANT", t(header), startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)

  # Apply header styles: first 4 columns darkblue, rest blue
  for (c in seq_along(header)) {
    st <- if (c <= 4) hdr_darkblue else hdr_blue
    openxlsx::addStyle(wb, "GANT", style = st, rows = 1, cols = c, gridExpand = FALSE, stack = TRUE)
  }

  # Write body
  if (nrow(excel_body) > 0) {
    openxlsx::writeData(wb, "GANT", excel_body, startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)
    # Apply default borders
    openxlsx::addStyle(wb, "GANT", style = style_default, rows = 2:(nrow(excel_body)+1), cols = 1:ncol(excel_body), gridExpand = TRUE, stack = TRUE)
    # Apply cell colors for ar columns (cols 5..ncol)
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

  # Column widths: first 4 wider, day columns narrow
  openxlsx::setColWidths(wb, "GANT", cols = 1, widths = 10)
  openxlsx::setColWidths(wb, "GANT", cols = 2, widths = 20)
  openxlsx::setColWidths(wb, "GANT", cols = 3:4, widths = 8)
  if (ncol(excel_body) >= 5) {
    openxlsx::setColWidths(wb, "GANT", cols = 5:ncol(excel_body), widths = 3)
  }
  openxlsx::freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)

  openxlsx::saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)
} else {
  # Fallback: write simple Excel via writexl if openxlsx not available
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(list(GANT = rbind(header, excel_body)), path = file.path(out, "gant.xlsx"))
  }
}

# -------------------------------------------------------------------------
# Plain-text rendering for table output (required for table types)
txt_path <- file.path(out, "gant.txt")
con <- file(txt_path, open = "wt")
writeLines("Gantt Chart Data - Event Durations by Subject", con)
writeLines(paste0("Time range: ", min_val, " to ", max_val, " (excluding 0), range=", range_val), con)
writeLines("", con)
# Print header and rows as fixed-width table
# Use capture.output on the gant data frame
writeLines(paste(c("st", "en", "SUBJID", "AETERM", "AESTDY", "AEENDY", paste0("ar", seq_len(range_val))), collapse = " | "), con)
writeLines(paste(rep("-", 120), collapse = ""), con)
for (r in seq_len(nrow(gant))) {
  vals <- sapply(gant[r, ], function(x) ifelse(is.na(x), "", as.character(x)))
  writeLines(paste(vals, collapse = " | "), con)
}
close(con)
```