```r
# P063_event_gant_excel.sas -> R translation
# Reproduces SAS %event_gant_excel with built-in dummy data
# Expected dataset `gant` is the Excel sheet GANT content:
#   ID, EVENT, STDY, ENDY + one column per study day (min..max excluding 0)
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
  library(ggplot2)
})

# ---- test_dummy_data (data dummy) ----
dummy <- data.frame(
  SUBJID = c("AAA","AAA","BBB","BBB","BBB","CCC"),
  AETERM = c("Adverse Event1","Adverse Event2","Adverse Event1","Adverse Event2","Adverse Event3","Adverse Event1"),
  AESTDY = c(1,3,2,-2,4,15),
  AEENDY = c(5,8,30,3,NA,20),
  stringsAsFactors = FALSE
)
target_dataest <- dummy

st_vec <- target_dataest[["AESTDY"]]
en_vec <- target_dataest[["AEENDY"]]

# ---- proc sql: select min(min(st,1)) into :min ; select max(max(st,en)) into :max ----
row_min <- ifelse(is.na(st_vec), 1, pmin(st_vec, 1))
min_val <- as.integer(min(row_min, na.rm = TRUE))

row_max <- mapply(function(a, b) {
  if (is.na(a) && is.na(b)) return(NA_real_)
  if (is.na(a)) return(b)
  if (is.na(b)) return(a)
  max(a, b)
}, st_vec, en_vec)
max_val <- as.integer(max(row_max, na.rm = TRUE))

# %let range = %eval(%sysfunc(range(&min,&max)) + (0<&min));
range_val <- as.integer((max_val - min_val) + as.integer(0 < min_val))

# ---- data __wk1 ----
wk <- target_dataest
n <- nrow(wk)
if (min_val < 1) {
  lt1_st <- ifelse(is.na(wk[["AESTDY"]]), 1L, as.integer(wk[["AESTDY"]] < 1))
  st <- wk[["AESTDY"]] - min_val + lt1_st
  en <- ifelse(is.na(wk[["AEENDY"]]), NA_real_,
               wk[["AEENDY"]] - min_val + as.integer(wk[["AEENDY"]] < 1))
} else {
  st <- wk[["AESTDY"]]
  en <- wk[["AEENDY"]]
}

# array ar{range} $1.; do i=1 to range;
#   if st<=i<=en then ar="Y";
#   else if st=i and missing(en) then ar="Y";
#   else if st<=i and missing(en) then ar="O";
ar_mat <- matrix(NA_character_, nrow = n, ncol = range_val)
for (r in seq_len(n)) {
  s <- st[r]; e <- en[r]
  s_missing <- is.na(s); e_missing <- is.na(e)
  for (i in seq_len(range_val)) {
    v <- NA_character_
    if (!s_missing && !e_missing && s <= i && i <= e) {
      v <- "Y"
    } else if (!s_missing && e_missing && s == i) {
      v <- "Y"
    } else if (!s_missing && e_missing && s <= i) {
      v <- "O"
    } else {
      v <- NA_character_
    }
    ar_mat[r, i] <- v
  }
}

# ---- header day labels: &min %to &max, excluding 0 (macro %hed skips 0) ----
day_labels <- setdiff(seq(min_val, max_val, by = 1), 0)
# day_labels length must equal range_val
# ar index i maps to day_labels[i]

# ---- build GANT sheet dataset: ID, EVENT, STDY, ENDY + day columns ----
gant <- data.frame(
  ID = wk[["SUBJID"]],
  EVENT = wk[["AETERM"]],
  STDY = wk[["AESTDY"]],
  ENDY = wk[["AEENDY"]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)
day_df <- as.data.frame(ar_mat, stringsAsFactors = FALSE, check.names = FALSE)
colnames(day_df) <- as.character(day_labels)
gant <- cbind(gant, day_df)
# ensure column order: ID EVENT STDY ENDY -2 -1 1 2 ...
# gant now has 4 + range_val = 36 columns for this test data

# ---- write dataset gant.csv (SAS dataset variable names, case-insensitive match) ----
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# ---- ODS EXCEL equivalent: gant.xlsx with RWI styling (sheet GANT) ----
disp_header <- c("ID", "Event", "STDY", "ENDY", as.character(day_labels))
wb <- createWorkbook()
addWorksheet(wb, "GANT")

hdr_darkblue <- createStyle(fgFill = "#00008B", fontColour = "white", textDecoration = "bold",
                            halign = "center", valign = "center", border = "TopBottomLeftRight")
hdr_blue <- createStyle(fgFill = "#0000FF", fontColour = "white", textDecoration = "bold",
                        halign = "center", valign = "center", border = "TopBottomLeftRight")
style_Y <- createStyle(fgFill = "#FF0000", fontColour = "#FF0000", halign = "center", border = "TopBottomLeftRight")
style_O <- createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB", halign = "center", border = "TopBottomLeftRight")
style_W <- createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF", halign = "center", border = "TopBottomLeftRight")
style_id <- createStyle(halign = "left", valign = "center", border = "TopBottomLeftRight")
style_num <- createStyle(halign = "center", valign = "center", border = "TopBottomLeftRight")

writeData(wb, "GANT", t(disp_header), startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)
if (nrow(gant) > 0) {
  writeData(wb, "GANT", gant, startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)
}
nc <- ncol(gant); nr <- nrow(gant)
for (c in seq_along(disp_header)) {
  stl <- if (c <= 4) hdr_darkblue else hdr_blue
  addStyle(wb, "GANT", style = stl, rows = 1, cols = c, gridExpand = FALSE, stack = TRUE)
}
for (r in seq_len(nr)) {
  rr <- r + 1
  addStyle(wb, "GANT", style = style_id, rows = rr, cols = 1, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_id, rows = rr, cols = 2, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_num, rows = rr, cols = 3, gridExpand = FALSE, stack = TRUE)
  addStyle(wb, "GANT", style = style_num, rows = rr, cols = 4, gridExpand = FALSE, stack = TRUE)
  for (c in 5:nc) {
    v <- ar_mat[r, c - 4]
    if (!is.na(v) && v == "Y") {
      addStyle(wb, "GANT", style = style_Y, rows = rr, cols = c, gridExpand = FALSE, stack = TRUE)
    } else if (!is.na(v) && v == "O") {
      addStyle(wb, "GANT", style = style_O, rows = rr, cols = c, gridExpand = FALSE, stack = TRUE)
    } else {
      addStyle(wb, "GANT", style = style_W, rows = rr, cols = c, gridExpand = FALSE, stack = TRUE)
    }
  }
}
setColWidths(wb, "GANT", cols = 1, widths = 10)
setColWidths(wb, "GANT", cols = 2, widths = 18)
setColWidths(wb, "GANT", cols = 3:4, widths = 8)
if (nc >= 5) setColWidths(wb, "GANT", cols = 5:nc, widths = 4)
freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)
saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# ---- plain-text rendering gant.txt (same numbers as Excel) ----
txt_path <- file.path(out, "gant.txt")
con <- file(txt_path, open = "wt")
cat("Gantt chart of events per subject (Y=event duration, O=ongoing)\n", file = con)
cat(sprintf("Range: min=%d max=%d range=%d (day 0 excluded from timeline)\n", min_val, max_val, range_val), file = con)
cat("\n", file = con)
hdr_line <- sprintf("%-6s | %-16s | %4s %4s | %s", "ID", "Event", "STDY", "ENDY",
                    paste(sprintf("%3s", day_labels), collapse = " "))
cat(hdr_line, "\n", file = con, sep = "")
cat(paste(rep("-", nchar(hdr_line)), collapse = ""), "\n", file = con, sep = "")
for (r in seq_len(nr)) {
  stdy_s <- ifelse(is.na(gant$STDY[r]), "   .", sprintf("%4d", gant$STDY[r]))
  endy_s <- ifelse(is.na(gant$ENDY[r]), "   .", sprintf("%4d", gant$ENDY[r]))
  cells <- sapply(seq_len(range_val), function(k) {
    v <- ar_mat[r, k]
    if (is.na(v)) "   " else if (v == "Y") "  Y" else "  O"
  })
  line <- sprintf("%-6s | %-16s | %s %s |%s", gant$ID[r], gant$EVENT[r], stdy_s, endy_s,
                  paste(cells, collapse = ""))
  cat(line, "\n", file = con, sep = "")
}
cat("\nLegend: Y (red) = event day, O (pink) = ongoing after start, blank = no event\n", file = con)
cat(sprintf("Counts: n_rows=%d n_day_cols=%d\n", nr, range_val), file = con)
close(con)

# ---- figure gant.png (ggplot timeline) ----
plot_df <- data.frame(
  label = paste(gant$ID, gant$EVENT, sep = " / "),
  y = seq_len(nrow(gant)),
  AESTDY = gant$STDY,
  AEENDY = gant$ENDY,
  stringsAsFactors = FALSE
)
plot_df$ongoing <- is.na(plot_df$AEENDY)
plot_df$xend <- ifelse(plot_df$ongoing, max_val, plot_df$AEENDY)
p <- ggplot(plot_df, aes(y = reorder(label, y))) +
  geom_segment(aes(x = AESTDY, xend = xend, yend = reorder(label, y), colour = ongoing),
               linewidth = 4, lineend = "butt") +
  geom_point(aes(x = AESTDY), size = 2) +
  geom_point(data = subset(plot_df, !ongoing), aes(x = AEENDY), size = 2) +
  scale_colour_manual(values = c("FALSE" = "red", "TRUE" = "pink"),
                      labels = c("FALSE" = "Completed", "TRUE" = "Ongoing"), name = "Status") +
  labs(title = "Gantt chart of events per subject", x = "Study Day", y = "Subject / Event") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave(file.path(out, "gant.png"), plot = p, width = 10, height = 4, dpi = 150)
```