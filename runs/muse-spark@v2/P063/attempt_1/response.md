```r
# P063 event_gant_excel translation
# Reproduces SAS %event_gant_excel with built-in dummy data
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(openxlsx)
  library(ggplot2)
})

# SAS round half away from zero helper (w.d formats)
round_half_up <- function(x, d = 0) {
  p <- 10^d
  sign(x) * floor(abs(x) * p + 0.5) / p
}

cat("Building dummy data\n")
dummy <- tibble::tibble(
  SUBJID = c("AAA", "AAA", "BBB", "BBB", "BBB", "CCC"),
  AETERM = c("Adverse Event1", "Adverse Event2", "Adverse Event1",
             "Adverse Event2", "Adverse Event3", "Adverse Event1"),
  AESTDY = c(1, 3, 2, -2, 4, 15),
  AEENDY = c(5, 8, 30, 3, NA_real_, 20)
)

target_id <- "SUBJID"
target_event_name <- "AETERM"
target_st <- "AESTDY"
target_en <- "AEENDY"

# proc sql: min(min(st,1)) and max(max(st,en))
# SAS MAX/MIN ignore missing
st_vec <- dummy[[target_st]]
en_vec <- dummy[[target_en]]
min_vec <- pmin(st_vec, 1)
min_val <- min(min_vec, na.rm = TRUE)
# row-wise max ignoring missing (SAS max(4,.)=4)
row_max <- suppressWarnings(pmax(st_vec, en_vec, na.rm = TRUE))
# pmax with all NA gives -Inf with warning; fix
row_max[is.infinite(row_max)] <- NA_real_
max_val <- max(row_max, na.rm = TRUE)
min_val <- as.integer(min_val)
max_val <- as.integer(max_val)
cat("min=", min_val, " max=", max_val, "\n")

range_val <- as.integer((max_val - min_val) + as.integer(0 < min_val))
cat("range=", range_val, "\n")
stopifnot(!is.na(min_val), !is.na(max_val), range_val > 0)

n <- nrow(dummy)
# st / en mapping
st <- rep(NA_real_, n)
en <- rep(NA_real_, n)
if (min_val < 1) {
  for (k in seq_len(n)) {
    a_st <- st_vec[k]
    a_en <- en_vec[k]
    if (is.na(a_st)) {
      st[k] <- NA_real_
    } else {
      st[k] <- a_st - min_val + as.integer(!is.na(a_st) & a_st < 1)
      # SAS: missing<1 is TRUE, but st stays missing
      # above handles non-missing only
    }
    if (is.na(a_en)) {
      en[k] <- NA_real_
    } else {
      en[k] <- a_en - min_val + as.integer(a_en < 1)
    }
  }
} else {
  st <- st_vec
  en <- en_vec
}

# array ar{range} $1. with SAS missing-first semantics
ar_mat <- matrix(NA_character_, nrow = n, ncol = range_val)
for (k in seq_len(n)) {
  s <- st[k]
  e <- en[k]
  e_missing <- is.na(e)
  s_missing <- is.na(s)
  for (i in seq_len(range_val)) {
    # if st <= i <= en  (SAS: missing is smallest)
    cond1_left <- s_missing | (!is.na(s) & s <= i)
    cond1_right <- (!e_missing) & (i <= e)
    cond1 <- isTRUE(cond1_left) && isTRUE(cond1_right)
    if (cond1) {
      ar_mat[k, i] <- "Y"
    } else if (!s_missing && !is.na(s) && s == i && e_missing) {
      ar_mat[k, i] <- "Y"
    } else if ((s_missing || (!is.na(s) && s <= i)) && e_missing && !s_missing) {
      # SAS would also give O when s missing; restrict to non-missing s
      # to avoid filling all cells for missing start (no such rows in dummy)
      # faithful SAS would be: (s_missing | s<=i) & e_missing -> "O"
      ar_mat[k, i] <- "O"
    } else if (s_missing && e_missing) {
      # faithful SAS: st=. , en=. -> st<=i true, so O
      ar_mat[k, i] <- "O"
    } else {
      ar_mat[k, i] <- NA_character_
    }
  }
}

# header days min..max excluding 0
day_labels <- setdiff(seq(min_val, max_val, by = 1L), 0L)
stopifnot(length(day_labels) == range_val)

# __wk1 equivalent (SAS dataset)
wk1 <- data.frame(st = st, en = en,
                  SUBJID = dummy$SUBJID,
                  AETERM = dummy$AETERM,
                  AESTDY = dummy$AESTDY,
                  AEENDY = dummy$AEENDY,
                  stringsAsFactors = FALSE)
for (j in seq_len(range_val)) {
  wk1[[paste0("ar", j)]] <- ar_mat[, j]
}

# Excel / GANT table body: ID, Event, STDY, ENDY + day columns
gant_table <- data.frame(
  ID = dummy$SUBJID,
  Event = dummy$AETERM,
  STDY = dummy$AESTDY,
  ENDY = dummy$AEENDY,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
for (j in seq_len(range_val)) {
  gant_table[[as.character(day_labels[j])]] <- ar_mat[, j]
}

cat("Writing gant.csv\n")
write.csv(gant_table, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# Plain-text rendering of the table (same numbers)
cat("Writing gant.txt\n")
txt_con <- file(file.path(out, "gant.txt"), open = "wt")
writeLines("GANT chart - Event durations per subject (GANT sheet)", txt_con)
writeLines(paste0("Days from ", min_val, " to ", max_val, " excluding 0; range=", range_val), txt_con)
writeLines("", txt_con)
# header line
hdr <- c("ID", "Event", "STDY", "ENDY", as.character(day_labels))
# format body with fixed width for readability; keep Y/O/blank and numbers exact
df_print <- as.data.frame(gant_table, stringsAsFactors = FALSE, check.names = FALSE)
# represent blank day cells as blank, missing ENDY as blank (na.print="")
out_txt <- capture.output(print(df_print, na.print = "", right = FALSE, row.names = FALSE))
writeLines(out_txt, txt_con)
writeLines("", txt_con)
writeLines("Cell codes: Y = event day (red), O = ongoing after start (pink), blank = no event (white).", txt_con)
writeLines("Missing ENDY indicates ongoing event: Y at start day, O thereafter.", txt_con)
close(txt_con)

# Formatted Excel reproducing ODS EXCEL + RWI colours
cat("Writing gant.xlsx\n")
excel_df <- gant_table
# openxlsx writes NA as blank; ensure character day cols stay character
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "GANT")
openxlsx::writeData(wb, "GANT", excel_df, colNames = TRUE, rowNames = FALSE, keepNA = FALSE)
headerDark <- openxlsx::createStyle(fgFill = "#00008B", fontColour = "#FFFFFF",
                                    halign = "center", valign = "center", textDecoration = "bold",
                                    border = "TopBottomLeftRight")
headerBlue <- openxlsx::createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF",
                                    halign = "center", valign = "center", textDecoration = "bold",
                                    border = "TopBottomLeftRight")
styleY <- openxlsx::createStyle(fgFill = "#FF0000", fontColour = "#FF0000", halign = "center",
                                border = "TopBottomLeftRight")
styleO <- openxlsx::createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB", halign = "center",
                                border = "TopBottomLeftRight")
styleW <- openxlsx::createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF", halign = "center",
                                border = "TopBottomLeftRight")
styleID <- openxlsx::createStyle(halign = "left", valign = "center",
                                 border = "TopBottomLeftRight")
ncol_all <- ncol(excel_df)
nrow_all <- nrow(excel_df) + 1
openxlsx::addStyle(wb, "GANT", headerDark, rows = 1, cols = 1:4, gridExpand = TRUE)
if (ncol_all >= 5) {
  openxlsx::addStyle(wb, "GANT", headerBlue, rows = 1, cols = 5:ncol_all, gridExpand = TRUE)
}
# default body borders for first 4 cols
if (nrow(excel_df) >= 1) {
  openxlsx::addStyle(wb, "GANT", styleID, rows = 2:(nrow(excel_df) + 1), cols = 1:4, gridExpand = TRUE)
}
# day cells coloured by value
for (r in seq_len(nrow(excel_df))) {
  excel_row <- r + 1
  for (c in 5:ncol_all) {
    v <- excel_df[r, c]
    if (is.na(v) || v == "") {
      openxlsx::addStyle(wb, "GANT", styleW, rows = excel_row, cols = c, gridExpand = FALSE)
    } else if (v == "Y") {
      openxlsx::addStyle(wb, "GANT", styleY, rows = excel_row, cols = c, gridExpand = FALSE)
    } else if (v == "O") {
      openxlsx::addStyle(wb, "GANT", styleO, rows = excel_row, cols = c, gridExpand = FALSE)
    }
  }
}
openxlsx::setColWidths(wb, "GANT", cols = 1, widths = 10)
openxlsx::setColWidths(wb, "GANT", cols = 2, widths = 18)
openxlsx::setColWidths(wb, "GANT", cols = 3:4, widths = 8)
if (ncol_all >= 5) openxlsx::setColWidths(wb, "GANT", cols = 5:ncol_all, widths = 4)
openxlsx::freezePane(wb, "GANT", firstActiveRow = 2, firstActiveCol = 5)
openxlsx::saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# PNG figure of the same Gantt data (plotted data = gant.csv)
cat("Writing gant.png\n")
plot_df <- dummy %>%
  mutate(
    label = paste(SUBJID, AETERM, sep = " | "),
    end_plot = ifelse(is.na(AEENDY), max_val, AEENDY),
    ongoing = is.na(AEENDY)
  ) %>%
  arrange(AESTDY)
plot_df$label <- factor(plot_df$label, levels = rev(plot_df$label))
p <- ggplot(plot_df, aes(y = label)) +
  geom_segment(aes(x = AESTDY, xend = end_plot, yend = label,
                   colour = ongoing), linewidth = 6, lineend = "butt") +
  scale_colour_manual(values = c("FALSE" = "#FF0000", "TRUE" = "#FF69B4"),
                      labels = c("FALSE" = "Completed", "TRUE" = "Ongoing"),
                      name = "Status") +
  labs(title = "GANT chart - Event durations per subject",
       x = "Study day", y = "Subject | Event") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")
ggsave(file.path(out, "gant.png"), plot = p, width = 10, height = 4, dpi = 150)

cat("Done\n")
```