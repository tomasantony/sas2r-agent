```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- test_dummy_data (dummy) ---
dummy <- data.frame(
  SUBJID = c("AAA", "AAA", "BBB", "BBB", "BBB", "CCC"),
  AETERM = c("Adverse Event1", "Adverse Event2", "Adverse Event1",
             "Adverse Event2", "Adverse Event3", "Adverse Event1"),
  AESTDY = c(1, 3, 2, -2, 4, 15),
  AEENDY = c(5, 8, 30, 3, NA_real_, 20),
  stringsAsFactors = FALSE
)
target_dataest <- dummy

# --- proc sql: min(min(AESTDY,1)), max(max(AESTDY,AEENDY)) ---
# SAS: missing is smallest, so pmin/pmax ignore missing in favour of non-missing
pmin_sas <- ifelse(is.na(target_dataest$AESTDY), 1L,
                   pmin(target_dataest$AESTDY, 1L))
min_val <- min(pmin_sas, na.rm = TRUE)

pmax_sas <- mapply(function(a, b) {
  if (!is.na(a) && !is.na(b)) max(a, b)
  else if (!is.na(a)) a
  else if (!is.na(b)) b
  else NA_real_
}, target_dataest$AESTDY, target_dataest$AEENDY)
max_val <- max(pmax_sas, na.rm = TRUE)

# %let range = range(min,max) + (0<min); RANGE = max-min
range_val <- as.integer((max_val - min_val) + as.integer(0 < min_val))

# --- data __wk1 ---
n <- nrow(target_dataest)
st_vec <- numeric(n)
en_vec <- numeric(n)
for (k in seq_len(n)) {
  a_st <- target_dataest$AESTDY[k]
  a_en <- target_dataest$AEENDY[k]
  if (min_val < 1) {
    # st = &target_st - &min + (&target_st<1); SAS missing<1 is TRUE
    lt_st <- if (is.na(a_st)) 1L else as.integer(a_st < 1)
    st_vec[k] <- if (is.na(a_st)) NA_real_ else a_st - min_val + lt_st
    if (is.na(a_en)) {
      en_vec[k] <- NA_real_
    } else {
      lt_en <- as.integer(a_en < 1)
      en_vec[k] <- a_en - min_val + lt_en
    }
  } else {
    st_vec[k] <- a_st
    en_vec[k] <- a_en
  }
}

# array ar{range} $1.; do i=1 to range; if st<=i<=en then Y etc.
ar_mat <- matrix("", nrow = n, ncol = range_val)
for (k in seq_len(n)) {
  st <- st_vec[k]
  en <- en_vec[k]
  for (i in seq_len(range_val)) {
    v <- ""
    cond1 <- !is.na(st) && !is.na(en) && (st <= i) && (i <= en)
    if (cond1) {
      v <- "Y"
    } else if (!is.na(st) && is.na(en) && !is.na(st == i) && (st == i)) {
      v <- "Y"
    } else if (!is.na(st) && is.na(en) && (st <= i)) {
      v <- "O"
    } else {
      v <- ""
    }
    ar_mat[k, i] <- v
  }
}

wk1 <- data.frame(
  ST = st_vec,
  EN = en_vec,
  SUBJID = target_dataest$SUBJID,
  AETERM = target_dataest$AETERM,
  AESTDY = target_dataest$AESTDY,
  AEENDY = target_dataest$AEENDY,
  stringsAsFactors = FALSE
)
ar_names <- paste0("AR", seq_len(range_val))
for (j in seq_len(range_val)) {
  wk1[[ar_names[j]]] <- ar_mat[, j]
}

# dataset gant -> OUT_DIR/gant.csv
gant <- wk1
write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# --- header day labels: min to max excluding 0 ---
day_vals <- seq.int(min_val, max_val)
day_vals <- day_vals[day_vals != 0]
stopifnot(length(day_vals) == range_val)

# --- plain-text rendering gant.txt (same numbers as Excel sheet) ---
hdr <- c("ID", "Event", "STDY", "ENDY", as.character(day_vals))
con <- file(file.path(out, "gant.txt"), open = "wt")
writeLines("GANT - Event Gantt (sheet GANT)", con)
writeLines(paste0("Range: min=", min_val, " max=", max_val, " range=", range_val), con)
writeLines(paste(hdr, collapse = "|"), con)
for (k in seq_len(n)) {
  a_st <- if (is.na(gant$AESTDY[k])) "" else as.character(gant$AESTDY[k])
  a_en <- if (is.na(gant$AEENDY[k])) "" else as.character(gant$AEENDY[k])
  rowv <- c(gant$SUBJID[k], gant$AETERM[k], a_st, a_en, as.character(ar_mat[k, ]))
  # show blanks as " " to keep columns visible, Y/O as-is
  rowv_disp <- ifelse(rowv == "", " ", rowv)
  writeLines(paste(rowv_disp, collapse = "|"), con)
}
close(con)

# --- Excel reproduction gant.xlsx (sheet GANT) with ODS EXCEL/RWI colours ---
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "GANT")
  # header row
  header_row <- c("ID", "Event", "STDY", "ENDY", as.character(day_vals))
  # data rows
  df_excel <- data.frame(matrix(ncol = length(header_row), nrow = 0))
  colnames(df_excel) <- header_row
  for (k in seq_len(n)) {
    newrow <- c(gant$SUBJID[k], gant$AETERM[k],
                ifelse(is.na(gant$AESTDY[k]), "", as.character(gant$AESTDY[k])),
                ifelse(is.na(gant$AEENDY[k]), "", as.character(gant$AEENDY[k])),
                ar_mat[k, ])
    df_excel[k, ] <- newrow
  }
  openxlsx::writeData(wb, "GANT", df_excel, colNames = TRUE)
  # styles mimicking SAS RWI: darkblue/white, blue/white, red/red, pink/pink, white/white
  st_darkblue <- openxlsx::createStyle(fgFill = "#00008B", fontColour = "#FFFFFF",
                                       textDecoration = "bold", halign = "center", valign = "center")
  st_blue <- openxlsx::createStyle(fgFill = "#0000FF", fontColour = "#FFFFFF",
                                   halign = "center", valign = "center")
  st_red <- openxlsx::createStyle(fgFill = "#FF0000", fontColour = "#FF0000",
                                  halign = "center", valign = "center")
  st_pink <- openxlsx::createStyle(fgFill = "#FFC0CB", fontColour = "#FFC0CB",
                                   halign = "center", valign = "center")
  st_white <- openxlsx::createStyle(fgFill = "#FFFFFF", fontColour = "#FFFFFF",
                                    halign = "center", valign = "center")
  st_id <- openxlsx::createStyle(halign = "left", valign = "center")
  # header styles
  openxlsx::addStyle(wb, "GANT", st_darkblue, rows = 1, cols = 1:4, gridExpand = TRUE)
  if (length(header_row) > 4) {
    openxlsx::addStyle(wb, "GANT", st_blue, rows = 1, cols = 5:length(header_row), gridExpand = TRUE)
  }
  # body: first 4 cols plain, day cells coloured by Y/O/blank
  for (k in seq_len(n)) {
    r <- k + 1
    openxlsx::addStyle(wb, "GANT", st_id, rows = r, cols = 1:4, gridExpand = TRUE)
    for (j in seq_len(range_val)) {
      cidx <- 4 + j
      v <- ar_mat[k, j]
      s <- if (v == "Y") st_red else if (v == "O") st_pink else st_white
      openxlsx::addStyle(wb, "GANT", s, rows = r, cols = cidx, gridExpand = TRUE)
    }
  }
  openxlsx::setColWidths(wb, "GANT", cols = 1:2, widths = c(12, 20))
  openxlsx::setColWidths(wb, "GANT", cols = 3:4, widths = 8)
  if (length(header_row) > 4) {
    openxlsx::setColWidths(wb, "GANT", cols = 5:length(header_row), widths = 4)
  }
  openxlsx::saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)
}
```