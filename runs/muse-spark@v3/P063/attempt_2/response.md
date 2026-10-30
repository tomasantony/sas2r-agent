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

range_val <- as.integer((max_val - min_val) + as.integer(0 < min_val))

# --- data __wk1 : st/en + array ar{range} ---
n <- nrow(target_dataest)
st_vec <- numeric(n)
en_vec <- numeric(n)
for (k in seq_len(n)) {
  a_st <- target_dataest$AESTDY[k]
  a_en <- target_dataest$AEENDY[k]
  if (min_val < 1) {
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

ar_mat <- matrix(NA_character_, nrow = n, ncol = range_val)
for (k in seq_len(n)) {
  st <- st_vec[k]
  en <- en_vec[k]
  for (i in seq_len(range_val)) {
    v <- NA_character_
    cond1 <- !is.na(st) && !is.na(en) && (st <= i) && (i <= en)
    if (cond1) {
      v <- "Y"
    } else if (!is.na(st) && is.na(en) && (st == i)) {
      v <- "Y"
    } else if (!is.na(st) && is.na(en) && (st <= i)) {
      v <- "O"
    } else {
      v <- NA_character_
    }
    ar_mat[k, i] <- v
  }
}

# --- header day labels: min to max excluding 0 (length must equal range) ---
day_vals <- seq.int(min_val, max_val)
day_vals <- day_vals[day_vals != 0]
stopifnot(length(day_vals) == range_val)

# --- dataset gant : mirror of Excel sheet GANT ---
# columns: ID, EVENT, STDY, ENDY, "-2","-1","1",...,"30"
day_names <- as.character(day_vals)
gant <- data.frame(matrix(ncol = 0, nrow = n), check.names = FALSE)
gant[["ID"]] <- target_dataest$SUBJID
gant[["EVENT"]] <- target_dataest$AETERM
gant[["STDY"]] <- target_dataest$AESTDY
gant[["ENDY"]] <- target_dataest$AEENDY
for (j in seq_len(range_val)) {
  gant[[day_names[j]]] <- ar_mat[, j]
}

write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")

# --- plain-text rendering gant.txt (same numbers as Excel sheet) ---
hdr <- c("ID", "EVENT", "STDY", "ENDY", day_names)
con <- file(file.path(out, "gant.txt"), open = "wt")
writeLines("GANT - Event Gantt (sheet GANT)", con)
writeLines(paste0("Range: min=", min_val, " max=", max_val, " range=", range_val), con)
writeLines(paste(hdr, collapse = "|"), con)
for (k in seq_len(n)) {
  a_st <- if (is.na(gant[["STDY"]][k])) "" else as.character(gant[["STDY"]][k])
  a_en <- if (is.na(gant[["ENDY"]][k])) "" else as.character(gant[["ENDY"]][k])
  cells <- vapply(day_names, function(dn) {
    v <- gant[[dn]][k]
    if (is.na(v)) " " else v
  }, character(1))
  rowv <- c(gant[["ID"]][k], gant[["EVENT"]][k], a_st, a_en, cells)
  writeLines(paste(rowv, collapse = "|"), con)
}
close(con)

# --- Excel reproduction gant.xlsx (sheet GANT) with ODS EXCEL/RWI colours ---
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "GANT")
  df_excel <- gant
  colnames(df_excel) <- hdr
  openxlsx::writeData(wb, "GANT", df_excel, colNames = TRUE)
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
  openxlsx::addStyle(wb, "GANT", st_darkblue, rows = 1, cols = 1:4, gridExpand = TRUE)
  if (length(hdr) > 4) {
    openxlsx::addStyle(wb, "GANT", st_blue, rows = 1, cols = 5:length(hdr), gridExpand = TRUE)
  }
  for (k in seq_len(n)) {
    r <- k + 1
    openxlsx::addStyle(wb, "GANT", st_id, rows = r, cols = 1:4, gridExpand = TRUE)
    for (j in seq_len(range_val)) {
      cidx <- 4 + j
      v <- ar_mat[k, j]
      s <- if (!is.na(v) && v == "Y") st_red else if (!is.na(v) && v == "O") st_pink else st_white
      openxlsx::addStyle(wb, "GANT", s, rows = r, cols = cidx, gridExpand = TRUE)
    }
  }
  openxlsx::setColWidths(wb, "GANT", cols = 1:2, widths = c(12, 20))
  openxlsx::setColWidths(wb, "GANT", cols = 3:4, widths = 8)
  if (length(hdr) > 4) {
    openxlsx::setColWidths(wb, "GANT", cols = 5:length(hdr), widths = 4)
  }
  openxlsx::saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)
}
```