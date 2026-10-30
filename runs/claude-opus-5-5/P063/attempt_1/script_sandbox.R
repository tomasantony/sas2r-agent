# P063 - event_gant_excel: Gantt-style day array built from dummy AE data
suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- test dummy data ----
dummy <- data.frame(
  SUBJID = c("AAA", "AAA", "BBB", "BBB", "BBB", "CCC"),
  AETERM = c("Adverse Event1", "Adverse Event2", "Adverse Event1",
             "Adverse Event2", "Adverse Event3", "Adverse Event1"),
  AESTDY = c(1, 3, 2, -2, 4, 15),
  AEENDY = c(5, 8, 30, 3, NA, 20),
  stringsAsFactors = FALSE
)

# ---- PROC SQL: min / max ----
# SAS min()/max() ignore missing values, so na.rm = TRUE
mn <- min(pmin(dummy$AESTDY, 1, na.rm = TRUE), na.rm = TRUE)
mx <- max(pmax(dummy$AESTDY, dummy$AEENDY, na.rm = TRUE), na.rm = TRUE)
rng <- (mx - mn) + as.integer(0 < mn)
cat("MIN=", mn, " MAX=", mx, " RANGE=", rng, "\n")

# ---- DATA __wk1 ----
wk1 <- dummy
if (mn < 1) {
  wk1$st <- wk1$AESTDY - mn + as.integer(wk1$AESTDY < 1)
  wk1$en <- ifelse(is.na(wk1$AEENDY), NA,
                   wk1$AEENDY - mn + as.integer(wk1$AEENDY < 1))
} else {
  wk1$st <- wk1$AESTDY
  wk1$en <- wk1$AEENDY
}

ar <- matrix("", nrow = nrow(wk1), ncol = rng)
for (r in seq_len(nrow(wk1))) {
  st <- wk1$st[r]
  en <- wk1$en[r]
  for (i in seq_len(rng)) {
    if (!is.na(en) && st <= i && i <= en) {
      ar[r, i] <- "Y"
    } else if (st == i && is.na(en)) {
      ar[r, i] <- "Y"
    } else if (st <= i && is.na(en)) {
      ar[r, i] <- "O"
    }
  }
}
colnames(ar) <- paste0("ar", seq_len(rng))
wk1_out <- cbind(wk1[, c("SUBJID", "AETERM", "AESTDY", "AEENDY", "st", "en")],
                 as.data.frame(ar, stringsAsFactors = FALSE))

# ---- Excel report (GANT sheet) ----
# Header: ID, Event, STDY, ENDY, then day labels min..max excluding 0
day_labels <- setdiff(seq(mn, mx), 0)
gant <- data.frame(ID = wk1$SUBJID, Event = wk1$AETERM,
                   STDY = wk1$AESTDY, ENDY = wk1$AEENDY,
                   stringsAsFactors = FALSE)
ar_df <- as.data.frame(ar, stringsAsFactors = FALSE)
names(ar_df) <- as.character(day_labels)
gant <- cbind(gant, ar_df)

write.csv(gant, file.path(out, "gant.csv"), row.names = FALSE, na = "")
write.csv(wk1_out, file.path(out, "wk1.csv"), row.names = FALSE, na = "")

# Excel with colouring, mimicking the ODS EXCEL output
wb <- createWorkbook()
addWorksheet(wb, "GANT")
writeData(wb, "GANT", gant, keepNA = FALSE)
hdr_style <- createStyle(fgFill = "darkblue", fontColour = "white")
day_style <- createStyle(fgFill = "blue", fontColour = "white")
addStyle(wb, "GANT", hdr_style, rows = 1, cols = 1:4)
addStyle(wb, "GANT", day_style, rows = 1, cols = 4 + seq_along(day_labels))
yst <- createStyle(fgFill = "red", fontColour = "red")
ost <- createStyle(fgFill = "pink", fontColour = "pink")
wst <- createStyle(fgFill = "white", fontColour = "white")
for (r in seq_len(nrow(ar))) {
  for (i in seq_len(ncol(ar))) {
    s <- if (ar[r, i] == "Y") yst else if (ar[r, i] == "O") ost else wst
    addStyle(wb, "GANT", s, rows = r + 1, cols = 4 + i)
  }
}
saveWorkbook(wb, file.path(out, "gant.xlsx"), overwrite = TRUE)

# Plain-text rendering
txt <- capture.output(print(gant, row.names = FALSE))
writeLines(txt, file.path(out, "gant.txt"))

cat("Done: gant.csv written with", nrow(gant), "rows\n")
