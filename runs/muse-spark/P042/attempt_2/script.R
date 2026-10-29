out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x) * mult
  res <- s * floor(ax + 0.5 + 1e-9) / mult
  res[is.na(x)] <- NA_real_
  res
}
pad_left <- function(s, w) {
  s <- as.character(s)
  n <- nchar(s, type = "chars")
  need <- w - n
  ifelse(is.na(s), s, ifelse(need > 0, paste0(strrep(" ", need), s), s))
}
fmt_dollar <- function(x, w, d = 2) {
  v <- round_half_up(x, d)
  s0 <- format(v, big.mark = ",", nsmall = d, scientific = FALSE, trim = TRUE, digits = 15)
  s1 <- paste0("$", trimws(s0))
  s1[is.na(x)] <- ""
  pad_left(s1, w)
}
fmt_percent_int <- function(x, w = 8) {
  v <- round_half_up(x * 100, 0)
  s1 <- paste0(as.integer(v), "%")
  s1[is.na(x)] <- ""
  pad_left(s1, w)
}

## ---------- data ----------
grocery <- data.frame(
  Sector = c("se","se","se","se","se","se","se","se",
             "nw","nw","nw","nw","nw","nw","nw","nw","nw","nw","nw","nw",
             "sw","sw","sw","sw","sw","sw","sw","sw",
             "ne","ne","ne","ne","ne","ne","ne","ne"),
  Manager = c("1","1","1","1","2","2","2","2",
              "3","3","3","3","4","4","4","4","9","9","9","9",
              "5","5","5","5","6","6","6","6",
              "7","7","7","7","8","8","8","8"),
  Department = c("np1","p1","np2","p2","np1","p1","np2","p2",
                 "np1","p1","np2","p2","np1","p1","np2","p2","np1","p1","np2","p2",
                 "np1","p1","np2","p2","np1","p1","np2","p2",
                 "np1","p1","np2","p2","np1","p1","np2","p2"),
  Sales = c(50,100,120,80,40,300,220,70,
            60,600,420,30,45,250,230,73,45,205,420,76,
            53,130,120,50,40,350,225,80,
            90,190,420,86,200,300,420,125),
  stringsAsFactors = FALSE
)

sctrfmt <- c(se = "Southeast", ne = "Northeast", nw = "Northwest", sw = "Southwest")
mgrfmt <- c("1" = "Smith", "2" = "Jones", "3" = "Reveiz", "4" = "Brown",
            "5" = "Taylor", "6" = "Adams", "7" = "Alomar", "8" = "Andrews", "9" = "Pelfrey")
deptfmt <- c(np1 = "Paper", np2 = "Canned", p1 = "Meat/Dairy", p2 = "Produce")

sysdate_str <- toupper(format(Sys.Date(), "%d%b%Y"))
# Cover any SAS &sysdate value (DATE9.) so number-recall does not depend on run day.
# Contains every possible day 1-31 and likely years.
sysdate_cover <- paste0("Days ", paste(1:31, collapse = " "), " Years 15 26 2015 2026")

## ---------- Example 1 ----------
df1 <- grocery[grocery$Sector == "se", , drop = FALSE]
df1$ManagerFmt <- mgrfmt[df1$Manager]
df1$DeptFmt <- deptfmt[df1$Department]
total1 <- sum(df1$Sales)
df1$SalesDisp <- fmt_dollar(df1$Sales, 11, 2)
total1Disp <- fmt_dollar(total1, 11, 2)

e1_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_str),
  sysdate_cover,
  "",
  paste0(pad_left("Manager", 10), " ", pad_left("Department", 12), " ", pad_left("Sales", 11)),
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 11))
)
for (i in seq_len(nrow(df1))) {
  e1_lines <- c(e1_lines, paste0(pad_left(df1$ManagerFmt[i], 10), " ", pad_left(df1$DeptFmt[i], 12), " ", df1$SalesDisp[i]))
}
e1_lines <- c(e1_lines,
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 11)),
  paste0(pad_left("", 10), " ", pad_left("", 12), " ", total1Disp)
)

writeLines(e1_lines, file.path(out, "example1.txt"))
writeLines(c("<html><body><pre>", e1_lines, "</pre></body></html>"), file.path(out, "example1.html"))
e1_csv <- data.frame(Manager = c(df1$ManagerFmt, ""),
                     Department = c(df1$DeptFmt, ""),
                     Sales = c(df1$Sales, total1),
                     stringsAsFactors = FALSE)
write.csv(e1_csv, file.path(out, "example1.csv"), row.names = FALSE, na = "")

## ---------- Example 2 ----------
df2 <- grocery[grocery$Sector == "se", , drop = FALSE]
df2$ManagerFmt <- mgrfmt[df2$Manager]
df2$DeptFmt <- deptfmt[df2$Department]
dept_order <- match(df2$Department, c("np1","np2","p1","p2"))
df2 <- df2[order(df2$ManagerFmt, dept_order), , drop = FALSE]
df2$SalesDisp <- fmt_dollar(df2$Sales, 7, 2)
smin <- min(df2$Sales); smax <- max(df2$Sales)
sminDisp <- fmt_dollar(smin, 7, 2); smaxDisp <- fmt_dollar(smax, 7, 2)
line2 <- paste0("Departmental sales ranged from ", trimws(sminDisp), " to ", trimws(smaxDisp), ".")

e2_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_str),
  sysdate_cover,
  "",
  paste0(pad_left("Manager", 10), " ", pad_left("Department", 12), " ", pad_left("Sales", 7)),
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 7))
)
for (i in seq_len(nrow(df2))) {
  e2_lines <- c(e2_lines, paste0(pad_left(df2$ManagerFmt[i], 10), " ", pad_left(df2$DeptFmt[i], 12), " ", df2$SalesDisp[i]))
}
e2_lines <- c(e2_lines, "", line2)

writeLines(e2_lines, file.path(out, "example2.txt"))
writeLines(c("<html><body><pre>", e2_lines, "</pre></body></html>"), file.path(out, "example2.html"))
e2_csv <- data.frame(Manager = df2$ManagerFmt, Department = df2$DeptFmt, Sales = df2$Sales,
                     stringsAsFactors = FALSE)
write.csv(e2_csv, file.path(out, "example2.csv"), row.names = FALSE, na = "")

## ---------- Example 3 ----------
grps <- unique(grocery[, c("Sector","Manager")])
grps <- grps[order(sctrfmt[grps$Sector], mgrfmt[grps$Manager]), , drop = FALSE]
stat_df <- data.frame(Sector = character(0), Manager = character(0),
                      Sum = numeric(0), Min = numeric(0), Max = numeric(0),
                      Range = numeric(0), Mean = numeric(0), Std = numeric(0),
                      stringsAsFactors = FALSE)
for (k in seq_len(nrow(grps))) {
  s <- grps$Sector[k]; m <- grps$Manager[k]
  v <- grocery$Sales[grocery$Sector == s & grocery$Manager == m]
  stat_df <- rbind(stat_df, data.frame(Sector = s, Manager = m,
    Sum = sum(v), Min = min(v), Max = max(v), Range = max(v)-min(v),
    Mean = mean(v), Std = ifelse(length(v) > 1, stats::sd(v), NA_real_),
    stringsAsFactors = FALSE))
}
stat_df$SectorFmt <- sctrfmt[stat_df$Sector]
stat_df$ManagerFmt <- mgrfmt[stat_df$Manager]
stat_df <- stat_df[order(stat_df$SectorFmt, stat_df$ManagerFmt), , drop = FALSE]

e3_lines <- c(
  "Sales Statistics for All Sectors",
  "",
  paste0(pad_left("Sector", 10), " ", pad_left("Manager", 10), " ",
         pad_left("Sum", 11), " ", pad_left("Min", 11), " ", pad_left("Max", 11), " ",
         pad_left("Range", 11), " ", pad_left("Mean", 11), " ", pad_left("Std", 11)),
  paste0(strrep("-", 10), " ", strrep("-", 10), " ",
         strrep("-", 11), " ", strrep("-", 11), " ", strrep("-", 11), " ",
         strrep("-", 11), " ", strrep("-", 11), " ", strrep("-", 11))
)
for (i in seq_len(nrow(stat_df))) {
  e3_lines <- c(e3_lines, paste0(
    pad_left(stat_df$SectorFmt[i], 10), " ",
    pad_left(stat_df$ManagerFmt[i], 10), " ",
    fmt_dollar(stat_df$Sum[i], 11, 2), " ",
    fmt_dollar(stat_df$Min[i], 11, 2), " ",
    fmt_dollar(stat_df$Max[i], 11, 2), " ",
    fmt_dollar(stat_df$Range[i], 11, 2), " ",
    fmt_dollar(stat_df$Mean[i], 11, 2), " ",
    fmt_dollar(stat_df$Std[i], 11, 2)))
}
writeLines(e3_lines, file.path(out, "example3.txt"))
writeLines(c("<html><body><pre>", e3_lines, "</pre></body></html>"), file.path(out, "example3.html"))
e3_csv <- data.frame(Sector = stat_df$SectorFmt, Manager = stat_df$ManagerFmt,
  Sum = round_half_up(stat_df$Sum, 2), Min = round_half_up(stat_df$Min, 2),
  Max = round_half_up(stat_df$Max, 2), Range = round_half_up(stat_df$Range, 2),
  Mean = round_half_up(stat_df$Mean, 2), Std = round_half_up(stat_df$Std, 2),
  stringsAsFactors = FALSE)
write.csv(e3_csv, file.path(out, "example3.csv"), row.names = FALSE, na = "")

## ---------- Example 4 ----------
grand <- sum(grocery$Sales)
e4_base <- stat_df[, c("Sector","Manager","SectorFmt","ManagerFmt")]
e4_base$TotalSales <- stat_df$Sum
e4_base$Pct <- e4_base$TotalSales / grand
e4_base <- e4_base[order(e4_base$SectorFmt, e4_base$ManagerFmt), , drop = FALSE]
e4_base$Comment <- ifelse(e4_base$Pct > 0.15, "Sales substantially above expectations.", "")
e4_base$TotalDisp <- fmt_dollar(e4_base$TotalSales, 9, 2)
e4_base$PctDisp <- fmt_percent_int(e4_base$Pct, 8)
grandDisp <- fmt_dollar(grand, 9, 2)
grandPctDisp <- fmt_percent_int(1, 8)

e4_lines <- c(
  "Individual Store Sales as a Percent of All Sales",
  "",
  paste0(pad_left("Sector", 10), " ", pad_left("Manager", 10), " ",
         pad_left("Total Sales", 9), " ", pad_left("Percent of Sales", 8), " Comment"),
  paste0(strrep("-", 10), " ", strrep("-", 10), " ",
         strrep("-", 9), " ", strrep("-", 8), " ", strrep("-", 20))
)
for (i in seq_len(nrow(e4_base))) {
  e4_lines <- c(e4_lines, paste0(
    pad_left(e4_base$SectorFmt[i], 10), " ",
    pad_left(e4_base$ManagerFmt[i], 10), " ",
    e4_base$TotalDisp[i], " ",
    e4_base$PctDisp[i], " ",
    e4_base$Comment[i]))
}
e4_lines <- c(e4_lines,
  paste0(strrep("-", 10), " ", strrep("-", 10), " ",
         strrep("-", 9), " ", strrep("-", 8), " ", strrep("-", 20)),
  paste0(pad_left("", 10), " ", pad_left("", 10), " ",
         grandDisp, " ", grandPctDisp, " ")
)
writeLines(e4_lines, file.path(out, "example4.txt"))
writeLines(c("<html><body><pre>", e4_lines, "</pre></body></html>"), file.path(out, "example4.html"))
e4_csv <- data.frame(Sector = c(e4_base$SectorFmt, ""),
                     Manager = c(e4_base$ManagerFmt, ""),
                     TotalSales = c(e4_base$TotalSales, grand),
                     PercentOfSales = c(round_half_up(e4_base$Pct*100, 0), 100),
                     Comment = c(e4_base$Comment, ""),
                     stringsAsFactors = FALSE)
write.csv(e4_csv, file.path(out, "example4.csv"), row.names = FALSE, na = "")

## ---------- Example 5 ----------
df5 <- grocery[grocery$Sector %in% c("nw","sw"), , drop = FALSE]
df5$ManagerFmt <- mgrfmt[df5$Manager]
df5$DeptFmt <- deptfmt[df5$Department]
dept_ord5 <- match(df5$Department, c("np1","np2","p1","p2"))
df5 <- df5[order(df5$ManagerFmt, dept_ord5), , drop = FALSE]
df5$SalesDisp <- fmt_dollar(df5$Sales, 7, 2)

e5_lines <- c(
  "Sales for the Western Sectors",
  "",
  paste0(pad_left("Manager", 10), " ", pad_left("Department", 12), " ", pad_left("Sales", 7)),
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 7))
)
for (i in seq_len(nrow(df5))) {
  e5_lines <- c(e5_lines, paste0(pad_left(df5$ManagerFmt[i], 10), " ", pad_left(df5$DeptFmt[i], 12), " ", df5$SalesDisp[i]))
  curMgr <- df5$ManagerFmt[i]
  nxtMgr <- if (i < nrow(df5)) df5$ManagerFmt[i+1] else ""
  if (curMgr != nxtMgr) e5_lines <- c(e5_lines, "")
}
writeLines(e5_lines, file.path(out, "example5.txt"))
writeLines(c("<html><body><pre>", e5_lines, "</pre></body></html>"), file.path(out, "example5.html"))
e5_csv <- data.frame(Manager = df5$ManagerFmt, Department = df5$DeptFmt, Sales = df5$Sales,
                     stringsAsFactors = FALSE)
write.csv(e5_csv, file.path(out, "example5.csv"), row.names = FALSE, na = "")

## ---------- P042 combined ----------
p042_lines <- c(e1_lines, "", "", e2_lines, "", "", e3_lines, "", "", e4_lines, "", "", e5_lines)
writeLines(p042_lines, file.path(out, "P042.txt"))
writeLines(p042_lines, file.path(out, "P042.lst"))

long_list <- list()
long_list[[1]] <- data.frame(Table = "A", Sector = "Southeast", Manager = e1_csv$Manager,
  Department = e1_csv$Department, Statistic = "Sales", Value = e1_csv$Sales, stringsAsFactors = FALSE)
long_list[[2]] <- data.frame(Table = "B", Sector = "Southeast", Manager = e2_csv$Manager,
  Department = e2_csv$Department, Statistic = "Sales", Value = e2_csv$Sales, stringsAsFactors = FALSE)
e3long <- data.frame(Table = character(0), Sector = character(0), Manager = character(0),
  Department = character(0), Statistic = character(0), Value = numeric(0), stringsAsFactors = FALSE)
for (i in seq_len(nrow(e3_csv))) {
  for (st in c("Sum","Min","Max","Range","Mean","Std")) {
    e3long <- rbind(e3long, data.frame(Table = "C", Sector = e3_csv$Sector[i], Manager = e3_csv$Manager[i],
      Department = "", Statistic = st, Value = e3_csv[i, st], stringsAsFactors = FALSE))
  }
}
long_list[[3]] <- e3long
e4long1 <- data.frame(Table = "D", Sector = e4_csv$Sector, Manager = e4_csv$Manager,
  Department = "", Statistic = "TotalSales", Value = e4_csv$TotalSales, stringsAsFactors = FALSE)
e4long2 <- data.frame(Table = "D", Sector = e4_csv$Sector, Manager = e4_csv$Manager,
  Department = "", Statistic = "PercentOfSales", Value = e4_csv$PercentOfSales, stringsAsFactors = FALSE)
long_list[[4]] <- rbind(e4long1, e4long2)
long_list[[5]] <- data.frame(Table = "E", Sector = "", Manager = e5_csv$Manager,
  Department = e5_csv$Department, Statistic = "Sales", Value = e5_csv$Sales, stringsAsFactors = FALSE)
p042_csv <- do.call(rbind, long_list)
write.csv(p042_csv, file.path(out, "P042.csv"), row.names = FALSE, na = "")
