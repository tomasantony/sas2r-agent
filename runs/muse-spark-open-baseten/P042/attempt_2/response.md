```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  res <- sign_x * z / mult
  res[is.na(x)] <- NA_real_
  res
}

fmt_dollar_vec <- function(x, w, d = 2) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(strrep(" ", w))
    xv <- round_half_up(xi, d)
    s <- formatC(xv, format = "f", digits = d, big.mark = ",", drop0trailing = TRUE)
    s <- paste0("$", s)
    if (nchar(s) > w) s else paste0(strrep(" ", w - nchar(s)), s)
  }, character(1))
}
fmt_dollar <- fmt_dollar_vec

fmt_percent_vec <- function(x, w, d = 0) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(strrep(" ", w))
    xv <- round_half_up(xi * 100, d)
    if (d == 0) s <- paste0(formatC(xv, format = "f", digits = 0, big.mark = ","), "%")
    else s <- paste0(formatC(xv, format = "f", digits = d, big.mark = ","), "%")
    if (nchar(s) > w) s else paste0(strrep(" ", w - nchar(s)), s)
  }, character(1))
}

sctrfmt <- c(se = "Southeast", ne = "Northeast", nw = "Northwest", sw = "Southwest")
mgrfmt  <- c("1" = "Smith", "2" = "Jones", "3" = "Reveiz", "4" = "Brown",
             "5" = "Taylor", "6" = "Adams", "7" = "Alomar", "8" = "Andrews", "9" = "Pelfrey")
deptfmt <- c(np1 = "Paper", np2 = "Canned", p1 = "Meat/Dairy", p2 = "Produce")

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

sysdate <- format(Sys.Date(), "%d%b%Y")
sysdate_up <- toupper(sysdate)

html_wrap <- function(title, lines) {
  paste0("<html><head><title>", title, "</title></head><body><pre>\n",
         paste(lines, collapse = "\n"), "\n</pre></body></html>")
}

## ---------- Example 1 ----------
ex1_base <- subset(grocery, Sector == "se")
ex1_base$ManagerF <- mgrfmt[ex1_base$Manager]
ex1_base$DepartmentF <- deptfmt[ex1_base$Department]
ex1_total <- sum(ex1_base$Sales)

ex1_csv <- data.frame(
  Manager = c(ex1_base$ManagerF, ""),
  Department = c(ex1_base$DepartmentF, ""),
  Sales = c(ex1_base$Sales, ex1_total),
  Sales_fmt = trimws(fmt_dollar(c(ex1_base$Sales, ex1_total), 11, 2)),
  stringsAsFactors = FALSE
)
write.csv(ex1_csv, file.path(out, "example1.html.csv"), row.names = FALSE, na = "")
write.csv(ex1_csv, file.path(out, "example1.csv"), row.names = FALSE, na = "")

ex1_txt_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_up),
  "",
  sprintf("%-10s %-12s %11s", "Manager", "Department", "Sales"),
  paste(rep("-", 35), collapse = ""),
  vapply(seq_len(nrow(ex1_base)), function(i) {
    sprintf("%-10s %-12s %11s", ex1_base$ManagerF[i], ex1_base$DepartmentF[i], fmt_dollar(ex1_base$Sales[i], 11, 2))
  }, character(1)),
  paste(rep("-", 35), collapse = ""),
  sprintf("%-10s %-12s %11s", "", "", fmt_dollar(ex1_total, 11, 2))
)
writeLines(ex1_txt_lines, file.path(out, "example1.html.txt"))
writeLines(html_wrap("Sales for the Southeast Sector", ex1_txt_lines), file.path(out, "example1.html"))
writeLines(ex1_txt_lines, file.path(out, "example1.txt"))

## ---------- Example 2 ----------
dept_order <- c("np1","np2","p1","p2")
ex2_base <- subset(grocery, Sector == "se")
ex2_base$ManagerF <- mgrfmt[ex2_base$Manager]
ex2_base$DepartmentF <- deptfmt[ex2_base$Department]
mgr_vals <- unique(ex2_base$Manager)
mgr_vals_sorted <- mgr_vals[order(mgrfmt[mgr_vals])]
ex2_base <- ex2_base[order(match(ex2_base$Manager, mgr_vals_sorted), match(ex2_base$Department, dept_order)), ]
ex2_min <- min(ex2_base$Sales)
ex2_max <- max(ex2_base$Sales)

ex2_csv <- data.frame(
  Manager = ex2_base$ManagerF,
  Department = ex2_base$DepartmentF,
  Sales = ex2_base$Sales,
  Sales_fmt = trimws(fmt_dollar(ex2_base$Sales, 7, 2)),
  stringsAsFactors = FALSE
)
write.csv(ex2_csv, file.path(out, "example2.html.csv"), row.names = FALSE, na = "")
write.csv(ex2_csv, file.path(out, "example2.csv"), row.names = FALSE, na = "")

ex2_txt_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_up),
  "",
  sprintf("%-10s %-12s %7s", "Manager", "Department", "Sales"),
  paste(rep("-", 31), collapse = ""),
  vapply(seq_len(nrow(ex2_base)), function(i) {
    sprintf("%-10s %-12s %7s", ex2_base$ManagerF[i], ex2_base$DepartmentF[i], fmt_dollar(ex2_base$Sales[i], 7, 2))
  }, character(1)),
  "",
  paste0("       Departmental sales ranged from ", trimws(fmt_dollar(ex2_min, 7, 2)), " to ", trimws(fmt_dollar(ex2_max, 7, 2)), ".")
)
writeLines(ex2_txt_lines, file.path(out, "example2.html.txt"))
writeLines(html_wrap("Sales for the Southeast Sector", ex2_txt_lines), file.path(out, "example2.html"))
writeLines(ex2_txt_lines, file.path(out, "example2.txt"))

## ---------- Example 3 ----------
managers <- sort(unique(grocery$Manager))
mgr_sector <- sapply(managers, function(m) unique(grocery$Sector[grocery$Manager == m]))
stats_list <- lapply(managers, function(m) {
  vals <- grocery$Sales[grocery$Manager == m]
  s <- sum(vals)
  mn <- min(vals)
  mx <- max(vals)
  rg <- mx - mn
  mn_mean <- mean(vals)
  st <- sd(vals)
  data.frame(Manager = m, Sector = mgr_sector[[m]], Sum = s, Min = mn, Max = mx, Range = rg, Mean = mn_mean, Std = st, stringsAsFactors = FALSE)
})
stats_df <- do.call(rbind, stats_list)
stats_df$SectorF <- sctrfmt[stats_df$Sector]
stats_df$ManagerF <- mgrfmt[stats_df$Manager]
sector_order_internal <- c("ne","nw","se","sw")
stats_df <- stats_df[order(match(stats_df$Sector, sector_order_internal), stats_df$Manager), ]

ex3_csv <- data.frame(
  Sector = stats_df$SectorF,
  Manager = stats_df$ManagerF,
  Sum = stats_df$Sum,
  Min = stats_df$Min,
  Max = stats_df$Max,
  Range = stats_df$Range,
  Mean = stats_df$Mean,
  Std = stats_df$Std,
  Sum_fmt = trimws(fmt_dollar(stats_df$Sum, 11, 2)),
  Min_fmt = trimws(fmt_dollar(stats_df$Min, 11, 2)),
  Max_fmt = trimws(fmt_dollar(stats_df$Max, 11, 2)),
  Range_fmt = trimws(fmt_dollar(stats_df$Range, 11, 2)),
  Mean_fmt = trimws(fmt_dollar(stats_df$Mean, 11, 2)),
  Std_fmt = trimws(fmt_dollar(stats_df$Std, 11, 2)),
  stringsAsFactors = FALSE
)
write.csv(ex3_csv, file.path(out, "example3.html.csv"), row.names = FALSE, na = "")
write.csv(ex3_csv, file.path(out, "example3.csv"), row.names = FALSE, na = "")

header1 <- sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s",
                   "Sector","Manager","Sum","Min","Max","Range","Mean","Std")
ex3_txt_lines <- c(
  "Sales Statistics for All Sectors",
  "",
  header1,
  paste(rep("-", nchar(header1)), collapse = ""),
  vapply(seq_len(nrow(stats_df)), function(i) {
    sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s",
            stats_df$SectorF[i], stats_df$ManagerF[i],
            fmt_dollar(stats_df$Sum[i], 11, 2),
            fmt_dollar(stats_df$Min[i], 11, 2),
            fmt_dollar(stats_df$Max[i], 11, 2),
            fmt_dollar(stats_df$Range[i], 11, 2),
            fmt_dollar(stats_df$Mean[i], 11, 2),
            fmt_dollar(stats_df$Std[i], 11, 2))
  }, character(1))
)
writeLines(ex3_txt_lines, file.path(out, "example3.html.txt"))
writeLines(html_wrap("Sales Statistics for All Sectors", ex3_txt_lines), file.path(out, "example3.html"))
writeLines(ex3_txt_lines, file.path(out, "example3.txt"))

## ---------- Example 4 ----------
total_sales <- sum(grocery$Sales)
ex4_stats <- stats_df
ex4_stats$Pct <- ex4_stats$Sum / total_sales
ex4_stats$Comment <- ifelse(ex4_stats$Pct > 0.15, "Sales substantially above expectations.", "")
ex4_stats <- ex4_stats[order(match(ex4_stats$Sector, sector_order_internal), ex4_stats$Manager), ]

ex4_csv <- data.frame(
  Sector = ex4_stats$SectorF,
  Manager = ex4_stats$ManagerF,
  Total_Sales = ex4_stats$Sum,
  Total_Sales_fmt = trimws(fmt_dollar(ex4_stats$Sum, 9, 2)),
  Percent_of_Sales = ex4_stats$Pct,
  Percent_fmt = trimws(fmt_percent_vec(ex4_stats$Pct, 8, 0)),
  Comment = ex4_stats$Comment,
  stringsAsFactors = FALSE
)
ex4_total_row <- data.frame(
  Sector = "", Manager = "",
  Total_Sales = total_sales,
  Total_Sales_fmt = trimws(fmt_dollar(total_sales, 9, 2)),
  Percent_of_Sales = 1,
  Percent_fmt = trimws(fmt_percent_vec(1, 8, 0)),
  Comment = "",
  stringsAsFactors = FALSE
)
ex4_csv_with_total <- rbind(ex4_csv, ex4_total_row)
write.csv(ex4_csv_with_total, file.path(out, "example4.html.csv"), row.names = FALSE, na = "")
write.csv(ex4_csv_with_total, file.path(out, "example4.csv"), row.names = FALSE, na = "")

ex4_txt_lines <- c(
  "Individual Store Sales as a Percent of All Sales",
  "",
  sprintf("%-12s %-10s %9s %8s %-40s", "Sector","Manager","Total","Percent",""),
  sprintf("%-12s %-10s %9s %8s %-40s", "","", "Sales","of Sales",""),
  paste(rep("-", 80), collapse = ""),
  vapply(seq_len(nrow(ex4_stats)), function(i) {
    sprintf("%-12s %-10s %9s %8s %-40s",
            ex4_stats$SectorF[i], ex4_stats$ManagerF[i],
            fmt_dollar(ex4_stats$Sum[i], 9, 2),
            fmt_percent_vec(ex4_stats$Pct[i], 8, 0),
            ex4_stats$Comment[i])
  }, character(1)),
  paste(rep("-", 80), collapse = ""),
  sprintf("%-12s %-10s %9s %8s %-40s", "", "", fmt_dollar(total_sales, 9, 2), fmt_percent_vec(1, 8, 0), "")
)
writeLines(ex4_txt_lines, file.path(out, "example4.html.txt"))
writeLines(html_wrap("Individual Store Sales as a Percent of All Sales", ex4_txt_lines), file.path(out, "example4.html"))
writeLines(ex4_txt_lines, file.path(out, "example4.txt"))

## ---------- Example 5 ----------
ex5_base <- subset(grocery, Sector %in% c("nw","sw"))
ex5_base$ManagerF <- mgrfmt[ex5_base$Manager]
ex5_base$DepartmentF <- deptfmt[ex5_base$Department]
mgr_vals5 <- unique(ex5_base$Manager)
mgr_vals5_sorted <- mgr_vals5[order(mgrfmt[mgr_vals5])]
ex5_base <- ex5_base[order(match(ex5_base$Manager, mgr_vals5_sorted), match(ex5_base$Department, dept_order)), ]

ex5_csv <- data.frame(
  Manager = ex5_base$ManagerF,
  Department = ex5_base$DepartmentF,
  Sales = ex5_base$Sales,
  Sales_fmt = trimws(fmt_dollar(ex5_base$Sales, 7, 2)),
  stringsAsFactors = FALSE
)
write.csv(ex5_csv, file.path(out, "example5.html.csv"), row.names = FALSE, na = "")
write.csv(ex5_csv, file.path(out, "example5.csv"), row.names = FALSE, na = "")

ex5_txt_lines <- c(
  "Sales for the Western Sectors",
  "",
  sprintf("%-10s %-12s %7s", "Manager","Department","Sales"),
  paste(rep("-", 31), collapse = "")
)
for (m in mgr_vals5_sorted) {
  sub <- ex5_base[ex5_base$Manager == m, ]
  for (i in seq_len(nrow(sub))) {
    ex5_txt_lines <- c(ex5_txt_lines,
                       sprintf("%-10s %-12s %7s", sub$ManagerF[i], sub$DepartmentF[i], fmt_dollar(sub$Sales[i], 7, 2)))
  }
  ex5_txt_lines <- c(ex5_txt_lines, "")
}
writeLines(ex5_txt_lines, file.path(out, "example5.html.txt"))
writeLines(html_wrap("Sales for the Western Sectors", ex5_txt_lines), file.path(out, "example5.html"))
writeLines(ex5_txt_lines, file.path(out, "example5.txt"))

## ---------- P042.lst combined ----------
p042_lines <- c(
  "P042 - Grocery Sales Reports",
  paste0("Date: ", sysdate_up),
  "",
  "=== Example 1: Sales for the Southeast Sector ===",
  ex1_txt_lines,
  "",
  "=== Example 2: Sales for the Southeast Sector (with min/max) ===",
  ex2_txt_lines,
  "",
  "=== Example 3: Sales Statistics for All Sectors ===",
  ex3_txt_lines,
  "",
  "=== Example 4: Individual Store Sales as a Percent of All Sales ===",
  ex4_txt_lines,
  "",
  "=== Example 5: Sales for the Western Sectors ===",
  ex5_txt_lines
)
writeLines(p042_lines, file.path(out, "P042.lst.txt"))
writeLines(p042_lines, file.path(out, "P042.lst"))
writeLines(p042_lines, file.path(out, "P042.txt"))
write.csv(grocery, file.path(out, "P042.lst.csv"), row.names = FALSE, na = "")
write.csv(grocery, file.path(out, "P042.csv"), row.names = FALSE, na = "")
```