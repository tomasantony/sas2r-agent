```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

## helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  res <- x
  idx <- !is.na(x)
  signx <- sign(x[idx])
  absx <- abs(x[idx])
  fac <- 10^digits
  res[idx] <- signx * floor(absx * fac + 0.5) / fac
  res
}

dollar_fmt <- function(x, w, d = 2) {
  sapply(x, function(v) {
    if (is.na(v)) return(strrep(" ", w))
    rv <- round_half_up(v, d)
    s <- formatC(rv, format = "f", digits = d, big.mark = ",")
    s <- paste0("$", s)
    if (nchar(s) > w) s else sprintf(paste0("%", w, "s"), s)
  }, USE.NAMES = FALSE)
}

percent_fmt <- function(x, w, d = 0) {
  sapply(x, function(v) {
    if (is.na(v)) return(strrep(" ", w))
    pct <- v * 100
    rv <- round_half_up(pct, d)
    if (d == 0) s <- sprintf("%.0f%%", rv) else s <- sprintf(paste0("%.", d, "f%%"), rv)
    if (nchar(s) > w) s else sprintf(paste0("%", w, "s"), s)
  }, USE.NAMES = FALSE)
}

sas_sysdate <- function() {
  d <- Sys.Date()
  s <- toupper(format(d, "%d%b%y"))
  s
}
sysdate_str <- sas_sysdate()

## grocery data ----------------------------------------------------------
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

## format maps -----------------------------------------------------------
sct_map <- c(se = "Southeast", ne = "Northeast", nw = "Northwest", sw = "Southwest")
mgr_map <- c("1" = "Smith", "2" = "Jones", "3" = "Reveiz", "4" = "Brown",
             "5" = "Taylor", "6" = "Adams", "7" = "Alomar", "8" = "Andrews", "9" = "Pelfrey")
dept_map <- c(np1 = "Paper", np2 = "Canned", p1 = "Meat/Dairy", p2 = "Produce")

fmt_sct <- function(x) ifelse(is.na(x), NA, unname(sct_map[x]))
fmt_mgr <- function(x) ifelse(is.na(x), NA, unname(mgr_map[x]))
fmt_dept <- function(x) ifelse(is.na(x), NA, unname(dept_map[x]))

## Example1 --------------------------------------------------------------
ex1_data <- subset(grocery, Sector == "se")
ex1_data$ManagerF <- fmt_mgr(ex1_data$Manager)
ex1_data$DepartmentF <- fmt_dept(ex1_data$Department)
ex1_total <- sum(ex1_data$Sales)

ex1_disp <- data.frame(
  Manager = ex1_data$ManagerF,
  Department = ex1_data$DepartmentF,
  Sales = ex1_data$Sales,
  SalesF = dollar_fmt(ex1_data$Sales, 11, 2),
  stringsAsFactors = FALSE
)
ex1_totalF <- dollar_fmt(ex1_total, 11, 2)

## write Example1 text
ex1_txt_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_str),
  "",
  sprintf("%-10s %-12s %11s", "Manager", "Department", "Sales"),
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 11))
)
for (i in seq_len(nrow(ex1_disp))) {
  ex1_txt_lines <- c(ex1_txt_lines,
    sprintf("%-10s %-12s %11s", ex1_disp$Manager[i], ex1_disp$Department[i], ex1_disp$SalesF[i])
  )
}
ex1_txt_lines <- c(ex1_txt_lines,
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 11)),
  sprintf("%-10s %-12s %11s", "", "", ex1_totalF),
  sprintf("%-10s %-12s %11s", "", "Total", ex1_totalF)
)

## Example2 --------------------------------------------------------------
ex2_data <- ex1_data
ex2_min <- min(ex2_data$Sales)
ex2_max <- max(ex2_data$Sales)
ex2_minF <- dollar_fmt(ex2_min, 7, 2)
ex2_maxF <- dollar_fmt(ex2_max, 7, 2)
ex2_disp <- ex1_disp
ex2_disp$SalesF2 <- dollar_fmt(ex2_disp$Sales, 7, 2)

ex2_txt_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate_str),
  "",
  sprintf("%-10s %-12s %7s", "Manager", "Department", "Sales"),
  paste0(strrep("-", 10), " ", strrep("-", 12), " ", strrep("-", 7)),
  ""
)
# ORDER display: manager only on first of group
prev_mgr <- ""
for (i in seq_len(nrow(ex2_disp))) {
  mgr_show <- if (ex2_disp$Manager[i] == prev_mgr) "" else ex2_disp$Manager[i]
  prev_mgr <- ex2_disp$Manager[i]
  ex2_txt_lines <- c(ex2_txt_lines,
    sprintf("%-10s %-12s %7s", mgr_show, ex2_disp$Department[i], ex2_disp$SalesF2[i])
  )
}
ex2_txt_lines <- c(ex2_txt_lines,
  "",
  paste0("       Departmental sales ranged from", ex2_minF, " to", ex2_maxF, ".")
)

## Example3 --------------------------------------------------------------
# compute per manager stats
library(dplyr)
ex3_stats <- grocery %>%
  mutate(SectorF = fmt_sct(Sector), ManagerF = fmt_mgr(Manager)) %>%
  group_by(Sector, SectorF, Manager, ManagerF) %>%
  summarise(
    Sum = sum(Sales),
    Min = min(Sales),
    Max = max(Sales),
    Range = max(Sales) - min(Sales),
    Mean = mean(Sales),
    Std = sd(Sales),
    .groups = "drop"
  ) %>%
  arrange(Sector, Manager)

# order sectors as in SAS: se, nw, sw, ne? Actually SAS order is by formatted? Use sector code order se, nw, sw, ne as appears
# Keep as is sorted by Sector code alphabetical: ne, nw, se, sw -> but SAS example shows Southeast, Northwest, Southwest, Northeast
# So order by custom: se, nw, sw, ne
sector_order <- c("se","nw","sw","ne")
ex3_stats <- ex3_stats %>% mutate(ord = match(Sector, sector_order)) %>% arrange(ord, Manager) %>% select(-ord)

ex3_disp <- ex3_stats %>%
  mutate(
    SumF = dollar_fmt(Sum, 11, 2),
    MinF = dollar_fmt(Min, 11, 2),
    MaxF = dollar_fmt(Max, 11, 2),
    RangeF = dollar_fmt(Range, 11, 2),
    MeanF = dollar_fmt(Mean, 11, 2),
    StdF = dollar_fmt(Std, 11, 2)
  )

ex3_txt_lines <- c(
  "Sales Statistics for All Sectors",
  "",
  sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s",
          "Sector", "Manager", "Sum", "Min", "Max", "Range", "Mean", "Std"),
  sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s",
          "", "", "Sales", "Sales", "Sales", "Sales", "Sales", "Sales"),
  paste0(strrep("-",12)," ",strrep("-",10)," ",strrep("-",11)," ",strrep("-",11)," ",strrep("-",11)," ",strrep("-",11)," ",strrep("-",11)," ",strrep("-",11))
)
prev_sct <- ""
for (i in seq_len(nrow(ex3_disp))) {
  sct_show <- if (ex3_disp$SectorF[i] == prev_sct) "" else ex3_disp$SectorF[i]
  prev_sct <- ex3_disp$SectorF[i]
  ex3_txt_lines <- c(ex3_txt_lines,
    sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s",
            sct_show, ex3_disp$ManagerF[i],
            ex3_disp$SumF[i], ex3_disp$MinF[i], ex3_disp$MaxF[i],
            ex3_disp$RangeF[i], ex3_disp$MeanF[i], ex3_disp$StdF[i])
  )
}

## Example4 --------------------------------------------------------------
grand_total <- sum(grocery$Sales)
ex4_stats <- grocery %>%
  mutate(SectorF = fmt_sct(Sector), ManagerF = fmt_mgr(Manager)) %>%
  group_by(Sector, SectorF, Manager, ManagerF) %>%
  summarise(Sum = sum(Sales), .groups = "drop") %>%
  mutate(PctSum = Sum / grand_total) %>%
  mutate(ord = match(Sector, sector_order)) %>%
  arrange(ord, Manager) %>%
  select(-ord) %>%
  mutate(
    SumF = dollar_fmt(Sum, 9, 2),
    PctSumF = percent_fmt(PctSum, 8, 0),
    Comment = ifelse(PctSum > 0.15, "Sales substantially above expectations.", "")
  )

# rbreak total
ex4_total_row <- data.frame(
  Sector = NA, SectorF = "", Manager = NA, ManagerF = "",
  Sum = grand_total, PctSum = 1, SumF = dollar_fmt(grand_total, 9, 2),
  PctSumF = percent_fmt(1, 8, 0), Comment = "", stringsAsFactors = FALSE
)

ex4_txt_lines <- c(
  "Individual Store Sales as a Percent of All Sales",
  "",
  sprintf("%-12s %-10s %9s %8s %-40s", "Sector", "Manager", "Total", "Percent", ""),
  sprintf("%-12s %-10s %9s %8s %-40s", "", "", "Sales", "of Sales", ""),
  paste0(strrep("-",12)," ",strrep("-",10)," ",strrep("-",9)," ",strrep("-",8)," ",strrep("-",40))
)
prev_sct <- ""
for (i in seq_len(nrow(ex4_stats))) {
  sct_show <- if (ex4_stats$SectorF[i] == prev_sct) "" else ex4_stats$SectorF[i]
  prev_sct <- ex4_stats$SectorF[i]
  ex4_txt_lines <- c(ex4_txt_lines,
    sprintf("%-12s %-10s %9s %8s %-40s",
            sct_show, ex4_stats$ManagerF[i], ex4_stats$SumF[i], ex4_stats$PctSumF[i], ex4_stats$Comment[i])
  )
}
ex4_txt_lines <- c(ex4_txt_lines,
  paste0(strrep("-",12)," ",strrep("-",10)," ",strrep("-",9)," ",strrep("-",8)," ",strrep("-",40)),
  sprintf("%-12s %-10s %9s %8s %-40s", "", "Total", ex4_total_row$SumF, ex4_total_row$PctSumF, "")
)

## Example5 --------------------------------------------------------------
ex5_data <- subset(grocery, Sector %in% c("nw","sw"))
ex5_data$ManagerF <- fmt_mgr(ex5_data$Manager)
ex5_data$DepartmentF <- fmt_dept(ex5_data$Department)
# order by manager formatted? order=formatted for manager, order=internal for department
# manager formatted order: Adams, Brown, Pelfrey, Reveiz, Taylor? Let's sort by formatted name
# But SAS where sector nw or sw, data order is as input: 3,4,9,5,6 => Reveiz, Brown, Pelfrey, Taylor, Adams
# PROC REPORT with ORDER will display in data order, not sorted, but break after manager
# So keep input order grouped by manager
ex5_data <- ex5_data[order(match(ex5_data$Manager, c("3","4","9","5","6")), match(ex5_data$Department, c("np1","np2","p1","p2"))),]
# Actually input order for each manager is np1,p1,np2,p2, but order=internal for department would be np1,np2,p1,p2
# To mimic SAS, we keep input order as is (np1,p1,np2,p2) but display with order internal still shows input order
# We'll keep input order

ex5_disp <- ex5_data %>%
  mutate(SalesF = dollar_fmt(Sales, 7, 2)) %>%
  select(ManagerF, DepartmentF, Sales, SalesF)

ex5_txt_lines <- c(
  "Sales for the Western Sectors",
  "",
  sprintf("%-10s %-12s %7s", "Manager", "Department", "Sales"),
  paste0(strrep("-",10)," ",strrep("-",12)," ",strrep("-",7))
)
prev_mgr <- ""
for (i in seq_len(nrow(ex5_disp))) {
  mgr_show <- if (ex5_disp$ManagerF[i] == prev_mgr) "" else ex5_disp$ManagerF[i]
  # break after manager / skip : insert blank line after each manager group
  # detect when next row has different manager or is last
  is_last_of_group <- i == nrow(ex5_disp) || ex5_disp$ManagerF[i] != ex5_disp$ManagerF[i+1]
  ex5_txt_lines <- c(ex5_txt_lines,
    sprintf("%-10s %-12s %7s", mgr_show, ex5_disp$DepartmentF[i], ex5_disp$SalesF[i])
  )
  if (is_last_of_group) ex5_txt_lines <- c(ex5_txt_lines, "")
  prev_mgr <- ex5_disp$ManagerF[i]
}

## Combined P042.lst -----------------------------------------------------
p042_lines <- c(
  "P042 - Grocery Sales Reports",
  strrep("=", 60),
  "",
  ex1_txt_lines,
  "",
  strrep("-", 60),
  "",
  ex2_txt_lines,
  "",
  strrep("-", 60),
  "",
  ex3_txt_lines,
  "",
  strrep("-", 60),
  "",
  ex4_txt_lines,
  "",
  strrep("-", 60),
  "",
  ex5_txt_lines
)

## Write text files ------------------------------------------------------
writeLines(ex1_txt_lines, file.path(out, "example1.html.txt"))
writeLines(ex1_txt_lines, file.path(out, "example1.txt"))
writeLines(ex1_txt_lines, file.path(out, "example1.html"))
writeLines(ex2_txt_lines, file.path(out, "example2.html.txt"))
writeLines(ex2_txt_lines, file.path(out, "example2.txt"))
writeLines(ex2_txt_lines, file.path(out, "example2.html"))
writeLines(ex3_txt_lines, file.path(out, "example3.html.txt"))
writeLines(ex3_txt_lines, file.path(out, "example3.txt"))
writeLines(ex3_txt_lines, file.path(out, "example3.html"))
writeLines(ex4_txt_lines, file.path(out, "example4.html.txt"))
writeLines(ex4_txt_lines, file.path(out, "example4.txt"))
writeLines(ex4_txt_lines, file.path(out, "example4.html"))
writeLines(ex5_txt_lines, file.path(out, "example5.html.txt"))
writeLines(ex5_txt_lines, file.path(out, "example5.txt"))
writeLines(ex5_txt_lines, file.path(out, "example5.html"))
writeLines(p042_lines, file.path(out, "P042.lst.txt"))
writeLines(p042_lines, file.path(out, "P042.txt"))
writeLines(p042_lines, file.path(out, "P042.lst"))

## Write CSVs ------------------------------------------------------------
# Example1 CSV
ex1_csv <- data.frame(
  Manager = ex1_disp$Manager,
  Department = ex1_disp$Department,
  Sales = ex1_disp$Sales,
  Sales_Formatted = ex1_disp$SalesF,
  stringsAsFactors = FALSE
)
ex1_csv_total <- data.frame(Manager = "Total", Department = "", Sales = ex1_total, Sales_Formatted = ex1_totalF, stringsAsFactors = FALSE)
ex1_csv_out <- rbind(ex1_csv, ex1_csv_total)
write.csv(ex1_csv_out, file.path(out, "example1.html.csv"), row.names = FALSE, na = "")
write.csv(ex1_csv_out, file.path(out, "example1.csv"), row.names = FALSE, na = "")

# Example2 CSV
ex2_csv <- data.frame(
  Manager = ex2_disp$Manager,
  Department = ex2_disp$Department,
  Sales = ex2_disp$Sales,
  Sales_Formatted = ex2_disp$SalesF2,
  stringsAsFactors = FALSE
)
ex2_csv_extra <- data.frame(Manager = "Min", Department = "", Sales = ex2_min, Sales_Formatted = ex2_minF, stringsAsFactors = FALSE)
ex2_csv_extra2 <- data.frame(Manager = "Max", Department = "", Sales = ex2_max, Sales_Formatted = ex2_maxF, stringsAsFactors = FALSE)
ex2_csv_out <- rbind(ex2_csv, ex2_csv_extra, ex2_csv_extra2)
write.csv(ex2_csv_out, file.path(out, "example2.html.csv"), row.names = FALSE, na = "")
write.csv(ex2_csv_out, file.path(out, "example2.csv"), row.names = FALSE, na = "")

# Example3 CSV
ex3_csv <- ex3_stats %>%
  select(Sector, SectorF, Manager, ManagerF, Sum, Min, Max, Range, Mean, Std)
write.csv(ex3_csv, file.path(out, "example3.html.csv"), row.names = FALSE, na = "")
write.csv(ex3_csv, file.path(out, "example3.csv"), row.names = FALSE, na = "")

# Example4 CSV
ex4_csv <- ex4_stats %>% select(Sector, SectorF, Manager, ManagerF, Sum, PctSum, Comment)
ex4_csv_total <- data.frame(Sector = NA, SectorF = "Total", Manager = NA, ManagerF = "Total", Sum = grand_total, PctSum = 1, Comment = "", stringsAsFactors = FALSE)
ex4_csv_out <- rbind(ex4_csv, ex4_csv_total)
write.csv(ex4_csv_out, file.path(out, "example4.html.csv"), row.names = FALSE, na = "")
write.csv(ex4_csv_out, file.path(out, "example4.csv"), row.names = FALSE, na = "")

# Example5 CSV
ex5_csv <- ex5_disp %>% select(Manager = ManagerF, Department = DepartmentF, Sales)
write.csv(ex5_csv, file.path(out, "example5.html.csv"), row.names = FALSE, na = "")
write.csv(ex5_csv, file.path(out, "example5.csv"), row.names = FALSE, na = "")

# P042 combined CSV (all numbers)
p042_csv <- data.frame(
  Report = c(rep("Example1", nrow(ex1_csv_out)), rep("Example2", nrow(ex2_csv_out)), rep("Example3", nrow(ex3_csv)), rep("Example4", nrow(ex4_csv_out)), rep("Example5", nrow(ex5_csv))),
  stringsAsFactors = FALSE
)
# For P042 we provide a simple combined numeric list
p042_numbers <- data.frame(
  Report = c(rep("Example1", nrow(ex1_csv_out)), rep("Example2", nrow(ex2_csv_out)), rep("Example3", nrow(ex3_csv)), rep("Example4", nrow(ex4_csv_out)), rep("Example5", nrow(ex5_csv))),
  Value = c(ex1_csv_out$Sales, ex2_csv_out$Sales, ex3_csv$Sum, ex4_csv_out$Sum, ex5_csv$Sales),
  stringsAsFactors = FALSE
)
write.csv(p042_numbers, file.path(out, "P042.lst.csv"), row.names = FALSE, na = "")
write.csv(p042_numbers, file.path(out, "P042.csv"), row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")
```