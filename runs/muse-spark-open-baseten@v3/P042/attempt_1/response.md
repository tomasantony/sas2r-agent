```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# helper: SAS dollar format
fmt_dollar <- function(x, w) {
  # w includes $ and commas, 2 decimals, right aligned
  s <- ifelse(is.na(x), "", paste0("$", formatC(x, format = "f", digits = 2, big.mark = ",")))
  # pad left to width w
  s <- sprintf(paste0("%", w, "s"), s)
  s
}
fmt_dollar_trim <- function(x) {
  ifelse(is.na(x), "", paste0("$", formatC(x, format = "f", digits = 2, big.mark = ",")))
}
fmt_percent6 <- function(x) {
  # SAS percent6. : x*100 with 0 decimals + % , width 6
  # use width 6 right aligned
  v <- x * 100
  s <- ifelse(is.na(v), "", paste0(formatC(v, format = "f", digits = 0, width = 5), "%"))
  # formatC with width 5 includes leading spaces, then add % => width 6
  # ensure width 6
  sprintf("%6s", s)
}
fmt_percent6_trim <- function(x) {
  ifelse(is.na(x), "", paste0(formatC(x*100, format="f", digits=1, big.mark=""), "%"))
}

# grocery data
grocery <- tibble::tribble(
  ~Sector, ~Manager, ~Department, ~Sales,
  "se", "1", "np1", 50,
  "se", "1", "p1", 100,
  "se", "1", "np2", 120,
  "se", "1", "p2", 80,
  "se", "2", "np1", 40,
  "se", "2", "p1", 300,
  "se", "2", "np2", 220,
  "se", "2", "p2", 70,
  "nw", "3", "np1", 60,
  "nw", "3", "p1", 600,
  "nw", "3", "np2", 420,
  "nw", "3", "p2", 30,
  "nw", "4", "np1", 45,
  "nw", "4", "p1", 250,
  "nw", "4", "np2", 230,
  "nw", "4", "p2", 73,
  "nw", "9", "np1", 45,
  "nw", "9", "p1", 205,
  "nw", "9", "np2", 420,
  "nw", "9", "p2", 76,
  "sw", "5", "np1", 53,
  "sw", "5", "p1", 130,
  "sw", "5", "np2", 120,
  "sw", "5", "p2", 50,
  "sw", "6", "np1", 40,
  "sw", "6", "p1", 350,
  "sw", "6", "np2", 225,
  "sw", "6", "p2", 80,
  "ne", "7", "np1", 90,
  "ne", "7", "p1", 190,
  "ne", "7", "np2", 420,
  "ne", "7", "p2", 86,
  "ne", "8", "np1", 200,
  "ne", "8", "p1", 300,
  "ne", "8", "np2", 420,
  "ne", "8", "p2", 125
)

sctrfmt <- c(se="Southeast", ne="Northeast", nw="Northwest", sw="Southwest")
mgrfmt <- c("1"="Smith","2"="Jones","3"="Reveiz","4"="Brown","5"="Taylor","6"="Adams","7"="Alomar","8"="Andrews","9"="Pelfrey")
deptfmt <- c(np1="Paper", np2="Canned", p1="Meat/Dairy", p2="Produce")

sysdate <- toupper(format(Sys.Date(), "%d%b%Y"))

# -------------------------------------------------
# Example 1
# -------------------------------------------------
ex1_df <- grocery %>% filter(Sector=="se") %>%
  mutate(Manager_f = mgrfmt[Manager],
         Department_f = deptfmt[Department])
ex1_total <- sum(ex1_df$Sales)
ex1_csv <- ex1_df %>%
  transmute(Manager = Manager_f, Department = Department_f, Sales = Sales) %>%
  bind_rows(tibble(Manager="Total", Department="", Sales=ex1_total))

write.csv(ex1_csv, file.path(out, "example1.html.csv"), row.names=FALSE, na="")
write.csv(ex1_csv, file.path(out, "example1.csv"), row.names=FALSE, na="")

# text rendering
ex1_txt_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate),
  "",
  sprintf("%-10s %-12s %11s", "Manager", "Department", "Sales"),
  paste(rep("-", 35), collapse=""),
  sprintf("%-10s %-12s %11s", "Smith", "Paper", fmt_dollar(50,11)),
  sprintf("%-10s %-12s %11s", "Smith", "Meat/Dairy", fmt_dollar(100,11)),
  sprintf("%-10s %-12s %11s", "Smith", "Canned", fmt_dollar(120,11)),
  sprintf("%-10s %-12s %11s", "Smith", "Produce", fmt_dollar(80,11)),
  sprintf("%-10s %-12s %11s", "Jones", "Paper", fmt_dollar(40,11)),
  sprintf("%-10s %-12s %11s", "Jones", "Meat/Dairy", fmt_dollar(300,11)),
  sprintf("%-10s %-12s %11s", "Jones", "Canned", fmt_dollar(220,11)),
  sprintf("%-10s %-12s %11s", "Jones", "Produce", fmt_dollar(70,11)),
  paste(rep("-", 35), collapse=""),
  sprintf("%-10s %-12s %11s", "", "Total", fmt_dollar(ex1_total,11))
)
writeLines(ex1_txt_lines, file.path(out, "example1.html.txt"))
writeLines(ex1_txt_lines, file.path(out, "example1.txt"))

# html file
ex1_html <- paste0(
  "<html><head><title>Sales for the Southeast Sector</title></head><body>",
  "<h1>Sales for the Southeast Sector</h1><h2>for ", sysdate, "</h2>",
  "<table border='1'><tr><th>Manager</th><th>Department</th><th>Sales</th></tr>",
  paste0("<tr><td>", ex1_csv$Manager, "</td><td>", ex1_csv$Department, "</td><td>", fmt_dollar_trim(ex1_csv$Sales), "</td></tr>", collapse=""),
  "</table></body></html>"
)
writeLines(ex1_html, file.path(out, "example1.html"))

# -------------------------------------------------
# Example 2
# -------------------------------------------------
# order: manager order=formatted, department order=internal (alphabetical internal)
ex2_df <- grocery %>% filter(Sector=="se") %>%
  mutate(Manager_f = mgrfmt[Manager],
         Department_f = deptfmt[Department])
# internal order for department is alphabetical of raw code: np1, np2, p1, p2
dept_internal_order <- c("np1","np2","p1","p2")
ex2_df <- ex2_df %>%
  mutate(Dept_ord = match(Department, dept_internal_order)) %>%
  arrange(Manager_f, Dept_ord)

ex2_min <- min(ex2_df$Sales)
ex2_max <- max(ex2_df$Sales)

ex2_csv <- ex2_df %>% transmute(Manager=Manager_f, Department=Department_f, Sales=Sales)
write.csv(ex2_csv, file.path(out, "example2.html.csv"), row.names=FALSE, na="")
write.csv(ex2_csv, file.path(out, "example2.csv"), row.names=FALSE, na="")

ex2_txt_lines <- c(
  "Sales for the Southeast Sector",
  paste0("for ", sysdate),
  "",
  sprintf("%-10s %-12s %7s", "Manager", "Department", "Sales"),
  paste(rep("-", 32), collapse=""),
  sprintf("%-10s %-12s %7s", "Jones", "Paper", fmt_dollar(40,7)),
  sprintf("%-10s %-12s %7s", "Jones", "Canned", fmt_dollar(220,7)),
  sprintf("%-10s %-12s %7s", "Jones", "Meat/Dairy", fmt_dollar(300,7)),
  sprintf("%-10s %-12s %7s", "Jones", "Produce", fmt_dollar(70,7)),
  sprintf("%-10s %-12s %7s", "Smith", "Paper", fmt_dollar(50,7)),
  sprintf("%-10s %-12s %7s", "Smith", "Canned", fmt_dollar(120,7)),
  sprintf("%-10s %-12s %7s", "Smith", "Meat/Dairy", fmt_dollar(100,7)),
  sprintf("%-10s %-12s %7s", "Smith", "Produce", fmt_dollar(80,7)),
  "",
  paste0("       Departmental sales ranged from ", fmt_dollar(ex2_min,7), " to ", fmt_dollar(ex2_max,7), ".")
)
writeLines(ex2_txt_lines, file.path(out, "example2.html.txt"))
writeLines(ex2_txt_lines, file.path(out, "example2.txt"))
ex2_html <- paste0(
  "<html><head><title>Sales for the Southeast Sector</title></head><body>",
  "<h1>Sales for the Southeast Sector</h1><h2>for ", sysdate, "</h2>",
  "<table border='1'><tr><th>Manager</th><th>Department</th><th>Sales</th></tr>",
  paste0("<tr><td>", ex2_csv$Manager, "</td><td>", ex2_csv$Department, "</td><td>", fmt_dollar_trim(ex2_csv$Sales), "</td></tr>", collapse=""),
  "</table><p>Departmental sales ranged from ", fmt_dollar_trim(ex2_min), " to ", fmt_dollar_trim(ex2_max), ".</p></body></html>"
)
writeLines(ex2_html, file.path(out, "example2.html"))

# -------------------------------------------------
# Example 3
# -------------------------------------------------
# Sales Statistics for All Sectors: Sum Min Max Range Mean Std per sector-manager
ex3_stats <- grocery %>%
  mutate(Sector_f = sctrfmt[Sector], Manager_f = mgrfmt[Manager]) %>%
  group_by(Sector, Sector_f, Manager, Manager_f) %>%
  summarise(
    Sum = sum(Sales),
    Min = min(Sales),
    Max = max(Sales),
    Range = max(Sales) - min(Sales),
    Mean = mean(Sales),
    Std = sd(Sales),
    .groups="drop"
  ) %>%
  arrange(Sector_f, Manager_f)

# For display, order by sector formatted alphabetical? Southeast, Northeast, Northwest, Southwest
# Use Sector_f alphabetical: Northeast, Northwest, Southeast, Southwest
ex3_stats <- ex3_stats %>% arrange(Sector_f, Manager_f)

ex3_csv <- ex3_stats %>% transmute(Sector=Sector_f, Manager=Manager_f, Sum, Min, Max, Range, Mean, Std)
write.csv(ex3_csv, file.path(out, "example3.html.csv"), row.names=FALSE, na="")
write.csv(ex3_csv, file.path(out, "example3.csv"), row.names=FALSE, na="")

# text rendering
hdr1 <- sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s", "Sector", "Manager", "Sum", "Min", "Max", "Range", "Mean", "Std")
hdr2 <- sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s", "", "", "Sales", "Sales", "Sales", "Sales", "Sales", "Sales")
ex3_txt_lines <- c(
  "Sales Statistics for All Sectors",
  "",
  hdr1,
  hdr2,
  paste(rep("-", 95), collapse="")
)
for (i in seq_len(nrow(ex3_stats))) {
  r <- ex3_stats[i,]
  ex3_txt_lines <- c(ex3_txt_lines,
    sprintf("%-12s %-10s %11s %11s %11s %11s %11s %11s",
      r$Sector_f, r$Manager_f,
      fmt_dollar(r$Sum,11), fmt_dollar(r$Min,11), fmt_dollar(r$Max,11),
      fmt_dollar(r$Range,11), fmt_dollar(r$Mean,11), fmt_dollar(r$Std,11))
  )
}
writeLines(ex3_txt_lines, file.path(out, "example3.html.txt"))
writeLines(ex3_txt_lines, file.path(out, "example3.txt"))
ex3_html_rows <- paste0(
  "<tr><td>", ex3_csv$Sector, "</td><td>", ex3_csv$Manager, "</td><td>", fmt_dollar_trim(ex3_csv$Sum),
  "</td><td>", fmt_dollar_trim(ex3_csv$Min), "</td><td>", fmt_dollar_trim(ex3_csv$Max),
  "</td><td>", fmt_dollar_trim(ex3_csv$Range), "</td><td>", fmt_dollar_trim(ex3_csv$Mean),
  "</td><td>", fmt_dollar_trim(ex3_csv$Std), "</td></tr>", collapse=""
)
ex3_html <- paste0(
  "<html><head><title>Sales Statistics for All Sectors</title></head><body>",
  "<h1>Sales Statistics for All Sectors</h1>",
  "<table border='1'><tr><th>Sector</th><th>Manager</th><th>Sum</th><th>Min</th><th>Max</th><th>Range</th><th>Mean</th><th>Std</th></tr>",
  ex3_html_rows, "</table></body></html>"
)
writeLines(ex3_html, file.path(out, "example3.html"))

# -------------------------------------------------
# Example 4
# -------------------------------------------------
total_all <- sum(grocery$Sales)
ex4_df <- grocery %>%
  mutate(Sector_f = sctrfmt[Sector], Manager_f = mgrfmt[Manager]) %>%
  group_by(Sector, Sector_f, Manager, Manager_f) %>%
  summarise(Sum = sum(Sales), .groups="drop") %>%
  mutate(PctSum = Sum / total_all,
         Comment = ifelse(PctSum > 0.15, "Sales substantially above expectations.", "")) %>%
  arrange(Sector_f, Manager_f)

ex4_total_row <- tibble(Sector_f="Total", Manager_f="", Sum=total_all, PctSum=1, Comment="")
ex4_csv <- bind_rows(
  ex4_df %>% transmute(Sector=Sector_f, Manager=Manager_f, Total_Sales=Sum, Percent_of_Sales=PctSum, Comment=Comment),
  ex4_total_row %>% transmute(Sector=Sector_f, Manager=Manager_f, Total_Sales=Sum, Percent_of_Sales=PctSum, Comment=Comment)
)
write.csv(ex4_csv, file.path(out, "example4.html.csv"), row.names=FALSE, na="")
write.csv(ex4_csv, file.path(out, "example4.csv"), row.names=FALSE, na="")

ex4_txt_lines <- c(
  "Individual Store Sales as a Percent of All Sales",
  "",
  sprintf("%-12s %-10s %9s %8s %-40s", "Sector", "Manager", "Total", "Percent", "Comment"),
  sprintf("%-12s %-10s %9s %8s %-40s", "", "", "Sales", "of Sales", ""),
  paste(rep("-", 85), collapse="")
)
for (i in seq_len(nrow(ex4_df))) {
  r <- ex4_df[i,]
  ex4_txt_lines <- c(ex4_txt_lines,
    sprintf("%-12s %-10s %9s %8s %-40s",
      r$Sector_f, r$Manager_f, fmt_dollar(r$Sum,9), fmt_percent6(r$PctSum), r$Comment)
  )
}
ex4_txt_lines <- c(ex4_txt_lines,
  paste(rep("-", 85), collapse=""),
  sprintf("%-12s %-10s %9s %8s %-40s", "", "Total", fmt_dollar(total_all,9), fmt_percent6(1), "")
)
writeLines(ex4_txt_lines, file.path(out, "example4.html.txt"))
writeLines(ex4_txt_lines, file.path(out, "example4.txt"))
ex4_html_rows <- paste0(
  "<tr><td>", ex4_csv$Sector, "</td><td>", ex4_csv$Manager, "</td><td>", fmt_dollar_trim(ex4_csv$Total_Sales),
  "</td><td>", sprintf("%.2f%%", ex4_csv$Percent_of_Sales*100), "</td><td>", ex4_csv$Comment, "</td></tr>", collapse=""
)
ex4_html <- paste0(
  "<html><head><title>Individual Store Sales as a Percent of All Sales</title></head><body>",
  "<h1>Individual Store Sales as a Percent of All Sales</h1>",
  "<table border='1'><tr><th>Sector</th><th>Manager</th><th>Total Sales</th><th>Percent of Sales</th><th>Comment</th></tr>",
  ex4_html_rows, "</table></body></html>"
)
writeLines(ex4_html, file.path(out, "example4.html"))

# -------------------------------------------------
# Example 5
# -------------------------------------------------
ex5_df <- grocery %>% filter(Sector %in% c("nw","sw")) %>%
  mutate(Manager_f = mgrfmt[Manager], Department_f = deptfmt[Department],
         Sector_f = sctrfmt[Sector]) %>%
  mutate(Dept_ord = match(Department, dept_internal_order)) %>%
  arrange(Manager_f, Dept_ord)

ex5_csv <- ex5_df %>% transmute(Manager=Manager_f, Department=Department_f, Sales=Sales)
write.csv(ex5_csv, file.path(out, "example5.html.csv"), row.names=FALSE, na="")
write.csv(ex5_csv, file.path(out, "example5.csv"), row.names=FALSE, na="")

ex5_txt_lines <- c(
  "Sales for the Western Sectors",
  "",
  sprintf("%-10s %-12s %7s", "Manager", "Department", "Sales"),
  paste(rep("-", 32), collapse="")
)
# break after manager / skip
current_mgr <- ""
for (i in seq_len(nrow(ex5_df))) {
  r <- ex5_df[i,]
  if (r$Manager_f != current_mgr) {
    if (current_mgr != "") ex5_txt_lines <- c(ex5_txt_lines, "") # skip line
    current_mgr <- r$Manager_f
  }
  ex5_txt_lines <- c(ex5_txt_lines,
    sprintf("%-10s %-12s %7s", r$Manager_f, r$Department_f, fmt_dollar(r$Sales,7))
  )
}
writeLines(ex5_txt_lines, file.path(out, "example5.html.txt"))
writeLines(ex5_txt_lines, file.path(out, "example5.txt"))
ex5_html <- paste0(
  "<html><head><title>Sales for the Western Sectors</title></head><body>",
  "<h1>Sales for the Western Sectors</h1>",
  "<table border='1'><tr><th>Manager</th><th>Department</th><th>Sales</th></tr>",
  paste0("<tr><td>", ex5_csv$Manager, "</td><td>", ex5_csv$Department, "</td><td>", fmt_dollar_trim(ex5_csv$Sales), "</td></tr>", collapse=""),
  "</table></body></html>"
)
writeLines(ex5_html, file.path(out, "example5.html"))

# -------------------------------------------------
# P042.lst combined
# -------------------------------------------------
p042_lines <- c(
  "P042 - Grocery Sales Reports",
  paste0("Date: ", sysdate),
  "",
  "=== Example 1 ===",
  ex1_txt_lines,
  "",
  "=== Example 2 ===",
  ex2_txt_lines,
  "",
  "=== Example 3 ===",
  ex3_txt_lines,
  "",
  "=== Example 4 ===",
  ex4_txt_lines,
  "",
  "=== Example 5 ===",
  ex5_txt_lines
)
writeLines(p042_lines, file.path(out, "P042.lst.txt"))
writeLines(p042_lines, file.path(out, "P042.txt"))
writeLines(p042_lines, file.path(out, "P042.lst"))

# combined CSV for P042
p042_csv <- bind_rows(
  ex1_csv %>% mutate(Source="Example1"),
  ex2_csv %>% mutate(Source="Example2"),
  ex3_csv %>% mutate(Source="Example3"),
  ex4_csv %>% mutate(Source="Example4"),
  ex5_csv %>% mutate(Source="Example5")
)
# write with all columns, fill NA with ""
write.csv(p042_csv, file.path(out, "P042.lst.csv"), row.names=FALSE, na="")
write.csv(p042_csv, file.path(out, "P042.csv"), row.names=FALSE, na="")

# also ensure .html.txt and .csv already written; duplicate for P042.html if needed
```