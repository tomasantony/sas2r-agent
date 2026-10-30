# ------------------------------------------------------------
# R translation of SAS program P042_2015_sas_html_outputs.sas
# ------------------------------------------------------------
# Required libraries (already installed in the environment)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)

# ------------------------------------------------------------------
# Helper functions to mimic SAS formatting and rounding behavior
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  ifelse(pos,
         floor(x * factor + 0.5) / factor,
         ceiling(x * factor - 0.5) / factor)
}

fmt_dollar <- function(x, width = 11, digits = 2) {
  # SAS DOLLARw.d includes a leading $ and commas, right‑justified
  x <- round_half_up(x, digits)
  txt <- formatC(x, format = "f", big.mark = ",", digits = digits)
  txt <- paste0("$", txt)
  str_pad(txt, width, side = "left")
}

fmt_percent <- function(x, width = 6, digits = 2) {
  # SAS PERCENTw.d shows e.g. " 15.00%"
  pct <- round_half_up(x * 100, digits)
  txt <- formatC(pct, format = "f", digits = digits)
  txt <- paste0(txt, "%")
  str_pad(txt, width, side = "right")
}

# ------------------------------------------------------------------
# Environment & output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------
# Input data (hard‑coded as in the SAS datalines)
# ------------------------------------------------------------------
grocery <- read_table2(
  "Sector Manager Department Sales
   se 1 np1 50    se 1 p1 100   se 1 np2 120   se 1 p2 80
   se 2 np1 40    se 2 p1 300   se 2 np2 220   se 2 p2 70
   nw 3 np1 60    nw 3 p1 600   nw 3 np2 420   nw 3 p2 30
   nw 4 np1 45    nw 4 p1 250   nw 4 np2 230   nw 4 p2 73
   nw 9 np1 45    nw 9 p1 205   nw 9 np2 420   nw 9 p2 76
   sw 5 np1 53    sw 5 p1 130   sw 5 np2 120   sw 5 p2 50
   sw 6 np1 40    sw 6 p1 350   sw 6 np2 225   sw 6 p2 80
   ne 7 np1 90    ne 7 p1 190   ne 7 np2 420   ne 7 p2 86
   ne 8 np1 200   ne 8 p1 300   ne 8 np2 420   ne 8 p2 125",
  col_names = c("Sector", "Manager", "Department", "Sales"),
  col_types = cols(
    Sector = col_character(),
    Manager = col_character(),
    Department = col_character(),
    Sales = col_double()
  )
)

# ------------------------------------------------------------------
# SAS formats (implemented as named vectors)
# ------------------------------------------------------------------
fmt_sector <- c(se = "Southeast",
                ne = "Northeast",
                nw = "Northwest",
                sw = "Southwest")

fmt_manager <- c(`1` = "Smith",   `2` = "Jones",
                 `3` = "Reveiz",  `4` = "Brown",
                 `5` = "Taylor",  `6` = "Adams",
                 `7` = "Alomar", `8` = "Andrews",
                 `9` = "Pelfrey")

fmt_dept <- c(np1 = "Paper",
              np2 = "Canned",
              p1  = "Meat/Dairy",
              p2  = "Produce")

apply_fmt <- function(vec, fmt) {
  # case‑insensitive lookup, preserving original case for missing values
  fmt[as.character(vec)]
}

# ------------------------------------------------------------------
# 1) example1.html  –  Simple report for sector = 'se' with a total line
# ------------------------------------------------------------------
ex1_data <- grocery %>%
  filter(Sector == "se") %>%
  mutate(
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept)
  ) %>%
  select(Manager, Department, Sales)

total_sales_ex1 <- sum(ex1_data$Sales, na.rm = TRUE)

# Build the table with a total row
ex1_table <- bind_rows(
  ex1_data,
  tibble(Manager = "Total", Department = "", Sales = total_sales_ex1)
)

# Write CSV (raw values)
write_csv(ex1_table, file.path(out_dir, "example1.csv"), na = "")

# Write formatted text file
txt_ex1 <- ex1_table %>%
  mutate(
    Sales = fmt_dollar(Sales, width = 11, digits = 2)
  ) %>%
  transmute(line = str_c(
    str_pad(Manager, 10, side = "right"),
    str_pad(Department, 12, side = "right"),
    Sales
  )) %>%
  pull(line) %>%
  str_c(collapse = "\n")

write_lines(txt_ex1, file.path(out_dir, "example1.txt"))

# ------------------------------------------------------------------
# 2) example2.html –  Manager‑ordered report with min/max line
# ------------------------------------------------------------------
ex2_data <- grocery %>%
  filter(Sector == "se") %>%
  mutate(
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept)
  ) %>%
  group_by(Manager, Department) %>%
  summarise(Sales = sum(Sales), .groups = "drop")

# Order: manager by formatted value (alphabetic), department by internal code
ex2_table <- ex2_data %>%
  arrange(Manager, factor(Department,
                          levels = c("Paper","Canned","Meat/Dairy","Produce")))

sales_min_ex2 <- min(ex2_table$Sales, na.rm = TRUE)
sales_max_ex2 <- max(ex2_table$Sales, na.rm = TRUE)

# Write CSV (raw)
write_csv(ex2_table, file.path(out_dir, "example2.csv"), na = "")

# Formatted text (including the after‑line)
txt_ex2_body <- ex2_table %>%
  mutate(Sales = fmt_dollar(Sales, width = 7, digits = 2)) %>%
  transmute(line = str_c(
    str_pad(Manager, 10, side = "right"),
    str_pad(Department, 12, side = "right"),
    Sales
  )) %>%
  pull(line) %>%
  str_c(collapse = "\n")

after_line <- str_c(
  "Departmental sales ranged from ",
  fmt_dollar(sales_min_ex2, width = 7, digits = 2), " to ",
  fmt_dollar(sales_max_ex2, width = 7, digits = 2), "."
)

txt_ex2 <- str_c(txt_ex2_body, after_line, sep = "\n")
write_lines(txt_ex2, file.path(out_dir, "example2.txt"))

# ------------------------------------------------------------------
# 3) example3.html –  Statistics per sector and manager
# ------------------------------------------------------------------
ex3_stats <- grocery %>%
  mutate(
    Sector = apply_fmt(Sector, fmt_sector),
    Manager = apply_fmt(Manager, fmt_manager)
  ) %>%
  group_by(Sector, Manager) %>%
  summarise(
    Sum   = sum(Sales, na.rm = TRUE),
    Min   = min(Sales, na.rm = TRUE),
    Max   = max(Sales, na.rm = TRUE),
    Range = max(Sales, na.rm = TRUE) - min(Sales, na.rm = TRUE),
    Mean  = mean(Sales, na.rm = TRUE),
    Std   = sd(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Sector, Manager)

# Write CSV (raw statistics)
write_csv(ex3_stats, file.path(out_dir, "example3.csv"), na = "")

# Formatted text version
txt_ex3 <- ex3_stats %>%
  mutate(
    Sum   = fmt_dollar(Sum,   width = 11, digits = 2),
    Min   = fmt_dollar(Min,   width = 11, digits = 2),
    Max   = fmt_dollar(Max,   width = 11, digits = 2),
    Range = fmt_dollar(Range,width = 11, digits = 2),
    Mean  = fmt_dollar(Mean,  width = 11, digits = 2),
    Std   = fmt_dollar(Std,   width = 11, digits = 2)
  ) %>%
  transmute(line = str_c(
    str_pad(Sector, 12, side = "right"),
    str_pad(Manager, 10, side = "right"),
    Sum, Min, Max, Range, Mean, Std
  )) %>%
  pull(line) %>%
  str_c(collapse = "\n")

write_lines(txt_ex3, file.path(out_dir, "example3.txt"))

# ------------------------------------------------------------------
# 4) example4.html –  Percent of total sales with comment column
# ------------------------------------------------------------------
total_sales_all <- sum(grocery$Sales, na.rm = TRUE)

ex4_data <- grocery %>%
  mutate(
    Sector = apply_fmt(Sector, fmt_sector),
    Manager = apply_fmt(Manager, fmt_manager)
  ) %>%
  group_by(Sector, Manager) %>%
  summarise(
    Sum    = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PctSum = Sum / total_sales_all,
    Comment = ifelse(PctSum > 0.15, "Sales substantially above expectations.", " ")
  ) %>%
  arrange(Sector, Manager)

# Write CSV (raw)
write_csv(ex4_data, file.path(out_dir, "example4.csv"), na = "")

# Formatted text
txt_ex4 <- ex4_data %>%
  mutate(
    Sum    = fmt_dollar(Sum, width = 9, digits = 2),
    PctSum = fmt_percent(PctSum, width = 8, digits = 2)
  ) %>%
  transmute(line = str_c(
    str_pad(Sector, 12, side = "right"),
    str_pad(Manager, 10, side = "right"),
    Sum, " ", PctSum, " ", str_pad(Comment, 20, side = "right")
  )) %>%
  pull(line) %>%
  str_c(collapse = "\n")

write_lines(txt_ex4, file.path(out_dir, "example4.txt"))

# ------------------------------------------------------------------
# 5) example5.html –  Report for western sectors with a break after each manager
# ------------------------------------------------------------------
ex5_data <- grocery %>%
  filter(Sector %in% c("nw", "sw")) %>%
  mutate(
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept)
  ) %>%
  arrange(Manager, Department)

# Write CSV (raw)
write_csv(ex5_data, file.path(out_dir, "example5.csv"), na = "")

# Formatted text (adding a blank line after each manager)
txt_ex5_body <- ex5_data %>%
  mutate(Sales = fmt_dollar(Sales, width = 7, digits = 2)) %>%
  group_by(Manager) %>%
  summarise(
    lines = str_c(
      str_pad(Manager, 10, side = "right"),
      str_pad(Department, 12, side = "right"),
      Sales,
      collapse = "\n"
    ),
    .groups = "drop"
  ) %>%
  pull(lines) %>%
  str_c(collapse = "\n\n")   # blank line between managers

write_lines(txt_ex5_body, file.path(out_dir, "example5.txt"))

# ------------------------------------------------------------------
# 6) P042.lst –  Full listing of the original grocery data (as a reference table)
# ------------------------------------------------------------------
# Produce a plain‑text listing similar to a SAS PROC PRINT output
lst_table <- grocery %>%
  mutate(
    Sector = apply_fmt(Sector, fmt_sector),
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept),
    Sales = fmt_dollar(Sales, width = 11, digits = 2)
  ) %>%
  transmute(line = str_c(
    str_pad(Sector, 12, side = "right"),
    str_pad(Manager, 10, side = "right"),
    str_pad(Department, 12, side = "right"),
    Sales
  ))

txt_lst <- lst_table$line %>% str_c(collapse = "\n")
write_lines(txt_lst, file.path(out_dir, "P042.lst"))
write_csv(grocery, file.path(out_dir, "P042.csv"), na = "")

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
