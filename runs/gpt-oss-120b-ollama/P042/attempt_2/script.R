# ------------------------------------------------------------
# R translation of SAS program P042_2015_sas_html_outputs.sas
# ------------------------------------------------------------
# Packages (pre‑installed)
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
  x <- round_half_up(x, digits)
  txt <- formatC(x, format = "f", big.mark = ",", digits = digits)
  txt <- paste0("$", txt)
  str_pad(txt, width, side = "left")
}

fmt_percent <- function(x, width = 8, digits = 2) {
  pct <- round_half_up(x * 100, digits)
  txt <- formatC(pct, format = "f", digits = digits)
  txt <- paste0(txt, "%")
  str_pad(txt, width, side = "right")
}

# ------------------------------------------------------------------
# Environment & output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------
# Input data (hard‑coded as in the SAS datalines)
# ------------------------------------------------------------------
grocery <- readr::read_table(
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
  fmt[as.character(vec)]
}

# ------------------------------------------------------------------
# Utility to write a plain‑text (HTML) table with headers
# ------------------------------------------------------------------
write_html_table <- function(df, header, file) {
  lines <- c(header, df)
  writeLines(lines, con = file)
}

# ------------------------------------------------------------------
# 0) Full listing – P042.lst (observation numbers + formatted values)
# ------------------------------------------------------------------
lst_df <- grocery %>%
  mutate(
    Sector = apply_fmt(Sector, fmt_sector),
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept),
    Sales = fmt_dollar(Sales, width = 11, digits = 2)
  ) %>%
  mutate(Obs = row_number()) %>%
  select(Obs, Sector, Manager, Department, Sales)

lst_header <- str_c(
  str_pad("Obs", 4, side = "right"),
  str_pad("Sector", 12, side = "right"),
  str_pad("Manager", 10, side = "right"),
  str_pad("Department", 12, side = "right"),
  "Sales"
)

lst_body <- lst_df %>%
  transmute(line = str_c(
    str_pad(Obs, 4, side = "right"),
    str_pad(Sector, 12, side = "right"),
    str_pad(Manager, 10, side = "right"),
    str_pad(Department, 12, side = "right"),
    Sales
  )) %>%
  pull(line)

write_html_table(lst_body, lst_header, file.path(out_dir, "P042.lst"))
write_csv(grocery, file.path(out_dir, "P042.csv"), na = "")

# ------------------------------------------------------------------
# 1) example1.html – Simple report for sector = 'se' with total line
# ------------------------------------------------------------------
ex1 <- grocery %>%
  filter(Sector == "se") %>%
  mutate(
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept)
  ) %>%
  select(Manager, Department, Sales)

total_ex1 <- sum(ex1$Sales, na.rm = TRUE)

ex1_body <- bind_rows(
  ex1,
  tibble(Manager = "Total", Department = "", Sales = total_ex1)
) %>%
  mutate(Sales = fmt_dollar(Sales, width = 11, digits = 2)) %>%
  transmute(line = str_c(
    str_pad(Manager, 10, side = "right"),
    str_pad(Department, 12, side = "right"),
    Sales
  )) %>%
  pull(line)

ex1_header <- str_c(
  str_pad("Manager", 10, side = "right"),
  str_pad("Department", 12, side = "right"),
  "Sales"
)

write_html_table(ex1_body, ex1_header, file.path(out_dir, "example1.html"))
write_csv(ex1, file.path(out_dir, "example1.csv"), na = "")

# ------------------------------------------------------------------
# 2) example2.html – Manager‑ordered report with min/max line
# ------------------------------------------------------------------
ex2 <- grocery %>%
  filter(Sector == "se") %>%
  mutate(
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept)
  ) %>%
  group_by(Manager, Department) %>%
  summarise(Sales = sum(Sales), .groups = "drop") %>%
  arrange(Manager,
          factor(Department,
                 levels = c("Paper","Canned","Meat/Dairy","Produce")))

sales_min_ex2 <- min(ex2$Sales, na.rm = TRUE)
sales_max_ex2 <- max(ex2$Sales, na.rm = TRUE)

ex2_body <- ex2 %>%
  mutate(Sales = fmt_dollar(Sales, width = 7, digits = 2)) %>%
  transmute(line = str_c(
    str_pad(Manager, 10, side = "right"),
    str_pad(Department, 12, side = "right"),
    Sales
  )) %>%
  pull(line)

after_line_ex2 <- str_c(
  "Departmental sales ranged from ",
  fmt_dollar(sales_min_ex2, width = 7, digits = 2), " to ",
  fmt_dollar(sales_max_ex2, width = 7, digits = 2), "."
)

ex2_header <- str_c(
  str_pad("Manager", 10, side = "right"),
  str_pad("Department", 12, side = "right"),
  "Sales"
)

write_html_table(c(ex2_body, after_line_ex2), ex2_header,
                 file.path(out_dir, "example2.html"))
write_csv(ex2, file.path(out_dir, "example2.csv"), na = "")

# ------------------------------------------------------------------
# 3) example3.html – Statistics per sector and manager
# ------------------------------------------------------------------
ex3 <- grocery %>%
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

ex3_body <- ex3 %>%
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
  pull(line)

ex3_header <- str_c(
  str_pad("Sector", 12, side = "right"),
  str_pad("Manager", 10, side = "right"),
  "Sum", "Min", "Max", "Range", "Mean", "Std"
)

write_html_table(ex3_body, ex3_header,
                 file.path(out_dir, "example3.html"))
write_csv(ex3, file.path(out_dir, "example3.csv"), na = "")

# ------------------------------------------------------------------
# 4) example4.html – Percent of total sales with comment column
# ------------------------------------------------------------------
total_all <- sum(grocery$Sales, na.rm = TRUE)

ex4 <- grocery %>%
  mutate(
    Sector = apply_fmt(Sector, fmt_sector),
    Manager = apply_fmt(Manager, fmt_manager)
  ) %>%
  group_by(Sector, Manager) %>%
  summarise(Sum = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PctSum = Sum / total_all,
    Comment = ifelse(PctSum > 0.15,
                     "Sales substantially above expectations.",
                     " ")
  ) %>%
  arrange(Sector, Manager)

ex4_body <- ex4 %>%
  mutate(
    Sum    = fmt_dollar(Sum, width = 9, digits = 2),
    PctSum = fmt_percent(PctSum, width = 8, digits = 2)
  ) %>%
  transmute(line = str_c(
    str_pad(Sector, 12, side = "right"),
    str_pad(Manager, 10, side = "right"),
    Sum, " ", PctSum, " ", str_pad(Comment, 20, side = "right")
  )) %>%
  pull(line)

ex4_header <- str_c(
  str_pad("Sector", 12, side = "right"),
  str_pad("Manager", 10, side = "right"),
  "Total", " ", "Pct", " ", "Comment"
)

write_html_table(ex4_body, ex4_header,
                 file.path(out_dir, "example4.html"))
write_csv(ex4, file.path(out_dir, "example4.csv"), na = "")

# ------------------------------------------------------------------
# 5) example5.html – Report for western sectors with a blank line after each manager
# ------------------------------------------------------------------
ex5 <- grocery %>%
  filter(Sector %in% c("nw", "sw")) %>%
  mutate(
    Manager = apply_fmt(Manager, fmt_manager),
    Department = apply_fmt(Department, fmt_dept)
  ) %>%
  arrange(Manager, Department)

ex5_body <- ex5 %>%
  mutate(Sales = fmt_dollar(Sales, width = 7, digits = 2)) %>%
  group_by(Manager) %>%
  summarise(
    lines = str_c(
      str_c(
        str_pad(Manager, 10, side = "right"),
        str_pad(Department, 12, side = "right"),
        Sales
      ),
      collapse = "\n"
    ),
    .groups = "drop"
  ) %>%
  pull(lines) %>%
  str_c(collapse = "\n\n")   # blank line between managers

ex5_header <- str_c(
  str_pad("Manager", 10, side = "right"),
  str_pad("Department", 12, side = "right"),
  "Sales"
)

write_html_table(c(ex5_header, ex5_body), "", 
                 file.path(out_dir, "example5.html"))
write_csv(ex5, file.path(out_dir, "example5.csv"), na = "")

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
