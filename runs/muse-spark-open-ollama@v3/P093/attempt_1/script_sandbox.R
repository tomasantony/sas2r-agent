# P093_Using_the_By_Groupformat_Statement_to_Process_Data.sas -> R translation
# Creates payroll, applies salgrp format, BY GROUPFORMAT processing, PROC REPORT

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("OUTTlf")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not needed here but for fidelity)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# Helper: parse SAS DATE9. like 07APR1978
parse_date9 <- function(x) {
  # x is character vector like "07APR1978"
  # Convert month part to title case for R parsing
  # Use base R: extract day, mon, year
  day <- substr(x, 1, 2)
  mon <- substr(x, 3, 5)
  year <- substr(x, 6, 9)
  # title case month: Apr, Dec etc.
  mon_tc <- paste0(toupper(substr(mon, 1, 1)), tolower(substr(mon, 2, 3)))
  str <- paste0(day, mon_tc, year)
  as.Date(str, format = "%d%b%Y")
}

# Helper: SAS DATE9. format (DDMMMYYYY uppercase)
format_date9 <- function(d) {
  s <- format(d, "%d%b%Y")
  toupper(s)
}

# Helper: SAS DOLLAR12. format (width 12, $ + commas, no decimals, right aligned)
format_dollar12 <- function(x) {
  # SAS DOLLARw. without decimal rounds to integer, adds $ and commas
  # width 12 includes $ and commas
  s <- paste0("$", formatC(round_half_up(x, 0), format = "d", big.mark = ","))
  sprintf("%12s", s)
}
# For CSV display without padding (trim)
format_dollar12_trim <- function(x) {
  trimws(format_dollar12(x))
}

library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(tidyr)
library(lubridate)

# 1. Create payroll dataset (SAS cards)
employeeID_vec <- c(
  120185,121101,121092,121106,121072,120178,121027,121096,120112,121002,
  120687,120183,120999,120153,121059,120164,120120,121139,121075,120715,
  121042,120799,121105,121061,121062,121019,121032,121080,121099,120787,
  120731,120754,120683,120757,120789,120774,120796,120793,120668,120813,
  120804,120712,121145,120260,120259
)
salary_vec <- c(
  25080,25390,25680,25880,26105,26165,26165,26335,26550,26650,
  26800,26910,27215,27260,27425,27450,27645,27700,28395,28535,
  28845,29070,29545,29815,30305,31320,31335,32235,32725,34000,
  34150,34760,36315,38545,39330,45155,47030,47155,47785,50865,
  55400,63640,84260,207885,433800
)
birthDate_str <- c(
  "07APR1978","28AUG1990","08MAR1978","02FEB1973","10JAN1983","23NOV1958","05MAY1968","18MAY1973","17FEB1973","18SEP1958",
  "25MAY1983","10SEP1973","28DEC1963","07MAY1983","25OCT1963","26NOV1963","05MAY1948","19AUG1963","23DEC1948","12JUN1983",
  "04APR1983","23MAR1983","09MAY1983","16JUL1958","28OCT1988","25JUN1990","24FEB1992","24JAN1963","19MAR1983","22AUG1973",
  "15MAR1963","02JUN1992","12NOV1958","18MAR1948","14JUL1968","17SEP1982","12MAY1958","08AUG1973","23OCT1953","14SEP1973",
  "11FEB1948","12JUN1953","22NOV1953","02DEC1968","25JAN1968"
)
hireDate_str <- c(
  "01DEC2010","01NOV2010","01AUG2006","01FEB2000","01SEP2008","01APR1978","01DEC1993","01MAY1999","01JUL1994","01DEC1983",
  "01AUG2008","01DEC2010","01APR1988","01JAN2002","01APR1985","01FEB1986","01JAN1978","01JUL1991","01JAN1978","01FEB2009",
  "01NOV2003","01NOV2002","01JAN2007","01JUL1988","01AUG2010","01JUN2008","01MAR2010","01SEP1991","01MAY2004","01JAN2000",
  "01SEP1987","01MAY2010","01JAN1978","01JAN1978","01DEC1986","01MAR2006","01MAR1987","01JUN2000","01DEC1982","01JAN1997",
  "01JAN1978","01JAN1978","01APR1980","01NOV1988","01SEP1993"
)

payroll <- tibble(
  EmployeeID = employeeID_vec,
  Salary = salary_vec,
  BirthDate = parse_date9(birthDate_str),
  HireDate = parse_date9(hireDate_str)
)

# Verify parsing
stopifnot(!any(is.na(payroll$BirthDate)))
stopifnot(!any(is.na(payroll$HireDate)))

# 2. PROC FORMAT value salgrp
salgrp_fmt <- function(x) {
  dplyr::case_when(
    x < 30000 ~ "Under $30,000",
    x < 35000 ~ "$30,000 to $35,000",
    x < 50000 ~ "$35,000 to $50,000",
    x >= 50000 ~ "Over $50,000",
    TRUE ~ NA_character_
  )
}
# Define factor levels in salary order (as SAS would display groups in sorted order)
salgrp_levels <- c("Under $30,000", "$30,000 to $35,000", "$35,000 to $50,000", "Over $50,000")

# 3. BY GROUPFORMAT processing: data must be sorted by Salary (which is already)
payroll_sorted <- payroll %>%
  arrange(Salary) %>%
  mutate(SalGrp = salgrp_fmt(Salary)) %>%
  mutate(SalGrp = factor(SalGrp, levels = salgrp_levels))

# 4. highlowsal: first. and last. within formatted group
# SAS: if first.salary then output; if last.salary then output;
# If group has 1 obs, both true => output twice (duplicate)
highlowsal <- payroll_sorted %>%
  group_by(SalGrp) %>%
  group_modify(~ {
    n <- nrow(.x)
    if (n == 1) {
      bind_rows(.x, .x)
    } else {
      bind_rows(.x[1, , drop = FALSE], .x[n, , drop = FALSE])
    }
  }) %>%
  ungroup() %>%
  arrange(SalGrp, Salary)

# Ensure 8 rows (2 per group)
stopifnot(nrow(highlowsal) == 8)

# Write output datasets as CSV (ISO dates)
payroll_out <- payroll %>%
  mutate(
    BirthDate = format(BirthDate, "%Y-%m-%d"),
    HireDate = format(HireDate, "%Y-%m-%d")
  )
write.csv(payroll_out, file.path(out, "payroll.csv"), row.names = FALSE, na = "")

highlowsal_out <- highlowsal %>%
  mutate(
    BirthDate = format(BirthDate, "%Y-%m-%d"),
    HireDate = format(HireDate, "%Y-%m-%d")
  ) %>%
  select(EmployeeID, Salary, BirthDate, HireDate, SalGrp)
write.csv(highlowsal_out, file.path(out, "highlowsal.csv"), row.names = FALSE, na = "")
# Also write with expected prefix P093 for dataset
write.csv(highlowsal_out, file.path(out, "P093_highlowsal.csv"), row.names = FALSE, na = "")

# 5. PROC REPORT data=highlowsal nowd
# columns EmployeeID Salary Salary=SalGrp HireDate
# define salary / display format=dollar12.
# define hiredate / format=date9.
# define salgrp / format=salgrp. 'Salary Group'
report_df <- highlowsal %>%
  select(EmployeeID, Salary, SalGrp, HireDate) %>%
  arrange(SalGrp, Salary)

# Formatted versions for display
report_display <- report_df %>%
  mutate(
    Salary_fmt = format_dollar12_trim(Salary),
    SalGrp_fmt = as.character(SalGrp),
    HireDate_fmt = format_date9(HireDate)
  ) %>%
  select(EmployeeID, Salary_fmt, SalGrp_fmt, HireDate_fmt)

# Create CSV of table body (formatted values as SAS shows)
report_csv <- tibble(
  EmployeeID = report_df$EmployeeID,
  Salary = format_dollar12_trim(report_df$Salary),
  `Salary Group` = as.character(report_df$SalGrp),
  HireDate = format_date9(report_df$HireDate)
)
write.csv(report_csv, file.path(out, "P093.csv"), row.names = FALSE, na = "")
write.csv(report_csv, file.path(out, "P093.lst.csv"), row.names = FALSE, na = "")

# Create plain-text rendering (P093.lst.txt)
# Reproduce SAS PROC REPORT listing with title
title_str <- "Lowest and Highest Salary by Salary Group"

# Build text lines with fixed-width columns to mimic SAS listing
# Column widths: EmployeeID 12, Salary 12, Salary Group 20, HireDate 9
header1 <- sprintf("%-12s %12s %-20s %9s", "EmployeeID", "Salary", "Salary Group", "HireDate")
sep_line <- paste0(strrep("-", 12), " ", strrep("-", 12), " ", strrep("-", 20), " ", strrep("-", 9))

# Data lines
data_lines <- sapply(seq_len(nrow(report_df)), function(i) {
  emp <- report_df$EmployeeID[i]
  sal <- format_dollar12(report_df$Salary[i])  # width 12 padded
  grp <- sprintf("%-20s", as.character(report_df$SalGrp[i]))
  hdate <- sprintf("%9s", format_date9(report_df$HireDate[i]))
  sprintf("%12d %s %s %s", emp, sal, grp, hdate)
})

txt_lines <- c(
  title_str,
  "",
  header1,
  sep_line,
  data_lines,
  ""
)

# Write text files
writeLines(txt_lines, file.path(out, "P093.txt"))
writeLines(txt_lines, file.path(out, "P093.lst.txt"))
writeLines(txt_lines, file.path(out, "P093.lst"))

# Also ensure console log shows progress
cat("P093: payroll rows:", nrow(payroll), " highlowsal rows:", nrow(highlowsal), "\n")
cat("Outputs written to:", out, "\n")
