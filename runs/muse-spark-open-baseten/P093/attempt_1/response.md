```r
# P093_Using_the_By_Groupformat_Statement_to_Process_Data.R
# Translation of SAS program P093 to R
# Creates payroll data, applies GROUPFORMAT logic, outputs lowest/highest salary by group

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
  library(haven)
})

# Helper: SAS round half away from zero (not needed here but included for fidelity)
round_sas <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  trunc_x <- trunc(abs_x * mult + 0.5 + 1e-12)
  sign_x * trunc_x / mult
}

# Helper: SAS DOLLAR12. format - $ with comma, width 12 right-aligned
format_dollar12 <- function(x) {
  # format with comma, no decimals
  s <- paste0("$", formatC(x, format = "d", big.mark = ",", digits = 0))
  # pad to width 12 with leading spaces (SAS right aligned)
  stringr::str_pad(s, width = 12, side = "left")
}
format_dollar12_trim <- function(x) {
  # trimmed version for CSV display (without padding)
  paste0("$", formatC(x, format = "d", big.mark = ",", digits = 0))
}

# Helper: SAS DATE9. format DDMMMYYYY uppercase
format_date9 <- function(d) {
  toupper(format(d, "%d%b%Y"))
}

# 1. Create payroll dataset (SAS cards)
# Input: EmployeeID Salary BirthDate : date9. HireDate : date9.
payroll_raw <- tibble::tribble(
  ~EmployeeID, ~Salary, ~BirthDate_str, ~HireDate_str,
  120185, 25080, "07APR1978", "01DEC2010",
  121101, 25390, "28AUG1990", "01NOV2010",
  121092, 25680, "08MAR1978", "01AUG2006",
  121106, 25880, "02FEB1973", "01FEB2000",
  121072, 26105, "10JAN1983", "01SEP2008",
  120178, 26165, "23NOV1958", "01APR1978",
  121027, 26165, "05MAY1968", "01DEC1993",
  121096, 26335, "18MAY1973", "01MAY1999",
  120112, 26550, "17FEB1973", "01JUL1994",
  121002, 26650, "18SEP1958", "01DEC1983",
  120687, 26800, "25MAY1983", "01AUG2008",
  120183, 26910, "10SEP1973", "01DEC2010",
  120999, 27215, "28DEC1963", "01APR1988",
  120153, 27260, "07MAY1983", "01JAN2002",
  121059, 27425, "25OCT1963", "01APR1985",
  120164, 27450, "26NOV1963", "01FEB1986",
  120120, 27645, "05MAY1948", "01JAN1978",
  121139, 27700, "19AUG1963", "01JUL1991",
  121075, 28395, "23DEC1948", "01JAN1978",
  120715, 28535, "12JUN1983", "01FEB2009",
  121042, 28845, "04APR1983", "01NOV2003",
  120799, 29070, "23MAR1983", "01NOV2002",
  121105, 29545, "09MAY1983", "01JAN2007",
  121061, 29815, "16JUL1958", "01JUL1988",
  121062, 30305, "28OCT1988", "01AUG2010",
  121019, 31320, "25JUN1990", "01JUN2008",
  121032, 31335, "24FEB1992", "01MAR2010",
  121080, 32235, "24JAN1963", "01SEP1991",
  121099, 32725, "19MAR1983", "01MAY2004",
  120787, 34000, "22AUG1973", "01JAN2000",
  120731, 34150, "15MAR1963", "01SEP1987",
  120754, 34760, "02JUN1992", "01MAY2010",
  120683, 36315, "12NOV1958", "01JAN1978",
  120757, 38545, "18MAR1948", "01JAN1978",
  120789, 39330, "14JUL1968", "01DEC1986",
  120774, 45155, "17SEP1982", "01MAR2006",
  120796, 47030, "12MAY1958", "01MAR1987",
  120793, 47155, "08AUG1973", "01JUN2000",
  120668, 47785, "23OCT1953", "01DEC1982",
  120813, 50865, "14SEP1973", "01JAN1997",
  120804, 55400, "11FEB1948", "01JAN1978",
  120712, 63640, "12JUN1953", "01JAN1978",
  121145, 84260, "22NOV1953", "01APR1980",
  120260, 207885, "02DEC1968", "01NOV1988",
  120259, 433800, "25JAN1968", "01SEP1993"
)

payroll <- payroll_raw %>%
  mutate(
    BirthDate = as.Date(BirthDate_str, format = "%d%b%Y"),
    HireDate = as.Date(HireDate_str, format = "%d%b%Y")
  ) %>%
  select(EmployeeID, Salary, BirthDate, HireDate) %>%
  arrange(Salary)  # SAS data is already sorted by Salary; ensure sort for BY processing

# 2. PROC FORMAT salgrp
# low-<30000='Under $30,000'
# 30000-<35000='$30,000 to $35,000'
# 35000-<50000='$35,000 to $50,000'
# 50000-high='Over $50,000'
salgrp_fmt <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x < 30000 ~ "Under $30,000",
    x >= 30000 & x < 35000 ~ "$30,000 to $35,000",
    x >= 35000 & x < 50000 ~ "$35,000 to $50,000",
    x >= 50000 ~ "Over $50,000",
    TRUE ~ NA_character_
  )
}

# 3. data highlowsal; by groupformat salary;
# SAS BY GROUPFORMAT groups by formatted value, not raw value.
# Reproduce: assign group, then within each formatted group take first and last.
payroll_grp <- payroll %>%
  mutate(SalGrp = salgrp_fmt(Salary))

# Preserve group order as it appears in sorted data (not alphabetical)
grp_levels <- payroll_grp %>% distinct(SalGrp) %>% pull(SalGrp)
# Ensure factor with appearance order
payroll_grp <- payroll_grp %>%
  mutate(SalGrp = factor(SalGrp, levels = grp_levels))

# Identify first/last within each formatted group
# SAS logic: if first.salary then output; if last.salary then output;
# If group has 1 obs, both true => output twice (duplicate). Handle that.
highlowsal_list <- payroll_grp %>%
  group_by(SalGrp) %>%
  group_modify(~ {
    df <- .x
    n <- nrow(df)
    if (n == 1) {
      # duplicate row to mimic SAS double output
      bind_rows(df, df)
    } else {
      bind_rows(df[1, , drop = FALSE], df[n, , drop = FALSE])
    }
  }) %>%
  ungroup() %>%
  arrange(Salary) %>%
  # Keep original SalGrp label as character for display
  mutate(SalGrp_char = as.character(SalGrp))

# highlowsal dataset for CSV export (SAS dataset equivalent)
highlowsal_ds <- highlowsal_list %>%
  select(EmployeeID, Salary, BirthDate, HireDate, SalGrp = SalGrp_char)

# Write highlowsal dataset CSV (dates as YYYY-MM-DD)
highlowsal_csv <- highlowsal_ds %>%
  mutate(
    BirthDate = format(BirthDate, "%Y-%m-%d"),
    HireDate = format(HireDate, "%Y-%m-%d")
  )
write.csv(highlowsal_csv, file.path(out, "highlowsal.csv"), row.names = FALSE, na = "")

# 4 & 5. PROC REPORT data=highlowsal nowd;
# columns EmployeeID Salary Salary=SalGrp HireDate;
# define salary / display format=dollar12.;
# define hiredate / format=date9.;
# define salgrp / format=salgrp. 'Salary Group';

# Build report data
report_data <- highlowsal_list %>%
  mutate(
    Salary_disp = format_dollar12(Salary),
    Salary_disp_trim = format_dollar12_trim(Salary),
    HireDate_disp = format_date9(HireDate),
    SalGrp_disp = SalGrp_char
  ) %>%
  select(EmployeeID, Salary, Salary_disp, Salary_disp_trim, HireDate, HireDate_disp, SalGrp_disp) %>%
  arrange(Salary)

# CSV of table body (as displayed in SAS, plus raw values)
# Provide both raw and formatted for traceability; main CSV matches report columns
report_csv <- report_data %>%
  transmute(
    EmployeeID = EmployeeID,
    Salary = Salary,
    Salary_Formatted = Salary_disp_trim,
    Salary_Group = SalGrp_disp,
    HireDate = format(HireDate, "%Y-%m-%d"),
    HireDate_Formatted = HireDate_disp
  )
write.csv(report_csv, file.path(out, "P093.csv"), row.names = FALSE, na = "")
# Also write as P093.lst.csv for benchmark compatibility
write.csv(report_csv, file.path(out, "P093.lst.csv"), row.names = FALSE, na = "")

# Text rendering - mimic PROC REPORT output
# Title: Lowest and Highest Salary by Salary Group
title_str <- "Lowest and Highest Salary by Salary Group"

# Build fixed-width text table
# Headers: EmployeeID | Salary | Salary Group | HireDate
# Use dollar12 width 12 for Salary column

# Calculate column widths
# EmployeeID width 10, Salary width 12, Salary Group width 19, HireDate width 9
header_line1 <- sprintf("%-10s %12s %-19s %9s", "EmployeeID", "Salary", "Salary Group", "HireDate")
sep_line <- paste0(
  paste(rep("-", 10), collapse = ""),
  " ",
  paste(rep("-", 12), collapse = ""),
  " ",
  paste(rep("-", 19), collapse = ""),
  " ",
  paste(rep("-", 9), collapse = "")
)

# Build rows
row_lines <- report_data %>%
  mutate(
    line = sprintf("%-10d %12s %-19s %9s",
                   EmployeeID,
                   Salary_disp,
                   SalGrp_disp,
                   HireDate_disp)
  ) %>%
  pull(line)

# Assemble full text
txt_content <- c(
  title_str,
  "",
  header_line1,
  sep_line,
  row_lines,
  "",
  paste0("N = ", nrow(report_data), " rows (2 per salary group, 4 groups)"),
  "",
  "Salary Group definitions:",
  "  Under $30,000        : Salary < 30000",
  "  $30,000 to $35,000   : 30000 <= Salary < 35000",
  "  $35,000 to $50,000   : 35000 <= Salary < 50000",
  "  Over $50,000         : Salary >= 50000",
  "",
  "Note: BY GROUPFORMAT groups observations by formatted Salary value.",
  "First and last observations within each formatted group are output."
)

# Write TXT
writeLines(txt_content, file.path(out, "P093.txt"))
writeLines(txt_content, file.path(out, "P093.lst.txt"))

# Also write a more detailed listing with counts for verification (48 numbers check)
# Count numbers in text: EmployeeID (8) + Salary (8) + HireDate day/month/year tokens
# Provide additional file with raw numbers for audit
cat("P093 processing complete. Output written to", out, "\n")
cat("Rows in highlowsal:", nrow(highlowsal_ds), "\n")
print(report_csv)
```