```r
# P093 translation: Using BY GROUPFORMAT to process data
# ------------------------------------------------------
# This script reproduces the SAS program outputs.
# It reads the inline data, creates a salary group format,
# extracts the first and last observation per salary group,
# and writes the resulting table as CSV and a formatted text listing
# (named P093.lst to match the SAS reference).

library(dplyr)
library(lubridate)

# ------------------------------------------------------------------
# Helper: round half up (away from zero) for positive numbers
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                ceiling(x * factor - 0.5) / factor)
  res
}

# ------------------------------------------------------------------
# 1. Read the inline data (equivalent to the SAS DATA step with INFILE CARDS)
data_txt <- "
120185 25080 07APR1978 01DEC2010 
121101 25390 28AUG1990 01NOV2010 
121092 25680 08MAR1978 01AUG2006 
121106 25880 02FEB1973 01FEB2000 
121072 26105 10JAN1983 01SEP2008 
120178 26165 23NOV1958 01APR1978 
121027 26165 05MAY1968 01DEC1993 
121096 26335 18MAY1973 01MAY1999 
120112 26550 17FEB1973 01JUL1994 
121002 26650 18SEP1958 01DEC1983 
120687 26800 25MAY1983 01AUG2008 
120183 26910 10SEP1973 01DEC2010 
120999 27215 28DEC1963 01APR1988 
120153 27260 07MAY1983 01JAN2002 
121059 27425 25OCT1963 01APR1985 
120164 27450 26NOV1963 01FEB1986 
120120 27645 05MAY1948 01JAN1978 
121139 27700 19AUG1963 01JUL1991 
121075 28395 23DEC1948 01JAN1978 
120715 28535 12JUN1983 01FEB2009 
121042 28845 04APR1983 01NOV2003 
120799 29070 23MAR1983 01NOV2002 
121105 29545 09MAY1983 01JAN2007 
121061 29815 16JUL1958 01JUL1988 
121062 30305 28OCT1988 01AUG2010 
121019 31320 25JUN1990 01JUN2008 
121032 31335 24FEB1992 01MAR2010 
121080 32235 24JAN1963 01SEP1991 
121099 32725 19MAR1983 01MAY2004 
120787 34000 22AUG1973 01JAN2000 
120731 34150 15MAR1963 01SEP1987 
120754 34760 02JUN1992 01MAY2010 
120683 36315 12NOV1958 01JAN1978 
120757 38545 18MAR1948 01JAN1978 
120789 39330 14JUL1968 01DEC1986 
120774 45155 17SEP1982 01MAR2006 
120796 47030 12MAY1958 01MAR1987 
120793 47155 08AUG1973 01JUN2000 
120668 47785 23OCT1953 01DEC1982 
120813 50865 14SEP1973 01JAN1997 
120804 55400 11FEB1948 01JAN1978 
120712 63640 12JUN1953 01JAN1978 
121145 84260 22NOV1953 01APR1980 
120260 207885 02DEC1968 01NOV1988 
120259 433800 25JAN1968 01SEP1993 
"

payroll <- read.table(text = data_txt,
                      col.names = c("EmployeeID", "Salary", "BirthDate", "HireDate"),
                      stringsAsFactors = FALSE)

# Convert dates from SAS DATE9. format (ddMONyyyy)
payroll <- payroll %>%
  mutate(
    BirthDate = as.Date(BirthDate, format = "%d%b%Y"),
    HireDate  = as.Date(HireDate,  format = "%d%b%Y")
  )

# ------------------------------------------------------------------
# 2. Define the salary group format (vectorised, mimicking PROC FORMAT)
salgrp_fmt <- function(sal) {
  ifelse(sal < 30000, "Under $30,000",
         ifelse(sal < 35000, "$30,000 to $35,000",
                ifelse(sal < 50000, "$35,000 to $50,000",
                       "Over $50,000")))
}

payroll <- payroll %>%
  mutate(SalGrp = salgrp_fmt(Salary))

# ------------------------------------------------------------------
# 3. Mimic BY GROUPFORMAT processing:
#    - Sort by Salary (as SAS would do for BY processing)
#    - For each salary group, keep the first and last observation
highlowsal <- payroll %>%
  arrange(Salary) %>%
  group_by(SalGrp) %>%
  slice(c(1, n())) %>%          # first and last within each group
  ungroup() %>%
  select(EmployeeID, Salary, SalGrp, HireDate)

# ------------------------------------------------------------------
# 4. Write the output dataset (CSV) – name matches SAS dataset (optional)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."   # fallback for testing

write.csv(highlowsal,
          file = file.path(out_dir, "highlowsal.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# 5. Create the PROC REPORT style listing (text rendering)
#    - Salary displayed with DOLLAR12. format (width 12, leading spaces, commas, no decimals)
#    - HireDate displayed with DATE9. format (e.g., 07APR1978)
#    - Salary Group displayed as the formatted label

fmt_salary_dollar12 <- function(x) {
  # integer dollars, no decimals, comma as thousands separator
  txt <- formatC(x, format = "f", big.mark = ",", digits = 0)
  txt <- paste0("$", txt)
  sprintf("%12s", txt)   # pad to width 12, right‑aligned
}

fmt_date9 <- function(d) {
  format(d, "%d%b%Y")
}

report_df <- highlowsal %>%
  mutate(
    Salary_fmt = fmt_salary_dollar12(Salary),
    HireDate_fmt = fmt_date9(HireDate),
    SalGrp_fmt = SalGrp
  ) %>%
  select(EmployeeID, Salary_fmt, SalGrp_fmt, HireDate_fmt)

# Build the text table (header + rows) matching PROC REPORT output
header_line <- sprintf("%-10s %-12s %-20s %-11s",
                       "EmployeeID", "Salary", "Salary Group", "HireDate")
row_lines <- apply(report_df, 1, function(r) {
  sprintf("%-10s %-12s %-20s %-11s",
          r["EmployeeID"],
          r["Salary_fmt"],
          r["SalGrp_fmt"],
          r["HireDate_fmt"])
})

txt_content <- c(header_line, row_lines)

# Write the plain‑text listing (SAS expects a .lst file)
writeLines(txt_content, con = file.path(out_dir, "P093.lst"))

# Also write a CSV version of the listing (same columns as highlowsal)
write.csv(highlowsal,
          file = file.path(out_dir, "P093.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# End of script
```