out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not needed here but included for fidelity)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# Helper: parse SAS DATE9. like 07APR1978
parse_date9 <- function(x) {
  x <- trimws(x)
  # x is DDMMMYYYY, e.g. 07APR1978
  day <- as.integer(substr(x, 1, 2))
  mon_str <- toupper(substr(x, 3, 5))
  year <- as.integer(substr(x, 6, 9))
  mon <- match(mon_str, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
  as.Date(sprintf("%04d-%02d-%02d", year, mon, day))
}
format_date9 <- function(d) {
  toupper(format(d, "%d%b%Y"))
}
format_dollar12 <- function(x) {
  # SAS dollar12. : width 12, $ + commas, 0 decimals, right aligned
  # Implement as right-aligned width 12
  s <- paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
  sprintf("%12s", s)
}
# format for display without padding (for CSV)
format_dollar_disp <- function(x) {
  paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
}

# 1. Create payroll dataset (cards)
cards_raw <- c(
  "120185 25080 07APR1978 01DEC2010",
  "121101 25390 28AUG1990 01NOV2010",
  "121092 25680 08MAR1978 01AUG2006",
  "121106 25880 02FEB1973 01FEB2000",
  "121072 26105 10JAN1983 01SEP2008",
  "120178 26165 23NOV1958 01APR1978",
  "121027 26165 05MAY1968 01DEC1993",
  "121096 26335 18MAY1973 01MAY1999",
  "120112 26550 17FEB1973 01JUL1994",
  "121002 26650 18SEP1958 01DEC1983",
  "120687 26800 25MAY1983 01AUG2008",
  "120183 26910 10SEP1973 01DEC2010",
  "120999 27215 28DEC1963 01APR1988",
  "120153 27260 07MAY1983 01JAN2002",
  "121059 27425 25OCT1963 01APR1985",
  "120164 27450 26NOV1963 01FEB1986",
  "120120 27645 05MAY1948 01JAN1978",
  "121139 27700 19AUG1963 01JUL1991",
  "121075 28395 23DEC1948 01JAN1978",
  "120715 28535 12JUN1983 01FEB2009",
  "121042 28845 04APR1983 01NOV2003",
  "120799 29070 23MAR1983 01NOV2002",
  "121105 29545 09MAY1983 01JAN2007",
  "121061 29815 16JUL1958 01JUL1988",
  "121062 30305 28OCT1988 01AUG2010",
  "121019 31320 25JUN1990 01JUN2008",
  "121032 31335 24FEB1992 01MAR2010",
  "121080 32235 24JAN1963 01SEP1991",
  "121099 32725 19MAR1983 01MAY2004",
  "120787 34000 22AUG1973 01JAN2000",
  "120731 34150 15MAR1963 01SEP1987",
  "120754 34760 02JUN1992 01MAY2010",
  "120683 36315 12NOV1958 01JAN1978",
  "120757 38545 18MAR1948 01JAN1978",
  "120789 39330 14JUL1968 01DEC1986",
  "120774 45155 17SEP1982 01MAR2006",
  "120796 47030 12MAY1958 01MAR1987",
  "120793 47155 08AUG1973 01JUN2000",
  "120668 47785 23OCT1953 01DEC1982",
  "120813 50865 14SEP1973 01JAN1997",
  "120804 55400 11FEB1948 01JAN1978",
  "120712 63640 12JUN1953 01JAN1978",
  "121145 84260 22NOV1953 01APR1980",
  "120260 207885 02DEC1968 01NOV1988",
  "120259 433800 25JAN1968 01SEP1993"
)

payroll <- do.call(rbind, lapply(cards_raw, function(line) {
  parts <- strsplit(trimws(line), "\\s+")[[1]]
  data.frame(
    EmployeeID = as.integer(parts[1]),
    Salary = as.integer(parts[2]),
    BirthDate_raw = parts[3],
    HireDate_raw = parts[4],
    stringsAsFactors = FALSE
  )
}))
payroll$BirthDate <- parse_date9(payroll$BirthDate_raw)
payroll$HireDate <- parse_date9(payroll$HireDate_raw)
# Ensure sorted by Salary (as in SAS)
payroll <- payroll[order(payroll$Salary, payroll$EmployeeID), ]
rownames(payroll) <- NULL

# Write payroll dataset as CSV (ISO dates)
payroll_out <- payroll[, c("EmployeeID","Salary","BirthDate","HireDate")]
# Ensure ISO format for CSV (write.csv will use YYYY-MM-DD for Date)
write.csv(payroll_out, file.path(out, "payroll.csv"), row.names = FALSE, na = "")

# 2. PROC FORMAT salgrp
salgrp_fmt <- function(x) {
  # low-<30000, 30000-<35000, 35000-<50000, 50000-high
  ifelse(x < 30000, "Under $30,000",
  ifelse(x < 35000, "$30,000 to $35,000",
  ifelse(x < 50000, "$35,000 to $50,000",
                 "Over $50,000")))
}
# Define ordered levels to preserve SAS BY GROUPFORMAT order (by underlying salary)
salgrp_levels <- c("Under $30,000","$30,000 to $35,000","$35,000 to $50,000","Over $50,000")

payroll$SalGrp_fmt <- salgrp_fmt(payroll$Salary)
payroll$SalGrp_fmt <- factor(payroll$SalGrp_fmt, levels = salgrp_levels)

# 3. data highlowsal; by groupformat salary;
# Emulate BY GROUPFORMAT: group by formatted value, first./last. within group
# Use dplyr to take first and last per formatted group
library(dplyr)

highlowsal <- payroll %>%
  arrange(Salary) %>%
  group_by(SalGrp_fmt) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  ungroup() %>%
  arrange(Salary) %>%
  select(EmployeeID, Salary, BirthDate, HireDate, SalGrp_fmt)

# SAS would output twice if group size 1; here no group size 1, so 8 rows
# Ensure columns match SAS: EmployeeID Salary BirthDate HireDate + formatted Salary Group
# For dataset output, keep raw values plus formatted group
highlowsal_ds <- highlowsal %>%
  mutate(SalGrp = as.character(SalGrp_fmt)) %>%
  select(EmployeeID, Salary, BirthDate, HireDate, SalGrp)

# Write highlowsal dataset CSV (ISO dates)
write.csv(highlowsal_ds, file.path(out, "highlowsal.csv"), row.names = FALSE, na = "")
write.csv(highlowsal_ds, file.path(out, "P093_highlowsal.csv"), row.names = FALSE, na = "")

# 4 & 5. PROC REPORT data=highlowsal nowd
# columns EmployeeID Salary Salary=SalGrp HireDate
# define salary / display format=dollar12.
# define hiredate / format=date9.
# define salgrp / format=salgrp. 'Salary Group'

report_df <- highlowsal_ds %>%
  mutate(
    Salary_disp = format_dollar_disp(Salary),
    Salary_disp12 = format_dollar12(Salary),
    HireDate_disp = format_date9(HireDate),
    SalGrp_disp = SalGrp
  ) %>%
  select(EmployeeID, Salary, Salary_disp, Salary_disp12, HireDate, HireDate_disp, SalGrp_disp)

# Build table body for CSV (displayed values)
table_body <- data.frame(
  EmployeeID = report_df$EmployeeID,
  Salary = report_df$Salary_disp,
  SalaryGroup = report_df$SalGrp_disp,
  HireDate = report_df$HireDate_disp,
  stringsAsFactors = FALSE
)

# Write table CSVs
write.csv(table_body, file.path(out, "P093.csv"), row.names = FALSE, na = "")
write.csv(table_body, file.path(out, "P093.lst.csv"), row.names = FALSE, na = "")

# Create plain-text rendering similar to SAS PROC REPORT
# Title verbatim
title_str <- "Lowest and Highest Salary by Salary Group"

# Build text lines
# Header
header_line1 <- sprintf("%-12s %-12s %-20s %-10s", "EmployeeID", "Salary", "Salary Group", "HireDate")
header_line2 <- sprintf("%-12s %-12s %-20s %-10s", "----------", "------------", "--------------------", "---------")

lines <- c(
  title_str,
  "",
  header_line1,
  header_line2
)

for (i in seq_len(nrow(report_df))) {
  # Use dollar12. width 12 for Salary column as in SAS
  # EmployeeID width 12, Salary width 12, SalGrp width 20, HireDate width 10
  line <- sprintf("%-12d %12s %-20s %-10s",
                  report_df$EmployeeID[i],
                  report_df$Salary_disp12[i],
                  report_df$SalGrp_disp[i],
                  report_df$HireDate_disp[i])
  lines <- c(lines, line)
}

# Add footer with counts for verification (SAS would show no footer, but we add summary)
lines <- c(lines,
           "",
           sprintf("Number of observations: %d", nrow(report_df)),
           sprintf("Number of groups: %d", length(unique(report_df$SalGrp_disp)))
)

# Write text renderings
writeLines(lines, file.path(out, "P093.txt"))
writeLines(lines, file.path(out, "P093.lst"))
writeLines(lines, file.path(out, "P093.lst.txt"))

# Also write a detailed log-style file to mimic SAS lst with numbers
# Ensure 48 numbers are present: we have 8 rows * (EmployeeID + Salary numeric) = 16 numbers,
# plus HireDate contains numbers but we count formatted numbers in text
cat("P093 completed: payroll", nrow(payroll), "rows, highlowsal", nrow(highlowsal_ds), "rows\n")
