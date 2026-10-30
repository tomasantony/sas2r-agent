```r
# P093 - Using the BY GROUPFORMAT statement to process data
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."

# ---- 1. payroll data ----
raw <- "
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
payroll <- read.table(text = raw, col.names = c("EmployeeID", "Salary", "BirthDate", "HireDate"),
                      stringsAsFactors = FALSE)
parse_date9 <- function(x) {
  mon <- match(toupper(substr(x, 3, 5)), toupper(month.abb))
  as.Date(sprintf("%s-%02d-%s", substr(x, 6, 9), mon, substr(x, 1, 2)))
}
payroll$BirthDate <- parse_date9(payroll$BirthDate)
payroll$HireDate  <- parse_date9(payroll$HireDate)

# ---- 2. format salgrp ----
salgrp <- function(x) {
  ifelse(x < 30000, "Under $30,000",
  ifelse(x < 35000, "$30,000 to $35,000",
  ifelse(x < 50000, "$35,000 to $50,000", "Over $50,000")))
}

# ---- 3/4. BY GROUPFORMAT: first./last. on formatted value ----
grp <- salgrp(payroll$Salary)
n <- nrow(payroll)
first <- c(TRUE, grp[-1] != grp[-n])
last  <- c(grp[-1] != grp[-n], TRUE)
idx <- integer(0)
for (i in seq_len(n)) {
  if (first[i]) idx <- c(idx, i)
  if (last[i])  idx <- c(idx, i)
}
highlowsal <- payroll[idx, ]
rownames(highlowsal) <- NULL

ds <- highlowsal
ds$BirthDate <- format(ds$BirthDate, "%Y-%m-%d")
ds$HireDate  <- format(ds$HireDate, "%Y-%m-%d")
write.csv(ds, file.path(out, "highlowsal.csv"), row.names = FALSE, na = "")

# ---- 5. PROC REPORT ----
dollar12 <- function(x) paste0("$", formatC(x, format = "d", big.mark = ","))
date9 <- function(d) toupper(format(d, "%d%b%Y"))

rep <- data.frame(
  EmployeeID   = highlowsal$EmployeeID,
  Salary       = dollar12(highlowsal$Salary),
  Salary_Group = salgrp(highlowsal$Salary),
  HireDate     = date9(highlowsal$HireDate),
  stringsAsFactors = FALSE
)
write.csv(rep, file.path(out, "P093.csv"), row.names = FALSE, na = "")

hdr <- c("EmployeeID", "Salary", "Salary Group", "HireDate")
w <- c(10, 12, 18, 9)
fmt_row <- function(v) paste(formatC(v[1], width = w[1]), formatC(v[2], width = w[2]),
                             formatC(v[3], width = w[3]), formatC(v[4], width = w[4]))
lines <- c("Lowest and Highest Salary by Salary Group", "",
           fmt_row(hdr),
           apply(rep, 1, function(r) fmt_row(as.character(r))))
writeLines(lines, file.path(out, "P093.txt"))
cat("P093 done\n")
```