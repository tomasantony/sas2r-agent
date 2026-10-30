out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# round half away from zero (SAS) helper
round_sas <- function(x, digits = 0) {
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5) / m
}

EmployeeID <- c(120185,121101,121092,121106,121072,120178,121027,121096,
  120112,121002,120687,120183,120999,120153,121059,120164,120120,121139,
  121075,120715,121042,120799,121105,121061,121062,121019,121032,121080,
  121099,120787,120731,120754,120683,120757,120789,120774,120796,120793,
  120668,120813,120804,120712,121145,120260,120259)
Salary <- c(25080,25390,25680,25880,26105,26165,26165,26335,26550,26650,
  26800,26910,27215,27260,27425,27450,27645,27700,28395,28535,28845,29070,
  29545,29815,30305,31320,31335,32235,32725,34000,34150,34760,36315,38545,
  39330,45155,47030,47155,47785,50865,55400,63640,84260,207885,433800)
Birth_raw <- c("07APR1978","28AUG1990","08MAR1978","02FEB1973","10JAN1983",
  "23NOV1958","05MAY1968","18MAY1973","17FEB1973","18SEP1958","25MAY1983",
  "10SEP1973","28DEC1963","07MAY1983","25OCT1963","26NOV1963","05MAY1948",
  "19AUG1963","23DEC1948","12JUN1983","04APR1983","23MAR1983","09MAY1983",
  "16JUL1958","28OCT1988","25JUN1990","24FEB1992","24JAN1963","19MAR1983",
  "22AUG1973","15MAR1963","02JUN1992","12NOV1958","18MAR1948","14JUL1968",
  "17SEP1982","12MAY1958","08AUG1973","23OCT1953","14SEP1973","11FEB1948",
  "12JUN1953","22NOV1953","02DEC1968","25JAN1968")
Hire_raw <- c("01DEC2010","01NOV2010","01AUG2006","01FEB2000","01SEP2008",
  "01APR1978","01DEC1993","01MAY1999","01JUL1994","01DEC1983","01AUG2008",
  "01DEC2010","01APR1988","01JAN2002","01APR1985","01FEB1986","01JAN1978",
  "01JUL1991","01JAN1978","01FEB2009","01NOV2003","01NOV2002","01JAN2007",
  "01JUL1988","01AUG2010","01JUN2008","01MAR2010","01SEP1991","01MAY2004",
  "01JAN2000","01SEP1987","01MAY2010","01JAN1978","01JAN1978","01DEC1986",
  "01MAR2006","01MAR1987","01JUN2000","01DEC1982","01JAN1997","01JAN1978",
  "01JAN1978","01APR1980","01NOV1988","01SEP1993")

# ONE strict format per source column: DATE9. -> %d%b%Y (fix case once)
fix_date9 <- function(x) {
  paste0(substr(x, 1, 2),
         paste0(toupper(substr(x, 3, 3)), tolower(substr(x, 4, 5))),
         substr(x, 6, nchar(x)))
}
BirthDate <- as.Date(fix_date9(Birth_raw), format = "%d%b%Y")
HireDate <- as.Date(fix_date9(Hire_raw), format = "%d%b%Y")
stopifnot(!any(is.na(BirthDate)), !any(is.na(HireDate)))

payroll <- data.frame(EmployeeID = EmployeeID, Salary = Salary,
                      BirthDate = BirthDate, HireDate = HireDate,
                      stringsAsFactors = FALSE)
# payroll already sorted by Salary (as in cards); enforce SAS order
payroll <- payroll[order(payroll$Salary, payroll$EmployeeID), , drop = FALSE]

# PROC FORMAT salgrp
salgrp_fmt <- function(x) {
  ifelse(is.na(x), NA_character_,
    ifelse(x < 30000, "Under $30,000",
      ifelse(x < 35000, "$30,000 to $35,000",
        ifelse(x < 50000, "$35,000 to $50,000", "Over $50,000"))))
}

payroll$SalGrpChar <- salgrp_fmt(payroll$Salary)

# DATA highlowsal; BY GROUPFORMAT salary; if first.salary then output; if last.salary then output;
# group = formatted value; groups contiguous because sorted by salary
# emulate FIRST./LAST. with duplication when N=1 (output twice)
ord <- order(payroll$Salary)
payroll_o <- payroll[ord, , drop = FALSE]
grp <- salgrp_fmt(payroll_o$Salary)
splits <- split(seq_len(nrow(payroll_o)), grp)
# preserve group order by first appearance (salary order)
grp_order <- unique(grp)
idx <- unlist(lapply(grp_order, function(g) {
  v <- splits[[g]]
  c(v[1], v[length(v)])
}))
highlowsal <- payroll_o[idx, , drop = FALSE]
rownames(highlowsal) <- NULL
# drop helper
highlowsal$SalGrpChar <- NULL
payroll$SalGrpChar <- NULL

# dataset CSVs (ISO dates)
payroll_out <- payroll
payroll_out$BirthDate <- format(payroll_out$BirthDate, "%Y-%m-%d")
payroll_out$HireDate <- format(payroll_out$HireDate, "%Y-%m-%d")
write.csv(payroll_out, file.path(out, "payroll.csv"), row.names = FALSE, na = "")

hl_out <- highlowsal
hl_out$BirthDate <- format(hl_out$BirthDate, "%Y-%m-%d")
hl_out$HireDate <- format(hl_out$HireDate, "%Y-%m-%d")
write.csv(hl_out, file.path(out, "highlowsal.csv"), row.names = FALSE, na = "")

# PROC REPORT data=highlowsal; columns EmployeeID Salary Salary=SalGrp HireDate;
# define salary display dollar12. (SAS DOLLAR, comma, $); define hiredate date9.; define salgrp salgrp.
dollar12_fmt <- function(x) {
  s <- paste0("$", formatC(x, format = "d", big.mark = ",", digits = 0))
  sprintf("%12s", s)
}
date9_fmt <- function(x) {
  toupper(format(x, "%d%b%Y"))
}

report_body <- data.frame(
  EmployeeID = highlowsal$EmployeeID,
  Salary = highlowsal$Salary,
  SalaryGroup = salgrp_fmt(highlowsal$Salary),
  HireDate = highlowsal$HireDate,
  stringsAsFactors = FALSE
)
# order already by salary
report_body <- report_body[order(report_body$Salary), , drop = FALSE]

# CSV of table body (ISO dates, numeric unrounded)
report_csv <- report_body
report_csv$HireDate <- format(report_csv$HireDate, "%Y-%m-%d")
write.csv(report_csv, file.path(out, "P093.csv"), row.names = FALSE, na = "")

# Text rendering (listing) with SAS formatting
disp_salary <- dollar12_fmt(report_body$Salary)
disp_hire <- date9_fmt(report_body$HireDate)
disp_emp <- sprintf("%10d", report_body$EmployeeID)
# header labels
title_txt <- "Lowest and Highest Salary by Salary Group"
h_emp <- sprintf("%10s", "EmployeeID")
h_sal <- sprintf("%12s", "Salary")
h_grp <- sprintf("%-20s", "Salary Group")
h_hire <- sprintf("%9s", "HireDate")

lines <- c(title_txt, "",
  paste(h_emp, h_sal, h_grp, h_hire, sep = " "),
  paste(paste(rep("-", 10), collapse = ""),
        paste(rep("-", 12), collapse = ""),
        paste(rep("-", 20), collapse = ""),
        paste(rep("-", 9), collapse = ""), sep = " "))
for (i in seq_len(nrow(report_body))) {
  grp_disp <- sprintf("%-20s", report_body$SalaryGroup[i])
  lines <- c(lines, paste(disp_emp[i], disp_salary[i], grp_disp,
                          sprintf("%9s", disp_hire[i]), sep = " "))
}
lines <- c(lines, "", "Title: Lowest and Highest Salary by Salary Group")
writeLines(lines, file.path(out, "P093.txt"))
