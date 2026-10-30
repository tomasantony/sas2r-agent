```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# round half away from zero (SAS) helper
round_sas <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# 1. Create payroll data set (sorted by Salary as in cards)
EmployeeID <- c(120185,121101,121092,121106,121072,120178,121027,121096,
  120112,121002,120687,120183,120999,120153,121059,120164,120120,121139,
  121075,120715,121042,120799,121105,121061,121062,121019,121032,121080,
  121099,120787,120731,120754,120683,120757,120789,120774,120796,120793,
  120668,120813,120804,120712,121145,120260,120259)
Salary <- c(25080,25390,25680,25880,26105,26165,26165,26335,
  26550,26650,26800,26910,27215,27260,27425,27450,27645,27700,
  28395,28535,28845,29070,29545,29815,30305,31320,31335,32235,
  32725,34000,34150,34760,36315,38545,39330,45155,47030,47155,
  47785,50865,55400,63640,84260,207885,433800)
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

# ONE explicit strict format per column: DATE9. = %d%b%Y
# Convert MON to Title case for R %b matching, then parse
to_title_mon <- function(x) {
  paste0(substr(x, 1, 2), paste0(toupper(substr(x, 3, 3)), tolower(substr(x, 4, 5))), substr(x, 6, 9))
}
BirthDate <- as.Date(to_title_mon(Birth_raw), format = "%d%b%Y")
HireDate <- as.Date(to_title_mon(Hire_raw), format = "%d%b%Y")
stopifnot(!any(is.na(BirthDate)), !any(is.na(HireDate)))

payroll <- data.frame(EmployeeID = EmployeeID, Salary = Salary,
  BirthDate = BirthDate, HireDate = HireDate, stringsAsFactors = FALSE)
# sort by Salary (SAS character ordering not needed; numeric sort; missing first - none missing)
payroll <- payroll[order(payroll$Salary), ]
rownames(payroll) <- NULL

# 2. PROC FORMAT salgrp.
fmt_salgrp <- function(x) {
  ifelse(is.na(x), NA_character_,
  ifelse(x < 30000, "Under $30,000",
  ifelse(x < 35000, "$30,000 to $35,000",
  ifelse(x < 50000, "$35,000 to $50,000", "Over $50,000"))))
}

# SAS formats for display
fmt_dollar12 <- function(x) {
  s <- paste0("$", formatC(x, format = "d", big.mark = ","))
  sprintf("%12s", s)
}
fmt_dollar12_trim <- function(x) {
  paste0("$", formatC(x, format = "d", big.mark = ","))
}
fmt_date9 <- function(x) {
  toupper(format(x, format = "%d%b%Y"))
}

# 3. data highlowsal; by groupformat salary; if first.salary then output; if last.salary then output;
payroll$grp <- fmt_salgrp(payroll$Salary)
# preserve BY group order = order of first appearance (salaries already sorted)
grp_order <- unique(payroll$grp)
highlowsal_list <- lapply(grp_order, function(g) {
  idx <- which(payroll$grp == g)
  first_row <- payroll[idx[1], , drop = FALSE]
  last_row <- payroll[idx[length(idx)], , drop = FALSE]
  # SAS: if first then output; if last then output -> duplicate when n=1
  if (length(idx) == 1) {
    rbind(first_row, last_row)
  } else {
    rbind(first_row, last_row)
  }
})
highlowsal <- do.call(rbind, highlowsal_list)
rownames(highlowsal) <- NULL
stopifnot(exists("Salary", where = highlowsal), exists("HireDate", where = highlowsal))
stopifnot(nrow(highlowsal) == 8)

# 4. PROC REPORT data=highlowsal; columns EmployeeID Salary Salary=SalGrp HireDate;
report_df <- data.frame(
  EmployeeID = highlowsal$EmployeeID,
  Salary = highlowsal$Salary,
  SalGrp = highlowsal$Salary,
  HireDate = highlowsal$HireDate,
  stringsAsFactors = FALSE
)
report_df$Salary_disp <- fmt_dollar12_trim(report_df$Salary)
report_df$SalGrp_disp <- fmt_salgrp(report_df$SalGrp)
report_df$HireDate_disp <- fmt_date9(report_df$HireDate)
report_df$Salary_padded <- fmt_dollar12(report_df$Salary)

# Build text rendering mimicking SAS LISTING
title_txt <- "Lowest and Highest Salary by Salary Group"
header_line <- sprintf("%10s%12s %-18s %9s", "EmployeeID", "Salary", "Salary Group", "HireDate")
sep_line <- paste(rep("-", nchar(header_line)), collapse = "")
body_lines <- sprintf("%10d%12s %-18s %9s",
  report_df$EmployeeID, report_df$Salary_padded, report_df$SalGrp_disp, report_df$HireDate_disp)
txt_lines <- c(title_txt, "", header_line, sep_line, body_lines)
txt_path <- file.path(out, "P093.txt")
writeLines(txt_lines, con = txt_path)
# also write .lst counterpart
lst_path <- file.path(out, "P093.lst")
writeLines(txt_lines, con = lst_path)

# CSV of table body (displayed values as SAS shows)
csv_body <- data.frame(
  EmployeeID = report_df$EmployeeID,
  Salary = report_df$Salary_disp,
  `Salary Group` = report_df$SalGrp_disp,
  HireDate = report_df$HireDate_disp,
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(csv_body, file.path(out, "P093.csv"), row.names = FALSE, na = "")
```