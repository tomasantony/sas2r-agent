out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# SAS round half away from zero (helper, not needed here but for fidelity)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# --- 1. Create payroll dataset (infile cards) ---
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
BirthDate_raw <- c("07APR1978","28AUG1990","08MAR1978","02FEB1973","10JAN1983",
  "23NOV1958","05MAY1968","18MAY1973","17FEB1973","18SEP1958","25MAY1983",
  "10SEP1973","28DEC1963","07MAY1983","25OCT1963","26NOV1963","05MAY1948",
  "19AUG1963","23DEC1948","12JUN1983","04APR1983","23MAR1983","09MAY1983",
  "16JUL1958","28OCT1988","25JUN1990","24FEB1992","24JAN1963","19MAR1983",
  "22AUG1973","15MAR1963","02JUN1992","12NOV1958","18MAR1948","14JUL1968",
  "17SEP1982","12MAY1958","08AUG1973","23OCT1953","14SEP1973","11FEB1948",
  "12JUN1953","22NOV1953","02DEC1968","25JAN1968")
HireDate_raw <- c("01DEC2010","01NOV2010","01AUG2006","01FEB2000","01SEP2008",
  "01APR1978","01DEC1993","01MAY1999","01JUL1994","01DEC1983","01AUG2008",
  "01DEC2010","01APR1988","01JAN2002","01APR1985","01FEB1986","01JAN1978",
  "01JUL1991","01JAN1978","01FEB2009","01NOV2003","01NOV2002","01JAN2007",
  "01JUL1988","01AUG2010","01JUN2008","01MAR2010","01SEP1991","01MAY2004",
  "01JAN2000","01SEP1987","01MAY2010","01JAN1978","01JAN1978","01DEC1986",
  "01MAR2006","01MAR1987","01JUN2000","01DEC1982","01JAN1997","01JAN1978",
  "01JAN1978","01APR1980","01NOV1988","01SEP1993")

parse_date9 <- function(x) {
  # SAS DATE9.: DDMMMYYYY, case-insensitive English month
  x <- trimws(toupper(x))
  day <- suppressWarnings(as.integer(substr(x, 1, 2)))
  mon <- substr(x, 3, 5)
  year <- suppressWarnings(as.integer(substr(x, 6, 9)))
  months <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
  mnum <- unname(months[mon])
  as.Date(sprintf("%04d-%02d-%02d", year, mnum, day))
}
format_date9 <- function(x) {
  # SAS DATE9.: DDMMMYYYY uppercase, zero-padded day
  mon_abbr <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  d <- as.integer(format(x, "%d"))
  m <- as.integer(format(x, "%m"))
  y <- format(x, "%Y")
  sprintf("%02d%s%s", d, mon_abbr[m], y)
}
format_dollar_trim <- function(x) {
  # SAS DOLLAR12. : $ with commas, no decimals, trimmed
  paste0("$", formatC(x, format = "d", big.mark = ",", digits = 0))
}
format_dollar12 <- function(x, width = 12) {
  s <- format_dollar_trim(x)
  # right-align to width 12 like SAS (pad left with blanks)
  sprintf(paste0("%", width, "s"), s)
}
format_salgrp <- function(x) {
  # PROC FORMAT value salgrp low-<30000, 30000-<35000, 35000-<50000, 50000-high
  ifelse(is.na(x), NA_character_,
    ifelse(x < 30000, "Under $30,000",
      ifelse(x < 35000, "$30,000 to $35,000",
        ifelse(x < 50000, "$35,000 to $50,000", "Over $50,000"))))
}

payroll <- data.frame(
  EmployeeID = EmployeeID,
  Salary = Salary,
  BirthDate = parse_date9(BirthDate_raw),
  HireDate = parse_date9(HireDate_raw),
  stringsAsFactors = FALSE
)

# Data is already sorted by Salary; enforce SAS sort order (missing first, ascending)
# Preserve input order for ties (SAS stable for duplicates as entered)
payroll$seq <- seq_len(nrow(payroll))
payroll <- payroll[order(payroll$Salary, payroll$seq), ]
payroll$seq <- NULL
rownames(payroll) <- NULL

# --- 2. BY GROUPFORMAT processing: first. and last. by formatted Salary ---
payroll$SalGrp_fmt <- format_salgrp(payroll$Salary)

# Determine group order by Salary (formatted groups in salary order)
grp_order <- unique(payroll$SalGrp_fmt)  # already in salary order because payroll sorted
# Build highlowsal: for each group output first and last observation
highlowsal_list <- lapply(grp_order, function(g) {
  sub <- payroll[payroll$SalGrp_fmt == g, , drop = FALSE]
  # sub is already sorted by Salary
  first_row <- sub[1, , drop = FALSE]
  last_row <- sub[nrow(sub), , drop = FALSE]
  # SAS: if first.salary then output; if last.salary then output;
  # if n==1 this outputs the same row twice
  rbind(first_row, last_row)
})
highlowsal <- do.call(rbind, highlowsal_list)
rownames(highlowsal) <- NULL

# highlowsal retains Salary order (low1, high1, low2, high2, ...)
# Verify expected 8 rows
# --- 3. PROC REPORT data=highlowsal ---
# columns EmployeeID Salary Salary=SalGrp HireDate
# define salary / display format=dollar12.
# define hiredate / format=date9.
# define salgrp / format=salgrp. 'Salary Group'

report_df <- data.frame(
  EmployeeID = highlowsal$EmployeeID,
  Salary = highlowsal$Salary,
  SalGrp = highlowsal$SalGrp_fmt,
  HireDate = highlowsal$HireDate,
  stringsAsFactors = FALSE
)

# Display-formatted values (as SAS shows)
report_disp <- data.frame(
  EmployeeID = report_df$EmployeeID,
  Salary = format_dollar_trim(report_df$Salary),
  Salary_Group = report_df$SalGrp,
  HireDate = format_date9(report_df$HireDate),
  stringsAsFactors = FALSE
)
# For fixed-width txt, use dollar12 padded version
salary_padded <- format_dollar12(report_df$Salary, width = 12)

# --- Write CSV of table body (displayed values) ---
write.csv(report_disp, file.path(out, "P093.csv"), row.names = FALSE, na = "")

# --- Write plain-text rendering (PROC REPORT listing) ---
txt_path <- file.path(out, "P093.txt")
lst_path <- file.path(out, "P093.lst")
con <- file(txt_path, open = "wt")
writeLines("Lowest and Highest Salary by Salary Group", con)
writeLines("", con)
# Header mimicking PROC REPORT
hdr <- sprintf("%-10s %12s %-20s %-9s", "EmployeeID", "Salary", "Salary Group", "HireDate")
writeLines(hdr, con)
sep <- sprintf("%-10s %12s %-20s %-9s", "----------", "------------", "--------------------", "---------")
writeLines(sep, con)
for (i in seq_len(nrow(report_df))) {
  line <- sprintf("%-10d %12s %-20s %-9s",
                  report_df$EmployeeID[i],
                  salary_padded[i],
                  report_df$SalGrp[i],
                  format_date9(report_df$HireDate[i]))
  writeLines(line, con)
}
writeLines("", con)
writeLines("Title: Lowest and Highest Salary by Salary Group", con)
close(con)
# Also write .lst copy (expected SAS listing name)
file.copy(txt_path, lst_path, overwrite = TRUE)

# --- CSV of plotted data not needed (no figure); console progress only ---
cat(sprintf("P093: payroll n=%d, highlowsal n=%d written to %s\n", nrow(payroll), nrow(highlowsal), out))
