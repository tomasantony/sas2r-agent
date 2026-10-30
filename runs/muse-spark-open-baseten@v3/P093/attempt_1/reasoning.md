Need to translate SAS program to R.

We need to understand expected outputs: P093.lst (table/listing text, 48 numbers) -> write text rendering and CSV.

We need to reproduce SAS behavior.

Steps:

- Create payroll dataset from cards.

- proc format value salgrp low-<30000='Under $30,000' etc. Note ranges: low-<30000, 30000-<35000, 35000-<50000, 50000-high. So inclusive lower bound, exclusive upper. So 30000 exactly goes to second group, 35000 to third, 50000 to fourth.

- data highlowsal; set payroll; by groupformat salary; format salary salgrp.; if first.salary then output; if last.salary then output;

This is tricky: BY GROUPFORMAT salary means grouping by formatted value, not raw. So need to sort by salary? The comment says data is sorted by value to be formatted. The payroll data appears sorted ascending by salary? Let's check: 25080,25390,25680,... yes ascending.

Then BY GROUPFORMAT will treat consecutive observations with same formatted value as a BY group. So first.salary and last.salary refer to first/last within formatted group.

We need to output both first and last per group. If group has only one observation, then first and last are same, so it would output twice? In SAS, if first.salary and last.salary both true, then two outputs? The code does if first.salary then output; if last.salary then output; So if single observation group, it would output twice (duplicate). But in this data, groups sizes: Let's compute.

Groups:
Under $30k: <30000: salaries up to 29815. Let's count: from 25080 to 29815 inclusive. That's 24 observations? Let's count list: 120185(25080),121101(25390),121092(25680),121106(25880),121072(26105),120178(26165),121027(26165),121096(26335),120112(26550),121002(26650),120687(26800),120183(26910),120999(27215),120153(27260),121059(27425),120164(27450),120120(27645),121139(27700),121075(28395),120715(28535),121042(28845),120799(29070),121105(29545),121061(29815) => 24.

$30k to $35k: 30000-<35000: 121062(30305),121019(31320),121032(31335),121080(32235),121099(32725),120787(34000),120731(34150),120754(34760) => 8

$35k to $50k: 35000-<50000: 120683(36315),120757(38545),120789(39330),120774(45155),120796(47030),120793(47155),120668(47785) => 7

Over $50k: >=50000: 120813(50865),120804(55400),120712(63640),121145(84260),120260(207885),120259(433800) => 6

Total 45.

For each group, we output first and last. So highlowsal will have 8 rows (2 per group *4 groups). Let's see: first and last per group.

Under $30k: first = 120185 25080, last = 121061 29815
$30k-35k: first 121062 30305, last 120754 34760
$35k-50k: first 120683 36315, last 120668 47785
Over $50k: first 120813 50865, last 120259 433800

So 8 rows.

Then PROC REPORT data=highlowsal nowd; columns EmployeeID Salary Salary=SalGrp HireDate; define salary / display format=dollar12.; define hiredate / format=date9.; define salgrp / format=salgrp. 'Salary Group'; title ...

So report shows 8 rows with columns: EmployeeID, Salary (formatted dollar12.), Salary Group (formatted salgrp.), HireDate (date9.)

We need to reproduce table.

We need to write outputs into OUT_DIR. Expected outputs: P093.lst -> write text rendering and CSV of same table.

We need to create R script that does this.

We must follow rules: write each output dataset as CSV, each table/listing additionally as plain-text rendering and CSV of table body. Reproduce SAS formatting.

We need to handle dates: BirthDate and HireDate are date9. format like 07APR1978. Need to parse with as.Date(x, format="%d%b%Y")? Actually date9. is DDMMMYYYY with uppercase month. Use as.Date with format "%d%b%Y" but need to handle case. Use lubridate? But rule says use ONE explicit strict format. We'll parse with as.Date(toupper? Actually %b is locale dependent. Better to parse manually. Use as.Date(x, format="%d%b%Y") after converting to proper case? In R, %b expects abbreviated month name in current locale (English). "APR" uppercase may not match "Apr". So we can use tolower? Let's test mental: as.Date("07APR1978", format="%d%b%Y") may fail if locale expects "Apr". Safer to use lubridate::dmy? But rule says never use tryFormats or multi-order parsers. However we can use as.Date with format "%d%b%Y" after converting to title case? Let's do: stringr::str_to_title? "07Apr1978" would match "%d%b%Y". So we can do: BirthDate_str = "07APR1978", convert to "07Apr1978" via paste0(substr..., etc). Simpler: use lubridate::dmy with one format? But rule says dates: look at raw values and parse each source column with ONE explicit strict format (as.Date(x, format="%d/%m/%Y"), "%d-%b-%Y", "%Y-%m-%d" ...). Never use tryFormats or multi-order lubridate parsers: "%Y/%m/%d" accepts 1-4 digit years and silently swallows dd/mm/yyyy. SAS ANYDTDTE with DATESTYLE=MDY or DMY is one fixed order. Always write ISO dates.

So we should parse with as.Date(x, format="%d%b%Y") but need to handle case. We can transform string to have proper case: e.g., tools: substr(x,1,2) + str_to_title(substr(x,3,5)) + substr(x,6,9). Let's implement helper.

Also need to handle dollar12. format: SAS dollar12. formats numbers with $ and commas, width 12. Example: 25080 -> "     $25,080" ? Need to check SAS dollar format: dollarw.d - writes numeric values with leading dollar sign, commas, period. Width includes $ and commas. For dollar12., 25080 would be formatted as "     $25,080" (5 leading spaces + $ + 6 chars =12). But in PROC REPORT display, it may show without leading spaces? In text rendering, we should show formatted values similar to SAS. Need to reproduce.

Similarly date9. format: DDMMMYYYY with leading zero day, uppercase month, e.g., 01DEC2010 -> "01DEC2010". In R we can format as format(date, "%d%b%Y") and toupper.

Salary Group format: salgrp.

We need to produce P093.lst text rendering with 48 numbers. Let's count numbers: 8 rows * (EmployeeID, Salary, HireDate? Actually HireDate is date, not number, but maybe counts as numbers? The spec says 48 numbers. Let's see: 8 rows * 2 numeric columns (EmployeeID, Salary) =16, plus maybe HireDate contains numbers? But spec says 48 numbers - maybe they count all numeric characters? Not important.

We need to create output files: out <- Sys.getenv("OUT_DIR"); write CSV and TXT.

We need to produce:

- file.path(out, "P093.csv") maybe? And P093.txt and P093.lst? The expected output is P093.lst -> write text rendering and CSV of same table. So we should create P093.lst.txt? But rule says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... and as CSV of table body. So for P093.lst, we should create P093.lst.txt and P093.lst.csv? Or P093.txt and P093.csv? The spec says Expected outputs: P093.lst -> write text rendering and CSV. So we should create files named P093.lst.csv and P093.lst.txt? Or P093.csv/txt? Safer to create both variants to ensure matching.

The instruction: Write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv"), ...), each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) ... So <name> is the output name without extension? For P093.lst, <name> is P093. So we should write P093.csv and P093.txt. But also to be safe, create P093.lst.csv etc.

We should also write highlowsal dataset? The SAS program creates payroll and highlowsal datasets, but expected outputs only list P093.lst. However rule says write every output dataset as CSV. So we should write payroll.csv and highlowsal.csv as well? But expected outputs only mention P093.lst. We can write those too; not harmful.

We need to ensure script runs non-interactively.

We need to implement round-half-up helper (not needed here).

We need to reproduce titles and footnotes verbatim.

We need to create text rendering that looks like SAS PROC REPORT output.

We should create a simple text file with title, column headers, rows.

We need to format Salary with dollar12.: use scales::dollar? But we can implement: formatC with format="f", big.mark=",", etc. SAS dollar12. with 0 decimals: "$" + format with commas. Width 12 right-aligned. So we can do sprintf("%12s", paste0("$", formatC(salary, format="f", digits=0, big.mark=",")))

Similarly HireDate date9.: format(date, "%d%b%Y") toupper.

Salary Group: formatted via salgrp.

EmployeeID as integer.

We need to ensure highlowsal is sorted by salary group order? The BY GROUPFORMAT processes in order of formatted values? Actually BY GROUPFORMAT groups by formatted value, but order is by underlying value? The data is sorted by salary, so groups appear in order of salary ascending, which corresponds to format order: Under $30k, $30k-35k, $35k-50k, Over $50k. So report will show in that order, with two rows per group (low then high). So order is as we listed.

We need to produce CSV of table body: columns EmployeeID, Salary, SalaryGroup, HireDate. Salary numeric? Or formatted? For CSV, should contain raw values? But spec says CSV of table body contains same numbers SAS shows. For table, we should provide CSV with displayed values? The rule: each table or listing additionally as plain-text rendering and as CSV of the table body. Reproduce SAS formatting of displayed numbers. So CSV should contain formatted values? Or raw? Safer to provide formatted values as displayed.

We should also write ISO dates in dataset CSVs: dates as YYYY-MM-DD.

We need to handle OUT_DIR creation.

We need to include library calls.

We need to ensure script is standalone.

We should implement helper for date parsing.

We should also handle writing files.

Let's craft R script.

We need to include out <- Sys.getenv("OUT_DIR"); if empty, use temp.

We need to create payroll data frame.

We need to define salgrp function.

We need to create highlowsal via group_by formatted salary.

We need to create report data frame.

We need to write text file.

We need to ensure 48 numbers: maybe they count each numeric value in text file: 8 rows * 6 numbers? Let's not worry.

We should produce text file with title, header, rows, maybe footer.

We should also produce CSV.

We should ensure we use haven etc not needed.

We should follow SAS semantics: missing values sort first, etc not needed.

We should ensure we don't call install.packages.

We should use ggsave not needed.

We should produce final script inside ```r block.

Let's craft script.

Potential pitfalls: date parsing locale. Use lubridate? But we can implement manual month mapping.

Simpler: create vector of month abbreviations: c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") and parse.

We can write function parse_date9 <- function(x) { day <- as.integer(substr(x,1,2)); mon_str <- substr(x,3,5); year <- as.integer(substr(x,6,9)); mon <- match(toupper(mon_str), c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")); as.Date(paste(year, mon, day, sep="-")) } But as.Date(paste(...)) with format "%Y-%m-%d". That will work.

We need to handle both BirthDate and HireDate.

We need to create payroll data frame with EmployeeID, Salary, BirthDate, HireDate.

We need to write payroll.csv with ISO dates.

We need to create highlowsal: group by salgrp.

Implementation of BY GROUPFORMAT: we need to compute formatted group, then for each group, take first and last row (by salary order). Since data already sorted by salary, we can group_by(salgrp_fmt) and filter row_number()==1 | row_number()==n().

Note: need to preserve order of groups as they appear in data, not alphabetical. So use factor with levels in order of appearance.

We should define salgrp_fmt function:

salgrp_fmt <- function(x) {
  ifelse(x < 30000, "Under $30,000",
  ifelse(x < 35000, "$30,000 to $35,000",
  ifelse(x < 50000, "$35,000 to $50,000",
  "Over $50,000")))
}

Note boundaries: low-<30000, 30000-<35000, etc. So x <30000, x>=30000 & x<35000, etc.

We need to ensure 30000 exactly goes to second group, etc.

Then highlowsal = payroll %>% mutate(SalGrp = salgrp_fmt(Salary)) %>% group_by(SalGrp) ... But need to keep order.

We should create factor with levels = c("Under $30,000","$30,000 to $35,000","$35,000 to $50,000","Over $50,000") to ensure order.

Then for each group, take first and last.

We need to output EmployeeID, Salary, SalGrp, HireDate.

We need to format Salary dollar12., HireDate date9., SalGrp salgrp.

We need to create report data frame for display.

We need to write text rendering.

We should create text file content:

Title: "Lowest and Highest Salary by Salary Group"

Header: EmployeeID Salary Salary Group HireDate

Rows: each row with formatted values.

We should align columns similar to SAS.

We should also include counts.

We need to ensure we write 48 numbers: maybe they count each digit? Not needed.

We should write CSV with columns: EmployeeID, Salary, SalaryGroup, HireDate (ISO or formatted?). For table CSV, use displayed values: EmployeeID, Salary (numeric or formatted?), SalaryGroup, HireDate (date9). Let's provide both: Salary numeric and formatted? But spec says CSV of table body contains same numbers SAS shows. So we should provide formatted Salary as "$25,080" etc.

We should create two CSVs: P093.csv with formatted table, and highlowsal.csv with raw dataset.

We should also create P093.lst.csv and P093.lst.txt to match expected name.

We should ensure we create OUT_DIR if not exists.

We should add stopifnot checks.

We should include helper round_half_up (not needed but include).

We should ensure script runs with Rscript.

Let's craft final R script.

We need to ensure we use only allowed packages. We can use dplyr, stringr, etc.

We should not use arrow etc.

We should write files with write.csv(..., row.names=FALSE, na="").

We should write text file with writeLines.

We should include progress messages via cat.

We should ensure we handle OUT_DIR env var.

We should produce final output.

Let's produce script.