Need to translate SAS program P093 to R.

We need to read inputs? The program creates data payroll via cards. So no external input. But we must produce outputs under OUT_DIR.

Expected output: P093.lst (table/listing text, 48 numbers) -> write text rendering and CSV of same table.

We need to understand SAS logic:

- payroll dataset with 45 observations? Let's count.

- proc format salgrp: low-<30000='Under $30,000', 30000-<35000='$30,000 to $35,000', 35000-<50000='$35,000 to $50,000', 50000-high='Over $50,000'

- data highlowsal: set payroll; by groupformat salary; format salary salgrp.; if first.salary then output; if last.salary then output;

Important: SAS BY GROUPFORMAT: groups by formatted value, not raw. So need to sort by salary? The payroll data is already sorted by salary ascending? Let's check list: salaries increasing: 25080,25390,...433800 yes sorted.

BY GROUPFORMAT salary means SAS will treat consecutive observations with same formatted value as a BY group. So groups are defined by salgrp format.

Then first.salary and last.salary refer to first/last within formatted group.

If first.salary then output; if last.salary then output;

So for each group, output first and last observation. If group has only one observation, then both conditions true? In SAS, if both true, it would output twice? Actually code does two separate if statements, not else. So if a group has single observation, it would output twice (duplicate). But does any group have single? Let's see groups:

Under $30k: salaries <30000. Count: let's list:

25080,25390,25680,25880,26105,26165,26165,26335,26550,26650,26800,26910,27215,27260,27425,27450,27645,27700,28395,28535,28845,29070,29545,29815 => that's 24 observations. So first is 25080 (EmployeeID 120185), last is 29815 (121061)

$30k to $35k: 30000 <= salary <35000: 30305,31320,31335,32235,32725,34000,34150,34760 => 8 obs. First 30305 (121062), last 34760 (120754)

$35k to $50k: 35000 <= salary <50000: 36315,38545,39330,45155,47030,47155,47785 => 7 obs? Wait also 50865? No 50865 is >=50000? Actually 50000-high is Over $50k, so 50865 belongs to Over. So $35k to $50k includes up to 49999. So list: 36315 (120683),38545(120757),39330(120789),45155(120774),47030(120796),47155(120793),47785(120668) => 7 obs. First 36315, last 47785.

Over $50k: >=50000: 50865,55400,63640,84260,207885,433800 => 6 obs. First 50865 (120813), last 433800 (120259)

So total highlowsal should have 8 rows (2 per group *4 groups). No single-observation groups, so no duplicates.

Then PROC REPORT data=highlowsal nowd; columns EmployeeID Salary Salary=SalGrp HireDate; define salary / display format=dollar12.; define hiredate / format=date9.; define salgrp / format=salgrp. 'Salary Group';

So report shows 4 columns: EmployeeID, Salary (formatted dollar12.), Salary Group (same salary variable but formatted with salgrp), HireDate (date9.)

Need to reproduce exactly.

We need to write R script that creates payroll data, applies grouping, creates highlowsal, then produces report.

Outputs: need to write CSV and TXT.

Expected outputs: P093.lst -> write text rendering and CSV of same table.

We must use OUT_DIR env var.

We need to produce:

- file.path(out, "P093.csv") and file.path(out, "P093.txt") ? Or maybe "P093.lst.csv" ? The spec says each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers... and as CSV of table body.

So for P093.lst, we should produce P093.csv and P093.txt (or P093.lst.csv/txt). The expected outputs list says P093.lst -> write text rendering and CSV. So name is P093. Let's produce both P093.csv and P093.txt, also maybe P093.lst.csv? Safer to produce P093.csv and P093.txt and also P093.lst.csv/txt? But spec says write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt"))... So for table P093, we should produce P093.csv and P093.txt.

We should also ensure numeric formatting matches SAS.

SAS formats:

- dollar12.: e.g., $25,080 ? Let's check SAS dollar format: dollar12. would produce with comma, dollar sign, width 12. Example: "     $25,080" maybe? Need to replicate.

- date9.: format like 01DEC2010 (DDMMMYYYY uppercase).

- salgrp format as defined.

We need to produce text rendering that contains same numbers SAS output shows (counts, percentages...). For this program, numbers are salaries, employee IDs, hire dates.

We need to create a plain-text table similar to PROC REPORT.

We should also produce CSV of table body with columns: EmployeeID, Salary, Salary_Group, HireDate maybe.

We need to ensure dates as YYYY-MM-DD in CSV? But spec says dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric values unrounded unless SAS rounds them. However for table CSV, we should probably include formatted values? The spec says each output dataset as CSV with SAS dataset's variable names, dates as YYYY-MM-DD. But for table, we need CSV of table body. Might need to provide both raw and formatted? We'll provide formatted as displayed.

Better to create two CSVs: one for dataset highlowsal (raw), and one for report table.

Simplify: produce P093.csv containing report data with columns EmployeeID, Salary, Salary_Group, HireDate (formatted as in report). And P093.txt containing text rendering.

Also produce highlowsal dataset CSV? But expected output is just P093.lst, so we can just produce P093.csv/txt.

We must also handle SAS rounding half away from zero helper, though not needed here.

We need to implement payroll data creation exactly as SAS cards.

Let's list all rows with dates:

We have 45 rows. Let's parse:

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

We need to create R data frame with these.

Then define salgrp function:

salgrp <- function(x) {
  case_when(
    x < 30000 ~ "Under $30,000",
    x >=30000 & x <35000 ~ "$30,000 to $35,000",
    x >=35000 & x <50000 ~ "$35,000 to $50,000",
    x >=50000 ~ "Over $50,000",
    TRUE ~ NA_character_
  )
}

Note SAS low-<30000 includes low to 29999.999..., 30000-<35000 includes 30000 inclusive to 34999..., etc.

Then BY GROUPFORMAT: need to sort by salary (already sorted). Then identify groups by formatted value. Then for each group, take first and last.

Implementation: payroll_sorted <- payroll %>% arrange(Salary) (though already sorted). Then mutate(grp = salgrp(Salary)). Then group_by(grp) with order preserved? Need to keep group order as appearance order, not alphabetical. Use factor with levels in order of appearance.

Then highlowsal <- payroll_sorted %>% group_by(grp) %>% filter(row_number()==1 | row_number()==n()) %>% ungroup() %>% arrange(Salary)  # to keep overall salary order

But need to ensure that if group has 1 row, it would duplicate? In SAS it would output twice. Our filter would output once. But no such case. To be faithful, we could implement logic that duplicates if n==1. But not needed. Could implement to duplicate: if n==1 then duplicate row. But we can just handle: for each group, if n==1, output twice. Let's implement that to be exact.

Simpler: do group_modify to duplicate.

Then PROC REPORT: columns EmployeeID Salary Salary=SalGrp HireDate

So report has 8 rows, 4 columns.

We need to format Salary as dollar12.: SAS dollar12. format: width 12, includes $ and commas, right-aligned. Example: dollar12. for 25080 would be "     $25,080" (5 spaces + $ + 6 chars =12?). Let's check: "$25,080" length 7, so pad to 12 with 5 leading spaces. For 433800 => "$433,800" length 8, pad 4 spaces. We'll replicate formatting for display.

HireDate format date9.: DDMMMYYYY with leading zero day, uppercase month abbrev, e.g., 01DEC2010.

Salary Group is same Salary variable formatted with salgrp.

We need to produce text rendering.

We should create a text file that mimics SAS PROC REPORT output with title.

Title: 'Lowest and Highest Salary by Salary Group'

We need to produce a plain-text table with headers: EmployeeID, Salary, Salary Group, HireDate

We should format Salary with dollar format.

We need to ensure 48 numbers in text: Let's count numbers: 8 rows * (EmployeeID + Salary + HireDate) = 8*3 =24 numbers, but Salary appears twice? Actually Salary column and Salary Group column: Salary Group is text, not number. So numbers per row: EmployeeID (1), Salary (1), HireDate (contains day/year numbers but maybe counted as numbers?). The expected 48 numbers maybe counts each numeric token? Not important.

We need to produce CSV of table body: columns EmployeeID, Salary, Salary_Group, HireDate

For CSV, should we use raw numeric salary or formatted? Spec says numeric values unrounded unless SAS rounds them. But for table CSV, we should provide displayed values. We'll provide both: Salary numeric and Salary formatted? Better to provide formatted as displayed plus raw.

Simplify: CSV will have EmployeeID, Salary (numeric), Salary_Formatted (dollar), Salary_Group, HireDate (YYYY-MM-DD and also formatted date9). But to match SAS, we can provide columns as in report: EmployeeID, Salary, Salary_Group, HireDate with Salary as numeric and HireDate as YYYY-MM-DD. However text rendering will show formatted.

We need to decide. We'll produce CSV with columns: EmployeeID, Salary, Salary_Group, HireDate (HireDate as YYYY-MM-DD). And also include Salary_Display for dollar format? Might be okay.

Better to produce CSV with exactly report columns: EmployeeID, Salary, Salary_Group, HireDate where Salary is numeric, HireDate is date string YYYY-MM-DD, Salary_Group is text.

And text file will show formatted dollar and date9.

We also need to ensure output directory exists.

We must write R script that does all.

We need to include helper for SAS date handling? Not needed.

We need to ensure script runs non-interactively.

We should include library calls.

We need to handle OUT_DIR env var.

We should produce both P093.csv and P093.txt, and also maybe P093.lst.csv/txt for safety.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "output" etc. Create dir.

- Create payroll data frame via tibble::tribble or data.frame.

- Parse dates: as.Date with format "%d%b%Y" but need uppercase month. Use lubridate::dmy or as.Date with %d%b%Y.

- Define salgrp function.

- Create highlowsal.

- Create report data.

- Format salary dollar: function format_dollar(x) { paste0("$", formatC(x, format="d", big.mark=",")) } but need width 12? For display we can pad.

- Format date9: format(date, "%d%b%Y") %>% toupper()

- Build report_df for display.

- Write CSV: write.csv(report_df_raw, file.path(out, "P093.csv"), row.names=FALSE, na="")

- Write TXT: capture table.

We need to create a nice text rendering.

We can use sprintf to create fixed-width table.

We should include title line.

We should also ensure we write highlowsal dataset CSV as well? The spec says each output dataset as CSV. highlowsal is an intermediate dataset, but we could write it as P093_highlowsal.csv.

But expected output is just P093.lst, so we need at least P093.csv/txt.

We'll produce:

- P093.csv (report body)
- P093.txt (text rendering)
- highlowsal.csv (dataset)

To be safe, also produce P093.lst.csv and P093.lst.txt as copies.

Let's implement.

We need to ensure dates in CSV are YYYY-MM-DD.

We need to ensure numeric values unrounded.

We should also implement round-half-up helper (not needed).

We should add progress messages via cat.

We must not call install.packages.

We should use only allowed packages.

We need to ensure script is standalone.

Let's craft final R code block.