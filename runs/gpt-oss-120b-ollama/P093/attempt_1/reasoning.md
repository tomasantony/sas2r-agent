We need to produce R script that replicates SAS program. Steps:

- Read data from inline cards. In SAS, data step reads from cards (inline). In R, we can embed the data as a text string and read via read.table(text=...). Use same variable names: EmployeeID, Salary, BirthDate, HireDate. Dates are date9. format (ddmmmyyyy). We'll parse with lubridate::dmy? Actually date9. is like 07APR1978. So format "%d%b%Y". Use as.Date with format "%d%b%Y". So we need to read as character then convert.

- Create format salgrp: mapping salary ranges to group labels. In SAS, groupformat on BY statement groups by the formatted value of salary using that format. The BY statement with groupformat salary means that BY groups are defined by the formatted value of salary (i.e., the group label). So data is sorted by salary (by default? The data step earlier didn't sort; but BY with groupformat requires data sorted by the grouping variable (salary) after applying format? Actually SAS requires data sorted by the BY variable (salary) before using BY. The data is not sorted; but they rely on BY with groupformat to group by the format value, and SAS will treat first.salary as first observation within each group (i.e., each salary group). However, they didn't sort; but the data is originally unsorted. In SAS, BY with groupformat still requires data sorted by the BY variable (salary) after applying format? I think SAS will still need sorted by the BY variable (salary) but they didn't sort; maybe they rely on the fact that BY with groupformat works even if not sorted? Not sure. For replication, we can mimic the logic: Determine groups based on salary ranges, then for each group, output the first and last observation (by original order? In SAS, first.salary is first observation in the BY group (i.e., after sorting by salary). Since they didn't sort, the first and last will be based on the order of data as read, but BY groups are defined by salary values (not sorted). Actually BY statement without prior sort will cause error in SAS unless data is sorted. But maybe they rely on BY with groupformat which automatically sorts? I think SAS will sort by the formatted value internally. Not 100% but for our replication we can just compute groups and then for each group, find the observation with minimum Salary (which is same as group) and maximum Salary (same). Since Salary is constant within group? Wait group is based on salary ranges, not exact salary. So within a group, there are multiple salary values. first.salary should be the observation with the lowest salary within that group (since data sorted by salary). last.salary is highest salary within group. So we need to sort by Salary ascending, then group by salgrp, then pick first and last rows per group.

- Then produce report: columns EmployeeID Salary Salary=SalGrp HireDate. Salary displayed with dollar12., hiredate date9., salgrp format salgrp. Title.

We need to output a table listing the rows (first and last per group) with columns: EmployeeID, Salary, SalGrp (the formatted group label), HireDate. The SAS PROC REPORT output likely includes a header row with column names: EmployeeID, Salary, Salary (maybe duplicate?), SalGrp, HireDate? Actually columns EmployeeID Salary Salary=SalGrp HireDate; define salary display format dollar12.; define hiredate format date9.; define salgrp format salgrp. 'Salary Group'; So column order: EmployeeID, Salary (displayed as dollar), SalGrp (label "Salary Group") derived from salary formatted, HireDate.

The output listing (P093.lst) contains 48 numbers (maybe 12 groups * 4 columns?). Let's compute groups: salary ranges: Under $30,000 (low-<30000), $30k-35k, $35k-50k, Over $50k. Data includes salaries from 207885 etc. So groups: Under 30k, 30-35k, 35-50k, Over 50k. For each group, we output first and last salary rows, so 2 rows per group => 8 rows total. Each row has 4 columns (EmployeeID, Salary, Salary Group, HireDate). That's 8*4=32 numbers plus maybe counts? Not sure. But we just need to produce same table.

Implementation steps:

1. Create data frame payroll with columns EmployeeID (numeric), Salary (numeric), BirthDate (Date), HireDate (Date). Use read.table from textConnection.

2. Define function salgrp_format(salary) that returns group label per ranges.

3. Add column SalGrp = salgrp_format(Salary).

4. Sort payroll by Salary ascending.

5. Group by SalGrp, then for each group, get first row and last row (by Salary). Use dplyr::group_by and slice.

6. Combine rows (first then last) preserving order of groups as they appear in sorted data (i.e., Under, 30-35, 35-50, Over). Use arrange.

7. For output dataset highlowsal, keep columns EmployeeID, Salary, SalGrp, HireDate (maybe also BirthDate but not needed). Ensure SalGrp is character.

8. Write CSV: write.csv(highlowsal, file.path(out, "highlowsal.csv"), row.names=FALSE, na="").

9. Produce listing text: need to render table similar to PROC REPORT. We'll create a formatted character matrix with column headers and formatted values: Salary as dollar with commas and 2 decimals? dollar12. format includes $ and commas, width 12. Use formatC with format = "f", big.mark=",", digits=2, prefix="$". HireDate as date9. format "ddmmmyyyy". Use format(date, "%d%b%Y") uppercase month abbreviation.

SalGrp as label.

We'll create a data frame display_df with formatted strings.

Then write to txt: writeLines of header and rows.

Also write CSV of table body (maybe same as highlowsal.csv). Already done.

But requirement: each table/listing also as plain-text rendering and CSV of same table. So we need to write both .txt and .csv for the listing. The .csv already done. The .txt should contain the formatted table.

We'll name the listing "P093.lst" as per expected output. So write file.path(out, "P093.txt")? Actually they expect P093.lst (listing). We'll output "P093.txt" maybe but spec says write each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")). So name should be "P093.txt". But they said expected output P093.lst. We'll produce both .txt and .csv with base name "P093". Let's produce "P093.txt" and "P093.csv". Also maybe produce "P093.lst"? Safer to produce "P093.txt". But spec: write each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")). So name is same as dataset? We'll use "P093.txt". Also produce CSV "P093.csv". The dataset highlowsal maybe not needed as separate dataset output; but we can also output "highlowsal.csv". The listing is the report.

Thus script steps.

Edge: rounding half away from zero for dollar formatting. R's round uses round to even. For formatting dollars, we can use sprintf with format "%.2f" which uses round half away? Actually sprintf uses round half away? It uses round to nearest, ties to even? Not sure. We'll implement custom round_half_up function: function(x) floor(x + 0.5). For positive numbers, that's fine. Salary are positive. So we can compute rounded = floor(salary*100 + 0.5)/100.

Then format with commas: use formatC(rounded, format="f", big.mark=",", digits=2). Then prepend "$". Ensure width 12: pad left with spaces to width 12.

Implement.

Now produce script.

Need to get OUT_DIR env var.

Let's craft final R script.

