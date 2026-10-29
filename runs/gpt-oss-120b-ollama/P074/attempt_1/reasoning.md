We need to translate the SAS program to R script that reproduces outputs: a table t_lb_shift.rtf (but we output as txt and csv). Must follow rules: read datasets from adam library path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . The datasets needed: adlb, adsl. They are SAS datasets (.sas7bdat). Use haven::read_sas.

We need to compute big N counts per treatment group (TRTA) for safety set (SAFFL='Y'). Macro m_counts creates macro variables and header strings, but for output we need to produce table with shift counts per parameter per treatment group. The final report is a PROC REPORT with columns: PARAMCD, PARAM, bl_label, wg0-wg4, total. It loops over treatment groups (TRTAN values 1..ntrt). ntrt macro variable set earlier to 3 (from macro variable). So we need to produce separate pages per treatment group, but for output we can combine all rows and include TRTAN column maybe not needed. The SAS output likely includes separate tables per treatment group, each with rows for each parameter (PARAMCD) and baseline grade rows.

We need to produce a single table listing all rows, maybe with grouping variable TRTAN. The requirement: produce t_lb_shift.rtf as text rendering and CSV. We'll output a CSV with columns: TRTAN, TRTA, PARAMCD, PARAM, bl_label, wg0, wg1, wg2, wg3, wg4, total. Also produce a txt rendering similar to SAS PROC REPORT output. We need to format numbers as integers (no decimals). The header strings include column titles.

Implement steps:

1. Set paths: out <- Sys.getenv("OUT_DIR") . Ensure directory exists? assume exists.

2. Read adsl and adlb using haven::read_sas.

3. Compute big N: for each TRTA where SAFFL='Y', count distinct USUBJID. Also total across all.

4. Filter adlb to safety set: join with adsl on USUBJID, keep SAFFL='Y', and PARAMCD in list.

5. Baseline grade: rows where ABLFL='Y' and BTOXGR not missing/empty. BTOXGR is character? In SAS they treat '' as missing. In R, treat as NA or empty string. We'll filter BTOXGR != "" and !is.na.

Select distinct USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR as bl_grade.

6. Worst grade: rows where ABLFL != 'Y' (i.e., not baseline) and ATOXGR not missing/empty and AVISITN > 0. Compute numeric grade: as.numeric(ATOXGR) after converting to numeric (if non-numeric maybe). Use as.numeric, treat NA. Then max per subject per param.

Create worst_grade_n numeric, then convert to character string: if grade_n == 0 then "0" else as.character(grade_n). Also ensure if grade_n is NA? but filtered.

7. Merge baseline and worst by USUBJID, PARAMCD, TRTA (and TRTAN). Use inner join.

8. Count distinct USUBJID per combination of TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade.

9. Create grades data frame with grade 0-4 as character.

10. Cross join to get all cells: for each combination of treatment group (TRTA, TRTAN, PARAMCD, PARAM) present in shift_cnt, cross join with grades for bl and worst. Actually SAS cross join uses shift_cnt a (distinct combos) cross join grades b and c. So we need distinct combos of TRTA, TRTAN, PARAMCD, PARAM from shift_cnt (or from baseline/worst). Then expand.

Implementation: distinct_params <- shift_cnt %>% distinct(TRTA, TRTAN, PARAMCD, PARAM)

Then expand_grid with grades for bl and worst.

Then left join shift_cnt to get cnt, replace NA with 0.

10. Transpose worst_grade into columns: pivot_wider with names_prefix "wg", names_from worst_grade, values_from cnt, values_fill = 0. Ensure columns wg0..wg4 exist; if missing, add with 0.

11. Compute bl_label mapping: 0->Normal, 1->Grade 1, etc.

12. Compute total = rowSums of wg0..wg4.

13. Output dataset report.

14. Write CSV: write.csv(report, file.path(out, "t_lb_shift.csv"), row.names=FALSE, na="").

15. Write txt rendering: we need to mimic PROC REPORT output. We'll create a formatted table as character strings. Use format similar to SAS: column headers: Parameter, Baseline Grade, Normal, Grade 1, Grade 2, Grade 3, Grade 4, Total. For each treatment group, we may separate with a header line: "Treatment: <TRTA>" maybe. In SAS they have title lines but not needed. We'll produce simple text: for each TRTAN group, print header then rows.

Implement using sprintf with appropriate widths. Determine column widths: PARAM (maybe 2in ~ 20 chars). We'll set width 20 for PARAM, 12 for bl_label, each wg column width 8, total width 8. Use format: left align for PARAM, bl_label, right align for numbers.

Construct lines and write to txt.

Also need to output CSV of the table body (already done). The txt file should be named "t_lb_shift.txt".

No figures.

Implement round-half-up? Not needed for counts.

Edge Cases: ensure character ordering: SAS sorts missing first, then uppercase before lowercase. Our sorting: use arrange with factor levels? For treatment groups, order by TRTAN numeric ascending. For PARAMCD, alphabetical default case-sensitive; R's default order is lexical with case-sensitive (uppercase before lowercase). Should be fine.

Implement using dplyr.

Now produce final R script inside ```r``` block.