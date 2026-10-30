Need to translate SAS program to R. Must read adam.adsl and adam.adlb from D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Check what files exist. Likely .sas7bdat or .xpt. Use haven.

Steps:

1. Get Big N - not needed for output but maybe for titles.

2. Derive lb: join adlb with adsl where SAFFL='Y' and PARAMCD in list, select fields.

3. bl_grade: distinct where ABLFL='Y' and BTOXGR != '' (and not missing). Note SAS: BTOXGR ne '' . In R, handle NA and empty string.

4. worst_grade: where ABLFL != 'Y' and ATOXGR != '' and AVISITN >0, group by USUBJID, TRTA, TRTAN, PARAMCD, max(input(ATOXGR, best.)) . So convert ATOXGR to numeric.

5. worst_grade2: convert numeric to character strip. If 0 then '0'.

6. shift: inner join bl_grade and worst_grade2 on USUBJID, PARAMCD, TRTA.

7. shift_cnt: count distinct USUBJID per TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade.

8. grades: 0-4 as character.

9. all_cells: cross join distinct TRTA,TRTAN,PARAMCD,PARAM from shift_cnt with grades b and c.

10. shift_full: left join counts, coalesce 0.

11. transpose: by TRTAN TRTA PARAMCD PARAM bl_grade, id worst_grade, var cnt => columns wg0..wg4.

12. report: label baseline, replace missing 0, total = sum wg.

13. Generate output: need to produce t_lb_shift.rtf text rendering and CSV. Expected outputs: t_lb_shift.rtf (table/listing text, 969 numbers) -> write text rendering and CSV of same table.

We need to understand expected output structure. SAS proc report with columns PARAMCD PARAM bl_label wg0 wg1 wg2 wg3 wg4 total, break after PARAMCD / page, and loop over t=1..ntrt (3 treatment groups). So output is shift table per treatment group per parameter.

We need to produce CSV and TXT that contain same numbers. The spec says: each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers SAS output shows (counts, percentages...), and as CSV of table body.

We need to create out directory from Sys.getenv("OUT_DIR").

We need to produce t_lb_shift.csv and t_lb_shift.txt (and maybe .rtf? But spec says write CSV and TXT). The expected output is t_lb_shift.rtf -> we should produce t_lb_shift.csv and t_lb_shift.txt (and maybe also .rtf placeholder). The instruction says write every output into OUT_DIR: each output dataset as CSV, each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers, and as CSV of table body. So for t_lb_shift, we need to produce t_lb_shift.csv and t_lb_shift.txt.

We need to replicate SAS formatting: counts as integers.

We need to handle sorting: SAS sorts character ordering uppercase before lowercase. But here TRTA values.

We need to implement round-half-up helper (not needed for counts).

We need to read data. Let's explore file system via R? We can't run here but we can guess. Use haven::read_sas or read_xpt. Try both.

We need to handle missing values: BTOXGR ne '' means not empty string and not missing. In R, filter !is.na(BTOXGR) & BTOXGR != "" & trimws(BTOXGR) != "".

Similarly ATOXGR.

Also ABLFL handling: ABLFL = 'Y' vs ne 'Y'. In SAS, missing ABLFL would be considered ne 'Y'. So in R, filter ABLFL != 'Y' or is.na.

But spec: where ABLFL ne 'Y' and ATOXGR ne '' and AVISITN >0 . So include missing ABLFL? Probably.

We need to ensure TRTAN ordering.

We need to produce final report dataset: columns: TRTAN, TRTA, PARAMCD, PARAM, bl_grade, bl_label, wg0..wg4, total. Sorted by TRTAN, PARAMCD, bl_grade.

We need to write CSV: write.csv(report, file.path(out, "t_lb_shift.csv"), row.names=FALSE, na="")

And TXT: need to render table similar to SAS. Should include titles, footnotes, and table per treatment group.

We should create a text file that shows shift tables.

We need to ensure 969 numbers: Let's calculate: 3 treatment groups * 8 parameters * 5 baseline grades * 6 columns (wg0-4 + total) = 3*8*5*6 = 720 numbers. Plus maybe totals? Actually 720. But spec says 969 numbers - maybe includes Big N? Or maybe some parameters missing? Let's not worry.

We need to produce comprehensive TXT.

We should also handle that some parameters may have no data for some treatment groups? But all_cells derived from shift_cnt distinct TRTA/PARAMCD, so if a parameter has no shift data for a treatment group, it won't appear. That's SAS behavior.

We need to implement exactly.

We need to handle input paths: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . List files.

We should write R script that checks for .sas7bdat, .xpt, .csv.

We need to ensure OUT_DIR exists.

We need to produce final script inside one ```r code block.

We must not call install.packages.

We need to include helper functions: round_half_up, etc.

We need to ensure dates formatting not needed.

We need to produce PNG? No figure.

We need to ensure script runs non-interactively.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if out=="" then out <- getwd(); dir.create(out, recursive=TRUE, showWarnings=FALSE)

- root <- "D:/meta_hackathon/sas2r-agent" etc.

- adam_path <- file.path(root, "data/raw/own-synthetic/adam")

- sdtm_path similarly.

- Function to read adam dataset: try read_sas, read_xpt, csv.

We need to list files: list.files(adam_path)

We need to handle haven reading: haven::read_sas, haven::read_xpt.

We need to convert haven labelled to character/numeric: use haven::as_factor? Better to use zap_labels? But we need TRTA values as character.

We should use haven::read_sas(..., col_select=NULL) and then convert labelled to character via haven::as_factor or zap.

Simpler: after reading, for each column if inherits "haven_labelled", convert to as.character(haven::as_factor(.)) or as.numeric if needed. But TRTAN is numeric, TRTA is character.

We need to handle.

We need to implement SAS logic faithfully.

Let's draft script.

Potential pitfalls:

- ATOXGR may be character "0","1", etc. Input conversion: max(input(ATOXGR, best.)) . So numeric conversion, ignoring non-numeric? Use suppressWarnings(as.numeric(ATOXGR)).

- worst_grade_n is max numeric per subject/param.

- Then worst_grade = strip(put(worst_grade_n, best.)) ; if 0 then '0'. In SAS, put with best. for 0 gives "0". So same.

- bl_grade is BTOXGR as is (character). Need to ensure it's trimmed.

- Grades: 0-4 as character via strip(put(i, best.)) => "0","1","2","3","4"

- Cross join: distinct TRTA,TRTAN,PARAMCD,PARAM from shift_cnt.

- shift_full: left join.

- Transpose: need to ensure all wg0..wg4 columns exist even if no data for that worst_grade. In SAS proc transpose, if a worst_grade value never appears, column won't be created. But since we cross-joined all grades, all worst_grade values 0-4 will appear, so all columns wg0-wg4 will exist. However if some treatment/param has no counts for some worst_grade, still column exists because all_cells includes all combos, but transpose will create columns for each distinct worst_grade present in shift_full (which includes all 0-4). So all 5 columns will exist.

- In R, we can pivot_wider.

- Report: bl_label mapping.

- Replace missing with 0: after pivot, NAs to 0.

- total = sum wg0..wg4

- Sorting: order by TRTAN, PARAMCD, bl_grade, worst_grade earlier, but final report order by TRTAN, PARAMCD, bl_grade.

We need to ensure bl_grade ordering: SAS orders by bl_grade character? In shift_full order by TRTAN, PARAMCD, bl_grade, worst_grade. bl_grade is character "0".."4", so lexical order matches numeric.

We need to produce final TXT: For each TRTAN (1..ntrt), print header with treatment label and Big N, then for each PARAMCD, print table with rows bl_label and columns wg0..wg4, total.

We should include titles from u_titles.sas: Table 14.3.4 Shift Table for Laboratory Parameters, Safety Analysis Set.

Footnotes: Data cutoff, Source, Program, plus footnote4,5.

We need to write CSV of table body: likely the report dataset.

We should write both t_lb_shift.csv (report) and t_lb_shift.txt.

We should also ensure we handle ntrt =3.

We need to implement m_counts logic to get Big N for header.

We should produce output that matches SAS exactly in counts.

Let's craft final R code.

We need to handle reading files: Use function read_adam(name):

- Check for file.path(adam_path, paste0(name, ".sas7bdat")), .xpt, .sas7bdat, .xpt lowercase, etc.

- Also check for .xpt with name.xpt

- Use haven::read_sas if exists, else haven::read_xpt.

- If not found, list.files and try to find case-insensitive.

We should also handle that adam_path may contain .xpt files named adsl.xpt etc.

We need to implement robust.

We should also handle that haven::read_sas may fail due to encoding, fallback.

We need to ensure we convert all character columns to character (trim).

We need to handle that ADSL and ADLB may have different column names case: USUBJID, TRTA, TRTAN, etc. Use case-insensitive matching? But SAS is case-insensitive for variable names, but R is case-sensitive. We should normalize column names to uppercase for processing, then output with original case? The spec says write CSV with SAS dataset's variable names (same case is fine, matching is case-insensitive). So we can use uppercase.

Simpler: after reading, convert names to uppercase via toupper(names(df)).

Then processing uses uppercase names.

We need to ensure we handle PARAMCD filtering: c('ALT','AST','BILI','CREAT','HGB','WBC','PLAT','NEUT')

We need to handle inner join adsl where SAFFL='Y' - but lb step already filters b.SAFFL='Y' and inner join. So we can filter adsl SAFFL='Y' then join.

We need to handle that adlb may have multiple records per subject/visit.

We need to implement steps exactly.

Let's write R script.

We should include library calls: library(haven), library(dplyr), tidyr, etc.

We need to ensure we don't open graphics.

We need to write progress messages via cat.

We need to handle OUT_DIR.

We need to produce final files: file.path(out, "t_lb_shift.csv") and file.path(out, "t_lb_shift.txt")

We should also produce t_lb_shift.rtf? But spec says write text rendering and CSV. We can also copy txt to rtf if needed. But we will produce both .txt and .csv, and also .rtf as copy of txt for compatibility.

We should ensure we write 969 numbers: Our TXT should contain numbers.

We need to format TXT with fixed-width columns.

We should create a helper to format table.

We need to ensure we handle missing wg columns: ensure wg0..wg4 exist.

We need to handle that report dataset may have PARAMCD order: SAS orders by TRTAN, PARAMCD. PARAMCD alphabetical? Let's sort by TRTAN, PARAMCD.

We should also handle that PARAM is same for each PARAMCD, but we keep first.

We need to implement all steps in R with dplyr.

Let's craft final code block.