Need to translate SAS program to R. Must read inputs from absolute paths given. Librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam, sdtm similarly. Need to find files. Likely .sas7bdat or .xpt. Check via listing? We don't have filesystem view but assume haven can read. Need to produce t_lb_shift.rtf equivalent: table/listing text with 969 numbers -> write text rendering and CSV.

We need to reproduce logic:

- m_counts: Big N per TRTA where SAFFL=Y. Need to get N per treatment. Also ntrt=3.

- Step2: lb table from adam.adlb joined with adsl where SAFFL=Y and PARAMCD in list.

- Step3: bl_grade distinct where ABLFL='Y' and BTOXGR ne ''.

- Step4: worst_grade: where ABLFL ne 'Y' and ATOXGR ne '' and AVISITN>0, group by USUBJID,TRTA,TRTAN,PARAMCD, max(input(ATOXGR,best.)) as worst_grade_n. Note ATOXGR may be character like '0','1', etc. Convert to numeric max.

- Step5: merge baseline and worst.

- Step6: shift_cnt count distinct USUBJID per TRTA,TRTAN,PARAMCD,PARAM,bl_grade,worst_grade

- Step7: grades 0-4, cross join all_cells: distinct TRTA,TRTAN,PARAMCD,PARAM from shift_cnt cross join grades b cross grades c. So all_cells includes every combination of bl_grade and worst_grade for each TRTA/PARAMCD present.

- Merge actual counts, coalesce 0.

- Step8: transpose worst_grade into columns wg0..wg4. By TRTAN TRTA PARAMCD PARAM bl_grade. Then report: bl_label mapping, replace missing with 0, total = sum wg.

- Step9: generate output per treatment group.

We need to produce outputs under OUT_DIR. Need to write CSV and TXT.

We need to figure out treatment labels: from u_setup: trt1lbl Placebo, trt2lbl [Drug] Low Dose, trt3lbl [Drug] High Dose. But actual TRTA values in ADSL may be those strings.

We need to read ADSL and ADLB.

We need to handle paths: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Let's list files via R? We can attempt to list directory.

We need to produce script that does that dynamically.

Also need to handle Big N header: not needed for CSV but for text.

We need to produce t_lb_shift.csv and t_lb_shift.txt . The expected output has 969 numbers. Let's estimate: 3 treatments * 8 parameters * 5 baseline grades * 6 columns (wg0-4 + total) = 3*8*5*6 = 720 numbers. Plus maybe totals? Actually 969 numbers suggests includes something else? Let's compute: 3*8*5*5 =600 for wg cells, plus totals 120 =720. 969 numbers maybe includes counts in text? Could be 969 numbers in RTF. Our text rendering should contain same numbers.

We need to ensure we replicate SAS behavior: ordering by TRTAN, PARAMCD, bl_grade, worst_grade. PARAMCD order alphabetical? In SAS, order by TRTAN, PARAMCD. So PARAMCD alphabetical.

We need to produce CSV of table body: likely columns: TRTA, TRTAN, PARAMCD, PARAM, bl_label, wg0..wg4, total.

Also need to produce TXT rendering that mimics PROC REPORT with titles/footnotes.

We must also handle missing values: BTOXGR ne '' and ATOXGR ne ''.

We need to implement round-half-up helper but not needed here.

We need to ensure we handle character comparison case-sensitive.

We need to write script that reads .sas7bdat or .xpt. Use haven::read_sas and haven::read_xpt. Need to detect files.

We need to produce OUT_DIR handling: out <- Sys.getenv("OUT_DIR"); if empty, use temp.

We need to ensure we write CSV with na="".

We need to produce PNG? No, table only.

We need to include titles/footnotes verbatim.

Let's craft R script.

Steps in R:

- library haven, dplyr, tidyr, etc.

- Define paths: adam_path = "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam", sdtm_path similarly.

- Function to read dataset: try list.files, find file matching adsl (case-insensitive) with extension .sas7bdat or .xpt. Use haven.

- Read adsl, adlb.

- Ensure columns exist: USUBJID, TRTA, TRTAN, SAFFL, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL.

- Convert character columns to character, trim.

- Compute Big N: filter SAFFL == "Y" (upcase). Count distinct USUBJID per TRTA, order by TRTAN.

- Derive lb: inner join adlb with adsl on USUBJID where SAFFL=Y and PARAMCD in list. Actually SAS does inner join but also filters b.SAFFL='Y'. So we need to filter adsl SAFFL=Y then join.

- bl_grade: distinct where ABLFL=='Y' and BTOXGR != '' and not NA.

- worst_grade: filter ABLFL != 'Y' (including missing? SAS: ABLFL ne 'Y' includes missing? In SAS, missing ne 'Y' true. So include NA or '' not equal 'Y') and ATOXGR != '' and AVISITN >0. Then group by USUBJID, TRTA, TRTAN, PARAMCD, compute max of as.numeric(ATOXGR). Need to handle non-numeric? ATOXGR should be 0-4.

- worst_grade2: convert to character, if 0 then '0'.

- shift: inner join bl_grade and worst_grade2 on USUBJID, PARAMCD, TRTA.

- shift_cnt: count distinct USUBJID per TRTA,TRTAN,PARAMCD,PARAM,bl_grade,worst_grade.

- grades: 0-4 as character.

- all_cells: distinct TRTA,TRTAN,PARAMCD,PARAM from shift_cnt cross join grades.

- shift_full: left join.

- transpose: pivot_wider.

- report: create bl_label, replace NA with 0, total.

- Need to ensure ordering: by TRTAN, PARAMCD, bl_grade (as numeric order 0-4).

- Generate CSV: report ordered.

- Generate TXT: need to reproduce titles.

Titles from u_titles.sas for T_LB_SHIFT:
title4 Table 14.3.4
title5 Shift Table for Laboratory Parameters
title6 Safety Analysis Set
footnote1 Data cutoff: [DDMONYYYY]
footnote2 Source: ADaM datasets
footnote3 Program: &sysin. | Run: date
footnote4 Grades based on NCI-CTCAE v[X.X]
footnote5 Shift from baseline grade to worst post-baseline grade.

We need to include sponsor, protocol placeholders: [Sponsor Name], [Protocol Number], [STUDYID].

We should include those verbatim.

We need to produce text rendering with columns.

We need to ensure 969 numbers: our CSV will have numbers.

We should also produce counts for verification.

We need to handle haven labelled vectors: convert to character/numeric via haven::as_factor? Better use haven::zap_labels.

We need to implement reading function that handles both .sas7bdat and .xpt.

We need to ensure we handle TRTA ordering: TRTAN numeric.

We need to produce final script inside one ```r code block.

We must not call install.packages.

We need to write files to OUT_DIR.

Let's craft script.

Potential pitfalls: ADLB may have PARAMCD values not in list? Filter.

We need to ensure we handle BTOXGR and ATOXGR as character; they may be numeric labelled.

We need to ensure we handle AVISITN numeric.

We need to ensure we handle ABLFL character.

We need to ensure we handle distinct.

We need to ensure we handle worst_grade_n max correctly: if multiple rows per subject, take max.

We need to ensure we handle inner join on TRTA as well: SAS joins on USUBJID, PARAMCD, TRTA. So if TRTA differs between baseline and post, they must match. We'll do that.

We need to ensure we handle PARAM: from bl_grade (distinct). Use that.

We need to ensure we handle all_cells cross join: distinct TRTA,TRTAN,PARAMCD,PARAM from shift_cnt. So if a parameter has no subjects in a treatment, it won't appear for that treatment. That's SAS behavior.

We need to ensure we handle transpose: by TRTAN TRTA PARAMCD PARAM bl_grade. So each row is baseline grade.

We need to ensure we handle wg columns: wg0..wg4.

We need to ensure we handle total.

We need to produce text file with table per treatment group.

We should also produce CSV of plotted data? Not needed.

We need to ensure we handle OUT_DIR creation.

We need to ensure we handle writing CSV with row.names FALSE, na "".

We need to ensure we handle dates as YYYY-MM-DD but not needed.

We need to ensure we handle numeric unrounded.

We need to ensure we handle SAS sorting: character ordering uppercase before lowercase - not relevant.

We need to ensure we handle missing values sort first - not needed.

We need to ensure we handle PROC REPORT page break after PARAMCD: in text we can separate by parameter.

We need to produce final script.

Let's write R code.

