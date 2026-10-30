We need to translate the SAS program to R, producing outputs: a table listing saved as text and CSV. No RTF needed; we output .txt and .csv.

We must follow SAS semantics: sorting, counts, percentages, formatting.

We have macro m_counts that calculates big N per treatment group and total. In SAS, they use TRTA variable values (likely "Placebo", "Drug X Low Dose", "Drug X High Dose") from adsl. We'll compute big N similarly.

We need to read adam.adsl and adam.adae from the given directories. The libref adam points to D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam. So files likely are .sas7bdat. Use haven::read_sas.

Paths: file.path(root, "data/raw/own-synthetic/adam", "adsl.sas7bdat")? Actually dataset names: adsl, adae. So we can construct: file.path(Sys.getenv("root")? Not given. But macro variables are set earlier; we can just use the absolute paths given in libname: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam. So adsl path: "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam/adsl.sas7bdat". Similarly adae.

We'll read them.

Implement round-half-up? For percentages we need format 5.1 (one decimal). SAS default rounding is half away from zero. In R, we can use function round_half_up <- function(x, digits) { sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits }. Use for formatting.

Steps:

1. Compute big N per TRTA where SAFFL='Y' in adsl. Use distinct USUBJID.

2. Compute total big N across all.

3. Build header strings: col1hdr = "Placebo~(N=...)" etc. The macro uses trtlbl from dataset (TRTA values). In SAS they set macro variables trtlbl1 etc from the dataset values. We'll replicate: get unique TRTA values sorted by TRTA (order by). Use same order as in adsl dataset after sorting by TRTA (character ordering). We'll store vector trt_labels.

4. Build col headers: paste0(trt_label, " (N=", bign, ")") but with ~ for line break? In SAS they use "~" to indicate line break in ODS. We'll keep same string with "~". So col1hdr = paste0(trt_labels[1], "~(N=", bign[1], ")").

5. Subset ae: join adae with adsl on USUBJID, filter SAFFL='Y' and TRTEMFL='Y'.

6. Count subjects by SOC (AEBODSYS) per TRTA: distinct USUBJID.

7. Compute total per SOC across all TRTA.

8. Count by SOC+PT (AEDECOD) per TRTA.

9. Total per PT across all TRTA.

10. Any TEAE rows: for each TRTA, count distinct USUBJID in ae. rowtype=0, soc_ord=0, pt_ord=0.

11. Build soc_rows: join soc_cnt with soc_tot to get cnt_tot as soc_ord (sort order). rowtype=1.

12. Build pt_rows: join pt_cnt with pt_tot and soc_tot to get soc_ord and pt_ord. rowtype=2.

13. Combine any_ae, soc_rows, pt_rows into combined.

14. Merge with big N per TRTA to compute pct = cnt / bign *100.

15. Create value string: if cnt>0 then paste0(cnt, " (", formatted pct with 1 decimal, ")") else "0". Use formatting with no leading spaces.

16. Sorting: order by soc_ord desc, AEBODSYS, rowtype, pt_ord desc, AEDECOD, TRTA. In SAS they also sort by descending soc_ord, AEBODSYS, rowtype, descending pt_ord, AEDECOD, TRTA. We'll implement using arrange with dplyr.

Note: need to keep missing values handling: missing counts become 0 later.

17. After sorting, need to transpose: rows become columns per TRTA. Use tidyr::pivot_wider with names_from = TRTA, values_from = value. Then rename columns to col1, col2, col3 according to order of treatment labels (same as col headers). Ensure order matches.

18. After pivot, we need to have columns col1, col2, col3. The SAS code uses PROC TRANSPOSE with prefix=col and then rename colPlacebo etc. We'll just create col1-col3.

19. Create display_term: if rowtype==0 then display_term = AEBODSYS (Any TEAE). else if rowtype==1 then display_term = AEBODSYS (SOC). else display_term = paste0("  ", AEDECOD) (indented). Also need to keep rowtype, soc_ord, pt_ord for sorting but not in final output.

20. Replace missing with "0": after pivot, any NA become "0". We'll fill.

21. Final report dataset includes columns: display_term, col1, col2, col3. Also maybe rowtype etc not needed for output.

22. Write CSV: write.csv(report_df, file.path(out, "t_ae_soc_pt.csv"), row.names=FALSE, na="").

23. Write text rendering: need to produce plain-text table similar to SAS PROC REPORT output. We'll create a simple text table with columns: System Organ Class  Preferred Term (display_term) and three treatment columns with headers. Use format aligning. Could use knitr::kable but not allowed. We'll construct manually: create header line with column names (display_term header maybe "System Organ Class  Preferred Term") and col headers. Then rows.

We need to format numbers as in SAS: counts and percentages with one decimal, e.g., "12 ( 34.5%)"? SAS format 5.1 yields like "34.5". In SAS they used cat with put(cnt,8.) and put(pct,5.1). That yields no extra spaces inside parentheses except maybe leading spaces due to width. We'll just produce "12 (34.5%)". Acceptable.

Write text file: use writeLines.

Also need to write CSV of the table body (maybe same as CSV already). Already done.

Also need to write any intermediate datasets? Not required.

Set environment variable OUT_DIR: out <- Sys.getenv("OUT_DIR"). Ensure not empty; else use current dir.

Implement round_half_up for pct formatting.

Edge Cases: Ensure ordering of treatment columns matches col headers order. We'll get trt_order vector from big N dataset sorted by TRTA.

Implementation steps in R script.

Now produce final R script inside ```r``` block only.