We need to translate SAS program to R script that reproduces outputs: P048.lst (text listing) and sasrtf.rtf (same). The script must generate these outputs as text and CSV. The SAS program creates a table via PROC REPORT, with columns: pagex, ord1, ord2, ord3, col1, then treatment columns trt1, trt2, ... up to maxc (max treatment code). It also sets placeholder ###0 for missing cells, and replaces spaces with '#'.

We need to replicate the logic.

Steps:

1. Set paths from macro variables: rawadam path etc. But we can just use environment variable OUT_DIR for output. Input file: source.adsl.xpt located at &rawadam./adsl.xpt. rawadam macro variable is D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam. So path: "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt". Use haven::read_xpt.

2. Data processing:

- Filter where trt01p != '' (character). In SAS, trt01p is character variable. In R, after reading, treat as character.

- Recode trt01pn: if 0 then 3; else if 54 then 1; else if 81 then 2. trt01pn numeric.

3. PROC FREQ topline: table studyid * trt01pn * trt01p * disconfl / out=topline (drop=percent). This produces counts for each combination. In R, we can use dplyr::count.

Similarly midline1: where DSRAEFL='Y' or DTHFL='Y', table studyid * trt01pn * trt01p * DCREASCD -> midline1; also table studyid * DCREASCD -> sortord1.

midline2: where DCdecod='LACK OF EFFICACY', same table -> midline2; sortord2.

midline3: where DCdecod^='LACK OF EFFICACY' and DSRAEFL^='Y' and DTHFL^='Y' and disconfl='Y', produce midline3 and sortord3.

Note: In SAS, they have two WHERE statements: first where DCdecod^='LACK OF EFFICACY' and DSRAEFL^='Y' and DTHFL^='Y'; then also where disconfl='Y'; The second overrides? Actually SAS allows multiple WHERE statements; they are combined with AND. So final condition: DCdecod not 'LACK OF EFFICACY' AND DSRAEFL not 'Y' AND DTHFL not 'Y' AND disconfl='Y'.

4. Combine datasets:

- all: set topline, midline1, midline2, midline3 with in= flags a,b,c,d. By studyid trt01pn trt01p. In SAS they use BY statement but dataset not sorted? They didn't sort before set; but BY requires sorted. However they didn't sort; maybe they rely on order of set? We'll replicate by just binding rows and then assign ord1/ord2 based on source.

Implementation: add columns ord1, ord2 based on source.

- alls: set sortord1, sortord2, sortord3 with flags b,c,d. By studyid. Add ord1/ord2 accordingly.

5. Sort alls by ord1 ord2 descending count. Then compute ord3: within each ord1 ord2 group, descending count, assign sequential order starting 1.

Implementation: arrange, then group_by(ord1, ord2) %>% mutate(ord3 = row_number()).

6. Pop dataset: PROC FREQ studyid * trt01pn * trt01p -> count tot. rename count to tot.

7. allp: merge all with pop by studyid trt01pn trt01p. In SAS they merge all pop; then if count not missing, create countc = put(count,4.)||' ('||put(round(count/tot,0.1),5.1)||')'. Need to replicate formatting: count formatted width 4 (right-aligned). In SAS, put(count,4.) yields count as character with width 4, leading spaces. Then round(count/tot,0.1) rounds to one decimal using SAS round half away from zero. We'll implement custom round_half_up function.

Then countc = paste0(count_formatted, " (", perc_formatted, ")").

If ord1=1 then set DCREASCD based on disconfl: if disconfl='Y' then 'Discontinued' else 'Completed the Study'.

Note: DCREASCD variable may already exist from earlier datasets (midline). For topline rows, DCREASCD may be missing; they set it accordingly.

8. Sort allp by studyid ord1 ord2 dcreascd trt01pn.

9. Transpose: var countc, by studyid ord1 ord2 dcreascd, id trt01pn, prefix trt. So each trt01pn becomes column trt<value>. In R, we can pivot_wider.

10. Determine treatment groups: from pop dataset, need list of distinct trt01pn per study? They compute macro variables globally across all data (not per study). They create popc string of trt01pn values separated by ', '. Also set macro variables trt<code>c = trt01p (character treatment name) and trt<code>n = trt01p (numeric treatment count?) Actually they set both to trt01p (character). Probably they intend to use for column headers: "&&trt&i.c @(N=&&trt&i.n)". So they need treatment name and count.

Thus we need to compute for each trt01pn: treatment name (trt01p) and count (pop count). We'll store in a data frame.

11. Replace missing cells with '###0' and replace spaces with '#'.

In allpt dataset after transpose, missing cells are NA. They set trt&i.='' then '###0' else replace spaces with '#'.

Implementation: for each trt column, if NA or empty string, set to '###0'; else replace spaces with '#'.

12. Merge sort order (alls2) onto allpt2 by studyid ord1 ord2. Then adjust ord3: if ord1=1 and upcase(DCREASCD)='DISCONTINUED' then ord3=2 else if ord1=1 then ord3=1.

13. Sort final table by studyid ord1 ord2 ord3.

14. Create final dataset with col1: if ord1=1 then dcreascd; else if ord1=3 then paste0('####', dcreascd). Also pagex=1.

15. Output: need to produce listing text and CSV replicating PROC REPORT output.

The PROC REPORT layout: columns pagex ord1 ord2 ord3 col1 then each trt column with header "  n   %". The column headers include treatment name and count: "&&trt&i.c @(N=&&trt&i.n)". In SAS they use split='@' to split header at '@' into two lines? Not needed for text output.

We need to produce a plain-text rendering similar to SAS listing. Could approximate by creating a formatted table with values separated by spaces.

Simplify: produce CSV of final table (including pagex, ord1, ord2, ord3, col1, trt columns). Also produce a text file where each row is concatenated with spaces, maybe using format similar to SAS: each column width? Not specified. We'll produce a simple tab-separated text.

But requirement: "contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". So we need to ensure numbers are same as SAS: countc strings already have formatting.

Thus text file can be same as CSV but with spaces? We'll output as plain text using write.table with sep=" " and row.names=FALSE, col.names=TRUE.

Also need to output sasrtf.rtf as text rendering and CSV. Since R cannot produce RTF easily, we can output same as txt but name .rtf.

Thus produce two files: P048.lst.txt and P048.lst.csv, and sasrtf.rtf.txt and sasrtf.rtf.csv? Actually expected outputs: P048.lst (table/listing text) and sasrtf.rtf (table/listing text). So we need to write text files named "P048.lst.txt"? The spec says write each output dataset as CSV with name. For tables/listings, also write plain-text rendering (file.path(out, "<name>.txt")). So for listing name "P048.lst", we write "P048.lst.txt". For rtf, "sasrtf.rtf.txt". Also CSV of table body: "P048.lst.csv" and "sasrtf.rtf.csv". We'll follow that.

Implementation steps in R script:

- Define out <- Sys.getenv("OUT_DIR"); if empty, set to ".".

- Read adsl.xpt.

- Process as described using dplyr.

- Helper round_half_up: function(x, unit) { sign(x) * floor(abs(x)/unit + 0.5) * unit } but need to round to one decimal: round_half_up(x, 0.1). Then format with one decimal, width 5.1 (SAS format 5.1 includes leading spaces). We'll use sprintf("%5.1f", value).

- For count formatted width 4: sprintf("%4d", count).

- For countc: paste0(count_fmt, " (", perc_fmt, ")").

- After transpose, ensure columns for all treatment codes present across data. Determine max trt01pn value (maxc). We'll create vector of all possible trt codes from pop$trt01pn sorted ascending.

- Ensure columns exist; missing ones fill with NA.

- Apply zero macro: replace NA with "###0", replace spaces with "#". Also for existing strings, replace spaces with "#". Use gsub(" ", "#", x).

- Merge sort order.

- Adjust ord3.

- Final dataset.

- Write outputs.

Edge Cases: Character case sensitivity: SAS BY sorts missing first, case-sensitive. Our operations using dplyr will preserve order if we sort accordingly.

Sorting: For final sort, we need by studyid ord1 ord2 ord3. We'll arrange accordingly.

Now produce R script.

