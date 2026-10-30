We need to translate SAS program to R script that reproduces outputs: final_report table (listing) and also dataset outputs? The declared output types: table;dataset. The final_report is a dataset and also a table listing (P045.lst). So we need to write CSV of final_report and also a text rendering of the table with same numbers formatting.

We need to read source files: ada e.xpt and adsl.xpt from &rawadam. path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adae.xpt and adsl.xpt.

We need to replicate SAS steps.

Key steps:

1. Read adae, keep variables: usubjid trtan aedecod aeser aocc02fl aocc04fl saffl; filter where (aocc02fl='Y' or aocc04fl='Y') and aeser='Y' and saffl='Y'.

2. Sort by usubjid (not needed for merge but we can use dplyr arrange).

3. Read adsl, keep usubjid trt01an; where trt01an not missing.

4. Sort adsl nodupkey by usubjid trt01an (unique). So distinct.

5. Merge adsl and adae by usubjid, keep only where both present (inner join). This yields adae_sl.

6. Denominator calculation: PROC FREQ on adsl to get counts per trt01an. So sum_1: trt01an, count.

7. Create macro variables: trtcn<value> = count for each trt01an; trtnum<index> = trt01an; numtrt = number of treatments.

We need these for later labeling and dummy dataset.

8. Sort adae_sl by usubjid aedecod, remove duplicates (nodupkey). So adae_sl2 distinct by usubjid aedecod.

9. Create adae_dummy: distinct aedecod where aocc02fl='Y' (from adae_sl). So list of aedecod that have aocc02fl='Y'.

10. Sort adae_sl2 by aedecod.

11. Merge adae_sl2 with adae_dummy by aedecod, keep only where both (inner). This yields adae_sl3 (only rows where aedecod present in dummy). So essentially filter adae_sl2 to those aedecod where aocc02fl='Y' (since dummy contains those aedecod). But note adae_dummy only includes aedecod where aocc02fl='Y' (any row). So adae_sl3 = adae_sl2 where aedecod in that set.

12. PROC FREQ on adae_sl3: table aedecod*trtan / out = sum_2 (keep aedecod trtan count). So count of rows per aedecod and trtan.

13. Macro dummy: create dummy_2 dataset: set adae_dummy; by aedecod; for each aedecod, loop i=1 to numtrt, if first.aedecod then trtan = &&trtnum&i; output; end. So for each aedecod, create rows for each treatment number (trtan) equal to each treatment code (trtnum1, trtnum2,...). So dummy_2 contains all combinations of aedecod (from dummy) and trtan (all treatment codes). No other variables.

14. Merge dummy_2 with sum_2 by aedecod trtan => adae_sl4. So ensures rows for all combos, with count possibly missing.

15. Sort adae_sl4 by trtan.

16. Merge adae_sl4 with sum_1 (renamed trt01an=trtan, count=bign) by trtan => adae_sl5. Keep only rows where a (from adae_sl4) i.e., keep all rows from adae_sl4 (since merge with sum_1 adds denominator). So adae_sl5 has aedecod, trtan, count (from sum_2 maybe missing), bign (denominator per trtan).

17. Data step adae_sl6: set adae_sl5; by trtan aedecod; do j=0 to 1; if first.aedecod then resp=j; then compute count_final: if resp=1 and count not missing then count_final = count; else if resp=1 and count missing then 0; else if resp=0 and count not missing then count_final = bign - count; else if resp=0 and count missing then count_final = bign; output; end; So for each aedecod/trtan, produce two rows: resp=0 (non-event) count_final = denominator - count (or denominator if count missing), resp=1 (event) count_final = count (or 0). So this creates data for Fisher test.

18. Sort adae_sl6 by aedecod.

19. PROC FREQ with weight count_final; table trtan*resp / out = sum_final sparse fisher; by aedecod; exact fisher; ODS output FishersExact = fish1. So for each aedecod, compute weighted frequencies of trtan vs resp, produce sum_final (contingency table) and fisher p-value.

We need to replicate Fisher's exact test with weights. In R, we can use fisher.test on matrix of counts. For each aedecod, we have counts for each trtan (two treatments? maybe more) and resp (0/1). Weighted counts are count_final. So we need to build contingency table: rows = trtan, columns = resp (0,1). Then fisher.test with alternative two-sided, get p-value. The SAS ODS output FishersExact contains variable CVALUE1 (p-value) and maybe other columns. We'll extract p-value.

Implementation: For each aedecod, create matrix with rows sorted by trtan (maybe order). Use fisher.test(matrix) to get p-value. Save as fish1 with columns aedecod, cvalue1 (p-value). Note SAS uses exact fisher with sparse option; R fisher.test handles zeros.

20. fish2: keep aedecod cvalue1 where name1='XP2_FISH' (the ODS output includes variable name1). In R we just have fish1 with aedecod and pvalue; we can rename column to cvalue1.

21. sum_final: from sum_final (the out= sum_final from PROC FREQ). It contains trtan, resp, count (weighted). Then they filter where resp=1; create cp = compress(put(count,8.0))||'('||compress(put(count,8.1))||')'; So cp is count integer then (count with 1 decimal). In SAS count is integer; put(count,8.1) prints with one decimal (e.g., 5.0). So cp like "5(5.0)". Then set ord: if trtan=81 then ord=1 else ord=2. (Assuming treatment codes 81 and 54? Actually later they have trt0, trt54, trt81. So maybe trtan values: 0, 54, 81. ord used to sort.

22. Sort sum_final by aedecod ord.

23. Retain count_max: if ord=1 then count_max = count. So for each aedecod, count_max is count for trtan=81 (since ord=1). So count_max is the count for treatment 81 (maybe active). This is used for sorting later.

24. Sort sum_final by aedecod count_max.

25. PROC TRANSPOSE: by aedecod count_max; id trtan; var cp; prefix=trt. So produce columns trt0, trt54, trt81 with cp values.

26. Merge sum_final2 with fish2 by aedecod (adds p-value column cvalue1).

27. Sort sum_final2 by descending count_max aedecod.

28. Next part: compute at least one SAE per subject.

- adae_sl_sub: from adae_sl keep usubjid where aocc02fl='Y', distinct.

- adae_sl_sub2: merge adsl (all) with adae_sl_sub by usubjid; if a then keep all subjects; if a and b then flg=1 else flg=0. So flg indicates subject had at least one SAE.

- Fisher test: PROC FREQ on adae_sl_sub2 (rename trt01an to trtan) table trtan*flg / out = sum_final_sub sparse fisher; exact fisher; ODS output FishersExact = fish_sub.

- fish_sub2: keep cvalue1 where name1='XP2_FISH'.

- sum_final_sub: length aedecod $100; set sum_final_sub; where flg=1; cp = count|| '('|| count with 1 decimal || ')'; aedecod = 'Number of subjects reporting serious adverse events';

- sort sum_final_sub by aedecod; transpose similar to earlier: by aedecod; id trtan; var cp; prefix trt; produce sum_final_sub2.

- merge sum_final_sub2 with fish_sub2 (adds p-value column cvalue1).

29. final_report: concatenate sum_final_sub2 and sum_final2 (stack rows). So final_report has rows: first the "Number of subjects reporting serious adverse events" row (with cp values per treatment) and then rows for each aedecod (preferred term) with cp per treatment and p-value.

30. PROC REPORT: produce listing with columns aedecod trt0 trt54 trt81 cvalue1, with headings and compute lines. The output is a table listing.

Thus we need to produce final_report CSV and also a text rendering replicating PROC REPORT formatting: column widths, headings, lines before/after page. Since we only need a text file with numbers (203 numbers). We'll produce a simple plain text table with same column order and values formatted as in SAS: aedecod left, trt0 etc as cp strings, cvalue1 as p-value (maybe formatted with 6.4? Not specified). We'll just output as CSV and also a .txt with same as CSV? But spec says "plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". So we need to produce a text file that mimics PROC REPORT output. Could just write the data frame to a text file using write.table with sep = "\t", row.names=FALSE, col.names=TRUE. That will contain numbers.

We'll also write CSV of the table body (same as final_report). The .txt may include headings lines as in compute before/after. We'll add those lines manually.

Implementation details:

- Need round-half-up for any rounding? The only rounding is put(count,8.1) which prints with one decimal; count is integer, so .0. So no rounding needed.

- Need to ensure character ordering: SAS sorts character with case-sensitive, uppercase before lowercase. In R, default order is lexical with case-sensitive (based on locale). We'll use arrange with .by_group.

- Missing values: treat as NA.

- For macro variables: we need mapping trtcn values for each treatment code to use in headings. We'll compute a named vector trtcn[code] = count.

- For headings: "Placebo#N=&trtcn0" etc. So we need to know which treatment codes correspond to placebo (0), treatment1 (54), treatment2 (81). We'll just use the codes present.

- In final_report, columns may be missing for some trtan if not present; after transpose, missing values become . (empty). We'll keep as empty strings.

Implementation steps in R:

- Use haven::read_xpt to read .xpt.

- Use dplyr for manipulations.

- For distinct, use distinct.

- For merging, use left_join / inner_join.

- For weighted Fisher test: For each aedecod, create matrix with rows = trtan values sorted ascending, columns = resp (0,1). Use counts from sum_final (which includes both resp rows). We'll compute counts via adae_sl6 filtered for each aedecod, then pivot_wider.

Simplify: Use adae_sl6, filter resp==0 and resp==1 counts. Build matrix.

Implementation:

library(dplyr); library(tidyr); library(purrr); library(stringr); library(readr); library(haven); library(ggplot2) maybe not needed.

Processing:

rawadam <- Sys.getenv("rawadam")? Actually macro variable &rawadam. But we have environment variable OUT_DIR only. Paths are absolute; we can hardcode using the macro values? The script should use the same absolute paths as given. So we can set rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam". Use forward slashes.

Read adae <- read_xpt(file.path(rawadam, "adae.xpt")) %>% select(usubjid, trtan, aedecod, aeser, aocc02fl, aocc04fl, saffl) %>% filter((aocc02fl == "Y" | aocc04fl == "Y") & aeser == "Y" & saffl == "Y")

Read adsl similarly.

Proceed.

Denominator sum_1: adsl %>% count(trt01an, name="count") -> sum_1.

Create vectors:

trt_counts <- setNames(sum_1$count, sum_1$trt01an) # names are treatment codes as character.

trt_codes <- sum_1$trt01an %>% as.character()

numtrt <- length(trt_codes)

Create trtnum vector: index -> code.

trtnum <- setNames(trt_codes, seq_len(numtrt))

Now dummy_2: for each aedecod in adae_dummy (distinct aedecod where aocc02fl='Y'), create rows for each trtnum.

Implementation: expand_grid(aedecod = adae_dummy$aedecod, trtan = trt_codes) -> dummy_2.

Proceed.

Now compute adae_sl6 as described.

After that, for each aedecod, compute fisher p-value.

We need sum_final (the out= sum_final from PROC FREQ). In SAS, sum_final contains rows for each trtan and resp (0/1) with weighted counts (count_final). We'll compute sum_final from adae_sl6: group_by(aedecod, trtan, resp) summarise(count = sum(count_final)). That's same.

Then for each aedecod, compute cp for resp=1 rows later.

But we also need sum_final (the dataset) for later steps: they filter resp=1 and compute cp.

So we can compute sum_final_df = adae_sl6 %>% group_by(aedecod, trtan, resp) %>% summarise(count = sum(count_final), .groups='drop')

Now compute fisher p-values:

fish_df <- sum_final_df %>% group_by(aedecod) %>% summarise(pvalue = {
   mat <- pivot_wider(., names_from = resp, values_from = count, values_fill = list(count=0)) %>%
          select(trtan, `0`, `1`) %>% arrange(trtan)
   # create matrix rows = trtan, columns = resp
   m <- as.matrix(mat %>% select(`0`, `1`))
   # fisher.test expects 2x2? Actually can be RxC. Use fisher.test(m)
   ft <- tryCatch(fisher.test(m), error=function(e) list(p.value=NA))
   ft$p.value
}, .groups='drop')

Now fish2: keep aedecod and cvalue1 (pvalue). We'll rename pvalue to cvalue1 and format maybe with 6.4? SAS default prints with BEST12. We'll format with scientific? We'll just keep as numeric.

Now sum_final processing:

sum_final_resp1 <- sum_final_df %>% filter(resp == 1) %>% mutate(cp = paste0(count, "(", sprintf("%.1f", count), ")")) %>% mutate(ord = ifelse(trtan == 81, 1, 2))

But trtan values may be numeric; we need to compare to 81. Use as.numeric.

Now sort by aedecod, ord.

Then compute count_max: for each aedecod, count_max = count where ord==1 (i.e., trtan==81). So we can compute via mutate(count_max = ifelse(ord==1, count, NA)) then fill.

Simpler: create a data frame with count_max per aedecod: max_count <- sum_final_resp1 %>% filter(ord==1) %>% select(aedecod, count_max = count)

Then join back.

Now transpose: we need wide format with columns trt0, trt54, trt81 (prefix trt). Use pivot_wider with names_from = trtan, values_from = cp, names_prefix = "trt". Ensure missing become "".

sum_final2 <- sum_final_resp1 %>% select(aedecod, trtan, cp, count_max) %>% pivot_wider(names_from = trtan, values_from = cp, names_prefix = "trt", values_fill = list(cp = ""))

Now merge with fish2: left_join by aedecod.

Now sort descending count_max then aedecod.

Now compute sum_final_sub:

- adae_sl_sub: distinct usubjid where aocc02fl='Y' from adae_sl (which is adae_sl earlier). adae_sl we have earlier (merged adsl+adae). So adae_sl_sub = adae_sl %>% filter(aocc02fl == "Y") %>% distinct(usubjid)

- adae_sl_sub2: merge adsl (all) with adae_sl_sub (left join) to get flg. Use left_join, then flg = ifelse(!is.na(usubjid.y), 1, 0). Keep trt01an as trtan.

Implementation: adsl_flg <- adsl %>% left_join(adae_sl_sub, by="usubjid", suffix=c("",".y")) %>% mutate(flg = ifelse(!is.na(usubjid.y), 1, 0)) %>% select(usubjid, trt01an, flg)

Rename trt01an to trtan.

Now fisher test for subjects: similar to earlier but using weight = 1 (no weight). Actually PROC FREQ with table trtan*flg / out = sum_final_sub sparse fisher; no weight. So we need counts of subjects per trtan and flg.

Compute sum_final_sub_df <- adsl_flg %>% group_by(trtan, flg) %>% summarise(count = n(), .groups='drop')

Now compute fisher p-value for each aedecod? Wait there is no aedecod here; they set aedecod = 'Number of subjects reporting serious adverse events' later. So fish_sub is just one p-value for overall test across treatments (since no grouping). So we compute fisher test on matrix of trtan vs flg.

Construct matrix: rows = trtan, columns = flg (0,1). Use pivot_wider.

ft_sub <- fisher.test(as.matrix(pivot_wider(sum_final_sub_df, names_from = flg, values_from = count, values_fill = list(count=0)) %>% select(-trtan) %>% arrange(trtan)))

p_sub <- ft_sub$p.value

Now sum_final_sub: take rows where flg=1 (i.e., subjects with SAE). So we need count for flg=1 per trtan. So subset sum_final_sub_df where flg==1, compute cp similar.

sum_final_sub <- sum_final_sub_df %>% filter(flg == 1) %>% mutate(cp = paste0(count, "(", sprintf("%.1f", count), ")")) %>% mutate(aedecod = "Number of subjects reporting serious adverse events")

Now transpose: pivot_wider names_from trtan, values_from cp, names_prefix = "trt", values_fill = "".

sum_final_sub2 <- sum_final_sub %>% select(aedecod, trtan, cp) %>% pivot_wider(names_from = trtan, values_from = cp, names_prefix = "trt", values_fill = list(cp=""))

Add cvalue1 column = formatted p_sub (maybe with 6.4). We'll format as sprintf("%.4f", p_sub).

Now final_report = bind_rows(sum_final_sub2, sum_final2) (order: sub first then AE rows). Ensure columns same: aedecod, trt0, trt54, trt81, cvalue1 (cvalue1 may be NA for sub row? Actually sub row also has p-value from fish_sub2; they merge fish_sub2 (which contains only cvalue1) with sum_final_sub2, so sub row also has cvalue1. So we need to add cvalue1 column to sum_final_sub2 with p_sub.

Thus sum_final_sub2 <- sum_final_sub2 %>% mutate(cvalue1 = sprintf("%.4f", p_sub))

Now final_report columns: aedecod, trt0, trt54, trt81, cvalue1.

Now write outputs:

out <- Sys.getenv("OUT_DIR")

write.csv(final_report, file.path(out, "final_report.csv"), row.names=FALSE, na="")

Also write text rendering: We'll create a character vector lines:

First heading lines as in PROC REPORT compute before page:

"Table 7.5 Summary of Serious Adverse Events"
"Safety Population"
"-"
Then the table: we can format using format strings: maybe use sprintf with widths.

Simplify: Use write.table with sep = "\t", row.names=FALSE, col.names=TRUE, file = txt_path.

But need also heading lines before and after. We'll write lines to file manually.

Implementation:

txt_path <- file.path(out, "final_report.txt")
con <- file(txt_path, open="wt")
writeLines(c("Table 7.5 Summary of Serious Adverse Events",
             "Safety Population",
             "-"), con)
write.table(final_report, con, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
writeLines(c("-",
             "Denominator for each % is treatment column N",
             "*b = p-values are from Fisher's Exact Test"), con)
close(con)

Also write CSV of table body (maybe same as final_report.csv). Already done.

Also need to output dataset(s) maybe adae_sl6 etc? The declared output types: table;dataset. The dataset likely final_report. So we have that.

Now ensure packages loaded.

Edge Cases: Treatment codes may not be 0,54,81; but headings use trtcn0 etc. We'll compute trtcn for each code.

When writing final_report, column names should be exactly as in PROC REPORT: aedecod, trt0, trt54, trt81, cvalue1. If a treatment code missing, column may be missing; but we need columns for 0,54,81. We'll ensure they exist: after pivot_wider, we can add missing columns with empty strings.

Implementation: after sum_final2, ensure columns trt0, trt54, trt81 exist: for each code in c(0,54,81) if not present, add mutate(!!paste0("trt",code) := "")

Similarly for sum_final_sub2.

Now final_report.

Now produce script.

