We need to translate the SAS program to R script that reproduces outputs: a table t_cm.rtf (but we output as text and CSV). Must follow rules: write outputs to OUT_DIR env var. Need to produce CSV of final table and a plain-text rendering with same numbers formatting.

We need to replicate steps:

- Read adam.adsl and adam.adcm datasets from given libpaths. The libnames point to directories: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . So files likely .sas7bdat. Use haven::read_sas.

- Compute big N per treatment group (TRTA) where SAFFL='Y' in adsl. Also total big N.

- Build header strings: col1hdr, col2hdr, col3hdr using macro variables: they are set in m_counts macro: col1hdr = trtlbl1~(N=bign1) etc. The macro uses upcase of TRTA values? Actually trtlbl is the value of TRTA variable (e.g., "Placebo", "Drug X Low Dose", "Drug X High Dose") from adsl. So we need to capture those.

- Then create cm table: join adcm with adsl on USUBJID where SAFFL='Y' and CONCOMFL='Y', select fields, order by TRTAN, CMCLAS, CMDECOD. Note: TRTAN is numeric order variable? We'll keep.

- _any: per TRTA count distinct USUBJID, display_term = "Any Concomitant Medication", atc_ord=0, pref_ord=0, rowtype=0? Actually rowtype set to 0? In SAS they set rowtype=0? They set rowtype as 0. In code: 0 as rowtype. So rowtype=0.

- _atc: counts per TRTA, CMCLAS. _atc_tot: total per CMCLAS across all TRTA.

- Merge to get rows with display_term = CMCLAS, atc_ord = cnt_tot, pref_ord=0, rowtype=1.

- _pref: counts per TRTA, CMCLAS, CMDECOD. _pref_tot: total per CMCLAS, CMDECOD.

- _pref_rows: join to get cnt, pref_ord = cnt_tot (total across TRTA), atc_ord = total per CMCLAS (from _atc_tot), display_term = "  " || strip(CMDECOD) (two spaces prefix). rowtype=2.

- Combine _any, _atc_rows, _pref_rows into combined.

- Join with big N dataset (work._bign) to compute pct = cnt / bign * 100.

- Create value string: if cnt>0 then "cnt (pct)" with pct formatted with 1 decimal (5.1 format). else "0". In SAS they used put(cnt,8.) and put(pct,5.1). So we need to format similarly: cnt as integer without leading spaces, pct with one decimal.

- Sort combined3 by descending atc_ord, CMCLAS, rowtype, descending pref_ord, display_term, TRTA.

- Transpose: rows become columns per TRTA. Use pivot_wider: id = TRTA, values = value, names_prefix = "col". The order of columns should be according to treatment order (Placebo, Low Dose, High Dose). In SAS they rename colPlacebo=col1 etc. So we need to map TRTA values to col1, col2, col3 based on order of treatment groups (as in big N order). The macro m_counts orders by TRTA (character). But they also have TRTAN variable for ordering. In final transpose they rename colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3. So we need to replicate that mapping: after pivot, rename columns accordingly.

- Output final table: columns: atc_ord, rowtype, pref_ord, display_term, col1, col2, col3. The atc_ord, rowtype, pref_ord are not displayed (noprint) but still present in dataset. For CSV we include all columns? Probably include displayed columns plus maybe hidden ones? The SAS output only shows display_term and three columns. But we can output all columns for completeness. However spec: write every output dataset as CSV with same variable names. The final dataset is "final". So we output final.csv with all variables.

- Also need plain-text rendering of the table: similar to PROC REPORT output. We need to produce a text file with header rows (titles, footnotes) and the table body formatted. This is complex. But we can approximate: produce a simple text with titles lines then each row: display_term padded? The spec: "contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". So we need to ensure numbers match. Formatting of table may not need exact column widths, just numbers. We'll output a simple pipe-delimited? But they expect plain-text rendering similar to SAS listing. Could just output the table as CSV? But they also require a .txt rendering. We'll produce a simple text where each line is the display_term followed by col1 col2 col3 separated by tabs.

- Also need to output CSV of the table body (maybe same as final.csv). Already done.

- Also need to output CSV of any plotted data? No figures.

- Ensure rounding half away from zero: SAS's default rounding for put with format 5.1 uses round-half-up? Actually SAS round uses round-half-away-from-zero. We'll implement a custom rounding function for pct to one decimal using round_half_up.

Implement round_half_up: function(x, digits) = floor(x * 10^digits + 0.5)/10^digits for positive, for negative need away from zero: floor(x * 10^digits - 0.5)/10^digits? Simpler: use sign.

We'll compute pct as cnt / bign * 100, then format with one decimal using sprintf("%.1f", round_half_up(pct,1)).

- Ensure missing values: treat as NA.

- Sorting: need to sort by atc_ord descending, CMCLAS, rowtype, pref_ord descending, display_term, TRTA. We'll keep.

- For ordering of TRTA columns: we need to know the treatment labels from adsl. We'll get distinct TRTA values sorted by TRTAN (numeric) maybe. In adsl there is TRTAN variable. We'll join to get order.

- Build col mapping: after pivot, rename columns to col1, col2, col3 based on order of TRTA values.

- Write outputs: final dataset CSV: write.csv(final, file.path(out, "final.csv"), row.names=FALSE, na="").

- Text rendering: create a character vector lines: titles from set_titles macro for T_CM. Titles: title4 "Table 14.2.1", title5 "Summary of Concomitant Medications by ATC Class and Preferred Name", title6 "Safety Analysis Set". Also footnotes: footnote4, footnote5. We'll include them.

- Then table header: "ATC Class  Preferred Name" maybe as column header. We'll produce header line: display_term then col1 col2 col3.

- For each row in final sorted as earlier, produce line: display_term padded? We'll just separate by tab.

- Write to .txt.

- Also write CSV of table body: maybe same as final.csv.

- Ensure environment variable OUT_DIR.

Now craft R script.

Potential packages: dplyr, tidyr, readr, haven, stringr.

Implementation steps:

1. out <- Sys.getenv("OUT_DIR"); if empty, stop.

2. Read adsl <- read_sas(file.path("D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam","adsl.sas7bdat")).

3. Read adcm similarly.

4. Compute big N:

bign <- adsl %>% filter(toupper(SAFFL) == "Y") %>% group_by(TRTA) %>% summarise(bign = n_distinct(USUBJID), .groups='drop') %>% arrange(TRTA)

bigntot <- adsl %>% filter(toupper(SAFFL) == "Y") %>% summarise(bign = n_distinct(USUBJID)) %>% pull(bign)

Create col headers:

col_hdrs <- bign %>% mutate(hdr = paste0(TRTA, "~(N=", bign, ")")) %>% arrange(TRTA) # order same as bign

col1hdr <- col_hdrs$hdr[1]; col2hdr <- col_hdrs$hdr[2]; col3hdr <- col_hdrs$hdr[3]; coltothdr <- paste0("Total~(N=", bigntot, ")")

5. Prepare cm:

cm <- adcm %>%
  inner_join(adsl %>% select(USUBJID, SAFFL), by="USUBJID") %>%
  filter(SAFFL == "Y", CONCOMFL == "Y") %>%
  select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD, CMINDC, CMSTDTC, CMENDTC) %>%
  arrange(TRTAN, CMCLAS, CMDECOD)

6. _any:

any_tbl <- cm %>% group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID), .groups='drop') %>%
  mutate(display_term = "Any Concomitant Medication", atc_ord = 0L, pref_ord = 0L, rowtype = 0L)

7. _atc:

atc <- cm %>% group_by(TRTA, CMCLAS) %>% summarise(cnt = n_distinct(USUBJID), .groups='drop')
atc_tot <- cm %>% group_by(CMCLAS) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups='drop')
atc_rows <- atc %>%
  left_join(atc_tot, by="CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0L,
         rowtype = 1L) %>%
  select(TRTA, CMCLAS, cnt, atc_ord, pref_ord, rowtype, display_term)

8. _pref:

pref <- cm %>% group_by(TRTA, CMCLAS, CMDECOD) %>% summarise(cnt = n_distinct(USUBJID), .groups='drop')
pref_tot <- cm %>% group_by(CMCLAS, CMDECOD) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups='drop')
pref_rows <- pref %>%
  left_join(pref_tot, by=c("CMCLAS","CMDECOD")) %>%
  left_join(atc_tot, by="CMCLAS") %>%
  mutate(display_term = paste0("  ", CMDECOD),
         atc_ord = cnt_tot.y, # from atc_tot
         pref_ord = cnt_tot.x,
         rowtype = 2L) %>%
  select(TRTA, CMCLAS, cnt, atc_ord, pref_ord, rowtype, display_term)

Note: after joins, we have cnt (from pref), cnt_tot.x (pref total), cnt_tot.y (atc total). Use appropriate.

9. Combine:

combined <- bind_rows(any_tbl, atc_rows, pref_rows)

10. Join with bign:

combined2 <- combined %>%
  left_join(bign, by="TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

Define round_half_up function:

round_half_up <- function(x, digits=0) {
  pos <- x >= 0
  mult <- 10^digits
  res <- ifelse(pos,
                floor(x * mult + 0.5) / mult,
                ceiling(x * mult - 0.5) / mult)
  return(res)
}

Compute formatted value:

combined3 <- combined2 %>%
  mutate(pct_fmt = ifelse(!is.na(pct), sprintf("%.1f", round_half_up(pct,1)), "")) %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(cnt, " (", pct_fmt, ")"),
                        "0"))

11. Sort:

combined3 <- combined3 %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA)

12. Remove duplicates as per SAS: they sort and then proc sort nodupkey by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA. We'll apply distinct after sorting:

combined3 <- combined3 %>% distinct(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, .keep_all = TRUE)

13. Transpose: pivot_wider.

final_wide <- combined3 %>%
  select(TRTA, display_term, value, atc_ord, rowtype, pref_ord) %>%
  pivot_wider(names_from = TRTA, values_from = value, names_prefix = "col") %>%
  # ensure columns order as per treatment order
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term)

But we need to keep CMCLAS column for ordering but not in final output? In SAS final dataset after transpose includes atc_ord, rowtype, pref_ord, display_term, col1-col3. It also retains CMCLAS? Not in rename. The PROC TRANSPOSE with BY includes CMCLAS as a BY variable, so it will be kept in output. In code they didn't drop it. So final dataset includes CMCLAS column. We'll keep it.

Thus after pivot_wider, we have columns: atc_ord, CMCLAS, rowtype, pref_ord, display_term, col<TRTA values>. Then rename columns to col1, col2, col3 based on order of treatment labels.

We need mapping: order of TRTA values as per bign sorted by TRTA (or maybe TRTAN). We'll use bign order as in col_hdrs.

trt_order <- bign$TRTA

Then rename:

final <- final_wide %>%
  rename(col1 = !!sym(paste0("col", trt_order[1])),
         col2 = !!sym(paste0("col", trt_order[2])),
         col3 = !!sym(paste0("col", trt_order[3]))) %>%
  select(atc_ord, rowtype, pref_ord, display_term, col1, col2, col3)

If any missing columns, fill with NA.

14. Output CSV:

write.csv(final, file.path(out, "final.csv"), row.names=FALSE, na="")

15. Text rendering:

lines <- c()
# titles
lines <- c(lines, "Table 14.2.1")
lines <- c(lines, "Summary of Concomitant Medications by ATC Class and Preferred Name")
lines <- c(lines, "Safety Analysis Set")
lines <- c(lines, "") # spacer
# header
lines <- c(lines, sprintf("%-40s %10s %10s %10s", "ATC Class  Preferred Name", col1hdr, col2hdr, col3hdr))
# rows
for(i in seq_len(nrow(final))) {
  row <- final[i,]
  lines <- c(lines, sprintf("%-40s %10s %10s %10s",
                            row$display_term,
                            ifelse(is.na(row$col1), "", row$col1),
                            ifelse(is.na(row$col2), "", row$col2),
                            ifelse(is.na(row$col3), "", row$col3)))
}
# footnotes
lines <- c(lines, "")
lines <- c(lines, "WHO Drug Dictionary [Version]")
lines <- c(lines, "Subjects counted once per ATC class and once per preferred name within ATC class.")

writeLines(lines, file.path(out, "final.txt"))

Also write CSV of table body: maybe same as final.csv.

All done.

Edge cases: need to ensure column names in pivot_wider: colPlacebo etc. The names will be "colPlacebo" etc. So rename accordingly.

Implement using dplyr and tidyr.

Now produce final R script.