We need to translate the SAS program to R, reproducing outputs: a table t_ae_sum.rtf (but we output as text and CSV). The script must read ADaM datasets: adam.adsl and adam.adae from the given lib paths. The libref adam points to D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam. So files likely are .sas7bdat. We'll use haven::read_sas.

We need to compute big N per treatment group (TRTA) for safety set (SAFFL='Y').

Then categories as defined.

We need to produce final table with rows ordered by ord then TRTA columns.

Columns: col1 for Placebo, col2 for Drug_X_Low_Dose, col3 for Drug_X_High_Dose (based on treatment labels). The macro m_counts also creates macro variables col1hdr etc. In SAS they use &col1hdr. etc. The macro m_counts builds col&i.hdr = &&trtlbl&i.~(N=&&bign&i.); where trtlbl is the treatment label (value of TRTA). So col1hdr is something like "Placebo~(N=XX)". The tilde ~ is used as split char in PROC REPORT to break line. In R we can just keep as "Placebo (N=XX)" maybe. But need to match SAS output formatting: they used split='~' so header shows "Placebo (N=XX)" with line break. For our text rendering we can output the header string as "Placebo (N=XX)". We'll compute.

We need to output a text rendering of the table: each row: category and values per treatment.

The SAS code: after transpose, they rename columns colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose to col1, col2, col3. So final dataset 'report' has columns: ord, category, col1, col2, col3. They also replace missing with '0'.

The value column is formatted as "cnt (pct)" with pct one decimal (5.1 format). If cnt=0 then value='0'.

Thus final table rows: categories (including "Subjects with no TEAE", "Subjects with at least one TEAE", etc). The order ord 0-8.

We need to produce CSV: write.csv(report, file.path(out, "t_ae_sum.csv")).

Also produce text rendering: maybe using write.table with sep="\t" and no row names. But need to include header row with column names: category, col1, col2, col3. Also include the header strings? In SAS PROC REPORT they define column headers using col1hdr etc. So the output table includes column headers as those strings. We'll produce a text file where first line is something like "ord category col1 col2 col3" maybe not needed. But spec: "plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". So we need a text file with the table body (maybe with headers). We'll include header row with category and the three columns.

Implementation steps:

1. Set out <- Sys.getenv("OUT_DIR"); if empty, maybe default to current dir.

2. Load libraries.

3. Read adsl and adae.

4. Compute big N:

bign_df <- adsl %>% filter(toupper(SAFFL) == "Y") %>% group_by(TRTA) %>% summarise(bign = n_distinct(USUBJID)) %>% arrange(TRTA)

Also total N: bigntot = n_distinct(USUBJID) for SAFFL='Y'.

5. Create header strings: for each treatment in order of TRTA (as in bign_df). We'll need to map to col1, col2, col3 based on order of treatment groups (assuming there are exactly 3 groups). The macro m_counts also sets ntrt macro variable to number of groups (3). The order is order by TRTA (character). So we need to sort TRTA alphabetically (uppercase before lowercase). We'll keep as is.

Assume TRTA values: "Placebo", "Drug_X_Low_Dose", "Drug_X_High_Dose". We'll map accordingly.

col_hdrs <- paste0(bign_df$TRTA, " (N=", bign_df$bign, ")") . Use split char maybe "~". We'll use "~" to mimic.

col1hdr <- col_hdrs[1]; col2hdr <- col_hdrs[2]; col3hdr <- col_hdrs[3]; total header not needed.

6. Category calculations:

Define a function to compute counts per TRTA for a given where clause.

We'll join adae with adsl to filter safety set and TEAE flag.

Base data: adae_teae <- adae %>% inner_join(adsl %>% filter(SAFFL == "Y"), by="USUBJID") %>% filter(TRTEMFL == "Y")

Now categories:

- Any TEAE: no extra filter.

- Related TEAE: AEREL in c('POSSIBLE','PROBABLE','DEFINITE','RELATED')

- Grade >=3 TEAE: as.numeric(AETOXGR) >=3 (AETOXGR may be character; use as.numeric after suppress warnings). Use input best. in SAS which reads numeric from character.

- Serious AE: AESER == 'Y'

- Related SAE: AESER == 'Y' and AEREL in list.

- AE leading to discontinuation: AEACN == 'DRUG WITHDRAWN'

- AE leading to dose modification: AEACN in c('DOSE REDUCED','DRUG INTERRUPTED','DOSE INCREASED')

- AE leading to death: AEOUT == 'FATAL'

For each, compute count distinct USUBJID per TRTA.

Also category 0: subjects with no TEAE: those in adsl SAFFL='Y' and not in adae where TRTEMFL='Y'.

Implementation:

ae0 <- adsl %>% filter(SAFFL == "Y") %>% 
   left_join(adae %>% filter(TRTEMFL == "Y") %>% distinct(USUBJID), by="USUBJID") %>%
   filter(is.na(USUBJID.y)) %>% group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID)) %>% mutate(category = "Subjects with no TEAE", ord = 0)

For other categories, we can define a list of list with label, filter expression, ord.

We'll create a function that takes filter expression (as a quosure) and returns df.

Simpler: for each category, filter adae_teae accordingly, then group_by(TRTA) summarise(cnt=n_distinct(USUBJID)).

We'll store in list and bind rows.

Category definitions:

list(
  list(ord=1, label="Subjects with at least one TEAE", filter=TRUE),
  list(ord=2, label="  Related TEAE", filter= AEREL %in% c('POSSIBLE','PROBABLE','DEFINITE','RELATED')),
  list(ord=3, label="  Grade >=3 TEAE", filter= as.numeric(AETOXGR) >=3),
  list(ord=4, label="  Serious Adverse Event", filter= AESER == 'Y'),
  list(ord=5, label="  Related Serious Adverse Event", filter= AESER == 'Y' & AEREL %in% c('POSSIBLE','PROBABLE','DEFINITE','RELATED')),
  list(ord=6, label="  TEAE Leading to Study Drug Discontinuation", filter= AEACN == 'DRUG WITHDRAWN'),
  list(ord=7, label="  TEAE Leading to Dose Modification", filter= AEACN %in% c('DOSE REDUCED','DRUG INTERRUPTED','DOSE INCREASED')),
  list(ord=8, label="  TEAE with Outcome of Death", filter= AEOUT == 'FATAL')
)

Note: label strings have leading spaces in SAS to indent; we keep them.

For each, compute counts.

After obtaining each df with columns TRTA, cnt, category, ord.

Combine with ae0.

7. Merge with big N to compute pct.

all_ae2 <- all_ae %>% left_join(bign_df, by="TRTA") %>% mutate(pct = ifelse(bign>0, cnt / bign * 100, NA_real_))

8. Create value column: if cnt>0 then paste0(cnt, " (", format(round_half_up(pct,1), nsmall=1), ")") else "0". Need round-half-up (SAS round). R's round uses round to even? Actually round uses "round to even" for .5. SAS uses round half away from zero. We'll implement custom function round_half_up(x, digits) = floor(x * 10^digits + 0.5)/10^digits for positive numbers, and ceiling for negative? Use sign.

Implement:

round_half_up <- function(x, digits=0) {
  pos <- x >= 0
  mult <- 10^digits
  res <- ifelse(pos, floor(x*mult + 0.5)/mult, ceiling(x*mult - 0.5)/mult)
  return(res)
}

Then format with sprintf with one decimal.

pct_formatted <- ifelse(is.na(pct), "", sprintf("%.1f", round_half_up(pct,1)))

value <- ifelse(cnt>0, paste0(cnt, " (", pct_formatted, ")"), "0")

9. Transpose: we need rows per category (ordered by ord) and columns per TRTA.

We can pivot_wider:

report <- all_ae3 %>% select(ord, category, TRTA, value) %>% pivot_wider(names_from=TRTA, values_from=value, values_fill = list(value="0"))

Now column names will be treatment values. Need to rename to col1, col2, col3 in order of bign_df$TRTA.

col_order <- bign_df$TRTA

report <- report %>% select(ord, category, all_of(col_order))

Rename columns:

new_names <- c("ord","category", "col1","col2","col3")
names(report) <- new_names

Also ensure missing replaced with "0" already.

10. Output:

write.csv(report, file.path(out, "t_ae_sum.csv"), row.names=FALSE, na="")

For text rendering: we can write.table with sep="\t", quote=FALSE, row.names=FALSE.

write.table(report, file.path(out, "t_ae_sum.txt"), sep="\t", quote=FALSE, row.names=FALSE)

Also maybe include header strings as column names: col1, col2, col3 currently. We could replace column names with col1hdr etc.

But spec: "plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". So column names may not be needed. We'll keep.

Also need to output the header strings maybe as separate lines? Not required.

11. No figures.

12. Clean up.

Now produce final R script inside ```r``` block.

Make sure to load packages.

Implement round_half_up.

Edge Cases: AETOXGR may be character with missing; as.numeric will give NA; filter condition should handle NA.

Implementation details: Use dplyr, tidyr.

Now craft script.

