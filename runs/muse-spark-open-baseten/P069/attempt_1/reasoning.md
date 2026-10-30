Need to translate SAS program to R. Must read adam datasets from D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam . Likely .sas7bdat files. Use haven::read_sas.

Steps:

- Get Big N: from adsl where SAFFL='Y' group by TRTA. Need to handle TRTA values: Placebo, [Drug] Low Dose, [Drug] High Dose . But SAS uses TRTA variable. Need to check actual values. Probably "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose" etc. But we need to reproduce column headers.

- AE categories: 9 categories (ord 0-8). Need to count distinct USUBJID per TRTA where conditions.

Conditions:
0: Subjects with no TEAE : from adsl SAFFL=Y left join distinct USUBJID from adae where TRTEMFL=Y, where a.USUBJID is null
1: Subjects with at least one TEAE : b.SAFFL=Y and a.TRTEMFL=Y
2: Related TEAE : AEREL in ('POSSIBLE','PROBABLE','DEFINITE','RELATED')
3: Grade >=3 TEAE : input(a.AETOXGR, best.) >=3 . AETOXGR is character numeric grade. Need to convert to numeric.
4: Serious AE : AESER='Y'
5: Related SAE : AESER='Y' and AEREL in ...
6: AE leading to discontinuation : AEACN='DRUG WITHDRAWN'
7: AE leading to dose modification : AEACN in ('DOSE REDUCED','DRUG INTERRUPTED','DOSE INCREASED')
8: AE leading to death : AEOUT='FATAL'

Note: All categories also require TRTEMFL='Y' and SAFFL='Y' via join.

- Then calculate pct = cnt / bign *100

- Format value = cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') if cnt>0 else '0'. Note SAS put(pct,5.1) will produce like " 34.5" but strip removes spaces. So format is "cnt (pct)" with one decimal. Need to replicate rounding half away? SAS rounds half away? Use round half up.

- Transpose: by ord category, id TRTA, var value. Need to produce col1 col2 col3 corresponding to Placebo, Low Dose, High Dose. The SAS transpose uses prefix=col and id TRTA, so column names become colPlacebo etc. Then rename to col1 col2 col3. In R we need to pivot_wider.

- Ensure zero rows display: empty -> '0'

- Generate output: report dataset with ord, category, col1 col2 col3. Need to write CSV and txt.

- Also need to handle Big N headers: col1hdr etc. In SAS m_counts creates macro variables col1hdr = "Placebo (N=xx)" etc. Need to replicate for txt header.

- Need to produce t_ae_sum.rtf equivalent: txt rendering and CSV.

We must inspect actual data to know TRTA values and counts.

We need to write R script that reads adam.adsl and adam.adae.

Paths: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

List files.

We need to implement round half up: function round_half_up(x, digits).

Also need to handle missing values.

We need to produce OUT_DIR outputs: t_ae_sum.csv and t_ae_sum.txt plus maybe also dataset CSV.

The expected output types: t_ae_sum.rtf (table/listing text, 46 numbers) -> write text rendering and CSV of same table.

So we need to create file.path(out, "t_ae_sum.csv") and file.path(out, "t_ae_sum.txt")

The CSV should contain the table body: ord, category, col1, col2, col3 maybe with headers including N.

The txt should contain formatted table with titles/footnotes.

We should replicate titles from u_titles.sas for T_AE_SUM:

Title4: Table 14.3.1
Title5: Overall Summary of Treatment-Emergent Adverse Events
Title6: Safety Analysis Set

Footnotes:
Data cutoff: [DDMONYYYY] (placeholder)
Source: ADaM datasets
Program: ... etc
Footnote4: TEAE = Treatment-Emergent Adverse Event; SAE = Serious Adverse Event
Footnote5: Subjects are counted once per category.

We need to produce txt that contains numbers.

We must ensure script runs non-interactively.

We need to handle reading .sas7bdat or .xpt. Use haven::read_sas for .sas7bdat, haven::read_xpt for .xpt.

We should list files in adam directory.

We need to implement logic for ae_category.

We also need to handle case sensitivity: SAFFL='Y', TRTEMFL='Y', etc. Use upcase comparison? SAS where b.SAFFL='Y' is case-sensitive. But we should do exact match.

We need to ensure distinct counts.

We need to produce final report sorted by ord.

We need to handle missing TRTA groups: ensure all TRTA present even if cnt=0.

We need to compute bign per TRTA.

We need to handle transpose: ensure columns ordered as Placebo, Low Dose, High Dose. The SAS order by TRTA (character ordering). Need to check ordering: Placebo < [Drug] High Dose < [Drug] Low Dose? Actually alphabetical: "Placebo" (P), "Xanomeline High Dose" (X... H), "Xanomeline Low Dose" (X... L). So High before Low. But SAS m_counts orders by TRTA, so col1 = Placebo, col2 = High? But the transpose rename in patched code: rename=(colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3) suggests they expect col1=Placebo, col2=Low, col3=High. That implies TRTA values are "Placebo", "Drug X Low Dose", "Drug X High Dose" maybe with different naming that sorts differently? Let's inspect actual data to determine.

We need to write R code that dynamically determines TRTA levels sorted as in SAS (character order) but then maps to col1 col2 col3 as per SAS rename patch. However the patch explicitly renames colPlacebo, colDrug_X_Low_Dose, colDrug_X_High_Dose. That suggests TRTA values after SAS name normalization become those. In R we can just pivot and then order columns by desired order: Placebo, Low Dose, High Dose.

Simpler: Determine unique TRTA values sorted, then assign col1..col3 in that sorted order, but also ensure header matches.

Better to sort TRTA levels by the order they appear in _bign (which is order by TRTA). Then col1 is first sorted TRTA, etc. But the report expects col1=Placebo, col2=Low, col3=High. If sorted order is Placebo, High, Low, then col2 would be High, not Low. So patch would be wrong. Maybe actual TRTA values are "Placebo", "Low Dose", "High Dose" without drug name, so alphabetical order is High, Low, Placebo? Let's check.

We need to inspect data.

We cannot run code here but we can anticipate. We can write R code that handles generically: get bign dataframe ordered by TRTA (character). Then create headers accordingly. Then when pivoting, ensure columns are ordered by that same order, and rename to col1, col2, col3 in that order. That will match SAS behavior regardless of actual labels.

For txt header, we need to display "Placebo (N=xx)" etc.

We should implement reading and then printing.

We need to produce CSV with columns: ord, category, col1, col2, col3 but with header row containing N? In SAS report, column headers are like "Placebo (N=86)". In CSV we can include those as column names or as separate header row. Simpler: CSV column names are category, Placebo (N=...), etc.

We need to produce both.

We should also produce a dataset CSV for report.

We need to ensure OUT_DIR exists.

We need to write helper functions.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "."
- adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
- list files
- read adsl, adae using haven::read_sas if .sas7bdat exists else read_xpt
- Convert to data.frame, handle haven labelled.

- Clean: convert all character columns to character, trim.

- Compute bign: filter SAFFL == "Y" (case-sensitive? Use toupper? SAS where upcase(SAFFL)="%upcase(&popval.)" so case-insensitive. So filter toupper(SAFFL)=="Y")

- Group by TRTA, count distinct USUBJID.

- Order by TRTA (character, SAS ordering: uppercase before lowercase, but R default is similar? Use sort)

- Create headers.

- Define function ae_category(where_filter, label, ord)

But we can implement each category via filtering.

We need to join adae with adsl to get TRTA? In SAS, they select a.TRTA from adae, inner join adsl on USUBJID where b.SAFFL='Y' and a.TRTEMFL='Y' ... So TRTA comes from adae. But adsl also has TRTA. Should be same. Use adae$TRTA.

We need to ensure we only count subjects where adsl SAFFL=Y and adae TRTEMFL=Y.

For each category, filter adae accordingly, then join to adsl to ensure SAFFL=Y, then count distinct USUBJID per TRTA.

For "no TEAE": left join distinct TEAE subjects.

Implement.

- Combine all_ae.

- Left join bign to get pct.

- Format value.

- Pivot wider.

- Ensure zero rows: fill missing with "0"

- Sort by ord.

- Generate txt.

We need to handle AETOXGR conversion: as.numeric(AETOXGR) >=3 . Note SAS input(..., best.) will convert character to numeric, missing if not numeric.

- AEREL values: check case.

- AEACN, AESER, AEOUT.

We need to produce final report dataframe.

We need to write CSV and txt.

We should also handle rounding: pct formatted with 1 decimal using round_half_up.

Implement round_half_up.

We need to ensure we produce 46 numbers: probably counts and percentages.

We need to write txt with table borders.

We should include titles and footnotes.

Let's craft final R script.