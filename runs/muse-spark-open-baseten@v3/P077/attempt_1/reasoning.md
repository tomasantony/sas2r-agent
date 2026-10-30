Need to translate SAS program P077_l_death.sas to R.

We need to read inputs from adam and sdtm librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam ; sdtm similarly. But program uses adam.adsl, adam.adae. Need to locate files. Likely .sas7bdat or .xpt. Check available. We need to write R script that reads from absolute paths given in context (forward slashes). The macro variables: &root. etc. But librefs point to adam and sdtm directories. So we need to read those.

We need to produce outputs under OUT_DIR: l_death.rtf (table/listing text, 46 numbers) -> write text rendering and CSV of same table. Expected outputs: l_death.rtf -> write text rendering and CSV.

We need to reproduce SAS semantics: identify deaths where DTHFL='Y', compute TRT_START = TRTSDT, TRT_END = TRTEDT, DEATH_DATE = DTHDT, DAYS_LASTDOSE_DEATH = DTHDT - TRTEDT, DAYS_FIRSTDOSE_DEATH = DTHDT - TRTSDT +1. Then add fatal AE info where AEOUT='FATAL', distinct USUBJID, AEBODSYS, AEDECOD, AEREL. Left join.

Then format display: demog = catx('/', age, sex, substr(race,1,1)), days_display = if not missing then put(DAYS_LASTDOSE_DEATH, 8.) else 'N/A'.

Then proc report with columns.

We need to produce CSV and TXT rendering.

We need to handle dates: In ADSL, TRTSDT, TRTEDT, DTHDT are numeric SAS dates. When reading via haven::read_sas or read_xpt, they will be Date objects? Need to handle.

We need to explore file system to know what files exist. But we cannot run? We need to infer. We must write R script that robustly reads either .sas7bdat or .xpt.

Also need to handle treatment ordering: order by TRTA, USUBJID. TRTA values: Placebo, [Drug] Low Dose, [Drug] High Dose etc. But we need to preserve SAS sort order (character ordering uppercase before lowercase). R default sort similar.

We need to produce text rendering that contains same numbers SAS output shows (counts, percentages...). For listing, need to show all rows.

46 numbers: likely count of cells? Need to ensure output CSV has correct columns and rows.

We need to implement titles/footnotes as in u_titles.sas for L_DEATH:

title1 sponsor + Page ^{pageof}
title2 Protocol: &protocol.
title3 spacer
title4 Listing 16.2.7.3
title5 Listing of Deaths
title6 All Subjects
footnote1 Data cutoff: &cutdate.
footnote2 Source: ADaM datasets
footnote3 Program: &sysin. | Run: date time

But sponsor, protocol, cutdate are placeholders [STUDYID] etc. In SAS they are macro variables set to "[STUDYID]" etc. So titles will contain those literal strings. We should reproduce verbatim.

We need to write R script that writes CSV and TXT.

We need to handle OUT_DIR env var.

We need to ensure script runs non-interactively.

We need to include helper for SAS date handling, etc.

We need to produce l_death.csv and l_death.txt (and maybe l_death.rtf? But spec says write text rendering and CSV. Expected outputs l_death.rtf -> write text rendering and CSV of same table. So we should write l_death.csv and l_death.txt, maybe also .rtf? But requirement says each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers. So we need to produce l_death.txt and l_death.csv. Possibly also produce .rtf? But we can produce .rtf as well via text.

Simpler: produce l_death.csv and l_death.txt.

We need to ensure numeric formatting: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS.

We need to handle missing values.

We need to implement reading logic: list files in adam directory.

We should write R code that checks for .sas7bdat and .xpt.

We need to handle TRTA ordering: SAS order by TRTA, USUBJID. So we sort.

We need to handle demog: catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE,1,1))). In SAS, put(AGE, best.) gives default best format (maybe 12.). For integer ages, it will be like "35". So we can just as.character(AGE) without decimals. Need to handle missing.

We need to handle days_display: strip(put(DAYS_LASTDOSE_DEATH, 8.)). SAS 8. format right-aligned width 8, but strip removes spaces. So just as.character integer.

We need to handle TRT_START, TRT_END, DEATH_DATE display as date9. format: e.g., 01JAN2020. But for CSV we should write ISO dates YYYY-MM-DD. For TXT rendering, we should show date9. style? The SAS listing shows dates with date9. format. We should reproduce that in TXT? The spec says write ISO dates for CSV, but TXT should contain same numbers SAS output shows. For dates, SAS shows date9. So TXT should show date9. format. But spec says dates as YYYY-MM-DD for CSV. So we need to do both: CSV ISO, TXT date9.

We need to decide.

We also need to handle treatment labels: TRTA values already contain labels.

We need to produce final dataset death_final with columns: TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE, days_display, CAUSE_OF_DEATH, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL, plus maybe DAYS_LASTDOSE_DEATH etc. But for output we need those 11 columns.

We need to ensure we handle case where no deaths? But there are deaths.

We need to write R script that does all steps.

We need to include library loading.

We need to handle OUT_DIR creation.

We need to produce text file with titles, column headers, rows.

We need to ensure 46 numbers: maybe total numbers in listing? We need to ensure we output all rows.

We should implement robust reading.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
- sdtm_path similarly.

- helper function read_sas_dataset: look for file with name lowercased: adsl.sas7bdat, adsl.xpt, etc. Use haven::read_sas or read_xpt.

- Need to handle haven reading: haven::read_sas returns tibble with labelled vectors. Need to convert to data.frame and handle dates.

- For ADSL, variables: USUBJID, TRTA, AGE, SEX, RACE, TRTSDT, TRTEDT, DTHDT, DTHCAUS, DTHFL.

- For ADAE: USUBJID, AEBODSYS, AEDECOD, AEREL, AEOUT.

- Need to ensure we handle case where TRTSDT etc are numeric SAS dates (days since 1960-01-01). haven should convert to Date automatically if format is date9. Let's check: haven::read_sas should preserve Date class.

- If not, we need to convert numeric to Date: as.Date(x, origin="1960-01-01").

- Then create deaths: filter DTHFL == 'Y'.

- Compute TRT_START = TRTSDT, TRT_END = TRTEDT, DEATH_DATE = DTHDT, DAYS_LASTDOSE_DEATH = as.numeric(DTHDT - TRTEDT), DAYS_FIRSTDOSE_DEATH = as.numeric(DTHDT - TRTSDT + 1).

- Order by TRTA, USUBJID.

- Create death_ae: filter AEOUT == 'FATAL', select distinct USUBJID, FATAL_AE_SOC = AEBODSYS, FATAL_AE_PT = AEDECOD, FATAL_AE_REL = AEREL, order by USUBJID, distinct.

- Left join.

- Create death_final: demog = paste(AGE, SEX, substr(RACE,1,1), sep="/") but need to handle missing: if AGE missing then? Use ifelse. Use trimws.

- days_display = ifelse(!is.na(DAYS_LASTDOSE_DEATH), as.character(DAYS_LASTDOSE_DEATH), "N/A")

- Need to handle AGE formatting: SAS best. gives no trailing .0. So use formatC? For integer, as.character(as.integer(AGE))? But AGE may be numeric with decimals? Use trimws(format(AGE, trim=TRUE, scientific=FALSE)).

- Then sort again.

- For CSV output: need to write with ISO dates: TRT_START, TRT_END, DEATH_DATE as YYYY-MM-DD. Use format(..., "%Y-%m-%d").

- For TXT: need to render table with titles/footnotes and column headers.

We should create a function to format date9 for display: format(date, "%d%b%Y") uppercase month: e.g., 01JAN2020. Use toupper(format(..., "%d%b%Y")).

- Then produce TXT file: include titles lines, then header row with column names, then separator, then rows.

- Also need to ensure we output 46 numbers: maybe count of numeric values in listing? We need to ensure TXT contains numbers.

- Write CSV: write.csv(df, file.path(out, "l_death.csv"), row.names=FALSE, na="")

- Write TXT: writeLines to file.path(out, "l_death.txt")

- Also maybe write RTF? But spec says l_death.rtf -> write text rendering and CSV. We can also write l_death.rtf as same as txt but with .rtf extension? Safer to write both .txt and .rtf.

We should also handle directory creation.

We need to ensure script is standalone.

We need to include required libraries: haven, dplyr, etc.

We need to avoid install.packages.

We need to ensure script runs with Rscript.

We should add stopifnot checks.

We need to handle case where adam files are .xpt: need to list.

Let's craft final R code block.

