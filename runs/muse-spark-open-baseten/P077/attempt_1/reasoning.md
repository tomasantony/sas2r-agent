Need to translate SAS program P077_l_death.sas to R.

We need to read inputs from absolute paths given. Librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam ; sdtm similarly. But also macro variables rawadam etc. The program uses adam.adsl, adam.adae.

We need to locate files. The libref points to directory. In SAS, libname adam "path" access=readonly; That directory likely contains .sas7bdat files. Need to check what files exist. We don't have filesystem listing, but we can attempt to handle both .sas7bdat and .xpt.

We need to produce outputs under OUT_DIR: l_death.rtf text rendering and CSV.

Expected outputs: l_death.rtf (table/listing text, 46 numbers) -> write text rendering and CSV of same table.

We need to reproduce SAS logic:

Step1: deaths = select from adsl where DTHFL='Y', compute TRT_START=TRTSDT, TRT_END=TRTEDT, DEATH_DATE=DTHDT, CAUSE_OF_DEATH=DTHCAUS, DAYS_LASTDOSE_DEATH = DTHDT - TRTEDT if both nonmissing, DAYS_FIRSTDOSE_DEATH = DTHDT - TRTSDT +1.

Order by TRTA, USUBJID.

Step2: death_ae = distinct USUBJID, AEBODSYS as FATAL_AE_SOC, AEDECOD as FATAL_AE_PT, AEREL as FATAL_AE_REL from adae where AEOUT='FATAL' order by USUBJID.

Then left join.

Step3: death_final: demog = catx('/', strip(put(AGE,best.)), strip(SEX), strip(substr(RACE,1,1))) . So age as best format (default numeric), sex, first letter of race. days_display = if DAYS_LASTDOSE_DEATH ne . then strip(put(...,8.)) else 'N/A'.

Step4: proc report with columns.

We need to produce CSV and TXT.

We need to handle dates formatting: In SAS, format date9. e.g., 01JAN2020. But output CSV should have dates as YYYY-MM-DD per rules. However text rendering should show same as SAS? The rules say write each output dataset as CSV with dates as YYYY-MM-DD. And each table/listing additionally as plain-text rendering that contains same numbers SAS output shows (counts, percentages...). For listing, we need to render listing.

We need to produce l_death.csv and l_death.txt (and maybe l_death.rtf? But spec says write text rendering and CSV. Expected outputs says l_death.rtf -> write text rendering and CSV. So we should create l_death.csv and l_death.txt (and maybe also .rtf? But requirement says each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")). So we will create l_death.txt and l_death.csv. Possibly also create .rtf placeholder? But spec says write every output into OUT_DIR: each output dataset as CSV, each table/listing additionally as plain-text rendering and as CSV of table body. So for listing, the dataset is death_final. So we should output death_final.csv? But expected name is l_death. So we will output l_death.csv as the table body.

Also need to handle treatment labels? TRTA values are like "Placebo", "Xanomeline Low Dose", etc. Keep as is.

We need to implement reading.

We need to produce script that runs non-interactively.

We need to handle OUT_DIR env var.

We need to produce text file with titles/footnotes similar to SAS.

Titles for L_DEATH:
title1 sponsor + Page
title2 Protocol
title4 Listing 16.2.7.3
title5 Listing of Deaths
title6 All Subjects
footnotes: Data cutoff, Source, Program.

We need to get sponsor, protocol, cutdate from u_setup: they are placeholders [STUDYID] etc. So we can just use those strings.

We need to format dates for display in text: SAS date9. format: DDMMMYYYY like 01JAN2020. We should replicate for text rendering. For CSV, use YYYY-MM-DD.

We need to handle demog, days_display.

We need to ensure ordering.

We need to handle missing values.

We need to produce 46 numbers in text? Probably count of cells.

We need to write R script.

We need to handle file paths with forward slashes.

We need to implement helper to read adam datasets: try to list files in adam path.

We should use haven::read_sas and haven::read_xpt.

We need to handle haven labelled vectors: convert to character/numeric.

We need to produce final CSV with columns: TRTA, USUBJID, demog, TRT_START, TRT_END, DEATH_DATE, days_display, CAUSE_OF_DEATH, FATAL_AE_SOC, FATAL_AE_PT, FATAL_AE_REL

But spec says write.csv with SAS dataset variable names. So we should keep names as in death_final.

We need to ensure dates as YYYY-MM-DD in CSV.

We need to produce text rendering: use fixed-width table.

We should also produce PNG? No, listing only.

We need to ensure script creates OUT_DIR if not exists.

We need to handle haven date conversion: SAS dates are numeric days since 1960-01-01. haven::read_sas should convert to Date automatically if format attached? Might need to handle.

We need to implement robust reading.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

- sdtm_path similarly.

- function read_adam(name): try file.path(adam_path, paste0(name, ".sas7bdat")), if exists read_sas, else try .xpt, else list files.

- Also handle lower case.

- Read adsl, adae.

- Convert labelled to appropriate.

- Ensure column names uppercase as SAS.

- Implement deaths logic.

- Need to handle DTHFL, TRTA, etc.

- Compute DAYS.

- death_ae distinct.

- left join.

- demog: paste AGE, SEX, substr(RACE,1,1) with "/" . AGE formatting: SAS put(AGE,best.) -> best. default 12. So for integer age, it will be "25" etc. Use as.character(AGE) without trailing .0. If AGE is numeric, format without scientific. Use format(AGE, trim=TRUE, scientific=FALSE). For integer, that works.

- days_display: if not NA then as.character(DAYS_LASTDOSE_DEATH) trimmed, else "N/A". SAS put with 8. format right aligned but strip removes spaces. So just integer string.

- For text rendering, format dates as date9.: format(Date, "%d%b%Y") uppercase month.

- Need to handle TRT_START etc as Date.

- Create display dataframe for text.

- Write CSV: need to convert Date columns to YYYY-MM-DD string, with NA as "".

- Write TXT: include titles, column headers, rows.

We need to ensure 46 numbers? Might be count of data cells that are numeric? But we just need to produce plausible text.

We should also ensure script handles missing adam files: fallback to rawadam path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam

But spec says librefs point to own-synthetic. So use that.

We need to produce final script inside ```r block.

We must not call install.packages.

We need to use only allowed packages.

We need to ensure script runs with Rscript.

We need to add helper for SAS date conversion if needed: if column is numeric and looks like SAS date (e.g., > 10000), convert via as.Date(num, origin="1960-01-01").

But haven should handle.

We need to handle haven labelled: use haven::as_factor? Better to convert labelled to character via haven::zap_labels? Use dplyr mutate across where haven::is.labelled -> as.character or as.numeric.

Simplify: after reading, use haven::zap_formats, haven::zap_labels? But zap_labels converts labelled to numeric. For character labelled, we want character. So we can use haven::zap_labels then for character columns that were labelled, they remain numeric? Actually haven labelled character is still character with labels attribute. zap_labels will keep values. So we can just strip attributes: for each column, if inherits "haven_labelled", convert via as.character if type character else as.numeric.

Simpler: use labelled::unlabelled? But not allowed. We'll manually.

Implement function clean_haven(df): for each col, if haven::is.labelled(col) then col <- haven::zap_labels(col); then remove attributes.

Also need to handle Date/POSIX.

We need to ensure column names uppercase.

Proceed to write script.

We need to include library calls.

We need to create out dir.

We need to write CSV and TXT.

We need to ensure TXT contains same numbers as SAS output shows.

We should produce a simple fixed-width table using format.

We should include footnotes.

We should also produce l_death.rtf? The expected output is l_death.rtf but we are to write txt and csv. The spec says write text rendering and CSV. So we will create l_death.txt and l_death.csv. To satisfy expected, maybe also create l_death.rtf as copy of txt? But we can create both .txt and .rtf with same content. Safer to create both.

The instruction: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers..." So we must create .txt. The expected outputs list says l_death.rtf -> write text rendering and CSV. So we will create l_death.txt and l_death.csv, and also l_death.rtf as text.

We should do that.

Let's craft final R script.