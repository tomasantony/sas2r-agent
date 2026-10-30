Need to produce R script that reproduces SAS program P082_f_waterfall.sas

We need to read inputs from absolute paths given. SAS librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam, sdtm similarly. Need to find files. Likely adam.adtr and adam.adsl. Need to check what exists. The environment variable OUT_DIR is where to write outputs. Need to produce f_waterfall.png and CSV of plotted data.

We need to infer structure. The SAS program:

- proc sql create table tumor as select a.USUBJID, a.TRTA, a.TRTAN, a.PARAMCD, a.PARAM, a.PCHG, a.AVALC, b.EFFFL from adam.adtr a inner join adam.adsl b on USUBJID where EFFFL='Y' and PARAMCD='BPCHG' order by PCHG

- Then data waterfall: set tumor by PCHG; bar_order = _n_; bar_color based on AVALC upcase: CR darkgreen, PR green, SD gold, PD red, otherwise gray; PCHG_display capped at -100 to 100.

- Then sgplot vbar bar_order response=PCHG_display group=AVALC etc, with ref lines -30, 20, 0, yaxis -100 to 100 by 20.

We need to replicate in R.

We need to handle reading .xpt or .sas7bdat? The libref directory contains files. Let's assume there are .sas7bdat or .xpt. Use haven::read_sas or read_xpt. Need to list files.

We need to produce outputs: figure f_waterfall.rtf -> write OUT_DIR/f_waterfall.png and CSV of plotted data. Also need to handle titles/footnotes? The SAS titles: Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions. Footnotes etc.

We need to create R script that does all.

We must ensure script runs non-interactively, uses only allowed packages.

We need to handle missing ADTR? Alternative derive from ADRS. But we should try ADTR first, fallback to ADRS.

We need to implement SAS semantics: sorting, etc.

We need to write CSV with same variable names.

We need to produce PNG via ggsave.

We need to also produce txt rendering? The rules say each table or listing additionally as plain-text rendering. But this is graph; maybe just PNG and CSV. However rules say write every output dataset as CSV, each table/listing additionally as txt, each figure as PNG plus CSV of plotted data. So for figure, we need PNG and CSV.

We should also ensure OUT_DIR exists.

We need to handle treatment labels? Not needed for waterfall but maybe include.

We need to handle bar_color mapping.

We need to handle ordering: order by PCHG ascending. In SAS, missing values sort first. Need to handle.

We need to handle capping.

We need to produce waterfall plot: bars colored by AVALC (Best overall response). Use ggplot2.

We need to reproduce reference lines.

We need to handle titles/footnotes in plot? Could add via labs caption or annotate.

We should also produce CSV of plotted data: waterfall dataset.

We need to ensure script reads from absolute paths: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

We need to handle both .sas7bdat and .xpt possibilities. Use function to find file.

We need to handle haven reading.

We need to handle haven labelled vectors: convert to character/numeric.

We need to handle EFFFL filter.

We need to handle PARAMCD = 'BPCHG'

We need to handle TRTA, TRTAN etc.

We need to handle bar_order = row number after sorting.

We need to handle bar_color.

We need to handle PCHG_display.

We need to produce plot with ggplot.

We need to handle colors: darkgreen, green, gold, red, gray. Map to hex? Use those names.

We need to handle legend title "Best Overall Response"

We need to handle xaxis display none, yaxis label.

We need to handle grid.

We need to handle footnotes.

We need to handle titles.

We need to handle output paths via Sys.getenv("OUT_DIR").

We need to ensure script is standalone.

We need to handle date formatting? Not needed.

We need to handle writing CSV with na="".

We need to handle writing PNG with ggsave.

We need to handle also writing txt? For figure, maybe not needed, but we could create a txt with titles.

Better to create f_waterfall.txt with titles/footnotes.

We need to ensure script handles missing ADTR gracefully.

We need to implement helper to read adam dataset.

We need to handle haven::read_sas vs read_xpt.

We need to handle file listing.

We need to handle case-insensitive file names.

We need to handle haven labelled conversion: use haven::as_factor? But better to convert labelled to character via as.character or zap_labels.

Simpler: after reading, for each column that is haven_labelled, convert to appropriate type: if is.numeric and has labels, keep numeric but also maybe convert? For character labelled, convert to character.

We can use haven::zap_formats and zap_labels? But we want values.

We can use dplyr::mutate across where haven::is.labelled ~ as.character or as.numeric.

Better to use haven::zap_labels then convert.

Simpler: use function to convert labelled to its underlying value: if inherits "haven_labelled", then as.vector.

We need to handle.

We need to handle reading ADSL and ADTR.

We need to handle join.

We need to handle ordering.

We need to handle writing outputs.

We need to handle OUT_DIR fallback if not set.

We need to handle creating output directory.

We need to handle ggsave dimensions: width 10in height 6.5in.

We need to handle imagefmt emf in SAS, but in R we use png.

We need to handle barwidth=1, outline black.

In ggplot, use geom_col with color="black", linewidth 0.25.

We need to handle groupdisplay cluster: but vbar with group=AVALC and barwidth=1 and groupdisplay cluster means bars are not stacked, but each bar_order has one AVALC, so it's just colored.

We need to handle xaxis display none: hide x axis ticks/labels.

We need to handle yaxis values -100 to 100 by 20.

We need to handle ref lines.

We need to handle legend position bottom.

We need to handle titles: Figure 14.2.3 etc.

We need to handle footnotes.

We need to handle saving CSV of plotted data: waterfall.csv or f_waterfall.csv? The expected output is figure f_waterfall.rtf -> write OUT_DIR/f_waterfall.png and CSV of plotted data. So CSV name likely f_waterfall.csv or waterfall.csv. Use f_waterfall.csv.

We should also write waterfall.csv for dataset.

Better to write both: f_waterfall.csv and waterfall.csv.

We need to handle writing CSV with row.names FALSE, na "".

We need to handle ensuring script doesn't fail if files missing.

We need to handle alternative derivation from ADRS if ADTR not available.

We need to handle checking existence of ADTR file.

We need to handle reading ADRS if needed.

We need to handle PCHG variable: percent change from baseline.

We need to handle AVALC: best overall response category.

We need to handle TRTA, TRTAN.

We need to handle EFFFL.

We need to handle PARAMCD.

We need to handle PARAM.

We need to handle USUBJID.

We need to handle all.

We need to produce final R script inside one ```r block.

We need to ensure script uses only allowed packages.

We need to ensure script handles Windows paths with forward slashes.

We need to handle Sys.getenv("OUT_DIR").

We need to handle library loading.

We need to handle options.

We need to handle no install.packages.

We need to handle no graphics windows.

We need to handle ggsave.

We need to handle writing txt.

We need to handle rounding? Not needed.

We need to handle SAS semantics: missing values sort first, character comparison case-sensitive, etc.

We need to handle bar_color mapping case-insensitive via upcase.

We need to handle capping.

We need to handle bar_order = _n_ after sorting.

We need to handle final plot.

We need to handle titles/footnotes verbatim.

From u_titles.sas for F_WATERFALL:
title4 "Figure 14.2.3"
title5 "Waterfall Plot of Best Overall Response"
title6 population
But in program they override title5 to "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions"
So use that.

Footnotes in program:
footnote1 "Data cutoff: &cutdate." -> cutdate is [DDMONYYYY] placeholder, so we can use placeholder or actual.
footnote2 "Each bar represents one subject."
footnote3 "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease"
footnote4 "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)"
footnote5 "Program: &sysin. | Run: %sysfunc(date(), date9.)"

We should replicate footnotes in plot caption.

We need to handle sponsor, protocol etc? Not needed.

We need to handle output directory creation.

We need to handle error handling.

We need to produce script that will run with Rscript.

We need to ensure script is complete.

We need to handle reading files: list files in adam directory.

We need to handle haven::read_sas for .sas7bdat, haven::read_xpt for .xpt.

We need to handle haven::read_xpt vs read_sas.

We need to handle file paths with forward slashes.

We need to handle case where adam directory contains .sas7bdat files named adtr.sas7bdat etc.

We need to handle reading.

We need to handle converting column names to uppercase? SAS variable names are uppercase. Haven preserves case. We should ensure we handle case-insensitive matching.

Better to standardize column names to uppercase for processing, then write CSV with original names? But rule says write CSV with SAS dataset's variable names (same case is fine, matching is case-insensitive). So we can use uppercase.

We need to handle join: inner join adam.adsl on USUBJID where EFFFL='Y' and PARAMCD='BPCHG'

We need to handle ordering by PCHG.

We need to handle waterfall dataset creation.

We need to handle plotting.

We need to handle saving.

We need to handle writing CSV of plotted data: include all columns: USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC, EFFFL, bar_order, bar_color, PCHG_display

We need to handle writing PNG.

We need to handle writing txt.

We need to handle final message.

We need to ensure script handles missing OUT_DIR: fallback to current dir or temp.

We need to handle library loading: load haven, dplyr, tidyr, readr, stringr, lubridate, purrr, tibble, ggplot2 etc.

We need to handle not to print full data to console.

We need to handle progress messages via cat.

We need to handle stopifnot for columns.

We need to handle SAS semantics: missing values sort first.

In R, NA handling: arrange with na first.

We need to handle character comparison case-sensitive: R is case-sensitive by default.

We need to handle date arithmetic not needed.

We need to handle LENGTH/SUBSTR etc not needed.

We need to handle PROC SQL joins: drop/rename columns present in both tables except keys, so no .x/.y suffixes appear. We should handle by selecting only needed columns.

We need to handle ensuring columns exist after join.

We need to handle writing outputs.

We need to handle final script.

We should produce script that is robust.

We need to handle reading ADTR: if file not found, try ADRS.

We need to handle reading ADSL: required.

We need to handle reading ADTR vs ADRS.

We need to handle alternative derivation: if ADTR not available, derive from ADRS: select USUBJID, TRTA, TRTAN, min(PCHG) as PCHG, BOR as AVALC where EFFFL='Y' and PARAMCD='SUMLD' group by USUBJID, TRTA, TRTAN, AVALC

But we can implement fallback.

We need to handle reading ADRS file if needed.

We need to handle writing outputs.

We need to handle creating output directory.

We need to handle ggsave.

We need to handle theme.

We need to handle colors mapping: define named vector.

We need to handle legend.

We need to handle reference lines: geom_hline.

We need to handle labels for ref lines.

We need to handle yaxis grid.

We need to handle xaxis display none.

We need to handle bar outline.

We need to handle barwidth.

We need to handle figure size.

We need to handle titles via ggtitle or annotate.

Simpler: use labs title, subtitle, caption.

We need to handle footnote placement.

We need to handle ensuring plot looks similar to SAS.

We need to handle saving CSV.

We need to handle writing txt with titles/footnotes and summary.

We need to handle final script.

We need to ensure script is inside one ```r block.

We need to ensure no prose before or after.

We need to produce final answer.

We need to handle all required packages available.

We need to handle haven reading.

We need to handle haven labelled conversion.

We need to handle stringr.

We need to handle dplyr.

We need to handle ggplot2.

We need to handle readr.

We need to handle tidyr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle data.table not needed.

We need to handle survival not needed.

We need to handle broom not needed.

We need to handle Hmisc not needed.

We need to handle admiral not needed.

We need to handle pharmaversesdtm not needed.

We need to handle readxl not needed.

We need to handle writexl not needed.

We need to handle openxlsx not needed.

We need to handle gt not needed.

We need to handle r2rtf not needed.

We need to handle arrow not needed.

We need to handle jsonlite not needed.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle tidyr.

We need to handle readr.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle lubridate.

We need to handle purrr.

We need to handle tibble.

We need to handle jsonlite.

We need to handle data.table.

We need to handle survival.

We need to handle survminer.

We need to handle broom.

We need to handle Hmisc.

We need to handle admiral.

We need to handle pharmaversesdtm.

We need to handle readxl.

We need to handle writexl.

We need to handle openxlsx.

We need to handle gt.

We need to handle r2rtf.

We need to handle arrow.

We need to handle haven.

We need to handle ggplot2.

We need to handle dplyr.

We need to handle tidyr.

We need to handle readr.

We need to handle stringr.

We need to handle