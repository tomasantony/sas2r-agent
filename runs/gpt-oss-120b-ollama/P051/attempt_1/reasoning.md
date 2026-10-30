We need to produce R script that replicates SAS program. Steps:

- Read macro variables: paths given. Use environment variable OUT_DIR for outputs.

- Read ADCM.xpt from rawadam path: rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam. So source file path: file.path(rawadam, "adcm.xpt").

- Read using haven::read_xpt.

- Apply filter where POPN (SAFFL) = 'Y'.

- Sort by TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD. SAS sort out=work.adcm but we can just arrange.

- Create ADCM2 dataset: add columns StartStop, DoseUnit, Period (not used later), Ongoing.

   - length StartStop $25 Doseunit $50 not needed.

   - Period = 'Study Period' (not used in report).

   - If CMSTDTC == '' then set to 'NK'. In SAS, missing character is '' (blank). In R, missing is NA. So we need to replace NA or empty string with 'NK'.

   - Same for CMENDTC.

   - StartStop = compbl(CMSTDTC||'/'||CMENDTC). compbl removes multiple blanks. In R, we can paste0(CMSTDTC, '/', CMENDTC) and then trim and collapse spaces. Since values are dates strings maybe already fine. Use stringr::str_squish.

   - DoseUnit: if cmunit not missing then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE). cmunit variable? In SAS dataset, variable CMDOSU maybe? Actually they refer to cmunit variable; maybe it's CMDOSU. We'll assume column CMDOSU. So if CMDOSU not missing (not NA) then combine.

   - Ongoing: if AENRF='Ongoing' then 'Y' else 'N'.

- Then produce report: PROC REPORT with BY TRTA. Columns: TRTA (noprint), USUBJID, CMTRT, cmdecod, cmclas, DoseUnit, CMINDC, StartStop, ADURN, Ongoing.

   - Need to output a table listing for each TRTA group, with titles.

   - Output as RTF in SAS; we need to produce plain text rendering and CSV of same table.

   - So we need to create a data frame with rows sorted by TRTA then as per PROC REPORT default ordering (by TRTA, then order of appearance). Since PROC REPORT with BY TRTA will produce separate sections but overall we can just keep sorted.

   - The output table should include columns as defined, with widths etc not needed.

   - Need to format numbers: USUBJID width 11 flow (wrap). Not needed.

   - ADURN numeric days; keep as is.

   - Ongoing Y/N.

   - Title lines: first line: "Safety Population" (since POPNLABEL = Safety). Title2: "Study Period". Title3: "Treatment: <TRTA value>" for each group.

   - For plain-text rendering, we can create a string that includes titles and then the table rows formatted similarly to SAS PROC REPORT default output. That's complex. But requirement: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body." For this listing, there are no counts/percentages, just raw values. So we can output a simple text file with titles and then the table rows separated by spaces or tabs.

   - Provide CSV of table body: write.csv of ADCM2 selected columns (excluding TRTA? but TRTA is not printed). However they may want the full listing as shown, which includes TRTA as grouping but not column. We'll include TRTA column in CSV for reference.

- Output dataset: maybe they expect ADCM dataset? The SAS program writes only listing, not dataset output. But we need to produce dataset outputs as per "Declared output types: table;dataset". So we should output ADCM2 as CSV dataset.

- Name of outputs: Target36.rtf -> we produce Target36.txt and Target36.csv (table). Also dataset maybe ADCM2.csv.

- Use out <- Sys.getenv("OUT_DIR").

- Write CSV: write.csv(adcm2, file.path(out, "ADCM2.csv"), row.names=FALSE, na="").

- For table body CSV: maybe name "Target36.csv". Use same columns as report (including TRTA? maybe not). We'll include all columns used in report plus TRTA for grouping.

- For text rendering: create a string builder.

Implementation details:

- Use dplyr for transformations.

- Sorting: arrange(TRTA, STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD)

- Ensure character ordering matches SAS: uppercase before lowercase. R's default ordering is based on locale; we can use base::order with method = "radix"? Not needed.

- Date handling: CMSTDTC and CMENDTC are character dates maybe in ISO format. Keep as character.

- For missing values: treat NA as missing; replace with "NK".

- Use stringr::str_squish for compbl.

- Write text file: iterate over each TRTA group, write titles and then rows formatted with fixed width? Could just use write.table with sep = " " and row.names=FALSE, col.names=TRUE.

Simplify: Use write.table(adcm_report, file.path(out, "Target36.txt"), sep = " ", row.names=FALSE, col.names=TRUE, quote=FALSE). Prepend titles using cat.

We'll create a vector of lines: first title line, second, then for each group: title3 line, then table rows.

Implement:

lines <- c()
lines <- c(lines, paste0(POPNLABEL, " Population"))
lines <- c(lines, "Study Period")
for each trta in unique(adcm_report$TRTA):
   lines <- c(lines, paste0("Treatment: ", trta))
   subdf <- filter(adcm_report, TRTA == trta) %>% select(-TRTA)
   # format each row as space-separated values
   rows <- apply(subdf, 1, function(x) paste(x, collapse = " "))
   lines <- c(lines, rows)

writeLines(lines, file.path(out, "Target36.txt"))

CSV of table body: write.csv(adcm_report %>% select(-TRTA), ...)

Now produce final R script.

Edge Cases: variable names case: use same as SAS: STUDYID, USUBJID, TRTA, CMTRT, CMDECOD, CMCLAS, DoseUnit, CMINDC, StartStop, ADURN, Ongoing.

Note: CMTRT variable exists in source dataset? Not listed in variable list but used in report. We'll keep it.

Implementation steps:

- Set root, rawadam etc from environment? Not needed; we can use Sys.getenv but they are macro variables not environment. We'll just construct paths using given values.

- Use rawadam path variable.

- Use out <- Sys.getenv("OUT_DIR").

Now produce script.

