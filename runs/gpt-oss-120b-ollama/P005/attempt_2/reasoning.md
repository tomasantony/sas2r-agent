We need to fix error: TRTSDT variable likely is character or date? In ADSL dataset, TRTSDT is probably a SAS date numeric (days since 1960). When read via haven, dates become Date class (days since 1970). So subtracting Date - Date works. But TRTSDT may be numeric (double) not Date. Let's inspect: we can't, but we can coerce both to Date. In adsl, TRTSDT likely is numeric SAS date. Haven reads numeric dates as double with class "haven_labelled"? Actually read_sas reads SAS date as double with class "haven_labelled"? It adds class "labelled". But also attribute "label". It may not be Date. We need to convert to Date using as_date with origin = "1960-01-01". So after reading adsl, we should convert TRTSDT and TRTEDT to Date.

Similarly, LBDTC is character string; we parse.

So modify after reading adsl: mutate(TRTSDT = as_date(TRTSDT, origin = "1960-01-01"), TRTEDT = as_date(TRTEDT, origin = "1960-01-01")). Also ensure they are Date.

Also need to ensure ASTDT is Date.

Also need to output listing as .txt and .csv as required. The earlier script wrote .lst and .csv but spec expects .txt and .csv? It said each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")). So we should write .txt not .lst. Also need to write .txt with numbers formatted like SAS (maybe default). We'll write using write.table with sep = "\t". Provide .txt.

Also need to ensure adlb dataset written as csv with name adlb.csv (done). Ensure output directory path.

Also need to ensure we keep variable order as SAS keep list. Already selected.

Potential other issues: In mutate for ASTDY we used ifelse with Date objects; ifelse will coerce to numeric? Actually ifelse returns same type as yes/no; but subtraction returns difftime? Actually Date - Date returns numeric (difftime in days). That's fine.

But we used ifelse with Date objects; ifelse expects logical test, returns values; but if test is vector, yes and no must be same type. We used as.numeric for else. That's okay.

But need to ensure ASTDT is Date; we used as.Date.

Now need to write listing .txt and .csv. Also need to write .txt for the table (maybe same as .csv?). We'll produce .txt.

Also need to ensure we write the listing of first 15 rows as per PROC PRINT (obs=15). Already.

Now adjust script.

Also need to ensure we keep variable order: In adlb final dataset, SAS kept variables: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ BASE CHG ABLFL LBNRIND. Our select includes those plus maybe missing TRT01A, TRTEDT. We omitted TRT01A and TRTEDT in select earlier. Need to include them. In adlb_draft we selected them, but later after join we selected only certain columns. We need to include TRT01A, TRTEDT. So modify select after join to include them.

Let's adjust: after left_join, mutate, then select(STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT, PARAMCD, PARAM, AVAL, AVISIT, AVISITN, ASTDT, ASTDY, LBSTRESU, SRCDOM, SRCVAR, SRCSEQ, BASE, CHG, ABLFL, LBNRIND). Ensure order.

Also need to ensure BASE variable present (from base_flags). We have BASE column.

Now write adlb dataset.

Also need to ensure missing values are written as empty strings in CSV (na = ""). Already.

Now produce final script.