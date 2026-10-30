We need to translate SAS program to R script that reads data from inline datalines (hardcoded). Since SAS creates datasets rs, tu, adsl from datalines. In R we can create data frames directly using readr::read_delim on text connections.

We need to output datasets: adeff, adeff_resp, adeff_tumor, adsl as CSVs. Also produce listing P088.lst (table/listing text, 676 numbers) – unclear what listing is; SAS program ends with proc contents and proc print. Likely the listing is the printed dataset (proc print). So we need to generate a text rendering of adeff (maybe same as SAS print). Also a CSV of the same table. We'll create a text file with formatted table similar to SAS default: columns with values, maybe using write.table with row.names=FALSE, sep=" ". But need to match numbers formatting: dates as date9. (e.g., 15JAN2024). In SAS print, date formatted as date9. So we need to format ADT as DDMMMYYYY (uppercase month). Also ADY numeric integer. AVAL numeric maybe integer. CHG, PCHG may be numeric with maybe 2 decimals? SAS default prints with best12. We'll just output raw values.

Simplify: produce a text file with write.table(adeff, file, sep="\t", row.names=FALSE, na="") maybe.

Also need to output CSV of adeff.

Implement functions for date conversion: input RSDTC string "2024-01-15" to Date using ymd. Then ADT = date. ADY calculation.

Implement baseline flag: ABLFL = if AVISIT == "Baseline" then "Y" else "".

ANL01FL: if ABLFL != "Y" and !is.na(AVAL) then "Y" else "".

For adeff_resp: need AVALC = RSSTRESC, AVAL numeric mapping.

For adeff_tumor: AVAL = TUSTRESN, AVALU = TUSTRESU.

Derive baseline and change: compute BASE from baseline rows (ABLFL='Y') per USUBJID and PARAMCD.

Add CHG, PCHG.

BOR: from adeff_resp where ABLFL != 'Y', select lowest AVAL per USUBJID (i.e., min). In SAS they sort by USUBJID AVAL ascending, then first per USUBJID. So we can group_by USUBJID, filter ABLFL!='Y', slice_min(AVAL, with ties? first). Use slice_min.

Create BOR rows with fields: PARAMCD='BOR', PARAM='Best Overall Response', PARAMN=3, AVISIT='Best Response', AVISITN=99, ABLFL='', ANL01FL='Y'. Also need other columns: keep same as adeff_resp? Should include all columns from adeff_resp (including STUDYID etc). For BOR, need to merge baseline variables from adsl (like other columns). In SAS they derived BOR from _resp_sorted which is adeff_resp rows (which already have those columns). So we can take the first row per USUBJID from adeff_resp where ABLFL!='Y', then replace PARAMCD etc.

Responder flag: from _bor, create RESPFL variable, then create new row with PARAMCD='RESPFL', PARAM='Objective Response Flag', PARAMN=4, AVAL = if RESPFL='Y' then 1 else 0, AVALC = RESPFL. Also need other columns same as BOR (including STUDYID etc). Also set ANL01FL? Not in SAS; they didn't set ANL01FL, but they set ABLFL='' earlier. We'll keep same as BOR.

Nadir: compute min AVAL per USUBJID where ABLFL!='Y' (post-baseline). Use group_by USUBJID, summarise min_val = min(AVAL, na.rm=TRUE). Then merge with baseline row (where ABLFL='Y') to get BASE and other subject-level vars. Then create row with PARAMCD='NADIRTUM', PARAM='Nadir Tumor Size', PARAMN=5, AVALU='mm', AVISIT='Nadir', AVISITN=98, ABLFL='', ANL01FL='Y', compute CHG, PCHG similar.

Combine all datasets: bind_rows adeff_resp, adeff_tumor, BOR, RESPFL, NADIRTUM.

Add ASEQ: sort by STUDYID USUBJID PARAMN AVISITN ADT, then within each USUBJID assign ASEQ starting at 0 then +1.

Add RSCAT: for rows where PARAMCD in ('OVRLRESP','BOR'), map AVALC to categories.

Add labels: not needed for output.

Write outputs.

Implement round-half-up? Not needed as we don't round.

Write CSVs: adeff.csv, adeff_resp.csv, adeff_tumor.csv, adsl.csv.

Also write listing: adeff.txt (maybe P088.lst). Use write.table with appropriate formatting.

Also write CSV of listing: adeff_listing.csv.

Implement environment variable OUT_DIR.

Let's craft script.

