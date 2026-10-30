We need to produce R script that reproduces outputs: dataset adae.csv (and maybe other outputs? Only dataset adae is declared). The script must read raw data from paths given: rawadam, rawsdtm etc. The SAS program loads XPT files: ae.xpt, suppae.xpt, adsl.xpt, adae.xpt (but adae.xpt is overwritten later). Actually they load adae.xpt into adamnew library, then later they create new dataset adamnew.ADAE from AE. So final output is the new ADaE dataset.

Thus we need to replicate steps:

1. Read sdtmnew.AE from XPT file: path D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/ae.xpt

2. Sort by USUBJID AESEQ.

3. Create ASTDT and AENDT from AESTDTC and AEENDTC using yymmdd10. format (SAS date). In R, parse with ymd (lubridate). Use input(strip(...), ?? yymmdd10.) meaning if invalid returns missing. So we can use ymd with quiet = TRUE.

4. If both dates not missing, ADUR = AENDT - ASTDT + 1.

5. Format dates as DATE9. (SAS DATE9 is ddMONyyyy). For output CSV we need ISO format (YYYY-MM-DD). So we will keep Date class.

6. Load adsl from XPT: path .../adam/adsl.xpt. Keep variables: SITEID USUBJID TRT01A TRT01AN AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT. Rename TRT01A to TRTA, TRT01AN to TRTAN.

7. Merge AE with adsl (TEMP) by USUBJID, keep all AE rows (if inAE). So left join.

8. Additional processing:

- If ASTDT missing and length(AESTDTC)=7 then compute ASTDT = input(compress(AESTDTC||'-01'), yymmdd10.) and set ASTDTF='D'.

In SAS, length(AESTDTC)=7 means string length 7 (e.g., '2020-01'?). So we need to check nchar(AESTDTC)==7.

- Compute ASTDY: if ASTDT>=TRTSDT>.z then ASTDY=ASTDT-TRTSDT+1; else if TRTSDT>ASTDT>.z then ASTDY=ASTDT-TRTSDT;

Note: .z is SAS missing numeric? Actually .z is a special missing value. In R, treat missing as NA. So condition ASTDT>=TRTSDT>.z means both not missing and ASTDT >= TRTSDT. So we implement with if (!is.na(ASTDT) && !is.na(TRTSDT) && ASTDT >= TRTSDT) etc.

- Similarly AENDY.

- ANLSTDY = ASTDT-TRTSDT+1 if both not missing.

- TRTEMFL = 'Y' if (ASTDT>=TRTSDT>.z) else 'N'.

- CQ01NAM: if index(aedecod,'APPLICATION') or ... etc then set 'DERMATOLOGIC EVENTS'. Use grepl.

9. Then macro flagfrst creates flags for first occurrence.

We need to implement three calls for AOCCFL, AOCCSFL, AOCCPFL, plus others for serious etc. But final dataset includes AOCCFL, AOCCSFL, AOCCPFL, AOCC02FL, AOCC03FL, AOCC04FL, AOCC01FL. So we need to compute all.

Macro flagfrst:

Parameters: inds (input dataset), sortvars (list of variables to sort by), invar (variable to check first occurrence), outvar (new flag variable), outds (output dataset name), ser (if 'Y' then filter aeser='Y'), derm (if 'Y' then filter CQ01NAM='DERMATOLOGIC EVENTS').

Implementation steps:

- Sort data by USUBJID, sortvars, ASTDT, where trtemfl='Y' and optional filters.

- Then data step ds02: keep USUBJID, outvar, aeseq. For each BY group (by USUBJID sortvars ASTDT), if first occurrence (first.usubjid if invar empty else first.invar) then set outvar='Y' and output.

- Then sort ds02 by USUBJID aeseq and merge back to AE.

Thus each flag dataset contains rows with USUBJID, aeseq, flag=Y for first occurrence per group.

Implementation in R:

Define a function flagfrst(df, sortvars, invar, outvar, ser=FALSE, derm=FALSE). It will:

- Filter df where TRTEMFL == 'Y'.

- If ser TRUE, filter aeser == 'Y'.

- If derm TRUE, filter CQ01NAM == 'DERMATOLOGIC EVENTS'.

- Arrange by USUBJID, then sortvars (if any), then ASTDT.

- Group_by USUBJID and any sortvars (if any). Within each group, find first row (by order). If invar is empty, first row per USUBJID (ignoring sortvars?). Actually macro: by usubjid &sortvars ASTDT; then condition: if %if &invar= %then first.usubjid; else first.&invar; So if invar empty, they use first.usubjid (first per USUBJID). If invar provided, they use first.invar (first occurrence where invar changes? Actually first.&invar means first occurrence of that variable within BY group? In SAS, first.variable is true when the variable value changes (i.e., first observation of each BY group for that variable). Since they also have BY list includes that variable, first.invar will be true for first observation of each distinct value of that variable within USUBJID and sortvars and ASTDT ordering. So we need to replicate.

Simplify: Use dplyr::group_by at appropriate levels and then slice first row per group.

Implementation details:

If sortvars empty and invar empty: group_by USUBJID.

If sortvars non-empty and invar empty: group_by USUBJID, then sortvars? Actually BY list includes sortvars and ASTDT. So grouping is USUBJID + sortvars. So we group_by USUBJID, then any sortvars.

If invar non-empty: BY list includes sortvars and invar and ASTDT. So grouping is USUBJID, sortvars, invar.

Thus we can compute grouping_vars = c("USUBJID", sortvars, if(invar!="") invar else NULL). Then arrange by USUBJID, sortvars, ASTDT. Then for each group, take first row, set flag= 'Y'.

Return a data frame with USUBJID, AESEQ, outvar.

Edge: need to keep AESEQ column.

Implementation steps for each flag:

- ds_flag <- flagfrst(AE, sortvars = c(), invar = "", outvar = "AOCCFL", ser = FALSE, derm = FALSE)

- ds_flag2 <- flagfrst(AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCCSFL", ser = FALSE, derm = FALSE)

- ds_flag3 <- flagfrst(AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL", ser = FALSE, derm = FALSE)

- ds_flag4 (serious) AOCC02FL: sortvars empty, invar empty, ser=TRUE

- ds_flag5 AOCC03FL: sortvars = "AEBODSYS", invar = "AEBODSYS", ser=TRUE

- ds_flag6 AOCC04FL: sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", ser=TRUE

- ds_flag7 AOCC01FL: sortvars empty, invar empty, derm=TRUE

After generating these flag datasets, merge them back to AE by USUBJID AESEQ (left join). The SAS code merges AE with AEA AEB AEC SAEA SAEB SAEC DERM (these are the flag datasets). So we need to join all.

Finally retain variable order as per retain statement.

The final dataset includes many variables; we need to keep those listed.

We'll produce final data frame AE_final with selected columns in order.

Then write to CSV: write.csv(AE_final, file.path(out, "adae.csv"), row.names=FALSE, na="").

Also need to output .txt rendering? The spec says each output dataset as CSV; also each table/listing as txt. But only dataset output is required. So we just output CSV.

Make sure to set environment variable OUT_DIR.

Implement round-half-up? Not needed.

Implement date parsing: use lubridate::ymd.

Implement compress: remove non-alphanumeric? In SAS compress(AESTDTC||'-01') removes spaces? Actually compress removes all blanks by default. So we can just paste0(AESTDTC, "-01") and remove spaces.

Edge: AESTDTC may be missing; length 7 check.

Implement ASTDTF variable: set to 'D' when imputed.

Now produce script.

