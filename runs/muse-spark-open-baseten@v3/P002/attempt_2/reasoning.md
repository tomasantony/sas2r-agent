Need to produce R script translating SAS program P002_adae.sas.

We need to read inputs: sdtmnew AE, suppae, adamnew ADSL, ADAE? Actually program creates ADAE from AE + ADSL. Need to replicate.

Paths: root = D:/meta_hackathon/sas2r-agent
rawsdtm = .../data/raw/cdisc-pilot/sdtm
rawadam = .../data/raw/cdisc-pilot/adam
derived? But setup loads xpt from raw. So we should read from raw xpt files.

We need to find actual files. Use haven::read_xpt.

We need to implement logic:

- proc sort AE by USUBJID AESEQ
- data AE: ASTDT = input(strip(AESTDTC), yymmdd10.), AENDT similarly. In SAS yymmdd10. expects YYYY-MM-DD. Use as.Date with format "%Y-%m-%d". With ?? means invalid -> missing. So parse.
- ADUR = AENDT - ASTDT +1 if both > .z (i.e., not missing)
- format DATE9.

- TEMP from adamnew.ADSL keep SITEID USUBJID TRT01A TRT01AN AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT rename TRT01A=TRTA TRT01AN=TRTAN. Sort by USUBJID.

- merge AE with TEMP by USUBJID (left join, keep if inAE). So AE left join ADSL.

- if ASTDT=. and length(AESTDTC)=7 then ASTDT = input(compress(AESTDTC||'-01'), yymmdd10.) and ASTDTF='D'. Note length 7 means like "YYYY-MM" ? Actually AESTDTC length 7 would be "2006-01" etc. Then compress and add -01 => "2006-01-01" -> parse. Need to handle.

- CQ01NAM length 25.

- ASTDY: if ASTDT >= TRTSDT then ASTDT-TRTSDT+1 else if TRTSDT > ASTDT then ASTDT-TRTSDT

- AENDY similarly.

- ANLSTDY = ASTDT-TRTSDT+1 if both not missing (but not used later?)

- TRTEMFL = 'Y' if ASTDT >= TRTSDT else 'N' (when ASTDT >= TRTSDT and TRTSDT not missing). SAS: IF (ASTDT>=TRTSDT>.z) then TRTEMFL='Y'; ELSE TRTEMFL='N'; This means if TRTSDT missing then false -> N. Also if ASTDT missing then false -> N.

- CQ01NAM = 'DERMATOLOGIC EVENTS' if index(aedecod,'APPLICATION') or index(aedecod,'DERMATITIS') or index(aedecod,'ERYTHEMA') or index(aedecod,'BLISTER') or (aebodsys='SKIN AND SUBCUTANEOUS TISSUE DISORDERS' and aedecod not in ('COLD SWEAT','HYPERHIDROSIS','ALOPECIA')) . Note index is case-sensitive substring search. In SAS index is case-sensitive. So need to check.

- flagfrst macro: sorts by usubjid &sortvars ASTDT where trtemfl='Y' and optional filters, then first occurrence per group.

We have 7 calls:
1. AOCCFL: sortvars=, invar=, ser=, derm= => first per USUBJID
2. AOCCSFL: sortvars=aebodsys, invar=aebodsys => first per USUBJID + aebodsys
3. AOCCPFL: sortvars=aebodsys aedecod, invar=aedecod => first per aedecod? Wait code: by usubjid &sortvars ASTDT; if first.&invar then. So for third, by usubjid aebodsys aedecod ASTDT, first.aedecod. That effectively first per USUBJID+AEBODSYS+AEDCOD? But first.aedecod will be true when aedecod changes within usubjid+aebodsys group. Since sorted by aebodsys aedecod, first.aedecod corresponds to first occurrence of each PT within SOC per subject. However if same PT appears in different SOC? Probably not.

4. AOCC02FL: ser=Y => where aeser='Y' and trtemfl='Y', first per USUBJID
5. AOCC03FL: ser=Y, sortvars=aebodsys invar=aebodsys => first serious per SOC
6. AOCC04FL: ser=Y, sortvars=aebodsys aedecod invar=aedecod => first serious per PT
7. AOCC01FL: derm=Y => where CQ01NAM='DERMATOLOGIC EVENTS' and trtemfl='Y', first per USUBJID

Then merge back to AE by usubjid aeseq.

Then retain ordering and keep selected variables.

Finally create adamnew.ADAE with format DATE9.

We need to output dataset adae.csv under OUT_DIR.

We must handle dates: TRTSDT, TRTEDT from ADSL are likely numeric SAS dates or character? In ADSL xpt, they are probably character DTC or numeric date. Need to inspect. But we can handle both: if numeric, convert via as.Date with origin 1960-01-01. If character, parse.

Similarly AE dates.

We need to write script that reads xpt files.

We need to ensure output matches SAS dataset. Need to check expected output dataset adae.csv from SAS to replicate column order and values.

We don't have actual files to inspect, but we can write generic code that handles both.

We must also handle haven labelled vectors: convert to appropriate types.

We need to implement SAS date handling: haven reads SAS dates as Date? Actually haven::read_xpt converts SAS dates to Date? For transport files, dates are stored as numeric with format. haven will convert to Date if format is DATE. Might be Date. We'll handle.

Simplify: use haven::read_xpt, then for each date column, if inherits Date, keep; if numeric, convert; if character, parse.

We need to implement round-half-up helper though not needed.

We need to write CSV with na="".

We need to ensure OUT_DIR exists.

We need to produce single R script.

We should include logic for parsing AESTDTC: yymmdd10. expects "YYYY-MM-DD". Use as.Date(strip, "%Y-%m-%d"). If fails, NA.

For length 7 case: AESTDTC length 7 => e.g., "2012-01" (7 chars). Then compress(AESTDTC||'-01') => "2012-01-01" -> parse.

Note SAS code checks length(AESTDTC)=7 and ASTDT=. . So only impute if original parse failed and length 7.

We need to implement CQ01NAM logic: index checks substring. Use grepl with fixed=TRUE.

Also need to handle missing values: index returns 0 if missing? In SAS index of missing returns 0. So treat NA as FALSE.

We need to implement flagfrst: For each, filter trtemfl=='Y' plus optional, sort by USUBJID, sortvars, ASTDT. Note SAS sort missing first. R arrange will put NA first if na.last=FALSE? Need to emulate. Use arrange with appropriate.

Then group_by USUBJID and sortvars? Actually first.&invar logic: if invar empty, first.usubjid; else first.&invar. In SAS, BY usubjid &sortvars ASTDT; first.&invar checks first occurrence of invar within the BY group. Since BY includes usubjid, sortvars, ASTDT, the first.&invar will be true at start of each invar group. For invar=aebodsys, BY is usubjid aebodsys ASTDT, so first.aebodsys is first per USUBJID+AEBODSYS. For invar=aedecod, BY is usubjid aebodsys aedecod ASTDT, so first.aedecod is first per USUBJID+AEBODSYS+AEDCOD.

For invar empty, first.usubjid is first per USUBJID.

We can implement via group_by.

We need to handle ties: if multiple records same USUBJID, same sortvars, same ASTDT, SAS will keep order as input? But first flag will be first row in that group. Our implementation should pick first row after sorting.

We need to create datasets aea etc with columns usubjid, outvar, aeseq.

Then merge back.

We need to retain variable order as in final retain statement.

List: STUDYID SITEID USUBJID TRTA TRTAN AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT ASTDT ASTDTF ASTDY AENDT AENDY ADUR AETERM AELLT AELLTCD AEDECOD AEPTCD AEHLT AEHLTCD AEHLGT AEHLGTCD AEBODSYS AESOC AESOCCD AESEV AESER AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD AEREL AEACN AEOUT AESEQ TRTEMFL AOCCFL AOCCSFL AOCCPFL AOCC02FL AOCC03FL AOCC04FL CQ01NAM AOCC01FL

We need to keep only those that exist.

We need to handle labels? Not needed for CSV.

We need to write dates as YYYY-MM-DD.

We need to ensure numeric values unrounded.

We need to produce script that runs non-interactively.

We should include Sys.getenv("OUT_DIR") handling.

We should also handle reading from absolute paths given in context: &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm etc. Use forward slashes.

We should attempt to read from both raw and derived locations for robustness.

We need to output adae.csv.

We should also ensure we handle haven labelled: use haven::zap_labels etc or convert.

We need to produce complete R script.

Let's craft R code.

