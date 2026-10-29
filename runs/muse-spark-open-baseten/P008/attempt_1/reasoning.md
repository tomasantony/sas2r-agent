Need to translate SAS program to R.

We need to read inputs from absolute paths given. Librefs:
 raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
 sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
 adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

We need to find files. Likely adam.adsl and sdtm.vs are SAS datasets? Could be .sas7bdat or .xpt. Let's check directory listing via R? But we need to write script that handles both.

We need to produce outputs:
 P008.lst (table/listing text, 180 numbers) -> write text rendering and CSV of same table
 dataset advs -> write OUT_DIR/advs.csv

The SAS program does:
- sort adsl by USUBJID
- sort vs by USUBJID VSDTC VSTESTCD
- merge adsl_sorted and vs_sorted by USUBJID, keep only b (vs records)
- derive PARAMCD=VSTESTCD, PARAM=VSTEST, AVAL=VSSTRESN, AVISIT=VISIT, AVISITN=VISITNUM, SRCDOM='VS', SRCVAR='VSSEQ', SRCSEQ=VSSEQ
- ASTDT = input(substr(VSDTC,1,10), yymmdd10.) if length>=10
- ASTDY = (ASTDT - TRTSDT)+1 if ASTDT>=TRTSDT else ASTDT-TRTSDT, if both not missing
- keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ

- Identify baseline: sort advs_draft by USUBJID PARAMCD ASTDT AVISITN SRCSEQ where ASTDT <= TRTSDT and not missing AVAL
- then by USUBJID PARAMCD, take last.PARAMCD => BASE=AVAL, BASE_DT=ASTDT, BASE_SEQ=SRCSEQ

- Merge back: sort advs_draft by USUBJID PARAMCD, merge with base_flags, if SRCSEQ==BASE_SEQ then ABLFL='Y' else ''
- CHG = AVAL - BASE if both not missing
- drop BASE_DT BASE_SEQ
- sort by USUBJID PARAMCD ASTDT

- Visual audit: proc print obs=15 var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL

We need to reproduce exactly.

Need to handle reading. Let's assume files are .sas7bdat. Use haven::read_sas. Also check for .xpt.

We need to handle variable names uppercase due to options validvarname=upcase. So all vars uppercase.

We need to handle dates: TRTSDT, TRTEDT, ASTDT are numeric SAS dates (days since 1960-01-01). When reading via haven, they may be labelled with Date format. haven reads SAS dates as numeric? Actually haven::read_sas converts SAS date/time to R Date/POSIXct if format present. Need to handle both.

We need to ensure ASTDT derived as Date.

ASTDY calculation: SAS date arithmetic in days.

BASE logic: need to handle missing.

ABLFL logic: if SRCSEQ == BASE_SEQ then 'Y' else '' . Note if BASE_SEQ is NA (no baseline), then comparison yields NA -> should be ''? In SAS, missing numeric vs missing? If no baseline, BASE_SEQ missing, SRCSEQ not missing, then SRCSEQ = BASE_SEQ false => ABLFL=''. If both missing? Unlikely. Need to handle NA.

CHG: if not missing AVAL and BASE then CHG = AVAL - BASE

We need to write advs.csv with same variable names, dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric unrounded.

Also need to write listing: P008.lst text rendering and CSV. The listing is proc print obs=15. So we need to produce P008.txt and P008.csv? But expected outputs says P008.lst -> write text rendering and CSV of same table. And dataset advs -> write OUT_DIR/advs.csv

We should produce:
- OUT_DIR/advs.csv
- OUT_DIR/P008.lst.txt? Or P008.txt? The instruction says each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers. And each output dataset as CSV. The expected outputs list P008.lst and dataset advs. So we need to produce files that correspond. The generic rule: write every output into OUT_DIR with <name>.csv and <name>.txt. So for P008.lst, we should write P008.csv and P008.txt? Or P008.lst.csv and P008.lst.txt? Safer to produce both variants.

The instruction: "Write every output into the directory in the environment variable OUT_DIR (out <- Sys.getenv("OUT_DIR")):
  * each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv"), ...
  * each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) ..."

So for dataset advs, we write advs.csv (and maybe advs.txt not needed but could). For listing P008.lst, we write P008.lst.csv and P008.lst.txt, plus also P008.csv/txt for compatibility.

We need to ensure 180 numbers? The listing has 15 obs * maybe? Let's see variables: USUBJID, PARAMCD, AVISIT, ASTDT, TRTSDT, AVAL, BASE, CHG, ABLFL = 9 vars. 15 rows. Numbers: AVAL, BASE, CHG are numeric, plus dates. 180 numbers suggests maybe 15*? Let's not worry.

We need to implement SAS semantics: sorting, missing handling.

We need to write R script that reads inputs from absolute paths.

We should explore file system to know exact file names. But we can't run interactive? We can attempt to write script that dynamically finds files.

We need to handle reading ADSL and VS.

Potential paths:
- D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.sas7bdat or .xpt
- D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/vs.sas7bdat

We should write helper function to find file.

Also need to handle STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT, etc.

We need to ensure retain STUDYID USUBJID ordering: just column order.

We need to handle VSDTC parsing: substr(VSDTC,1,10) then yymmdd10. So if VSDTC is like "2020-01-02T..." we take first 10 chars. Need to parse as Date. Use as.Date(substr(...), format="%Y-%m-%d") . SAS yymmdd10. handles various separators? But assume ISO.

We need to handle length(VSDTC) >=10 . In R, nchar.

We need to handle TRTSDT may be Date or numeric SAS date. Convert to Date.

We need to handle output dates as YYYY-MM-DD. So when writing CSV, format Date columns as character YYYY-MM-DD.

We need to handle numeric unrounded.

We need to produce text rendering: similar to PROC PRINT. Use simple formatted table.

We should also ensure sorting: SAS sorts with character ordering uppercase before lowercase; R default similar? Use base order.

We need to handle missing values sort first: In SAS, missing sorts first. In R, NA sorts last by default. Need to handle for baseline candidate sorting: by USUBJID PARAMCD ASTDT AVISITN SRCSEQ. If ASTDT missing, it would be first in SAS, but we filter where ASTDT <= TRTSDT and not missing AVAL, but ASTDT could be missing? Then condition ASTDT <= TRTSDT would be false if ASTDT missing? In SAS, missing < any number, so missing <= TRTSDT would be true, but they also have where ASTDT <= TRTSDT and not missing(AVAL). However they also have earlier where ASTDT derived only if length>=10, else missing. So missing ASTDT would pass filter if TRTSDT not missing? But then baseline could be missing date. However SAS where clause: where ASTDT <= TRTSDT and not missing(AVAL) . If ASTDT missing, then ASTDT <= TRTSDT is true (since missing is smallest), so it would be included. But is that intended? Possibly. In R, NA <= date is NA, not TRUE, so would be excluded. Need to replicate SAS semantics: missing sorts first and comparison missing <= anything is TRUE. So we need to handle.

But also they sort by ASTDT, with missing first. So baseline selection last.PARAMCD would pick latest date among candidates. If missing dates are first, they wouldn't be last unless all missing.

We need to decide to replicate SAS missing handling: For filtering, include rows where is.na(ASTDT) or ASTDT <= TRTSDT . However SAS also treats missing TRTSDT? If TRTSDT missing, then ASTDT <= missing? Missing is smallest, so any non-missing ASTDT > missing => false. So only missing ASTDT would pass. But in R, NA comparison yields NA. Need to handle.

Simpler: implement SAS logic: if is.na(ASTDT) then condition true? But only if TRTSDT not missing? Actually SAS: . <= 20000 is true (since . is -inf). So missing ASTDT <= any non-missing TRTSDT is true. Missing ASTDT <= missing TRTSDT? Both missing: . <= . is true? In SAS, missing equals missing, so true. So condition would be true for missing ASTDT regardless of TRTSDT. But is that intended? Might be edge case not present.

We could implement filter as: (is.na(ASTDT) | is.na(TRTSDT) | ASTDT <= TRTSDT) & !is.na(AVAL) . But that would include missing ASTDT even when TRTSDT missing. That matches SAS.

However typical SAS where ASTDT <= TRTSDT would exclude missing TRTSDT? Let's think: In SAS, if TRTSDT is missing, then ASTDT <= . is false unless ASTDT also missing. So non-missing ASTDT <= missing is false. So our logic (is.na(TRTSDT) => true) would be wrong. Need to replicate: condition is ASTDT <= TRTSDT . In SAS, if TRTSDT missing, then only missing ASTDT passes. So we need to implement: if is.na(ASTDT) & is.na(TRTSDT) => true; if is.na(ASTDT) & !is.na(TRTSDT) => true; if !is.na(ASTDT) & is.na(TRTSDT) => false; if both non-missing => ASTDT <= TRTSDT.

So logic: (is.na(ASTDT) | (!is.na(TRTSDT) & ASTDT <= TRTSDT)) . But need to handle NA in comparison.

We can implement: ast_le_trtsdt <- ifelse(is.na(ASTDT) & is.na(TRTSDT), TRUE,
                               ifelse(is.na(ASTDT) & !is.na(TRTSDT), TRUE,
                               ifelse(!is.na(ASTDT) & is.na(TRTSDT), FALSE,
                                      ASTDT <= TRTSDT)))

Simpler: ast_le_trtsdt <- is.na(ASTDT) | (!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT <= TRTSDT) | (is.na(ASTDT) & is.na(TRTSDT)) but first covers.

Actually is.na(ASTDT) alone covers both cases where ASTDT missing => true regardless of TRTSDT. So ast_le_trtsdt <- is.na(ASTDT) | (!is.na(TRTSDT) & ASTDT <= TRTSDT)

But need to handle NA in second part: if TRTSDT is NA, then !is.na(TRTSDT) false => false, so overall false unless ASTDT is NA. Good.

So filter: ast_le_trtsdt & !is.na(AVAL)

We should implement that.

Also need to handle sorting with missing first: For baseline candidates, sort by USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ with NA first. In R, we can use order with na.last=FALSE.

Similarly final sort by USUBJID PARAMCD ASTDT with NA first.

We need to handle merge: SAS merge by USUBJID: adsl_sorted (one row per USUBJID) with vs_sorted (many rows per USUBJID). Merge by USUBJID keeps all vs rows and adds adsl vars. If adsl has duplicate USUBJID? Should be unique. So we can do left_join vs with adsl.

We need to handle keep variables: STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ

Note STUDYID comes from both? In merge, if both have STUDYID, SAS takes from last dataset? Actually merge with by USUBJID, if both have STUDYID, the value from vs_sorted will overwrite adsl_sorted if vs has it. But we retain STUDYID USUBJID. We should take STUDYID from adsl if available, else vs. Simpler to take from adsl.

We need to handle TRT01A, TRTSDT, TRTEDT from ADSL.

We need to handle PARAM, PARAMCD etc from VS.

We need to handle VS variables: VSTESTCD, VSTEST, VSSTRESN, VISIT, VISITNUM, VSDTC, VSSEQ

We need to handle reading: ensure column names uppercase.

We need to write helper to read SAS dataset.

We also need to handle output directory.

We need to produce text rendering: Use capture or manual.

We should write R script that does all.

Let's craft final R script.

