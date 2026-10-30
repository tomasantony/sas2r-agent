Need to translate SAS program to R. Must produce outputs: adsl.csv, advs.csv, advs_base.csv, and P087.lst text rendering + CSV.

We need to replicate SAS logic exactly.

Steps:

1. Create mock SDTM VS data: 18 rows as per datalines. Need to parse.

2. Create mock ADSL subset: 6 rows.

3. Merge and create base ADVS structure: advs_base.

Details: vs has STUDYID, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU, VISIT, VISITNUM, VSDTC.

adsl has STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT.

Merge by STUDYID USUBJID inner join where both present (a and b). Since all vs subjects are in adsl, all 18 rows remain.

Then derive PARAMCD = VSTESTCD, PARAM = VSTEST, PARCAT1='VITAL SIGNS', PARAMN mapping, AVAL=VSSTRESN, AVALU=VSSTRESU, ADT from VSDTC yymmdd10, ADY = ADT - TRTSDT +1 if >= else ADT-TRTSDT, AVISIT=VISIT, AVISITN=VISITNUM, ATPT='', ATPTN=NA.

Drop original VS vars.

Step 4: Derive baseline flag. SAS does:

proc sort by USUBJID PARAMCD ADT
data advs_base set advs_base by USUBJID PARAMCD; if not missing ADT and TRTSDT then if ADT <= TRTSDT then _pre_trt=1 else 0
Then sort descending ADT, then if first.PARAMCD and _pre_trt=1 then ABLFL='Y' else ''
Then sort by USUBJID PARAMCD ADT

Note: first.PARAMCD after sorting by USUBJID PARAMCD descending ADT means within each USUBJID PARAMCD group, first is latest ADT. So baseline is last pre-treatment record (max ADT <= TRTSDT). Since each subject has only one baseline record per param, that will be flagged.

Need to replicate.

Step 5: Derive BASE, CHG, PCHG

Get baseline values: where ABLFL='Y', BASE=AVAL keep USUBJID PARAMCD BASE

Merge back by USUBJID PARAMCD, derive CHG, PCHG, ANL01FL = 'Y' if ABLFL != 'Y' and not missing AVAL and ADT > TRTSDT else ''

Step 6: Add derived records (maximum post-baseline)

proc means where ABLFL ne 'Y' and ADT > TRTSDT by USUBJID PARAMCD var AVAL output max=AVAL

Then merge with baseline? Actually code: merge _max_post(in=a) advs(where=(ABLFL='Y') keep=ABLFL USUBJID PARAMCD BASE) by USUBJID PARAMCD; if a; DTYPE='MAXIMUM' AVISIT='Maximum Post-Baseline' AVISITN=99 if not missing AVAL and BASE then CHG=AVAL-BASE

Note: This merge will bring BASE for those with baseline. But what about subjects without baseline? They still have max record but BASE missing.

Also need to consider that _max_post after proc means has USUBJID PARAMCD AVAL. Then merging with baseline adds BASE and ABLFL? But they keep ABLFL USUBJID PARAMCD BASE from advs where ABLFL='Y'. So ABLFL column will be overwritten? In SAS merge, if both have same variable, second dataset's value overwrites? Actually merge with two datasets: _max_post has USUBJID PARAMCD AVAL, second has USUBJID PARAMCD BASE ABLFL. So resulting has ABLFL from second if matched, else missing. But they don't explicitly set ABLFL for max records; it will be missing or 'Y' if matched? Wait they keep ABLFL from baseline dataset, so for max records that have a baseline, ABLFL will be 'Y' (since baseline record had ABLFL='Y'). That seems odd but that's SAS code. However later they set advs = set advs _max_post; So max records will have ABLFL='Y' if they had baseline? Let's check SAS logic: they merge _max_post with advs(where ABLFL='Y' keep ABLFL USUBJID PARAMCD BASE). So for each USUBJID PARAMCD that has a max post-baseline, they try to find baseline. If found, they bring BASE and ABLFL='Y'. So max records will have ABLFL='Y' incorrectly? But maybe they intend to keep BASE only, but inadvertently also bring ABLFL. Need to replicate exactly.

Also need to consider other variables for max records: they will have missing STUDYID, TRTP etc? Actually _max_post only has USUBJID PARAMCD AVAL plus BASE ABLFL DTYPE AVISIT AVISITN CHG. But when they do set advs _max_post, the resulting dataset will have missing values for other columns for those new rows. However SAS proc means by USUBJID PARAMCD will keep those by vars, but not STUDYID etc. So max records will be incomplete. Is that intended? Let's see SAS code: proc means by USUBJID PARAMCD output max=AVAL. So _max_post has USUBJID PARAMCD AVAL. Then merge with baseline to get BASE. Then they set DTYPE etc. They don't populate STUDYID, PARAM, etc. But when they do data advs; set advs _max_post; run; The _max_post rows will have missing STUDYID, PARAM, etc. However maybe they expect those to be missing? But maybe they should have been populated via merge with advs? Let's check typical ADaM: maximum post-baseline should have same PARAM etc. But SAS code as given is incomplete; but we must replicate exactly.

However we need to check expected outputs: dataset advs, advs_base, adsl. We need to infer what expected CSVs contain. The task says P087.lst has 296 numbers. That suggests listing output.

We need to produce adsl.csv, advs.csv, advs_base.csv with correct columns and formatting.

We also need to produce P087.lst text rendering and CSV.

We need to decide what P087.lst should contain. In SAS program, they do proc contents and proc print obs=20. So listing likely is print of advs first 20 rows.

We need to create a text file that contains same numbers SAS output shows. We need to generate a reasonable listing.

We must also ensure dates formatted as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS.

We need to handle TRTSDT, TRTEDT, ADT as Date.

We need to implement SAS date handling: TRTSDT input as date9. e.g., 15JAN2024 -> 2024-01-15.

We need to create out directory.

We need to write CSVs with write.csv(..., na="").

We need to ensure variable names match SAS dataset variable names (case-insensitive). Use same case as SAS: STUDYID, USUBJID, etc.

We need to produce advs_base.csv and advs.csv and adsl.csv.

We need to also produce P087.lst.txt and P087.lst.csv? The instruction says each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers and as CSV of table body. Expected outputs list says P087.lst (table/listing text, 296 numbers) -> write text rendering and CSV of same table. So we need to produce P087.lst.txt and P087.lst.csv? Or P087.txt? The spec says write every output into directory with <name>.txt and <name>.csv. For dataset outputs, just CSV. For table/listing, both txt and csv. The expected output name is P087.lst -> we should create P087.lst.txt and P087.lst.csv? Or P087.txt? Safer to create both P087.lst.txt and P087.txt and P087.csv etc. But spec says file.path(out, "<name>.txt") where <name> is output name. So for P087.lst, <name> is "P087" maybe? However expected outputs list says dataset adsl -> write OUT_DIR/adsl.csv, dataset advs -> advs.csv, dataset advs_base -> advs_base.csv, and P087.lst -> write text rendering and CSV. So we should create P087.lst.txt and P087.lst.csv, plus also P087.txt/csv for safety.

We need to ensure script runs non-interactively.

We need to implement SAS semantics: sorting, etc.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- Create vs data frame manually.

- Create adsl data frame.

- Merge.

- Derive advs_base.

- Derive baseline flag.

- Derive BASE, CHG, PCHG, ANL01FL.

- Derive max post-baseline.

- Add ASEQ.

- Write CSVs.

- Create listing text.

We need to handle dates: use as.Date.

We need to ensure numeric values unrounded unless SAS rounds them.

We need to handle missing TRTEDT for USUBJID 1005: should be NA.

We need to handle ADT: from VSDTC.

We need to handle ADY calculation.

We need to handle PARAMN mapping.

We need to handle ATPT, ATPTN.

We need to handle ABLFL, ANL01FL.

We need to handle DTYPE for max records.

We need to handle ASEQ: by STUDYID USUBJID PARAMCD AVISITN ADT sorted, then by STUDYID USUBJID group, ASEQ increments.

We need to handle labels: not needed for CSV but could store as attribute.

We need to produce P087.lst text: proc print obs=20. So we need to print first 20 rows of advs sorted as final.

We need to count numbers: 296 numbers in listing. Let's estimate: 20 rows * maybe ~15 numeric columns = 300 numbers. So plausible.

We need to create text file with table.

We also need to create CSV of table body: same as printed rows.

We should also ensure advs_base.csv contains columns after step 4 but before step 5? Actually advs_base after baseline flag derivation, before BASE merge. In SAS, advs_base is created in step 3 and then modified in step 4 (baseline flag). So final advs_base after step 4 is what should be output. It has no BASE, CHG, etc. It has ABLFL.

We need to capture that.

Similarly adsl is as created in step 2.

We need to ensure column order matches SAS.

Let's define column order for adsl: STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT

For advs_base: STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT, PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, ADT, ADY, ATPT, ATPTN, AVISIT, AVISITN, ABLFL

But in SAS they dropped VSTESTCD etc and kept others. Let's list all variables present after merge and derivation:

From vs: STUDYID, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU, VISIT, VISITNUM, VSDTC

From adsl: STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT

After data advs_base merge, they define new vars: PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, ADT, ADY, ATPT, ATPTN, AVISIT, AVISITN, ABLFL, ANL01FL? Actually ANL01FL defined later in advs, not advs_base. So advs_base has ABLFL but not ANL01FL, BASE, CHG, PCHG, DTYPE, ASEQ.

They also drop VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU VISIT VISITNUM VSDTC

So remaining: STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT, PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, ADT, ADY, ATPT, ATPTN, AVISIT, AVISITN, ABLFL

Check also VSSEQ kept? They didn't drop VSSEQ, so it remains.

For advs final: after merging baseline and adding max records and ASEQ, variables: STUDYID, USUBJID, VSSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT, PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, ADT, ADY, ATPT, ATPTN, AVISIT, AVISITN, ABLFL, BASE, CHG, PCHG, ANL01FL, DTYPE, ASEQ

Plus maybe other vars like _pre_trt dropped.

We need to ensure we replicate.

For max records, many vars will be NA, but we should still have columns.

We need to handle sorting for ASEQ: proc sort by STUDYID USUBJID PARAMCD AVISITN ADT; then data advs set advs by STUDYID USUBJID; if first.USUBJID then ASEQ=0; ASEQ+1;

So ASEQ is sequential within USUBJID ordered by PARAMCD AVISITN ADT.

We need to implement.

We also need to handle date formatting for CSV: dates as YYYY-MM-DD.

We need to write CSVs with na="".

We need to create text rendering.

Let's craft R code.

