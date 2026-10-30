Need to produce R script that reproduces SAS program P019 qc_core.sas.

We have to read inputs from absolute paths given. The librefs: raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw, sdtm -> .../data/sdtm, adam -> .../data/adam

We need to locate files. Use OUT_DIR env var.

We need to implement 39? Actually many checks.

List checks:

- chk_adsl01: ADSL-001 ADSL USUBJID must be perfectly unique -> count duplicates groups having count>1. N_FAIL = number of duplicate USUBJID groups? In SAS: select count(*) from (select USUBJID from adam.adsl group by USUBJID having count(*)>1). So count of distinct USUBJIDs that are duplicated.

- chk_adae01: ADAE-001 AEENDT must be >= AESTDT where AEENDT < AESTDT and not missing both.

- chk_advs01: ADVS-001 Max 1 Baseline per Param -> count groups USUBJID,PARAMCD where ABLFL='Y' count>1

- chk_adlb01 similar

- chk_dm01: SDTM-001 USUBJID must be perfectly unique in DM

- chk_ae01: SDTM-002 Every AE.USUBJID exists in DM -> left join where d.USUBJID is null count

- chk_ex01 SDTM-003 EX

- chk_lb01 SDTM-004 LB

- chk_vs01 SDTM-005 VS

- chk_adsl02 ADSL-002 SAFFL=Y requires exposure in EX -> left join adsl to ex on USUBJID where SAFFL='Y' and e.USUBJID is null

- chk_adtte01 ADTTE-001 AVAL must be >=1 where missing(AVAL) or AVAL<1

- chk_adtte02 ADTTE-002 AVAL and CNSR agree with event or subject follow-up date: where missing(TRTSDT) or missing(CNSR) or CNSR not in (0,1) or (CNSR=0 and (missing(EVENTDT) or AVAL != (EVENTDT - TRTSDT)+1)) or (CNSR=1 and (not missing(EVENTDT) or missing(EOSDT) or AVAL != (EOSDT - TRTSDT)+1))

Note: dates are numeric SAS dates. In R, they will be Date objects. Difference in days.

- chk_adtte03 ADTTE-003 Event and follow-up dates must be chronologically valid: where missing(EOSDT) or EOSDT < TRTSDT or (not missing(EVENTDT) and (EVENTDT < TRTSDT or EVENTDT > EOSDT))

- chk_adsl03 ADSL-003 EOSDT must agree with DM.RFPENDTC: from adam.adsl a left join sdtm.dm d on USUBJID where d.USUBJID is null or missing(a.EOSDT) or a.EOSDT != input(substr(d.RFPENDTC,1,10), yymmdd10.)

So EOSDT is numeric date, RFPENDTC is string "YYYY-MM-DD". Need to parse first 10 chars as yymmdd10. That's YYYY-MM-DD.

- chk_adsl04 ADSL-004 AGE must equal completed years at informed consent: where missing(BRTHDT) or missing(RFICDT) or missing(AGE) or AGE != floor(yrdif(BRTHDT, RFICDT, 'AGE'))

Need to implement yrdif with AGE basis. SAS YRDIF with 'AGE' computes actual age: number of full years between dates, considering leap years. Equivalent to floor of age in years. SAS docs: YRDIF(start,end,'AGE') = age. It calculates exact age. Implementation: if month/day of end >= month/day of start then year diff else year diff -1. But need to handle Feb 29 etc. Simpler: use lubridate? But we can implement as: age = year(end) - year(start) - (month_day(end) < month_day(start) ?1:0). However SAS YRDIF AGE uses precise calculation: YRDIF with AGE basis returns (end - start)/365.2425? No, actually AGE basis is special: it computes age as integer years. Let's check SAS documentation: YRDIF(s,e,'AGE') computes person's age. It is same as INTCK('year',s,e) adjusted. So we can implement as floor(yrdif) where yrdif =? But SAS code does floor(yrdif(..., 'AGE')). However YRDIF with AGE already returns fractional? Let's see: SAS YRDIF with AGE returns number of years including fraction? Actually documentation: YRDIF with 'AGE' basis computes age in years by considering birth date. It returns decimal? Example: YRDIF('01JAN1960'd,'01JAN2000'd,'AGE') = 40. So floor of that =40. So we can just compute completed years as described.

We need to replicate exactly. Use: age_calc = as.integer(lubridate::interval(BRTHDT, RFICDT) %/% lubridate::years(1))? But interval division may be tricky.

Simpler: compute year diff and adjust.

Define function yrdif_age = function(start, end) { y1 <- year(start); y2 <- year(end); m1 <- month(start); d1 <- day(start); m2 <- month(end); d2 <- day(end); age <- y2 - y1; if (m2 < m1 || (m2==m1 && d2 < d1)) age <- age -1; return(age) } Then floor(yrdif) = age. But SAS YRDIF returns fractional? For AGE basis, it returns age with fraction based on? Actually SAS YRDIF AGE returns integer + fraction? Let's test mental: If birth 15JUN1990 and end 14JUN2020, YRDIF AGE = 29.xxx, floor =29. Our method gives 29. Good. If end 15JUN2020, YRDIF=30, floor 30, our method 30. So matches.

Edge: Feb 29 birthdays. SAS handles Feb 28? Need to approximate. We'll use lubridate logic.

- chk_advs02 ADVS-002 Baseline date has exactly one eligible source record: select a.USUBJID, a.PARAMCD from adam.advs a inner join (select USUBJID, PARAMCD, max(ASTDT) as BASE_DT from adam.advs where ASTDT <= TRTSDT and not missing(AVAL) group by USUBJID,PARAMCD) b on a.USUBJID=b.USUBJID and a.PARAMCD=b.PARAMCD and a.ASTDT=b.BASE_DT where not missing(a.AVAL) group by a.USUBJID,a.PARAMCD having count(*)>1 . Then count(*) as N_FAIL = number of groups with >1 same-day candidate.

- chk_adlb02 similar for ADLB

- chk_eg01 SDTM-006 Every EG.USUBJID exists in DM

- chk_eg02 SDTM-007 Required EG variables must exist: 12 - count(distinct upcase(name)) where libname='SDTM' and memname='EG' and upcase(name) in (list). So N_FAIL = missing required variables count. In R, we need to check columns existence in EG dataset. Count distinct upcase names that exist among required list. So N_FAIL = 12 - number present.

- chk_eg03 SDTM-008 EG date/value contract must be valid: where missing(EGDTC) or prxmatch('/^\d{4}-\d{2}-\d{2}$/', strip(EGDTC))=0 or missing(EGSTRESN) or missing(EGSTRESC)

- chk_eg04 SDTM-009 Raw EG variables must not leak to SDTM: count(*) from dictionary.columns where libname='SDTM' and memname='EG' and upcase(name) in ('SUBJECT','TEST_NAME','RESULT','UNIT','DATE') . So N_FAIL = number of those raw variables that exist in EG. In SAS, count(*) counts rows matching. So if none, 0.

- chk_mh01 SDTM-010 MH referential

- chk_mh02 SDTM-011 Required MH variables must exist: 6 - count distinct where in list STUDYID,DOMAIN,USUBJID,MHSEQ,MHTERM,MHSTDTC

- chk_mh03 SDTM-012 MH date/term contract: where missing(MHSTDTC) or prxmatch... or missing(MHTERM)

- chk_mh04 SDTM-013 Raw MH variables must not leak: count where name in SUBJECT,CONDITION,DIAGNOSIS_DATE

- chk_dm02 SDTM-014 DM reference dates complete and ordered: where missing(RFICDTC) or missing(RFSTDTC) or missing(RFENDTC) or missing(RFPENDTC) or prxmatch... for each or RFSTDTC < RFICDTC or RFENDTC < RFSTDTC or RFPENDTC != RFENDTC

Note: RF*DTC are character ISO dates. Comparison as strings works because YYYY-MM-DD lexicographically equals chronological.

- chk_sv01 SDTM-015 Every SV.USUBJID exists in DM

- chk_sv02 SDTM-016 USUBJID and VISITNUM must be unique: count(*) from (select USUBJID,VISITNUM from sdtm.sv group by USUBJID,VISITNUM having count(*) !=1) . Note having count(*) !=1 includes count>1 only, but if count=0 not in group. So same as >1. But SAS uses ne 1, same.

- chk_sv03 SDTM-017 SV dates valid and inside reference period: from sdtm.sv s inner join sdtm.dm d on USUBJID where missing(s.VISITNUM) or missing(s.VISIT) or missing(s.SVSTDTC) or missing(s.SVENDTC) or prxmatch... for SVSTDTC, SVENDTC or SVENDTC < SVSTDTC or SVSTDTC < d.RFSTDTC or SVENDTC > d.RFENDTC

- chk_ds01 SDTM-018 Exactly one final DS record per DM subject: count(*) from ( select d.USUBJID, count(s.USUBJID) as N_DS from sdtm.dm d left join sdtm.ds s on d.USUBJID=s.USUBJID group by d.USUBJID having calculated N_DS !=1 union all select s.USUBJID, count(*) as N_DS from sdtm.ds s left join sdtm.dm d on s.USUBJID=d.USUBJID where d.USUBJID is null group by s.USUBJID ) . So counts DM subjects with not exactly 1 DS record plus orphan DS subjects.

- chk_ds02 SDTM-019 DS date matches DM participation end: from sdtm.ds s inner join sdtm.dm d on USUBJID where missing(s.DSDECOD) or missing(s.DSSTDTC) or prxmatch... or s.DSSTDTC != d.RFPENDTC

- chk_dm03 SDTM-020 RACE uses supported CDISC terminology: where missing(RACE) or upcase(strip(RACE)) not in ('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')

- chk_dm04 SDTM-021 SITEID present and consistent with SUBJID: where missing(SITEID) or strip(SITEID) != scan(strip(SUBJID),1,'-')

SCAN with delimiter '-' first word.

- chk_adae02 ADAE-002 AOCCFL valid and restricted to TRTEMFL=Y: where (not missing(AOCCFL) and AOCCFL != 'Y') or (AOCCFL='Y' and TRTEMFL != 'Y')

- chk_adae03 ADAE-003 One AOCCFL=Y per subject and TEAE term: from (select USUBJID,AEDECOD from adam.adae group by USUBJID,AEDECOD having sum(case when TRTEMFL='Y' then 1 else 0 end)>0 and sum(case when AOCCFL='Y' then 1 else 0 end) !=1 ) count

- chk_cm01 SDTM-022 CM subject, medication, dose and dates must be valid: from sdtm.cm c left join sdtm.dm d on USUBJID where d.USUBJID is null or missing(CMTRT) or missing(CMDECOD) or (upcase(strip(CMTRT))='PARACETAMOL' and upcase(strip(CMDECOD)) != 'ACETAMINOPHEN') or missing(CMDOSE) or missing(CMDOSU) or missing(CMSTDTC) or prxmatch... CMSTDTC or (not missing(CMENDTC) and prxmatch... CMENDTC) or (not missing(CMENDTC) and CMENDTC < CMSTDTC) or (CMENRTPT='ONGOING' and not missing(CMENDTC)) or (CMENRTPT != 'ONGOING' and missing(CMENDTC))

Note: CMENRTPT comparison case-sensitive? SAS upcase? In this rule, they use c.CMENRTPT = 'ONGOING' directly, not upcase. But later rule uses upcase. We'll follow exactly: case-sensitive.

- chk_ds03 SDTM-023 DSDECOD uses supported disposition terminology: where upcase(strip(DSDECOD)) not in ('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT','ADVERSE EVENT','SCREEN FAILURE')

- chk_seq01 SDTM-024 Domain sequence keys must be unique per subject: union all of duplicates per domain: AE,EX,LB,VS,CM,MH,EG each group by USUBJID,SEQ having count>1, then count(*)

- chk_cm02 SDTM-025 Ongoing CM must be anchored to DM reference end: from sdtm.cm c inner join sdtm.dm d on USUBJID where upcase(strip(CMENRTPT)) not in ('','ONGOING') or (upcase(strip(CMENRTPT))='ONGOING' and ( not missing(CMENDTC) or missing(CMENTPT) or prxmatch... CMENTPT =0 or CMENTPT != d.RFENDTC )) or (upcase(strip(CMENRTPT)) != 'ONGOING' and not missing(CMENTPT))

Note: CMENTPT is character date? Should be ISO.

We need to implement all.

We need to read datasets. Paths: project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So adam dir = .../data/adam, sdtm = .../data/sdtm

List files: need to discover. Use haven::read_sas or read_xpt? The librefs point to directories with .sas7bdat files. Let's assume .sas7bdat.

We need to handle reading: try list.files.

We also need to handle variable names uppercase. haven reads with original case, but we can convert to uppercase.

We need to handle missing values: SAS missing for character is "" or NA, for numeric is NA.

We need to implement helpers: is_missing for character: is.na(x) | trimws(x)==""

For numeric: is.na(x)

For date variables: In ADaM, dates like TRTSDT, EOSDT, BRTHDT, RFICDT, etc are numeric SAS dates (days since 1960-01-01). When read via haven, they will be Date objects? haven converts SAS dates to Date? Possibly. Need to handle both numeric and Date.

Similarly SDTM DTC variables are character.

We need to implement yrdif, and date parsing.

We need to produce qc_report dataset with columns CHECK_ID, DOMAIN, RULE, N_FAIL, STATUS

Order as in SAS set statement: chk_adsl01 chk_adae01 chk_advs01 chk_adlb01 chk_advs02 chk_adlb02 chk_dm01 chk_ae01 chk_ex01 chk_lb01 chk_vs01 chk_adsl02 chk_adsl03 chk_adsl04 chk_adtte01 chk_adtte02 chk_adtte03 chk_eg01 chk_eg02 chk_eg03 chk_eg04 chk_mh01 chk_mh02 chk_mh03 chk_mh04 chk_dm02 chk_sv01 chk_sv02 chk_sv03 chk_ds01 chk_ds02 chk_dm03 chk_dm04 chk_adae02 chk_adae03 chk_cm01 chk_ds03 chk_seq01 chk_cm02

That's 38 checks? Let's count: 1-4, plus 2 (advs02,adlb02) =6, + dm01=7, + ae01,ex01,lb01,vs01=11, + adsl02,03,04=14, + adtte01,02,03=17, + eg01-04=21, + mh01-04=25, + dm02=26, + sv01-03=29, + ds01,02=31, + dm03,04=33, + adae02,03=35, + cm01=36, + ds03=37, + seq01=38, + cm02=39. So 39 rows.

We need to produce P019.lst text rendering with 156 numbers? The expected output says 156 numbers. That likely means the table has many numeric values. But our qc_report has 39 rows * maybe N_FAIL column numbers =39 numbers, not 156. Maybe they count all numbers in text? Could be that P019.lst is the printed qc_report with 39 rows each with N_FAIL (39 numbers) plus maybe other numbers? But spec says 156 numbers. Could be that the text rendering includes counts? Let's just produce a text file that contains the table.

We need to write OUT_DIR/qc_report.csv and also P019.lst text and csv.

The spec says: Declared output types: table;listing;dataset . Expected outputs: P019.lst (table/listing text, 156 numbers) -> write a text rendering and CSV of same table, dataset qc_report -> write OUT_DIR/qc_report.csv

So P019.lst is likely the same as qc_report printed. We'll create P019.txt and P019.csv? But expected name is P019.lst . The instruction says write each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... and each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) . So for P019.lst, we should create P019.csv and P019.txt? Or qc_report.csv and qc_report.txt? The spec says dataset qc_report -> write OUT_DIR/qc_report.csv . And P019.lst -> write text rendering and CSV of same table. So we need to produce both: qc_report.csv and P019.csv + P019.txt (or P019.lst.txt). Safer to produce all variants: qc_report.csv, qc_report.txt, P019.csv, P019.txt, P019.lst, P019.lst.csv, P019.lst.txt

We'll do that.

We need to implement reading.

Let's craft R script.

Potential pitfalls: file paths with spaces, need to handle.

We need to handle dictionary.columns checks: we need to inspect column names of EG, MH datasets.

For chk_eg02: 12 - count(distinct upcase(name)) where name in required list. So if EG has all 12, N_FAIL=0. If missing some, N_FAIL = number missing.

Similarly chk_eg04: count(*) where name in raw list. So if EG contains any of those raw variables, N_FAIL = count of them present. Note: dictionary.columns counts distinct? In SAS, count(*) counts rows matching, each column is one row. So if EG has SUBJECT column, count=1.

Similarly for MH.

We need to implement strip, upcase, prxmatch.

prxmatch('/^\d{4}-\d{2}-\d{2}$/', strip(EGDTC)) =0 means not matching. In R, use grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(x))

Missing checks: missing(EGDTC) means is.na or trimws==""

We need to handle EGSTRESN missing: numeric NA.

We need to handle CM checks: CMTRT, CMDECOD, CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT, CMENTPT

We need to handle DM RACE, SITEID, SUBJID

We need to handle DS DSDECOD, DSSTDTC

We need to handle SV VISITNUM, VISIT, SVSTDTC, SVENDTC

We need to handle ADSL SAFFL, EOSDT, BRTHDT, RFICDT, AGE

We need to handle ADTTE TRTSDT, CNSR, EVENTDT, EOSDT, AVAL

We need to handle ADVS ABLFL, PARAMCD, ASTDT, TRTSDT, AVAL

We need to handle ADLB similar

We need to handle ADAE AEENDT, AESTDT, AOCCFL, TRTEMFL, AEDECOD

We need to handle SDTM DM RFICDTC etc.

We need to handle reading all datasets.

Let's write helper functions.

We also need to handle date arithmetic: AVAL ne (EVENTDT - TRTSDT)+1 . In SAS, dates are numeric days, difference is days. In R, if Date objects, difference is difftime. Use as.numeric(EVENTDT - TRTSDT) +1

Similarly for EOSDT.

We need to handle missing logic: missing(EVENTDT) etc.

We need to handle input(substr(d.RFPENDTC,1,10), yymmdd10.) : parse first 10 chars as YYYY-MM-DD. Use as.Date(substr(...,1,10), format="%Y-%m-%d")

We need to handle yrdif.

We need to handle scan(strip(SUBJID),1,'-'): split by '-' and take first token.

We need to handle upcase(strip(RACE)) etc.

We need to handle count distinct groups.

We need to ensure we handle case where datasets may be empty or missing.

We need to locate files: list.files for adam and sdtm directories.

We should try to read both .sas7bdat and .xpt.

We need to handle validvarname=upcase: SAS forces variable names to uppercase. So in R we should convert all column names to uppercase.

We need to handle haven labelled vectors: convert to appropriate types. Use haven::as_factor? But we want raw values. For character, haven returns character; for numeric, numeric; for dates, Date.

We need to handle haven's tagged NA.

We need to write output.

We need to ensure OUT_DIR exists.

We need to produce text rendering: simple table with title.

We need to ensure 156 numbers appear? The qc_report has 39 N_FAIL numbers, plus maybe other numbers? But spec says 156 numbers in P019.lst. Could be that N_FAIL values are numbers, plus maybe other columns? Let's count: If each row has N_FAIL (39 numbers) and maybe CHECK_ID contains numbers? But 156 suggests 4 numbers per row *39 =156. So maybe they count each numeric token in the text rendering including row numbers? We'll just ensure our text file contains the table with N_FAIL values.

We need to implement all checks robustly, handling missing datasets gracefully (if dataset not found, treat as 0? But better to error).

We need to write R script that does all.

Let's craft final R code.

