We need to produce R script that reproduces outputs: a listing (P019.lst) which is the printed report (table) and dataset qc_report.csv. Also need to write text rendering and CSV of the table. The script must read data from libraries: raw, sdtm, adam directories under project_path. The macro variables give paths. The OUT_DIR env var is where to write outputs.

We need to implement each check as in SAS, using dplyr etc. Need to be careful with SAS semantics: missing values, date handling, character case sensitivity, etc.

Assume data files are SAS datasets (.sas7bdat) in those directories. Use haven::read_sas to read .sas7bdat. The path: e.g., adam.adsl corresponds to file D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.sas7bdat (maybe). We'll construct paths: file.path(project_path, "data", "adam", "adsl.sas7bdat").

Similarly for sdtm.*.

Implement helper functions: is_missing (is.na). For date strings, SAS date variables are numeric days since 1960-01-01. In ADaM, dates may be numeric. In SAS they compare numeric dates directly. In R, after reading with haven, dates become class labelled? Haven reads SAS date as Date? I think haven converts SAS date numeric to Date class (days since 1960-01-01). So we can compare directly.

For character variables, keep as character.

Implement each check:

1 chk_adsl01: count duplicate USUBJID in adam.adsl. N_FAIL = number of duplicate records (i.e., number of rows where USUBJID appears more than once). In SAS they select from (select USUBJID from adam.adsl group by USUBJID having count(*) > 1). That returns distinct USUBJID that are duplicated, then count(*). So N_FAIL = number of distinct USUBJID with duplicates. We'll compute similarly.

2 chk_adae01: count rows where AEENDT < AESTDT and both not missing. N_FAIL = number of such rows.

3 chk_advs01: count distinct USUBJID, PARAMCD where ABLFL='Y' and count >1. So number of (USUBJID, PARAMCD) combos with >1 baseline. N_FAIL = count of such combos.

4 chk_adlb01: same for adlb.

5 chk_dm01: duplicate USUBJID in sdtm.dm.

6 chk_ae01: left join sdtm.ae to dm, count rows where dm missing. Equivalent to rows in ae with no matching dm. N_FAIL = count.

7 chk_ex01: same for ex.

8 chk_lb01: same for lb.

9 chk_vs01: same for vs.

10 chk_adsl02: SAFFL='Y' requires exposure in EX. Count adsl rows where SAFFL='Y' and no matching ex record. So left join adsl to ex, where e.USUBJID is null.

11 chk_adtte01: count rows where missing AVAL or AVAL <1.

12 chk_adtte02: complex condition. We'll translate.

Condition: missing(TRTSDT) or missing(CNSR) or CNSR not in (0,1) or (CNSR=0 and (missing(EVENTDT) or AVAL != (EVENTDT - TRTSDT) + 1)) or (CNSR=1 and (not missing(EVENTDT) or missing(EOSDT) or AVAL != (EOSDT - TRTSDT) + 1))

Need to handle dates as Date. Subtraction yields days difference. So (EVENTDT - TRTSDT) returns difftime in days? In R, Date subtraction returns numeric days. So compute as as.numeric(EVENTDT - TRTSDT). Then +1.

13 chk_adtte03: where missing(EOSDT) or EOSDT < TRTSDT or (EVENTDT not missing and (EVENTDT < TRTSDT or EVENTDT > EOSDT))

14 chk_adsl03: join adsl to dm, where dm missing OR missing EOSDT OR EOSDT != input(substr(d.RFPENDTC,1,10), yymmdd10.). RFPENDTC is character date string? In SDTM, RFPENDTC is character ISO date. So we need to parse first 10 chars to Date. Use as.Date(substr(RFPENDTC,1,10), "%Y-%m-%d"). Then compare to EOSDT (Date). N_FAIL count rows where condition true.

15 chk_adsl04: AGE must equal floor(yrdif(BRTHDT, RFICDT, 'AGE')). SAS yrdif returns age in years with fraction? In R we can compute as floor(as.numeric(difftime(RFICDT, BRTHDT, units="days"))/365.25). But SAS yrdif with 'AGE' returns integer age (completed years). We'll approximate using floor(years). Use lubridate::time_length(interval(BRTHDT, RFICDT), "years") then floor. We'll compute age_calc = floor(time_length(interval(BRTHDT, RFICDT), "years")). Then compare to AGE.

16 chk_advs02: baseline date selection uniqueness. Complex. We'll implement as SAS: inner join advs a with subquery b that selects USUBJID, PARAMCD, max(ASTDT) as BASE_DT where ASTDT <= TRTSDT and not missing(AVAL). Then join on those fields, where not missing a.AVAL, group by USUBJID, PARAMCD having count(*) >1. N_FAIL = count of such groups.

Similarly for adlb02.

17 chk_eg01: referential integrity EX? Actually EG. Count EG rows where no matching DM.

18 chk_eg02: required variables must exist. Use dictionary.columns: In R we can get column names of sdtm.eg dataset. Count missing required variables. N_FAIL = 12 - count(distinct upcase(name)) where name in required list. So compute required list, intersect with actual column names (case-insensitive). N_FAIL = length(required) - length(intersect).

19 chk_eg03: EG date/value contract. Conditions: missing EGDTC OR not matching regex /^\d{4}-\d{2}-\d{2}$/ OR missing EGSTRESN OR missing EGSTRESC. Count rows where any condition true.

20 chk_eg04: raw import variables must not leak: count rows where any of those variable names exist in EG dataset. Actually they query dictionary.columns where name in list. So they count number of columns that are in that list. N_FAIL = count of such columns. So we need to count how many of those raw variable names are present in EG dataset. So N_FAIL = sum of presence.

21 chk_mh01: referential integrity for MH.

22 chk_mh02: required variables count similar to EG.

23 chk_mh03: date/term contract: missing MHSTDTC OR not matching regex OR missing MHTERM.

24 chk_mh04: raw variables leak count.

25 chk_dm02: DM reference dates complete and ordered. Conditions: missing any of RFICDTC, RFSTDTC, RFENDTC, RFPENDTC OR regex not match OR RFSTDTC < RFICDTC OR RFENDTC < RFSTDTC OR RFPENDTC != RFENDTC. Count rows where any condition true.

26 chk_sv01: SV referential integrity.

27 chk_sv02: USUBJID and VISITNUM must be unique. Count rows where duplicate combos exist (i.e., groups with count !=1). In SAS they select groups having count(*) ne 1. So N_FAIL = number of such groups.

28 chk_sv03: date validity and inside reference period. Conditions: missing VISITNUM or VISIT or SVSTDTC or SVENDTC OR regex not match for dates OR SVENDTC < SVSTDTC OR SVSTDTC < d.RFSTDTC OR SVENDTC > d.RFENDTC. Count rows where any condition true.

29 chk_ds01: exactly one final DS per DM subject. Complex union. We'll compute two sets: For each DM subject, count DS rows; if count !=1 then fail. Also any DS row with no matching DM (orphan) fails. So N_FAIL = number of subjects failing? In SAS they union both sets and then count rows. The first part selects d.USUBJID, count(s.USUBJID) as N_DS from dm left join ds, group by d.USUBJID having calculated N_DS ne 1. That returns rows for each DM subject where N_DS !=1. The second part selects s.USUBJID, count(*) as N_DS from ds left join dm where dm missing, group by s.USUBJID. So orphan DS subjects. So total rows = number of subjects failing. We'll compute similarly.

30 chk_ds02: DS date matches DM participation end. Conditions: missing DSDECOD or DSSTDTC or regex not match or DSSTDTC != d.RFPENDTC. Count rows where condition true.

31 chk_dm03: RACE uses supported terminology. Count rows where missing RACE or upcase(RACE) not in list.

32 chk_dm04: SITEID present and consistent with SUBJID. Condition: missing SITEID OR strip(SITEID) != scan(SUBJID,1,'-'). In R, split SUBJID by '-' and take first part. Count rows where condition true.

33 chk_adae02: AOCCFL valid and restricted to TRTEMFL=Y. Conditions: (not missing AOCCFL and AOCCFL != 'Y') OR (AOCCFL='Y' and TRTEMFL!='Y'). Count rows where condition true.

34 chk_adae03: One AOCCFL=Y per subject and TEAE term. For each USUBJID, AEDECOD, compute sum of TRTEMFL='Y' and sum of AOCCFL='Y'. If sum(TRTEMFL='Y')>0 and sum(AOCCFL='Y') !=1 then fail. Count number of groups failing.

35 chk_cm01: many conditions. Count rows where any condition true.

36 chk_ds03: DSDECOD uses supported terminology. Count rows where upcase(DSDECOD) not in list.

37 chk_seq01: sequence uniqueness across domains. For each domain, count distinct USUBJID, SEQ combos where count>1. N_FAIL = total number of such duplicate combos across all domains.

38 chk_cm02: Ongoing CM timing must have explicit observed reference point. Complex conditions. We'll translate.

After creating all check data frames with columns CHECK_ID, DOMAIN, RULE, N_FAIL, we need to combine them into qc_report data frame, add STATUS = if N_FAIL==0 PASS else FAIL.

Write qc_report.csv to OUT_DIR/qc_report.csv.

Also produce listing: P019.lst (text rendering) and CSV of same table. The listing is the printed report with columns CHECK_ID DOMAIN RULE N_FAIL STATUS, no obs numbers. We'll write a text file with formatted table. Need to match SAS formatting: numbers maybe no decimals. We'll just write using format similar to SAS default: N_FAIL as integer, maybe right-aligned. We'll produce using write.table with sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE.

Also write CSV of table body: same as qc_report.csv? Already done. But they also want CSV of the table body for listing. Could be same file name "P019.csv"? Not specified. We'll write P019.csv as same as qc_report.csv? The spec: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body. So for listing P019.lst we need P019.txt (text) and P019.csv (CSV). We'll name listing base "P019". So write out file.path(out, "P019.txt") and "P019.csv". The CSV will be same as qc_report.csv but maybe only the table body (same columns). We'll write same.

Also need to write dataset qc_report.csv (already). So we will have two CSVs: qc_report.csv and P019.csv (duplicate). That's fine.

Implement round-half-up helper if needed, but not needed for counts.

Implement regex check using grepl.

Implement reading dictionary columns: we can get column names from dataset; no need to query dictionary.

Implement missing detection: is.na for numeric and character.

Implement date regex: pattern "^\\d{4}-\\d{2}-\\d{2}$".

Implement function to parse date strings: as.Date(str, "%Y-%m-%d").

Now produce script.

Edge Cases: Some datasets may have variable types as labelled; we can convert to character via as.character.

We'll ensure all character columns are trimmed (strip) using trimws.

Implement case-insensitive matching for variable names.

Now produce final R script.

