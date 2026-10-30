Need to translate SAS program to R. Must read inputs from absolute paths given. Librefs: raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw, sdtm -> .../data/sdtm, adam -> .../data/adam. Need to list files.

We need to produce qc_report.csv and P019.lst text rendering.

We must implement all 39? Actually 32? Let's count checks: chk_adsl01, chk_adae01, chk_advs01, chk_adlb01, chk_advs02, chk_adlb02, chk_dm01, chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04, chk_adtte01, chk_adtte02, chk_adtte03, chk_eg01, chk_eg02, chk_eg03, chk_eg04, chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02, chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02, chk_dm03, chk_dm04, chk_adae02, chk_adae03, chk_cm01, chk_ds03, chk_seq01, chk_cm02. That's 39 checks.

Need to replicate SAS logic exactly.

We need to explore data files to understand column names and types.

We must write R script that reads from those directories.

We need to handle SAS semantics: missing values, date handling.

Key tricky parts:

- ADSL-001: duplicate USUBJID
- ADAE-001: AEENDT < AESTDT and not missing
- ADVS-001: max 1 baseline per param
- ADLB-001 similar
- SDTM-001 duplicate DM
- SDTM-002 AE referential
- SDTM-003 EX referential
- SDTM-004 LB referential
- SDTM-005 VS referential
- ADSL-002 SAFFL=Y requires EX
- ADTTE-001 AVAL missing or <1
- ADTTE-002: missing TRTSDT or missing CNSR or CNSR not in 0,1 or (CNSR=0 and (missing EVENTDT or AVAL != (EVENTDT - TRTSDT)+1)) or (CNSR=1 and (not missing EVENTDT or missing EOSDT or AVAL != (EOSDT - TRTSDT)+1))
- ADTTE-003: missing EOSDT or EOSDT < TRTSDT or (not missing EVENTDT and (EVENTDT < TRTSDT or EVENTDT > EOSDT))
- ADSL-003: EOSDT must agree with DM.RFPENDTC: where d.USUBJID is null or missing(a.EOSDT) or a.EOSDT != input(substr(d.RFPENDTC,1,10), yymmdd10.)
  Note: RFPENDTC is string like "YYYY-MM-DD". Need to parse first 10 chars as yymmdd10. That's YYYY-MM-DD. So convert to Date.
  EOSDT is numeric SAS date? In ADaM, EOSDT likely numeric date. Need to check.
- ADSL-004: AGE must equal floor(yrdif(BRTHDT, RFICDT, 'AGE')). Need to implement SAS YRDIF with 'AGE' basis. SAS YRDIF with AGE computes exact age in years. Definition: SAS docs: YRDIF(start,end,'AGE') computes age. Algorithm: uses actual days? Need to replicate. Common approximation: floor((RFICDT - BRTHDT)/365.2425) ??? But SAS AGE method is specific: It calculates number of full years between dates, considering leap years. Equivalent to: age = year(RFICDT)-year(BRTHDT) - (month/day of RFICDT < month/day of BRTHDT ?1:0). That's completed years. Let's verify. SAS YRDIF with 'AGE' returns fractional? Actually floor(yrdif(..., 'AGE')) should equal completed years. So we can compute age as integer years difference.

We need to check data to see BRTHDT, RFICDT types.

- ADVS-002: baseline date has exactly one eligible source record. Logic: select a.USUBJID, a.PARAMCD from adam.advs a inner join (select USUBJID, PARAMCD, max(ASTDT) as BASE_DT from adam.advs where ASTDT <= TRTSDT and not missing(AVAL) group by ...) b on ... and a.ASTDT = b.BASE_DT where not missing(a.AVAL) group by ... having count(*) >1 . So count distinct? Actually count(*) >1 means more than one record on same max date with non-missing AVAL.

Similarly ADLB-002.

- SDTM-006 EG referential
- SDTM-007 EG required variables must exist: 12 - count(distinct upcase(name)) where name in list. So N_FAIL = 12 - count present. So if all 12 present, N_FAIL 0. Need to check dictionary.columns equivalent: we need to inspect EG dataset column names. If missing columns, N_FAIL >0.

- SDTM-008 EG date/value contract: missing EGDTC or not matching regex ^\d{4}-\d{2}-\d{2}$ or missing EGSTRESN or missing EGSTRESC

- SDTM-009 Raw EG variables must not leak: count(*) where name in ('SUBJECT','TEST_NAME','RESULT','UNIT','DATE') . So if any of those columns exist in EG, N_FAIL = count of them present? Actually query: select count(*) as N_FAIL from dictionary.columns where libname='SDTM' and memname='EG' and upcase(name) in (...) . So N_FAIL is number of leaking columns present.

Similarly MH checks.

- SDTM-014 DM reference dates complete and ordered: missing RFICDTC etc or regex fail or RFSTDTC < RFICDTC or RFENDTC < RFSTDTC or RFPENDTC != RFENDTC

Note RF*DTC are character ISO dates.

- SDTM-015 SV referential
- SDTM-016 USUBJID and VISITNUM must be unique: count(*) where group having count(*) !=1 . Note SAS having count(*) ne 1 includes duplicates and also? If count(*) !=1, duplicates only, since count(*) >=1. So same as >1.

- SDTM-017 SV dates valid and inside reference period: inner join DM, where missing VISITNUM etc or regex fail or SVENDTC < SVSTDTC or SVSTDTC < RFSTDTC or SVENDTC > RFENDTC

- SDTM-018 Exactly one final DS record per DM subject: union of two counts: left join DM->DS having N_DS !=1, plus DS left join DM where DM null group by. So counts subjects with 0 or >1 DS, plus orphan DS.

- SDTM-019 DS date matches DM participation end: missing DSDECOD or DSSTDTC or regex fail or DSSTDTC != RFPENDTC

- SDTM-020 RACE terminology

- SDTM-021 SITEID present and consistent with SUBJID: missing SITEID or strip(SITEID) != scan(strip(SUBJID),1,'-')

- ADAE-002 AOCCFL valid and restricted to TRTEMFL=Y: (not missing AOCCFL and AOCCFL != 'Y') or (AOCCFL='Y' and TRTEMFL != 'Y')

- ADAE-003 One AOCCFL=Y per subject and TEAE term: group by USUBJID, AEDECOD having sum(TRTEMFL='Y')>0 and sum(AOCCFL='Y') !=1

- SDTM-022 CM subject, medication, dose and dates must be valid: left join DM where DM null or missing CMTRT or CMDECOD or (upcase CMTRT='PARACETAMOL' and upcase CMDECOD != 'ACETAMINOPHEN') or missing CMDOSE or CMDOSU or missing CMSTDTC or regex fail for CMSTDTC or (not missing CMENDTC and regex fail) or (not missing CMENDTC and CMENDTC < CMSTDTC) or (CMENRTPT='ONGOING' and not missing CMENDTC) or (CMENRTPT != 'ONGOING' and missing CMENDTC)

Note CMENRTPT handling: SAS missing is '' . So condition: (c.CMENRTPT = 'ONGOING' and not missing(c.CMENDTC)) or (c.CMENRTPT ne 'ONGOING' and missing(c.CMENDTC))

- SDTM-023 DSDECOD terminology

- SDTM-024 Domain sequence keys must be unique per subject: union all duplicates across AE, EX, LB, VS, CM, MH, EG

- SDTM-025 Ongoing CM must be anchored: inner join DM where upcase(CMENRTPT) not in ('','ONGOING') or (ONGOING and (not missing CMENDTC or missing CMENTPT or regex fail or CMENTPT != RFENDTC)) or (not ONGOING and not missing CMENTPT)

Need to implement all.

We need to inspect actual data files to ensure column names uppercase.

Let's list directories.