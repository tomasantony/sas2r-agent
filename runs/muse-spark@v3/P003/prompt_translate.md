Translate the SAS program below to R.

Program id: P003   file: P003_at14-5-02.sas   source: cdisc-org/sdtm-adam-pilot-project   family: pilot
Declared output types: table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P003
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P003
  &_bench_t0. = %sysfunc(datetime())

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  sdtmnew -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm
  adamnew -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
  _x -> &path./&ds..xpt

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P003.lst (table/listing text, 104 numbers)  -> write a text rendering and CSV of the same table

--- family environment (pilot) that ran before the program ---
/* original CDISC pilot programs: setup.sas replacement defines sdtmnew/adamnew */
%include "&root.\programs\sas\macros\cdisc-pilot\setup.sas";

/* ---- included: setup.sas ---- */
/* setup.sas for the original CDISC pilot programs (P002 adae.sas, P003 at14-5-02.sas).
   The pilot submission package does not ship its setup.sas, so this replacement defines
   the librefs the programs expect (sdtmnew, adamnew) on top of the local pilot data.
   The xpt files are SAS transport files; load the ones needed into a SAS library first. */
%let root = D:\meta_hackathon\sas2r-agent;
%let rawsdtm = &root.\data\raw\cdisc-pilot\sdtm;
%let rawadam = &root.\data\raw\cdisc-pilot\adam;
options dlcreatedir;
libname sdtmnew "&root.\data\derived\cdisc-pilot\sdtm";
libname adamnew "&root.\data\derived\cdisc-pilot\adam";
%macro load_xpt(lib, path, ds);
  libname _x xport "&path.\&ds..xpt";
  proc copy in=_x out=&lib.; run;
  libname _x clear;
%mend;
%load_xpt(sdtmnew, &rawsdtm., ae);
%load_xpt(sdtmnew, &rawsdtm., suppae);
%load_xpt(adamnew, &rawadam., adsl);
%load_xpt(adamnew, &rawadam., adae);
/* benchmark: the loads above refresh the input copies; restart the output-capture clock so only what the
   program itself creates (ADAMNEW.ADAE) is copied to output/sas/datasets/<id> */
%let _bench_t0 = %sysfunc(datetime());


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P003_at14-5-02.sas) ---
**********************************************************************************;
* Program Name:      at14-5-02.sas
* Purpose:           combines adverse event data with subject level analysis data
                     calculates n (%) for body system, preferred term and
                     number of events for both and then produces adverse
                     event table for SERIOUS events only
                     
 *****************************************************************************; 

libname adam "&root.\data\derived\cdisc-pilot\adam";
data ADSL;
	set ADAM.ADSL;
run;
data ADAE;
	set ADAM.ADAE;
run;
** GET DESCRIPTIVE STATISTICS - COUNTS FREQUENCIES AND UNIQUE COUNTS AT SUBJECT, SOC AND PT LEVELS**;
PROC SQL;
CREATE TABLE DENOM AS
SELECT COUNT(distinct usubjid) as denom, TRT01AN as TRTAN
FROM ADSL(where=(SAFFL='Y'))
GROUP BY TRTAN
ORDER BY TRTAN;
/* COUNT UNIQUE SUBJECTS BY SOC AND PREFERRED TERM AND TREATMENT */
CREATE TABLE AEPTD AS
SELECT COUNT(distinct usubjid) AS count, AEBODSYS, AEDECOD, TRTAN
FROM ADAE(where=(TRTEMFL='Y' and AESER='Y'))
GROUP BY TRTAN, AEBODSYS, AEDECOD
ORDER BY TRTAN, AEBODSYS, AEDECOD;
** COUNT ALL EVENTS BY SOC AND PREFERRED TERM AND TREATMENT **;
CREATE TABLE AEPTA AS
SELECT COUNT(usubjid) AS events, AEBODSYS, AEDECOD, TRTAN
FROM ADAE(where=(TRTEMFL='Y' and AESER='Y'))
GROUP BY TRTAN, AEBODSYS, AEDECOD
ORDER BY TRTAN, AEBODSYS, AEDECOD;
** GET RESULTS AT PTLEVEL **;
create table RESULTPT as select a.*, 3 as SUMMARYLEVEL, b.DENOM, c.events,
strip(put(count,best.))||' ('||strip(put((count/denom)*100,8.1))||'%) ['||strip(put(events,best.))||']'
as RESULT LENGTH=50
from AEPTD as A left join DENOM as b
on a.TRTAN=b.TRTAN
left join AEPTA as c
on a.TRTAN=c.TRTAN and a.AEBODSYS=c.AEBODSYS and a.AEDECOD=c.AEDECOD
order by AEBODSYS, AEDECOD, TRTAN;

/* COUNT UNIQUE SUBJECTS BY SOC AND TREATMENT */
CREATE TABLE AESOCD AS
SELECT COUNT(distinct usubjid) AS count, AEBODSYS, TRTAN
FROM ADAE(where=(TRTEMFL='Y' and AESER='Y'))
GROUP BY TRTAN, AEBODSYS
ORDER BY TRTAN, AEBODSYS;
** COUNT ALL EVENTS BY SOC TREATMENT **;
CREATE TABLE AESOCA AS
SELECT COUNT(usubjid) AS events, AEBODSYS, TRTAN
FROM ADAE(where=(TRTEMFL='Y' and AESER='Y'))
GROUP BY TRTAN, AEBODSYS
ORDER BY TRTAN, AEBODSYS;
** GET RESULTS AT SOCLEVEL **;
create table RESULTSOC as select a.*, 2 as SUMMARYLEVEL, b.DENOM, c.events,
strip(put(count,best.))||' ('||strip(put((count/denom)*100,8.1))||'%) ['||strip(put(events,best.))||']'
as RESULT LENGTH=50
from AESOCD as A left join DENOM as b
on a.TRTAN=b.TRTAN
left join AESOCA as c
on a.TRTAN=c.TRTAN and a.AEBODSYS=c.AEBODSYS
order by AEBODSYS, TRTAN;

/* COUNT UNIQUE SUBJECTS BY TREATMENT */
CREATE TABLE AESUBD AS
SELECT COUNT(distinct usubjid) AS count, TRTAN
FROM ADAE(where=(TRTEMFL='Y' and AESER='Y'))
GROUP BY TRTAN
ORDER BY TRTAN;
** COUNT ALL EVENTS BY TREATMENT **;
CREATE TABLE AESUBA AS
SELECT COUNT(usubjid) AS events, TRTAN
FROM ADAE(where=(TRTEMFL='Y' and AESER='Y'))
GROUP BY TRTAN
ORDER BY TRTAN;
** GET RESULTS AT SUBJECT LEVEL **;
create table RESULTSUB as select a.*, 1 as SUMMARYLEVEL, b.DENOM, c.events,
strip(put(count,best.))||' ('||strip(put((count/denom)*100,8.1))||'%) ['||strip(put(events,best.))||']'
as RESULT LENGTH=50
from AESUBD as A left join DENOM as b
on a.TRTAN=b.TRTAN
left join AESUBA as c
on a.TRTAN=c.TRTAN
order by TRTAN;
QUIT;
** INFERENTIAL STATISTICS **;
** PT LEVEL **;
proc sql;
create table BACKBONE as select * from
(select distinct USUBJID, TRT01AN as TRTAN from ADSL(where=(SAFFL='Y'))) ,
(select distinct AEDECOD from ADAE(where=(TRTEMFL='Y' and AESER='Y')));
create table HADEVENT as select distinct STUDYID, USUBJID, TRTAN, AEDECOD
from ADAE(where=(TRTEMFL='Y' and AESER='Y'));

create table FISHER as select a.USUBJID, a.TRTAN, a.AEDECOD,
case when STUDYID='CDISCPILOT01' then 1
else 0
end as EVENT
from BACKBONE a left join HADEVENT b
on a.USUBJID=b.USUBJID and a.TRTAN=b.TRTAN and a.AEDECOD=b.AEDECOD
order by a.AEDECOD, a.TRTAN;
quit;
** HIGH DOSE VERSUS PLACEBO **;
proc freq data=FISHER; 
by AEDECOD; 
where TRTAN in (0,2);
table TRTAN*EVENT/exact; 
ods output FishersExact=F1PT(where=(Name1='XP2_FISH'));
run;
** LOW DOSE VERSUS PLACEBO **;
proc freq data=FISHER; 
by AEDECOD; 
where TRTAN in (0,1);
table TRTAN*EVENT/exact; 
ods output FishersExact=F2PT(where=(Name1='XP2_FISH'));
run;
** SOC LEVEL**;
proc sql;
create table BACKBONE1 as select * from
(select distinct USUBJID, TRT01AN as TRTAN from ADSL(where=(SAFFL='Y'))) ,
(select distinct AEBODSYS from ADAE(where=(TRTEMFL='Y' and AESER='Y')));
create table HADEVENT1 as select distinct STUDYID, USUBJID, TRTAN, AEBODSYS
from ADAE(where=(TRTEMFL='Y' and AESER='Y'));

create table FISHER1 as select a.USUBJID, a.TRTAN, a.AEBODSYS,
case when STUDYID='CDISCPILOT01' then 1
else 0
end as EVENT
from BACKBONE1 a left join HADEVENT1 b
on a.USUBJID=b.USUBJID and a.TRTAN=b.TRTAN and a.AEBODSYS=b.AEBODSYS
order by a.AEBODSYS, a.TRTAN;
quit;
** HIGH DOSE VERSUS PLACEBO **;
proc freq data=FISHER1; 
by AEBODSYS; 
where TRTAN in (0,2);
table TRTAN*EVENT/exact; 
ods output FishersExact=F1SOC(where=(Name1='XP2_FISH'));
run;
** LOW DOSE VERSUS PLACEBO **;
proc freq data=FISHER1; 
by AEBODSYS; 
where TRTAN in (0,1);
table TRTAN*EVENT/exact; 
ods output FishersExact=F2SOC(where=(Name1='XP2_FISH'));
run;
** SUBJECT LEVEL**;
proc sql;
create table BACKBONE2 as select * from
(select distinct USUBJID, TRT01AN as TRTAN from ADSL(where=(SAFFL='Y')));
create table HADEVENT2 as select distinct STUDYID, USUBJID, TRTAN
from ADAE(where=(TRTEMFL='Y' and AESER='Y'));
create table FISHER2 as select a.USUBJID, a.TRTAN,
case when STUDYID='CDISCPILOT01' then 1
else 0
end as EVENT
from BACKBONE2 a left join HADEVENT2 b
on a.USUBJID=b.USUBJID and a.TRTAN=b.TRTAN
order by a.TRTAN;
quit;
** HIGH DOSE VERSUS PLACEBO **;
proc freq data=FISHER2; 
where TRTAN in (0,2);
table TRTAN*EVENT/exact; 
ods output FishersExact=F1SUB(where=(Name1='XP2_FISH'));
run;
** LOW DOSE VERSUS PLACEBO **;
proc freq data=FISHER2; 
where TRTAN in (0,1);
table TRTAN*EVENT/exact; 
ods output FishersExact=F2SUB(where=(Name1='XP2_FISH'));
run;

--- end of SAS program ---

Write the complete R script now.