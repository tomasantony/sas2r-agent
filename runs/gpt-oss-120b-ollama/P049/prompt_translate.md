Translate the SAS program below to R.

Program id: P049   file: P049_disposition_b1.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: table;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P049
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P049

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P049.lst (table/listing text, 59 numbers)  -> write a text rendering and CSV of the same table

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P049_disposition_b1.sas) ---
********************************************************************************************
   CODE NAME         : target31.sas
   DESCRIPTION       : Reference SAS program for target 31
   SOFTWARE/VERSION# : SAS V9
   INPUT             : http://phuse-scripts.googlecode.com/svn/trunk/scriptathon2014/data/adsl.xpt
Dataset: ADSL
Variables: USUBJID, TRTAN, DSREASCD, ITTFL
Record Selection: WHERE ITTFL=Y 

Completed the Study = count patients where DSREASCD='Completed'.
Discontinued = count patients where DSREASCD ne 'Completed'.
Display all discontinuation reasons alphabetically by DSREASCD.
Percentage = 100 * n in cell / N with ADSL.TRTAN value. 
********************************************************************************************;

**-----------------------------------------------------------------------------;
**-----------------------------------------------------------------------------;
%* modification 2019-12-23 - update path as data has been moved;

filename source "&rawadam./adsl.xpt";
libname source xport;

data work.adsl;
  set source.adsl;
run;

**-----------------------------------------------------------------------------;
**-----------------------------------------------------------------------------;
proc sql;
  create table disp as 
	select distinct TRT01P,TRT01PN, count(usubjid) as cnt, 1 as ord,
  case 
    when DCREASCD='Completed'  then DCREASCD 
    else 'Discontinued' end as status
  from adsl
	  where ITTFL='Y'
	  group by TRT01P, calculated status
  union all
	  select distinct TRT01P,TRT01PN, count(usubjid) as cnt, 2 as ord,
	  DCREASCD as status
  from adsl
	  where ITTFL='Y' and DCREASCD^='Completed'
	  group by TRT01P,DCREASCD
  order by TRT01PN,calculated ord, calculated status
;

  create table freq as
	select distinct TRT01PN,count (usubjid) as tot_cnt
  from adsl
  group by TRT01PN
  order by TRT01PN
;
quit;

**-----------------------------------------------------------------------------;
**-----------------------------------------------------------------------------;
data disp_2;
	merge disp freq;
	by TRT01PN;
	treat=  strip(TRT01P) ||" (N="||strip(tot_cnt)||")";
	cnt_prcnt= strip(cnt)||" ("||strip(put(cnt/tot_cnt,percent8.1))||")";
run;

proc sort data= disp_2; by status ord; run;

proc transpose data=disp_2 out=trans_2 prefix=tr;
	by status ord;
	id TRT01PN;
	idlabel treat;
	var cnt_prcnt;
run; 

proc sort data =trans_2; by ord status; run;

**-----------------------------------------------------------------------------;
**-----------------------------------------------------------------------------;
title1 font="Courier New" height=8pt "Subject Disposition";
  
proc report data=trans_2 nowd headline headskip split="\" spacing=2;
  columns  ord status  tr:;
  
  define ord / group  order=data noprint;
  define status/group "Subject Disposition"  order=data;
  define tr: /display;
  
  break after ord/skip;
  
  compute after _page_;
		line @2 "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category";
		line @2 "% = Percentage of subjects with N as denominator";
	endcomp;
quit;
run;

--- end of SAS program ---

Write the complete R script now.