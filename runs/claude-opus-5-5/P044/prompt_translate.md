Translate the SAS program below to R.

Program id: P044   file: P044_ae_common.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: graph;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P044
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P044

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  figure SGPlot.png  -> write OUT_DIR/sgplot.png and a CSV of the plotted data

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P044_ae_common.sas) ---
%* modification 2019-12-23 - update path as data has been moved;

filename source "&rawadam./adsl.xpt" ;
libname source xport ;

data work.adsl ;
  set source.adsl ;
run ;

 filename source "&rawadam./adae.xpt" ;
 libname source xport ;

 data work.adae ;
   set source.adae ;
   keep USUBJID TRTAN AEDECOD TRTEMFL AOCCPFL SAFFL;
 run ;

   proc sort data=work.adae;
   by aedecod;
run;

   data adae1;
   set adae;
   where TRTEMFL="Y" and AOCCPFL='Y' and saffl="Y";
   run;


   proc sql;
   	create table countae as
	select distinct count(usubjid) as numAE, aedecod, trtan
	from adae1
	group by trtan, aedecod;

    create table nsubj as
	select distinct count(distinct usubjid) as numusubjid, TRT01AN
	from adsl
	group by TRT01AN;
	quit;

data count;
merge countae(in=_countae)  nsubj(in=_nsubj rename=(trt01an=trtan));
by trtan;
if not _nsubj then do;
abort cancel;
end;
label AEpercent="AEs (%)";
if not missing(numAE) and not missing(numusubjid) then do;
AEpercent= numAE/numusubjid;
end;
else do;
AEpercent= .;
end;
run;

proc sort data=count;
by aedecod;
run;

data tcount;
merge
count
count(where=(trtan_1=0) rename=(AEpercent=AEpercent_1 trtan=trtan_1))
;
by aedecod;

if missing(AEpercent) then do;
AEpercent=0;
end;

if missing(AEpercent_1) then do;
AEpercent_1=0;
end;

RelRisk= AEpercent - AEpercent_1;
aeseqno+1;
run;

proc sql;
create table maxpercen2 as 
select distinct -max(AEpercent) as minusmaxAEpercent, aedecod
from tcount
group by aedecod
order by minusmaxAEpercent;
quit;

data maxpercen2;
set maxpercen2;
dispseqno+1;
run;

proc sort data=maxpercen2;
by aedecod;
run;

data tcount2;
merge tcount maxpercen2;
by aedecod;
run;

proc sql;
create table AEgrid as 
select * from (select distinct trtan from tcount2),
(select distinct aedecod, dispseqno from tcount2)
order by aedecod, trtan;
quit;

data tcount3;
merge tcount2 AEgrid;
by aedecod trtan;
if missing(AEpercent) then do;
AEpercent=0;
end;
run;

proc sort data=tcount3;
by dispseqno;
run;


title1 "Common Treatment-Emergent Adverse Events";
proc sgplot data=tcount3;
label trtan="Treatment";
label aeseqno="AE ordered in descending order";
where dispseqno<=20;
  scatter x=AEpercent y=dispseqno / group=trtan; 
  xaxis grid;
  yaxis grid values=(1 to 20 by 1) valueshint;
 run; 


--- end of SAS program ---

Write the complete R script now.