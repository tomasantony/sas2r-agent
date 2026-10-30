Translate the SAS program below to R.

Program id: P051   file: P051_medication_listing.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: table;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P051
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P051

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  Target36.rtf (table/listing text, 46030 numbers)  -> write a text rendering and CSV of the same table

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P051_medication_listing.sas) ---
/****************************************************************************/
/*         PROGRAM NAME: Target36.sas                                       */
/*                                                                          */
/*          DESCRIPTION: Listing of medications                             */
/*                                                                          */
/*               AUTHOR: Adrienne M Bonwick                                 */
/*                                                                          */
/*                 DATE: 12th October 2014 (PhUSE scriptathon)              */
/*                                                                          */
/*  EXTERNAL FILES USED: ADCM.xpt                                           */
/*                                                                          */
/****************************************************************************/

%* modification 2019-12-23 - update path as data has been moved;

filename source "&rawadam./adcm.xpt";
libname source xport ;

%let POPN=SAFFL;
%let POPNLABEL=Safety;

options orientation=landscape pagesize=41 linesize=119 nodate nonumber;

proc sort out=work.adcm/*(keep=STUDYID USUBJID TRTA TRTAN SAFFL CMCLAS CMDECOD CMDOSE CMDOSU CMINDC CMSTDTC CMENDTC ADURN AENRF &POPN)*/
   data=source.ADCM ;
by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD;
WHERE &POPN='Y' ;
run ;

Data ADCM2;
  set ADCM;
  length StartStop $25 Doseunit $50;
  Period ='Study Period';
  if CMSTDTC='' then CMSTDTC='NK';
  if CMENDTC='' then CMENDTC='NK';
  StartStop=compbl(CMSTDTC||'/'||CMENDTC);
  If cmunit ne . then   DoseUnit = compbl(CMDOSE || '('|| cmunit || ')');
    else DoseUnit = compbl(CMDOSE);
  *** duration=AENDT-ASTDT;
  if AENRF='Ongoing' then Ongoing='Y'; 
    else Ongoing='N';
run;


ods listing close;
ods rtf file="Target36.rtf" style=styles.journal;
options nobyline;
Proc Report data=ADCM2 headline ;
by TRTA;
Columns TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing;
define TRTA / noprint;
define USUBJID / width=11 flow;
define CMTRT / width=10  flow;
define cmdecod / width=10 flow; 
define cmclas  / width=10 flow;
define DoseUnit / width=6 "Dose (unit)" flow; 
define CMINDC  / width=10 flow;
define StartStop / width=10 "Start Date / Stop Date" flow; 
define ADURN  / width=6 "Dur (Days)" flow;
define Ongoing / width=7 flow;
title JUSTIFY=L "&POPNLABEL Population";
title2 JUSTIFY=L "Study Period";
title3 JUSTIFY=L "Treatment: #byval1";
run;quit;
ods rtf close;
ods listing;


/*Dataset: ADCM
 Variables: STUDYID, USUBJID, TRTA, TRTAN, SAFFL, CMCLAS, CMDECOD, CMDOSE, CMDOSU, CMINDC, CMSTDTC, CMENDTC, ADURN, AENRF, SAFFL
 Record Selection: 

Subject ID = USUBJID
 Medication ATC term(s) = CMCLAS
 Generic Name = CMDECOD
 Dose (unit) = CMDOSE (CMDOSU)
 Primary Indication for Use = CMINDC
 Start Date/Stop Date = CMSTDTC/CMENDTC
 Dur. (days) = ADURN
 Cont. (Y/N) = display 'Y' if AENRF='Ongoing'; else display 'N'
 Sort by STUDYID, USUBJID, CMSTDTC, CMENDTC, CMDECOD. 
*/

--- end of SAS program ---

Write the complete R script now.