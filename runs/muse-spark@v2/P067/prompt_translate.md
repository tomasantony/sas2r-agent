Translate the SAS program below to R.

Program id: P067   file: P067_ae.sas   source: wxzhai/Clinical-Trial-SAS-Programming-Portfolio   family: wxzhai
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P067
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P067

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset ae  -> write OUT_DIR/ae.csv
  dataset raw_ae  -> write OUT_DIR/raw_ae.csv

--- family environment (wxzhai) that ran before the program ---
libname raw  "&root.\data\derived\wxzhai\raw";
libname sdtm "&root.\data\derived\wxzhai\sdtm";
libname adam "&root.\data\derived\wxzhai\adam";


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P067_ae.sas) ---
/*******************************************************************************
* PROGRAM: ae.sas
* PURPOSE: Create SDTM datasets based on CDISC
* AUTHOR:  JAMES ZHAI, SAS Certified Clinical Trials Programmer
* DATE: 2025/10/01
*******************************************************************************/

proc sort data=raw.raw_ae; by SUBJID; run;
proc sort data=sdtm.dm; by SUBJID; run;

data sdtm.ae;
    merge raw.raw_ae (in=a)
          sdtm.dm;
    by SUBJID;
    if a; /* Keep only subjects who have AE */
   
    length STUDYID $10 AESTDTC AEENDTC $10;

    STUDYID = "ABC-301";           /* study identifier */
    AESTDTC = put(AESTDT, yymmdd10.); /* ISO 8601 date */
    AEENDTC = put(AEENDT, yymmdd10.);
    
    /* Convert RFSTDTC to numeric for calculation */
    rfstdt_num = input(RFSTDTC, yymmdd10.);

    /* AE Start Day */
    if not missing(AESTDT) then do;
        if AESTDT >= rfstdt_num then ASTDY = AESTDT - rfstdt_num + 1;
        else ASTDY = AESTDT - rfstdt_num; /* pre-dose events negative */
    end;

    /* AE End Day */
    if not missing(AEENDT) then do;
        if AEENDT >= rfstdt_num then AENDY = AEENDT - rfstdt_num + 1;
        else AENDY = AEENDT - rfstdt_num;
    end;

    format ASTDY AENDY 8.;
    * drop rfstdt_num;

    keep STUDYID USUBJID SUBJID AETERM AESER AESEV 
         AESTDTC AEENDTC ASTDY AENDY;
run;


--- end of SAS program ---

Write the complete R script now.