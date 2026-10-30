Translate the SAS program below to R.

Program id: P013   file: P013_sdtm_eg.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P013
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P013
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset eg  -> write OUT_DIR/eg.csv

--- family environment (c2c) that ran before the program ---
%global project_path usubjid_length;
%let project_path = &root./data/derived/c2c;   /* programs expect &project_path./data/{raw,sdtm,adam} */
%include "&project_path./programs/00_setup.sas";

/* ---- included: 00_setup.sas ---- */
/*******************************************************************************
Program Name: 00_setup.sas
Description:  Master configuration file. Defines global macro variables 
              and library references for the entire clinical pipeline.
*******************************************************************************/

/* =====================================================================
   INSTRUCTIONS FOR REVIEWERS/USERS:
   Define PROJECT_PATH before including this file. The supported entry point
   is RUN_ALL.SAS, where the repository path is configured once.
   ===================================================================== */

%macro validate_project_path;
    %if not %symexist(project_path) %then %do;
        %put ERROR: PROJECT_PATH is not defined.;
        %put ERROR: Configure PROJECT_PATH in RUN_ALL.SAS and execute that program.;
        %abort cancel;
    %end;
    %else %if not %sysfunc(fileexist(&project_path./programs/00_setup.sas)) %then %do;
        %put ERROR: PROJECT_PATH does not point to the repository root: &project_path.;
        %abort cancel;
    %end;
%mend;

%validate_project_path;
%let usubjid_length = 40;

/* Initialize SAS Libraries */
libname raw  "&project_path./data/raw";
libname sdtm "&project_path./data/sdtm";
libname adam "&project_path./data/adam";

/* Force variables to uppercase (CDISC Standard compliance) */
options validvarname=upcase;

/* Log confirmation */
%put --------------------------------------------------------;
%put SETUP COMPLETE: Libraries successfully assigned to:;
%put &project_path;
%put --------------------------------------------------------;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P013_sdtm_eg.sas) ---
/*******************************************************************************
Program Name: sdtm_eg.sas
Description:  Transforms raw ECG data into CDISC SDTM EG domain.
*******************************************************************************/
/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

proc import datafile="&project_path./data/raw/raw_ecg.csv" 
    out=work.raw_eg dbms=csv replace; 
    getnames=yes; 
run;

data sdtm.eg;
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length. EGTESTCD $8 EGTEST $40
           EGORRES $20 EGORRESU $20 EGSTRESC $20 EGSTRESU $20 EGDTC $10;
    
    set work.raw_eg;
    
    STUDYID = "CDISC-01";
    DOMAIN = "EG";
    USUBJID = catx('-', STUDYID, strip(SUBJECT)); 
    VISIT = strip(VISIT_NAM);
    VISITNUM = VISIT_NUM;
    
    EGTEST = strip(TEST_NAME); /* Lê a variável de origem correta */
    
    /* Assign short codes based on the test name */
    if EGTEST = "Heart Rate" then EGTESTCD = "HR";
    else if EGTEST = "QT Duration" then EGTESTCD = "QT";
    else if EGTEST = "RR Duration" then EGTESTCD = "RR";
    else EGTESTCD = "UNKNOWN";
    
    EGORRES = strip(put(RESULT, best32.));
    EGORRESU = strip(UNIT);
    EGSTRESN = RESULT;
    EGSTRESC = strip(put(EGSTRESN, best32.));
    EGSTRESU = strip(UNIT);

    /* PROC IMPORT may infer DATE as a numeric SAS date. Always emit ISO 8601. */
    if vtype(DATE) = 'C' then _egdt = input(strip(DATE), anydtdte.);
    else _egdt = DATE;

    if not missing(_egdt) then EGDTC = put(_egdt, is8601da.);

    keep STUDYID DOMAIN USUBJID VISITNUM VISIT EGTESTCD EGTEST EGORRES EGORRESU
         EGSTRESC EGSTRESN EGSTRESU EGDTC;
run;

proc sort data=sdtm.eg; 
    by USUBJID EGDTC EGTESTCD; 
run;

data sdtm.eg; 
    retain STUDYID DOMAIN USUBJID EGSEQ VISITNUM VISIT EGTESTCD EGTEST EGORRES EGORRESU
           EGSTRESC EGSTRESN EGSTRESU EGDTC;
    set sdtm.eg; 
    by USUBJID; 
    if first.USUBJID then EGSEQ=1; 
    else EGSEQ+1; 

    keep STUDYID DOMAIN USUBJID EGSEQ VISITNUM VISIT EGTESTCD EGTEST EGORRES EGORRESU
         EGSTRESC EGSTRESN EGSTRESU EGDTC;
run;

--- end of SAS program ---

Write the complete R script now.