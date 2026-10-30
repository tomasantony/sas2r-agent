Translate the SAS program below to R.

Program id: P016   file: P016_sdtm_mh.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P016
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P016
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset mh  -> write OUT_DIR/mh.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P016_sdtm_mh.sas) ---
/*******************************************************************************
Program Name: sdtm_mh.sas
Description:  Transforms raw Medical History data into CDISC SDTM MH domain.
*******************************************************************************/
/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

proc import datafile="&project_path./data/raw/raw_mh.csv" 
    out=work.raw_mh dbms=csv replace; 
    getnames=yes; 
run;

data sdtm.mh;
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length. MHTERM $60 MHSTDTC $10;
    
    set work.raw_mh;
    
    STUDYID = "CDISC-01";
    DOMAIN = "MH";
    USUBJID = catx('-', STUDYID, strip(SUBJECT)); 
    MHTERM = strip(CONDITION);

    /* PROC IMPORT may infer DIAGNOSIS_DATE as a numeric SAS date. */
    if vtype(DIAGNOSIS_DATE) = 'C' then
        _mhdt = input(strip(DIAGNOSIS_DATE), anydtdte.);
    else _mhdt = DIAGNOSIS_DATE;

    if not missing(_mhdt) then MHSTDTC = put(_mhdt, is8601da.);

    keep STUDYID DOMAIN USUBJID MHTERM MHSTDTC;
run;

proc sort data=sdtm.mh; 
    by USUBJID MHSTDTC; 
run;

data sdtm.mh;
    retain STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC; 
    set sdtm.mh; 
    by USUBJID; 
    if first.USUBJID then MHSEQ=1; 
    else MHSEQ+1; 

    keep STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC;
run;

--- end of SAS program ---

Write the complete R script now.