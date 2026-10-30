Translate the SAS program below to R.

Program id: P007   file: P007_adam_adtte.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P007
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P007
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P007.lst (table/listing text, 99 numbers)  -> write a text rendering and CSV of the same table
  dataset adtte  -> write OUT_DIR/adtte.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P007_adam_adtte.sas) ---
/*******************************************************************************
Program Name: adam_adtte.sas
Description:  Derivation of ADTTE (Time-to-Event Analysis Dataset).
              Calculates Time to First Adverse Event (Survival Analysis).
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. GET FIRST ADVERSE EVENT PER SUBJECT */
proc sort data=adam.adae out=work.adae_first;
    by USUBJID AESTDT; 
    where TRTEMFL = 'Y'; 
run;

data work.ae_target;
    set work.adae_first;
    by USUBJID;
    if first.USUBJID;
    keep USUBJID AESTDT;
    rename AESTDT = EVENTDT; 
run;


proc sort data=adam.adsl out=work.adsl_sorted;
    by USUBJID;
run;

data adam.adtte;
    retain STUDYID USUBJID PARAMCD PARAM;
    
    merge work.adsl_sorted(in=a) work.ae_target(in=b);
    by USUBJID;
    
    if a and SAFFL = 'Y'; 

    PARAMCD = "TTFAEV";
    PARAM   = "Time to First Adverse Event";

    /* --------------------------------------------------------
       3. DERIVE CENSORING (CNSR) AND TIME (AVAL)
       -------------------------------------------------------- */
    if missing(TRTSDT) then do;
        CNSR = .;
        AVAL = .;
    end;
    else if not missing(EVENTDT) then do;
        /* The subject experienced the first treatment-emergent event. */
        CNSR = 0; 
        AVAL = (EVENTDT - TRTSDT) + 1;
    end;
    else do;
        /* Censor at the participant-specific end of study participation.
           Treatment end is not a substitute for end of clinical follow-up. */
        CNSR = 1;
        if not missing(EOSDT) then AVAL = (EOSDT - TRTSDT) + 1;
        else AVAL = .;
    end;

    keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT
         PARAMCD PARAM CNSR AVAL EVENTDT;
run;


/* 4. VISUAL AUDIT */
title "ADTTE Audit - Time to First Adverse Event";
proc print data=adam.adtte(obs=15);
    var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT;
run;
title;

--- end of SAS program ---

Write the complete R script now.