Translate the SAS program below to R.

Program id: P006   file: P006_adam_adsl.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P006
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P006
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P006.lst (table/listing text, 64 numbers)  -> write a text rendering and CSV of the same table
  dataset adsl  -> write OUT_DIR/adsl.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P006_adam_adsl.sas) ---
/*******************************************************************************
Program Name: adam_adsl.sas
Description:  Derivation of ADSL (Subject-Level Analysis Dataset).
              Merges SDTM DM and EX to create population flags (SAFFL, ITTFL), 
              numeric analysis dates, and calculates age and treatment duration.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. GET FIRST EXPOSURE RECORD PER SUBJECT */
proc sort data=sdtm.ex out=work.ex_first;
    by USUBJID EXSTDTC;
run;

data work.ex_trt;
    set work.ex_first;
    by USUBJID;
    if first.USUBJID; /* Keep only the initial dosing record */
    keep USUBJID EXTRT EXSTDTC EXENDTC;
run;

/* 2. SORT DM BEFORE MERGING (Crucial SAS Rule) */
proc sort data=sdtm.dm out=work.dm_sorted;
    by USUBJID;
run;


/* 3. MERGE DEMOGRAPHICS AND EXPOSURE TO CREATE ADSL */
data work.adsl_draft;
    retain STUDYID USUBJID SUBJID SITEID;
    merge work.dm_sorted(in=a) work.ex_trt(in=b);
    by USUBJID;
    if a; /* Keep all randomized subjects from DM */

    /* Core Attributes */
    STUDYID = STUDYID;
    USUBJID = USUBJID;
    SUBJID  = SUBJID;

   /* --------------------------------------------------------
       TREATMENT VARIABLES & POPULATION FLAGS
       -------------------------------------------------------- */
    
    /* 1. Planned Treatment (TRT01P) comes from DM Randomization */
    if ARMCD ne 'SCRNFAIL' then TRT01P = ARM;
    else TRT01P = 'UNPLANNED';

    /* 2. Actual Treatment (TRT01A) comes from Exposure (EX) */
    if b and not missing(EXTRT) then TRT01A = EXTRT;
    else TRT01A = "NOT TREATED";

    /* 3. Intent-To-Treat (ITT) Flag: Only Randomized subjects */
    if ARMCD ne 'SCRNFAIL' then ITTFL = "Y";
    else ITTFL = "N";

    /* 4. Safety Population (SAFFL) Flag: Took at least one dose */
    if not missing(EXSTDTC) then SAFFL = "Y";
    else SAFFL = "N";


    /* --------------------------------------------------------
       NUMERIC DATE DERIVATIONS (For Statistical Math)
       -------------------------------------------------------- */
    /* Convert ISO 8601 character dates to SAS numeric dates */
    if length(BRTHDTC) >= 10 then BRTHDT = input(substr(BRTHDTC, 1, 10), yymmdd10.);
    if length(RFICDTC) >= 10 then RFICDT = input(substr(RFICDTC, 1, 10), yymmdd10.);
    if length(EXSTDTC) >= 10 then TRTSDT = input(substr(EXSTDTC, 1, 10), yymmdd10.);
    if length(EXENDTC) >= 10 then TRTEDT = input(substr(EXENDTC, 1, 10), yymmdd10.);
    if length(RFPENDTC) >= 10 then EOSDT = input(substr(RFPENDTC, 1, 10), yymmdd10.);

    format BRTHDT RFICDT TRTSDT TRTEDT EOSDT date9.; /* Example: 05JAN2023 */

    /* --------------------------------------------------------
       CLINICAL MATH DERIVATIONS
       -------------------------------------------------------- */
    /* 1. Calculate completed age at informed consent.
       YRDIF with the AGE basis handles birthdays and leap years. */
    if not missing(BRTHDT) and not missing(RFICDT) then
        AGE = floor(yrdif(BRTHDT, RFICDT, 'AGE'));

    /* 2. Calculate Treatment Duration (Days) */
    if not missing(TRTSDT) and not missing(TRTEDT) then
        TRTDURD = (TRTEDT - TRTSDT) + 1; /* Inclusive of start day */

    keep STUDYID USUBJID SUBJID SITEID TRT01P TRT01A ITTFL SAFFL
         BRTHDT RFICDT TRTSDT TRTEDT EOSDT AGE TRTDURD SEX RACE;
run;


/* 4. FINALIZE AND SAVE TO ADAM LIBRARY */
proc sort data=work.adsl_draft out=adam.adsl;
    by USUBJID;
run;

/* 5. VISUAL AUDIT */
title "ADSL Audit - Subject Level Analysis Dataset";
proc print data=adam.adsl(obs=10);
    var USUBJID TRT01P TRT01A ITTFL SAFFL AGE;
run;
title;

--- end of SAS program ---

Write the complete R script now.