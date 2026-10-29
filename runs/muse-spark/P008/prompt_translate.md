Translate the SAS program below to R.

Program id: P008   file: P008_adam_advs.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P008
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P008
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P008.lst (table/listing text, 180 numbers)  -> write a text rendering and CSV of the same table
  dataset advs  -> write OUT_DIR/advs.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P008_adam_advs.sas) ---
/*******************************************************************************
Program Name: adam_advs.sas
Description:  Derivation of ADVS (Vital Signs Analysis Dataset).
              Merges ADSL with SDTM VS to derive Baseline Flags (ABLFL),
              Baseline Values (BASE), and Change from Baseline (CHG).
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. MERGE ADSL AND VS */
proc sort data=adam.adsl out=work.adsl_sorted;
    by USUBJID;
run;

proc sort data=sdtm.vs out=work.vs_sorted;
    by USUBJID VSDTC VSTESTCD;
run;

data work.advs_draft;
	retain STUDYID USUBJID;
    merge work.adsl_sorted(in=a) work.vs_sorted(in=b);
    by USUBJID;
    if b; /* Keep only records that have vital signs */

    /* --------------------------------------------------------
       CORE ANALYSIS VARIABLES
       -------------------------------------------------------- */
    PARAMCD = VSTESTCD;
    PARAM   = VSTEST;
    AVAL    = VSSTRESN;
    AVISIT  = VISIT;
    AVISITN = VISITNUM;
    SRCDOM  = 'VS';
    SRCVAR  = 'VSSEQ';
    SRCSEQ  = VSSEQ;

    /* Convert ISO Date to Numeric Analysis Date */
    if length(VSDTC) >= 10 then ASTDT = input(substr(VSDTC, 1, 10), yymmdd10.);
    format ASTDT date9.;

    /* Analysis Relative Day (ASTDY) */
    if not missing(ASTDT) and not missing(TRTSDT) then do;
        if ASTDT >= TRTSDT then ASTDY = (ASTDT - TRTSDT) + 1;
        else ASTDY = ASTDT - TRTSDT;
    end;

    keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL
         AVISIT AVISITN ASTDT ASTDY SRCDOM SRCVAR SRCSEQ;
run;


/* 2. IDENTIFY BASELINE RECORD */
proc sort data=work.advs_draft out=work.advs_base_candidates;
    by USUBJID PARAMCD ASTDT AVISITN SRCSEQ;
    where ASTDT <= TRTSDT and not missing(AVAL);
run;

data work.base_flags;
    set work.advs_base_candidates;
    by USUBJID PARAMCD;
    
    if last.PARAMCD then do;
        BASE = AVAL;
        BASE_DT = ASTDT;
        BASE_SEQ = SRCSEQ;
        output;
    end;
    keep USUBJID PARAMCD BASE BASE_DT BASE_SEQ;
run;


/* 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE */
proc sort data=work.advs_draft;
    by USUBJID PARAMCD;
run;

data adam.advs;
	retain STUDYID USUBJID;
    merge work.advs_draft(in=a) work.base_flags(in=b);
    by USUBJID PARAMCD;
    if a;

    /* Flag the exact source record; the QC gate rejects same-day ambiguity. */
    if SRCSEQ = BASE_SEQ then ABLFL = 'Y';
    else ABLFL = '';

    /* 2. Calculate Change from Baseline */
    if not missing(AVAL) and not missing(BASE) then do;
        CHG = AVAL - BASE;
    end;
    
    drop BASE_DT BASE_SEQ;
run;

/* Re-sort for final presentation */
proc sort data=adam.advs;
    by USUBJID PARAMCD ASTDT;
run;


/* 4. VISUAL AUDIT */
title "ADVS Audit - Vital Signs Baseline and Change Analysis";
proc print data=adam.advs(obs=15);
    var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL;
run;
title;

--- end of SAS program ---

Write the complete R script now.