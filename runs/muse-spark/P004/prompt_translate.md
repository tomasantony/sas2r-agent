Translate the SAS program below to R.

Program id: P004   file: P004_adam_adae.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P004
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P004
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P004.lst (table/listing text, 148 numbers)  -> write a text rendering and CSV of the same table
  dataset adae  -> write OUT_DIR/adae.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P004_adam_adae.sas) ---
/*******************************************************************************
Program Name: adam_adae.sas
Description:  Derivation of ADAE (Adverse Events Analysis Dataset).
              Merges ADSL with SDTM AE to calculate numeric event dates, 
              study days (ASTDY), and Treatment-Emergent Flags (TRTEMFL).
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. SORT INPUT DATASETS BY UNIQUE SUBJECT ID */
proc sort data=adam.adsl out=work.adsl_sorted;
    by USUBJID;
run;

proc sort data=sdtm.ae out=work.ae_sorted;
    by USUBJID AESEQ;
run;


/* 2. MERGE ADSL AND AE TO CREATE ADAE */
data work.adae_draft;
	retain STUDYID USUBJID;
    /* ADSL is the master patient list, AE contains multiple rows per patient */
    merge work.adsl_sorted(in=a) work.ae_sorted(in=b);
    by USUBJID;
    
    /* Keep only subjects who actually experienced an Adverse Event */
    if b; 

    /* --------------------------------------------------------
       NUMERIC DATE DERIVATIONS
       -------------------------------------------------------- */
    /* Convert ISO 8601 character dates to SAS numeric dates */
    if length(AESTDTC) >= 10 then AESTDT = input(substr(AESTDTC, 1, 10), yymmdd10.);
    if length(AEENDTC) >= 10 then AEENDT = input(substr(AEENDTC, 1, 10), yymmdd10.);
    
    format AESTDT AEENDT date9.;

    /* --------------------------------------------------------
       ANALYSIS RELATIVE DAYS (ASTDY)
       -------------------------------------------------------- */
    /* How many days after starting the drug did the event happen? 
       Rule: If event is on or after treatment start, Day 1 is TRTSDT. */
    if not missing(AESTDT) and not missing(TRTSDT) then do;
        if AESTDT >= TRTSDT then ASTDY = (AESTDT - TRTSDT) + 1;
        else ASTDY = AESTDT - TRTSDT;
    end;

    /* --------------------------------------------------------
       TREATMENT-EMERGENT FLAG (TRTEMFL)
       -------------------------------------------------------- */
    /* An event is Treatment-Emergent if it starts on or after the first dose 
       and the patient actually took the drug (SAFFL = 'Y') */
    if SAFFL = 'Y' and not missing(AESTDT) and not missing(TRTSDT) then do;
        if AESTDT >= TRTSDT then TRTEMFL = "Y";
        else TRTEMFL = "";
    end;
    else TRTEMFL = "";

    /* Keep the essential clinical and analysis variables */
    keep STUDYID USUBJID SUBJID TRT01A TRTSDT TRTEDT AGE SEX 
         AESEQ AETERM AEDECOD AESEV AEREL AEOUT AESER
         AESTDT AEENDT ASTDY TRTEMFL;
run;


/* 3. DERIVE FIRST TREATMENT-EMERGENT OCCURRENCE BY DECODED TERM */
data work.adae_occurrence_order;
    set work.adae_draft;
    /* Put treatment-emergent records first within each decoded term. */
    if TRTEMFL = 'Y' then _occ_order = 0;
    else _occ_order = 1;
run;

proc sort data=work.adae_occurrence_order;
    by USUBJID AEDECOD _occ_order AESTDT AESEQ;
run;

data work.adae_flagged;
    set work.adae_occurrence_order;
    by USUBJID AEDECOD _occ_order;
    length AOCCFL $1;

    /* Sponsor rule: first treatment-emergent occurrence per subject/PT. */
    if TRTEMFL = 'Y' and first._occ_order then AOCCFL = 'Y';
    else AOCCFL = '';

    drop _occ_order;
run;


/* 4. FINALIZE AND SAVE TO ADAM LIBRARY */
proc sort data=work.adae_flagged out=adam.adae;
    by USUBJID AESTDT AESEQ;
run;


/* 5. VISUAL AUDIT */
title "ADAE Audit - Treatment-Emergent and First-Occurrence Flags";
proc print data=adam.adae(obs=15);
    var USUBJID TRT01A AETERM AESEV AESTDT TRTSDT ASTDY TRTEMFL AOCCFL;
run;
title;

--- end of SAS program ---

Write the complete R script now.