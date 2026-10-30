Translate the SAS program below to R.

Program id: P005   file: P005_adam_adlb.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P005
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P005
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P005.lst (table/listing text, 120 numbers)  -> write a text rendering and CSV of the same table
  dataset adlb  -> write OUT_DIR/adlb.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P005_adam_adlb.sas) ---
/*******************************************************************************
Program Name: adam_adlb.sas
Description:  Derivation of ADLB (Laboratory Analysis Dataset).
              Merges ADSL with SDTM LB to derive Baseline Flags (ABLFL),
              Change from Baseline (CHG), and Normal/Abnormal Indicators (LBNRIND).
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. MERGE ADSL AND LB */
proc sort data=adam.adsl out=work.adsl_sorted;
    by USUBJID;
run;

proc sort data=sdtm.lb out=work.lb_sorted;
    by USUBJID LBDTC LBTESTCD;
run;

data work.adlb_draft;
    retain STUDYID USUBJID PARAMCD PARAM;
    
    merge work.adsl_sorted(in=a) work.lb_sorted(in=b);
    by USUBJID;
    if b; /* Keep only records that have lab tests */

    /* --------------------------------------------------------
       CORE ANALYSIS VARIABLES
       -------------------------------------------------------- */
    PARAMCD = LBTESTCD;
    PARAM   = LBTEST;
    AVAL    = LBSTRESN;
    AVISIT  = VISIT;
    AVISITN = VISITNUM;
    SRCDOM  = 'LB';
    SRCVAR  = 'LBSEQ';
    SRCSEQ  = LBSEQ;

    /* Convert ISO Date to Numeric Analysis Date */
    if length(LBDTC) >= 10 then ASTDT = input(substr(LBDTC, 1, 10), yymmdd10.);
    format ASTDT date9.;

    /* Analysis Relative Day (ASTDY) */
    if not missing(ASTDT) and not missing(TRTSDT) then do;
        if ASTDT >= TRTSDT then ASTDY = (ASTDT - TRTSDT) + 1;
        else ASTDY = ASTDT - TRTSDT;
    end;

    keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL
         AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ;
run;


/* 2. IDENTIFY BASELINE RECORD */
proc sort data=work.adlb_draft out=work.adlb_base_candidates;
    by USUBJID PARAMCD ASTDT AVISITN SRCSEQ;
    where ASTDT <= TRTSDT and not missing(AVAL);
run;

data work.base_flags;
    set work.adlb_base_candidates;
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
proc sort data=work.adlb_draft;
    by USUBJID PARAMCD;
run;

data adam.adlb;
    retain STUDYID USUBJID PARAMCD PARAM;
    
    merge work.adlb_draft(in=a) work.base_flags(in=b);
    by USUBJID PARAMCD;
    if a;

    /* Flag the exact source record; the QC gate rejects same-day ambiguity. */
    if SRCSEQ = BASE_SEQ then ABLFL = 'Y';
    else ABLFL = '';

    /* Calculate Change from Baseline */
    if not missing(AVAL) and not missing(BASE) then do;
        CHG = AVAL - BASE;
    end;
    
   /* --------------------------------------------------------
       5. DERIVE ABNORMALITY INDICATOR (LBNRIND)
       (Applies clinical logic with internal unit conversion)
       -------------------------------------------------------- */
    length LBNRIND $10;
    
    if not missing(AVAL) then do;
        /* Variaveis temporárias para não alterar o AVAL original submetido */
        _calc_val = AVAL;
        
        select (PARAMCD);
            when ('GLUC') do;
                /* Convert mg/dL to mmol/L (factor: 0.0555) */
                if upcase(LBSTRESU) = 'MG/DL' then _calc_val = AVAL * 0.0555;
                
                if _calc_val < 3.9 then LBNRIND = 'LOW';
                else if _calc_val > 5.6 then LBNRIND = 'HIGH';
                else LBNRIND = 'NORMAL';
            end;
            
            when ('HGB') do;
                /* Convert g/dL to g/L (factor: 10) */
                if upcase(LBSTRESU) = 'G/DL' then _calc_val = AVAL * 10;
                
                if _calc_val < 120 then LBNRIND = 'LOW';
                else if _calc_val > 175 then LBNRIND = 'HIGH';
                else LBNRIND = 'NORMAL';
            end;
            
            otherwise LBNRIND = 'UNKNOWN'; /* Para ALT e AST, omitimos nesta prova de conceito */
        end;
    end;
    drop _calc_val BASE_DT BASE_SEQ;
    
run;

/* Re-sort for final presentation */
proc sort data=adam.adlb;
    by USUBJID PARAMCD ASTDT;
run;


/* 4. VISUAL AUDIT */
title "ADLB Audit - Laboratory Analysis with Clinical Indicators";
proc print data=adam.adlb(obs=15);
    var USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND;
run;
title;

--- end of SAS program ---

Write the complete R script now.