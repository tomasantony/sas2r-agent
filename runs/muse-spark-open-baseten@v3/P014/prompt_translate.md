Translate the SAS program below to R.

Program id: P014   file: P014_sdtm_ex.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P014
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P014
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P014.lst (table/listing text, 126 numbers)  -> write a text rendering and CSV of the same table
  dataset ex  -> write OUT_DIR/ex.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P014_sdtm_ex.sas) ---
/*******************************************************************************
Program Name: sdtm_ex.sas
Description:  Mapping of raw EDC data to the SDTM EX (Exposure) domain.
              Captures study drug administration, doses, and duration.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. IMPORT RAW EXPOSURE DATA */
proc import datafile="&project_path./data/raw/raw_exposure.csv"
    out=work.raw_ex
    dbms=csv
    replace;
    getnames=yes;
run;


/* 2. CORE TRANSFORMATION */
data work.ex_mapped;
    set work.raw_ex;
    
    /* Pre-define variable lengths to prevent SAS truncation */
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length.
           EXTRT $50 EXDOSU $20 EXSTDTC $10 EXENDTC $10;
    
    /* Core Identifiers */
    STUDYID = "CDISC-01";
    DOMAIN  = "EX";
    USUBJID = catx("-", STUDYID, PATIENT);
    
    /* Treatment Details */
    EXTRT  = strip(TREATMENT);
    EXDOSE = DOSE;
    EXDOSU = strip(UNIT);
    
    /* --------------------------------------------------------
       ISO 8601 DATE CONVERSION
       -------------------------------------------------------- */
    /* Start Date */
    if vtype(START_DATE) = 'C' then _start_num = input(strip(START_DATE), anydtdte.);
    else _start_num = START_DATE;
    
    if not missing(_start_num) then EXSTDTC = put(_start_num, is8601da.);
    
    /* End Date (Handling missing end dates) */
    if vtype(END_DATE) = 'C' then _end_num = input(strip(END_DATE), anydtdte.);
    else _end_num = END_DATE;
    
    if not missing(_end_num) then EXENDTC = put(_end_num, is8601da.);
    else EXENDTC = ""; /* Ongoing Exposure */
    
    keep STUDYID DOMAIN USUBJID EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC;
run;


/* 3. GENERATE SEQUENCE NUMBER (EXSEQ) */
proc sort data=work.ex_mapped;
    by USUBJID EXSTDTC;
run;

data sdtm.ex;
	retain STUDYID DOMAIN USUBJID EXSEQ;
    set work.ex_mapped;
    by USUBJID;
    
    /* Retain and increment a counter for each Subject */
    if first.USUBJID then EXSEQ = 1;
    else EXSEQ + 1;
run;


/* 4. VISUAL AUDIT */
title "EX Domain Audit (Study Drug Exposure)";
proc print data=sdtm.ex(obs=10);
    var USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC;
run;
title;

--- end of SAS program ---

Write the complete R script now.