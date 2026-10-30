Translate the SAS program below to R.

Program id: P009   file: P009_sdtm_ae.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P009
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P009
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P009.lst (table/listing text, 129 numbers)  -> write a text rendering and CSV of the same table
  dataset ae  -> write OUT_DIR/ae.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P009_sdtm_ae.sas) ---
/*******************************************************************************
Program Name: sdtm_ae.sas
Description:  Mapping of raw EDC data to the SDTM AE (Adverse Events) domain.
              Demonstrates derivation of Sequence Numbers (AESEQ), dictionary 
              mapping placeholders, and handling of ongoing clinical events.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. IMPORT RAW ADVERSE EVENTS */
proc import datafile="&project_path./data/raw/raw_ae.csv"
    out=work.raw_ae
    dbms=csv
    replace;
    getnames=yes;
    /* Scan the complete synthetic extract so longer terms are not truncated. */
    guessingrows=max;
run;


/* 2. CORE TRANSFORMATION & DERIVATION */
data work.ae_mapped;
    set work.raw_ae;
    
    /* Pre-define variable lengths to prevent SAS truncation */
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length.
           AETERM AEDECOD $60 AEREL $15 AESEV $10 AEOUT $30 AESER $1;
    
    /* Core Identifiers */
    STUDYID = "CDISC-01";
    DOMAIN  = "AE";
    USUBJID = catx("-", STUDYID, ID);
    
    /* --------------------------------------------------------
       EVENT TERMINOLOGY & ATTRIBUTES
       -------------------------------------------------------- */
    AETERM = strip(AE_TERM);
    AEDECOD = upcase(AETERM);
    
    /* Severity */
    AESEV = upcase(strip(SEV));
    
    /* Serious Event Flag */
    if upcase(strip(SERIOUS)) = 'Y' then AESER = 'Y';
    else AESER = 'N';
    
    /* Causality / Relationship to Study Drug */
    if upcase(strip(RELATED)) = 'Y' then AEREL = 'RELATED';
    else if upcase(strip(RELATED)) = 'N' then AEREL = 'NOT RELATED';
    else AEREL = 'UNKNOWN';
    
    /* --------------------------------------------------------
       ISO 8601 DATE CONVERSION & OUTCOME DERIVATION
       -------------------------------------------------------- */
    if vtype(START) = 'C' then _start_num = input(strip(START), anydtdte.);
    else _start_num = START;
    if not missing(_start_num) then AESTDTC = put(_start_num, is8601da.);
    
    if vtype(END) = 'C' then _end_num = input(strip(END), anydtdte.);
    else _end_num = END;
    
    if not missing(_end_num) then do;
        AEENDTC = put(_end_num, is8601da.);
        AEOUT   = 'RECOVERED/RESOLVED';
    end;
    else do;
        AEENDTC = "";
        AEOUT   = 'NOT RECOVERED/NOT RESOLVED';
    end;

    keep STUDYID DOMAIN USUBJID AETERM AEDECOD AESEV AESER AEREL AESTDTC AEENDTC AEOUT;
run;


/* 3. GENERATE SEQUENCE NUMBER (AESEQ) */
/* Sort by Subject and Start Date first to ensure chronological sequence */
proc sort data=work.ae_mapped;
    by USUBJID AESTDTC;
run;

/* Use BY-group processing to create the Sequence Number */
data sdtm.ae;
	retain STUDYID DOMAIN USUBJID AESEQ;
    set work.ae_mapped;
    by USUBJID;
    
    /* Retain and increment a counter for each Subject */
    if first.USUBJID then AESEQ = 1;
    else AESEQ + 1;
run;


/* 4. VISUAL AUDIT */
title "AE Domain Audit (Adverse Events with Ongoing Logic)";
proc print data=sdtm.ae(obs=12);
    var USUBJID AESEQ AETERM AESEV AESER AEREL AESTDTC AEENDTC AEOUT;
run;
title;

--- end of SAS program ---

Write the complete R script now.