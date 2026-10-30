Translate the SAS program below to R.

Program id: P015   file: P015_sdtm_lb.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P015
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P015
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P015.lst (table/listing text, 120 numbers)  -> write a text rendering and CSV of the same table
  dataset lb  -> write OUT_DIR/lb.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P015_sdtm_lb.sas) ---
/*******************************************************************************
Program Name: sdtm_lb.sas
Description:  Mapping of raw EDC data to the SDTM LB (Laboratory) domain.
              Demonstrates conditional dictionary mapping for test codes, 
              categories, and standardization of SI units.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. IMPORT RAW LAB DATA */
proc import datafile="&project_path./data/raw/raw_lab.csv"
    out=work.raw_lb
    dbms=csv
    replace;
    getnames=yes;
run;


/* 2. CORE TRANSFORMATION & DICTIONARY MAPPING */
data work.lb_mapped;
    set work.raw_lb;
    
    /* Pre-define lengths to prevent truncation */
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length.
           LBTESTCD $8 LBTEST $40 LBCAT $20 LBORRES $20 LBORRESU $20
           LBSTRESC $20 LBSTRESU $20;
    
    /* Core Identifiers */
    STUDYID = "CDISC-01";
    DOMAIN  = "LB";
    USUBJID = catx("-", STUDYID, SUBJ);
    VISIT   = strip(VISIT_NAM);
    VISITNUM = VISIT_NUM;
    
    /* --------------------------------------------------------
       TEST CODES, CATEGORIES, AND UNITS MAPPING
       -------------------------------------------------------- */
    /* Note: In a real CRO, this is usually merged from a master Excel 
       dictionary or Medidata Rave/Codelist format. Here we hardcode the logic. */
       
    select (upcase(strip(TEST_NAME)));
        when ('GLUCOSE') do;
            LBTESTCD = 'GLUC';
            LBTEST   = 'Glucose';
            LBCAT    = 'CHEMISTRY';
        end;
        when ('HEMOGLOBIN') do;
            LBTESTCD = 'HGB';
            LBTEST   = 'Hemoglobin';
            LBCAT    = 'HEMATOLOGY';
        end;
        when ('ALT') do;
            LBTESTCD = 'ALT';
            LBTEST   = 'Alanine Aminotransferase';
            LBCAT    = 'CHEMISTRY';
        end;
        when ('AST') do;
            LBTESTCD = 'AST';
            LBTEST   = 'Aspartate Aminotransferase';
            LBCAT    = 'CHEMISTRY';
        end;
        otherwise do;
            LBTESTCD = 'UNKNOWN';
            LBTEST   = TEST_NAME;
            LBCAT    = 'UNKNOWN';
        end;
    end;
    
    /* Results Formatting */
    LBORRES  = strip(put(RESULT, best.)); /* Original as text */
    LBSTRESN = RESULT;                    /* Standard as numeric */
    LBSTRESC = LBORRES;                   /* Standard as text */
    
    /* Standardize units (Cleaning up 'mg/dL' vs 'MG/DL' from the EDC) */
    LBORRESU = strip(UNIT);
    if upcase(LBORRESU) = 'MG/DL' then LBSTRESU = 'mg/dL';
    else if upcase(LBORRESU) = 'G/DL' then LBSTRESU = 'g/dL';
    else if upcase(LBORRESU) = 'U/L' then LBSTRESU = 'U/L';
    else LBSTRESU = LBORRESU;
    
    /* --------------------------------------------------------
       ISO 8601 DATE CONVERSION
       -------------------------------------------------------- */
    if vtype(LAB_DAT) = 'C' then _lab_num = input(strip(LAB_DAT), anydtdte.);
    else _lab_num = LAB_DAT;
    
    if not missing(_lab_num) then LBDTC = put(_lab_num, is8601da.);
    
    keep STUDYID DOMAIN USUBJID VISITNUM VISIT LBTESTCD LBTEST LBCAT LBORRES LBORRESU LBSTRESC LBSTRESN LBSTRESU LBDTC;
run;


/* 3. GENERATE SEQUENCE NUMBER (LBSEQ) */
proc sort data=work.lb_mapped;
    by USUBJID LBDTC LBTESTCD;
run;

data sdtm.lb;
	retain STUDYID DOMAIN USUBJID LBSEQ;
    set work.lb_mapped;
    by USUBJID;
    
    /* Retain and increment a counter for each Subject */
    if first.USUBJID then LBSEQ = 1;
    else LBSEQ + 1;
run;


/* 4. VISUAL AUDIT */
title "LB Domain Audit (Laboratory Test Results)";
proc print data=sdtm.lb(obs=12);
    var USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC;
run;
title;

--- end of SAS program ---

Write the complete R script now.