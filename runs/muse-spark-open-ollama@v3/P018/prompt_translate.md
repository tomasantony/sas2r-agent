Translate the SAS program below to R.

Program id: P018   file: P018_sdtm_vs.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P018
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P018
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P018.lst (table/listing text, 132 numbers)  -> write a text rendering and CSV of the same table
  dataset vs  -> write OUT_DIR/vs.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P018_sdtm_vs.sas) ---
/*******************************************************************************
Program Name: sdtm_vs.sas
Description:  Mapping of raw EDC data to the SDTM VS (Vital Signs) domain.
              Demonstrates horizontal-to-vertical unpivoting and unit mapping.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 1. IMPORT RAW VITALS */
proc import datafile="&project_path./data/raw/raw_vitals.csv"
    out=work.raw_vs
    dbms=csv
    replace;
    getnames=yes;
run;


/* 2. HORIZONTAL TO VERTICAL TRANSFORMATION (UNPIVOT) */
data work.vs_mapped;
    set work.raw_vs;
    
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length.
           VSTESTCD $8 VSTEST $40 VSORRESU VSSTRESU $20 VSORRES $200;

    STUDYID = "CDISC-01";
    DOMAIN  = "VS";
    USUBJID = catx("-", STUDYID, PT_ID); 
    VISITNUM = VISIT_NUM;

    if vtype(VS_DATE) = 'C' then _vs_num = input(strip(VS_DATE), anydtdte.);
    else _vs_num = VS_DATE;
    if not missing(_vs_num) then VSDTC = put(_vs_num, is8601da.);

    /* 1. Systolic Blood Pressure */
    if not missing(SYS_BP) then do;
        VSTESTCD = "SYSBP";
        VSTEST   = "Systolic Blood Pressure";
        VSORRES  = strip(put(SYS_BP, best.));
        VSSTRESN = SYS_BP;
        VSORRESU = "mmHg";
        VSSTRESU = "mmHg";
        output;
    end;

    /* 2. Diastolic Blood Pressure */
    if not missing(DIA_BP) then do;
        VSTESTCD = "DIABP";
        VSTEST   = "Diastolic Blood Pressure";
        VSORRES  = strip(put(DIA_BP, best.));
        VSSTRESN = DIA_BP;
        VSORRESU = "mmHg";
        VSSTRESU = "mmHg";
        output;
    end;

    /* 3. Heart Rate -> CDISC Official: PULSE */
    if not missing(HR_BPM) then do;
        VSTESTCD = "PULSE";
        VSTEST   = "Pulse Rate";
        VSORRES  = strip(put(HR_BPM, best.));
        VSSTRESN = HR_BPM;
        VSORRESU = "beats/min";
        VSSTRESU = "beats/min";
        output;
    end;

    /* 4. Weight */
    if not missing(WEIGHT_KG) then do;
        VSTESTCD = "WEIGHT";
        VSTEST   = "Weight";
        VSORRES  = strip(put(WEIGHT_KG, best.));
        VSSTRESN = WEIGHT_KG;
        VSORRESU = "kg";
        VSSTRESU = "kg";
        output;
    end;

    keep STUDYID DOMAIN USUBJID VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU;
run;


/* 3. GENERATE SEQUENCE NUMBER (VSSEQ) */
proc sort data=work.vs_mapped;
    by USUBJID VSDTC VSTESTCD;
run;

data sdtm.vs;
	retain STUDYID DOMAIN USUBJID VSSEQ;
    set work.vs_mapped;
    by USUBJID;
    if first.USUBJID then VSSEQ = 1;
    else VSSEQ + 1;
run;


/* 4. VISUAL AUDIT */
title "VS Domain Audit (Unpivoted Format with VSSEQ)";
proc print data=sdtm.vs(obs=12);
    var USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU;
run;
title;

--- end of SAS program ---

Write the complete R script now.