Translate the SAS program below to R.

Program id: P012   file: P012_sdtm_ds.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P012
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P012
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P012.lst (table/listing text, 108 numbers)  -> write a text rendering and CSV of the same table
  dataset ds  -> write OUT_DIR/ds.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P012_sdtm_ds.sas) ---
/*******************************************************************************
Program Name: sdtm_ds.sas
Description:  Creates the final study disposition for every subject.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

proc import datafile="&project_path./data/raw/raw_demog.csv"
    out=work.raw_ds dbms=csv replace;
    getnames=yes;
run;

data sdtm.ds;
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length. DSCAT DSSCAT $40
           DSTERM DSDECOD $80 EPOCH $20 DSSTDTC $10;
    retain STUDYID DOMAIN USUBJID DSSEQ DSCAT DSSCAT DSTERM DSDECOD EPOCH DSSTDTC;
    set work.raw_ds;

    STUDYID = "CDISC-01";
    DOMAIN = "DS";
    USUBJID = catx("-", STUDYID, SUBJ_ID);
    DSSEQ = 1;
    DSCAT = "DISPOSITION EVENT";
    DSSCAT = "STUDY PARTICIPATION";
    DSTERM = upcase(strip(DISPOSITION_REASON));
    DSDECOD = DSTERM;
    if upcase(strip(DISPOSITION)) = "SCREEN FAILURE" then EPOCH = "SCREENING";
    else EPOCH = "FOLLOW-UP";

    if vtype(STUDY_END_DAT) = 'C' then _dsdt = input(strip(STUDY_END_DAT), anydtdte.);
    else _dsdt = STUDY_END_DAT;
    if not missing(_dsdt) then DSSTDTC = put(_dsdt, is8601da.);

    keep STUDYID DOMAIN USUBJID DSSEQ DSCAT DSSCAT DSTERM DSDECOD EPOCH DSSTDTC;
run;

/* Sort without deleting records: duplicate keys must remain visible to QC. */
proc sort data=sdtm.ds;
    by USUBJID DSSEQ;
run;

title "DS Domain Audit";
proc print data=sdtm.ds(obs=12);
run;
title;

--- end of SAS program ---

Write the complete R script now.