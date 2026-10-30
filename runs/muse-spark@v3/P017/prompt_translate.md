Translate the SAS program below to R.

Program id: P017   file: P017_sdtm_sv.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P017
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P017
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P017.lst (table/listing text, 168 numbers)  -> write a text rendering and CSV of the same table
  dataset sv  -> write OUT_DIR/sv.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P017_sdtm_sv.sas) ---
/*******************************************************************************
Program Name: sdtm_sv.sas
Description:  Creates one SDTM SV record for each actual subject visit.
*******************************************************************************/

/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

proc import datafile="&project_path./data/raw/raw_vitals.csv"
    out=work.raw_sv dbms=csv replace;
    getnames=yes;
run;

data sdtm.sv;
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length. VISIT $40
           SVSTDTC SVENDTC $10;
    retain STUDYID DOMAIN USUBJID VISITNUM VISIT VISITDY SVSTDTC SVENDTC;
    set work.raw_sv;

    STUDYID = "CDISC-01";
    DOMAIN = "SV";
    USUBJID = catx("-", STUDYID, PT_ID);
    VISITNUM = VISIT_NUM;
    VISIT = strip(VISIT);
    select (VISITNUM);
        when (1) VISITDY = 1;
        when (2) VISITDY = 31;
        when (3) VISITDY = 61;
        otherwise VISITDY = .;
    end;

    /*
       VVALUE works for either a character date or a formatted numeric SAS date.
       Width 32 is explicit so DATE11 values such as 15-JAN-2023 are not
       truncated to 15-JAN-20 and misread as a date in 2020.
    */
    _svdt = input(strip(vvalue(VS_DATE)), anydtdte32.);
    if not missing(_svdt) then do;
        SVSTDTC = put(_svdt, is8601da.);
        SVENDTC = SVSTDTC;
    end;

    keep STUDYID DOMAIN USUBJID VISITNUM VISIT VISITDY SVSTDTC SVENDTC;
run;

/* Sort without deleting records: duplicate keys must remain visible to QC. */
proc sort data=sdtm.sv;
    by USUBJID VISITNUM;
run;

title "SV Domain Audit";
proc print data=sdtm.sv(obs=12);
run;
title;

--- end of SAS program ---

Write the complete R script now.