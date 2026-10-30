Translate the SAS program below to R.

Program id: P021   file: P021_tlf_table1.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P021
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P021
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P021.lst (table/listing text, 56 numbers)  -> write a text rendering and CSV of the same table

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P021_tlf_table1.sas) ---
/*******************************************************************************
Program Name: tlf_table1.sas
Description:  Generates Table 1: Demographics and Baseline Characteristics.
              Uses ODS RTF to export a professional Word document.
*******************************************************************************/

/* 1. INITIALIZE MASTER CONFIGURATION */
/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* Open ODS RTF destination with Journal style without closing SAS Studio output */
ods rtf file="&project_path./tlfs/tlf_table1.rtf" style=Journal;

title1 "Table 1: Demographics and Baseline Characteristics";
title2 "Population: Intent-to-Treat (ITT)";

proc tabulate data=adam.adsl missing;
    /* Filter only randomized subjects */
    where ITTFL = 'Y'; 
    
    class TRT01P SEX RACE; 
    var AGE; 
    
    table 
        /* --- ROWS (Clean formatting and percentages) --- */
        ALL="Total Subjects" * N=""
        SEX="Sex" * (N="n" * f=5.0 ColPctN="%" * f=5.1)
        RACE="Race" * (N="n" * f=5.0 ColPctN="%" * f=5.1)
        AGE="Age (years)" * (N="n" * f=5.0 Mean="Mean" * f=5.1 Std="SD" * f=5.2 Min="Min" * f=5.0 Max="Max" * f=5.0),
        
        /* --- COLUMNS --- */
        (TRT01P="Planned Treatment" ALL="Total")
        
        / box="Demographic Parameter" row=float misstext="0";
run;

title;

/* Close only the RTF file */
ods rtf close;

--- end of SAS program ---

Write the complete R script now.