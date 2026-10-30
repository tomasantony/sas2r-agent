Translate the SAS program below to R.

Program id: P010   file: P010_sdtm_cm.sas   source: MrCosta77/Clinical-data-to-CDISC   family: c2c
Declared output types: listing;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P010
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P010
  &project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
  &usubjid_length. = 40

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P010.lst (table/listing text, 195 numbers)  -> write a text rendering and CSV of the same table
  dataset cm  -> write OUT_DIR/cm.csv

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P010_sdtm_cm.sas) ---
/*******************************************************************************
Program Name: sdtm_cm.sas
Description:  Creates SDTM Concomitant Medications (CM) domain.
              Maps real-world background medication to CDISC standards.
*******************************************************************************/

/* 1. INITIALIZE MASTER CONFIGURATION */
/* Execute through RUN_ALL.SAS, which initializes PROJECT_PATH and libraries. */

/* 2. IMPORT RAW DATA (Defensive Programming: Bypassing PROC IMPORT) */
data work.raw_cm;
    /* Lemos o CSV diretamente, dizendo ao SAS para não adivinhar nada */
    infile "&project_path./data/raw/raw_conmeds.csv" delimiter=',' dsd missover firstobs=2;
    
    /* Forçamos as datas a entrar como texto seguro ($10.) */
    length SUBJECT $10 MEDICATION $50 DOSE $20 START_DT $10 END_DT $10 ONGOING $1;
    input SUBJECT $ MEDICATION $ DOSE $ START_DT $ END_DT $ ONGOING $;
run;

/* 3. PROCESS AND MAP TO SDTM */
data work.cm_draft;
    length STUDYID $8 DOMAIN $2 USUBJID $&usubjid_length.
           CMTRT CMDECOD $50 CMDOSU $20 CMSTDTC CMENDTC $10 CMENRTPT $8;
    set work.raw_cm;
    
    /* Identifiers */
    STUDYID = "CDISC-01";
    DOMAIN  = "CM";
    USUBJID = strip(STUDYID) || "-" || strip(SUBJECT);
    
    /* Educational medication normalization. The approved Paracetamol source
       synonym is represented by the standard US ingredient name. */
    CMTRT = strip(MEDICATION);
    select (upcase(CMTRT));
        when ('PARACETAMOL') CMDECOD = 'ACETAMINOPHEN';
        otherwise CMDECOD = upcase(CMTRT);
    end;
    
    /* Dose and Unit parsing */
    CMDOSE = input(scan(DOSE, 1, ' '), best.);
    CMDOSU = strip(scan(DOSE, 2, ' '));
    
    /* Dates to ISO 8601 (Agora a conversão funciona a 100% porque a origem é texto limpo!) */
    if not missing(START_DT) then do;
        _start = input(START_DT, ddmmyy10.);
        CMSTDTC = put(_start, is8601da.);
    end;
    
    if not missing(END_DT) then do;
        _end = input(END_DT, ddmmyy10.);
        CMENDTC = put(_end, is8601da.);
    end;
    
    /* The synthetic EDC defines ONGOING at STUDY_END_DAT, which becomes
       DM.RFENDTC. CMENTPT is attached below from that existing assessment
       boundary; it is not an imputed medication stop date. */
    if ONGOING = 'Y' then CMENRTPT = 'ONGOING';
    else CMENRTPT = '';
    
    keep STUDYID DOMAIN USUBJID CMTRT CMDECOD CMDOSE CMDOSU CMSTDTC CMENDTC CMENRTPT;
run;

/* 4. SORT AND ASSIGN SEQUENCE NUMBER (CMSEQ) */
proc sort data=work.cm_draft;
    by USUBJID CMSTDTC CMTRT;
run;

proc sort data=sdtm.dm(keep=USUBJID RFENDTC)
          out=work.dm_cm_anchor;
    by USUBJID;
run;

data work.dm_cm_anchor;
    set work.dm_cm_anchor;
    length _CMENTPT_ANCHOR $10;
    _CMENTPT_ANCHOR = RFENDTC;
    keep USUBJID _CMENTPT_ANCHOR;
run;

data sdtm.cm;
    /* Retain ensures strict CDISC column order */
    retain STUDYID DOMAIN USUBJID CMSEQ CMTRT CMDECOD CMDOSE CMDOSU
           CMSTDTC CMENDTC CMENRTPT CMENTPT;
    merge work.cm_draft(in=in_cm) work.dm_cm_anchor;
    by USUBJID;
    if in_cm;

    /* CMENRTPT must always identify the reference time point it describes. */
    if CMENRTPT = 'ONGOING' then do;
        CMENTPT = _CMENTPT_ANCHOR;
        if missing(CMENTPT) then
            putlog 'ERROR: Ongoing CM record has no DM reference end: ' USUBJID=;
    end;
    else CMENTPT = '';
    
    if first.USUBJID then CMSEQ = 1;
    else CMSEQ + 1;

    drop _CMENTPT_ANCHOR;
run;

/* 5. VISUAL AUDIT */
title "SDTM CM Domain - Concomitant Medications";
proc print data=sdtm.cm(obs=15);
run;
title;

--- end of SAS program ---

Write the complete R script now.