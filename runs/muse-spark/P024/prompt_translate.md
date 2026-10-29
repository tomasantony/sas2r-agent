Translate the SAS program below to R.

Program id: P024   file: P024_ds.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P024
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P024

WORK datasets pre-loaded by the environment from these Excel files (dataset name -> file):
  edc_ae -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx
  edc_death -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_death.xlsx
  edc_dm -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx
  edc_ds -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ds.xlsx
  edc_ex -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx
  edc_lb -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_lb.xlsx
  edc_rs -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_rs.xlsx
  edc_tr -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tr.xlsx
  edc_tu -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx
  edc_vs -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_vs.xlsx
  ivrs_rand -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P024.lst (table/listing text, 45 numbers)  -> write a text rendering and CSV of the same table
  dataset ds  -> write OUT_DIR/ds.csv

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P024_ds.sas) ---
/****************************************************************/
/* STEP 5.1: SDTM DS MAPPING & DERIVATION                       */
/****************************************************************/


/*--------------------------------------------------------------*/
/* 1. Sort DM for Reference Dates                               */
/*--------------------------------------------------------------*/

proc sort 
    data=sdtm_dm(
        keep=USUBJID SUBJID RFSTDTC DTHDTC
    )
    out=dm_for_ds;

    by USUBJID;

run;


/*--------------------------------------------------------------*/
/* 2. Prepare Raw DS Data                                       */
/*--------------------------------------------------------------*/

data edc_ds_prep;

    set edc_ds; length TERM DECOD CAT $50 STDT $11; TERM = DS_TERM; DECOD = DS_TERM; CAT = DS_CAT; STDT = DS_DT; %* benchmark: the committed raw sheet names these columns DS_TERM/DS_CAT/DS_DT and has no DECOD column, so the program as written produced an all-blank DS;

    /* Create USUBJID */

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);

run;


/*--------------------------------------------------------------*/
/* 3. Sort Datasets for Merge                                   */
/*--------------------------------------------------------------*/

proc sort data=edc_ds_prep;

    by USUBJID;

run;

proc sort data=dm_for_ds;

    by USUBJID;

run;


/*--------------------------------------------------------------*/
/* 4. Create SDTM DS Dataset                                    */
/*--------------------------------------------------------------*/

data sdtm_ds1;

    /* Define Variable Lengths */

    length 
        STUDYID $10
        DOMAIN $2
        USUBJID $25
        DSTERM $50
        DSDECOD $30
        DSCAT $30
        DSSTDTC $19
        DTHFL $1;

    
    /* Merge DS Data with DM */

    merge 
        edc_ds_prep(in=a)
        dm_for_ds;

    by USUBJID;

    if a;


    /* Identifier Variables */

    STUDYID = "ONCO-2026";

    DOMAIN = "DS";


    /* DS Variables */

    DSTERM = tranwrd(TERM, "_", " ");

    DSDECOD = upcase(DECOD);

    DSCAT = tranwrd(CAT, "_", " ");


    /* Death Flag */

    if not missing(DTHDTC) then
        DTHFL = "Y";


    /* Convert Date to ISO 8601 */

    if STDT ne "" then
        DSSTDTC = put(input(STDT, date11.), yymmdd10.);


    /*----------------------------------------------------------*/
    /* Derive DSSTDY                                            */
    /*----------------------------------------------------------*/

    if DSSTDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(DSSTDTC, yymmdd10.);

        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            DSSTDY = (_stdt - _rfdt) + 1;

        else
            DSSTDY = (_stdt - _rfdt);

    end;


    /* Consistency Check for Subject 103 */

    if USUBJID = "ONCO-2026-103" and DSSTDTC ne DTHDTC then 
        put "WARNING: Date mismatch! DS=" DSSTDTC " DM=" DTHDTC;

run;


/*--------------------------------------------------------------*/
/* 5. Sort Final DS Dataset                                     */
/*--------------------------------------------------------------*/

proc sort data=sdtm_ds1;

    by USUBJID DSSTDTC;

run;


/*--------------------------------------------------------------*/
/* 6. Derive DSSEQ                                              */
/*--------------------------------------------------------------*/

data sdtm_ds;
    length
        STUDYID $10
        DOMAIN $2
        USUBJID $25
        DSSEQ 8
        DSTERM $50
        DSDECOD $30
        DSCAT $30
        DSSTDTC $19
        DSSTDY 8
        DTHFL $1;

    set sdtm_ds1;

    by USUBJID;

    retain DSSEQ;

    if first.USUBJID then
        DSSEQ = 1;

    else
        DSSEQ + 1;


    /* Keep Final SDTM Variables */

    keep
        STUDYID
        DOMAIN
        USUBJID
        DSSEQ
        DSTERM
        DSDECOD
        DSCAT
        DSSTDTC
        DSSTDY
        DTHFL;

run;


/*--------------------------------------------------------------*/
/* 7. View Final DS Dataset                                     */
/*--------------------------------------------------------------*/

proc print data=sdtm_ds;
run;


/*--------------------------------------------------------------*/
/* 8. Export Final DS Dataset                                   */
/*--------------------------------------------------------------*/

proc export
    data=sdtm_ds
    outfile="&outds./ds.xlsx"
    dbms=xlsx
    replace;
run;

--- end of SAS program ---

Write the complete R script now.