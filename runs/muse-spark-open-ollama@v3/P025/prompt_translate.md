Translate the SAS program below to R.

Program id: P025   file: P025_ex.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P025
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P025

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
  P025.lst (table/listing text, 102 numbers)  -> write a text rendering and CSV of the same table

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P025_ex.sas) ---
/****************************************************************/
/* STEP 3.1: SDTM EX MAPPING PROGRAM                            */
/****************************************************************/


/*--------------------------------------------------------------*/
/* 1. Get RFSTDTC from DM for Study Day Calculation             */
/*--------------------------------------------------------------*/

proc sort 
    data=sdtm_dm(
        keep=USUBJID RFSTDTC
    )
    out=dm_for_ex;

    by USUBJID;

run;


/*--------------------------------------------------------------*/
/* 2. Prepare Raw EX Data                                       */
/*--------------------------------------------------------------*/

data ex_prep;

    set edc_ex;

    /* Create USUBJID */

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);


    /* Exclude records where dose was not administered */

    if DOSE ne .;


    /* Convert Dates to ISO 8601 */

    length EXSTDTC EXENDTC $19;

    if STDT ne "" then
        EXSTDTC = put(input(STDT, date11.), yymmdd10.);

    if ENDT ne "" then
        EXENDTC = put(input(ENDT, date11.), yymmdd10.);

run;


/*--------------------------------------------------------------*/
/* 3. Sort Data for Merge and Sequencing                        */
/*--------------------------------------------------------------*/

proc sort data=ex_prep;

    by USUBJID EXSTDTC;

run;


/*--------------------------------------------------------------*/
/* 4. Create Intermediate EX Dataset                            */
/*--------------------------------------------------------------*/

data sdtm_ex1;

    /* Define Variable Lengths */

    length 
        STUDYID   $10
        DOMAIN    $2
        USUBJID   $25
        EXTRT     $40
        EXDOSE    8
        EXDOSU    $10
        EXDOSFRM  $30
        EXDOSFRQ  $10
        EXVLDIST  $20
        EXLOT     $15
        EXSTDTC   $19
        EXENDTC   $19
        VISIT     $40;

    
    /* Merge EX Data with DM */

    merge 
        ex_prep(in=a)
        dm_for_ex;

    by USUBJID;

    if a;


    /* Identifier Variables */

    STUDYID = "ONCO-2026";

    DOMAIN = "EX";


    /* Exposure Variables */

    EXTRT = upcase(TRT);

    EXDOSE = DOSE;

    EXDOSU = upcase(UNIT);

    EXDOSFRM = tranwrd(FORM, "_", " ");

    EXDOSFRQ = upcase(FREQ);

    EXVLDIST = upcase(ROUTE);

    EXLOT = LOT;


    /* Derive EXSTDY */

    if EXSTDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(EXSTDTC, yymmdd10.);

        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            EXSTDY = (_stdt - _rfdt) + 1;

        else
            EXSTDY = (_stdt - _rfdt);

    end;


    /* Derive EXENDY */

    if EXENDTC ne "" and RFSTDTC ne "" then do;

        _endt = input(EXENDTC, yymmdd10.);

        _rfdt = input(RFSTDTC, yymmdd10.);

        if _endt >= _rfdt then
            EXENDY = (_endt - _rfdt) + 1;

        else
            EXENDY = (_endt - _rfdt);

    end;

run;


/*--------------------------------------------------------------*/
/* 5. Sort Final EX Dataset                                     */
/*--------------------------------------------------------------*/

proc sort data=sdtm_ex1;

    by USUBJID EXSTDTC;

run;


/*--------------------------------------------------------------*/
/* 6. Derive EXSEQ                                              */
/*--------------------------------------------------------------*/

data sdtm_ex;

    set sdtm_ex1;

    by USUBJID;

    retain EXSEQ;

    if first.USUBJID then
        EXSEQ = 1;

    else
        EXSEQ + 1;


    /* Keep Final SDTM Variables */

    keep
        STUDYID
        DOMAIN
        USUBJID
        EXSEQ
        EXTRT
        EXDOSE
        EXDOSU
        EXDOSFRM
        EXDOSFRQ
        EXVLDIST
        EXLOT
        EXSTDTC
        EXENDTC
        EXSTDY
        EXENDY
        VISIT;

run;


/*--------------------------------------------------------------*/
/* 7. View Final EX Dataset                                     */
/*--------------------------------------------------------------*/

proc print data=sdtm_ex;
run;

--- end of SAS program ---

Write the complete R script now.