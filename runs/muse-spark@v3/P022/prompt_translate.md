Translate the SAS program below to R.

Program id: P022   file: P022_ae.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P022
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P022

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
  P022.lst (table/listing text, 56 numbers)  -> write a text rendering and CSV of the same table
  dataset ae  -> write OUT_DIR/ae.csv

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P022_ae.sas) ---
/****************************************************************/
/* STEP 4.1: SDTM AE MAPPING & DERIVATION                       */
/****************************************************************/


/*--------------------------------------------------------------*/
/* 1. Sort DM for Study Day Calculations                        */
/*--------------------------------------------------------------*/

proc sort 
    data=sdtm_dm(
        keep=USUBJID RFSTDTC
    )
    out=dm_for_ae;

    by USUBJID;

run;


/*--------------------------------------------------------------*/
/* 2. Process Raw AE Data                                       */
/*--------------------------------------------------------------*/

data ae_prep;

    set edc_ae;
    
    /* Create USUBJID */

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);


    /* Convert Raw Dates to ISO 8601 */

    length AESTDTC AEENDTC $19;

    if STDT ne "" then
        AESTDTC = put(input(STDT, date11.), yymmdd10.);

    if ENDT ne "" then
        AEENDTC = put(input(ENDT, date11.), yymmdd10.);

run;


/*--------------------------------------------------------------*/
/* 3. Sort AE Data                                              */
/*--------------------------------------------------------------*/

proc sort data=ae_prep;

    by USUBJID AESTDTC TERM;

run;


/*--------------------------------------------------------------*/
/* 4. Final Mapping and Derivation                              */
/*--------------------------------------------------------------*/

data sdtm_ae1;

    /* Define Variable Lengths */

    length 
        STUDYID $10
        DOMAIN $2
        USUBJID $25
        AETERM $40
        AEDECOD $40
        AEBODSYS $40
        AESEV $10
        AETXGR 8
        AEREL $20
        AEOUT $20
        AESER $1
        AESTDTC $19
        AEENDTC $19;

    
    /* Merge AE Data with DM */

    merge 
        ae_prep(in=a)
        dm_for_ae;

    by USUBJID;

    if a;


    /* Identifier Variables */

    STUDYID = "ONCO-2026";

    DOMAIN = "AE";


    /* AE Variables */

    AETERM = TERM;

    AEDECOD = upcase(DECOD);

    /* Format Body System */

    AEBODSYS = tranwrd(BODSYS, "_", " ");

    AESEV = upcase(SEV);

    AETXGR = TXGR;

    AEREL = upcase(REL);

    AEOUT = upcase(OUT);

    AESER = SER;


    /*----------------------------------------------------------*/
    /* Derive AESTDY                                            */
    /*----------------------------------------------------------*/

    if AESTDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(AESTDTC, yymmdd10.);

        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            AESTDY = (_stdt - _rfdt) + 1;

        else
            AESTDY = (_stdt - _rfdt);

    end;


    /*----------------------------------------------------------*/
    /* Derive AEENDY                                            */
    /*----------------------------------------------------------*/

    if AEENDTC ne "" and RFSTDTC ne "" then do;

        _endt = input(AEENDTC, yymmdd10.);

        _rfdt = input(RFSTDTC, yymmdd10.);

        if _endt >= _rfdt then
            AEENDY = (_endt - _rfdt) + 1;

        else
            AEENDY = (_endt - _rfdt);

    end;

run;


/*--------------------------------------------------------------*/
/* 5. Sort Final AE Dataset                                     */
/*--------------------------------------------------------------*/

proc sort data=sdtm_ae1;

    by USUBJID AESTDTC TERM;

run;


/*--------------------------------------------------------------*/
/* 6. Derive AESEQ                                              */
/*--------------------------------------------------------------*/

data sdtm_ae;
    length 
        STUDYID $10
        DOMAIN $2
        USUBJID $25
		    AESEQ 8
        AETERM $40
        AEDECOD $40
        AEBODSYS $40
        AESEV $10
        AETXGR 8
        AEREL $20
        AEOUT $20
        AESER $1
        AESTDTC $19
        AEENDTC $19;

    set sdtm_ae1;

    by USUBJID;

    retain AESEQ;

    if first.USUBJID then
        AESEQ = 1;

    else
        AESEQ + 1;


    /* Keep Final SDTM Variables */

    keep
        STUDYID
        DOMAIN
        USUBJID
        AESEQ
        AETERM
        AEDECOD
        AEBODSYS
        AESEV
        AETXGR
        AEREL
        AEOUT
        AESER
        AESTDTC
        AEENDTC
        AESTDY
        AEENDY;

run;


/*--------------------------------------------------------------*/
/* 7. View Final AE Dataset                                     */
/*--------------------------------------------------------------*/

proc print data=sdtm_ae;
run;

/*--------------------------------------------------------------*/
/* 8. Export Final AE Dataset                                   */
/*--------------------------------------------------------------*/

proc export
    data=sdtm_ae
    outfile="&outds./ae.xlsx"
    dbms=xlsx
    replace;
run;

--- end of SAS program ---

Write the complete R script now.