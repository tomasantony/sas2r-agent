Translate the SAS program below to R.

Program id: P028   file: P028_tr.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P028
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P028

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
  P028.lst (table/listing text, 52 numbers)  -> write a text rendering and CSV of the same table
  dataset tr  -> write OUT_DIR/tr.csv

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P028_tr.sas) ---
/****************************************************************/
/* STEP 9.1: SDTM TR MAPPING & DERIVATION                       */
/****************************************************************/

/* 1. Prepare DM Reference Data */

proc sort data=sdtm_dm
          out=dm_for_tr(keep=USUBJID RFSTDTC);
    by USUBJID;
run;

/* 2. Prepare Raw Data */

data tr_prep;

    set edc_tr;

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);

run;

proc sort data=tr_prep;
    by USUBJID;
run;

proc sort data=dm_for_tr;
    by USUBJID;
run;

/* 3. Map TR Domain */

data sdtm_tr1;

    length STUDYID $10 DOMAIN $2 USUBJID $25
           TRLNKID $5
           TRTESTCD $8
           TRTEST $40
           TRORRES $10
           TRORRESU $10
           TRSTRESC $10
           TRSTRESN 8
           TRSTRESU $10
           TREVAL $15
           VISIT $20
           TRDTC $19;

    merge tr_prep(in=t)
          dm_for_tr;

    by USUBJID;

    if t;

    STUDYID = "ONCO-2026";
    DOMAIN  = "TR";

    TRLNKID = LNKID;

    TRTESTCD = TESTCD;

    if TRTESTCD = "LDIAM" then
        TRTEST = "Longest Diameter";

    TRORRES  = ORRES;
    TRORRESU = UNIT;

    TRSTRESC = ORRES;
    TRSTRESN = input(ORRES, best.);
    TRSTRESU = UNIT;

    TREVAL = "INVESTIGATOR";

    VISIT = upcase(tranwrd(VISIT, "_", " "));

    /* Date Conversion */

    if TRDT ne "" then
        TRDTC = put(input(TRDT, date11.), yymmdd10.);

    /* Study Day */

    if TRDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(TRDTC, yymmdd10.);
        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            TRSTDY = (_stdt - _rfdt) + 1;

        else
            TRSTDY = (_stdt - _rfdt);

    end;

run;

/* 4. Derive TRSEQ */

proc sort data=sdtm_tr1;
    by USUBJID TRDTC;
run;

data sdtm_tr;
    length STUDYID $10 DOMAIN $2 USUBJID $25 TRSEQ 8
           TRLNKID $5
           TRTESTCD $8
           TRTEST $40
           TRORRES $10
           TRORRESU $10
           TRSTRESC $10
           TRSTRESN 8
           TRSTRESU $10
           TREVAL $15
           VISIT $20
           TRDTC $19;

    set sdtm_tr1;

    by USUBJID;

    retain TRSEQ;

    if first.USUBJID then
        TRSEQ = 1;

    else
        TRSEQ + 1;

    keep STUDYID DOMAIN USUBJID TRSEQ
         TRLNKID
         TRTESTCD TRTEST
         TRORRES TRORRESU
         TRSTRESC TRSTRESN TRSTRESU
         TREVAL
         TRDTC TRSTDY
         VISIT;

run;

/* 5. View Final Dataset */

proc print data=sdtm_tr;
run;

/* 6. Export Final Dataset */

proc export
    data=sdtm_tr
    outfile="&outds./tr.xlsx"
    dbms=xlsx
    replace;
run;

--- end of SAS program ---

Write the complete R script now.