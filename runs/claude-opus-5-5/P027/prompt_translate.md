Translate the SAS program below to R.

Program id: P027   file: P027_rs.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P027
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P027

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
  P027.lst (table/listing text, 30 numbers)  -> write a text rendering and CSV of the same table
  dataset rs  -> write OUT_DIR/rs.csv

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P027_rs.sas) ---
/****************************************************************/
/* STEP 10.1: SDTM RS MAPPING & DERIVATION                      */
/****************************************************************/

/* 1. Prepare DM Reference Data */

proc sort data=sdtm_dm
          out=dm_for_rs(keep=USUBJID RFSTDTC);
    by USUBJID;
run;

/* 2. Prepare Raw Data */

data rs_prep;

    set edc_rs;

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);

run;

proc sort data=rs_prep;
    by USUBJID;
run;

proc sort data=dm_for_rs;
    by USUBJID;
run;

/* 3. Map RS Domain */

data sdtm_rs1;

    length STUDYID $10 DOMAIN $2 USUBJID $25
           RSTESTCD $8
           RSTEST $40
           RSCAT $15
           RSORRES $10
           RSSTRESC $10
           RSEVAL $15
           VISIT $20
           RSDTC $19;

    merge rs_prep(in=r)
          dm_for_rs;

    by USUBJID;

    if r;

    STUDYID = "ONCO-2026";
    DOMAIN  = "RS";

    RSTESTCD = TESTCD;

    if RSTESTCD = "OVRLRESP" then
        RSTEST = "Overall Response";

    RSCAT = "RECIST 1.1";

    RSORRES  = upcase(ORRES);
    RSSTRESC = RSORRES;

    RSEVAL = "INVESTIGATOR";

    VISIT = upcase(tranwrd(VISIT, "_", " "));

    /* Date Conversion */

    if RSDT ne "" then
        RSDTC = put(input(RSDT, date11.), yymmdd10.);

    /* Study Day */

    if RSDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(RSDTC, yymmdd10.);
        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            RSSTDY = (_stdt - _rfdt) + 1;

        else
            RSSTDY = (_stdt - _rfdt);

    end;

run;

/* 4. Derive RSSEQ */

proc sort data=sdtm_rs1;
    by USUBJID RSDTC;
run;

data sdtm_rs;
    length STUDYID $10 DOMAIN $2 USUBJID $25 RSSEQ 8
           RSTESTCD $8
           RSTEST $40
           RSCAT $15
           RSORRES $10
           RSSTRESC $10
           RSEVAL $15
           VISIT $20
           RSDTC $19;

    set sdtm_rs1;

    by USUBJID;

    retain RSSEQ;

    if first.USUBJID then
        RSSEQ = 1;

    else
        RSSEQ + 1;

    keep STUDYID DOMAIN USUBJID RSSEQ
         RSTESTCD RSTEST
         RSCAT
         RSORRES RSSTRESC
         RSEVAL
         RSDTC RSSTDY
         VISIT;

run;

/* 5. View Final Dataset */

proc print data=sdtm_rs;
run;

/* 6. Export Final Dataset */

proc export
    data=sdtm_rs
    outfile="&outds./rs.xlsx"
    dbms=xlsx
    replace;
run;

--- end of SAS program ---

Write the complete R script now.