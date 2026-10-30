Translate the SAS program below to R.

Program id: P026   file: P026_lb.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P026
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026

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
  P026.lst (table/listing text, 92 numbers)  -> write a text rendering and CSV of the same table
  dataset lb  -> write OUT_DIR/lb.csv

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P026_lb.sas) ---
/****************************************************************/
/* STEP 7.1: SDTM LB MAPPING & DERIVATION                       */
/****************************************************************/

/* 1. Prepare DM Reference Data */

proc sort data=sdtm_dm
          out=dm_for_lb(keep=USUBJID RFSTDTC);
    by USUBJID;
run;

/* 2. Prepare Raw Data with USUBJID */

data lb_prep;
    set edc_lb;

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);
run;

proc sort data=lb_prep;
    by USUBJID;
run;

proc sort data=dm_for_lb;
    by USUBJID;
run;

/* 3. Map LB Domain */

data sdtm_lb1;

    length STUDYID $10 DOMAIN $2 USUBJID $25
           LBTESTCD $8 LBTEST $40 LBCAT $15
           LBORRES $10 LBORRESU $20
           LBSTRESC $10 LBSTRESN 8 LBSTRESU $20
           LBORNRLO $10 LBORNRHI $10
           LBDTC $19 VISIT $20
           LBNRIND $8;

    merge lb_prep(in=l)
          dm_for_lb;

    by USUBJID;

    if l;

    STUDYID = "ONCO-2026";
    DOMAIN  = "LB";

    LBTESTCD = TESTCD;
    LBCAT    = CAT;

    /* Map Test Names */

    if LBTESTCD = "HGB" then
        LBTEST = "Hemoglobin";

    else if LBTESTCD = "ALT" then
        LBTEST = "Alanine Aminotransferase";

    LBORRES  = ORRES;
    LBORRESU = UNIT;

    LBSTRESC = ORRES;
    LBSTRESN = input(ORRES, best.);
    LBSTRESU = UNIT;

    LBORNRLO = LOW;
    LBORNRHI = HIGH;

    VISIT = tranwrd(VISIT, "_", " ");

    /* Reference Range Indicator */

    if not missing(LBSTRESN) then do;

        if LBSTRESN < input(LOW, best.) then
            LBNRIND = "LOW";

        else if LBSTRESN > input(HIGH, best.) then
            LBNRIND = "HIGH";

        else
            LBNRIND = "NORMAL";

    end;

    /* Date Conversion */

    if LBDT ne "" then
        LBDTC = put(input(LBDT, date11.), yymmdd10.);

    /* Study Day */

    if LBDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(LBDTC, yymmdd10.);
        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            LBSTDY = (_stdt - _rfdt) + 1;

        else
            LBSTDY = (_stdt - _rfdt);

    end;

run;

/* 4. Derive LBSEQ */

proc sort data=sdtm_lb1;
    by USUBJID LBDTC;
run;

data sdtm_lb;
    length STUDYID $10 DOMAIN $2 USUBJID $25
           LBSEQ 8 LBTESTCD $8 LBTEST $40
           LBCAT $15 LBORRES $10 LBORRESU $20
           LBSTRESC $10 LBSTRESN 8 LBSTRESU $20
           LBORNRLO $10 LBORNRHI $10
           LBDTC $19 VISIT $20
           LBNRIND $8;

    set sdtm_lb1;

    by USUBJID;

    retain LBSEQ;

    if first.USUBJID then
        LBSEQ = 1;

    else
        LBSEQ + 1;

    keep STUDYID DOMAIN USUBJID LBSEQ
         LBTESTCD LBTEST LBCAT
         LBORRES LBORRESU
         LBSTRESC LBSTRESN LBSTRESU
         LBORNRLO LBORNRHI
         LBNRIND
         LBDTC LBSTDY
         VISIT;

run;

/* 5. View Final Dataset */

proc print data=sdtm_lb;
run;

/* 6. Export Final Dataset */

proc export
    data=sdtm_lb
    outfile="&outds./lb.xlsx"
    dbms=xlsx
    replace;
run;

--- end of SAS program ---

Write the complete R script now.