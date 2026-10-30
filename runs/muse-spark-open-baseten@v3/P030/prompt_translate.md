Translate the SAS program below to R.

Program id: P030   file: P030_vs.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P030
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P030

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
  P030.lst (table/listing text, 114 numbers)  -> write a text rendering and CSV of the same table

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P030_vs.sas) ---
/****************************************************************/
/* STEP 6.1: SDTM VS MAPPING & DERIVATION                       */
/****************************************************************/

proc sort data=sdtm_dm out=dm_for_vs(keep=USUBJID SUBJID RFSTDTC RFXSTDTC);
    by USUBJID;
run;

/* 1. Prepare Raw Data with USUBJID */

data vs_prep;
    set edc_vs;

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);
run;

proc sort data=vs_prep;
    by USUBJID;
run;

proc sort data=dm_for_vs;
    by USUBJID;
run;

/* 2. Map VS Domain */

data sdtm_vs1;

    length STUDYID $10 DOMAIN $2 USUBJID $25
           VSTESTCD $8 VSTEST $40
           VSORRES $10 VSORRESU $20
           VSSTRESC $10 VSSTRESN 8 VSSTRESU $20
           VSDTC $19 VISIT $20 VSPOS $10;

    merge vs_prep(in=v)
          dm_for_vs(keep=USUBJID RFSTDTC RFXSTDTC);

    by USUBJID;

    if v;

    STUDYID = "ONCO-2026";
    DOMAIN  = "VS";

    VSTESTCD = TESTCD;

    if VSTESTCD = "SYSBP" then VSTEST = "Systolic Blood Pressure";
    else if VSTESTCD = "DIABP" then VSTEST = "Diastolic Blood Pressure";

    VSORRES  = ORRES;
    VSORRESU = UNIT;

    VSSTRESC = ORRES;
    VSSTRESN = input(ORRES, best.);
    VSSTRESU = UNIT;

    VISIT = tranwrd(VISIT, "_", " ");

    VSPOS = upcase(POS);

    if VSDT ne "" then
        VSDTC = put(input(VSDT, date11.), yymmdd10.);

    /* Study Day */

    if VSDTC ne "" and RFSTDTC ne "" then do;
        _stdt = input(VSDTC, yymmdd10.);
        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            VSTDY = (_stdt - _rfdt) + 1;
        else
            VSTDY = (_stdt - _rfdt);
    end;

run;

/* 3. Derive VSSEQ */

proc sort data=sdtm_vs1;
    by USUBJID VSDTC;
run;

data sdtm_vs;
    length STUDYID $10 DOMAIN $2 USUBJID $25
           VSSEQ 8 VSTESTCD $8 VSTEST $40
           VSORRES $10 VSORRESU $20
           VSSTRESC $10 VSSTRESN 8 VSSTRESU $20
           VSDTC $19 VISIT $20 VSPOS $10;

    set sdtm_vs1;

    by USUBJID;

    retain VSSEQ;

    if first.USUBJID then VSSEQ = 1;
    else VSSEQ + 1;

    keep STUDYID DOMAIN USUBJID VSSEQ
         VSTESTCD VSTEST
         VSORRES VSORRESU
         VSSTRESC VSSTRESN VSSTRESU
         VSDTC VSTDY
         VISIT VSPOS;

run;

proc print data=sdtm_vs;
run;

--- end of SAS program ---

Write the complete R script now.