Translate the SAS program below to R.

Program id: P029   file: P029_tu.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P029
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P029

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
  P029.lst (table/listing text, 44 numbers)  -> write a text rendering and CSV of the same table
  dataset tu  -> write OUT_DIR/tu.csv

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P029_tu.sas) ---
/****************************************************************/
/* STEP 8.1: SDTM TU MAPPING & DERIVATION                       */
/****************************************************************/

/* 1. Prepare DM Reference Data */

proc sort data=sdtm_dm
          out=dm_for_tu(keep=USUBJID RFSTDTC);
    by USUBJID;
run;

/* 2. Prepare Raw Data */

data tu_prep;

    set edc_tu;

    length USUBJID $25;

    USUBJID = catx("-", "ONCO-2026", SUBJID);

    TULOC    = upcase(tranwrd(LOC, "_", " "));
    TUMETHOD = upcase(tranwrd(METH, "_", " "));

run;

proc sort data=tu_prep;
    by USUBJID;
run;

proc sort data=dm_for_tu;
    by USUBJID;
run;

/* 3. Map TU Domain */

data sdtm_tu1;

    length STUDYID $10 DOMAIN $2 USUBJID $25
           TULNKID $5
           TUTESTCD $8
           TUTEST $40
           TUORRES $15
           TUSTRESC $15
           TULOC $20
           TUMETHOD $20
           TUEVAL $15
           VISIT $20
           TUDTC $19;

    merge tu_prep(in=t)
          dm_for_tu;

    by USUBJID;

    if t;

    STUDYID = "ONCO-2026";
    DOMAIN  = "TU";

    TULNKID = LNKID;

    TUTESTCD = "TUMIDENT";
    TUTEST   = "Tumor Identification";

    TUORRES  = upcase(TYPE);
    TUSTRESC = TUORRES;

    TUEVAL = "INVESTIGATOR";

    VISIT = upcase(tranwrd(VISIT, "_", " "));

    if TUDT ne "" then
        TUDTC = put(input(TUDT, date11.), yymmdd10.);

    /* Study Day */

    if TUDTC ne "" and RFSTDTC ne "" then do;

        _stdt = input(TUDTC, yymmdd10.);
        _rfdt = input(RFSTDTC, yymmdd10.);

        if _stdt >= _rfdt then
            TUSTDY = (_stdt - _rfdt) + 1;

        else
            TUSTDY = (_stdt - _rfdt);

    end;

run;

/* 4. Derive TUSEQ */

proc sort data=sdtm_tu1;
    by USUBJID TUDTC;
run;

data sdtm_tu;
    length STUDYID $10 DOMAIN $2 USUBJID $25 TUSEQ 8
           TULNKID $5
           TUTESTCD $8
           TUTEST $40
           TUORRES $15
           TUSTRESC $15
           TULOC $20
           TUMETHOD $20
           TUEVAL $15
           VISIT $20
           TUDTC $19;

    set sdtm_tu1;

    by USUBJID;

    retain TUSEQ;

    if first.USUBJID then
        TUSEQ = 1;

    else
        TUSEQ + 1;

    keep STUDYID DOMAIN USUBJID TUSEQ
         TULNKID
         TUTESTCD TUTEST
         TUORRES TUSTRESC
         TULOC
         TUMETHOD
         TUEVAL
         TUDTC TUSTDY
         VISIT;

run;

/* 5. View Final Dataset */

proc print data=sdtm_tu;
run;

/* 6. Export Final Dataset */

proc export
    data=sdtm_tu
    outfile="&outds./tu.xlsx"
    dbms=xlsx
    replace;
run;

--- end of SAS program ---

Write the complete R script now.