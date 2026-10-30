Translate the SAS program below to R.

Program id: P065   file: P065_adlb_qc.sas   source: wxzhai/Clinical-Trial-SAS-Programming-Portfolio   family: wxzhai
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P065
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P065

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset adlb_qc  -> write OUT_DIR/adlb_qc.csv

--- family environment (wxzhai) that ran before the program ---
libname raw  "&root.\data\derived\wxzhai\raw";
libname sdtm "&root.\data\derived\wxzhai\sdtm";
libname adam "&root.\data\derived\wxzhai\adam";


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P065_adlb_qc.sas) ---
/*----------------------------------------------------------
  QC Program: adlb_qc.sas
  Purpose: Independent QC of ADLB
----------------------------------------------------------*/

proc sort data=sdtm.lb out=lb_qc;
    by USUBJID PARAM LBDTC;
run;

proc sort data=adam.adsl out=adsl_qc;
    by USUBJID;
run;

data adam.adlb_qc;
    merge lb_qc (in=a)
          adsl_qc (in=b keep=USUBJID RFSTDTC TRT01P);
    by USUBJID;
    if a and b;

    lbdt = input(LBDTC, yymmdd10.);
    rfstdt = input(RFSTDTC, yymmdd10.);

    retain BASE;

    if first.PARAM then BASE = .;

    if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;

    if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y";
    else ABLFL = "";

    if not missing(BASE) then CHG = AVAL - BASE;

    drop lbdt rfstdt;
run;

--- end of SAS program ---

Write the complete R script now.