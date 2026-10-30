Translate the SAS program below to R.

Program id: P064   file: P064_adlb.sas   source: wxzhai/Clinical-Trial-SAS-Programming-Portfolio   family: wxzhai
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P064
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P064

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  raw -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw
  sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset adlb  -> write OUT_DIR/adlb.csv

--- family environment (wxzhai) that ran before the program ---
libname raw  "&root.\data\derived\wxzhai\raw";
libname sdtm "&root.\data\derived\wxzhai\sdtm";
libname adam "&root.\data\derived\wxzhai\adam";


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P064_adlb.sas) ---
/*******************************************************************************
* PROGRAM: adlb.sas
* PURPOSE: Create ADaM ADLB dataset based on CDISC
* AUTHOR:  JAMES ZHAI, SAS Certified Clinical Trials Programmer
* DATE: 2025/10/01
*******************************************************************************/

/* Note: For simplicity, this program didn't consider more than 1 lab test before treatment */

proc sort data=sdtm.lb; by USUBJID PARAM LBDTC; run;
proc sort data=adam.adsl; by USUBJID; run;

data adam.adlb;
    merge sdtm.lb (in=a)
          adam.adsl (in=b keep=USUBJID TRT01P RFSTDTC);
    by USUBJID;
    if a and b;

    retain BASE;

    /* Baseline = last non-missing pre-dose value */
    if first.PARAM then BASE = .;
    if input(LBDTC, yymmdd10.) < input(RFSTDTC, yymmdd10.) and not missing(AVAL) then BASE = AVAL;

    /* Baseline flag */
    if input(LBDTC, yymmdd10.) < input(RFSTDTC, yymmdd10.) and AVAL = BASE then ABLFL = "Y";
    else ABLFL = "";

    /* Change from baseline */
    CHG = AVAL - BASE;

    drop LBDTC RFSTDTC;
run;



--- end of SAS program ---

Write the complete R script now.