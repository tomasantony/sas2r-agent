Translate the SAS program below to R.

Program id: P001   file: P001_adsl.sas   source: kbd0011/cdisc-pilot-replication   family: pilotrep
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P001
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P001
  &sdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &qsraw. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &adam. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset adsl  -> write OUT_DIR/adsl.csv

--- family environment (pilotrep) that ran before the program ---
%global sdtm adam qsraw;
%let sdtm  = &root./data/raw/cdisc-pilot/sdtm;    /* P001 reads xpt via libname xport */
%let qsraw = &root./data/raw/cdisc-pilot/sdtm;
%let adam  = &root.\data\derived\cdisc-pilot\adam; /* R001 libname adam "&adam." */


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P001_adsl.sas) ---
/*****************************************************************************
 * adsl.sas - Parallel ADSL build in SAS 9.4 to prove dual-implementation.
 *
 * Source SDTM (XPT v5 in &sdtm. and &qsraw.):
 *   dm.xpt, ex.xpt, ds.xpt, ae.xpt   (from R/export_sdtm_for_sas.R)
 *   qs.xpt                            (from data/raw/, original CDISC Pilot)
 *
 * Output:
 *   &adam.adsl.sas7bdat
 *   &out.adsl.xpt        (XPT v5 for PROC COMPARE against R)
 *
 * Mirrors the derivation logic in R/01_adsl.R + R/05_adqsadas.R (EFFFL part).
 * Section numbers below match R/01_adsl.R headers.
 *
 * SAS ODA workflow:
 *   1. Upload data/xpt/sdtm/ + data/raw/qs.xpt to your ODA home.
 *   2. Update &sdtm., &qsraw., &adam., &out. macro paths below.
 *   3. Run this program.
 *****************************************************************************/

/* ---------------------------------------------------------- libraries -- */
%let sdtm  = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm;     /* dm.xpt, ex.xpt, ds.xpt, ae.xpt    */
%let qsraw = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm;      /* qs.xpt                            */
%let adam  = &outds.;     /* sas7bdat output                   */
%let out   = &outds.;      /* xpt output                        */

libname sdtmin xport "&sdtm./dm.xpt" access=readonly;
libname exin   xport "&sdtm./ex.xpt" access=readonly;
libname dsin   xport "&sdtm./ds.xpt" access=readonly;
libname aein   xport "&sdtm./ae.xpt" access=readonly;
libname qsin   xport "&qsraw./qs.xpt" access=readonly;
libname adam   "&adam.";

/* Copy XPT contents into WORK so we can manipulate freely. */
proc copy in=sdtmin out=work; select dm; run;
proc copy in=exin   out=work; select ex; run;
proc copy in=dsin   out=work; select ds; run;
proc copy in=aein   out=work; select ae; run;
proc copy in=qsin   out=work; select qs; run;


/* -------------------------------------- 2. Base subject record from DM */
data adsl_base;
  set dm;
  keep STUDYID USUBJID SUBJID SITEID AGE AGEU SEX RACE ETHNIC COUNTRY
       ARM ARMCD ACTARM ACTARMCD RFSTDTC RFENDTC RFXSTDTC RFXENDTC
       DTHDTC DTHFL;
run;


/* ------------------------------------------ 3. Treatment dates ------- */
data adsl_trt;
  set adsl_base;
  length _dt 8;
  format TRTSDT TRTEDT DTHDT date9.
         TRTSDTM TRTEDTM e8601dt19.;

  if not missing(RFXSTDTC) then do;
    TRTSDT  = input(substr(RFXSTDTC, 1, 10), yymmdd10.);
    TRTSDTM = input(RFXSTDTC, ?? e8601dt19.);
  end;
  if not missing(RFXENDTC) then do;
    TRTEDT  = input(substr(RFXENDTC, 1, 10), yymmdd10.);
    TRTEDTM = input(RFXENDTC, ?? e8601dt19.);
  end;
  if not missing(DTHDTC) then DTHDT = input(substr(DTHDTC, 1, 10), yymmdd10.);

  if not missing(TRTSDT) and not missing(TRTEDT) then
    TRTDURD = TRTEDT - TRTSDT + 1;
  drop _dt;
run;


/* ------------------------------ 4. Population flags ------------------ */
/* SAFFL = Y if subject has >=1 EX record with non-missing EXTRT
 * (Placebo has EXDOSE=0 in pharmaversesdtm, so use EXTRT not EXDOSE). */
proc sql;
  create table _saf as
    select distinct STUDYID, USUBJID, "Y" as SAFFL length=1
    from ex
    where not missing(EXTRT);
quit;

data adsl_pop;
  merge adsl_trt(in=a) _saf(in=b);
  by STUDYID USUBJID;
  if a;
  length ITTFL $1;
  if missing(SAFFL) then SAFFL = "N";
  if ARM in ("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
    then ITTFL = "Y";
    else ITTFL = "N";
run;


/* ------------------------------ 5. Disposition from DS --------------- */
proc sort data=ds; by USUBJID DSDTC; run;

data _disp;
  set ds;
  by USUBJID DSDTC;
  where DSCAT = "DISPOSITION EVENT" and DSDECOD ne "SCREEN FAILURE";
  if last.USUBJID;            /* latest disposition record per subject */
  length EOSSTT $12 DCSREAS $100 DCSREASP $200;
  if DSDECOD = "COMPLETED" then do;
    EOSSTT = "COMPLETED";
    DCSREAS = "";
    DCSREASP = "";
  end;
  else do;
    EOSSTT = "DISCONTINUED";
    DCSREAS = DSDECOD;
    if DSDECOD = "OTHER" then DCSREASP = DSTERM; else DCSREASP = "";
  end;
  keep USUBJID EOSSTT DCSREAS DCSREASP;
run;

data adsl_disp;
  merge adsl_pop(in=a) _disp;
  by USUBJID;
  if a;
  if missing(EOSSTT) then EOSSTT = "DISCONTINUED";
  EOTSTT = EOSSTT;
run;


/* ------------------------------ 6. Age groups + numeric codes -------- */
data adsl_age;
  set adsl_disp;
  length AGEGR1 $6;
  if AGE < 65 then do;
    AGEGR1 = "<65";  AGEGR1N = 1;
  end;
  else if AGE >= 65 and AGE <= 80 then do;
    AGEGR1 = "65-80"; AGEGR1N = 2;
  end;
  else if AGE > 80 then do;
    AGEGR1 = ">80";   AGEGR1N = 3;
  end;

  if SEX = "M" then SEXN = 1;
  else if SEX = "F" then SEXN = 2;

  if RACE = "WHITE"                                    then RACEN = 1;
  else if RACE = "BLACK OR AFRICAN AMERICAN"           then RACEN = 2;
  else if RACE = "ASIAN"                               then RACEN = 3;
  else if RACE = "AMERICAN INDIAN OR ALASKA NATIVE"    then RACEN = 4;
  else if RACE = "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"
                                                       then RACEN = 5;
  else if RACE = "MULTIPLE"                            then RACEN = 6;
run;


/* ------------------------------ 7. TRT01P / TRT01A ------------------- */
data adsl_trt01;
  set adsl_age;
  length TRT01P TRT01A $40;
  TRT01P = ARM;
  TRT01A = ACTARM;
run;


/* ------------------------------ 8. Efficacy flag from QS ------------- */
/* EFFFL = Y if SAFFL=Y AND >=1 post-baseline ACTOT score in QS. */
proc sql;
  create table _eff as
    select distinct USUBJID, "Y" as EFFFL length=1
    from qs
    where QSTESTCD = "ACTOT"
      and QSCAT    = "ALZHEIMER'S DISEASE ASSESSMENT SCALE"
      and not missing(QSSTRESN)
      and upcase(VISIT) ne "BASELINE"
      and not index(upcase(VISIT), "SCREENING");
quit;

data adsl_eff;
  merge adsl_trt01(in=a) _eff;
  by USUBJID;
  if a;
  if SAFFL = "Y" and EFFFL = "Y" then EFFFL = "Y";
                                  else EFFFL = "N";
run;


/* ------------------------------ 9. Labels, formats, lengths ---------- */
data adam.adsl;
  retain
    STUDYID USUBJID SUBJID SITEID AGE AGEU AGEGR1 AGEGR1N SEX SEXN
    RACE RACEN ETHNIC COUNTRY ARM ARMCD ACTARM ACTARMCD TRT01P TRT01A
    RFSTDTC RFENDTC RFXSTDTC RFXENDTC TRTSDT TRTEDT TRTSDTM TRTEDTM
    TRTDURD DTHDTC DTHDT DTHFL DCSREAS DCSREASP EOSSTT EOTSTT
    SAFFL ITTFL EFFFL;
  set adsl_eff;

  /* Labels (must match metadata/adsl_spec.xlsx) */
  label
    STUDYID  = "Study Identifier"
    USUBJID  = "Unique Subject Identifier"
    SUBJID   = "Subject Identifier for the Study"
    SITEID   = "Study Site Identifier"
    AGE      = "Age"
    AGEU     = "Age Units"
    AGEGR1   = "Pooled Age Group 1"
    AGEGR1N  = "Pooled Age Group 1 (N)"
    SEX      = "Sex"
    SEXN     = "Sex (N)"
    RACE     = "Race"
    RACEN    = "Race (N)"
    ETHNIC   = "Ethnicity"
    COUNTRY  = "Country"
    ARM      = "Description of Planned Arm"
    ARMCD    = "Planned Arm Code"
    ACTARM   = "Description of Actual Arm"
    ACTARMCD = "Actual Arm Code"
    TRT01P   = "Planned Treatment for Period 01"
    TRT01A   = "Actual Treatment for Period 01"
    RFSTDTC  = "Subject Reference Start Date/Time"
    RFENDTC  = "Subject Reference End Date/Time"
    RFXSTDTC = "Date/Time of First Study Treatment"
    RFXENDTC = "Date/Time of Last Study Treatment"
    TRTSDT   = "Date of First Exposure to Treatment"
    TRTEDT   = "Date of Last Exposure to Treatment"
    TRTSDTM  = "Datetime of First Exposure"
    TRTEDTM  = "Datetime of Last Exposure"
    TRTDURD  = "Total Treatment Duration (Days)"
    DTHDTC   = "Date/Time of Death"
    DTHDT    = "Date of Death"
    DTHFL    = "Subject Death Flag"
    DCSREAS  = "Reason for Discontinuation of Study"
    DCSREASP = "Reason Specify for Discontinuation"
    EOSSTT   = "End of Study Status"
    EOTSTT   = "End of Treatment Status"
    SAFFL    = "Safety Population Flag"
    ITTFL    = "Intent-To-Treat Population Flag"
    EFFFL    = "Efficacy Population Flag";

  format TRTSDT TRTEDT DTHDT date9.
         TRTSDTM TRTEDTM e8601dt19.;
run;

proc sort data=adam.adsl; by USUBJID; run;


/* ------------------------------ 10. Write XPT v5 --------------------- */
libname adslout xport "&out./sas_adsl.xpt";
proc copy in=adam out=adslout memtype=data;
  select adsl;
run;
libname adslout clear;

/* Quick log: row + variable counts */
proc sql noprint;
  select count(*) into :n_rows from adam.adsl;
quit;
proc contents data=adam.adsl noprint out=_c; run;
proc sql noprint;
  select count(*) into :n_vars from _c;
quit;
%put NOTE: SAS ADSL built: &n_rows. subjects, &n_vars. variables;

--- end of SAS program ---

Write the complete R script now.