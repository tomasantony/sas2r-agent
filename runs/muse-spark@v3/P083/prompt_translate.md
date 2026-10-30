Translate the SAS program below to R.

Program id: P083   file: P083_adsl.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P083
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P083

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P083.lst (table/listing text, 291 numbers)  -> write a text rendering and CSV of the same table
  dataset adsl  -> write OUT_DIR/adsl.csv

--- family environment (own-adam) that ran before the program ---
/* owner's ADaM programs: inline data, macros under programs/sas/macros/own-adam (include path rewritten in the run copies) */


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-adam/m_adam_utilities.sas ---
/*****************************************************************************
* Program:    m_adam_utilities.sas
* Purpose:    Core utility macros for ADaM dataset generation
* Standard:   CDISC ADaM IG 1.3
*****************************************************************************/

/*===========================================================================
  %derive_ady - Derive Analysis Day (--DY) from date and reference date
===========================================================================*/
%macro derive_ady(ds=, date_var=, ref_var=TRTSDT, out_var=ADY);
  data &ds.;
    set &ds.;
    if not missing(&date_var.) and not missing(&ref_var.) then do;
      if &date_var. >= &ref_var. then
        &out_var. = &date_var. - &ref_var. + 1;
      else
        &out_var. = &date_var. - &ref_var.;
    end;
  run;
%mend derive_ady;

/*===========================================================================
  %derive_epoch - Derive EPOCH based on date relative to treatment period
===========================================================================*/
%macro derive_epoch(ds=, date_var=, trtsdt=TRTSDT, trtedt=TRTEDT);
  data &ds.;
    set &ds.;
    length EPOCH $20;
    if not missing(&date_var.) then do;
      if not missing(&trtsdt.) and &date_var. < &trtsdt. then
        EPOCH = 'SCREENING';
      else if not missing(&trtedt.) and &date_var. > &trtedt. then
        EPOCH = 'FOLLOW-UP';
      else if not missing(&trtsdt.) then
        EPOCH = 'TREATMENT';
      else
        EPOCH = '';
    end;
  run;
%mend derive_epoch;

/*===========================================================================
  %derive_basetype - Derive BASETYPE for multiple baseline definitions
===========================================================================*/
%macro derive_basetype(ds=, basetype_val=);
  data &ds.;
    set &ds.;
    length BASETYPE $40;
    BASETYPE = "&basetype_val.";
  run;
%mend derive_basetype;

/*===========================================================================
  %derive_ablfl - Derive Analysis Baseline Flag
===========================================================================*/
%macro derive_ablfl(ds=, by_vars=USUBJID PARAMCD, date_var=ADT, trtsdt=TRTSDT,
                    method=LAST);
  /* method: LAST = last non-missing before/on TRTSDT
             FIRST = first non-missing
             WORST = worst value before/on TRTSDT */

  proc sort data=&ds.; by &by_vars. &date_var.; run;

  %if %upcase(&method.) = LAST %then %do;
    data &ds.;
      set &ds.;
      by &by_vars.;
      length ABLFL $1;

      /* Select last record on or before treatment start */
      if not missing(&date_var.) and not missing(&trtsdt.) and
         &date_var. <= &trtsdt. then _eligible = 1;
      else _eligible = 0;

      /* Look ahead to find last eligible */
      _next_elig = lag(_eligible);
      if first.PARAMCD then _next_elig = .;
    run;

    /* Identify last eligible record */
    data &ds.;
      set &ds.;
      by &by_vars.;
      retain _last_elig_seq;
      if first.PARAMCD then _last_elig_seq = .;
      if _eligible = 1 then _last_elig_seq = _n_;
    run;

    data &ds.;
      set &ds. end=eof;
      by &by_vars.;
      retain _max_seq;
      if first.PARAMCD then _max_seq = .;
      if _eligible = 1 then _max_seq = _n_;
      if last.PARAMCD and _max_seq = _n_ and _eligible = 1 then ABLFL = 'Y';
      drop _eligible _next_elig _last_elig_seq _max_seq;
    run;

    /* Simplified approach */
    proc sort data=&ds.; by &by_vars. descending &date_var.; run;
    data &ds.;
      set &ds.;
      by &by_vars.;
      if first.PARAMCD and not missing(&date_var.) and
         not missing(&trtsdt.) and &date_var. <= &trtsdt. then ABLFL = 'Y';
      else if missing(ABLFL) then ABLFL = '';
    run;
    proc sort data=&ds.; by &by_vars. &date_var.; run;
  %end;
%mend derive_ablfl;

/*===========================================================================
  %derive_base_chg - Derive BASE, CHG, PCHG from AVAL and baseline
===========================================================================*/
%macro derive_base_chg(ds=, by_vars=USUBJID PARAMCD);
  proc sort data=&ds.; by &by_vars.; run;

  /* Get baseline values */
  data _base_;
    set &ds.(where=(ABLFL='Y'));
    BASE = AVAL;
    keep &by_vars. BASE;
  run;

  /* Merge baseline back */
  data &ds.;
    merge &ds.(in=a) _base_;
    by &by_vars.;
    if a;

    /* Derive change from baseline */
    if not missing(AVAL) and not missing(BASE) then do;
      CHG = AVAL - BASE;
      if BASE ne 0 then PCHG = (CHG / BASE) * 100;
    end;
  run;

  proc delete data=_base_; run;
%mend derive_base_chg;

/*===========================================================================
  %derive_avisit - Derive AVISIT and AVISITN from visit windows
===========================================================================*/
%macro derive_avisit(ds=, ady_var=ADY, windows=);
  /* windows format: VISITNAME|LOWER|UPPER (pipe-delimited, semicolon between windows)
     Example: Baseline|-999|1;Week 1|2|10;Week 2|11|17 */

  data &ds.;
    set &ds.;
    length AVISIT $40 AVISITN 8 AWTARGET AWLO AWHI 8;

    %let i = 1;
    %let window = %scan(&windows., &i., ;);
    %do %while(%length(&window.) > 0);
      %let vname = %scan(&window., 1, |);
      %let vlo = %scan(&window., 2, |);
      %let vhi = %scan(&window., 3, |);

      if &vlo. <= &ady_var. <= &vhi. then do;
        AVISIT = "&vname.";
        AVISITN = &i.;
        AWLO = &vlo.;
        AWHI = &vhi.;
        AWTARGET = (&vlo. + &vhi.) / 2;
      end;

      %let i = %eval(&i. + 1);
      %let window = %scan(&windows., &i., ;);
    %end;
  run;
%mend derive_avisit;

/*===========================================================================
  %derive_anrind - Derive Analysis Normal Range Indicator
===========================================================================*/
%macro derive_anrind(ds=, aval_var=AVAL, lo_var=ANRLO, hi_var=ANRHI);
  data &ds.;
    set &ds.;
    length ANRIND $20;

    if not missing(&aval_var.) then do;
      if not missing(&lo_var.) and &aval_var. < &lo_var. then ANRIND = 'LOW';
      else if not missing(&hi_var.) and &aval_var. > &hi_var. then ANRIND = 'HIGH';
      else if not missing(&lo_var.) or not missing(&hi_var.) then ANRIND = 'NORMAL';
    end;
  run;
%mend derive_anrind;

/*===========================================================================
  %derive_dtype - Derive DTYPE for derived records
===========================================================================*/
%macro derive_dtype(ds=, dtype_val=);
  data &ds.;
    set &ds.;
    length DTYPE $20;
    DTYPE = "&dtype_val.";
  run;
%mend derive_dtype;

/*===========================================================================
  %adam_seq - Assign --SEQ variable for ADaM datasets
===========================================================================*/
%macro adam_seq(ds=, prefix=, by_vars=USUBJID);
  proc sort data=&ds.; by &by_vars.; run;

  data &ds.;
    set &ds.;
    by &by_vars.;
    if first.%scan(&by_vars., -1) then &prefix.SEQ = 0;
    &prefix.SEQ + 1;
  run;
%mend adam_seq;

/*===========================================================================
  %derive_trtemfl - Derive Treatment-Emergent Flag (for AE/safety)
===========================================================================*/
%macro derive_trtemfl(ds=, start_var=ASTDT, trtsdt=TRTSDT, trtedt=TRTEDT,
                      grace_period=30);
  data &ds.;
    set &ds.;
    length TRTEMFL $1;

    if not missing(&start_var.) and not missing(&trtsdt.) then do;
      /* Started on or after first dose */
      if &start_var. >= &trtsdt. then do;
        /* And before end of treatment + grace period (if end date exists) */
        if missing(&trtedt.) then TRTEMFL = 'Y';
        else if &start_var. <= &trtedt. + &grace_period. then TRTEMFL = 'Y';
      end;
    end;

    if missing(TRTEMFL) then TRTEMFL = '';
  run;
%mend derive_trtemfl;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P083_adsl.sas) ---
/*****************************************************************************
* Program:    adsl.sas
* Purpose:    Create ADSL (Subject-Level Analysis Dataset)
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM DM, DS, EX, SV (mock data inline)
* Output:     ADSL
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock SDTM source data
==========================================================================*/

/*--- DM (Demographics) ---*/
data dm;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 SUBJID $8 SITEID $4 SEX $1 RACE $50
         ETHNIC $30 ARMCD $8 ARM $40 COUNTRY $3 BRTHDTC $10 RFICDTC $10
         RFSTDTC $10 RFENDTC $10 RFXSTDTC $10 RFXENDTC $10 AGE 8;
  input STUDYID $ USUBJID $ SUBJID $ SITEID $ SEX $ RACE $ ETHNIC $
        ARMCD $ ARM $ COUNTRY $ BRTHDTC $ RFICDTC $ RFSTDTC $
        RFENDTC $ RFXSTDTC $ RFXENDTC $ AGE;
  format _all_;
datalines;
STUDY01|STUDY01-1001|1001|101|M|WHITE|NOT HISPANIC|TRT|Treatment A|USA|1965-03-15|2024-01-10|2024-01-15|2024-06-15|2024-01-15|2024-06-15|59
STUDY01|STUDY01-1002|1002|101|F|BLACK OR AFRICAN AMERICAN|NOT HISPANIC|PBO|Placebo|USA|1978-08-22|2024-01-12|2024-01-18|2024-06-18|2024-01-18|2024-06-18|45
STUDY01|STUDY01-1003|1003|102|M|ASIAN|HISPANIC OR LATINO|TRT|Treatment A|USA|1980-11-01|2024-01-15|2024-01-20|2024-04-01|2024-01-20|2024-04-01|43
STUDY01|STUDY01-1004|1004|102|F|WHITE|NOT HISPANIC|TRT|Treatment A|USA|1972-06-30|2024-01-18|2024-01-25|2024-06-25|2024-01-25|2024-06-25|51
STUDY01|STUDY01-1005|1005|103|M|ASIAN|NOT HISPANIC|PBO|Placebo|USA|1988-02-14|2024-01-20|2024-01-28|.|2024-01-28|.|36
STUDY01|STUDY01-1006|1006|103|F|WHITE|HISPANIC OR LATINO|TRT|Treatment A|USA|1955-12-05|2024-01-22|2024-01-30|2024-07-01|2024-01-30|2024-07-01|68
;
run;

/*--- DS (Disposition) ---*/
data ds;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 DSTERM $50 DSDECOD $50 DSCAT $30 DSSTDTC $10;
  input STUDYID $ USUBJID $ DSTERM $ DSDECOD $ DSCAT $ DSSTDTC $;
datalines;
STUDY01|STUDY01-1001|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-15
STUDY01|STUDY01-1002|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-18
STUDY01|STUDY01-1003|ADVERSE EVENT|ADVERSE EVENT|DISPOSITION EVENT|2024-04-01
STUDY01|STUDY01-1004|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-06-25
STUDY01|STUDY01-1005|ONGOING|ONGOING|DISPOSITION EVENT|.
STUDY01|STUDY01-1006|COMPLETED|COMPLETED|DISPOSITION EVENT|2024-07-01
;
run;

/*--- EX (Exposure) for treatment dates ---*/
data ex;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 EXTRT $40 EXSTDTC $10 EXENDTC $10;
  input STUDYID $ USUBJID $ EXTRT $ EXSTDTC $ EXENDTC $;
datalines;
STUDY01|STUDY01-1001|Treatment A|2024-01-15|2024-06-15
STUDY01|STUDY01-1002|Placebo|2024-01-18|2024-06-18
STUDY01|STUDY01-1003|Treatment A|2024-01-20|2024-04-01
STUDY01|STUDY01-1004|Treatment A|2024-01-25|2024-06-25
STUDY01|STUDY01-1005|Placebo|2024-01-28|.
STUDY01|STUDY01-1006|Treatment A|2024-01-30|2024-07-01
;
run;

/*==========================================================================
  STEP 2: Derive ADSL variables
==========================================================================*/

/*--- Start with DM as base ---*/
data adsl;
  set dm;

  length TRTSDT TRTEDT RANDDT 8;
  length TRTP $40 TRTPN 8 TRTA $40 TRTAN 8;
  length AGEGR1 $20 AGEGR1N 8 RACEN 8 SEXN 8;
  length SAFFL ITTFL EFFFL COMPLFL $1;
  length DCSREAS $50 EOSSTT $20;
  length TRTDURD 8;

  /*--- Treatment variables ---*/
  TRTP = ARM;
  if ARMCD = 'TRT' then TRTPN = 1;
  else if ARMCD = 'PBO' then TRTPN = 2;

  TRTA = TRTP;
  TRTAN = TRTPN;

  /*--- Convert dates ---*/
  if RFXSTDTC ne '' then TRTSDT = input(RFXSTDTC, yymmdd10.);
  if RFXENDTC ne '' and RFXENDTC ne '.' then TRTEDT = input(RFXENDTC, yymmdd10.);
  if RFSTDTC ne '' then RANDDT = input(RFSTDTC, yymmdd10.);

  /*--- Treatment duration ---*/
  if not missing(TRTSDT) and not missing(TRTEDT) then
    TRTDURD = TRTEDT - TRTSDT + 1;

  /*--- Age group ---*/
  if AGE < 65 then do; AGEGR1 = '<65'; AGEGR1N = 1; end;
  else do; AGEGR1 = '>=65'; AGEGR1N = 2; end;

  /*--- Sex numeric ---*/
  if SEX = 'M' then SEXN = 1;
  else if SEX = 'F' then SEXN = 2;

  /*--- Race numeric ---*/
  select(RACE);
    when('WHITE') RACEN = 1;
    when('BLACK OR AFRICAN AMERICAN') RACEN = 2;
    when('ASIAN') RACEN = 3;
    otherwise RACEN = 4;
  end;

  /*--- Population flags ---*/
  SAFFL = 'Y'; /* All with at least one dose */
  ITTFL = 'Y'; /* All randomized */
  EFFFL = 'Y'; /* Efficacy evaluable */

  format TRTSDT TRTEDT RANDDT date9.;
run;

/*--- Merge disposition ---*/
proc sort data=adsl; by STUDYID USUBJID; run;
proc sort data=ds; by STUDYID USUBJID; run;

data adsl;
  merge adsl(in=a) ds(in=b keep=STUDYID USUBJID DSTERM DSDECOD);
  by STUDYID USUBJID;
  if a;

  /*--- End of Study Status ---*/
  if DSDECOD = 'COMPLETED' then do;
    EOSSTT = 'COMPLETED';
    COMPLFL = 'Y';
    DCSREAS = '';
  end;
  else if DSDECOD = 'ONGOING' then do;
    EOSSTT = 'ONGOING';
    COMPLFL = 'N';
    DCSREAS = '';
  end;
  else do;
    EOSSTT = 'DISCONTINUED';
    COMPLFL = 'N';
    DCSREAS = DSDECOD;
  end;

  drop DSTERM DSDECOD;
run;

/*==========================================================================
  STEP 3: Add labels and finalize
==========================================================================*/
data adsl;
  set adsl;

  label
    STUDYID  = 'Study Identifier'
    USUBJID  = 'Unique Subject Identifier'
    SUBJID   = 'Subject Identifier for the Study'
    SITEID   = 'Study Site Identifier'
    SEX      = 'Sex'
    SEXN     = 'Sex (N)'
    RACE     = 'Race'
    RACEN    = 'Race (N)'
    ETHNIC   = 'Ethnicity'
    AGE      = 'Age'
    AGEGR1   = 'Age Group 1'
    AGEGR1N  = 'Age Group 1 (N)'
    ARM      = 'Description of Planned Arm'
    ARMCD    = 'Planned Arm Code'
    TRTP     = 'Planned Treatment'
    TRTPN    = 'Planned Treatment (N)'
    TRTA     = 'Actual Treatment'
    TRTAN    = 'Actual Treatment (N)'
    TRTSDT   = 'Date of First Exposure to Treatment'
    TRTEDT   = 'Date of Last Exposure to Treatment'
    TRTDURD  = 'Total Treatment Duration (Days)'
    RANDDT   = 'Date of Randomization'
    SAFFL    = 'Safety Population Flag'
    ITTFL    = 'Intent-to-Treat Population Flag'
    EFFFL    = 'Efficacy Population Flag'
    COMPLFL  = 'Completers Population Flag'
    EOSSTT   = 'End of Study Status'
    DCSREAS  = 'Reason for Discontinuation'
    COUNTRY  = 'Country'
    ;
run;

proc sort data=adsl; by STUDYID USUBJID; run;

/*--- Output ---*/
proc contents data=adsl varnum; run;
proc print data=adsl(obs=10) noobs; run;

--- end of SAS program ---

Write the complete R script now.