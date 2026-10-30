Translate the SAS program below to R.

Program id: P086   file: P086_adtte.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P086
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P086

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P086.lst (table/listing text, 279 numbers)  -> write a text rendering and CSV of the same table
  dataset adrs  -> write OUT_DIR/adrs.csv
  dataset adsl  -> write OUT_DIR/adsl.csv
  dataset adtte  -> write OUT_DIR/adtte.csv
  dataset adtte_os  -> write OUT_DIR/adtte_os.csv
  dataset adtte_pfs  -> write OUT_DIR/adtte_pfs.csv
  dataset adtte_ttr  -> write OUT_DIR/adtte_ttr.csv

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


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-adam/m_derive_tte.sas ---
/*****************************************************************************
* Program:    m_derive_tte.sas
* Purpose:    Macros for deriving Time-to-Event (TTE) analysis variables
* Standard:   CDISC ADaM IG 1.3 (ADTTE)
*****************************************************************************/

/*===========================================================================
  %derive_tte - Derive time-to-event variables (AVAL, CNSR, EVNTDESC, etc.)
===========================================================================*/
%macro derive_tte(in_ds=, out_ds=, start_var=TRTSDT, event_var=, event_dt=,
                  censor_dt=, paramcd=, param=, event_desc=);
  data &out_ds.;
    set &in_ds.;
    length PARAMCD $8 PARAM $200 EVNTDESC $200 CNSDTDSC $200;

    PARAMCD = "&paramcd.";
    PARAM = "&param.";

    /* Determine event or censoring */
    if not missing(&event_dt.) and &event_var. = 1 then do;
      /* Event occurred */
      ADT = &event_dt.;
      CNSR = 0;
      EVNTDESC = "&event_desc.";
      CNSDTDSC = '';
    end;
    else if not missing(&censor_dt.) then do;
      /* Censored */
      ADT = &censor_dt.;
      CNSR = 1;
      EVNTDESC = '';
      CNSDTDSC = 'Censored';
    end;

    /* Calculate AVAL (time in days) */
    if not missing(ADT) and not missing(&start_var.) then do;
      AVAL = ADT - &start_var. + 1;
      if AVAL < 1 then AVAL = 1;
    end;

    /* Start date reference */
    STARTDT = &start_var.;
    format ADT STARTDT date9.;
  run;
%mend derive_tte;

/*===========================================================================
  %derive_tte_multi - Derive TTE from multiple potential event sources
===========================================================================*/
%macro derive_tte_multi(in_ds=, out_ds=, start_var=TRTSDT,
                        event_list=, /* event_flag|event_date|description; ... */
                        censor_var=, censor_dt=, censor_desc=,
                        paramcd=, param=);
  data &out_ds.;
    set &in_ds.;
    length PARAMCD $8 PARAM $200 EVNTDESC $200 CNSDTDSC $200;
    length _event_found 8 _earliest_dt 8;

    PARAMCD = "&paramcd.";
    PARAM = "&param.";

    _event_found = 0;
    _earliest_dt = .;

    /* Check each potential event source - take earliest */
    %let i = 1;
    %let event = %scan(&event_list., &i., ;);
    %do %while(%length(&event.) > 0);
      %let ev_flag = %scan(&event., 1, |);
      %let ev_dt = %scan(&event., 2, |);
      %let ev_desc = %scan(&event., 3, |);

      if &ev_flag. = 1 and not missing(&ev_dt.) then do;
        if _event_found = 0 or &ev_dt. < _earliest_dt then do;
          _earliest_dt = &ev_dt.;
          EVNTDESC = "&ev_desc.";
          _event_found = 1;
        end;
      end;

      %let i = %eval(&i. + 1);
      %let event = %scan(&event., &i., ;);
    %end;

    /* Set event or censor */
    if _event_found = 1 then do;
      ADT = _earliest_dt;
      CNSR = 0;
      CNSDTDSC = '';
    end;
    else if not missing(&censor_dt.) then do;
      ADT = &censor_dt.;
      CNSR = 1;
      EVNTDESC = '';
      CNSDTDSC = "&censor_desc.";
    end;

    /* Calculate AVAL */
    if not missing(ADT) and not missing(&start_var.) then do;
      AVAL = ADT - &start_var. + 1;
      if AVAL < 1 then AVAL = 1;
    end;

    STARTDT = &start_var.;
    format ADT STARTDT date9.;
    drop _event_found _earliest_dt;
  run;
%mend derive_tte_multi;

/*===========================================================================
  %derive_cnsr_desc - Derive censoring descriptions
===========================================================================*/
%macro derive_cnsr_desc(ds=, cnsr_var=CNSR, reason_var=);
  data &ds.;
    set &ds.;
    length CNSDTDSC $200;

    if &cnsr_var. = 1 then do;
      %if %length(&reason_var.) > 0 %then %do;
        if not missing(&reason_var.) then CNSDTDSC = &reason_var.;
        else CNSDTDSC = 'Censored - No event';
      %end;
      %else %do;
        CNSDTDSC = 'Censored';
      %end;
    end;
  run;
%mend derive_cnsr_desc;

/*===========================================================================
  %derive_aval_months - Convert AVAL from days to months
===========================================================================*/
%macro derive_aval_months(ds=, in_var=AVAL, out_var=AVAL_M);
  data &ds.;
    set &ds.;
    if not missing(&in_var.) then
      &out_var. = &in_var. / 30.4375; /* Average days per month */
  run;
%mend derive_aval_months;

/*===========================================================================
  %derive_srcseq - Derive source sequence for traceability
===========================================================================*/
%macro derive_srcseq(ds=, src_ds=, src_var=, src_seq=);
  /* Placeholder for source traceability */
  data &ds.;
    set &ds.;
    length SRCDOM $8 SRCVAR $40 SRCSEQ 8;
    SRCDOM = "&src_ds.";
    SRCVAR = "&src_var.";
    /* SRCSEQ would be derived from source dataset */
  run;
%mend derive_srcseq;

/*===========================================================================
  %create_adtte_shell - Create empty ADTTE structure
===========================================================================*/
%macro create_adtte_shell(out_ds=adtte_shell);
  data &out_ds.;
    length
      STUDYID $12 USUBJID $40 SUBJID $20
      SITEID $10 TRTP $40 TRTPN 8 TRTA $40 TRTAN 8
      AGE 8 AGEU $10 SEX $1 RACE $50
      PARAMCD $8 PARAM $200 PARAMN 8
      AVAL 8 AVALU $20 CNSR 8
      ADT 8 STARTDT 8 ADY 8
      EVNTDESC $200 CNSDTDSC $200
      SRCDOM $8 SRCVAR $40 SRCSEQ 8
      ;
    format ADT STARTDT date9.;
    stop;
  run;
%mend create_adtte_shell;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P086_adtte.sas) ---
/*****************************************************************************
* Program:    adtte.sas
* Purpose:    Create ADTTE (Time-to-Event Analysis Dataset)
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM AE, DS, ADSL
* Output:     ADTTE
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;
%include "&root./programs/sas/macros/own-adam/m_derive_tte.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock source data
==========================================================================*/

/*--- ADSL with dates ---*/
data adsl;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 TRTP $40 TRTPN 8 AGE 8 AGEGR1 $10 SEX $1 RACE $50
         SAFFL $1 ITTFL $1 TRTSDT 8 TRTEDT 8 EOSDT 8 DTHDT 8 DTHFL $1;
  input STUDYID $ USUBJID $ TRTP $ TRTPN AGE AGEGR1 $ SEX $ RACE $ SAFFL $ ITTFL $
        TRTSDT :date9. TRTEDT :date9. EOSDT :date9. DTHDT :date9. DTHFL $;
  format TRTSDT TRTEDT EOSDT DTHDT date9.;
datalines;
STUDY01|STUDY01-1001|Treatment A|1|59|<65|M|WHITE|Y|Y|15JAN2024|15JUN2024|15JUN2024|.|N
STUDY01|STUDY01-1002|Placebo|2|45|<65|F|BLACK|Y|Y|18JAN2024|18JUN2024|18JUN2024|.|N
STUDY01|STUDY01-1003|Treatment A|1|43|<65|M|ASIAN|Y|Y|20JAN2024|01APR2024|01APR2024|15APR2024|Y
STUDY01|STUDY01-1004|Treatment A|1|51|<65|F|WHITE|Y|Y|25JAN2024|25JUN2024|25JUN2024|.|N
STUDY01|STUDY01-1005|Placebo|2|36|<65|M|ASIAN|Y|Y|28JAN2024|.|15JUL2024|.|N
STUDY01|STUDY01-1006|Treatment A|1|68|>=65|F|WHITE|Y|Y|30JAN2024|01JUL2024|01JUL2024|.|N
;
run;

/*--- Disease progression events ---*/
data adrs;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 RSSTRESC $20 RSDTC $10;
  input STUDYID $ USUBJID $ RSSTRESC $ RSDTC $;
datalines;
STUDY01|STUDY01-1001|STABLE DISEASE|2024-03-15
STUDY01|STUDY01-1001|STABLE DISEASE|2024-06-15
STUDY01|STUDY01-1002|STABLE DISEASE|2024-03-18
STUDY01|STUDY01-1002|PROGRESSIVE DISEASE|2024-05-20
STUDY01|STUDY01-1003|PARTIAL RESPONSE|2024-02-20
STUDY01|STUDY01-1003|PROGRESSIVE DISEASE|2024-03-25
STUDY01|STUDY01-1004|PARTIAL RESPONSE|2024-03-25
STUDY01|STUDY01-1005|STABLE DISEASE|2024-04-28
STUDY01|STUDY01-1006|COMPLETE RESPONSE|2024-04-30
;
run;

/*==========================================================================
  STEP 2: Derive PFS (Progression-Free Survival)
==========================================================================*/

/* Get progression date */
proc sort data=adrs; by USUBJID RSDTC; run;

data _prog_;
  set adrs(where=(RSSTRESC='PROGRESSIVE DISEASE'));
  by USUBJID;
  if first.USUBJID;
  PROGDT = input(RSDTC, yymmdd10.);
  keep USUBJID PROGDT;
  format PROGDT date9.;
run;

/* Merge progression and death */
proc sort data=adsl; by USUBJID; run;
proc sort data=_prog_; by USUBJID; run;

data adtte_pfs;
  merge adsl(in=a) _prog_;
  by USUBJID;
  if a;

  length PARAMCD $8 PARAM $200 PARAMN 8;
  length ADT STARTDT 8 AVAL 8 CNSR 8;
  length EVNTDESC $200 CNSDTDSC $200;
  length SRCDOM $8;

  PARAMCD = 'PFS';
  PARAM = 'Progression-Free Survival (Days)';
  PARAMN = 1;

  STARTDT = TRTSDT;

  /*--- Determine event or censoring ---*/
  /* Event: progression or death, whichever comes first */
  if not missing(PROGDT) and not missing(DTHDT) then do;
    if PROGDT <= DTHDT then do;
      ADT = PROGDT;
      EVNTDESC = 'Disease Progression';
      SRCDOM = 'RS';
    end;
    else do;
      ADT = DTHDT;
      EVNTDESC = 'Death';
      SRCDOM = 'ADSL';
    end;
    CNSR = 0;
  end;
  else if not missing(PROGDT) then do;
    ADT = PROGDT;
    CNSR = 0;
    EVNTDESC = 'Disease Progression';
    SRCDOM = 'RS';
  end;
  else if not missing(DTHDT) then do;
    ADT = DTHDT;
    CNSR = 0;
    EVNTDESC = 'Death';
    SRCDOM = 'ADSL';
  end;
  else do;
    /* Censored at last known alive date */
    ADT = coalesce(EOSDT, TRTEDT, TRTSDT);
    CNSR = 1;
    EVNTDESC = '';
    CNSDTDSC = 'Censored - No progression or death';
    SRCDOM = 'ADSL';
  end;

  /*--- Derive AVAL (days) ---*/
  if not missing(ADT) and not missing(STARTDT) then do;
    AVAL = ADT - STARTDT + 1;
    if AVAL < 1 then AVAL = 1;
  end;

  format ADT STARTDT date9.;
  keep STUDYID USUBJID TRTP TRTPN AGE AGEGR1 SEX RACE SAFFL ITTFL
       PARAMCD PARAM PARAMN STARTDT ADT AVAL CNSR EVNTDESC CNSDTDSC SRCDOM;
run;

/*==========================================================================
  STEP 3: Derive OS (Overall Survival)
==========================================================================*/
data adtte_os;
  set adsl;

  length PARAMCD $8 PARAM $200 PARAMN 8;
  length ADT STARTDT 8 AVAL 8 CNSR 8;
  length EVNTDESC $200 CNSDTDSC $200;
  length SRCDOM $8;

  PARAMCD = 'OS';
  PARAM = 'Overall Survival (Days)';
  PARAMN = 2;

  STARTDT = TRTSDT;

  /*--- Event = death ---*/
  if DTHFL = 'Y' and not missing(DTHDT) then do;
    ADT = DTHDT;
    CNSR = 0;
    EVNTDESC = 'Death';
    CNSDTDSC = '';
    SRCDOM = 'ADSL';
  end;
  else do;
    /* Censored at last known alive */
    ADT = coalesce(EOSDT, TRTEDT, TRTSDT);
    CNSR = 1;
    EVNTDESC = '';
    CNSDTDSC = 'Censored - Alive';
    SRCDOM = 'ADSL';
  end;

  /*--- Derive AVAL ---*/
  if not missing(ADT) and not missing(STARTDT) then do;
    AVAL = ADT - STARTDT + 1;
    if AVAL < 1 then AVAL = 1;
  end;

  format ADT STARTDT date9.;
  keep STUDYID USUBJID TRTP TRTPN AGE AGEGR1 SEX RACE SAFFL ITTFL
       PARAMCD PARAM PARAMN STARTDT ADT AVAL CNSR EVNTDESC CNSDTDSC SRCDOM;
run;

/*==========================================================================
  STEP 4: Derive TTR (Time to Response) - for responders only
==========================================================================*/
/* Get first response */
data _resp_;
  set adrs(where=(RSSTRESC in ('COMPLETE RESPONSE', 'PARTIAL RESPONSE')));
  by USUBJID;
  if first.USUBJID;
  RESPDT = input(RSDTC, yymmdd10.);
  keep USUBJID RESPDT;
  format RESPDT date9.;
run;

proc sort data=adsl; by USUBJID; run;
proc sort data=_resp_; by USUBJID; run;

data adtte_ttr;
  merge adsl(in=a) _resp_;
  by USUBJID;
  if a;

  length PARAMCD $8 PARAM $200 PARAMN 8;
  length ADT STARTDT 8 AVAL 8 CNSR 8;
  length EVNTDESC $200 CNSDTDSC $200;

  PARAMCD = 'TTR';
  PARAM = 'Time to Response (Days)';
  PARAMN = 3;

  STARTDT = TRTSDT;

  if not missing(RESPDT) then do;
    ADT = RESPDT;
    CNSR = 0;
    EVNTDESC = 'Response';
  end;
  else do;
    ADT = coalesce(EOSDT, TRTEDT, TRTSDT);
    CNSR = 1;
    EVNTDESC = '';
    CNSDTDSC = 'Censored - No response';
  end;

  if not missing(ADT) and not missing(STARTDT) then do;
    AVAL = ADT - STARTDT + 1;
    if AVAL < 1 then AVAL = 1;
  end;

  format ADT STARTDT date9.;
  keep STUDYID USUBJID TRTP TRTPN AGE AGEGR1 SEX RACE SAFFL ITTFL
       PARAMCD PARAM PARAMN STARTDT ADT AVAL CNSR EVNTDESC CNSDTDSC;
run;

/*==========================================================================
  STEP 5: Combine all TTE parameters
==========================================================================*/
data adtte;
  set adtte_pfs adtte_os adtte_ttr;
run;

/* Derive AVAL in months */
data adtte;
  set adtte;
  if not missing(AVAL) then AVAL_M = AVAL / 30.4375;
  label AVAL_M = 'Analysis Value (Months)';
run;

/*==========================================================================
  STEP 6: Add sequence and labels
==========================================================================*/
proc sort data=adtte; by STUDYID USUBJID PARAMN; run;

data adtte;
  set adtte;
  by STUDYID USUBJID;
  if first.USUBJID then ASEQ = 0;
  ASEQ + 1;

  label
    STUDYID  = 'Study Identifier'
    USUBJID  = 'Unique Subject Identifier'
    ASEQ     = 'Analysis Sequence Number'
    PARAMCD  = 'Parameter Code'
    PARAM    = 'Parameter'
    PARAMN   = 'Parameter (N)'
    AVAL     = 'Analysis Value'
    CNSR     = 'Censor'
    ADT      = 'Analysis Date'
    STARTDT  = 'Time-to-Event Origin Date'
    EVNTDESC = 'Event Description'
    CNSDTDSC = 'Censor Description'
    SRCDOM   = 'Source Domain'
    TRTP     = 'Planned Treatment'
    TRTPN    = 'Planned Treatment (N)'
    SAFFL    = 'Safety Population Flag'
    ITTFL    = 'Intent-to-Treat Population Flag'
    ;
run;

proc delete data=_prog_ _resp_; run;

/*--- Output ---*/
proc contents data=adtte varnum; run;
proc print data=adtte noobs; run;

--- end of SAS program ---

Write the complete R script now.