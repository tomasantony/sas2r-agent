Translate the SAS program below to R.

Program id: P085   file: P085_adlb.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P085
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P085

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P085.lst (table/listing text, 465 numbers)  -> write a text rendering and CSV of the same table
  dataset adlb  -> write OUT_DIR/adlb.csv
  dataset adlb_base  -> write OUT_DIR/adlb_base.csv
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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P085_adlb.sas) ---
/*****************************************************************************
* Program:    adlb.sas
* Purpose:    Create ADLB (Laboratory Analysis Dataset) - BDS Structure
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM LB, ADSL
* Output:     ADLB
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock SDTM LB data
==========================================================================*/
data lb;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 LBSEQ 8 LBTESTCD $8 LBTEST $40
         LBCAT $20 LBORRES $20 LBORRESU $15 LBSTRESN 8 LBSTRESU $15
         LBSTNRLO 8 LBSTNRHI 8 VISIT $20 VISITNUM 8 LBDTC $10 LBNRIND $10;
  input STUDYID $ USUBJID $ LBSEQ LBTESTCD $ LBTEST $ LBCAT $
        LBORRES $ LBORRESU $ LBSTRESN LBSTRESU $ LBSTNRLO LBSTNRHI
        VISIT $ VISITNUM LBDTC $ LBNRIND $;
datalines;
STUDY01|STUDY01-1001|1|ALT|Alanine Aminotransferase|CHEMISTRY|24|U/L|24|U/L|7|56|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|2|AST|Aspartate Aminotransferase|CHEMISTRY|26|U/L|26|U/L|10|40|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|3|CREAT|Creatinine|CHEMISTRY|0.9|mg/dL|0.9|mg/dL|0.7|1.3|Baseline|1|2024-01-15|NORMAL
STUDY01|STUDY01-1001|4|ALT|Alanine Aminotransferase|CHEMISTRY|28|U/L|28|U/L|7|56|Week 4|2|2024-02-12|NORMAL
STUDY01|STUDY01-1001|5|AST|Aspartate Aminotransferase|CHEMISTRY|30|U/L|30|U/L|10|40|Week 4|2|2024-02-12|NORMAL
STUDY01|STUDY01-1001|6|ALT|Alanine Aminotransferase|CHEMISTRY|32|U/L|32|U/L|7|56|Week 8|3|2024-03-11|NORMAL
STUDY01|STUDY01-1002|1|ALT|Alanine Aminotransferase|CHEMISTRY|18|U/L|18|U/L|7|56|Baseline|1|2024-01-18|NORMAL
STUDY01|STUDY01-1002|2|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Week 4|2|2024-02-15|NORMAL
STUDY01|STUDY01-1003|1|ALT|Alanine Aminotransferase|CHEMISTRY|35|U/L|35|U/L|7|56|Baseline|1|2024-01-20|NORMAL
STUDY01|STUDY01-1003|2|ALT|Alanine Aminotransferase|CHEMISTRY|95|U/L|95|U/L|7|56|Week 4|2|2024-02-17|HIGH
STUDY01|STUDY01-1003|3|ALT|Alanine Aminotransferase|CHEMISTRY|180|U/L|180|U/L|7|56|Week 8|3|2024-03-15|HIGH
STUDY01|STUDY01-1004|1|ALT|Alanine Aminotransferase|CHEMISTRY|20|U/L|20|U/L|7|56|Baseline|1|2024-01-25|NORMAL
STUDY01|STUDY01-1005|1|ALT|Alanine Aminotransferase|CHEMISTRY|19|U/L|19|U/L|7|56|Baseline|1|2024-01-28|NORMAL
STUDY01|STUDY01-1006|1|ALT|Alanine Aminotransferase|CHEMISTRY|30|U/L|30|U/L|7|56|Baseline|1|2024-01-30|NORMAL
;
run;

/*==========================================================================
  STEP 2: Mock ADSL
==========================================================================*/
data adsl;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 TRTP $40 TRTPN 8 AGE 8 SEX $1
         TRTSDT 8 TRTEDT 8 SAFFL $1;
  input STUDYID $ USUBJID $ TRTP $ TRTPN AGE SEX $ SAFFL $
        TRTSDT :date9. TRTEDT :date9.;
  format TRTSDT TRTEDT date9.;
datalines;
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024
;
run;

/*==========================================================================
  STEP 3: Merge and derive ADLB variables
==========================================================================*/
proc sort data=lb; by STUDYID USUBJID; run;
proc sort data=adsl; by STUDYID USUBJID; run;

data adlb_base;
  merge lb(in=a) adsl(in=b);
  by STUDYID USUBJID;
  if a and b;

  length PARAMCD $8 PARAM $200 PARAMN 8 PARCAT1 $50;
  length AVAL 8 AVALC $200 AVALU $20;
  length ADT 8 ADY 8;
  length AVISIT $40 AVISITN 8;
  length ANRLO ANRHI 8 ANRIND $20;
  length ABLFL $1 ANL01FL $1;
  length A1LO A1HI 8; /* Analysis normal ranges */
  length SHIFT1 $40 BNRIND $10;

  /*--- Parameter ---*/
  PARAMCD = LBTESTCD;
  PARAM = LBTEST;
  PARCAT1 = LBCAT;

  /*--- PARAMN ---*/
  select(PARAMCD);
    when('ALT')   PARAMN = 1;
    when('AST')   PARAMN = 2;
    when('ALP')   PARAMN = 3;
    when('BILI')  PARAMN = 4;
    when('CREAT') PARAMN = 5;
    when('BUN')   PARAMN = 6;
    when('GLUC')  PARAMN = 7;
    when('HGB')   PARAMN = 8;
    when('WBC')   PARAMN = 9;
    when('PLT')   PARAMN = 10;
    otherwise PARAMN = 99;
  end;

  /*--- Analysis value ---*/
  AVAL = LBSTRESN;
  AVALU = LBSTRESU;

  /*--- Normal ranges ---*/
  ANRLO = LBSTNRLO;
  ANRHI = LBSTNRHI;
  A1LO = ANRLO;
  A1HI = ANRHI;

  /*--- Derive analysis normal range indicator ---*/
  if not missing(AVAL) then do;
    if not missing(ANRLO) and AVAL < ANRLO then ANRIND = 'LOW';
    else if not missing(ANRHI) and AVAL > ANRHI then ANRIND = 'HIGH';
    else if not missing(ANRLO) or not missing(ANRHI) then ANRIND = 'NORMAL';
  end;

  /*--- Analysis date and day ---*/
  if LBDTC ne '' then ADT = input(LBDTC, yymmdd10.);
  if not missing(ADT) and not missing(TRTSDT) then do;
    if ADT >= TRTSDT then ADY = ADT - TRTSDT + 1;
    else ADY = ADT - TRTSDT;
  end;

  /*--- Analysis visit ---*/
  AVISIT = VISIT;
  AVISITN = VISITNUM;

  format ADT date9.;
  drop LBTESTCD LBTEST LBCAT LBORRES LBORRESU LBSTRESN LBSTRESU
       LBSTNRLO LBSTNRHI VISIT VISITNUM LBDTC LBNRIND;
run;

/*==========================================================================
  STEP 4: Derive baseline flag
==========================================================================*/
proc sort data=adlb_base; by USUBJID PARAMCD ADT; run;

data adlb_base;
  set adlb_base;
  by USUBJID PARAMCD;
  if not missing(ADT) and not missing(TRTSDT) then do;
    if ADT <= TRTSDT then _pre_trt = 1;
    else _pre_trt = 0;
  end;
run;

proc sort data=adlb_base; by USUBJID PARAMCD descending ADT; run;

data adlb_base;
  set adlb_base;
  by USUBJID PARAMCD;
  if first.PARAMCD and _pre_trt = 1 then ABLFL = 'Y';
  else ABLFL = '';
  drop _pre_trt;
run;

proc sort data=adlb_base; by USUBJID PARAMCD ADT; run;

/*==========================================================================
  STEP 5: Derive BASE, BNRIND, CHG, PCHG, SHIFT
==========================================================================*/
data _baseline_;
  set adlb_base(where=(ABLFL='Y'));
  BASE = AVAL;
  BNRIND = ANRIND;
  keep USUBJID PARAMCD BASE BNRIND;
run;

proc sort data=adlb_base; by USUBJID PARAMCD; run;
proc sort data=_baseline_; by USUBJID PARAMCD; run;

data adlb;
  merge adlb_base(in=a) _baseline_;
  by USUBJID PARAMCD;
  if a;

  /*--- Change from baseline ---*/
  if not missing(AVAL) and not missing(BASE) then do;
    CHG = AVAL - BASE;
    if BASE ne 0 then PCHG = (CHG / BASE) * 100;
  end;

  /*--- Shift from baseline ---*/
  if not missing(BNRIND) and not missing(ANRIND) then
    SHIFT1 = catx(' to ', BNRIND, ANRIND);

  /*--- Analysis flag ---*/
  if ABLFL ne 'Y' and not missing(AVAL) and ADT > TRTSDT then ANL01FL = 'Y';
  else ANL01FL = '';

  /*--- Ratio to ULN ---*/
  if not missing(AVAL) and not missing(ANRHI) and ANRHI > 0 then
    R2A1HI = AVAL / ANRHI;

  /*--- Baseline ratio to ULN ---*/
  if not missing(BASE) and not missing(A1HI) and A1HI > 0 then
    BR2A1HI = BASE / A1HI;
run;

proc delete data=_baseline_; run;

/*==========================================================================
  STEP 6: Derive potentially clinically significant (PCS) flags
==========================================================================*/
data adlb;
  set adlb;
  length ANRIND2 $20; /* Extended flag */

  /*--- Mark values >3x ULN ---*/
  if not missing(R2A1HI) and R2A1HI > 3 then ANRIND2 = '>3xULN';
  else if not missing(R2A1HI) and R2A1HI > 2 then ANRIND2 = '>2xULN';
  else if ANRIND = 'HIGH' then ANRIND2 = 'HIGH';
  else if ANRIND = 'LOW' then ANRIND2 = 'LOW';
  else ANRIND2 = ANRIND;
run;

/*==========================================================================
  STEP 7: Add sequence and labels
==========================================================================*/
proc sort data=adlb; by STUDYID USUBJID PARAMCD AVISITN ADT; run;

data adlb;
  set adlb;
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
    PARCAT1  = 'Parameter Category 1'
    AVAL     = 'Analysis Value'
    AVALU    = 'Analysis Value Unit'
    BASE     = 'Baseline Value'
    CHG      = 'Change from Baseline'
    PCHG     = 'Percent Change from Baseline'
    ADT      = 'Analysis Date'
    ADY      = 'Analysis Relative Day'
    AVISIT   = 'Analysis Visit'
    AVISITN  = 'Analysis Visit (N)'
    ABLFL    = 'Baseline Record Flag'
    ANL01FL  = 'Analysis Record Flag 01'
    ANRLO    = 'Analysis Normal Range Lower Limit'
    ANRHI    = 'Analysis Normal Range Upper Limit'
    ANRIND   = 'Analysis Normal Range Indicator'
    BNRIND   = 'Baseline Normal Range Indicator'
    SHIFT1   = 'Shift 1'
    R2A1HI   = 'Ratio to Analysis Range 1 Upper Limit'
    BR2A1HI  = 'Baseline Ratio to Analysis Range 1 Upper Limit'
    TRTP     = 'Planned Treatment'
    TRTPN    = 'Planned Treatment (N)'
    SAFFL    = 'Safety Population Flag'
    ;
run;

/*--- Output ---*/
proc contents data=adlb varnum; run;
proc print data=adlb(obs=20) noobs; run;

--- end of SAS program ---

Write the complete R script now.