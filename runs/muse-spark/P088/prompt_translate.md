Translate the SAS program below to R.

Program id: P088   file: P088_adeff.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P088
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P088

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P088.lst (table/listing text, 950 numbers)  -> write a text rendering and CSV of the same table
  dataset adeff  -> write OUT_DIR/adeff.csv
  dataset adeff_resp  -> write OUT_DIR/adeff_resp.csv
  dataset adeff_tumor  -> write OUT_DIR/adeff_tumor.csv
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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P088_adeff.sas) ---
/*****************************************************************************
* Program:    adeff.sas
* Purpose:    Create ADEFF (Efficacy Analysis Dataset) - BDS Structure
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM RS (Response), TU (Tumor), ADSL
* Output:     ADEFF
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock tumor response data
==========================================================================*/
data rs;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 RSSEQ 8 RSTESTCD $8 RSTEST $60
         RSSTRESC $30 RSSTRESN 8 VISIT $20 VISITNUM 8 RSDTC $10;
  input STUDYID $ USUBJID $ RSSEQ RSTESTCD $ RSTEST $ RSSTRESC $
        RSSTRESN VISIT $ VISITNUM RSDTC $;
datalines;
STUDY01|STUDY01-1001|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|3|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 16|4|2024-05-06
STUDY01|STUDY01-1001|4|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 24|5|2024-07-01
STUDY01|STUDY01-1002|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-14
STUDY01|STUDY01-1002|3|OVRLRESP|Overall Response|PROGRESSIVE DISEASE|.|Week 16|4|2024-05-09
STUDY01|STUDY01-1003|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 8|3|2024-03-16
STUDY01|STUDY01-1003|3|OVRLRESP|Overall Response|COMPLETE RESPONSE|.|Week 16|4|2024-05-11
STUDY01|STUDY01-1004|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-25
STUDY01|STUDY01-1004|2|OVRLRESP|Overall Response|STABLE DISEASE|.|Week 8|3|2024-03-21
STUDY01|STUDY01-1005|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-28
STUDY01|STUDY01-1005|2|OVRLRESP|Overall Response|PROGRESSIVE DISEASE|.|Week 8|3|2024-03-24
STUDY01|STUDY01-1006|1|OVRLRESP|Overall Response|STABLE DISEASE|.|Baseline|1|2024-01-30
STUDY01|STUDY01-1006|2|OVRLRESP|Overall Response|PARTIAL RESPONSE|.|Week 8|3|2024-03-26
;
run;

/*--- Tumor measurements ---*/
data tu;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 TUSEQ 8 TUTESTCD $8 TUTEST $40
         TUORRES $10 TUSTRESC $10 TUSTRESN 8 TUSTRESU $10
         TULOC $30 VISIT $20 VISITNUM 8 TUDTC $10;
  input STUDYID $ USUBJID $ TUSEQ TUTESTCD $ TUTEST $ TUORRES $
        TUSTRESC $ TUSTRESN TUSTRESU $ TULOC $ VISIT $ VISITNUM TUDTC $;
datalines;
STUDY01|STUDY01-1001|1|LDIAM|Longest Diameter|45|45|45|mm|LIVER|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|LDIAM|Longest Diameter|42|42|42|mm|LIVER|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|3|LDIAM|Longest Diameter|35|35|35|mm|LIVER|Week 16|4|2024-05-06
STUDY01|STUDY01-1001|4|LDIAM|Longest Diameter|30|30|30|mm|LIVER|Week 24|5|2024-07-01
STUDY01|STUDY01-1002|1|LDIAM|Longest Diameter|38|38|38|mm|LUNG|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|LDIAM|Longest Diameter|40|40|40|mm|LUNG|Week 8|3|2024-03-14
STUDY01|STUDY01-1002|3|LDIAM|Longest Diameter|52|52|52|mm|LUNG|Week 16|4|2024-05-09
STUDY01|STUDY01-1003|1|LDIAM|Longest Diameter|55|55|55|mm|LYMPH NODE|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|LDIAM|Longest Diameter|40|40|40|mm|LYMPH NODE|Week 8|3|2024-03-16
STUDY01|STUDY01-1003|3|LDIAM|Longest Diameter|0|0|0|mm|LYMPH NODE|Week 16|4|2024-05-11
STUDY01|STUDY01-1004|1|LDIAM|Longest Diameter|32|32|32|mm|LIVER|Baseline|1|2024-01-25
STUDY01|STUDY01-1004|2|LDIAM|Longest Diameter|30|30|30|mm|LIVER|Week 8|3|2024-03-21
STUDY01|STUDY01-1005|1|LDIAM|Longest Diameter|28|28|28|mm|LUNG|Baseline|1|2024-01-28
STUDY01|STUDY01-1005|2|LDIAM|Longest Diameter|42|42|42|mm|LUNG|Week 8|3|2024-03-24
STUDY01|STUDY01-1006|1|LDIAM|Longest Diameter|48|48|48|mm|LIVER|Baseline|1|2024-01-30
STUDY01|STUDY01-1006|2|LDIAM|Longest Diameter|36|36|36|mm|LIVER|Week 8|3|2024-03-26
;
run;

/*==========================================================================
  STEP 2: Create mock ADSL
==========================================================================*/
data adsl;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 TRTP $40 TRTPN 8 AGE 8 SEX $1
         TRTSDT 8 TRTEDT 8 SAFFL $1 EFFFL $1;
  input STUDYID $ USUBJID $ TRTP $ TRTPN AGE SEX $ SAFFL $ EFFFL $
        TRTSDT :date9. TRTEDT :date9.;
  format TRTSDT TRTEDT date9.;
datalines;
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|Y|30JAN2024|01JUL2024
;
run;

/*==========================================================================
  STEP 3: Create tumor response parameter (OVRLRESP)
==========================================================================*/
proc sort data=rs; by STUDYID USUBJID; run;
proc sort data=adsl; by STUDYID USUBJID; run;

data adeff_resp;
  merge rs(in=a where=(RSTESTCD='OVRLRESP')) adsl(in=b);
  by STUDYID USUBJID;
  if a and b;

  length PARAMCD $8 PARAM $200 PARAMN 8;
  length AVAL 8 AVALC $40;
  length ADT 8 ADY 8;
  length AVISIT $40 AVISITN 8;
  length ABLFL $1 ANL01FL $1;

  PARAMCD = 'OVRLRESP';
  PARAM = 'Overall Response';
  PARAMN = 1;

  /*--- Convert response to numeric ---*/
  AVALC = RSSTRESC;
  select(upcase(RSSTRESC));
    when('COMPLETE RESPONSE') AVAL = 1;
    when('PARTIAL RESPONSE') AVAL = 2;
    when('STABLE DISEASE') AVAL = 3;
    when('PROGRESSIVE DISEASE') AVAL = 4;
    when('NOT EVALUABLE') AVAL = 5;
    otherwise AVAL = .;
  end;

  /*--- Dates ---*/
  if RSDTC ne '' then ADT = input(RSDTC, yymmdd10.);
  if not missing(ADT) and not missing(TRTSDT) then do;
    if ADT >= TRTSDT then ADY = ADT - TRTSDT + 1;
    else ADY = ADT - TRTSDT;
  end;

  /*--- Visit ---*/
  AVISIT = VISIT;
  AVISITN = VISITNUM;

  /*--- Baseline flag ---*/
  if AVISIT = 'Baseline' then ABLFL = 'Y';
  else ABLFL = '';

  /*--- Post-baseline analysis flag ---*/
  if ABLFL ne 'Y' and not missing(AVAL) then ANL01FL = 'Y';
  else ANL01FL = '';

  format ADT date9.;
  keep STUDYID USUBJID TRTP TRTPN SAFFL EFFFL TRTSDT TRTEDT AGE SEX
       PARAMCD PARAM PARAMN AVAL AVALC ADT ADY AVISIT AVISITN ABLFL ANL01FL;
run;

/*==========================================================================
  STEP 4: Create tumor size parameters
==========================================================================*/
proc sort data=tu; by STUDYID USUBJID; run;

data adeff_tumor;
  merge tu(in=a where=(TUTESTCD='LDIAM')) adsl(in=b);
  by STUDYID USUBJID;
  if a and b;

  length PARAMCD $8 PARAM $200 PARAMN 8;
  length AVAL 8 AVALC $40 AVALU $20;
  length ADT 8 ADY 8;
  length AVISIT $40 AVISITN 8;
  length ABLFL $1 ANL01FL $1;

  PARAMCD = 'SUMDIAM';
  PARAM = 'Sum of Longest Diameters';
  PARAMN = 2;

  AVAL = TUSTRESN;
  AVALU = TUSTRESU;

  /*--- Dates ---*/
  if TUDTC ne '' then ADT = input(TUDTC, yymmdd10.);
  if not missing(ADT) and not missing(TRTSDT) then do;
    if ADT >= TRTSDT then ADY = ADT - TRTSDT + 1;
    else ADY = ADT - TRTSDT;
  end;

  /*--- Visit ---*/
  AVISIT = VISIT;
  AVISITN = VISITNUM;

  /*--- Baseline flag ---*/
  if AVISIT = 'Baseline' then ABLFL = 'Y';
  else ABLFL = '';

  /*--- Post-baseline flag ---*/
  if ABLFL ne 'Y' and not missing(AVAL) then ANL01FL = 'Y';
  else ANL01FL = '';

  format ADT date9.;
  keep STUDYID USUBJID TRTP TRTPN SAFFL EFFFL TRTSDT TRTEDT AGE SEX
       PARAMCD PARAM PARAMN AVAL AVALC AVALU ADT ADY AVISIT AVISITN ABLFL ANL01FL;
run;

/*==========================================================================
  STEP 5: Derive baseline and change from baseline for tumor
==========================================================================*/
data _base_tumor;
  set adeff_tumor(where=(ABLFL='Y'));
  BASE = AVAL;
  keep USUBJID PARAMCD BASE;
run;

proc sort data=adeff_tumor; by USUBJID PARAMCD; run;
proc sort data=_base_tumor; by USUBJID PARAMCD; run;

data adeff_tumor;
  merge adeff_tumor(in=a) _base_tumor;
  by USUBJID PARAMCD;
  if a;

  /*--- Change from baseline ---*/
  if not missing(AVAL) and not missing(BASE) then do;
    CHG = AVAL - BASE;
    if BASE > 0 then PCHG = (CHG / BASE) * 100;
  end;
run;

proc delete data=_base_tumor; run;

/*==========================================================================
  STEP 6: Derive best overall response (BOR)
==========================================================================*/
proc sort data=adeff_resp out=_resp_sorted;
  by USUBJID AVAL;
  where ABLFL ne 'Y';
run;

data _bor;
  set _resp_sorted;
  by USUBJID;
  if first.USUBJID; /* Best response = lowest AVAL (CR=1, PR=2, etc.) */

  PARAMCD = 'BOR';
  PARAM = 'Best Overall Response';
  PARAMN = 3;
  AVISIT = 'Best Response';
  AVISITN = 99;
  ABLFL = '';
  ANL01FL = 'Y';
run;

proc delete data=_resp_sorted; run;

/*==========================================================================
  STEP 7: Derive responder flag
==========================================================================*/
data _respfl;
  set _bor;

  length RESPFL $1;
  /* Responder = CR or PR */
  if AVAL in (1, 2) then RESPFL = 'Y';
  else RESPFL = 'N';

  PARAMCD = 'RESPFL';
  PARAM = 'Objective Response Flag';
  PARAMN = 4;
  AVAL = ifn(RESPFL = 'Y', 1, 0);
  AVALC = RESPFL;
run;

/*==========================================================================
  STEP 8: Derive minimum tumor size (nadir)
==========================================================================*/
proc means data=adeff_tumor(where=(ABLFL ne 'Y')) noprint;
  by USUBJID;
  var AVAL;
  output out=_nadir(drop=_type_ _freq_) min=AVAL;
run;

data _nadir;
  merge _nadir(in=a) adeff_tumor(where=(ABLFL='Y') keep=ABLFL USUBJID STUDYID TRTP TRTPN
               SAFFL EFFFL TRTSDT TRTEDT AGE SEX BASE);
  by USUBJID;
  if a;

  PARAMCD = 'NADIRTUM';
  PARAM = 'Nadir Tumor Size';
  PARAMN = 5;
  AVALU = 'mm';
  AVISIT = 'Nadir';
  AVISITN = 98;
  ABLFL = '';
  ANL01FL = 'Y';
  if not missing(AVAL) and not missing(BASE) then do;
    CHG = AVAL - BASE;
    if BASE > 0 then PCHG = (CHG / BASE) * 100;
  end;
run;

/*==========================================================================
  STEP 9: Combine all efficacy parameters
==========================================================================*/
data adeff;
  set adeff_resp adeff_tumor _bor _respfl _nadir;
run;

proc delete data=_bor _respfl _nadir; run;

/*==========================================================================
  STEP 10: Add sequence and labels
==========================================================================*/
proc sort data=adeff; by STUDYID USUBJID PARAMN AVISITN ADT; run;

data adeff;
  set adeff;
  by STUDYID USUBJID;

  if first.USUBJID then ASEQ = 0;
  ASEQ + 1;

  /*--- Derive response categories ---*/
  length RSCAT $30;
  if PARAMCD in ('OVRLRESP', 'BOR') then do;
    if AVALC in ('COMPLETE RESPONSE', 'PARTIAL RESPONSE') then RSCAT = 'RESPONDER';
    else if AVALC = 'STABLE DISEASE' then RSCAT = 'STABLE';
    else if AVALC = 'PROGRESSIVE DISEASE' then RSCAT = 'PROGRESSOR';
    else RSCAT = 'OTHER';
  end;

  label
    STUDYID  = 'Study Identifier'
    USUBJID  = 'Unique Subject Identifier'
    ASEQ     = 'Analysis Sequence Number'
    PARAMCD  = 'Parameter Code'
    PARAM    = 'Parameter'
    PARAMN   = 'Parameter (N)'
    AVAL     = 'Analysis Value'
    AVALC    = 'Analysis Value (C)'
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
    RSCAT    = 'Response Category'
    TRTP     = 'Planned Treatment'
    TRTPN    = 'Planned Treatment (N)'
    EFFFL    = 'Efficacy Population Flag'
    SAFFL    = 'Safety Population Flag'
    ;
run;

/*--- Output ---*/
proc contents data=adeff varnum; run;
proc print data=adeff noobs; run;

--- end of SAS program ---

Write the complete R script now.