Translate the SAS program below to R.

Program id: P089   file: P089_adex.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P089
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P089

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P089.lst (table/listing text, 444 numbers)  -> write a text rendering and CSV of the same table
  dataset adex  -> write OUT_DIR/adex.csv
  dataset adex_base  -> write OUT_DIR/adex_base.csv
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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P089_adex.sas) ---
/*****************************************************************************
* Program:    adex.sas
* Purpose:    Create ADEX (Exposure Analysis Dataset) - BDS Structure
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM EX, ADSL
* Output:     ADEX
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock SDTM EX data
==========================================================================*/
data ex;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 EXSEQ 8 EXTRT $40 EXDOSE 8 EXDOSU $20
         EXDOSFRM $20 EXROUTE $20 EXSTDTC $10 EXENDTC $10 VISITNUM 8 VISIT $20;
  input STUDYID $ USUBJID $ EXSEQ EXTRT $ EXDOSE EXDOSU $ EXDOSFRM $
        EXROUTE $ EXSTDTC $ EXENDTC $ VISITNUM VISIT $;
datalines;
STUDY01|STUDY01-1001|1|Treatment A|100|mg|TABLET|ORAL|2024-01-15|2024-01-28|1|Week 1-2
STUDY01|STUDY01-1001|2|Treatment A|100|mg|TABLET|ORAL|2024-01-29|2024-02-11|2|Week 3-4
STUDY01|STUDY01-1001|3|Treatment A|150|mg|TABLET|ORAL|2024-02-12|2024-03-10|3|Week 5-8
STUDY01|STUDY01-1001|4|Treatment A|150|mg|TABLET|ORAL|2024-03-11|2024-06-15|4|Week 9+
STUDY01|STUDY01-1002|1|Placebo|0|mg|TABLET|ORAL|2024-01-18|2024-02-14|1|Week 1-4
STUDY01|STUDY01-1002|2|Placebo|0|mg|TABLET|ORAL|2024-02-15|2024-06-18|2|Week 5+
STUDY01|STUDY01-1003|1|Treatment A|100|mg|TABLET|ORAL|2024-01-20|2024-02-16|1|Week 1-4
STUDY01|STUDY01-1003|2|Treatment A|100|mg|TABLET|ORAL|2024-02-17|2024-03-15|2|Week 5-8
STUDY01|STUDY01-1003|3|Treatment A|50|mg|TABLET|ORAL|2024-03-16|2024-04-01|3|Week 9+
STUDY01|STUDY01-1004|1|Treatment A|100|mg|TABLET|ORAL|2024-01-25|2024-06-25|1|All
STUDY01|STUDY01-1005|1|Placebo|0|mg|TABLET|ORAL|2024-01-28|.|1|All
STUDY01|STUDY01-1006|1|Treatment A|100|mg|TABLET|ORAL|2024-01-30|2024-03-30|1|Week 1-8
STUDY01|STUDY01-1006|2|Treatment A|150|mg|TABLET|ORAL|2024-03-31|2024-07-01|2|Week 9+
;
run;

/*==========================================================================
  STEP 2: Create mock ADSL
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
  STEP 3: Merge and create base exposure variables
==========================================================================*/
proc sort data=ex; by STUDYID USUBJID; run;
proc sort data=adsl; by STUDYID USUBJID; run;

data adex_base;
  merge ex(in=a) adsl(in=b);
  by STUDYID USUBJID;
  if a and b;

  length PARAMCD $8 PARAM $200 PARAMN 8;
  length AVAL 8 AVALC $40 AVALU $20;
  length ASTDT AENDT 8 ASTDY AENDY 8;
  length AVISIT $40 AVISITN 8;
  length DOSINTFL DOSEADFL $1;

  /*--- Parameter: Dose ---*/
  PARAMCD = 'DOSE';
  PARAM = 'Dose';
  PARAMN = 1;
  AVAL = EXDOSE;
  AVALU = EXDOSU;

  /*--- Convert dates ---*/
  if EXSTDTC ne '' and EXSTDTC ne '.' then ASTDT = input(EXSTDTC, yymmdd10.);
  if EXENDTC ne '' and EXENDTC ne '.' then AENDT = input(EXENDTC, yymmdd10.);

  /*--- Analysis day ---*/
  if not missing(ASTDT) and not missing(TRTSDT) then do;
    if ASTDT >= TRTSDT then ASTDY = ASTDT - TRTSDT + 1;
    else ASTDY = ASTDT - TRTSDT;
  end;
  if not missing(AENDT) and not missing(TRTSDT) then do;
    if AENDT >= TRTSDT then AENDY = AENDT - TRTSDT + 1;
    else AENDY = AENDT - TRTSDT;
  end;

  /*--- Analysis visit ---*/
  AVISIT = VISIT;
  AVISITN = VISITNUM;

  /*--- Duration of this exposure period ---*/
  if not missing(ASTDT) and not missing(AENDT) then
    EXDURN = AENDT - ASTDT + 1;

  /*--- Dose flags ---*/
  /* Dose interruption flag (gap from previous) */
  DOSINTFL = 'N';
  /* Dose adjustment flag */
  DOSEADFL = 'N';

  format ASTDT AENDT date9.;
  drop EXSTDTC EXENDTC VISIT VISITNUM;
run;

/*==========================================================================
  STEP 4: Identify dose adjustments
==========================================================================*/
proc sort data=adex_base; by USUBJID ASTDT; run;

data adex_base;
  set adex_base;
  by USUBJID;

  retain _prev_dose _prev_end;

  if first.USUBJID then do;
    _prev_dose = .;
    _prev_end = .;
  end;

  /*--- Check for dose adjustment from previous ---*/
  if not first.USUBJID and not missing(_prev_dose) and AVAL ne _prev_dose then
    DOSEADFL = 'Y';

  /*--- Check for dose interruption ---*/
  if not first.USUBJID and not missing(_prev_end) and not missing(ASTDT) then do;
    if ASTDT - _prev_end > 1 then DOSINTFL = 'Y';
  end;

  _prev_dose = AVAL;
  _prev_end = AENDT;

  drop _prev_dose _prev_end;
run;

/*==========================================================================
  STEP 5: Create summary exposure parameters
==========================================================================*/

/*--- Total duration ---*/
proc sql;
  create table _totdur as
  select STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         sum(EXDURN) as AVAL,
         'TOTDUR' as PARAMCD length=8,
         'Total Exposure Duration (Days)' as PARAM length=200,
         2 as PARAMN,
         'days' as AVALU length=20
  from adex_base
  group by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX;
quit;

/*--- Average daily dose ---*/
proc sql;
  create table _avgdose as
  select STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         sum(AVAL * EXDURN) / sum(EXDURN) as AVAL,
         'AVGDOSE' as PARAMCD length=8,
         'Average Daily Dose' as PARAM length=200,
         3 as PARAMN,
         'mg' as AVALU length=20
  from adex_base
  where AVAL > 0
  group by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX;
quit;

/*--- Maximum dose ---*/
proc sql;
  create table _maxdose as
  select STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         max(AVAL) as AVAL,
         'MAXDOSE' as PARAMCD length=8,
         'Maximum Dose' as PARAM length=200,
         4 as PARAMN,
         'mg' as AVALU length=20
  from adex_base
  where AVAL > 0
  group by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX;
quit;

/*--- Cumulative dose ---*/
proc sql;
  create table _cumdose as
  select STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         sum(AVAL * EXDURN) as AVAL,
         'CUMDOSE' as PARAMCD length=8,
         'Cumulative Dose' as PARAM length=200,
         5 as PARAMN,
         'mg' as AVALU length=20
  from adex_base
  where AVAL > 0
  group by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX;
quit;

/*--- Number of dose adjustments ---*/
proc sql;
  create table _ndosadj as
  select STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         sum(case when DOSEADFL = 'Y' then 1 else 0 end) as AVAL,
         'NDOSADJ' as PARAMCD length=8,
         'Number of Dose Adjustments' as PARAM length=200,
         6 as PARAMN,
         '' as AVALU length=20
  from adex_base
  group by STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX;
quit;

/*==========================================================================
  STEP 6: Combine all parameters
==========================================================================*/
data adex;
  set adex_base(keep=STUDYID USUBJID TRTP TRTPN SAFFL TRTSDT TRTEDT AGE SEX
                     PARAMCD PARAM PARAMN AVAL AVALU ASTDT AENDT ASTDY AENDY
                     EXDURN DOSINTFL DOSEADFL EXTRT EXDOSE EXDOSU EXDOSFRM EXROUTE EXSEQ AVISIT AVISITN)
      _totdur _avgdose _maxdose _cumdose _ndosadj;
run;

proc delete data=_totdur _avgdose _maxdose _cumdose _ndosadj; run;

/*==========================================================================
  STEP 7: Add sequence and labels
==========================================================================*/
proc sort data=adex; by STUDYID USUBJID PARAMN ASTDT; run;

data adex;
  set adex;
  by STUDYID USUBJID;

  if first.USUBJID then ASEQ = 0;
  ASEQ + 1;

  /*--- Derive dose category ---*/
  length DOSECAT $20;
  if PARAMCD = 'DOSE' then do;
    if AVAL = 0 then DOSECAT = 'Placebo';
    else if AVAL <= 50 then DOSECAT = 'Low';
    else if AVAL <= 100 then DOSECAT = 'Medium';
    else DOSECAT = 'High';
  end;

  label
    STUDYID   = 'Study Identifier'
    USUBJID   = 'Unique Subject Identifier'
    ASEQ      = 'Analysis Sequence Number'
    PARAMCD   = 'Parameter Code'
    PARAM     = 'Parameter'
    PARAMN    = 'Parameter (N)'
    AVAL      = 'Analysis Value'
    AVALU     = 'Analysis Value Unit'
    ASTDT     = 'Analysis Start Date'
    AENDT     = 'Analysis End Date'
    ASTDY     = 'Analysis Start Relative Day'
    AENDY     = 'Analysis End Relative Day'
    AVISIT    = 'Analysis Visit'
    AVISITN   = 'Analysis Visit (N)'
    EXDURN    = 'Exposure Duration (Days)'
    EXTRT     = 'Name of Treatment'
    EXDOSE    = 'Dose per Administration'
    EXDOSU    = 'Dose Units'
    EXDOSFRM  = 'Dose Form'
    EXROUTE   = 'Route of Administration'
    DOSINTFL  = 'Dose Interruption Flag'
    DOSEADFL  = 'Dose Adjustment Flag'
    DOSECAT   = 'Dose Category'
    TRTP      = 'Planned Treatment'
    TRTPN     = 'Planned Treatment (N)'
    SAFFL     = 'Safety Population Flag'
    ;
run;

/*--- Output ---*/
proc contents data=adex varnum; run;
proc print data=adex(obs=20) noobs; run;

--- end of SAS program ---

Write the complete R script now.