Translate the SAS program below to R.

Program id: P084   file: P084_adae.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P084
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P084

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P084.lst (table/listing text, 373 numbers)  -> write a text rendering and CSV of the same table
  dataset adae  -> write OUT_DIR/adae.csv
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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P084_adae.sas) ---
/*****************************************************************************
* Program:    adae.sas
* Purpose:    Create ADAE (Adverse Events Analysis Dataset)
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM AE, ADSL
* Output:     ADAE
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock SDTM AE data
==========================================================================*/
data ae;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 AESEQ 8 AETERM $60 AEDECOD $60 AEBODSYS $60
         AESTDTC $10 AEENDTC $10 AESEV $10 AESER $1 AEREL $20 AEACN $25
         AEOUT $40 AETOXGR $2;
  input STUDYID $ USUBJID $ AESEQ AETERM $ AEDECOD $ AEBODSYS $
        AESTDTC $ AEENDTC $ AESEV $ AESER $ AEREL $ AEACN $ AEOUT $ AETOXGR $;
datalines;
STUDY01|STUDY01-1001|1|Headache|Headache|Nervous system disorders|2024-02-01|2024-02-03|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1001|2|Nausea|Nausea|Gastrointestinal disorders|2024-02-15|2024-02-18|MODERATE|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|2
STUDY01|STUDY01-1002|1|Fatigue|Fatigue|General disorders|2024-02-10|2024-03-01|MILD|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1003|1|Dizziness|Dizziness|Nervous system disorders|2024-02-20|2024-02-22|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1003|2|Rash|Rash|Skin disorders|2024-03-05|2024-03-15|MODERATE|N|PROBABLY RELATED|DOSE REDUCED|RECOVERED|2
STUDY01|STUDY01-1003|3|Hepatotoxicity|Hepatotoxicity|Hepatobiliary disorders|2024-03-25|.|SEVERE|Y|RELATED|DRUG WITHDRAWN|NOT RECOVERED|3
STUDY01|STUDY01-1004|1|Insomnia|Insomnia|Psychiatric disorders|2024-02-25|2024-03-10|MILD|N|UNLIKELY|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1005|1|Vomiting|Vomiting|Gastrointestinal disorders|2024-03-01|2024-03-02|MODERATE|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|2
STUDY01|STUDY01-1006|1|Back pain|Back pain|Musculoskeletal disorders|2024-02-28|2024-03-05|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1
;
run;

/*==========================================================================
  STEP 2: Create mock ADSL
==========================================================================*/
data adsl;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 SITEID $4 TRTP $40 TRTPN 8
         AGE 8 AGEGR1 $10 SEX $1 RACE $50 SAFFL $1
         TRTSDT 8 TRTEDT 8;
  input STUDYID $ USUBJID $ SITEID $ TRTP $ TRTPN AGE AGEGR1 $ SEX $ RACE $ SAFFL $
        TRTSDT :date9. TRTEDT :date9.;
  format TRTSDT TRTEDT date9.;
datalines;
STUDY01|STUDY01-1001|101|Treatment A|1|59|<65|M|WHITE|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|101|Placebo|2|45|<65|F|BLACK OR AFRICAN AMERICAN|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|102|Treatment A|1|43|<65|M|ASIAN|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|102|Treatment A|1|51|<65|F|WHITE|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|103|Placebo|2|36|<65|M|ASIAN|Y|28JAN2024|.
STUDY01|STUDY01-1006|103|Treatment A|1|68|>=65|F|WHITE|Y|30JAN2024|01JUL2024
;
run;

/*==========================================================================
  STEP 3: Merge AE with ADSL and derive ADAE variables
==========================================================================*/
proc sort data=ae; by STUDYID USUBJID; run;
proc sort data=adsl; by STUDYID USUBJID; run;

data adae;
  merge ae(in=a) adsl(in=b keep=STUDYID USUBJID SITEID TRTP TRTPN AGE AGEGR1
                               SEX RACE SAFFL TRTSDT TRTEDT);
  by STUDYID USUBJID;
  if a and b;

  length ASTDT AENDT 8 ASTDY AENDY 8;
  length TRTEMFL AESER_FL AOCCFL AOCCSFL AOCCPFL $1;
  length ASEV $10 ASEVN 8 AREL $20 ARELN 8;
  length AESOC $60 AEDECOD_UP $60;

  /*--- Convert dates ---*/
  if AESTDTC ne '' then ASTDT = input(AESTDTC, yymmdd10.);
  if AEENDTC ne '' and AEENDTC ne '.' then AENDT = input(AEENDTC, yymmdd10.);

  /*--- Derive analysis day ---*/
  if not missing(ASTDT) and not missing(TRTSDT) then do;
    if ASTDT >= TRTSDT then ASTDY = ASTDT - TRTSDT + 1;
    else ASTDY = ASTDT - TRTSDT;
  end;
  if not missing(AENDT) and not missing(TRTSDT) then do;
    if AENDT >= TRTSDT then AENDY = AENDT - TRTSDT + 1;
    else AENDY = AENDT - TRTSDT;
  end;

  /*--- Treatment-emergent flag ---*/
  if not missing(ASTDT) and not missing(TRTSDT) then do;
    if ASTDT >= TRTSDT then do;
      if missing(TRTEDT) then TRTEMFL = 'Y';
      else if ASTDT <= TRTEDT + 30 then TRTEMFL = 'Y'; /* 30-day grace */
    end;
  end;
  if missing(TRTEMFL) then TRTEMFL = 'N';

  /*--- Analysis severity ---*/
  ASEV = AESEV;
  select(upcase(AESEV));
    when('MILD') ASEVN = 1;
    when('MODERATE') ASEVN = 2;
    when('SEVERE') ASEVN = 3;
    otherwise ASEVN = .;
  end;

  /*--- Analysis relationship ---*/
  AREL = AEREL;
  select(upcase(AEREL));
    when('NOT RELATED') ARELN = 1;
    when('UNLIKELY') ARELN = 2;
    when('POSSIBLY RELATED') ARELN = 3;
    when('PROBABLY RELATED') ARELN = 4;
    when('RELATED') ARELN = 5;
    otherwise ARELN = .;
  end;

  /*--- Serious flag ---*/
  AESER_FL = AESER;

  /*--- SOC and PT ---*/
  AESOC = AEBODSYS;
  AEDECOD_UP = upcase(AEDECOD);

  /*--- AE duration ---*/
  if not missing(ASTDT) and not missing(AENDT) then
    AEDUR = AENDT - ASTDT + 1;

  format ASTDT AENDT date9.;
run;

/*==========================================================================
  STEP 4: Derive occurrence flags
==========================================================================*/
proc sort data=adae; by USUBJID AEBODSYS AEDECOD ASTDT; run;

data adae;
  set adae;
  by USUBJID AEBODSYS AEDECOD;

  /*--- First occurrence flags ---*/
  if first.USUBJID then AOCCFL = 'Y'; else AOCCFL = '';
  if first.AEBODSYS then AOCCSFL = 'Y'; else AOCCSFL = '';
  if first.AEDECOD then AOCCPFL = 'Y'; else AOCCPFL = '';
run;

/*==========================================================================
  STEP 5: Add labels and sequence
==========================================================================*/
proc sort data=adae; by STUDYID USUBJID ASTDT AESEQ; run;

data adae;
  set adae;
  by STUDYID USUBJID;

  if first.USUBJID then ASEQ = 0;
  ASEQ + 1;

  label
    STUDYID  = 'Study Identifier'
    USUBJID  = 'Unique Subject Identifier'
    AESEQ    = 'Sequence Number'
    ASEQ     = 'Analysis Sequence Number'
    AETERM   = 'Reported Term for the Adverse Event'
    AEDECOD  = 'Dictionary-Derived Term'
    AEBODSYS = 'Body System or Organ Class'
    AESOC    = 'Primary System Organ Class'
    ASTDT    = 'Analysis Start Date'
    AENDT    = 'Analysis End Date'
    ASTDY    = 'Analysis Start Relative Day'
    AENDY    = 'Analysis End Relative Day'
    ASEV     = 'Analysis Severity/Intensity'
    ASEVN    = 'Analysis Severity/Intensity (N)'
    AREL     = 'Analysis Causality'
    ARELN    = 'Analysis Causality (N)'
    TRTEMFL  = 'Treatment Emergent Analysis Flag'
    AESER_FL = 'Serious Event Flag'
    AOCCFL   = 'First Occurrence of Any AE Flag'
    AOCCSFL  = 'First Occurrence Within SOC Flag'
    AOCCPFL  = 'First Occurrence of Preferred Term Flag'
    AEDUR    = 'AE Duration (Days)'
    TRTP     = 'Planned Treatment'
    TRTPN    = 'Planned Treatment (N)'
    TRTSDT   = 'Date of First Exposure to Treatment'
    TRTEDT   = 'Date of Last Exposure to Treatment'
    SAFFL    = 'Safety Population Flag'
    ;
run;

/*--- Output ---*/
proc contents data=adae varnum; run;
proc print data=adae(obs=10) noobs; run;

--- end of SAS program ---

Write the complete R script now.