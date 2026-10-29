Translate the SAS program below to R.

Program id: P090   file: P090_adcm.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P090
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P090

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P090.lst (table/listing text, 362 numbers)  -> write a text rendering and CSV of the same table
  dataset adcm  -> write OUT_DIR/adcm.csv
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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P090_adcm.sas) ---
/*****************************************************************************
* Program:    adcm.sas
* Purpose:    Create ADCM (Concomitant Medications Analysis Dataset)
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM CM, ADSL
* Output:     ADCM
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock SDTM CM data
==========================================================================*/
data cm;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 CMSEQ 8 CMTRT $60 CMDECOD $60 CMCLAS $60
         CMSTDTC $10 CMENDTC $10 CMDOSE 8 CMDOSU $20 CMROUTE $20 CMFRQ $20
         CMINDC $100 CMONGO $1;
  input STUDYID $ USUBJID $ CMSEQ CMTRT $ CMDECOD $ CMCLAS $
        CMSTDTC $ CMENDTC $ CMDOSE CMDOSU $ CMROUTE $ CMFRQ $ CMINDC $ CMONGO $;
datalines;
STUDY01|STUDY01-1001|1|ASPIRIN|ASPIRIN|ANALGESICS|2023-06-15|.|81|mg|ORAL|QD|CARDIOVASCULAR PROTECTION|Y
STUDY01|STUDY01-1001|2|LISINOPRIL|LISINOPRIL|ACE INHIBITORS|2023-01-10|.|10|mg|ORAL|QD|HYPERTENSION|Y
STUDY01|STUDY01-1001|3|ACETAMINOPHEN|PARACETAMOL|ANALGESICS|2024-02-01|2024-02-03|500|mg|ORAL|PRN|HEADACHE|N
STUDY01|STUDY01-1002|1|METFORMIN|METFORMIN|ANTIDIABETICS|2022-03-20|.|500|mg|ORAL|BID|DIABETES|Y
STUDY01|STUDY01-1002|2|ATORVASTATIN|ATORVASTATIN|STATINS|2023-08-01|.|20|mg|ORAL|QD|HYPERLIPIDEMIA|Y
STUDY01|STUDY01-1003|1|OMEPRAZOLE|OMEPRAZOLE|PROTON PUMP INHIBITORS|2024-01-15|.|20|mg|ORAL|QD|GERD|Y
STUDY01|STUDY01-1003|2|IBUPROFEN|IBUPROFEN|NSAIDS|2024-02-20|2024-02-22|400|mg|ORAL|TID|BACK PAIN|N
STUDY01|STUDY01-1004|1|AMLODIPINE|AMLODIPINE|CALCIUM CHANNEL BLOCKERS|2023-05-10|.|5|mg|ORAL|QD|HYPERTENSION|Y
STUDY01|STUDY01-1005|1|LORATADINE|LORATADINE|ANTIHISTAMINES|2024-03-01|2024-03-15|10|mg|ORAL|QD|ALLERGIES|N
STUDY01|STUDY01-1006|1|LEVOTHYROXINE|LEVOTHYROXINE|THYROID HORMONES|2020-01-01|.|75|mcg|ORAL|QD|HYPOTHYROIDISM|Y
STUDY01|STUDY01-1006|2|CALCIUM|CALCIUM CARBONATE|SUPPLEMENTS|2023-09-01|.|600|mg|ORAL|QD|BONE HEALTH|Y
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
  STEP 3: Merge CM with ADSL and derive variables
==========================================================================*/
proc sort data=cm; by STUDYID USUBJID; run;
proc sort data=adsl; by STUDYID USUBJID; run;

data adcm;
  merge cm(in=a) adsl(in=b);
  by STUDYID USUBJID;
  if a and b;

  length ASTDT AENDT 8 ASTDY AENDY 8;
  length ONTRTFL PREFL CONCOMFL $1;
  length ATCCD1 $10 ATCCD2 $10 ATC1 $60 ATC2 $60;
  length CMDECOD_UP $60 CMCLAS_UP $60;
  length ADTEFFL $1 ADTEFN 8;
  length CMDURN 8;

  /*--- Convert dates ---*/
  if CMSTDTC ne '' and CMSTDTC ne '.' then ASTDT = input(CMSTDTC, yymmdd10.);
  if CMENDTC ne '' and CMENDTC ne '.' then AENDT = input(CMENDTC, yymmdd10.);

  /*--- Derive analysis day ---*/
  if not missing(ASTDT) and not missing(TRTSDT) then do;
    if ASTDT >= TRTSDT then ASTDY = ASTDT - TRTSDT + 1;
    else ASTDY = ASTDT - TRTSDT;
  end;
  if not missing(AENDT) and not missing(TRTSDT) then do;
    if AENDT >= TRTSDT then AENDY = AENDT - TRTSDT + 1;
    else AENDY = AENDT - TRTSDT;
  end;

  /*--- Duration ---*/
  if not missing(ASTDT) and not missing(AENDT) then
    CMDURN = AENDT - ASTDT + 1;

  /*--- Timing flags ---*/
  /* Prior medication: started before first dose */
  if not missing(ASTDT) and not missing(TRTSDT) then do;
    if ASTDT < TRTSDT then PREFL = 'Y';
    else PREFL = 'N';
  end;

  /* On-treatment: active during treatment period */
  if not missing(TRTSDT) then do;
    _trt_end = coalesce(TRTEDT, today());
    if not missing(ASTDT) then do;
      if missing(AENDT) then do;
        /* Ongoing medication */
        if ASTDT <= _trt_end then ONTRTFL = 'Y';
      end;
      else do;
        /* Completed medication - check overlap with treatment */
        if ASTDT <= _trt_end and AENDT >= TRTSDT then ONTRTFL = 'Y';
      end;
    end;
  end;
  if missing(ONTRTFL) then ONTRTFL = 'N';

  /* Concomitant: taken during study */
  if ONTRTFL = 'Y' or PREFL = 'Y' then CONCOMFL = 'Y';
  else CONCOMFL = 'N';

  /*--- Ongoing flag at end of treatment ---*/
  if CMONGO = 'Y' or missing(AENDT) then ADTEFFL = 'Y';
  else ADTEFFL = 'N';
  if ADTEFFL = 'Y' then ADTEFN = 1;
  else ADTEFN = 0;

  /*--- Standardized terms (uppercase) ---*/
  CMDECOD_UP = upcase(CMDECOD);
  CMCLAS_UP = upcase(CMCLAS);

  /*--- Mock ATC codes (in real studies, these come from WHO Drug Dictionary) ---*/
  select(CMCLAS_UP);
    when('ANALGESICS') do; ATCCD1 = 'N02'; ATC1 = 'ANALGESICS'; end;
    when('ACE INHIBITORS') do; ATCCD1 = 'C09A'; ATC1 = 'ACE INHIBITORS, PLAIN'; end;
    when('ANTIDIABETICS') do; ATCCD1 = 'A10'; ATC1 = 'DRUGS USED IN DIABETES'; end;
    when('STATINS') do; ATCCD1 = 'C10AA'; ATC1 = 'HMG COA REDUCTASE INHIBITORS'; end;
    when('PROTON PUMP INHIBITORS') do; ATCCD1 = 'A02BC'; ATC1 = 'PROTON PUMP INHIBITORS'; end;
    when('NSAIDS') do; ATCCD1 = 'M01A'; ATC1 = 'ANTIINFLAMMATORY, ANTIRHEUMATIC NON-STEROIDS'; end;
    when('CALCIUM CHANNEL BLOCKERS') do; ATCCD1 = 'C08CA'; ATC1 = 'DIHYDROPYRIDINE DERIVATIVES'; end;
    when('ANTIHISTAMINES') do; ATCCD1 = 'R06A'; ATC1 = 'ANTIHISTAMINES FOR SYSTEMIC USE'; end;
    when('THYROID HORMONES') do; ATCCD1 = 'H03AA'; ATC1 = 'THYROID HORMONES'; end;
    when('SUPPLEMENTS') do; ATCCD1 = 'A12AA'; ATC1 = 'CALCIUM'; end;
    otherwise do; ATCCD1 = ''; ATC1 = ''; end;
  end;

  format ASTDT AENDT date9.;
  drop _trt_end;
run;

/*==========================================================================
  STEP 4: Add sequence and labels
==========================================================================*/
proc sort data=adcm; by STUDYID USUBJID ASTDT CMSEQ; run;

data adcm;
  set adcm;
  by STUDYID USUBJID;

  if first.USUBJID then ASEQ = 0;
  ASEQ + 1;

  label
    STUDYID   = 'Study Identifier'
    USUBJID   = 'Unique Subject Identifier'
    CMSEQ     = 'Sequence Number'
    ASEQ      = 'Analysis Sequence Number'
    CMTRT     = 'Reported Name of Drug/Therapy'
    CMDECOD   = 'Standardized Medication Name'
    CMCLAS    = 'Medication Class'
    ASTDT     = 'Analysis Start Date'
    AENDT     = 'Analysis End Date'
    ASTDY     = 'Analysis Start Relative Day'
    AENDY     = 'Analysis End Relative Day'
    CMDOSE    = 'Dose per Administration'
    CMDOSU    = 'Dose Units'
    CMROUTE   = 'Route of Administration'
    CMFRQ     = 'Dosing Frequency'
    CMINDC    = 'Indication'
    CMONGO    = 'Ongoing Medication'
    ONTRTFL   = 'On-Treatment Flag'
    PREFL     = 'Prior Medication Flag'
    CONCOMFL  = 'Concomitant Medication Flag'
    ADTEFFL   = 'Medication Active at End of Treatment Flag'
    CMDURN    = 'Medication Duration (Days)'
    ATCCD1    = 'ATC Level 1 Code'
    ATC1      = 'ATC Level 1 Text'
    TRTP      = 'Planned Treatment'
    TRTPN     = 'Planned Treatment (N)'
    SAFFL     = 'Safety Population Flag'
    ;
run;

/*--- Output ---*/
proc contents data=adcm varnum; run;
proc print data=adcm(obs=15) noobs; run;

--- end of SAS program ---

Write the complete R script now.