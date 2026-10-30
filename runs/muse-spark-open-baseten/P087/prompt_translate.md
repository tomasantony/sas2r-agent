Translate the SAS program below to R.

Program id: P087   file: P087_advs.sas   source: own-sas2r/adam   family: own-adam
Declared output types: dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P087
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P087

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P087.lst (table/listing text, 296 numbers)  -> write a text rendering and CSV of the same table
  dataset adsl  -> write OUT_DIR/adsl.csv
  dataset advs  -> write OUT_DIR/advs.csv
  dataset advs_base  -> write OUT_DIR/advs_base.csv

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


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-adam/m_derive_param.sas ---
/*****************************************************************************
* Program:    m_derive_param.sas
* Purpose:    Macros for deriving PARAM, PARAMCD, PARAMN, PARCAT variables
* Standard:   CDISC ADaM IG 1.3
*****************************************************************************/

/*===========================================================================
  %create_param_lookup - Create parameter lookup dataset from metadata
===========================================================================*/
%macro create_param_lookup(out_ds=param_lookup, metadata=);
  /* metadata format: PARAMCD|PARAM|PARAMN|PARCAT1 (pipe-delim, semicolon between)
     Example: SYSBP|Systolic Blood Pressure|1|VITAL SIGNS */

  data &out_ds.;
    length PARAMCD $8 PARAM $200 PARAMN 8 PARCAT1 $50;
    %let i = 1;
    %let param = %scan(&metadata., &i., ;);
    %do %while(%length(&param.) > 0);
      PARAMCD = "%scan(&param., 1, |)";
      PARAM = "%scan(&param., 2, |)";
      PARAMN = %scan(&param., 3, |);
      PARCAT1 = "%scan(&param., 4, |)";
      output;
      %let i = %eval(&i. + 1);
      %let param = %scan(&metadata., &i., ;);
    %end;
  run;
%mend create_param_lookup;

/*===========================================================================
  %assign_paramn - Assign PARAMN based on PARAMCD order
===========================================================================*/
%macro assign_paramn(ds=, by_var=PARAMCD);
  proc sql noprint;
    select distinct &by_var. into :params separated by '|'
    from &ds.
    order by &by_var.;
  quit;

  data &ds.;
    set &ds.;
    %let i = 1;
    %let p = %scan(&params., &i., |);
    %do %while(%length(&p.) > 0);
      if &by_var. = "&p." then PARAMN = &i.;
      %let i = %eval(&i. + 1);
      %let p = %scan(&params., &i., |);
    %end;
  run;
%mend assign_paramn;

/*===========================================================================
  %derive_param_exist - Derive existence parameters (Y/N flags to numeric)
===========================================================================*/
%macro derive_param_exist(in_ds=, out_ds=, flag_var=, paramcd=, param=,
                          by_vars=USUBJID, parcat1=);
  data &out_ds.;
    set &in_ds.;
    length PARAMCD $8 PARAM $200 PARCAT1 $50 AVAL 8 AVALC $20;

    PARAMCD = "&paramcd.";
    PARAM = "&param.";
    %if %length(&parcat1.) > 0 %then %do;
      PARCAT1 = "&parcat1.";
    %end;

    if &flag_var. = 'Y' then do;
      AVAL = 1;
      AVALC = 'Y';
    end;
    else do;
      AVAL = 0;
      AVALC = 'N';
    end;

    keep &by_vars. PARAMCD PARAM PARCAT1 AVAL AVALC;
  run;
%mend derive_param_exist;

/*===========================================================================
  %derive_param_count - Derive count parameters
===========================================================================*/
%macro derive_param_count(in_ds=, out_ds=, count_var=, paramcd=, param=,
                          by_vars=USUBJID, parcat1=);
  proc sql;
    create table &out_ds. as
    select distinct
      %let i = 1;
      %let v = %scan(&by_vars., &i.);
      %do %while(%length(&v.) > 0);
        &v.,
        %let i = %eval(&i. + 1);
        %let v = %scan(&by_vars., &i.);
      %end;
      "&paramcd." as PARAMCD length=8,
      "&param." as PARAM length=200,
      "&parcat1." as PARCAT1 length=50,
      count(&count_var.) as AVAL
    from &in_ds.
    group by
      %let i = 1;
      %let v = %scan(&by_vars., &i.);
      %do %while(%length(&v.) > 0);
        &v.
        %let i = %eval(&i. + 1);
        %let v = %scan(&by_vars., &i.);
        %if %length(&v.) > 0 %then ,;
      %end;
    ;
  quit;
%mend derive_param_count;

/*===========================================================================
  %transpose_to_param - Transpose wide data to ADaM BDS structure
===========================================================================*/
%macro transpose_to_param(in_ds=, out_ds=, by_vars=USUBJID VISIT VISITNUM ADT ADY,
                          var_list=, paramcd_list=, param_list=, parcat1=);
  /* var_list, paramcd_list, param_list should have same number of items */

  data &out_ds.;
    set &in_ds.;
    length PARAMCD $8 PARAM $200 PARCAT1 $50 AVAL 8;

    %if %length(&parcat1.) > 0 %then %do;
      PARCAT1 = "&parcat1.";
    %end;

    %let n = %sysfunc(countw(&var_list.));
    %do i = 1 %to &n.;
      %let var = %scan(&var_list., &i.);
      %let pcd = %scan(&paramcd_list., &i.);
      %let pnm = %scan(&param_list., &i., |);

      if not missing(&var.) then do;
        PARAMCD = "&pcd.";
        PARAM = "&pnm.";
        AVAL = &var.;
        output;
      end;
    %end;

    keep &by_vars. PARAMCD PARAM PARCAT1 AVAL;
  run;
%mend transpose_to_param;

/*===========================================================================
  %derive_paramtyp - Derive PARAMTYP (DERIVED vs original)
===========================================================================*/
%macro derive_paramtyp(ds=, dtype_var=DTYPE);
  data &ds.;
    set &ds.;
    length PARAMTYP $20;
    if not missing(&dtype_var.) then PARAMTYP = 'DERIVED';
    else PARAMTYP = '';
  run;
%mend derive_paramtyp;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-adam/m_derive_extreme.sas ---
/*****************************************************************************
* Program:    m_derive_extreme.sas
* Purpose:    Macros for deriving extreme/summary records (LOCF, WOCF, etc.)
* Standard:   CDISC ADaM IG 1.3
*****************************************************************************/

/*===========================================================================
  %derive_locf - Last Observation Carried Forward
===========================================================================*/
%macro derive_locf(in_ds=, out_ds=, by_vars=USUBJID PARAMCD,
                   date_var=ADT, value_var=AVAL, target_visits=);
  /* target_visits: visits where LOCF should be applied if missing */

  proc sort data=&in_ds. out=_locf_temp;
    by &by_vars. &date_var.;
  run;

  data _locf_temp;
    set _locf_temp;
    by &by_vars.;
    retain _last_aval _last_adt;

    if first.PARAMCD then do;
      _last_aval = .;
      _last_adt = .;
    end;

    if not missing(&value_var.) then do;
      _last_aval = &value_var.;
      _last_adt = &date_var.;
    end;
  run;

  /* Create LOCF records for target visits */
  %if %length(&target_visits.) > 0 %then %do;
    data &out_ds.;
      set _locf_temp;
      length DTYPE $20;

      /* If value is missing at target visit, use LOCF */
      if missing(&value_var.) and _last_aval ne . then do;
        /* Check if this is a target visit - implementation depends on structure */
        &value_var. = _last_aval;
        DTYPE = 'LOCF';
      end;
      drop _last_aval _last_adt;
    run;
  %end;
  %else %do;
    data &out_ds.;
      set _locf_temp;
      drop _last_aval _last_adt;
    run;
  %end;

  proc delete data=_locf_temp; run;
%mend derive_locf;

/*===========================================================================
  %derive_wocf - Worst Observation Carried Forward
===========================================================================*/
%macro derive_wocf(in_ds=, out_ds=, by_vars=USUBJID PARAMCD,
                   value_var=AVAL, direction=HIGH);
  /* direction: HIGH = higher is worse, LOW = lower is worse */

  proc sort data=&in_ds. out=_wocf_temp;
    by &by_vars.;
  run;

  data _wocf_temp;
    set _wocf_temp;
    by &by_vars.;
    retain _worst_aval;

    if first.PARAMCD then _worst_aval = .;

    if not missing(&value_var.) then do;
      %if %upcase(&direction.) = HIGH %then %do;
        if _worst_aval = . or &value_var. > _worst_aval then
          _worst_aval = &value_var.;
      %end;
      %else %do;
        if _worst_aval = . or &value_var. < _worst_aval then
          _worst_aval = &value_var.;
      %end;
    end;
  run;

  data &out_ds.;
    set _wocf_temp;
    WORST_AVAL = _worst_aval;
    drop _worst_aval;
  run;

  proc delete data=_wocf_temp; run;
%mend derive_wocf;

/*===========================================================================
  %derive_extreme_record - Create MIN/MAX/MEAN summary records
===========================================================================*/
%macro derive_extreme_record(in_ds=, out_ds=, by_vars=USUBJID PARAMCD,
                              value_var=AVAL, stat=MAX, dtype_val=MAXIMUM,
                              avisit_val=Maximum Post-Baseline);
  /* stat: MAX, MIN, MEAN, MEDIAN, LAST, FIRST */

  proc sort data=&in_ds. out=_extreme_temp;
    by &by_vars.;
  run;

  %if %upcase(&stat.) = MAX %then %do;
    proc means data=_extreme_temp noprint;
      by &by_vars.;
      var &value_var.;
      output out=_extreme_rec(drop=_type_ _freq_) max=AVAL;
    run;
  %end;
  %else %if %upcase(&stat.) = MIN %then %do;
    proc means data=_extreme_temp noprint;
      by &by_vars.;
      var &value_var.;
      output out=_extreme_rec(drop=_type_ _freq_) min=AVAL;
    run;
  %end;
  %else %if %upcase(&stat.) = MEAN %then %do;
    proc means data=_extreme_temp noprint;
      by &by_vars.;
      var &value_var.;
      output out=_extreme_rec(drop=_type_ _freq_) mean=AVAL;
    run;
  %end;
  %else %if %upcase(&stat.) = LAST %then %do;
    data _extreme_rec;
      set _extreme_temp;
      by &by_vars.;
      if last.PARAMCD;
      keep &by_vars. &value_var.;
      rename &value_var. = AVAL;
    run;
  %end;

  data &out_ds.;
    set _extreme_rec;
    length DTYPE $20 AVISIT $40;
    DTYPE = "&dtype_val.";
    AVISIT = "&avisit_val.";
  run;

  proc delete data=_extreme_temp _extreme_rec; run;
%mend derive_extreme_record;

/*===========================================================================
  %derive_shift - Derive shift from baseline categories
===========================================================================*/
%macro derive_shift(ds=, base_cat=BNRIND, post_cat=ANRIND, shift_var=SHIFT);
  data &ds.;
    set &ds.;
    length &shift_var. $40;

    if not missing(&base_cat.) and not missing(&post_cat.) then
      &shift_var. = catx(' to ', &base_cat., &post_cat.);
  run;
%mend derive_shift;

/*===========================================================================
  %derive_crit_flag - Derive criterion flag based on threshold
===========================================================================*/
%macro derive_crit_flag(ds=, value_var=AVAL, threshold=, direction=GE,
                        flag_var=CRIT1FL, crit_desc=CRIT1);
  /* direction: GE, GT, LE, LT, EQ */

  data &ds.;
    set &ds.;
    length &flag_var. $1 &crit_desc. $200;

    &crit_desc. = "&value_var. &direction. &threshold.";

    if not missing(&value_var.) then do;
      %if %upcase(&direction.) = GE %then %do;
        if &value_var. >= &threshold. then &flag_var. = 'Y';
        else &flag_var. = 'N';
      %end;
      %else %if %upcase(&direction.) = GT %then %do;
        if &value_var. > &threshold. then &flag_var. = 'Y';
        else &flag_var. = 'N';
      %end;
      %else %if %upcase(&direction.) = LE %then %do;
        if &value_var. <= &threshold. then &flag_var. = 'Y';
        else &flag_var. = 'N';
      %end;
      %else %if %upcase(&direction.) = LT %then %do;
        if &value_var. < &threshold. then &flag_var. = 'Y';
        else &flag_var. = 'N';
      %end;
    end;
  run;
%mend derive_crit_flag;

/*===========================================================================
  %derive_avalcat - Derive AVAL categories
===========================================================================*/
%macro derive_avalcat(ds=, value_var=AVAL, cat_var=AVALCAT1,
                      breaks=, labels=);
  /* breaks: numeric cutpoints (comma-separated)
     labels: category labels (|pipe-separated)
     Example: breaks=0,10,20 labels=Low|Medium|High */

  data &ds.;
    set &ds.;
    length &cat_var. $50;

    if not missing(&value_var.) then do;
      %let n_breaks = %sysfunc(countw(&breaks., %str(,)));
      %let n_labels = %sysfunc(countw(&labels., |));

      %do i = 1 %to &n_breaks.;
        %let brk = %scan(&breaks., &i., %str(,));
        %let lbl = %scan(&labels., &i., |);

        %if &i. = 1 %then %do;
          if &value_var. < &brk. then &cat_var. = "&lbl.";
        %end;
        %else %do;
          %let prev_brk = %scan(&breaks., %eval(&i.-1), %str(,));
          else if &value_var. >= &prev_brk. and &value_var. < &brk. then
            &cat_var. = "&lbl.";
        %end;
      %end;

      /* Last category (>= last break) */
      %let last_lbl = %scan(&labels., %eval(&n_breaks.+1), |);
      %let last_brk = %scan(&breaks., &n_breaks., %str(,));
      else if &value_var. >= &last_brk. then &cat_var. = "&last_lbl.";
    end;
  run;
%mend derive_avalcat;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P087_advs.sas) ---
/*****************************************************************************
* Program:    advs.sas
* Purpose:    Create ADVS (Vital Signs Analysis Dataset) - BDS Structure
* Standard:   CDISC ADaM IG 1.3
* Input:      SDTM VS, ADSL
* Output:     ADVS
*****************************************************************************/

%include "&root./programs/sas/macros/own-adam/m_adam_utilities.sas" / nosource2;
%include "&root./programs/sas/macros/own-adam/m_derive_param.sas" / nosource2;
%include "&root./programs/sas/macros/own-adam/m_derive_extreme.sas" / nosource2;

/*==========================================================================
  STEP 1: Create mock SDTM VS data
==========================================================================*/
data vs;
  infile datalines dlm='|' truncover;
  length STUDYID $12 USUBJID $20 VSSEQ 8 VSTESTCD $8 VSTEST $40
         VSORRES $20 VSORRESU $10 VSSTRESN 8 VSSTRESU $10
         VISIT $20 VISITNUM 8 VSDTC $10;
  input STUDYID $ USUBJID $ VSSEQ VSTESTCD $ VSTEST $ VSORRES $
        VSORRESU $ VSSTRESN VSSTRESU $ VISIT $ VISITNUM VSDTC $;
datalines;
STUDY01|STUDY01-1001|1|SYSBP|Systolic Blood Pressure|128|mmHg|128|mmHg|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|2|DIABP|Diastolic Blood Pressure|82|mmHg|82|mmHg|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|3|PULSE|Pulse Rate|72|beats/min|72|beats/min|Baseline|1|2024-01-15
STUDY01|STUDY01-1001|4|SYSBP|Systolic Blood Pressure|122|mmHg|122|mmHg|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|5|DIABP|Diastolic Blood Pressure|78|mmHg|78|mmHg|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|6|PULSE|Pulse Rate|70|beats/min|70|beats/min|Week 4|2|2024-02-12
STUDY01|STUDY01-1001|7|SYSBP|Systolic Blood Pressure|118|mmHg|118|mmHg|Week 8|3|2024-03-11
STUDY01|STUDY01-1001|8|DIABP|Diastolic Blood Pressure|76|mmHg|76|mmHg|Week 8|3|2024-03-11
STUDY01|STUDY01-1002|1|SYSBP|Systolic Blood Pressure|140|mmHg|140|mmHg|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|2|DIABP|Diastolic Blood Pressure|90|mmHg|90|mmHg|Baseline|1|2024-01-18
STUDY01|STUDY01-1002|3|SYSBP|Systolic Blood Pressure|138|mmHg|138|mmHg|Week 4|2|2024-02-15
STUDY01|STUDY01-1002|4|DIABP|Diastolic Blood Pressure|88|mmHg|88|mmHg|Week 4|2|2024-02-15
STUDY01|STUDY01-1003|1|SYSBP|Systolic Blood Pressure|120|mmHg|120|mmHg|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|2|DIABP|Diastolic Blood Pressure|75|mmHg|75|mmHg|Baseline|1|2024-01-20
STUDY01|STUDY01-1003|3|SYSBP|Systolic Blood Pressure|115|mmHg|115|mmHg|Week 4|2|2024-02-17
STUDY01|STUDY01-1004|1|SYSBP|Systolic Blood Pressure|132|mmHg|132|mmHg|Baseline|1|2024-01-25
STUDY01|STUDY01-1005|1|SYSBP|Systolic Blood Pressure|118|mmHg|118|mmHg|Baseline|1|2024-01-28
STUDY01|STUDY01-1006|1|SYSBP|Systolic Blood Pressure|145|mmHg|145|mmHg|Baseline|1|2024-01-30
;
run;

/*==========================================================================
  STEP 2: Create mock ADSL subset
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
  STEP 3: Merge and create base ADVS structure
==========================================================================*/
proc sort data=vs; by STUDYID USUBJID; run;
proc sort data=adsl; by STUDYID USUBJID; run;

data advs_base;
  merge vs(in=a) adsl(in=b);
  by STUDYID USUBJID;
  if a and b;

  length PARAMCD $8 PARAM $200 PARAMN 8 PARCAT1 $50;
  length AVAL 8 AVALC $200 AVALU $20;
  length ADT 8 ADY 8 ATPT $20 ATPTN 8;
  length AVISIT $40 AVISITN 8;
  length ABLFL $1 ANL01FL $1;

  /*--- Map to ADaM PARAM structure ---*/
  PARAMCD = VSTESTCD;
  PARAM = VSTEST;
  PARCAT1 = 'VITAL SIGNS';

  /*--- PARAMN ---*/
  select(PARAMCD);
    when('SYSBP') PARAMN = 1;
    when('DIABP') PARAMN = 2;
    when('PULSE') PARAMN = 3;
    when('TEMP')  PARAMN = 4;
    when('RESP')  PARAMN = 5;
    when('WEIGHT') PARAMN = 6;
    when('HEIGHT') PARAMN = 7;
    otherwise PARAMN = 99;
  end;

  /*--- Analysis value ---*/
  AVAL = VSSTRESN;
  AVALU = VSSTRESU;

  /*--- Analysis date and day ---*/
  if VSDTC ne '' then ADT = input(VSDTC, yymmdd10.);

  if not missing(ADT) and not missing(TRTSDT) then do;
    if ADT >= TRTSDT then ADY = ADT - TRTSDT + 1;
    else ADY = ADT - TRTSDT;
  end;

  /*--- Analysis visit ---*/
  AVISIT = VISIT;
  AVISITN = VISITNUM;

  /*--- Analysis timepoint (if multiple per day) ---*/
  ATPT = '';
  ATPTN = .;

  format ADT date9.;
  drop VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU VISIT VISITNUM VSDTC;
run;

/*==========================================================================
  STEP 4: Derive baseline flag
==========================================================================*/
proc sort data=advs_base; by USUBJID PARAMCD ADT; run;

data advs_base;
  set advs_base;
  by USUBJID PARAMCD;

  /*--- Last value on or before TRTSDT is baseline ---*/
  if not missing(ADT) and not missing(TRTSDT) then do;
    if ADT <= TRTSDT then _pre_trt = 1;
    else _pre_trt = 0;
  end;
run;

/* Identify baseline - last pre-treatment record */
proc sort data=advs_base; by USUBJID PARAMCD descending ADT; run;

data advs_base;
  set advs_base;
  by USUBJID PARAMCD;
  if first.PARAMCD and _pre_trt = 1 then ABLFL = 'Y';
  else ABLFL = '';
  drop _pre_trt;
run;

proc sort data=advs_base; by USUBJID PARAMCD ADT; run;

/*==========================================================================
  STEP 5: Derive BASE, CHG, PCHG
==========================================================================*/
/* Get baseline values */
data _baseline_;
  set advs_base(where=(ABLFL='Y'));
  BASE = AVAL;
  keep USUBJID PARAMCD BASE;
run;

/* Merge baseline */
proc sort data=advs_base; by USUBJID PARAMCD; run;
proc sort data=_baseline_; by USUBJID PARAMCD; run;

data advs;
  merge advs_base(in=a) _baseline_;
  by USUBJID PARAMCD;
  if a;

  /*--- Derive change from baseline ---*/
  if not missing(AVAL) and not missing(BASE) then do;
    CHG = AVAL - BASE;
    if BASE ne 0 then PCHG = (CHG / BASE) * 100;
  end;

  /*--- Analysis flag for post-baseline ---*/
  if ABLFL ne 'Y' and not missing(AVAL) and ADT > TRTSDT then ANL01FL = 'Y';
  else ANL01FL = '';
run;

proc delete data=_baseline_; run;

/*==========================================================================
  STEP 6: Add derived records (e.g., maximum post-baseline)
==========================================================================*/
/* Maximum post-baseline value */
proc means data=advs(where=(ABLFL ne 'Y' and ADT > TRTSDT)) noprint;
  by USUBJID PARAMCD;
  var AVAL;
  output out=_max_post(drop=_type_ _freq_) max=AVAL;
run;

data _max_post;
  merge _max_post(in=a) advs(where=(ABLFL='Y') keep=ABLFL USUBJID PARAMCD BASE);
  by USUBJID PARAMCD;
  if a;
  length DTYPE $20 AVISIT $40;
  DTYPE = 'MAXIMUM';
  AVISIT = 'Maximum Post-Baseline';
  AVISITN = 99;
  if not missing(AVAL) and not missing(BASE) then CHG = AVAL - BASE;
run;

data advs;
  set advs _max_post;
run;

/*==========================================================================
  STEP 7: Add sequence and labels
==========================================================================*/
proc sort data=advs; by STUDYID USUBJID PARAMCD AVISITN ADT; run;

data advs;
  set advs;
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
    DTYPE    = 'Derivation Type'
    TRTP     = 'Planned Treatment'
    TRTPN    = 'Planned Treatment (N)'
    TRTSDT   = 'Date of First Exposure to Treatment'
    SAFFL    = 'Safety Population Flag'
    ;
run;

/*--- Output ---*/
proc contents data=advs varnum; run;
proc print data=advs(obs=20) noobs; run;

--- end of SAS program ---

Write the complete R script now.