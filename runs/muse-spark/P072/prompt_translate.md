Translate the SAS program below to R.

Program id: P072   file: P072_t_dm.sas   source: own-sas2r/tlf   family: own-tlf
Declared output types: table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P072
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P072
  &pgmpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &macpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &ntrt. = 3

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
  sdtm -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  t_dm.rtf (table/listing text, 179 numbers)  -> write a text rendering and CSV of the same table

--- family environment (own-tlf) that ran before the program ---
/* owner's TLF programs on the synthetic study: the programs %include utilities/u_setup.sas via &pgmpath */
%global pgmpath macpath ntrt;
%let pgmpath = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf;
%let macpath = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf;
%let ntrt = 3;
libname adam "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam" access=readonly;
libname sdtm "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm" access=readonly;
ods listing close;   /* figures go to the RTF, not the listing */


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf/utilities/u_setup.sas ---
/* u_setup.sas (test copy): study paths for the owner's TLF programs, pointed at the synthetic study (make_synthetic_study.sas)
   from phuse-scripts (loaded into D:\meta_hackathon\sas2r-agent\data\derived\cdisc-pilot). */
%let root     = D:/meta_hackathon/sas2r-agent;
%let adampath = D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam;
%let sdtmpath = D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm;
%let tlfpath  = D:/meta_hackathon/sas2r-agent/output/sas/tlf;
%let macpath  = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf;
%let pgmpath  = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf;
%let fmtpath  = D:/meta_hackathon/sas2r-agent/data/derived/own-formats;
%let logpath  = D:/meta_hackathon/sas2r-agent/output/sas/logs;
options dlcreatedir;
libname adam   "&adampath." access=readonly;
libname sdtm   "&sdtmpath." access=readonly;
libname outlib  "&tlfpath.";
libname library "&fmtpath.";

/*--- Load formats ---*/
options fmtsearch=(library work);

/*--- Global study parameters ---*/
%let studyid  = [STUDYID];
%let protocol = [Protocol Number];
%let sponsor  = [Sponsor Name];
%let cutdate  = [DDMONYYYY];       /* Data cutoff date */

/*--- Treatment labels and display order ---*/
%let trt1lbl = Placebo;
%let trt2lbl = [Drug] Low Dose;
%let trt3lbl = [Drug] High Dose;
%let trt4lbl = [Drug] Total;

/*--- Population flags ---*/
/* ITTFL  = Intent-to-Treat flag       */
/* SAFFL  = Safety Analysis Set flag    */
/* EFFFL  = Efficacy Analysis Set flag  */
/* PPROTFL= Per-Protocol flag           */

/*--- ODS setup for RTF outputs ---*/
%macro ods_on(filename=, style=);
    ods listing close;
    options orientation=landscape nodate nonumber;  /* benchmark: must precede ODS RTF open; m_report sets it too late */
    ods escapechar='^';
    ods rtf file="&tlfpath./&filename..rtf" 
        style=%if &style.= %then %do; journal %end; %else %do; &style. %end;
        bodytitle;
%mend ods_on;

%macro ods_off;
    ods rtf close;
    ods listing;
%mend ods_off;

/*--- Standard page setup ---*/
%macro page_setup(orient=landscape);
    options orientation=&orient.;
    ods rtf style=journal;
%mend page_setup;

/*--- Timestamp for log ---*/
%put NOTE: ============================================;
%put NOTE: Setup completed at %sysfunc(datetime(), datetime20.);
%put NOTE: Study: &studyid.;
%put NOTE: Data cutoff: &cutdate.;
%put NOTE: ============================================;
%let ntrt = 3;   /* test copy: number of treatment groups, used by t_lb_shift */


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf/m_counts.sas ---
/*****************************************************************************
* Macro:      m_counts.sas
* Purpose:    Derive Big N and small n treatment group counts
* Author:     [Programmer Name]
* Date:       [Date]
* Parameters: inds     = Input ADaM dataset
*             trtvar   = Treatment variable (default: TRTA)
*             popflag  = Population flag variable (default: SAFFL)
*             popval   = Population flag value (default: Y)
*             subjvar  = Subject variable (default: USUBJID)
*             outds    = Output dataset with counts
* Notes:      Creates Big N header values and macro variables
*****************************************************************************/

%macro m_counts(inds=adam.adsl, trtvar=TRTA, popflag=SAFFL, popval=Y,
                subjvar=USUBJID, outds=work._bign);

    /*--- Get Big N for each treatment group ---*/
    proc sql noprint;
        create table &outds. as
        select &trtvar.,
               count(distinct &subjvar.) as bign format=8.
        from &inds.
        where upcase(&popflag.) = "%upcase(&popval.)"
        group by &trtvar.
        order by &trtvar.;
    quit;

    /*--- Create macro variables for Big N ---*/
    data _null_;
        set &outds.;
        call symputx(cats('bign', _n_), bign);
        call symputx(cats('trtlbl', _n_), &trtvar.);
        call symputx('ntrt', _n_);
    run;

    /*--- Create Total if not present ---*/
    proc sql noprint;
        select count(distinct &subjvar.) into :bigntot trimmed
        from &inds.
        where upcase(&popflag.) = "%upcase(&popval.)";
    quit;

    /*--- Build header strings ---*/
    %do i = 1 %to &ntrt.;
        %global col&i.hdr;
        %let col&i.hdr = &&trtlbl&i.~(N=&&bign&i.);
    %end;
    %global coltothdr;
    %let coltothdr = Total~(N=&bigntot.);

    %put NOTE: ============================================;
    %put NOTE: Treatment group counts:;
    %do i = 1 %to &ntrt.;
        %put NOTE:   &&trtlbl&i.: N=&&bign&i.;
    %end;
    %put NOTE:   Total: N=&bigntot.;
    %put NOTE: ============================================;

%mend m_counts;


/*****************************************************************************
* Macro:      m_smalln
* Purpose:    Derive small n (number of subjects with non-missing values)
*             for a specific analysis variable within each treatment group
* Parameters: inds     = Input dataset
*             trtvar   = Treatment variable
*             anavar   = Analysis variable to count non-missing
*             byvar    = Optional BY variable (e.g., PARAMCD, AVISIT)
*             subjvar  = Subject variable
*             outds    = Output dataset
*****************************************************************************/

%macro m_smalln(inds=, trtvar=TRTA, anavar=, byvar=, subjvar=USUBJID, outds=work._smalln);

    proc sql noprint;
        create table &outds. as
        select &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
               , count(distinct &subjvar.) as n format=8.
        from &inds.
        where &anavar. is not missing
        group by &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
        order by &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
        ;
    quit;

%mend m_smalln;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf/m_desc_stats.sas ---
/*****************************************************************************
* Macro:      m_desc_stats.sas
* Purpose:    Compute descriptive statistics for continuous and categorical
*             variables, formatted for TLF reporting
* Author:     [Programmer Name]
* Date:       [Date]
* Parameters: See individual sub-macros below
*****************************************************************************/

/*==========================================================================
  CONTINUOUS VARIABLES - Descriptive Statistics
  n, Mean, SD, Median, Min, Max, Q1, Q3
==========================================================================*/

%macro m_cont_stats(inds=, anavar=, trtvar=TRTA, byvar=, popflag=, 
                    popval=Y, subjvar=USUBJID, outds=work._contstats,
                    decimals=1);

    %let dp = &decimals.;
    %let dp1 = %eval(&decimals. + 1);

    /*--- Compute statistics ---*/
    proc means data=&inds. nway noprint;
        %if &popflag. ne %then %do;
            where upcase(&popflag.) = "%upcase(&popval.)";
        %end;
        class &trtvar. %if &byvar. ne %then %do; &byvar. %end;;
        var &anavar.;
        output out=_stats(drop=_type_ _freq_)
            n=n mean=mean std=std median=median min=min max=max
            q1=q1 q3=q3;
    run;

    /*--- Format statistics for display ---*/
    data &outds.;
        length stat $40 value $60;
        set _stats;

        array _dispord{8} _temporary_ (1 2 3 4 5 6 7 8);
        array _statlbl{8} $40 _temporary_ 
            ('n' 'Mean' 'SD' 'Median' 'Q1' 'Q3' 'Min' 'Max');
        
        /* n */
        stat = 'n'; ord = 1;
        value = strip(put(n, 8.));
        output;

        /* Mean (dp1) */
        stat = 'Mean'; ord = 2;
        if n > 0 then value = strip(put(mean, 12.&dp1.));
        else value = '';
        output;

        /* SD (dp1) */
        stat = 'SD'; ord = 3;
        if n > 1 then value = strip(put(std, 12.&dp1.));
        else value = '';
        output;

        /* Median (dp) */
        stat = 'Median'; ord = 4;
        if n > 0 then value = strip(put(median, 12.&dp.));
        else value = '';
        output;

        /* Q1, Q3 (dp) */
        stat = 'Q1, Q3'; ord = 5;
        if n > 0 then value = catx(', ', strip(put(q1, 12.&dp.)), 
                                          strip(put(q3, 12.&dp.)));
        else value = '';
        output;

        /* Min, Max (dp) */
        stat = 'Min, Max'; ord = 6;
        if n > 0 then value = catx(', ', strip(put(min, 12.&dp.)), 
                                          strip(put(max, 12.&dp.)));
        else value = '';
        output;

        keep &trtvar. %if &byvar. ne %then %do; &byvar. %end; stat value ord;
    run;

    proc sort data=&outds.; 
        by %if &byvar. ne %then %do; &byvar. %end; ord &trtvar.; 
    run;

    proc datasets lib=work nolist; delete _stats; quit;

%mend m_cont_stats;


/*==========================================================================
  CATEGORICAL VARIABLES - Frequency Counts n (%)
==========================================================================*/

%macro m_cat_stats(inds=, catvar=, trtvar=TRTA, byvar=, popflag=,
                   popval=Y, subjvar=USUBJID, outds=work._catstats,
                   denom=bign, pctfmt=5.1);

    /*--- Count subjects per category ---*/
    proc sql noprint;
        create table _catcnt as
        select &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
               , &catvar.
               , count(distinct &subjvar.) as cnt format=8.
        from &inds.
        %if &popflag. ne %then %do;
            where upcase(&popflag.) = "%upcase(&popval.)")
        %end;
        group by &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
               , &catvar.
        ;
    quit;

    /*--- Merge with Big N for denominator ---*/
    proc sql noprint;
        create table &outds. as
        select a.*,
               b.bign,
               case when b.bign > 0 
                    then a.cnt / b.bign * 100 
                    else . 
               end as pct format=&pctfmt.
        from _catcnt a
        left join work._bign b
        on a.&trtvar. = b.&trtvar.
        order by %if &byvar. ne %then %do; a.&byvar., %end; 
                 a.&catvar., a.&trtvar.;
    quit;

    /*--- Format n (%) display string ---*/
    data &outds.;
        set &outds.;
        length value $30;
        if cnt > 0 then 
            value = cat(strip(put(cnt, 8.)), ' (', 
                        strip(put(pct, &pctfmt.)), ')');
        else value = '0';
    run;

    proc datasets lib=work nolist; delete _catcnt; quit;

%mend m_cat_stats;


/*==========================================================================
  P-VALUE FORMATTING
==========================================================================*/

%macro m_pvalue(pval=, outvar=pval_fmt);

    length &outvar. $10;

    if &pval. = . then &outvar. = '';
    else if &pval. < 0.001 then &outvar. = '<0.001';
    else if &pval. > 0.999 then &outvar. = '>0.999';
    else &outvar. = strip(put(&pval., 6.3));

%mend m_pvalue;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf/m_report.sas ---
/*****************************************************************************
* Macro:      m_report.sas
* Purpose:    Standard PROC REPORT wrapper for consistent TLF formatting
* Author:     [Programmer Name]
* Date:       [Date]
* Parameters: inds       = Input dataset
*             columns    = PROC REPORT column specification
*             defvars    = Variable definitions (passed as-is)
*             compute    = Optional compute blocks
*             breakafter = Optional break-after variable
*             orient     = Page orientation (landscape|portrait)
*             style      = ODS style name
*****************************************************************************/

%macro m_report(inds=, columns=, defvars=, compute=, breakafter=,
                orient=landscape, style=journal, filename=, width=9.5in);

    options orientation=&orient. nodate nonumber;

    /*--- Column width helper ---*/
    ods escapechar='^';

    proc report data=&inds. nowd split='~'
        style(report) = [outputwidth=&width. font_size=9pt]
        style(header) = [just=center fontweight=bold 
                         borderbottomcolor=black borderbottomwidth=1pt]
        style(column) = [just=left verticalalign=top font_size=9pt]
        ;

        columns &columns.;
        
        &defvars.

        %if &compute. ne %then %do;
            &compute.
        %end;

        %if &breakafter. ne %then %do;
            break after &breakafter. / page;
        %end;

    run;

%mend m_report;


/*****************************************************************************
* Macro:      m_report_listing
* Purpose:    Standard PROC REPORT wrapper optimized for listings
*****************************************************************************/

%macro m_report_listing(inds=, columns=, defvars=, orient=landscape, 
                         filename=, width=9.5in);

    options orientation=&orient. nodate nonumber;
    ods escapechar='^';

    proc report data=&inds. nowd split='~' 
        style(report) = [outputwidth=&width. font_size=8pt]
        style(header) = [just=center fontweight=bold font_size=8pt
                         borderbottomcolor=black borderbottomwidth=1pt]
        style(column) = [just=left verticalalign=top font_size=8pt
                         cellpadding=2pt]
        ;

        columns &columns.;
        &defvars.

    run;

%mend m_report_listing;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf/utilities/u_titles.sas ---
/*****************************************************************************
* Program:    u_titles.sas
* Purpose:    Centralized titles and footnotes management for all TLFs
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Notes:      Maintains consistency across all outputs
*****************************************************************************/

/*--- Master macro to set titles and footnotes ---*/
%macro set_titles(output=, population=Safety Analysis Set, pageof=Y);

    %let _popn = %upcase(&population.);

    /*--- Titles ---*/
    title1 j=l "&sponsor." j=r "Page ^{pageof}";
    title2 j=l "Protocol: &protocol.";
    title3 j=c " ";  /* spacer */

    /*=== TABLES ===*/

    /* Demographics */
    %if &output.=T_DM %then %do;
        title4 j=c "Table 14.1.1";
        title5 j=c "Summary of Demographics and Baseline Characteristics";
        title6 j=c "&population.";
    %end;

    /* Disposition */
    %if &output.=T_DISP %then %do;
        title4 j=c "Table 14.1.2";
        title5 j=c "Subject Disposition";
        title6 j=c "&population.";
    %end;

    /* AE Summary */
    %if &output.=T_AE_SUM %then %do;
        title4 j=c "Table 14.3.1";
        title5 j=c "Overall Summary of Treatment-Emergent Adverse Events";
        title6 j=c "&population.";
    %end;

    /* AE by SOC and PT */
    %if &output.=T_AE_SOC_PT %then %do;
        title4 j=c "Table 14.3.2";
        title5 j=c "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term";
        title6 j=c "&population.";
    %end;

    /* Concomitant Medications */
    %if &output.=T_CM %then %do;
        title4 j=c "Table 14.2.1";
        title5 j=c "Summary of Concomitant Medications by ATC Class and Preferred Name";
        title6 j=c "&population.";
    %end;

    /* Primary Efficacy */
    %if &output.=T_EFFICACY %then %do;
        title4 j=c "Table 14.2.1";
        title5 j=c "Analysis of Primary Efficacy Endpoint";
        title6 j=c "&population.";
    %end;

    /* Lab Shift */
    %if &output.=T_LB_SHIFT %then %do;
        title4 j=c "Table 14.3.4";
        title5 j=c "Shift Table for Laboratory Parameters";
        title6 j=c "&population.";
    %end;

    /* Vital Signs */
    %if &output.=T_VS %then %do;
        title4 j=c "Table 14.3.5";
        title5 j=c "Summary of Vital Signs by Visit";
        title6 j=c "&population.";
    %end;

    /* Exposure */
    %if &output.=T_EXPOSURE %then %do;
        title4 j=c "Table 14.1.3";
        title5 j=c "Summary of Study Drug Exposure";
        title6 j=c "&population.";
    %end;

    /* Medical History */
    %if &output.=T_MH %then %do;
        title4 j=c "Table 14.1.4";
        title5 j=c "Medical History by System Organ Class and Preferred Term";
        title6 j=c "&population.";
    %end;

    /*=== LISTINGS ===*/

    %if &output.=L_AE %then %do;
        title4 j=c "Listing 16.2.7.1";
        title5 j=c "Listing of Adverse Events";
        title6 j=c "&population.";
    %end;

    %if &output.=L_SAE %then %do;
        title4 j=c "Listing 16.2.7.2";
        title5 j=c "Listing of Serious Adverse Events";
        title6 j=c "&population.";
    %end;

    %if &output.=L_DEATH %then %do;
        title4 j=c "Listing 16.2.7.3";
        title5 j=c "Listing of Deaths";
        title6 j=c "All Subjects";
    %end;

    %if &output.=L_DV %then %do;
        title4 j=c "Listing 16.2.2";
        title5 j=c "Listing of Protocol Deviations";
        title6 j=c "All Subjects";
    %end;

    %if &output.=L_CM %then %do;
        title4 j=c "Listing 16.2.5";
        title5 j=c "Listing of Concomitant Medications";
        title6 j=c "&population.";
    %end;

    /*=== FIGURES ===*/

    %if &output.=F_KM %then %do;
        title4 j=c "Figure 14.2.1";
        title5 j=c "Kaplan-Meier Estimate of Overall Survival";
        title6 j=c "&population.";
    %end;

    %if &output.=F_FOREST %then %do;
        title4 j=c "Figure 14.2.2";
        title5 j=c "Forest Plot of Subgroup Analyses";
        title6 j=c "&population.";
    %end;

    %if &output.=F_WATERFALL %then %do;
        title4 j=c "Figure 14.2.3";
        title5 j=c "Waterfall Plot of Best Overall Response";
        title6 j=c "&population.";
    %end;

    %if &output.=F_MEAN_CFB %then %do;
        title4 j=c "Figure 14.2.4";
        title5 j=c "Mean Change from Baseline Over Time";
        title6 j=c "&population.";
    %end;

    /*--- Standard footnotes ---*/
    footnote1 j=l "Data cutoff: &cutdate.";
    footnote2 j=l "Source: ADaM datasets";
    footnote3 j=l "Program: &sysin. | Run: %sysfunc(date(), date9.) %sysfunc(time(), time5.)";

%mend set_titles;

/*--- Clear titles and footnotes ---*/
%macro clear_titles;
    title;
    footnote;
%mend clear_titles;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf/m_counts.sas ---
/*****************************************************************************
* Macro:      m_counts.sas
* Purpose:    Derive Big N and small n treatment group counts
* Author:     [Programmer Name]
* Date:       [Date]
* Parameters: inds     = Input ADaM dataset
*             trtvar   = Treatment variable (default: TRTA)
*             popflag  = Population flag variable (default: SAFFL)
*             popval   = Population flag value (default: Y)
*             subjvar  = Subject variable (default: USUBJID)
*             outds    = Output dataset with counts
* Notes:      Creates Big N header values and macro variables
*****************************************************************************/

%macro m_counts(inds=adam.adsl, trtvar=TRTA, popflag=SAFFL, popval=Y,
                subjvar=USUBJID, outds=work._bign);

    /*--- Get Big N for each treatment group ---*/
    proc sql noprint;
        create table &outds. as
        select &trtvar.,
               count(distinct &subjvar.) as bign format=8.
        from &inds.
        where upcase(&popflag.) = "%upcase(&popval.)"
        group by &trtvar.
        order by &trtvar.;
    quit;

    /*--- Create macro variables for Big N ---*/
    data _null_;
        set &outds.;
        call symputx(cats('bign', _n_), bign);
        call symputx(cats('trtlbl', _n_), &trtvar.);
        call symputx('ntrt', _n_);
    run;

    /*--- Create Total if not present ---*/
    proc sql noprint;
        select count(distinct &subjvar.) into :bigntot trimmed
        from &inds.
        where upcase(&popflag.) = "%upcase(&popval.)";
    quit;

    /*--- Build header strings ---*/
    %do i = 1 %to &ntrt.;
        %global col&i.hdr;
        %let col&i.hdr = &&trtlbl&i.~(N=&&bign&i.);
    %end;
    %global coltothdr;
    %let coltothdr = Total~(N=&bigntot.);

    %put NOTE: ============================================;
    %put NOTE: Treatment group counts:;
    %do i = 1 %to &ntrt.;
        %put NOTE:   &&trtlbl&i.: N=&&bign&i.;
    %end;
    %put NOTE:   Total: N=&bigntot.;
    %put NOTE: ============================================;

%mend m_counts;


/*****************************************************************************
* Macro:      m_smalln
* Purpose:    Derive small n (number of subjects with non-missing values)
*             for a specific analysis variable within each treatment group
* Parameters: inds     = Input dataset
*             trtvar   = Treatment variable
*             anavar   = Analysis variable to count non-missing
*             byvar    = Optional BY variable (e.g., PARAMCD, AVISIT)
*             subjvar  = Subject variable
*             outds    = Output dataset
*****************************************************************************/

%macro m_smalln(inds=, trtvar=TRTA, anavar=, byvar=, subjvar=USUBJID, outds=work._smalln);

    proc sql noprint;
        create table &outds. as
        select &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
               , count(distinct &subjvar.) as n format=8.
        from &inds.
        where &anavar. is not missing
        group by &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
        order by &trtvar.
               %if &byvar. ne %then %do; , &byvar. %end;
        ;
    quit;

%mend m_smalln;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P072_t_dm.sas) ---
/*****************************************************************************
* Program:    t_dm.sas
* Purpose:    Table 14.1.1 - Summary of Demographics and Baseline 
*             Characteristics
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Input:      adam.adsl
* Output:     t_dm.rtf
* Population: Safety Analysis Set (SAFFL='Y')
* Macros:     m_counts, m_desc_stats, m_report, u_titles
* Validation: Independent programming / double programming
*****************************************************************************/

%include "&pgmpath./utilities/u_setup.sas";
%include "&macpath./m_counts.sas";
%include "&macpath./m_desc_stats.sas";
%include "&macpath./m_report.sas";
%include "&pgmpath./utilities/u_titles.sas";

/*--- Step 1: Get population counts ---*/
%m_counts(inds=adam.adsl, trtvar=TRTA, popflag=SAFFL, popval=Y);

/*--- Step 2: Prepare analysis dataset ---*/
data dm;
    set adam.adsl;
    where SAFFL = 'Y';

    /* Derive age groups */
    length AGEGR1 $20;
    if      AGE < 18 then AGEGR1 = '<18';
    else if AGE < 65 then AGEGR1 = '18 to <65';
    else if AGE >= 65 then AGEGR1 = '>=65';

    /* Ensure consistent ordering */
    if SEX = 'M' then SEXN = 1;
    else if SEX = 'F' then SEXN = 2;

    if RACE = 'WHITE' then RACEN = 1;
    else if RACE = 'BLACK OR AFRICAN AMERICAN' then RACEN = 2;
    else if RACE = 'ASIAN' then RACEN = 3;
    else RACEN = 4;

    if ETHNIC = 'HISPANIC OR LATINO' then ETHNICN = 1;
    else if ETHNIC = 'NOT HISPANIC OR LATINO' then ETHNICN = 2;
    else ETHNICN = 3;
run;

/*--- Step 3: Continuous variables (Age, Weight, Height, BMI) ---*/
%macro cont_block(var=, label=, ord=, decimals=1);

    %m_cont_stats(inds=dm, anavar=&var., trtvar=TRTA, outds=_c_&var., 
                  decimals=&decimals.);

    data _c_&var.;
        set _c_&var.;
        length category $60;
        category = "&label.";
        block = &ord.;
    run;

%mend cont_block;

%cont_block(var=AGE, label=Age (years), ord=1);
%cont_block(var=WEIGHTBL, label=Weight (kg), ord=5);
%cont_block(var=HEIGHTBL, label=Height (cm), ord=6);
%cont_block(var=BMIBL, label=BMI (kg/m^{super 2}), ord=7);

/*--- Step 4: Categorical variables (Sex, Race, Ethnicity, Age Group) ---*/
%macro cat_block(var=, label=, ord=);

    %m_cat_stats(inds=dm, catvar=&var., trtvar=TRTA, outds=_f_&var.);

    data _f_&var.;
        set _f_&var.;
        length category $60 stat $40;
        category = "&label.";
        stat = &var.;
        block = &ord.;
    run;

%mend cat_block;

%cat_block(var=SEX, label=Sex, ord=2);
%cat_block(var=RACE, label=Race, ord=3);
%cat_block(var=ETHNIC, label=Ethnicity, ord=4);
%cat_block(var=AGEGR1, label=Age Group, ord=8);

/*--- Step 5: Combine all results ---*/
data all_stats;
    set _c_age _f_sex _f_race _f_ethnic
        _c_weightbl _c_heightbl _c_bmibl _f_agegr1;
run;

/*--- Step 6: Transpose by treatment for columnar display ---*/
proc sort data=all_stats; by block ord stat; run;

proc transpose data=all_stats out=final(drop=_name_ rename=(colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3)) prefix=col;  /* test-copy patch: name the transposed columns col1-col3 as the report expects */
    by block category ord stat;
    id TRTA;
    var value;
run;

/*--- Step 7: Create report dataset ---*/
data report;
    retain block category stat col1 col2 col3;
    set final;
    by block category;

    /* Display category label only on first row of block */
    length display_cat $60;
    if first.category then display_cat = category;
    else display_cat = ' ';

    /* Indent statistic labels */
    stat = '  ' || strip(stat);
run;

/*--- Step 8: Generate output ---*/
%set_titles(output=T_DM, population=Safety Analysis Set);
%ods_on(filename=t_dm);

proc report data=report nowd split='~'
    style(report)=[outputwidth=9.5in]
    style(header)=[just=center fontweight=bold];

    columns block display_cat stat col1 col2 col3;

    define block       / order noprint;
    define display_cat / display "Parameter" style(column)=[width=2.5in fontweight=bold];
    define stat        / display "Statistic" style(column)=[width=1.5in];
    define col1        / display "&col1hdr." style(column)=[width=1.5in just=center];
    define col2        / display "&col2hdr." style(column)=[width=1.5in just=center];
    define col3        / display "&col3hdr." style(column)=[width=1.5in just=center];

    compute after block;
        line ' ';
    endcomp;

run;

%ods_off;
%clear_titles;

/*--- Cleanup ---*/
proc datasets lib=work nolist;
    delete dm all_stats final report _c_: _f_: _bign _smalln;
quit;

%put NOTE: t_dm.sas completed successfully.;

--- end of SAS program ---

Write the complete R script now.