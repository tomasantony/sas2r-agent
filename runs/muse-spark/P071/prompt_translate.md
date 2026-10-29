Translate the SAS program below to R.

Program id: P071   file: P071_t_disp.sas   source: own-sas2r/tlf   family: own-tlf
Declared output types: table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P071
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P071
  &pgmpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &macpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &ntrt. = 3

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
  sdtm -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  t_disp.rtf (table/listing text, 73 numbers)  -> write a text rendering and CSV of the same table

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P071_t_disp.sas) ---
/*****************************************************************************
* Program:    t_disp.sas
* Purpose:    Table 14.1.2 - Subject Disposition
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Input:      adam.adsl
* Output:     t_disp.rtf
* Population: All Randomized Subjects
*****************************************************************************/

%include "&pgmpath./utilities/u_setup.sas";
%include "&macpath./m_counts.sas";
%include "&macpath./m_report.sas";
%include "&pgmpath./utilities/u_titles.sas";

/*--- Step 1: Get Big N (all randomized) ---*/
%m_counts(inds=adam.adsl, trtvar=TRTA, popflag=RANDFL, popval=Y);

/*--- Step 2: Prepare data ---*/
data disp;
    set adam.adsl;
    where RANDFL = 'Y';
run;

/*--- Step 3: Population analysis sets ---*/
%macro pop_count(flag=, label=, ord=);
    proc sql noprint;
        create table _pop_&flag. as
        select TRTA, 
               count(distinct USUBJID) as cnt,
               "&label." as category length=80,
               &ord. as ord
        from disp
        where upcase(&flag.) = 'Y'
        group by TRTA;
    quit;
%mend pop_count;

%pop_count(flag=RANDFL,  label=Randomized,                    ord=1);
%pop_count(flag=ITTFL,   label=%str(  Intent-to-Treat Set),   ord=2);
%pop_count(flag=SAFFL,   label=%str(  Safety Analysis Set),   ord=3);
%pop_count(flag=EFFFL,   label=%str(  Efficacy Analysis Set), ord=4);
%pop_count(flag=PPROTFL, label=%str(  Per-Protocol Set),      ord=5);

/*--- Step 4: Completion / Discontinuation ---*/
proc sql noprint;
    /* Completed study */
    create table _completed as
    select TRTA,
           count(distinct USUBJID) as cnt,
           'Completed Study' as category length=80,
           10 as ord
    from disp
    where EOSSTT = 'COMPLETED'
    group by TRTA;

    /* Discontinued */
    create table _discont as
    select TRTA,
           count(distinct USUBJID) as cnt,
           'Discontinued Study' as category length=80,
           11 as ord
    from disp
    where EOSSTT = 'DISCONTINUED'
    group by TRTA;
quit;

/*--- Step 5: Reasons for discontinuation ---*/
proc sql noprint;
    create table _reasons as
    select TRTA,
           DCSREAS,
           count(distinct USUBJID) as cnt
    from disp
    where EOSSTT = 'DISCONTINUED' and DCSREAS ne ''
    group by TRTA, DCSREAS;
quit;

data _reasons2;
    set _reasons;
    length category $80;
    category = '  ' || strip(DCSREAS);
    
    /* Assign display order for reasons */
    select(upcase(DCSREAS));
        when('ADVERSE EVENT')          ord = 12;
        when('DEATH')                  ord = 13;
        when('LACK OF EFFICACY')       ord = 14;
        when('LOST TO FOLLOW-UP')      ord = 15;
        when('PHYSICIAN DECISION')     ord = 16;
        when('PROTOCOL VIOLATION')     ord = 17;
        when('WITHDRAWAL BY SUBJECT')  ord = 18;
        otherwise                      ord = 19;
    end;
run;

/*--- Step 6: Combine all sections ---*/
data all_disp;
    set _pop_randfl _pop_ittfl _pop_saffl _pop_efffl _pop_pprotfl
        _completed _discont _reasons2;
run;

/*--- Step 7: Add percentages and format ---*/
proc sql noprint;
    create table all_disp2 as
    select a.*,
           b.bign,
           case when b.bign > 0 
                then a.cnt / b.bign * 100 
                else . 
           end as pct
    from all_disp a
    left join work._bign b
    on a.TRTA = b.TRTA
    order by ord, TRTA;
quit;

data all_disp3;
    set all_disp2;
    length value $30;
    if ord = 1 then value = strip(put(cnt, 8.));  /* Just N for randomized */
    else if cnt > 0 then 
        value = cat(strip(put(cnt, 8.)), ' (', strip(put(pct, 5.1)), ')');
    else value = '0';
run;

/*--- Step 8: Transpose for columnar display ---*/
proc sort data=all_disp3; by ord category; run;

proc transpose data=all_disp3 out=final(drop=_name_ rename=(colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3)) prefix=col;  /* test-copy patch: name the transposed columns col1-col3 as the report expects */
    by ord category;
    id TRTA;
    var value;
run;

/*--- Step 9: Section headers ---*/
data report;
    length section $80;
    set final;
    by ord;

    if ord = 1 then section = 'Analysis Populations';
    else if ord = 10 then section = 'Study Completion';
    else if ord >= 12 then section = '  Reason for Discontinuation';
    else section = '';

    /* Show section header only once */
    retain _lastsect '';
    if section ne _lastsect and section ne '' then do;
        _show_section = section;
        _lastsect = section;
    end;
    else _show_section = '';
    drop _lastsect;
run;

/*--- Step 10: Generate output ---*/
%set_titles(output=T_DISP, population=All Randomized Subjects);
%ods_on(filename=t_disp);

proc report data=report nowd split='~'
    style(report)=[outputwidth=9.5in]
    style(header)=[just=center fontweight=bold];

    columns ord _show_section category col1 col2 col3;

    define ord            / order noprint;
    define _show_section  / display '' style(column)=[width=2in fontweight=bold] noprint;
    define category       / display 'Category' style(column)=[width=2.5in];
    define col1           / display "&col1hdr." style(column)=[width=1.5in just=center];
    define col2           / display "&col2hdr." style(column)=[width=1.5in just=center];
    define col3           / display "&col3hdr." style(column)=[width=1.5in just=center];

run;

%ods_off;
%clear_titles;

proc datasets lib=work nolist;
    delete disp all_disp: final report _pop_: _completed _discont _reasons:;
quit;

%put NOTE: t_disp.sas completed successfully.;

--- end of SAS program ---

Write the complete R script now.