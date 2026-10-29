Translate the SAS program below to R.

Program id: P077   file: P077_l_death.sas   source: own-sas2r/tlf   family: own-tlf
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P077
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P077
  &pgmpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &macpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &ntrt. = 3

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
  sdtm -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  l_death.rtf (table/listing text, 119 numbers)  -> write a text rendering and CSV of the same table

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P077_l_death.sas) ---
/*****************************************************************************
* Program:    l_death.sas
* Purpose:    Listing 16.2.7.3 - Listing of Deaths
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Input:      adam.adsl, adam.adae
* Output:     l_death.rtf
* Population: All Subjects
*****************************************************************************/

%include "&pgmpath./utilities/u_setup.sas";
%include "&macpath./m_report.sas";
%include "&pgmpath./utilities/u_titles.sas";

/*--- Step 1: Identify deaths ---*/
proc sql noprint;
    create table deaths as
    select a.USUBJID,
           a.TRTA, a.AGE, a.SEX, a.RACE,
           a.TRTSDT format=date9. as TRT_START,
           a.TRTEDT format=date9. as TRT_END,
           a.DTHDT format=date9. as DEATH_DATE,
           a.DTHCAUS as CAUSE_OF_DEATH,
           /* Days from last dose to death */
           case when a.DTHDT ne . and a.TRTEDT ne .
                then a.DTHDT - a.TRTEDT
                else .
           end as DAYS_LASTDOSE_DEATH,
           /* Days from first dose to death */
           case when a.DTHDT ne . and a.TRTSDT ne .
                then a.DTHDT - a.TRTSDT + 1
                else .
           end as DAYS_FIRSTDOSE_DEATH
    from adam.adsl a
    where a.DTHFL = 'Y'
    order by a.TRTA, a.USUBJID;
quit;

/*--- Step 2: Add fatal AE information ---*/
proc sql noprint;
    create table death_ae as
    select distinct USUBJID,
           AEBODSYS as FATAL_AE_SOC,
           AEDECOD as FATAL_AE_PT,
           AEREL as FATAL_AE_REL
    from adam.adae
    where AEOUT = 'FATAL'
    order by USUBJID;
quit;

proc sql noprint;
    create table death_report as
    select a.*,
           b.FATAL_AE_SOC, b.FATAL_AE_PT, b.FATAL_AE_REL
    from deaths a
    left join death_ae b on a.USUBJID = b.USUBJID
    order by a.TRTA, a.USUBJID;
quit;

/*--- Step 3: Format display ---*/
data death_final;
    set death_report;
    length demog $40 days_display $20;
    
    demog = catx('/', strip(put(AGE, best.)), strip(SEX), strip(substr(RACE, 1, 1)));
    
    if DAYS_LASTDOSE_DEATH ne . then 
        days_display = strip(put(DAYS_LASTDOSE_DEATH, 8.));
    else days_display = 'N/A';
run;

/*--- Step 4: Generate listing ---*/
%set_titles(output=L_DEATH, population=All Subjects);
%ods_on(filename=l_death);

proc report data=death_final nowd split='~'
    style(report)=[outputwidth=9.5in font_size=8pt]
    style(header)=[just=center fontweight=bold font_size=8pt]
    style(column)=[just=left verticalalign=top font_size=8pt];

    columns TRTA USUBJID demog TRT_START TRT_END DEATH_DATE
            days_display CAUSE_OF_DEATH FATAL_AE_SOC FATAL_AE_PT FATAL_AE_REL;

    define TRTA           / order 'Treatment' style(column)=[width=0.8in];
    define USUBJID        / display 'Subject~ID' style(column)=[width=0.8in];
    define demog          / display 'Age/Sex/~Race' style(column)=[width=0.5in];
    define TRT_START      / display 'First~Dose' style(column)=[width=0.7in];
    define TRT_END        / display 'Last~Dose' style(column)=[width=0.7in];
    define DEATH_DATE     / display 'Death~Date' style(column)=[width=0.7in];
    define days_display   / display 'Days Last~Dose to~Death' style(column)=[width=0.6in just=center];
    define CAUSE_OF_DEATH / display 'Cause of~Death' style(column)=[width=1.2in];
    define FATAL_AE_SOC   / display 'Fatal AE~SOC' style(column)=[width=1.2in];
    define FATAL_AE_PT    / display 'Fatal AE~PT' style(column)=[width=1.0in];
    define FATAL_AE_REL   / display 'Relationship' style(column)=[width=0.8in];

run;

%ods_off;
%clear_titles;

proc datasets lib=work nolist; delete deaths death_ae death_report death_final; quit;

%put NOTE: l_death.sas completed successfully.;

--- end of SAS program ---

Write the complete R script now.