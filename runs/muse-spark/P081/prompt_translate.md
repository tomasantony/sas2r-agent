Translate the SAS program below to R.

Program id: P081   file: P081_f_mean_cfb.sas   source: own-sas2r/tlf   family: own-tlf
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P081
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P081
  &pgmpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &macpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &ntrt. = 3

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
  sdtm -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  f_mean_cfb.rtf (table/listing text, 3 numbers)  -> write a text rendering and CSV of the same table

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P081_f_mean_cfb.sas) ---
/*****************************************************************************
* Program:    f_mean_cfb.sas
* Purpose:    Figure 14.2.4 - Mean Change from Baseline Over Time
*             (with error bars showing SE or 95% CI)
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Input:      adam.adeff (or adam.adlb, adam.advs, adam.adqs)
* Output:     f_mean_cfb.rtf
* Population: Efficacy Analysis Set (EFFFL='Y')
*****************************************************************************/

%include "&pgmpath./utilities/u_setup.sas";
%include "&pgmpath./utilities/u_titles.sas";

/*--- Parameters ---*/
%let eff_param = ACTOT;   /* test copy: placeholder filled with the synthetic efficacy parameter */    /* Analysis parameter */
%let y_label   = Change from Baseline in ADAS-Cog Total Score;

/*--- Step 1: Prepare analysis data ---*/
proc sql noprint;
    create table eff as
    select a.USUBJID, a.TRTA, a.TRTAN,
           a.PARAMCD, a.PARAM,
           a.AVISIT, a.AVISITN,
           a.AVAL, a.BASE, a.CHG,
           a.ANL01FL
    from adam.adeff a
    inner join adam.adsl b on a.USUBJID = b.USUBJID
    where b.EFFFL = 'Y'
          and a.PARAMCD = "&eff_param."
          and a.ANL01FL = 'Y'
          and a.CHG ne .
    order by a.TRTAN, a.AVISITN;
quit;

/*--- Step 2: Compute summary statistics by visit and treatment ---*/
proc means data=eff nway noprint;
    class TRTA TRTAN AVISIT AVISITN;
    var CHG;
    output out=stats(drop=_type_ _freq_)
        n=n mean=mean stderr=se 
        lclm=lcl uclm=ucl;
run;

/*--- Step 3: Small offset to prevent overlapping points ---*/
data plot_data;
    set stats;
    /* Offset x-position slightly by treatment for readability */
    select(TRTAN);
        when(1) xpos = AVISITN - 0.15;
        when(2) xpos = AVISITN;
        when(3) xpos = AVISITN + 0.15;
        otherwise xpos = AVISITN;
    end;
run;

/*--- Step 4: Generate figure ---*/
%set_titles(output=F_MEAN_CFB, population=Efficacy Analysis Set);

options orientation=landscape; ods rtf file="&tlfpath./f_mean_cfb.rtf" style=journal bodytitle;
ods graphics on / reset width=10in height=6.5in imagefmt=emf;

proc sgplot data=plot_data;
    title4 j=c "Figure 14.2.4";
    title5 j=c "Mean Change from Baseline Over Time";
    title6 j=c "Efficacy Analysis Set";

    /* Error bars (95% CI) */
    highlow x=xpos low=lcl high=ucl / group=TRTA
        lineattrs=(thickness=1.5px)
        type=bar barwidth=0.05;

    /* Mean values connected by lines */
    series x=xpos y=mean / group=TRTA
        lineattrs=(thickness=2px) name="trt"
        markers markerattrs=(size=8px symbol=circlefilled);

    /* Reference line at 0 */
    refline 0 / axis=y lineattrs=(pattern=dash color=gray thickness=1px);

    /* Axes */
    xaxis label="Visit (Week)" 
          type=linear
          integer;
    yaxis label="&y_label." 
          grid gridattrs=(pattern=dot color=lightgray);

    keylegend "trt" / location=outside position=bottom across=3;

    footnote1 j=l "Data cutoff: &cutdate.";
    footnote2 j=l "Error bars represent 95% confidence intervals.";
    footnote3 j=l "n = number of subjects with non-missing change from baseline.";
    footnote4 j=l "Program: &sysin. | Run: %sysfunc(date(), date9.)";
run;

ods graphics off;
ods rtf close;
%clear_titles;

/*--- Step 5: Supporting data table (n per visit per treatment) ---*/
proc tabulate data=plot_data out=_ntable;
    class TRTA AVISIT AVISITN;
    var n;
    table TRTA, AVISIT * n * sum / box='n per visit';
run;

proc datasets lib=work nolist;
    delete eff stats plot_data _ntable;
quit;

%put NOTE: f_mean_cfb.sas completed successfully.;

--- end of SAS program ---

Write the complete R script now.