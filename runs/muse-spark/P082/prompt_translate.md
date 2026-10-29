Translate the SAS program below to R.

Program id: P082   file: P082_f_waterfall.sas   source: own-sas2r/tlf   family: own-tlf
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P082
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P082
  &pgmpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &macpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &ntrt. = 3

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
  sdtm -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  f_waterfall.rtf (table/listing text, 3 numbers)  -> write a text rendering and CSV of the same table

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P082_f_waterfall.sas) ---
/*****************************************************************************
* Program:    f_waterfall.sas
* Purpose:    Figure 14.2.3 - Waterfall Plot of Best Percentage Change
*             from Baseline in Target Lesion Size
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Input:      adam.adtr (or adam.adrs), adam.adsl
* Output:     f_waterfall.rtf
* Population: Efficacy Analysis Set (EFFFL='Y')
* Notes:      Typically used in oncology trials (RECIST criteria)
*****************************************************************************/

%include "&pgmpath./utilities/u_setup.sas";
%include "&pgmpath./utilities/u_titles.sas";

/*--- Step 1: Get best percentage change per subject ---*/
proc sql noprint;
    create table tumor as
    select a.USUBJID, a.TRTA, a.TRTAN,
           a.PARAMCD, a.PARAM,
           a.PCHG,        /* Percent change from baseline */
           a.AVALC,       /* Best overall response category */
           b.EFFFL
    from adam.adtr a      /* Tumor response dataset */
    inner join adam.adsl b on a.USUBJID = b.USUBJID
    where b.EFFFL = 'Y'
          and a.PARAMCD = 'BPCHG'  /* Best percent change */
    order by a.PCHG;
quit;

/* Alternative: Derive from ADRS if ADTR not available */
/*
proc sql noprint;
    create table tumor as
    select a.USUBJID, a.TRTA, a.TRTAN,
           min(a.PCHG) as PCHG,
           b.AVALC as BOR
    from adam.adrs a
    inner join adam.adsl b on a.USUBJID = b.USUBJID
    where b.EFFFL = 'Y' and a.PARAMCD = 'SUMLD'
    group by a.USUBJID, a.TRTA, a.TRTAN, b.AVALC;
quit;
*/

/*--- Step 2: Assign display order (sorted by percent change) ---*/
data waterfall;
    set tumor;
    by PCHG;
    bar_order = _n_;
    
    /* Color by response category */
    length bar_color $20;
    select(upcase(AVALC));
        when('CR') bar_color = 'darkgreen';
        when('PR') bar_color = 'green';
        when('SD') bar_color = 'gold';
        when('PD') bar_color = 'red';
        otherwise  bar_color = 'gray';
    end;
    
    /* Cap at reasonable bounds */
    if PCHG > 100 then PCHG_display = 100;
    else if PCHG < -100 then PCHG_display = -100;
    else PCHG_display = PCHG;
run;

/*--- Step 3: Generate waterfall plot ---*/
%set_titles(output=F_WATERFALL, population=Efficacy Analysis Set);

options orientation=landscape; ods rtf file="&tlfpath./f_waterfall.rtf" style=journal bodytitle;
ods graphics on / reset width=10in height=6.5in imagefmt=emf;

proc sgplot data=waterfall;
    title4 j=c "Figure 14.2.3";
    title5 j=c "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions";

    /* Bars colored by best overall response */
    vbar bar_order / response=PCHG_display 
        group=AVALC
        groupdisplay=cluster
        barwidth=1
        outline outlineattrs=(color=black thickness=0.5px);

    /* Reference lines for RECIST thresholds */
    refline -30 / axis=y lineattrs=(pattern=dash color=blue thickness=1px)
        label='-30% (PR threshold)' labelattrs=(size=7pt);
    refline 20 / axis=y lineattrs=(pattern=dash color=red thickness=1px)
        label='+20% (PD threshold)' labelattrs=(size=7pt);
    refline 0 / axis=y lineattrs=(color=black thickness=1px);

    xaxis display=none label=' ';
    yaxis label="Best Percentage Change from Baseline (%)"
          values=(-100 to 100 by 20)
          grid gridattrs=(pattern=dot color=lightgray);

    keylegend / title='Best Overall Response' 
        location=outside position=bottom across=5;

    footnote1 j=l "Data cutoff: &cutdate.";
    footnote2 j=l "Each bar represents one subject.";
    footnote3 j=l "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease";
    footnote4 j=l "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)";
    footnote5 j=l "Program: &sysin. | Run: %sysfunc(date(), date9.)";
run;

ods graphics off;
ods rtf close;
%clear_titles;

proc datasets lib=work nolist; delete tumor waterfall; quit;

%put NOTE: f_waterfall.sas completed successfully.;

--- end of SAS program ---

Write the complete R script now.