Translate the SAS program below to R.

Program id: P080   file: P080_f_forest.sas   source: own-sas2r/tlf   family: own-tlf
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P080
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P080
  &pgmpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &macpath. = D:/meta_hackathon/sas2r-agent/programs/sas/macros/own-tlf
  &ntrt. = 3

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam
  sdtm -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  f_forest.rtf (table/listing text, 3 numbers)  -> write a text rendering and CSV of the same table

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


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P080_f_forest.sas) ---
%macro yvals; %do r = 1 %to &sysnobs.; -&r. %end; %mend yvals;  /* test-copy patch, see tlf_test_patches.md */
/*****************************************************************************
* Program:    f_forest.sas
* Purpose:    Figure 14.2.2 - Forest Plot of Subgroup Analyses
* Study:      [STUDYID]
* Author:     [Programmer Name]
* Date:       [Date]
* Input:      adam.adtte (or adam.adeff), adam.adsl
* Output:     f_forest.rtf
* Population: Efficacy Analysis Set
* Notes:      Hazard Ratios (or Odds Ratios) with 95% CI by subgroup
*****************************************************************************/

%include "&pgmpath./utilities/u_setup.sas";
%include "&pgmpath./utilities/u_titles.sas";

/*--- Step 1: Define subgroups ---*/
/* Each subgroup analysis is done separately, then combined */

%macro subgroup_analysis(data=, subgrp=, subgrp_label=, paramcd=OS, 
                          subgrp_ord=, levels=);

    proc sort data=&data. out=_sg; by &subgrp.; run;

    /* Run Cox model within each subgroup level */
    ods output ParameterEstimates = _pe_&subgrp_ord.;
    
    proc phreg data=_sg;
        by &subgrp.;
        class TRTA(ref="&trt1lbl.");
        model AVAL * CNSR(1) = TRTA;
        hazardratio TRTA;
    run;

    ods output close;

    /* Format results */
    data _sg_&subgrp_ord.;
        set _pe_&subgrp_ord.;
        where Parameter = 'TRTA';
        
        length subgroup $60 subgroup_level $60;
        subgroup = "&subgrp_label.";
        subgroup_level = strip(vvalue(&subgrp.));
        subgrp_ord = &subgrp_ord.;
        
        HR = exp(Estimate);
        HR_LCL = exp(Estimate - 1.96 * StdErr);
        HR_UCL = exp(Estimate + 1.96 * StdErr);
        
        /* Display strings */
        length hr_display $30 ci_display $40;
        hr_display = strip(put(HR, 5.2));
        ci_display = cat('(', strip(put(HR_LCL, 5.2)), ', ', 
                              strip(put(HR_UCL, 5.2)), ')');
        
        keep subgroup subgroup_level subgrp_ord HR HR_LCL HR_UCL 
             hr_display ci_display ProbChiSq;
    run;

%mend subgroup_analysis;

/*--- Step 2: Prepare data ---*/
proc sql noprint;
    create table eff as
    select a.USUBJID, a.TRTA, a.TRTAN, a.AVAL, a.CNSR,
           b.SEX, b.AGEGR1, b.RACE, b.REGION1, b.ECOGBL
    from adam.adtte a
    inner join adam.adsl b on a.USUBJID = b.USUBJID
    where b.EFFFL = 'Y' and a.PARAMCD = 'OS';
quit;

/*--- Step 3: Run subgroup analyses ---*/
%subgroup_analysis(data=eff, subgrp=SEX, subgrp_label=Sex, subgrp_ord=1);
%subgroup_analysis(data=eff, subgrp=AGEGR1, subgrp_label=Age Group, subgrp_ord=2);
%subgroup_analysis(data=eff, subgrp=RACE, subgrp_label=Race, subgrp_ord=3);
%subgroup_analysis(data=eff, subgrp=REGION1, subgrp_label=Region, subgrp_ord=4);

/*--- Step 4: Overall HR ---*/
ods output ParameterEstimates = _pe_overall;
proc phreg data=eff;
    class TRTA(ref="&trt1lbl.");
    model AVAL * CNSR(1) = TRTA;
run;
ods output close;

data _sg_overall;
    set _pe_overall;
    where Parameter = 'TRTA';
    length subgroup $60 subgroup_level $60;
    subgroup = 'Overall';
    subgroup_level = 'All Subjects';
    subgrp_ord = 0;
    HR = exp(Estimate);
    HR_LCL = exp(Estimate - 1.96 * StdErr);
    HR_UCL = exp(Estimate + 1.96 * StdErr);
    length hr_display $30 ci_display $40;
    hr_display = strip(put(HR, 5.2));
    ci_display = cat('(', strip(put(HR_LCL, 5.2)), ', ', 
                          strip(put(HR_UCL, 5.2)), ')');
    keep subgroup subgroup_level subgrp_ord HR HR_LCL HR_UCL 
         hr_display ci_display ProbChiSq;
run;

/*--- Step 5: Combine all subgroups ---*/
data forest_data;
    set _sg_overall _sg_1 _sg_2 _sg_3 _sg_4;
    
    /* Row ordering */
    row = _n_;
    yval = -_n_;  /* For plotting top-to-bottom */
run;

/*--- Step 6: Generate forest plot ---*/
%set_titles(output=F_FOREST, population=Efficacy Analysis Set);

options orientation=landscape; ods rtf file="&tlfpath./f_forest.rtf" style=journal bodytitle;
ods graphics on / reset width=10in height=7in imagefmt=emf;

proc sgplot data=forest_data noautolegend;
    title4 j=c "Figure 14.2.2";
    title5 j=c "Forest Plot of Subgroup Analyses - Overall Survival";

    /* Reference line at HR=1 */
    refline 1 / axis=x lineattrs=(pattern=dash color=gray);

    /* CI lines */
    highlow y=yval low=HR_LCL high=HR_UCL / 
        lineattrs=(thickness=2px color=darkblue);

    /* Point estimates */
    scatter y=yval x=HR / 
        markerattrs=(symbol=diamondfilled size=10px color=darkblue);

    /* Subgroup labels on y-axis */
    yaxis type=discrete 
          values=(%yvals)  /* test-copy patch: open-code %DO is not allowed, loop moved into macro %yvals */
          display=(nolabel)
          fitpolicy=none;

    xaxis label="Hazard Ratio (95% CI)" 
          type=log logbase=2
          values=(0.1 0.25 0.5 1 2 4)
          grid;

    /* Favors labels */
    inset ("Favors &trt2lbl." = " ") / 
        position=bottomleft textattrs=(size=8pt);
    inset ("Favors &trt1lbl." = " ") / 
        position=bottomright textattrs=(size=8pt);

    footnote1 j=l "Data cutoff: &cutdate.";
    footnote2 j=l "Hazard Ratios from Cox proportional hazards model.";
    footnote3 j=l "Program: &sysin. | Run: %sysfunc(date(), date9.)";
run;

ods graphics off;
ods rtf close;

%clear_titles;

proc datasets lib=work nolist;
    delete eff _sg _sg_: _pe_: forest_data;
quit;

%put NOTE: f_forest.sas completed successfully.;

--- end of SAS program ---

Write the complete R script now.