Translate the SAS program below to R.

Program id: P058   file: P058_pk_overlay_conc.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: graph;table;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P058
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P058

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  figure SGRender.png  -> write OUT_DIR/sgrender.png and a CSV of the plotted data
  figure SGRender1.png  -> write OUT_DIR/sgrender1.png and a CSV of the plotted data
  figure SGRender2.png  -> write OUT_DIR/sgrender2.png and a CSV of the plotted data
  figure SGRender3.png  -> write OUT_DIR/sgrender3.png and a CSV of the plotted data
  figure SGRender4.png  -> write OUT_DIR/sgrender4.png and a CSV of the plotted data
  figure SGRender5.png  -> write OUT_DIR/sgrender5.png and a CSV of the plotted data

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P058_pk_overlay_conc.sas) ---
******************************************;
* Program name: P_ADPC.sas               *;
*                                        *;
* Description:  Plot of PK Conc          *;
*               (Target 11)              *;
*                                        *;
* Author:       John Salter              *;
*               12OCT2014                *;
******************************************;

%* modification 2019-12-23 - update path as data has been moved;

filename source "&rawadam./adpc.xpt";
libname source xport ;

* Bring in specified data;
data conc;
 set source.adpc (keep=usubjid trta aperiodc atpt parcat3l aval adtm saffn pkfn paexcfln pclloq parcat1);
 t=input(compress(atpt,' HOURSPOSTDOSE'),best.);
 concen=aval;
 order=input(compress(parcat1,' DRUGANALPlasma'),best.);
 label t='Time'
       concen='Concentration (ug/mL)';
 * Apply LLOQ;
 if pclloq ne . then concen=pclloq;
run;

proc sort data=conc;
*  by order trta usubjid t;
  by usubjid t;
run;



* END DATA MANIPULATION *;
ods graphics on /width=24cm height=12cm noborder ; 
ods listing image_dpi=300 style=listing; 

proc template;
define statgraph pkplot;
  begingraph;
   layout lattice / columns=2 columnweights=(.50 .50);
   /* LINEAR PLOT  */
   layout OVERLAY / walldisplay=none border=false 
     	           halign=center;
     entry halign=center "Linear view" /  location=outside valign=top;      
     seriesplot x=t y=concen/group=usubjid lineattrs=(color=black pattern=1);
   endlayout;
   /* LOG-LINEAR PLOT */
   layout OVERLAY / walldisplay=none border=false 
     	           halign=center
     yaxisopts=(type=log logopts=(base=10 minorticks=TRUE) label=(' ')   );
     entry halign=center "Semilogarithmic view" /  location=outside valign=top;      
     seriesplot x=t y=concen/group=usubjid lineattrs=(color=black pattern=1);
   endlayout;
  endlayout;
endgraph;
end;
run;	

%macro plot(trta=);

%do i=1 %to 3;

title1 "Overlaid PK Concentration Plots";
title2 "Analyte: &I; Treatment=&TRTA";
proc sgrender data=conc template=PKPlot;
  where order=&I and trta="&TRTA";
run; 

%end;

%mend;
%plot(trta=A);
%plot(trta=C);

ods graphics off;

--- end of SAS program ---

Write the complete R script now.