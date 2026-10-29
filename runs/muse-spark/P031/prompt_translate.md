Translate the SAS program below to R.

Program id: P031   file: P031_Boxplot_group.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P031
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P031

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P031_f_plsr_box_plot.rtf (table/listing text, 3 numbers)  -> write a text rendering and CSV of the same table

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P031_Boxplot_group.sas) ---
DATA X;
LENGTH TRT $10.;
DO TRT="Study Drug","Placebo","AC";

DO J= 1 TO 10;

SUBJID=J+100*(TRT="Study Drug")+200*(TRT="Placebo")+300*(TRT="AC");

DO VISIT=3 TO 6;

X=ROUND(J+2*NORMAL(777),0.1);
OUTPUT;
END;
END;
END;
DROP J;
RUN;


DATA XX;
SET X;

IF TRT="Study Drug" THEN VIS=VISIT;
ELSE IF TRT="Placebo" THEN VIS=VISIT+0.15;
ELSE IF TRT="AC" THEN VIS=VISIT+0.30;

IF VISIT IN (4,5) THEN VIS=VIS-0.15;

IF VISIT=6 THEN VIS=VIS-0.3;
RUN;


PROC SORT DATA=XX;
BY VIS;
RUN;

/*
 Create the annotate dataset which will be supplemented to PROC BOXPLOT for annotation of mean values on to the plot 
data final_anno(keep=xsys ysys function size text angle style position y x trtcd);
set desc_stats;
length function $8;
retain xsys ysys '2' position "8" angle 0
function 'label' size 1 style 'simplex'
hsys '3';
x = week; y = mean;
text = compress(left(put(mean,4.2)));
run;
symbol1 v=dot h=10 c=purple ;
proc boxplot data=lb_rep;
plot lbstresn*week / ANNOTATE = final_anno

*/

ODS LISTING CLOSE;

ODS RTF FILE="&outtlf.\P031_f_plsr_box_plot.rtf";

GOPTIONS RESET=ALL HSIZE=9 IN VSIZE=7 IN DEV=SASEMF;


SYMBOL1 V=PLUS C=BLACK;
SYMBOL2 V=TRIANGLE C=BLUE;
SYMBOL3 V=SQUARE C=RED;


AXIS LABEL=("Visit") ORDER=(3 TO 6 BY 1) MAJOR=NONE MINOR=NONE OFFSET=(2,2);

TITLE1 "Box Plot of X over Time by Treatment";


PROC BOXPLOT DATA=XX;

FORMAT VIS 4.;
PLOT X*vis=TRT/ /*BOXCONNECT=MEAN*/  HAXIS=AXIS1 BOXSTYLE=SCHEMATICID CBOX=BLUE 
             IDSYMBOL=STAR IDCOLOR=RED 
             ;
RUN;
QUIT;

ODS RTF CLOSE;
ODS LISTING;


--- end of SAS program ---

Write the complete R script now.