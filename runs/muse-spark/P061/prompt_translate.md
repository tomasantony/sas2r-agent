Translate the SAS program below to R.

Program id: P061   file: P061_oncology_sdtm_adam_tlf_analysis.sas   source: sumit-biostat/SAS-Oncology-SDTM-ADaM-TLFs   family: sumit
Declared output types: graph;table;listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P061
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P061

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P061.lst (table/listing text, 36884 numbers)  -> write a text rendering and CSV of the same table
  figure sasgraph.png  -> write OUT_DIR/sasgraph.png and a CSV of the plotted data
  figure SGPlot.png  -> write OUT_DIR/sgplot.png and a CSV of the plotted data
  figure SGPlot1.png  -> write OUT_DIR/sgplot1.png and a CSV of the plotted data
  figure SGPlot2.png  -> write OUT_DIR/sgplot2.png and a CSV of the plotted data
  figure SGPlot3.png  -> write OUT_DIR/sgplot3.png and a CSV of the plotted data
  figure SGPlot4.png  -> write OUT_DIR/sgplot4.png and a CSV of the plotted data
  figure SGPlot5.png  -> write OUT_DIR/sgplot5.png and a CSV of the plotted data
  figure SGPlot6.png  -> write OUT_DIR/sgplot6.png and a CSV of the plotted data
  P061_t_oncology_report.pdf (table/listing text, 9146 numbers)  -> write a text rendering and CSV of the same table
  P061_f_oncology_graphs.pdf (table/listing text, 9134 numbers)  -> write a text rendering and CSV of the same table

--- family environment (sumit) that ran before the program ---
/* no family setup needed */
options validvarname=any;  /* author ran under SAS Studio defaults; CSV headers have spaces */


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P061_oncology_sdtm_adam_tlf_analysis.sas) ---
/*-----------------------------------------------------------*
Project: Oncology Clinical Trial Analysis
Standard: CDISC SDTM / ADaM
Programming: SAS
Dataset: Breast Cancer METABRIC
Author: Sumit Choudhary

Description:
This project demonstrates clinical trial data preparation,
SDTM dataset creation, ADaM dataset derivation, and generation
of Tables, Listings, and Figures (TLFs) for oncology analysis.
*-----------------------------------------------------------*/




libname Cancer "&outds.";

/* Import Raw Data */

proc import datafile="&rawdir.\sumit-oncology\Breast Cancer METABRIC.csv"
out=raw_metabric
dbms=csv
replace;
getnames=yes;
run;

/* Create SDTM DM (Demographics) */

data sdtm_dm;
set raw_metabric;

STUDYID="ONC001";
DOMAIN="DM";

USUBJID=catx("-",STUDYID,patient_id);

AGE=age_at_diagnosis;

run;

/* Create ADaM Dataset */

data adam_adsl;
set sdtm_dm;

TRT01P="CHEMO";
SAFFL="Y";

run;

/* Validate ADaM Dataset */

proc contents data=adam_adsl;
run;

proc print data=adam_adsl(obs=10);
run;

/* Create Demographic Summary Table */

proc means data=adam_adsl n mean std min max;
var 'Age at Diagnosis'n;
title "Demographic Summary - Age";
run;

/* Gender Distribution Table */

proc freq data=adam_adsl;
tables sex;
title "Gender Distribution";
run;

/* Tumor Stage Table */

proc freq data=adam_adsl;
tables 'Tumor Stage'n;
title "Tumor Stage Distribution";
run;

/* Tumor Size Summary Table */

proc means data=adam_adsl n mean std min max;
var 'Tumor Size'n;
title "Tumor Size Summary";
run;

/* Survival Analysis Dataset */

data adam_adtte;
set adam_adsl;

AVAL='Overall Survival (Months)'n;

if 'Overall Survival Status'n="Deceased" then CNSR=0;
else CNSR=1;

run;

/* Kaplan-Meier Survival Curve */

proc lifetest data=adam_adtte plots=survival;
time AVAL*CNSR(1);
title "Kaplan-Meier Survival Curve";
run;

/* Treatment Analysis */

proc freq data=adam_adsl;
tables Chemotherapy;
title "Chemotherapy Treatment Summary";
run;

/* ER / HER2 Status Analysis */
/* Oncology me biomarker analysis important hota hai. */

proc freq data=adam_adsl;
tables 'ER Status'n 'HER2 Status'n;
run;



/* Create TLF (Tables Listings Figures) */

ods pdf file="&outtlf.\P061_t_oncology_report.pdf";

proc means data=adam_adsl;
var 'AGE at Diagnosis'n;
run;

proc freq data=adam_adsl;
tables sex;
run;

proc freq data=adam_adsl;
tables 'Tumor Stage'n;
run;

proc lifetest data=adam_adtte plots=survival;
time AVAL*CNSR(1);
run;

ods pdf close;

/* Kaplan-Meier Survival Graph (Figure 1) */

proc lifetest data=adam_adtte plots=survival;
time AVAL*CNSR(1);
title "Kaplan-Meier Survival Curve";
run;

/* Tumor Stage Bar Graph */

proc sgplot data=adam_adsl;
vbar 'Tumor Stage'n;
title "Tumor Stage Distribution";
run;

/* Age Distribution Histogram */

proc sgplot data=adam_adsl;
histogram 'AGE at Diagnosis'n;
density 'AGE at Diagnosis'n;
title "Age Distribution of Patients";
run;

/* Tumor Size vs Survival Scatter Plot */

proc sgplot data=adam_adsl;
scatter x='Tumor Size'n y='Overall Survival (Months)'n;
title "Tumor Size vs Survival";
run;

/* ER Status vs Survival Box Plot */

proc sgplot data=adam_adsl;
vbox 'Overall Survival (Months)'n / category='ER Status'n;
title "Survival by ER Status";
run;

/* Treatment Analysis Graph */

proc sgplot data=adam_adsl;
vbar Chemotherapy;
title "Chemotherapy Treatment Distribution";
run;

/* Export All Graphs in PDF */

ods pdf file="&outtlf.\P061_f_oncology_graphs.pdf";

proc sgplot data=adam_adsl;
histogram 'AGE at Diagnosis'n;
run;

proc sgplot data=adam_adsl;
vbar 'Tumor Stage'n;
run;

proc lifetest data=adam_adtte plots=survival;
time AVAL*CNSR(1);
run;

ods pdf close;




--- end of SAS program ---

Write the complete R script now.