Translate the SAS program below to R.

Program id: P046   file: P046_Box_Plot_Baseline.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: graph;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P046
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P046

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P046.lst (table/listing text, 29 numbers)  -> write a text rendering and CSV of the same table
  figure SGPlot.png  -> write OUT_DIR/sgplot.png and a CSV of the plotted data
  figure SGPlot1.png  -> write OUT_DIR/sgplot1.png and a CSV of the plotted data

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P046_Box_Plot_Baseline.sas) ---
%* modification 2019-12-23 - update path as data has been moved;

filename source "&rawadam./advs.xpt";
libname source xport;
data advs;
	keep USUBJID SAFFL PARAMCD TRTPN TRTP AVAL AVISIT AVISITN ATPT;
	set source.advs;
	where SAFFL='Y' and PARAMCD in ('DIABP', 'PULSE') 
		and ATPT='AFTER LYING DOWN FOR 5 MINUTES' and AVISITN=99;
run;
data advs1;
	set advs;
	if PARAMCD = 'DIABP' then PARAMCD_NUM=0;
	else PARAMCD_NUM=1;
run;
/*fitting a linear model with DIABP and PULSE
	I'm not quite sure the ourcome and independent variables are, so I will pick what I 
	think could be interesting y=AVAL x=PARAMCD and focus more on the code*/
proc sgplot data=advs1;
	scatter y=AVAL x=PARAMCD_NUM;
run;
proc reg data=advs1;
	model AVAL=PARAMCD_NUM;
	output out=regout p=fitted_value r=residuals j;
run;
proc sgplot data=regout;
	scatter x=fitted_value y=residuals;
	loess x=fitted_value y=residuals;
	refline 0;
run;

--- end of SAS program ---

Write the complete R script now.