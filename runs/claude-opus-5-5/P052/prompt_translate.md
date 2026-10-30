Translate the SAS program below to R.

Program id: P052   file: P052_outliers_Scatter_MeanTime.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: graph;dataset

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P052
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P052

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P052.lst (table/listing text, 4 numbers)  -> write a text rendering and CSV of the same table
  figure SGRender.png  -> write OUT_DIR/sgrender.png and a CSV of the plotted data

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P052_outliers_Scatter_MeanTime.sas) ---
/************************************/
/* Author:	Kriss Harris			*/
/* Date:    12 October 2014			*/
/* Target:	18						*/
/************************************/

%* modification 2019-12-23 - update path as data has been moved;

filename source "&rawadam./adsl.xpt";
libname source xport ;
 data work.adsl ;
   set source.adsl ;
 run ;


filename source "&rawadam./advsmax.xpt" ;
libname source xport ;

  data advsmax ;
   set source.advsmax ;
 run ;

 /* Selecting appropriate data */

proc sort data = advsmax;
by studyid;
run;

data advsmax_final;
set advsmax;
by studyid;

/* Creating labels for reference lines */
WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL ="Y" and ADY > 1 and SAFFL="Y";
if first.studyid then do;
	LLN_label = "LLN";
	ULN_label = "ULN";
end;
run;

/* Creating ranges so that axis can be equated */
proc sql;
create table range as
select min(base) as min_base, max(base) as max_base, min(aval) as min_aval, max(aval) as max_aval 
from advsmax_final;
quit;

data range2;
set range;
min = min(min_base, min_aval);
max = max(max_base, max_aval);
run;

proc sql;
select min(min), max(max) into : min, : max
	from range2;
quit; 

/* Finding out the amount of studyids */

proc sql;
create table advsmax_final2 as
select *, count(distinct studyid) as count_study
from advsmax_final;
quit;

data advsmax_final3;
set advsmax_final2;
if count_study <=2 then do;
	column_numbers = 2 ;
	row_numbers = 1;
end;
if count_study >2 then do;
	column_numbers = 2 ;
	row_numbers = 2;
end;
run;

proc sql;
select distinct column_numbers, row_numbers into: column_numbers, :row_numbers
from advsmax_final3;
run;

/* Creating template */

proc template;
define statgraph shiftplot;
nmvar min max row_numbers column_numbers;
	begingraph;

		layout datapanel classvars = (studyid) / order = rowmajor rows = row_numbers columns = column_numbers
												 rowaxisopts = (label = "Maximum Post-baseline Measurment" linearopts = (integer = true viewmin = min viewmax = max)) 
												 columnaxisopts = (label = "Maximum Baseline Measurment" linearopts = (integer = true viewmin = min viewmax = max));

			layout prototype;
				scatterplot x = base y = aval / group = trta name = "legend";
				lineparm x = 0 y = 0 slope = 1;
				referenceline x = ANRLO / lineattrs = (pattern = 2) xaxis = x curvelabel = LLN_label curvelabelposition = min;
				referenceline x = ANRHI / lineattrs = (pattern = 2) xaxis = x curvelabel = ULN_label curvelabelposition = min;

				referenceline y = ANRLO / lineattrs = (pattern = 2) yaxis = y curvelabel = LLN_label curvelabelposition = min;
				referenceline y = ANRHI / lineattrs = (pattern = 2) yaxis = y curvelabel = ULN_label curvelabelposition = min;

			endlayout;
			sidebar / align=bottom;
    			discretelegend "legend";
  			endsidebar;
		endlayout;

	endgraph;
end;
run;

proc sgrender data = advsmax_final3 template = shiftplot;
run;




/*



proc template;
define statgraph shiftplot2;
	begingraph;
			layout overlayequated /equatetype = square;
				scatterplot x = base y = aval / group = trta name = "legend";
				discretelegend "legend";
			endlayout;
	endgraph;
end;
run;

proc sgrender data = advsmax_final template = shiftplot2;
by studyid;
run;


*/





 

--- end of SAS program ---

Write the complete R script now.