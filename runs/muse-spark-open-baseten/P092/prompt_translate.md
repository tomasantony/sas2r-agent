Translate the SAS program below to R.

Program id: P092   file: P092_sct1.sas   source: general/sascommunities/graphics-programming   family: general
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P092
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P092

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  sct1.htm (table/listing text, 0 numbers)  -> write a text rendering and CSV of the same table
  figure sct1.png  -> write OUT_DIR/sct1.png and a CSV of the plotted data

--- family environment (general) that ran before the program ---
/* no family setup needed */


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P092_sct1.sas) ---
%let name=sct1;

/* 
Set your current-working-directory (to read/write files), if you need to ...
%let rc=%sysfunc(dlgcdir('c:\someplace\public_html')); 
*/
filename odsout '.';

data my_data;
input x y1 y2;
datalines;
0.5 1.0 3.1 
1.5 2.8 1.3 
2.5 0.5 1.1 
3.5 2.0 3.1 
;
run;


ODS LISTING CLOSE;
ODS HTML path=odsout body="&name..htm" 
 (title="SGplot Series Plot Lines With Markers")
 style=htmlblue;

ods graphics / imagefmt=png imagename="&name"
 width=650px height=650px noborder imagemap;

title1 color=gray33 ls=0.0 h=23pt "Series Plot Lines";
title2 color=gray33 ls=0.2 h=17pt "With Markers";

proc sgplot data=my_data aspect=1 noautolegend;
styleattrs datacolors=(cx9999ff cx993366);
series x=x y=y1 / lineattrs=(color=navy thickness=3px) 
 markers markerattrs=(color=navy symbol=diamondfilled size=12pt);
series x=x y=y2 / lineattrs=(color=magenta thickness=3px) 
 markers markerattrs=(color=magenta symbol=squarefilled size=12pt);
yaxis 
 values=(0 to 4 by 1) label='Y Axis'
 labelattrs=(size=16pt weight=bold color=gray33) 
 valueattrs=(size=16pt weight=bold color=gray33)
 offsetmin=0 offsetmax=0 grid minor minorcount=3;
xaxis 
 values=(0 to 4 by 1) label='X Axis'
 labelattrs=(size=16pt weight=bold color=gray33) 
 valueattrs=(size=16pt weight=bold color=gray33) 
 offsetmin=0 offsetmax=0 grid minor minorcount=3;
run;

quit;
ODS HTML CLOSE;
ODS LISTING;

--- end of SAS program ---

Write the complete R script now.