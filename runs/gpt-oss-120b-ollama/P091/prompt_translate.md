Translate the SAS program below to R.

Program id: P091   file: P091_bar1.sas   source: general/sascommunities/graphics-programming   family: general
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P091
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P091

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  bar1.htm (table/listing text, 0 numbers)  -> write a text rendering and CSV of the same table
  figure bar1.png  -> write OUT_DIR/bar1.png and a CSV of the plotted data

--- family environment (general) that ran before the program ---
/* no family setup needed */


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P091_bar1.sas) ---
%let name=bar1;

/* 
Set your current-working-directory (to read/write files), if you need to ...
%let rc=%sysfunc(dlgcdir('c:\someplace\public_html')); 
*/
filename odsout '.';

data my_data;
input CATEGORY SERIES $ 3-11 AMOUNT;
datalines;
1 Series A  5
2 Series A  7.8
1 Series B  9.5
2 Series B  5.9
;
run;


ODS LISTING CLOSE;
ODS HTML path=odsout body="&name..htm" 
 (title="SGplot Horizontal Grouped Bar") 
 style=htmlblue;

ods graphics / imagefmt=png imagename="&name" 
 width=800px height=600px noborder imagemap; 

title1 color=gray33 ls=0.5 h=23pt "Horizontal Grouped Bar";
title2 color=gray33 ls=0.5 h=17pt "Compares values across categories";

proc sgplot data=my_data noautolegend;
styleattrs datacolors=(cx9999ff cx993366);
hbar category / response=amount stat=sum 
 group=series groupdisplay=cluster grouporder=descending
 outlineattrs=(color=black) nostatlabel;
xaxis 
 values=(0 to 10 by 2)
 labelattrs=(size=16pt weight=bold color=gray33) 
 valueattrs=(size=16pt weight=bold color=gray33) 
 offsetmax=0 grid minor minorcount=1;
yaxis 
 labelattrs=(size=16pt weight=bold color=gray33) 
 valueattrs=(size=16pt weight=bold color=gray33)
 display=(noticks);
run;

quit;
ODS HTML CLOSE;
ODS LISTING;

--- end of SAS program ---

Write the complete R script now.