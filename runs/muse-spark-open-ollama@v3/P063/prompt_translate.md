Translate the SAS program below to R.

Program id: P063   file: P063_event_gant_excel.sas   source: Morioka-Yutaka/vis_review_kit   family: morioka
Declared output types: table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P063
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P063

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  dataset gant  -> write OUT_DIR/gant.csv

--- family environment (morioka) that ran before the program ---
/* no family setup needed */


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P063_event_gant_excel.sas) ---
/*** HELP START ***//*

Macro Name        : %event_gant_excel

Description       : 
    This macro generates a Gantt chart in Excel format based on user-specified event data.
    Given a dataset containing event start and end days, the macro builds a day-wise array  
    and visually highlights the event duration using colored cells via ODS EXCEL and 
    SAS RWI (Report Writing Interface)

Main Features     :
    - Automatically calculates the time range based on event dates
    - Distinguishes ongoing events (with missing end date) using separate cell styling
    - Displays events per subject along a horizontal timeline
    - All input elements (dataset, variable names, output path, etc.) are configurable

Input Parameters  :
    outpath             - Output directory path for the Excel file 
    outfile             - Output file name (e.g., gant.xlsx)
    target_dataest      - Input dataset (e.g., work.ae)
    target_id           - Subject identifier variable (e.g., SUBJID)
    target_event_name   - Event name variable (e.g., AETERM)
    target_st           - Event start day variable (e.g., AESTDY)
    target_en           - Event end day variable (e.g., AEENDY)

Assumptions       :
    - Event start and end values are integers (typically study day numbers)
    - Missing end date indicates an ongoing event

Limitations       :
    - Large time spans or a high number of subjects may lead to excessive memory use,
      potentially resulting in runtime errors

Author            : Yutaka Morioka  

Usage Example     :
    %event_gant_excel(
        outpath=F:\Project\output,
        outfile=ae_gant.xlsx,
        target_dataest=work.ae,
        target_id=SUBJID,
        target_event_name=AETERM,
        target_st=AESTDY,
        target_en=AEENDY
    );

*//*** HELP END ***/

%macro event_gant_excel(
outpath = ./
,outfile =gant.xlsx
,target_dataest=dummy
,target_id = SUBJID
,target_event_name = AETERM
,target_st = AESTDY
,target_en = AEENDY);

/*test_dummy_data*/
data dummy;
   SUBJID="AAA";AETERM="Adverse Event1"; AESTDY=1;AEENDY=5;output;
   SUBJID="AAA";AETERM="Adverse Event2"; AESTDY=3;AEENDY=8;output;
   SUBJID="BBB";AETERM="Adverse Event1"; AESTDY=2;AEENDY=30;output;
   SUBJID="BBB";AETERM="Adverse Event2"; AESTDY=-2;AEENDY=3;output;
   SUBJID="BBB";AETERM="Adverse Event3"; AESTDY=4;AEENDY=.;output;
   SUBJID="CCC";AETERM="Adverse Event1"; AESTDY=15;AEENDY=20;output;
run;

proc sql noprint;
 select min(min(&target_st,1)) into:min trimmed from &target_dataest;
 select max(max(&target_st,&target_en)) into:max trimmed from &target_dataest;
quit;
%put &=min;
%put &=max;
%let range =%eval( %sysfunc(range(&min,&max))+ (0<&min));
%put &=range;

data __wk1;
 set &target_dataest;
 array ar{&range.} $1.;
 if &min < 1 then do;
    st = &target_st - &min + (&target_st<1);
    if ^missing(&target_en) then en = &target_en - &min + (&target_en<1);
 end;
 else do; 
    st=&target_st;
    en=&target_en;
 end;
 do i = 1 to &range;
    if st <=i <=en then ar{i}="Y";
    else if st=i and missing(en) then ar{i}="Y";  
    else if st<=i and missing(en) then ar{i}="O";  
 end;
keep st en &target_id &target_event_name &target_st &target_en ar:;
run;

%macro hed();
 %do i = &min %to &max;
    if &i ne 0 then ob.format_cell(data: "&i",  style_attr: "background=blue color=white");
 %end;
%mend; 

%macro row();
 %do i = 1 %to %eval(&range);
   if ar&i="Y" then  ob.format_cell(data: ar&i,  style_attr: "background=red color=red");
   else if ar&i="O" then  ob.format_cell(data: ar&i,  style_attr: "background=pink color=pink");
   else ob.format_cell(data: ar&i,  style_attr: "background=white color=white");
 %end;
%mend; 

ods excel file="&outpath\&outfile." options( sheet_name= "GANT" );
data _NULL_;

   set __wk1 end=EOF;

   if _N_=1 then do;
       dcl odsout ob();
       ob.table_start();

    *** header ;
       ob.head_start();
       ob.row_start();
            ob.format_cell(data: "ID",  style_attr: "background=darkblue color=white");
            ob.format_cell(data: "Event",  style_attr: "background=darkblue color=white");
            ob.format_cell(data: "STDY",  style_attr: "background=darkblue color=white");
            ob.format_cell(data: "ENDY",  style_attr: "background=darkblue color=white");
            %hed();
       ob.row_end();
       ob.head_end();

   end;

   *** report ;
   ob.row_start();
        ob.format_cell(data: &target_id);
        ob.format_cell(data: &target_event_name);
        ob.format_cell(data: &target_st);
        ob.format_cell(data: &target_en);
         %row();
   ob.row_end();

   if EOF then ob.table_end();
run;
ods excel close;
%mend;

/* benchmark driver: the program only defines %event_gant_excel; run it with its built-in test data */
%event_gant_excel(outpath=&outds., outfile=gant.xlsx);

--- end of SAS program ---

Write the complete R script now.