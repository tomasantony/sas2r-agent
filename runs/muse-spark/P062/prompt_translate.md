Translate the SAS program below to R.

Program id: P062   file: P062_diagram_plot.sas   source: Morioka-Yutaka/trial_flow   family: morioka
Declared output types: graph

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P062
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P062

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  figure SGPlot.png  -> write OUT_DIR/sgplot.png and a CSV of the plotted data

--- family environment (morioka) that ran before the program ---
/* no family setup needed */


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/morioka/diagram_box.sas ---
/*** HELP START ***//*

Macro Name:    diagram_box
   Purpose:
     Creates a rectangular box (polygon) and corresponding
     text annotation dataset for use in flow diagrams.
     The macro generates two datasets:
       - diagram_box_<boxid>:  Coordinates of the box
       - diagram_box_text_<boxid>: Position and content of text
 
   Parameters:
     boxid      = Unique identifier for the box (default=1)
     left_x     = X coordinate of the left edge   (default=10)
     top_y      = Y coordinate of the top edge    (default=100)
     right_x    = X coordinate of the right edge  (default=30)
     bottom_y   = Y coordinate of the bottom edge (default=90)
     text       = Label text to be displayed inside the box
     text_just  = Text justification (CENTER [default] or LEFT)
     text_size = Text font size [1-100 pt] (default =10 pt)
     text_color = Text font color (default = black)
     linecolor = Box linecolor (default = black)
     linepattern = Box linepattern (default=Solid)
     linethickness = Box linethickness (default=1)

   Notes:
     - Coordinates define an axis-aligned rectangle.
     - Text position is calculated based on justification:
         CENTER 竊・centered at the box
          LEFT   竊・aligned to the left edge, vertically centered
     - Text can include line breaks using the SPLITCHAR in SGPLOT.
     - The macro is typically used in combination with diagram_line
       and diagram_plot to build flow diagrams (e.g., CONSORT).

  Usage Example:
    %diagram_box(
        boxid=1,
        left_x=30,
        top_y=100,
        right_x=70,
        bottom_y=90,
        text=Assessed for eligibility (n=100)
    );
 
    %diagram_box(
        boxid=2,
        left_x=65,
        top_y=86,
        right_x=95,
        bottom_y=60,
        text_just=left,
        text=Excluded (n=10)#- Not meeting inclusion criteria (n=4)
             #- Declined to participate (n=4)
             #- Other reasons (n=2)
    );

*//*** HELP END ***/

%macro diagram_box(boxid=1,left_x=10,top_y=100,right_x=30,bottom_y=90,text=,text_just=center,text_size=10,text_color=black,linecolor=black,linepattern=solid,linethickness=1);
data diagram_box_&boxid.;
DIAGID=cats(&boxid.);
boxid=&boxid.;box_x=&left_x.;box_y=&top_y.;output;
boxid=&boxid.;box_x=&right_x.;box_y=&top_y.;output;
boxid=&boxid.;box_x=&right_x.;box_y=&bottom_y.;output;
boxid=&boxid.;box_x=&left_x.;box_y=&bottom_y.;output;
run;

data diagram_box_text_&boxid.;
length text_x text_y text_x2 text_y2 8. text text2 $1000. sizeresponse 8.;
call missing(of  text_x text_y text_x2 text_y2 text text2 );
    x1=&left_x.; y1=&top_y.;   
    x2=&right_x.; y2=&top_y.;   
    x3=&right_x.; y3=&bottom_y.  ;
    x4=&left_x.; y4=&bottom_y.;  

    den = (x1-x3)*(y2-y4) - (y1-y3)*(x2-x4);

    if den = 0 then do;
        put "WARNING: The diagonals are parallel or coincide. A quadrilateral cannot be formed.";
    end;
    else do;
        num1 = (x1*y3 - y1*x3);
        num2 = (x2*y4 - y2*x4);
        %if %upcase(&text_just) ne LEFT %then %do;
          text_x = ( num1*(x2-x4) - (x1-x3)*num2 ) / den;
          text_y = ( num1*(y2-y4) - (y1-y3)*num2 ) / den;
          text="&text";
        %end;
        %if %upcase(&text_just) eq LEFT %then %do;
          text_x2 = x1 + 1;
          text_y2 = ( num1*(y2-y4) - (y1-y3)*num2 ) / den;
          text2="&text";
        %end;
    end;
    sizeresponse = &text_size.;
    DIAGID=cats("&boxid.");
drop den num1 num2;
run;

data diag_style_&boxid.;
length id $20. value textcolor linecolor linepattern  $30. linethickness 8.;
id="DIAGID";
value =cats("&boxid.");
textcolor= "&text_color";
linecolor="&linecolor";
linepattern="&linepattern";
linethickness=&linethickness;
run;

%mend;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/morioka/diagram_line.sas ---
/*** HELP START ***//*

Macro Name : diagram_line
Purpose : Create a dataset containing line coordinates (start -> [mid] -> end).
Useful for plotting diagrams where lines are defined by 2 or 3 points.

 Parameters :
   lineid      = (numeric, default=1)
                 Identifier for the line. Used as a variable in the output dataset name.

   start_x     = (numeric, default=30)
   start_y     = (numeric, default=90)
                 Coordinates of the starting point of the line.

   midpoint_x  = (numeric, optional)
   midpoint_y  = (numeric, optional)
                 Coordinates of an optional midpoint. If both are provided,
                 the line will include this point.

   end_x       = (numeric, default=30)
   end_y       = (numeric, default=60)
                 Coordinates of the end point of the line.

   linecolor    = Lline color (default = black)
   linepattern = Line pattern (default=Solid)
   linethickness = Linethickness (default=1)


 Output     :
   A dataset named diagram_line_<lineid> with variables:
     - lineid : the line identifier
     - line_x : x coordinate of points
     - line_y : y coordinate of points

 Example    :
   %diagram_line(lineid=2, start_x=10, start_y=20, midpoint_x=15, midpoint_y=25, end_x=30, end_y=40);
  -> Produces dataset diagram_line_2 with 3 points: (10,20) -> (15,25) -> (30,40)

*//*** HELP END ***/

%macro diagram_line(lineid=1,
                      start_x=30,
                      start_y=90,
                      midpoint_x=,
                      midpoint_y=,
                      end_x=30,
                      end_y=60,
                      linecolor=black,
                      linepattern=solid,
                      linethickness=1
);
data diagram_line_&lineid.;
_LINEID=cats(&lineid.);
lineid=&lineid.;line_x=&start_x.;line_y=&start_y.;output;
%if %length(&midpoint_x.) ne  0 and %length(&midpoint_y.) ne  0 %then %do;
lineid=&lineid.;line_x=&midpoint_x.;line_y=&midpoint_y.;output;
%end;
lineid=&lineid.;line_x=&end_x.;line_y=&end_y.;output;
run;

data line_style_&lineid.;
length id $20. value  linecolor linepattern  $30. linethickness 8.;
id="_LINEID";
value =cats("&lineid.");
linecolor="&linecolor";
linepattern="&linepattern";
linethickness=&linethickness;
run;
%mend;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/morioka/diagram_box.sas ---
/*** HELP START ***//*

Macro Name:    diagram_box
   Purpose:
     Creates a rectangular box (polygon) and corresponding
     text annotation dataset for use in flow diagrams.
     The macro generates two datasets:
       - diagram_box_<boxid>:  Coordinates of the box
       - diagram_box_text_<boxid>: Position and content of text
 
   Parameters:
     boxid      = Unique identifier for the box (default=1)
     left_x     = X coordinate of the left edge   (default=10)
     top_y      = Y coordinate of the top edge    (default=100)
     right_x    = X coordinate of the right edge  (default=30)
     bottom_y   = Y coordinate of the bottom edge (default=90)
     text       = Label text to be displayed inside the box
     text_just  = Text justification (CENTER [default] or LEFT)
     text_size = Text font size [1-100 pt] (default =10 pt)
     text_color = Text font color (default = black)
     linecolor = Box linecolor (default = black)
     linepattern = Box linepattern (default=Solid)
     linethickness = Box linethickness (default=1)

   Notes:
     - Coordinates define an axis-aligned rectangle.
     - Text position is calculated based on justification:
         CENTER 竊・centered at the box
          LEFT   竊・aligned to the left edge, vertically centered
     - Text can include line breaks using the SPLITCHAR in SGPLOT.
     - The macro is typically used in combination with diagram_line
       and diagram_plot to build flow diagrams (e.g., CONSORT).

  Usage Example:
    %diagram_box(
        boxid=1,
        left_x=30,
        top_y=100,
        right_x=70,
        bottom_y=90,
        text=Assessed for eligibility (n=100)
    );
 
    %diagram_box(
        boxid=2,
        left_x=65,
        top_y=86,
        right_x=95,
        bottom_y=60,
        text_just=left,
        text=Excluded (n=10)#- Not meeting inclusion criteria (n=4)
             #- Declined to participate (n=4)
             #- Other reasons (n=2)
    );

*//*** HELP END ***/

%macro diagram_box(boxid=1,left_x=10,top_y=100,right_x=30,bottom_y=90,text=,text_just=center,text_size=10,text_color=black,linecolor=black,linepattern=solid,linethickness=1);
data diagram_box_&boxid.;
DIAGID=cats(&boxid.);
boxid=&boxid.;box_x=&left_x.;box_y=&top_y.;output;
boxid=&boxid.;box_x=&right_x.;box_y=&top_y.;output;
boxid=&boxid.;box_x=&right_x.;box_y=&bottom_y.;output;
boxid=&boxid.;box_x=&left_x.;box_y=&bottom_y.;output;
run;

data diagram_box_text_&boxid.;
length text_x text_y text_x2 text_y2 8. text text2 $1000. sizeresponse 8.;
call missing(of  text_x text_y text_x2 text_y2 text text2 );
    x1=&left_x.; y1=&top_y.;   
    x2=&right_x.; y2=&top_y.;   
    x3=&right_x.; y3=&bottom_y.  ;
    x4=&left_x.; y4=&bottom_y.;  

    den = (x1-x3)*(y2-y4) - (y1-y3)*(x2-x4);

    if den = 0 then do;
        put "WARNING: The diagonals are parallel or coincide. A quadrilateral cannot be formed.";
    end;
    else do;
        num1 = (x1*y3 - y1*x3);
        num2 = (x2*y4 - y2*x4);
        %if %upcase(&text_just) ne LEFT %then %do;
          text_x = ( num1*(x2-x4) - (x1-x3)*num2 ) / den;
          text_y = ( num1*(y2-y4) - (y1-y3)*num2 ) / den;
          text="&text";
        %end;
        %if %upcase(&text_just) eq LEFT %then %do;
          text_x2 = x1 + 1;
          text_y2 = ( num1*(y2-y4) - (y1-y3)*num2 ) / den;
          text2="&text";
        %end;
    end;
    sizeresponse = &text_size.;
    DIAGID=cats("&boxid.");
drop den num1 num2;
run;

data diag_style_&boxid.;
length id $20. value textcolor linecolor linepattern  $30. linethickness 8.;
id="DIAGID";
value =cats("&boxid.");
textcolor= "&text_color";
linecolor="&linecolor";
linepattern="&linepattern";
linethickness=&linethickness;
run;

%mend;


--- macro / included file: D:/meta_hackathon/sas2r-agent/programs/sas/macros/morioka/diagram_line.sas ---
/*** HELP START ***//*

Macro Name : diagram_line
Purpose : Create a dataset containing line coordinates (start -> [mid] -> end).
Useful for plotting diagrams where lines are defined by 2 or 3 points.

 Parameters :
   lineid      = (numeric, default=1)
                 Identifier for the line. Used as a variable in the output dataset name.

   start_x     = (numeric, default=30)
   start_y     = (numeric, default=90)
                 Coordinates of the starting point of the line.

   midpoint_x  = (numeric, optional)
   midpoint_y  = (numeric, optional)
                 Coordinates of an optional midpoint. If both are provided,
                 the line will include this point.

   end_x       = (numeric, default=30)
   end_y       = (numeric, default=60)
                 Coordinates of the end point of the line.

   linecolor    = Lline color (default = black)
   linepattern = Line pattern (default=Solid)
   linethickness = Linethickness (default=1)


 Output     :
   A dataset named diagram_line_<lineid> with variables:
     - lineid : the line identifier
     - line_x : x coordinate of points
     - line_y : y coordinate of points

 Example    :
   %diagram_line(lineid=2, start_x=10, start_y=20, midpoint_x=15, midpoint_y=25, end_x=30, end_y=40);
  -> Produces dataset diagram_line_2 with 3 points: (10,20) -> (15,25) -> (30,40)

*//*** HELP END ***/

%macro diagram_line(lineid=1,
                      start_x=30,
                      start_y=90,
                      midpoint_x=,
                      midpoint_y=,
                      end_x=30,
                      end_y=60,
                      linecolor=black,
                      linepattern=solid,
                      linethickness=1
);
data diagram_line_&lineid.;
_LINEID=cats(&lineid.);
lineid=&lineid.;line_x=&start_x.;line_y=&start_y.;output;
%if %length(&midpoint_x.) ne  0 and %length(&midpoint_y.) ne  0 %then %do;
lineid=&lineid.;line_x=&midpoint_x.;line_y=&midpoint_y.;output;
%end;
lineid=&lineid.;line_x=&end_x.;line_y=&end_y.;output;
run;

data line_style_&lineid.;
length id $20. value  linecolor linepattern  $30. linethickness 8.;
id="_LINEID";
value =cats("&lineid.");
linecolor="&linecolor";
linepattern="&linepattern";
linethickness=&linethickness;
run;
%mend;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P062_diagram_plot.sas) ---
/*** HELP START ***//*

Macro name:    diagram_plot
  Purpose:
    Combines all generated diagram components (boxes, text,
    and lines) and plots them as a flow diagram using PROC SGPLOT.
    The macro collects all datasets prefixed with "diagram_" and
    produces a complete diagram without axes or borders.

  Parameters:
    minx  = Minimum value of the X-axis (default=0)
    maxx  = Maximum value of the X-axis (default=100)
    miny  = Minimum value of the Y-axis (default=0)
    maxy  = Maximum value of the Y-axis (default=100)
    text_font = Text font (default=Courier)

  Notes:
    - This macro expects that %diagram_box and %diagram_line
      have already been called to create the component datasets.
    - Text is automatically split by the SPLITCHAR (#) option.
    - Arrows are drawn for lines with arrowhead at the end.

  Usage Example:
    %diagram_box(
        boxid=1,
        left_x=30, top_y=100,
        right_x=70, bottom_y=90,
        text=Assessed for eligibility (n=100)
    );

    %diagram_line(
        lineid=1,
        start_x=50, start_y=90,
        end_x=50,   end_y=55
    );

    %diagram_box(
        boxid=2,
        left_x=37, top_y=55,
        right_x=63, bottom_y=45,
        text=Randomized (n=90)
    );

    %diagram_plot(minx=0, maxx=100, miny=0, maxy=100);

*//*** HELP END ***/

%macro diagram_plot(minx=0,maxx=100,miny=0,maxy=100,text_font=Courier);
data plot_diag;
length text text2 $1000.;
set diagram_:;
run;

data plot_diag_attr;
set diag_style_: line_style_:;
run;

proc sgplot data=plot_diag noborder noautolegend dattrmap=plot_diag_attr;
  polygon id=boxid x=box_x y=box_y/attrid=DIAGID group=DIAGID;
  text x=text_x y=text_y text=text /textattrs=(family="&text_font") attrid=DIAGID group=DIAGID sizemin=1pt sizemaxresponse=100 sizemax=100pt sizeresponse = sizeresponse  splitchar='#' splitpolicy=splitalways;
  text x=text_x2 y=text_y2 text=text2 /textattrs=(family="&text_font") attrid=DIAGID group=DIAGID  sizemin=1pt sizemaxresponse=100 sizemax=100pt sizeresponse = sizeresponse  sizeresponse = sizeresponse  position=right splitchar='#' splitpolicy=splitalways;
  series x=line_x y=line_y/ attrid=_LINEID  group=_LINEID  arrowheadshape=filled arrowheadpos=end  arrowheadscale=0.3;

  xaxis display=none min=&minx. max=&maxx. offsetmin=0 offsetmax=0;
  yaxis display=none min=&miny. max=&maxy. offsetmin=0 offsetmax=0;
run;
%mend;

/* benchmark driver: the program only defines %diagram_plot; build the header's usage example with the
   companion macros from the same repository (programs/sas/macros/morioka) and plot it */
%include "&root./programs/sas/macros/morioka/diagram_box.sas";
%include "&root./programs/sas/macros/morioka/diagram_line.sas";
%diagram_box(boxid=1, left_x=30, top_y=100, right_x=70, bottom_y=90, text=Assessed for eligibility (n=100));
%diagram_line(lineid=1, start_x=50, start_y=90, end_x=50, end_y=55);
%diagram_box(boxid=2, left_x=37, top_y=55, right_x=63, bottom_y=45, text=Randomized (n=90));
%diagram_plot(minx=0, maxx=100, miny=0, maxy=100);

--- end of SAS program ---

Write the complete R script now.