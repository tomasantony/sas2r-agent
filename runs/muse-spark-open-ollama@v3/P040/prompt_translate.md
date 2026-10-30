Translate the SAS program below to R.

Program id: P040   file: P040_kmplot.sas   source: phuse-org/phuse-scripts   family: phuse
Declared output types: graph;table

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P040
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P040

SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine):
  inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P040_f_kmplot.pdf (table/listing text, 373 numbers)  -> write a text rendering and CSV of the same table

--- family environment (phuse) that ran before the program ---
/* PHUSE scripts: util_access_test_data (patched copy) always uses &rawadam / &rawsplit; some scripts expect inlib = pilot ADaM */
libname inlib "&root.\data\derived\cdisc-pilot\adam" access=readonly;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P040_kmplot.sas) ---
/*
This is a comment area. 
Test editing
Adding some more text for editing.
Testing by adding a comment. Also, tested by doing a Windows CTRL-A/CTRL-C to select all and copy
into buffer. Then, paste into local SAS for execution.
Idea: at the risk of big file sizes - better off using a separate which only generates source data
- good for SAS and R???
*/
data one ;
  length arm $8 ;
  censor = 0 ;
  arm = "Arm 1" ;
  do day = 143,164,188,188,190,192,206,213,216,220,227.230,
           234,246,265,304 ;
    output ;
  end ;
  censor = 1 ;
  day = 216 ; output ;
  day = 244 ; output ;
  censor = 0 ;
  arm = "Arm 2" ;
  do day = 142,156,163,198,205,232,232,233,233,233,233,239,
           240, 261, 280, 280, 296, 296, 323 ;
    output ;
  end ;
  censor = 1 ;
  day = 204 ; output ;
  day = 344 ; output ;
  run ;
  
ods pdf file = "&outtlf.\P040_f_kmplot.pdf" ;
ods listing close ;
ods graphics on ;

proc lifetest data=one plots=survival (atrisk=0 to 330 by 30) ;
  time day*censor(1) ;
  srata arm ;
  title1 "Example K-M Plot" ;
  run ;
  
ods graphics off ;
ods pdf close ;

--- end of SAS program ---

Write the complete R script now.