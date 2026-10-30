Translate the SAS program below to R.

Program id: P094   file: P094_5_Retain_the_first_observation_in_each_BY_group_and_saving_removed_rows.sas   source: general/sassoftware/sas-code-examples   family: general
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P094
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P094

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P094.lst (table/listing text, 56 numbers)  -> write a text rendering and CSV of the same table

--- family environment (general) that ran before the program ---
/* no family setup needed */


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P094_5_Retain_the_first_observation_in_each_BY_group_and_saving_removed_rows.sas) ---
/************************************************************************************************
 RETAIN THE FIRST OBSERVATION IN EACH BY GROUP AND SAVE REMOVED ROWS                                                                             
     This program keeps only the first row for each unique value of a variable. 
     Keywords: PROC SORT, remove duplicates                                                              
     SAS Versions: SAS 9, SAS Viya                                                    
     Documentation: https://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=default&docsetId=proc&docsetTarget=p1nd17xr6wof4sn19zkmid81p926.htm
     1. Create the WORK.CLASSTEST table and print it.                            
     2. The first PROC SORT arranges rows by desending scores for each Name and Subject. 
     3. The second PROC SORT keeps only the first occurence of each Name and Subject combination.    
        The DUPOUT= options saves rows that are removed in a separate table named CLASSTEST_DUPS.                                                                     
************************************************************************************************/

data classtest;                                                       /*1*/
   infile datalines dsd;
   input
      Name :$7.
      Subject :$7.
      Score;
datalines4;
Judy,Reading,91
Judy,Math,79
Barbara,Math,90
Barbara,Reading,86
Barbara,Math,90
Louise,Math,72
Louise,Reading,65
William,Math,61
William,Reading,71
Henry,Math,62
Henry,Reading,75
Henry,Reading,84
Jane,Math,94
Jane,Reading,96
;;;;
run;

title "CLASSTEST table before sorting";
proc print data=classtest;
run;

proc sort data=classtest out=classtest_sort;                          /*2*/
    by Name Subject descending Score;
run;

proc sort data=classtest_sort dupout=classtest_dups nodupkey;         /*3*/
    by Name Subject;
run;

title "CLASSTEST_SORT table keeping the high Score for each Name and Subject";
proc print data=classtest_sort;
run;

title "CLASSTEST_DUPS table with the removed lower test scores";
proc print data=classtest_dups;
run;
title;

--- end of SAS program ---

Write the complete R script now.