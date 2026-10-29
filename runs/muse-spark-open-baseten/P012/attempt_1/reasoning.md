Need to translate SAS program P012_sdtm_ds.sas to R.

We need to read inputs from absolute paths given. The SAS program:

proc import datafile="&project_path./data/raw/raw_demog.csv" out=work.raw_ds dbms=csv replace;

project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

So file is D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv

Then creates sdtm.ds dataset with transformations.

We need to reproduce exactly.

We need to understand raw_demog.csv structure. Let's explore filesystem.

We need to write R script that reads that file, does transformations, writes OUT_DIR/ds.csv and also P012.lst text and csv.

Expected outputs: P012.lst (table/listing text, 108 numbers) -> write text rendering and CSV of same table
dataset ds -> write OUT_DIR/ds.csv

The SAS program does proc print data=sdtm.ds(obs=12); So listing is first 12 observations of ds.

We need to reproduce that listing.

We need to check what raw_demog.csv contains. Also need to handle SAS semantics: validvarname=upcase means variables become uppercase. So SUBJ_ID, DISPOSITION_REASON, DISPOSITION, STUDY_END_DAT etc.

We need to inspect file.

We should also check expected outputs under output/sas/datasets/P012 and output/sas/tlf ?

Let's list directories.