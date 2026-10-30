Need to translate SAS program to R.

We need to read inputs from absolute paths given in context. Let's parse.

Macro variables:
&root. = D:/meta_hackathon/sas2r-agent
&project_path. = D:/meta_hackathon/sas2r-agent/data/derived/c2c
&outtlf etc but OUT_DIR env var.

Librefs:
raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw
sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm
adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam

Program reads:
infile "&project_path./data/raw/raw_conmeds.csv" => D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_conmeds.csv
and sdtm.dm(keep=USUBJID RFENDTC) => D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/dm.sas7bdat or .xpt? Need to check.

Outputs:
dataset cm -> OUT_DIR/cm.csv
P010.lst (table/listing text, 195 numbers) -> write text rendering and CSV of same table

We need to explore filesystem to see what exists.