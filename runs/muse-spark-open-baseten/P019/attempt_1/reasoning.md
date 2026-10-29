Need to translate SAS program to R. Must read inputs from absolute paths given. Librefs: raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw, sdtm -> .../data/sdtm, adam -> .../data/adam. Need to list files.

We need to produce outputs: P019.lst (table/listing text, 156 numbers) -> write text rendering and CSV of same table, and dataset qc_report -> write OUT_DIR/qc_report.csv

We need to implement all 39? Actually 39 checks listed.

We need to explore data to understand structure.

We must write R script that does all checks.

We need to handle SAS semantics: missing, dates, yrdif, etc.

We need to locate files.