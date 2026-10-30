Need to translate SAS program P014_sdtm_ex.sas to R.

We need to read inputs from absolute paths given in context. The SAS program imports raw_exposure.csv from &project_path./data/raw/raw_exposure.csv . project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So path is D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_exposure.csv

We need to check what exists. Also librefs raw -> .../data/raw etc. But program uses raw_exposure.csv.

We need to produce outputs: P014.lst (table/listing text, 126 numbers) -> write text rendering and CSV of same table, and dataset ex -> write OUT_DIR/ex.csv

We need to understand expected outputs. The SAS program does:

- proc import raw_exposure.csv -> work.raw_ex
- data work.ex_mapped: length definitions, STUDYID="CDISC-01", DOMAIN="EX", USUBJID=catx("-", STUDYID, PATIENT), EXTRT=strip(TREATMENT), EXDOSE=DOSE, EXDOSU=strip(UNIT), date conversion using vtype and anydtdte., then keep.
- proc sort by USUBJID EXSTDTC
- data sdtm.ex retain STUDYID DOMAIN USUBJID EXSEQ; set work.ex_mapped; by USUBJID; if first.USUBJID then EXSEQ=1 else EXSEQ+1;
- proc print obs=10 var USUBJID EXSEQ EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC with title "EX Domain Audit (Study Drug Exposure)"

So expected outputs: dataset ex (sdtm.ex) and listing P014.lst which is the print.

We need to reproduce SAS semantics faithfully.

Key details:

- PROC IMPORT of CSV assigns ONE informat to each column after scanning its values (GUESSINGROWS): a column of n/n/yyyy values becomes numeric SAS date read as DDMMYY10. when any value has first field above 12 (otherwise MMDDYY10.), and every value in that column is then read in that same order. So later INPUT(..., anydtdte.) or VTYPE()="C" branch never runs on that column. Emulate this: decide order once per column from all its values, then parse every value with that single format; never choose per value.

But in this program, the date conversion checks vtype(START_DATE) = 'C' then input with anydtdte., else _start_num = START_DATE. So if PROC IMPORT read START_DATE as numeric date, then vtype is N, and it just uses numeric value. If read as character, then parse.

We need to emulate PROC IMPORT behavior. However in R we can read raw_exposure.csv directly. We need to understand its content to decide parsing.

We should inspect file.