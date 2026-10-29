Need to translate SAS program P024_ds.sas to R.

We need to understand inputs. The SAS program uses:

- sdtm_dm dataset with keep USUBJID SUBJID RFSTDTC DTHDTC . Where does sdtm_dm come from? In family environment, they import edc_* datasets. But sdtm_dm is not edc_dm? Possibly sdtm_dm is derived earlier? However the program references sdtm_dm. In the environment, maybe sdtm_dm is available as WORK dataset pre-loaded? But list of WORK datasets pre-loaded includes edc_* and ivrs_rand only. No sdtm_dm. So we need to infer sdtm_dm creation? Perhaps sdtm_dm is derived from edc_dm? Let's look at typical faijal oncology project. We need to search for other programs? But we have only this program. However we need to reproduce expected outputs: P024.lst (table/listing text, 45 numbers) -> write text rendering and CSV of same table, and dataset ds -> OUT_DIR/ds.csv

The SAS program does:

- proc sort data=sdtm_dm(keep=USUBJID SUBJID RFSTDTC DTHDTC) out=dm_for_ds; by USUBJID;

- data edc_ds_prep; set edc_ds; length TERM DECOD CAT $50 STDT $11; TERM=DS_TERM; DECOD=DS_TERM; CAT=DS_CAT; STDT=DS_DT; USUBJID=catx("-", "ONCO-2026", SUBJID);

- merge edc_ds_prep with dm_for_ds by USUBJID

- create STUDYID="ONCO-2026", DOMAIN="DS", DSTERM=tranwrd(TERM,"_"," "), DSDECOD=upcase(DECOD), DSCAT=tranwrd(CAT,"_"," "), DTHFL="Y" if not missing DTHDTC, DSSTDTC = put(input(STDT,date11.), yymmdd10.) if STDT ne "", DSSTDY calculation.

- sort by USUBJID DSSTDTC

- derive DSSEQ per USUBJID

- proc print, proc export to ds.xlsx

So we need to replicate sdtm_dm. Since not provided, we must construct sdtm_dm from available data: edc_dm.xlsx and maybe other sources. Let's inspect what files exist. We have paths:

&rawsdtm = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
&rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
&rawdir = D:/meta_hackathon/sas2r-agent/data/raw
&root = D:/meta_hackathon/sas2r-agent

We need to explore filesystem to see what's available.