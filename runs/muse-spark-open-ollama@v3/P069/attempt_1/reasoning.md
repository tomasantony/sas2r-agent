Need to translate SAS program P069_t_ae_sum.sas to R.

We need to read inputs from absolute paths given. Librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam, sdtm similarly. Need to list files.

We need to produce outputs under OUT_DIR: t_ae_sum.rtf? But instructions say write CSV and txt and png. Expected outputs: t_ae_sum.rtf (table/listing text, 42 numbers) -> write text rendering and CSV.

We need to reproduce SAS logic.

Steps:

- Get Big N from adam.adsl where SAFFL='Y', grouped by TRTA.

- AE categories: 8 categories plus 0.

Categories:
0: Subjects with no TEAE
1: Subjects with at least one TEAE
2: Related TEAE (AEREL in POSSIBLE, PROBABLE, DEFINITE, RELATED)
3: Grade >=3 TEAE (input(AETOXGR, best.) >=3)
4: Serious Adverse Event (AESER='Y')
5: Related Serious Adverse Event (AESER='Y' and AEREL in ...)
6: TEAE Leading to Study Drug Discontinuation (AEACN='DRUG WITHDRAWN')
7: TEAE Leading to Dose Modification (AEACN in DOSE REDUCED, DRUG INTERRUPTED, DOSE INCREASED)
8: TEAE with Outcome of Death (AEOUT='FATAL')

All filtered also by TRTEMFL='Y' and SAFFL='Y' via join.

Note: For category 0, left join distinct USUBJID from adae where TRTEMFL='Y' (no SAFFL filter? But then filter b.SAFFL='Y' and a.USUBJID is null). So subjects in safety set with no TEAE.

- Then combine, calculate pct = cnt / bign *100

- Then value = cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') if cnt>0 else '0'

Note put(pct,5.1) in SAS: width 5, 1 decimal, rounding half away from zero? Need to emulate. SAS format 5.1 rounds. So we need round half up.

Also need to handle pct formatting: strip(put(pct,5.1)) -> e.g., "12.3" . Then value = "12 (12.3)"? Actually example: cnt=12 pct=34.5 => "12 (34.5)". Need to check SAS: cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') => "12 (34.5)". Yes.

- Then transpose by ord category, id TRTA, var value. So columns per TRTA.

Need to know TRTA values: likely "Placebo", "Drug X Low Dose", "Drug X High Dose" ? From u_setup: trt1lbl Placebo, trt2lbl [Drug] Low Dose, trt3lbl [Drug] High Dose. But actual TRTA values in ADSL may be those strings. Need to inspect data.

- Then ensure zero rows display: if cols{i}='' then '0'

- Then report with columns ord category col1 col2 col3, where col1hdr etc are "Placebo (N=xx)" etc.

We need to reproduce exactly.

We need to explore data files.

We have OUT_DIR env var. Need to read adam datasets.

Let's list directory.