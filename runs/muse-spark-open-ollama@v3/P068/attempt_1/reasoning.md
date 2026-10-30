Need to translate SAS program to R. Need to read inputs from absolute paths given. Librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam, sdtm similarly. Need to find files. Likely .sas7bdat or .xpt. Check OUT_DIR env var.

We need to reproduce table t_ae_soc_pt.rtf with 107 numbers. Need to understand logic.

Steps:
- Get Big N from adam.adsl where SAFFL='Y', grouped by TRTA.
- Subset to TEAE: join adae with adsl where SAFFL='Y' and TRTEMFL='Y'. Select USUBJID, TRTA, AEBODSYS, AEDECOD, AESOC, AESOCCD.
- Count subjects by SOC per TRTA, and total per SOC.
- Count by SOC+PT per TRTA, and total per PT.
- Any TEAE row per TRTA.
- Build display rows: soc_rows with soc_ord = cnt_tot (total distinct subjects per SOC), pt_rows with soc_ord = soc total, pt_ord = pt total.
- Combine, calculate pct = cnt / bign *100.
- value = if cnt>0 then "cnt (pct)" with pct formatted 5.1 (one decimal), else "0".
- Transpose by TRTA to columns col1 col2 col3 corresponding to Placebo, Drug X Low Dose, Drug X High Dose? Need to check actual TRTA values.
- Create display_term: if rowtype 0 or 1 then AEBODSYS else '  ' + AEDECOD.
- Replace missing with '0'.
- Sort order: descending soc_ord, AEBODSYS, rowtype, descending pt_ord, AEDECOD, TRTA. But final report order is by descending soc_ord, AEBODSYS, rowtype, descending pt_ord, AEDECOD. Need to replicate.

We need to inspect data to know TRTA values and counts.

We must write R script that reads adam datasets, does same, writes CSV and TXT.

Expected outputs: t_ae_soc_pt.rtf -> write text rendering and CSV of same table. The instruction says write each output dataset as CSV, each table additionally as plain-text rendering and CSV of table body. So we need to produce t_ae_soc_pt.csv and t_ae_soc_pt.txt (and maybe png not needed). The expected output is table with 107 numbers. Need to ensure formatting matches SAS.

We need to explore file system.