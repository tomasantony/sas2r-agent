Need to translate SAS program P022_ae.sas to R.

We have inputs: edc_ae, edc_dm? Actually sdtm_dm is needed. But family environment says edc_<domain> datasets in WORK. However SAS program uses sdtm_dm (keep USUBJID RFSTDTC). Where does sdtm_dm come from? Possibly earlier steps? But we have edc_dm.xlsx and ivrs_rand etc. Need to derive sdtm_dm? The program expects sdtm_dm dataset. But we don't have it pre-loaded. We need to derive it from raw inputs using standard CDISC definition.

We need to understand what sdtm_dm contains: USUBJID, RFSTDTC. RFSTDTC is reference start date (first exposure or randomization). We have edc_dm, edc_ex, ivrs_rand.

We need to look at actual files to infer.

We need to write R script that reads inputs from absolute paths given. The macro variables: &rawdir = D:/meta_hackathon/sas2r-agent/data/raw ; &outds etc. But for R we need to read from those paths. However instruction says read inputs from absolute paths given in context (forward slashes). The WORK datasets pre-loaded from Excel files: edc_ae etc. So we should read those Excel files directly.

Also need sdtm_dm. Since not listed, we must derive it. Let's explore file system.

We need to list files.