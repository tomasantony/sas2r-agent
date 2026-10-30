Need to translate SAS program to R. Need to read inputs from absolute paths given. Librefs: raw, sdtm, adam -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/{raw,sdtm,adam}

We need to find actual files. The program uses adam.adsl and sdtm.ae. So need to locate those datasets.

We have environment variable OUT_DIR. Need to write adae.csv and P004.lst text rendering and CSV.

We need to explore file system to understand data.

We should list directories.

We need to produce R script that reads inputs from absolute paths given in context (forward slashes). SAS transport files (.xpt) and SAS datasets (.sas7bdat) are read with haven.

We need to check what files exist in those directories.

We need to run R script non-interactively.

We need to reproduce SAS semantics faithfully.

Steps:

1. Sort ADSL and AE by USUBJID etc.
2. Merge ADSL and AE to create ADAE draft.
   - Keep only subjects who actually experienced AE (if b)
   - Numeric date derivations: if length(AESTDTC) >=10 then AESTDT = input(substr(AESTDTC,1,10), yymmdd10.)
   Similarly AEENDTC.
   Format date9. but we will output ISO.
   - ASTDY: if not missing AESTDT and TRTSDT then if AESTDT >= TRTSDT then ASTDY = (AESTDT - TRTSDT)+1 else ASTDY = AESTDT - TRTSDT
   - TRTEMFL: if SAFFL='Y' and not missing AESTDT and TRTSDT then if AESTDT >= TRTSDT then TRTEMFL="Y" else "" else ""
   - Keep variables: STUDYID USUBJID SUBJID TRT01A TRTSDT TRTEDT AGE SEX AESEQ AETERM AEDECOD AESEV AEREL AEOUT AESER AESTDT AEENDT ASTDY TRTEMFL

3. Derive first treatment-emergent occurrence by decoded term:
   - _occ_order = 0 if TRTEMFL='Y' else 1
   - sort by USUBJID AEDECOD _occ_order AESTDT AESEQ
   - by USUBJID AEDECOD _occ_order; if TRTEMFL='Y' and first._occ_order then AOCCFL='Y' else ''

Note: first._occ_order means first within group defined by USUBJID AEDECOD _occ_order. So for each subject and decoded term, within the treatment-emergent group (order 0), the first record gets AOCCFL='Y'. Non-TE records never get flag. Also if multiple TE records for same PT, only first gets Y.

4. Final sort by USUBJID AESTDT AESEQ and save to adam.adae

5. Visual audit: proc print obs=15 var USUBJID TRT01A AETERM AESEV AESTDT TRTSDT ASTDY TRTEMFL AOCCFL

We need to produce listing P004.lst with 88 numbers? The expected output says P004.lst (table/listing text, 88 numbers) -> write text rendering and CSV.

We need to understand what numbers are in listing. Probably the audit print.

We need to check actual SAS output if available? Look at output/sas/tlf etc.

We should explore filesystem.

We need to write R script that does all steps and writes outputs.

We need to handle dates: AESTDTC is ISO 8601 character dates. SAS does substr 1,10 and input yymmdd10. That expects YYYY-MM-DD. So we need to parse similarly. Use as.Date(substr(...,1,10), format="%Y-%m-%d") but need to handle missing.

Also TRTSDT, TRTEDT come from ADSL. Need to check their format. In ADSL they are likely numeric SAS dates. When reading via haven, they will be Date objects? Need to check.

We need to ensure we handle haven labelled etc.

We need to produce adae.csv with same variable names uppercase.

We need to also produce P004.lst text rendering and CSV.

We need to decide what to include in listing. The SAS program's proc print prints 15 obs with those vars. So we should replicate that.

We need to also ensure we handle sorting: SAS sorts with character ordering uppercase before lowercase. R's default sort is similar? Need to ensure.

We need to implement round-half-up helper though not needed here.

We need to handle missing values sort first.

We need to check actual data files.

Let's list directories.