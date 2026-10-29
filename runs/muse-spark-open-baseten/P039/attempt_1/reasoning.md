Need to translate SAS macro SUPP2PAR_v1 to R. Expected output dataset aefull -> OUT_DIR/aefull.csv

We need to understand logic. The macro merges supplemental qualifiers onto parent domain.

We have inputs: inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam ??? But driver uses psdtm lib? Wait family environment says inlib -> derived adam. But driver calls %SUPP2PAR_v1(inlib=psdtm, parent=AE, supp=SUPPAE, outlib=outds, outname=AEFULL);

We need to locate psdtm, outds librefs. They are defined by benchmark before program ran? The context says macro variables &rawadam etc, and librefs in effect: inlib -> ... But psdtm and outds not listed. Possibly benchmark sets them? Need to infer paths.

We have &rawsdtm = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
&rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
&rawsplit etc
&outds = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P039

But psdtm likely points to SDTM data. Could be D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm ? Let's explore filesystem.

We need to write R script that reads inputs from absolute paths given. We need to discover files.

We should list directories.

We need to produce aefull.csv matching SAS output. Need to replicate SAS logic exactly.

We need to implement SUPP2PAR logic in R.

Steps summarized:

- Validate inlib/outlib existence, parent exists, not empty, supp exists etc. For R, we just need to implement core merging.

- Check required variables: parent requires STUDYID, DOMAIN, USUBJID. Supp requires STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QVAL, QLABEL.

- Filter supp where RDOMAIN = upcase(parent) . If empty -> copy parent.

- Check supp records where USUBJID or RDOMAIN or QNAM empty -> ignore, continue.

- Check uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM -> if duplicates terminate (error). For R we should handle.

- Check same QNAM occurs with different QLABEL -> terminate.

- Determine length of USUBJID etc not needed in R.

- Create S2P_SUPP: if IDVAR == '' and IDVARVAL == '' then IDVAR='USUBJID', IDVARVAL=USUBJID. If IDVAR == '' and IDVARVAL != '' -> error termination (RC 8).

- Check QNAM already on parent: if QNAM value equals a variable name on parent, rename QNAM to QNAM_2 (append _2). Need to handle.

- Check QNAM valid SAS variable name: not needed but warn.

- Determine distinct IDVAR values in supp where RDOMAIN = parent. For each IDVAR value, determine type (numeric vs character) based on parent variable type. If IDVAR not present on parent -> error.

- For each IDVAR group:
   - Create subset where IDVAR == that value and RDOMAIN == parent.
   - Convert IDVARVAL to appropriate type: if numeric, input as numeric; if character, keep as is. Create new column named IDVAR with value.
   - Sort by STUDYID RDOMAIN USUBJID IDVAR (if not USUBJID)
   - Transpose: by STUDYID DOMAIN USUBJID IDVAR, var qval, id qnam, idlabel qlabel. So each QNAM becomes column with QVAL value.
   - Sort parent by same keys
   - Merge parent and transposed supp by keys, keeping all parent records, and capturing supp-only drops.

- Determine if parent has SEQ variable (parent.SEQ e.g., AESEQ). Check existence.

- For each i, sort S2P_parent_STEP_i by STUDYID DOMAIN USUBJID [SEQ if exists]

- Merge all STEP datasets together by same keys (full merge? Actually SAS merge with by will combine). Need to understand: If multiple IDVAR groups, they are merged together. Example AE may have IDVAR = AESEQ and maybe USUBJID? Let's check typical SUPPAE.

- Then create DROP dataset combining all drops.

- Output final dataset.

We need to implement this in R.

We need to find actual data to test.

Let's explore file system via R? But we are to write script; we can attempt to infer but better to run exploratory commands mentally? We can attempt to list files using R code in analysis, but we are the assistant producing final script. We need to ensure script works regardless of exact file locations.

We need to handle reading SDTM AE and SUPPAE. Where are they?

Possibly psdtm libref points to D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm . Let's check typical pilot SDTM files: ae.xpt, suppae.xpt etc.

We should write R code that tries multiple possible locations:

- &rawsdtm directory
- &rawadam etc
- inlib directory

We should search for AE and SUPPAE files.

We need to handle .xpt, .sas7bdat, .csv.

We should implement helper to find file.

Also need to handle variable names case-insensitive but preserve.

We need to replicate SAS sorting and merging semantics.

Key details:

- SAS merge by STUDYID DOMAIN USUBJID IDVAR etc. Need to ensure character comparison case-sensitive, missing handling.

- Transpose: QNAM values become column names. QVAL is character. Need to handle duplicate QNAM per key? But uniqueness ensures one per key.

- QLABEL becomes variable label (SAS label). In R we can set attribute label.

- If QNAM already exists on parent, rename to QNAM_2. Need to handle case where QNAM upcase? SAS variable names are case-insensitive but stored uppercase? In SDTM, variables are uppercase. QNAM values are uppercase. So check.

- IDVAR handling: For AE, IDVAR likely "AESEQ". So IDVAR column value is "AESEQ", IDVARVAL is sequence number as character. Need to convert to numeric if parent AESEQ is numeric.

- For USUBJID case: IDVAR='USUBJID', IDVARVAL=USUBJID, then IDCLN is empty, so by keys are STUDYID RDOMAIN USUBJID only.

We need to implement steps faithfully.

Also need to handle RDOMAIN rename to DOMAIN after transpose.

We need to produce aefull.csv with same columns as SAS output. Need to ensure column order: parent columns first, then new QNAM columns in order encountered? SAS proc transpose will create columns in order of QNAM appearance? Need to check.

We should attempt to replicate column order: parent variables in original order, then transposed variables in alphabetical? Actually SAS proc transpose id order is order of appearance in data? Let's assume order of QNAM sorted? Need to check typical.

We can attempt to mimic SAS: proc transpose with by and id qnam will create columns in order of QNAM values as they appear sorted? In SAS, without sorting, the order is order of data. But we have sorted supp by STUDYID RDOMAIN USUBJID IDVAR. So QNAM order within each by group may vary. However SAS transpose will create columns for all distinct QNAM values across entire dataset, in order of first appearance.

We can implement by pivoting wider: distinct QNAM values, order by first appearance.

We also need to handle QNAM_2 renaming.

We need to handle multiple IDVAR groups merging: final merge by STUDYID DOMAIN USUBJID [SEQ]. This will combine columns from different IDVAR groups. For AE, likely only one IDVAR (AESEQ), so final is just one STEP.

We need to implement drop handling: records in supp that have no parent match are dropped and stored in SUPPAE_DROP dataset (not required output, but we could ignore).

We need to produce aefull.csv.

We should also handle case where supp is empty after RDOMAIN filter -> copy parent.

We need to write R script that does all this and writes CSV.

We must also handle reading with haven::read_xpt etc.

We need to ensure output directory exists.

We need to produce script that runs non-interactively.

We should include helper functions for SAS-like handling: trim, upcase, etc.

We need to handle missing values: SAS missing character is "" (empty string) or NA? In R, haven reads missing as NA or ""? Need to treat both as empty.

We need to handle IDVARVAL conversion: if parent variable is numeric, convert IDVARVAL to numeric via as.numeric. If fails, NA.

We need to handle sorting: SAS sorts character with uppercase before lowercase, missing first. R's default sort is similar but need to ensure.

We should implement merge as left join.

We need to handle final sorting: by STUDYID DOMAIN USUBJID AESEQ if exists.

We should also preserve variable labels? Not needed for CSV but could.

We need to write script that logs progress.

We should attempt to locate files: Let's assume psdtm files are in D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm . List files: ae.xpt, suppae.xpt.

We can write code to search recursively.

We should also handle inlib path for fallback.

We need to produce final R script.

We need to ensure we handle case sensitivity of variable names: SAS is case-insensitive, but R is case-sensitive. We should work with uppercase variable names for matching, but preserve original names for output.

Simplify: Convert all column names to uppercase for processing, then output with uppercase names (SAS style). That will match expected.

We need to check expected output dataset aefull column names: likely uppercase.

We should implement steps:

1. Determine out dir: Sys.getenv("OUT_DIR") else temp.

2. Find AE and SUPPAE files.

Helper find_dataset(lib_path, name): look for files named name.xpt, name.sas7bdat, name.csv case-insensitive in directory.

We need to know lib paths: psdtm = raw sdtm, outds = output, inlib = derived adam. But for this macro, inlib is psdtm. So we need to locate psdtm directory. We can try:

- Sys.getenv("RAWSDTM")? But macro variables are not env vars. We have to hardcode paths given in context: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm etc.

We can try multiple candidates:

candidates_psdtm = c("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
                     "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
                     Sys.getenv("RAWSdtm"), Sys.getenv("RAWSDTM"))

We should search all.

Better to search recursively under D:/meta_hackathon/sas2r-agent/data for ae.xpt.

We can implement search function that lists files recursively.

We should also handle that haven::read_xpt may fail if file is not xpt.

We need to implement reading function read_sas_dataset(path): if ends with .xpt -> read_xpt, .sas7bdat -> read_sas, .csv -> read_csv.

We need to handle haven labelled vectors: convert to character/numeric appropriately, using haven::as_factor? Better to use haven::zap_labels etc. But we want to preserve values.

We can use haven::read_xpt with .name_repair.

We should convert labelled to plain: use haven::zap_labels? Or keep.

Simplify: after reading, convert all haven_labelled to character if needed.

We need to handle dates: SAS dates are numeric days since 1960-01-01. haven will read as numeric with labels. We should keep as is.

But for CSV output, we need to write dates as YYYY-MM-DD if they are Date objects. haven may read dates as Date? Actually read_xpt will convert SAS date formats to Date? Not necessarily. We'll just write as is.

We need to ensure we don't round.

We should implement core logic in R functions.

Let's draft script outline.

