Need to translate SAS program P039_Supp2Par_v1.sas to R.

We need to understand what it does: macro SUPP2PAR_v1 merges supplemental qualifiers onto parent domain.

Inputs: inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam but actually psdtm? Wait librefs: inlib -> derived adam, but program uses psdtm? Let's see benchmark driver: %SUPP2PAR_v1(inlib=psdtm, parent=AE, supp=SUPPAE, outlib=outds, outname=AEFULL);

So psdtm libref? Not defined in family environment snippet, but likely defined elsewhere. Need to locate actual data files.

We have environment variables: &root = D:/meta_hackathon/sas2r-agent, &rawadam, &rawsdtm, &rawsplit, &rawdir, &outtlf, &outlog, &outds.

But psdtm likely points to SDTM data. Let's explore filesystem.

We need to produce script that reads inputs from absolute paths given in context (forward slashes). SAS transport files (.xpt) and SAS datasets (.sas7bdat) are read with haven.

We need to find where AE and SUPPAE are.

Potential locations:
- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
- D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

We need to list files.

We have OUT_DIR env var.

We need to produce aefull.csv.

We need to replicate SAS logic faithfully.

Let's think about steps:

Macro does:

- Validate inlib, outlib, parent exists, not empty, supp exists, not empty, outname valid.
- Check parent requires STUDYID, DOMAIN, USUBJID
- Supp requires STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QVAL, QLABEL
- Filter supp where RDOMAIN = upcase(parent) -> S2P_RDOMCHECK, if empty then copy parent.
- Contents dataset for parent
- Checks on supplemental:
  - Records where USUBJID or RDOMAIN or QNAM empty -> ignore, output to &supp._IGNOR, rest to S2P_&supp._Accept
  - Check uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM -> if duplicates terminate
  - Check same QNAM occurs with different labels -> terminate
- Creating version in WORK: determine length of USUBJID and IDVAR, then data S2P_&SUPP: if IDVAR='' and IDVARVAL='' then IDVAR='USUBJID', IDVARVAL=USUBJID; else if IDVAR='' and IDVARVAL ne '' then terminate (RC 8)
- Check for QNAM already on parent: if QNAM value equals a variable name on parent, rename QNAM to QNAM_2
- Check for illegal variable name in QNAM (nvalid or length>8) -> warning but continue (PROC TRANSPOSE will correct)
- Determine IDVAR values in supplemental where RDOMAIN=parent: proc freq table idvar -> IDMAX and IDn
- For each IDVAR, determine type (N or C) from parent contents, check if IDVAR variable exists on parent, if not terminate
- For each i:
  - Create S2P_&parent._SUPP&i: set S2P_&SUPP where IDVAR=IDi and RDOMAIN=parent, then create variable named IDi = input(IDVARVAL,best.) if numeric else IDVARVAL
  - Sort by STUDYID RDOMAIN USUBJID IDCLN (IDCLN is IDi unless IDi=USUBJID then empty)
  - Transpose: by STUDYID DOMAIN USUBJID IDCLN, var qval, id qnam, idlabel qlabel, drop _NAME_ _LABEL_, rename RDOMAIN to DOMAIN
  - Sort parent by STUDYID DOMAIN USUBJID IDCLN
  - Merge parent and transposed supp by those keys, output to S2P_&parent._STEP_&i if PARENT, else to S2P_&supp.DROP_&i if SUPP without PARENT
- Determine if --SEQ variable exists on parent (parent.SEQ)
- For each i, sort S2P_&parent._STEP_&i by STUDYID DOMAIN USUBJID [parent.SEQ if exists]
- Merge all STEP datasets together by same keys into S2P_FINAL_&parent
- Set all DROP datasets together into &supp._DROP
- Output dataset: outlib.outname = S2P_FINAL_&parent
- Determine drops exist -> warning
- Copy termination: if S2P_ParCopy then output is simple copy of parent
- Return code handling, cleaning

We need to replicate this in R.

We need to find actual AE and SUPPAE data to test.

We should write R script that:

- Locates AE and SUPPAE files. Could be .xpt or .sas7bdat or .csv.

We need to explore filesystem via R? But we are to write script that will run in environment. We need to make script robust to find files.

We have paths:

- rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
- rawsdtm = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
- rawsplit = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
- rawdir = D:/meta_hackathon/sas2r-agent/data/raw
- inlib = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

But psdtm libref not defined in snippet, but likely points to SDTM. Could be D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm or derived.

We need to search for AE files.

We should write R code that tries multiple locations.

Also need to handle reading .xpt, .sas7bdat, .csv.

We need to replicate SAS behavior for merging.

Key details:

- SAS handling of character vs numeric, case sensitivity, sorting.

- SAS transpose: QNAM becomes variable names, QVAL values. If QNAM illegal, SAS will correct via nvalid. In R, we need to emulate? But likely QNAMs are valid.

- SAS merge: by STUDYID DOMAIN USUBJID [IDVAR]. It's a one-to-one or one-to-many? Parent may have multiple records per USUBJID, with AESEQ. Supplemental records are per parent record identified by IDVAR/IDVARVAL. For AE, IDVAR is AESEQ, IDVARVAL is the sequence number. So merge key includes AESEQ.

- Need to handle IDVAR type: if parent variable is numeric, then IDVARVAL is converted via input(..., best.). In AE, AESEQ is numeric.

- Also need to handle IDVAR='' case: replaced with USUBJID.

- Need to handle QNAM already on parent: rename to _2.

- Need to handle sorting: SAS sorts character ordering uppercase before lowercase. R default sort is similar? Need to ensure.

- Need to handle missing values sort first.

- Need to produce aefull.csv with same variable names, dates as YYYY-MM-DD, etc.

We need to inspect actual data to know columns.

We should attempt to list files in the environment. Since we cannot run code here, we need to infer.

We can write R script that does file discovery at runtime.

We need to produce script that:

- Sets out <- Sys.getenv("OUT_DIR")
- Finds AE and SUPPAE

We should search in:

- Sys.getenv("rawsdtm") if exists, else default path
- Sys.getenv("root") etc.

But the instructions say: Read inputs from the absolute paths given in the context (forward slashes). SAS transport files (.xpt) and SAS datasets (.sas7bdat) are read with haven::read_xpt / haven::read_sas; Excel with readxl::read_excel; CSV with readr or base read.csv.

So we should use those absolute paths: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm etc.

We need to handle both .xpt and .sas7bdat.

We should write helper function to find file: list.files with pattern.

We also need to handle haven reading: haven::read_xpt, haven::read_sas.

We need to handle character conversion: haven imports with labelled vectors; need to convert to character/numeric appropriately, and handle missing.

We need to replicate SAS dataset structure: variable names uppercase.

We should also handle that AE and SUPPAE may be in psdtm libref which could be D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm? Let's check typical phuse structure: data/derived/cdisc-pilot/adam contains ADaM datasets, data/raw/cdisc-pilot/sdtm contains SDTM. So psdtm likely points to raw sdtm.

We should search multiple directories.

We also need to handle that outds is D:/meta_hackathon/sas2r-agent/output/sas/datasets/P039, but we write to OUT_DIR.

We need to produce aefull.csv.

We need to ensure we replicate SAS semantics for dates: AE has dates like AEDTC, etc. Need to write as YYYY-MM-DD.

We need to handle numeric values unrounded unless SAS rounds them.

We need to handle haven's handling of dates: SAS dates are numeric with format.

We need to write CSV with na = "".

We should also handle that SAS output dataset includes all parent variables plus transposed QNAM variables.

We need to implement transpose logic: For each IDVAR group, pivot wider.

Simplify: Instead of looping per IDVAR, we can do overall: For each distinct IDVAR, create separate transpose and merge, then merge all together. But we can also do single step: For each IDVAR, the merge key is STUDYID, DOMAIN, USUBJID, plus IDVAR value. However, if there are multiple IDVAR values, the final merge is by STUDYID DOMAIN USUBJID AESEQ (if SEQ exists) merging all STEP datasets. This is essentially merging on the common keys, with each STEP dataset contributing its own QNAM columns.

But we can implement more directly: For each IDVAR group, pivot and merge, then full join all.

We need to handle IDVAR=USUBJID case: then key is just STUDYID DOMAIN USUBJID (no extra).

We need to handle IDVAR type conversion: If parent variable is numeric, convert IDVARVAL to numeric via as.numeric.

We need to handle IDVARVAL empty substitution.

We need to handle QNAM renaming.

We need to handle duplicate check: If duplicates exist, SAS terminates. In R we should check and stop if duplicates.

We need to handle empty QNAM/RDOMAIN/USUBJID filtering.

We need to handle RDOMAIN filter.

We need to handle SEQ detection.

We need to handle final sorting.

We should write R script that logs progress.

We need to ensure we handle haven's column names: they may be lower case? But SAS names are uppercase. We should convert to uppercase for processing, but preserve original case for output? The instruction says write CSV with SAS dataset's variable names (same case is fine, matching is case-insensitive). So we can output uppercase.

We need to handle haven's labelled vectors: convert to character.

We should write helper to convert haven data to data.frame with proper types.

We need to handle that haven::read_xpt may produce tibble with haven_labelled.

We should use haven::read_xpt and then mutate across where haven::is.labelled to as.character or as.numeric.

Better to use haven::read_xpt and then use haven::zap_labels, zap_formats.

We should also handle that some columns may be numeric SAS dates: need to convert to Date if format indicates date. But we can just keep as is and write as is; but instruction says dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS. So we need to detect date columns.

We can check if column has class Date or POSIXct after reading.

Alternatively, we can just write whatever haven gives, but ensure dates are ISO.

We need to handle that AE dataset may have character dates like AEDECOD etc.

We should implement generic date handling: If column is Date, format as "%Y-%m-%d", if POSIXct, format as "%Y-%m-%dT%H:%M:%S".

We need to handle that haven may read SAS dates as numeric with attributes; we need to convert.

We should use haven::read_sas with appropriate.

We need to write script that is standalone.

We should also handle that the SAS program's expected output is dataset aefull -> write OUT_DIR/aefull.csv

We need to ensure we create OUT_DIR if not exists.

We should also handle that the SAS program may have produced aefull with specific columns. We need to replicate exactly.

We need to infer what SUPPAE contains. Let's try to recall CDISC pilot SDTM SUPPAE: It contains supplemental qualifiers for AE, with QNAM like AESEV, AEREL, etc? Actually AE already has those, but SUPPAE may have additional.

We can search memory: CDISC pilot SDTM AE has variables: STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AEBODSYS, AESEV, AESER, AEACN, AEREL, AEOUT, AESTDTC, AEENDTC, etc. SUPPAE may have QNAM like AETRTEM, etc.

We need to handle transpose: QNAM values become new columns.

We should implement logic to handle QLABEL as variable label, but not needed for CSV.

We need to handle that QVAL is character, but may need to keep as character.

We need to handle that after transpose, the new columns are character.

We need to handle that if QNAM already exists on parent, rename to _2.

We need to handle that if QNAM is invalid SAS name, SAS will correct via PROC TRANSPOSE. In R, we need to make valid names? But we can just use QNAM as is, but ensure we handle.

We should implement check for QNAM length >8 or invalid: warning, but continue.

We need to handle that SAS PROC TRANSPOSE with ID QNAM will create variables named after QNAM values, with correction for invalid names (e.g., adding _). We can emulate by using make.names? But likely not needed.

We should write R code that does pivot_wider with names_from = QNAM, values_from = QVAL.

We need to handle that QNAM values are uppercase, and parent variable names are uppercase, so comparison is case-insensitive? SAS does upcase for RDOMAIN check, but for QNAM check, it does merge by NAME vs QNAM, with NAME from contents (which is uppercase). QNAM values may be uppercase. We should do case-sensitive? SAS is case-sensitive for variable names but QNAM values are typically uppercase. We should do exact match after upcasing QNAM? In SAS code, they do: proc sort data=S2P_contents_&parent out=S2P_QNAM_PAR(keep=NAME) by NAME; and proc sort nodupkey data=S2P_&supp._Accept out=S2P_QNAM_SUP (keep=QNAM) by QNAM; then merge by NAME (QNAM renamed to NAME). This is case-sensitive? SAS variable names are uppercase, QNAM values are uppercase, so it's case-sensitive but both uppercase.

We should do case-sensitive after trimming.

We need to handle that SAS does strip(QNAM) ne '' check.

We need to handle that SAS does nvalid check: nvalid(strip(QNAM)) checks if valid SAS variable name. In R, we can check with regex: ^[A-Za-z_][A-Za-z0-9_]*$ and length <=32? But SAS says length >8 is warning for XPT V5. We can just warn.

We need to handle that SAS does length(trim(left("&OUTNAME"))) gt 32 check.

We need to handle that SAS does prxmatch('/\W/',trim(left("&OUTNAME"))) check.

We should implement similar.

We need to handle that SAS does call symput for IDVARLEN etc.

We need to handle that SAS does data S2P_&SUPP with length IDVARVAL &USUBLEN IDVAR &IDVARLEN; This ensures IDVARVAL length matches USUBJID length to avoid truncation. In R, we don't need to worry about length truncation, but we should ensure we handle character lengths.

We need to handle that SAS does if IDVAR='' and IDVARVAL='' then IDVAR='USUBJID', IDVARVAL=USUBJID; This is for records where IDVAR is missing, they are at subject level, not record level. So they should be merged by USUBJID only.

We need to handle that SAS does if IDVAR='' and IDVARVAL ne '' then terminate (RC 8). This is error case.

We need to handle that SAS does proc freq for IDVAR values where RDOMAIN=parent, then creates IDn macrovariables.

We need to handle that SAS does determining TYPE for each IDVAR from parent contents: type=1 numeric, type=2 character.

We need to handle that SAS does if IDVAR not present on parent, terminate.

We need to handle that SAS does for each IDVAR, create S2P_&parent._SUPP&i with variable named IDi = input(IDVARVAL,best.) if numeric else IDVARVAL.

We need to handle that SAS does proc sort and transpose.

We need to handle that SAS does merge parent and transposed supp, output to STEP and DROP.

We need to handle that SAS does final merge of all STEP datasets by STUDYID DOMAIN USUBJID [SEQ].

We need to handle that SAS does final output.

We should implement this in R.

We need to handle that SAS does sorting with missing values first.

We need to handle that SAS does handling of STUDYID, DOMAIN, USUBJID as keys.

We need to handle that SAS does handling of --SEQ variable: if parent has variable named &parent.SEQ (e.g., AESEQ), then final sort and merge uses that.

We should detect if parent has column named paste0(parent, "SEQ") e.g., AESEQ.

We need to handle that SAS does handling of IDCLN: if IDi ne USUBJID then IDCLN = IDi else empty. So for USUBJID, the by variables are STUDYID RDOMAIN USUBJID (without extra), for others, include IDVAR.

We need to handle that SAS does transpose by STUDYID DOMAIN USUBJID IDCLN.

We need to handle that SAS does merge by same.

We need to handle that SAS does final merge by STUDYID DOMAIN USUBJID [SEQ] (not including IDVAR). Wait final merge is by STUDYID DOMAIN USUBJID &parent.SEQ if exists, else by STUDYID DOMAIN USUBJID. This merges the different IDVAR subsets together. But how does that work if different IDVAR subsets have different keys? For AE, IDVAR is AESEQ, so all records are by AESEQ, so there's only one IDVAR value, so final merge is trivial. If there were multiple IDVAR values (e.g., USUBJID and AESEQ), then merging by USUBJID and AESEQ would combine subject-level and record-level supplemental data.

We need to implement final merge as full join by common keys.

We should implement loop for each IDVAR:

- For each IDVAR value, create supp subset, convert IDVARVAL to appropriate type, create key column, pivot, merge with parent.

But we can simplify: For each IDVAR, we can create a wide supp dataset, then left join to parent.

Then we need to combine all wide supp datasets: they will have different keys, but we can left join each to parent sequentially, or merge all STEP datasets.

Simpler: Start with parent as base, then for each IDVAR, left join the transposed supp to parent by appropriate keys, producing intermediate dataset, then combine.

But SAS does: For each IDVAR, merge parent with transposed supp to create STEP dataset (which is parent plus supp columns for that IDVAR). Then final merge of all STEP datasets by SEQ. This is equivalent to left joining each supp to parent, then merging the results.

We can implement as: parent_base = parent

For each IDVAR i:
  supp_i = supp filtered to IDVAR=i and RDOMAIN=parent
  supp_i_wide = pivot wider by keys
  parent_i = left_join(parent_base, supp_i_wide, by = keys_i)

Then final = Reduce(full_join, parent_i list) by SEQ keys? But if we left_join each to parent_base, then each parent_i has same number of rows as parent_base (plus maybe extra columns), so merging them by SEQ should be just combining columns.

We can do: final = parent_base
For each i:
  final = left_join(final, supp_i_wide, by = keys_i)

But need to handle that supp_i_wide may have different keys: For USUBJID, keys = STUDYID, DOMAIN, USUBJID; for AESEQ, keys = STUDYID, DOMAIN, USUBJID, AESEQ.

If we left_join sequentially, it will work.

We need to handle that SAS does merge with by STUDYID DOMAIN USUBJID IDCLN, but final merge is by STUDYID DOMAIN USUBJID SEQ. So for AESEQ case, the intermediate STEP dataset already has parent plus supp columns, sorted by SEQ, and final merge just merges them (but there's only one).

We should implement final as left_join loop.

We need to handle that SAS does handling of DROP datasets: records in supp that have no parent are output to DROP. In R, we can identify those as anti_join.

We need to handle that SAS does handling of IGNOR dataset: records where USUBJID or RDOMAIN or QNAM empty are ignored.

We need to handle that SAS does handling of duplicate check: if duplicates on USUBJID RDOMAIN IDVAR IDVARVAL QNAM, terminate.

We need to handle that SAS does handling of QNAM label uniqueness: if same QNAM has different QLABEL, terminate.

We need to handle that SAS does handling of QNAM already on parent: rename to _2.

We need to handle that SAS does handling of illegal QNAM: warning.

We need to handle that SAS does handling of IDVAR empty substitution.

We need to handle that SAS does handling of RDOMAIN filter.

We need to handle that SAS does handling of parent copy if supp missing or empty or RDOMAIN filter empty.

We should implement all these checks.

We need to handle that SAS does handling of outlib default to WORK if not exists.

We need to handle that SAS does handling of outname validation.

We should implement similar.

We need to handle that SAS does handling of return code.

We should just implement core logic and produce aefull.csv.

We need to write R script that does all this.

We should also handle that the input files may be in different formats: .xpt, .sas7bdat, .csv.

We should write helper function find_and_read(dataset_name, lib_paths) that searches for file.

We should search in:

- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
- D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
- D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm (if exists)
- D:/meta_hackathon/sas2r-agent/data/raw
- D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split

We should also check for psdtm libref: maybe it's defined as D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm

We should use Sys.getenv to get paths, but fallback to absolute.

We should write function to list files recursively.

We should handle that haven::read_xpt expects .xpt file, haven::read_sas expects .sas7bdat.

We should also handle that some files may be .csv.

We should write function read_sas_dataset(name, paths) that tries to find file with name.xpt, name.sas7bdat, name.csv, case-insensitive.

We should also handle that the file may be in a subdirectory.

We should implement reading with haven, then convert.

We need to handle that haven may produce tibble with haven_labelled; we should zap.

We should also handle that the dataset may have variable labels.

We should write helper to convert haven data to data.frame with proper types.

We need to handle that SAS datasets may have numeric dates with format; haven will read as numeric with attributes, but we need to convert to Date if format is DATE.

We can check attributes: if column has format like "DATE" or "DATETIME", convert.

But simpler: we can just keep as is and write as is; but instruction says dates as YYYY-MM-DD.

We should attempt to detect date columns by name pattern: *DTC, *DT, etc. But better to check haven's format.

We can use haven::read_sas with col_select.

We should implement function to convert SAS dates: If column is numeric and has attribute "format.sas" containing "DATE" or "DATETIME", convert.

We can use: attr(col, "format.sas").

We should handle that haven::read_xpt may already convert dates to Date if format is known.

We should test.

We need to write R script that is robust.

We should also handle that the output dataset aefull should have same columns as SAS output, with same order? SAS output order is parent variables first, then transposed QNAM variables in order of appearance? PROC TRANSPOSE will create variables in order of QNAM values sorted? Actually PROC TRANSPOSE with BY and ID will create variables in order of QNAM values as they appear in data, but with sorting? The SAS code does proc sort by STUDYID RDOMAIN USUBJID IDCLN before transpose, but not sorting by QNAM. So QNAM order is as encountered.

In R, pivot_wider will create columns in order of appearance or sorted? We should ensure we preserve order.

We should also handle that SAS does idlabel QLABEL to apply labels, but not needed.

We need to handle that SAS does drop _NAME_ _LABEL_.

We should implement pivot_wider with names_from = QNAM, values_from = QVAL.

We need to handle that QVAL is character, but may be numeric as character.

We should ensure we handle that QNAM values are uppercased? In SAS, they do QNAM = upcase(QNAM) for checking, but not for transpose? Actually they do QNAM = upcase(QNAM) in S2P_SUPPFREQ_&parent dataset for checking, but not for transpose. However, they also do check for QNAM already on parent by merging NAME (which is uppercase) with QNAM (which is as is, but they rename QNAM to NAME and merge by NAME, which is case-sensitive? But they do not upcase QNAM for that check, but they do for troublelist. However, they do upcase for RDOMAIN check: RDOMAIN = "%upcase(&parent)".

We should handle QNAM as is, but for checking already on parent, we should do case-insensitive? SAS variable names are uppercase, QNAM values are typically uppercase, so it's fine.

We should implement QNAM already on parent check: if any QNAM value (after trimming) equals any parent variable name (case-insensitive), then rename that QNAM to QNAM_2.

We need to handle that SAS does: data S2P_&SUPP.; length QNAM $10.; set S2P_&SUPP.; if QNAM = "NAME" then QNAM = "NAME_2"; This is done via call execute generating code for each duplicate.

We should implement: For each QNAM that is already on parent, rename it to paste0(QNAM, "_2").

We need to handle that SAS does length QNAM $10. to allow _2 suffix (max 8+2=10).

We should do same.

We need to handle that SAS does check for QNAM illegal: nvalid(strip(QNAM)) or length>8. We should warn.

We need to handle that SAS does handling of IDVAR type: type=1 numeric, type=2 character. We need to determine type from parent dataset. In R, we can check if parent column is numeric or character.

We need to handle that SAS does input(trim(left(IDVARVAL)),best.) for numeric IDVAR. In R, we can do as.numeric(IDVARVAL).

We need to handle that SAS does handling of IDVARVAL length: they set length IDVARVAL &USUBLEN to avoid truncation. In R, we don't need.

We need to handle that SAS does handling of IDVAR empty: if IDVAR='' and IDVARVAL='' then IDVAR='USUBJID', IDVARVAL=USUBJID.

We should implement.

We need to handle that SAS does handling of IDVAR empty with IDVARVAL not empty -> terminate.

We should implement check and stop.

We need to handle that SAS does handling of duplicate check: proc sort nodupkey dupout.

We should implement duplicate check: group by USUBJID, RDOMAIN, IDVAR, IDVARVAL, QNAM and check for duplicates.

We need to handle that SAS does handling of QNAM label uniqueness: proc freq QNAM*QLABEL, then check if same QNAM has different QLABEL.

We should implement: group by QNAM, check n_distinct(QLABEL) >1.

We need to handle that SAS does handling of RDOMAIN filter: S2P_RDOMCHECK where RDOMAIN = upcase(parent). If empty, copy parent.

We should implement.

We need to handle that SAS does handling of parent copy: if supp missing or empty or RDOMAIN filter empty, output is simple copy of parent.

We should implement.

We need to handle that SAS does handling of final output: data outlib.outname set S2P_FINAL_&parent.

We should implement.

We need to handle that SAS does handling of cleaning: delete S2P_ datasets if clean=Y.

We don't need.

We need to handle that SAS does handling of return code.

We don't need.

We should write R script that logs to console.

We need to ensure we handle that the input files may be in different locations, and we need to find them.

We should write helper function to find file:

find_file <- function(name, dirs) {
  for (dir in dirs) {
    if (!dir.exists(dir)) next
    files <- list.files(dir, recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
    # Look for exact match with extension
    pattern <- paste0("^", name, "\\.(xpt|sas7bdat|csv)$")
    matches <- files[grepl(pattern, basename(files), ignore.case=TRUE)]
    if (length(matches) > 0) return(matches[1])
    # Also look for name without extension? 
  }
  return(NA)
}

We should also handle that the file may be named ae.xpt, suppae.xpt, etc. Lowercase.

We should search case-insensitive.

We should also handle that the file may be in a subdirectory with same name.

We should implement.

We need to handle that the script must run non-interactively with Rscript on Windows. Never call install.packages(), never prompt, never read from stdin, never open graphics windows.

We should use only allowed packages.

We should write script that uses haven, dplyr, tidyr, etc.

We need to ensure we handle that the script writes to OUT_DIR.

We should get out <- Sys.getenv("OUT_DIR"); if out == "" then out <- "output" or temp.

We should create dir if not exists.

We should write CSV with write.csv(..., row.names=FALSE, na="").

We should handle dates: format as YYYY-MM-DD.

We should handle that the output dataset may have many columns, we need to ensure we write all.

We should also handle that the SAS output may have specific column order: parent columns first, then new QNAM columns. We should preserve that.

We should implement final output ordering: parent columns in original order, then new columns in order of appearance.

We should also handle that the SAS output may have variable labels, but we don't need.

We should write R script that does all.

We need to ensure we handle that the input datasets may have been read with haven, which may have converted some columns to labelled.

We should implement conversion:

convert_haven <- function(df) {
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  df <- haven::zap_label(df)
  # Convert haven_labelled to appropriate
  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled")) {
      df[[col]] <- haven::zap_labels(df[[col]])
    }
  }
  return(df)
}

We should also handle that some columns may be numeric with SAS date format, but haven may have already converted to Date if format is DATE9. etc. We should check.

We should also handle that the dataset may have character columns with trailing spaces: SAS trims? We should trim.

We should implement helper to trim whitespace for character columns.

We should also handle that SAS does strip() for checks, which removes leading/trailing spaces.

We should do same.

We need to handle that SAS does upcase for RDOMAIN check: RDOMAIN = "%upcase(&parent)" -> parent is AE, so RDOMAIN = "AE". So we filter supp where RDOMAIN == "AE" (case-sensitive? SAS does where RDOMAIN = "AE", which is case-sensitive, but RDOMAIN values are typically uppercase).

We should do toupper(RDOMAIN) == "AE".

We should implement.

We need to handle that SAS does handling of STUDYID, DOMAIN, USUBJID as keys. These are character.

We need to handle that SAS does handling of IDVAR values: they are variable names, like "AESEQ". So we need to get unique IDVAR values.

We should implement.

We need to handle that SAS does handling of IDVAR type: For each IDVAR, determine if parent variable is numeric or character.

We should implement.

We need to handle that SAS does handling of IDVAR conversion: For numeric, input(IDVARVAL,best.) -> as.numeric.

We should implement.

We need to handle that SAS does handling of transpose: by STUDYID DOMAIN USUBJID IDCLN, var qval, id qnam.

We should implement pivot_wider.

We need to handle that SAS does handling of merge: by STUDYID DOMAIN USUBJID IDCLN.

We should implement left_join.

We need to handle that SAS does handling of final merge: by STUDYID DOMAIN USUBJID SEQ.

We should implement.

We should write R script that does all.

We need to ensure we handle that the output dataset may have been produced by SAS with specific handling of missing values: SAS missing values sort first, character missing is "".

We should handle.

We should write R script that is complete.

We need to ensure we handle that the script must be inside one ```r code block.

We should produce final answer with R code.

We need to ensure we handle that the script must not call install.packages().

We should use only allowed packages.

We should write script that is robust.

We should also handle that the script may need to handle that the input files are in .xpt format, which haven::read_xpt can read.

We should implement reading function:

read_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types=FALSE)
  } else {
    stop("Unknown extension")
  }
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  # Convert to data.frame
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  # Convert column names to uppercase for consistency
  names(df) <- toupper(names(df))
  return(df)
}

We should also handle that the dataset may have lowercase names, but we convert to uppercase.

We should also handle that the dataset may have variable names with special characters, but we handle.

We need to handle that the output CSV should have variable names as in SAS dataset (same case is fine, matching is case-insensitive). So we can output uppercase.

We should ensure we handle that the output dataset may have been produced with specific column order, but we can output in any order? The instruction says write every output into directory with SAS dataset's variable names (same case is fine, matching is case-insensitive). So order may not matter, but we should try to preserve.

We should write script that logs progress.

We should handle that the script must handle that the input files may not be found, then we should try alternative paths.

We should implement search in multiple dirs.

We should also handle that the script may need to handle that the input files are in a zip or something, but likely not.

We should write script that is complete.

We need to ensure we handle that the script must handle that the output directory may not exist, create it.

We should implement.

We should also handle that the script must handle that the output dataset may have been produced with specific handling of dates: we need to format dates as YYYY-MM-DD.

We should implement helper to format dates:

format_dates <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], "Date")) {
      df[[col]] <- format(df[[col]], "%Y-%m-%d")
    } else if (inherits(df[[col]], "POSIXct") || inherits(df[[col]], "POSIXt")) {
      df[[col]] <- format(df[[col]], "%Y-%m-%dT%H:%M:%S")
    } else if (is.numeric(df[[col]]) && grepl("DTC|DT$", col)) {
      # Maybe SAS date numeric, but we don't know
    }
  }
  return(df)
}

But we need to handle that haven may have read SAS dates as numeric with no Date class, but with format attribute. We should check format.sas attribute before zapping.

We should capture format before zapping.

We should implement:

df_raw <- haven::read_xpt(path)
# Check for date formats
for (col in names(df_raw)) {
  fmt <- attr(df_raw[[col]], "format.sas")
  if (!is.null(fmt) && grepl("DATE", fmt, ignore.case=TRUE)) {
    # Convert SAS date (days since 1960-01-01) to Date
    # SAS date is numeric days since 1960-01-01
    # R Date is days since 1970-01-01, so need to convert
    # But haven may already have converted? Let's check
    # If column is numeric and fmt contains DATE, convert
    if (is.numeric(df_raw[[col]])) {
      df_raw[[col]] <- as.Date(df_raw[[col]], origin="1960-01-01")
    }
  }
}

But haven::read_xpt may already handle SAS dates correctly if format is known. We should test.

We should implement generic.

We should also handle that the dataset may have character dates like "2012-05-15" already.

We should just handle Date and POSIXct.

We should write script that does this.

We need to ensure we handle that the script must handle that the output dataset may have been produced with specific handling of numeric values: unrounded unless SAS rounds them.

We should just write as is.

We should write script that is complete.

We should also handle that the script must handle that the output dataset may have been produced with specific handling of missing values: write as "".

We should use write.csv with na="".

We should ensure we handle that the script must handle that the output dataset may have been produced with specific handling of character values: trim?

We should not trim unless SAS does.

We should write script that is complete.

We need to ensure we handle that the script must handle that the output dataset may have been produced with specific handling of duplicate check: if duplicates, SAS terminates, but we should also terminate with error.

We should implement check and stop.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of QNAM label uniqueness: if same QNAM has different QLABEL, terminate.

We should implement.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of QNAM already on parent: rename.

We should implement.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of illegal QNAM: warning.

We should implement warning.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of IDVAR empty: substitution.

We should implement.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of RDOMAIN filter.

We should implement.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of parent copy.

We should implement.

We need to handle that the script must handle that the output dataset may have been produced with specific handling of final merge.

We should implement.

We should write R script that does all.

We need to ensure we handle that the script must handle that the output dataset may have been produced with specific handling of sorting.

We should implement sorting.

We should write R script that is complete.

We should now produce final answer.

We need to ensure we handle that the script must be inside one ```r code block.

We should produce final answer with R code.

We should include comments.

We should ensure we handle that the script must not have prose before or after.

We should produce final answer.

