Need to translate SAS macro SUPP2PAR_v1 to R. Need to understand inputs.

Librefs: inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
But program uses psdtm? Wait benchmark driver: %SUPP2PAR_v1(inlib=psdtm, parent=AE, supp=SUPPAE, outlib=outds, outname=AEFULL);

But family environment says inlib -> .../adam. However psdtm libref not defined in family environment snippet. Possibly defined elsewhere? Need to check actual directories.

We need to locate data. The rawadam, rawsdtm etc. The psdtm likely points to SDTM data. Let's explore filesystem via R? But we need to write script that will run in environment. We need to infer paths.

We have env variables: &root = D:/meta_hackathon/sas2r-agent
&rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
&rawsdtm = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
&rawsplit etc
&outtlf etc
&outds = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P039

But inlib in family environment is D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam

However psdtm likely is SDTM. Could be D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm or derived.

We need to write R script that reads AE and SUPPAE from appropriate location. Need to handle both possibilities: try to find files.

We need to produce dataset aefull -> OUT_DIR/aefull.csv

We need to replicate macro logic.

Simplify: The macro merges supplemental qualifiers onto parent domain.

Steps:

- Check parent and supp existence. If supp doesn't exist or empty or RDOMAIN filter empty, then output is simple copy of parent.

- Otherwise:

- Check required variables: parent requires STUDYID, DOMAIN, USUBJID. Supp requires STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QVAL, QLABEL.

- Check supp records where USUBJID or RDOMAIN or QNAM empty -> ignore those records (they go to _IGNOR). Continue.

- Check uniqueness on USUBJID RDOMAIN IDVAR IDVARVAL QNAM -> if duplicates, terminate (error). In R, we should handle: if duplicates, we would terminate? But expected output is aefull.csv, so presumably no duplicates.

- Check same QNAM occurs with different QLABEL -> terminate.

- Determine length of USUBJID and IDVAR to handle truncation? In R not needed.

- Create S2P_SUPP: if IDVAR == '' and IDVARVAL == '' then set IDVAR='USUBJID', IDVARVAL=USUBJID. If IDVAR == '' and IDVARVAL != '' then error termination (RC=8).

- Check for QNAM already on parent: if QNAM value equals a variable name on parent, rename QNAM to QNAM_2.

- Check QNAM legal variable name: if not valid or length >8, warn but proc transpose will correct. In R, we need to emulate? SAS proc transpose with id qnam will create variables named after QNAM values, but if illegal, it will correct (e.g., add underscore?). However for this dataset, QNAMs are likely valid.

- Then core processing:

Determine distinct IDVAR values in supp where RDOMAIN == parent (AE). For each IDVAR value:

- Adapt supplemental so IDVAR is turned into variable identical as on parent domain (same type). If parent variable is numeric, convert IDVARVAL via input(best.). If character, assign directly.

- Sort supp subset by STUDYID RDOMAIN USUBJID IDVAR (if not USUBJID) and drop IDVARVAL IDVAR.

- Transpose: by STUDYID DOMAIN USUBJID IDVAR (where DOMAIN is RDOMAIN renamed), var qval, id qnam, idlabel qlabel. So each QNAM becomes a column with QVAL values.

- Sort parent by STUDYID DOMAIN USUBJID IDVAR (if not USUBJID)

- Merge parent and transposed supp by those keys. If parent exists, output to step; else if supp without parent, output to DROP.

- After loop over IDVAR values, determine if parent has --SEQ variable (AESEQ). If exists, sort each step by STUDYID DOMAIN USUBJID AESEQ, else by STUDYID DOMAIN USUBJID.

- Merge all steps together by those keys (so if multiple IDVAR groups, they are merged).

- Set DROP datasets together.

- Output final dataset.

We need to implement this in R.

We need to locate AE and SUPPAE files.

Potential locations:

- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
- D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
- D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam

We should search for files named ae.xpt, suppae.xpt, ae.sas7bdat, etc.

In R script, we can implement logic to find files: check multiple candidate directories.

We need to handle reading .xpt via haven::read_xpt.

Also need to handle case sensitivity.

We should write R script that:

- defines out <- Sys.getenv("OUT_DIR")
- defines root etc from env? But we can just search.

Simplify: try to list files in candidate dirs.

Candidate dirs for psdtm:

- Sys.getenv("rawsdtm") maybe? But env var names in R are different? The macro variables are not automatically env vars. However OUT_DIR is env var. We can try to infer root as "D:/meta_hackathon/sas2r-agent" hardcode, but better to try relative.

We can attempt to find AE dataset by searching common paths.

We should implement helper function find_dataset(lib, name):

Search order:

1. file.path(Sys.getenv("OUT_DIR"), ...) not.

2. Try paths:

- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm"
- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
- "./data/..."
- etc.

We can also use Sys.getenv("rawsdtm") if set? But benchmark says macro variables set, not env vars. However we can try Sys.getenv("rawsdtm") and Sys.getenv("root") etc. Might be empty.

Better to hardcode root as "D:/meta_hackathon/sas2r-agent" as given.

We should also handle that inlib in family environment is adam derived, but psdtm is likely sdtm. So we will search both.

Implement function read_sdtm_dataset(name):

candidates <- c(
  file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(tolower(name), ".xpt")),
  file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(toupper(name), ".xpt")),
  file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm", paste0(tolower(name), ".sas7bdat")),
  file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm", paste0(tolower(name), ".xpt")),
  file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", paste0(tolower(name), ".xpt")),
  file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", paste0(tolower(name), ".xpt")),
  file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split", paste0(tolower(name), ".xpt"))
)

Check existence, read first found.

Also need to handle .xpt lowercase vs uppercase.

We should also list directory to debug.

We need to produce aefull.csv with same columns as SAS output.

We need to replicate SAS behavior for dates: write ISO dates.

We need to ensure we preserve variable order: parent variables first, then transposed QNAM variables.

We need to handle QLABEL as variable label? In R we can set attribute label.

But output CSV just needs columns.

We need to implement transpose: For each IDVAR group, pivot_wider.

Steps detailed:

- Read AE and SUPPAE.

- Ensure column names uppercase? SAS is case-insensitive but preserves case. We'll keep original names but for merging we need to handle case-insensitive matching. We'll convert names to uppercase for logic, but preserve original for output? Simpler to convert all names to uppercase.

SAS variable names are uppercase typically. So we can uppercase all column names.

- Check required variables exist.

- Filter SUPPAE where RDOMAIN == "AE" (upcase parent). If none, then copy.

- Handle empty USUBJID/RDOMAIN/QNAM: filter where strip(RDOMAIN)=='' or strip(USUBJID)=='' or strip(QNAM)=='' -> ignore. In R, treat NA or "" or whitespace only as empty. Use trimws.

- Check duplicates: group by USUBJID, RDOMAIN, IDVAR, IDVARVAL, QNAM. If any duplicates, error. But we can just deduplicate? However spec says terminate. For this dataset, likely no duplicates.

- Check QNAM with different QLABEL: group by QNAM (upcase) and count distinct QLABEL. If >1, error.

- Handle IDVAR empty substitution: For rows where IDVAR is NA/"" and IDVARVAL is NA/"", set IDVAR="USUBJID", IDVARVAL=USUBJID. If IDVAR empty but IDVARVAL not empty -> error.

- Check QNAM already on parent: get parent variable names (uppercase). Get distinct QNAM values (uppercase). Intersection -> rename those QNAM values to QNAM_2 (append "_2"). Need to handle length 10.

- Check QNAM legal: not needed for output, but we can note.

- Determine distinct IDVAR values after filtering RDOMAIN==AE.

- For each idvar in distinct list:

   - Determine type of parent variable: if parent column is numeric (is.numeric) then type N else C. Need to handle haven labelled? haven reads numeric with labels.

   - Create supp subset where IDVAR == idvar and RDOMAIN == "AE".

   - Create new column named idvar with value converted: if numeric, as.numeric(IDVARVAL) (best. equivalent). If character, IDVARVAL as is.

   - Keep columns: STUDYID, RDOMAIN, USUBJID, idvar, QNAM, QVAL, QLABEL (and maybe others? But for transpose we need by keys). Actually SAS keeps STUDYID RDOMAIN USUBJID idvar plus QNAM/QVAL/QLABEL.

   - Sort by STUDYID, RDOMAIN, USUBJID, idvar (if not USUBJID)

   - Transpose: pivot_wider id_cols = c(STUDYID, DOMAIN, USUBJID, idvar) values_from = QVAL names_from = QNAM. Need to handle QLABEL as label attribute.

   Note: RDOMAIN renamed to DOMAIN in transpose.

   - For parent, sort by same keys.

   - Merge: left join parent with transposed supp? But SAS does merge by keys, keeping all parent records, and dropping supp without parent to DROP dataset. So we can do left_join(parent, supp_transposed, by=keys). And also identify supp records with no parent match for DROP.

   However SAS merge is one-to-one by keys? Parent may have multiple records per USUBJID+AESEQ, but supp keys include AESEQ? Let's see typical SUPPAE: IDVAR = AESEQ, IDVARVAL = AESEQ value. So keys include AESEQ. So parent has AESEQ variable, supp transposed has AESEQ as key. So merge is on STUDYID DOMAIN USUBJID AESEQ.

   If IDVAR is USUBJID, then keys are STUDYID DOMAIN USUBJID only, but then transpose would produce one row per USUBJID, and merging onto parent would duplicate? Actually if IDVAR=USUBJID, then supp is at subject level, not event level. Then merging by USUBJID would replicate supp values across all parent records for that subject. That's intended.

   So we need to handle both.

   Implementation: For each idvar group, create transposed supp with keys = c("STUDYID","DOMAIN","USUBJID", idvar_if_not_USUBJID). Then merge with parent.

   But parent may have multiple rows per subject; merging subject-level supp will duplicate.

   We need to do iterative merging: start with parent, then for each idvar group, left_join with its transposed supp.

   However SAS does separate merges per idvar group, creating intermediate datasets S2P_AE_STEP_i, then merges those steps together by STUDYID DOMAIN USUBJID AESEQ (or without SEQ). That is equivalent to sequentially left joining.

   Simpler: start with parent_sorted, then for each idvar, left_join with transposed.

   Need to handle DROP: supp records that have no matching parent. For each idvar group, we can anti_join(transposed, parent, by=keys) to find drops.

- After all idvar groups merged, we have final dataset.

- Need to handle sorting final by STUDYID DOMAIN USUBJID AESEQ if AESEQ exists else by STUDYID DOMAIN USUBJID.

- Output to aefull.csv.

We also need to handle variable attributes: QLABEL becomes variable label. Not needed for CSV but we can set attr.

We need to ensure we preserve all parent columns, including those that may be numeric dates.

We need to write CSV with dates as YYYY-MM-DD. How to detect date columns? In SDTM, dates are character ISO (AEDECOD etc) or numeric SAS dates? In pilot SDTM, AE dataset has AESTDTC etc as character. So no conversion needed. But we should handle haven labelled dates: if column is Date or POSIXct, format accordingly.

We should implement helper to format dates for CSV: if inherits Date, format "%Y-%m-%d", if POSIXct, format "%Y-%m-%dT%H:%M:%S".

We also need to ensure we don't round numbers.

We should also produce txt rendering? The expected outputs says dataset aefull -> write OUT_DIR/aefull.csv . The rules say each table or listing additionally as txt, but this is dataset output, so only CSV needed. However we should also ensure we create directory.

We need to write R script that does all this.

We should also handle case where SUPPAE not found -> copy parent.

We need to implement robust file finding.

Let's craft R script.

We need to include library calls.

We should add progress messages via cat.

We need to ensure script runs non-interactively.

We should implement helper functions.

We need to handle haven reading: haven::read_xpt may produce tibble with labelled vectors. We should convert to data.frame and handle.

We should also handle that AE and SUPPAE may be in .xpt with lowercase names.

We should uppercase column names for processing, but preserve original case for output? SAS output variable names are uppercase. So we can output uppercase names.

Simplify: convert all names to uppercase.

We need to handle IDVAR values: they are like "AESEQ". Need to upcase for comparison.

We need to handle RDOMAIN filter: upcase.

We need to handle QNAM values: upcase for checking, but original QNAM values become variable names. In SAS, QNAM values are used as variable names as is (upcase?). Typically QNAM values are uppercase like "AETRTEM" etc. We'll keep as is but ensure valid.

We need to handle transpose: QNAM values become column names. If QNAM already exists on parent, we renamed to QNAM_2. So we need to apply that renaming before transpose.

We need to handle QVAL is character, may be numeric as string.

We need to handle that after transpose, new columns are character (QVAL).

We need to handle that parent AESEQ is numeric.

We need to handle merging: need to ensure keys are same type. For numeric AESEQ, supp's AESEQ derived from IDVARVAL string converted to numeric.

We need to handle that IDVARVAL may have leading spaces.

We need to implement best. conversion: as.numeric(trimws(IDVARVAL))

We should also handle that if parent variable is character, keep as character.

We should implement final output column order: parent columns in original order, then new QNAM columns in order of appearance? SAS transpose order is by QNAM sorted? Actually proc transpose with by statement will create columns in order of QNAM values as they appear? Need to check. But for CSV, order may not be critical as long as all columns present. However to match SAS exactly, we should order new columns alphabetically or by first appearance. We'll use order of distinct QNAM as they appear in supp.

We should also handle that if multiple IDVAR groups, new columns from each group are added.

We should write final CSV.

We need to ensure we handle missing values: write.csv with na=""

We should also handle that OUT_DIR may not exist, create it.

We should add stopifnot checks.

Let's craft final R script.