We need to translate the macro to R script that reproduces outputs. The macro does many checks, logs, warnings, and finally merges supplemental qualifiers onto parent dataset. The expected output is dataset aefull (since parent=AE, outname=AEFULL). The macro writes dataset aefull.csv in OUT_DIR.

We need to implement similar logic using R, reading from inlib (psdtm) which is a libref pointing to D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam (as per libname in family environment). So parent dataset AE is in that directory as a SAS dataset (likely .sas7bdat). Similarly SUPPAE is supplemental.

We need to read using haven::read_sas. Path: file.path(root, "data/derived/cdisc-pilot/adam", "AE.sas7bdat")? Actually libref inlib=psdtm, but earlier they set libname inlib "&root.\data\derived\cdisc-pilot\adam". So psdtm points to that directory. So we can construct path: file.path(Sys.getenv("root")? Not set. But macro variables: &root. = D:/meta_hackathon/sas2r-agent. In R we can get from environment? Not provided. We can just use the absolute path given: D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/AE.sas7bdat. Similarly SUPPAE.sas7bdat.

We need to output to OUT_DIR env var.

Implement steps:

- Define helper round_half_up: function(x, digits) { sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits }

- Read parent and supp.

- Perform checks: existence, non-empty, required variables.

- If any early termination, we still need to write a ReturnCode? Not required; only dataset aefull expected. But we should mimic behavior: if errors, maybe not produce aefull. But for test, likely data is proper, so we produce merged dataset.

Implement merging:

- Filter supp where RDOMAIN == toupper(parent) (AE). If none, copy parent.

- Remove records with missing RDOMAIN, USUBJID, QNAM.

- Identify duplicates on USUBJID,RDOMAIN,IDVAR,IDVARVAL,QNAM. If any, error.

- Check QNAM with multiple QLABELs: if any, error.

- Determine IDVAR values present (distinct IDVAR where RDOMAIN matches). For each IDVAR, process:

   - If IDVAR empty and IDVARVAL empty, set IDVAR='USUBJID', IDVARVAL=USUBJID.

   - If IDVAR empty and IDVARVAL not empty -> error.

- Determine variable types in parent for each IDVAR.

- For each IDVAR, create subset of supp where IDVAR matches and RDOMAIN matches.

   - Convert IDVARVAL to appropriate type (numeric if parent var numeric, else character).

   - Keep needed columns: STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QVAL, QLABEL.

   - Sort by STUDYID, RDOMAIN, USUBJID, IDVAR (if not USUBJID) (the SAS code uses IDCLN which is empty if IDVAR is USUBJID). We'll replicate.

   - Transpose: wide format where each QNAM becomes column with value QVAL, and apply QLABEL as variable label (R doesn't have labels; we can ignore).

   - Merge with parent on STUDYID, DOMAIN (parent has DOMAIN variable? In AE dataset, there is DOMAIN variable). Use left join (parent rows kept). If multiple IDVAR values, need to merge each subset and then combine.

Simplify: Use dplyr::left_join for each IDVAR subset, then bind rows.

Implementation steps:

- parent_df <- read_sas(parent_path) %>% as_tibble()

- supp_df <- read_sas(supp_path) %>% as_tibble()

- Convert variable names to uppercase for consistency.

- Ensure required columns exist.

- Process.

Edge cases: IDVAR may be USUBJID; then we don't need extra column.

Implementation details:

- For each IDVAR value (distinct IDVAR in supp after filtering), we need to ensure IDVAR exists in parent columns. If not, error.

- For each IDVAR, create a transformed supp subset:

   supp_sub <- supp_df %>% filter(RDOMAIN == toupper(parent), IDVAR == idvar)

   If idvar == "USUBJID": then we will merge on USUBJID (already present). No extra column.

   Else: create new column with name idvar, value = if parent column numeric then as.numeric(IDVARVAL) else as.character(IDVARVAL). Use type detection: is.numeric(parent_df[[idvar]]) else character.

   Then select needed columns: STUDYID, RDOMAIN, USUBJID, idvar, QNAM, QVAL.

   Then pivot_wider(names_from = QNAM, values_from = QVAL) (use tidyr::pivot_wider). This will create columns for each QNAM.

   Then left_join parent_df (by STUDYID, DOMAIN, USUBJID, and idvar if exists). Note parent has DOMAIN variable; ensure it's uppercase.

- After processing all IDVAR subsets, combine rows (bind_rows). Since each subset contains rows for distinct IDVAR values, they should not overlap.

- The final dataset is the merged result.

- Write CSV to OUT_DIR/aefull.csv.

Also need to write ReturnCode? Not required.

Implement logging via cat to console (progress). Not needed.

Implement warnings via warning().

Implement early termination via stop().

Now produce R script.

We need to ensure we use only allowed packages.

We'll use haven, dplyr, tidyr, stringr, readr, tibble.

Implement helper functions.

Now write script.

