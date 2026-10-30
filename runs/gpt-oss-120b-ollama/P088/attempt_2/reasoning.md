We need to fix errors.

Issues:

- read_delim warning: need I() for literal data. Use read_delim(I(rs_txt), ...). Similarly for others.

- TRTEDT parsing: some missing values '.' leads to NA. dmy on '.' returns NA with warning. That's fine.

- ADY calculation: using if_else with date differences returns difftime objects (duration). Need to convert to integer days. Use as.integer(ADT - TRTSDT). So compute diff days as as.integer(ADT - TRTSDT). Then add 1.

- In adeff_tumor, we used AVALC = NA_character_ but later select includes AVALC column; need to keep column name. Also need to include BASE, CHG, PCHG columns for tumor and nadir etc. In final adeff, need all variables as SAS expects: includes BASE, CHG, PCHG, etc. Our selection omitted many.

We need to replicate SAS variable set. Let's define final column order similar to SAS keep list in each step.

From adeff_resp keep list: STUDYID USUBJID TRTP TRTPN SAFFL EFFFL TRTSDT TRTEDT AGE SEX PARAMCD PARAM PARAMN AVAL AVALC ADT ADY AVISIT AVISITN ABLFL ANL01FL

No BASE, CHG, PCHG.

adeff_tumor keep list includes BASE? In SAS code for tumor they kept: STUDYID USUBJID TRTP TRTPN SAFFL EFFFL TRTSDT TRTEDT AGE SEX PARAMCD PARAM PARAMN AVAL AVALC AVALU ADT ADY AVISIT AVISITN ABLFL ANL01FL

No BASE, CHG, PCHG initially. Later they added CHG, PCHG after merging baseline. So final adeff_tumor should have BASE, CHG, PCHG.

Similarly _nadir includes many variables: includes BASE, CHG, PCHG.

Also _respfl includes AVAL, AVALC, etc.

Thus final adeff should have columns: STUDYID USUBJID TRTP TRTPN SAFFL EFFFL TRTSDT TRTEDT AGE SEX PARAMCD PARAM PARAMN AVAL AVALC AVALU BASE CHG PCHG ADT ADY AVISIT AVISITN ABLFL ANL01FL RSCAT ASEQ

We need to ensure all present.

Simplify: after building each dataset, ensure they have same columns (add missing with NA). Then bind_rows.

Implement helper to add missing columns.

Also need to write adeff_resp, adeff_tumor, adeff (already). Ensure adeff_resp and adeff_tumor written.

Also need to write listing P088.lst and P088.csv.

Fix ADY calculation: use case_when with as.integer.

Implement.

Also need to ensure date format output as YYYY-MM-DD (write.csv will use default format; dates become "2024-01-15". That's fine.

Implement script.

