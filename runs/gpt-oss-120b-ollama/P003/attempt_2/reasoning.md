We need to fix errors: variable TRT01AN not present in ADAE? In SAS, ADAE likely has variable TRT01AN (treatment arm numeric). In our read, column names may be uppercase? haven reads variable names as lower case? Actually haven preserves original case but converts to lower case? It keeps as is but R makes them lower case? In haven, column names are as in file, but may be all caps. So we need to use consistent naming. Let's inspect: ADSL has variable TRT01AN, SAFFL, USUBJID. ADAE has TRT01AN, TRTEMFL, AESER, AEBODSYS, AEDECOD, STUDYID, USUBJID. So we need to reference correct column names. In dplyr, we used TRT01AN but after reading, column names may be lower case? Actually haven reads variable names as they appear, preserving case, but R's data frames allow case-sensitive names. So we should use the exact names as in SAS: TRT01AN, SAFFL, USUBJID, etc. In our code we used TRT01AN in group_by but also used rename TRTAN = TRT01AN later. The error occurs because TRT01AN not found in ADAE after previous steps? In AEPTD step we used group_by(TRTAN = TRT01AN...). But maybe column name is "TRT01AN " with trailing spaces? Not likely. Could be that ADAE variable is "TRT01AN" but we used "TRT01AN" correctly. However error says object 'TRT01AN' not found, meaning column not present. Maybe the variable is named "TRT01A" or "TRT01A"? Let's assume it's "TRT01A". But we need to check SAS code: they use TRT01AN as variable in ADSL and ADAE. So likely present.

Potential issue: after reading with haven, column names become lower case? Actually haven converts variable names to lower case? I recall haven keeps original case but makes them syntactically valid, e.g., "TRT01AN" stays "TRT01AN". So should be fine.

But error occurs in AEPTD step: after reading ADAE, we pipe to filter then group_by(TRTAN = TRT01AN...). The error says TRT01AN not found. Maybe the column is "TRT01A" or "TRT01AN " with trailing spaces. Could be that the variable is numeric but named "TRT01AN". Let's add a step to rename all columns to upper case to avoid case issues: use rename_all(toupper). That will ensure consistent naming.

Also need to ensure that we keep variable names as SAS expects for output. We'll rename after processing back to original case.

Simplify: after reading, rename columns to upper case, then work with uppercase names.

Also need to ensure that the final listing matches SAS numbers: they expect counts, percentages with one decimal, etc. Our RESULT tables produce RESULT column with count (percent) [events] but we need to ensure formatting matches SAS: count as integer, percent with one decimal, events integer. Use sprintf for count and events as integer without decimal.

Also need to ensure that denominator (denom) column is present in final listing? SAS final listing likely includes columns: SUMMARYLEVEL, TRTAN, DENOM, EVENTS, RESULT? Not sure. The reference says 104 numbers. Our final listing includes many columns.

But main error is variable not found. Let's rename columns to uppercase.

Also need to adjust the Fisher function: group_var for subject level should be none; we can compute directly.

Simplify: We'll produce same tables as before but ensure column names match.

Also need to write final listing as P003.lst (text) and CSV. Our write_outputs writes P003.csv and P003.txt. That's fine.

Let's rewrite script with column renaming.

Also need to ensure that we use left_join correctly: In RESULTPT we need to keep AEBODSYS, AEDECOD, TRTAN, SUMMARYLEVEL, DENOM, events, RESULT. In SAS they also have count variable? Not needed.

Also need to ensure ordering matches SAS: order by AEBODSYS, AEDECOD, TRTAN.

Now produce corrected script.