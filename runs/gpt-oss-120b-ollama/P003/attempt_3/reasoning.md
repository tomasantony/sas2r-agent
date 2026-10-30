The error: column TRT01AN not found in ADAE after rename_all(toupper). Actually column names become uppercase, so TRT01AN becomes "TRT01AN". That's fine. But after rename_all(toupper), we have column names uppercase, but later we refer to TRT01AN (case-sensitive). That's okay. However error occurs in AEPTD creation: group_by(TRT01AN, AEBODSYS, AEDECOD). The column may be "TRT01AN" but maybe the original variable is "TRT01AN" but after rename_all(toupper) it's "TRT01AN". So should exist. But maybe the original variable is "TRT01AN" but in ADAE dataset it's "TRT01AN"? Let's check typical ADaE dataset: variable TRT01AN may be numeric. However maybe the column is "TRT01AN" but we renamed to uppercase, still same. The error says column not found, meaning after rename_all(toupper) the column name is something else, maybe "TRT01AN" is not present; maybe the variable is "TRT01AN" but we renamed to "TRT01AN"? That seems same. Could be that the column is "TRT01AN" but we used rename_all(toupper) which also changes "TRT01AN" to "TRT01AN" (no change). So why missing? Maybe the column is "TRT01AN" but we later renamed to "TRTAN" later? Wait we rename later after summarise. The error occurs before rename, at group_by. So column not present. Let's inspect typical ADaE dataset: It may have variable "TRT01AN" but maybe it's "TRT01A" or "TRT01A". Could be "TRT01AN" is not present; maybe the variable is "TRT01A" (treatment group). In the SAS code they use TRT01AN as treatment numeric variable. So dataset should have it. But maybe after reading XPT, the column name becomes "TRT01AN" but with trailing spaces? Haven may preserve variable labels? Usually read_xpt returns column names as they appear, maybe lower case? Actually haven reads variable names as they are, preserving case. If the XPT variable is "TRT01AN", it will be "TRT01AN". So group_by should work.

But error persists, maybe the dataset ADSL also missing TRT01AN? The error occurs in AEPTD creation, which uses ADAE. So maybe ADAE does not have TRT01AN; maybe it has TRT01A (without N). Let's check typical ADaE: variable TRT01AN is treatment arm numeric. Could be present.

Given we cannot inspect data, we can make code robust: use whichever variable exists: if TRT01AN not present, try TRT01A or TRTAN. We'll detect column names.

Implement a helper to get treatment variable name: find column containing "TRT" and numeric? Use first match.

Simplify: after reading, keep original column names, do not rename to uppercase; use case-insensitive matching via dplyr's .data[[var]].

We can use `select` with matches.

Better: define function get_var(df, candidates) that returns first existing column.

Implement:

treat_var <- intersect(c("TRT01AN","TRT01A","TRTAN"), names(df))[1]

Then use that variable in group_by via .data[[treat_var]].

But dplyr's group_by can use !!sym(treat_var).

We'll adjust all references to treatment variable.

Also for ADSL.

Implement at top:

treat_var_adsl <- intersect(c("TRT01AN","TRT01A","TRTAN"), names(ADSL))[1]
treat_var_adae <- intersect(c("TRT01AN","TRT01A","TRTAN"), names(ADAE))[1]

Then use treat_var_adae in group_by.

Also for BACKBONE etc.

Also need to ensure that we keep variable names as in SAS for output: use original names (TRTAN after rename). We'll rename after summarise.

Implement modifications.

Also final listing: need to match SAS output formatting: numbers maybe not have spaces. The earlier mismatch shows only DENOM output being written, not others. Possibly script stopped early due to error, so only DENOM written. After fixing, all should be written.

Implement final script.

