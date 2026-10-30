The error: SAFFL column not present in adsl? Actually adsl likely has SAFFL variable but maybe lower case? Haven reads variable names as they are, maybe "SAFFL" is present but we used toupper(SAFFL) which fails if SAFFL is character? Actually toupper works on character vector, but if SAFFL is numeric? In ADaM, SAFFL is character flag Y/N. The error says object 'SAFFL' not found, meaning column name is not SAFFL but maybe "SAFFL " with trailing spaces? Or maybe adsl dataset variable is "SAFFL" but we used read_sas which converts variable names to lower case? Haven keeps original case but dplyr uses case-sensitive column names. Let's inspect: we can't run, but we can assume variable names are uppercase as in SAS. However error suggests not present. Could be that adsl dataset doesn't have SAFFL (maybe safety flag variable is "SAFFL" but we need to use "SAFFL" exactly). Could be that adsl dataset is not loaded because path wrong? We used read_adam reading .sas7bdat; maybe file extension is .xpt? The synthetic data may be .xpt. Our read_adam picks .sas7bdat first; if not found, .xpt. Might be .xpt present but we used read_sas for .sas7bdat; if .xpt, we use read_xpt. That should work.

But error still: SAFFL not found. Let's check adsl variable names: maybe they are lower case after reading with haven? Haven keeps original case but converts to lower case? Actually haven reads variable names as they appear, preserving case, but R data frames convert to lower case? No, they keep as is. However dplyr's filter uses column names as symbols; if column name is "SAFFL", we can refer to SAFFL directly. The error suggests column not present. Could be that adsl dataset doesn't have SAFFL (maybe safety flag is "SAFFL" but in this synthetic dataset it's "SAFFL" but we need to use "SAFFL" after converting to upper case? Not needed.

Maybe the dataset variable is "SAFFL" but we used toupper(SAFFL) which tries to apply toupper to a factor? Still works. The error is that SAFFL column not found, meaning the column name is something else, maybe "SAFFL " with trailing spaces, or "SAFFL1". Let's list column names? Can't run. But we can make code robust: use dplyr::select with matches? Use any_of(c("SAFFL","SAFFL_","SAFFL"))? Better: use case-insensitive matching: find column name that matches "SAFFL" ignoring case.

We can create a helper to get column name:

get_col <- function(df, name) {
  nm <- names(df)[tolower(names(df)) == tolower(name)]
  if (length(nm) == 0) stop(paste("Column", name, "not found"))
  nm
}

Then use !!sym(get_col(df, "SAFFL")).

Simplify: use dplyr::filter with .data[[col]].

Implement: saffl_col <- get_col(adsl, "SAFFL"); then filter(.data[[saffl_col]] == "Y").

Similarly for CONCOMFL in adcm.

Also TRTA column may be lower case? We'll similarly get column names.

Implement generic function get_col.

Also need to ensure ordering of treatment groups matches original macro: they used order by TRTA in m_counts (proc sql order by TRTA). So we need to sort by TRTA.

When renaming columns to col1-col3, we used trt_order from col_hdrs$TRTA which is from bign sorted by TRTA. That's fine.

Now need to ensure output includes numbers like 14.2 etc. Those are percentages maybe. Our rounding uses round_half_up with 1 decimal. Should match.

Also need to ensure that we write both CSV and txt. The script writes t_cm.csv and t_cm.txt. The expected output is t_cm.rtf but we produce txt and csv; that's acceptable per spec: each table also as plain-text rendering and CSV. So fine.

Now adjust code accordingly.

Also need to ensure that we treat missing values correctly: if cnt=0, value "0". Good.

Now produce corrected script.