We need to fix the error in building plain text rows. The trans_wide has columns: ord, status, plus columns named after TRT01PN values (numeric maybe). In our code we attempted row[, treat_map$TRT01PN] but column names are numeric strings like "0", "1", etc. treat_map$TRT01PN contains numeric values (maybe integer). Need to convert to character to match column names.

Better: use as.character(treat_map$TRT01PN) for subsetting.

Also need to ensure blank line after ord change correctly: SAS break after ord means after each group of same ord, a blank line. In our loop we inserted blank line when ord changes (i.e., before new group). Should be after group, but fine.

Also need to ensure header includes treatment labels in order of TRT01PN. treat_map already sorted.

When building csv_body we rename columns using treat_map mapping; but we used rename_with incorrectly: rename_with(~ treat_map$treat[match(., treat_map$TRT01PN)], .cols = -status). The . is column name; we need to map column names that are TRT01PN values to treat labels. That may work but ensure correct.

Potential other issues: In disp_part1 we used mutate(status=ifelse(...)) before grouping; that replicates each row's status, then group_by includes status, which is fine. However SAS query: first part selects distinct TRT01P, TRT01PN, count(usubjid) as cnt, 1 as ord, case when DCREASCD='Completed' then DCREASCD else 'Discontinued' end as status from adsl where ITTFL='Y' group by TRT01P, calculated status. Note they didn't group by TRT01PN? Actually they group by TRT01P, calculated status (no TRT01PN). But they also select distinct TRT01P, TRT01PN. In SAS, if TRT01PN is not in group by, it's taken from first row of each group (maybe ambiguous). But likely TRT01PN is same for each TRT01P (treatment code). We'll keep grouping by TRT01P, status, TRT01PN to be safe.

Second part groups by TRT01P, TRT01PN, DCREASCD.

Now fix subsetting.

Also need to ensure that trans_wide column order matches SAS: PROC TRANSPOSE with BY status ord; ID TRT01PN; IDLABEL treat; VAR cnt_prcnt. This creates columns named after TRT01PN values, with column labels (maybe not used). The order of columns is by the order of ID values as they appear in the data sorted by BY variables. Since we sorted disp_2 by status, ord, then pivot_wider will order columns alphabetically by TRT01PN (character). SAS order: ID values appear in order of first occurrence within each BY group, but after transpose they are sorted by ID value? Actually PROC TRANSPOSE with ID= variable creates columns in the order of the ID values as they appear in the input data (sorted by BY). Since we sorted disp_2 by status, ord, then within each BY group the rows are ordered by TRT01PN (since we didn't sort). In SAS they didn't sort by TRT01PN before transpose, but they had BY status ord, and the data is already sorted by status ord TRT01PN because earlier they sorted disp_2 by status ord. So columns order will be ascending TRT01PN (numeric). We'll replicate by ordering columns by numeric TRT01PN.

Our pivot_wider will order columns alphabetically (character). For numeric strings, alphabetical matches numeric order if same length? Might be okay. We'll ensure ordering by sorting TRT01PN numeric.

We can set factor levels.

Simplify: after pivot_wider, reorder columns using treat_map order.

Implementation: after pivot_wider, select ord, status, then treat_map$TRT01PN as characters.

trans_wide <- disp_2 %>%
  select(status, ord, TRT01PN, cnt_prcnt) %>%
  pivot_wider(names_from = TRT01PN, values_from = cnt_prcnt) %>%
  arrange(ord, status) %>%
  select(ord, status, all_of(as.character(treat_map$TRT01PN)))

Now for csv_body rename columns: use treat_map$treat in same order.

csv_body <- trans_wide %>%
  select(-ord) %>%
  rename_at(vars(-status), ~ treat_map$treat[match(., treat_map$TRT01PN)])

But rename_at expects a function; easier: create a named vector.

new_names <- setNames(treat_map$treat, as.character(treat_map$TRT01PN))
csv_body <- trans_wide %>%
  select(-ord) %>%
  rename(!!!new_names)

Now for text output: header line: first column "Subject Disposition", then treat labels in order.

Rows: for each row, need to extract values for each TRT01PN column in order.

We can do:

row_vals <- c(row$status, as.character(row[ , as.character(treat_map$TRT01PN)]))

But row is a tibble with one row; subsetting with row[ , colnames] returns tibble; need as.character.

Implement:

vals <- as.character(unlist(row[ , as.character(treat_map$TRT01PN), drop = TRUE]))

Now blank line after each ord group: after finishing rows of a given ord, insert blank line. We'll track previous ord and when it changes (i.e., after writing row, if next row ord != current ord, add blank line). Simpler: after each row, if i == nrow or next ord != current ord, add blank line.

Implement.

Now produce final script.

