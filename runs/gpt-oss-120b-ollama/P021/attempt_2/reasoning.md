We need to fix errors. The error: after bind_rows(lapply(table_rows, as_tibble)), the resulting tibble may not have column named Parameter because each row is a named character vector but as_tibble will treat names as column names, not rows. Actually we want each row as a data frame with columns Parameter and each treatment column. Using as_tibble(t) where t is a named vector will produce a tibble with one row and column per element (named). So we need to combine them correctly.

Better: create a data frame by row-binding using rbindlist from data.table or do.call(rbind, ...). Use tibble::as_tibble with .name_repair = "minimal". Let's construct:

tbl_df <- bind_rows(lapply(table_rows, function(x) as_tibble(as.list(x), .name_repair = "minimal")))

But each element is character; need to ensure column names are Parameter and treatment columns. That should work.

Also need to ensure missing values are represented as empty string in CSV and text.

Also need to write .txt rendering as plain text with numbers formatted as SAS (e.g., N as integer, percentages with one decimal, etc). Already we format.

But need to write both .txt and .csv for the table. The spec: each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics), and as CSV of the table body. So we need .txt and .csv. In previous script we wrote .csv as P021.csv and .txt as P021.lst. Should be .txt maybe name same as .lst? The spec says .txt. The SAS output expected P021.lst (table/listing text). So we need .txt? The earlier spec: "each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt"))". But expected output is P021.lst. So we should output both .txt and .lst? To be safe, output .txt and .csv. Also maybe .lst is expected; we can output .txt and .lst both.

Also need to ensure column order: Parameter first, then each treatment column in order defined earlier (col_names). Ensure column names are exactly as in SAS: treatment values as they appear (character). The SAS PROC TABULATE uses class TRT01P with levels sorted? SAS sorts class values in order of appearance? Actually default order is sorted by internal order (character sort). We'll use sort.

Also need to compute total N per column correctly: For each treatment, N is count of rows where TRT01P equals that level. For total column, N is total rows.

Our total_n_by_col computed correctly.

But for Sex and Race rows, we need to include missing categories if any. We added "Missing". Good.

Now fix the binding.

Also need to ensure that numeric formatting for Age stats: N integer, Mean one decimal, SD two decimals, Min/Max zero decimals.

Our fmt_num uses round_half_up and sprintf.

Now implement corrected script.

Also need to write .txt with tab delim maybe but SAS listing may be space delimited. We'll use tab.

Also need to write .csv of table body: we already wrote P021.csv.

Add .txt file: writeLines.

Add .lst file as alias maybe.

Also ensure that we convert empty strings for NA.

When we create tbl_df, missing values (e.g., percentages where denominator zero) will be NA; we need to replace with "" before writing.

Implement:

tbl_df_clean <- tbl_df %>% mutate(across(everything(), ~ifelse(is.na(.), "", .)))

Write csv with na = "" already.

Write txt: use same cleaned values.

Now produce final script.

