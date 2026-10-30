Need to fix cbind issue. The row[,c(...)] returns tibble with 1 row, but tmp has 7 rows. Need to replicate row values. Use cbind with rep.

Also need to ensure OUT_DIR handling and text output.

We need to produce P035.lst text with numbers. The error was cbind mismatch. Fix by expanding.

Also need to ensure script writes to OUT_DIR correctly.

We should produce complete script.

We need to handle _PARAM column name with special char.

Simplify: create adpp_sum_t by crossing.

Implement make_rows returning data.frame with s_order and value, then bind with repeated keys.

Use dplyr approach: for each row, create 7 rows via tibble.

Let's craft final script.

Also need to ensure haven read works.

We must output exactly one R script block.

We should include all required writes: CSV and TXT.

We need to ensure OUT_DIR exists.

We should handle case where adpp_path not found: try multiple locations.

We need to ensure script runs non-interactively.

We should fix the cbind bug: use `cbind(row[rep(1,7), c(...)], tmp)` or use `bind_cols`.

Let's produce final script.

