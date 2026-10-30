Need to fix error: fmt_int vectorized issue. The function fmt_int uses if(is.na(x)) but x is vector. Need vectorized version. Also fmt_num similar. Need to fix cat_stats and cont_stats to handle vectors.

Also need to ensure outputs: t_dm.rtf text rendering and CSV. The previous script wrote t_dm.txt and t_dm.csv and copied to rtf. But error prevented. Need to fix.

Also need to ensure numbers match SAS: 176 numbers. Need to reproduce formatting exactly.

We need to fix fmt_int and fmt_num to be vectorized.

Define fmt_int_vec, fmt_num_vec.

Also need to handle SAS quantile type 2? SAS PROC MEANS Q1 Q3 definition: SAS uses PCTLDEF=5 by default? Actually PROC MEANS default is PCTLDEF=5. R quantile type 2 is closest? Might need type 5? Let's check. SAS PCTLDEF=5 uses empirical distribution function with averaging. R type 2 is similar but not identical. Need to check typical. But we can keep type 2 for now; may need adjustment.

Also need to ensure rounding half away from zero.

Also need to ensure categorical ordering matches SAS.

Also need to ensure transpose column names: colPlacebo etc. The SAS code uses proc transpose with prefix=col and id TRTA. So column names become colPlacebo etc. Then rename to col1 col2 col3. In R we did pivot_wider.

Need to ensure output text contains numbers like 14.1 etc. The text rendering must contain numbers.

Fix fmt functions:

fmt_int <- function(x) {
  ifelse(is.na(x), "", as.character(as.integer(x)))
}
But as.integer truncates; need to handle vector.

fmt_num <- function(x, digits) {
  # vectorized
  sapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return("")
    r <- round_half_up(xi, digits)
    sprintf(paste0("%.", digits, "f"), r)
  })
}

But need to handle vector input.

Also cat_stats uses fmt_int(cnt) where cnt is vector; fmt_int must be vectorized.

Also need to ensure output directory handling.

Also need to ensure haven reading.

Provide complete script.

Also need to ensure t_dm.rtf is written as text rendering containing numbers. The validator looks for OUT_DIR/<any>.txt or .csv rendering. It said R text files read: [] meaning no txt/csv found? But we did write. However script failed before writing, so no files.

Fix script to not fail.

Also need to ensure write.csv with na="".

Provide final script.

