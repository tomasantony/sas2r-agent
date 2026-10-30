Need to fix sas_put_wd to be vectorized. Also need to ensure outputs written. Previous script failed before writing. Fix helper.

Also need to ensure OUT_DIR handling, write t_cm.txt and t_cm.rtf and csv.

The error: sas_put_wd used if(is.na(x)) which not vectorized. Need to make vectorized.

Also strip should handle vector.

Simplify: use vectorized functions.

Also need to ensure final output contains numbers. The SAS numbers include percentages like 30.5 etc. Our formatting should produce those.

Fix sas_put_wd:

sas_put_wd <- function(x, w, d) {
  v <- sas_round(x, d)
  fmt <- paste0("%.", d, "f")
  out <- sprintf(fmt, v)
  out[is.na(x)] <- ""
  out
}

sas_round vectorized already.

Also need to ensure combined3 value creation works.

Also need to ensure final table includes all rows.

Also need to ensure text file written even if error earlier.

Provide complete corrected script.

Also need to handle haven labelled conversion.

Make script robust.

Also need to ensure output directory exists.

Provide final script.