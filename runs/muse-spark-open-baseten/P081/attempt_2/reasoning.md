Need to fix syntax error: round_sas function has extra parenthesis.

Original:
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

Extra ) . Should be:

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

Also need ensure script runs. Check other issues.

We should output complete script.

Also ensure OUT_DIR handling.

We should keep same logic but fix syntax.

Potential other issues: reading adam files. Ensure robust.

We should provide corrected script.

