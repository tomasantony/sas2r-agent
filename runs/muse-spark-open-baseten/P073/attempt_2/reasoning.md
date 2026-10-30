Need to fix fmt_num to handle vector input. Previous fmt_num used if(is.na(x)) which fails for vector. Need vectorized version.

Also need to ensure outputs written even if error.

We need to rewrite script with vectorized fmt_num.

Also need to ensure OUT_DIR handling, reading files, etc.

We should produce t_exposure.rtf text rendering and CSV.

We need to fix fmt_num, fmt_int, etc to be vectorized.

Also need to handle mutate with ifelse vector.

Simplify: define fmt_num_vec that handles vector.

Also need to ensure round_half_up vectorized.

Implement:

round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign(x) * floor(abs(x)*mult + 0.5 + 1e-12)/mult
}
fmt_num <- function(x, digits) {
  # vectorized
  sapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi) || is.nan(xi)) return("")
    r <- round_half_up(xi, digits)
    sprintf(paste0("%.", digits, "f"), r)
  })
}

But sapply returns vector.

Similarly fmt_int vectorized.

Also need to fix durcat_fmt creation: previously had duplicate mutate causing error. Simplify.

Also need to ensure final outputs contain 58 numbers matching SAS. Need to ensure stats match SAS exactly: n, Mean, SD, Median, Q1 Q3, Min Max with correct decimals and rounding half up.

We need to ensure quantile type 2 matches SAS PCTLDEF=5. SAS default for PROC MEANS is PCTLDEF=5. So type 2 is correct.

Also need to ensure SD calculation matches SAS: PROC MEANS uses sample SD (n-1). R sd() does same.

Need to ensure Mean, SD formatting with dp1 =2? Wait decimals=1 => dp1=2? Actually code: dp=decimals, dp1=decimals+1. So Mean and SD with 2 decimals, Median etc with 1 decimal. That matches.

Need to ensure duration categories counts and percentages: pct = cnt / bign *100, formatted 5.1 => 1 decimal.

Need to ensure Big N correct.

Also need to ensure transpose column names: col1 etc.

We should rewrite script cleanly, fixing error.

Also need to ensure text file contains numbers: need to write t_exposure.txt and t_exposure.rtf with numbers.

We should ensure OUT_DIR exists.

We should also handle reading ADSL: use haven.

We should avoid duplicate mutate that caused error.

Let's craft final script.

We need to ensure no install.packages.

We should include library calls.

We should ensure we handle TRTAN ordering.

We should ensure we write CSV with correct headers.

We should ensure we handle missing values.

Let's produce final R script.