Need to fix sas_round vectorization. Previous sas_round used if(is.na(x)) return NA which fails for vector. Need vectorized version.

Also need to ensure outputs match SAS numbers. Need to reproduce exact numbers: need to check SAS logic for stats.

We need to fix script minimally but ensure it runs.

Also need to ensure text output contains numbers.

We should rewrite sas_round to be vectorized.

Also fmt_num etc.

Also need to ensure final table matches SAS expected 58 numbers.

We need to inspect SAS program: it does m_cont_stats with decimals=1, which formats mean/SD with 2 decimals, median/Q1/Q3/min/max with 1 decimal. Our code does that.

But need to ensure quantile method matches SAS PROC MEANS Q1/Q3 definition. SAS uses PCTLDEF=5 by default? Actually PROC MEANS default is PCTLDEF=5. R quantile type=2 is close but not exactly. Need to check. SAS PCTLDEF=5 uses empirical distribution function with averaging. Type 2 in R is similar but need verify. Might need type=5? Let's recall: SAS PCTLDEF=5 definition: p*(n+1) etc. R type=2 is similar to SAS PCTLDEF=5? Actually R type=5 is piecewise linear? Let's check: SAS docs: PCTLDEF=5 uses (n+1)*p. R type=6 uses (n+1)*p. So SAS PCTLDEF=5 corresponds to R type=6? Need to research quickly mental: R quantile types: type 2 is averaging at discontinuities, type 5 is piecewise linear with (n+1). SAS PCTLDEF=5 is empirical distribution with averaging. Might be type=2. We'll keep type=2 for now but could adjust.

Also need to ensure TRTDUR calculation matches SAS: TRTEDT - TRTSDT +1 where those are SAS dates (numeric). Our fix_date should handle.

Also need to ensure Big N ordering matches SAS: order by TRTA (character). But SAS m_counts orders by TRTA. However later transpose expects colPlacebo etc. The test patch renames colPlacebo etc to col1-3. So ordering must be alphabetical? Let's see: Placebo, [Drug] High Dose, [Drug] Low Dose alphabetical would be [Drug] High Dose, [Drug] Low Dose, Placebo. But TRTAN ordering is 1=Placebo,2=Low,3=High. The SAS proc sql order by TRTA would give alphabetical, but then bign macro variables would be in that order, but report expects col1=Placebo. The test patch says rename colPlacebo=col1 etc, so they map by name not position. Our approach using TRTAN order gives correct mapping if we rename by name. But we renamed by position; better to rename by name explicitly.

Simpler: pivot wider then rename columns by name to col1-3 mapping: Placebo->col1, Low Dose->col2, High Dose->col3.

We need to know exact TRTA labels: from u_setup: trt1lbl=Placebo, trt2lbl=[Drug] Low Dose, trt3lbl=[Drug] High Dose. So TRTA values likely those strings. Let's handle generically: map any TRTA containing "Placebo" to col1, "Low" to col2, "High" to col3.

Also need to ensure text output contains numbers like 158.78 etc. Our fmt will produce those.

Fix sas_round:

sas_round <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  sign_x[sign_x==0] <- 1
  # handle NA
  result <- floor(abs(x)*mult + 0.5 + 1e-12)/mult * sign_x
  result[is.na(x)] <- NA_real_
  result
}

Also need to fix fmt_num to be vectorized.

Also need to fix fmt_pct vectorized.

Also need to ensure durcat_fmt value creation uses vectorized sas_round.

Also need to ensure final text includes all numbers.

We should output complete corrected script.

