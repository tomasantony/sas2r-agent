We need to fix fmt_num to handle vector input. Currently fmt_num expects scalar; used inside cont_stats with vector. Should vectorize. Modify fmt_num to handle vector: if length>1, apply over each element. Use sapply.

Also earlier error: in cont_stats we used fmt_num(stats$mean, decimals+1) where stats$mean is vector; fmt_num returns error because if(is.na(x)) expects length 1. So change fmt_num to vectorized.

Also need to ensure that missing values produce empty string not "NA". Already handled.

Also need to ensure that the output includes numbers like 55.62 etc. Need to verify decimals: For continuous stats, SAS uses dp1 = decimals+1 for mean and sd, dp for median, q1,q3, min,max. In our code we used decimals+1 for mean and sd, decimals for others. Good.

But also need to ensure that for Q1, Q3 and Min, Max we format with decimals (dp). Already.

Potential issue: In cont_stats we used bind_rows with tibble where value = fmt_num(stats$mean, decimals+1). fmt_num returns vector of strings length equal to stats$mean length. That's okay.

But we need to ensure that for each treatment we have separate rows. In bind_rows we used tibble(TRTA = stats$TRTA, ...) which will recycle? Actually tibble with vector of length n will create column TRTA as vector, and value as vector; they will be same length. That's fine.

But we need to ensure that for each statistic we have rows per treatment. The tibble creation as above will produce columns of length n (number of treatments). That's correct.

Now fix fmt_num.

Also need to ensure that the output files are written. The earlier error prevented script from completing, so no files.

Implement vectorized fmt_num:

fmt_num <- function(x, digits) {
  if (all(is.na(x))) return(rep("", length(x)))
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf(paste0("%.", digits, "f"), round_half_up(v, digits))
  })
}

Also need to ensure that in cont_stats we treat n as integer string without formatting. Already as.character.

Now adjust script accordingly.

Also ensure that the column headers use correct treatment order. In big_n we have order by TRTA alphabetical (character). In SAS they order by TRTA as appears in dataset (maybe Placebo, Drug X Low Dose, Drug X High Dose). Our big_n ordering may be alphabetical: "Drug X High Dose", "Drug X Low Dose", "Placebo". But we later map using trt_order_target, so fine.

Now produce final script.