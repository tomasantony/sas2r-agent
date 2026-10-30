We need to fix error: fmt_pvalue expects scalar but nvalue1 is vector. In mutate with ifelse, nvalue1 is vector, but fmt_pvalue expects scalar; we can vectorize it. Modify fmt_pvalue to handle vector input using sapply.

Also need to ensure output files are written. The previous script wrote csv and txt. But error prevented writing. After fixing, ensure paths exist: out_dir may not exist; need dir.create.

Also need to ensure formatting matches SAS numbers: need percentages with one decimal, parentheses, etc. Also need to ensure nvalue1c formatting matches SAS pvalue5.3: SAS prints with leading blanks? but we just need numbers.

Also need to ensure ordering: SAS sorts by param descending tefl nvalue1. In our code we sorted by desc(PARAM), tefl, TRTPN. Need to sort by param descending, tefl, nvalue1 (maybe). In SAS they later sort by param descending tefl nvalue1. Then they keep first nvalue1 per param/tefl/nvalue1 (so duplicates). Our grouping may be okay.

Also need to ensure pctc formatting: SAS uses put(pct,5.1) which yields width 5 with one decimal, leading blanks trimmed by strip. So our sprintf("%.1f") fine.

Need to ensure n column formatting: SAS uses best. we used as.character.

Now fix fmt_pvalue to vectorize.

Also ensure that when nvalue1 is NA, we return "".

Implement fmt_pvalue <- function(p){ ifelse(is.na(p), "", ifelse(p < 0.001, "<.001", sprintf("%.3f", p))) }

That works vectorized.

Also ensure that we create out_dir if not exists.

Also ensure that we write txt with proper spacing maybe tabs? SAS listing uses spaces; but numbers need to appear. Use tab separation maybe.

Better to mimic PROC REPORT output: columns separated by spaces; but numbers need to be present.

We'll keep as tab separated for easier.

Now produce final script.

