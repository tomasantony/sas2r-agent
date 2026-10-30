Need to fix sprintf error: "%*.*f" has two asterisks, not allowed. Need to construct format string dynamically.

Also need to ensure outputs are written. Previous error prevented any output. Fix sas_put_wd.

Also need to ensure numbers appear: the percentages etc. The SAS numbers missing include 25, 0.3 etc. Need to ensure text contains those numbers. Our previous logic for pct was wrong: we used round(count/tot,0.1) which gives fraction like 0.3 not 30. But SAS code: put(round(count/tot,0.1),5.1) -> that would be 0.3 for 25/86? Let's see: 25/86=0.29 round to 0.3 -> displays "  0.3". But expected numbers include 0.3, 0.7 etc. So maybe SAS indeed shows fractions not percentages. But footnote says %=Percentage... So maybe they intentionally show fraction? However typical disposition table shows percentages like 29.1%. But SAS code as given does round(count/tot,0.1) not *100. So we should keep that to match SAS numbers: 0.3 etc. Our earlier fix to *100 would produce 29.1 not 0.3, causing mismatch. So revert to fraction.

Also need to ensure sprintf works.

Also need to ensure OUT_DIR handling.

We must produce complete script.

Fix sas_put_wd:

sas_put_wd <- function(x,w,d){
  ifelse(is.na(x), strrep(" ",w),
    {
      xv <- round_half_up(x,d)
      fmt <- paste0("%",w,".",d,"f")
      sprintf(fmt, xv)
    })
}

But need vectorized: sprintf can handle vector.

Also need to handle w,d as scalars.

Also need to ensure countc formatting: put(count,4.) -> width 4 integer. Use sas_put_4.

And put(round(count/tot,0.1),5.1) -> width 5 with 1 decimal.

Implement.

Also need to ensure final text contains numbers like 25, 0.3 etc. Our countc will be like "  25 (  0.3)" -> contains those numbers.

Need to ensure we write files correctly.

Also need to handle library loading without error.

Provide final script.