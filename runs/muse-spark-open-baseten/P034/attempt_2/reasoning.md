Need to fix syntax error: extra parenthesis. The round_sas function had:

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

There is extra ) . Should be:

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

But actually need closing for ifelse: ifelse(test, yes, no) -> need one ) . The code has two lines: sign(...) /10^digits) then ) on next line. That's correct? Let's count: ifelse( is.na(x), NA_real_, sign(x)*floor(...)/10^digits) -> that's ifelse with 3 args, needs closing ). The code has:

ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )

First line: ifelse(
Second line: sign(...) /10^digits) -> this closes floor? Actually floor(abs(x)*10^digits+0.5) -> then /10^digits) -> this ) closes ifelse? Wait.

Let's write: sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits

That has floor( ... ) -> one ). Then ifelse( ..., ..., that_expression ) -> need one ). So total after expression should be one ). The code has:

sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )

First ) closes ifelse, second ) is extra. So remove one.

Also need to ensure script runs. Provide complete corrected script.

We should output single R code block.

We need to ensure no syntax errors elsewhere.

Provide corrected script identical but fixed round_sas.

Also ensure other parentheses correct.

Let's produce final script.