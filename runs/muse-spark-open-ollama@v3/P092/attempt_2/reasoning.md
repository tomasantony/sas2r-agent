Need to fix syntax error: extra parenthesis. The round_half_up function had extra ). Also need to ensure figure generation works.

Previous script had:
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

That has mismatched parens. Should be:
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

But actually need closing for ifelse: ifelse(test, yes, no) -> 3 args. So we have ifelse(is.na(x), NA_real_, sign(...) ) -> need one closing ). The code had two lines: sign(...) ) then ) on next line. That's actually correct? Let's count: ifelse( -> open1, is.na(x), NA_real_, sign(x)*floor(...)/... ) -> close1, then } . But they had an extra ) on next line. Wait original:

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

Line2: ifelse(is.na(x), NA_real_,
Line3:          sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
Line4:   )
Line5: }

Line3 ends with ) closing floor? Actually floor(abs(x)*10^digits+0.5) -> that's one ), then /10^digits then ) to close ifelse? Wait need to parse: sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits -> floor has its own ). So sign(x) * floor(...) /10^digits is expression. Then ifelse needs closing ). So line3's trailing ) is floor's ), not ifelse's. Then line4's ) is ifelse's. But error says unexpected ')' in line with sign... So maybe they had an extra ) on line3? Let's see error: "unexpected ')' in: "         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits) " Execution halted. That suggests line3 has an extra ). Actually floor(abs(x) * 10^digits + 0.5) -> that's floor( abs(x) * 10^digits +0.5 ) -> need two ) ? No abs(x) has ), floor has ). So floor(abs(x) * 10^digits + 0.5) is correct with one ) after 0.5. Then /10^digits) would be extra. Wait original line: sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits) -> there is a ) after digits. That is extra. Should be sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits  without trailing ). Then next line ) closes ifelse.

So fix that.

Also need to ensure figure generation doesn't fail due to other issues. Keep minimal edit.

We should provide complete corrected script.

Also ensure out dir handling, write files.

We should keep same logic but fix syntax.

Potential other issues: ggsave with width 650 px dpi 96 may be okay. Ensure ggplot2 available.

We should ensure script runs non-interactively.

Provide final script with corrected round_half_up.

Also ensure no extra parenthesis.

Let's craft final script.

