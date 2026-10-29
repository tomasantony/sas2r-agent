Need to fix error: fmt_result vectorized issue. round_half_up not vectorized. Need to make vectorized.

Also need to ensure outputs match SAS reference. Need to understand expected P003.lst table. It's serious TEAE table. Need to reproduce numbers exactly.

We need to fix round_half_up to handle vector.

Also need to ensure OUT_DIR handling and file naming: expected P003.lst -> write text rendering and CSV. Previous script wrote P003.txt etc but failed before writing. Need to ensure files written.

Also need to ensure formatting matches SAS: strip(put(count,best.))||' ('||strip(put((count/denom)*100,8.1))||'%) ['||strip(put(events,best.))||']'

So count as integer without formatting, pct with 8.1 (one decimal), events integer.

Our fmt_result should do that.

Also need to handle vectorization.

Also need to ensure Fisher p-values formatting? But main table is descriptive counts? The SAS program creates RESULT tables but not final table output? The expected output is P003.lst with 104 numbers. Need to infer what SAS actually outputs. The program snippet provided doesn't include PROC REPORT etc. Possibly truncated? The provided SAS program ends with Fisher tests but no final report. However expected output is table with 104 numbers. Need to reconstruct.

We need to look at pilot project: at14-5-02.sas likely produces table. Let's search memory: CDISC pilot at14-5-02 is serious adverse events table. Might be similar to our constructed table.

We need to ensure text contains numbers like '1','0','86','100' etc. Our previous text would contain those.

Fix vectorization:

round_half_up vectorized:

round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

But need to handle NA.

fmt_pct1 vectorized: sprintf("%.1f", round_half_up(pct,1))

fmt_result vectorized:

fmt_result <- function(count, denom, events) {
  pct <- ifelse(is.na(denom) | denom==0, 0, count/denom*100)
  pct_s <- sprintf("%.1f", round_half_up(pct,1))
  paste0(trimws(as.character(count)), " (", trimws(pct_s), "%) [", trimws(as.character(events)), "]")
}

Also need to ensure haven reading works.

Also need to ensure OUT_DIR files: need to write P003.lst.txt or P003.txt etc. The checker looks for any .txt or .csv rendering. It said R text files read: [] meaning no files written because error before write. So fixing error will allow files.

We should also ensure we write to OUT_DIR/P003.lst as text? The spec says each table additionally as plain-text rendering (file.path(out, "<name>.txt")) . But expected output is P003.lst -> we should write P003.lst.txt and P003.txt etc. Provide both.

Also need to ensure we handle missing TRTAN values? In ADSL TRT01AN numeric 0,1,2. In ADAE TRTAN numeric.

We should also ensure we handle SAS sorting: uppercase before lowercase etc not critical.

We should produce complete script with fixes.

Also need to ensure we don't use crossing that may fail if tidyr version.

We should keep script simple, avoid errors.

Potential other errors: fisher_by_group uses table() with factor levels; need to ensure works.

We should test logic for vectorization.

Also need to ensure we handle NA in fmt_result.

We should return complete corrected R script.

We must ensure script runs non-interactively.

We should add library calls.

We should ensure out dir creation.

We should ensure we write CSV and txt.

We should ensure we handle case where ADSL TRT01AN is haven_labelled; need to convert to numeric.

Use haven::as_factor? Better to convert to numeric via as.numeric.

ADSL$TRT01AN may be labelled. Use as.numeric.

Similarly ADAE$TRTAN.

We should coerce.

Also ensure SAFFL etc are character.

We should add zap_labels.

Simplify: after reading, convert all haven_labelled to appropriate.

We can use haven::zap_labels or dplyr mutate.

We should implement helper to convert.

We should ensure script doesn't fail on missing columns.

We should produce final script.

