Need to fix script. Error is haven::is_labelled not exported. Use haven::is.labelled? Actually haven exports is_labelled? In newer haven maybe not. Use labelled::is.labelled or check. Simpler: just zap_all_labels or skip mutate.

Also need to ensure output files: expected P021.lst text rendering. Previous script wrote to OUT_DIR/P021.txt etc but failed before. Need to ensure script runs and produces text with numbers matching SAS.

We need to understand expected numbers: examples: 1,50,19,28,47,8,12,20,42.1 etc. Need to reproduce table exactly.

We need to inspect ADSL data to know values. But we can infer: Table 1 demographics.

We need to fix script minimally but also ensure numbers match.

We should simplify reading and avoid haven::is_labelled.

Also need to ensure OUT_DIR handling and file naming: Expected outputs: P021.lst -> write text rendering and CSV. The checker looks for any .txt or .csv in OUT_DIR. Previous script wrote P021.txt etc but failed. Now fix.

We need to ensure script writes P021.lst.txt or P021.txt with numbers.

We should also ensure formatting matches SAS: PROC TABULATE with N and ColPctN.

We need to ensure we compute correctly.

Also need to handle haven labelled conversion: use haven::zap_labels directly without is_labelled check, or use lapply.

Simplify: adsl <- haven::zap_labels(adsl_raw) ; then zap_formats.

Also need to handle column names uppercase.

We should produce complete script.

We should also ensure we don't use across with haven::is_labelled.

We need to ensure script writes correct numbers: need to test logic.

We should also ensure we handle TRT01P levels correctly.

We should output text file containing numbers like 42.1 etc.

We need to ensure we write to OUT_DIR/P021.lst (without extension? but spec says .txt). The checker says expected P021.lst -> write text rendering and CSV. So we should write P021.lst (no extension) plus .txt? Safer to write both P021.lst and P021.txt and P021.csv.

Previous script wrote P021.txt and P021.lst (no extension). That should satisfy "any .txt or .csv". But checker said R text files read: [] meaning no files found. Because script failed before writing.

Now fix.

We should also ensure we use correct rounding.

We should produce script that runs.

We need to include library loading.

We should avoid using haven::is_labelled.

We should also ensure we handle missing.

We should output numbers with same decimals as SAS: N as integer, ColPctN as 5.1, Mean 5.1, Std 5.2, Min/Max 5.0.

We need to ensure text contains numbers like 42.1 etc.

We should implement simple.

We should also ensure we write CSV with same numbers.

We should produce final script.

