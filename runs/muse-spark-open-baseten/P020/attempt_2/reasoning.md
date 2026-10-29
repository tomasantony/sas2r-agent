Need to fix error: surv_summary unused argument times. Use different approach. Also need to ensure text output is written to OUT_DIR/*.txt and contains numbers. Previous script failed before writing text, so missing. Need to fix that and ensure text contains numbers matching SAS.

We need to understand SAS output numbers: examples: 1,50,0,21,3,0.9524 etc. Those are survival estimates. Our surv_df should produce those.

We need to fix surv_summary call. Use manual atrisk calculation instead.

Also need to ensure OUT_DIR handling and file writing.

We should produce P020.lst text with numbers.

We need to ensure script runs without error.

Also need to handle quartile estimates etc.

We should simplify: remove surv_summary call, use manual.

Also need to ensure ggplot saving works.

We should produce text file with numbers: include survival estimates with 4 decimals like 0.9524 etc.

We need to ensure script writes P020.txt and P020.lst etc.

We should also ensure CSV outputs.

We need to produce complete R script.

We should also handle missing packages: patchwork not available, but we try.

We need to ensure survminer functions not needed.

We should rewrite script with fixes.

We need to keep same logic but fix error.

Also need to ensure project_path handling: use /bench path.

We should make script robust.

We need to output one R code block.

We should include round_half_up etc.

We need to ensure text contains 308 numbers. Our previous text generation would have many numbers, but need to ensure it runs.

We should fix atrisk_df generation: use manual.

We should also ensure surv_df lower/upper handling: survfit gives lower/upper.

We should ensure we write text files.

We should also ensure we write CSVs.

We should test logic for surv_df: need to ensure surv values match SAS. SAS PROC LIFETEST default method.

We should ensure we use correct event definition.

We should produce final script.

We need to ensure we don't call surv_summary.

We should also handle surv_median fallback.

We should ensure we don't error on patchwork.

We should produce final answer.

