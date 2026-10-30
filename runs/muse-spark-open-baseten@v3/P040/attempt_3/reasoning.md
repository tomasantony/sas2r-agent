Need to fix syntax error: unexpected ')' in ")". Look at previous script: there is stray line:

atrisk_mat <- sapply(atrisk_times, function(t) sapply(arms, function(a) sum(filter(one, arm==a)$day >= t)))
)
Extra ). Also mean_restricted had duplicate logic but okay.

Need to produce complete corrected script.

Also need ensure outputs match SAS reference. Previous attempt 1 had 0.879 recall missing numbers like 0,17,10,8,21,233,11.206,4.0033,94,-4.0033,-94,4135.35 . Those are likely rank statistics, covariance matrices, etc. Attempt 2 tried to fix but had syntax error.

We need to provide script that runs and produces text with all numbers.

We should fix syntax and ensure text includes all numbers.

We need to reconstruct script cleanly.

We should ensure OUT_DIR handling, write CSV and txt and png.

We need to ensure numbers: SAS output has 373 numbers. Need to include product-limit table with time 0 row, survival etc. Also quartile estimates, mean, summary, rank statistics, covariance matrices, test of equality.

We need to ensure formatting matches SAS numbers: e.g., 0.9412 etc.

We need to ensure we compute correctly.

We should base on attempt1 but fix missing blocks: attempt1 missed some numbers like 11.206 etc. Those likely are rank statistics values? Let's see missing numbers: 0,17,10,8,21,233,11.206,4.0033,94,-4.0033,-94,4135.35 . Could be covariance matrix values.

We need to ensure we output those.

We have manual calculation for rank statistics. Need to verify values.

We should implement correct SAS formulas for log-rank etc. Use manual loop as in attempt2.

We need to fix syntax error: remove extra ).

Also need to ensure mean_restricted correct.

We should produce final script with no syntax errors.

We need to include all required libraries.

We should test mentally.

We need to output txt file named P040_f_kmplot.txt and csv.

We should ensure script writes to OUT_DIR.

We should include ggsave.

We should ensure we don't have stray parentheses.

Let's craft final script.

We will reuse attempt2 but fix syntax.

Check lines:

atrisk_mat <- sapply(atrisk_times, function(t) sapply(arms, function(a) sum(filter(one, arm==a)$day >= t)))
)
Should be:
atrisk_mat <- sapply(atrisk_times, function(t) sapply(arms, function(a) sum(filter(one, arm==a)$day >= t)))

But sapply returns matrix? Need to handle.

Simpler: compute atrisk_df directly via expand.

We can do:

atrisk_df <- tibble(arm=rep(arms, each=length(atrisk_times)), time=rep(atrisk_times, times=length(arms))) %>% mutate(n_at_risk = map2_int(arm,time, ~ sum(filter(one, arm==.x)$day >= .y))) %>% pivot_wider...

But easier: use lapply.

Let's do:

atrisk_list <- lapply(arms, function(a) sapply(atrisk_times, function(t) sum(one$day[one$arm==a] >= t)))
atrisk_df <- as.data.frame(do.call(rbind, atrisk_list))
colnames(atrisk_df) <- paste0("t", atrisk_times)
atrisk_df$arm <- arms

That avoids nested sapply issue.

Also mean_restricted had duplicate A calculation with placeholder; need clean.

We should rewrite mean_restricted cleanly.

Also need to ensure no extra ) elsewhere.

We should produce final script with correct R syntax.

We need to ensure we load purrr for map2_int if used, but we can avoid.

We should keep script minimal edits but fix syntax.

We will output complete script.

