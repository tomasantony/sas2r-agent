We need to fix errors: pivot_longer is from tidyr, not loaded. Load tidyr. Also need to ensure numeric formatting matches SAS. The listing mismatches numbers like 429.53 etc. Those are from PROC MEANS sum and mean values. Our means_full calculation may be off: we used summarise then pivot_longer incorrectly. Let's compute PROC MEANS output similar to SAS: PROC MEANS data=Round sum mean ndec=3; var Length Round RoundEven; So SAS prints N, Mean, Sum for each variable. Need to compute N (same for all), Mean (3 decimals), Sum (3 decimals). Also need to compute proportion rounded up: PROC MEANS data=Round mean ndec=3; var RoundUp RoundEvenUp; So mean of binary indicator (proportion). Need to format numbers with 3 decimals.

Our listing currently prints means_full with Mean and Sum but we used pivot_longer incorrectly. Simpler: compute N = nrow(round_df). Then for each variable compute mean and sum. Build a data frame.

Also need to ensure rounding to 3 decimals.

Also need to include numbers like 429.53 etc. Let's compute.

Implement:

N <- nrow(round_df)

means_full <- tibble::tibble(
 Variable = c("Length","Round","RoundEven"),
 N = N,
 Mean = round(mean(round_df$Length),3),
 Sum = round(sum(round_df$Length),3)
) then add rows for Round and RoundEven.

But need separate rows for each variable. So:

means_full <- tibble::tibble(
 Variable = c("Length","Round","RoundEven"),
 N = N,
 Mean = c(round(mean(round_df$Length),3),
          round(mean(round_df$Round),3),
          round(mean(round_df$RoundEven),3)),
 Sum = c(round(sum(round_df$Length),3),
         round(sum(round_df$Round),3),
         round(sum(round_df$RoundEven),3))
)

Similarly means_up:

means_up <- tibble::tibble(
 Variable = c("RoundUp","RoundEvenUp"),
 Mean = c(round(mean(round_df$RoundUp),3),
          round(mean(round_df$RoundEvenUp),3))
)

Now listing formatting: need to match SAS formatting: numbers with 3 decimals, no extra spaces. Use sprintf with appropriate width.

Also need to include N column for means_up? SAS output likely shows Variable and Mean only (no N). We'll follow.

Now also need to ensure seq table formatting: SAS PROC PRINT noobs label prints x, Round, RoundEven with default format (maybe best). Our listing prints Round and RoundEven as integers (no decimals). That's fine.

But missing numbers like 1.011 etc likely from means of Length etc. Ensure we compute correctly.

Also need to ensure round_half_away function works for rounding to nearest integer (unit=1) and to nearest 0.1. Our implementation uses floor(abs_x/unit + 0.5)*unit. For negative numbers, SAS round away from zero: e.g., -1.5 -> -2. Our function sign_x * rounded works: sign_x = -1, rounded = floor(1.5+0.5)=2 => -2 correct.

Now need to load tidyr for pivot_longer not needed after change.

Also need to ensure we write listing text file with correct numbers. Use sprintf with appropriate formatting: For means_full, we need columns: Variable (maybe 12 chars), N (5), Mean (12.3f), Sum (12.3f). Already.

For means_up: Variable (12), Mean (12.3f). Already.

Now ensure we write CSV of tables as required: Already.

Also need to ensure we write SGPlot_data.csv (we used round_to_even_df). That's fine.

Now adjust script accordingly.

