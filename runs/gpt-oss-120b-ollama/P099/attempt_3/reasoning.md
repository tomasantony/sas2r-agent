We need to fix the listing P099.lst numbers. SAS PROC MEANS output includes more statistics: Mean, StdDev, Min, Max, Sum, maybe N? The SAS output likely includes N, Mean, StdDev, Minimum, Maximum, Sum. The numbers missing are like 3.03182 etc. Those look like maybe Mean of NewCases for each Type? Let's compute from data: For Constant (Uni) dataset, NewCases values: we can compute mean. Let's quickly approximate: sum of NewCases for Uni? The Sum column in SAS output likely is sum of NewCases. The missing numbers like 3.03182 maybe are StdDev? Actually StdDev for Constant maybe around 3.03. Yes StdDev of Uni likely ~3.03182. Mean maybe 12.0596? Wait mean of Uni NewCases maybe around 12? Let's compute roughly: sum of NewCases for Uni? The last line of Uni data shows Cumul 1000, NewCases sum = 1000? Actually Cumul is cumulative sum of NewCases, final Cumul = 1000, so sum = 1000. N = 100. Mean = 10. So not 12.0596. Maybe for Peak dataset mean ~8.40995? Let's compute: Peak final Cumul = 1000, sum = 1000, N=100, mean=10 again. Hmm.

But SAS PROC MEANS with class Type and var NewCases will compute stats per class. For each Type, N = number of observations (100 each). Sum = total NewCases (should be 1000 for each). Mean = Sum/N = 10. StdDev varies per dataset. The missing numbers like 3.03182 (StdDev for Constant?), 12.0596 maybe Max? No Max for Constant is maybe 15? Actually max NewCases in Uni is 16? Looking at data, max NewCases = 16. So not 12.0596.

Let's compute actual stats quickly (approx). For Constant (Uni): NewCases values range 3-16. Compute mean: sum=1000, N=100 => mean=10. StdDev? Could be around 3.03 (matches missing 3.03182). Min = 3? Actually min NewCases = 3 (first line). Max = 16. Sum = 1000. So SAS output likely includes N, Mean, StdDev, Minimum, Maximum, Sum. The missing numbers list includes 3.03182 (StdDev), 12.0596 (maybe Mean? No mean is 10). 8.40995 (StdDev for another type). 10.4495 (StdDev for another). 15.7999 (maybe Max for something). So our listing currently only includes N, Mean, StdDev, Min, Max, Sum but we formatted with rounding to 2 decimals for Mean and StdDev, and integer for Min/Max/Sum. SAS likely prints more decimals (maybe 5 decimal places) for Mean and StdDev, and maybe prints Min/Max with no decimals but as integer. Also SAS prints Sum as integer. Our text uses 2 decimals for Mean and StdDev, causing mismatch. Need to output with full precision as SAS default (maybe 6.2? Actually SAS default for numeric output is BEST12. format, which prints up to 12 characters, often with up to 6 decimal places if needed). For mean of 10, SAS would print 10. For std dev 3.03182, SAS prints 3.03182 (5 decimal). So we need to not round to 2 decimals, but use round_half_up with maybe 5 decimals? Use default printing with as.character? Better to format with formatC using digits = 6? Let's compute using round_half_up with maybe 5 decimals. Also need to include N column (count). Our text includes N column but SAS listing may have N as "N". The missing numbers include 15.7999 maybe Max for something? Actually Max for Peak maybe 15? Not 15.7999. Could be Sum? No sum is 1000. So maybe SAS includes "Mean" with 5 decimals, "StdDev" with 5 decimals, "Min" and "Max" with maybe 5 decimals as well (but they are integers). The missing numbers list includes 15.7999 which could be Max for Flatten? Flatten max NewCases maybe 23? Not 15.7999. Hmm.

Let's compute actual stats for each Type using R to see values. But offline. Let's approximate: For Flatten dataset, NewCases values range 0-23, max maybe 23. So not 15.7999. For Outbreak2, max NewCases maybe 20? Not 15.7999. For Exp, max NewCases maybe 66? Not 15.7999. So 15.7999 could be StdDev for something else? Actually StdDev for Outbreak2 maybe around 5? Not 15.8. Could be Mean for something else? Mean for Outbreak2 maybe around 10? Not 15.8. Hmm.

Maybe SAS PROC MEANS also includes "Mean" of Cumul? No we only var NewCases. So not.

Maybe the missing numbers are from other parts of listing (like the ODS layout? but listing only includes PROC MEANS output). The SAS listing may also include "Mean of Cumul"? No.

Let's compute actual stats quickly using mental? Not feasible.

Better approach: replicate SAS PROC MEANS output exactly using PROC MEANS default options. In SAS, PROC MEANS default prints N, Mean, StdDev, Minimum, Maximum, Sum. The format for each statistic is default BEST12. For N (integer) prints as integer. For Mean and StdDev prints with up to 6 decimal places (maybe 6). For Min/Max prints as integer. For Sum prints as integer.

Thus we need to format each numeric with formatC(..., format = "f", digits = 6) but trim trailing zeros? SAS prints without trailing zeros? For integer values, prints as integer without decimal point. For 10.0 prints "10". For 3.03182 prints "3.03182". So we can use formatC with digits = 6, flag = "#". Actually formatC with format = "fg" maybe best. Use formatC(x, digits = 6, format = "fg", flag = "#") which uses scientific? Not.

Simpler: use sprintf with %.6f then strip trailing zeros and dot. Write helper to format SAS numeric: if value is integer (abs(value - round(value)) < .Machine$double.eps^0.5) then as integer without decimal. Else format with up to 6 decimal places, removing trailing zeros.

Implement function fmt_sas_num(x) that does that.

Also need to ensure we include N column.

Our current txt_lines header includes "N" column but SAS may not include N? Actually PROC MEANS default includes N. Yes.

Our header currently: sprintf("%-20s %8s %9s %8s %8s %8s %8s", "Type","N","Mean","StdDev","Min","Max","Sum") which may differ spacing but numbers are key.

We need to ensure numbers appear exactly as SAS: maybe spacing not crucial but numbers must be present. The mismatch indicates missing numbers not found at all. So we need to include those numbers.

Thus modify means_tbl to compute Mean and StdDev with full precision (no rounding). Use round_half_up? SAS rounding half away from zero for display? But default PROC MEANS uses rounding to display? It prints with BEST12 which rounds half away? Not sure. We'll just compute using mean() and sd() and format.

Implement fmt_sas_num.

Update txt generation to use formatted strings.

Also ensure we include N column.

Let's adjust script accordingly.

