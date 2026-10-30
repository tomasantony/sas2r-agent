We need to translate SAS PROC TABULATE to R. Need to produce a table with rows: Total Subjects (overall), Sex, Race, Age (years) with stats. Columns: Planned Treatment (TRT01P) and Total. So essentially cross-tab of treatment groups plus overall column.

We need to read adam.adsl dataset from path: &project_path./data/adam . The macro variable project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c . So dataset path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.sas7bdat . Use haven::read_sas.

Filter ITTFL='Y'.

Class variables: TRT01P (treatment), SEX, RACE. Variable: AGE numeric.

We need to compute for each column (treatment groups and total) the following rows:

- Total Subjects: N (count of subjects) (no percentages)
- Sex: N and column percent (percentage of total column) with format 5.0 for N and 5.1 for percent.
- Race: same as Sex.
- Age: N, Mean, Std, Min, Max with formats.

Also need overall column (ALL) which is total across all treatments.

The SAS PROC TABULATE with "missing" includes missing categories.

We need to produce a table similar to SAS output. Provide a text rendering (plain text) and CSV of same table.

We need to output to OUT_DIR environment variable. Use out <- Sys.getenv("OUT_DIR").

File names: P021.lst (text) and maybe also CSV: P021.csv? The spec says each table/listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers, and as CSV of the table body. The name of the listing is P021.lst (maybe .txt). We'll produce "P021.txt" for text and "P021.csv" for CSV.

But spec: Expected outputs: P021.lst (table/listing text, 56 numbers) -> write a text rendering and CSV of the same table. So we need to write a .txt file named "P021.lst.txt"? Actually they say .lst is the SAS listing file. We'll output "P021.lst.txt" maybe. Safer: output file named "P021.lst" as text, and also "P021.csv". Use writeLines for text.

We need to format numbers same as SAS: N with no decimals (5.0 means width 5, 0 decimals). Percent with 1 decimal. Mean 1 decimal, Std 2 decimals, Min/Max 0 decimals.

We need to round half away from zero (SAS default). R's round uses "round to even". We'll implement custom round_half_up function.

Implement rounding for each statistic to required decimals.

Compute counts including missing categories: For SEX and RACE, need to include missing as a category. In SAS, class variables include missing values as a separate level when MISSING option is used (proc tabulate data=... missing;). So we need to treat NA as a level labeled '.'? SAS prints '.' for missing. We'll label missing as "Missing". We'll replicate.

Compute column percentages for each level within each column: (count / total column count) * 100.

Construct data frame with rows: For each row type (Total Subjects, Sex categories, Race categories, Age stats). For each column (treatment levels and Total). We'll need column names: treatment levels (unique values of TRT01P) plus "Total". Order: as in SAS, the column order is TRT01P levels then ALL (Total). The order of treatment levels is alphabetical? SAS default order is order of appearance in data? Actually PROC TABULATE sorts class levels in ascending order (character). So we sort TRT01P values ascending (character). We'll also include "Total" as last column.

Rows: We'll create a hierarchical label column: maybe "Parameter". We'll produce a table with first column "Parameter" and subsequent columns for each treatment and total.

Rows details:

1. Total Subjects row: label "Total Subjects". Value N per column (count of subjects in that column). No percentages.

2. Sex rows: For each level of SEX (including missing). Row label maybe "Sex = <level>"? SAS prints "Sex" as row header then subrows for each level with N and % in same cell? Actually PROC TABULATE with "SEX='Sex' * (N='n' * f=5.0 ColPctN='%' * f=5.1)" will produce a row for each SEX level, with two cells: N and %? Wait the syntax: class variable SEX, then in table statement: SEX="Sex" * (N="n" * f=5.0 ColPctN="%" * f=5.1). This creates a row for each SEX level, and for each column, two subcolumns: N and %.

But the overall table layout: rows are hierarchical: first row "Total Subjects" with N only. Then a row "Sex" as a header? Actually PROC TABULATE will produce a row label "Sex" spanning across subcolumns? The syntax: SEX="Sex" * (N="n" * f=5.0 ColPctN="%" * f=5.1). This will produce a row for each level of SEX, with two statistics per column: N and %.

Similarly for RACE.

For AGE row: AGE="Age (years)" * (N="n" * f=5.0 Mean="Mean" * f=5.1 Std="SD" * f=5.2 Min="Min" * f=5.0 Max="Max" * f=5.0). This will produce a row for AGE (single row) with multiple stats.

Thus final table will have many rows.

We need to produce a plain text rendering similar to SAS output. Could be a simple tab-delimited representation.

Simplify: produce a data frame where each row corresponds to a statistic row (including subrows). We'll create columns: Parameter, TRT01P1, TRT01P2, ..., Total. For Sex and Race, we need separate rows for each level and each statistic (N and %). Could combine as separate rows: e.g., "Sex=Male - n", "Sex=Male - %". But SAS shows them side by side in same row: N and % as separate columns under each column. Actually each column will have two subcolumns: N and %. In a flat table, we could have separate columns like "TRT01P1_N", "TRT01P1_%". But spec expects a table listing with numbers; not strict format.

Simplify: produce a tidy table with rows: variable, level, statistic, value per column. But to match SAS listing maybe they expect a matrix style.

Given limited time, produce a simple representation: a data frame where each row is a combination of variable, level (or statistic), and columns for each treatment and total. For Sex and Race, we will have two rows per level: one for N, one for %.

We'll also output a text file using format similar to SAS: we can use knitr::kable? Not allowed. We'll just paste rows with tabs.

Implement round_half_up function:

round_half_up <- function(x, digits=0) {
  posneg <- sign(x)
  absx <- abs(x)
  pow <- 10^digits
  floor_val <- floor(absx * pow + 0.5)
  result <- floor_val / pow
  return(result * posneg)
}

Now steps:

- Load libraries.
- Set out dir.
- Read adsl.
- Filter ITTFL == "Y".
- Ensure variables are character.
- Get treatment levels sorted.
- Compute total column counts.

Define function to compute stats for a vector: N (non-missing count), Mean, SD, Min, Max.

For Age: compute stats per column.

For Sex and Race: compute counts per level (including missing). For missing, treat as "Missing". Compute percentages = count / total column count * 100.

Create rows list.

Construct data frame rows with columns: Parameter, then each column name.

Parameter naming: For Total Subjects row: "Total Subjects". For Sex rows: paste0("Sex: ", level, " (n)"), and "Sex: ", level, " (%)". Similarly for Race.

For Age rows: "Age (years) N", "Age (years) Mean", "Age (years) SD", "Age (years) Min", "Age (years) Max".

Now produce CSV: write.csv(df, file.path(out, "P021.csv"), row.names=FALSE, na="").

For text rendering: create a string with header row: Parameter and column names separated by tabs. Then each row values formatted as per SAS: N as integer (no decimals), percentages with 1 decimal, mean 1 decimal, SD 2 decimals, min/max integer.

We'll format using sprintf with appropriate width? Not needed.

We'll generate lines and writeLines to file.path(out, "P021.lst").

Also need to output CSV of the plotted data? No figure.

Thus final script.

Edge: need to ensure missing categories included. Use factor with levels = sort(unique(...), na.last=TRUE) plus NA as level "Missing". We'll compute counts using table with useNA="ifany".

Implementation details:

treat_levels <- sort(unique(df$TRT01P[!is.na(df$TRT01P)]))

Create a list of column names: treat_levels plus "Total".

Compute per column stats:

For each trt in treat_levels, subset data where TRT01P == trt.

Also total subset = all.

Define function get_stats(vec) returning list N, Mean, SD, Min, Max.

Now build rows.

Initialize list rows <- list()

Add total subjects row:

row <- c(Parameter="Total Subjects", values for each column = N_total per column). N_total per column is count of rows in that column (including missing? For total subjects, it's count of subjects in that column (i.e., number of rows). So for each treatment column, N = nrow(subset). For total column, N = total rows.

Add Sex rows:

levels_sex <- sort(unique(df$SEX), na.last=TRUE) ; treat NA as "Missing". We'll create vector sex_levels <- c(levels_sex[!is.na(levels_sex)], "Missing") if any NA.

For each level, compute counts per column: for each column subset, count where SEX == level (or is.na for Missing). Percent = count / column N * 100.

Add rows for N and %.

Similarly for Race.

Age rows: compute stats per column.

Now format numbers:

Define format_number(x, digits) using round_half_up then sprintf with appropriate format: e.g., digits=0 -> sprintf("%.0f", x). digits=1 -> sprintf("%.1f", x). digits=2 -> sprintf("%.2f", x).

Now produce data frame.

Implementation.

Let's craft script.

