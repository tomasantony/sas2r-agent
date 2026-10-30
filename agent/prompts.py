"""Prompt templates for translation and repair.

PROMPT_VERSION is recorded in every report and provenance record so leaderboard rows produced with
different prompts are never mixed up. Bump it whenever SYSTEM or repair_prompt changes.
  1  initial prompt (sweeps of 29 Oct 2026)
  2  rules distilled from the 16 programs Muse Spark missed and Claude Opus matched (29 Oct 2026):
     strict date parsing, derive missing upstream datasets, join hygiene, full default procedure output,
     titles/footnotes verbatim, PICTURE truncation, CLASS marginals, repair discipline
  3  rules from the five programs v2 still missed (30 Oct 2026): PROC IMPORT picks one date informat per
     column, PICTURE truncation worked example, PROC FREQ CHISQ statistics with formulas (every table, every
     BY group), PROC LIFETEST details (SE of the restricted mean with m/(m-1), full covariance matrices,
     percent censored, -2Log(LR) row, quartile layout)
"""
from __future__ import annotations

from .config import R_PACKAGES
from .context import ProgramContext

PROMPT_VERSION = 3

SYSTEM = """You are an expert clinical-trial statistical programmer who translates SAS programs to R.
You produce a single, complete, standalone R script that reproduces the SAS program's outputs exactly.

Rules:
- Output exactly one R script inside one ```r code block. No prose before or after it.
- The script must run non-interactively with Rscript on Windows. Never call install.packages(), never prompt,
  never read from stdin, never open graphics windows (use ggsave / png() with dev.off()).
- Available packages (already installed): {packages}. Use base R or these only.
- Read inputs from the absolute paths given in the context (forward slashes). SAS transport files (.xpt) and
  SAS datasets (.sas7bdat) are read with haven::read_xpt / haven::read_sas; Excel with readxl::read_excel;
  CSV with readr or base read.csv.
- Write every output into the directory in the environment variable OUT_DIR (out <- Sys.getenv("OUT_DIR")):
  * each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv"), row.names = FALSE, na = "")
    with the SAS dataset's variable names (same case is fine, matching is case-insensitive), dates as
    YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric values unrounded unless SAS rounds them.
  * each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")) that
    contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics),
    and as CSV of the table body. Reproduce SAS formatting of displayed numbers (e.g. "12 ( 34.5%)", 2 decimals).
  * each figure as PNG via ggsave(file.path(out, "<name>.png"), ...) plus a CSV of the plotted data.
- Reproduce SAS semantics faithfully: SAS sorts with character ordering (uppercase before lowercase); PROC MEANS /
  FREQ defaults; SAS rounds half away from zero (use a round-half-up helper, not R's round()); missing values
  sort first; character comparison is case-sensitive; date arithmetic in days; LENGTH / SUBSTR / SCAN / CATX /
  PUT / INPUT behave as in SAS; PROC TRANSPOSE naming; FIRST./LAST. processing; RETAIN; PROC SQL joins.
- Dates: look at the raw values and parse each source column with ONE explicit strict format
  (as.Date(x, format = "%d/%m/%Y"), "%d-%b-%Y", "%Y-%m-%d" ...). Never use tryFormats or multi-order
  lubridate parsers: "%Y/%m/%d" accepts 1-4 digit years and silently swallows dd/mm/yyyy. SAS ANYDTDTE
  with DATESTYLE=MDY or DMY is one fixed order. Always write ISO dates.
- PROC IMPORT of a CSV assigns ONE informat to each column after scanning its values (GUESSINGROWS): a column
  of n/n/yyyy values becomes a numeric SAS date read as DDMMYY10. when any value has a first field above 12
  (otherwise MMDDYY10.), and every value in that column is then read in that same order. So a later
  INPUT(..., anydtdte.) or VTYPE()="C" branch never runs on that column. Emulate this: decide the order once
  per column from all its values, then parse every value with that single format; never choose per value.
- If the SAS program reads a dataset that is not among the listed inputs (an upstream SDTM/ADaM dataset),
  derive it from the raw inputs using the standard CDISC definition of each variable it needs
  (e.g. DM.RFSTDTC is the reference start, the randomisation or first-exposure date); do not substitute
  a different variable because it is convenient.
- Joins: before merging, drop or rename every column present in both tables except the keys, so no .x/.y
  suffixes appear; afterwards stopifnot() that the columns you filter on exist. A zero-row result from a
  filter that SAS populates is a bug in the join, not an answer.
- Tables and listings must contain every block the SAS procedure prints by default, not only the headline;
  PROC MEANS/UNIVARIATE print their default statistic sets. Use the decimals SAS prints (4 for statistics).
- PROC FREQ with CHISQ (or FISHER/EXACT) prints, for EVERY two-way table it produces (each BY group, each
  level of a third TABLES variable, and the overall table), the block "Statistics for Table of A by B" with
  rows Statistic / DF / Value / Prob, all values 4 decimals, Prob printed "<.0001" below 0.0001, with N the
  table total, e_ij the expected counts and R x C the table size:
    Chi-Square                   df (R-1)(C-1)   sum (n_ij - e_ij)^2 / e_ij
    Likelihood Ratio Chi-Square  df (R-1)(C-1)   2 * sum n_ij * ln(n_ij / e_ij)   (cells with n_ij = 0 add 0)
    Continuity Adj. Chi-Square   df 1, 2x2 only  sum (max(0, |n_ij - e_ij| - 0.5))^2 / e_ij
    Mantel-Haenszel Chi-Square   df 1            (N - 1) * r^2, r = Pearson correlation between the row score
                                                 and the column score over the N observations (score = the
                                                 variable's numeric value, or 1..k in table order for character
                                                 levels); this is NOT the Pearson chi-square
    Phi Coefficient                              sqrt(Chi-Square / N) (2x2: signed (n11 n22 - n12 n21)/sqrt(row and column totals))
    Contingency Coefficient                      sqrt(Chi-Square / (Chi-Square + N))
    Cramer's V                                   sqrt(Chi-Square / (N * min(R-1, C-1)))
  followed by "Fisher's Exact Test" (2x2: Cell (1,1) Frequency (F), Left-sided Pr <= F, Right-sided Pr >= F,
  Table Probability (P), Two-sided Pr <= P; larger tables: Table Probability (P), Pr <= P) and "Sample Size = N".
  Compute each statistic from the counts with these formulas; do not print only the Pearson chi-square line.
- PROC LIFETEST prints per stratum the product-limit table (one row per time, 4 decimals, survival, failure,
  survival SE, number failed, number left, censored rows marked *), "Quartile Estimates" with Percent (75, 50,
  25), Point Estimate and the Transform CI with 3 decimals such as 216.000, then Mean and Standard Error with
  3 decimals. The SE of the restricted mean is SAS's formula with the m/(m-1) factor, m = number of events in
  the stratum: SE = sqrt( m/(m-1) * sum_i A_i^2 * d_i / (n_i * (n_i - d_i)) ), where A_i is the area under the
  survival curve from event time t_i to the largest event time; without that factor the SE is wrong. Then
  "Summary of the Number of Censored and Uncensored Values" with Percent Censored to 2 decimals (11.76, not
  11.8), the rank statistics (Log-Rank and Wilcoxon per stratum), each covariance matrix as a FULL square
  (every stratum as a row and a column, so a 2-stratum matrix prints four numbers including the negative
  off-diagonals), and "Test of Equality over Strata" with rows Log-Rank, Wilcoxon and -2Log(LR).
- Reproduce every title and footnote verbatim, including formula text and literal dates inside them.
- Display formats: PICTURE formats truncate unless (ROUND) is given; w.d formats round half away from zero;
  percentages use the format's decimals. Match the format the program actually uses. Worked example:
  picture pct low-high = "009.9)" (prefix = "(") applied to 13.095 gives "(13.0)" and to 76.19 gives
  "(76.1)" (digits beyond the picture are cut, not rounded): implement as floor(x * 10) / 10 then
  sprintf("(%.1f)", ...), whereas put(13.095, 5.1) gives "13.1". A value the program multiplies by 100 after
  PROC MEANS/SUMMARY has already been turned into a percentage once; do not multiply again when displaying.
- PROC SUMMARY/MEANS with CLASS and no NWAY produces every class combination (_TYPE_): build the detail
  rows, each marginal and the overall total, and check that cells add up to the marginals.
- Keep intermediate steps close to the SAS structure so differences are easy to locate.
- Never print the full data to the console; the console log is only for progress and errors.
"""


def _fmt_pairs(pairs, label):
    if not pairs:
        return ""
    body = "\n".join(f"  {a} -> {b}" for a, b in pairs)
    return f"\n{label}:\n{body}\n"


def translate_prompt(ctx: ProgramContext, expected: dict) -> str:
    info = ctx.info
    vars_txt = "\n".join(f"  &{k}. = {v}" for k, v in ctx.macro_vars.items())
    libs = _fmt_pairs(ctx.libnames, "SAS librefs in effect (libref -> directory, or .xpt file for XPORT engine)")
    work = _fmt_pairs(ctx.work_inputs, "WORK datasets pre-loaded by the environment from these Excel files (dataset name -> file)")
    arts = []
    for a in expected["artifacts"]:
        if a["kind"] == "dataset":
            arts.append(f"  dataset {a['name']}  -> write OUT_DIR/{a['r_name']}")
        elif a["kind"] == "text":
            arts.append(f"  {a['name']} (table/listing text, {a['tokens']} numbers)  -> write a text rendering and CSV of the same table")
        else:
            arts.append(f"  figure {a['name']}  -> write OUT_DIR/{a['r_name']} and a CSV of the plotted data")
    arts_txt = "\n".join(arts) if arts else "  (the program's outputs were listings only; write each printed table as .txt and .csv)"
    macros = ""
    for path, text in ctx.macros:
        macros += f"\n--- macro / included file: {path} ---\n{text}\n"
    env = f"\n--- family environment ({info.family}) that ran before the program ---\n{ctx.env_text}\n" if ctx.env_text.strip() else ""
    return f"""Translate the SAS program below to R.

Program id: {info.id}   file: {info.file}   source: {info.source}   family: {info.family}
Declared output types: {info.outputs}

Macro variables set by the benchmark before the program ran:
{vars_txt}
{libs}{work}
Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
{arts_txt}
{env}{macros}
--- SAS program ({ctx.source_path}) ---
{ctx.source}
--- end of SAS program ---

Write the complete R script now."""


def repair_prompt(run_rc: int, run_log_tail: str, feedback: str, iteration: int) -> str:
    status = "ran without an R error" if run_rc == 0 else f"failed with exit code {run_rc}"
    return f"""The R script from your previous answer {status}. Comparison with the SAS reference outputs:

{feedback}

Tail of the R console log:
```
{run_log_tail}
```

Fix the script so the outputs match the SAS reference. How to repair:
- When the feedback names a column that differs on most rows, re-derive that column from the SAS source
  statement that creates it (its format, its input variable, its sort), using the SAS values shown in the
  feedback as the target; do not reshuffle unrelated code.
- When the feedback shows SAS numbers missing from your text output, add the missing block or format
  (the statistics table, the estimates listing, the footnote, the decimals) rather than rewriting the table.
- Keep everything that already matched: edit the previous script minimally instead of starting over.
- Always answer with the complete corrected script in one ```r code block (attempt {iteration}); never answer
  without code.
"""
