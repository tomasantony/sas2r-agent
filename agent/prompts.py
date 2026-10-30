"""Prompt templates for translation and repair.

PROMPT_VERSION is recorded in every report and provenance record so leaderboard rows produced with
different prompts are never mixed up. Bump it whenever SYSTEM or repair_prompt changes.
  1  initial prompt (sweeps of 29 Oct 2026)
  2  rules distilled from the 16 programs Muse Spark missed and Claude Opus matched (29 Oct 2026):
     strict date parsing, derive missing upstream datasets, join hygiene, full default procedure output,
     titles/footnotes verbatim, PICTURE truncation, CLASS marginals, repair discipline
"""
from __future__ import annotations

from .config import R_PACKAGES
from .context import ProgramContext

PROMPT_VERSION = 2

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
- If the SAS program reads a dataset that is not among the listed inputs (an upstream SDTM/ADaM dataset),
  derive it from the raw inputs using the standard CDISC definition of each variable it needs
  (e.g. DM.RFSTDTC is the reference start, the randomisation or first-exposure date); do not substitute
  a different variable because it is convenient.
- Joins: before merging, drop or rename every column present in both tables except the keys, so no .x/.y
  suffixes appear; afterwards stopifnot() that the columns you filter on exist. A zero-row result from a
  filter that SAS populates is a bug in the join, not an answer.
- Tables and listings must contain every block the SAS procedure prints by default, not only the headline:
  PROC FREQ with CHISQ/FISHER/EXACT prints the full statistics table (chi-square, likelihood ratio,
  Mantel-Haenszel, phi, contingency coefficient, Cramer's V, Fisher cells); PROC LIFETEST prints the
  product-limit estimates (one row per time), quartiles with CIs, mean and SE (restricted to the largest
  event time), the censored summary, rank statistics (O-E), covariance matrices and the test table;
  PROC MEANS/UNIVARIATE print their default statistic sets. Use the decimals SAS prints (4 for statistics).
- Reproduce every title and footnote verbatim, including formula text and literal dates inside them.
- Display formats: PICTURE formats truncate unless (ROUND) is given; w.d formats round half away from zero;
  percentages use the format's decimals. Match the format the program actually uses.
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
