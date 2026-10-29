"""Prompt templates for translation and repair."""
from __future__ import annotations

from .config import R_PACKAGES
from .context import ProgramContext

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

Fix the script so the outputs match the SAS reference. Keep everything that already matched. Return the complete
corrected R script in one ```r code block (attempt {iteration})."""
