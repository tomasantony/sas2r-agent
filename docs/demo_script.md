> Superseded by [demo_narration.md](demo_narration.md), the narration of the recorded cut. Kept as the planning draft; the bracketed numbers were never filled in here.

# Demo video script (3 minutes)

Record from the dashboard (`python -m http.server 8765`, open
http://127.0.0.1:8765/dashboard/) with the repo open in a second window.
Numbers in brackets are filled from `runs/leaderboard.md` on recording day.

## 0:00 - 0:25  The problem

- Clinical trials submit analyses as SAS programs: datasets, tables, listings, figures.
- The industry is moving to R, and every program has to be translated and proven
  equivalent to the SAS original. Done by hand, that is weeks per study.
- sas2r-agent does the translation and the proof, program by program, with a Meta model.

## 0:25 - 1:05  How the agent works (show README "How it works", then one run folder)

- One loop, four roles: context builder reads the SAS program, its macros, inputs
  and the SAS reference outputs; translator (Muse Spark 1.3 through the Meta
  Model API) writes one R script; a Docker sandbox with no network runs it;
  the comparator checks every output against the SAS reference (dataset cells,
  table numbers, figures) and sends a repair prompt with the differences.
- Up to three attempts. Every attempt leaves a provenance record: hashes of the
  prompt, the answer, the script, every file produced, the SAS references it was
  compared with, chained into `runs/ledger.jsonl`. `python -m agent verify`
  proves nothing was edited afterwards.

## 1:05 - 1:50  The benchmark and the leaderboard (dashboard)

- 100 public SAS programs (PHUSE, CDISC pilot, PharmaSUG, plus my own), 90 clinical
  and 10 general, each run in real SAS to produce the frozen reference outputs.
  Pre-existing work, in the separate sas2r-benchmark repo.
- Scorecard: Muse Spark 1.3 [v3: xx% equivalent at $x.xx per 100 programs];
  Claude Opus 5.5 baseline 97% at $19.45; Muse Spark Open (open weights) on
  Baseten and Ollama Cloud [xx%]; gpt-oss-120b baseline 29%.
- Price per correct translation is the column that matters: Muse Spark at
  [$0.0xx] versus Claude at $0.201.

## 1:50 - 2:30  The agent improves itself from its failures (program grid)

- Click a red cell from the v1 row, open the comparison: the feedback names
  the column or the missing numbers. The failure analysis of the first sweep
  became prompt v2 (strict date parsing, full procedure output, join hygiene):
  86% to 95%. The five remaining misses became v3 (PROC IMPORT informats,
  PICTURE truncation, PROC FREQ and LIFETEST formulas): [95% to xx%].
- Each prompt version is its own leaderboard row, so the improvement is
  measured, not claimed.

## 2:30 - 3:00  Drill-down and close

- Click a green cell: the generated R script, the comparison report, the
  provenance record with its hash chain position.
- Everything in the entry repo was built inside the window; the ledger has
  [n] entries. Next: more programs, figure comparison by image, and a
  reviewer-facing diff view for the regulatory submission package.
