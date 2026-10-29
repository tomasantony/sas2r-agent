# sas2r-agent

An agentic SAS-to-R translator for clinical trial programming, built for the
**Meta Global AI Developer Hackathon** (Indie track, 26 October to 4 November 2026).

The agent takes a SAS program that produces a clinical dataset, table, listing or
graph, translates it to R with Meta's Muse Spark models, runs the R, and compares
the R output against the real SAS output. Results are scored on a 100-program
benchmark and shown on a leaderboard.

## Status

Build in progress. This repository was created on 29 October 2026 and contains
only work done inside the hackathon window.

## What was pre-existing

Per the hackathon rules, everything that existed before the window lives outside
this repository and is only referenced from here:

- **sas2r-benchmark** (separate repository, to be linked here when published):
  the 100-program SAS benchmark corpus, its input data, the reference outputs
  produced by SAS 9.4, and the runner and audit scripts. The corpus was
  assembled from open sources (PHUSE scripts, CDISC pilot, PharmaSUG and other
  MIT/Apache-licensed repositories) plus the author's own programs run on a
  synthetic study. Nothing in it is client code.
- Hand-written R reference translations for a handful of programs, held out
  from the agent and used only for scoring.

Everything else, the agent, the model calls, the comparison harness and the
dashboard, is written during the window.

## Meta tooling

- Muse Spark 1.3 via the Meta Model API (primary translation and repair loop)
- Muse Spark Open via Baseten (open-weight comparison row on the leaderboard)
- Muse Code used during development

## Layout (planned)

```
agent/        translation and repair loop
compare/      SAS vs R output comparison and scoring
dashboard/    leaderboard
runs/         per-model results on the benchmark
```

## Running

Instructions will be added as the pieces land. API keys are read from a `.env`
file that is never committed.

## Licence

MIT, see `LICENSE`.
