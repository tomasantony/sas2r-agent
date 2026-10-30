# sas2r-agent

An agentic SAS-to-R translator for clinical trial programming, built for the
**Meta Global AI Developer Hackathon** (Indie track, 26 October to 4 November 2026).

The agent takes a SAS program that produces a clinical dataset, table, listing or
graph, translates it to R with Meta's Muse Spark models, runs the R in a sandbox,
compares every R output against the real SAS output, and repairs its own translation
from the comparison feedback. Results are scored on a 100-program benchmark and
shown on a leaderboard, with a tamper-evident provenance record for every attempt.

## Status

This repository was created on 29 October 2026 and contains only work done inside
the hackathon window. The agent, sandbox, comparator, provenance ledger, leaderboard
and dashboard are working; model sweeps are in progress (see `runs/leaderboard.md`).

## What was pre-existing

Per the hackathon rules, everything that existed before the window lives outside
this repository and is only referenced from here:

- **sas2r-benchmark** (`github.com/tomasantony/sas2r-benchmark`, published at
  submission): the 100-program SAS benchmark corpus, its input data, the reference
  outputs produced by SAS 9.4 (600 files, frozen and hashed in
  `runs/reference_freeze.json`), and the runner and audit scripts. The corpus was
  assembled from open sources (PHUSE scripts, CDISC pilot, PharmaSUG and other
  MIT/Apache-licensed repositories) plus the author's own programs run on a
  synthetic study. Nothing in it is client code.

Everything else, the agent, the model calls, the comparison harness, the provenance
ledger and the dashboard, was written during the window.

## How it works

One loop with four roles, up to three attempts per program:

1. **Context builder** (`agent/context.py`): reads the SAS program, the family
   environment it ran under (librefs, macro variables, included macros, raw
   inputs) and the list of outputs the SAS run produced.
2. **Translator** (`agent/prompts.py`, `agent/llm.py`): asks the model for one
   complete R script that writes every expected output under `OUT_DIR`.
3. **Sandboxed executor** (`agent/runner.py`, `sandbox/Dockerfile`): runs the
   script in a Docker container with no network, a read-only mount of the
   benchmark, a 6 GB memory cap and only the attempt folder writable
   (`SANDBOX=local` falls back to the host `Rscript`).
4. **Comparator and repair** (`agent/compare.py`): datasets are compared cell by
   cell (numeric tolerance, date normalisation, row re-matching), tables and
   listings by recall of the numbers in the SAS output, figures by presence of
   the image. A program is **equivalent** only when every artifact matches. When
   it is not, the comparison feedback and the R log go back to the model for a
   repair attempt.

Every attempt writes `provenance.json` (SHA-256 of prompt, answer, reasoning,
script, every output, the R log and the SAS reference files it was compared to,
plus model, tokens, git commits and sandbox settings) and appends a hash-chained
entry to `runs/ledger.jsonl`. `python -m agent verify` re-checks the reference
hashes and the chain.

## Meta tooling

- **Muse Spark 1.3** via the Meta Model API (`api.meta.ai/v1`), the primary row.
- **Muse Spark Open** (open weights) via Baseten (`meta-models/muse-spark-open-1026`)
  and via Ollama Cloud, the open-weight rows. With the same prompt (v3) the same
  weights score 85% on Baseten and 81% on Ollama Cloud, where three programs never
  produced code before the output limit and a program took three times longer:
  serving settings are part of the result, which is why the host is in the row name.
- Baselines, labelled as such on the leaderboard: OpenAI gpt-oss-120b (open
  weight, Ollama Cloud) and Claude Opus 5.5 (Anthropic SDK).
- Muse Code was used during development.

## Leaderboard

`runs/leaderboard.md` is the current table; `dashboard/index.html` is the
interactive version (bar chart, scorecard with list-price cost per program and
per equivalent translation, a program-by-model grid, and drill-down into the
generated R, the comparison and the provenance record of any cell). Open it
from a local HTTP server or GitHub Pages so the drill-down can read `runs/`:

```
python -m http.server 8765
# then http://127.0.0.1:8765/dashboard/
```

### Prompt versions: the agent improving from its own failures

Each prompt version is a separate leaderboard row (`run --tag vN`), recorded in
every report and provenance record, so an improvement is measured on the same
100 programs rather than claimed. Muse Spark 1.3:

| prompt | equivalent | mean attempts | cost (list) | $/equivalent | what changed |
|---|---|---|---|---|---|
| v1 | 86% | 1.99 | $11.81 | $0.137 | initial prompt |
| v2 | 95% | 1.66 | $9.37 | $0.099 | rules from the 16 programs v1 missed and Claude matched: strict date parsing, derive missing upstream datasets, join hygiene, full default procedure output, PICTURE truncation, CLASS marginals, repair discipline |
| v3 | 94% | 1.60 | $8.90 | $0.095 | rules from the 5 programs v2 missed: PROC IMPORT picks one date informat per column, PICTURE worked example, PROC FREQ CHISQ statistics with formulas, PROC LIFETEST details (SE of the restricted mean, full covariance matrices) |

v3 fixed all five programs it targeted (P009, P036, P040, P045, P053) and lost
six borderline ones that v2 had passed only on its last repair attempt (P019,
P022, P035, P056, P061, P097). Every program passes under v2 or v3, so a single
sweep's score carries a variance of about five programs; the cost and attempt
counts fall monotonically with each version. The open-weight model on Baseten
went from 81% (v1) to 85% (v3) with the same prompt change.

## Running

```
pip install -r requirements.txt
cp .env.example .env            # add MODEL_API_KEY (Meta), BASETEN_API_KEY, OLLAMA_API_KEY, ANTHROPIC_API_KEY
git clone https://github.com/tomasantony/sas2r-benchmark D:\meta_hackathon\sas2r-agent   # or set BENCHMARK_ROOT
docker build -t sas2r-sandbox:r4.6 sandbox/                                              # or set SANDBOX=local

python -m agent check                                   # paths, keys present (never printed)
python -m agent expected --all                          # expected artifacts per program from the SAS outputs
python -m agent selftest --ids P001 P020 P064           # SAS reference fed back through the comparator: must score 1.0
python -m agent run --provider muse-spark --ids P001    # one program
python -m agent run --provider muse-spark --all --skip-done --budget-usd 50   # a sweep with a spend guard
python -m agent rescore --all                           # re-compare existing attempts after a comparator change (no model calls)
python -m agent leaderboard                             # runs/leaderboard.{json,md} and dashboard/data.js
python -m agent verify                                  # reference hashes and ledger chain
```

Providers are defined in `agent/config.py` (`muse-spark`, `muse-spark-open-baseten`,
`muse-spark-open-ollama`, `gpt-oss-120b-ollama`, `claude-opus-5-5`, `local`).
Cost columns use each vendor's published list price per million tokens
(`_DEFAULT_PRICES`, overridable with `PRICE_<PROVIDER>=in,out` in `.env`);
hackathon credits are not reflected.

## Layout

```
agent/        context builder, prompts, LLM clients, sandboxed runner, comparator, loop, provenance, leaderboard, CLI
sandbox/      Dockerfile for the R 4.6 sandbox image
dashboard/    static leaderboard (index.html + data.js written by `agent leaderboard`)
runs/         per-provider results: <provider>/<Pnnn>/attempt_n/{script.R, out/, run.log, compare.json, provenance.json}
              ledger.jsonl (hash chain), reference_freeze.json, leaderboard.{json,md}, usage logs
```

## Licence

MIT, see `LICENSE`.
