# Demo narration, word for word (target 2:50 at a normal speaking pace)

Setup before recording: `python -m http.server 8765` in the repo, Chrome on
http://127.0.0.1:8765/dashboard/ scrolled to the top, a terminal in the repo with
a large font, and the README open in a second tab. Keep the mouse still while
talking; move it only on the cue lines.

| time | on screen | say |
|---|---|---|
| 0:00 | dashboard top, title and tiles | "Every clinical trial submits its analyses as SAS programs: datasets, tables, listings and figures. The industry is moving to R, and each program has to be translated and then proven to give the same result as the SAS original. Done by hand, that is weeks per study and the proof is the expensive part." |
| 0:20 | same | "sas2r-agent does both: it translates each program with a Meta model, and it proves equivalence by running the R and comparing every output against the SAS reference. This is the leaderboard of that proof over one hundred programs." |
| 0:35 | README, "How it works" section | "One loop, four roles. The context builder reads the SAS program, its macros, its inputs and the SAS outputs we are trying to match. The translator, Muse Spark 1.3 through the Meta Model API, writes one R script. A Docker sandbox with no network runs it. The comparator checks every dataset cell, every table number and every figure against the frozen SAS reference and sends the differences back as a repair prompt. Up to three attempts." |
| 1:00 | terminal: `python -m agent run --provider muse-spark --ids P009 --tag demo` already running, log scrolling | "Here it is on one program. The first attempt runs, the comparator reports which column differs, and the second attempt matches." |
| 1:10 | terminal: `python -m agent verify` output | "Every attempt leaves a provenance record: hashes of the prompt, the answer, the script, every file produced and the SAS reference it was compared with, chained into an append-only ledger. Verify recomputes the whole chain." |
| 1:25 | dashboard bar chart, cue: hover Claude bar, then Muse Spark v3 bar | "The benchmark is one hundred public SAS programs, ninety clinical and ten general, run in real SAS to produce the reference outputs. Claude Opus 5.5 is the baseline at ninety-seven percent. Muse Spark 1.3 reaches ninety-five, at half the price per correct translation." |
| 1:45 | scorecard, cue: point at the $/equivalent column | "That column is the one that matters for a company: the cost of one translation that fully matched SAS. Muse Spark is about ten cents. The open-weight Muse Spark Open runs at eighty-five percent on Baseten, so the whole pipeline also works without any API." |
| 2:00 | program grid, cue: click a red cell in the Muse Spark v1 row (P079), detail panel opens | "The agent improved from its own failures. Here is a program the first prompt missed: the join produced the wrong column and the comparison shows it." |
| 2:15 | cue: click the same program in the v3 row, detail panel shows match | "The failure analysis of those misses became prompt version two: strict date parsing, full procedure output, join hygiene. Eighty-six to ninety-five percent. The five remaining misses became version three, with the exact SAS formulas. Each version is its own row, so the improvement is measured on the same hundred programs, not claimed." |
| 2:35 | detail panel, cue: click the script and the provenance links | "Any cell opens the generated R, the comparison report and the provenance record with its position in the ledger." |
| 2:45 | README top | "Everything in this repo was built inside the hackathon window. The benchmark is pre-existing work in its own repo. Next: image comparison for figures and a reviewer-facing diff for the submission package. Thank you." |

Word count about 470; at 160 words per minute that is 2 minutes 55 seconds.
If it runs long, cut the 1:00 terminal segment and say "a run log is in every attempt folder" instead.

## Commands to have ready

```
python -m http.server 8765
python -m agent run --provider muse-spark --ids P009 --tag demo
python -m agent verify
```

The demo run writes under `runs/muse-spark@demo/` and takes about one minute; start it
just before you press record so the log is scrolling at the 1:00 mark.
