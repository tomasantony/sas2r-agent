# Demo narration (current cut)

Word for word, with the visual shown for each line. Generated from `demo_narration.json`; the cue sheet for a human voice-over is the same text.

| line | visual | words |
|---|---|---|
| s01a | docs/demo_title.html, SAS to R problem card | SAS is the industry-standard software used to clean, analyze, and report clinical trial data for regulatory submissions to agencies like the FDA. Today, the biopharma industry is moving to R, a modern open-source platform. I have been a clinical SAS programmer in pharma since 2008, and have used R since 2016. |
| s01b | docs/demo_opening.html, SAS program and output beside the generated R and its output | However, sponsors hold vast libraries of legacy SAS programs that must be converted to R code, and each must be rigorously validated against the original. A script that runs cleanly can still produce incorrect numbers, making dual programming 30 to 50% of the entire programming effort. That bottleneck is why I built sas2r-agent. |
| s02 | dashboard top, bars | It runs on Meta's Muse Spark 1.3 and was built in the hackathon window. It writes the R, runs it, finds the numbers that differ, and repairs its own translation. The leaderboard: one hundred programs. |
| s03 | docs/demo_flow.html, the loop as a node graph | One loop, four roles. The context builder reads the SAS program, its inputs and outputs. Muse Spark 1.3, through the Meta Model API, writes one R script. A sandbox runs it. The comparator checks the outputs against the SAS reference and sends the differences back, up to three times. |
| s04 | terminal page, run output (punched in on P036), then docs/demo_repair.html (the P036 miss and the repair) | Two programs. The first matches SAS straight away. The second doesn't: the comparator lists the missing values, sends them back, and the second attempt matches. |
| s04b | docs/demo_compare.html | A match, by eye: the SAS figure on the left, the figure from the generated R on the right. Same curves, same numbers at risk. Below, the statistics table the comparator checks, identical to four decimals. |
| s05 | terminal page, verify output (punched in on the ledger lines) | Every attempt leaves a provenance trail: hashes of the prompt, the answer, the script, the outputs and the SAS reference, in an append-only ledger. |
| s06 | dashboard bars | One hundred public SAS programs, run in real SAS. Muse Spark 1.3 passes every check on ninety-four. On the thirty-nine that produce datasets it matches thirty-seven cell for cell, the same as Claude Opus, and it passes three programs Claude misses, at half the cost. |
| s07 | scorecard, Claude Opus and Muse Spark v3 rows outlined, $ per passed column | What matters is cost per passed program, not per call. Muse Spark: about ten cents. Muse Spark Open, the open weights, scores eighty-five, so a sponsor can run this inside its own firewall. |
| s08 | program grid, then P079 under prompt v1 | The agent learns from its misses. Here's a listing prompt version one got wrong: four of three hundred and twelve numbers found in the R output. |
| s09 | P079 under prompt v3 | Analysing those misses gave prompt version two: eighty-six to ninety-five. Version three fixed the five that were left and lost six others: ninety-four, within the swing between two runs. |
| s10 | generated R in the detail panel | Any cell opens the generated R, the comparison and the provenance record. All fifteen hundred scripts are in the repo. |
| s11 | README top, then docs/demo_closing.html (end card) | One limit: figures are checked for being produced, not compared. The benchmark is my earlier work. sas2r-agent doesn't just generate R code. It tests whether R reproduces SAS, and uses the evidence to fix what doesn't. |
