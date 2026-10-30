| row | model | prompt | programs | runs | all outputs | equivalent | mean iters | mean sec | list $/M in/out | cost | $/program | $/equivalent |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| baseline | claude-opus-5-5 (claude-opus-5-5) | v1 | 35 | 100% | 100% | 100% | 1.46 | 68 | 4.00/20.00 | $7.26 | $0.207 | $0.207 |
| baseline | gpt-oss-120b-ollama (gpt-oss:120b) | v1 | 51 | 53% | 61% | 24% | 2.75 | 27 | 0.15/0.60 | $0.37 | $0.007 | $0.031 |
| meta | muse-spark (muse-spark-1.3) | v1 | 100 | 100% | 100% | 86% | 1.99 | 109 | 1.25/4.25 | $11.81 | $0.118 | $0.137 |
| meta | muse-spark-open-baseten (meta-models/muse-spark-open-1026) | v1 | 71 | 99% | 99% | 80% | 2.10 | 60 | 1.25/4.25 | $5.68 | $0.080 | $0.100 |
| meta | muse-spark-open-ollama (muse-spark-open) | v1 | 2 | 100% | 100% | 100% | 2.00 | 72 | 1.25/4.25 | $0.14 | $0.070 | $0.070 |

Costs are list prices per million tokens on 2026-10-29 applied to the tokens each run used (reasoning tokens billed as output); the hackathon credits that actually paid for the runs are not reflected.
