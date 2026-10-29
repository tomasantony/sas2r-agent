| row | model | programs | runs | all outputs | equivalent | mean iters | mean sec | list $/M in/out | cost | $/program | $/equivalent |
|---|---|---|---|---|---|---|---|---|---|---|---|
| baseline | claude-opus-5-5 (claude-opus-5-5) | 3 | 100% | 100% | 100% | 1.00 | 27 | 4.00/20.00 | $0.23 | $0.075 | $0.075 |
| baseline | gpt-oss-120b-ollama (gpt-oss:120b) | 8 | 62% | 62% | 38% | 2.50 | 23 | 0.15/0.60 | $0.06 | $0.008 | $0.020 |
| meta | muse-spark (muse-spark-1.3) | 100 | 100% | 100% | 84% | 1.99 | 109 | 1.25/4.25 | $11.81 | $0.118 | $0.141 |
| meta | muse-spark-open-baseten (meta-models/muse-spark-open-1026) | 24 | 100% | 100% | 79% | 2.21 | 55 | 1.25/4.25 | $2.12 | $0.089 | $0.112 |
| meta | muse-spark-open-ollama (muse-spark-open) | 2 | 100% | 100% | 100% | 2.00 | 72 | 1.25/4.25 | $0.14 | $0.070 | $0.070 |

Costs are list prices per million tokens on 2026-10-29 applied to the tokens each run used (reasoning tokens billed as output); the hackathon credits that actually paid for the runs are not reflected.
