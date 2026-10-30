| row | model | prompt | programs | runs | all outputs | equivalent | mean iters | mean sec | list $/M in/out | cost | $/program | $/equivalent |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| baseline | claude-opus-5-5 (claude-opus-5-5) | v1 | 100 | 100% | 100% | 97% | 1.42 | 62 | 4.00/20.00 | $19.45 | $0.195 | $0.201 |
| baseline | gpt-oss-120b-ollama (gpt-oss:120b) | v1 | 100 | 56% | 61% | 29% | 2.67 | 27 | 0.15/0.60 | $0.77 | $0.008 | $0.027 |
| meta | muse-spark (muse-spark-1.3) | v1 | 100 | 100% | 100% | 86% | 1.99 | 109 | 1.25/4.25 | $11.81 | $0.118 | $0.137 |
| meta | muse-spark-open-baseten (meta-models/muse-spark-open-1026) | v1 | 100 | 96% | 96% | 81% | 2.03 | 54 | 1.25/4.25 | $7.80 | $0.078 | $0.096 |
| meta | muse-spark-open-ollama (muse-spark-open) | v1 | 2 | 100% | 100% | 100% | 2.00 | 72 | 1.25/4.25 | $0.14 | $0.070 | $0.070 |
| meta | muse-spark@v2 (muse-spark-1.3) | v2 | 100 | 100% | 100% | 95% | 1.66 | 115 | 1.25/4.25 | $9.37 | $0.094 | $0.099 |

Costs are list prices per million tokens on 2026-10-29 applied to the tokens each run used (reasoning tokens billed as output); the hackathon credits that actually paid for the runs are not reflected.
