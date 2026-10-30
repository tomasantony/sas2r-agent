"""Aggregate per-program reports into a leaderboard (JSON, Markdown, HTML)."""
from __future__ import annotations

import json
from pathlib import Path

from .config import RUNS_DIR, REPO_ROOT


def collect() -> dict[str, list[dict]]:
    out: dict[str, list[dict]] = {}
    for prov in sorted(p for p in RUNS_DIR.iterdir() if p.is_dir() and p.name != "expected" and not p.name.startswith("_")):
        rows = []
        for rep in sorted(prov.glob("P*/report.json")):
            rows.append(json.loads(rep.read_text(encoding="utf-8")))
        if rows:
            out[prov.name] = rows
    return out


def summarise(rows: list[dict]) -> dict:
    from .config import estimate_cost_usd, price_for
    n = len(rows)
    def pct(key):
        return 100.0 * sum(1 for r in rows if r[key]) / n if n else 0.0
    prov = rows[0]["provider"] if rows else ""
    tin = sum(r["input_tokens"] for r in rows)
    tout = sum(r["output_tokens"] for r in rows)
    cost = estimate_cost_usd(prov, tin, tout)
    n_eq = sum(1 for r in rows if r["equivalent"])
    return {
        "programs": n,
        "translated_pct": pct("translated"),
        "runs_pct": pct("runs"),
        "produced_all_pct": pct("produced_all"),
        "equivalent_pct": pct("equivalent"),
        "mean_score": sum(r["best_score"] for r in rows) / n if n else 0.0,
        "mean_iterations": sum(r["iterations"] for r in rows) / n if n else 0.0,
        "input_tokens": tin,
        "output_tokens": tout,
        "list_price_per_m": price_for(prov),
        "cost_usd": cost,
        "cost_per_program_usd": cost / n if n else 0.0,
        "cost_per_equivalent_usd": cost / n_eq if n_eq else None,
        "mean_seconds": sum(r["seconds"] for r in rows) / n if n else 0.0,
        "model": rows[0]["model"] if rows else "",
        "prompt_versions": sorted({r.get("prompt_version", 1) for r in rows}),
    }


def by_group(rows: list[dict], manifest_groups: dict[str, str]) -> dict[str, dict]:
    groups: dict[str, list[dict]] = {}
    for r in rows:
        groups.setdefault(manifest_groups.get(r["id"], "?"), []).append(r)
    return {g: summarise(v) for g, v in groups.items()}


def build(write_html: bool = True) -> dict:
    from .context import load_manifest
    groups = {k: v.group for k, v in load_manifest().items()}
    data_all = collect()
    # a row needs a reasonable share of the benchmark to be comparable; pilots are listed, not charted
    MIN_PROGRAMS = 20
    partial = {k: len(v) for k, v in data_all.items() if len(v) < MIN_PROGRAMS}
    data = {k: v for k, v in data_all.items() if len(v) >= MIN_PROGRAMS}
    from .config import PROVIDERS
    board = {prov: {"overall": summarise(rows), "by_group": by_group(rows, groups),
                    "role": PROVIDERS[prov.split("@")[0]].role if prov.split("@")[0] in PROVIDERS else "?",
                    "note": PROVIDERS[prov.split("@")[0]].note if prov.split("@")[0] in PROVIDERS else "",
                    "programs": [{k: r.get(k) for k in ("id", "family", "translated", "runs", "produced_all", "equivalent", "best_score", "iterations", "best_attempt", "input_tokens", "output_tokens", "seconds")} for r in rows]}
             for prov, rows in data.items()}
    RUNS_DIR.mkdir(parents=True, exist_ok=True)
    (RUNS_DIR / "leaderboard.json").write_text(json.dumps(board, indent=1), encoding="utf-8")
    from .config import PROVIDERS
    md = ["| row | model | prompt | programs | runs | all outputs | equivalent | mean iters | mean sec | list $/M in/out | cost | $/program | $/equivalent |",
          "|---|---|---|---|---|---|---|---|---|---|---|---|---|"]
    for prov, b in board.items():
        o = b["overall"]
        role = PROVIDERS[prov.split("@")[0]].role if prov.split("@")[0] in PROVIDERS else "?"
        pv = "v" + "+".join(str(v) for v in o.get("prompt_versions", [1]))
        pin, pout = o["list_price_per_m"]
        per_eq = f"${o['cost_per_equivalent_usd']:.3f}" if o["cost_per_equivalent_usd"] is not None else "n/a"
        md.append(f"| {role} | {prov} ({o['model']}) | {pv} | {o['programs']} | {o['runs_pct']:.0f}% | "
                  f"{o['produced_all_pct']:.0f}% | {o['equivalent_pct']:.0f}% | {o['mean_iterations']:.2f} | {o['mean_seconds']:.0f} | "
                  f"{pin:.2f}/{pout:.2f} | ${o['cost_usd']:.2f} | ${o['cost_per_program_usd']:.3f} | {per_eq} |")
    md.append("")
    if partial:
        md.append("Partial runs (fewer than %d programs, not charted): %s" % (MIN_PROGRAMS, ", ".join(f"{k} ({n} programs)" for k, n in partial.items())))
        md.append("")
    md.append("Costs are list prices per million tokens on 2026-10-29 applied to the tokens each run used "
              "(reasoning tokens billed as output); the hackathon credits that actually paid for the runs are not reflected.")
    (RUNS_DIR / "leaderboard.md").write_text("\n".join(md) + "\n", encoding="utf-8")
    if write_html:
        import time
        from .context import load_manifest as _lm
        dash = REPO_ROOT / "dashboard"
        dash.mkdir(exist_ok=True)
        (dash / "leaderboard.json").write_text(json.dumps(board, indent=1), encoding="utf-8")
        ledger = RUNS_DIR / "ledger.jsonl"
        n_ledger = sum(1 for _ in ledger.open(encoding="utf-8")) if ledger.exists() else 0
        programs = [{"id": k, "group": v.group, "family": v.family} for k, v in _lm().items()]
        payload = {"board": board, "programs": programs,
                   "meta": {"built_at": time.strftime("%Y-%m-%d %H:%M"), "ledger_entries": n_ledger,
                            "benchmark": "tomasantony/sas2r-benchmark"}}
        # data.js lets dashboard/index.html work from file:// as well as over HTTP / GitHub Pages
        (dash / "data.js").write_text("window.BOARD = " + json.dumps(payload) + ";\n", encoding="utf-8")
    return board
