"""Command line: python -m agent <command> ...

  check                         show configuration and which provider keys are present (never the keys)
  models --provider NAME        list the models the endpoint serves
  expected [--ids ...] [--force]  build the expected-artifact cache from the benchmark
  context ID                    print the prompt that would be sent for one program
  run --provider NAME --ids P001 P004 ... [--max-iters N] [--all] [--group clinical|general]
  selftest --ids ...            feed the SAS reference outputs back through the comparator (must score 1.0)
  leaderboard                   aggregate runs/ into leaderboard.json/.md and dashboard/
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from .config import BENCHMARK_ROOT, PROVIDERS, RSCRIPT, RUNS_DIR, SETTINGS


def ids_from_args(args) -> list[str]:
    from .context import load_manifest
    man = load_manifest()
    if getattr(args, "all", False):
        ids = list(man)
    elif getattr(args, "ids", None):
        ids = []
        for tok in args.ids:
            if "-" in tok and tok.count("-") == 1 and tok[0] == "P":
                a, b = tok.split("-")
                ids += [f"P{n:03d}" for n in range(int(a[1:]), int(b[1:] if b.startswith("P") else b) + 1)]
            else:
                ids.append(tok)
    else:
        ids = []
    if getattr(args, "group", None):
        ids = [i for i in ids if man[i].group == args.group]
    return ids


def cmd_check(args):
    print(f"benchmark root : {BENCHMARK_ROOT}  exists={BENCHMARK_ROOT.exists()}")
    print(f"runs dir       : {RUNS_DIR}")
    print(f"Rscript        : {RSCRIPT}  exists={Path(RSCRIPT).exists()}")
    print(f"settings       : {SETTINGS}")
    for name, p in PROVIDERS.items():
        print(f"provider {name:26s} model={p.model:40s} base={p.base_url:40s} key {p.api_key_env}: {'set' if p.api_key else 'MISSING'}")


def cmd_models(args):
    from .llm import LLM
    print("\n".join(LLM(PROVIDERS[args.provider]).list_models()))


def cmd_expected(args):
    from .expected import build_expected
    for pid in ids_from_args(args) or [f"P{n:03d}" for n in range(1, 101)]:
        d = build_expected(pid, force=args.force)
        kinds = [a["kind"][0] for a in d["artifacts"]]
        print(f"{pid}: {len(d['artifacts'])} artifacts ({''.join(kinds)})")


def cmd_context(args):
    from .context import build_context
    from .expected import build_expected
    from .prompts import translate_prompt
    ctx = build_context(args.id)
    print(translate_prompt(ctx, build_expected(args.id)))


def provider_spend(provider_name: str) -> tuple[int, int, float]:
    """Tokens and estimated USD this provider has used so far, from its usage log."""
    import json
    from .config import estimate_cost_usd
    log = RUNS_DIR / provider_name / "usage.jsonl"
    tin = tout = 0
    if log.exists():
        for line in log.read_text(encoding="utf-8").splitlines():
            try:
                d = json.loads(line)
                tin += d.get("in", 0)
                tout += d.get("out", 0)
            except json.JSONDecodeError:
                continue
    return tin, tout, estimate_cost_usd(provider_name, tin, tout)


def cmd_run(args):
    from .loop import run_program
    prov = PROVIDERS[args.provider]
    ids = ids_from_args(args)
    if not ids:
        sys.exit("no program ids given (use --ids P001 P002 or --all)")
    if args.skip_done:
        ids = [i for i in ids if not (RUNS_DIR / prov.name / i / "report.json").exists()]
    tin, tout, spent = provider_spend(prov.name)
    print(f"{prov.name}: {len(ids)} programs to run; spent so far ${spent:.2f} ({tin:,} in / {tout:,} out); "
          f"budget ${args.budget_usd:.2f}" if args.budget_usd else f"{prov.name}: {len(ids)} programs to run", flush=True)
    for pid in ids:
        if args.budget_usd:
            _, _, spent = provider_spend(prov.name)
            if spent >= args.budget_usd:
                print(f"BUDGET STOP: estimated ${spent:.2f} >= ${args.budget_usd:.2f} before {pid}", flush=True)
                break
        rep = run_program(pid, prov, max_iterations=args.max_iters)
        _, _, spent = provider_spend(prov.name)
        print(f"{pid}: translated={rep.translated} runs={rep.runs} all_outputs={rep.produced_all} "
              f"equivalent={rep.equivalent} score={rep.best_score:.3f} iters={rep.iterations} "
              f"tokens={rep.input_tokens}/{rep.output_tokens} {rep.seconds:.0f}s  spent=${spent:.2f}", flush=True)


def cmd_selftest(args):
    """Convert the SAS reference outputs to the R-side formats and compare them with themselves."""
    import shutil
    import pandas as pd
    from .compare import compare_run, read_sas_dataset
    from .expected import build_expected, extract_text
    for pid in ids_from_args(args):
        exp = build_expected(pid)
        out = RUNS_DIR / "_selftest" / pid
        shutil.rmtree(out, ignore_errors=True)
        out.mkdir(parents=True)
        for a in exp["artifacts"]:
            p = Path(a["path"])
            if a["kind"] == "dataset":
                df = read_sas_dataset(p)
                for c in df.columns:
                    if pd.api.types.is_datetime64_any_dtype(df[c]):
                        df[c] = df[c].dt.strftime("%Y-%m-%dT%H:%M:%S").str.replace("T00:00:00", "", regex=False)
                df.to_csv(out / a["r_name"], index=False, na_rep="")
            elif a["kind"] == "text":
                (out / a["r_name"]).write_text(extract_text(p), encoding="utf-8")
            else:
                (out / a["r_name"]).write_bytes(b"png")
        rep = compare_run(exp, out)
        flag = "OK " if rep.equivalent else "BAD"
        print(f"{flag} {pid}: score={rep.score:.4f} " + "; ".join(f"{r.kind}:{r.name}={r.status}/{r.score:.3f}" for r in rep.artifacts))


def cmd_keys(args):
    from .keys import scan, update_env
    folder = Path(args.folder)
    found = scan(folder)
    if not found:
        sys.exit(f"no key files recognised in {folder}")
    for line in update_env(found):
        print("set", line)


def cmd_freeze(args):
    from .provenance import freeze_reference, FREEZE
    d = freeze_reference(force=True)
    print(f"froze {len(d['files'])} SAS reference files at benchmark commit {d['benchmark_commit'][:12]} -> {FREEZE}")


def cmd_verify(args):
    """Check the SAS reference files against the freeze and the run ledger's hash chain."""
    from .provenance import freeze_reference, verify_reference, verify_ledger
    d = freeze_reference()
    base = BENCHMARK_ROOT / "output" / "sas"
    bad = [rel for rel in d["files"] if not (base / rel).exists() or not verify_reference(base / rel)["ok"]]
    print(f"reference files: {len(d['files'])}, changed or missing: {len(bad)}")
    for rel in bad[:20]:
        print("  changed:", rel)
    n, first_bad = verify_ledger()
    print(f"ledger entries: {n}, chain {'intact' if first_bad < 0 else f'BROKEN at entry {first_bad}'}")


def cmd_leaderboard(args):
    from .leaderboard import build
    board = build()
    print((RUNS_DIR / "leaderboard.md").read_text(encoding="utf-8"))


def main(argv=None):
    ap = argparse.ArgumentParser(prog="python -m agent", description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("check").set_defaults(fn=cmd_check)
    p = sub.add_parser("models"); p.add_argument("--provider", required=True, choices=list(PROVIDERS)); p.set_defaults(fn=cmd_models)
    p = sub.add_parser("expected"); p.add_argument("--ids", nargs="*"); p.add_argument("--all", action="store_true"); p.add_argument("--force", action="store_true"); p.set_defaults(fn=cmd_expected)
    p = sub.add_parser("context"); p.add_argument("id"); p.set_defaults(fn=cmd_context)
    p = sub.add_parser("run"); p.add_argument("--provider", required=True, choices=list(PROVIDERS)); p.add_argument("--ids", nargs="*"); p.add_argument("--all", action="store_true"); p.add_argument("--group"); p.add_argument("--max-iters", type=int); p.add_argument("--budget-usd", type=float, help="stop before the next program once the provider's estimated spend reaches this"); p.add_argument("--skip-done", action="store_true", help="skip programs that already have a report.json"); p.set_defaults(fn=cmd_run)
    p = sub.add_parser("selftest"); p.add_argument("--ids", nargs="*"); p.add_argument("--all", action="store_true"); p.set_defaults(fn=cmd_selftest)
    p = sub.add_parser("keys", help="write .env from a folder of key files (values never printed)"); p.add_argument("--folder", default=r"D:\meta_hackathon\api key"); p.set_defaults(fn=cmd_keys)
    sub.add_parser("freeze", help="hash every SAS reference file into runs/reference_freeze.json").set_defaults(fn=cmd_freeze)
    sub.add_parser("verify", help="verify SAS reference hashes and the provenance ledger chain").set_defaults(fn=cmd_verify)
    sub.add_parser("leaderboard").set_defaults(fn=cmd_leaderboard)
    args = ap.parse_args(argv)
    args.fn(args)


if __name__ == "__main__":
    main()
