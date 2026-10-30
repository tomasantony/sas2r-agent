"""Provenance: hash everything a run touches and chain the records so they are tamper-evident.

Per attempt: provenance.json with SHA-256 of the prompt, the model's answer, the R script, every file the
script produced, and the SAS reference files it was compared against (each checked against the frozen
reference manifest). Per repo: runs/ledger.jsonl, an append-only hash chain over those records.
"""
from __future__ import annotations

import hashlib
import json
import platform
import subprocess
import time
from pathlib import Path

from .config import BENCHMARK_ROOT, RUNS_DIR, REPO_ROOT

FREEZE = RUNS_DIR / "reference_freeze.json"


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def git_head(repo: Path) -> str:
    try:
        return subprocess.run(["git", "rev-parse", "HEAD"], cwd=repo, capture_output=True, text=True, timeout=20).stdout.strip()
    except Exception:  # noqa: BLE001
        return ""


# ------------------------------------------------------------------ SAS reference freeze

def freeze_reference(force: bool = False) -> dict:
    """SHA-256 of every file under the benchmark's output/sas plus the benchmark git commit."""
    if FREEZE.exists() and not force:
        return json.loads(FREEZE.read_text(encoding="utf-8"))
    base = BENCHMARK_ROOT / "output" / "sas"
    files = {}
    for p in sorted(base.rglob("*")):
        if p.is_file():
            files[str(p.relative_to(base)).replace("\\", "/")] = {"sha256": sha256(p), "bytes": p.stat().st_size}
    data = {"frozen_at": time.strftime("%Y-%m-%dT%H:%M:%S"), "benchmark_root": str(BENCHMARK_ROOT),
            "benchmark_commit": git_head(BENCHMARK_ROOT), "files": files}
    FREEZE.parent.mkdir(parents=True, exist_ok=True)
    FREEZE.write_text(json.dumps(data, indent=1), encoding="utf-8")
    return data


def verify_reference(path: Path) -> dict:
    """Check one SAS reference file against the freeze; returns {ok, sha256, frozen_sha256}."""
    data = freeze_reference()
    base = BENCHMARK_ROOT / "output" / "sas"
    rel = str(Path(path).resolve().relative_to(base.resolve())).replace("\\", "/")
    actual = sha256(Path(path))
    frozen = data["files"].get(rel, {}).get("sha256")
    return {"file": rel, "sha256": actual, "frozen_sha256": frozen, "ok": frozen == actual}


# ------------------------------------------------------------------ per-attempt record and ledger

def hash_tree(folder: Path) -> dict[str, str]:
    return {str(p.relative_to(folder)).replace("\\", "/"): sha256(p) for p in sorted(folder.rglob("*")) if p.is_file()}


def write_attempt_provenance(adir: Path, *, pid: str, provider: str, model: str, attempt: int,
                             completion, run_result, compare_report, expected: dict, sandbox: dict,
                             prompt_version: int = 1, run_name: str = "") -> dict:
    refs = []
    for a in expected["artifacts"]:
        try:
            refs.append(verify_reference(Path(a["path"])))
        except Exception as e:  # noqa: BLE001
            refs.append({"file": a["path"], "ok": False, "error": str(e)})
    rec = {
        "schema": "sas2r-agent/provenance/1",
        "program": pid, "provider": provider, "model": model, "attempt": attempt,
        "prompt_version": prompt_version, "run_name": run_name or provider,
        "recorded_at": time.strftime("%Y-%m-%dT%H:%M:%S"),
        "agent_commit": git_head(REPO_ROOT),
        "benchmark_commit": freeze_reference()["benchmark_commit"],
        "host": {"platform": platform.platform(), "node": platform.node()},
        "sandbox": sandbox,
        "llm": {"input_tokens": completion.input_tokens, "output_tokens": completion.output_tokens,
                "reasoning_tokens": completion.reasoning_tokens, "finish_reason": completion.finish_reason,
                "seconds": round(completion.seconds, 1)},
        "inputs": {name: sha256(adir.parent / name) for name in ("prompt_translate.md",) if (adir.parent / name).exists()},
        "model_output": {name: sha256(adir / name) for name in ("response.md", "reasoning.md", "script.R") if (adir / name).exists()},
        "r_run": {"rc": run_result.rc, "seconds": round(run_result.seconds, 1), "timed_out": run_result.timed_out,
                  "log_sha256": sha256(Path(run_result.log_path)) if Path(run_result.log_path).exists() else None},
        "r_outputs": hash_tree(adir / "out") if (adir / "out").exists() else {},
        "sas_reference": refs,
        "reference_verified": all(r.get("ok") for r in refs) if refs else None,
        "comparison": {"equivalent": compare_report.equivalent, "score": compare_report.score,
                       "produced_all": compare_report.produced_all},
    }
    (adir / "provenance.json").write_text(json.dumps(rec, indent=1), encoding="utf-8")
    append_ledger(rec)
    return rec


def append_ledger(rec: dict) -> str:
    """Append a hash-chained line to runs/ledger.jsonl; returns the new entry hash."""
    ledger = RUNS_DIR / "ledger.jsonl"
    prev = ""
    if ledger.exists():
        lines = ledger.read_text(encoding="utf-8").splitlines()
        if lines:
            prev = json.loads(lines[-1])["entry_hash"]
    body = json.dumps({"prev_hash": prev, "record": rec}, sort_keys=True)
    entry_hash = hashlib.sha256(body.encode("utf-8")).hexdigest()
    with open(ledger, "a", encoding="utf-8") as f:
        f.write(json.dumps({"prev_hash": prev, "entry_hash": entry_hash, "record": rec}, sort_keys=True) + "\n")
    return entry_hash


def verify_ledger() -> tuple[int, int]:
    """Recompute the chain; returns (entries, first bad index or -1)."""
    ledger = RUNS_DIR / "ledger.jsonl"
    if not ledger.exists():
        return 0, -1
    prev = ""
    lines = ledger.read_text(encoding="utf-8").splitlines()
    for i, line in enumerate(lines):
        e = json.loads(line)
        body = json.dumps({"prev_hash": e["prev_hash"], "record": e["record"]}, sort_keys=True)
        if e["prev_hash"] != prev or hashlib.sha256(body.encode("utf-8")).hexdigest() != e["entry_hash"]:
            return len(lines), i
        prev = e["entry_hash"]
    return len(lines), -1
