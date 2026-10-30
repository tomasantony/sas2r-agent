"""Provenance: hash everything a run touches and chain the records so they are tamper-evident.

Per attempt: provenance.json with SHA-256 of the prompt, the model's answer, the R script, every file the
script produced, and the SAS reference files it was compared against (each checked against the frozen
reference manifest). Per repo: runs/ledger.jsonl, an append-only hash chain over those records.
"""
from __future__ import annotations

import contextlib
import hashlib
import json
import os
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


@contextlib.contextmanager
def _locked(path: Path):
    """Exclusive lock so concurrent sweeps append one at a time (Windows msvcrt / POSIX fcntl)."""
    lock = path.with_suffix(".lock")
    fh = open(lock, "a+b")
    try:
        if os.name == "nt":
            import msvcrt
            for _ in range(600):
                try:
                    msvcrt.locking(fh.fileno(), msvcrt.LK_NBLCK, 1)
                    break
                except OSError:
                    time.sleep(0.1)
        else:
            import fcntl
            fcntl.flock(fh.fileno(), fcntl.LOCK_EX)
        yield
    finally:
        try:
            if os.name == "nt":
                import msvcrt
                fh.seek(0)
                msvcrt.locking(fh.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                import fcntl
                fcntl.flock(fh.fileno(), fcntl.LOCK_UN)
        except OSError:
            pass
        fh.close()


def _last_entry_hash(ledger: Path) -> str:
    if not ledger.exists() or ledger.stat().st_size == 0:
        return ""
    with open(ledger, "rb") as f:
        f.seek(0, os.SEEK_END)
        size = f.tell()
        back = min(size, 1 << 16)
        f.seek(size - back)
        tail = f.read().decode("utf-8", errors="replace").rstrip("\n").splitlines()
    return json.loads(tail[-1])["entry_hash"] if tail else ""


def append_ledger(rec: dict) -> str:
    """Append a hash-chained line to runs/ledger.jsonl under a file lock; returns the new entry hash."""
    ledger = RUNS_DIR / "ledger.jsonl"
    ledger.parent.mkdir(parents=True, exist_ok=True)
    with _locked(ledger):
        prev = _last_entry_hash(ledger)
        body = json.dumps({"prev_hash": prev, "record": rec}, sort_keys=True)
        entry_hash = hashlib.sha256(body.encode("utf-8")).hexdigest()
        with open(ledger, "a", encoding="utf-8") as f:
            f.write(json.dumps({"prev_hash": prev, "entry_hash": entry_hash, "record": rec}, sort_keys=True) + "\n")
            f.flush()
    return entry_hash


def verify_ledger() -> dict:
    """Check every entry: its hash matches its body, and its prev_hash is the hash of an earlier entry.

    Returns {entries, bad_hash (first index with a wrong entry hash, -1 if none), unresolved (first index
    whose prev_hash is not an earlier entry, -1 if none), forks (entries whose prev is an earlier entry but
    not the immediately preceding one: two sweeps appended at the same moment before file locking was added
    on 2026-10-29; the records are intact and ordered by their recorded_at timestamps)}.
    """
    ledger = RUNS_DIR / "ledger.jsonl"
    out = {"entries": 0, "bad_hash": -1, "unresolved": -1, "forks": []}
    if not ledger.exists():
        return out
    seen: set[str] = set()
    prev = ""
    lines = ledger.read_text(encoding="utf-8").splitlines()
    out["entries"] = len(lines)
    for i, line in enumerate(lines):
        e = json.loads(line)
        body = json.dumps({"prev_hash": e["prev_hash"], "record": e["record"]}, sort_keys=True)
        if hashlib.sha256(body.encode("utf-8")).hexdigest() != e["entry_hash"] and out["bad_hash"] < 0:
            out["bad_hash"] = i
        if e["prev_hash"] != prev:
            if e["prev_hash"] in seen:
                out["forks"].append(i)
            elif out["unresolved"] < 0:
                out["unresolved"] = i
        seen.add(e["entry_hash"])
        prev = e["entry_hash"]
    return out
