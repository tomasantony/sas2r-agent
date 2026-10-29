"""Run an R script, either in the Docker sandbox (default) or with the host Rscript.

Sandbox: pinned image, no network, the benchmark mounted read-only at /bench, only the attempt folder
writable, unprivileged user, memory/CPU/pid limits, hard timeout. Paths in the model's script that point
at the benchmark on the host are rewritten to /bench in a copy of the script; the model's original file
is kept untouched as evidence.
"""
from __future__ import annotations

import os
import re
import subprocess
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path

from .config import BENCHMARK_ROOT, RSCRIPT, SETTINGS

SANDBOX_MODE = os.environ.get("SANDBOX", "docker")          # docker | local
SANDBOX_IMAGE = os.environ.get("SANDBOX_IMAGE", "sas2r-sandbox:r4.6")
SANDBOX_MEMORY = os.environ.get("SANDBOX_MEMORY", "6g")
SANDBOX_CPUS = os.environ.get("SANDBOX_CPUS", "2")


@dataclass
class RunResult:
    rc: int
    seconds: float
    log_path: str
    timed_out: bool = False
    sandbox: dict = field(default_factory=dict)

    def log_tail(self, max_chars: int = 4000) -> str:
        try:
            text = Path(self.log_path).read_text(encoding="utf-8", errors="replace")
        except OSError:
            return ""
        return text[-max_chars:]


def _bench_variants() -> list[str]:
    r = str(BENCHMARK_ROOT)
    fwd = r.replace("\\", "/")
    return [fwd, r, r.replace("\\", "\\\\"), fwd.replace("/", "\\\\"), fwd.lower(), r.lower()]


def rewrite_for_sandbox(code: str) -> str:
    """Point every host benchmark path at the read-only /bench mount."""
    for v in sorted(set(_bench_variants()), key=len, reverse=True):
        code = re.sub(re.escape(v), "/bench", code, flags=re.I)
    # the model is told OUT_DIR is an env var; a few scripts hard-code the host attempt path instead
    return code


def image_digest(image: str) -> str:
    try:
        out = subprocess.run(["docker", "image", "inspect", image, "--format", "{{.Id}}"], capture_output=True, text=True, timeout=30)
        return out.stdout.strip()
    except Exception:  # noqa: BLE001
        return ""


def run_r(script: Path, out_dir: Path, log_path: Path, timeout: int | None = None) -> RunResult:
    script, out_dir, log_path = script.resolve(), out_dir.resolve(), log_path.resolve()
    out_dir.mkdir(parents=True, exist_ok=True)
    timeout = timeout or SETTINGS.r_timeout_sec
    if SANDBOX_MODE == "docker":
        return _run_docker(script, out_dir, log_path, timeout)
    return _run_local(script, out_dir, log_path, timeout)


def _run_local(script: Path, out_dir: Path, log_path: Path, timeout: int) -> RunResult:
    env = os.environ.copy()
    env["OUT_DIR"] = str(out_dir).replace("\\", "/")
    t0 = time.time()
    timed_out = False
    with open(log_path, "w", encoding="utf-8", errors="replace") as log:
        try:
            proc = subprocess.run([RSCRIPT, "--vanilla", str(script)], cwd=str(script.parent), env=env,
                                  stdout=log, stderr=subprocess.STDOUT, timeout=timeout)
            rc = proc.returncode
        except subprocess.TimeoutExpired:
            rc, timed_out = -9, True
            log.write(f"\n*** timed out after {timeout} s ***\n")
    return RunResult(rc, time.time() - t0, str(log_path), timed_out, {"mode": "local", "rscript": RSCRIPT})


def _run_docker(script: Path, out_dir: Path, log_path: Path, timeout: int) -> RunResult:
    adir = script.parent
    sandbox_script = adir / "script_sandbox.R"
    sandbox_script.write_text(rewrite_for_sandbox(script.read_text(encoding="utf-8", errors="replace")), encoding="utf-8")
    name = f"sas2r-{uuid.uuid4().hex[:12]}"
    cmd = ["docker", "run", "--rm", "--name", name,
           "--network", "none", "--memory", SANDBOX_MEMORY, "--cpus", SANDBOX_CPUS, "--pids-limit", "512",
           "--security-opt", "no-new-privileges", "--cap-drop", "ALL",
           "-v", f"{BENCHMARK_ROOT}:/bench:ro", "-v", f"{adir}:/work",
           "-e", "OUT_DIR=/work/out", "-w", "/work", SANDBOX_IMAGE, "script_sandbox.R"]
    t0 = time.time()
    timed_out = False
    with open(log_path, "w", encoding="utf-8", errors="replace") as log:
        log.write("sandbox: " + " ".join(cmd[:-2]) + " ...\n")
        log.flush()
        try:
            proc = subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT, timeout=timeout)
            rc = proc.returncode
        except subprocess.TimeoutExpired:
            rc, timed_out = -9, True
            subprocess.run(["docker", "kill", name], capture_output=True)
            log.write(f"\n*** timed out after {timeout} s; container killed ***\n")
    info = {"mode": "docker", "image": SANDBOX_IMAGE, "image_id": image_digest(SANDBOX_IMAGE),
            "network": "none", "benchmark_mount": "/bench:ro", "memory": SANDBOX_MEMORY, "cpus": SANDBOX_CPUS}
    return RunResult(rc, time.time() - t0, str(log_path), timed_out, info)
