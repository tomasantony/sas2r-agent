"""Run an R script with Rscript, capturing the console and enforcing a timeout."""
from __future__ import annotations

import os
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path

from .config import RSCRIPT, SETTINGS


@dataclass
class RunResult:
    rc: int
    seconds: float
    log_path: str
    timed_out: bool = False

    def log_tail(self, max_chars: int = 4000) -> str:
        try:
            text = Path(self.log_path).read_text(encoding="utf-8", errors="replace")
        except OSError:
            return ""
        return text[-max_chars:]


def run_r(script: Path, out_dir: Path, log_path: Path, timeout: int | None = None) -> RunResult:
    # Rscript crashes (0xC0000005) when given a relative script path together with cwd, so resolve everything.
    script = script.resolve()
    out_dir = out_dir.resolve()
    log_path = log_path.resolve()
    out_dir.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    env["OUT_DIR"] = str(out_dir).replace("\\", "/")
    t0 = time.time()
    timed_out = False
    with open(log_path, "w", encoding="utf-8", errors="replace") as log:
        try:
            proc = subprocess.run([RSCRIPT, "--vanilla", str(script)], cwd=str(script.parent), env=env,
                                  stdout=log, stderr=subprocess.STDOUT, timeout=timeout or SETTINGS.r_timeout_sec)
            rc = proc.returncode
        except subprocess.TimeoutExpired:
            rc, timed_out = -9, True
            log.write(f"\n*** timed out after {timeout or SETTINGS.r_timeout_sec} s ***\n")
    return RunResult(rc, time.time() - t0, str(log_path), timed_out)
