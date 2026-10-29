"""The translate -> run -> compare -> repair loop for one program."""
from __future__ import annotations

import json
import shutil
import time
from dataclasses import dataclass, field, asdict
from pathlib import Path

from .compare import compare_run, save_report
from .config import RUNS_DIR, SETTINGS, Provider
from .context import build_context
from .expected import build_expected
from .llm import LLM, extract_r_code
from .prompts import SYSTEM, translate_prompt, repair_prompt
from .runner import run_r
from .config import R_PACKAGES


@dataclass
class Attempt:
    n: int
    rc: int | None
    seconds_r: float
    input_tokens: int
    output_tokens: int
    seconds_llm: float
    produced_all: bool
    equivalent: bool
    score: float
    note: str = ""


@dataclass
class ProgramReport:
    id: str
    provider: str
    model: str
    family: str
    translated: bool
    runs: bool
    produced_all: bool
    equivalent: bool
    best_score: float
    iterations: int
    input_tokens: int
    output_tokens: int
    seconds: float
    attempts: list[Attempt] = field(default_factory=list)
    best_attempt: int | None = None


def run_program(pid: str, provider: Provider, max_iterations: int | None = None, fresh: bool = True) -> ProgramReport:
    max_it = max_iterations or SETTINGS.max_iterations
    ctx = build_context(pid)
    expected = build_expected(pid)
    llm = LLM(provider)
    base = RUNS_DIR / provider.name / pid
    if fresh and base.exists():
        shutil.rmtree(base)
    base.mkdir(parents=True, exist_ok=True)

    system = SYSTEM.format(packages=", ".join(R_PACKAGES))
    messages = [{"role": "system", "content": system}, {"role": "user", "content": translate_prompt(ctx, expected)}]
    (base / "prompt_translate.md").write_text(messages[1]["content"], encoding="utf-8")

    rep = ProgramReport(pid, provider.name, provider.model, ctx.info.family, False, False, False, False, 0.0, 0, 0, 0, 0.0)
    t_start = time.time()
    for n in range(1, max_it + 1):
        adir = base / f"attempt_{n}"
        adir.mkdir(exist_ok=True)
        comp = llm.chat(messages, tag=f"{pid}:{n}")
        rep.input_tokens += comp.input_tokens
        rep.output_tokens += comp.output_tokens
        (adir / "response.md").write_text(comp.text, encoding="utf-8")
        code = extract_r_code(comp.text)
        if code is None:
            rep.attempts.append(Attempt(n, None, 0, comp.input_tokens, comp.output_tokens, comp.seconds, False, False, 0.0, "no R code in answer"))
            messages.append({"role": "assistant", "content": comp.text})
            messages.append({"role": "user", "content": "Your answer contained no R code block. Return the complete R script in one ```r block."})
            continue
        rep.translated = True
        script = adir / "script.R"
        script.write_text(code, encoding="utf-8")
        out_dir = adir / "out"
        run = run_r(script, out_dir, adir / "run.log")
        report = compare_run(expected, out_dir)
        save_report(report, adir / "compare.json")
        att = Attempt(n, run.rc, run.seconds, comp.input_tokens, comp.output_tokens, comp.seconds,
                      report.produced_all, report.equivalent, report.score, "timed out" if run.timed_out else "")
        rep.attempts.append(att)
        rep.iterations = n
        if run.rc == 0:
            rep.runs = True
        if report.produced_all:
            rep.produced_all = True
        if report.score > rep.best_score or rep.best_attempt is None:
            rep.best_score, rep.best_attempt = report.score, n
        if report.equivalent and run.rc == 0:
            rep.equivalent = True
            break
        if n == max_it:
            break
        # repair turn: keep only the last few assistant/user exchanges to bound the context
        messages.append({"role": "assistant", "content": f"```r\n{code}```"})
        messages.append({"role": "user", "content": repair_prompt(run.rc, run.log_tail(), report.feedback(), n + 1)})
        keep = 2 + 2 * SETTINGS.history_attempts
        if len(messages) > keep:
            messages = messages[:2] + messages[-(keep - 2):]
    rep.seconds = time.time() - t_start
    (base / "report.json").write_text(json.dumps(asdict(rep), indent=1), encoding="utf-8")
    return rep
