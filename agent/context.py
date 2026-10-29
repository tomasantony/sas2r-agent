"""Build the context handed to the model for one benchmark program.

The benchmark repository holds the SAS program as it was actually run (programs/sas_run, with the
documented path patches), the family environment (benchmark/env/<family>.sas) that defines the
librefs and macro variables, and the macros the program includes. The context resolves every
path to an absolute location on this machine so the R translation can read the same inputs.
"""
from __future__ import annotations

import csv
import re
from dataclasses import dataclass, field
from pathlib import Path

from .config import BENCHMARK_ROOT, SETTINGS


def _fwd(p: str | Path) -> str:
    return str(p).replace("\\", "/")


@dataclass
class ProgramInfo:
    id: str
    file: str
    group: str
    source: str
    licence: str
    outputs: str
    family: str
    note: str = ""


def load_manifest() -> dict[str, ProgramInfo]:
    fam = {}
    with open(BENCHMARK_ROOT / "benchmark" / "families.csv", newline="", encoding="utf-8-sig") as f:
        for row in csv.DictReader(f):
            fam[row["id"]] = row["family"]
    out: dict[str, ProgramInfo] = {}
    with open(BENCHMARK_ROOT / "benchmark" / "manifest.csv", newline="", encoding="utf-8-sig") as f:
        for row in csv.DictReader(f):
            out[row["id"]] = ProgramInfo(
                id=row["id"], file=row["file"], group=row["group"], source=row["source"],
                licence=row["licence"], outputs=row["outputs"], family=fam.get(row["id"], "general"),
                note=row.get("note", ""),
            )
    return out


# Macro variables set by benchmark/autoexec.sas for every program.
def base_macro_vars(pid: str) -> dict[str, str]:
    r = _fwd(BENCHMARK_ROOT)
    return {
        "root": r,
        "pid": pid,
        "rawadam": f"{r}/data/raw/cdisc-pilot/adam",
        "rawsdtm": f"{r}/data/raw/cdisc-pilot/sdtm",
        "rawsplit": f"{r}/data/derived/cdisc-pilot/adam-split",
        "rawdir": f"{r}/data/raw",
        "outtlf": f"{r}/output/sas/tlf",
        "outlog": f"{r}/output/sas/logs",
        "outds": f"{r}/output/sas/datasets/{pid}",
    }


_LET = re.compile(r"%let\s+(\w+)\s*=\s*([^;]*);", re.I)
_LIBNAME = re.compile(r"libname\s+(\w+)\s+(?:(xport)\s+)?[\"']([^\"']+)[\"']", re.I)
_INCLUDE = re.compile(r"%include\s+[\"']([^\"']+)[\"']", re.I)
_MACRO_CALL = re.compile(r"%(\w+)\s*\(", re.I)


def resolve(text: str, vars_: dict[str, str]) -> str:
    """Substitute &name. / &name references with known values (simple, non-recursive)."""
    def sub(m):
        name = m.group(1)
        return vars_.get(name.lower(), vars_.get(name, m.group(0)))
    return re.sub(r"&(\w+)\.?", sub, text)


def family_env(pid: str, family: str, vars_: dict[str, str]) -> tuple[str, dict[str, str], list[tuple[str, str]]]:
    """Return (env text, updated macro vars, libnames) for the program's family."""
    env_file = BENCHMARK_ROOT / "benchmark" / "env" / f"{family}.sas"
    text = env_file.read_text(encoding="utf-8", errors="replace") if env_file.exists() else ""
    for m in _LET.finditer(text):
        vars_[m.group(1).lower()] = _fwd(resolve(m.group(2).strip(), vars_))
    libs = [(m.group(1), _fwd(resolve(m.group(3), vars_))) for m in _LIBNAME.finditer(text)]
    # Pilot family: the setup.sas defines the librefs; pull them in.
    for m in _INCLUDE.finditer(text):
        inc = Path(resolve(m.group(1), vars_))
        if inc.exists():
            inc_text = inc.read_text(encoding="utf-8", errors="replace")
            for mm in _LET.finditer(inc_text):
                vars_[mm.group(1).lower()] = _fwd(resolve(mm.group(2).strip(), vars_))
            libs += [(mm.group(1), _fwd(resolve(mm.group(3), vars_))) for mm in _LIBNAME.finditer(inc_text)]
            text += f"\n/* ---- included: {inc.name} ---- */\n" + inc_text
    return text, vars_, libs


def macro_sources(source: str, vars_: dict[str, str], budget: int) -> list[tuple[str, str]]:
    """Source text of macros the program %includes or calls by autocall (PHUSE utilities, owner macros)."""
    found: list[tuple[str, str]] = []
    seen: set[str] = set()
    macro_root = BENCHMARK_ROOT / "programs" / "sas" / "macros"
    index = {p.stem.lower(): p for p in macro_root.rglob("*.sas")}
    # explicit includes
    for m in _INCLUDE.finditer(source):
        p = Path(resolve(m.group(1), vars_))
        if p.exists() and p.name.lower() not in seen:
            seen.add(p.name.lower())
            found.append((_fwd(p), p.read_text(encoding="utf-8", errors="replace")))
    # autocall macros referenced by name
    for m in _MACRO_CALL.finditer(source):
        name = m.group(1).lower()
        if name in index and name not in seen and not name.startswith(("if", "do", "let", "put", "sysfunc", "macro", "mend")):
            seen.add(name)
            p = index[name]
            found.append((_fwd(p), p.read_text(encoding="utf-8", errors="replace")))
    # cap total size
    out, used = [], 0
    for path, text in found:
        if used + len(text) > budget:
            out.append((path, text[: max(0, budget - used)] + "\n/* ... truncated ... */"))
            break
        out.append((path, text))
        used += len(text)
    return out


def faijal_work_inputs() -> list[tuple[str, str]]:
    """faijal programs read WORK.edc_<domain> that the family env imports from raw xlsx files."""
    d = BENCHMARK_ROOT / "data" / "raw" / "faijal-oncology" / "raw_data"
    return [(p.stem, _fwd(p)) for p in sorted(d.glob("*.xlsx"))]


@dataclass
class ProgramContext:
    info: ProgramInfo
    source_path: str
    source: str
    env_text: str
    macro_vars: dict[str, str]
    libnames: list[tuple[str, str]]
    macros: list[tuple[str, str]] = field(default_factory=list)
    work_inputs: list[tuple[str, str]] = field(default_factory=list)


def build_context(pid: str) -> ProgramContext:
    manifest = load_manifest()
    info = manifest[pid]
    run_copy = BENCHMARK_ROOT / "programs" / "sas_run" / info.file
    source = run_copy.read_text(encoding="utf-8", errors="replace")
    vars_ = base_macro_vars(pid)
    env_text, vars_, libs = family_env(pid, info.family, vars_)
    budget = max(10_000, SETTINGS.max_context_chars - len(source) - len(env_text))
    macros = macro_sources(source, vars_, budget)
    work = faijal_work_inputs() if info.family == "faijal" else []
    return ProgramContext(info, _fwd(run_copy), source, env_text, vars_, libs, macros, work)
