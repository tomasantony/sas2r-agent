"""Expected artifacts per program, derived from the SAS reference outputs in the benchmark.

Kinds:
  dataset  - a sas7bdat (or xlsx) the program wrote; the R translation must write <name>.csv
  text     - a listing (.lst), RTF, PDF or HTML; compared by the multiset of numbers it contains
  figure   - a PNG/GIF image; the R translation must write an image with the same stem
"""
from __future__ import annotations

import csv
import json
import re
from collections import Counter
from dataclasses import dataclass, asdict
from pathlib import Path

from .config import BENCHMARK_ROOT, RUNS_DIR

OUT = BENCHMARK_ROOT / "output" / "sas"
NUM = re.compile(r"-?\d+(?:\.\d+)?")


@dataclass
class Artifact:
    kind: str          # dataset | text | figure
    name: str          # dataset name (lowercase) or file name
    path: str          # absolute path of the SAS reference file
    r_name: str        # file the R translation must produce under OUT_DIR
    tokens: int = 0    # number of numeric tokens (text kind)


def rtf_to_text(rtf: str) -> str:
    t = re.sub(r"\{\\\*[^{}]*\}", "", rtf)
    for _ in range(50):  # drop embedded pictures (may nest one level)
        t2 = re.sub(r"\{\\pict(?:[^{}]|\{[^{}]*\})*\}", " ", t)
        if t2 == t:
            break
        t = t2
    t = re.sub(r"\\pard?\b", "\n", t)
    t = re.sub(r"\\(cell|row|line|tab)\b", " | ", t)
    t = re.sub(r"\\'([0-9a-f]{2})", lambda m: chr(int(m.group(1), 16)), t)
    t = re.sub(r"\\[a-zA-Z]+-?\d* ?", "", t)
    t = re.sub(r"[{}]", "", t)
    return t


def pdf_to_text(path: Path) -> str:
    import pymupdf  # installed on the reference machine
    doc = pymupdf.open(str(path))
    return "\n".join(p.get_text() for p in doc)


def html_to_text(html: str) -> str:
    html = re.sub(r"<(script|style)[^>]*>.*?</\1>", " ", html, flags=re.S | re.I)
    return re.sub(r"<[^>]+>", " ", html)


def extract_text(path: Path) -> str:
    ext = path.suffix.lower()
    if ext == ".rtf":
        return rtf_to_text(path.read_text(encoding="latin-1", errors="replace"))
    if ext == ".pdf":
        return pdf_to_text(path)
    if ext in (".html", ".htm"):
        return html_to_text(path.read_text(encoding="utf-8", errors="replace"))
    return path.read_text(encoding="utf-8", errors="replace")


def number_tokens(text: str) -> Counter:
    """Multiset of numbers in a text, normalised so 1.50 and 1.5 agree and huge ids are ignored."""
    c: Counter = Counter()
    for tok in NUM.findall(text):
        if len(tok.lstrip("-").split(".")[0]) > 9:   # identifiers, timestamps, byte counts
            continue
        try:
            v = float(tok)
        except ValueError:
            continue
        c[f"{v:.6g}"] += 1
    return c


def _audit_rows() -> dict[str, list[tuple[str, str]]]:
    """(file, type) rows per id from the benchmark's output audit."""
    rows: dict[str, list[tuple[str, str]]] = {}
    with open(BENCHMARK_ROOT / "benchmark" / "output_audit.csv", newline="", encoding="utf-8-sig") as f:
        for r in csv.DictReader(f):
            if r["type"] in ("log", "none", "bak", "sas7bndx", "xpt"):
                continue
            if r["check"] == "empty":
                continue
            rows.setdefault(r["id"], []).append((r["file"], r["type"]))
    return rows


def _locate(pid: str, fname: str, ftype: str) -> Path | None:
    cands = [OUT / "datasets" / pid / fname, OUT / "tlf" / fname, OUT / "logs" / fname]
    for c in cands:
        if c.exists():
            return c
    hits = list((OUT / "tlf").rglob(fname)) + list((OUT / "datasets" / pid).rglob(fname))
    return hits[0] if hits else None


def expected_artifacts(pid: str) -> list[Artifact]:
    arts: list[Artifact] = []
    for fname, ftype in _audit_rows().get(pid, []):
        p = _locate(pid, fname, ftype)
        if p is None:
            continue
        stem = p.stem.lower()
        if ftype in ("sas7bdat", "xlsx"):
            arts.append(Artifact("dataset", stem, str(p), f"{stem}.csv"))
        elif ftype in ("lst", "rtf", "pdf", "html", "htm"):
            arts.append(Artifact("text", p.name, str(p), f"{stem}.txt"))
        elif ftype in ("png", "gif"):
            arts.append(Artifact("figure", p.name, str(p), f"{stem}.png"))
    return arts


def expected_dir() -> Path:
    d = RUNS_DIR / "expected"
    d.mkdir(parents=True, exist_ok=True)
    return d


def build_expected(pid: str, force: bool = False) -> dict:
    """Cache the expected artifacts and the number multisets of the text ones."""
    cache = expected_dir() / f"{pid}.json"
    if cache.exists() and not force:
        return json.loads(cache.read_text(encoding="utf-8"))
    arts = expected_artifacts(pid)
    tokens: dict[str, dict[str, int]] = {}
    for a in arts:
        if a.kind == "text":
            try:
                c = number_tokens(extract_text(Path(a.path)))
            except Exception as e:  # noqa: BLE001
                c = Counter()
                a.name += f" (unreadable: {e})"
            tokens[a.name] = dict(c)
            a.tokens = sum(c.values())
    data = {"id": pid, "artifacts": [asdict(a) for a in arts], "tokens": tokens}
    cache.write_text(json.dumps(data, indent=1), encoding="utf-8")
    return data
