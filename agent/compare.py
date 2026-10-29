"""Compare what an R translation produced with the SAS reference artifacts.

Datasets: cell-by-cell after normalising names, numbers (tolerance), dates and blanks; rows are
matched by position first, then after sorting both sides when counts agree but order differs.
Text artifacts (tables, listings, figures' companion text): recall of the SAS number multiset
in everything the R run wrote as .txt/.csv/.md. Figures: an image with the same stem exists.
"""
from __future__ import annotations

import json
import math
import re
from collections import Counter
from dataclasses import dataclass, field, asdict
from pathlib import Path

import pandas as pd

from .config import SETTINGS
from .expected import number_tokens, extract_text

DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
DT_RE = re.compile(r"^\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}(:\d{2})?(\.\d+)?$")


@dataclass
class ArtifactResult:
    kind: str
    name: str
    r_file: str | None
    status: str                 # ok | mismatch | missing | error
    score: float                # 1.0 = equivalent
    detail: dict = field(default_factory=dict)


@dataclass
class CompareReport:
    produced_all: bool
    equivalent: bool
    score: float
    artifacts: list[ArtifactResult]

    def to_json(self) -> dict:
        return {"produced_all": self.produced_all, "equivalent": self.equivalent, "score": self.score,
                "artifacts": [asdict(a) for a in self.artifacts]}

    def feedback(self, limit: int = 12) -> str:
        """Human-readable summary used in the repair prompt."""
        lines = []
        for a in self.artifacts:
            if a.status == "ok":
                lines.append(f"- {a.kind} {a.name}: OK")
                continue
            lines.append(f"- {a.kind} {a.name}: {a.status.upper()} (score {a.score:.3f})")
            d = a.detail
            if a.status == "missing":
                lines.append(f"    expected the R run to write OUT_DIR/{d.get('expected_file')}")
            if "rows_sas" in d:
                lines.append(f"    rows SAS={d['rows_sas']} R={d['rows_r']}; columns SAS={d['cols_sas']} R={d['cols_r']}")
            if d.get("missing_cols"):
                lines.append(f"    columns missing in R: {', '.join(d['missing_cols'][:15])}")
            if d.get("extra_cols"):
                lines.append(f"    extra columns in R (ignored): {', '.join(d['extra_cols'][:15])}")
            if d.get("mismatch_examples"):
                lines.append("    first differing cells (column, row, SAS value, R value):")
                for ex in d["mismatch_examples"][:limit]:
                    lines.append(f"      {ex}")
            if d.get("worst_columns"):
                lines.append(f"    columns with most differences: {d['worst_columns']}")
            if "recall" in d:
                lines.append(f"    {d['found']} of {d['expected']} numbers from the SAS output found in the R text "
                             f"(recall {d['recall']:.3f}); R text files read: {d.get('r_files')}")
                if d.get("missing_examples"):
                    lines.append(f"    examples of SAS numbers not found in R text: {d['missing_examples']}")
            if d.get("error"):
                lines.append(f"    error: {d['error']}")
        return "\n".join(lines)


# ---------------------------------------------------------------- datasets

def read_sas_dataset(path: Path) -> pd.DataFrame:
    if path.suffix.lower() == ".xlsx":
        return pd.read_excel(path)
    import pyreadstat
    df, _ = pyreadstat.read_sas7bdat(str(path))
    return df


def _norm_cols(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df.columns = [str(c).strip().upper() for c in df.columns]
    return df


def _norm_cell(v):
    """Return ('n', float) | ('s', str) | ('', None) for a cell."""
    if v is None:
        return ("", None)
    if isinstance(v, float) and math.isnan(v):
        return ("", None)
    if isinstance(v, (pd.Timestamp,)):
        if v.hour == 0 and v.minute == 0 and v.second == 0:
            return ("s", v.strftime("%Y-%m-%d"))
        return ("s", v.strftime("%Y-%m-%dT%H:%M:%S"))
    if hasattr(v, "isoformat") and not isinstance(v, str):  # date / datetime
        s = v.isoformat()
        return ("s", s[:10] if s.endswith("T00:00:00") else s.replace(" ", "T"))
    if isinstance(v, (int, float)) and not isinstance(v, bool):
        return ("n", float(v))
    s = str(v).strip()
    if s == "" or s.upper() in ("NA", "NAN", "."):
        return ("", None)
    if DT_RE.match(s):
        s = s.replace(" ", "T")
        if s.endswith("T00:00:00"):
            s = s[:10]
        return ("s", s)
    if DATE_RE.match(s):
        return ("s", s)
    try:
        return ("n", float(s))
    except ValueError:
        return ("s", s)


def _cells_equal(a, b) -> bool:
    ka, va = _norm_cell(a)
    kb, vb = _norm_cell(b)
    if ka == "" or kb == "":
        return ka == kb
    if ka == "n" and kb == "n":
        return abs(va - vb) <= SETTINGS.numeric_atol + SETTINGS.numeric_rtol * max(abs(va), abs(vb))
    if ka == "n" and kb == "s" or ka == "s" and kb == "n":
        try:
            return abs(float(va) - float(vb)) <= SETTINGS.numeric_atol + SETTINGS.numeric_rtol * max(abs(float(va)), abs(float(vb)))
        except (TypeError, ValueError):
            return False
    return str(va) == str(vb)


def _sort_key_frame(df: pd.DataFrame, cols: list[str]) -> pd.DataFrame:
    key = df[cols].astype(str).apply(lambda s: s.str.strip())
    return df.loc[key.sort_values(by=cols, kind="mergesort").index].reset_index(drop=True)


def compare_dataset(sas_path: Path, r_csv: Path) -> ArtifactResult:
    name = sas_path.stem.lower()
    try:
        sas = _norm_cols(read_sas_dataset(sas_path))
        r = _norm_cols(pd.read_csv(r_csv, dtype=str, keep_default_na=False, na_values=[""]))
    except Exception as e:  # noqa: BLE001
        return ArtifactResult("dataset", name, str(r_csv), "error", 0.0, {"error": str(e)})
    common = [c for c in sas.columns if c in r.columns]
    missing = [c for c in sas.columns if c not in r.columns]
    extra = [c for c in r.columns if c not in sas.columns]
    detail = {"rows_sas": len(sas), "rows_r": len(r), "cols_sas": len(sas.columns), "cols_r": len(r.columns),
              "missing_cols": missing, "extra_cols": extra}
    if not common or len(sas) == 0:
        return ArtifactResult("dataset", name, str(r_csv), "mismatch", 0.0, detail)

    def score(a: pd.DataFrame, b: pd.DataFrame):
        n = min(len(a), len(b))
        total = len(sas) * len(sas.columns)
        ok = 0
        mism: list[str] = []
        per_col: Counter = Counter()
        for c in common:
            av, bv = a[c].tolist()[:n], b[c].tolist()[:n]
            for i in range(n):
                if _cells_equal(av[i], bv[i]):
                    ok += 1
                else:
                    per_col[c] += 1
                    if len(mism) < 10:
                        mism.append(f"({c}, {i + 1}, {av[i]!r}, {bv[i]!r})")
        return ok / total if total else 0.0, mism, per_col

    s1, m1, p1 = score(sas, r)
    best = (s1, m1, p1, "positional")
    if s1 < 1.0 and len(sas) == len(r):
        ss, rr = _sort_key_frame(sas, common), _sort_key_frame(r, common)
        s2, m2, p2 = score(ss, rr)
        if s2 > s1:
            best = (s2, m2, p2, "sorted")
    sc, mism, per_col, mode = best
    detail.update({"cell_match": sc, "row_match_mode": mode, "mismatch_examples": mism,
                   "worst_columns": dict(per_col.most_common(5))})
    status = "ok" if sc >= SETTINGS.dataset_cell_threshold and not missing and len(sas) == len(r) else "mismatch"
    return ArtifactResult("dataset", name, str(r_csv), status, sc, detail)


# ---------------------------------------------------------------- text and figures

def r_text_tokens(out_dir: Path) -> tuple[Counter, list[str]]:
    c: Counter = Counter()
    files = []
    for p in sorted(out_dir.rglob("*")):
        if p.suffix.lower() in (".txt", ".csv", ".md", ".html", ".htm", ".rtf", ".tsv", ".json"):
            try:
                c.update(number_tokens(extract_text(p)))
                files.append(p.name)
            except Exception:  # noqa: BLE001
                continue
    return c, files


def compare_text(name: str, expected: Counter, r_tokens: Counter, r_files: list[str]) -> ArtifactResult:
    total = sum(expected.values())
    if total == 0:
        return ArtifactResult("text", name, None, "ok", 1.0, {"recall": 1.0, "expected": 0, "found": 0, "r_files": r_files})
    found = sum(min(n, r_tokens.get(tok, 0)) for tok, n in expected.items())
    recall = found / total
    missing = [tok for tok, n in expected.items() if r_tokens.get(tok, 0) < n][:12]
    status = "ok" if recall >= SETTINGS.text_recall_threshold else ("missing" if not r_files else "mismatch")
    detail = {"recall": recall, "expected": total, "found": found, "r_files": r_files, "missing_examples": missing}
    if status == "missing":
        detail["expected_file"] = "<any>.txt or .csv rendering of the table/listing"
    return ArtifactResult("text", name, None, status, recall, detail)


def compare_run(expected: dict, out_dir: Path) -> CompareReport:
    results: list[ArtifactResult] = []
    r_tok, r_files = r_text_tokens(out_dir)
    images = {p.stem.lower(): p for p in out_dir.rglob("*") if p.suffix.lower() in (".png", ".gif", ".jpg", ".jpeg", ".pdf", ".svg")}
    for a in expected["artifacts"]:
        kind, name = a["kind"], a["name"]
        if kind == "dataset":
            cand = out_dir / a["r_name"]
            if not cand.exists():
                alt = list(out_dir.rglob(a["r_name"]))
                cand = alt[0] if alt else None
            if cand is None:
                results.append(ArtifactResult(kind, name, None, "missing", 0.0, {"expected_file": a["r_name"]}))
            else:
                results.append(compare_dataset(Path(a["path"]), cand))
        elif kind == "text":
            results.append(compare_text(name, Counter(expected["tokens"].get(name, {})), r_tok, r_files))
        elif kind == "figure":
            stem = Path(a["r_name"]).stem.lower()
            hit = images.get(stem) or (next(iter(images.values())) if images else None)
            if hit is None:
                results.append(ArtifactResult(kind, name, None, "missing", 0.0, {"expected_file": a["r_name"]}))
            else:
                results.append(ArtifactResult(kind, name, str(hit), "ok", 1.0, {"matched_by": "stem" if stem in images else "any image"}))
    produced_all = all(r.status != "missing" for r in results) if results else bool(list(out_dir.iterdir()))
    score = sum(r.score for r in results) / len(results) if results else 0.0
    equivalent = bool(results) and all(r.status == "ok" for r in results)
    return CompareReport(produced_all, equivalent, score, results)


def save_report(report: CompareReport, path: Path) -> None:
    path.write_text(json.dumps(report.to_json(), indent=1, default=str), encoding="utf-8")
