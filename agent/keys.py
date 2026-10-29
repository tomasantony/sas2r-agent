"""Build .env from a folder of key files, so a new key only needs to be dropped into the folder.

File names decide the variable; the first token in the file that looks like a key is used. Values are
never printed. Example folder: meta.txt, anthropic.txt, baseten.txt, ollama cloud.txt.
"""
from __future__ import annotations

import re
from pathlib import Path

from .config import REPO_ROOT

# (substring of the file name, env var, regex for the key value)
RULES = [
    ("meta", "MODEL_API_KEY", r"[A-Za-z0-9_\-\.]{24,}"),
    ("anthropic", "ANTHROPIC_API_KEY", r"sk-ant-[A-Za-z0-9_\-]{20,}"),
    ("baseten", "BASETEN_API_KEY", r"b10_[A-Za-z0-9_\-\.]{20,}"),
    ("api key", "BASETEN_API_KEY", r"b10_[A-Za-z0-9_\-\.]{20,}"),   # the hackathon file is named "api key.txt"
    ("ollama", "OLLAMA_API_KEY", r"[A-Za-z0-9_\-\.]{40,}"),
]


def scan(folder: Path) -> dict[str, tuple[str, str]]:
    """Return {ENV_VAR: (value, source file name)} for every key file found."""
    found: dict[str, tuple[str, str]] = {}
    for p in sorted(folder.iterdir()):
        if not p.is_file() or p.stat().st_size > 20_000:
            continue
        name = p.name.lower()
        text = p.read_text(encoding="utf-8", errors="replace")
        for sub, var, pat in RULES:
            if sub in name and var not in found:
                m = re.search(pat, text)
                if m:
                    found[var] = (m.group(0), p.name)
                break
    return found


def update_env(found: dict[str, tuple[str, str]], env_path: Path | None = None) -> list[str]:
    env_path = env_path or REPO_ROOT / ".env"
    lines = env_path.read_text(encoding="utf-8").splitlines() if env_path.exists() else []
    kept = [l for l in lines if not any(l.startswith(v + "=") for v in found)]
    kept += [f"{v}={val}" for v, (val, _) in found.items()]
    env_path.write_text("\n".join(kept) + "\n", encoding="utf-8")
    return [f"{v} <- {src} (length {len(val)})" for v, (val, src) in found.items()]
