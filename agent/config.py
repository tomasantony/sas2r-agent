"""Configuration: paths, model providers and loop settings.

Everything secret comes from the environment (a .env file next to this package's repo root is
loaded if present). Nothing here prints or logs key values.
"""
from __future__ import annotations

import os
from dataclasses import dataclass, field
from pathlib import Path

from dotenv import load_dotenv

REPO_ROOT = Path(__file__).resolve().parent.parent
load_dotenv(REPO_ROOT / ".env")

# The benchmark (SAS programs, data, reference outputs) lives in a separate repository.
BENCHMARK_ROOT = Path(os.environ.get("BENCHMARK_ROOT", r"D:\meta_hackathon\sas2r-agent")).resolve()
RUNS_DIR = Path(os.environ.get("RUNS_DIR", REPO_ROOT / "runs")).resolve()
RSCRIPT = os.environ.get("RSCRIPT", r"C:\Program Files\R\R-4.6.0\bin\Rscript.exe")


@dataclass(frozen=True)
class Provider:
    """An OpenAI-compatible chat endpoint serving one model."""

    name: str
    base_url: str
    model: str
    api_key_env: str
    note: str = ""
    max_output_tokens: int = 16000
    temperature: float = 0.0
    extra_body: dict = field(default_factory=dict)
    sdk: str = "openai"            # "openai" (chat completions) or "anthropic" (official Anthropic SDK)
    effort: str = ""               # Anthropic output_config.effort: low | medium | high | xhigh | max
    role: str = "meta"             # leaderboard grouping: meta | baseline

    @property
    def api_key(self) -> str | None:
        return os.environ.get(self.api_key_env)


PROVIDERS: dict[str, Provider] = {
    # Primary: Meta Model API, closed Muse Spark. Chat Completions protocol.
    "muse-spark": Provider(
        name="muse-spark",
        base_url="https://api.meta.ai/v1",
        model="muse-spark-1.3",
        api_key_env="MODEL_API_KEY",
        note="Meta Model API, Muse Spark 1.3",
        # reasoning counts against max_tokens here too (P004 attempt 1 spent 16k tokens reasoning, no code)
        max_output_tokens=48000,
        extra_body={"reasoning_effort": "medium"},
    ),
    # Open-weight row: Muse Spark Open served by Baseten (hackathon grant key).
    "muse-spark-open-baseten": Provider(
        name="muse-spark-open-baseten",
        base_url="https://inference.baseten.co/v1",
        model="meta-models/muse-spark-open-1026",
        api_key_env="BASETEN_API_KEY",
        note="Baseten, Muse Spark Open (hackathon model id)",
        # Muse Spark Open always reasons (default effort xhigh) and reasoning counts against max_tokens:
        # a 250-line program burnt 16k tokens of reasoning before any code. Medium effort, big budget.
        max_output_tokens=48000,
        extra_body={"reasoning_effort": "medium"},
    ),
    # Open-weight row via Ollama Cloud (model id to confirm against /v1/models).
    "muse-spark-open-ollama": Provider(
        name="muse-spark-open-ollama",
        base_url="https://ollama.com/v1",
        model=os.environ.get("OLLAMA_MODEL", "muse-spark-open"),
        api_key_env="OLLAMA_API_KEY",
        note="Ollama Cloud, Muse Spark Open",
        max_output_tokens=32000,
    ),
    # Baseline rows (not Meta models; labelled as baselines on the leaderboard).
    "gpt-oss-120b-ollama": Provider(
        name="gpt-oss-120b-ollama",
        base_url="https://ollama.com/v1",
        model="gpt-oss:120b",
        api_key_env="OLLAMA_API_KEY",
        note="baseline: OpenAI gpt-oss-120b (open weight) on Ollama Cloud",
        max_output_tokens=32000,
        role="baseline",
    ),
    "claude-opus-5-5": Provider(
        name="claude-opus-5-5",
        base_url="",
        model="claude-opus-5-5",
        api_key_env="ANTHROPIC_API_KEY",
        note="baseline: Claude Opus 5.5 via the Anthropic SDK (adaptive thinking, effort from ANTHROPIC_EFFORT)",
        max_output_tokens=16000,
        sdk="anthropic",
        effort=os.environ.get("ANTHROPIC_EFFORT", "medium"),
        role="baseline",
    ),
    # Local serving (vLLM / Ollama on this machine or the DGX Spark), no key needed.
    "local": Provider(
        name="local",
        base_url=os.environ.get("LOCAL_BASE_URL", "http://localhost:11434/v1"),
        model=os.environ.get("LOCAL_MODEL", "muse-spark-open"),
        api_key_env="LOCAL_API_KEY",
        note="local OpenAI-compatible server",
    ),
}


@dataclass
class LoopSettings:
    max_iterations: int = 3          # translate + up to (max_iterations - 1) repairs
    r_timeout_sec: int = 600
    text_recall_threshold: float = 0.95   # tables/listings/figures: share of SAS numbers found in R text
    dataset_cell_threshold: float = 0.999  # datasets: share of cells equal (after tolerance)
    numeric_atol: float = 1e-6
    numeric_rtol: float = 1e-6
    max_context_chars: int = 120_000      # cap on SAS source + macro text handed to the model
    history_attempts: int = 2             # previous attempts kept in the conversation for repair


SETTINGS = LoopSettings()

# USD per million tokens (input, output); reasoning tokens are billed as output everywhere.
# Override with PRICE_<PROVIDER_NAME_UPPER>="in,out" in .env when a vendor publishes a different rate.
_DEFAULT_PRICES: dict[str, tuple[float, float]] = {
    "muse-spark": (float(os.environ.get("MUSE_SPARK_PRICE_IN", "0")), float(os.environ.get("MUSE_SPARK_PRICE_OUT", "0"))),
    "muse-spark-open-baseten": (0.0, 0.0),     # hackathon grant key, $100 pool
    "muse-spark-open-ollama": (0.0, 0.0),
    "gpt-oss-120b-ollama": (0.0, 0.0),
    "claude-opus-5-5": (4.0, 20.0),
    "local": (0.0, 0.0),
}


def price_for(provider_name: str) -> tuple[float, float]:
    env = os.environ.get("PRICE_" + provider_name.upper().replace("-", "_"))
    if env:
        a, b = env.split(",")
        return float(a), float(b)
    return _DEFAULT_PRICES.get(provider_name, (0.0, 0.0))


def estimate_cost_usd(provider_name: str, input_tokens: int, output_tokens: int) -> float:
    pin, pout = price_for(provider_name)
    return input_tokens / 1e6 * pin + output_tokens / 1e6 * pout

# R packages the model may rely on (all installed on the reference machine).
R_PACKAGES = [
    "haven", "dplyr", "tidyr", "readr", "stringr", "lubridate", "purrr", "tibble", "jsonlite",
    "data.table", "ggplot2", "survival", "survminer", "broom", "Hmisc", "admiral", "pharmaversesdtm",
    "readxl", "writexl", "openxlsx", "gt", "r2rtf", "arrow",
]
