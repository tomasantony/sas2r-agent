"""Thin client over OpenAI-compatible chat endpoints with usage accounting and code extraction."""
from __future__ import annotations

import json
import re
import time
from dataclasses import dataclass
from pathlib import Path

from openai import OpenAI

from .config import Provider, RUNS_DIR


@dataclass
class Completion:
    text: str
    input_tokens: int
    output_tokens: int
    seconds: float
    model: str


class LLM:
    def __init__(self, provider: Provider):
        if not provider.api_key and provider.api_key_env != "LOCAL_API_KEY":
            raise RuntimeError(f"{provider.api_key_env} is not set (put it in .env)")
        self.provider = provider
        self.client = OpenAI(base_url=provider.base_url, api_key=provider.api_key or "none", max_retries=4, timeout=600)
        self.usage_log = RUNS_DIR / provider.name / "usage.jsonl"
        self.usage_log.parent.mkdir(parents=True, exist_ok=True)

    def chat(self, messages: list[dict], tag: str = "") -> Completion:
        t0 = time.time()
        kwargs = dict(model=self.provider.model, messages=messages, temperature=self.provider.temperature,
                      max_tokens=self.provider.max_output_tokens)
        if self.provider.extra_body:
            kwargs["extra_body"] = self.provider.extra_body
        resp = self.client.chat.completions.create(**kwargs)
        msg = resp.choices[0].message
        text = msg.content or ""
        # some open-weight servers put the answer after a reasoning block
        text = re.sub(r"<think>.*?</think>", "", text, flags=re.S).strip()
        usage = resp.usage
        c = Completion(text, getattr(usage, "prompt_tokens", 0) or 0, getattr(usage, "completion_tokens", 0) or 0,
                       time.time() - t0, resp.model or self.provider.model)
        with open(self.usage_log, "a", encoding="utf-8") as f:
            f.write(json.dumps({"ts": time.time(), "tag": tag, "model": c.model, "in": c.input_tokens,
                                "out": c.output_tokens, "sec": round(c.seconds, 1)}) + "\n")
        return c

    def list_models(self) -> list[str]:
        return [m.id for m in self.client.models.list().data]


CODE_BLOCK = re.compile(r"```(?:r|R)?\s*\n(.*?)```", re.S)


def extract_r_code(text: str) -> str | None:
    blocks = CODE_BLOCK.findall(text)
    if blocks:
        # take the longest block: models sometimes add a short usage snippet after the script
        return max(blocks, key=len).strip() + "\n"
    stripped = text.strip()
    if stripped and ("<-" in stripped or "library(" in stripped):
        return stripped + "\n"
    return None
