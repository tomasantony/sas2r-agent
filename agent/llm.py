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
    reasoning: str = ""
    reasoning_tokens: int = 0
    finish_reason: str = ""

    @property
    def truncated(self) -> bool:
        return self.finish_reason == "length"


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
        choice = resp.choices[0]
        msg = choice.message
        text = msg.content or ""
        md = msg.model_dump()
        # Baseten returns reasoning in reasoning_content; Ollama/vLLM builds use reasoning or <think> tags.
        reasoning = md.get("reasoning_content") or md.get("reasoning") or ""
        m = re.search(r"<think>(.*?)</think>", text, flags=re.S)
        if m:
            reasoning = reasoning or m.group(1)
            text = re.sub(r"<think>.*?</think>", "", text, flags=re.S)
        text = text.strip()
        usage = resp.usage
        details = getattr(usage, "completion_tokens_details", None)
        rtok = getattr(details, "reasoning_tokens", 0) if details else 0
        c = Completion(text, getattr(usage, "prompt_tokens", 0) or 0, getattr(usage, "completion_tokens", 0) or 0,
                       time.time() - t0, resp.model or self.provider.model, reasoning or "", rtok or 0,
                       choice.finish_reason or "")
        with open(self.usage_log, "a", encoding="utf-8") as f:
            f.write(json.dumps({"ts": time.time(), "tag": tag, "model": c.model, "in": c.input_tokens,
                                "out": c.output_tokens, "reasoning": c.reasoning_tokens, "finish": c.finish_reason,
                                "sec": round(c.seconds, 1)}) + "\n")
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
