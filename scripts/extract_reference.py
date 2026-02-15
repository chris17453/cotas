#!/usr/bin/env python3
"""
TAS Professional 5.1 AI-Powered Reference Extractor

Uses Azure OpenAI with structured output (JSON schema) to extract
documentation from TASPro5Book.txt into strict, repeatable markdown.

Every command and function file follows an identical template.
The LLM is forced to return a fixed JSON schema — no freeform output.

Usage:
    uv run python extract_reference.py                # Full extraction
    uv run python extract_reference.py --chapter 5    # Commands only
    uv run python extract_reference.py --chapter 6    # Functions only
    uv run python extract_reference.py --dry-run      # Preview chunks
    uv run python extract_reference.py --clean        # Wipe cache + output first

Config (.env):
    AZURE_OPENAI_MODEL=gpt-5-nano       # Switch model here
    AZURE_OPENAI_MAX_TOKENS=16384       # Max completion tokens
"""

import json
import os
import re
import time
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

from dotenv import load_dotenv
from openai import AzureOpenAI

load_dotenv()

SOURCE = "TASPro5Book.txt"
OUTPUT_DIR = "docs"
CACHE_DIR = ".cache"
JSON_DIR = os.path.join(OUTPUT_DIR, "json")

AZURE_MODEL = os.getenv("AZURE_OPENAI_MODEL", "gpt-5-nano")
AZURE_MAX_TOKENS = int(os.getenv("AZURE_OPENAI_MAX_TOKENS", "16384"))

_client = AzureOpenAI(
    api_version=os.getenv("AZURE_OPENAI_API_VERSION", "2024-12-01-preview"),
    azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT"),
    api_key=os.getenv("AZURE_OPENAI_API_KEY"),
)

print(f"Model: {AZURE_MODEL} | Max tokens: {AZURE_MAX_TOKENS}")


# ═══════════════════════════════════════════════════════════════════════════
# JSON SCHEMAS — these force identical structure from the LLM every time
# ═══════════════════════════════════════════════════════════════════════════

COMMAND_SCHEMA = {
    "type": "json_schema",
    "json_schema": {
        "name": "command_extraction",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                "commands": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "abbreviation": {"type": ["string", "null"]},
                            "platform": {"type": ["string", "null"]},
                            "description": {"type": "string"},
                            "syntax": {"type": "string"},
                            "parameters": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "name": {"type": "string"},
                                        "type": {"type": "string"},
                                        "required": {"type": "boolean"},
                                        "description": {"type": "string"},
                                    },
                                    "required": ["name", "type", "required", "description"],
                                    "additionalProperties": False,
                                },
                            },
                            "comments": {"type": ["string", "null"]},
                            "example": {"type": ["string", "null"]},
                            "sample_program": {"type": ["string", "null"]},
                            "program_editor": {"type": ["string", "null"]},
                            "see_also": {"type": "array", "items": {"type": "string"}},
                            "is_alias": {"type": "boolean"},
                            "alias_target": {"type": ["string", "null"]},
                        },
                        "required": [
                            "name", "abbreviation", "platform", "description",
                            "syntax", "parameters", "comments", "example",
                            "sample_program", "program_editor", "see_also",
                            "is_alias", "alias_target",
                        ],
                        "additionalProperties": False,
                    },
                },
            },
            "required": ["commands"],
            "additionalProperties": False,
        },
    },
}

FUNCTION_SCHEMA = {
    "type": "json_schema",
    "json_schema": {
        "name": "function_extraction",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                "functions": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "signature": {"type": "string"},
                            "platform": {"type": ["string", "null"]},
                            "purpose": {"type": "string"},
                            "parts": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "number": {"type": "string"},
                                        "type": {"type": "string"},
                                        "description": {"type": "string"},
                                    },
                                    "required": ["number", "type", "description"],
                                    "additionalProperties": False,
                                },
                            },
                            "return_type": {"type": "string"},
                            "comments": {"type": ["string", "null"]},
                            "example": {"type": ["string", "null"]},
                            "sample_program": {"type": ["string", "null"]},
                            "program_editor": {"type": ["string", "null"]},
                            "see_also": {"type": "array", "items": {"type": "string"}},
                        },
                        "required": [
                            "name", "signature", "platform", "purpose",
                            "parts", "return_type", "comments", "example",
                            "sample_program", "program_editor", "see_also",
                        ],
                        "additionalProperties": False,
                    },
                },
            },
            "required": ["functions"],
            "additionalProperties": False,
        },
    },
}

STRUCTURE_SCHEMA = {
    "type": "json_schema",
    "json_schema": {
        "name": "structure_extraction",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                "structures": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "description": {"type": "string"},
                            "syntax": {"type": "string"},
                            "keywords": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "keyword": {"type": "string"},
                                        "description": {"type": "string"},
                                    },
                                    "required": ["keyword", "description"],
                                    "additionalProperties": False,
                                },
                            },
                            "max_nesting": {"type": "string"},
                            "example": {"type": ["string", "null"]},
                        },
                        "required": ["name", "description", "syntax", "keywords", "max_nesting", "example"],
                        "additionalProperties": False,
                    },
                },
            },
            "required": ["structures"],
            "additionalProperties": False,
        },
    },
}

COMPILER_SCHEMA = {
    "type": "json_schema",
    "json_schema": {
        "name": "compiler_extraction",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                "overview": {"type": "string"},
                "directives": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "syntax": {"type": "string"},
                            "description": {"type": "string"},
                        },
                        "required": ["name", "syntax", "description"],
                        "additionalProperties": False,
                    },
                },
                "notes": {"type": "array", "items": {"type": "string"}},
            },
            "required": ["overview", "directives", "notes"],
            "additionalProperties": False,
        },
    },
}


# ═══════════════════════════════════════════════════════════════════════════
# SYSTEM PROMPTS
# ═══════════════════════════════════════════════════════════════════════════

COMMAND_SYSTEM = """\
You are a precise documentation extractor for TAS Professional 5.1, a 4GL programming language.

Extract EVERY command entry from the text into strict JSON. Rules:

DESCRIPTION field:
- ONLY the prose paragraph(s) explaining what the command does.
- Do NOT include syntax lines, parameter lists, parameter names/types/descriptions, page numbers, copyright lines, or any other metadata.
- If the description naturally includes a table (e.g., data type codes), include it as prose.

SYNTAX field:
- The exact usage line(s) as printed, e.g. "ASK question DFLT default_response".
- Multi-line syntax is fine. Do NOT include descriptions of parameters here.

PARAMETERS field:
- Extract EACH parameter separately. Each gets: name, type (f/c/e, sac, n, fn/v, etc.), required (true/false), description.
- The description for each parameter must be COMPLETE — include all sub-options, value tables, notes, everything the book says about that parameter.
- Do NOT duplicate parameter descriptions in the description field.

COMMENTS field:
- Text from the "COMMENTS" section of the entry. Full text, never summarize.

Other rules:
- Alias entries (e.g., "Please see the REMOVE ARRAY command") — set is_alias=true, alias_target to the target name, description to the alias text.
- Commands with no parameters: set parameters to empty array.
- IGNORE page headers ("Command Reference"), footers ("TAS Professional 5.1"), copyright lines ("Copyright©Business Tools"), and page numbers (e.g., "5-27").
- If a command spans a page break, combine the content — don't split it into two entries.
- Preserve ALL text — never summarize or truncate. But put each piece in the CORRECT field.
"""

FUNCTION_SYSTEM = """\
You are a precise documentation extractor for TAS Professional 5.1, a 4GL programming language.

Extract EVERY function entry from the text. Functions are identified by NAME(args) headers like ABS(1), STR(1,2,3), ACS().

Rules:
1. signature = exactly as shown, e.g. "ABS(1)", "STR(1,2,3)", "ACS()"
2. name = function name without parens, e.g. "ABS", "STR"
3. Every part must have: number (1, 2, 3...), type (f/c/e, etc.), and FULL description
4. Functions with "NO OTHER PARTS" = empty parts array
5. return_type = the return type and any additional info (e.g., "N", "A The size depends on part 2")
6. Preserve ALL text — never summarize
7. Ignore page headers, footers, copyright lines, page numbers
"""

STRUCTURE_SYSTEM = """\
You are a precise documentation extractor for TAS Professional 5.1.

Extract the 5 structured programming constructs: FOR/NEXT, IF/ENDIF, SELECT/CASE, SCAN/ENDSCAN, WHILE/ENDWHILE.

For each: full description, exact syntax layout, every sub-keyword (FEXIT, FEXIT_IF, FLOOP, ELSE, OTHERWISE, etc.) with descriptions, nesting limits, and examples. Preserve ALL text.
"""

COMPILER_SYSTEM = """\
You are a precise documentation extractor for TAS Professional 5.1.

Extract compiler information: overview, every compiler directive with name/syntax/description, and any important notes. Preserve ALL text.
"""


# ═══════════════════════════════════════════════════════════════════════════
# LLM CLIENT
# ═══════════════════════════════════════════════════════════════════════════

def call_llm_structured(system: str, user: str, schema: dict, max_tokens: int | None = None) -> dict | None:
    """Call Azure OpenAI with structured output. Returns parsed dict or None."""
    if max_tokens is None:
        max_tokens = AZURE_MAX_TOKENS

    for attempt in range(3):
        try:
            resp = _client.chat.completions.create(
                model=AZURE_MODEL,
                messages=[
                    {"role": "system", "content": system},
                    {"role": "user", "content": user},
                ],
                response_format=schema,
                max_completion_tokens=max_tokens,
            )

            choice = resp.choices[0] if resp.choices else None
            if not choice or not choice.message or not choice.message.content:
                reason = choice.finish_reason if choice else "no_choice"
                if reason == "length":
                    print(f" ⚠ truncated, retrying 2x tokens...", end="", flush=True)
                    max_tokens = min(max_tokens * 2, 65536)
                    continue
                print(f" ⚠ empty ({reason}), retry...", end="", flush=True)
                time.sleep(5)
                continue

            return json.loads(choice.message.content)

        except json.JSONDecodeError as e:
            print(f" ⚠ JSON error: {e}", end="", flush=True)
            if attempt < 2:
                time.sleep(5)
        except Exception as e:
            err = str(e)
            if "429" in err or "rate" in err.lower():
                print(f" ⚠ rate limit, 30s...", end="", flush=True)
                time.sleep(30)
                continue
            print(f" ⚠ error: {e}", end="", flush=True)
            if attempt < 2:
                time.sleep(10)
            else:
                raise

    return None


# ═══════════════════════════════════════════════════════════════════════════
# TEXT CHUNKING
# ═══════════════════════════════════════════════════════════════════════════

def load_lines():
    with open(SOURCE, "r", errors="replace") as f:
        return f.readlines()


def split_into_pages(lines, start, end):
    """Split a line range into pages at form-feed (\\x0c) boundaries.
    Returns list of (start_line, [lines])."""
    pages = []
    current_start = start
    current = []
    for i in range(start, end):
        if "\x0c" in lines[i] and current:
            pages.append((current_start, current))
            current_start = i
            current = []
        current.append(lines[i])
    if current:
        pages.append((current_start, current))
    return pages


def _page_header(page_lines):
    """Get the running header from a page — the command/function name repeated
    at the top of each page as a running header.

    The page layout after a form-feed is typically:
        <form-feed line>
        Command Reference          (or Function Reference)
        <blank>
        COMMAND_NAME               <-- this is the running header
        <blank or content>

    But sometimes the order varies. We look at the first ~8 lines and pick
    the first ALL-CAPS line that isn't boilerplate or a section keyword.
    """
    # Things that are NOT the running header
    skip = {
        "Command Reference", "Function Reference",
        "Structured Programming Commands", "Compiler Information",
        "Compiler Errors", "Runtime Errors", "Windows Programming",
        "Conversion from Previous Versions",
        "GENERAL REFERENCE INFORMATION",
        "CONVENTIONS USED IN THE COMMAND REFERENCE SECTION",
        "INTENTIONALLY BLANK",
        # Section keywords that appear within entries:
        "COMMENTS", "PROGRAM EDITOR", "SEE ALSO", "SYNTAX",
        "EXAMPLE", "EXAMPLES", "SAMPLE PROGRAM", "SAMPLE PROGRAMS",
        "PURPOSE", "PARTS", "RETURN TYPE", "NO OTHER PARTS",
        "OPTIONS", "USER INTERFACE", "NOTE",
    }
    for l in page_lines[:8]:
        s = l.strip()
        if not s:
            continue
        if "\x0c" in l:
            continue
        if s in skip:
            continue
        if s.startswith("TAS Professional"):
            continue
        if "Copyright" in s or "Business Tools" in s:
            continue
        # Skip page numbers like "5-27"
        if re.fullmatch(r"\d+-\d+", s):
            continue
        # Skip bullet characters
        if s in ("•", "\u2022"):
            continue
        # Skip "TAS" / "PROFESSIONAL" / "VERSION 5.1" title pages
        if s in ("TAS", "PROFESSIONAL") or s.startswith("VERSION "):
            continue
        return s
    return "???"


def _is_valid_header(s):
    """Check if a string looks like a valid command/function running header
    (ALL CAPS name, reasonably short, not a content line)."""
    if not s or s == "???":
        return False
    if len(s) > 60:
        return False
    # Reject filenames (e.g. DATETEST.SRC, INKEY.SRC)
    if re.search(r"\.\w{2,4}$", s):
        return False
    # Must be ALL CAPS (with allowed special chars) — not a sentence
    if re.fullmatch(r"[A-Z?;*{}&][A-Z0-9 _/()&;*?{}.+#=,-]*", s):
        return True
    # Also allow "(DOS only)" / "(Windows Only)" suffixed headers
    if re.fullmatch(r"[A-Z?;*{}&][A-Z0-9 _/()&;*?{}.+#=,-]*\s*\((DOS|Windows) [Oo]nly\)", s):
        return True
    return False


def group_pages_by_header(pages):
    """Group consecutive pages with the same running header.
    This keeps multi-page commands (SCAN, DEFINE FIELD, etc.) together.

    If a page's detected header doesn't look like a valid command name
    (e.g. it picked up a content line), we inherit the previous page's header
    since that page is a continuation of the same entry.

    Returns list of (header_name, combined_text)."""
    groups = []
    cur_name = None
    cur_text = []
    for _start, plines in pages:
        h = _page_header(plines)
        # If the detected header doesn't look like a real command name,
        # treat this page as a continuation of the previous command
        if not _is_valid_header(h) and cur_name is not None:
            h = cur_name
        if h != cur_name:
            if cur_text:
                groups.append((cur_name, "".join(cur_text)))
            cur_name = h
            cur_text = list(plines)
        else:
            cur_text.extend(plines)
    if cur_text:
        groups.append((cur_name, "".join(cur_text)))
    return groups


def batch_groups(groups, max_chars=18000):
    """Batch command groups without ever splitting a multi-page command.
    LEGACY — kept for compatibility but no longer used by default."""
    batches = []
    batch = []
    size = 0
    for name, text in groups:
        tlen = len(text)
        if size + tlen > max_chars and batch:
            batches.append(batch)
            batch = []
            size = 0
        batch.append((name, text))
        size += tlen
    if batch:
        batches.append(batch)
    return batches


# ═══════════════════════════════════════════════════════════════════════════
# CACHE
# ═══════════════════════════════════════════════════════════════════════════

def cache_path(chapter: int, key) -> Path:
    """Cache path for a chapter + key. Key can be int (legacy batch) or str (entry name)."""
    if isinstance(key, int):
        fname = f"ch{chapter}_b{key:03d}.json"
    else:
        fname = f"ch{chapter}_{key}.json"
    p = Path(CACHE_DIR) / fname
    p.parent.mkdir(parents=True, exist_ok=True)
    return p


def load_cache(chapter: int, key):
    p = cache_path(chapter, key)
    if p.exists():
        with open(p) as f:
            return json.load(f)
    return None


def save_cache(chapter: int, key, data):
    with open(cache_path(chapter, key), "w") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)


# ═══════════════════════════════════════════════════════════════════════════
# MARKDOWN TEMPLATES — strict, identical format for every entry
# ═══════════════════════════════════════════════════════════════════════════

def write_file(path: str, content: str):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(content)


def write_json_entry(category: str, name: str, data: dict):
    """Save individual JSON entry alongside markdown for rebuild capability."""
    fname = safe_filename(name) + ".json"
    path = os.path.join(JSON_DIR, category, fname)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)


def safe_filename(name: str) -> str:
    out = name
    for old, new in [("?", "PRINT_MSG"), (";", "SEMI"), ("*", "STAR"),
                     ("&&", "AMP"), ("{", "LBRACE"), ("}", "RBRACE"),
                     ("(", ""), (")", ""), ("/", "_"), (" ", "_"),
                     (".", "_"), ("#", "HASH"), ("=", "EQ"), ("+", "PLUS"),
                     (",", "_"), ("&", "AND"), ("'", ""), ('"', "")]:
        out = out.replace(old, new)
    return re.sub(r"_+", "_", out).strip("_")


def command_to_md(cmd: dict) -> str:
    """Render a command dict to strict markdown. Every file looks the same."""
    lines = []
    name = cmd["name"]
    abbr = cmd.get("abbreviation")
    platform = cmd.get("platform")

    # ── Header ──
    lines.append(f"# {name}")
    lines.append("")
    lines.append("| | |")
    lines.append("|---|---|")
    lines.append(f"| **Category** | Command |")
    if abbr:
        lines.append(f"| **Abbreviation** | `{abbr}` |")
    if platform:
        lines.append(f"| **Platform** | {platform} |")
    lines.append("")

    # ── Alias redirect ──
    if cmd.get("is_alias"):
        target = cmd.get("alias_target", "")
        lines.append(f"> ↪ See [{target}]({safe_filename(target)}.md)")
        lines.append("")
        if cmd.get("description"):
            lines.append(cmd["description"])
            lines.append("")
        return "\n".join(lines)

    # ── Description ──
    lines.append("## Description")
    lines.append("")
    lines.append(cmd.get("description", ""))
    lines.append("")

    # ── Syntax ──
    syntax = cmd.get("syntax", "")
    if syntax:
        lines.append("## Syntax")
        lines.append("")
        lines.append("```text")
        lines.append(syntax)
        lines.append("```")
        lines.append("")

    # ── Parameters ──
    params = cmd.get("parameters", [])
    if params:
        lines.append("## Parameters")
        lines.append("")
        for p in params:
            req = "Required" if p["required"] else "Optional"
            lines.append(f"- **`{p['name']}`** · `{p['type']}` · *{req}*")
            lines.append("")
            # Indent description as a block under the bullet
            for desc_line in p["description"].split("\n"):
                lines.append(f"  {desc_line}")
            lines.append("")

    # ── Comments ──
    if cmd.get("comments"):
        lines.append("## Comments")
        lines.append("")
        lines.append(cmd["comments"])
        lines.append("")

    # ── Example ──
    if cmd.get("example"):
        lines.append("## Example")
        lines.append("")
        lines.append("```tas")
        lines.append(cmd["example"])
        lines.append("```")
        lines.append("")

    # ── Sample Program ──
    if cmd.get("sample_program"):
        lines.append("## Sample Program")
        lines.append("")
        lines.append(f"`{cmd['sample_program']}`")
        lines.append("")

    # ── Program Editor ──
    if cmd.get("program_editor"):
        lines.append("## Program Editor")
        lines.append("")
        lines.append(f"`{cmd['program_editor']}`")
        lines.append("")

    # ── See Also ──
    see = cmd.get("see_also", [])
    if see:
        lines.append("## See Also")
        lines.append("")
        for ref in see:
            ref = ref.strip()
            if not ref:
                continue
            fname = safe_filename(ref.rstrip("()"))
            if ref.endswith("()"):
                lines.append(f"- [{ref}](../functions/{fname}.md)")
            else:
                lines.append(f"- [{ref}]({safe_filename(ref)}.md)")
        lines.append("")

    return "\n".join(lines)


def function_to_md(func: dict) -> str:
    """Render a function dict to strict markdown. Every file looks the same."""
    lines = []
    name = func["name"]
    sig = func.get("signature", f"{name}()")
    platform = func.get("platform")

    # ── Header ──
    lines.append(f"# {sig}")
    lines.append("")
    lines.append("| | |")
    lines.append("|---|---|")
    lines.append(f"| **Category** | Function |")
    lines.append(f"| **Name** | `{name}` |")
    if platform:
        lines.append(f"| **Platform** | {platform} |")
    rt = func.get("return_type", "")
    if rt:
        lines.append(f"| **Returns** | `{rt.split()[0]}` |")
    lines.append("")

    # ── Purpose ──
    lines.append("## Purpose")
    lines.append("")
    lines.append(func.get("purpose", ""))
    lines.append("")

    # ── Parts (Parameters) ──
    parts = func.get("parts", [])
    if parts:
        lines.append("## Parts")
        lines.append("")
        lines.append("| # | Type | Description |")
        lines.append("|---|------|-------------|")
        for p in parts:
            desc = p["description"].replace("\n", " ").replace("|", "\\|")
            lines.append(f"| {p['number']} | `{p['type']}` | {desc} |")
        lines.append("")
    else:
        lines.append("*No parameters.*")
        lines.append("")

    # ── Return Type ──
    if rt:
        lines.append("## Return Type")
        lines.append("")
        lines.append(rt)
        lines.append("")

    # ── Comments ──
    if func.get("comments"):
        lines.append("## Comments")
        lines.append("")
        lines.append(func["comments"])
        lines.append("")

    # ── Example ──
    if func.get("example"):
        lines.append("## Example")
        lines.append("")
        lines.append("```tas")
        lines.append(func["example"])
        lines.append("```")
        lines.append("")

    # ── Sample Program ──
    if func.get("sample_program"):
        lines.append("## Sample Program")
        lines.append("")
        lines.append(f"`{func['sample_program']}`")
        lines.append("")

    # ── Program Editor ──
    if func.get("program_editor"):
        lines.append("## Program Editor")
        lines.append("")
        lines.append(f"`{func['program_editor']}`")
        lines.append("")

    # ── See Also ──
    see = func.get("see_also", [])
    if see:
        lines.append("## See Also")
        lines.append("")
        for ref in see:
            ref = ref.strip()
            if not ref:
                continue
            fname = safe_filename(ref.rstrip("()"))
            if ref.endswith("()"):
                lines.append(f"- [{ref}]({fname}.md)")
            else:
                lines.append(f"- [{ref}](../commands/{fname}.md)")
        lines.append("")

    return "\n".join(lines)


def structure_to_md(s: dict) -> str:
    """Render a structured programming entry to markdown."""
    lines = []
    lines.append(f"# {s['name']}")
    lines.append("")
    lines.append("| | |")
    lines.append("|---|---|")
    lines.append("| **Category** | Structured Programming |")
    lines.append(f"| **Max Nesting** | {s.get('max_nesting', '20')} |")
    lines.append("")

    lines.append("## Description")
    lines.append("")
    lines.append(s.get("description", ""))
    lines.append("")

    if s.get("syntax"):
        lines.append("## Syntax")
        lines.append("")
        lines.append("```tas")
        lines.append(s["syntax"])
        lines.append("```")
        lines.append("")

    kws = s.get("keywords", [])
    if kws:
        lines.append("## Keywords")
        lines.append("")
        for kw in kws:
            lines.append(f"### `{kw['keyword']}`")
            lines.append("")
            lines.append(kw["description"])
            lines.append("")

    if s.get("example"):
        lines.append("## Example")
        lines.append("")
        lines.append("```tas")
        lines.append(s["example"])
        lines.append("```")
        lines.append("")

    return "\n".join(lines)


# ═══════════════════════════════════════════════════════════════════════════
# EXTRACTION PIPELINES
# ═══════════════════════════════════════════════════════════════════════════

def _extract_single_command(name, text, cmd_dir):
    """Extract a single command entry via LLM. Returns list of command dicts."""
    # Check cache first (per-command cache: ch5_CMD_NAME)
    cache_key = safe_filename(name)
    cached = load_cache(5, cache_key)
    if cached is not None:
        # Write files from cache
        for cmd in cached:
            write_file(os.path.join(cmd_dir, safe_filename(cmd["name"]) + ".md"),
                      command_to_md(cmd))
            write_json_entry("commands", cmd["name"], cmd)
        return cached

    result = call_llm_structured(COMMAND_SYSTEM, text, COMMAND_SCHEMA)

    if result and result.get("commands"):
        cmds = result["commands"]
        save_cache(5, cache_key, cmds)
        for cmd in cmds:
            write_file(os.path.join(cmd_dir, safe_filename(cmd["name"]) + ".md"),
                      command_to_md(cmd))
            write_json_entry("commands", cmd["name"], cmd)
        return cmds

    # Save empty cache so we don't retry on next run (can delete cache file to force retry)
    save_cache(5, cache_key, [])
    return []


def extract_commands(lines, dry_run=False, max_workers=8):
    """Extract commands from Chapter 5 — one LLM call per command, in parallel."""
    print("\n═══ Commands (Chapter 5) ═══")

    pages = split_into_pages(lines, 9030, 18254)
    cmd_pages = []
    started = False
    for s, plines in pages:
        text = "".join(plines)
        if "? (PRINT MESSAGE)" in text or started:
            started = True
            cmd_pages.append((s, plines))

    groups = group_pages_by_header(cmd_pages)
    groups = [(n, t) for n, t in groups if n != "GENERAL REFERENCE INFORMATION" and n != "???"]
    print(f"  {len(cmd_pages)} pages → {len(groups)} command groups")

    if dry_run:
        for name, text in groups:
            print(f"  {name:40s} {len(text):6,} chars")
        return []

    cmd_dir = os.path.join(OUTPUT_DIR, "commands")
    all_cmds = []
    n_cached = 0
    n_new = 0
    n_failed = 0

    def process_command(item):
        name, text = item
        return name, _extract_single_command(name, text, cmd_dir)

    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        futures = {pool.submit(process_command, g): g[0] for g in groups}
        for future in as_completed(futures):
            name = futures[future]
            try:
                name, cmds = future.result()
                if cmds:
                    # Check if it was from cache
                    cache_key = safe_filename(name)
                    was_cached = load_cache(5, cache_key) is not None
                    if was_cached:
                        n_cached += 1
                        print(f"  ✓ {name} (cached, {len(cmds)} entries)")
                    else:
                        n_new += 1
                        cmd_names = [c["name"] for c in cmds]
                        print(f"  ✓ {name} → {', '.join(cmd_names)}")
                    all_cmds.extend(cmds)
                else:
                    n_failed += 1
                    print(f"  ✗ {name} FAILED")
            except Exception as e:
                n_failed += 1
                print(f"  ✗ {name} ERROR: {e}")

    # Dedup (keep richer entry if duplicates)
    seen = {}
    for cmd in all_cmds:
        cname = cmd["name"].strip()
        if not cname:
            continue
        if cname not in seen or len(cmd.get("parameters", [])) > len(seen[cname].get("parameters", [])):
            seen[cname] = cmd

    # Write final index
    for cmd in seen.values():
        write_file(os.path.join(cmd_dir, safe_filename(cmd["name"]) + ".md"),
                  command_to_md(cmd))
        write_json_entry("commands", cmd["name"], cmd)
    write_file(os.path.join(cmd_dir, "_index.md"),
              _make_index("Command Reference", sorted(seen.values(), key=lambda c: c["name"]), "command"))

    commands = list(seen.values())
    print(f"\n  ═ Commands: {len(commands)} extracted ({n_cached} cached, {n_new} new, {n_failed} failed)")
    return commands


def _extract_single_function(name, text, func_dir):
    """Extract a single function entry via LLM. Returns list of function dicts."""
    cache_key = safe_filename(name)
    cached = load_cache(6, cache_key)
    if cached is not None:
        for func in cached:
            write_file(os.path.join(func_dir, safe_filename(func["name"]) + ".md"),
                      function_to_md(func))
            write_json_entry("functions", func["name"], func)
        return cached

    result = call_llm_structured(FUNCTION_SYSTEM, text, FUNCTION_SCHEMA)

    if result and result.get("functions"):
        funcs = result["functions"]
        save_cache(6, cache_key, funcs)
        for func in funcs:
            write_file(os.path.join(func_dir, safe_filename(func["name"]) + ".md"),
                      function_to_md(func))
            write_json_entry("functions", func["name"], func)
        return funcs

    save_cache(6, cache_key, [])
    return []


def extract_functions(lines, dry_run=False, max_workers=8):
    """Extract functions from Chapter 6 — one LLM call per function, in parallel."""
    print("\n═══ Functions (Chapter 6) ═══")

    pages = split_into_pages(lines, 18254, 22537)
    func_pages = []
    started = False
    for s, plines in pages:
        text = "".join(plines)
        if re.search(r"^[A-Z_]+\(\d*\)", text, re.MULTILINE) or started:
            started = True
            func_pages.append((s, plines))

    groups = group_pages_by_header(func_pages)
    groups = [(n, t) for n, t in groups if n != "???" and _is_valid_header(n)]
    print(f"  {len(func_pages)} pages → {len(groups)} function groups")

    if dry_run:
        for name, text in groups:
            print(f"  {name:40s} {len(text):6,} chars")
        return []

    func_dir = os.path.join(OUTPUT_DIR, "functions")
    all_funcs = []
    n_cached = 0
    n_new = 0
    n_failed = 0

    def process_function(item):
        name, text = item
        return name, _extract_single_function(name, text, func_dir)

    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        futures = {pool.submit(process_function, g): g[0] for g in groups}
        for future in as_completed(futures):
            name = futures[future]
            try:
                name, funcs = future.result()
                if funcs:
                    cache_key = safe_filename(name)
                    was_cached = load_cache(6, cache_key) is not None
                    if was_cached:
                        n_cached += 1
                        print(f"  ✓ {name} (cached, {len(funcs)} entries)")
                    else:
                        n_new += 1
                        func_names = [f["name"] for f in funcs]
                        print(f"  ✓ {name} → {', '.join(func_names)}")
                    all_funcs.extend(funcs)
                else:
                    n_failed += 1
                    print(f"  ✗ {name} FAILED")
            except Exception as e:
                n_failed += 1
                print(f"  ✗ {name} ERROR: {e}")

    # Dedup
    seen = {}
    for func in all_funcs:
        fname = func["name"].strip()
        if not fname:
            continue
        if fname not in seen or len(func.get("parts", [])) > len(seen[fname].get("parts", [])):
            seen[fname] = func

    for func in seen.values():
        write_file(os.path.join(func_dir, safe_filename(func["name"]) + ".md"),
                  function_to_md(func))
        write_json_entry("functions", func["name"], func)
    write_file(os.path.join(func_dir, "_index.md"),
              _make_index("Function Reference", sorted(seen.values(), key=lambda f: f["name"]), "function"))

    functions = list(seen.values())
    print(f"\n  ═ Functions: {len(functions)} extracted ({n_cached} cached, {n_new} new, {n_failed} failed)")
    return functions


def extract_structures(lines, dry_run=False):
    """Extract structured programming from Chapter 7 (lines 22537–22976)."""
    print("\n═══ Structured Programming (Chapter 7) ═══")
    text = "".join(lines[22537:22976])
    print(f"  {len(text):,} chars")

    if dry_run:
        return []

    cached = load_cache(7, 0)
    if cached is not None:
        print(f"  cached: {len(cached)} structures")
        struct_dir = os.path.join(OUTPUT_DIR, "structured-programming")
        for s in cached:
            write_file(os.path.join(struct_dir, safe_filename(s["name"]) + ".md"),
                      structure_to_md(s))
            write_json_entry("structures", s["name"], s)
        return cached

    print("  extracting...", end="", flush=True)
    result = call_llm_structured(STRUCTURE_SYSTEM, text, STRUCTURE_SCHEMA)

    if result and result.get("structures"):
        structs = result["structures"]
        print(f" → {len(structs)} structures")
        save_cache(7, 0, structs)
        struct_dir = os.path.join(OUTPUT_DIR, "structured-programming")
        idx_lines = ["# Structured Programming", "", f"Total: {len(structs)}", ""]
        for s in structs:
            fname = safe_filename(s["name"])
            write_file(os.path.join(struct_dir, fname + ".md"), structure_to_md(s))
            write_json_entry("structures", s["name"], s)
            idx_lines.append(f"- [{s['name']}]({fname}.md)")
        idx_lines.append("")
        write_file(os.path.join(struct_dir, "_index.md"), "\n".join(idx_lines))
        return structs

    print(" → FAILED")
    return []


def extract_compiler(lines, dry_run=False):
    """Extract compiler info from Chapter 4 (lines 8507–9030)."""
    print("\n═══ Compiler Info (Chapter 4) ═══")
    text = "".join(lines[8507:9030])
    print(f"  {len(text):,} chars")

    if dry_run:
        return None

    cached = load_cache(4, 0)
    if cached is not None:
        print("  cached")
        return cached

    print("  extracting...", end="", flush=True)
    result = call_llm_structured(COMPILER_SYSTEM, text, COMPILER_SCHEMA)

    if result:
        print(" → done")
        save_cache(4, 0, result)
        # Write markdown
        md = ["# Compiler Information", ""]
        md.append("## Overview")
        md.append("")
        md.append(result.get("overview", ""))
        md.append("")
        for d in result.get("directives", []):
            md.append(f"### `{d['name']}`")
            md.append("")
            if d.get("syntax"):
                md.extend(["```tas", d["syntax"], "```", ""])
            md.append(d.get("description", ""))
            md.append("")
        if result.get("notes"):
            md.append("## Notes")
            md.append("")
            for n in result["notes"]:
                md.append(f"- {n}")
            md.append("")
        write_file(os.path.join(OUTPUT_DIR, "compiler", "overview.md"), "\n".join(md))
        return result

    print(" → FAILED")
    return None


def extract_errors(lines, start, end):
    """Extract errors with simple regex — no LLM needed."""
    errors = []
    cur_num = None
    cur_lines = []

    for i in range(start, end):
        line = lines[i]
        s = line.strip()
        if "\x0c" in line or s.startswith("TAS Professional") or \
           ("Business Tools" in s and "Copyright" in s) or \
           re.fullmatch(r"\d+-\d+", s):
            continue
        if re.fullmatch(r"\d{1,3}", s):
            if cur_num is not None:
                errors.append({"number": int(cur_num), "message": "\n".join(cur_lines).strip()})
            cur_num = s
            cur_lines = []
        elif cur_num is not None and s and s not in ("Compiler Errors", "Runtime Errors", "INTENTIONALLY BLANK"):
            cur_lines.append(s)

    if cur_num is not None:
        errors.append({"number": int(cur_num), "message": "\n".join(cur_lines).strip()})

    return errors


def extract_windows(lines, dry_run=False):
    """Extract Windows programming from Chapter 11 — raw text, no LLM."""
    print("\n═══ Windows Programming (Chapter 11) ═══")
    # Simple boilerplate strip
    clean = []
    skip = 0
    for i in range(25132, min(len(lines), 28000)):
        if "\x0c" in lines[i]:
            skip = 2
            continue
        if skip > 0:
            skip -= 1
            continue
        s = lines[i].strip()
        if s.startswith("TAS Professional") or ("Business Tools" in s and "Copyright" in s) or re.fullmatch(r"\d+-\d+", s):
            continue
        clean.append(lines[i].rstrip())

    text = "\n".join(clean)
    write_file(os.path.join(OUTPUT_DIR, "windows", "overview.md"),
              f"# Windows Programming\n\n{text}\n")
    print(f"  ✓ {len(text):,} chars")
    return text


# ═══════════════════════════════════════════════════════════════════════════
# INDEX & STATIC FILES
# ═══════════════════════════════════════════════════════════════════════════

def _make_index(title, entries, entry_type):
    lines = [f"# {title}", "", f"Total: {len(entries)}", ""]
    letter = ""
    for e in entries:
        name = e.get("name", "")
        first = name[0].upper() if name else "?"
        if first != letter:
            letter = first
            lines.extend([f"## {letter}", ""])
        fname = safe_filename(name)
        if entry_type == "function":
            sig = e.get("signature", f"{name}()")
            lines.append(f"- [{sig}]({fname}.md)")
        else:
            lines.append(f"- [{name}]({fname}.md)")
    lines.append("")
    return "\n".join(lines)


DATA_TYPES_MD = """\
# Data Types & Notation

## Field Types

| Type | Name | Internal Size | Description |
|------|------|---------------|-------------|
| `A` | Alphanumeric | max 4 GB | Text/string data |
| `N` | Numeric | 8 bytes | IEEE floating-point |
| `D` | Date | 4 bytes | Date value |
| `T` | Time | 4 bytes | Time value |
| `I` | Integer | 2 bytes | 16-bit signed integer |
| `B` | Byte | 1 byte | Single byte value |
| `F` | F-type Pointer | 5 bytes | Field pointer (use with `&` redirector) |
| `P` | P-type Pointer | 14 bytes | Program pointer |
| `R` | Record Number | 4 bytes | 32-bit longint / record number |
| `L` | Logical | 1 byte | Boolean `.T.` or `.F.` |
| `O` | Old BCD | max 10 bytes | TAS Pro 3.0 BCD numeric (compat) |
| `V` | Overlay | variable | Overlay field (internal A type) |

## Entry Type Notation

| Notation | Meaning |
|----------|---------|
| `f/c/e` | Field, Constant, or Expression |
| `fn/v` | Field Name or Variable field |
| `file_expr` | File name (unquoted) or `@field` for variable-opened files |
| `key_expr` | `@n` (key number), key name, or `@0` (direct access) |
| `lexpr` | Logical expression → `.T.` or `.F.` |
| `sac` | Special Alpha Constant — unquoted, starts A-Z, max 14 chars |
| `flist` | Array of F-type pointers (replaces explicit field list) |
| `scope` | `A` (All), `F` (First n), `N` (Next n), `R` (Rest) |
| `prt_where` | `S` (Screen), `P` (Printer), `D` (Disk), `A` (Ask) |

## Operators

| Operator | Meaning |
|----------|---------|
| `.A.` / `.AND.` | Logical AND |
| `.O.` / `.OR.` | Logical OR |
| `.N.` / `.NOT.` | Logical NOT |
| `.T.` | True |
| `.F.` | False |

## Special Syntax

- Integer constant suffix: `!` (e.g., `0!`)
- Continuation: `\\\\` at end of line
- Multiple commands per line: `\\\\|` separator
- Remarks: `;`, `*`, or `&&`
- Line labels: up to 14 chars ending with `:` (e.g., `START:`)
- Field redirector: `&` (e.g., `&fptr[n]`)
"""

CONVENTIONS_MD = """\
# Conventions

## Parameter Notation

| Marker | Meaning |
|--------|---------|
| *Required* | Parameter must be provided |
| *Optional* | Parameter may be omitted |
| `f/c/e` | Accepts field, constant, or expression |
| `sac` | Special alpha constant (e.g., `#ADD_FLDS`) |

## Program Editor Paths

Menu paths like `User interface -> Messages -> Ask` show where to find each command in the TAS Program Editor.

## Platform Markers

| Marker | Meaning |
|--------|---------|
| **Windows Only** | Ignored (no error) in DOS |
| **DOS Only** | Ignored (no error) in Windows |
| *(none)* | Works in both |

## Field Name Rules

- Max 10 characters
- Must start with a letter (A–Z)
- Letters, digits, underscores only
- Case insensitive
- Cannot be a reserved word

## File Numbering

- Files numbered 1–99
- File 0 reserved for system
- `@file_number` references a file by runtime number
"""


# ═══════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="TAS Pro 5.1 reference extractor")
    parser.add_argument("--chapter", type=int, help="Extract only this chapter (4–11)")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--rebuild", action="store_true", help="Rebuild markdown from saved JSON (no API calls)")
    parser.add_argument("--workers", type=int, default=8, help="Parallel workers for LLM calls (default: 8)")
    args = parser.parse_args()

    lines = load_lines()
    print(f"Loaded {len(lines):,} lines from {SOURCE}")

    # ── Rebuild mode: regenerate markdown from saved JSON files ──
    if args.rebuild:
        print("\n═══ REBUILD from JSON ═══")
        n_cmd = n_func = n_struct = 0

        # Commands
        json_cmd_dir = os.path.join(JSON_DIR, "commands")
        cmd_dir = os.path.join(OUTPUT_DIR, "commands")
        if os.path.isdir(json_cmd_dir):
            cmds = {}
            for f in sorted(Path(json_cmd_dir).glob("*.json")):
                cmd = json.loads(f.read_text())
                cmds[cmd["name"]] = cmd
                write_file(os.path.join(cmd_dir, f.stem + ".md"), command_to_md(cmd))
            write_file(os.path.join(cmd_dir, "_index.md"),
                      _make_index("Command Reference", sorted(cmds.values(), key=lambda c: c["name"]), "command"))
            n_cmd = len(cmds)
            print(f"  Commands: {n_cmd}")

        # Functions
        json_func_dir = os.path.join(JSON_DIR, "functions")
        func_dir = os.path.join(OUTPUT_DIR, "functions")
        if os.path.isdir(json_func_dir):
            funcs = {}
            for f in sorted(Path(json_func_dir).glob("*.json")):
                func = json.loads(f.read_text())
                funcs[func["name"]] = func
                write_file(os.path.join(func_dir, f.stem + ".md"), function_to_md(func))
            write_file(os.path.join(func_dir, "_index.md"),
                      _make_index("Function Reference", sorted(funcs.values(), key=lambda f: f["name"]), "function"))
            n_func = len(funcs)
            print(f"  Functions: {n_func}")

        # Structures
        json_struct_dir = os.path.join(JSON_DIR, "structures")
        struct_dir = os.path.join(OUTPUT_DIR, "structured-programming")
        if os.path.isdir(json_struct_dir):
            structs = []
            for f in sorted(Path(json_struct_dir).glob("*.json")):
                s = json.loads(f.read_text())
                structs.append(s)
                write_file(os.path.join(struct_dir, f.stem + ".md"), structure_to_md(s))
            idx_lines = ["# Structured Programming", "", f"Total: {len(structs)}", ""]
            for s in structs:
                idx_lines.append(f"- [{s['name']}]({safe_filename(s['name'])}.md)")
            idx_lines.append("")
            write_file(os.path.join(struct_dir, "_index.md"), "\n".join(idx_lines))
            n_struct = len(structs)
            print(f"  Structures: {n_struct}")

        print(f"\n✓ Rebuilt {n_cmd} commands, {n_func} functions, {n_struct} structures from JSON")
        return

    chapters = {args.chapter} if args.chapter else {4, 5, 6, 7, 9, 10, 11}

    n_cmd = n_func = n_struct = n_cerr = n_rerr = 0

    if 5 in chapters:
        cmds = extract_commands(lines, args.dry_run, max_workers=args.workers)
        n_cmd = len(cmds)

    if 6 in chapters:
        funcs = extract_functions(lines, args.dry_run, max_workers=args.workers)
        n_func = len(funcs)

    if 7 in chapters:
        structs = extract_structures(lines, args.dry_run)
        n_struct = len(structs)

    if 4 in chapters:
        extract_compiler(lines, args.dry_run)

    if 9 in chapters:
        print("\n═══ Compiler Errors (Chapter 9) ═══")
        cerrs = extract_errors(lines, 23376, 24166)
        n_cerr = len(cerrs)
        md = ["# Compiler Errors", "", f"Total: {n_cerr}", ""]
        for e in sorted(cerrs, key=lambda x: x["number"]):
            md.extend([f"## Error {e['number']}", "", e["message"], ""])
        write_file(os.path.join(OUTPUT_DIR, "errors", "compiler-errors.md"), "\n".join(md))
        print(f"  ✓ {n_cerr} errors")

    if 10 in chapters:
        print("\n═══ Runtime Errors (Chapter 10) ═══")
        rerrs = extract_errors(lines, 24166, 25132)
        n_rerr = len(rerrs)
        md = ["# Runtime Errors", "", f"Total: {n_rerr}", ""]
        for e in sorted(rerrs, key=lambda x: x["number"]):
            md.extend([f"## Error {e['number']}", "", e["message"], ""])
        write_file(os.path.join(OUTPUT_DIR, "errors", "runtime-errors.md"), "\n".join(md))
        print(f"  ✓ {n_rerr} errors")

    if 11 in chapters:
        extract_windows(lines, args.dry_run)

    if not args.dry_run:
        # Static files
        write_file(os.path.join(OUTPUT_DIR, "data-types.md"), DATA_TYPES_MD)
        write_file(os.path.join(OUTPUT_DIR, "conventions.md"), CONVENTIONS_MD)

        # README
        write_file(os.path.join(OUTPUT_DIR, "README.md"), f"""\
# TAS Professional 5.1 — Reference

| Section | Count |
|---------|-------|
| [Commands](commands/_index.md) | {n_cmd} |
| [Functions](functions/_index.md) | {n_func} |
| [Structured Programming](structured-programming/_index.md) | {n_struct} |
| [Compiler Info](compiler/overview.md) | — |
| [Compiler Errors](errors/compiler-errors.md) | {n_cerr} |
| [Runtime Errors](errors/runtime-errors.md) | {n_rerr} |
| [Windows Programming](windows/overview.md) | — |
| [Data Types](data-types.md) | — |
| [Conventions](conventions.md) | — |
""")

    print("\n" + "=" * 50)
    print("DONE")
    print("=" * 50)
    print(f"  Commands:      {n_cmd}")
    print(f"  Functions:     {n_func}")
    print(f"  Structures:    {n_struct}")
    print(f"  Comp Errors:   {n_cerr}")
    print(f"  RT Errors:     {n_rerr}")


if __name__ == "__main__":
    main()
