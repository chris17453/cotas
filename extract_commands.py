#!/usr/bin/env python3
"""
Extract TAS Professional 5.1 command reference entries from TASPro5Book.txt
into structured per-command markdown files.

Heuristics:
- Treat a line of uppercase text (letters/digits/punctuation) as a command name
  when followed by a line containing lowercase text.
- The command summary is the prose between the name and the first signature line
  (which starts with the command name).
- The signature block runs until the next blank line.
- Everything that follows (until the next command heading) is treated as the
  parameter/details block.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import List, Optional

ROOT = Path(__file__).parent
SOURCE = ROOT / "TASPro5Book.txt"
# Write outputs to a separate directory to keep iterations non-destructive.
OUTPUT_DIR = ROOT / "command_reference_v2"


def read_lines() -> List[str]:
    if not SOURCE.exists():
        raise SystemExit(f"Missing source text: {SOURCE}")
    text = SOURCE.read_text(encoding="utf-8", errors="ignore")
    return [line.rstrip("\n") for line in text.splitlines()]


def locate_command_section(lines: List[str]) -> List[str]:
    start = 0
    for idx, line in enumerate(lines):
        if line.strip() == "Command Reference":
            start = idx + 1
            break
    for idx in range(start, len(lines)):
        if lines[idx].strip() == "ADD":
            start = idx
            break
    end = len(lines)
    for idx in range(start, len(lines)):
        if lines[idx].strip() == "Index":
            end = idx
            break
    return lines[start:end]


STOP_TOKENS = (
    "REFERENCE",
    "GENERAL",
    "CHAPTER",
    "APPENDIX",
    "INDEX",
    "TABLE",
    "TAS PROFESSIONAL",
    "VERSION",
    "INTENTIONALLY",
    "COPYRIGHT",
    "DISCLAIMER",
    "PURPOSE",
    "PARTS",
    "RETURN TYPE",
    "EXAMPLE",
    "EXAMPLES",
    "COMMENTS",
    "PROGRAM EDITOR",
    "SAMPLE PROGRAMS",
    "FUNCTION REFERENCE",
    "FUNCTIONS",
    "OPERATORS",
    "OPERATOR",
)


def is_boilerplate(line: str) -> bool:
    text = line.strip()
    if not text:
        return False
    if re.fullmatch(r"\d+-\d+", text):
        return True
    if text.startswith("TAS Professional"):
        return True
    if text.startswith("Copyright"):
        return True
    if text in {"Command Reference", "Command Reference "}:  # some entries include trailing space
        return True
    return False


def clean_line(line: str) -> Optional[str]:
    if is_boilerplate(line):
        return None
    stripped = line.strip()
    return stripped or None


def is_command_heading(line: str) -> bool:
    text = line.strip()
    if not text or len(text) > 40:
        return False
    if any(token in text for token in STOP_TOKENS):
        return False
    if not re.fullmatch(r"[A-Z0-9 \-+#&/().]+", text):
        return False
    if not re.search(r"[A-Z]", text):
        return False
    if re.fullmatch(r"[0-9./\\-]+", text):
        return False
    return True


def has_lowercase_ahead(lines: List[str], idx: int) -> bool:
    for j in range(idx, min(idx + 10, len(lines))):
        if lines[j].strip() and re.search(r"[a-z]", lines[j]):
            return True
    return False


def extract_commands(lines: List[str]):
    commands = []
    seen = set()
    headings = [
        idx
        for idx, line in enumerate(lines)
        if is_command_heading(line) and has_lowercase_ahead(lines, idx + 1)
    ]

    for pos, start in enumerate(headings):
        name = lines[start].strip()
        end = headings[pos + 1] if pos + 1 < len(headings) else len(lines)
        block = lines[start + 1 : end]

        ptr = 0
        while ptr < len(block) and not block[ptr].strip():
            ptr += 1

        summary: List[str] = []
        while ptr < len(block) and block[ptr].strip():
            cleaned = clean_line(block[ptr])
            if cleaned is not None and cleaned != name:
                summary.append(cleaned)
            ptr += 1

        while ptr < len(block) and not block[ptr].strip():
            ptr += 1

        signature: List[str] = []
        while ptr < len(block) and block[ptr].strip():
            cleaned = clean_line(block[ptr])
            if cleaned is not None and cleaned != name:
                signature.append(cleaned)
            ptr += 1

        while ptr < len(block) and not block[ptr].strip():
            ptr += 1

        details: List[str] = []
        while ptr < len(block):
            cleaned = clean_line(block[ptr])
            if cleaned is not None and cleaned != name:
                details.append(cleaned)
            ptr += 1

        if (summary or signature or details) and name not in seen:
            seen.add(name)
            commands.append(
                {
                    "name": name,
                    "summary": "\n".join(summary).strip(),
                    "signature": "\n".join(signature).strip(),
                    "details": "\n".join(details).strip(),
                }
            )

    # Add aliases: if a function NAME(1) exists but NAME() does not, duplicate content.
    by_name = {c["name"]: c for c in commands}
    for name, cmd in list(by_name.items()):
        m = re.match(r"^(.*)\(1\)$", name)
        if m:
            alias = f"{m.group(1)}()"
            if alias not in by_name:
                new_cmd = {
                    "name": alias,
                    "summary": cmd["summary"],
                    "signature": cmd["signature"].replace(name, alias),
                    "details": cmd["details"],
                }
                commands.append(new_cmd)
                by_name[alias] = new_cmd

    return commands


def write_command_files(commands):
    OUTPUT_DIR.mkdir(exist_ok=True)
    for old in OUTPUT_DIR.glob("*.md"):
        old.unlink()
    for cmd in commands:
        safe_name = re.sub(r"[^A-Za-z0-9_.()\\-]+", "_", cmd["name"]).strip("_")
        target = OUTPUT_DIR / f"{safe_name}.md"
        with target.open("w", encoding="utf-8") as fh:
            fh.write(f"# {cmd['name']}\n\n")
            if cmd["summary"]:
                fh.write("## Summary\n")
                fh.write(cmd["summary"] + "\n\n")
            if cmd["signature"]:
                fh.write("## Signature\n")
                fh.write("```\n")
                fh.write(cmd["signature"] + "\n")
                fh.write("```\n\n")
            if cmd["details"]:
                fh.write("## Details\n")
                fh.write(cmd["details"] + "\n")


def main():
    lines = locate_command_section(read_lines())
    commands = extract_commands(lines)
    if not commands:
        raise SystemExit("No commands extracted; adjust heuristics.")
    write_command_files(commands)
    print(f"Wrote {len(commands)} command files to {OUTPUT_DIR}")


if __name__ == "__main__":
    main()
