#!/usr/bin/env python3
"""
CoTAS Technical Reference Manual - Book Builder

Assembles markdown content + auto-generated reference docs into a
gorgeous print-ready PDF via Jinja2 + WeasyPrint.

Usage:
    uv run python book/build_book.py                  # Full build
    uv run python book/build_book.py --html-only      # HTML preview only
    uv run python book/build_book.py --chapter 5      # Single chapter
"""

import argparse
import json
import os
import re
import sys
from pathlib import Path

import markdown
import yaml
from jinja2 import Environment, FileSystemLoader
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import TextLexer

ROOT = Path(__file__).resolve().parent.parent
BOOK_DIR = ROOT / "book"
DOCS_DIR = ROOT / "docs"
JSON_DIR = DOCS_DIR / "json"
CONTENT_DIR = BOOK_DIR / "content"
TEMPLATE_DIR = BOOK_DIR / "templates"
STYLES_DIR = BOOK_DIR / "styles"
OUTPUT_DIR = BOOK_DIR / "output"


def slugify(name: str) -> str:
    """Convert a name to a URL-safe slug for use in id/href attributes."""
    s = name.strip().upper()
    s = re.sub(r'[^A-Z0-9]+', '-', s)
    return s.strip('-')


def strip_emdashes(text: str) -> str:
    """Remove em dashes from text."""
    return text.replace('\u2014', '-').replace('\u2013', '-')

CHAPTER_COLORS = {
    1: "#6C5CE7",
    2: "#00CEC9",
    3: "#0984E3",
    4: "#E17055",
    5: "#00B894",
    6: "#FDCB6E",
    7: "#E84393",
    8: "#74B9FF",
    9: "#A29BFE",
    10: "#55EFC4",
    11: "#FF7675",
    12: "#636E72",
}

CHAPTERS = [
    {"number": 1, "title": "Introduction to CoTAS", "subtitle": "A modern runtime for a proven language", "file": "01-introduction.md"},
    {"number": 2, "title": "Getting Started", "subtitle": "Installation, first run, and project layout", "file": "02-getting-started.md"},
    {"number": 3, "title": "Language Fundamentals", "subtitle": "Types, fields, operators, and expressions", "file": "03-language-fundamentals.md"},
    {"number": 4, "title": "Structured Programming", "subtitle": "IF, WHILE, FOR, SELECT, and SCAN", "file": "04-structured-programming.md"},
    {"number": 5, "title": "Command Reference", "subtitle": "Complete A-Z command documentation", "auto": "commands"},
    {"number": 6, "title": "Function Reference", "subtitle": "Complete A-Z function documentation", "auto": "functions"},
    {"number": 7, "title": "File & Data Management", "subtitle": "Files, records, arrays, and storage", "file": "07-file-data-management.md"},
    {"number": 8, "title": "Screen & User Interface", "subtitle": "SAY, ENTER, COLOR, WINDOW, and TRAP", "file": "08-screen-ui.md"},
    {"number": 9, "title": "The CoTAS Runtime", "subtitle": "Parser, interpreter, storage, and bridge", "file": "09-runtime.md"},
    {"number": 10, "title": "Compiler Directives & Preprocessor", "subtitle": "#lib, #inc, #proc, and more", "file": "10-preprocessor.md"},
    {"number": 11, "title": "Error Reference", "subtitle": "Compiler and runtime error codes", "auto": "errors"},
    {"number": 12, "title": "Windows Programming (Legacy)", "subtitle": "OLE, DLLs, and platform compatibility", "file": "12-windows-legacy.md"},
]


def load_config() -> dict:
    with open(BOOK_DIR / "config.yaml") as f:
        return yaml.safe_load(f)


def load_css() -> str:
    css = ""
    for name in ["book.css"]:
        path = STYLES_DIR / name
        if path.exists():
            css += path.read_text() + "\n"
    return css


def get_syntax_css() -> str:
    formatter = HtmlFormatter(style="monokai", nowrap=False)
    return formatter.get_style_defs(".highlight")


def render_markdown(text: str) -> str:
    md = markdown.Markdown(
        extensions=["tables", "fenced_code", "codehilite", "attr_list", "toc"],
        extension_configs={
            "codehilite": {"css_class": "highlight", "guess_lang": False},
        },
    )
    return md.convert(text)


def load_commands_json() -> list[dict]:
    """Load command reference data from docs/json/commands/."""
    commands = []
    cmd_dir = JSON_DIR / "commands"
    if not cmd_dir.exists():
        print(f"  Warning: {cmd_dir} not found, skipping JSON commands")
        return commands

    for f in sorted(cmd_dir.glob("*.json")):
        try:
            data = json.loads(f.read_text())
            if isinstance(data, dict):
                if data.get("is_alias") and data.get("alias_target"):
                    continue
                # Strip em dashes from all string values
                for key in ("description", "comments", "syntax", "example"):
                    if key in data and isinstance(data[key], str):
                        data[key] = strip_emdashes(data[key])
                if "parameters" in data:
                    for p in data["parameters"]:
                        if "description" in p:
                            p["description"] = strip_emdashes(p["description"])
                commands.append(data)
        except (json.JSONDecodeError, KeyError) as e:
            print(f"  Warning: Could not parse {f.name}: {e}")
    return commands


def load_functions_json() -> list[dict]:
    """Load function reference data from docs/json/functions/."""
    functions = []
    fn_dir = JSON_DIR / "functions"
    if not fn_dir.exists():
        print(f"  Warning: {fn_dir} not found, skipping JSON functions")
        return functions

    for f in sorted(fn_dir.glob("*.json")):
        try:
            data = json.loads(f.read_text())
            if isinstance(data, dict):
                for key in ("description", "purpose", "example"):
                    if key in data and isinstance(data[key], str):
                        data[key] = strip_emdashes(data[key])
                if "parts" in data:
                    for p in data["parts"]:
                        if "description" in p:
                            p["description"] = strip_emdashes(p["description"])
                functions.append(data)
        except (json.JSONDecodeError, KeyError) as e:
            print(f"  Warning: Could not parse {f.name}: {e}")
    return functions


def build_name_lookup(commands: list[dict], functions: list[dict]) -> dict:
    """Build a lookup table mapping any name/abbreviation to (type, slug).

    This resolves see_also references that use abbreviations, short names,
    or function names with () to the correct anchor.
    """
    lookup = {}

    for cmd in commands:
        name = cmd.get("name", "")
        abbr = cmd.get("abbreviation", "")
        slug = slugify(name)
        lookup[name.upper()] = ("cmd", slug)
        if abbr:
            lookup[abbr.upper()] = ("cmd", slug)

    for fn in functions:
        name = fn.get("name", "")
        slug = slugify(name)
        lookup[name.upper()] = ("fn", slug)
        # Also map NAME() -> fn
        lookup[name.upper() + "()"] = ("fn", slug)

    return lookup


def resolve_see_also(see_also: list[str], lookup: dict) -> list[dict]:
    """Resolve see_also names to {name, href} dicts with correct anchors."""
    resolved = []
    for s in see_also:
        s_clean = s.strip()
        s_upper = s_clean.upper()
        # Try exact match, then without parens, then as function
        if s_upper in lookup:
            kind, slug = lookup[s_upper]
            resolved.append({"name": s_clean, "href": f"#{kind}-{slug}"})
        elif s_upper.rstrip("()") in lookup:
            kind, slug = lookup[s_upper.rstrip("()")]
            resolved.append({"name": s_clean, "href": f"#{kind}-{slug}"})
        else:
            # No match - render without link
            resolved.append({"name": s_clean, "href": ""})
    return resolved


def load_errors() -> tuple[str, str]:
    """Load error reference from docs/errors/."""
    compiler = ""
    runtime = ""
    ce = DOCS_DIR / "errors" / "compiler-errors.md"
    re_ = DOCS_DIR / "errors" / "runtime-errors.md"
    if ce.exists():
        compiler = ce.read_text()
    if re_.exists():
        runtime = re_.read_text()
    return compiler, runtime


def parse_errors_md(text: str) -> list[dict]:
    """Parse error markdown into structured entries."""
    errors = []
    current_code = None
    current_desc_lines = []

    for line in text.split("\n"):
        m = re.match(r"^## Error (\d+)", line)
        if m:
            if current_code is not None:
                errors.append({
                    "code": current_code,
                    "description": "\n".join(current_desc_lines).strip(),
                })
            current_code = m.group(1)
            current_desc_lines = []
        elif current_code is not None:
            if not line.startswith("# ") and not line.startswith("Total:"):
                current_desc_lines.append(line)

    if current_code is not None:
        errors.append({
            "code": current_code,
            "description": "\n".join(current_desc_lines).strip(),
        })
    return errors


def build_command_chapter(env: Environment, commands: list[dict], lookup: dict) -> str:
    """Render command reference chapter from JSON data."""
    tmpl = env.get_template("command_ref.html")
    html_parts = []

    current_letter = ""
    for cmd in sorted(commands, key=lambda c: c.get("name", "").upper()):
        name = cmd.get("name", "UNKNOWN")
        letter = name[0].upper() if name else "?"
        if letter != current_letter:
            current_letter = letter
            html_parts.append(
                f'<div class="letter-divider">'
                f'<div class="letter-divider-letter">{letter}</div>'
                f'<div class="letter-divider-label">Commands</div>'
                f'</div>'
            )

        see_also_resolved = resolve_see_also(cmd.get("see_also", []), lookup)

        html_parts.append(tmpl.render(
            name=name,
            slug=slugify(name),
            abbreviation=cmd.get("abbreviation"),
            platform=cmd.get("platform"),
            description=cmd.get("description", ""),
            syntax=cmd.get("syntax", ""),
            parameters=cmd.get("parameters", []),
            comments=cmd.get("comments"),
            example=cmd.get("example"),
            see_also=see_also_resolved,
        ))

    return '<div class="ref-section">' + "\n".join(html_parts) + '</div>'


def build_function_chapter(env: Environment, functions: list[dict], lookup: dict) -> str:
    """Render function reference chapter from JSON data."""
    tmpl = env.get_template("function_ref.html")
    html_parts = []

    current_letter = ""
    for fn in sorted(functions, key=lambda f: f.get("name", "").upper()):
        name = fn.get("name", "UNKNOWN")
        letter = name[0].upper() if name else "?"
        if letter != current_letter:
            current_letter = letter
            html_parts.append(
                f'<div class="letter-divider">'
                f'<div class="letter-divider-letter">{letter}</div>'
                f'<div class="letter-divider-label">Functions</div>'
                f'</div>'
            )

        see_also_resolved = resolve_see_also(fn.get("see_also", []), lookup)

        html_parts.append(tmpl.render(
            name=name,
            slug=slugify(name),
            parts_count=fn.get("parts_count"),
            returns=fn.get("return_type"),
            purpose=fn.get("purpose", fn.get("description", "")),
            parts=fn.get("parts", []),
            example=fn.get("example"),
            see_also=see_also_resolved,
        ))

    return '<div class="ref-section">' + "\n".join(html_parts) + '</div>'


def build_error_chapter(compiler_errors: list[dict], runtime_errors: list[dict]) -> str:
    """Render error reference chapter."""
    parts = ['<h2>Compiler Errors</h2>', f'<p class="text-muted">{len(compiler_errors)} error codes</p>']
    for err in compiler_errors:
        parts.append(
            f'<div class="error-entry">'
            f'<div class="error-code">Error {err["code"]}</div>'
            f'<div class="error-desc">{err["description"]}</div>'
            f'</div>'
        )

    parts.append('<div class="page-break"></div>')
    parts.append('<h2>Runtime Errors</h2>')
    parts.append(f'<p class="text-muted">{len(runtime_errors)} error codes</p>')
    for err in runtime_errors:
        parts.append(
            f'<div class="error-entry">'
            f'<div class="error-code">Error {err["code"]}</div>'
            f'<div class="error-desc">{err["description"]}</div>'
            f'</div>'
        )

    return "\n".join(parts)


def build_appendices(commands: list[dict], functions: list[dict]) -> str:
    """Build appendix content: compact quick-reference lists."""
    parts = []

    # Appendix A: Command Quick-Reference
    parts.append('<div class="appendix-opener" id="appendix-a">')
    parts.append('<div class="appendix-label">Appendix A</div>')
    parts.append('<div class="appendix-title">Command Quick-Reference</div>')
    parts.append('</div>')
    parts.append('<div class="qref">')
    for cmd in sorted(commands, key=lambda c: c.get("name", "").upper()):
        name = cmd.get("name", "")
        desc = cmd.get("description", "")
        parts.append(f'<div class="qref-item"><a href="#cmd-{slugify(name)}"><code>{name}</code></a> <span>{desc}</span></div>')
    parts.append('</div>')

    parts.append('<div class="page-break"></div>')

    # Appendix B: Function Quick-Reference
    parts.append('<div class="appendix-opener" id="appendix-b">')
    parts.append('<div class="appendix-label">Appendix B</div>')
    parts.append('<div class="appendix-title">Function Quick-Reference</div>')
    parts.append('</div>')
    parts.append('<div class="qref">')
    for fn in sorted(functions, key=lambda f: f.get("name", "").upper()):
        name = fn.get("name", "")
        ret = fn.get("return_type", "")
        purpose = fn.get("purpose", fn.get("description", ""))
        ret_tag = f' <em>{ret}</em>' if ret else ''
        parts.append(f'<div class="qref-item"><a href="#fn-{slugify(name)}"><code>{name}</code></a>{ret_tag} <span>{purpose}</span></div>')
    parts.append('</div>')

    return "\n".join(parts)


def build_index(commands: list[dict], functions: list[dict],
                compiler_errors: list[dict], runtime_errors: list[dict]) -> str:
    """Build alphabetical index at the back of the book."""
    entries = {}

    for cmd in commands:
        name = cmd.get("name", "")
        if name:
            slug = slugify(name)
            entries[name.upper()] = (name, "cmd", f"#cmd-{slug}")
    for fn in functions:
        name = fn.get("name", "")
        if name:
            slug = slugify(name)
            entries[name.upper()] = (name, "fn", f"#fn-{slug}")

    parts = []
    parts.append('<div class="appendix-opener" id="book-index">')
    parts.append('<div class="appendix-label">Index</div>')
    parts.append('<div class="appendix-title">Alphabetical Reference</div>')
    parts.append('</div>')
    parts.append('<div class="index-section">')

    current_letter = ""
    for key in sorted(entries.keys()):
        name, kind, href = entries[key]
        letter = key[0]
        if letter != current_letter:
            current_letter = letter
            parts.append(f'<div class="index-letter">{letter}</div>')

        badge = "CMD" if kind == "cmd" else "FN"
        badge_cls = "command" if kind == "cmd" else "function"
        parts.append(
            f'<div class="index-item">'
            f'<a href="{href}"><code>{name}</code></a>'
            f' <span class="index-badge {badge_cls}">{badge}</span>'
            f'</div>'
        )

    parts.append('</div>')
    return "\n".join(parts)


def build_book(args):
    print("=" * 60)
    print("  CoTAS Technical Reference Manual - Builder")
    print("=" * 60)

    config = load_config()
    css = load_css()
    syntax_css = get_syntax_css()

    env = Environment(loader=FileSystemLoader(str(TEMPLATE_DIR)))
    chapter_tmpl = env.get_template("chapter.html")
    base_tmpl = env.get_template("base.html")

    # Load reference data
    print("\n📚 Loading reference data...")
    commands = load_commands_json()
    print(f"   Commands: {len(commands)}")
    functions = load_functions_json()
    print(f"   Functions: {len(functions)}")
    compiler_md, runtime_md = load_errors()
    compiler_errors = parse_errors_md(compiler_md)
    runtime_errors = parse_errors_md(runtime_md)
    print(f"   Compiler errors: {len(compiler_errors)}")
    print(f"   Runtime errors: {len(runtime_errors)}")

    # Build name lookup for cross-referencing
    lookup = build_name_lookup(commands, functions)
    print(f"   Name lookup entries: {len(lookup)}")

    # Build chapters
    print("\n📖 Building chapters...")
    chapter_data = []

    for ch in CHAPTERS:
        num = ch["number"]

        if args.chapter and args.chapter != num:
            continue

        color = CHAPTER_COLORS.get(num, "#6C5CE7")
        print(f"   Chapter {num}: {ch['title']}")

        if "file" in ch:
            # Markdown content chapter
            md_path = CONTENT_DIR / ch["file"]
            if md_path.exists():
                content_html = render_markdown(md_path.read_text())
            else:
                content_html = f'<p class="text-muted"><em>Content coming soon.</em></p>'
                print(f"     ⚠ Missing: {ch['file']}")
        elif ch.get("auto") == "commands":
            content_html = build_command_chapter(env, commands, lookup)
        elif ch.get("auto") == "functions":
            content_html = build_function_chapter(env, functions, lookup)
        elif ch.get("auto") == "errors":
            content_html = build_error_chapter(compiler_errors, runtime_errors)
        else:
            content_html = '<p class="text-muted"><em>Content coming soon.</em></p>'

        rendered = chapter_tmpl.render(
            number=num,
            title=ch["title"],
            subtitle=ch.get("subtitle", ""),
            color=color,
            content=content_html,
        )

        # Extract section headers for TOC
        sections = re.findall(r"<h2[^>]*>(.*?)</h2>", content_html)

        chapter_data.append({
            "number": num,
            "title": ch["title"],
            "html": rendered,
            "sections": sections,
        })

    # Build appendices
    print("\n📋 Building appendices...")
    appendices_html = build_appendices(commands, functions)

    # Build index
    print("\n📇 Building index...")
    index_html = build_index(commands, functions, compiler_errors, runtime_errors)

    # Assemble final HTML
    print("\n🔨 Assembling HTML...")
    final_html = base_tmpl.render(
        config=config,
        css=css,
        syntax_css=syntax_css,
        chapters=chapter_data,
        appendices_html=appendices_html,
        index_html=index_html,
    )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    html_path = OUTPUT_DIR / "CoTAS-Reference-Manual.html"
    html_path.write_text(final_html)
    print(f"   ✅ HTML: {html_path}")

    if args.html_only:
        print("\n✨ Done (HTML only)")
        return

    # Generate PDF
    print("\n📄 Generating PDF (this may take a moment)...")
    try:
        from weasyprint import HTML
        pdf_path = OUTPUT_DIR / "CoTAS-Reference-Manual.pdf"
        HTML(string=final_html, base_url=str(BOOK_DIR)).write_pdf(str(pdf_path))
        size_mb = pdf_path.stat().st_size / (1024 * 1024)
        print(f"   ✅ PDF: {pdf_path} ({size_mb:.1f} MB)")
    except Exception as e:
        print(f"   ❌ PDF generation failed: {e}")
        print("   HTML file is still available for review.")
        sys.exit(1)

    print("\n✨ Done!")
    print(f"   Book: {pdf_path}")


def main():
    parser = argparse.ArgumentParser(description="Build the CoTAS reference manual")
    parser.add_argument("--html-only", action="store_true", help="Generate HTML only, skip PDF")
    parser.add_argument("--chapter", type=int, help="Build only a specific chapter number")
    args = parser.parse_args()
    build_book(args)


if __name__ == "__main__":
    main()
