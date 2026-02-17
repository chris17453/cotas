#!/usr/bin/env python3
"""
Batch round-trip test: decompile all .RUN files, recompile, and diff against originals.

Usage:
    python3 scripts/batch_roundtrip.py [--decompile-only] [--compile-only] [--diff-only]

Steps:
    1. Decompile blank/*.RUN → decompiled2/*.SRC
    2. Compile decompiled2/*.SRC → test/run/*.RUN
    3. Diff blank/*.RUN vs test/run/*.RUN and report
"""

import os
import sys
import struct
import subprocess
import argparse
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed

ROOT = Path(__file__).resolve().parent.parent
BLANK_DIR = ROOT / "blank"
SRC_DIR = ROOT / "decompiled2"
RUN_DIR = ROOT / "test" / "run"
CLI_EXE = ROOT / "src" / "CoTAS.Cli" / "bin" / "Release" / "net10.0" / "CoTAS.Cli"
CLI = str(CLI_EXE)
CLI_ARGS: list[str] = []


def decompile_all():
    """Decompile all .RUN files to .SRC."""
    SRC_DIR.mkdir(parents=True, exist_ok=True)
    run_files = sorted(BLANK_DIR.glob("*.RUN"))
    print(f"Decompiling {len(run_files)} files → {SRC_DIR}/")

    ok = 0
    fail = 0
    errors = []
    for rf in run_files:
        src_path = SRC_DIR / rf.with_suffix(".SRC").name
        try:
            result = subprocess.run(
                [CLI] + CLI_ARGS + ["--decompile", str(rf)],
                capture_output=True, text=True, timeout=30
            )
            if result.returncode == 0 and result.stdout.strip():
                src_path.write_text(result.stdout)
                ok += 1
            else:
                fail += 1
                errors.append((rf.name, result.stderr.strip()[:120]))
        except Exception as e:
            fail += 1
            errors.append((rf.name, str(e)[:120]))

    print(f"  Decompiled: {ok} OK, {fail} FAIL")
    for name, err in errors[:20]:
        print(f"    FAIL {name}: {err}")
    if len(errors) > 20:
        print(f"    ... and {len(errors)-20} more")


def compile_all():
    """Compile all .SRC files to .RUN."""
    RUN_DIR.mkdir(parents=True, exist_ok=True)
    src_files = sorted(SRC_DIR.glob("*.SRC"))
    print(f"Compiling {len(src_files)} files → {RUN_DIR}/")

    ok = 0
    fail = 0
    errors = []
    for sf in src_files:
        run_path = RUN_DIR / sf.with_suffix(".RUN").name
        try:
            result = subprocess.run(
                [CLI] + CLI_ARGS + ["--compile", str(sf), "-o", str(run_path)],
                capture_output=True, text=True, timeout=30
            )
            if result.returncode == 0:
                ok += 1
            else:
                fail += 1
                errors.append((sf.name, result.stderr.strip()[:120]))
        except Exception as e:
            fail += 1
            errors.append((sf.name, str(e)[:120]))

    print(f"  Compiled: {ok} OK, {fail} FAIL")
    for name, err in errors[:20]:
        print(f"    FAIL {name}: {err}")
    if len(errors) > 20:
        print(f"    ... and {len(errors)-20} more")


def diff_section(orig, comp, offset, size, name):
    """Compare a segment and return (name, num_diffs)."""
    o = orig[offset:offset+size]
    c = comp[offset:offset+size] if offset+size <= len(comp) else b""
    if len(o) != len(c):
        return name, -1  # size mismatch
    diffs = sum(1 for a, b in zip(o, c) if a != b)
    return name, diffs


def diff_run(orig_path, comp_path):
    """Diff two .RUN files section by section. Returns dict of results."""
    result = {"file": orig_path.name, "status": "OK", "details": {}}

    if not comp_path.exists():
        result["status"] = "MISSING"
        return result

    orig = orig_path.read_bytes()
    comp = comp_path.read_bytes()

    if len(orig) != len(comp):
        result["status"] = "SIZE"
        result["details"]["size"] = f"{len(orig)} vs {len(comp)} ({len(comp)-len(orig):+d})"
        return result

    if orig == comp:
        result["status"] = "IDENTICAL"
        return result

    # Parse header to find segment boundaries
    try:
        code_size = struct.unpack_from("<I", orig, 0)[0]
        const_size = struct.unpack_from("<I", orig, 4)[0]
        spec_size = struct.unpack_from("<I", orig, 8)[0]
        label_size = struct.unpack_from("<I", orig, 12)[0]
        fld_name_size = struct.unpack_from("<I", orig, 32)[0]

        hdr = 128
        buf = 1600
        code_off = hdr + buf
        const_off = code_off + code_size
        spec_off = const_off + const_size
        label_off = spec_off + spec_size
        field_off = label_off + label_size

        sections = [
            ("header", 0, hdr),
            ("buffers", hdr, buf),
            ("code", code_off, code_size),
            ("constants", const_off, const_size),
            ("spec", spec_off, spec_size),
            ("labels", label_off, label_size),
            ("fields", field_off, fld_name_size),
        ]

        diffs = {}
        total_diffs = 0
        for name, off, sz in sections:
            _, nd = diff_section(orig, comp, off, sz, name)
            if nd != 0:
                diffs[name] = nd
                if nd > 0:
                    total_diffs += nd

        if diffs:
            result["status"] = "DIFF"
            result["details"] = diffs
            result["total_diffs"] = total_diffs
        else:
            result["status"] = "IDENTICAL"

    except Exception as e:
        result["status"] = "ERROR"
        result["details"]["error"] = str(e)[:100]

    return result


def diff_all():
    """Diff all original .RUN vs compiled .RUN and report."""
    run_files = sorted(BLANK_DIR.glob("*.RUN"))
    print(f"\nDiffing {len(run_files)} files: blank/*.RUN vs test/run/*.RUN\n")

    identical = []
    size_mismatch = []
    byte_diffs = []
    missing = []
    errors = []

    for rf in run_files:
        comp = RUN_DIR / rf.name
        r = diff_run(rf, comp)

        if r["status"] == "IDENTICAL":
            identical.append(r)
        elif r["status"] == "MISSING":
            missing.append(r)
        elif r["status"] == "SIZE":
            size_mismatch.append(r)
        elif r["status"] == "DIFF":
            byte_diffs.append(r)
        else:
            errors.append(r)

    # Summary
    total = len(run_files)
    print("=" * 70)
    print(f"ROUND-TRIP RESULTS: {total} files")
    print("=" * 70)
    print(f"  IDENTICAL:      {len(identical):4d}  ({100*len(identical)/total:.1f}%)")
    print(f"  BYTE DIFFS:     {len(byte_diffs):4d}")
    print(f"  SIZE MISMATCH:  {len(size_mismatch):4d}")
    print(f"  MISSING:        {len(missing):4d}")
    print(f"  ERRORS:         {len(errors):4d}")
    print()

    # Byte diff details (sorted by total diffs ascending — closest to matching first)
    if byte_diffs:
        byte_diffs.sort(key=lambda r: r.get("total_diffs", 999999))
        print(f"--- BYTE DIFFS ({len(byte_diffs)} files) ---")
        for r in byte_diffs[:50]:
            sections = ", ".join(f"{k}:{v}" for k, v in r["details"].items())
            print(f"  {r['file']:25s}  {r.get('total_diffs', '?'):>5} bytes  [{sections}]")
        if len(byte_diffs) > 50:
            print(f"  ... and {len(byte_diffs)-50} more")
        print()

    # Size mismatches
    if size_mismatch:
        print(f"--- SIZE MISMATCH ({len(size_mismatch)} files) ---")
        for r in size_mismatch[:30]:
            print(f"  {r['file']:25s}  {r['details'].get('size', '?')}")
        if len(size_mismatch) > 30:
            print(f"  ... and {len(size_mismatch)-30} more")
        print()

    # Missing
    if missing:
        print(f"--- MISSING ({len(missing)} files, compile failed) ---")
        for r in missing[:20]:
            print(f"  {r['file']}")
        if len(missing) > 20:
            print(f"  ... and {len(missing)-20} more")
        print()

    # Errors
    if errors:
        print(f"--- ERRORS ({len(errors)} files) ---")
        for r in errors:
            print(f"  {r['file']}: {r['details']}")

    return len(identical), total


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Batch round-trip test")
    parser.add_argument("--decompile-only", action="store_true")
    parser.add_argument("--compile-only", action="store_true")
    parser.add_argument("--diff-only", action="store_true")
    args = parser.parse_args()

    if args.diff_only:
        diff_all()
    elif args.decompile_only:
        decompile_all()
    elif args.compile_only:
        compile_all()
    else:
        decompile_all()
        compile_all()
        diff_all()
