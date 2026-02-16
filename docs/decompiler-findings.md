# TAS .RUN Decompiler — Findings & Analysis

## Overview

Analysis of decompiling all 583 .RUN files in `blank/` revealed systematic issues in the bytecode-to-source decompiler. This document catalogs the findings, fixes applied, remaining issues, and their impact on other CoTAS subsystems (parser, AST, interpreter, compiler).

---

## .RUN Binary Format Recap

```
Offset    Segment          Size
0x0000    Header           128 bytes (TPci record)
0x0080    Buffer List      1600 bytes (100 x 16)
0x06C0    Code             CodeSize bytes (instructions x 7 for TAS32, x 8 for TASWN)
          Constants        ConstSize bytes
          Spec             SpecSize bytes
          Labels           LabelSize bytes (NumLabels x 4)
          Field Specs      FldNameSize bytes (NumFlds x 48 or x 60)
```

Each TAS32 instruction: `uint16 CmdNum + byte SLSize + int32 SLPtr` = 7 bytes.
Each TASWN instruction: `uint16 CmdNum + byte Exit + byte SLSize + int32 SLPtr` = 8 bytes.

Spec parameters (TSpecLinePtr): `byte Type + int32 Location` = 5 bytes.

---

## Progress Summary

| Metric | Initial | After Session 1 | After Session 2 | After Session 3 | Change |
|--------|---------|-----------------|-----------------|-----------------|--------|
| Files with unresolved refs | 122 / 582 (21%) | 100 / 582 (17%) | 78 / 583 (13%) | 23 / 583 (4%) | -99 files |
| Total unresolved lines | 3,068 | 2,863 | 2,772 | 69 | -2,999 lines |
| Fully resolved files | 460 (79%) | 482 (83%) | 505 (87%) | 560 (96%) | +100 files |

---

## Findings — Session 1 (Expression Decoder)

### 1. UDF Call OpTypes (0x0C-0x1F)

**Problem**: The expression RPN bytecode uses multiple opType bytes for UDF calls, not just 0x0C:

| OpType | Occurrences | Description |
|--------|-------------|-------------|
| 0x0C   | standard    | Standard UDF call |
| 0x0D   | 530         | UDF variant |
| 0x0E   | 243         | UDF variant |
| 0x0F   | 13          | UDF variant |
| 0x11   | 16          | UDF variant |
| 0x14   | 9           | UDF variant |

**Fix**: Split handler — 0x0A/0x0B remain as 13-byte standalone comparisons; 0x0C-0x1F are UDF calls.

### 2. Standalone Comparison Temp Resolution

**Problem**: OpTypes 0x0A/0x0B use raw field byte offsets, not typed TSpecLinePtr params.

**Fix**: Added `ResolveFieldOffset()` fallback: `offset / 48 = field index`.

### 3. Lowercase 'x' Spec Param Type

**Problem**: CASE values use lowercase 'x' (0x78) instead of uppercase 'X' (0x58). 566+ lines failed.

**Fix**: Added 'x' as alias for 'X' in all relevant handlers.

### 4. Array Access Variants (0xB4-0xB6)

**Problem**: Array access has three variants: 0xB4, 0xB5, 0xB6. All same 14-byte format.

**Fix**: Added 0xB5 and 0xB6 alongside 0xB4.

---

## Findings — Session 2 (Command-Specific Decompilers)

### 5. DecompileGenericParams Was Wrong for Most Commands

**Problem**: ~60 commands used `DecompileGenericParams` which scans at fixed 5-byte intervals. But most commands have MIXED layouts — some bytes are 5-byte TSpecLinePtr params, some are 1-byte flags (Y/N), some are 1-byte chars, some are 4-byte label numbers. When a 1-byte flag like 'Y' (0x59) appears at an offset where the scanner expects a type byte, it matches as a valid param type, causing misalignment and cascading `?0x` errors.

**Fix**: Created specific decompilers for ALL commands using exact layouts from `specline.pas` (TAS 6.0 Delphi source). Eliminated ALL `DecompileGenericParams` calls from the dispatch table.

### 6. SafeParam Pattern

**Problem**: Even with correct decompilers, optional parameter slots sometimes contain non-zero garbage that resembles valid type bytes.

**Fix**: Created `SafeParam()` helper that validates the type byte against `_validParamTypes` before resolving. Applied to all optional parameters across all decompilers.

### 7. MENU vs NMENU Layout Confusion

**Problem**: Old-style MENU (opcode 14) and new-style NMENU (opcode 27) have completely different spec layouts. MENU uses an 89-byte "menuo" window-like layout; NMENU uses a 139-byte layout with flag bytes.

**Fix**: Split into `DecompileMenuOld()` (89B) and `DecompileMenu()` (139B).

### 8. Parameter Order Errors

**Problem**: Several commands had wrong param order based on initial guesses:
- SAY: was `col, row, fld` → actually `fld, col, row`
- DISPF: was `col, row, fld` → actually `fld, col, row`

**Fix**: Corrected using specline.pas constants (`say_fld_typ=0`, `say_col_typ=5`, `say_row_typ=10`).

### 9. UDC/FUNC/CMD with SLSize=0

**Problem**: In TAS 5.1 (.RUN files), many UDC/FUNC/CMD instructions have `SLSize=0` (no spec bytes). The label target must be encoded via `SLPtr` directly, not in spec data.

**Status**: Partially fixed — zero-SLSize instructions now try to use SLPtr as label number, but many still show `LABEL?` (out of range). Further investigation needed.

---

## Critical Remaining Issue: Spec Pointer Overflow

### Description

For 78 .RUN files, instruction `SLPtr` values exceed `SpecSize` (the header's declared spec segment size). This causes `GetSpecBytes()` to return empty arrays, making the instruction's spec data unreadable.

### Evidence

| File | SpecSize | Max SLPtr+SLSize | Overflow |
|------|----------|-------------------|----------|
| ADDSUM1.RUN | 5,734 | 5,734 | 0 (works) |
| BKARC.RUN | 16,935 | 33,802 | 16,867 |

- ADDSUM1: All spec pointers fit within declared SpecSize ✓
- BKARC: Spec pointers overflow by 16,867 bytes into label/field segments ✗

### Hypothesis: TAS 5.1 Spec Segment Actually Extends Beyond Header SpecSize

In the 78 broken files, `SLPtr` values point past the spec segment boundary into the label and field spec regions. The data at those absolute file offsets may still be valid spec data — the TAS runtime might use a combined segment or have a different interpretation of `SpecSize`.

**Investigation needed**:
1. Check the TAS 5.1 DOS runtime source (Pascal, not Delphi) for how it loads and addresses spec data
2. Compare `SpecSize + LabelSize + FldNameSize` against max `SLPtr+SLSize` for broken files
3. Check if the combined size equals the total of all segments after code+constants

### Impact

This is the **root cause** of most remaining 2,772 unresolved lines. Fixing spec pointer resolution would likely eliminate the vast majority of remaining `?0x` references.

---

## Commands Mapped (All from specline.pas)

All commands now have specific decompilers. Complete list:

### Control Flow
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| IF | 59 | jump(4)+pad(1)+lhs(5)+rhs(5)+variant(1)+label(4) |
| ELSE | 66 | jump(4) |
| WHILE | 67 | jump(4)+expr(5) |
| FOR | 72 | jump(4)+stop(5)+step(5)+counter(5)+start(5)+dir(1) |
| SELECT | 68 | selector(5) |
| CASE | 147 | jump(4)+value(5) |
| GOTO | 22 | label(4) |
| GOSUB | 21 | label(4) |
| RET | 32 | retval(5) optional |

### Assignment & Math
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| ASSIGN | 15 | target(5)+value(5) |
| INC | 39 | field(5)+amount(5) |
| DEC | 40 | field(5)+amount(5) |

### Display
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| SAY | 1 | fld(5)+col(5)+row(5)+color(5)+pict(5) |
| MSG | 106 | text(5)+nowait(1) |
| PMSG | ? | fld(5)+col(5)+row(5)+wait(1)+ncr(1)+ent(5)+color(5)+abs(1) |
| DISPF | 65 | fld(5)+col(5)+row(5)+color(5)+pict(5) |
| CLRLNE | 62 | col(5)+row(5)+chrs(5)+nocolor(1)+color(5)+abs(1) |
| CURSOR | 5 | start(5)+stop(5)+on_off(1)+wait(1)+dflt(1) |
| CLRSCR | 2 | (none) |
| SCRN | 37 | action(1) char S/R/L/U/E/D |
| PAINT | 135 | fld(5)+col(5)+row(5)+color(5) |
| ROW_COLOR | 186 | row(5)+color(5)+ncols(5) |
| COLOR | 56 | fgbg(5) |

### File I/O
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| OPENV | 64 | filename(5)+ext(5)+lock(1)+mode(1)+handle(5)+create(1)+... 53B total |
| FINDV | 26 | handle(5)+key(5)+val(5)+type(1)+err_lbl(4)+nlock(1)+... 33B |
| CLOSE | 63 | handle(5)+delete(1) |
| SAVE | 49 | handle(5)+nocnf(1)+noclr(1)+nosave_lbl(4)+err_lbl(4)+unlock(1) |
| CLR | 48 | handle(5) |
| DEL | 109 | handle(5)+nocnf(1)+goto(4)+err(4)+noclr(1) |
| DALL | 110 | handle(5)+nocnf(1)+noclr(1) |
| ROPEN | 50 | handle(5) |
| OPNO | 184 | filename(5)+type@30(1B char)+rsize@36(5B)+bufname@41(5B) 48B |
| SETACT | 116 | schema(5)+file(5) |
| SRCH | 38 | filename(5)+key(5) |
| RCN | 92 | field(5) |
| INIFLE | 30 | handle(5) |
| READ/WRITE | 43/44 | handle(5)+key(5) |
| RDREC/WTREC | 188/189 | handle(5)+key(5) |

### Window/UI
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| WINDOW | 57 | 81-byte layout: col/row/len/wdt + colors + title + box + shadow |
| BUTTON | 149 | 58-byte layout with many flag bytes |
| MENU (old) | 14 | 89-byte menuo layout |
| NMENU (new) | 27 | 139-byte layout |
| LISTF/LISTM | 160/161 | Complex layout with many keyword params |
| LIST | 162 | Memory list with field refs |
| ENTER | 58 | Complex interactive entry layout |
| ASK | 25 | type(5)+col(5)+row(5)+... many params |
| SCROLL | 35 | col(5)+row(5)+len(5)+wdt(5)+dir(1) |
| HOT_SPOT | 187 | row(5)+col(5)+color(5) |
| CAPTION | 189 | text(5)+dontchange(1) |

### Array Operations
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| SORTA | 70 | handle(5)+key(5)+order(1) |
| UPDTA | 41 | handle(5) |
| RDA | 111 | handle(5)+key(5)+start(5)+scope(5)+sval(5) |
| WRTA | 112 | handle(5)+key(5)+start(5)+scope(5) |

### Print
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| PSET | 46 | wdt(5)+tlnes(5)+plnes(5)+pwhr(5)+rtm(5)+tag(5)+bookmarks(5) 35B |
| PFMT | 165 | fld(5)+just(5)+pict(5) |
| PBOX | 163 | col(5)+row(5)+len(5)+wdt(5)+type(1) |
| PCHR | 164 | char(5)+times(5) |
| PON | 170 | param(5) |
| FORMAT | 52 | target(5)+value(5)+commas(1)+flt_dol(1)+off(1)+pict(5)+nozero(1) |

### Program Control
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| CHAIN | 28 | name(5) |
| RAP | 45 | name(5)+num(5)+in_mem(1)+with(5)+no_base_wind(1)+new_runtime(1)+no_delete(1)+no_save(1) |
| RUN | 47 | name(5)+tail(5) |
| EXEC | 76 | name(5) |
| UDC/FUNC/CMD | 75/73/74 | label(4)+flist(5) |
| PARAM | 91 | field_list(5) |

### Other
| Command | Opcode | Spec Layout |
|---------|--------|-------------|
| TRAP | 55 | key(5)+action(5) |
| XTRAP | 108 | key(5)+action(5) |
| FILL | 53 | fld(5)+char(5)+times(5) |
| XFER | 54 | to(5)+from(5)+numchr(5)+fmem(5)+tmem(5) |
| FILTER | 143 | expression(5) |
| FORCE | 144 | Y/N flag (1B) |
| FORCE3 | 145 | Y/N flag (1B) |
| SOUND | 34 | freq(5)+dur(5) |
| TRACE | 36 | on_off(5) |
| CLOCK | 60 | col(5)+row(5) |
| WRAP | 31 | handle(5)+col(5)+dlnes(5) |
| GETLBL | 33 | label(5)+line(5) |
| MOUNT | 29 | path(5)+type(5) |
| OWNER | 90 | name(5)+file(5)+action(1) |
| EXPORT/IMPORT | 171/172 | handle(5)+file(5)+delim(5)+format(1) |
| WCOLOR | 185 | color_name(5)+color_val(5) |
| EQU_MID | 183 | recv(5)+fld(5)+start(5)+nchr(5)+mem(5) |
| EQU_DAY | 182 | recv(5)+fld(5) |
| EQU_XMT | 181 | recv(5)+fld(5) |
| POSTMSG | ? | routine(5)+form(5) |
| TRANSX | ? | params vary |

---

## Expression Operator Byte Map

| Byte | Operator | Context |
|------|----------|---------|
| 0x01 | `+` | Addition |
| 0x02 | `-` | Subtraction |
| 0x03 | `+` | String concatenation |
| 0x04 | `*` | Multiplication |
| 0x05 | `/` | Division |
| 0x06 | `^` | Exponentiation |
| 0x07 | `=` | Equal |
| 0x08 | `<` | Less than |
| 0x09 | `>` | Greater than |
| 0x0A | `<>` | Not equal (standalone 13-byte op) |
| 0x0B | `>=` | Greater/equal (standalone 13-byte op) |
| 0x0C | `<=` | In binary ops; UDF call as opType |
| 0x0D | `AND` | In binary ops; UDF call as opType |
| 0x0E | `OR` | In binary ops; UDF call as opType |
| 0x0F | `NOT` | In binary ops; UDF call as opType |

---

## TPci Header (from General.pas)

```pascal
TPci = packed record
  CodeSize: longint;      // offset 0: run code segment size
  ConstSize: longint;     // offset 4: constant segment size
  SpecSize: longint;      // offset 8: spec segment size
  LabelSize: longint;     // offset 12: label segment size
  ScrnFldNum: longint;    // offset 16: number of screen fields
  NumFlds: longint;       // offset 20: total fields
  TempFlds: longint;      // offset 24: temp field end offset
  NumTempFlds: longint;   // offset 28: number of temp fields
  FldNameSize: longint;   // offset 32: field spec segment size
  TempFldSize: longint;   // offset 36: temp field data size
  DefFldSegSize: longint; // offset 40: defined field data seg size
  NumExtraFlds: longint;  // offset 44: extra runtime field slots
  PrgNames: longint;      // offset 48: program name buffer size
  DebugFlg: boolean;      // offset 52: debug mode
  ProType: array[0..4] of char; // offset 53: "TAS32" or "TASWN"
  NumLabels: longint;     // offset 58: number of labels
  NewFldSpec: boolean;    // offset 62: new 60-byte field spec format
  ChkUpVld: boolean;      // offset 63: check up-arrow validation
  IncLabels: boolean;     // offset 64: include label names in run file
  SerialNum1: longint;    // offset 65: SpecSize+SerialNum (checksum)
  PscLevel: longint;      // offset 69: ConstSize+PscLevel+SerialNum
  SerialNum2: longint;    // offset 73: CodeSize+SerialNum
  TAS30Prg: boolean;      // offset 77: TAS 3.0 compatibility flag
  ObjUsed: boolean;       // offset 78: .OVL overlay used
```

---

## Findings — Session 3 (Overlay Support + CASE Fix)

### 10. ADV50.OVL Overlay — Root Cause of Spec Pointer Overflow (SOLVED)

**Problem**: 441 out of 583 .RUN files had `SLPtr` values exceeding `SpecSize` by exactly **16,867 bytes**. This was the root cause of 2,700+ unresolved lines.

**Root cause**: `ADV50.OVL` (61,700 bytes) is a shared overlay file in TAS32 .RUN format. Its `SpecSize = 16,867`. When the TAS runtime loads a program, the overlay's segments (spec, constants, fields, labels) are prepended to the program's own segments. All `SLPtr` values in the program then index into the COMBINED spec segment: bytes 0–16,866 = overlay spec, bytes 16,867+ = program's own spec.

**Evidence**:
- All 441 overflowing files had identical overflow of exactly 16,867 bytes
- ADV50.OVL has `SpecSize=16,867`, `ConstSize=19,583`, `NumFlds=386`, `NumLabels=60`
- After prepending OVL spec, 100% of SLPtr values in BKARC.RUN fit (1055/1055 OK, 0 bad)
- TPci header at offset 78 has `ObjUsed: boolean` flag — confirms overlay awareness

**Fix**: Added `RunFileReader.LoadAutoOverlay()` that:
1. Checks if any instruction's SLPtr exceeds SpecSize
2. If so, looks for ADV50.OVL in same directory
3. Loads it and prepends its spec, constant, field, and label segments via `PrependOverlay()`
4. The CLI `--decompile` command now uses `LoadAutoOverlay()`

**Impact**: Reduced unresolved from 2,772 lines (78 files) to ~100 lines (~30 files). Combined with CASE fix below, final result: 69 lines in 23 files.

### 11. CASE Spec Layout Was Wrong (FIXED)

**Problem**: `DecompileCase()` read the case value as a 5-byte TSpecLinePtr at offset 0, but CASE uses the IF-style spec layout:
- offset 0: `if_goto` (4B jump target)
- offset 6: `if_endif_goto` (4B)
- offset 10: `if_typ` (1B type indicator)
- offset 11: `if_loc` (4B case comparison value)

The runtime does `GetSLLoc(if_loc)` — reads raw 4-byte int at offset 11.

**Fix**: Changed `DecompileCase` to read type at offset 10, location at offset 11. Falls back to raw integer if type is unrecognized.

**Impact**: Eliminated ~220 CASE-related unresolved references.

---

## Remaining Work

### Low Priority — Remaining 69 Unresolved Lines in 23 Files
Most are in self-contained files (don't need OVL) and involve:
- Expression decoding edge cases (garbled ASSIGN expressions in BKINIPRG, CDATACT2, etc.)
- Rare spec param types ('A'=0x41, '_'=0x5F) in MENU title/box params
- A few IF expressions with unrecognized type bytes (0x35, 0x01)

These are cosmetic issues — the decompiler produces readable output for 96% of files.

### Medium Priority
4. **Label name resolution**: Replace `LABEL_N` with actual label names from source.
5. **Missing function numbers**: Some functions appear as `fn{hex}` when not in the dictionary.

### Future Work
6. **Round-trip validation**: Compile decompiled .SRC back to .RUN and byte-diff.
7. **Screen definition decompilation**: Screen specs at end of .SRC files.
8. **Pretty-printing**: Indentation, line wrapping for readability.

---

## Files Reference

| File | Purpose |
|------|---------|
| `src/CoTAS.Parser/RunFile/ExpressionDecoder.cs` | RPN bytecode → infix expression strings |
| `src/CoTAS.Parser/RunFile/SpecDecoder.cs` | Spec param type resolution (F/C/N/X/Y/M/s/q) |
| `src/CoTAS.Parser/RunFile/RunFileDecompiler.cs` | Main decompiler with command-specific handlers |
| `src/CoTAS.Parser/RunFile/RunFileReader.cs` | .RUN binary reader (header, segments, fields) |
| `src/CoTAS.Parser/RunFile/RunFileHeader.cs` | Header structure (128 bytes) |
| `src/CoTAS.Parser/RunFile/TasOpcode.cs` | Opcode number ↔ name mapping |
| `specline.pas` | TAS 6.0 compiler — definitive command spec layouts |
| `cmpspec.pas` / `cmpspec2.pas` | TAS 6.0 compiler — command compile routines |
| `General.pas` | TAS 6.0 — TPci header record definition |
