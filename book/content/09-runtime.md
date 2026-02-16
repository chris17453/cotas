This chapter explains how CoTAS executes TAS programs - the parser pipeline, interpreter engine, bytecode compiler, storage layer, and multi-user session management. All seven .NET 10 projects are covered.

## Dual Execution Paths

CoTAS supports two ways to run your programs:

### Source-Direct Execution (Default)

```
.SRC → Lexer (113 tokens) → Preprocessor (#lib/#inc) → Parser → AST → Interpreter
```

This is the primary mode. CoTAS reads your `.SRC` source file, tokenizes it (113 token types), preprocesses directives (with circular include protection, max depth 20), parses it into an Abstract Syntax Tree (21 statement types, 6 expression types), and walks the tree to execute. No intermediate bytecode - like Python, execution happens directly from source.

### Legacy Bytecode Execution

```
.RUN → RunFileReader (128-byte header) → RunFileDecompiler → ExpressionDecoder → AST → Interpreter
```

For existing compiled `.RUN` files, CoTAS reads the binary format - 128-byte header, 7-byte instructions (TAS 5.1 `TAS32` signature) or 8-byte instructions (TAS 6.0 `TASWN` signature), constant pool, spec segments, field specs (48 or 60 bytes each), and label table - decompiles everything back into the same AST structure, and feeds it to the same interpreter.

<div class="callout new">
<div class="callout-title">⚡ Both Paths, Same Interpreter</div>
Whether you start from <code>.SRC</code> or <code>.RUN</code>, the result is the same AST fed to the same interpreter. This means every command, function, and feature works identically regardless of the input format.
</div>

## The Parser Pipeline (CoTAS.Parser)

### Stage 1: Lexical Analysis

The `Lexer` reads raw source text and produces a stream of tokens. It recognizes 113 distinct token types:

- **4 literal types**: `StringLiteral`, `IntegerLiteral`, `NumericLiteral`, `Identifier`
- **6 field definition keywords**: `Define`, `Type`, `Size`, `Dec`, `Array`, `Reset`
- **18 control flow keywords**: `If`, `Then`, `Else`, `ElseIf`, `EndIf`, `While`, `EndW`, `For`, `Next`, `Goto`, `Gosub`, `Ret`, `Select`, `Case`, `Otherwise`, `EndC`, `Scan`, `EndS`
- **12 loop control keywords**: `Exit`, `ExitIf`, `Loop`, `LoopIf`, `Floop`, `FloopIf`, `Fexit`, `FexitIf`, `Sloop`, `SloopIf`, `Sexit`, `SexitIf`
- **20 operators**: `+`, `-`, `*`, `/`, `=`, `<>`, `<`, `<=`, `>`, `>=`, `!`, `$`, `|`, `\`, `?`, `~`, `%`, `&`, `^`
- **3 logical operators**: `.AND.`/`.A.`, `.OR.`/`.O.`, `.NOT.`/`.N.`
- **Special tokens**: `Newline`, `Comment`, `Preprocessor`, `Label`, `Eof`

Special tokenization rules: `;` inside parentheses becomes a separator (for `FOR(I;1;10;1)` syntax); identifiers can contain `.` for `DICT.KEY` patterns; keywords are case-insensitive.

### Stage 2: Preprocessing

The `Preprocessor` resolves file inclusion directives before parsing:

| Directive | Behavior |
|-----------|----------|
| `#lib name` | Search current dir → library dirs for `.LIB` file, inline contents |
| `#inc name` | Search current dir → library dirs for `.SRC` file, inline contents |
| `#proc`/`#endp` | Pass through (procedure markers handled by interpreter) |
| `#udx`/`#udc` | Pass through (compiler flags) |
| `#psc` | Pass through |
| `#add_flds` | Pass through |

Features: Max include depth of 20, circular include detection by full path, case-insensitive file resolution (Linux compatibility), comment stripping from directive arguments.

### Stage 3: Recursive-Descent Parsing

The `TasParser` uses **recursive descent with Pratt-style expression parsing** to build the AST. It produces `TasProgram(List<Statement>)` containing 21 statement types and 6 expression types.

Expression precedence (5 levels): `.OR.` → `.AND.` → comparisons → `+`/`-` → `*`/`/`

The parser handles TAS quirks: `DO` keyword optional after `IF`, flexible `FOR` separators (`;` or `,`), keywords-as-identifiers, and `GenericCommandStmt` as a catch-all for the 127 commands that the parser doesn't need to understand structurally.

## The Interpreter Engine (CoTAS.Interpreter)

### Two-Pass Execution

The `TasInterpreter` executes programs in two passes:

**Pass 1 (Collection):** Scans all statements and collects:
- Labels → name-to-index mapping (case-insensitive)
- DEFINE statements → field definitions (processed at compile time)
- FUNC/CMD definitions → user-defined function/command registration

**Pass 2 (Execution):** Walks statements sequentially with a program counter (PC):
- `RunStatementAsync(Statement)` dispatches to the appropriate handler
- GOTO modifies PC and sets `_jumped = true`
- GOSUB pushes return address to call stack
- RET pops return address (at top-level, halts execution)
- `_breakRequested` / `_continueRequested` flags handle loop control

### The Command Registry - 127 Handlers

Commands are registered by name (case-insensitive) in `CommandRegistry`. When the interpreter encounters a `GenericCommandStmt`, it looks up the handler and calls `ExecuteAsync()`.

**By Category:**

| Category | Count | Examples |
|----------|-------|---------|
| Arithmetic & Field | 6 | INC, DEC, FILL, TRIM, UPCASE |
| I/O | 3 | ASK, ENTER, PMSG |
| Trap Handling | 4 | TRAP, PUSHT, POPT, XTRAP |
| Screen | 17 | WINDOW, PAINT, CURSOR, SCROLL, COLOR, BUTTON, HOTSPOT, SAVES, RSCR |
| Data & Field | 15 | FORMAT, PICTURE, JUSTIFY, REDEFINE, PUSHF, POPF, REPLACE, INSRT |
| Entry Mode | 8 | AUTODEC, AUTOENTER, AUTOINC, NOCLR, NOFD, NOCMA, NOZERO |
| Navigation | 5 | UPAR, DNAR, FEXIT, SEXIT, SLOOP |
| Printing | 12 | PON, POFF, TOF, RPTFMT, PRTALL, PRTO, PRTBOX |
| File I/O | 18 | OPENV, FINDV, SAVE, DEL, CLOSE, READ, WRITE, SRCH, FILTER, RELATE |
| Arrays | 7 | RDA, WRA, UDA, SORT, RMVA, DSPA, DLCA |
| System & Misc | 25 | CHAIN, RUN, TRACE, CLOCK, MENU, EXPORT, IMPORT, MOUNT, ERROR |
| **Total** | **127** | |

### The Expression Evaluator

`ExpressionEvaluator` recursively evaluates AST expression nodes:

- **Literals**: Return value directly
- **Identifiers**: Look up in `FieldManager`
- **Array Access**: Get element by 1-based index
- **Binary Operations**: Apply operator with automatic type coercion
- **Unary Operations**: Negation or `.NOT.`
- **Function Calls**: Dispatch to registered function (170+ built-ins) or UDF callback

String concatenation (`+` with an Alpha operand), case-insensitive string comparison, and automatic numeric↔string coercion are handled transparently.

### Built-In Functions - 170+

Functions are organized into modules that self-register with the `ExpressionEvaluator`:

| Module | Count | Highlights |
|--------|-------|-----------|
| Inline (ExpressionEvaluator) | 17 | TRIM, UP, LOW, STR, VAL, MID, LEN, CHR, ASC, ABS, INT, CLNUM, CINT, CFLT, LTOC, CTOL, time conversions |
| StringFunctions | 21 | LOC, SEG, NULL, SNDX, IIF, ISNUM, ISAL, REPLACE, INSRT, HEX, EDIT, FILL, WRAP |
| MathFunctions | 20 | ROUND, SQRT, MOD, SIGN, CEIL, FLOOR, RNDM, PI, SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, EXP, LOG, LOG10, DTOR, RTOD |
| DateTimeFunctions | 15 | DATE, TIME, MNTH, YEAR, DOM, DOW, CDOW, CMNTH, DTOC, CTOD, DTOS, DIFF, MDY |
| SystemFunctions | 17 | GETENV, OS (returns "COTAS"), VER (returns "2.0"), PRGNME, PRGLNE, ESC, ENTER, INKEY, EXEC, AVAIL, MEM, CO, PROP |
| FieldInfoFunctions | 30 | SIZE, ALC_FLD, ALOC, ALOCARY, GET_ELEM_NUM, AEV, LIKE, MIN, MAX, DMY, COPY_FILE, MAKE_DIR, RETVAL |
| FileFunctions | 32 | EOF, BOF, FLERR, FNUM, RCN, CREC, RSIZE, FTYP, FLDNME, NUMFLDS, LCKD, FFILE, FARRAY, GFL, TEST, PROP, CPATH, DPATH |
| ScreenFunctions | 18 | ROW, COL, LROW, MCOL, MROW, PCOL, PROW, SCRCHR, ISCLR, WINDOWS, MOUSE_ACT |
| **Total** | **170+** | |

### The Field Manager

`FieldManager` stores all field values and metadata:

- **Scalar fields**: Case-insensitive dictionary
- **Arrays**: Separate dictionary, 1-based indexing
- **Field stack**: `PUSHF`/`POPF` for nested scopes (UDF parameter save/restore)
- **Screen buffer**: Internal 25×80 character grid for `SAVES`/`RSCR`
- **Metadata**: Per-field format, picture mask, justification (L/R/C)
- **System properties**: ProgramName, CursorRow/Col, PrinterRow/Col, LastKeyPressed, LastAskResult, TraceEnabled, and dozens more

### User-Defined Functions & Commands

CoTAS extends TAS 5.1 with full UDF/UDC support:

```tas
FUNC CALC_TAX(subtotal, rate)
  tax = subtotal * rate
  RET tax
#endp

CMD SHOW_HEADER(title)
  SAY 1,1 title
  SAY 2,1 "================================"
#endp
```

- **FUNC**: Callable from expressions, returns a value via `RET`
- **CMD**: Callable as a command (GOSUB-based), no return value
- **Parameters**: Fields with automatic save/restore
- **Nesting**: Call depth tracked, parameters preserved across nested calls
- **Return values**: `_lastReturnValue` accessible via `RETVAL()` function

## The Bytecode Compiler (CoTAS.Compiler)

CoTAS includes a full two-pass compiler that produces `.RUN` files:

### Compilation Pipeline

1. **Pass 1 (Collect)**: Scan for DEFINE, LABEL, forward references
2. **Pass 2 (Emit)**: Generate bytecode instructions
3. **BuildRunFile()**: Assemble segments into binary

### .RUN Binary Format

```
Offset    Content
────────────────────────────────────
0-127     Header (128 bytes)
            Code/Const/Spec/Label sizes
            Field counts, ProType ("TAS32")
128-1727  Buffer List (100 × 16 bytes)
1728+     Code Segment (7 bytes/instruction)
            [opcode:2][spec_size:1][spec_ptr:4]
...       Constant Segment (deduped pool)
...       Spec Segment (5-byte param entries)
...       Label Segment (4 bytes each)
...       Field Specs (48 bytes each for TAS51)
            [name:20][offset:4][type:1][dec:1]
            [size:2][array:2][internal:2]...
```

### Expression Encoder (RPN)

The `ExpressionEncoder` compiles expression ASTs to Reverse Polish Notation bytecode stored in the constant segment:

- Binary operations: 16-byte entries (opcode + lhs + rhs + result)
- Function calls: Variable-length (180+ built-in function numbers mapped)
- Array access: 14-byte entries (base + index + result)
- Constants: Type-tagged entries in the constant pool with deduplication

### Round-Trip Testing

The compiler supports byte-identical round-trips:

```bash
CoTAS.Cli --roundtrip BKTEST.RUN    # Read → write → compare
CoTAS.Cli --batch-roundtrip blank/   # Test all .RUN files
```

## The Storage Layer (CoTAS.Storage)

### Components

| Class | Purpose |
|-------|---------|
| `StorageEngine` | Main database interface - FIND/SCAN/SAVE/DEL (MSSQL now; MySQL & PostgreSQL in progress) |
| `DdfParser` | Parse `.int` files → `TableSchema` |
| `TableSchema` | Record schema (fields, indexes, database) |
| `FieldDefinition` | Field metadata (name, NativeType, offset, length) |
| `FileHandle` | Runtime file state (buffer, EOF/BOF, lock mode) |
| `RecordBuffer` | In-memory record with dirty tracking |
| `StorageConfig` | Connection strings, DDF paths, database mapping |

### Initialization

At startup, `StorageEngine.PreloadAllDdfFiles()` scans the DDF directory for all `.int` files and parses them into `TableSchema` objects. If a TAS program opens a file with no `.int`, CoTAS falls back to `DiscoverSchemaFromSqlAsync()` which queries `INFORMATION_SCHEMA.COLUMNS`.

### SQL Generation

All SQL is parameterized to prevent injection:

| Operation | SQL Pattern |
|-----------|------------|
| Find First | `SELECT TOP 1 * FROM [schema].[table] ORDER BY (SELECT NULL)` |
| Find Match | `SELECT TOP 1 * FROM [schema].[table] WHERE [key] = @key0` |
| Find GE | `SELECT TOP 1 * FROM [schema].[table] WHERE [key] >= @key0 ORDER BY [key]` |
| Scan | `SELECT * FROM [schema].[table] WHERE [key] >= @key0 ORDER BY [key1], [key2]` |
| Insert | `INSERT INTO [schema].[table] ([cols]) VALUES (@p0, @p1, ...)` |
| Update | `UPDATE [schema].[table] SET [col] = @s0 WHERE [key] = @w0` |
| Delete | `DELETE FROM [schema].[table] WHERE [key] = @p0` |
| Auto-Create | `CREATE TABLE ... CREATE INDEX ...` (deferred, on first open) |

## Multi-User Sessions

In the web runtime, each SignalR connection gets its own completely isolated environment:

| Resource | Isolation |
|----------|-----------|
| TAS Interpreter | Independent instance |
| Field Table | Private field values and arrays |
| File Handles | Separate open files and record cursors |
| Call Stack | Private GOSUB/RETURN state |
| Trap Table | Independent key trap registrations |
| Screen Buffer | Dedicated 25×80 character grid |
| Cancellation | Per-session `CancellationTokenSource` |

Users cannot interfere with each other's program state. The only shared resource is the database itself, which uses native row-level locking for concurrent access. Session cleanup happens automatically on disconnect via `InterpreterHub.OnDisconnectedAsync()`.

<div class="callout new">
<div class="callout-title">⚡ Key Differences from TAS 5.1</div>
<table>
<thead><tr><th>Feature</th><th>TAS 5.1</th><th>CoTAS</th></tr></thead>
<tbody>
<tr><td>Execution</td><td>Synchronous, blocking</td><td>Async/await throughout</td></tr>
<tr><td>UI</td><td>Native DOS/Windows app</td><td>Browser terminal via SignalR</td></tr>
<tr><td>Storage</td><td>Btrieve ISAM files (paid drivers)</td><td>MSSQL, MySQL, PostgreSQL - open-source drivers, no licensing fees</td></tr>
<tr><td>Users</td><td>Single-user per process</td><td>Multi-user (one interpreter per connection)</td></tr>
<tr><td>UDFs</td><td>Limited</td><td>Full FUNC/CMD/RET with parameters</td></tr>
<tr><td>Commands</td><td>~100</td><td>127 implemented</td></tr>
<tr><td>Functions</td><td>~120</td><td>170+ implemented</td></tr>
<tr><td>OS() returns</td><td>"DOS" or "WIN"</td><td>"COTAS"</td></tr>
<tr><td>VER() returns</td><td>"5.1"</td><td>"2.0"</td></tr>
<tr><td>Loop protection</td><td>None</td><td>1M iteration limit</td></tr>
<tr><td>Platform</td><td>DOS/Win16</td><td>.NET 10, any OS</td></tr>
</tbody>
</table>
</div>

## Complete Opcode Map

The following table documents every opcode reverse-engineered from the TAS 5.1 bytecode format. These 228 opcodes represent the complete instruction set of the TAS virtual machine - decoded over 15 years of binary analysis.

**Standard Opcodes (0-220):**

| Range | Opcodes |
|-------|---------|
| 0-9 | NOP, PMSG, CLRSCR, CURSOR, BELL, -, CLR, CLRLNE, CLOSE, RDA |
| 10-19 | CLRSF, POSTMSG, DEL, WRTA, MENU, ASSIGN, DISPF, FILL, POINTER, FIND |
| 20-29 | SRCH, GOSUB, GOTO, SAY, INIFLE, UPDTA, FINDV, NMENU, MOUNT, XFER |
| 30-39 | ON, OPEN, RET, ENTER, PBLNK, PBOX, PCHR, PFMT, PON, BRKRET |
| 40-49 | PVERT, INC, MID_CMD, CO, REDEF, REDSP, WINDEF, WINACT, CHAIN, SAVES |
| 50-59 | SAVES, SCROLL, SORTA, PTOF, TRANSX, TRAP, ULKALL, WINDOW, REENT, IF |
| 60-69 | POPS, CLSO, PUT_FLD, SORT3, OPENV, REMVA, ELSE, WHILE, SELECT, ENDW |
| 70-79 | LOOP_IF, EXIT_IF, FOR, FUNC, CMD, UDC, RAP, CHAINR, CLSPF, LISTM |
| 80-89 | EXPORT, RSCR, ADD, LISTF, LIST, REPL, EXEC, QUIT, ERR, OWNER |
| 90-99 | DALL, ASK, RCN_CMD, REL, FORCE, DEC, PAINT, DEALOC, READ, WRITE |
| 100-109 | INSERT, SETLINE, REMOUNT, SCRN, RDLIST, -, MSG, CLRPE, UPAR, ALLOC |
| 110-119 | PSET, COLOR, IMPORT, PARAM, -, XTRAP, SETACT, BKG, FRG, REVERSE |
| 120-129 | FORMAT, DISPM, FILLMEM, DELC, GOTOL, DELF, RENF, DATE, TIME, PUSHF |
| 130-139 | POPF, ROPEN, FILTER, WRAP, REWRAP, UP, CLOCK, SCAN, SET_SCAN_FLG, TRIM |
| 140-149 | IFDUP, IFNA, PRTALL, NOREDSP, AUTOINC, PUSHT, POPT, CASE, PRT_NUM, LD_PDRV |
| 150-159 | GOSUBL, GETLBL, CDPATH, SOUND, TRACE, INT_CMD, LIST_EXIT, POKE, PEEK, KBDUP |
| 160-169 | NORSTRT, NOVLDMSG, AUTOENTER, AUTODEC, SHOW_PLINE, AUTO_RUN, RUN, MEM_PTR, MEM_SPC, SAVES3 |
| 170-179 | REDSP3, WRAP3, SSPCF, RDREC, WTREC, OPNO, EQU_MID, EQU_DAY, EQU_XMT, FORCE3 |
| 180-189 | MOUSE, PLAYWAV, COMM, JUST, PORT, START_SCAN, AUTONEW, WCOLOR, BUTTON, CAPTION |
| 190-220 | LOAD_PICTURE, GRAY, BRACE_OPEN, BRACE_CLOSE, ROW_COLOR, -, HOT_SPOT, ..., NEXT |

**Control Flow Opcodes (500+):**

| Opcode | Name | Purpose |
|--------|------|---------|
| 501 | DEFINE | Field definition |
| 502 | ENDIF | End IF block |
| 503 | ELSE_IF | Else-if branch |
| 504 | LOOP | Loop continue |
| 506 | OTHERWISE | SELECT default case |
| 507 | ENDC | End SELECT |
| 508 | EXIT_CMD | Exit UDC |
| 510 | FLOOP | FOR loop continue |
| 511 | FEXIT | FOR loop exit |
| 512 | FLOOP_IF | Conditional FOR continue |
| 513 | FEXIT_IF | Conditional FOR exit |
| 514 | SEXIT | SCAN exit |
| 515 | SEXIT_IF | Conditional SCAN exit |
| 516 | SLOOP | SCAN loop continue |
| 517 | SLOOP_IF | Conditional SCAN continue |
| 518 | ENDS | End SCAN |
