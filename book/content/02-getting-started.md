## Prerequisites

- **.NET 10 SDK** or later
- **SQL Server** (for data file access - optional for parse-only or programs without file I/O)
- A modern web browser (Chrome, Firefox, Edge) for the Web UI

## Installation

Clone the repository and restore dependencies:

```bash
git clone https://github.com/your-org/cotas.git
cd cotas
make restore
make build
```

## Running Your First Program

### Web Mode (Default)

Launch the full web interface:

```bash
make run
```

Open your browser to `http://localhost:5000`. You'll see the CoTAS web terminal - an 80×25 character-mode display with a program sidebar on the left. Click any `.SRC` file to run it in the browser.

<div class="callout tip">
<div class="callout-title">💡 Hot Reload</div>
Use <code>make dev</code> for development mode - the server automatically restarts when you modify .NET source files via <code>dotnet watch</code>.
</div>

### CLI Mode

Execute a single TAS program from the command line:

```bash
make cli SRC=user_code/BKTEST.SRC
```

This runs the program in your terminal using the `ConsoleBridge`.

### CLI Tool - Full Options

The `CoTAS.Cli` project is a Swiss-army knife for TAS programs:

| Command | Description |
|---------|-------------|
| `CoTAS.Cli <file.SRC>` | Parse and execute a program |
| `CoTAS.Cli <file.SRC> --parse-only` | Parse without executing (validate syntax) |
| `CoTAS.Cli --compile <file.SRC> [-o out.RUN]` | Compile source to `.RUN` bytecode |
| `CoTAS.Cli --decompile <file.RUN>` | Decompile `.RUN` back to pseudo-source |
| `CoTAS.Cli --read-run <file.RUN> [--verbose]` | Inspect `.RUN` file (header, fields, labels, instructions) |
| `CoTAS.Cli --roundtrip <file.RUN>` | Read → write → compare bytes (validates compiler fidelity) |
| `CoTAS.Cli --batch-parse <dir>` | Parse all `.SRC` files in a directory (with 5-second timeout each) |
| `CoTAS.Cli --batch-roundtrip <dir>` | Round-trip test all `.RUN` files in a directory |

<div class="callout new">
<div class="callout-title">⚡ Round-Trip Testing</div>
The <code>--roundtrip</code> flag validates that CoTAS can read a compiled <code>.RUN</code> file and write it back <strong>byte-for-byte identical</strong>. This proves the compiler and decompiler are perfect inverses - critical for trusting the legacy bytecode path.
</div>

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 2 | Lexer or parser error |
| 3 | Interpreter error |
| 4 | Storage error |

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `COTAS_CONNECTION_STRING` | MSSQL connection string (overrides appsettings.json) |
| `COTAS_DDF_DIR` | Path to DDF directory (overrides appsettings.json) |

## The Web UI

The CoTAS web interface is a **browser-based terminal emulator** built with Bootstrap 5.3, vanilla JavaScript, and SignalR. It faithfully reproduces the TAS 80×25 character-mode display:

- **Terminal Grid** - 25 rows × 80 columns, rendered as DOM `<span>` elements (not canvas), Consolas monospace font at 14px, classic green-on-black CRT aesthetic
- **Program Sidebar** - 260px panel listing all `.SRC` files from `user_code/`, with real-time case-insensitive filtering
- **Input Handling** - `ENTER` fields appear as positioned text overlays at the exact row/col; `ASK` prompts appear as modal Y/N dialogs
- **Status Indicator** - Navbar badge pulses green when running, gray when idle, yellow when reconnecting
- **Message Log** - 120px scrollable panel showing timestamped system events
- **Toolbar** - Stop button (cancels running program), Clear button (clears terminal)

### SignalR Protocol

The hub endpoint is `/hub/interpreter`. Each connection gets an isolated session:

**Browser → Server:**

| Method | Parameters | Purpose |
|--------|-----------|---------|
| `GetPrograms()` | - | List available `.SRC` files |
| `RunProgram(fileName)` | string | Load, parse, execute in background task |
| `StopProgram()` | - | Cancel via `CancellationTokenSource` |
| `SubmitInput(value)` | string | Complete pending `TaskCompletionSource` |
| `SendKeyPress(key)` | string | Trap key dispatch (F1-F10, ESC) |

**Server → Browser:**

| Event | Parameters | Purpose |
|-------|-----------|---------|
| `Say(text, row, col)` | string, int, int | Write text at screen position |
| `RequestInput(field, row, col, size)` | string, int, int, int | Show input field overlay |
| `RequestAsk(prompt, default)` | string, bool | Show Y/N modal dialog |
| `ClearScreen()` | - | Clear 25×80 buffer |
| `ProgramStarted(name)` | string | Update status to Running |
| `ProgramEnded(name)` | string | Update status to Idle |
| `Error(message)` | string | Display error (red text) |

Each browser tab runs an independent TAS session. The `InterpreterHub` maintains a `ConcurrentDictionary<connectionId, (WebBridge, CancellationTokenSource)>` - cleanup happens automatically on disconnect.

## Project Structure

```
cotas/
├── src/
│   ├── CoTAS.Parser/        # Lexer (113 token types), preprocessor,
│   │                        # recursive-descent parser, AST (21 statement
│   │                        # types, 6 expression types), .RUN decompiler
│   ├── CoTAS.Interpreter/   # Tree-walking engine, 127 command handlers,
│   │                        # 170+ functions, FieldManager, TrapManager
│   ├── CoTAS.Compiler/      # Two-pass compiler: AST → .RUN bytecode,
│   │                        # ExpressionEncoder (RPN), ConstantPool
│   ├── CoTAS.Storage/       # DdfParser (.int files), StorageEngine (MSSQL),
│   │                        # FileHandle, RecordBuffer, TableSchema
│   ├── CoTAS.Bridge/        # IUIBridge interface, ConsoleBridge
│   ├── CoTAS.Web/           # ASP.NET + SignalR hub, WebBridge,
│   │                        # 80×25 browser terminal (wwwroot/)
│   └── CoTAS.Cli/           # CLI runner: execute, compile, decompile,
│                            # round-trip, batch-parse
├── docs/                    # LLM-extracted reference (231 commands,
│                            # 169 functions, 267 error codes)
├── user_code/               # 300+ real TAS programs
├── db/                      # DDF files (GPACIFIC, JADVDATA schemas)
├── blank/                   # Legacy compiled .RUN/.OBJ/.OVL files
├── test/                    # Integration tests + decompiled samples
└── tests/                   # xUnit unit tests
```

## Execution Modes

| Mode | Bridge | How | Use Case |
|------|--------|-----|----------|
| **Web** | `WebBridge` | `make run` → browser at :5000 | Production - multi-user browser terminal |
| **CLI** | `ConsoleBridge` | `make cli SRC=file.SRC` | Single program execution from terminal |
| **Parse-Only** | - | `make parse SRC=file.SRC` | Syntax validation without execution |
| **Compile** | - | `--compile file.SRC` | Generate `.RUN` bytecode |
| **Decompile** | - | `--decompile file.RUN` | Reverse `.RUN` to pseudo-source |

All execution modes use the same parser, interpreter, and storage layer. The only difference is how screen output and user input are handled.
