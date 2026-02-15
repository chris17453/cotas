# CoTAS — .NET Runtime Plan for TAS Professional 5.1

## Problem Statement

TAS Professional 5.1 is a legacy 4GL business application language with ~300+ real-world .SRC programs (accounting, inventory, sales orders, AR/AP/GL). It currently runs via a DOS/Windows compiler+runtime that produces .RUN bytecode files executed by the TAS runtime.

**Goal**: Build a new **64-bit .NET 8+ interpreter** that:
1. Reads and executes .SRC files directly (like Python — no compilation step)
2. Separates backend (interpreter + data) from frontend (React/Vite UI in browser)
3. Serves a React UI via WebSocket/SignalR so existing TAS programs render in a browser
4. Requires **zero changes** to existing TAS source code
5. Supports **multi-user** (each browser session = independent TAS program instance)
6. Uses **MSSQL** for data storage (already migrated from Btrieve), with a database adapter abstraction behind a connection interface so the backend can be swapped

## Guiding Principle: Faithful Execution First

**The existing TAS source code does not change. Ever.**

The interpreter must behave identically to the original TAS runtime. Every SCAN loops row-by-row. Every FIND walks the index. Every ENTER blocks until input. If the original runtime did it a certain way, we do it that way. Correctness and compatibility are non-negotiable.

Performance optimizations (e.g., pushing SCAN predicates down to SQL WHERE clauses) are a future concern — Phase 8 at the earliest, and only after the entire codebase runs correctly. We will never sacrifice behavioral fidelity for speed.

## Architecture Overview

```
┌──────────────────────┐
│   Browser (HTML/JS)  │  ← Renders character grid, captures keystrokes
│   Terminal-like UI   │
└────────┬─────────────┘
         │ WebSocket / SignalR
┌────────┴─────────────┐
│   ASP.NET Host       │  ← HTTP server, session management
│   Session Manager    │  ← One TAS interpreter instance per user session
└────────┬─────────────┘
         │
┌────────┴─────────────┐
│   UI Bridge Layer    │  ← Translates SAY/ENTER/MOUNT/WINDOW/COLOR into
│   (The Middleman)    │     JSON UI messages; receives input events back
└────────┬─────────────┘
         │
┌────────┴─────────────┐
│   TAS Interpreter    │  ← Parser + AST + tree-walking interpreter
│   (.SRC → Execute)   │     Executes TAS commands/functions line by line
└────────┬─────────────┘
         │
┌────────┴─────────────┐
│   Storage Layer      │  ← Database adapter (MSSQL default) behind
│   (IDbConnection)    │     connection interface; keyed access, locking
└──────────────────────┘
```

## Solution Components

### Layer 1: TAS Language Parser & AST

**What it does**: Reads .SRC files, tokenizes, parses into an Abstract Syntax Tree.

**Key challenges**:
- TAS has no formal grammar spec — must be reverse-engineered from the manual + 300 real programs
- `#lib` / `#inc` directives inline other files (preprocessor step)
- `#proc` / `#endp` define callable procedures
- Labels (`label_name:`) for GOTO/GOSUB targets
- Expressions mix field references, functions, constants, and operators
- Special syntax: `findv M fnum handle key field val expression`

**Sub-tasks**:
- [ ] Define formal grammar (EBNF or PEG) from docs + real code
- [ ] Lexer/tokenizer for TAS source
- [ ] Preprocessor (#lib, #inc, #udx, #proc/#endp expansion)
- [ ] Expression parser (arithmetic, string, logical, function calls)
- [ ] Command parser (each command has unique syntax — ~65 commands)
- [ ] AST node types for all constructs
- [ ] Unit tests against real .SRC files (parse without crash)

### Layer 2: TAS Interpreter Engine

**What it does**: Walks the AST and executes each node. Manages program state.

**Program State includes**:
- **Field table**: All defined fields with name, type, size, value, array dimensions
- **File table**: Open files (up to 99), their handles, dictionary metadata, current record buffers
- **Call stack**: GOSUB return addresses, CHAIN save/restore
- **Trap table**: Active key traps (ESC, INT, F1-F10) → label targets
- **Screen state**: Current cursor position, color settings, active window
- **Memory areas**: 4 binary work buffers (MEM areas 1-4)

**Key interpreter behaviors**:
- **Sequential execution** with GOTO/GOSUB/label flow control
- **IF/ELSE/ENDIF, WHILE/ENDW, CASE/SELECT/ENDCASE** structured flow
- **CHAIN**: Save current state, load + run another .SRC, restore on return
- **TRAP**: Register interrupt handlers; on key event, jump to label
- **ENTER**: **Blocks execution** until user provides input (async wait via bridge)
- **ASK**: Blocks for Y/N response

**Sub-tasks**:
- [ ] Interpreter core (AST walker, program counter, execution loop)
- [ ] Field/variable management (DEFINE, allocation, deallocation, arrays)
- [ ] Expression evaluator (type coercion, operator precedence)
- [ ] Flow control (IF/WHILE/CASE/GOTO/GOSUB/EXIT)
- [ ] Trap system (register/unregister key handlers)
- [ ] CHAIN implementation (load program, pass params, return)
- [ ] Error handling (runtime errors, TRAP routing)
- [ ] Implement all ~169 built-in functions (STR, VAL, MID, TRIM, EOF, etc.)
- [ ] Implement all ~65 commands (ENTER, SAY, OPEN, FIND, DELETE, etc.)

### Layer 3: Storage Engine (MSSQL via Database Adapter)

**What it does**: Provides TAS-compatible file/record/index operations on top of a relational database. Uses MSSQL as the default backend, but all access goes through an adapter interface so the database can be swapped.

**Design**: Each TAS "data file" maps to an existing MSSQL table (already migrated from Btrieve via SQLBtr). Schema definitions live in DDF files (one per table, in a `db/` directory) that were generated by the SQLBtr middleware. All database access flows through an `IStorageAdapter` that takes an `IDbConnection`, so the interpreter never talks to MSSQL directly.

**DDF Files (Data Dictionary Files)**:
The SQLBtr migration produced DDF files that contain the complete schema mapping between TAS and MSSQL. Each file defines:
- Table metadata: server, database, table name, schema, page size, record length
- Field definitions: name, native type (0=string, 1=int16, 2=float64, 3=date), length, offset
- Index definitions: which fields compose each index, segment flags, null handling

Example DDF (BKPMTAP):
```
DRIVER_NAME SQL_BTR
SERVER_NAME 10.0.0.220
DATABASE_SPACE_NAME GPacific
TABLE_NAME BKPMTAP
SCHEMA_NAME dbo

FIELD_NUMBER 1
FIELD_NAME BKPMT_CODE
FIELD_NATIVE_TYPE 0          ← String (Alpha)
FIELD_NATIVE_LENGTH 10
FIELD_NATIVE_OFFSET 0

INDEX_NUMBER 1
INDEX_NUMBER_SEGMENTS 2
INDEX_SEGMENT_FIELD 3        ← First segment: field #3
INDEX_SEGMENT_FLAG 259
INDEX_SEGMENT_FIELD 0        ← Second segment: field #0 (?)
INDEX_SEGMENT_FLAG -1
```

**Native type mapping**:
| FIELD_NATIVE_TYPE | TAS Type | MSSQL Type | C# Type |
|---|---|---|---|
| 0 | Alpha (A) | NVARCHAR(n) | string |
| 1 | Integer (I) | SMALLINT | short |
| 2 | Real/Numeric (R/N) | FLOAT | double |
| 3 | Date (D) | DATE | DateTime |
| (others TBD) | | | |

**Adapter pattern**:
```csharp
public interface IStorageAdapter : IDisposable
{
    // Connection lifecycle
    Task OpenAsync(string connectionString);
    
    // Table/schema operations
    Task CreateTableFromDictionary(FileDefinition fileDef);
    
    // Record operations
    Task<Record> FindExact(int fileNum, int keyNum, object value);
    Task<Record> FindGreater(int fileNum, int keyNum, object value);
    Task<Record> FindLess(int fileNum, int keyNum, object value);
    Task<IAsyncEnumerable<Record>> Scan(int fileNum, int keyNum);
    Task Add(int fileNum, Record record);
    Task Update(int fileNum, Record record);
    Task Delete(int fileNum, Record record);
    
    // Locking
    Task<bool> LockRecord(int fileNum, long recordId);
    Task UnlockRecord(int fileNum, long recordId);
}

// Default implementation
public class MssqlStorageAdapter : IStorageAdapter { ... }

// Future alternatives
// public class SqliteStorageAdapter : IStorageAdapter { ... }
// public class PostgresStorageAdapter : IStorageAdapter { ... }
```

**Key mappings**:
| TAS Concept | Database Implementation |
|---|---|
| Data file (e.g., BKARCUST) | Table |
| Record | Row |
| Field | Column (typed: NVARCHAR, INT, FLOAT, DATETIME, BIT) |
| Key/Index | Database index (unique or non-unique) |
| Record pointer | Identity / primary key |
| File buffer | In-memory record object |
| FIND (exact/greater) | SELECT with WHERE / ORDER BY via adapter |
| SCAN | Cursor iteration (row-by-row, faithful to original) |
| Record locking | SQL Server row-level locking (ROWLOCK hints) |

**Data Dictionary**:
- Parse DDF files from the `db/` directory (one per table, generated by SQLBtr)
- DDF files contain field names, types, offsets, lengths, and index definitions
- Map DDF native types to TAS types and C# types
- Cache parsed dictionary in memory at startup
- Connection info (server, database) also comes from the DDF headers

**Sub-tasks**:
- [ ] DDF file parser (read SQLBtr-generated data dictionary files from db/ directory)
- [ ] `IStorageAdapter` interface + `MssqlStorageAdapter`
- [ ] Table creation from dictionary definitions
- [ ] Record CRUD (ADD, DELETE, UPDATE via field writes + saves)
- [ ] Keyed access (FIND exact, FIND greater-than, FIND less-than)
- [ ] Sequential scan (faithful row-by-row iteration — no shortcuts)
- [ ] Record buffer management (load into fields, write back)
- [ ] File open/close lifecycle (OPENV, CLOSE_FILE)
- [ ] Multi-user locking (SQL Server row-level locks)
- [ ] Connection string configuration (appsettings.json)

### Layer 4: UI Bridge (The Middleman)

**What it does**: Intercepts all UI commands from the interpreter and translates them into JSON messages for the browser. Receives user input events and feeds them back to the blocked interpreter.

**This is the critical innovation layer.** The interpreter thinks it's talking to a character terminal. The bridge translates to/from HTML.

**Protocol (interpreter → browser)**:
```json
{"type": "clear_screen"}
{"type": "set_cursor", "row": 5, "col": 10}
{"type": "say", "row": 3, "col": 5, "text": "Customer Name:", "color": "NORM"}
{"type": "enter", "id": "fld_001", "row": 3, "col": 20, "size": 30, "type": "A",
 "mask": "AAAAAAA", "default": "", "help": "Enter customer name"}
{"type": "window", "row": 5, "col": 10, "height": 15, "width": 50, "title": "Lookup"}
{"type": "mount", "fields": [...], "layout": {...}}
{"type": "color", "norm": "#fff/#000", "high": "#ff0/#000", ...}
{"type": "bell"}
{"type": "message", "text": "Record saved.", "type": "info"}
{"type": "ask", "id": "ask_001", "prompt": "Delete this record?", "default": "N"}
{"type": "menu", "id": "menu_001", "options": ["A. Inventory", "B. Reports", ...]}
{"type": "list", "id": "list_001", "columns": [...], "rows": [...], "page": 1}
{"type": "button", "id": "btn_001", "row": 20, "col": 5, "label": "Save"}
{"type": "print_preview", "content": "..."}
```

**Protocol (browser → interpreter)**:
```json
{"type": "field_input", "id": "fld_001", "value": "ACME Corp"}
{"type": "key_press", "key": "F2"}
{"type": "key_press", "key": "ESC"}
{"type": "key_press", "key": "PGDN"}
{"type": "ask_response", "id": "ask_001", "value": "Y"}
{"type": "menu_select", "id": "menu_001", "choice": "A"}
{"type": "mouse_click", "row": 20, "col": 5}
{"type": "button_click", "id": "btn_001"}
```

**Blocking model**: When the interpreter hits an ENTER command, it:
1. Sends the field definition to the browser via the bridge
2. **Suspends** (async await) until the browser sends back the input value
3. Resumes execution with the entered value

**Sub-tasks**:
- [ ] Define JSON wire protocol (all message types)
- [ ] Implement bridge interface (IUIBridge) in interpreter
- [ ] SAY → message translation
- [ ] ENTER → field input request + await response
- [ ] ASK → Y/N prompt + await response
- [ ] WINDOW/MOUNT → layout messages
- [ ] COLOR → theme/palette messages
- [ ] CURSOR/CLEAR → screen control messages
- [ ] DISPLAY_ARRAY_FIELDS → list/table rendering
- [ ] BUTTON/CAPTION → interactive element messages
- [ ] Key trap forwarding (function keys, ESC, navigation)
- [ ] Mouse event forwarding
- [ ] Print/report output → print preview / PDF generation

### Layer 5: Web Host (ASP.NET + SignalR)

**What it does**: HTTP server that serves the HTML UI, manages WebSocket connections via SignalR, and maps each connection to a TAS interpreter instance.

**Session management**:
- Each SignalR connection = one user session
- Each session gets its own TAS interpreter instance (isolated state)
- Interpreter runs on a background thread; UI bridge communicates via SignalR
- Session timeout / cleanup when browser disconnects

**Sub-tasks**:
- [ ] ASP.NET minimal API project setup
- [ ] SignalR hub for TAS UI protocol
- [ ] Session manager (create/destroy interpreter instances)
- [ ] Static file serving for HTML/JS/CSS frontend
- [ ] Authentication / user identification (optional, later)
- [ ] Connection lifecycle (connect, disconnect, reconnect)
- [ ] Concurrent session handling (thread safety)

### Layer 6: React/Vite Frontend (Browser Terminal)

**What it does**: Renders TAS screens in the browser using a **React + Vite** SPA. Essentially a character-grid terminal emulator built as a React component tree that understands TAS UI semantics.

**Tech stack**:
- **Vite** — build tool (fast HMR during development)
- **React 18+** — component-based UI with hooks
- **TypeScript** — type safety for the UI protocol messages
- **@microsoft/signalr** — SignalR client for WebSocket communication
- **CSS Modules or Tailwind** — styling the terminal grid

**Approach**: The core is a `<TerminalGrid>` React component that renders an 80×25 character grid. Each cell is a styled `<span>` managed via React state. Input fields, windows, and dialogs are React components overlaid on the grid at exact row,col positions.

**Component architecture**:
```
<App>
  <ConnectionProvider>           ← SignalR connection context
    <SessionProvider>            ← TAS session state context
      <TerminalScreen>           ← Main layout container
        <TerminalGrid>           ← 80×25 character cell grid
          <Cell row={r} col={c}> ← Individual character cells
        </TerminalGrid>
        <FieldInput>             ← Positioned <input> overlays for ENTER
        <WindowOverlay>          ← WINDOW dialogs (bordered regions)
        <MenuOverlay>            ← Menu selection UI
        <ListOverlay>            ← LISTF/LISTM scrollable tables
        <AskDialog>              ← Y/N prompt overlay
      </TerminalScreen>
      <StatusBar>                ← Connection status, session info
      <PrintPreview>             ← Modal for print output / PDF download
    </SessionProvider>
  </ConnectionProvider>
</App>
```

**Key React hooks**:
- `useSignalR()` — manages connection, reconnection, message dispatch
- `useTerminalState()` — reduces incoming UI messages into grid state
- `useKeyCapture()` — global keyboard listener, forwards to SignalR
- `useFieldInput()` — manages active input field state + validation

**State management**: Use `useReducer` for the terminal grid state — incoming messages from SignalR dispatch actions that update the character buffer, colors, cursor position, active fields, and window stack. This keeps rendering efficient (only changed cells re-render).

**Key features**:
- Character grid rendering (monospace font, exact row/col positioning)
- Color support (map TAS 16-color palette to CSS variables)
- Input field overlay (React `<input>` components at exact grid positions)
- Keyboard capture (function keys, ESC, arrows, PgUp/PgDn — prevent browser defaults)
- Window/dialog rendering (React portals for z-ordered overlays)
- List/table display (virtualized scrollable array field display)
- Menu rendering (keyboard-navigable option list)
- Print preview (modal with iframe or embedded PDF viewer)
- Mouse click → row,col translation on the grid

**Sub-tasks**:
- [ ] Vite + React + TypeScript project scaffold
- [ ] TypeScript types for all UI protocol messages (shared with .NET)
- [ ] `<TerminalGrid>` component (80×25+ grid of styled spans)
- [ ] `useSignalR` hook (connect, reconnect, send/receive typed messages)
- [ ] `useTerminalState` reducer (process SAY, CLEAR, COLOR, CURSOR messages)
- [ ] `useKeyCapture` hook (capture F-keys, ESC, arrows, prevent defaults)
- [ ] Color palette mapping (TAS 16 colors → CSS custom properties)
- [ ] `<FieldInput>` component (text, numeric, date, masked input at grid positions)
- [ ] `<WindowOverlay>` component (bordered dialog regions)
- [ ] `<ListOverlay>` component (array field display with PgUp/PgDn pagination)
- [ ] `<MenuOverlay>` component (keyboard-navigable menu)
- [ ] `<AskDialog>` component (Y/N prompt)
- [ ] `<PrintPreview>` modal (rendered print output, PDF download)
- [ ] Responsive layout (scale grid to browser window, maintain aspect ratio)
- [ ] Dev mode: message inspector panel (see raw SignalR JSON for debugging)

---

## Implementation Phases

### Phase 1: Foundation (Get "Hello World" running)
- [ ] .NET 8 solution structure (projects for Parser, Interpreter, Storage, Bridge, Web)
- [ ] Basic lexer + parser (handle DEFINE, SAY, IF/ENDIF, GOTO, simple expressions)
- [ ] Minimal interpreter (execute SAY, DEFINE, basic assignments)
- [ ] Console-mode UI bridge (render to terminal first, before HTML)
- [ ] Run a trivial .SRC program end-to-end

### Phase 2: Core Language
- [ ] Full expression evaluator (all operators, type coercion)
- [ ] All flow control (IF/ELSE/ENDIF, WHILE/ENDW, CASE/ENDCASE, GOTO/GOSUB)
- [ ] DEFINE with all types (A, N, I, R, D, T, L) and arrays
- [ ] Implement 30 most-used functions (STR, VAL, MID, TRIM, EOF, BOF, SIZE, DATE, TIME, etc.)
- [ ] #lib / #inc preprocessor
- [ ] #proc / #endp procedure support
- [ ] TRAP system
- [ ] CHAIN (load and execute another .SRC, return)

### Phase 3: Storage Engine
- [ ] DDF file parser (read all table definitions from db/ directory)
- [ ] `IStorageAdapter` interface + `MssqlStorageAdapter`
- [ ] Table mapping from DDF → MSSQL connection
- [ ] OPENV / CLOSE_FILE
- [ ] Record buffer management
- [ ] FIND (exact, greater, less)
- [ ] SCAN
- [ ] ADD / DELETE / UPDATE records
- [ ] Record locking (multi-user)
- [ ] Implement remaining file functions (FLERR, FNUM, REC_PTR, CREC, etc.)

### Phase 4: UI Bridge + React/Vite Frontend
- [ ] SignalR hub + session manager
- [ ] Vite + React + TypeScript project scaffold
- [ ] `<TerminalGrid>` character grid component
- [ ] `useSignalR` hook + `useTerminalState` reducer
- [ ] SAY → render text on grid
- [ ] ENTER → `<FieldInput>` overlay + blocking await
- [ ] ASK → `<AskDialog>` overlay
- [ ] MOUNT / screen format loading
- [ ] `<WindowOverlay>` component
- [ ] COLOR system (CSS custom properties)
- [ ] `useKeyCapture` hook (F-keys, ESC, arrows)
- [ ] Run BKTEST.SRC in browser

### Phase 5: Full Language Coverage
- [ ] All remaining ~169 functions
- [ ] All remaining ~65 commands
- [ ] Array field display (DISPF)
- [ ] List functions (LISTF, LISTM)
- [ ] Non-TAS file I/O (OPEN/CLOSE/READ/WRITE flat files)
- [ ] EXECUTE_PROGRAM (shell out to OS commands)
- [ ] Print/report system
- [ ] Memory areas (MEM 1-4)

### Phase 6: Real Program Validation
- [ ] Parse all 300+ .SRC files without errors
- [ ] Run BKLMENU1.SRC (menu navigation)
- [ ] Run BKGLA.SRC (GL entry with data)
- [ ] Run BKARA.SRC (AR customer entry)
- [ ] Run BKSOA.SRC (sales order entry — complex, multi-file)
- [ ] Performance profiling (target: responsive UI, <100ms per command)
- [ ] Multi-user stress test

### Phase 7: Polish & Production
- [ ] Error reporting (source file + line number in browser)
- [ ] Session management UI (admin panel)
- [ ] Logging / audit trail
- [ ] Configuration (paths, database location, port)
- [ ] Documentation
- [ ] Docker container packaging

### Phase 8: Performance Optimizations (Future — only after full correctness)
- [ ] SCAN predicate pushdown: analyze loop bodies for simple IF conditions on file fields, translate to SQL WHERE clauses so the database filters instead of the interpreter iterating every row
- [ ] FIND batching: pre-fetch adjacent records when sequential access patterns detected
- [ ] AST caching: cache parsed .SRC → AST in memory, only re-parse on file change
- [ ] Connection pooling tuning (per-session vs shared connections)
- [ ] Profile real workloads (BKSOA order entry, BKARA customer lookup) and optimize hot paths

> **Rule**: Every optimization must produce identical results to naive row-by-row execution. If there's any doubt, don't optimize it.

---

## Key Design Decisions

### 1. Interpreter Style: Tree-Walking
Use a tree-walking interpreter (parse to AST, walk and execute). Not a bytecode VM. Simpler to build, easier to debug, and TAS programs are not performance-critical (they're mostly I/O-bound waiting for user input or database).

### 2. Async Execution Model
The interpreter runs on a background task per session. When it hits a blocking UI command (ENTER, ASK), it uses `async/await` with a `TaskCompletionSource` — the bridge sets the result when the browser responds. This avoids blocking threads.

```csharp
// Pseudocode
async Task ExecuteEnter(EnterCommand cmd) {
    var request = new FieldInputRequest(cmd.Field, cmd.Row, cmd.Col, ...);
    await _bridge.SendToClient(request);          // Send to browser
    var response = await _bridge.WaitForInput();   // Suspend until user types
    SetFieldValue(cmd.Field, response.Value);      // Resume with value
}
```

### 3. Field Storage: Dictionary<string, TasValue>
Fields stored in a flat dictionary. TasValue is a discriminated union holding the typed value + metadata (type, size, decimals, array index).

### 4. No .RUN Files
We never produce .RUN files. The .SRC is the program. Parse on load, cache the AST in memory. Re-parse on change (like Python).

### 5. Screen Format Files
MOUNT references screen format files. These need to be either:
- Parsed from existing format files (binary format, needs reverse-engineering), OR
- Replaced with a text-based format definition (JSON/YAML) that maps field positions

### 6. Printing
TAS printing goes to physical printers or disk files. In the web version:
- Print commands generate a **print preview** in the browser
- User can download as PDF or print from browser
- Existing HTML template programs (BKRMAHTM, BKSONHTM) work naturally since they already generate HTML

---

## Project Structure

```
CoTAS/
├── CoTAS.sln
├── src/
│   ├── CoTAS.Parser/            # Lexer, parser, AST definitions
│   │   ├── Lexer.cs
│   │   ├── Parser.cs
│   │   ├── Preprocessor.cs      # #lib, #inc expansion
│   │   ├── Ast/
│   │   │   ├── Nodes.cs         # All AST node types
│   │   │   ├── Expressions.cs
│   │   │   └── Commands.cs
│   │   └── Grammar/
│   │       └── TasGrammar.cs    # Formal grammar rules
│   │
│   ├── CoTAS.Interpreter/       # Execution engine
│   │   ├── Interpreter.cs       # Main execution loop
│   │   ├── ExpressionEvaluator.cs
│   │   ├── FieldManager.cs      # Field definitions, values, arrays
│   │   ├── FileManager.cs       # Open file table, buffers
│   │   ├── TrapManager.cs       # Key trap registration
│   │   ├── CallStack.cs         # GOSUB/CHAIN stack
│   │   ├── Functions/           # Built-in function implementations
│   │   │   ├── StringFunctions.cs   # STR, VAL, MID, TRIM, etc.
│   │   │   ├── MathFunctions.cs     # ABS, SQRT, SIN, COS, etc.
│   │   │   ├── DateFunctions.cs     # DATE, TIME, CTOD, DTOC, etc.
│   │   │   ├── FileFunctions.cs     # EOF, BOF, FNUM, SIZE, etc.
│   │   │   └── ScreenFunctions.cs   # ROW, COL, INKEY, etc.
│   │   └── Commands/           # Command implementations
│   │       ├── DataEntryCommands.cs  # ENTER, ASK, SAY
│   │       ├── FileCommands.cs       # OPEN, CLOSE, FIND, SCAN
│   │       ├── FlowCommands.cs       # IF, WHILE, CASE, GOTO
│   │       ├── FieldCommands.cs      # DEFINE, ADD, DEALLOCATE
│   │       └── ScreenCommands.cs     # CLEAR, COLOR, CURSOR, WINDOW
│   │
│   ├── CoTAS.Storage/           # Database adapter layer
│   │   ├── IStorageAdapter.cs   # Adapter interface (swap DB backends)
│   │   ├── MssqlStorageAdapter.cs  # MSSQL implementation (default)
│   │   ├── DdfParser.cs         # Parse SQLBtr DDF files
│   │   ├── StorageEngine.cs     # TAS file/record semantics on top of adapter
│   │   ├── DataDictionary.cs    # In-memory schema cache from DDFs
│   │   ├── RecordBuffer.cs
│   │   ├── IndexManager.cs
│   │   └── LockManager.cs
│   │
│   ├── CoTAS.Bridge/            # UI abstraction layer
│   │   ├── IUIBridge.cs         # Interface: interpreter ↔ UI
│   │   ├── UIMessage.cs         # JSON message types
│   │   ├── ConsoleBridge.cs     # Terminal rendering (dev/debug)
│   │   └── WebBridge.cs         # SignalR rendering (production)
│   │
│   ├── CoTAS.Web/               # ASP.NET host
│   │   ├── Program.cs
│   │   ├── TasHub.cs            # SignalR hub
│   │   └── SessionManager.cs
│   │
│   └── CoTAS.Cli/               # CLI runner (optional)
│       └── Program.cs           # Run .SRC from command line
│
├── client/                      # React/Vite frontend
│   ├── package.json
│   ├── vite.config.ts
│   ├── tsconfig.json
│   ├── index.html
│   └── src/
│       ├── main.tsx
│       ├── App.tsx
│       ├── types/
│       │   └── protocol.ts          # Shared UI message types
│       ├── hooks/
│       │   ├── useSignalR.ts        # SignalR connection management
│       │   ├── useTerminalState.ts  # Grid state reducer
│       │   ├── useKeyCapture.ts     # Keyboard event capture
│       │   └── useFieldInput.ts     # Active field input state
│       ├── components/
│       │   ├── TerminalScreen.tsx    # Main layout container
│       │   ├── TerminalGrid.tsx     # 80×25 character cell grid
│       │   ├── Cell.tsx             # Individual character cell
│       │   ├── FieldInput.tsx       # Positioned input overlays
│       │   ├── WindowOverlay.tsx    # WINDOW dialog regions
│       │   ├── MenuOverlay.tsx      # Menu selection UI
│       │   ├── ListOverlay.tsx      # LISTF/LISTM scrollable tables
│       │   ├── AskDialog.tsx        # Y/N prompt overlay
│       │   ├── StatusBar.tsx        # Connection/session status
│       │   └── PrintPreview.tsx     # Print output modal
│       ├── context/
│       │   ├── ConnectionContext.tsx # SignalR connection provider
│       │   └── SessionContext.tsx    # TAS session state provider
│       └── styles/
│           ├── terminal.css         # Grid and cell styles
│           └── colors.css           # TAS color palette CSS vars
│
├── tests/
│   ├── CoTAS.Parser.Tests/
│   ├── CoTAS.Interpreter.Tests/
│   └── CoTAS.Storage.Tests/
```

---

## Risk Areas & Open Questions

1. **Screen format files**: MOUNT loads binary format files. Either reverse-engineer the format or create a conversion tool. Could also infer layouts from SAY/ENTER positions in code.

2. **#lib path resolution**: Libraries use relative paths like `#lib pr`, `#lib \adv51src\listg50`, `#lib 120110\pr`. Need configurable library search paths.

3. **EXECUTE_PROGRAM**: Shells out to OS commands. Need to sandbox or restrict this in a web environment.

4. **DDF completeness**: Need to verify that all tables used by the 300+ .SRC programs have corresponding DDF files. Also need to confirm the full set of FIELD_NATIVE_TYPE values (we've seen 0, 1, 2, 3 — there may be more for Logical, Time, etc.).

5. **Non-TAS file I/O**: Some programs read/write flat files, CSVs, HTML templates. Need standard file I/O alongside the database layer.

6. **Performance**: Tree-walking interpreter should be fine for interactive business apps, but SCAN operations over large datasets could be slow. May need query optimization.

7. **Completeness**: 169 functions + 65 commands is a lot. Prioritize by frequency in the real codebase — implement what the actual programs use first.

8. **Windows-only features**: OLE objects, picture fields, laser printer functions — may be skipped or stubbed initially.
