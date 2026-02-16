## What is CoTAS?

**CoTAS** is a modern, cross-platform runtime environment that executes programs written in the TAS Professional 5.1 programming language. It replaces the original 16-bit DOS runtime, compiler, and Btrieve database engine with a 64-bit .NET 10 implementation that runs on Windows, Linux, and macOS - and delivers your programs through a web browser instead of a DOS console.

If you have TAS source code (`.SRC` files) or compiled programs (`.RUN` files), CoTAS runs them. Not a single line of existing code needs to change.

## Free and Open Source

CoTAS is **100% open source**. The full source code, the runtime engine, and all supporting tools are available on GitHub. You can download it, build it, modify it, extend it, and redistribute it. There are no license fees, no per-seat charges, no annual subscriptions, and no runtime royalties. Free as in free.

You can clone the repository, compile the engine yourself, and run your TAS programs today at zero cost.

While the software itself is and always will be free, **Watkins Labs** does offer professional services for organizations that need help getting up and running. This includes initial setup and migration assistance, custom development for TAS programs that use non-standard or heavily modified features, and bug fixes for edge cases specific to your codebase. If your software has been customized beyond the standard TAS 5.1 specification and something does not work, we can fix it for you.

The software is free. The expertise is available when you need it.

## What is TAS?

**TAS** - *The Accounting Solution* - is a fourth-generation programming language (4GL) created and developed by **Phil Mickelson** beginning in **1984**. TAS was purpose-built for business application development: accounting systems, inventory management, sales order processing, payroll, and general ledger - the kind of software that businesses depend on every day.

TAS is not a general-purpose language. It is a domain-specific language designed around data entry screens, record-oriented file I/O, report generation, and structured business logic. A TAS program opens data files, displays entry screens, validates input, reads and writes records, and prints reports. The language handles all of this with dedicated commands - `OPENV`, `FINDV`, `SCAN`, `ENTER`, `SAY`, `SAVE` - rather than requiring the programmer to build these abstractions from scratch.

Over its lifetime, thousands of business applications were written in TAS. The most significant of these is **Advanced Accounting**, a comprehensive accounting system that has been in continuous production use since its original release in the fall of 1996.

## What is Advanced Accounting?

**Advanced Accounting** is a full-featured, modular accounting system written entirely in TAS Professional. It is the primary application that CoTAS was built to support.

Advanced Accounting includes 13 fully integrated modules:

- **General Ledger** - Fully transactionalized, post back up to 5 prior years, departmental support, consolidated financials
- **Accounts Receivable** - Customer management, aging, collections tracking
- **Accounts Payable** - Vendor management, payment processing
- **Sales Order** - Full order entry and processing with bar code support
- **Purchase Order** - Multi-location receiving on the same PO
- **Inventory Control** - Multi-location with cost tracking at the location level, 25-character product codes
- **Payroll** - User-maintained tax tables (no forced annual update purchases)
- **Point of Sale** - Integrated POS with cash drawers, pole displays, register/clerk support
- **Job Cost** - Project-based cost tracking
- **Bill of Materials** - Multi-level finished goods and sub-assemblies, work orders from sales orders
- **Consignment** - Both consignee and consignor handling
- **Report Output** - Built-in PDF, RTF, HTML, Excel, and text output - no additional purchases required
- **Security** - User permissions, module-level access control, branch office restrictions

Advanced Accounting is notable for being **fully modifiable** - developers who purchase the source code can modify actual program logic, not just configuration options. This has led to decades of customized installations across every US state and in multiple countries. Many installations have been in continuous daily use for 20+ years.

The software is actively supported and sold by multiple independent providers, including:

- **Addsum Business Software, Inc.** ([www.addsuminc.com](https://www.addsuminc.com/)) - Tony Frates. Publisher of Advanced Accounting 5.1 through 8, TAS Premier 7i/7ix. Based in Salt Lake City, Utah. Has been providing support and custom programming since 1994, with some customers supported continuously for over 26 years.
- **Computer Accounting Solutions** ([www.cassoftware.com](http://www.cassoftware.com/)) - Publisher of Advanced Accounting 7 with online documentation and custom programming services. Based in Kingman, Arizona.

## Why Replace the Original Runtime?

The original TAS 5.1 runtime has served well for decades, but it faces real limitations in today's computing environment:

<div class="callout warning">
<div class="callout-title">⚠ The Problem</div>
<ul>
<li><strong>16-bit DOS executable</strong> - Cannot run natively on 64-bit Windows (requires Virtual PC or emulation)</li>
<li><strong>Btrieve/Pervasive dependency</strong> - Requires paid, per-seat database engine licenses (Actian Zen)</li>
<li><strong>No remote access</strong> - Character-mode DOS interface with no web or network access</li>
<li><strong>No cross-platform support</strong> - DOS/Windows only</li>
<li><strong>Original vendor defunct</strong> - The Business Tools, Inc. website (<a href="https://www.business-tools.com/">www.business-tools.com</a>) has been defunct for over a decade with no active content or support</li>
<li><strong>No source code for the runtime itself</strong> - Bugs in the TAS runtime cannot be fixed</li>
</ul>
</div>

Meanwhile, thousands of businesses still depend on TAS-based software every day. Their programs work. Their data is valuable. They need a path forward that doesn't require rewriting everything from scratch.

## What CoTAS Provides

CoTAS solves every one of these problems:

- **Run existing code unchanged** - Both `.SRC` source files and compiled `.RUN` binaries
- **64-bit .NET 10 runtime** - Modern, supported, actively maintained
- **No paid database drivers** - Direct MSSQL connectivity today, MySQL and PostgreSQL in progress, all using free open-source ADO.NET providers
- **Web-based terminal** - Access your programs from any browser, anywhere, via SignalR
- **Cross-platform** - Windows, Linux, macOS
- **Multi-user** - Each browser connection gets an isolated session with its own field table, file handles, and call stack
- **Full language coverage** - 127 commands, 170+ functions, ~95% of the TAS 5.1 specification implemented

### Lost Your Source Code? We Can Help.

Many TAS installations have been running compiled `.RUN` files for decades. The original `.SRC` source code may have been lost to time, hardware failures, or staff turnover. CoTAS includes a full **decompiler** that can recover source code from compiled `.RUN` binaries.

This is not an approximation. The decompiler reads the `.RUN` bytecode, reconstructs the AST, and produces readable TAS source code. This gives you:

- **Run legacy programs immediately** - Even without source code, CoTAS executes `.RUN` files directly by decompiling them on the fly
- **Recover your business logic** - Decompile to `.SRC` and see exactly what your programs do, field by field, command by command
- **Maintain old software** - Fix bugs, add features, and update programs that haven't been touched in years
- **A path to modernization** - With recovered source code in hand, you can plan a gradual migration, rewrite specific modules, or integrate with modern systems, all without losing the business rules your organization has depended on for decades

<div class="callout tip">
<div class="callout-title">Round-Trip Fidelity</div>
CoTAS can read a <code>.RUN</code> file, decompile it to source, recompile it, and produce byte-identical output. The <code>--roundtrip</code> CLI flag tests this, validating that the compiler and decompiler are perfect inverses. Your recovered source code is not a guess. It is a faithful representation of the original program.
</div>

## How CoTAS Came to Be

<div class="pull-quote">
"Every opcode was figured out by examining compiled output. Every expression encoding rule was reverse-engineered from real .RUN files."
<span class="attribution">15 years of binary archaeology</span>
</div>

CoTAS is a labor of love spanning over **15 years** of dedicated effort.

It began with hand-decoding `.RUN` binary files, byte by byte, trying to figure out how the original TAS 5.1 compiler laid out its instructions. There was no specification document. There was no open-source reference implementation. The original vendor's website had gone dark. The only published technical reference for TAS was a single PDF manual, which provided command names, function signatures, and a general idea of what they did, but nothing about the internal bytecode format, the expression encoding, or the runtime execution model.

Every opcode was figured out by examining compiled output. Every expression encoding rule was reverse-engineered from real `.RUN` files. The field spec format, the constant pool layout, the label segment structure, the 128-byte file header - all of it was decoded by hand and cross-referenced against the published manual to verify behavior.

Meanwhile, information about TAS was disappearing from the internet. Forum posts, developer notes, technical articles - the kind of institutional knowledge that an aging platform depends on - were vanishing as websites went offline and communities moved on. What could be found was collected and preserved. What couldn't be found was reconstructed through testing and experimentation.

The library of knowledge grew slowly. First a parser that could tokenize TAS source. Then a decompiler that could read `.RUN` files. Then an expression evaluator. Then command handlers, one at a time, tested against real programs. Then a storage engine to replace Btrieve. Then a web interface to replace the DOS console. Seven .NET projects, over 10,000 lines of C#, built up piece by piece over more than a decade.

This book, and the runtime it documents, exist because someone cared enough to keep this software alive.

## History of TAS

TAS grew through several major versions:

- **TAS Professional 3.0** - DOS-based, BCD numeric types, Btrieve data engine
- **TAS Professional 4.0** - Enhanced 4GL features, improved compiler
- **TAS Professional 5.0** - Major rewrite: IEEE floating-point, expanded field sizes, Windows support
- **TAS Professional 5.1** - Final DOS-era release: Windows extensions, OLE automation, DLL support, 7-byte bytecode format (`TAS32` signature)

In **2003**, sole worldwide exclusive re-licensing rights to the TAS Windows Development Environment and the TAS Accounting Source Code were purchased in perpetuity by **Naresh Daswani**.

TAS was later developed further into the Windows era as **TAS Professional 6.0** (and possibly newer versions), which introduced a Windows-native runtime with updated bytecode (`TASWN` signature) and larger field specs. The original TAS website ([www.business-tools.com](https://www.business-tools.com/)) has been defunct for over a decade, with no active content or support available.

CoTAS focuses on the **DOS-era TAS 5.1 language** - the version most widely deployed and most in need of a modern path forward. The 5.1 bytecode format, command set, and execution model are what CoTAS faithfully reimplements.

## Architecture Overview

CoTAS is built from seven modular .NET 10 projects:

| Project | Purpose |
|---------|---------|
| **CoTAS.Parser** | Lexer, preprocessor, recursive-descent parser, AST, .RUN decompiler |
| **CoTAS.Interpreter** | Tree-walking execution engine, 127 command handlers, 170+ built-in functions |
| **CoTAS.Compiler** | Two-pass bytecode compiler: AST → .RUN binary (7-byte instructions, RPN expressions) |
| **CoTAS.Storage** | DDF schema parser, MSSQL adapter (MySQL & PostgreSQL in progress), no paid drivers, file handles, record buffers |
| **CoTAS.Bridge** | `IUIBridge` abstraction - `ConsoleBridge` (terminal) and `WebBridge` (SignalR) |
| **CoTAS.Web** | ASP.NET host, SignalR hub (`InterpreterHub`), 80×25 browser terminal |
| **CoTAS.Cli** | Command-line runner with compile, decompile, round-trip test, and batch-parse modes |

<div class="arch-diagram">
┌─────────────────────────────────────┐
│   Browser (80×25 Terminal Grid)     │
│   Vanilla JS + Bootstrap + SignalR  │
│   DOM-based rendering, green/black  │
└──────────────┬──────────────────────┘
               │ WebSocket (SignalR)
               │ Say, RequestInput, RequestAsk,
               │ ClearScreen, ProgramStarted/Ended
┌──────────────┴──────────────────────┐
│   ASP.NET Host (CoTAS.Web)          │
│   InterpreterHub - /hub/interpreter │
│   ConcurrentDictionary sessions     │
│   One (WebBridge, CTS) per client   │
└──────────────┬──────────────────────┘
               │
┌──────────────┴──────────────────────┐
│   IUIBridge Abstraction             │
│   WebBridge: 25×80 char buffer +    │
│     TaskCompletionSource for input  │
│   ConsoleBridge: Console.ReadLine   │
└──────────────┬──────────────────────┘
               │
┌──────────────┴──────────────────────┐
│   TAS Interpreter                   │
│   Pass 1: Collect labels, DEFINE,   │
│           FUNC, CMD definitions     │
│   Pass 2: Execute AST sequentially  │
│   FieldManager, TrapManager,        │
│   ExpressionEvaluator,              │
│   CommandRegistry (127 handlers)    │
└──────────────┬──────────────────────┘
               │
┌──────────────┴──────────────────────┐
│   StorageEngine (MSSQL • MySQL/PG soon) │
│   DdfParser → TableSchema           │
│   FIND → SELECT WHERE key = @p      │
│   SCAN → ORDER BY cursor reads      │
│   SAVE → INSERT / UPDATE            │
│   DEL  → DELETE WHERE pk = @p       │
└─────────────────────────────────────┘
</div>

### Dual Execution Paths

CoTAS can run your programs two ways:

**Source-Direct** (default): `.SRC` → Lexer → Preprocessor → Parser → AST → Interpreter

**Legacy Bytecode**: `.RUN` → RunFileReader → RunFileDecompiler → AST → Interpreter

Both paths produce the same AST and feed the same interpreter. The bytecode path reads the 128-byte `.RUN` header, decodes 7-byte instructions (opcode + spec pointer), reconstructs expressions from RPN bytecode via `ExpressionDecoder`, and produces an executable AST. This means you can run programs whether you have the source code or only the compiled binaries.

<div class="callout tip">
<div class="callout-title">💡 Round-Trip Fidelity</div>
CoTAS can read a <code>.RUN</code> file, write it back, and produce byte-identical output. The <code>--roundtrip</code> CLI flag tests this - validating that the compiler and decompiler are perfect inverses.
</div>

### The Compiler

CoTAS includes a full **two-pass bytecode compiler** that produces `.RUN` files compatible with the original TAS 5.1 format:

- **Pass 1** - Collect field definitions, labels, and forward references
- **Pass 2** - Emit bytecode instructions with RPN-encoded expressions
- **Output** - Standard `.RUN` binary: 128-byte header, buffer list, code/constant/spec/label/field segments

The compiler uses a `ConstantPool` with deduplication, an `ExpressionEncoder` that compiles expression ASTs to RPN bytecode (180+ built-in function numbers mapped), and a `FieldTable` that calculates offsets for the 48-byte field spec format.

## By the Numbers

<div class="stat-row">
<div class="stat-box">
<div class="stat-number">228</div>
<div class="stat-label">Opcodes Decoded</div>
</div>
<div class="stat-box">
<div class="stat-number">127</div>
<div class="stat-label">Command Handlers</div>
</div>
<div class="stat-box">
<div class="stat-number">170+</div>
<div class="stat-label">Built-in Functions</div>
</div>
<div class="stat-box">
<div class="stat-number">267</div>
<div class="stat-label">Error Codes</div>
</div>
</div>

<div class="stat-row">
<div class="stat-box">
<div class="stat-number">583</div>
<div class="stat-label">.RUN Files Tested</div>
</div>
<div class="stat-box">
<div class="stat-number">96%</div>
<div class="stat-label">Decompile Success</div>
</div>
<div class="stat-box">
<div class="stat-number">15</div>
<div class="stat-label">Years in Development</div>
</div>
<div class="stat-box">
<div class="stat-number">$0</div>
<div class="stat-label">License Cost</div>
</div>
</div>

## Anatomy of a .RUN File

Every compiled TAS program is a `.RUN` binary. Understanding this format is central to understanding how CoTAS works. Here is the exact byte layout:

<div class="hex-anatomy">
<div class="hex-title">TAS 5.1 .RUN Binary Layout (128-Byte Header)</div>
<div class="hex-row"><span class="hex-offset">0x0000</span>  <span class="hex-size">CodeSize(4B)</span> <span class="hex-size">ConstSize(4B)</span> <span class="hex-size">SpecSize(4B)</span> <span class="hex-size">LabelSize(4B)</span> <span class="hex-annotation">- Segment sizes</span></div>
<div class="hex-row"><span class="hex-offset">0x0010</span>  <span class="hex-field">ScrnFldNum(4B)</span> <span class="hex-field">NumFlds(4B)</span> <span class="hex-field">TempFlds(4B)</span> <span class="hex-field">NumTempFlds(4B)</span> <span class="hex-annotation">- Field counts</span></div>
<div class="hex-row"><span class="hex-offset">0x0020</span>  <span class="hex-field">FldNameSize(4B)</span> <span class="hex-field">TempFldSize(4B)</span> <span class="hex-field">DefFldSegSize(4B)</span> <span class="hex-field">NumExtraFlds(4B)</span></div>
<div class="hex-row"><span class="hex-offset">0x0030</span>  <span class="hex-const">PrgNames(4B)</span> <span class="hex-code">DebugFlg(1B)</span> <span class="hex-sig">ProType: "TAS32"(5B)</span> <span class="hex-label">NumLabels(4B)</span> <span class="hex-annotation">- Signature!</span></div>
<div class="hex-row"><span class="hex-offset">0x003E</span>  <span class="hex-code">NewFldSpec(1B)</span> <span class="hex-code">ChkUpVld(1B)</span> <span class="hex-code">IncLabels(1B)</span> <span class="hex-offset">[RESERVED 63B to 0x7F]</span></div>
<div class="hex-row"></div>
<div class="hex-row"><span class="hex-offset">0x0080</span>  <span class="hex-const">FILE BUFFER LIST: 100 entries x 16 bytes = 1600 bytes</span></div>
<div class="hex-row"><span class="hex-offset">0x06C0</span>  <span class="hex-code">CODE SEGMENT: 7-byte instructions (opcode + spec ptr)</span></div>
<div class="hex-row"><span class="hex-offset">  +CS </span>  <span class="hex-const">CONSTANT SEGMENT: strings, numbers, expressions (RPN)</span></div>
<div class="hex-row"><span class="hex-offset">  +KS </span>  <span class="hex-size">SPEC SEGMENT: parameter data for each instruction</span></div>
<div class="hex-row"><span class="hex-offset">  +SS </span>  <span class="hex-label">LABEL SEGMENT: 4-byte offsets per label</span></div>
<div class="hex-row"><span class="hex-offset">  +LS </span>  <span class="hex-field">FIELD SPEC SEGMENT: 48 bytes per field (name, type, size, decimals)</span></div>
</div>

### The 7-Byte Instruction

Every instruction in the code segment is exactly 7 bytes:

<div class="hex-anatomy">
<div class="hex-title">TAS32 Instruction Format</div>
<div class="hex-row"><span class="hex-code">Byte 0-1:</span> <span class="hex-sig">CommandNumber</span> (uint16) <span class="hex-annotation">- Opcode (0-518, maps to CLRSCR, SAY, OPENV, etc.)</span></div>
<div class="hex-row"><span class="hex-code">Byte 2:  </span> <span class="hex-const">SpecLineSize</span> (byte)   <span class="hex-annotation">- How many bytes of parameter data</span></div>
<div class="hex-row"><span class="hex-code">Byte 3-6:</span> <span class="hex-size">SpecLinePtr</span>  (int32)  <span class="hex-annotation">- Offset into spec segment for params</span></div>
</div>

### RPN Expression Encoding

Expressions are compiled to Reverse Polish Notation bytecode stored in the constant segment. The decompiler reconstructs readable expressions from this format:

<div class="hex-anatomy">
<div class="hex-title">Expression Structure in Constant Segment</div>
<div class="hex-row"><span class="hex-code">Byte 0:</span>   <span class="hex-sig">ResultType</span>   'A'=alpha, 'I'=integer, 'N'=numeric, 'L'=logical, 'D'=date</div>
<div class="hex-row"><span class="hex-code">Byte 1:</span>   <span class="hex-const">Decimals</span>     Number of decimal places</div>
<div class="hex-row"><span class="hex-code">Byte 2-3:</span> <span class="hex-size">DisplaySize</span>  Size of expression data</div>
<div class="hex-row"><span class="hex-code">Byte 4:</span>   <span class="hex-sig">0xFD</span>          Start marker</div>
<div class="hex-row"><span class="hex-code">Byte 5-8:</span> <span class="hex-field">TempBase</span>     Base offset for temp variables</div>
<div class="hex-row"><span class="hex-code">Byte 9+:</span>  <span class="hex-code">RPN Ops</span>      Variable-length operation stream</div>
<div class="hex-row"><span class="hex-code">Last:   </span>  <span class="hex-sig">0xFF</span>          Terminator</div>
</div>

The RPN operators: `0x01`=ADD, `0x02`=SUB, `0x03`=CONCAT, `0x04`=MUL, `0x05`=DIV, `0x06`=POW, `0x07`=EQ, `0x08`=LT, `0x09`=GT, `0x0A`=NE, `0x0B`=GE, `0x0C`=LE, `0x0D`=AND, `0x0E`=OR, `0x0F`=NOT.

Each operand is a 5-byte spec parameter: type byte (`F`=field, `C`=constant, `N`=numeric literal, `X`=expression, `Y`=array element) followed by a 4-byte offset.

## 1986 vs 2026: The Same Program, Two Eras

<div class="comparison-spread">
<div class="comparison-panel">
<div class="panel-header retro">DOS / TAS 5.1 / 1986</div>
<div class="panel-body">
  C:\TAS>TAS32.EXE MYAPP.RUN

  +-[ CUSTOMER ENTRY ]--------+
  | Cust#: 10042              |
  | Name:  ACME SUPPLY CO     |
  | Addr:  123 MAIN ST        |
  | City:  ANYTOWN            |
  | State: UT  Zip: 84101     |
  |                           |
  | Balance:      $12,450.00  |
  +---------------------------+
  F2=Save  F3=Delete  Esc=Exit

  > Requires: DOS, TAS32.EXE,
    Btrieve 6.15 ($$$),
    VGA monitor, 640K RAM
</div>
</div>
<div class="comparison-panel">
<div class="panel-header">BROWSER / CoTAS / 2026</div>
<div class="panel-body modern">
  http://your-server:5000

  +-[ CUSTOMER ENTRY ]--------+
  | Cust#: 10042              |
  | Name:  ACME SUPPLY CO     |
  | Addr:  123 MAIN ST        |
  | City:  ANYTOWN            |
  | State: UT  Zip: 84101     |
  |                           |
  | Balance:      $12,450.00  |
  +---------------------------+
  F2=Save  F3=Delete  Esc=Exit

  > Requires: Any browser.
    MSSQL/MySQL/PG (free).
    Runs on Windows/Linux/Mac.
</div>
</div>
</div>

The program is identical. The `.SRC` source code has not changed. The user sees the same 80x25 terminal interface. But underneath, everything is different: .NET 10 instead of DOS, SQL Server instead of Btrieve, a web browser instead of a VGA monitor, and zero license fees instead of per-seat charges.

## Migration Path

Not sure where to start? Here is a quick guide:

<div class="decision-flow">
Do you have .SRC source files?
        |
   +----+----+
   |         |
  YES        NO
   |         |
   v         v
Run them   Do you have .RUN compiled files?
directly        |
with       +----+----+
CoTAS      |         |
          YES        NO
           |         |
           v         v
     CoTAS runs    Contact us.
     them AND      We may be able
     decompiles    to help locate
     to .SRC       your programs.
           |
           v
  Now you have source!
  Modify, fix, extend,
  or modernize at will.
</div>

Whether you have source code, compiled binaries, or both, CoTAS gives you a path forward. And because it is open source, you are never locked in to a single vendor again.
