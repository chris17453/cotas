## Program Structure

A TAS program is a plain text `.SRC` file containing a sequence of commands, one per line. Programs execute from top to bottom unless redirected by flow control commands (GOTO, GOSUB, IF, WHILE, etc.).

```tas
; My first TAS program
DEFINE FIELD name A 30
DEFINE FIELD age N 3

SAY 1,1 "Enter your name:"
ENTER 1,20 name

SAY 2,1 "Enter your age:"
ENTER 2,20 age

SAY 4,1 "Hello, " + TRIM(name) + "! You are " + STR(age,3,0) + " years old."
```

### Preprocessor Directives

TAS supports several preprocessor directives that are processed before parsing:

| Directive | Purpose |
|-----------|---------|
| `#lib filename` | Include a library (.LIB) file - searched in current dir then library paths |
| `#inc filename` | Include a source (.SRC) file inline at current position |
| `#proc name` / `#endp` | Define a callable procedure block |
| `#udx` / `#udc` | Compiler mode flags |
| `#psc` | PSC directive |
| `#add_flds` | Enable runtime field addition |

<div class="callout info">
<div class="callout-title">ℹ️ Circular Include Protection</div>
The CoTAS preprocessor tracks included files by full path and enforces a maximum include depth of 20 levels. Circular includes are detected and rejected. File resolution is case-insensitive on Linux for compatibility with DOS-era programs.
</div>

## The Type System

CoTAS implements the TAS type system with six core types internally:

| TasType | TAS Code | Internal Representation | Default Value |
|---------|----------|------------------------|---------------|
| `Alpha` | `A` | String (space-padded to size) | Spaces |
| `Numeric` | `N` | Double-precision IEEE 754 | 0.0 |
| `Integer` | `I` | 32-bit integer | 0 |
| `Date` | `D` | String (YYYYMMDD internally) | Empty |
| `Time` | `T` | String (HH:MM:SS) | Empty |
| `Logical` | `L` | Boolean | `.F.` |

Additional file-level types (used in field specs and DDF definitions):

| Type | Name | Size | Description |
|------|------|------|-------------|
| `B` | Byte | 1 byte | Single byte (0-255) |
| `F` | F-Pointer | 5 bytes | Field pointer (used with `&` redirector) |
| `P` | P-Pointer | 14 bytes | Program pointer |
| `R` | Record Number | 4 bytes | 32-bit record number / longint |
| `O` | Old BCD | up to 10 bytes | TAS Pro 3.0 BCD numeric (compatibility) |
| `V` | Overlay | variable | Overlay field (internal A type) |

### Type Coercion Rules

The interpreter automatically coerces types when needed:

| From → To | Rule |
|-----------|------|
| String → Numeric | Parse as double; 0 if invalid |
| String → Logical | `.T.` unless empty, `".F."`, or `"0"` |
| Numeric → String | Format with field's decimal places |
| Boolean → String | `".T."` or `".F."` |
| Any → Integer | Cast via `AsInteger()` |

### Defining Fields

Fields are defined using the `DEFINE` command. In CoTAS, all DEFINE statements are processed in the **first pass** - fields exist at compile time, not runtime:

```tas
DEFINE FIELD customer_name A 30
DEFINE FIELD balance N 10 2        ; 10 display chars, 2 decimals
DEFINE FIELD hire_date D
DEFINE FIELD active L
DEFINE FIELD items N 5 0 ARRAY 100 ; Array of 100 numeric elements
DEFINE FIELD counter I             ; Integer field
```

Full DEFINE syntax:

```tas
DEFINE name [,name2,...] [TYPE t] [SIZE n] [DEC d] [ARRAY sz] [RESET]
```

## The Token System

The CoTAS lexer recognizes **113 token types**. Here are the key categories:

### Keywords

**Field Definition:** `DEFINE`, `TYPE`, `SIZE`, `DEC`, `ARRAY`, `RESET`

**Control Flow:** `IF`, `THEN`, `ELSE`, `ELSE_IF`, `ENDIF`, `WHILE`, `ENDW`, `FOR`, `NEXT`, `GOTO`, `GOSUB`, `RET`, `SELECT`, `CASE`, `OTHERWISE`, `ENDC`, `SCAN`, `ENDS`

**Loop Control:** `EXIT`, `EXIT_IF`, `LOOP`, `LOOP_IF`, `FLOOP`, `FLOOP_IF`, `FEXIT`, `FEXIT_IF`, `SLOOP`, `SLOOP_IF`, `SEXIT`, `SEXIT_IF`

**I/O:** `SAY`, `AT`, `MSG`, `ENTER`, `ASK`, `CLRSCR`, `QUIT`

### Special Tokenization Rules

- **Comments** - `;` starts a comment (except inside `FOR` parentheses where `;` is a separator), `&&` is inline, `*` at line start
- **Strings** - Single or double quotes
- **Dot tokens** - `.T.`, `.F.`, `.A.`/`.AND.`, `.O.`/`.OR.`, `.N.`/`.NOT.`
- **Labels** - `identifier:` (trailing colon at start of line)
- **Keywords as identifiers** - TAS allows keywords as field names (e.g., `ASK`, `ENTER`, `TYPE`, `SIZE`, `ARRAY`, `AT` can be field names)
- **Parenthesis tracking** - Inside parentheses, `;` becomes a separator token, not a comment

## Operators

### Arithmetic Operators

| Operator | Operation |
|----------|-----------|
| `+` | Addition (numeric) or concatenation (string) |
| `-` | Subtraction or unary negation |
| `*` | Multiplication |
| `/` | Division |
| `^` | Exponentiation |

### Comparison Operators

| Operator | Meaning |
|----------|---------|
| `=` | Equal |
| `<>` | Not equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |
| `$` | Substring containment |

### Logical Operators

| Operator | Alternative | Meaning |
|----------|-------------|---------|
| `.A.` | `.AND.` | Logical AND |
| `.O.` | `.OR.` | Logical OR |
| `.N.` | `.NOT.` | Logical NOT (unary) |

### Operator Precedence (Expression Evaluator)

| Level | Operators | Associativity |
|-------|-----------|---------------|
| 1 (lowest) | `.OR.` | Left |
| 2 | `.AND.` | Left |
| 3 | `=`, `<>`, `<`, `<=`, `>`, `>=`, `$` | Left |
| 4 | `+`, `-` | Left |
| 5 (highest) | `*`, `/` | Left |

### Logical Constants

| Constant | Meaning |
|----------|---------|
| `.T.` | True |
| `.F.` | False |

## The Abstract Syntax Tree

The parser produces an AST with **21 statement types** and **6 expression types**:

### Statement Nodes

| Node | TAS Construct |
|------|---------------|
| `DefineStmt` | `DEFINE FIELD ...` |
| `AssignmentStmt` | `field = expression` |
| `SayStmt` | `SAY row,col text` |
| `LabelStmt` | `LABEL_NAME:` |
| `GotoStmt` | `GOTO label` |
| `GosubStmt` | `GOSUB label` |
| `ReturnStmt` | `RET [expression]` |
| `IfThenStmt` | `IF expr THEN stmt` (single-line) |
| `IfBlockStmt` | `IF ... ELSE_IF ... ELSE ... ENDIF` |
| `WhileStmt` | `WHILE expr ... ENDW` |
| `ForStmt` | `FOR(i;start;stop;step) ... NEXT` |
| `SelectStmt` | `SELECT expr CASE ... OTHERWISE ... ENDC` |
| `ScanStmt` | `SCAN ... ENDS` |
| `ExitStmt` | `EXIT`, `FEXIT`, `SEXIT` |
| `LoopStmt` | `LOOP`, `FLOOP`, `SLOOP` |
| `QuitStmt` | `QUIT` |
| `ClearScreenStmt` | `CLRSCR` |
| `MessageStmt` | `MSG text` |
| `ExpressionStmt` | Expression as statement |
| `PreprocessorStmt` | `#directive` |
| `GenericCommandStmt` | Any other command (dispatched via CommandRegistry) |

### Expression Nodes

| Node | Example |
|------|---------|
| `LiteralExpr` | `"hello"`, `42`, `3.14`, `.T.` |
| `IdentifierExpr` | `customer_name`, `balance` |
| `ArrayAccessExpr` | `prices[5]`, `names(i)` |
| `BinaryExpr` | `x + 1`, `name = "ACME"`, `a .AND. b` |
| `UnaryExpr` | `-x`, `.NOT. found` |
| `FunctionCallExpr` | `TRIM(name)`, `STR(amount,10,2)` |

## Expressions and Notation

TAS uses a concise notation system for describing parameter types:

| Notation | Meaning |
|----------|---------|
| `f/c/e` | Field, Constant, or Expression |
| `fn/v` | Field Name or Variable field |
| `file_expr` | File name (unquoted) or `@field` for dynamically opened files |
| `key_expr` | `@n` (key number), key name, or `@0` (direct access) |
| `lexpr` | Logical expression evaluating to `.T.` or `.F.` |
| `sac` | Special Alpha Constant - unquoted, starts A-Z, max 14 characters |
| `flist` | Array of F-type pointers (replaces explicit field list) |
| `scope` | `A` (All), `F` (First n), `N` (Next n), `R` (Rest) |
| `prt_where` | `S` (Screen), `P` (Printer), `D` (Disk), `A` (Ask user) |

## Comments and Line Continuation

```tas
; This is a comment (semicolon)
* This is also a comment (asterisk at start of line)
SAY 1,1 "Hello" && Inline comment (double ampersand)

; Line continuation with backslash:
DEFINE FIELD very_long_field_name \
  A 100
```

## Labels

Labels are identifiers ending with a colon, used as targets for `GOTO` and `GOSUB`:

```tas
START:
  SAY 1,1 "Main menu"
  ASK choice
  IF choice = "1"
    GOSUB PROCESS_ORDER
  ENDIF
  GOTO START

PROCESS_ORDER:
  SAY 3,1 "Processing..."
  RETURN
```

Labels are collected in the **first pass** of the interpreter, so forward references work - you can `GOTO` a label that appears later in the source.

<div class="callout new">
<div class="callout-title">⚡ User-Defined Functions &amp; Commands</div>
CoTAS extends TAS 5.1 with full UDF/UDC support. Define functions with parameters and return values using <code>FUNC</code>/<code>CMD</code>/<code>RET</code>:

<pre><code>FUNC CALC_TAX(subtotal, rate)
  tax = subtotal * rate
  RET tax
#endp

total = subtotal + CALC_TAX(subtotal, 0.08)
</code></pre>

Parameters are fields with automatic save/restore - nested calls are fully supported with call depth tracking.
</div>
