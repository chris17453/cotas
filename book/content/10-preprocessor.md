The CoTAS preprocessor processes source directives before the parser builds the AST. These directives control file inclusion, procedure definition, and compiler flags.

## File Inclusion

### #lib - Library Include

```tas
#lib MYLIB
```

Includes a library (`.LIB`) file. The preprocessor searches the current directory first, then configured library paths. The included content is wrapped in `; --- #lib MYLIB ---` comments for traceability.

### #inc - Source Include

```tas
#inc COMMON.SRC
```

Inlines the contents of another source file at the current position. Searched in the current directory first, then library paths. Like `#lib`, wrapped in `; --- #inc COMMON.SRC ---` comments.

<div class="callout warning">
<div class="callout-title">⚠ Include Protection</div>
<strong>Max depth:</strong> 20 levels. The preprocessor tracks every included file by its full resolved path. If a circular include is detected (file A includes B which includes A), it is rejected immediately. On Linux, file resolution is case-insensitive for compatibility with DOS-era programs that used inconsistent casing.
</div>

## Procedure Definitions

### #proc / #endp - Named Procedures

```tas
#proc CALC_TAX
  tax = subtotal * tax_rate
  total = subtotal + tax
#endp

; Call it:
CALC_TAX
```

Procedures defined with `#proc`/`#endp` are passed through to the parser and handled by the interpreter. They share the program's field space.

<div class="callout new">
<div class="callout-title">⚡ FUNC/CMD vs #proc</div>
CoTAS introduces <code>FUNC</code> and <code>CMD</code> as modern alternatives to <code>#proc</code>. Unlike <code>#proc</code>, FUNC supports parameters with automatic save/restore and return values via <code>RET</code>. CMD works like GOSUB-based procedures. Both support nested calls with call depth tracking.
</div>

## Compiler Directives

All of these are passed through by the preprocessor to the parser/interpreter:

| Directive | Purpose |
|-----------|---------|
| `#udx` | User-defined extensions mode |
| `#udc` | User-defined commands mode |
| `#psc` | PSC directive |
| `#add_flds` | Enable runtime field addition via the `ADD` command |
| `#ext_fmt` | Use external screen/report format files instead of inline |

## Comment Handling

The preprocessor strips comments from directive arguments. Both `&&` and `;` comment styles are recognized:

```tas
#lib MYLIB        && This comment is stripped
#inc COMMON.SRC   ; This comment is also stripped
```

## How Preprocessing Fits the Pipeline

```
.SRC file
   │
   ▼
Preprocessor
   ├── Reads #lib directives → finds .LIB files → inlines content
   ├── Reads #inc directives → finds .SRC files → inlines content
   ├── Passes through #proc/#endp, #udx/#udc, #psc, #add_flds
   ├── Strips comments from directives
   ├── Enforces max depth (20) and circular include detection
   │
   ▼
Expanded source text (all includes resolved)
   │
   ▼
Lexer → Parser → AST → Interpreter
```

In CoTAS, preprocessing happens at parse time - not as a separate compilation step. The effect is identical: directives control what source gets parsed into the AST.
