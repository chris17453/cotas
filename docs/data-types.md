# Data Types & Notation

## Field Types

| Type | Name | Internal Size | Description |
|------|------|---------------|-------------|
| `A` | Alphanumeric | max 4 GB | Text/string data |
| `N` | Numeric | 8 bytes | IEEE floating-point |
| `D` | Date | 4 bytes | Date value |
| `T` | Time | 4 bytes | Time value |
| `I` | Integer | 2 bytes | 16-bit signed integer |
| `B` | Byte | 1 byte | Single byte value |
| `F` | F-type Pointer | 5 bytes | Field pointer (use with `&` redirector) |
| `P` | P-type Pointer | 14 bytes | Program pointer |
| `R` | Record Number | 4 bytes | 32-bit longint / record number |
| `L` | Logical | 1 byte | Boolean `.T.` or `.F.` |
| `O` | Old BCD | max 10 bytes | TAS Pro 3.0 BCD numeric (compat) |
| `V` | Overlay | variable | Overlay field (internal A type) |

## Entry Type Notation

| Notation | Meaning |
|----------|---------|
| `f/c/e` | Field, Constant, or Expression |
| `fn/v` | Field Name or Variable field |
| `file_expr` | File name (unquoted) or `@field` for variable-opened files |
| `key_expr` | `@n` (key number), key name, or `@0` (direct access) |
| `lexpr` | Logical expression → `.T.` or `.F.` |
| `sac` | Special Alpha Constant — unquoted, starts A-Z, max 14 chars |
| `flist` | Array of F-type pointers (replaces explicit field list) |
| `scope` | `A` (All), `F` (First n), `N` (Next n), `R` (Rest) |
| `prt_where` | `S` (Screen), `P` (Printer), `D` (Disk), `A` (Ask) |

## Operators

| Operator | Meaning |
|----------|---------|
| `.A.` / `.AND.` | Logical AND |
| `.O.` / `.OR.` | Logical OR |
| `.N.` / `.NOT.` | Logical NOT |
| `.T.` | True |
| `.F.` | False |

## Special Syntax

- Integer constant suffix: `!` (e.g., `0!`)
- Continuation: `\\` at end of line
- Multiple commands per line: `\\|` separator
- Remarks: `;`, `*`, or `&&`
- Line labels: up to 14 chars ending with `:` (e.g., `START:`)
- Field redirector: `&` (e.g., `&fptr[n]`)
