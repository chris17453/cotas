TAS provides five structured programming constructs for controlling program flow. These can be nested up to 20 levels deep.

## IF / ELSE_IF / ELSE / ENDIF

The `IF` structure provides conditional execution. CoTAS supports both single-line and block forms:

**Single-line (IfThenStmt):**

```tas
IF x = 0 GOTO DONE
IF x > 100 THEN SAY 1,1 "Too high"
IF choice = "Q" RET
```

**Block form (IfBlockStmt):**

```tas
IF x = 100
  SAY 1,1 "One hundred"
ELSE_IF x = 200
  SAY 1,1 "Two hundred"
ELSE
  SAY 1,1 "Something else"
ENDIF
```

- The `DO` keyword after `IF expr` is optional - CoTAS accepts it either way
- Only the first branch whose condition evaluates to `.T.` is executed
- `ELSE_IF` and `ELSE` are optional
- Every block `IF` must have a matching `ENDIF`
- Nesting up to 20 levels deep

## WHILE / ENDWHILE

The `WHILE` loop repeats as long as its condition is true:

```tas
DEFINE FIELD counter N 3
counter = 1

WHILE counter <= 10
  SAY counter,1 "Line " + STR(counter,3,0)
  counter = counter + 1
ENDW
```

Loop control keywords:

| Keyword | Effect |
|---------|--------|
| `EXIT` | Break out of the innermost loop |
| `EXIT_IF lexpr` | Break if condition is true |
| `LOOP` | Skip to next iteration |
| `LOOP_IF lexpr` | Skip if condition is true |

<div class="callout info">
<div class="callout-title">ℹ️ Loop Protection</div>
CoTAS enforces a maximum of <strong>1,000,000 iterations</strong> per WHILE, FOR, or SCAN loop. If exceeded, an <code>InterpreterException</code> is thrown. This prevents infinite loops from hanging the web server in multi-user environments.
</div>

## FOR / NEXT

The `FOR` loop iterates a field through a range. CoTAS supports flexible syntax:

```tas
FOR(I;1;100;1)
  SAY 1,1 STR(I,5,0)
NEXT
```

Alternative forms accepted by the parser:

```tas
FOR(I=1;10;1)        ; Assignment form
FOR(I,1,100,1)       ; Comma separators
FOR(I;1;100;2)       ; Step of 2
```

The separator between parts can be either `;` (semicolon) or `,` (comma).

## SELECT / CASE / ENDCASE

The `SELECT/CASE` structure provides multi-way branching:

```tas
SELECT choice
  CASE "A"
    GOSUB ADD_RECORD
  CASE "E"
    GOSUB EDIT_RECORD
  CASE "D"
    GOSUB DELETE_RECORD
  CASE "Q"
    GOTO EXIT_PROGRAM
  OTHERWISE
    BELL
ENDC
```

- Each `CASE` value is tested against the `SELECT` expression
- `OTHERWISE` handles unmatched values (equivalent to `DEFAULT`)
- Only the first matching case executes

## SCAN / ENDSCAN

The `SCAN` structure iterates through database records with advanced filtering:

```tas
SCAN @1 KEY custid START "A" WHILE custid < "Z"
  SAY ROW(),1 cust_name
  SAY ROW(),35 STR(balance,10,2)
ENDS
```

Full SCAN syntax:

```tas
SCAN @handle KEY keyname START startval [WHILE cond] [FOR cond] [NLOCK] [SCOPE R|G]
```

| Option | Purpose |
|--------|---------|
| `WHILE cond` | Stop condition - loop ends when false |
| `FOR cond` | Skip condition - skip records where false |
| `NLOCK` | Don't lock records during iteration |
| `SCOPE R` | Rest of file from current position |
| `SCOPE G` | Global (all records) |

SCAN-specific loop control:

| Keyword | Effect |
|---------|--------|
| `SEXIT` | Exit the SCAN loop |
| `SEXIT_IF lexpr` | Exit SCAN if condition is true |
| `SLOOP` | Skip to next record |
| `SLOOP_IF lexpr` | Skip if condition is true |

Similarly, `FOR` loops have `FEXIT`, `FEXIT_IF`, `FLOOP`, and `FLOOP_IF`.

<div class="callout new">
<div class="callout-title">⚡ SCAN in CoTAS</div>
The SCAN statement automatically populates fields from each record, advances to the next record, and respects BREAK/CONTINUE/GOTO. It has built-in infinite loop protection (1M iterations) and supports multiple start key values.
</div>

<div class="callout tip">
<div class="callout-title">💡 Nesting</div>
All five structures can be freely nested within each other, up to a maximum depth of 20. The compiler validates proper nesting and reports errors for mismatched or unclosed structures.
</div>
