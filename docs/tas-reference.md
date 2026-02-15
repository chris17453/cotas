# TAS Professional 5.1 Quick Reference

This document is a comprehensive quick reference for the TAS Professional 5.1 language,
extracted from the official TAS Professional 5.1 Reference Guide. It is intended as knowledge
for the CoTAS interpreter implementation.

---

## Table of Contents

1. [Data Types](#data-types)
2. [Constants](#constants)
3. [Arrays](#arrays)
4. [Expressions and Operators](#expressions-and-operators)
5. [Compiler Directives](#compiler-directives)
6. [User Defined Functions (UDF)](#user-defined-functions-udf)
7. [User Defined Commands (UDC)](#user-defined-commands-udc)
8. [Control Flow](#control-flow)
9. [Field Commands](#field-commands)
10. [File I/O Commands](#file-io-commands)
11. [Screen/UI Commands](#screenui-commands)
12. [Report/Print Commands](#reportprint-commands)
13. [System Commands](#system-commands)
14. [All Commands (Categorized)](#all-commands-categorized)
15. [All Functions (Categorized)](#all-functions-categorized)
16. [Trap Names](#trap-names)
17. [Syntax Conventions](#syntax-conventions)
18. [Miscellaneous Syntax](#miscellaneous-syntax)

---

## Data Types

| Type | Name             | Internal Size      | Display Size | Notes |
|------|------------------|--------------------|-------------|-------|
| A    | Alpha            | 1 - 4GB            | 1 - 64K     | All displayable characters |
| N    | Numeric          | 8 bytes (IEEE 754) | 1-18        | 0-8 decimal places |
| B    | Byte             | 1 byte             | 1-3         | Value 0-255 |
| L    | Logical          | 1 byte             | n/a         | .T., .Y., .F., .N. |
| I    | Integer          | 2 bytes            | 1-5         | Value 0-65535 |
| R    | Record/Long Int  | 4 bytes            | 1-10        | Value 0-4,294,967,295 |
| D    | Date             | 4 bytes            | 5,7,8,10    | Btrieve format (day/month/year) |
| T    | Time             | 4 bytes            | 5,7,8,10,11,13 | Btrieve format (hundredths/sec/min/hr) |
| F    | F-Pointer        | 5 bytes            | n/a         | Internal field pointer |
| P    | P-Pointer        | 14 bytes           | n/a         | Full pointer with type/size info |
| O    | BCD (3.0 legacy) | 1-10 bytes         | 1-20        | Binary Coded Decimal (legacy) |
| V    | Overlay          | 1 - 4GB            | n/a         | Treated like A type, overlays memory |

### Internal Details
- **A (Alpha)**: 1 byte per character, no overhead
- **N (Numeric)**: IEEE floating point, always 8 bytes regardless of display size
- **I (Integer)**: Standard Intel low-byte-low format, 2 bytes
- **R (Record)**: Standard Intel format, 4 bytes, also usable as "long integer"
- **D (Date)**: Low byte = day, next byte = month, high word = year (binary)
- **T (Time)**: Low byte = hundredths, next = seconds, next = minutes, high = hours (binary)
- **L (Logical)**: Internally .T. = 1, .F. = 0

---

## Constants

### Alpha Constants
Surrounded by single or double quotes (must match):
```
'ABC'
"should've done it"
'123'
```

### Numeric Constants
Must have a decimal point followed by at least 1 digit:
```
100.0
234.5678
0.25
```

### Byte Constants
Followed by `B` or `b`, no decimals:
```
10b
255B
```

### Integer Constants
Can be followed by `!` for clarity, value < 65536:
```
10
1!
```

### Record Constants
Followed by `R` or `r`:
```
100r
192543R
```

### Logical Constants
`.T.` or `.Y.` for true, `.F.` or `.N.` for false. Must have at least one space before and after.

### Date/Time Constants
No special constants; set via alpha strings:
```
time_field = '10:10:00'
date_field = '10/10/90'
date_field = '10 10 1990'
```

---

## Arrays

Any field can be defined as an array. Arrays are single-dimensioned.
Element specifier uses square brackets `[]` immediately after field name (no space).
First element is 1. Maximum elements: 4.3 billion (DOS), 64K (Windows).

```
SALES[12]
SALES[MONTH[CNTR]]
COST_OF_GOODS[month(date())]
ARRAY[(cntr2-1)*24+cntr1]    ; simulated multi-dimension
```

---

## Expressions and Operators

### Mathematical Operators
Standard operator precedence (not left-to-right unless `#OLD_MATH` / `#PRO3`):
- `+` Addition / String concatenation
- `-` Subtraction
- `*` Multiplication (also trim-concatenation for strings)
- `/` Division
- `^` or `**` Exponentiation
- `%` Modulo

### Comparison Operators
- `=`  Equal
- `<>` or `!=` Not equal
- `<`  Less than
- `>`  Greater than
- `<=` Less than or equal
- `>=` Greater than or equal

### Logical Operators
- `.A.` or `.AND.` Logical AND
- `.O.` or `.OR.`  Logical OR
- `.N.` or `.NOT.` Logical NOT

### String Concatenation
- `+` Concatenates with trailing spaces preserved
- `*` Concatenates with trailing spaces trimmed from left operand

### Field Redirector
- `&` Indirects through a pointer field: `&fldptr` accesses the field pointed to by `fldptr`

### File Reference
- `@file_num` References a file by its file number variable
- `@0` Direct access (no key)
- `@3` Key number 3

---

## Compiler Directives

Compiler directives start with `#` as the first character on a line. One directive per line.

| Directive       | Description |
|----------------|-------------|
| `#ADD_FLDS n`  | Add n extra field slots to field list for runtime ADD command |
| `#ALL_LOC`     | All subsequent DEFINE fields are LOCAL (requires PROC/ENDP) |
| `#CHK_UP_VLD`  | Check VALID option even when Up Arrow is pressed in ENTER |
| `#ENDP`        | End of PROC scope; terminates LOCAL field scope |
| `#EXT_FMT`     | Screen/report formats are separate files, not embedded in source |
| `#FMT`         | Use old-style screen/report formats |
| `#INC file`    | Include source file (appended at end during compilation) |
| `#LIB file`    | Include library file (e.g., `#LIB TASRTNS`) |
| `#OLD_MATH`    | Evaluate expressions left-to-right (no operator precedence) |
| `#PRO3`        | TAS Professional 3.0 compatibility mode |
| `#PROC`        | Begin procedure scope for LOCAL fields |
| `#PSC`         | (Program Source Code related) |
| `#SFLDS n`     | Add n screen field buffer slots (for SAY...AT command) |
| `#TDATA n`     | Set temporary data buffer size in bytes (default 64K) |
| `#UDC`         | Enable User Defined Commands |
| `#UDF`         | Enable User Defined Functions |
| `#UDX`         | Enable both UDCs and UDFs |
| `#XLATE`       | Enable translation file for command/option names |

---

## User Defined Functions (UDF)

### Syntax
```
FUNC function_name param1, param2, ..., paramN
  ; function body (any TAS commands)
  RET return_value
```

### Rules
- Requires `#UDF` or `#UDX` compiler directive before first reference
- Name: up to 14 characters (special alpha constant), must not duplicate a label name
- Maximum 20 parameters, separated by commas
- Ends with `RET value` -- returns a single value of any type (including expressions)
- Fields within UDF are NOT automatically local; use PUSHF/POPF for reentrancy, or #PROC/#ENDP with LOCAL
- Can be called as a function: `result = MY_FUNC(arg1, arg2)`
- Can also be called as a subroutine: `GOSUB MY_FUNC` (no parens, no args)
- After GOSUB to a UDF, use `RETVAL()` to get the return value

### Example
```
#udf
define x type n
clrscr
x = user_func(1, 3, 'A')
? x
quit

define y, z, a type n
define do type a size 1
func user_func y, z, do
  if do = 'A'
    a = y + z
  else_if do = 'S'
    a = y - z
  else_if do = 'M'
    a = y * z
  else_if do = 'D'
    a = y / z
  endif
  ret a
```
Output: `4.00`

---

## User Defined Commands (UDC)

### Definition Syntax
```
CMD command_name param1, param2, ..., paramN
  ; command body
  RET
```

### Invocation Syntax
```
command_name arg1, arg2, ..., argN
```
Note: Arguments are separated by commas, NOT surrounded by parentheses (unlike UDFs).

### Rules
- Requires `#UDC` or `#UDX` compiler directive before first reference
- Name: up to 14 characters, must not duplicate a label name
- Maximum 20 parameters
- At least one space between command name and arguments
- Ends with `RET` (no return value, unlike UDF)

---

## Control Flow

### IF / ELSE_IF / ELSE / ENDIF
```
IF logical_expression
  ; commands if true
ELSE_IF logical_expression
  ; commands if second condition true
ELSE
  ; commands if all conditions false
ENDIF
```

Variants:
- `IF cond THEN single_command` (single-line IF)
- `IFDUP` - test if current record is a duplicate
- `IFNA(file_num)` - test if no active record (function, not command)

### WHILE / ENDW
```
WHILE logical_expression
  ; loop body
  EXIT          ; unconditional exit
  EXIT_IF expr  ; conditional exit
  LOOP          ; unconditional continue (back to WHILE)
  LOOP_IF expr  ; conditional continue
ENDW
```

### FOR / NEXT
```
FOR counter = start TO stop STEP step_value
  ; loop body
  EXIT          ; unconditional exit
  EXIT_IF expr  ; conditional exit
  LOOP          ; unconditional continue (back to FOR)
  LOOP_IF expr  ; conditional continue
NEXT
```
- Counter is I type
- If loop runs to completion, counter is 1 past stop value (in 5.1; equals stop in 3.0)
- Step defaults to 1; can be negative

### SELECT / CASE / OTHERWISE / ENDC
```
SELECT expression       ; expression must resolve to I type
  CASE int_value
    ; commands for this case
  CASE int_value
    ; commands for another case
  OTHERWISE
    ; default commands
ENDC
```
- Only the first matching CASE is executed
- OTHERWISE is optional (like ELSE)
- Missing ENDC produces "If without Endif" compiler error

### SCAN / ENDS
```
SCAN file/@file_num KEY key/@key_num START start_val \
     SCOPE scope scope_val FOR for_expr WHILE while_expr \
     DISP NLOCK REV
  ; loop body (record automatically found each iteration)
  SEXIT          ; unconditional exit
  SEXIT_IF expr  ; conditional exit
  SLOOP          ; unconditional continue
  SLOOP_IF expr  ; conditional continue
ENDS
```
- Combines FIND + WHILE loop
- Automatically finds next record each iteration
- FOR filter: skips non-matching records, continues to EOF
- WHILE filter: stops loop on first non-match
- REV: scan in reverse order
- DISP: redisplay screen fields each record
- NLOCK: don't lock records

### GOTO / GOSUB / RET
```
GOTO label_name
GOTOL line_number          ; goto by line number (I type)
GOSUB label_name
GOSUBL line_number         ; gosub by line number
RET                        ; return from GOSUB
RET value                  ; return from UDF with value
POPS                       ; pop stack (remove GOSUB return address)
```

### ON
```
ON expression GOTO label1, label2, ..., labelN
ON expression GOSUB label1, label2, ..., labelN
```

### Code Process Controls
```
{
  ; code within braces is skipped during normal flow
  ; but callable via UDF references in PRE/POST
}
```

---

## Field Commands

### DEFINE
```
DEFINE field_list TYPE type SIZE size DEC dec ARRAY num_elements \
       PICT picture DUP dup_field UP RESET LOCAL INIT val1,val2,...
```
- Define up to 10 fields in one command with same specs
- Types: A, N, D, T, I, B, F, P, R, L, O, V
- Default type is N if not specified
- LOCAL: field scope limited to PROC/ENDP boundaries
- RESET: match field from previous (chained-from) program
- INIT: set initial values (constants only; A type without quotes, D/T with quotes)

### ADD
```
ADD field_name TYPE type SIZE size DEC dec ARRAY num \
    UP upcase PICT pict FILE file KEY key OFST offset FPTR ptr_field
```
- Add fields at runtime (requires `#ADD_FLDS` directive)
- Returns F-type pointer in FPTR field for access via `&ptr`

### ALLOC (Allocate Field)
```
ALLOC field TYPE type SIZE size DEC dec ARRAY num UP upcase PICT pict
```
- Reallocate a previously DEFINE'd field with new specs

### REDEF (Redefine)
```
REDEF field TYPE type SIZE size DEC dec FILE file OFST offset KEY key \
      PICT pict UP upcase LOC location_field
```

### DEALLOC (Deallocate / Remove Array)
```
REMVA field_name
```

### EQUAL (Assignment)
```
field = expression
field = expression     ; standard assignment
```

### MID (Stuff)
```
MID field, start, length, replacement_value
```
- Replace portion of alpha field

### Other Field Commands
| Command | Syntax | Description |
|---------|--------|-------------|
| `DELC`  | `DELC field AT start LEN length` | Delete characters from alpha field |
| `FILL`  | `FILL field WITH char` | Fill field with character |
| `TRIM`  | `TRIM field` | Trim spaces from field |
| `UPCASE`| `UPCASE field` | Convert field to uppercase |
| `INSRT` | `INSRT field AT position WITH value` | Insert characters into alpha field |
| `PTR`   | `PTR field TO target_field` | Set pointer field |
| `MOVE`  | `MOVE FROM source TO dest LEN length` | Move data in memory |
| `INC`   | `INC field` / `INC field BY value` | Increment field |
| `DEC`   | `DEC field` / `DEC field BY value` | Decrement field |
| `PUSHF` | `PUSHF field_list` | Push field values onto internal stack |
| `POPF`  | `POPF field_list` | Pop field values from stack (LIFO) |
| `FORMAT`| `FORMAT field PICT picture` | Set display format/picture |

---

## File I/O Commands

### OPEN (Legacy)
```
OPEN filename EXT ext LOCK lock_type ERR err_label OWNER owner PATH path FD fd
```
- Opens file by name (special alpha constant, no quotes)
- Reference file in other commands by name (not number)
- Max 32 files open at once (across all chained programs)

### OPENV (Open Variable) -- Preferred
```
OPENV filename FNUM file_num TYPE file_type EXT ext LOCK lock_type \
      ERR err_label OWNER owner PATH path FD fd SIZE size BUFF buff CREATE NOCLR
```
- `filename`: f/c/e (can be expression or alpha constant with quotes)
- `file_num`: fn/v, I type, receives the file handle
- `file_type`: T (TAS/Btrieve), D (dBASE III+), F (Fixed/SDF), X (Text), B (non-TAS Btrieve)
- `lock_type`: N (none), R (record), F (file), X (fix/read-only), A (accelerated)
- `CREATE`: create file if not found

### CLOSE
```
CLOSE filename/@file_num
```

### FIND (Legacy)
```
FIND find_type SRCH key_name REL related_field ERR err_label \
     FOR filter NLOCK KEYO NOCLR
```
- `find_type`: M (match), G (generic/>=), F (first), L (last), N (next), P (previous), R (related)

### FINDV (Find Variable) -- Preferred
```
FINDV find_type FNUM file_num KEY key/@key_num \
      VAL search_value ERR err_label FOR filter NLOCK KEYO NOCLR
```
- Same find types as FIND
- Uses file number instead of file name

### SAVE (Save Record)
```
SAVE filename/@file_num NOCNF NOCLR GOTO goto_label ERR err_label
```
- NOCNF: don't confirm with user
- NOCLR: don't clear record buffer after save

### DEL (Delete Record)
```
DEL filename/@file_num NOCNF ERR err_label
```

### CLR (Clear Record Buffer)
```
CLR filename/@file_num REC/FLD
```
- REC: clear entire record buffer
- FLD: clear individual field

### Other File Commands
| Command | Syntax | Description |
|---------|--------|-------------|
| `SRCH`  | `SRCH filename/@file_num KEY key` | Set default search file/key |
| `RELATE`| `RELATE file1 TO file2 KEY key` | Set automatic file relation |
| `FILTER`| `FILTER expression` | Set record filter (DOS only) |
| `REOPEN`| `REOPEN filename/@file_num` | Reopen a file |
| `SETACT`| `SETACT fd_name FILE file_name` | Set active file for shared FD |
| `OWNER` | `OWNER filename/@file_num code SET/CLR` | Set/clear file owner code |
| `DELALL`| `DELALL filename/@file_num` | Delete all records |
| `RDA`   | `RDA field_list TO array_list FILE file KEY key ...` | Read records into arrays |
| `WRA`   | `WRA field_list FROM array_list FILE file KEY key ...` | Write arrays to records |
| `LISTF` | `LISTF field_list FILE file KEY key ...` | Interactive file list/browse |
| `LISTM` | `LISTM field_list ...` | Interactive array list |
| `IMPORT`| `IMPORT field_list FROM file TYPE type` | Import from non-TAS file |
| `EXPORT`| `EXPORT field_list MEM FILE file KEY key` | Export to non-TAS file |
| `REPLACE`| `REPLACE field_list FILE file KEY key` | Replace field values in records |
| `RCN`   | `RCN file_num field GET/SET` | Get/set record number |
| `READ`  | `READ file_num START pos NCHR num TO field` | Read from non-TAS file |
| `WRITE` | `WRITE file_num START pos NCHR num FROM field` | Write to non-TAS file |
| `TRANS` | `TRANS type` | Begin/end/rollback transactions |
| `ULKALL`| `ULKALL` | Unlock all records |
| `RLCK`  | (trap name, not command) | Record lock handling via TRAP |

---

## Screen/UI Commands

### MOUNT
```
MOUNT format_name TYPE format_type PTO print_to PFN file EXTERN
```
- `format_type`: S (screen), R (report)
- Mounts a screen/report format compiled with the program
- Screen fields are then available for ENTER without AT coordinates

### SAY
```
SAY fieldname AT column, row COLOR color PICT picture
```
- Add a field to the screen buffer (like adding to mounted screen)

### PMSG (Print Message / ?)
```
PMSG message_list AT column, row WAIT NOCR PTW print_to \
     ENTER enter_field COLOR color ABS
```
- `?` is alias for PMSG
- Display message to screen, printer, or default device
- WAIT: pause for keypress after display
- NOCR: no carriage return after output
- ENTER: allow user entry after message

### ENTER
```
ENTER fieldname MASK mask HELP help AT col,row COLOR color \
      PRE pre_expr POST post_expr DFLT default VLD valid VLDM msg \
      ARRAY CNTR counter ENUM enum_list DO udf \
      UPAR up_label GROUP group UP ACR PSWD NOREV AUTO_SRCH
```
- Primary data entry command
- PRE/POST: UDF expressions executed before/after entry
- VLD: validation expression; if .F., displays VLDM and re-enters
- ENUM: enumerated list of valid values (PgUp/PgDn to cycle)
- DO: execute UDF for entry (F2 or Ctrl-Home)
- ARRAY: enter all elements of array field
- PSWD: password mode (hidden characters)
- UP: force uppercase

### ASK
```
ASK question DFLT default_response
```
- Y/N question; test result with `ASK()` function

### WINDOW
```
WINDOW AT col, row LEN length WDT width WCOLOR color \
       TTL title BOX box BCOLOR box_color SHD shadow SCOLOR shadow_color
```
- Open a window at specified position

### WINDEF (Window Define)
```
WINDEF AT col, row LEN length WDT width WCOLOR color \
       TTLW where TTL title BOX box BCOLOR bcolor SHD shadow \
       SCOLOR scolor ICOLOR icolor SAVEF save_field
```
- Define a window for later activation

### WINACT (Window Activate)
```
WINACT save_field
```
- Activate a previously defined window

### Other Screen Commands
| Command | Syntax | Description |
|---------|--------|-------------|
| `CLRSCR` | `CLRSCR` | Clear screen |
| `CLRLN`  | `CLRLN AT row` | Clear a screen line |
| `CLRSF`  | `CLRSF` | Clear screen fields |
| `CURSOR` | `CURSOR AT col, row` | Position cursor |
| `RSCR`   | `RSCR` | Reset screen |
| `SAVES`  | `SAVES field` | Save screen to field/buffer |
| `REDSP`  | `REDSP field` | Redisplay saved screen |
| `PAINT`  | `PAINT AT col,row LEN len WDT wdt COLOR color` | Paint area with color |
| `SCROLL` | `SCROLL AT col,row LEN rows WDT cols DIR direction NCHR num` | Scroll screen area |
| `SCRCHR` | `SCRCHR AT col,row DISP field ATRB attr GET/SET` | Get/set screen character |
| `SCRN`   | `SCRN L/U/R/S` | Lock/Unlock/Refresh/Reset screen |
| `WRAP`   | `WRAP field AT col,row LEN len WDT wdt` | Word-wrap display |
| `REWRAP` | `REWRAP` | Re-wrap field |
| `REENT`  | `REENT` | Re-enter (redisplay entry) |
| `NOVLDMSG`| `NOVLDMSG` | Suppress valid message |
| `BKG`    | `BKG color` | Set background color |
| `FG`     | `FG color` | Set foreground color |
| `REV`    | `REV` | Set reverse video |
| `FRG`    | `FRG` | Set foreground color (alias) |
| `NMENU`  | `NMENU ...` | 4.0-style menu |
| `MENU`   | `MENU ...` | 3.0-style menu |

### Color Control
```
BKG color_value     ; set background color
FG color_value      ; set foreground color
REV                 ; toggle reverse video
```

### List Commands
| Command | Description |
|---------|-------------|
| `LISTF`  | Interactive file record list/browse |
| `LISTM`  | Interactive memory array list |
| `AUTOINC`| Auto-increment in list |
| `AUTODEC`| Auto-decrement in list |
| `AUTOENTER` | Auto-enter in list |
| `RDLIST` | Redisplay list |
| `NOREDSP`| No redisplay |
| `NORSTRT`| No restart |
| `LIST_EXIT` | Exit from list |
| `SETLINE`| Set first/title lines for LISTF |

### Messages
| Command | Syntax | Description |
|---------|--------|-------------|
| `PMSG` / `?` | `? message_list` | Print/display message |
| `DSPMSG` | `DSPMSG message` | Display message in message area |
| `ASK`   | `ASK question` | Y/N question |
| `ERROR` | `ERROR error_num/message` | Display error message |
| `CLRPE` | `CLRPE` | Clear program error |

---

## Report/Print Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| `MOUNT`  | `MOUNT fmt TYPE R PTO where` | Mount report format |
| `PFMT`   | `PFMT line_number` | Print format line |
| `PBLK`   | `PBLK num_lines` | Print blank lines |
| `PCHR`   | `PCHR string` | Print character string/control chars |
| `PON`    | `PON` / `POFF` | Printer on/off |
| `PTOF`   | `PTOF` | Print top of form (form feed) |
| `PVTAB`  | `PVTAB row` | Print vertical tab |
| `PRT_NUM`| `PRT_NUM number` | Set printer number |
| `PRT_SET`| `PRT_SET option value` | Set printer options |
| `LD_PDRV`| `LD_PDRV driver_file` | Load printer driver |
| `CLSPF`  | `CLSPF` | Close print-to-disk file |
| `RPTFMT` | `RPTFMT` | Report format control |
| `PRTALL` | `PRTALL ON/OFF` | Print all on/off |

---

## System Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| `CHAIN`  | `CHAIN program WITH field_list NOBASEWIND` | Execute another TAS program |
| `CHAIN_RAP` | `CHAIN_RAP program` | Chain with Run Anytime Program |
| `RAP`    | `RAP program` | Run Anytime Program |
| `EXEC`   | `EXEC program WITH params` | Execute non-TAS program |
| `PARAM`  | `PARAM field_list` | Receive parameters from CHAIN |
| `COMPRG` | `COMPRG source PTO where ERR err DEBUG DICT dict` | Compile program |
| `CO`     | `CO code GET/SET` | Get/set company code |
| `QUIT`   | `QUIT level` | Exit program (0=DOS, 1=first program, etc.) |
| `DATE`   | `DATE date_field GET/SET` | Get/set system date |
| `TIME`   | `TIME time_field GET/SET` | Get/set system time |
| `CLOCK`  | `CLOCK AT col,row` | Display clock on screen |
| `BELL`   | `BELL` | Sound bell |
| `SOUND`  | `SOUND freq, duration` | Play sound (DOS) |
| `BREAK`  | `BREAK ON/OFF` | Enable/disable Ctrl-Break |
| `KBDUP`  | `KBDUP ON/OFF` | Keyboard uppercase on/off |
| `MOUSE`  | `MOUSE ON/OFF` | Mouse on/off |
| `TRACE`  | `TRACE ON/OFF` | Program trace on/off |
| `INT`    | `INT interrupt_num` | Execute DOS interrupt |
| `PEEK`   | `PEEK seg, offset TO field` | Read memory location |
| `POKE`   | `POKE seg, offset FROM field` | Write memory location |
| `DELF`   | `DELF filename` | Delete a file |
| `RENF`   | `RENF old_name TO new_name` | Rename a file |
| `INITF`  | `INITF filename` | Initialize (empty) a TAS file |
| `CHGDP`  | `CHGDP path` | Change dictionary path |
| `REMARK` | `; text` or `&& text` or `* text` | Comment/remark |

---

## All Commands (Categorized)

### Program Control
| Command | Description |
|---------|-------------|
| `IF` / `ELSE_IF` / `ELSE` / `ENDIF` | Conditional execution |
| `SELECT` / `CASE` / `OTHERWISE` / `ENDC` | Case/switch structure |
| `WHILE` / `EXIT` / `EXIT_IF` / `LOOP` / `LOOP_IF` / `ENDW` | While loop |
| `FOR` / `EXIT` / `EXIT_IF` / `LOOP` / `LOOP_IF` / `NEXT` | For loop |
| `SCAN` / `SEXIT` / `SEXIT_IF` / `SLOOP` / `SLOOP_IF` / `ENDS` | File scan loop |
| `GOTO` / `GOTOL` | Transfer control to label/line |
| `GOSUB` / `GOSUBL` | Subroutine call to label/line |
| `ON ... GOTO/GOSUB` | Computed goto/gosub |
| `RET` | Return from subroutine/UDF |
| `POPS` | Pop stack (remove GOSUB return) |
| `TRAP` / `PUSHT` / `POPT` / `XTRAP` | Trap management |
| `UPAR` | Up arrow control |
| `GETLBL` | Get label line number |
| `LABEL:` | Line label (ends with colon) |

### Field Management
| Command | Description |
|---------|-------------|
| `DEFINE` | Create program-local fields |
| `ADD` | Add fields at runtime |
| `ALLOC` | Reallocate field (change type/size) |
| `REDEF` | Redefine field properties |
| `DEALLOC` / `REMVA` | Deallocate/remove array |
| `=` (EQUAL) | Assignment |
| `MID` (STUFF) | Replace portion of alpha field |
| `DELC` | Delete characters |
| `FILL` | Fill field with character |
| `TRIM` | Trim field spaces |
| `UPCASE` | Convert to uppercase |
| `INSRT` | Insert characters |
| `PTR` | Set pointer |
| `MOVE` | Move data in memory |
| `INC` | Increment |
| `DEC` | Decrement |
| `PUSHF` | Push field values to stack |
| `POPF` | Pop field values from stack |
| `FORMAT` | Set field display format |

### File I/O
| Command | Description |
|---------|-------------|
| `OPEN` | Open file (legacy) |
| `OPENV` | Open file (variable/preferred) |
| `CLOSE` | Close file |
| `FIND` | Find record (legacy) |
| `FINDV` | Find record (variable/preferred) |
| `SAVE` | Save record |
| `DEL` | Delete record |
| `CLR` | Clear record buffer |
| `SRCH` | Set search file/key |
| `RELATE` | Set file relation |
| `FILTER` | Set record filter |
| `REOPEN` | Reopen file |
| `SETACT` | Set active file |
| `OWNER` | File owner code |
| `DELALL` | Delete all records |
| `RDA` | Read records to arrays |
| `WRA` | Write arrays to records |
| `REPLACE` | Replace field values in records |
| `RCN` | Get/set record number |
| `READ` | Read from non-TAS file |
| `WRITE` | Write to non-TAS file |
| `IMPORT` | Import from file |
| `EXPORT` | Export to file |
| `TRANS` | Transaction control |
| `ULKALL` | Unlock all records |

### User Interface
| Command | Description |
|---------|-------------|
| `ENTER` | Data entry |
| `PMSG` / `?` | Print/display message |
| `SAY` | Add field to screen |
| `ASK` | Y/N question |
| `ERROR` | Display error |
| `DSPMSG` | Display message |
| `MOUNT` | Mount screen/report format |
| `REMOUNT` | Remount format |
| `WINDOW` | Open window |
| `WINDEF` | Define window |
| `WINACT` | Activate window |
| `SAVES` | Save screen |
| `REDSP` | Redisplay screen |
| `CLRSCR` | Clear screen |
| `CLRLN` | Clear line |
| `CLRSF` | Clear screen fields |
| `CURSOR` | Position cursor |
| `RSCR` | Reset screen |
| `PAINT` | Paint screen area |
| `SCROLL` | Scroll screen area |
| `SCRCHR` | Get/set screen character |
| `SCRN` | Screen lock/unlock/refresh/reset |
| `WRAP` | Word-wrap display |
| `REWRAP` | Re-wrap |
| `REENT` | Re-enter |
| `NOVLDMSG` | Suppress valid message |
| `LISTF` | File list/browse |
| `LISTM` | Array list |
| `AUTOINC` / `AUTODEC` / `AUTOENTER` | List auto features |
| `RDLIST` | Redisplay list |
| `LIST_EXIT` | Exit list |
| `NOREDSP` / `NORSTRT` | List display control |
| `SETLINE` | Set list title lines |
| `BKG` / `FG` / `REV` / `FRG` | Color control |
| `NMENU` / `MENU` | Menu commands |

### Reports/Printing
| Command | Description |
|---------|-------------|
| `PFMT` | Print format line |
| `PBLK` | Print blank lines |
| `PCHR` | Print character string |
| `PON` / `POFF` | Printer on/off |
| `PTOF` | Top of form |
| `PVTAB` | Vertical tab |
| `PRT_NUM` | Set printer number |
| `PRT_SET` | Set printer options |
| `LD_PDRV` | Load printer driver |
| `CLSPF` | Close print-to-disk file |
| `PRTALL` | Print all on/off |

### System
| Command | Description |
|---------|-------------|
| `CHAIN` | Execute TAS program |
| `RAP` | Run Anytime Program |
| `EXEC` | Execute non-TAS program |
| `PARAM` | Receive CHAIN parameters |
| `COMPRG` | Compile program |
| `CO` | Company code |
| `QUIT` | Exit program |
| `DATE` / `TIME` / `CLOCK` | Date/time commands |
| `BELL` / `SOUND` | Audio |
| `BREAK` | Ctrl-Break control |
| `KBDUP` | Keyboard uppercase |
| `MOUSE` | Mouse control |
| `TRACE` | Program trace |
| `INT` / `PEEK` / `POKE` | Low-level system |
| `DELF` / `RENF` / `INITF` | File management |
| `CHGDP` | Change dictionary path |
| `CMD` | Define UDC |
| `FUNC` | Define UDF |
| `;` / `&&` / `*` | Remark/comment |
| `#directive` | Compiler directive |

---

## All Functions (Categorized)

### Math Functions
| Function | Return | Description |
|----------|--------|-------------|
| `ABS(n)` | N | Absolute value |
| `CEIL(n)` | N | Ceiling (round up to whole) |
| `FLOOR(n)` | N | Floor (round down to whole) |
| `INT(n)` | N | Integer portion (truncate decimals) |
| `ROUND(n, dec)` | N | Round to dec decimal places |
| `SIGN(n)` | N | Returns -1, 0, or 1 |
| `SQRT(n)` | N | Square root |
| `EXP(n)` | N | e^n |
| `LOG(n)` | N | Natural log (base e) |
| `LOG10(n)` | N | Log base 10 (DOS only) |
| `PI()` | N | Value of PI (3.14159265) |
| `MAX(a, b)` | ? | Maximum of two values |
| `MIN(a, b)` | ? | Minimum of two values |
| `RNDM()` | N | Random number |
| `MOD(n, d)` | N | Modulo |

### Trigonometric Functions
| Function | Return | Description |
|----------|--------|-------------|
| `SIN(n)` | N | Sine (radians) |
| `COS(n)` | N | Cosine (radians) |
| `TAN(n)` | N | Tangent (radians) |
| `ASIN(n)` | N | Arcsine (-PI/2 to PI/2) |
| `ACOS(n)` | N | Arccosine |
| `ATAN(n)` | N | Arctangent (-PI/2 to PI/2) |
| `ATAN2(y, x)` | N | Arctangent of y/x (-PI to PI, DOS only) |
| `DTOR(n)` | N | Degrees to radians |
| `RTOD(n)` | N | Radians to degrees (but note: also R-type to D-type conversion) |

### String Functions
| Function | Return | Description |
|----------|--------|-------------|
| `MID(str, start, len)` | A | Substring |
| `LOC(search, within, start, len, igncase)` | I | Find string position (instr) |
| `TRIM(str, 'T'/'L')` | A | Trim trailing/leading spaces |
| `UP(str)` | A | Convert to uppercase |
| `LOW(str)` | A | Convert to lowercase |
| `PROP(str)` | A | Proper case (capitalize words) |
| `FILL(size, char)` | A | Create string of repeated chars |
| `CHR(n)` | A | ASCII value to character |
| `ASC(str)` | I | First character to ASCII value |
| `STR(n)` | A | Number to string |
| `VAL(str)` | N | String to number |
| `HEX(n)` | A | Number to hex string |
| `SNDX(str)` | A | Soundex code |
| `DIFF(str1, str2)` | I | Soundex similarity (0-4) |
| `WRAP(str)` | A | Word wrap related |
| `WRAPL(str)` | A | Wrap length |
| `WRAPO(str)` | A | Wrap offset |
| `WRAPS(str)` | A | Wrap string |
| `SIZE(field, 'I'/'D'/'A')` | I | Field size (internal/display/actual) |
| `LSTCHR()` | I | ASCII value of last char entered |

### Type Conversion Functions
| Function | Return | Description |
|----------|--------|-------------|
| `CFLT(val)` | N | Convert to N (float) type |
| `CINT(val)` | I | Convert to I (integer) type |
| `CBYT(val)` | B | Convert to B (byte) type |
| `CREC(val)` | R | Convert to R (record/long) type |
| `CTOD(str)` | D | Alpha string to date |
| `CTOL(str)` | L | Alpha string to logical |
| `CTOT(str)` | T | Alpha string to time |
| `DTOS(date)` | A | Date to sortable string (YYYYMMDD) |
| `TTOC(time)` | A | Time to string |
| `TTOF(time)` | N | Time to float |
| `TTOR(time)` | R | Time to record/long |
| `FTOT(n)` | T | Float to time |
| `RTOD(r)` | D | Record/long to date |
| `RTOT(r)` | T | Record/long to time |
| `RTP(r)` | P | Record/long to pointer |
| `LTOC(l)` | A | Logical to string |

### Date/Time Functions
| Function | Return | Description |
|----------|--------|-------------|
| `DATE()` | D | Current system date |
| `TIME()` | T | Current system time |
| `DOW(date)` | I | Day of week (1=Sunday) |
| `DOM(date)` | I | Day of month |
| `MNTH(date)` | I | Month number |
| `YEAR(date)` | I | Year |
| `CDOW(date)` | A | Character day of week ("MONDAY") |
| `CMNTH(date)` | A | Character month ("JANUARY") |
| `MDY(date, 'S'/'L')` | A | "Jun 23, 94" or full format |
| `DMY(date, 'S'/'L')` | A | "23 Jun 94" or full format |
| `FLDATE(filename)` | D | File last modified date |
| `FLTIME(filename)` | T | File last modified time |
| `FLSZE(filename)` | R | File size in bytes |

### File/Record Functions
| Function | Return | Description |
|----------|--------|-------------|
| `FNUM(filename)` | I | Get file number for opened file |
| `FLERR(file_num)` | I | Last file error number (0=OK) |
| `EOF(file_num)` | L | End of file? |
| `BOF(file_num)` | L | Beginning of file? |
| `IFNA(file_num)` | L | No active record? (.T. if no record) |
| `TRC(file_num)` | R | Total record count |
| `RSIZE(file_num)` | I | Record size |
| `RCN(file_num)` | R | Get record number |
| `LCKD()` | L | Is current record locked? |
| `OPEN()` | I | Last OPEN error number |
| `FFILE(pattern, 'F'/'N')` | A | Find file (first/next) |
| `FTYP(field)` | A | Field type character |
| `FFLD(name, 'P'/'F'/'L')` | P/F/L | Get field pointer |
| `FLDNME(fptr)` | A | Get field name from F-pointer |
| `FLDFDNUM(fptr)` | I | Get file handle for field |
| `NUMFLDS()` | I | Number of fields in program |
| `NUMLBLS()` | I | Number of labels in program |
| `GFLD(file_num, delim, pos, type)` | ? | Get field from delimited buffer |
| `GFL(line_num)` | A | Get format line as string |
| `MID_REC(file_num, start, len)` | A | Get portion of active record |
| `GET_REC(buffer, rec_num)` | A | Get record from text buffer |

### UI/Input Functions
| Function | Return | Description |
|----------|--------|-------------|
| `ASK()` | L | Last ASK answer (.T.=Yes) |
| `ESC()` | L | Was ESC pressed? |
| `ENTER()` | L | POST expression result |
| `INKEY(wait)` | I | Check keyboard (0=no key, or ASCII) |
| `IFCR()` | L | Was Enter key pressed? |
| `VARREAD()` | A | Name of current/last entry field |
| `LSTCHR()` | I | ASCII of last character entered |
| `COL()` | I | Current screen column |
| `ROW()` | I | Current screen row |
| `PCOL()` | I | Current printer column |
| `PROW()` | I | Current printer row |
| `MCOL()` | I | Mouse column |
| `MROW()` | I | Mouse row |
| `LROW()` | I | Current row in LISTF/LISTM |
| `ISCLR()` | L | Is color monitor? |
| `ISAL(str)` | L | Is first char alphabetic? |
| `ISLO(str)` | L | Is first char lowercase? |
| `ISUP(str)` | L | Is first char uppercase? |
| `ISNUM(str)` | L | Is first char numeric? |
| `CLICKED_ON()` | L | Was field clicked? (Windows) |
| `IIF(cond, true_val, false_val)` | ? | Inline if |
| `RETVAL()` | ? | Return value from GOSUB to UDF |
| `PSTAT()` | I | Printer status |

### System Functions
| Function | Return | Description |
|----------|--------|-------------|
| `PERR()` | I | Last program error number |
| `EXEC(program)` | I | Execute and return result |
| `GETENV(var)` | A | Get environment variable |
| `OS()` | A | Operating system name/version |
| `VER()` | A | TAS Professional version |
| `MEM()` | I | Available memory in KB |
| `CPATH()` | A | Current path |
| `TPATH()` | A | TAS default path |
| `DPATH()` | A | Dictionary path |
| `ACS()` | A | Current ACS file name |
| `CO()` | A | Current company code |
| `PRGLNE()` | A | Current source line number (debug) |
| `PRGNME()` | A | Current source file name (debug) |
| `DELF(filename)` | L | Delete file (.T.=success) |
| `AVAIL(size)` | I | Available array elements for size |
| `CLNUM()` | I | Current line number |
| `DSPCE()` | I | Available disk space |
| `TEST(field, bits)` | L | Test bit values |
| `NULL(field)` | L | Is field null/empty? |
| `ALOCARY(field, num)` | I | Allocate array elements |
| `ALC_FLD(field, size)` | I | Allocate field |
| `ALOC(value, array, start)` | I | Find array element by value |
| `AEV(array, element)` | ? | Get array element value |
| `FARRAY(field)` | I | Number of array elements |
| `PSET(option)` | I | Printer setting (W/P/M) |
| `PROP(str)` | A | Proper case |

---

## Trap Names

Traps are set with the `TRAP` command and control program response to user keystrokes.

### Trap Actions
- `DFLT` - Default behavior for that key
- `IGNR` - Ignore the key press
- `GOTO label` - Transfer control to label
- `GOSUB label` - Subroutine call to label (must RET)

### Available Trap Names

| Trap | Description |
|------|-------------|
| `F1` - `F10` | Function keys |
| `SF1` - `SF10` | Shift + Function keys |
| `CTL_F1` - `CTL_F10` | Ctrl + Function keys |
| `ALT_F1` - `ALT_F10` | Alt + Function keys |
| `CTL_A` - `CTL_Z` | Ctrl + letter keys |
| `ALT_A` - `ALT_Z` | Alt + letter keys |
| `ESC` | Escape key (during entry) |
| `T_ESC` | Temporary ESC (cleared after ENTER/RETURN) |
| `INT` | Escape key (during program operation, not entry) |
| `UPAR` | Up arrow |
| `DNAR` | Down arrow |
| `LT_A` | Left arrow |
| `RT_A` | Right arrow |
| `LT_A_AS` | Left arrow at start of field |
| `RT_A_AS` | Right arrow at end of field |
| `HOME` | Home key |
| `END` | End key |
| `PG_UP` | Page Up |
| `PG_DN` | Page Down |
| `CTL_PG_UP` | Ctrl + Page Up |
| `CTL_PG_DN` | Ctrl + Page Down |
| `INSRT` | Insert key |
| `DEL_KEY` | Delete key |
| `WD_LT` | Ctrl + Left arrow (word left) |
| `WD_RT` | Ctrl + Right arrow (word right) |
| `TAB` | Tab key |
| `BCK_TAB` | Shift + Tab |
| `RSRCH` | Record search (must be GOSUB, not GOTO) |
| `RLCK` | Record lock detected |
| `L_EXIT` | List exit |
| `FERR` | File error |
| `PG_BRK` | Page break (must be GOSUB) |
| `HELP` / `F1` | Help (must be GOSUB) |

### Trap Management Commands
```
TRAP trap_names GOTO/GOSUB/IGNR/DFLT label
PUSHT trap_list       ; save trap settings to stack
POPT trap_list        ; restore trap settings from stack
XTRAP SAVE/RSTR       ; save/restore all traps at once
```

---

## Syntax Conventions

The following abbreviations are used in command syntax descriptions:

| Abbreviation | Meaning |
|-------------|---------|
| `f/c/e` | Field, Constant, or Expression |
| `fn/v` | Field name or Variable field (must be actual field, not constant) |
| `file_expr` | File name (sac) or `@file_num` |
| `key_expr` | Key name (sac) or `@key_num` or `@0` (direct) |
| `label` | Line label (up to 14 chars, ends with `:` in definition) |
| `lexpr` | Logical expression (resolves to .T. or .F.) |
| `sac` | Special Alpha Constant (unquoted, starts with letter, may contain numbers/periods/underscores) |
| `flist` | Array of F-type pointers used in place of field list |
| `scope` | A (all), F (first n), N (next n), R (rest) |
| `prt_where` | S (screen), P (printer), D (disk), A (ask) |

---

## Miscellaneous Syntax

### Line Continuation
Use `\` as last character on a line to continue to the next line:
```
? 'This is \
a test'
; Produces: This is a test
```

### Multiple Commands Per Line
Use `|` to separate multiple commands on one line:
```
X=1 | Y=2 | Z = 'ABC'
```

### Comments/Remarks
Three forms:
```
; This is a comment (semicolon)
&& This is also a comment
* This is a comment (asterisk, must be first char)
```
Note: Semicolons inside parentheses (e.g., in FOR loops) are treated as semicolons, not comments.

### Maximum Line Length
1024 characters per line.

### File References
- `OPEN`-ed files: reference by name as special alpha constant (no quotes): `FIND M SRCH CUSTFILE`
- `OPENV`-ed files: reference by `@file_num_variable`: `FINDV M FNUM cust_handle`

### Field Redirector
The `&` operator indirects through pointer fields:
```
ADD 'NEW_FIELD' TYPE A SIZE 10 FPTR my_ptr
&my_ptr = 'Hello'      ; assigns to the dynamically added field
ENTER &my_ptr AT 10,5   ; enters the field pointed to by my_ptr
```

### Indirect Field References
Fields can be referenced indirectly using the `&` redirector with F-type or P-type pointer fields.
This is essential for runtime-added fields (via ADD command) since their names aren't known at compile time.
