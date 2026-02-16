# TAS .RUN Compiler Byte Format Reference

Detailed byte-level documentation of every .RUN file format element,
derived from hex analysis of original TAS 5.1 compiler output.
This serves both the compiler (AST → .RUN) and decompiler (.RUN → AST).

## File Layout

```
Offset    Size    Segment
0         128     Header (TPci record)
128       1600    Buffer List (100 × 16 bytes)
1728      var     Code Segment (instructions, 7 bytes each)
1728+C    var     Constant Segment
1728+C+K  var     Spec Segment
1728+C+K+S var    Label Segment (4 bytes each)
1728+C+K+S+L var  Field Spec Segment (48 bytes each)
```

## Header (128 bytes)

All values little-endian int32 unless noted.

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0 | 4 | CodeSize | Code segment size in bytes |
| 4 | 4 | ConstSize | Constant segment size |
| 8 | 4 | SpecSize | Spec segment size |
| 12 | 4 | LabelSize | Label segment size |
| 16 | 4 | ScrnFldNum | Number of screen fields (non-derivable, from metadata) |
| 20 | 4 | NumFlds | Total number of fields |
| 24 | 4 | TempFlds | Temp field data area end offset |
| 28 | 4 | NumTempFlds | Number of temp fields |
| 32 | 4 | FldNameSize | Field spec segment size (= NumFlds × 48) |
| 36 | 4 | TempFldSize | Temp field data area size (non-derivable, often 0xFFFF=65535) |
| 40 | 4 | DefFldSegSize | Defined field data segment size |
| 44 | 4 | NumExtraFlds | Extra field slots available at runtime |
| 48 | 4 | PrgNames | Program name buffer size (non-derivable) |
| 52 | 1 | DebugFlg | Debug mode flag (0 or 1) |
| 53 | 5 | ProType | Signature: "TAS32" (DOS) or "TASWN" (Windows) |
| 58 | 4 | NumLabels | Number of labels |
| 62 | 1 | NewFldSpec | Uses 60-byte field specs (0=48-byte, 1=60-byte) |
| 63 | 1 | ChkUpVld | Check up-arrow validation |
| 64 | 1 | IncLabels | Include label names |
| 65-77 | 13 | Timestamps | Three 4-byte timestamps + trailing zero (non-derivable) |
| 78-127 | 50 | Reserved | Always zero |

### Non-derivable header values
These cannot be computed from source code alone. The decompiler emits
them in `@HEADER`/`@HEADER2`/`@HEADER3` comments. The compiler reads
them from metadata or the original `.RUN` file:
- `ScrnFldNum` (offset 16)
- `TempFldSize` (offset 36) — usually 0xFFFF (65535)
- `PrgNames` (offset 48)
- Timestamps (offsets 65-77)

## Buffer List (1600 bytes)

100 slots × 16 bytes each. Most files use all zeros (no file buffers).
Files that OPEN database files populate slots starting at index 0.

Each slot:
| Offset | Size | Field |
|--------|------|-------|
| 0 | 8 | Name (ASCII, space-padded) |
| 8 | 4 | BufferPtr |
| 12 | 4 | FileHandle |

## Instruction Format (7 bytes, TAS 5.1)

```
[CmdNum: uint16] [SLSize: byte] [SLPtr: int32]
```

- **CmdNum**: Opcode (see TasOpcode.cs)
- **SLSize**: Size of this instruction's spec data in bytes (0-255)
- **SLPtr**: Byte offset into the spec segment

The END marker is always the last instruction: `0xFE00 0x00 0x00000000`

## Constant Segment

Constants are stored sequentially. Each instruction references constants
by byte offset. The constant pool always starts with **integer zero** at offset 0.

### Integer constant (5 bytes)
```
49 XX XX XX XX
'I' + int32_value (little-endian)
```
This is a **compact binary format**, NOT the standard header format.
Example: integer zero = `49 00 00 00 00`

### String/Alpha constant (4 + N bytes)
```
41 DD SS SS [data...]
'A' + decimals(1) + displaySize(2 LE) + ASCII_data(displaySize)
```
Example: "TASEXT" = `41 00 06 00 54 41 53 45 58 54`

### Expression constant (4 + N bytes)
```
TT DD SS SS FD BB BB BB BB [operations...] FF
type(1) + dec(1) + displaySize(2 LE) + 0xFD + tempBase(4 LE) + RPN_ops + 0xFF
```
- **displaySize** = 1 (FD) + 4 (tempBase) + len(operations) + 1 (FF)
- **tempBase** = next available temp offset AFTER all temps used in this expression
  (e.g., if 1 temp used at offset 0 with fieldSpecSize=48, tempBase=48)
- First temp result at offset 0, increments by FieldSpecSize (48)

#### Expression operation types:
| Opcode | Format | Size |
|--------|--------|------|
| 0x00 | Binary op: `00 op(1) lhs(5) rhs(5) result(4)` | 16 |
| 0x01 | 0-arg function: `01 funcNum(1) result(4)` | 6 |
| 0x02 | 1-arg function: `02 funcNum(1) arg(5) result(4)` | 11 |
| 0x03+ | N-arg function: `(N+1) funcNum(1) args(5×N) result(4)` | 6+5N |
| 0x0C | UDF call: `0C labelIdx(4) pad(4) args(5×N) result(4)` | 13+5N |
| 0xB4 | Array access: `B4 field(5) indexOff(4) result(4)` | 14 |
| 0x0A-0x0F | Comparison: `op(1) lhsTemp(4) rhsTemp(4) resultTemp(4)` | 13 |

#### Operand format (5 bytes):
```
type(1) + location(4 LE)
```

## Spec Segment

Each instruction's parameters are stored as a byte sequence at a specific
offset in the spec segment. The instruction's SLPtr points here, SLSize
gives the length.

### 5-byte Spec Parameter (TSpecLinePtr)

| Type byte | Meaning | Location value |
|-----------|---------|----------------|
| `F` (0x46) | Field reference | fieldIndex × fieldSpecSize |
| `C` (0x43) | Constant | byte offset in constant segment |
| `N` (0x4E) | Numeric literal | direct int32 value |
| `X` (0x58) | Expression | byte offset to compiled RPN in constants |
| `Y` (0x59) | Array field | byte offset in constants to 10-byte ref |
| `M` (0x4D) | Macro | constant offset |
| `s` (0x73) | Single char | char code in low byte |
| `q` (0x71) | Macro array | constant offset |
| `G` (0x47) | GOTO label | label index |
| `S` (0x53) | GOSUB label | label index |
| `I` (0x49) | IGNR action | unused |
| `D` (0x44) | DFLT action | label index (0 = DFLT) |
| `T` (0x54) | THEN marker | unused |
| `R` (0x52) | REENT marker | unused |
| `E` (0x45) | RET marker | unused |

## Command Spec Layouts

All spec layouts derived from `RunFileDecompiler.cs` DecompileXxx methods.
Notation: `(5B)` = 5-byte TSpecLinePtr param, `(4B)` = int32, `(1B)` = byte/char.

---

### Core Commands

#### NOP (0x0000)
Total: 0 bytes

#### ASSIGN (0x000F) — Assignment (`field = value`)
```
[0-4]   target(5)   Target field param
[5-9]   value(5)    Value param
```
Total: 10 bytes

#### GOTO (0x001D) — Jump to label
```
[0-3]  labelIdx(4)  Label index (int32)
```
Total: 4 bytes

#### GOSUB (0x001C) — Call subroutine
```
[0-3]  labelIdx(4)  Label index (int32)
```
Total: 4 bytes

#### RET (0x0021) — Return
```
[0-4]  value(5)    Optional return value (or empty)
```
Total: 0 or 5 bytes

#### QUIT (0x0057)
Total: 0 bytes

#### BRKRET (0x0027) — Break/return
Total: 0 bytes

---

### Conditional / Control Flow

#### IF (0x003B) — Conditional
```
[0-3]   jump(4)     Jump target (byte offset, forward ref)
[4]     pad(1)      Always zero
[5-9]   lhs(5)      Left operand param (expression)
[10]    variant(1)  'D'=DO(block), 'T'=THEN, 'G'=GOTO, 'S'=GOSUB,
                    'R'=REENT, 'E'=RET
[11-14] label(4)    Label index (for 'G' and 'S' variants only)
```
Total: 16 bytes (CASE uses same layout: jump@0, type@10, location@11)

#### ELSE (0x0042) — Else branch
```
[0-3]  jump(4)     Jump target
```
Total: 4 bytes

#### ENDIF (0x01F6) — End if block
Total: 0 bytes

#### ON (0x001E) — Computed goto/gosub
```
[0-4]   val(5)      Selector value
[5]     tosub(1)    'S'=gosub, 'G'=goto
[6]     num_labels(1) Number of labels
[7+]    labels(4 each) Label indices
```
Total: 7 + (num_labels × 4) bytes

---

### Loop Control

#### WHILE (0x0043) — While loop
```
[0-3]  exitJump(4)  Jump to exit (byte offset)
[4-8]  expr(5)      Condition expression param
```
Total: 9 bytes

#### ENDW (0x0045) — End while
```
[0-3]  backJump(4)  Jump back to WHILE
```
Total: 4 bytes

#### FOR (0x0048) — For loop
```
[0-3]   exitJump(4)  Jump to exit
[4-8]   stop(5)      Stop value
[9-13]  step(5)      Step value
[14-18] counter(5)   Counter field
[19-23] start(5)     Start value
[24]    direction(1) 0=up, 1=down
```
Total: 25 bytes

#### NEXT (0x00DC) — Next iteration
Total: 0 bytes

#### LOOP (0x01F8) — Loop start
Total: 0 bytes

#### EXIT (0x01FC) — Loop exit
Total: 0 bytes

#### LOOP_IF (0x0046) — Conditional loop
```
[0-3]  jump(4)     Jump address
[4-8]  expr(5)     Condition expression
```
Total: 9 bytes

#### EXIT_IF (0x0047) — Conditional exit
```
[0-3]  jump(4)     Jump address
[4-8]  expr(5)     Condition expression
```
Total: 9 bytes

#### FLOOP (0x01FE) — FLOOP start
Total: 0 bytes

#### FEXIT (0x01FF) — FLOOP exit
Total: 0 bytes

#### FLOOP_IF (0x0200) — Conditional FLOOP
```
[0-3]  jump(4)     Jump address
[4-8]  expr(5)     Condition expression
```
Total: 9 bytes

#### FEXIT_IF (0x0201) — Conditional FEXIT
```
[0-3]  jump(4)     Jump address
[4-8]  expr(5)     Condition expression
```
Total: 9 bytes

---

### Case/Select

#### SELECT (0x0044) — Select statement
```
[0-4]  selector(5)  Selector expression
```
Total: 5 bytes

#### CASE (0x0093) — Case branch (uses IF-style layout)
```
[0-3]   jump(4)     Jump address for non-match
[4-9]   unused(6)
[10]    type(1)     Value type byte
[11-14] location(4) Value location
```
Total: 15 bytes

#### OTHERWISE (0x01FA) — Default case
Total: 0 bytes

#### ENDC (0x01FB) — End case
Total: 0 bytes

---

### Scan Control

#### SCAN (0x0089) — File scan loop
```
[0-3]   end_jump(4)  Jump to end
[4-8]   handle(5)    File handle
[9-13]  key(5)       Key field
[14-18] start(5)     Start value
[19]    scope(1)     Scope char
[20-24] sval(5)      Scope value
[25-29] for(5)       FOR expression
[30-34] while(5)     WHILE expression
[35]    display(1)   Y/N
[36]    no_lock(1)   Y/N
[42]    reverse(1)   Y/N
```
Total: 43 bytes

#### SLOOP (0x0204) — Scan loop
Total: 0 bytes

#### SEXIT (0x0202) — Scan exit
Total: 0 bytes

#### SLOOP_IF (0x0205) — Conditional scan loop
```
[0-3]  jump(4)     Jump address
[4-8]  expr(5)     Condition expression
```
Total: 9 bytes

#### SEXIT_IF (0x0203) — Conditional scan exit
```
[0-3]  jump(4)     Jump address
[4-8]  expr(5)     Condition expression
```
Total: 9 bytes

#### ENDS (0x0206) — End scan
Total: 0 bytes

#### START_SCAN (0x00B9) — Internal scan start (compiler-generated)
Skipped in decompile.

#### SET_SCAN_FLG (0x008A) — Internal scan flag (compiler-generated)
Skipped in decompile.

---

### Display Commands

#### SAY / DISPF (0x0017 / 0x0010) — Display field
```
[0-4]   fld(5)      Field to display
[5-9]   col(5)      Column position
[10-14] row(5)      Row position
[15-19] color(5)    Color attribute
[20-24] pict(5)     Picture format
```
Total: 25 bytes

#### PMSG (0x0001) — Print message (the `?` command)
```
[0-4]   col(5)      Column position
[5-9]   row(5)      Row position
[10-14] msg(5)      Message text
[15]    wait(1)     Y/N
[16]    ncr(1)      No carriage return Y/N
[17-21] ent(5)      Enter field
[22]    whr(1)      Where flag
[23-27] color(5)    Color attribute
[28]    abs(1)      Absolute position Y/N
```
Total: 29 bytes

#### MSG (0x006A) — Display message
```
[0-4]  fld(5)      Message text param
[5]    no_wait(1)  Y/N
```
Total: 6 bytes

#### POSTMSG (0x000B) — Post message
```
[0-4]  routine(5)  Routine param
[5-9]  form(5)     Form param
```
Total: 10 bytes

#### CLRSCR (0x0002) — Clear screen
Total: 0 bytes

#### CLRSF (0x000A) — Clear screen fields
Total: 0 bytes

#### CLRLNE (0x0007) — Clear line
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] chrs(5)     Number of characters
[15]    no_color(1) Y/N
[16-20] color(5)    Color attribute
[21]    abs(1)      Absolute Y/N
```
Total: 22 bytes

#### CURSOR (0x0003) — Set cursor
```
[0-4]   start(5)    Cursor start
[5-9]   stop(5)     Cursor stop
[10]    on_off(1)   On/off flag
[11]    wait(1)     Y/N
[12]    dflt(1)     Default Y/N
```
Total: 13 bytes

#### COLOR (0x006F) — Set color
```
[0-4]  color(5)    Color value
```
Total: 5 bytes

#### BKG (0x0075) — Set background color
```
[0-4]  color(5)    Color value
```
Total: 5 bytes

#### FRG (0x0076) — Set foreground color
```
[0-4]  color(5)    Color value
```
Total: 5 bytes

#### REVERSE (0x0077) — Reverse video
```
[0-4]  on_off(5)   On/off param
```
Total: 5 bytes

#### BELL (0x0004) — Ring bell
Total: 0 bytes

---

### Screen Control

#### SAVES (0x0032) — Save screen
```
[0-4]  handle(5)   Screen handle
```
Total: 5 bytes

#### SAVES3 (0x00A9) — Save screen (v3)
Total: 5 bytes (same as SAVES)

#### REDSP (0x002D) — Redisplay screen
```
[0-4]  handle(5)   Screen handle
```
Total: 5 bytes

#### REDSP3 (0x00AA) — Redisplay (v3)
Total: 5 bytes (same as REDSP)

#### RSCR (0x0051) — Restore screen
Total: 0 bytes

#### NOREDSP (0x008F) — No redisplay
Total: 0 bytes

#### SCRN (0x0067) — Screen action
```
[0]    action(1)   Char: S/R/L/U/E/D
```
Total: 1 byte

#### RDLIST (0x0068) — Redisplay list
```
[0-4]  field(5)    Field param
```
Total: 5 bytes

#### SCROLL (0x0033) — Scroll region
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] ht(5)       Height
[15-19] wdt(5)      Width
[20-24] lnes(5)     Lines
[25]    direction(1) Direction flag
```
Total: 26 bytes

#### PAINT (0x0060) — Paint region
```
[0-4]   frm_x(5)   From X
[5-9]   frm_y(5)   From Y
[10-14] thr_x(5)   Thru X
[15-19] thr_y(5)   Thru Y
[20-24] color(5)   Color
```
Total: 25 bytes

#### ROW_COLOR (0x00C2) — Set row color
```
[0-4]   from(5)    From position
[5-9]   thru(5)    Thru position
[10-14] bkg(5)     Background color
[15-19] text(5)    Text color
```
Total: 20 bytes

---

### Window Commands

#### WINDOW / WINDEF (0x0039 / 0x002E) — Define/open window
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] len(5)      Length
[15-19] wdt(5)      Width
[20-24] wcolor(5)   Window color
[25-29] ttl(5)      Title
[30-34] ttlw(5)     Title width
[35-39] box(5)      Box type
[40-44] bcolor(5)   Border color
[45-49] shd(5)      Shadow
[50-54] scolor(5)   Shadow color
[55-59] icolor(5)   Inactive color
[60-64] savef(5)    Save field
[65-68] sve_fld_sze(4) Save field size
[69-73] wtext(5)    Window text
[74-78] btext(5)    Border text
[79]    dco(1)      DCO flag
[80]    active(1)   Active Y/N
```
Total: 81 bytes

#### WINACT (0x002F) — Activate window
```
[0-4]  window(5)   Window param
```
Total: 5 bytes

#### WCOLOR (0x00BB) — Set window color
```
[0-4]  color_name(5) Color name
[5-9]  color_val(5)  Color value
```
Total: 10 bytes

#### BUTTON (0x00BC) — Create button
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] len(5)      Length
[15-19] wdt(5)      Width
[20-24] color(5)    Color
[25-29] caption(5)  Caption text
[30-34] txtcolor(5) Text color
[35-39] key(5)      Key code
[40]    remove(1)   Remove Y/N
[41-45] save_to(5)  Save to field
[46-50] restore_from(5) Restore from
[51]    off(1)      Off Y/N
[52]    on(1)       On Y/N
[53-57] using(5)    Using param
```
Total: 58 bytes

#### HOT_SPOT (0x00C4) — Define hot spot
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] len(5)      Length
[15-19] wdt(5)      Width
[20-24] hndl(5)     Handle
[25-29] key(5)      Key code
[30]    remove(1)   Remove Y/N
```
Total: 31 bytes

#### CAPTION (0x00BD) — Set caption
```
[0-4]  caption(5)  Caption text
[5]    dontchange(1) Don't change Y/N
```
Total: 6 bytes

#### GRAY (0x00BF) — Gray out
```
[0-4]  on_off(5)   On/off param
```
Total: 5 bytes

---

### Menu Commands

#### MENU (0x000E) — Old-style menu
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] lt(5)       Length
[15-19] wdt(5)      Width
[20-24] wclr(5)     Window color
[25-29] ttlfld(5)   Title field
[30-34] ttlloc(5)   Title location
[35-39] box(5)      Box type
[40-44] bclr(5)     Border color
[45-49] shd(5)      Shadow
[50-54] sclr(5)     Shadow color
[55-59] cpc(5)      Current pick color
[60-64] afld(5)     Action field
[65-69] cntr(5)     Counter
[70-74] nchcs(5)    Number of choices
[75-79] mcwdt(5)    Menu choice width
[80-83] esc_lbl(4)  ESC label index
[84-87] hlp_lbl(4)  Help label index
[88]    hold(1)     Hold Y/N
```
Total: 89 bytes

#### NMENU (0x001B) — New menu
```
[0-4]   flds(5)     Fields
[5-9]   col(5)      Column
[10-14] row(5)      Row
[15-19] msg(5)      Message
[20-24] width(5)    Width
[25-29] chcs(5)     Choices
[30-34] chose(5)    Chosen
[35-39] help(5)     Help
[40-44] ttl_w(5)    Title width
[45-49] ttlfld(5)   Title field
[50-54] cntr(5)     Counter
[55-59] box(5)      Box type
[60-64] shad_w(5)   Shadow width
[65]    hold(1)     Hold Y/N
[66]    array(1)    Array Y/N
[67-71] mcolor(5)   Menu color
[72-76] ccolor(5)   Current color
[77-81] scolor(5)   Shadow color
[82-85] larrow_lbl(4) Left arrow label
[86-89] rarrow_lbl(4) Right arrow label
[90-94] length(5)   Length
[95-99] bcolor(5)   Border color
[100]   auto(1)     Auto Y/N
[101]   nowait(1)   No wait Y/N
[102-106] on_mve(5) On move
[107]   use_traps(1) Use traps Y/N
[108-112] mtext(5)  Menu text
[113-117] ctext(5)  Current text
[118-122] btext(5)  Border text
[123]   dco(1)      DCO flag
[124-128] retval(5) Return value
[129-133] sub_num(5) Sub number
[134-138] no_esc(5) No escape
```
Total: 139 bytes

---

### List Commands

#### LISTF / LISTM (0x0053 / 0x004F) — List file/memory
```
[0-4]   lst(5)      List field
[5-9]   cntr(5)     Counter
[10-14] actv(5)     Active
[15-19] chse(5)     Chosen
[20]    rnd(1)      Round Y/N
[21]    menu(1)     Menu Y/N
[22-26] oth(5)      Other
[27-31] srch(5)     Search
[32-36] fhndl(5)    File handle
[37-41] fkey(5)     File key
[42-46] for(5)      FOR expression
[47-51] while(5)    WHILE expression
[52-56] start(5)    Start value
[57]    no_wait(1)  No wait Y/N
[58-62] enter(5)    Enter field
[63]    no_add(1)   No add Y/N
[64-68] lnes(5)     Lines
[69-73] hlp(5)      Help
[74-78] cc(5)       Current color
[79]    up(1)       Up Y/N
[80-84] on_mve(5)   On move
[85-89] styp(5)     Sort type
[90]    list_end(1) List end Y/N
[91-95] fline(5)    First line
[96-100] cbf(5)     Current buffer
[101-105] blnes(5)  Blank lines
[106]   noshift(1)  No shift Y/N
[107-111] ec(5)     End color
[112]   use_traps(1) Use traps Y/N
[113]   insrt_at_end(1) Insert at end Y/N
[114]   touch_scrn(1)  Touch screen Y/N
[115-119] cctext(5) Current color text
[120-124] ectext(5) End color text
[125]   dco(1)      DCO flag
```
Total: 126 bytes

#### LIST (0x0054) — Simple list
```
[0-4]   field_list(5) Fields
[5-9]   enter(5)    Enter
[10-14] srch(5)     Search
[15-19] hlp(5)      Help
[20-24] choose(5)   Choose
[25-29] other(5)    Other
```
Total: varies

#### LIST_EXIT (0x009C) — List exit
Total: 0 bytes

---

### File I/O Commands

#### OPENV / OPEN (0x0040 / 0x001F) — Open file
```
[0-4]   filename(5)    File name
[5-9]   ext_cc(5)      Extension/CC
[10]    lock_type(1)   Char: N/R/F/A/X/D/O
[11-14] err_label(4)   Error label index
[15-19] owner(5)       Owner
[20-24] path(5)        Path
[25-29] fd_schema(5)   FD/schema
[30]    file_type(1)   Char: T/X/D/F/B/C
[31-35] fnum_handle(5) FNUM handle
[36-40] rec_size(5)    Record size
[41-45] buffer_name(5) Buffer name
[46]    create_flg(1)  Create Y/N
[47]    noclr_flg(1)   No clear Y/N
[48-52] update_udf(5)  Update UDF
[53]    addallflds(1)  Add all fields Y/N
```
Total: 54 bytes

#### OPNO (0x00AF) — Open non-TAS file
```
[0-4]   filename(5)    File name
[5-29]  varies
[30]    type(1)        Type char
[31-35] varies
[36-40] rsize(5)       Record size
[41-45] bufname(5)     Buffer name
```
Total: 48 bytes

#### CLOSE (0x0008) — Close file
```
[0-4]  handle(5)   File handle
[5]    delete(1)   Delete Y/N
```
Total: 6 bytes

#### ROPEN (0x0083) — Reopen file
```
[0-4]  handle(5)   File handle
```
Total: 5 bytes

#### CLR (0x0006) — Clear file buffer
```
[0-4]  handle(5)   File handle
```
Total: 5 bytes

#### INIFLE (0x0018) — Initialize file
```
[0-4]  handle(5)   File handle
```
Total: 5 bytes

#### CLSO (0x003D) — Close non-TAS file
Total: 0 bytes

#### CLSPF (0x004E) — Close print-to-disk file
Total: 0 bytes

---

### Record Operations

#### FINDV / FIND (0x001A / 0x0013) — Find record
```
[0-4]   handle(5)     File handle
[5-9]   key(5)        Key field
[10-14] val(5)        Search value
[15]    find_type(1)  Char: M/G/F/L/N/P/R/E/S
[16-19] err_label(4)  Error label
[20]    nlock(1)      No lock Y/N
[21]    key_only(1)   Key only Y/N
[22-26] for_expr(5)   FOR expression
[27-31] while_expr(5) WHILE expression
[32]    noclr(1)      No clear Y/N
```
Total: 33 bytes

#### SAVE (0x0031) — Save record
```
[0-4]   handle(5)     File handle
[5]     no_clear(1)   No clear Y/N
[6]     no_ask(1)     No ask Y/N
[7-10]  nosave_lbl(4) No-save label
[11-14] err_lbl(4)    Error label
[15]    unlock(1)     Unlock Y/N
```
Total: 16 bytes

#### DEL (0x000C) — Delete record
```
[0-4]   num(5)        Record number
[5]     no_ask(1)     No ask Y/N
[6-9]   nodel_lbl(4)  No-delete label
[10-13] err_lbl(4)    Error label
```
Total: 15 bytes (note: 14 bytes data, but spec is 15)

#### DALL (0x005A) — Delete all records
```
[0-4]   hndl(5)     File handle
[5-9]   for(5)      FOR expression
[10-14] while(5)    WHILE expression
[15]    disp(1)     Display Y/N
[16-20] cntr(5)     Counter
[21]    scope(1)    Scope char
[22-26] sval(5)     Scope value
[27-31] key(5)      Key field
[32-36] strt(5)     Start value
```
Total: 37 bytes

#### SRCH (0x0014) — Search file
```
[0-4]  filename(5)  File name
[5-9]  key(5)       Key field
```
Total: 10 bytes

#### IFDUP (0x008C) — If duplicate record
```
[0-3]  jump(4)     Jump address
```
Total: 4 bytes

#### IFNA (0x008D) — If record not active
```
[0-3]  jump(4)     Jump address
```
Total: 4 bytes

#### ULKALL (0x0038) — Unlock all records
Total: 0 bytes

---

### Array Operations

#### RDA (0x0009) — Read array
```
[0-4]   frm(5)      From field
[5-9]   to(5)       To field
[10-14] hndl(5)     File handle
[15-19] key(5)      Key field
[20-24] strt(5)     Start value
[25]    scope(1)    Scope char
[26-30] sval(5)     Scope value
[31-35] for(5)      FOR expression
[36-40] while(5)    WHILE expression
[41-45] cntr(5)     Counter
[46]    display(1)  Display Y/N
```
Total: 47 bytes

#### WRTA (0x000D) — Write array
```
[0-4]   frm(5)      From field
[5-9]   to(5)       To field
[10]    scope(1)    Scope char
[11-15] sval(5)     Scope value
[16-20] for(5)      FOR expression
[21-25] hndl(5)     File handle
[26-30] rec(5)      Record
[31]    display(1)  Display Y/N
[32-36] cntr(5)     Counter
```
Total: 37 bytes

#### UPDTA (0x0019) — Update array
```
[0-4]   all(5)      All field
[5]     wtd(1)      What to do char
[6-10]  times(5)    Times
[11-15] val(5)      Value
[16-20] fn(5)       Function
```
Total: 21 bytes

#### SORTA (0x0034) — Sort array
```
[0-4]   fld(5)      Field
[5-9]   num(5)      Number
[10-14] move(5)     Move field
[15-19] cntr(5)     Counter
[20]    way(1)      Direction char
```
Total: 21 bytes

#### REMVA (0x0041) — Remove array
```
[0-4]  array(5)    Array field
```
Total: 5 bytes

#### SORT3 (0x003F) — Sort3
```
[0-4]   mem(5)      Memory
[5-9]   fld(5)      Field
[10-14] size(5)     Size
[15-19] num(5)      Number
[20-24] wtd(5)      What to do
```
Total: 25 bytes

---

### Data Entry

#### ENTER (0x0021) — Keyboard input
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] field(5)    Field
[15-19] mask(5)     Input mask
[20-24] help(5)     Help text
[25-28] up_label(4) Up-arrow label
[29-33] valid(5)    Validation
[34]    acr(1)      Auto CR Y/N
[35]    pswd(1)     Password Y/N
[36]    upc(1)      Uppercase Y/N
[37-41] color(5)    Color
[42-46] pre(5)      Pre-edit
[47-51] post(5)     Post-edit
[54-58] dflt(5)     Default value
[59-63] vmsg(5)     Validation message
[64]    norev(1)    No reverse Y/N
[65-69] do(5)       Do expression
[70]    nosave(1)   No save Y/N
[71]    return(1)   Return Y/N
[72]    noesc(1)    No escape Y/N
[73]    array(1)    Array Y/N
[74-78] enum(5)     Enumeration
[79-83] cntr(5)     Counter
[84]    auto_srch(1) Auto search Y/N
[85-89] group(5)    Group
[90]    nc_off(1)   NC off Y/N
[91]    nc_on(1)    NC on Y/N
[92-96] mbl(5)      Mobile
```
Total: 97 bytes

#### ASK (0x005B) — Ask dialog
```
[0-4]   message(5)    Message text
[5-9]   default(5)    Default value
[10-14] caption(5)    Caption
[15-19] use(5)        Use param
[20-24] leftbutton(5) Left button
[25-29] rightbutton(5) Right button
```
Total: 30 bytes

#### REENT (0x003A) — Reenter
Total: 0 bytes

#### UPAR (0x006B) — Up-arrow field
```
[0-4]  field(5)    Field param
```
Total: 5 bytes

#### AUTOENTER (0x00A2) — Auto enter
Total: 0 bytes

#### AUTOINC (0x0090) — Auto increment
Total: 0 bytes

#### AUTODEC (0x00A3) — Auto decrement
Total: 0 bytes

#### AUTONEW (0x00BA) — Auto new
Total: 0 bytes

#### NORSTRT (0x00A0) — No restart
Total: 0 bytes

#### NOVLDMSG (0x00A1) — No validation message
Total: 0 bytes

---

### Field Operations

#### INC (0x0029) — Increment field
```
[0-4]  fld(5)      Field param
```
Total: 5 bytes

#### DEC (0x005F) — Decrement field
```
[0-4]  fld(5)      Field param
```
Total: 5 bytes

#### FILL (0x0011) — Fill field
```
[0-4]   fld(5)      Field
[5-9]   chr(5)      Fill character
[10-14] times(5)    Times
[15]    where(1)    Where flag
```
Total: 16 bytes

#### MID (0x002A) — Mid string
```
[0-4]   target(5)   Target field
[5-9]   source(5)   Source field
[10-14] start(5)    Start position
[15-19] length(5)   Length
```
Total: 20 bytes

#### TRIM (0x008B) — Trim field
```
[0-4]  field(5)    Field param
```
Total: 5 bytes

#### UP (0x0087) — Uppercase
```
[0-4]  udf(5)      Field param
[5-8]  goto_lbl(4) Goto label
```
Total: 9 bytes

#### FORMAT (0x0078) — Format field
```
[0-4]   fld(5)      Field
[5-9]   recv(5)     Receive field
[10]    commas(1)   Commas Y/N
[11]    flt_dol(1)  Float dollar Y/N
[12]    neg_how(1)  Negative display
[13]    off(1)      Off Y/N
[14-18] pict(5)     Picture
[19]    no_zeros(1) No zeros Y/N
```
Total: 20 bytes

#### XFER (0x001D) — Transfer data
```
[0-4]   from_fld(5) From field
[5-9]   to_fld(5)   To field
[10-14] numchr(5)   Num chars
[15-19] fmem(5)     From memory
[20-24] tmem(5)     To memory
[25]    rec_buff(1) Record buffer flag
```
Total: 26 bytes

#### INSERT (0x0064) — Insert
```
[0-4]  fld(5)      Field
[5-9]  flst(5)     Field list
```
Total: 10 bytes

#### REPL (0x0055) — Replace
```
[0-4]   stuffed(5)  Stuffed field
[5-9]   at(5)       At position
[10-14] num_chrs(5) Num chars
[15-19] stuffee(5)  Stuffee field
[20]    overwrite(1) Overwrite Y/N
[21-25] mem(5)      Memory
```
Total: 26 bytes

#### DELC (0x007B) — Delete characters
```
[0-4]   fld(5)      Field
[5-9]   at(5)       At position
[10-14] nchr(5)     Num chars
[15-19] mem(5)      Memory
```
Total: 20 bytes

#### PUSHF (0x0081) — Push field
```
[0-4]  fld(5)      Field param
```
Total: 5 bytes

#### POPF (0x0082) — Pop field
```
[0-4]  fld(5)      Field param
```
Total: 5 bytes

#### POPS (0x003C) — Pop stack
```
[0-4]  count(5)    Count param
```
Total: 5 bytes

#### PUT_FLD (0x003E) — Put field
```
[0-4]  recv_fld(5) Receive field
```
Total: 5 bytes

#### SETLINE (0x0065) — Set line
```
[0-4]  recv(5)     Receive field
[5-9]  flst(5)     Field list
```
Total: 10 bytes

#### RCN (0x005C) — Record number
```
[0-4]  field(5)    Field param
```
Total: 5 bytes

#### KBDUP (0x009F) — Keyboard up
```
[0-4]  on_off(5)   On/off param
```
Total: 5 bytes

---

### Field Definition

#### DEFINE (0x01F5) — Define field (compile-time only)
Processed at compile time, never in bytecode.

#### ADD (0x0052) — Add field at runtime
```
[0-4]   name(5)     Field name
[5-9]   ftyp(5)     Field type
[10-14] fsze(5)     Field size
[15-19] fdec(5)     Decimals
[20-24] fupc(5)     Uppercase
[25-29] fmask(5)    Mask
[30-34] fhndl(5)    File handle
[35-39] fofst(5)    Offset
[40-44] ffptr(5)    File pointer
[45-49] fary(5)     Array
[50-54] fknum(5)    Key number
```
Total: 55 bytes

#### ALLOC (0x006D) — Allocate field
```
[0-4]   fname(5)    Field name
[5-9]   ftyp(5)     Field type
[10-14] fsze(5)     Field size
[15-19] fdec(5)     Decimals
[20-24] fary(5)     Array
[25-29] upcse(5)    Uppercase
[30-34] fmask(5)    Mask
```
Total: 35 bytes

#### DEALOC (0x0061) — Deallocate field
```
[0-4]  field(5)    Field param
```
Total: 5 bytes

#### REDEF (0x002C) — Redefine field
```
[0-4]   fld(5)      Field
[5-9]   ftyp(5)     Type
[10-14] fsze(5)     Size
[15-19] dchr(5)     Decimal chars
[20-24] fnum(5)     File number
[25-29] ofst(5)     Offset
[30-34] knum(5)     Key number
[35-39] pict(5)     Picture
[40-44] loc(5)      Location
[45-49] up(5)       Uppercase
```
Total: 50 bytes

#### REL (0x005D) — Relate
```
[0-4]   slv(5)      Slave
[5-9]   slv_key(5)  Slave key
[10-14] mstr_fle(5) Master file
[15-19] mstr_fld(5) Master field
```
Total: 20 bytes

#### SETACT (0x0074) — Set active
```
[0-4]  schema(5)   Schema param
[5-9]  file(5)     File param
```
Total: 10 bytes

---

### Printing

#### PON (0x0026) — Printer on
```
[0-4]  value(5)    Printer value
```
Total: 5 bytes

#### PBLNK (0x0022) — Print blank lines
```
[0-4]  lines(5)    Number of lines
```
Total: 5 bytes

#### PBOX (0x0023) — Print box
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10-14] lnth(5)     Length
[15-19] wdt(5)      Width
[20]    lines(1)    Lines flag
[21]    clr_set(1)  Color set
[22-26] color(5)    Color
[27-31] brdr(5)     Border
[32]    abs(1)      Absolute Y/N
```
Total: 33 bytes

#### PCHR (0x0024) — Print characters
```
[0]     ptw(1)      Print to where
[1-5]   num(5)      Number
[6-10]  fld(5)      Field
```
Total: 11 bytes

#### PFMT (0x0025) — Print format
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10]    wait(1)     Wait Y/N
[11]    ncr(1)      No CR Y/N
[12-16] line(5)     Line
[17]    whr(1)      Where
[18-22] thru(5)     Thru
[23]    abs(1)      Absolute Y/N
[24]    bks(1)      Blanks
```
Total: 25 bytes

#### PVERT (0x0028) — Print vertical tab
```
[0-4]  lines(5)    Lines
```
Total: 5 bytes

#### PTOF (0x0035) — Print top of form
Total: 0 bytes

#### PRTALL (0x008E) — Print all
```
[0-4]  field(5)    Field param
```
Total: 5 bytes

#### PSET (0x006E) — Printer set
```
[0-4]   wdt(5)      Width
[5-9]   tlnes(5)    Total lines
[10-14] plnes(5)    Page lines
[15-19] pwhr(5)     Print where
[20-24] rtm(5)      Right margin
[25-29] tag(5)      Tag
[30-34] bookmarks(5) Bookmarks
```
Total: 35 bytes

#### SHOW_PLINE (0x00A4) — Show print line
Total: 0 bytes

---

### Format/Mount

#### MOUNT (0x001C) — Mount format
```
[0-4]   fmt(5)      Format
[5]     typ(1)      Type
[6]     wtp(1)      Where to print
[7-11]  rfile(5)    Report file
[12-16] sve_to(5)   Save to
[17]    winform(1)  Window form Y/N
```
Total: 18 bytes

#### REMOUNT (0x0066) — Remount format
```
[0-4]  format(5)   Format param
```
Total: 5 bytes

---

### Import/Export

#### EXPORT (0x0050) — Export data
```
[0-4]   frm(5)      From field
[5-9]   to(5)       To field
[10-14] hndl(5)     File handle
[15-19] key(5)      Key
[20-24] strt(5)     Start
[25]    scope(1)    Scope char
[26-30] sval(5)     Scope value
[31-35] for(5)      FOR expression
[36-40] while(5)    WHILE expression
[41-45] cntr(5)     Counter
[46-50] numb(5)     Number
[51]    memory(1)   Memory Y/N
[52]    display(1)  Display Y/N
[53-57] ftyp(5)     File type
[58-62] dchr(5)     Delimiter char
[63-64] apnd(2+)    Append (partial)
[65]    name(1)     Name flag
[66-70] qual(5)     Qualifier
```
Total: 71 bytes

#### IMPORT (0x0070) — Import data
Total: 71 bytes (same layout as EXPORT)

---

### Raw File I/O

#### READ (0x0062) — Read from file
```
[0-4]   hndl(5)     File handle
[5-9]   pos(5)      Position
[10-14] nchrs(5)    Num chars
[15-19] tofrm(5)    To/from field
[20-24] mem_area(5) Memory area
[25-29] ofst(5)     Offset
```
Total: 30 bytes

#### WRITE (0x0063) — Write to file
Total: 30 bytes (same layout as READ)

#### RDREC (0x00AD) — Read record
Total: 30 bytes (same layout as READ)

#### WTREC (0x00AE) — Write record
Total: 30 bytes (same layout as READ)

---

### Memory Operations

#### DISPM (0x0079) — Display memory
```
[0-4]   col(5)      Column
[5-9]   row(5)      Row
[10]    mem_num(1)  Memory number
[11-15] strt(5)     Start
[16-20] esze(5)     Element size
[21-25] ofst(5)     Offset
[26-30] nchr(5)     Num chars
[31-35] nlne(5)     Num lines
```
Total: 36 bytes

#### FILLMEM (0x007A) — Fill memory
```
[0]     area_num(1) Area number
[1-5]   strt(5)     Start
[6-10]  nchrs(5)    Num chars
[11-15] wchr(5)     Fill char
```
Total: 16 bytes

---

### Trap/Error Handling

#### TRAP (0x0037) — Key trap
```
[0-4]  key(5)      Key param (constant with trap key codes)
[5]    wtd(1)      Action type: 'G'=goto, 'S'=gosub, 'I'=ignore, 'D'=default
[6-9]  label(4)    Label index for goto/gosub
```
Total: 10 bytes

#### PUSHT (0x0091) — Push trap
```
[0-4]  name(5)     Trap name
```
Total: 5 bytes

#### POPT (0x0092) — Pop trap
```
[0-4]  name(5)     Trap name
```
Total: 5 bytes

#### XTRAP (0x0073) — Extended trap
```
[0]     wtd(1)      Action char
[1-5]   fld(5)      Field
[6+]    cvals(var)  Condition values (varies)
```
Total: 6+ bytes

#### ERR (0x0058) — Error
```
[0-4]  fld(5)      Field
[5-9]  num(5)      Error number
```
Total: 10 bytes

#### CLRPE (0x006B) — Clear program error
Total: 0 bytes

---

### Program Control

#### CO (0x002B) — Company selection
```
[0-4]  param(5)    Company code expression
[5]    flag(1)     Result type: 'S'=string, 'N'=numeric
```
Total: 6 bytes (with argument) or 0 bytes (bare `co`)

#### CHAIN (0x0030) — Chain to program
```
[0-4]   param(5)    Program name (C-type constant)
[5-15]  zeros(11)   Padding
[16]    flag(1)     Always 'N'
[17]    zero(1)     Padding
```
Total: 18 bytes

#### CHAINR (0x004D) — Chain and return
```
[0-4]  program(5)  Program name
```
Total: 5 bytes

#### RUN (0x00A6) — Run program
```
[0-4]  name(5)     Program name
[5-9]  tail(5)     Command tail
```
Total: 10 bytes

#### EXEC (0x0056) — Execute program
```
[0-4]  prg(5)      Program name
```
Total: 5 bytes

#### RAP (0x004C) — Run anytime program
```
[0-4]   name(5)     Program name
[5-9]   num(5)      Number
[10]    in_mem(1)   In memory Y/N
[11-15] with(5)     With param
[16]    no_base_wind(1) No base window Y/N
[17]    new_runtime(1) New runtime Y/N
[18]    no_delete(1)  No delete Y/N
[19]    no_save(1)    No save Y/N
```
Total: 20 bytes

---

### User Functions/Commands

#### UDC (0x004B) — User-defined command
```
[0-3]  label(4)    Label index
[4-8]  flist(5)    Field list
```
Total: 9 bytes

#### FUNC (0x0049) — Function call
```
[0-3]  label(4)    Label index
[4-8]  flist(5)    Field list
```
Total: 9 bytes

#### CMD (0x004A) — Command call
```
[0-3]  label(4)    Label index
[4-8]  flist(5)    Field list
```
Total: 9 bytes

---

### Miscellaneous

#### OWNER (0x0059) — Set file owner
```
[0-4]   hndl(5)     File handle
[5]     clr_set(1)  Clear/set flag
[6-10]  name(5)     Owner name
[11]    crypt(1)    Encrypt flag
[12]    rd_ok(1)    Read OK flag
```
Total: 13 bytes

#### PARAM (0x0071) — Parameter
```
[0-4]  field_list(5) Field list
```
Total: 5 bytes

#### FILTER (0x0084) — Set filter
```
[0-4]  expression(5) Filter expression
```
Total: 5 bytes

#### FORCE (0x005E) — Force
```
[0]    on_off(1)   Y/N
```
Total: 1 byte

#### FORCE3 (0x00B3) — Force (v3)
```
[0]    on_off(1)   Y/N
```
Total: 1 byte

#### WRAP (0x0085) — Wrap field
```
[0-4]   fld(5)      Field
[5-9]   col(5)      Column
[10-14] dlnes(5)    Display lines
```
Total: 15 bytes

#### REWRAP (0x0086) — Rewrap field
Total: 15 bytes (same as WRAP)

#### GETLBL (0x0097) — Get label
```
[0-3]  label(4)    Label index
[4-8]  fld(5)      Field param
```
Total: 9 bytes

#### GOTOL (0x007C) — Goto line
```
[0-4]  line(5)     Line number
```
Total: 5 bytes

#### DELF (0x007D) — Delete file
```
[0-4]  filename(5) File name
```
Total: 5 bytes

#### RENF (0x007E) — Rename file
```
[0-4]  from(5)     From name
[5-9]  to(5)       To name
```
Total: 10 bytes

#### DATE (0x007F) — Set date
```
[0-4]  date(5)     Date value
```
Total: 5 bytes

#### TIME (0x0080) — Set time
```
[0-4]  time(5)     Time value
```
Total: 5 bytes

#### CLOCK (0x0088) — Clock display
```
[0]     on_off(1)   On/off
[1-5]   col(5)      Column
[6-10]  row(5)      Row
[11]    mil(1)      Military Y/N
```
Total: 12 bytes

#### SOUND (0x0099) — Play sound
```
[0-4]   note_ary(5) Note array
[5-9]   beat_ary(5) Beat array
[10-14] max_notes(5) Max notes
[15-19] wav_file(5) WAV file
```
Total: 20 bytes

#### TRACE (0x009A) — Trace
```
[0]     do_what(1)  Action flag
[1-5]   fw(5)       File/window
```
Total: 6 bytes

#### CDPATH (0x0098) — Change directory path
```
[0-4]  path(5)     Path param
```
Total: 5 bytes

#### MOUSE (0x00B4) — Mouse control
```
[0-4]   fn(5)       Function
[5-9]   cc(5)       Click count
[10]    lok(1)      Lock flag
[11-14] err_lbl(4)  Error label
[15-19] owner(5)    Owner
[20-24] path(5)     Path
[25-29] fd(5)       File descriptor
[30]    on_off(1)   On/off
```
Total: 31 bytes

#### TRANSX (0x0036) — Transaction
```
[0-4]  param1(5)   Param 1
[5-9]  param2(5)   Param 2
```
Total: 10 bytes

#### EQU_MID (0x00B0) — Equals mid
```
[0-4]   recv(5)     Receive field
[5-9]   fld(5)      Source field
[10-14] start(5)    Start position
[15-19] size(5)     Size
[20-24] mem(5)      Memory
```
Total: 25 bytes

#### EQU_DAY (0x00B1) — Equals day
```
[0-4]  recv(5)     Receive field
[5-9]  fld(5)      Source field
```
Total: 10 bytes

#### EQU_XMT (0x00B2) — Equals month
```
[0-4]  recv(5)     Receive field
[5-9]  fld(5)      Source field
```
Total: 10 bytes

#### BRACE_OPEN (0x00C0) — `{`
Total: 0 bytes

#### BRACE_CLOSE (0x00C1) — `}`
Total: 0 bytes

## Field Spec Format (48 bytes, TAS 5.1)

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0-14 | 15 | Name | ShortString (temp) or space-padded (file fields) |
| 15-18 | 4 | Offset | Data offset |
| 19 | 1 | FieldType | Type char ('A','I','N','D','T','L','R','B','F','P','S') |
| 20 | 1 | Decimals | Decimal places |
| 21-24 | 4 | DisplaySize | Display width |
| 25-28 | 4 | ArrayCount | Array dimension (0 = not array) |
| 29 | 1 | IsFileField | 1 if from database file |
| 30 | 1 | PictureType | Picture format type |
| 31-34 | 4 | PictureLocation | Picture constant offset |
| 35 | 1 | IsReset | 1 if RESET keyword used |
| 36 | 1 | ForceUpperCase | 1 if force uppercase |
| 37 | 1 | AllocFlag | Allocation flag |
| 38-41 | 4 | InternalSize | Internal storage size |
| 42 | 1 | KeyNumber | Key number for indexed access |
| 43 | 1 | FileBufferNumber | Buffer slot number |
| 44 | 1 | IsReady | 1 if field is ready for use |
| 45 | 1 | HasInitialValue | 1 if has default value |
| 46-47 | 2 | FileHandle | Database file handle |

### Temp Field Name Format (ShortString)

Temp fields use Pascal ShortString format for the 15-byte name area:
```
[len: 1] [name: len bytes] ['0': 1] [index: 1] [spaces: 15-len-3]
```

Example for field index 5:
```
05 54 45 4D 50 30 30 05 20 20 20 20 20 20 20
^  T  E  M  P  0  '0' ^idx          spaces
len=5                  =5
```

All temp fields have: FieldType='S', IsReady=1, all other fields=0.

### Non-temp Field Name Format

File fields and defined fields use space-padded fixed arrays:
```
[char1] [char2] ... [charN] [0x20...] (padded to 15 bytes with spaces)
```

## Integer Zero Convention

The TAS compiler ALWAYS emits an integer zero constant (`49 00 00 00 00`)
at offset 0 in the constant segment. This is present in every .RUN file
regardless of whether the source code uses integer zero explicitly.

## ADV50.OVL Overlay

441 of 583 .RUN files require the ADV50.OVL overlay prepended for
decompilation. The overlay provides shared spec, constants, fields, and labels.
The decompiler auto-detects this via `LoadAutoOverlay()`.

Overlay stats: 16,867B spec, 19,583B const, 386 fields, 60 labels.

## Verified Against

- BKSTART.RUN: 5 instructions, byte-identical real compilation ✓
- Uses commands: CO, CHAIN, QUIT
- 583 .RUN files in `blank/` directory (1 unsupported: zbkapg.RUN with TWINB signature)
