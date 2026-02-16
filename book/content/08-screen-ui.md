TAS provides a complete set of screen and user interface commands. In CoTAS, every screen command is routed through the `IUIBridge` interface - either `ConsoleBridge` (terminal) or `WebBridge` (browser via SignalR).

## Displaying Text - SAY

The `SAY` command writes text at a specific screen position on the 25×80 character grid:

```tas
SAY 1,1 "Customer Entry"
SAY 3,5 "Name:"
SAY 3,15 cust_name
SAY 5,5 "Balance: " + STR(balance,10,2)
```

The two numbers specify row and column (1-based). In the `WebBridge`, SAY updates the internal `char[25,80]` screen buffer and sends a `Say(text, row, col)` SignalR message to the browser.

## Getting Input - ENTER

`ENTER` creates an input field and blocks until the user submits a value:

```tas
SAY 3,1 "Customer name:"
ENTER 3,20 cust_name

SAY 4,1 "Amount:"
ENTER 4,20 amount PICTURE "999,999.99"
```

In the `WebBridge`, `ENTER` sends a `RequestInput(fieldName, row, col, size)` message and awaits via `TaskCompletionSource<string>`. The browser renders a positioned text input overlay at the exact character coordinates, and the user's submission completes the task.

<div class="callout info">
<div class="callout-title">ℹ️ Async Input</div>
In TAS 5.1, ENTER blocked the entire process. In CoTAS, ENTER is <code>async Task&lt;string&gt;</code> - the interpreter awaits without blocking the .NET thread pool, enabling true multi-user concurrency.
</div>

## Quick Prompts - ASK

`ASK` displays a prompt and waits for a yes/no response:

```tas
ASK "Continue? (Y/N)"
IF ASK() = .T.
  GOSUB PROCESS
ENDIF
```

The result is stored in `FieldManager.LastAskResult`. In the browser, ASK renders as a modal dialog with Y/N buttons on a dark overlay.

## The Color System

TAS supports a rich color system for screen elements:

```tas
COLOR NORM white/blue          ; Normal text: white on blue
COLOR HIGH yellow/blue         ; Highlighted text: yellow on blue
COLOR EDIT white/black         ; Edit field colors
COLOR BORDER cyan/blue         ; Window border colors
```

Color-related commands implemented in CoTAS:

| Command | Handler | Description |
|---------|---------|-------------|
| `COLOR` | `ColorCommand` | Set color scheme |
| `ROW_COLOR` | `RowColorCommand` | Color a specific row |
| `REV` | `ReverseCommand` | Reverse video mode |
| `PAINT` | `PaintCommand` | Draw colored window border |
| `GRAY` | `GrayCommand` | Gray out a field/region |

<div class="callout new">
<div class="callout-title">⚡ Web UI Terminal Colors</div>
The CoTAS web terminal uses a classic hacker aesthetic - bright green (#00ff00) text on pure black (#000000) background, Consolas monospace font at 14px. Input fields use dark green (#001a00) backgrounds with a green glow effect on focus. The full TAS 16-color palette support is planned for future releases.
</div>

## Window Management

TAS supports overlapping windows for organizing screen content:

```tas
WINDOW 5,10,15,50              ; Create window at row 5, col 10, 15 rows, 50 cols
SAY 1,1 "Inside the window"   ; Coordinates are relative to window
WINDOW OFF                     ; Close the window
```

Window commands in CoTAS:

| Command | Handler | Description |
|---------|---------|-------------|
| `WINDOW` / `WNDW` | `WindowCommand` | Create/manage windows |
| `PAINT` | `PaintCommand` | Draw window borders |
| `SAVES` | `SaveScreenCommand` | Save screen buffer (25×80 snapshot) |
| `RSCR` | `RestoreScreenCommand` | Restore saved screen |
| `SCROLL` | `ScrollCommand` | Scroll a screen region |

The `FieldManager` maintains an internal `_screenBuffer` (25×80 character grid) and `_savedScreen` for SAVES/RSCR operations.

## Screen Control Commands

| Command | Handler | Description |
|---------|---------|-------------|
| `CLRSCR` | Built-in | Clear entire screen |
| `CLRLNE` | `ClearLineCommand` | Clear a single line |
| `CLRSF` | `ClearScreenFieldsCommand` | Clear all fields on screen |
| `CURSOR` | `CursorCommand` | Move cursor position |
| `BELL` | `BellCommand` | Sound the terminal bell |
| `REDSP` / `REDSP3` | `RedisplayCommand` | Redisplay screen contents |
| `HOTSPOT` | `HotSpotCommand` | Define clickable region |
| `BUTTON` | `ButtonCommand` | Create clickable button |
| `CAPTION` | `CaptionCommand` | Set window caption text |
| `SCRN` | `ScreenDefCommand` | Screen definition |

## Screen Functions

| Function | Returns | Description |
|----------|---------|-------------|
| `ROW()` | N | Current cursor row (1-25) |
| `COL()` | N | Current cursor column (1-80) |
| `LROW()` | N | Last row (25) |
| `MROW()` / `MAX_ROWS()` | N | Maximum rows (25) |
| `MCOL()` / `MAX_COLS()` | N | Maximum columns (80) |
| `PCOL()` | N | Printer column |
| `PROW()` | N | Printer row |
| `SCRCHR(row,col)` | A | Character at screen position |
| `ISCLR()` | L | Is screen clear? |
| `WINDOWS()` | L | Windows mode? (always `.T.` in CoTAS) |

## Key Traps - TRAP

`TRAP` registers a handler for special keys. The `TrapManager` maintains a dictionary of key → action mappings with push/pop support:

```tas
TRAP ESC TO EXIT_PROGRAM        ; ESC key jumps to label
TRAP F1 TO HELP_SCREEN          ; F1 opens help
TRAP F2 TO SAVE_RECORD          ; F2 saves current record
```

Trap management commands:

| Command | Handler | Description |
|---------|---------|-------------|
| `TRAP` | `TrapCommand` | Register trap handler (Goto, Gosub, or Ignore) |
| `PUSHT` | `PushTrapCommand` | Save current trap state to stack |
| `POPT` | `PopTrapCommand` | Restore previous trap state |
| `XTRAP` | `XtrapCommand` | Clear a specific trap |

Three trap action types: **Goto** (jump to label), **Gosub** (call subroutine), **Ignore** (suppress the key).

## Entry Mode Commands

These commands control how `ENTER` fields behave:

| Command | Handler | Description |
|---------|---------|-------------|
| `AUTODEC` | `AutoDecCommand` | Auto-decimal entry mode |
| `AUTOENTER` | `AutoEnterCommand` | Auto-advance when field is full |
| `AUTOINC` | `AutoIncCommand` | Auto-increment on field exit |
| `NOCLR` | `NoclrCommand` | Don't clear field on entry |
| `NOFD` | `NofdCommand` | No field default |
| `NOCMA` | `NocmaCommand` | Suppress comma formatting |
| `NOZERO` | `NozeroCommand` | Suppress leading zeros |
| `FORMAT` | `FormatCommand` | Set display format mask |
| `PICTURE` | `PictureCommand` | Set input picture mask |
| `JUSTIFY` | `JustifyFieldCommand` | Left/right/center justify |
| `FORCE` | `ForceCommand` | Force entry into field |
| `REENT` | `ReenterCommand` | Re-enter current field |

## The Web UI Protocol

<div class="callout info">
<div class="callout-title">ℹ️ Under the Hood</div>
The <code>WebBridge</code> maintains a 25×80 <code>char[,]</code> screen buffer. Every <code>SayAsync()</code> call updates this buffer and sends a SignalR message. Input operations use <code>TaskCompletionSource</code> - the interpreter awaits asynchronously while the browser collects input.
</div>

The bridge sends messages like:

```
Say("Customer:", 3, 5)                    → Write text at position
RequestInput("cust_name", 3, 20, 30)      → Show input overlay
RequestAsk("Continue?", false)             → Show Y/N modal
ClearScreen()                             → Reset buffer + UI
```

The browser responds:

```
SubmitInput("ACME Corp")                  → Complete pending input
SendKeyPress("F2")                        → Dispatch to TrapManager
```

This clean separation means the TAS program has no idea it's running in a browser - it just uses SAY, ENTER, and ASK as always. The `IUIBridge` interface has only 5 methods:

```csharp
Task ClearScreenAsync();
Task SayAsync(string text, int row, int col);
Task MessageAsync(string text);
Task<string> EnterAsync(string fieldName, int row, int col, int size);
Task<bool> AskAsync(string prompt, bool defaultValue);
```

Implementing a new bridge (mobile app, desktop GUI, API) requires only implementing these 5 methods.
