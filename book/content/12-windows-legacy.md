TAS Professional 5.1 included Windows programming extensions that allowed TAS programs to interact with the Windows operating system. This chapter documents these legacy features for reference.

<div class="callout info">
<div class="callout-title">ℹ️ Legacy Content</div>
The Windows programming features documented here were designed for 16-bit Windows (Win 3.1x and Win95). In CoTAS, the web-based UI replaces these native Windows APIs. This chapter is included for reference when working with existing programs that use Windows-specific commands.
</div>

## Windows vs DOS Compatibility

One of TAS Pro 5.1's key features was dual-platform support. Programs could run on both DOS and Windows with minimal changes:

- **Windows-only commands** are silently ignored on DOS (no error)
- **DOS-only commands** are silently ignored on Windows (no error)
- The same source code and data files work on both platforms

## Key Windows Components

### TP5WIN.EXE

The Windows runtime executable. It loads and executes compiled `.RUN` programs in a Windows environment.

### TP5WIN.INI

Configuration file for the Windows runtime, specifying:

- Default program paths
- Btrieve settings
- Window size and position
- Font preferences

## Windows-Specific Commands

TAS Pro 5.1 added several commands for Windows integration:

### OLE Automation

TAS can control other Windows applications through OLE (Object Linking and Embedding):

- Create OLE objects
- Call methods on external applications
- Exchange data with Excel, Word, and other OLE-enabled software

### DLL Calls

The `CALL_DLL` command allows TAS programs to call functions in Windows Dynamic Link Libraries:

```tas
CALL_DLL "USER32.DLL" "MessageBoxA" result, hwnd, message, title, flags
```

### Windows UI Enhancements

- **BUTTON** - Create clickable buttons
- **CAPTION** - Set window title bar text
- **LISTBOX** - Create scrollable list controls
- **CHECKBOX** - Toggle controls
- **RADIO** - Radio button groups

## Platform Detection

Programs can detect their runtime environment:

```tas
IF WINDOWS()
  ; Running under Windows
  CAPTION "My Application"
ELSE
  ; Running under DOS
  SAY 1,1 "My Application"
ENDIF
```

## Limitations

The original Windows version of TAS was 16-bit, which imposed certain restrictions:

- Maximum field size: 64 KB (65,535 characters)
- Maximum array elements per field: 64 K
- Development system (IDE) runs in DOS only
- Tested primarily on Windows 95; limited Windows NT support

<div class="callout new">
<div class="callout-title">⚡ CoTAS Removes These Limits</div>
CoTAS runs as a 64-bit .NET application. The 64 KB field limit is gone - fields can be up to 4 GB. The web UI works in any modern browser on any operating system. There is no separate DOS vs Windows mode.
</div>
