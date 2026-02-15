# CLEAR SCREEN

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `CLRSCR` |

## Description

You use this command to clear the currently active window (may be entire screen).

## Syntax

```text
CLRSCR
```

## Comments

You can change the color in the current window by changing the normal color value in the COLOR command and then performing CLRSCR (Clear Screen). NOTE: If you have a screen mounted and then do a CLRSCR, the text and all the fields will be cleared. However, the next time you get a record or redisplay the entire screen, the fields that were part of that screen will be redisplayed. To remove them you must use the CLRSF (Clear Screen Fields) command.

## Program Editor

`User interface -> Screen control -> Clear screen`
