# CLOCK

| | |
|---|---|
| **Category** | Command |

## Description

Use this command to display a real-time clock on the screen.
CLOCK ON/OFF AT starting_column,row MIL
ON/OFF - Optional - If you set this option to ON the clock will be displayed on the screen,
OFF and it will be removed.

starting_column - f/c/e - Optional - The number of the column at which to start the display.
The first column on the screen (far left position) is number 1. The default value, if none is provided, is column 40.
row - f/c/e - Optional - The row or line number to use in displaying the clock. The first row on the screen (top) is number 1. The default value, if none is provided, is row 1.
MIL - Optional - If this option is included, the clock will display in 24 hour time instead of am/pm.

## Syntax

```text
CLOCK ON/OFF AT starting_column,row MIL
```

## Parameters

- **`ON/OFF`** · `e` · *Optional*

  ON/OFF - Optional - If you set this option to ON the clock will be displayed on the screen, OFF and it will be removed.

- **`starting_column`** · `f/c/e` · *Optional*

  starting_column - f/c/e - Optional - The number of the column at which to start the display.
  The first column on the screen (far left position) is number 1. The default value, if none is provided, is column 40.

- **`row`** · `f/c/e` · *Optional*

  row - f/c/e - Optional - The row or line number to use in displaying the clock. The first row on the screen (top) is number 1. The default value, if none is provided, is row 1.

- **`MIL`** · `f/c/e` · *Optional*

  MIL - Optional - If this option is included, the clock will display in 24 hour time instead of am/pm.
