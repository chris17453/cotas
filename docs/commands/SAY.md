# SAY

| | |
|---|---|
| **Category** | Command |

## Description

This command will add a field/expression to the screen buffer. This would have the same effect as though it were included in the original MOUNT command.

## Syntax

```text
SAY fieldname AT starting_column,row COLOR color PICT picture
```

## Parameters

- **`fieldname`** · `fn/v/e` · *Required*

  The field/expression to be added to the screen buffer.

- **`starting_column`** · `f/c/e` · *Optional*

  The number of the column at which to start the display of the SAY field. The first column on the screen (far left position) is number 1. The default value, if none is provided, is the current column value as set by the last display or enter to the screen. This value can be easily determined through the use of the COL() function.

- **`row`** · `f/c/e` · *Optional*

  The row or line number at which to display the SAY field. The first row on the screen (top) is number 1. The default value, if none is provided, is the current row value as set by the last display or enter to the screen. This value can be easily determined through the use of the ROW() function.

- **`color`** · `f/c/e` · *Optional*

  This is the color value to be used when displaying the field or expression in fieldname.

- **`picture`** · `f/c/e` · *Optional*

  This is a picture value to be used when displaying or entering the field.

## Comments

You should MOUNT some sort of a screen before using this command, even if the screen is blank. You
can create a blank screen by running the Screen Painter and then, when the initial screen is displayed,
presing ESC and saving it at that time. If you have the Source Code to the Utilities a screen format
named CLR_SCR.SCP was included with your programs. This is a blank screen.

## Sample Program

`SAYTEST`

## Program Editor

`User interface -> Screen control -> saY`

## See Also

- [MOUNT](MOUNT.md)
- [CLRSF](CLRSF.md)
