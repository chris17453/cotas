# PRINT MESSAGE

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `PMSG` |

## Description

The command will allow you to send a message, data, or combination of both to the SCREEN, PRINTER or default device.

## Syntax

```text
PMSG message_list AT starting_column,starting_row WAIT NOCR PTW print_to
ENTER enter_field COLOR color ABS
```

## Parameters

- **`message_list`** · `f/c/e1, f/c/e2,...` · *Required*

  Any combination of constants, expressions, and or field values. These will print in a continuous line. For example:
  PMSG ‘ABC’,’DEF’,X
  assuming X = ‘GHI’, will display:
  ABCDEFGHI
  You may include a maximum of 80 fields, constants, or expressions.

- **`starting_column`** · `f/c/e` · *Optional*

  Optional - The number of the column at which to start the display. The first column on the screen (far left position) is number 1. The default value, if none is provided, is the current column value as set by the last display or enter to the screen. This value can be easily determined through the use of the COL() function.

- **`starting_row`** · `f/c/e` · *Optional*

  Optional - The row or line number to display at. The first row on the screen (top) is number 1. The default value, if none is provided, is the current row value as set by the last display or enter to the screen. This value can be easily determined through the use of the ROW() function.

- **`WAIT`** · `flag` · *Optional*

  Optional - If this option is included, it will force the program to wait after completing this command for the user to press any key at the keyboard before continuing with the next command.

- **`NOCR`** · `flag` · *Optional*

  Optional - Normally, this command will print a CR/LF (0dh/0ah) after the message_list is printed. If this option is included in the command that will not happen. That will force the next line printed to print on the same line until or unless the line is automatically truncated or a print command without this option is executed.
  NOTE: If you use this feature and find that your lines are not being printed properly, make sure they don’t extend off the edge of the screen or printer. If they do, use the WRAP command (for long fields) or split up the lines.

- **`PTW`** · `flag` · *Optional*

  PTW.

- **`print_to`** · `SPD` · *Optional*

  Optional - You can use this option to specify where to print. The option ASK (a regular print_to option) is not available. The only options available are S screen, P - printer or D - default.

- **`ABS`** · `flag` · *Optional*

  Optional - If this option is included in the command the program will ignore any windows currently active and will place the box according to the location calculated from the absolute upper left hand corner of the screen. Normally, the location would be figured from the upper left hand corner of the current window. This only applies when displaying to the screen.

- **`ENTER`** · `section` · *Optional*

  ENTER enter_field COLOR color ABS

## Program Editor

`Reports -> prt Message`
