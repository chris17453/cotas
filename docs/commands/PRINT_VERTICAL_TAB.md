# PRINT VERTICAL TAB

| | |
|---|---|
| **Category** | Command |
| **Abbreviation** | `PVERT` |

## Description

The command will move the next output to the line specified in the command.

## Syntax

```text
PVERT line_number PTW print_to
```

## Parameters

- **`line_number`** · `f/c/e` · *Required*

  The line number to move to. If the value specifies a row before (or above) the current row location, this command will be ignored.

- **`print_to`** · `SPD` · *Optional*

  Optional - You can use this option to specify the output device. The option ASK (a regular print_to option) is not available. The only options available are S screen, P - printer or D - default.

## Comments

The column value is automatically reset to column 1 after this command is executed. The command does a series of CR/LF's (0dh/0ah) until the appropriate row is reached.

## Program Editor

`Reports -> Vert tab`

## See Also

- [LD_PDRV](LD_PDRV.md)
